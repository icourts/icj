# Functions for text processing including text extraction and
# construction of paragraph data
library(xml2)
library(rvest)

PARAGRAPH_NUMBER <- "^\\p{WHITE_SPACE}{0,9}([[:digit:]]+)\\."
# shall concat it though
DETECT_NUMBER_XPATH_COND <- '[normalize-space(translate(., "0123456789-&#x2010;&#x2011;&#x2012;", "")) = ""]'

extract_text <- function(x, pdf_column, text_column = 'text', dir = '.'){

  paths <- prepare_paths(x, pdf_column, dir)
  x[text_column] <- sapply(paths, function(p) extract_decision_text(p, 'text'))
  return(x)
}

extract_decision_text <- function(path, mode = 'text'){

  if (!(mode %in% c("html", "text"))) stop("Unrecognized mode parameter...")
  message(paste("Converting", path, "..."))

  pdfbox_path <- system.file('inst', package = 'icj', 'pdfbox.jar')
  # tmp <- tempfile()
  switches <- ifelse(mode == "html", "-encoding UTF-8 -html", "-encoding UTF-8")
  command <- paste("java -jar", pdfbox_path, "ExtractText", switches, path)

  shell(command)

  text <- paste(readLines(stri_replace_first_regex(path, "\\.pdf",
                               ifelse(mode== "text", '.txt', '.html')),
                                encoding = 'UTF-8'),
                collapse = '\n')
  return(text)
}

prepare_paths <- function(x, pdf_column, root){


  # Reconstruct the paths where the pdfs are expected to be found by combining
  # root/case_number/type/filename.pdf
  apply(x, 1, function (row) {
    type <- NULL
    if ('en_pdf' %in% names(row)){
      type <- file_category_code(row["decision_type"])
    } else if ('judgment_id' %in% names(row)){
      type <- 'Dissenting'
    } else if (length(names(row)) == 3){
      type <- 'Proceedings'
    } else
      stop('Input data.frame has unknown type')
    file.path(root, row['case_number'], type, get_filename_from_url(row[pdf_column]))
  }
  )
}

run_parse_text <- function(x, pdf_column, root = NULL){
  apply(x, 1, function(row) run_parse_row(row, pdf_column, root))
}

run_parse_row <- function(row, pdf_column, root){
  filename <- get_filename_from_url(row[pdf_column])

  local_file <- stri_replace_all_fixed(filename, ".pdf", ".html")
  full_path <- file.path(row["local_path"], local_file)
  type <- row_type(row)

  if (type == "Advisory Opinion" && row["case_number"] < 73)
    # Older advisory opinions don't have numbers and that's how we fool the parser
    h <- parse_text(full_path, 'Order')
  else
    h <- parse_text(full_path, type)

  if (!is.null(root)){
    local_path <- file.path(root, row['case_number'], file_category_code(type))
    full_path <- file.path(local_path, local_file)
    if (!dir.exists(local_path))
      dir.create(local_path, recursive = T)
  }

  write_html(h, full_path)

}


locate_first_par <- function(html, type = "Advisory Opinion"){
  start <- switch (type,
    "Advisory Opinion" = "gives the following",
    "Judgment" = "delivers the following",
    "Order" = "The International Court of Justice",
    FALSE # this will be the case for dissenting and hearings
  )

  if (!start == FALSE){
    text_start <- find_start_html(html, start)
    if (type %in% c("Advisory Opinion", "Judgment")){
      if (length(text_start) > 0)
        first_par <- html_node(text_start, xpath = 'following::*[starts-with(normalize-space(), "1.")]')
      else
        first_par <- html_node(html, xpath = '//p[starts-with(normalize-space(), "1.")]')
    }
    else
      first_par <- html_node(text_start, xpath = 'following::p')
    return(first_par)
  }

  # failsafe
  first_par <- html_node(html, xpath = '//p[1]')
  return(first_par)
}

find_start_html <- function(html, criterion){
    xpath_str <- sprintf('//*[contains(text(), "%s")]', criterion)
    first <- html_nodes(html, xpath = xpath_str)
    return(first)
}

conjunction <- function(str, formated_condition, op = " or "){
  # Takes a string vector str, a formatted condition str and an operand
  # and constructs a long condition where the formatted condition is repeated and the terms
  # are connected with the operand
  # eg. contains(normalized-text(), \"ORDER\"] or contains(normalized-text(), \"SEPARATE\"]"
  str_seq <- sapply(str, function(x) sprintf(formated_condition, x))
  condition <- paste0(str_seq, collapse = op)
  return(condition)
}

clean_html <- function(html){
  # boilerplate
  garbage <- html_nodes(html, xpath = '//*[contains(text(), ".indb")]')

  xml_remove(garbage)

  conditions <- c("ORDER",
                  "SEPARATE",
                  " JUDGE ",
                  "DISS. OP.",
                  "SEP. OP.",
                  "MINUTES",
                  "JUDGMENT",
                  "ADVISORY",
                  "ADVISOROYP",
                  "OPINION",
                  "DECLARATION",
                  "DECL.",
                  "CASE"
  )

  # remove some headers that appear right after the beginning of the page
  xpath_str <- paste0(c('//div/p[1][', conjunction(conditions, 'contains(normalize-space(), "%s")'), "]"), collapse = "")
  garbage <- html_nodes(html, xpath = xpath_str)
  xml_remove(garbage)

  # remove paragraphs containing only numbers because they are possibly just page numbers
  garbage <- html_nodes(html, xpath = paste0(c('//p', DETECT_NUMBER_XPATH_COND), collapse = ""))
  xml_remove(garbage)

  xml_remove(html_nodes(html, xpath = '//*[text() = ""]'))

}


parse_text <- function(path, type = "Advisory Opinion"){
  # parse a text that is a vailable at the given path and is of the given type.

  if (! endsWith(path, ".html")) stop("Can only parse html files")

  html <- read_html(path)
  clean_html(html)
  first_par <- locate_first_par(html, type)

  paragraphs <- html_nodes(first_par, xpath = ".|following::p")
  if(type == "Advisory Opinion")
    # NOTE THAT THE first adv opin with paragraphs is number 73 so we might have to pass case no as a param
    parse_numbered_paragraphs(paragraphs)
  else
    parse_paragraphs(paragraphs)

  return(html)
}

parse_numbered_paragraphs <- function(paragraphs){
  par_number <- 0
  # cur <- paragraphs[1]

  for (p in paragraphs){
    txt <- html_text(p, trim = T)

    # First try to replace gaps between the number in the paragraph beginning
    txt <- stri_replace_first_regex(str = txt, pattern = '^(\\d+)\\s+?(\\d+)(\\..+)', replacement = "$1$2$3",
                                    opts_regex = stri_opts_regex(dotall = T))
    txt <- stri_trim_left(txt, pattern = "[\\p{Alnum}]")
    xml_text(p) <- txt

    if (is_numbered(txt) && (get_par_number(txt) %in% c(par_number + 1, par_number + 2))){
      par_number <- get_par_number(txt)

      xml_set_attr(p, "number", par_number)
      cur <- p

    } else {
      cur_text <- html_text(cur, trim = T)
      p_text <- paste0(c(cur_text, txt), collapse = " ")
      # remove hyphens
      p_text <- replace_hyphens(p_text)

      # the add the text of this paragraph to cur which is the paragraph we saw at some point before
      # and delete the current node.
      fix_structure(cur)
      xml_text(cur) <- p_text
      xml_remove(p)
    }
  }
}

parse_paragraphs <- function(paragraphs){
  # join consecutive paragraphs i,i+1 if i doesn't end in punctuation or ends with a hyphen
  cur <- paragraphs[1]
  cur_txt <- html_text(cur, trim = T)
  for (p in paragraphs[2:length(paragraphs)]){

    p_txt <- html_text(p, trim = T)

    # if the current paragraph starts with number, capital letter, initial quotation or hyphen
    # and the last paragraph does not end in alphanumeric or hyphen
    # then reset cur to the p to open a new paragraph
    if (stri_startswith_charclass(p_txt, "[\\p{N}\\p{Lu}\\p{Pi}\\p{Pd}]") &&
        (!stri_detect_regex(cur_txt, "[,\\p{Alnum}\\p{Pd}]$") || is_uppercase(cur_txt))){
      cur <- p
      cur_txt <- html_text(cur, trim = T)
      fix_structure(cur)
      # xml_replace(cur, "p")
    } else {
      # concatenate text with cur
      cur_txt <- paste0(c(cur_txt, p_txt), collapse = " ")
      xml_remove(p)
    }

    # if (stri_endswith_charclass(cur_txt, "[\\p{Alnum}\\p{Pd}]")){
    # }
    cur_txt <- replace_hyphens(cur_txt)
    xml_text(cur) <- cur_txt

  }

}

replace_hyphens <- function(x){
  stri_replace_all_regex(x, "[\\-\u2010-\u2014\ufe58]\\p{WHITE_SPACE}{1,4}(?=\\p{L})", "")
}

get_par_number <- function(x){
  as.integer(stri_match_first_regex(x, pattern = PARAGRAPH_NUMBER, opts_regex = stri_opts_regex(case_insensitive = T))[,2])
}


is_numbered <- function(x){
  stri_detect_regex(str = x, pattern = PARAGRAPH_NUMBER)
}

is_uppercase <- function(x){
  toupper(x) == x
}

fix_structure <- function(node){
  # sometimes we encounter a node that has children that can be bold or italics
  # if we call xml_text(node) <- value, on that node, we will end up keeping the texts of the children
  # we can thus either remove all children elements as here
  # or append a new text child
  xml_remove(xml_nodes(node, xpath = 'descendant-or-self::*/text()'))
  xml_remove(xml_nodes(node, xpath = 'descendant::*'))
}

row_type <- function(row){
  type <- NULL
  if ('en_pdf' %in% names(row)){
    if(as.character(row['decision_type']) == "Advisory opinion"){
        type <- "Advisory Opinion"
    }
    else{
      type <- as.character(row['decision_type'])
    }
  } else if ('judgment_id' %in% names(row)){
    type <- 'Dissenting'
  } else if (length(names(row)) == 3){
    type <- 'Oral.proceedings'
  } else
    stop('Input data.frame has unknown type')
  return(type)
}

# xml processing (probably will not be used) ------------------------------


locate_judgment_start <- function(xml, criterion = "gives the following"){

  left <- sapply(xml_attrs(xml_find_all(xml, '//text')), "[[", "left")
  # count the frequencies
  t <- table(left)
  top_margin <- names(t[which.max(t)])

  first_par <- find_start(xml, criterion)
  first_row_margin <- xml_attr(first_par, "left")

  first_page <- find_page(first_par)
  last_page <- find_last_page(xml)

  return(list(first_par = first_par,
              paragraph_margin = top_margin,
              first_row_margin = first_row_margin,
              first_page = first_page,
              last_page = last_page))
}

run_parse_jud <- function(path, type = "Advisory Opinion", every_page = T){
  xml <- read_xml(path)
  setup <- locate_judgment_start(xml,
                                 criterion = ifelse(type == "Advisory Opinion",
                                                    "gives the following",
                                                    "delivers the following")
                                 )

  clean_document(xml, setup = setup)

  # In advisory opinions every sceond page is in English
  if (!every_page){
    pages = seq(as.integer(setup$first_page)+2, as.integer(setup$last_page), by = 2)
    condition <- paste('@number="', paste(pages, collapse = '" or @number="'), '"', sep="")
    xpath_str <- sprintf('.|./following::text[ancestor::page[@number="%s"]]|following::page[%s]/text', setup$first_page, condition)
    text_elems <- xml_find_all(setup$first_par, xpath = xpath_str)
  }
  else{
    text_elems <- xml_find_all(setup$first_par, xpath = '.|following::text')
  }

  new_root <- init_xml_tree(type)
  p <- xml_child(new_root)
  par_number  <- 0

  for (t in text_elems){
    txt <- xml_text(t, trim = T)

    if (is_numbered(txt)){
      n <- get_par_number(txt)

      if (n == par_number + 1 && is_par_start(t, setup$first_row_margin)){
        # start new paragraph
        par_number <- get_par_number(txt)
        if (!is.na(par_number) && par_number != 1){
          p <- xml_add_child(new_root, "paragraph")
          xml_attr(p, "number") <- par_number
        }
        t <- xml_add_child(p, "text")
        xml_text(t)<-txt

        # } else if (is_quote_start(txt)){
        #   # append to previous as quoted
        #   q <- xml_add_child(p, "quote")
        #   qt <- xml_add_child(q, "text")
        #   xml_text(qt) <- txt

      }
    } else {
      # append to previous and separate with a space
      # this will cause problems to hyphenated words
      p_text <- xml_text(p)
      p_text <- paste0(c(p_text, txt), collapse = " ")
      # remove hyphens
      p_text <- replace_hyphens(p_text)
      xml_text(p) <- p_text
    }

  }

  return(new_root)
}


is_par_start <- function(x, first_line_margin){
  return(xml_attr(x, "left") == first_line_margin)
}



is_quote_start <- function(x){
  stri_detect_regex(x, pattern = "^[\u201c\u0060\"]")
}



find_page <- function(x){
  xml_find_first(x, 'ancestor::page[1]') %>%
    xml_attr("number")
}

find_last_page <- function(x){
  pages <- xml_find_all(x, '//page')
  last <- xml_attr(pages[length(pages)], "number")
  return(last)
}

find_start <- function(x, criterion = "gives the following"){
  xpath_str <- sprintf('//*[contains(text(), "%s")]', criterion)
  first <- xml_find_first(x, xpath = xpath_str)
  first_par <- xml_find_first(first, xpath = 'following::*[starts-with(normalize-space(), "1.")]')
}

init_xml_tree <- function(doctype = "Advisory Opinion", ...){
  r <- xml_new_root('document')
  xml_set_attrs(r, c("type" = doctype))
  p <- xml_add_child(r, 'paragraph')
  xml_set_attrs(p, c("number" = 1))
  return(r)
}

clean_document <- function(xml, setup){
  garbage <- xml_find_all(xml, '//*[contains(text(), ".indb")]')
  xml_remove(garbage)

  top_margins <- as.integer(xml_attr(xml_find_all(setup$first_par, 'ancestor::page/text'), "top"))
  highest_line <- min(top_margins)
  lowest_line <- max(top_margins)

  xml_remove(xml_find_all(xml, sprintf('//text[@top="%d"]', highest_line)))
  xml_remove(xml_find_all(xml, sprintf('//text[@top="%d"]', lowest_line)))

  # remove lines that contain only numbers
  xml_remove(xml_find_all(setup$first_par, 'following::*[translate(., "0123456789", "") = ""]'))

}
