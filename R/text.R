# Functions for text processing including text extraction and
# construction of paragraph data
library(xml2)
library(rvest)

extract_text <- function(x, pdf_column, text_column = 'text', dir = '.'){

  paths <- prepare_paths(x, pdf_column, dir)
  x[text_column] <- sapply(paths, function(p) extract_decision_text(p, 'text'))
  return(x)
}

extract_decision_text <- function(path, mode = 'text'){

  message(paste("Converting", path, "..."))

  pdfbox_path <- system.file('inst', package = 'icj', 'pdfbox.jar')
  tmp <- tempfile()
  command <- paste("java -jar", pdfbox_path, "ExtractText -encoding UTF-8", path, tmp)

  shell(command)

  text <- paste(readLines(tmp, encoding = 'UTF-8'), collapse = '\n')
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

locate_judgment_start <- function(){
  jud <- "C:\\Users\\dcg601\\Documents\\142-pdfbox.html"
  html <- read_html(jud)

  par_texts <- html %>% html_nodes("p") %>% html_text()
  start <- xml_find_first(html, '//*[contains(text(), "delivers the following Judgment")]')
  next_ <- xml_find_first(start, 'following::div')
  next_p <- xml_find_all(n, './/p')
}
