library(rvest)
library(stringi)
library(dplyr)

###
#
# DEFINITIONS
#
###

BASE_URL <- "http://www.icj-cij.org"
JUDGMENTS_URL <- paste0(BASE_URL, "/en/decisions/all/%d/%d/desc")
TITLE_CSS <- '.docket-odd h4'
CASE_CSS <- '.docket-odd h5:nth-child(2) a'
SUBJECT_CSS <- '.docket-odd h5:nth-child(3)'
FR_PDF <- 'hr+ .docket-odd .btn+ .btn'
DISSENTING <- '.docket h5 > a'
ORAL_HEARING_TITLE <- 'h4 a'
ORAL_HEARING_TITLE_XPATH <- '//div[@class = "docket-odd"]//h4/a'
ORAL_HEARING_DESCRIPTION <- 'div.docket-odd > div > h6:nth-child(3)'
ORAL_HEARING_DESCRIPTION_XPATH <- '//div[@class="docket-odd"]/div/h5/h6[1]'

###
#
# FUNCTIONS
#
###

load_results <- function(url){

  h <- read_html_with_agent(url)

  title <-  h %>%
    html_nodes(TITLE_CSS) %>%
    html_text() %>%
    stri_trim_both()

  case <- h %>%
    html_nodes(CASE_CSS) %>%
    html_text() %>%
    stri_trim_both()

  case_url <- h %>%
    html_nodes(CASE_CSS) %>%
    html_attr("href")

  subject <- h %>%
    html_nodes(SUBJECT_CSS) %>%
    html_text() %>%
    stri_trim_both()

  en_pdf <-  h %>%
    html_nodes(paste(TITLE_CSS, "a")) %>%
    html_attr("href")

  fr_pdf <- h %>%
    html_nodes(FR_PDF) %>%
    html_attr("href")

  parsed_title <- parse_title(title)

  case_number <- parse_case_number(case_url)

  id = parse_decision_id(en_pdf)

  countries <- parse_countries(case)

  assertthat::are_equal(length(countries), 2)

  results <- data.frame(id = id,
                        title = as.character(title),
                        case_name = case,
                        case_number = case_number,
                        decision_type = parsed_title$decision_type,
                        date = parsed_title$date,
                        case_url =  case_url,
                        subject,
                        applicant = countries[[1]]$applicant,
                        defendant = countries[[1]]$defendant,
                        en_pdf = en_pdf,
                        fr_pdf = fr_pdf,
                        stringsAsFactors = F)

  return(results)
}

###
#
# Parsing functions
#
###
parse_title <- function(title){
  #
  # Splits the title to obtain decision type and decision date
  # We apply stri_split which is vectorized and
  # return a data frame
  #

  if( !length(title) > 0 ) stop("Cannot take empty vectors as input")

  title_df <- as.data.frame(stri_split_regex(title, "\\sof\\s", simplify = T))
  names(title_df) <- c("decision_type", "date")
  title_df$decision_type <- as.factor(title_df$decision_type)
  title_df$date <- as.Date(title_df$date, "%d %B %Y")

  return(title_df)
}

parse_case_number <- function(case_url){
  #
  # Isolate the case number from the case url
  #
  case_number <- stri_split_fixed(case_url, "/", simplify = T)[,4]
  return(as.integer(case_number))
}

parse_case_name <- function(str){
  return(stri_split_regex(str, pattern = "\\s+\\(", simplify = T)[1])
}

parse_countries <- function(str){
  parenthesis <- str %>%
    stri_match_last_regex("\\(.*?\\)") %>%
    stri_trim_both(pattern = "[\\p{Letter}\\p{Wspace}]")


  states <- match_states(parenthesis)
  # intervening <- NULL
  intervening <- match_intervening(states$defendant)

  # clean up the case where there is an intervening party
  states$defendant <- stri_split_fixed(states$defendant, pattern = ":", simplify = T, omit_empty = T)[,1]

  return(list(states, intervening))
}

parse_decision_id <- function(str){
  # Create a decision id that is the first 16 letters of the filename

  # str is a vector of urls to the pdf of the case
  # files is a matrix that holds all the url parts
  files <- stri_split_fixed(str, "/", simplify = T)

  # get the last column of the matrix that contains the path
  # extract the first 19 letters of this column as a document id
  substr(files[, dim(files)[2]], 1, 19)
}

match_intervening <- function(str){
  match <- stri_match_last_regex(str, pattern = ":\\s+(.+)\\s+intervening", simplify = T, omit_empty = F)
  return(ifelse(length(match) > 0, match[,2], NULL))
}

match_states <- function(str){
  states <- as.data.frame(stri_match_last_regex(str,
                                                "^(.*)\\s*(v\\.|/)\\s*(.*)",
                                                simplify = T)) %>%
    select("applicant" = V2, "defendant" = V4) %>%
    mutate(applicant = stri_trim_both(applicant),
           defendant = stri_trim_both(defendant)) %>%
    transmute(applicant = if_else(grepl("United Kingdom", applicant), "United Kingdom", applicant),
           defendant = if_else(grepl("United Kingdom", defendant), "United Kingdom", defendant))

  # The non dplyr version
  #
  # states <- stri_match_last_regex(str,
  #                                "^(.*)\\s*(v\\.|/)\\s*(.*)",
  #                                simplify = T) [, c(2,4)]
  # states <- as.data.frame(states)
  # # apply stri_trim_both and gather teh results as dataframe
  # states[] <- lapply(states, stri_trim_both)
  #
  # names(states) <- c("applicant", "defendant")
  return(states)
}

visit_case_url <- function(caseno, url){
  message(sprintf("Getting case data for %s ...", caseno))
  Sys.sleep(2)
  dissenting <- get_dissenting(caseno, url)
  Sys.sleep(2)
  proceedings <- get_oral_proceedings(caseno, url)
  return (list(dissenting=dissenting, proceedings=proceedings))
}

get_dissenting <- function(caseno, url){
  message(sprintf("Getting dissenting opinion data for %s ...", caseno))
  judgments <- paste0(url, "/judgments")
  html <- read_html_with_agent(judgments)

  dissenting_url <- html %>%
    html_nodes(DISSENTING) %>%
    html_attr('href')

  # The case might not have judgments or dissenting opinions
  # Yet the court doesn't give an error 404 if the case doesn't have judgments
  if (length(dissenting_url) == 0){
    return(NA)
  }

  dissenting_title <- html %>%
    html_nodes(DISSENTING) %>%
    html_text() %>%
    stri_trim_both()

  judgment_id <- parse_decision_id(dissenting_url)

  dissenting <- data.frame(
    case_number = caseno,
    judgment_id = judgment_id,
    title = dissenting_title,
    url = dissenting_url,
    stringsAsFactors = F,
    row.names = NULL
  )

  return (dissenting)
}

get_oral_proceedings <- function(caseno, url){
  message(sprintf("Getting oral proceeding data for case %s ...", caseno))
  # We distinguish the fact that url might be a file mainly for testing purposes
  if (startsWith(url, 'http')){
    oral_url <- paste0(url, '/oral-proceedings')
  } else
    oral_url <- url

  html <- read_html_with_agent(oral_url)

  url_list <- get_link_by_selector(html, xpath = ORAL_HEARING_TITLE_XPATH)

  title <- get_text_by_selector(html, xpath = ORAL_HEARING_DESCRIPTION_XPATH)

  if (length(title) == 0){
    return(NA)
  } else {

    oral_proc <- data.frame(
      case_number = caseno,
      title = title,
      url = url_list,
      stringsAsFactors = F,
      row.names = NULL
    )

    return(oral_proc)
  }
}

####
#
# Helper functions
#
####

get_text_by_selector <- function(html, css = NULL, xpath = NULL){
  if(is.null(xpath)){
    nodeset <- html_nodes(html, css = css)
  } else {
    nodeset <- html_nodes(html, xpath = xpath)
  }
  texts <- html_text(nodeset, trim = T)
  return(texts)
}

get_link_by_selector <- function(html, css = NULL, xpath = NULL){
  if(is.null(xpath)){
    nodeset <- html_nodes(html, css = css)
  } else {
    nodeset <- html_nodes(html, xpath = xpath)
  }
  links <- html_attr(nodeset, "href")
  return(links)}

read_html_with_agent <- function(url){
  r <- httr::GET(url = url, httr::user_agent("Mozilla/5.0 AppleWebKit/537.36 Chrome/61.0.3163.100 Safari/537.36"))
  read_html(r)
}
