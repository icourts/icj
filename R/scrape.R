library(rvest)
library(dplyr)


#' Get decision data for the specified year range
#'
#'
#' @param years a vector of the form (\code{year_from}, \code{year_to})
#'
#' @return list of four data.frames with all decision data
#' @export
#'
#' @examples
get_decision_data <- function(years = c(1946, 2017)){


  if (length(years)  != 2) {
    stop("The years parameter should always contain exactly two values")
  } else if (years[2] <= years[1]){
    stop("The second year value should be strictly higher than the first")
  } else if (years[1] < 1946){
    stop("Wrong start year. The ICJ does not have decisions before 1946")
  }


  # Get all the decisions from the specified years and crawl the site for case information
  results <- load_results(sprintf(JUDGMENTS_URL, years[1], years[2]))

  cases <- results %>%
    distinct(case_number, .keep_all = T) %>%
    select(case_number, case_name, case_url, applicant, defendant) %>%
    mutate(case_url = paste0(BASE_URL, case_url))

  # collect all case data in a list where each entry corresponds to a case andhas a field "$proceedings"
  # for the oral proceedings and a field "$dissenting" for the dissenting opinions of the case
  case_data <- apply(cases, 1, function(x) visit_case_url(caseno = x["case_number"], url = x["case_url"]))

  # Then concatenate all data into separate dataframes
  proceedings <- do.call(rbind, lapply(case_data, "[[", "proceedings"))
  dissenting <- do.call(rbind, lapply(case_data, "[[", "dissenting"))
  # Remove NAs introduced by cases without judgments
  proceedings <- proceedings[!is.na(proceedings$case_number), ]
  dissenting <- dissenting[!is.na(dissenting$case_number), ]

  results_list <- list("cases" = cases,
              "decisions" = results,
              "oral_proceegins" = proceedings,
              "dissenting_opinions" = dissenting)

  return(results_list)
}


#' Get all decision data and download all files
#'
#' @param years
#' @param mode character. takes two possible values: 'all' to download decisions,
#' dissenting opinions, and hearings, or 'judgments' to download only the judgments.
#' @param dir character. The path where to save the files.
#'
#' @return list of four data.frames with all decision data
#' @export
#'
#' @examples
get_decisions <- function(years = c(1946, 2017), mode = 'all', dir = '.'){

  if (mode == 'all'){
    decision_data <- get_decision_data(years)
    download_files(decision_data$decisions,
                   decision_data$proceedings,
                   decision_data$dissenting,
                   dir)
    return(decision_data)
  } else if (mode == 'decisions'){
    results <- load_results(url = sprintf(JUDGMENTS_URL, years[1], years[2]))
    download_files(decisions = results, dir = dir)
    return(results)
  } else
    stop("Unknown mode value")
}

download_files <- function(decisions, proceedings = NULL, dissenting = NULL, dir = '.'){

  apply(decisions, 1, function(x) download_decision(x, dir))
  if (!is.null(proceedings)){
    apply(proceedings, 1, function(x) download_file(x["case_number"], x["url"], dir, "Oral Proceedings"))
  }

  if (!is.null(dissenting)){
    apply(dissenting, 1, function(x) download_file(x["case_number"], x["url"], dir, "Dissenting"))
  }

}

download_decision <- function(decision, dir){

  for (lang_version in c("en_pdf", "fr_pdf")){
    download_file( decision["case_number"], decision[lang_version], dir, file_category_code(as.character(decision["decision_type"])))
  }

}

download_file <- function(case_number, file_url, root_dir, sub_dir){
  path <- create_path(root_dir, case_number, sub_dir)

  # get the filename from the url
  local_filename <- get_filename_from_url(file_url)

  local_full_path <- file.path(path, local_filename)

  if (file.exists(local_full_path)){
    message(paste(local_full_path, "already exists, nothing to do ..."))
    return()
  }

  message(paste("Downloading", local_filename, "..."))
  tryCatch( download.file(url = paste0(BASE_URL, file_url),
                destfile = local_full_path,
                mode = "wb",
                quiet = T),
            error = function(){
              warning(paste0("Error when downloading:", paste0(BASE_URL, file_url), " ..."))
            },
            finally = Sys.sleep(2)
            # Sleep after we have downloaded to prevent hammering the webserver
  )
}

create_path <- function(root, case_number, file_category="JUD"){
  new_path <- file.path(root, case_number, file_category)
  dir.create(new_path, recursive = T)
  return(new_path)
}

file_category_code <- function(type="Judgment"){
  switch (type,
    Judgment = "JUD",
    Order = "ORD",
    `Advisory Opinion` = "ADV",
    Dissenting = "Dissenting",
    `Oral Proceedings` = "Proceedings"
  )
}

get_filename_from_url <- function(file_url){
  parts <- stri_split_fixed(file_url, "/", simplify = T)
  file_name <- parts[length(parts)]
  return(file_name)
}
