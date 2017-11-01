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
get_decision_data <- function(years = c(2017, 2017)){


  if (length(years)  != 2) {
    stop("The years parameter should always contain exactly two values")
  } else if (years[2] < years[1]){
    stop("The second year value should be strictly higher or equal than the first")
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
              "oral_proceedings" = proceedings,
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
get_decisions <- function(years = c(1946, 2017), mode = 'all', path = '.'){

  if (mode == 'all'){
    decision_data <- get_decision_data(years)
    decision_data <- download_files(decision_data$decisions,
                   decision_data$oral_proceedings,
                   decision_data$dissenting,
                   path)


    return(decision_data)
  } else if (mode == 'decisions'){

    results <- load_results(url = sprintf(JUDGMENTS_URL, years[1], years[2]))
    res <- download_files(decisions = results, path = path)
    results <- res$decisions
    return(results)

  } else
    stop("Unknown mode value")
}

download_files <- function(decisions, proceedings = NULL, dissenting = NULL, path = '.'){

  save_path <- apply(decisions,
                     1,
                     function(x)
                      file.path(path, as.integer(x["case_number"]), file_category_code(x["decision_type"])))

  decisions["local_path"] <- save_path

  apply(decisions, 1, function(x) download_decision(x, x["local_path"]))

  if (!is.null(proceedings)){
    save_path <- lapply(proceedings["case_number"], function(x) file.path(path, as.integer(x), "Oral.proceedings"))
    proceedings["local_path"] <- save_path
    apply(proceedings, 1, function(x) download_file(x["url"], x["local_path"]))
  }

  if (!is.null(dissenting)){
    save_path <- lapply(dissenting["case_number"], function(x) file.path(path, as.integer(x), "Dissenting"))

    dissenting["local_path"] <- save_path
    apply(dissenting, 1, function(x) download_file(x["url"], x["local_path"]))
  }

  return(list(decisions = decisions, proceedings = proceedings, dissenting = dissenting))
}

download_decision <- function(decision, dir){
  for (lang_version in c("en_pdf", "fr_pdf")){
    download_file(decision[lang_version], dir)
  }

}

download_file <- function(file_url, dir){

  local_full_path <- create_path(dir, file_url)

  if (file.exists(local_full_path)){
    message(paste(local_full_path, "already exists, nothing to do ..."))
    return()
  }

  message(paste("Downloading to ", local_full_path, "..."))
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

create_path <- function(path, file_url){
  if (!dir.exists(path))
    dir.create(path, recursive = T)

  local_filename <- get_filename_from_url(file_url)

  local_full_path <- file.path(path, local_filename)
  return(local_full_path)
}

file_category_code <- function(type="Judgment"){
  switch (type,
    Judgment = "JUD",
    Order = "ORD",
    `Advisory Opinion` = "ADV",
    `Advisory opinion` = "ADV",
    Dissenting = "Dissenting",
    `Oral Proceedings` = "Proceedings"
  )
}

get_filename_from_url <- function(file_url){
  parts <- stri_split_fixed(file_url, "/", simplify = T)
  file_name <- parts[length(parts)]
  return(file_name)
}
