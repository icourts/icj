library(icj)

context("Test get_data functions")

test_that("get_decision_data produces errors with incorrect params", {
  expect_error(get_decision_data(years = c(1900, 2017)))
  expect_error(get_decision_data(years = c(2017)))
  expect_error(get_decision_data(years = c(2017, 2000)))
})

test_that("get decision with empty years should work",
          # this takes too long though
          {expect_is(get_decision_data(), "list")}
          )

test_that("get_decision_data works", {
  data_list <- get_decision_data(c(2015, 2016))
  expect_equal(length(data_list), 4)
  expect_equal(dim(data_list$decisions)[1], 29)
})

context("Test download functions")

file_url <- "/files/case-related/101/101-19990217-ORA-01-00-BI.pdf"

test_that("filename from url", {
  fname <- get_filename_from_url(file_url)
  expect_equal(fname, "101-19990217-ORA-01-00-BI.pdf")})

test_that("Folder creation", {
  d <- tempdir()
  caseno <- 101

  path <- create_path(file.path(d, caseno, "JUD"), file_url)
  expect_equal(path, file.path(d, caseno, "JUD", get_filename_from_url(file_url)))

  expect_true(dir.exists(file.path(d, caseno, "JUD")))
})

test_that("Download saves as intended", {
  d <- tempdir()

  file_name <- get_filename_from_url(file_url)

  download_file(file_url, dir = file.path( d, 101, "Proceedings"))
  expect_true(file.exists(file.path(d, 101, "Proceedings", file_name)))

  expect_message(
    download_file(file_url, dir = file.path(d, "Proceedings")),
    paste(file_name, "already exists, nothing to do ...")
  )

})

test_that("get_decisions returns local_path as well", {
  data <- get_decisions(years = c(2017, 2017), mode = "all", path = tempdir())

  expect_length(data, 3)

  judgments <- data$decisions
  expect_true("local_path" %in% names(judgments))
})

