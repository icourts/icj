library(icj)

context("Test get_data functions")

test_that("get_decision_data produces errors with incorrect params", {
  expect_error(get_decision_data(years = c(1900, 2017)))
  expect_error(get_decision_data(years = c(2017)))
  expect_error(get_decision_data(years = c(2017, 2000)))
})

test_that("get_decision_data works", {
  data_list <- get_decision_data(c(2015, 2016))
  expect_equal(length(data_list), 4)
  expect_equal(dim(data_list$decisions)[1], 29)
})

context("Test download functions")

test_that("File creation", {
  d <- tempdir()
  path <- create_path(d, 1, "JUD")
  expect_equal(path, file.path(d, 1, "JUD"))
  expect_true(dir.exists(file.path(d, 1, "JUD")))
})

test_that("Download saves as intended", {
  d <- tempdir()
  file_url <-
    "/files/case-related/101/101-19990217-ORA-01-00-BI.pdf"
  file_name <- get_filename_from_url(file_url)

  download_file(101, file_url, root_dir = d, sub_dir = "Proceedings")
  expect_true(file.exists(file.path(d, 101, "Proceedings", file_name)))

  expect_message(
    download_file(101, file_url, root_dir = d, sub_dir = "Proceedings"),
    paste(file_name, "already exists, nothing to do ...")
  )

})
