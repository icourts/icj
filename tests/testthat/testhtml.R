library(icj)


context("Test HTML functionalities")



test_that("load_results brings correct data for the years 1946-1948",
          {
            results <- load_results(sprintf(JUDGMENTS_URL, 1946, 1948))
            expect_is(results, "data.frame")
            expect_equal(dim(results),c(8, 12))
            expect_is(results$date, "Date")
            expect_equal(results[1, "date"], as.Date("1948/12/17"))
            expect_equal(results[1, "case_url"], "/en/case/1")
          }
          )


test_that("parse_title parses correctly",{
  example_titles = c("Order of 4 July 2011", "Judgment of 4 May 2011")

  title_date <- parse_title(example_titles)

  expect_is(title_date, "data.frame")
  expect_equal(names(title_date), c("decision_type", "date"))
  expect_equal(title_date$decision_type, as.factor(c("Order", "Judgment")))
  expect_equal(title_date$date, as.Date(c("2011-07-04", "2011-05-04")))
})


test_that("parse_title handles empty", {
  empty_titles = NULL
  expect_error(parse_title(empty_titles))}
  )

test_that("get_dissenting works",
          {
            results <- get_dissenting(101, "http://www.icj-cij.org/en/case/101")
            expect_equal(dim(results), c(3,4))
            expect_equal(results[1, "title"], "Dissenting Opinion of Vice-President Weeramantry")
            expect_equal(results[1, "url"], "/files/case-related/101/101-19990325-JUD-01-01-EN.pdf")
            expect_equal(results[1, "judgment_id"], "101-19990325-JUD-01")
          })

test_that("get_dissenting with more than one judgments per case",
          {
            results <- get_dissenting(1, "http://www.icj-cij.org/en/case/1")

            expect_equal(dim(results), c(9,4))

            # first judgment
            expect_equal(results[2, "title"], "Dissenting opinion by Judge ad hoc Daxner")
            expect_equal(results[2, "judgment_id"], "001-19480325-JUD-01")
            expect_equal(results[2, "case_number"], 1)
            # Second judgment
            expect_equal(results[3, "title"], "Separate opinion by Judge Alvarez")
            expect_equal(results[3, "judgment_id"], "001-19490409-JUD-01")
            expect_equal(results[3, "url"], "/files/case-related/1/001-19490409-JUD-01-01-EN.pdf")
          })

test_that("get dissenting with a case without judgments givs NA",{
  results <- get_dissenting(112, 'http://www.icj-cij.org/en/case/112')
  expect_true(is.na(results))
})

test_that("get_oral_proceedings works",{
  results <- get_oral_proceedings(1, "http://www.icj-cij.org/en/case/1")
  expect_equal(dim(results), c(4, 3))
  expect_equal(results[1, "case_number"], 1)
  expect_equal(results[1, "title"], "Minutes of the Sittings held from February 26th to March 5th, 1948")
  expect_equal(results[1, "url"], "/files/case-related/1/001-19480226-ORA-01-00-BI.pdf")
})

test_that("get_oral_proceedings in case 150",{
  results <- get_oral_proceedings(150, "http://www.icj-cij.org/en/case/150")
})

test_that("get_oral_proceedings with NA",{
  html <- system.file('extdata', 'Oral_proceedings_165.html', package = 'icj')
  results <- get_oral_proceedings(165, html)
  expect_true(is.na(results))
})

test_that("get by selector functions work",{
  html <- read_html(sprintf(JUDGMENTS_URL, 1946, 1948))

  links <- get_link_by_selector(html, css = CASE_CSS)
  expect_equal(length(links), 8)
  expect_equal(links[1], "/en/case/1")
  expect_is(links, "character")

  texts <- get_text_by_selector(html, css = CASE_CSS)
  expect_equal(length(texts), 8)
  expect_equal(texts[1], "Corfu Channel (United Kingdom of Great Britain and Northern Ireland v. Albania)")

  html <- read_html(sprintf(JUDGMENTS_URL, 1946, 1947))

  links <- get_link_by_selector(html, css = CASE_CSS)
  exp_links <- c("/en/case/3", "/en/case/1", "/en/case/1")
  expect_equal(links, exp_links)

  html <- read_html("http://www.icj-cij.org/en/case/1/oral-proceedings")
  links <- get_link_by_selector(html, xpath = ORAL_HEARING_TITLE_XPATH)
  exp_links <- c("/files/case-related/1/001-19480226-ORA-01-00-BI.pdf",
                 "/files/case-related/1/001-19491109-ORA-01-00-BI.pdf",
                 "/files/case-related/1/001-19491109-ORA-02-00-BI.pdf",
                 "/files/case-related/1/001-19491117-ORA-01-00-BI.pdf")
  expect_equal(links, exp_links)
})
