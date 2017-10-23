library(icj)


context("Test text parsing")


test_that("parse countries works correctly v. 2", {
  countries <- c("(Pakistan v. India)", "(Honduras v. France)", "(El Salvador/Honduras)", "(New Application: 2002) (Democratic Republic of the Congo v. Rwanda)")
  country_results <- parse_countries(countries)

  expect_equal(country_results[[1]]$applicant, c("Pakistan", "Honduras", "El Salvador", "Democratic Republic of the Congo"))
  expect_equal(country_results[[1]]$defendant, c("India", "France", "Honduras", "Rwanda"))
})

test_that("Intervention works", {
  intervening <- c("(El Salvador/Honduras: Nicaragua intervening)")
  expect_equal(parse_countries(intervening)[[2]], "Nicaragua")
})

test_that("match_countries works as expected", {
  countries <- data.frame(countries =
                            c("Denmark v. Norway",
                              "El Salvador/Honduras: Nicaragua intervening",
                              "Islamic Republic of Iran v. United States of America"),
                          stringsAsFactors = F)

  expect_equal(match_states(countries$countries),
               data.frame(applicant = c("Denmark", "El Salvador", "Islamic Republic of Iran"),
                          defendant = c("Norway", "Honduras: Nicaragua intervening", "United States of America"),
                          stringsAsFactors = F)
  )
})

test_that("match_intervening returns with intervention", {
  intervening <- c("(: Nicaragua intervening)", "(El Salvador v. Honduras)")
  expect_equal(match_intervening(intervening), c("Nicaragua", NULL))
})

test_that("document_id", {
  expect_equal(parse_decision_id("http://www.icj-cij.org/files/case-related/101/101-19990325-JUD-01-01-EN.pdf"),
               "101-19990325-JUD-01")
})
