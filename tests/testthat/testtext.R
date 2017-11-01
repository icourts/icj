library(icj)

context("test text functions")

test_that("is quoted start detects quote starts", {
  quoted_str <-
    '“On the basis of the evidence and legal arguments presented in this memorial, the Applicant Requests the Court :'
  Encoding(quoted_str) <- 'UTF-8'

  expect_true(is_quote_start(quoted_str))
  expect_true(is_quote_start('"quoted string 2'))
})

context("test xml functions")

xmlstr <- "<page number=\"1\">
            <text>things before</text>
          <text>gives the following Opinion</text>
          <text>  1. first paragraph</text>
          </page>"

xml <- read_xml(xmlstr)
start <- find_start(xml)

test_that("detect first",{
  expect_equal(stri_trim_both(xml_text(start)), "1. first paragraph")
})

xml2 <- read_xml('146.xml')
start <- find_start(xml2)
first_page <- find_page(start)

test_that("find start and page work on a real document", {
  expect_true(startsWith(xml_text(start, trim = T), "1."))
  expect_equal(xml_attr(start, "left"), "214")
  expect_equal(xml_attr(start, "top"), "1087")
  expect_equal(first_page, "12")
})

test_that("is numbered works", {
  expect_true(is_numbered("  1. test"))
})


test_that("get paragraph number", {
  expect_equal(get_par_number("  11. test"), 11)
  expect_true(is.na(get_par_number("   ")))
})

test_that("replace hyphens", {
  x <- "Develop- ment (hereinafter “IFAD” or the “Fund”) informed the Court that the Execu"
  Encoding(x) <- "UTF-8"
  r <- replace_hyphens(x)
  expect_true(startsWith(r, "Development"))
})

setup <- locate_judgment_start(xml2)


test_that("document clean",{
  clean_document(xml2, setup)
  boiler <- xml_find_all(xml2, '//*[contains(text(), ".indb")]')
  expect_equal(length(boiler), 0)
  top <- xml_find_all(xml2, '//text[@top="159"]')
  expect_equal(length(top), 0)
})

test_that("Produced document",{
  new_xml <- run_parse_jud('146.xml')
  pars <- xml_find_all(new_xml, 'paragraph')
  expect_length(pars, 100)
  expect_equal(xml_attr(pars[1], "number"), "1")
  expect_equal(xml_attr(pars[100], "number"), "100")

  expect_true(startsWith(xml_text(pars[1]), "1. By a letter dated 23 April 2010,"))
  end <- "What is the validity of the decision given by the ILOAT in its Judgment No.  2867 ?’”"
  Encoding(end) <- "UTF-8"
  #print(xml_text(pars[1], trim = T))
  expect_true(endsWith(xml_text(pars[1], trim = T), end))
})
