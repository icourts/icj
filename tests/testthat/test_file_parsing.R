library(icj)


test_that("find start for ADV",{
  html <- read_html('131-ADV.html')
  start <- find_start_html(html, criterion = "gives the following")
  expect_length(html_text(start, trim = T), 1)

  html <- read_html('146-ADV.html')
  start <- find_start_html(html, criterion = "gives the following")
  expect_length(html_text(start, trim = T), 1)
  expect_equal(html_text(start[1], trim = T), "gives the following Advisory Opinion :")
})

test_that("find start for ORD",{
  html <- read_html('112-ORD.html')
  start <- find_start_html(html, criterion = "The International Court of Justice")
  expect_length(html_text(start, trim = T), 1)
})

test_that("locate first par for ADV", {
  html <- read_html('131-ADV.html')
  first_par <- locate_first_par(html)
  expect_equal(html_name(first_par), "p")
  expect_true(startsWith(html_text(first_par, trim = T), "1. The question on which the advisory"))
})

test_that("locate first par for ORD", {
  html <- read_html('112-ORD.html')
  first_par <- locate_first_par(html, "Order")

  expect_true(startsWith(html_text(first_par, trim = T), "Composed as above. \nAfter deliberation,"))
})

test_that("locate first par for dissenting or oral hearings", {
  html <- read_html('130-DIS.html')
  first_par <- locate_first_par(html, "Dissenting")

  expect_true(startsWith(html_text(first_par, trim = T), "133"))

  html <- read_html('130-ORA.html')
  first_par <- locate_first_par(html, "Oral.proceedings")

  exp_start <- "Non-Corrigé"
  Encoding(exp_start) <- "UTF-8"
  expect_true(startsWith(html_text(first_par, trim = T), exp_start ))

})

test_that("row_type", {
  r <- list("en_pdf" = ".pdf", "decision_type" = factor(c("Advisory opinion"), levels = c("Advisory opinion")))
  rt <- row_type(r)
  expect_true( rt == "Advisory Opinion")
})

test_that("cleaning works",{
  html <- read_html("130-DIS.html")
  clean_html(html)

  diss <- html_nodes(html, xpath = '//div/p[1][contains(text(), "DISS. OP.")]')
  expect_length(diss, 0)

  page_number <- html_nodes(html, xpath = '//div/p[1][text() = "126"]')
  expect_length(page_number, 0)

  html <- read_html('072-ADV.html')
  pageno <- html_nodes(html, xpath = '//p[normalize-space() = "34"]')
  expect_length(pageno, 1)
  clean_html(html)
  pageno <- html_nodes(html, xpath = '//p[text() = "34"]')
  expect_length(pageno, 0)


  html <- read_html('130-ORA.html')
  pagestr <- "- 13 -"
  # Encoding(pagestr) <- "UTF-8"
  pageno <- html_nodes(html, xpath = sprintf('//p[normalize-space() = "%s"]', pagestr))
  expect_length(pageno, 1)
  clean_html(html)
  pageno <- html_nodes(html, xpath = '//p[normalize-space() = "- 13 -"]')
  expect_length(pageno, 0)

})

test_that("parse with numbered paragraphs", {
  h131 <- parse_text('131-ADV.html')
  par_19 <- html_text(html_node(h131, xpath = '//p[starts-with(text(), "19.")]'), trim = T)
  expect_true(startsWith(par_19, "19. The Tenth Emergency Special Session"))
  expect_true(endsWith(par_19, "and 8 December 2003)."))

  # check that the header is deleted
  expect_true(!stri_detect_fixed(par_19, "CONSTRUCTION OF A WALL (ADVISORY OPINION)"))

  h72 <- parse_text('072-ADV.html')
  par_55 <- html_text(html_node(h72, xpath = '//p[starts-with(normalize-space(), "55.")]'), trim = T)

  st <- "(Applicationf or Review \nof Judgement No. 158 of the United Nations Administrative Tribunal, \nI.C.J. Reports 1973, p. 201, para. 70.)"
  expect_true(endsWith(par_55, st))
})

test_that("with numbered paragraphs the final number of paragraphs equals the max paragraph no",{
  # unfortunately the same test with 131-ADV will fail because of OCR errors in recognizing
  # the paragraphs numbers in the above judgment
  h146 <- parse_text('146-ADV.html')
  first_par <- find_start_html(h146, "gives the following")
  expect_length(html_nodes(first_par, xpath = 'following::p'), 100)
})

test_that("parse dissenting opinion", {
  dis <- parse_text('130-DIS.html', type = "Dissenting")
  write_html(dis, '130.dis.test.html')
  par_3 <- html_text(html_node(dis, xpath = '//p[starts-with(normalize-space(), "3")]'), trim = T)

  # testing a paragraph that is split by headings and contains hyphanated words
  end <- "sovereignty based on the construction and commissioning of the lighthouse”\n(Judgment, para. 162)."
  Encoding(end) <- "UTF-8"

  expect_true(endsWith(par_3, end))

  exp_41 <- "41. In most situations acquiescence is linked to estoppel or prescription, but
  in this case it is connected instead to tacit agreement, in much the same way that was done
  by the Court in Land, Island and Maritime Frontier Dispute (El Salvador/Honduras : Nicaragua intervening)
  (Judgment, I.C.J. Reports 1992, p. 577, para. 364). Like tacit agreement,
  acquiescence must be strictly interpreted. According to I. C. MacGibbon:"

  par_41 <- html_text(html_node(dis, xpath = '//p[starts-with(normalize-space(), "41")]'), trim = T)
  expect_equal(stri_extract_all_words(par_41), stri_extract_all_words(exp_41))
})
