#### test non-shiny functions
library(testthat)
test_dir(system.file("tests", "", package = "abacus"))

#### app
db <- "test.db"
library(abacus)
Create_testDB("test.db")
library(shiny)

#### install
roxygen2::roxygenise()
devtools::install()

#### install and app
roxygen2::roxygenise()
devtools::install()
abacus::abacusApp("test.db")

#### clean db and app
abacus::Create_newDB("my.db")
abacus::abacusApp("my.db")



#### basic settings
settings <- list(
  upload = list(
    default = list(
      col = list(name = 6, iban = 7, bic = 8, date = 3, reference = 5, entry = 4, value = 9, currency = 10),
      type = "giro", date = "%d.%m.%Y", colSep = "\t", decSep = ",", head = TRUE, skip = 0, nMax = -1
    ),
    test = list(
      col = list(name = 7, iban = 6, bic = 3, date = 8, reference = 4, entry = 5, value = 10, currency = 9),
      type = "savings", date = "%Y-%m-%d", colSep = ";", decSep = ".", head = FALSE, skip = 1, nMax = 100
    )
  )
)
































