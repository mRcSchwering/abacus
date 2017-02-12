#### test non-shiny functions
library(testthat)
test_dir("./inst/tests")
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




































