library(abacus)
library(RSQLite)

context("Creating, writing and reading the test database")

db <- "test.db"


test_that("Create_testDB()", {
  expect_true(Create_testDB("."))
})


test_that("Insert()", {
  # with add_id
  df <- data.frame(owner = "Harry G", iban = "ASDF1234", bic = "ASD1234", type = "test", stringsAsFactors = FALSE)
  expect_true(Insert(df, "accounts", db, add_id = TRUE))
  con <- dbConnect(SQLite(), dbname = db)
  df2 <- dbGetQuery(con, "SELECT * FROM accounts WHERE id > 100")
  dbDisconnect(con)
  expect_identical(df, df2[, 2:5])
  
  # without add_id
  df <- data.frame(payor = 1, payee = 20, date = "2010-1-1", reference = "test", entry = "test", value = 1111, currency = "ASD", stringsAsFactors = FALSE)
  expect_true(Insert(df, "transactions", db))
  con <- dbConnect(SQLite(), dbname = db)
  df2 <- dbGetQuery(con, "SELECT * FROM transactions WHERE currency = 'ASD'")
  dbDisconnect(con)
  expect_equal(df, df2)
  
  # foreign keys eforced
  df <- data.frame(payor = 102, payee = 20, date = "2010-1-1", reference = "test", entry = "test", value = 1111, currency = "ASD", stringsAsFactors = FALSE)
  expect_error(Insert(df, "transactions", db))
  
  # insert a BLOB
  x <- list(a = 1:5, b = list(c = c("a", "b")))
  x_ <- list(serialize(x, NULL))
  df <- data.frame(type = "test2", model = I(x_), stringsAsFactors = FALSE)
  expect_true(Insert(df, "classifier", db))
  con <- dbConnect(SQLite(), dbname = db)
  df <- dbGetQuery(con, "SELECT * FROM classifier")
  x2 <- unserialize(df$model[[1]])
  expect_identical(x, x2)
  dbDisconnect(con)
})


file.remove(db)



# expect_equal # mit num ungenauigkeit
# expect_identical # identisch
# expect_match # string mathing
# expect_output # print output matchin

# expect_error / warning / message
#expect_error(1 + 1)


# epxectations = unterte ebene (element von testing)
# test = nächste Ebene Funktionalität testen
# conext/ file = obere Ebene, was wird allg getestet (Funktion oder so)