library(abacus)
library(RSQLite)

context("Creating, writing and reading the test database")

db <- "test.db"


test_that("Create_testDB()", {
  expect_true(Create_testDB("."))
})


test_that("test.db contents", {
  con <- dbConnect(SQLite(), dbname = db)
  df <- dbGetQuery(con,  "SELECT * FROM sqlite_master") # WHERE tbl_name = 'accounts'")
  dbDisconnect(con)
  expect_equal(df$type, c("table", "table", "index", "index", "table", "index", "table", "index", "table", "table", "index"))
  expect_equal(df$name, c("accounts", "transactions", "transact_payor_index", "transact_payee_index", "capital", 
                          "capital_index", "personalAccounts", "personalAccounts_index", "cashflow", "storage", "sqlite_autoindex_storage_1"))
  expect_equal(df$rootpage, c(2, 3, 4, 5, 6, 7, 9, 11, 12, 13, 14))
})


test_that("write db", {
  # with add_id
  df <- data.frame(owner = "Harry G", iban = "ASDF1234", bic = "ASD1234", stringsAsFactors = FALSE)
  expect_true(Insert(df, "accounts", db, add_id = TRUE))
  con <- dbConnect(SQLite(), dbname = db)
  df2 <- dbGetQuery(con, "SELECT * FROM accounts WHERE id > 100")
  dbDisconnect(con)
  expect_identical(df, df2[, 2:4])
  
  # without add_id
  df <- data.frame(payor = 1, payee = 20, date = "2010-1-1", reference = "test", entry = "test", value = 1111, currency = "ASD", type = "test", stringsAsFactors = FALSE)
  expect_true(Insert(df, "transactions", db))
  con <- dbConnect(SQLite(), dbname = db)
  df2 <- dbGetQuery(con, "SELECT * FROM transactions WHERE currency = 'ASD'")
  dbDisconnect(con)
  expect_equal(df, df2)
  
  # foreign keys eforced
  df <- data.frame(payor = 102, payee = 20, date = "2010-1-1", reference = "test", entry = "test", value = 1111, currency = "ASD", type = "test", stringsAsFactors = FALSE)
  expect_error(Insert(df, "transactions", db))
  
  # insert a BLOB
  x <- list(a = 1:5, b = list(c = c("a", "b")))
  expect_true(InsertBLOB("test", x, db))
  con <- dbConnect(SQLite(), dbname = db)
  df <- dbGetQuery(con, "SELECT * FROM storage")
  x2 <- unserialize(df$data[[1]])
  expect_identical(x, x2)
  dbDisconnect(con)
  
  # unique enforced
  expect_error(InsertBLOB("test", x, db))
})


test_that("read db", {
  
  # accounts with eq
  df <- Select("accounts", db, eq = list(id = 100))
  expect_identical(df$owner, "Malcom Absalon")
  
  # transactions (mult. join) with ge and le dates
  df <- Select("transactions", db, ge = list(date = "2010-1-1"), le = list(date = "2010-1-2"))
  expect_identical(colnames(df)[c(1, 8, 14)], c("payor_id", "payee_bic", "type"))
  expect_equal(nrow(df), 10)
  expect_identical(df$reference[5], "Thanks, Aldi")
  
  # transact (mult. join) with mult. eq, and le
  df <- Select("transactions", db, le = list(payor_id = 2), eq = list(type = c("food", "purchase")))
  expect_equal(nrow(df), 506)
  
  # personalAccounts (join)
  df <- Select("personalAccounts", db)
  expect_identical(df$account_owner[2], "Tressa Denham")
  
  # wrong table
  expect_error(Select("test", db))
  
  # wrong column
  expect_error(Select("accounts", eq = list(payor_id = 2)))
  
  # select a BLOB
  x <- list(a = 1:5, b = list(c = c("a", "b")))
  expect_true(InsertBLOB("test2", x, db))
  x2 <- SelectBLOB("test2", db)
  expect_identical(x, x2)
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