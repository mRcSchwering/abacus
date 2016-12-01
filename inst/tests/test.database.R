library(abacus)
library(RSQLite)






context("Creating, writing and reading the test database")

db <- "test.db"


test_that("Create_testDB()", {
  expect_true(Create_testDB(db))
})


test_that("test.db contents", {
  con <- dbConnect(SQLite(), dbname = db)
  df <- dbGetQuery(con,  "SELECT * FROM sqlite_master")
  dbDisconnect(con)
  expect_equal(df$type, c("table", "index", "table", "index", "index", "table", "index", "table", "index", "table", "table", "index"))
  expect_equal(df$name, c("accounts", "uq_accounts", "transactions", "transact_payor_index", "transact_payee_index", "capital", 
                          "capital_index", "personalAccounts", "personalAccounts_index", "cashflow", "storage", "sqlite_autoindex_storage_1"))
  expect_equal(df$rootpage, c(2, 3, 4, 5, 6, 7, 9, 11, 12, 13, 14, 15))
})


test_that("write db", {
  
  # with add_id
  df <- data.frame(owner = "Harry G", iban = "ASDFasd234", bic = "ASD1234", stringsAsFactors = FALSE)
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
  expect_error(Insert(data.frame(owner = "Harry G", iban = "ASDF1234", bic = "ASD1234"), "accounts", add_id = TRUE))
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

test_that("updating db", {
  
  # several updates
  df <- data.frame(id = 3:4, owner = rep("test",2), iban = c("tst1", "tst2"), bic = c("tst1", "tst2"), stringsAsFactors = FALSE)
  Update(df, "accounts", "id", db)
  df2 <- Select("accounts", db, eq = list(id = c(3,4)))
  expect_identical(df, df2)
  
  # with date
  df <- data.frame(payor = 1, payee = 1, date = "2010-1-29", reference = "test", entry = "test", value = 0, currency = "TST", type = "test")
  Update(df, "transactions", "date", db)
  df2 <- Select("transactions", db, eq = list(date = "2010-1-29"))
  expect_equal(df2$value, df$value)
  
  # Update BLOB
  d <- list(test = "some test")
  InsertBLOB("test3", list(a = 1:10, b = list(a = letters)), db)
  UpdateBLOB("test3", d, db)
  d2 <- SelectBLOB("test3", db)
  expect_identical(d, d2)
})


test_that("finding dublicate entries in db", {
  
  # for accounts
  df <- data.frame(owner = c("O1", "O2", "O3"), iban = c("AAA", "BBB", "CCC"), bic = c("AAA", "BBB", "CCC")) 
  expect_true(Insert(df, "accounts", db, add_id = TRUE))
  df2 <- rbind(df, df)
  df2$iban[c(1,2)] <- df2$iban[c(2,1)]
  df2$bic[6] <- df2$iban[5]
  res <- Intersect(df2, "accounts", db)
  expect_identical(res, c(F,F,T,T,T,F))
  
  # for storage
  expect_true(InsertBLOB("t1", "test", db))
  expect_true(InsertBLOB("t2", "test", db))
  res <- Intersect(data.frame(name = c("t3", "t2", "t1", "t0")), "storage", db)
  expect_identical(res, c(F,T,T,F))
})





file.remove(db)


