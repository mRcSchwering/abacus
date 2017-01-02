library(abacus)

context("Various procedures with test database")

set.seed(42)
db <- "test.db"
Create_testDB(db)

test_that("Predictor Update", {
  params <- list(nFeats = 200, DDL = FALSE, time = list(start = as.Date("2010-1-1"), end = as.Date("2011-1-1")))
  expect_error(Update_Predictor(db))
  expect_true(InsertBLOB("Params", params, db))
  expect_warning(Update_Predictor(db))
  feats <- SelectBLOB("FeatureList", db)
  expect_equal(nrow(feats), 200)
  expect_equal(feats$value[1:4] == c("debit", "transaction", "mod50", "mod100"), c(T,T,T,T))
  params <- list(nFeats = 100, DDL = TRUE, time = list(start = as.Date("2010-1-1"), end = as.Date("2011-1-1")))
  expect_true(UpdateBLOB("Params", params, db))
  expect_true(Update_Predictor(db))
  feats <- SelectBLOB("FeatureList", db)
  expect_equal(nrow(feats), 100)
  params <- list(nFeats = 100, DDL = TRUE, time = list(start = as.Date("2010-1-1"), end = as.Date("2010-1-1")))
  expect_true(UpdateBLOB("Params", params, db))
  expect_true(Update_Predictor(db))
  model <- SelectBLOB("Model", db)
  expect_equal(length(model$freqs), 5)
  params <- list(nFeats = 200, DDL = FALSE, time = list(year = 2))
  expect_true(UpdateBLOB("Params", params, db))
  expect_identical(Update_Predictor(db), "No transactions in database for the provided time intervall")
})


test_that("Predictor Evaluation", {
  params <- list(nFeats = 100, DDL = TRUE, time = list(start = as.Date("2010-1-1"), end = as.Date("2010-1-1")))
  expect_true(UpdateBLOB("Params", params, db))
  expect_error(Evaluate_Predictor(db)) # to few data points per nfold
  params <- list(nFeats = 100, DDL = FALSE, time = list(start = as.Date("2010-1-1"), end = as.Date("2010-2-1")))
  expect_true(UpdateBLOB("Params", params, db))
  expect_warning(Evaluate_Predictor(db))
  ranks <- SelectBLOB("Ranking", db)
  err <- SelectBLOB("Err", db)
  expect_lte(nrow(ranks), 100)
  expect_lte(nrow(err), 100)
  expect_gte(sum(err$class == err$prediction) / 92, 0.9)
  expect_identical(attr(ranks, "dimnames")[[2]][1:2], c("idx", "score"))
})


test_that("Update database with new transactions form file", {
  
  # read file and extract info
  f <- system.file("extdata", "test_transactions.csv", package = "abacus")
  cols <- list(name = 6, iban = 7, bic = 8, date = 3, reference = 5, entry = 4, value = 9, currency = 10)
  expect_error(Read_csv("giro1", f, cols, db)) # no such type in personalAccounts
  tas <- Read_csv("giro", f, cols, db)
  expect_identical(names(tas), c("Transactions", "Reference", "db"))
  expect_equal(nrow(tas$Transactions), 15)
  expect_equal(nrow(tas$Reference), 1)
  tas <- Read(tas)
  expect_true("NewAccounts" %in% names(tas))
  expect_equal(nrow(tas$NewAccounts), 3)
  
  # Predict function
  expect_error(Predict(tas)) # cause last owner name has "."
  tas$NewAccounts$owner[3] <- "New Owner 3"
  pred <- Predict(tas)
  expect_true("Prediction" %in% names(pred))
  expect_true("predicted type" %in% names(pred$Prediction))
  
  # Duplicated function
  expect_error(Duplicated(tas)) # doesnt have a prediction dataframe yet
  
  # No ta in db occuring
  res <- Duplicated(pred)
  expect_equal(sum(res$Duplicated$bool), 1)
  expect_equal(sum(res$Duplicated$type == c(rep("", 14), "savings withdrawal")), 15)
  
  # all of 3 tas in db occuring
  a <- Select("transactions", db, eq = list(date = "2010-12-30"))
  names(a)[14] <- "predicted type"
  tas_a <- tas
  tas_a$Prediction <- a
  res <- Duplicated(tas_a)
  expect_false(any(!res$Duplicated$bool))
  expect_equal(sum(res$Duplicated$type == c("savings", "cash withdrawal", "cash withdrawal")), 3)
  
  # 1 ta in db occuring
  a <- Select("transactions", db, eq = list(date = "2010-12-29"))
  names(a)[14] <- "predicted type"
  tas_a <- tas
  tas_a$Prediction <- a
  res <- Duplicated(tas_a)
  expect_true(res$Duplicated$bool)
  expect_true(res$Duplicated$type == "purchase")
  
  # Enter function
  pred <- Duplicated(pred)
  expect_true(Enter(pred))
  res <- Select("transactions", db, ge = list(date = "2011-1-1"))
  expect_equal(nrow(res), 15)
  expect_true(res$type[15] == "savings withdrawal") # last element was not entered
  
  # enter duplicates again
  expect_true(Enter(pred))
  res <- Select("transactions", db, ge = list(date = "2011-1-1"))
  expect_equal(nrow(res), 29)
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