library(abacus)

context("Machine learning functions")


test_that("Feature Extraction", {
  df <- data.frame(
    payor_id = 1:4, payor_owner = rep("a\\s[] d:.", 4), payor_iban = c("D\\E000342", "ENasd/;:.,", "es1234", "_d-#üäeö"), payor_bic = rep("as\\Df_`\\[]{}()"),
    payee_id = 4:1, payee_owner = rep("a\\s[] d:.", 4), payee_iban = c("DE000342", "EN\\[]{}/()", "es1234", "d_e1~*#!$%"), payee_bic = rep("asDf&^°|<>"),
    date = c("2010-1-1", "2010-01-01", "1990-1-1", "1990-01-01"),
    reference = c("as_d üäö:,|as\\d_%&/´", "t_e;üsö:,|t\\_%$§/´", "test test", "asd"),
    entry = c("as#'d üäö:^°as\\d_%&/´", "t_e(;))üsö:,|t\\_%$[§]/´", "test te{s}\\t", "asd"),
    value = c("500", "10000", "50219", "0")
  )
  df2 <- data.frame(account_id = 1)
  exp <- cbind(c(1,0,0,1), c(0,1,0,0), c(0,0,1,0))
  exp <- cbind(exp, exp)
  exp <- cbind(exp, matrix(1,4,4), c(1,0,0,0), c(0,0,0,1), c(1,0,0,1), c(0,1,1,0), c(1,0,0,1), c(0,1,1,0), 
               c(1,1,0,1), c(0,1,0,1), c(0,1,0,1), c(0,1,1,0))
  res <- FeatureExtraction(df, df2)
  feats <- res$FeatureList
  abt <- matrix(res$ABT, 4, 20)
  expect_equal(as.character(unique(feats$value[grepl("country", feats$name)])), c("de", "en", "es"))
  expect_equal(as.character(unique(feats$value[grepl("owner", feats$name)])), "as d")
  expect_equal(as.character(unique(feats$value[grepl("bank", feats$name)])), "asdf")
  expect_equal(as.character(unique(feats$value[grepl("reference", feats$name)])), c("asd", "test"))
  expect_equal(as.character(unique(feats$value[grepl("entry", feats$name)])), c("asd", "test"))
  expect_equal(exp, abt)
})



test_that("ABT Conversion", {
  abt1 <- matrix(1:60, 10, 6)
  feats1 <- data.frame(name = rep(letters[1:3], each = 2), value = letters[1:6])
  feats2 <- data.frame(name =c("a", "b", "b", "b", "c", "c", "a", "d", "d"),
                       value = c("b", "c", "d", "a", "a", "f", "a", "a", "b"))
  abt2 <- cbind(11:20, 21:30, 31:40, rep(0,10), rep(0,10), 51:60, 1:10, rep(0,10), rep(0,10))
  expect_error(Convert(abt1, feats2, feats1))
  res <- Convert(abt1, feats1, feats2)
  expect_equal(res, abt2)
  res <- Convert(abt2, feats2, feats1)
  abt1[, 5] <- 0
  expect_equal(abt1, res)
})


# expect_equal # mit num ungenauigkeit
# expect_identical # identisch
# expect_match # string mathing
# expect_output # print output matchin

# expect_error / warning / message
#expect_error(1 + 1)


# epxectations = unterte ebene (element von testing)
# test = nächste Ebene Funktionalität testen
# conext/ file = obere Ebene, was wird allg getestet (Funktion oder so)