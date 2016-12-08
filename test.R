
library(testthat)
test_dir("./inst/tests")
test_dir(system.file("tests", "", package = "abacus"))










library(abacus)


db <- "test.db"
Create_testDB(db)

params <- list(nFeats = 200, DDL = FALSE, time = list(start = as.Date("2010-1-1"), end = as.Date("2011-1-1")))
InsertBLOB("Params", params, db)




db <- "test.db"
cols <- list(name = 6, iban = 7, bic = 8, date = 3, reference = 5, entry = 4, value = 9, currency = 10)

tas <- Read_csv("giro", "test.csv", cols, db)
DT::datatable(tas$Transactions)
# get Tas with Refand Db in 1 object

tas <- Read(tas)
DT::datatable(tas$NewAccounts)
# get new accounts / possible to change owner

Predict(tas) # error cause last owner has "."
tas$NewAccounts$owner[3] <- "New Owner 3"

df <- Predict(tas)
DT::datatable(df)



Predict.Transactions_ <- function( x )
{
  if(!"NewAccounts" %in% names(x)) stop("Use Read method with Transactions_ object first")
  if(any(grepl("[^0-9A-Za-z ]", x$NewAccounts$owner))) stop("Please don't use special characters in owner names")
  if(any(is.na(x$Transactions$value))) stop("NA's in transaction values")
  
  Insert(x$NewAccounts, "accounts", x$db, add_id = TRUE)
  
  if( nrow(x$NewAccounts) > 0 ){
    for( i in 1:nrow(x$NewAccounts) ){
      idx <- cbind(x$NewAccounts$iban[i] == x$Transactions$iban, x$NewAccounts$bic[i] == x$Transactions$bic)
      idx <- which(apply(idx, 1, function(y) sum(y) > 1))
      x$Transactions$name[idx] <- x$NewAccounts$owner[i] 
    }
  }
  
  l <- lapply(1:nrow(x$Transactions), function(i) {
    r <- x$Transactions[i, ]
    sgn <- r$value > 0
    acc <- Select("accounts", x$db, eq = list(iban = r$iban, bic = r$bic), all_and = TRUE)
    data.frame(
      payor_id = if(sgn) acc$id else x$Reference$account_id,
      payor_owner = if(sgn) acc$owner else x$Reference$account_owner,
      payor_iban = if(sgn) acc$iban else x$Reference$account_iban,
      payor_bic = if(sgn) acc$bic else x$Reference$account_bic,
      payee_id = if(!sgn) acc$id else x$Reference$account_id,
      payee_owner = if(!sgn) acc$owner else x$Reference$account_owner,
      payee_iban = if(!sgn) acc$iban else x$Reference$account_iban,
      payee_bic = if(!sgn) acc$bic else x$Reference$account_bic,
      date = r$date,
      reference = r$reference,
      entry = r$entry,
      value = as.integer(abs(r$value) * 100),
      currency = toupper(r$currency),
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, l)
  
  return(df)
}

Predict <- function (x, ...) {
  UseMethod("Predict", x)
}

Read <- function (x, ...) {
  UseMethod("Read", x)
}

Read.Transactions_ <- function( x )
{
  tas <- x$Transactions
  acc <- tas[!duplicated(tas[, c("iban", "bic")]), c("name", "iban", "bic")]
  acc <- acc[!Intersect(acc, "accounts", x$db), c("name", "iban", "bic")]
  names(acc)[1] <- "owner"
  x[["NewAccounts"]] <- acc
  return(x)
}

Read <- function (x, ...) {
  UseMethod("Read", x)
}

Read_csv <- function( ref, fileName, columns, db, head = TRUE, colSep = "\t", decSep = ",", 
                      quoteChar = "\"", commentChar = "", nSkip = 0, nMax = -1, dateFormat = "%d.%m.%Y" )
{
  stopifnot(length(ref) == 1, length(fileName) == 1, length(db) == 1)
  stopifnot(inherits(ref, "character"), inherits(fileName, "character"), inherits(db, "character"))
  stopifnot(inherits(columns, "list"), names(columns) == c("name", "iban", "bic", "date", "reference", "entry", "value", "currency"))
  
  bool <- Intersect(data.frame(type = ref), "personalAccounts", db)
  if( !identical(TRUE, bool) ) stop("There is no personal account with type ", ref)
  ref <- Select("personalAccounts", db, eq = list(type = ref))
    
  csv <- read.csv(fileName, header = head, sep = colSep, quote = quoteChar, dec = decSep, 
                  comment.char = commentChar, skip = nSkip, nrows = nMax, stringsAsFactors = FALSE)
  df <- as.data.frame(do.call(cbind, lapply(cols, function(x) csv[, x])), stringsAsFactors = FALSE)
  df <- df[!apply(df, 1, function(x) any(is.null(x)) || any(is.na(x)) || any(x == "")), ]
  df$value <- as.numeric(df$value)
  df$date <- as.Date(df$date, dateFormat)
  df$iban <- toupper(df$iban)
  df$bic <- toupper(df$bic)
  
  out <- list(Transactions = df, Reference = ref, db = db)
  class(out) <- "Transactions_"
  return(out)
}











Evaluate_Predictor(db)

err <- SelectBLOB("Err", db)
ranks <- SelectBLOB("Ranking", db)
sum(err$class == err$prediction)











# create test.db
Create_testDB("./db")


tas <- Select("transactions", "db/test.db")
pas <- Select("personalAccounts", "db/test.db")
labs <- tas$type

res <- FeatureExtraction(tas, pas)
abt <- res$ABT
feats <- res$FeatureList

res <- Train(abt, labs, feats)
model <- res$Model
ranks <- res$Ranking

plot(ranks)
summary(ranks)
summary(model)

new <- tas[-900:-1,]
res <- FeatureExtraction(new, pas)
abt2 <- res$ABT
feats2 <- res$FeatureList

pred <- Predict(model, abt2, feats2, feats)
summary(pred)
sum(pred$class == new$type) / length(pred$class) * 100



CV
sum(err$class == err$prediction) / nrow(err) * 100










devtools::install()
roxygen2::roxygenise()





























###### ABT
# 100x11
# 1 cont feat
# 7 discr feat (counts), 4 of which sparse
# 2 binäre
# 5 classes
abt <- matrix(NA, 100, 11)
abt[, 11] <- rep(1:5, each = 20)
## random
rs <- 1:100
abt[rs, 1] <- sample(0:1000, length(rs), replace = TRUE)
abt[rs, 2:4] <- matrix(sample(1:10, length(rs) * 3, replace = TRUE), length(rs), 3)
abt[rs, 5:8] <- matrix(sample(c(rep(0, 9, replace = TRUE), 1:5), length(rs) * 4, replace = TRUE), length(rs), 4)
abt[rs, 9:10] <- matrix(sample(c(rep(0, 9, replace = TRUE), 1), length(rs) * 2, replace = TRUE), length(rs), 2)
## class 1: 1 > 1000
rs <- 1:20
abt[rs, 1] <- sample(1001:10000, length(rs), replace = TRUE)
## class 2: 3 > 5 and 9 = 1 
rs <- 21:40
abt[rs, 3] <- matrix(sample(5:10, length(rs) * 1, replace = TRUE), length(rs), 1)
abt[rs, 9] <- 1
## class 3: 2 as id: 2 = 4
rs <- 41:60
abt[rs, 2] <- 4
## class 4: 9:10 = 1 and 6 > 2
rs <- 61:80
abt[rs, 6] <- matrix(sample(3:5, length(rs) * 1, replace = TRUE), length(rs), 1)
abt[rs, 9:10] <- 1
## class 5: 2 as id: 2 = c(1, 5), 8 > 2
rs <- 81:100
abt[rs, 2] <- matrix(sample(c(1, 5), length(rs) * 1, replace = TRUE), length(rs), 1)
abt[rs, 8] <- matrix(sample(3:5, length(rs) * 1, replace = TRUE), length(rs), 1)



###### Training:


#### sda

library(sda)

ranked <- sda.ranking(abt, as.factor(ta$type))
plot(ranked)
trained <- sda(abt, as.factor(ta$type))
predicted <- predict(trained, abt)
strng <- 0.0 # beste 0.0 mit 0.85
post <- apply(predicted$posterior, 1, max)
sum(predicted$class[post > strng] == as.factor(ta$type)[post > strng])

plot(1:nrow(ranked), log(ranked[, 2]))
