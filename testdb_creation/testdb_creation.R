
#### create accounts
names <- read.csv("CSV_Database_of_First_Names.csv", stringsAsFactors = FALSE)
surnames <- read.csv("CSV_Database_of_Last_Names.csv", stringsAsFactors = FALSE)
IBANcountry <- c("DE", "AT", "CH", "EG", "BE", "FR", "GR", "IE", "IT", "NO", "SE", "ES", "GB", "AE")

n <- 100
konten <- data.frame(
  owner = paste(sample(names$firstname, n, replace = TRUE), sample(surnames$lastname, 10, replace = TRUE)),
  iban = "", bic = "", type = "bank account",
stringsAsFactors = FALSE)
konten$type[1] <- "giro"
konten$type[2] <- "savings"
konten$type[3:13] <- "cash"

set.seed(42)
konten$iban <- sapply(konten$iban, function(x) paste(c(sample(IBANcountry, 1), sample(c(0, 0, 0:9), sample(16:32, 1), replace = TRUE)), collapse = ""))
konten$ bic <- sapply(konten$bic, function(x) paste(c(sample(IBANcountry, 1), sample(c(toupper(letters), 0:9), sample(6:9, 1), replace = TRUE)), collapse = ""))





#### create transactions
set.seed(42)
trans <- data.frame(
  payor = NA, payee = NA,
  date = sort(sample(seq(as.Date("2010-1-1"), as.Date("2011-1-1"), by = "day"), 1000, replace = TRUE)),
  reference = "", entry = "", value = NA, currency = "EUR",
  label = sample(c(rep("cash", 20), rep("random", 40), "savings in", rep("savings out", 3)), 1000, replace = TRUE),
stringsAsFactors = FALSE)
trans$date <- as.character(trans$date)

# random in
s <- TRUE
for( i in 1:nrow(trans) ){
  if( s && as.POSIXlt(trans$date[i])$mday == 1 ) trans$label[i] <- "random in"
  s <- switch(as.character(as.POSIXlt(trans$date[i])$mday), "1" = FALSE, TRUE)
}
idx <- which(trans$label == "random in")
trans$payor[idx] <- 14
trans$payee[idx] <- 1
trans$value[idx] <- 200000L
trans$reference[idx] <- "salary"
trans$entry[idx] <- "booking"

# random out
idx <- which(trans$label == "random")
trans$payor[idx] <- 1
trans$payee[idx] <- sample(15:100, length(idx), replace = TRUE)
trans$value[idx] <- rpois(length(idx), 2000)
trans$reference[idx] <- sample(apply(expand.grid(c("order", "booking", "purchase order"), c("REWE", "Aldi", "Mueller", "Aral", "Media Markt")), 1, function(x) paste(x, collapse = " ")), length(idx), replace = TRUE)
trans$entry[idx] <- sample(c("booking", "withdrawal", "transfer", "transaction"), length(idx), replace = TRUE)

# cash
idx <- which(trans$label == "cash")
trans$payor[idx] <- 1
trans$payee[idx] <- sample(3:13, length(idx), replace = TRUE)
trans$value[idx] <- sample(c(2000, 4000), length(idx), replace = TRUE)
trans$reference[idx] <- sample(c("Cash Point", "cash machine", "autoteller"), length(idx), replace = TRUE)
trans$entry[idx] <- "withdrawal"

# savings in
idx <- which(trans$label == "savings in")
trans$payor[idx] <- 2
trans$payee[idx] <- 1
trans$value[idx] <- sample(c(50000, 100000, 200000), length(idx), replace = TRUE)
trans$reference[idx] <- sample(c("holiday", "withdrawal", "purchase", "migration"), length(idx), replace = TRUE)
trans$entry[idx] <- "transaction"

# savings out
idx <- which(trans$label == "savings out")
trans$payor[idx] <- 1
trans$payee[idx] <- 2
trans$value[idx] <- sample(c(50000, 100000, 200000), length(idx), replace = TRUE)
trans$reference[idx] <- "savings"
trans$entry[idx] <- "transaction"

trans <- trans[, -8]






# save
accounts <- konten
transactions <- trans
save(accounts, file = "accounts.rda")
save(transactions, file = "transactions.rda")
rm(list = ls())






# test enforce rules
test <- data.frame(payor = 200, payee = 2, date = as.Date("2010-1-1"), reference = "test", entry = "asd", value = 10L, currency = "USD")
Insert(test, "transactions", "../db/test.db")
















