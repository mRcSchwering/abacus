
#### create accounts
names <- read.csv("CSV_Database_of_First_Names.csv", stringsAsFactors = FALSE)
surnames <- read.csv("CSV_Database_of_Last_Names.csv", stringsAsFactors = FALSE)
IBANcountry <- c("DE", "AT", "CH", "EG", "BE", "FR", "GR", "IE", "IT", "NO", "SE", "ES", "GB", "AE")
n <- 100
set.seed(42)
konten <- data.frame(
  owner = paste(sample(names$firstname, n, replace = TRUE), sample(surnames$lastname, 10, replace = TRUE)),
  iban = "", bic = "", type = "food",
stringsAsFactors = FALSE)
konten$type[1] <- "giro"
konten$type[2] <- "savings"
konten$type[3:13] <- "cash"
konten$type[14] <- "insurance"
konten$type[15] <- "rent"
konten$type[16:46] <- "purchase"
konten$type[47:57] <- "health"
konten$type[58:68] <- "cloth"
konten$iban <- sapply(konten$iban, function(x) paste(c(sample(IBANcountry, 1), sample(c(0, 0, 0:9), sample(16:32, 1), replace = TRUE)), collapse = ""))
konten$ bic <- sapply(konten$bic, function(x) paste(c(sample(IBANcountry, 1), sample(c(toupper(letters), 0:9), sample(6:9, 1), replace = TRUE)), collapse = ""))


#### personal accounts
persKonten <- data.frame(account = 1:2, type = c("giro", "savings"), stringsAsFactors = FALSE)


#### create transactions
set.seed(42)
trans <- data.frame(
  payor = NA, payee = NA,
  date = as.character(sort(sample(seq(as.Date("2010-1-1"), as.Date("2011-1-1"), by = "day"), 1000, replace = TRUE))),
  reference = "", entry = "", value = NA, currency = "EUR",
  type = sample(c(rep("cash withdrawal", 20), rep("food", 20), rep("purchase", 14), rep("health/medicine", 2), rep("clothing", 4), "savings withdrawal", rep("savings", 3)), 1000, replace = TRUE),
stringsAsFactors = FALSE)
trans$payor[1] <- 1
trans$payee[1] <- 15
trans$reference[1] <- "Rent Rohrbacher 21"
trans$entry[1] <- "transaction"
trans$value[1] <- 80000
trans$type[1] <- "rent"
trans$payor[2] <- 1
trans$payee[2] <- 14
trans$reference[2] <- "HanseMerkur dues"
trans$entry[2] <- "transaction"
trans$value[2] <- 20000
trans$type[2] <- "insurance"
for( i in 2:(nrow(trans)-1) ){
  if( as.POSIXlt(as.Date(trans$date[i]))$mday == 1 && as.POSIXlt(as.Date(trans$date[i-1]))$mday > 1 ){
    trans$payor[i] <- 1
    trans$payee[i] <- 15
    trans$reference[i] <- "Rent Rohrbacher 21"
    trans$entry[i] <- "transaction"
    trans$value[i] <- 80000
    trans$type[i] <- "rent"
    trans$payor[i+1] <- 1
    trans$payee[i+1] <- 14
    trans$reference[i+1] <- "HanseMerkur dues"
    trans$entry[i+1] <- "transaction"
    trans$value[i+1] <- 20000
    trans$type[i+1] <- "insurance"
  }
}
for( i in 1:nrow(trans) ){
  if( trans$type[i] == "cash withdrawal" ){
    trans$payor[i] <- 1
    trans$payee[i] <- sample(which(konten$type == "cash"), 1)
    trans$reference[i] <- sample(c("EC 97749000DE992", "credit 83900021AF0029"), 1)
    trans$entry[i] <- sample(c("cashpoint", "teller", "auto teller", "cash terminal"), 1)
    trans$value[i] <- sample(c(20, 40, 60), 1)
  } else if( trans$type[i] == "food" ){
    trans$payor[i] <- 1
    trans$payee[i] <- sample(which(konten$type == "food"), 1)
    trans$reference[i] <- sample(c("Thanks, Aldi", "Rewe says thanks", "REWE", "Penny"), 1)
    trans$entry[i] <- sample(c("debit", "debit withdrawal", "debit entry", "debit note"), 1)
    trans$value[i] <- rnorm(1,25, 5)
  } else if( trans$type[i] == "purchase" ){
    trans$payor[i] <- 1
    trans$payee[i] <- sample(which(konten$type == "purchase"), 1)
    trans$reference[i] <- sample(c("Thanks, Kaufland", "Kaufland says thanks", "MUELLER", "Depot"), 1)
    trans$entry[i] <- sample(c("debit", "debit withdrawal", "debit entry", "debit note"), 1)
    trans$value[i] <- rnorm(1,35, 5)
  } else if( trans$type[i] == "health/medicine" ){
    trans$payor[i] <- 1
    trans$payee[i] <- sample(which(konten$type == "health"), 1)
    trans$reference[i] <- sample(c("Star Pharmacy", "StLouis Hospital", "Cross Pharm"), 1)
    trans$entry[i] <- sample(c("debit", "debit withdrawal", "debit entry", "debit note"), 1)
    trans$value[i] <- rnorm(1,45, 5)
  } else if( trans$type[i] == "clothing" ){
    trans$payor[i] <- 1
    trans$payee[i] <- sample(which(konten$type == "cloth"), 1)
    trans$reference[i] <- sample(c("H+M", "MarcOPolo", "HM"), 1)
    trans$entry[i] <- sample(c("debit", "debit withdrawal", "debit entry", "debit note"), 1)
    trans$value[i] <- rnorm(1,55, 5)
  } else if( trans$type[i] == "savings withdrawal" ){
    trans$payor[i] <- 2
    trans$payee[i] <- 1
    trans$reference[i] <- sample(c("vacation", "migrate", "backup"), 1)
    trans$entry[i] <- "transaction"
    trans$value[i] <- sample(c(200, 500, 1000), 1)
  } else if( trans$type[i] == "savings" ){
    trans$payor[i] <- 1
    trans$payee[i] <- 2
    trans$reference[i] <- sample(c("savings", "invest", "for depot", "for daily"), 1)
    trans$entry[i] <- "transaction"
    trans$value[i] <- sample(c(500, 1000), 1)
  }
}


#### save
accounts <- konten
accounts$type <- NULL
transactions <- trans
personalAccounts <- persKonten

save(accounts, file = "accounts.rda")
save(transactions, file = "transactions.rda")
save(personalAccounts, file = "personalAccounts.rda")
rm(list = ls())





