
library(abacus)

#### create accounts
names <- read.csv("CSV_Database_of_First_Names.csv", stringsAsFactors = FALSE)
surnames <- read.csv("CSV_Database_of_Last_Names.csv", stringsAsFactors = FALSE)
IBANcountry <- c("DE", "AT", "CH", "EG", "BE", "FR", "GR", "IE", "IT", "NO", "SE", "ES", "GB", "AE")

n <- 100
konten <- data.frame(
  owner = paste(sample(names$firstname, n, replace = TRUE), sample(surnames$lastname, 10, replace = TRUE)),
  iban = "", bic = "", type = "bank account",
stringsAsFactors = FALSE)

set.seed(42)
konten$iban <- sapply(konten$iban, function(x) paste(c(sample(IBANcountry, 1), sample(c(0, 0, 0:9), sample(16:32, 1), replace = TRUE)), collapse = ""))
konten$ bic <- sapply(konten$bic, function(x) paste(c(sample(IBANcountry, 1), sample(c(toupper(letters), 0:9), sample(6:9, 1), replace = TRUE)), collapse = ""))

Insert(konten, "accounts", "db/mydb.db")

#### create transactions










library(RSQLite)

dbName <- "db/mydb.db"


# connect
con <- dbConnect(SQLite(), dbname = dbName)

# enforce foreign keys
dbSendQuery(con, "PRAGMA foreign_keys = ON;")

# query
query <- ".schema"
query <- "INSERT INTO accounts (id, owner, iban, bic, type) VALUES (NULL, assi2, asdasdf, asdasdf2, test);"
query <- "PRAGMA foreign_keys = ON;"
query <- "SELECT * FROM accounts;"

# sned query
res <- dbSendQuery(con, query)

# fetch and clear results
df <- dbFetch(res)
dbClearResult(res)

# result
df

# disconnect
dbDisconnect(con)








