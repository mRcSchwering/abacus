
library(abacus)

# create test.db
Create_testDB("./db")

# test function insert
df <- data.frame(asd = "Harry G", asdf = "ASDF1234", asdfg = "ASD1234", type = "test")
Insert(df, "accounts", "./db/test.db", add_id = TRUE)
df <- data.frame(payor = 1, payee = 20, date = "2010-1-1", reference = "test", entry = "test", value = 1111, currency = "ASD")
Insert(df, "transactions", "./db/test.db")
df$payor <- 200
Insert(df, "transactions", "./db/test.db") # should fail because of foreign key rules

# remove test.db
file.remove("./db/test.db")






dbGetPreparedQuery(con, "INSERT INTO accounts VALUES(NULL, ?, ?, ?, ?)", df)

df <- data.frame(owner = "Tulla", iban = "ASDF1234", bic = "ASD1234", type = "test")

cols <- paste0("@", names(df), collapse = ", ")
if( add_id ) cols <- paste("NULL", cols, sep = ", ")

dbGetPreparedQuery(con, sprintf("INSERT INTO accounts VALUES (%s)", cols), df)



query <- paste("NULL, @owner, @iban, @bic, @type")
dbGetPreparedQuery(con, "INSERT INTO accounts VALUES (NULL, @owner, @iban, @bic, @type)", df)

# sprintf für SELECT
tab <- "accounts"
base <- 100
df <- dbGetQuery(con, sprintf("SELECT * FROM %s WHERE id > %s", tab, base))

# BLOBs : https://github.com/josephw/RSQLite/blob/master/inst/UnitTests/blob_test.R




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
