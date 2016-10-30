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
