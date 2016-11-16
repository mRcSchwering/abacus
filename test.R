
library(testthat)
test_dir("./inst/tests")




library(abacus)
library(RSQLite)

# create test.db
Create_testDB("./db")





###### Feature Extraction

db <- "db/test.db"
ta <- Select("transactions", db)
pa <- Select("personalAccounts", db)


# country code
x <- sort(unique(strtrim(ta$payor_iban, width = 2)))
payor_country <- lapply(x, function(x) grepl(tolower(x), tolower(ta$payor_iban)))
names(payor_country) <- tolower(x)

x <- sort(unique(strtrim(ta$payee_iban, width = 2)))
payee_country <- lapply(x, function(x) grepl(tolower(x), tolower(ta$payee_iban)))
names(payee_country) <- tolower(x)

# bank code
x <- sort(unique(strtrim(ta$payor_bic, width = 4)))
payor_bank <- lapply(x, function(x) grepl(tolower(x), tolower(ta$payor_bic)))
names(payor_bank) <- tolower(x)

x <- sort(unique(strtrim(ta$payee_bic, width = 4)))
payee_bank <- lapply(x, function(x) grepl(tolower(x), tolower(ta$payee_bic)))
names(payee_bank) <- tolower(x)

# owners
x <- sort(unique(tolower(ta$payor_owner)))
payor_owner <- lapply(x, function(x) grepl(tolower(x), tolower(ta$payor_owner)))
names(payor_owner) <- x

x <- sort(unique(tolower(ta$payee_owner)))
payee_owner <- lapply(x, function(x) grepl(tolower(x), tolower(ta$payee_owner)))
names(payee_owner) <- x

# own accounts
x <- sort(pa$account_id[pa$account_id %in% ta$payor_id])
payor_pa <- lapply(x, function(x) grepl(tolower(x), tolower(ta$payor_id)))
names(payor_pa) <- x

x <- sort(pa$account_id[pa$account_id %in% ta$payee_id])
payee_pa <- lapply(x, function(x) grepl(tolower(x), tolower(ta$payee_id)))
names(payee_pa) <- x

# reference
reference <- sort(gsub("[^a-z ]", "", tolower(ta$reference)))
x <- unlist(strsplit(unique(reference), " "))
reference <- lapply(x, function(x) grepl(tolower(x), reference))
names(reference) <- x

# entry
entry <- sort(gsub("[^a-z ]", "", tolower(ta$entry)))
x <- unlist(strsplit(unique(entry), " "))
entry <- lapply(x, function(x) grepl(tolower(x), entry))
names(entry) <- x

# value
value <- list(mod5 = ta$value %% 500 == 0, mod50 = ta$value %% 5000 == 0, mod100 = ta$value %% 10000 == 0, ge100 = ta$value >= 10000)

# create feature list and abt
feats <- c( "payor_country", "payee_country", "payor_bank", "payee_bank", "payor_owner", "payee_owner", "payor_pa", "payee_pa", 
            "reference", "entry", "value")
featureList <- data.frame(
  name = unlist(lapply( feats, function(x) rep(x, length(get(x))) )), 
  value = unlist(lapply( feats, function(x) names(get(x)) )),
stringsAsFactors = FALSE)
abt <- do.call(cbind, unlist(lapply(feats, get), recursive = FALSE))

abt[100:110, 100:110]

rownames(abt[100:130])


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
