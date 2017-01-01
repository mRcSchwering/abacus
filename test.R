
library(testthat)
test_dir("./inst/tests")
test_dir(system.file("tests", "", package = "abacus"))






library(abacus)

#### app
db <- "test.db"
library(abacus)
Create_testDB("test.db")
library(shiny)



##

roxygen2::roxygenise()
devtools::install()


##










































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
