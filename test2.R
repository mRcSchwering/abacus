
install.packages("rentrez") 
library(rentrez)

entrez_dbs()
res <- entrez_search("gene", term = "100689306")
res$QueryTranslation

entrez_db_searchable("nucleotide")
res <- entrez_search("nucleotide", term = "NW_003613713.1")

xml <- entrez_fetch("nucleotide", id = "NW_003613713.1", rettype = "html", feature.type = "CDS")
fasta<- entrez_fetch("nucleotide", id = "NW_003613713.1", rettype = "fasta")
writeLines(xml, "out.html")
  

cat(substr(xml, 1000, 3000))

l <- XML::xmlToList(xml)
str(xml)
?entrez_fetch

res$file
res$ids

str(res)



library(biomaRt)
ensembl <- useMart("ensembl", host = "www.pre.ensembl.org")



listEnsembl()
source("http://bioconductor.org/biocLite.R")
biocLite("BSgenome")
library(BSgenome)
available.genomes()
