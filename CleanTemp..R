#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly=TRUE)
args
inputPath <- args[1]
inputPath
outputPath <- args[2]
library(caret)

df <- read.csv(inputPath, header = TRUE, sep = ",", check.names=FALSE, fileEncoding = "latin1")
print('Original')
print(ncol(df))

# Columns with more than 20% missing values
a <- (colSums(is.na(df))/nrow(df) > 0.2)
a1 <- a[a==TRUE]
a1.names <- names(a1)
df <- df[ , !names(df) %in% a1.names]
a <- (colSums(df=='')/nrow(df) > 0.2)
a1 <- a[a==TRUE]
a1.names <- names(a1)
df <- df[ , !names(df) %in% a1.names]
print('Omitting more than 20% missing result')
print(ncol(df))

nzv1 <- nearZeroVar(df, saveMetrics= TRUE)
a<-row.names(nzv1)[nzv1$nzv == TRUE]
df<-df[, !names(df) %in% a]
print('Omitting nzv result')
print(ncol(df))

df.numeric <- data.matrix(df)
df.colin <- cor(df.numeric, use = "pairwise.complete.obs")
CorPath <- capture.output(cat(substr(outputPath,1,nchar(outputPath)-4),'_CorMat.csv',sep = ""))
write.csv(df.colin, CorPath, row.names=FALSE, na="")
hc = findCorrelation(df.colin[, -c(1,2)], cutoff = 0.75, names = TRUE)
df <- df[ , !names(df) %in% hc]
print('Omitting dependent result')
print(ncol(df))

write.csv(df, outputPath, row.names=FALSE, na="")
 