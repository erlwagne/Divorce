## Divorce ##

## Read data
divorce <- read.csv(file.path("~","R","Divorce","data","divorce1.csv"), fileEncoding = "UTF-8-BOM")
divorce1 <- read.csv(file.path("~","R","Divorce","data","divorce2.csv"), fileEncoding = "UTF-8-BOM")

library(dplyr)
library(rpart)
library(rpart.plot)
library(tree)
library(party)
set.seed(100)

## Give birds unique ordered id ##
# Males #
maleid <- transform(divorce, mid=as.numeric(factor(BandM))) 
maleid2 <- transform(divorce, mid=match(BandM, unique(BandM))) 
all.equal(maleid, maleid2)
maleid2 # maleid is new last column

# Females #
femaleid <- transform(divorce, fid=as.numeric(factor(BandF)))
femaleid2 <- transform(divorce, fid=match(BandF, unique(BandF))) 
all.equal(femaleid, femaleid2)
femaleid2

## new data sheet to working directory
write.csv(femaleid, "divorce1.csv")

## Some preliminary feeling out...

div.test <- rpart(together~nYrsMated + nFledged + RS, data = divorce)
printcp(div.test)
print(div.test)
summary(div.test)
plot(div.test)
text(div.test)

rtree_fit <-  rpart(together~nYrsMated + nFledged + RS, data = divorce)
rpart.plot(rtree_fit)
