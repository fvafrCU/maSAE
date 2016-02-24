#bwi3 <- read.csv(file="/tmp/dummy.data.csv", stringsAsFactors=FALSE)
bwi3 <- read.table("test.Data.txt")
sbwi3 <-  bwi3[bwi3$s2,]
library(data.table)
foo <- data.table( bwi3)
foo[foo$s1 == FALSE, x1 := NA]

bwi3tp <- as.data.frame(foo)
preds <- paste('x',1:3, sep='')

set.seed(1234)
trueMeans <- as.data.frame(
			   rbind(
				 colMeans(subset(bwi3, g =='a')[, preds], na.rm =TRUE)+ rnorm(length(preds))
				 , colMeans(subset(bwi3, g =='b')[, preds], na.rm =TRUE)+ rnorm(length(preds))
				 )
			   ); trueMeans$g=c('a', 'b')
partialMeans <- trueMeans[,-2]


source("maSAE.R")

sink();sink("dummy.r.out")
cat("### not clustered:\n")
cat("## Naive Designschaetzung:\n")
print(row.names = FALSE, predict(saObj(data = subset(bwi3, s2 == TRUE), f = y ~NULL | g)))
cat("## eSRE nach Mandallaz:\n")
########## simple sampling
## non exhaustive
nonex <- saObj(data = bwi3, f = y ~x1 + x2 + x3 | g, s2 = 's2')
m.nonex <- predict(nonex)
## three-phase sampling
threePhase <- saObj(data = bwi3tp, f = y ~x1 + x2 + x3 | g, s2 = 's2', s1 = 's1')
m.threePhase <- predict(threePhase)
## partially exhaustive
part <- saObj(data = bwi3, f = y ~x1 + x2 + x3 | g, s2 = 's2', smallAreaMeans = partialMeans)
m.part <- predict(part)
## exhaustive
ex <- saObj(data = sbwi3, f = y ~x1 + x2 + x3 | g, smallAreaMeans = trueMeans)
m.ex <- predict(ex)
cat('exhaustive:\n' )        ; print(m.ex, row.names = FALSE)
cat('partially exhaustive:\n'); print(m.part, row.names = FALSE)
cat('three-phase:\n')         ; print(m.threePhase, row.names = FALSE)
cat('non-exhaustive:\n')      ; print(m.nonex, row.names = FALSE)
cat("### clustered:\n")
cat("## Naive Designschaetzung:\n")
print(row.names = FALSE, predict(saObj(data = subset(bwi3, s2 == TRUE), include = 'InForest',  f = y ~NULL | g, cluster = 'TNr')))
cat("## eSRE nach Mandallaz:\n")
########## cluster sampling
## non exhaustive
nonex.cl <- saObj(data = bwi3, include = 'InForest',  f = y ~x1 + x2 + x3 | g, s2 = 's2', cluster = 'TNr')
m.nonex.cl <- predict(nonex.cl)
## three-phase
threePhase.cl <- saObj(data = bwi3tp, include = 'InForest',  f = y ~x1 + x2 + x3 | g, s2 = 's2', s1 = 's1', cluster = 'TNr')
m.threePhase.cl <- predict(threePhase.cl)
## partially exhaustive
part.cl <- saObj(data = bwi3, include = 'InForest',  f = y ~x1 + x2 + x3 | g, s2 = 's2', cluster = 'TNr', smallAreaMeans = partialMeans)
m.part.cl <-predict(part.cl)
## exhaustive
ex.cl <- saObj(data = bwi3, include = 'InForest',  f = y ~x1 + x2 + x3 | g, s2 = 's2', cluster = 'TNr', smallAreaMeans = trueMeans)
m.ex.cl <- predict(ex.cl)
cat('exhaustive:\n')          ;print(m.ex.cl, row.names = FALSE)
cat('partially exhaustive:\n');print(m.part.cl, row.names = FALSE)
cat('three-phase:\n')         ;print(m.threePhase.cl, row.names = FALSE)
cat('non-exhaustive:\n')      ;print(m.nonex.cl, row.names = FALSE)
sink()
