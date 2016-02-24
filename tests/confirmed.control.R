if(FALSE){
  ## Das war die alte Beschaffung aus irgendeinem csv von Arne. Aber es nur Testdaten.
  rows <- 10^4
  if (system("hostname", intern = TRUE) == "f060"){
    bwi3 <- read.csv("/trans/p/data/bwi/bwi3/BA_Anteile_TE_BWI3.csv", nrows = rows)
  } else {
    if (system("hostname", intern = TRUE) == "h5") {
      bwi3 <- read.csv("/home/nik/fvadat/bwi3/BA_Anteile_TE_BWI3.csv", nrows = rows)
    } else{
      bwi3 <- read.csv("/home/qwer/fvadat/bwi3/BA_Anteile_TE_BWI3.csv", nrows = rows)
    }
  }
  preds <- c('V_Fi', 'H_Lorey', 'n_PB_WZP4')
  bwi3 <- bwi3[, c("TNr", preds, 'V_ges')]
  preds <- c('x1', 'x2', 'x3')
  names(bwi3) <- c('TNr',preds , 'y')

  bwi3$InForest <- TRUE
  TNrs <- unique(bwi3$TNr)
  set.seed(1234)
  s1 <- sample(TNrs
	       , as.integer(length(TNrs)*0.1)
	       , replace = FALSE)
  set.seed(1234)
  s2 <- sample(s1
	       , as.integer(length(s1)*0.5)
	       , replace = FALSE)
  bwi3$s1 <- bwi3$TNr %in% s1
  bwi3$s2 <- bwi3$TNr %in% s2
  set.seed(347456)
  g <- sample(TNrs
	      , as.integer(length(TNrs)*.5)
	      , replace = FALSE)
  set.seed(3457213)
  g1 <- sample(g
	       , as.integer(length(g)*.5)
	       , replace = FALSE)
  bwi3$g <- ifelse(bwi3$TNr %in% g1 ,'a',
		   ifelse(bwi3$TNr %in% g ,'b', NA))


  bwi <- bwi3
  write.csv(bwi, file="/tmp/dummy.data.csv", row.names=FALSE); rm (bwi3);bwi3 <- read.csv(file="/tmp/dummy.data.csv", stringsAsFactors=FALSE)
  write.table(bwi3, "confirmed.Data.txt")
}

preds <- c('x1', 'x2', 'x3')
bwi3 <- read.table("confirmed.Data.txt", stringsAsFactors = TRUE)
sbwi3 <-  bwi3[bwi3$s2,]
library(data.table)
foo <- data.table( bwi3)
foo[foo$s1 == FALSE, x1 := NA]

bwi3tp <- as.data.frame(foo)

set.seed(1234)
trueMeans <- as.data.frame(
			   rbind(
				 colMeans(subset(bwi3, g =='a')[, preds], na.rm =TRUE)+ rnorm(length(preds))
				 , colMeans(subset(bwi3, g =='b')[, preds], na.rm =TRUE)+ rnorm(length(preds))
				 )
			   ); trueMeans$g=c('a', 'b')
partialMeans <- trueMeans[,-2]


source("~/R/mypackages/msae/msae/R/maSAE.R")


sink();sink("confirmed.control.R.out")
cat('Alexander Massey war so freundlich, mir die ungeclusterten Schaetzter nachzurechnen,
er kommt zu denselben Ergebnissen, auch wenn er noch von c1.43 statt von c2... spricht.
Der geclusterte Teil ist nicht nachgerechnet.\n')
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
