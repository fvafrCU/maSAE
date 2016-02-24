#{##############################################################################
## I cite 6 manuscripts:
##
## a2 Daniel Mandallaz. "Design-based properties of some small-area estimators in 
##    forest inventory with two-phase sampling". 
##    In: Canadian Journal of Forest Research 43.5 (2013), pp. 441-449. 
##    doi: 10.1139/cjfr-2012-0381.
## 
## a1 Daniel Mandallaz. Design-based properties of some small-area estimators in 
##    forest inventory with two-phase sampling. 
##    Tech. rep. Eidgenössische Technische Hochschule Zürich, 
##    Departement Umweltsystemwissenschaften, 2012. 
##    doi: 10.3929/ethz-a-007318974.
## 
## b2 Daniel Mandallaz, Jochen Breschan, and Andreas Hill. "New regression
##    estimators in forest inventories with two-phase sampling and partially
##    exhaustive information: a design-based Monte Carlo approach with applications
##    to small-area estimation". 
##    In: Canadian Journal of Forest Research 43.11 (2013), pp. 1023-1031. 
##    doi: 10.1139/cjfr-2013-0181.
## 
## b1 Daniel Mandallaz. Regression estimators in forest inventories with twophase 
##    sampling and partially exhaustive information with applications to small-area
##    estimation. 
##    Tech. rep. Eidgenössische Technische Hochschule Zürich, 
##    Departement Umweltsystemwissenschaften, 2013. 
##    doi: 10.3929/ethz-a-007623322.
## 
## c2 Daniel Mandallaz. "A three-phase sampling extension of the generalized 
##    regression estimator with partially exhaustive information". 
##    In: Canadian Journal of Forest Research early.online (2013), p. 22. 
##    doi: 10.1139/cjfr-2013-0449.
## 
## c1 Daniel Mandallaz. Regression estimators in forest inventories with threephase
##    sampling and two multivariate components of auxiliary information. 
##    Tech. rep. Eidgenössische Technische Hochschule Zürich, 
##    Departement Umweltsystemwissenschaften, 2013. 
##    doi: 10.3929/ethz-a-009990020.
##
## There's allways to versions for each topic ('a', 'b' and 'c'), 
## (1) a detailed report published via http://e-collection.library.ethz.ch/ and 
## (2) a paper in CFJR.
## I use topicversion to cite them. i.e. a1 is the report on topic a.
## I cite equations from the manuscripts using
## topicversion.equationnumber[.equationpart]
## where 'equationnumber. is the equatoin number from the manuscript
## and for multiline equations 'equationpart' is the line of the equation indexed
## by letters.
## So b2.12.c refers to \hat{\bar{Z}}_sub{k}, the third line of equation 12 in 
## "New regression estimators ..." in CFJR.
###############################################################################}
#' Class \code{"characterOrNULL"}
#' 
#' %% ~~ A concise (1-5 lines) description of what the class is. ~~
#' 
#' 
#' @name characterOrNULL-class
#' @docType class
#' @note %% ~~further notes~~
#' @section Objects from the Class: A virtual Class: No objects may be created
#' from it.
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{~~fun~~}}, ~~~ %% ~~or
#' \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
#' @references %% ~~put references to the literature/web site here~~
#' @keywords classes
#' @examples
#' 
#' showClass("characterOrNULL")
#' 
setClassUnion("characterOrNULL", c('character','NULL'))
#' Class \code{"data.frameOrNULL"}
#' 
#' %% ~~ A concise (1-5 lines) description of what the class is. ~~
#' 
#' 
#' @name data.frameOrNULL-class
#' @docType class
#' @note %% ~~further notes~~
#' @section Objects from the Class: A virtual Class: No objects may be created
#' from it.
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{~~fun~~}}, ~~~ %% ~~or
#' \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
#' @references %% ~~put references to the literature/web site here~~
#' @keywords classes
#' @examples
#' 
#' showClass("data.frameOrNULL")
#' 
setClassUnion("data.frameOrNULL", c('data.frame','NULL'))
#' Class \code{"logicalOrNULL"}
#' 
#' %% ~~ A concise (1-5 lines) description of what the class is. ~~
#' 
#' 
#' @name logicalOrNULL-class
#' @docType class
#' @note %% ~~further notes~~
#' @section Objects from the Class: A virtual Class: No objects may be created
#' from it.
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{~~fun~~}}, ~~~ %% ~~or
#' \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
#' @references %% ~~put references to the literature/web site here~~
#' @keywords classes
#' @examples
#' 
#' showClass("logicalOrNULL")
#' 
setClassUnion("logicalOrNULL", c('logical','NULL'))
#' Class \code{"saObj.virtual"}
#' 
#' %% ~~ A concise (1-5 lines) description of what the class is. ~~
#' 
#' 
#' @name saObj.virtual-class
#' @docType class
#' @note %% ~~further notes~~
#' @section Objects from the Class: A virtual Class: No objects may be created
#' from it.
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{~~fun~~}}, ~~~ %% ~~or
#' \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
#' @references %% ~~put references to the literature/web site here~~
#' @keywords classes
#' @examples
#' 
#' showClass("saObj.virtual")
#' 
setClass(Class = "saObj.virtual"
	 , contains = 'VIRTUAL'
	 , slots = c(
		     data = "data.frame"
		     , f = "formula"
		     , cluster = "characterOrNULL"
		     , include = "characterOrNULL"
					   )
	 , validity = function(object){
	   if (
	       length(grep('*', slot(object, "f"), fixed = TRUE)) != 0 |
	       length(grep(':', slot(object, "f"), fixed = TRUE)) != 0
	       ){return("formula must not contain interactions")}
	   if ( length(grep('|', slot(object, "f"), fixed = TRUE)) != 1){return('formula must contain a \' | smallArea\' term')}
	   varnames <- all.vars(slot(object, "f"))
	   smallArea <- varnames[length(varnames)]
	   predictand <- varnames[1]
	   predictand %in% colnames(slot(object, "data")) || return(paste("predictand ", predictand, " not found in data.", sep=''))
	   is.numeric(slot(object, "data")[, predictand]) || return(paste("data$", predictand, " has got to be numeric.", sep=''))
	   smallArea %in% colnames(slot(object, "data")) || return(paste("smallArea " , smallArea, " not found in data.", sep=''))
	   if(! is.null(slot(object, "cluster"))) {
	     slot(object, "cluster") %in% colnames(slot(object, "data")) || return(paste("clustering indicator ", slot(object, "cluster"), " not found in data.", sep=''))
	     any(is.na(slot(object, "data")[ , slot(object, "cluster")])) && 
	     return(paste("clustering indicator "
			  , slot(object, "cluster") , "contains NA, can't deal with" ,
			  "missing cluster indicators, use constructor function saObj.")
			  )
	   }
	   if(! is.null(slot(object, "include"))){
	     ! is.null(slot(object, "cluster")) || return(paste("inclusion indicator ", slot(object, "include"), " only valid for clustered data.", sep='')) 
	     slot(object, "include") %in% colnames(slot(object, "data")) || return(paste("inclusion indicator ", slot(object, "include"), " not found in data.", sep=''))
	     if(class(slot(object, "data")[, slot(object, "include")]) != "logical")
	       return(paste(slot(object, "include"), " has got to be of class 'logical'.", sep=''))
	   }
	 }
	 )
#' Class \code{"saObj"}
#' 
#' %% ~~ A concise (1-5 lines) description of what the class is. ~~
#' 
#' 
#' @name saObj-class
#' @aliases saObj-class predict,saObj-method
#' @docType class
#' @note %% ~~further notes~~
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("saObj", ...)}. %% ~~ describe objects here ~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{~~fun~~}}, ~~~ %% ~~or
#' \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
#' @references %% ~~put references to the literature/web site here~~
#' @keywords classes
#' @examples
#' 
#' showClass("saObj")
#' 
setClass(Class = "saObj"
	 , contains = 'saObj.virtual'
	 , validity = function(object){
	   if (length(all.vars(slot(object, "f"))) > 2) return('formula has to be of structure predictand ~ NULL | smallArea')
	   varnames <- all.vars(slot(object, "f"))
	   smallArea <- varnames[length(varnames)]
	   predictand <- varnames[1]
	   include <- slot(object, "data")[, slot(object, "include")]

	   if (is.null(slot(object, "include"))){
	     ! any(is.na(slot(object, 'data')[, predictand])) || return("can't handle missing values for predictand")
	   } else {
	     ! any(is.na(slot(object, 'data')[, predictand])) || return("can't handle missing values for predictand")
	     ## TODO
	     #! any(is.na(slot(object, 'data')[include, predictand])) || return("can't handle missing values for predictand")
	   }
	 }
	 )
#' Class \code{"saeObj"}
#' 
#' %% ~~ A concise (1-5 lines) description of what the class is. ~~
#' 
#' 
#' @name saeObj-class
#' @aliases saeObj-class predict,saeObj-method
#' @docType class
#' @note %% ~~further notes~~
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("saeObj", ...)}. %% ~~ describe objects here ~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{~~fun~~}}, ~~~ %% ~~or
#' \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
#' @references %% ~~put references to the literature/web site here~~
#' @keywords classes
#' @examples
#' 
#' showClass("saeObj")
#' 
setClass(Class = "saeObj"
	 , contains ="saObj.virtual"
	 , slots = c(predictors = "character"
					   , smallAreaMeans = "data.frameOrNULL"
					   , s1 = "characterOrNULL"
					   , s2 = "characterOrNULL"
					   )
	 , validity = function(object){
	   varnames <- all.vars(slot(object, "f"))
	   smallArea <- varnames[length(varnames)]
	   predictors <- varnames[-c(1,which(varnames == smallArea))]
	   length(predictors) > 0 || return(paste("got no predictor(s).", sep=''))
	   all(predictors %in% colnames(slot(object, "data"))) || return(paste("not all predictors found in data.", sep=''))
	   if(! is.numeric(as.matrix(slot(object, "data")[, c(predictors)])))
	     return("all predictors have got to be numeric.")
	   if(! is.null(slot(object, "s1"))){
	     if(class(slot(object, "data")[, slot(object, "s1")]) != "logical")
	       return(paste("data$", slot(object, "s1"), " has got to be of class 'logical'.", sep=''))
	     s1TOs0 <- sum(slot(object, "data")[,slot(object, "s1")]) /  nrow(slot(object, "data"))
	     if( s1TOs0 > 0.1)
	       message(paste("n(s0) >> n(s1) should hold, but you've given s1 resulting in n(s1)/n(s0) = ",s1TOs0))
	   }
	   if(! is.null(slot(object, "s2"))){
	     slot(object, "s2") %in% colnames(slot(object, "data")) || return(paste("s2 indicator ", slot(object, "s2"), " not found in data.", sep=''))
	     if(class(slot(object, "data")[, slot(object, "s2")]) != "logical")
	       return(paste("data$", slot(object, "s2"), " has got to be of class 'logical'.", sep=''))
	   } else {
	     if(!is.null(slot(object, "cluster")))
	       return('need s2 for cluster sampling.')
	     if(is.null(slot(object, "smallAreaMeans")))
	       return("got neither s2 nor smallAreaMeans.")
	     else {
	       if(! all(predictors %in% colnames(slot(object, "smallAreaMeans")) ))
		 return("got neither s2 nor exhaustive smallAreaMeans.")
	     }
	   }
	   if(! is.null(slot(object, "s1")) && ! is.null(slot(object, "s2")))
	     if(any(slot(object, "data")[,slot(object, "s2")] &  ! slot(object, "data")[,slot(object, "s1")]))
	       return('s2 is not a subset of s1!')
	   if(! is.null(slot(object, "smallAreaMeans"))){
	     if(any(is.na(slot(object, "smallAreaMeans"))))
	       return("Can't deal with missing data in smallAreaMeans")
	     if(!all(slot(object, "smallAreaMeans")[, smallArea] %in%
		     unique(slot(object, "data")[, smallArea][!is.na(slot(object, "data")[, smallArea])])))
	       return('found extraneous smallAreas in smallAreaMeans')
	     if(!all(unique(slot(object, "data")[, smallArea][!is.na(slot(object, "data")[, smallArea])]) %in%
		     slot(object, "smallAreaMeans")[, smallArea]))
	       return('missing smallAreas in smallAreaMeans')
	     if(! all(colnames(slot(object, "smallAreaMeans")) %in% c(predictors , smallArea)))
	       return(paste("smallAreaMeans don't really fit. 
			    Check predictor and smallArea naming and
			    remove all unnecessary variables from the frame.", sep=''))
	   }

	 }
	 )
#' %% ~~function to do ... ~~
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param data %% ~~Describe \code{data} here~~
#' @param f %% ~~Describe \code{f} here~~
#' @param smallAreaMeans %% ~~Describe \code{smallAreaMeans} here~~
#' @param s1 %% ~~Describe \code{s1} here~~
#' @param s2 %% ~~Describe \code{s2} here~~
#' @param cluster %% ~~Describe \code{cluster} here~~
#' @param include %% ~~Describe \code{include} here~~
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#' 'comp2'} %% ...
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function (data, f, smallAreaMeans = NULL, s1 = NULL, s2 = NULL, 
#'     cluster = NULL, include = NULL) 
#' {
#'     com <- NULL
#'     if (!is.null(cluster) & is.null(include)) {
#'         w <- "include is NULL, automatically adding it as TRUE to data."
#'         message(w)
#'         com <- c(com, w)
#'         data$include <- rep(TRUE, nrow(data))
#'         include <- "include"
#'     }
#'     if (!is.null(cluster) & any(is.na(data[, cluster]))) {
#'         w <- "found NA in cluster indicator, but we need them for \n\t\t   KLUDGE in s1. I automatically set them to a\n\t\t   default. This might not be what you intended."
#'         message(w)
#'         com <- c(com, w)
#'         data[is.na(data[, cluster]), cluster] <- as.character(seq.int(from = -1, 
#'             to = -length(data[is.na(data[, cluster]), cluster]), 
#'             by = -1))
#'     }
#'     if (length(all.vars(f)) > 2) {
#'         ret <- new(Class = "saeObj", data = data, f = f, smallAreaMeans = smallAreaMeans, 
#'             s1 = s1, s2 = s2, cluster = cluster, include = include)
#'     }
#'     else {
#'         ret <- new(Class = "saObj", data = data, f = f, cluster = cluster, 
#'             include = include)
#'     }
#'     comment(ret) <- com
#'     return(ret)
#'   }
#' 
	     saObj <- function(data, f
				 , smallAreaMeans = NULL
				 , s1 = NULL, s2 = NULL
				 , cluster = NULL, include = NULL)
	     {
	       com <- NULL
		 if  (! is.null(cluster) & is.null(include)){
		   w <- 'include is NULL, automatically adding it as TRUE to data.'
		   message(w); com <- c(com, w)
		   data$include <- rep(TRUE, nrow(data))
		   include <- 'include'
		 }
		 if  (! is.null(cluster) & any(is.na(data[, cluster]))){
		   w <- 'found NA in cluster indicator, but we need them for 
		   KLUDGE in s1. I automatically set them to a
		   default. This might not be what you intended.'
		   message(w); com <- c(com, w)
		   data[is.na(data[, cluster]), cluster] <- as.character(seq.int(from=-1
										 , to=-length(data[is.na(data[, cluster]), cluster])
										 , by =-1))

		 }
	       if (length(all.vars(f)) > 2) {
		   ret <- new(Class = "saeObj"
			      , data = data
			      , f = f
			  , smallAreaMeans = smallAreaMeans
			  , s1 = s1
			  , s2 = s2
			      , cluster = cluster
			      , include = include
			      )
	       } else {
		   ret <- new(Class = "saObj"
			      , data = data
			      , f = f
			      , cluster = cluster
			      , include = include
			      )
	       }

	       comment(ret) <- com
	       return(ret)
	     }

#' ~~ Methods for Function \code{predict} ~~
#' 
#' ~~ Methods for function \code{predict} ~~
#' 
#' 
#' @name predict-methods
#' @aliases predict-methods predict,ANY-method predict,saeObj-method
#' predict,saObj-method
#' @docType methods
#' @section Methods: \describe{
#' 
#' \item{list("signature(object = \"ANY\")")}{ %% ~~describe this method here~~
#' }
#' 
#' \item{list("signature(object = \"saeObj\")")}{ %% ~~describe this method
#' here~~ }
#' 
#' \item{list("signature(object = \"saObj\")")}{ %% ~~describe this method
#' here~~ } }
#' @keywords methods ~~ other possible keyword(s) ~~
	     setMethod(f = "predict"
		       , signature(object = "saObj")
		       , function(object
				  ){
			 VIMFOLD <- TRUE ## I use if(VIMFOLD){...}-blocks for folding in vim. They're just TRUE
			 varnames <- all.vars(slot(object, "f"))
			 smallArea <- varnames[length(varnames)]
			 predictand <- varnames[1]
			 com <- comment(object) ## recycle comments generated by the constructor function
			 include <- slot(object, "data")[, slot(object, "include")]
			 if (! is.null(slot(object, "cluster")))
			 {# cluster sampling 
			     M <- tapply(as.numeric(include)
					 , slot(object, "data")[, slot(object, "cluster")]
					 , sum
					 , na.rm = TRUE)
			     Y_c <- tapply(slot(object, "data")[, predictand] * as.numeric(include)
						    , slot(object, "data")[, slot(object, "cluster")], sum) / M
			 } 
			 else 
			 {# non-cluster sampling
			   Y <- slot(object, "data")[, predictand]
			 }
			 out <- data.frame()
			 for (group in sort(unique(slot(object, "data")[, smallArea])[! is.na(unique(slot(object, "data")[, smallArea]))]))
			 {
			     
			   if (! is.null(slot(object, "cluster")))
			   {# cluster sampling
			     if (is.factor(slot(object, "data")[, smallArea])){
			       smallarea <- tapply(as.character(slot(object, "data")[, smallArea]), slot(object, "data")[, slot(object, "cluster")], unique)
			     } else {
			       smallarea <- tapply(slot(object, "data")[, smallArea], slot(object, "data")[, slot(object, "cluster")], unique)
			     }
			     Gc <- smallarea == group; Gc[is.na(Gc)] <- FALSE
			     n_Gc <- sum(as.numeric(Gc))
			     p_cs <- Reduce('+',mapply('*', M[Gc]
						       , Y_c[Gc]
						       , SIMPLIFY=FALSE)) / sum(M[Gc])
			     var_cs <- 1 / sum(M[Gc])^2 * n_Gc / (n_Gc - 1) * sum((Y_c[Gc] * M[Gc] 
										   - p_cs * M[Gc])^2) 
			     result <- data.frame(smallArea = group
						  , p_cs = p_cs
						  , var_cs = var_cs
						  )
			   }
			   else 
			   {# non-cluster sampling 
			       G <- slot(object, "data")[, smallArea] == group; G[is.na(G)] <- FALSE
			       result <- data.frame(smallArea = group
						    , p_srs  = mean(Y[G])
						    , var_srs = var(Y[G]) / length(Y[G])
						    )
			   }
			   out <- rbind(out, result)
			 }
			 comment(out) <- com
			 return(out)
		       }
		       )
	     setMethod(f = "predict"
		       , signature(object = "saeObj")
		       , function(object
				  , ALL = FALSE
				  , design = FALSE
				  ){
			 VIMFOLD <- TRUE ## I use if(VIMFOLD){...}-blocks for folding in vim. They're just TRUE
			 varnames <- all.vars(slot(object, "f"))
			 smallArea <- varnames[length(varnames)]
			 predictors <- varnames[-c(1,which(varnames == smallArea))]
			 predictand <- varnames[1]

			 com <- comment(object) ## recycle comments generated by the constructor function
			 if (VIMFOLD){ ## which type of auxiliary data do we have?
			   if (is.null(slot(object, "smallAreaMeans"))){
			     if(is.null(slot(object, "s1"))){
			       pred.type <- 'non-exhaustive'
			     } else {
			       pred.type <- 'partially'
			       threePhase <- TRUE
			     }
			   } else {
			     if(
				length(colnames(slot(object, "smallAreaMeans"))) == length(c(predictors, smallArea)) && 
				all(sort(colnames(slot(object, "smallAreaMeans"))) == sort(c(predictors, smallArea)))
				){
			       pred.type  <- 'exhaustive'
			     } else {
			       pred.type <- 'partially'
			       threePhase <- FALSE
			     }
			   }
			 }

			 if (VIMFOLD){ ## cluster and non-cluster commons
			   if (pred.type == "partially")
			   {
			     if (threePhase){
			       ## find those predictors which are not all NA where s1 is FALSE
			       predictors1 <- predictors[
								apply(
								      ! is.na(
									      slot(object, "data")[! slot(object, "data")[, slot(object, "s1")], predictors]
									      )
								      ,2
								      ,all)]

			     } else {
			       predictors1 <- colnames(slot(object, "smallAreaMeans"))[-which(colnames(slot(object, "smallAreaMeans")) == smallArea)]
			     }
			     Z1 <-  as.list(as.data.frame(t(cbind(1, slot(object, "data")[, predictors1]))))  
			   }
			   if (is.null(slot(object, "cluster")) || 
			       pred.type == "partially")
			   {
			     if (is.null(slot(object, "s2"))){ 
			       s2 <- rep(TRUE, nrow(slot(object, "data"))) 
			     }  else { 
			       s2 <- slot(object, "data")[, slot(object, "s2")]
			     }
			     n_s2 <- sum(as.numeric(s2))
			     Y <- slot(object, "data")[, predictand]
			     Z <- as.list(as.data.frame(t(cbind(1, slot(object, "data")[, c(predictors)]))))
			   }
			 }
			 if (pred.type %in% c('exhaustive', 'partially', 'non-exhaustive')){ ## testing for missing values in samples s2/s1/s0
			   ## missing predictand in s2
			   if (is.null(slot(object, "s2"))){
			     pred.vals <- slot(object, 'data')[, predictand] 
			   } else {
			     pred.vals <- slot(object, 'data')[slot(object, 'data')[,slot(object, "s2")], predictand] 
			   }
			   ! any(is.na(pred.vals)) || stop("can't handle missing values for predictand in s2")
			   ## missing predictor in s2
			   if (is.null(slot(object, "s2"))){
			     pred.vals <- slot(object, 'data')[, predictors] 
			   } else {
			     pred.vals <- slot(object, 'data')[slot(object, 'data')[,slot(object, "s2")], predictors] 
			   }
			   ! any(is.na(pred.vals)) || stop("can't handle missing values for predictors in s2")
			   ## missing predictor in s1
			   if (pred.type %in% c('non-exhaustive', 'partially')){ 
			     if (is.null(slot(object, "s1"))){
			       pred.vals <- slot(object, 'data')[, predictors] 
			     } else {
			       pred.vals <- slot(object, 'data')[slot(object, 'data')[,slot(object, "s1")], predictors] 
			     }
			     ! any(is.na(pred.vals)) || stop("can't handle missing values for predictors in s1")
			   }
			   ## missing predictor in s0
			   ## works with three-phase implementation
			 }
			 if (! is.null(slot(object, "cluster")))
			 {# cluster sampling 
			   if (VIMFOLD){ ## commons
			     if (is.null(slot(object, "s1"))){ 
			       ## s1 is given by s2 %in% c(TRUE,FALSE)
			       ## we need to remove 'na'-Values for 'cluster'
			       s1c <- rep(TRUE
					  , length(unique(
							  slot(object, "data")[!is.na(slot(object, "data")[, slot(object, "cluster")])
									       , slot(object, "cluster")]
							  )
					  )
					  ) 
			       names(s1c) <- unique(
						    slot(object, "data")[!is.na(slot(object, "data")[, slot(object, "cluster")])
									 , slot(object, "cluster")]
						    )
			     } else {
			       s1c <-   tapply(slot(object, "data")[, slot(object, "s1")], slot(object, "data")[, slot(object, "cluster")], unique)
			     }
			     s2c <-   tapply(slot(object, "data")[, slot(object, "s2")], slot(object, "data")[, slot(object, "cluster")], unique)
			     n_s2c <- sum(as.numeric(s2c))
			     include <- slot(object, "data")[, slot(object, "include")]
			     M <- tapply(as.numeric(include), slot(object, "data")[, slot(object, "cluster")], sum, na.rm = TRUE)
			     Y_c <- tapply(slot(object, "data")[, predictand] * as.numeric(include)
						    , slot(object, "data")[, slot(object, "cluster")], sum) / M

			     if (length(predictors) == 1){
			     Z_c <- mapply(append
					   , mapply('/'
						    , by( slot(object, "data")[, c(predictors)] *  as.numeric(include)
							 , slot(object, "data")[, slot(object, "cluster")], sum, simplify = TRUE)
						    , M
						    , SIMPLIFY=FALSE)
					   , 1
					   , SIMPLIFY = FALSE
					   , after = 0) 
			     } else {
			     Z_c <- mapply(append
					   , mapply('/'
						    , by( slot(object, "data")[, c(predictors)] *  as.numeric(include)
							 , slot(object, "data")[, slot(object, "cluster")], colSums, simplify = TRUE)
						    , M
						    , SIMPLIFY=FALSE)
					   , 1
					   , SIMPLIFY = FALSE
					   , after = 0) 
			     } ## a2 p.445
			     ## set up A_cs2c
			     A_cs2c <- (Reduce('+'
					       , mapply('*', M[s2c]
							, lapply(Z_c[s2c]
								 , function(x) as.numeric(x) %o% as.numeric(x))
							, SIMPLIFY=FALSE))) / n_s2c ## from a2.38
			     iA_cs2c <- solve(A_cs2c)
			     ## estimate regression coefficients
			     a2.38 <- as.vector(iA_cs2c %*% Reduce('+', mapply('*', M[s2c]
									       ,   mapply('*', Y_c[s2c]
											  , Z_c[s2c]
											  , SIMPLIFY=FALSE)
									       , SIMPLIFY=FALSE)
			     ) / n_s2c)
			     ## calculate residuals 
			     R_c <- Y_c-sapply(Z_c, function(x) x %*% a2.38) ## a2 p.445
			     ## design-based variance-covariance matrix
			     a2.39 <-iA_cs2c %*%
			     (Reduce('+'
				     , mapply('*', M[s2c]^2
					      , mapply('*'
						       , R_c[s2c]^2
						       , lapply(Z_c[s2c], function(x) as.numeric(x) %o% as.numeric(x))
						       , SIMPLIFY=FALSE)
					      , SIMPLIFY=FALSE)
				     ) / n_s2c^2) %*% iA_cs2c
			   }
			   switch(pred.type # uncommons
				  , 'partially' = {
				    n_s1c <- sum(as.numeric(s1c))
				  }
				  , 'exhaustive' = # same as below
				  , 'non-exhaustive' = {
				  }
				  , stop(paste('unkown pred.type', pred.type))
				  )
			 } 
			 else 
			 {# non-cluster sampling
			   if (VIMFOLD){ ## commons
			     if (is.null(slot(object, "s1"))){
			       s1 <- rep(TRUE, nrow(slot(object, "data")))
			     } else {
			       s1 <- slot(object, "data")[, slot(object, "s1")]
			     }
			     ## set up A_s2
			     A_s2 <- Reduce('+', lapply(Z[s2], function(x) as.numeric(x) %o% as.numeric(x)))/ n_s2
			     iA_s2 <- solve(A_s2)
			     ## estimate regression coefficients
			     u_s2 <- Reduce('+', mapply('*', Z[s2], Y[s2], SIMPLIFY = FALSE)) /  n_s2
			     a2.13.b <- as.numeric(iA_s2 %*% u_s2)
			     ## calculate residuals 
			     R <- Y - sapply(Z, function(x) x %*% a2.13.b) # a2 p.443 & b2 p.1025
			     ## design-based variance-covariance matrix
			     a2.15 <- iA_s2 %*% (Reduce('+', mapply('*', R[s2]^2
								    , lapply(Z[s2], function(x) as.numeric(x) %o% as.numeric(x))
								    , SIMPLIFY=FALSE)) / n_s2 ^ 2) %*% iA_s2
			   }
			   switch(pred.type # uncommons
				  , 'exhaustive' = # same as below
				  , 'non-exhaustive' = {
				  }
				  , 'partially' = {
				    ## classical model
				    n_s1 <- sum(as.numeric(s1)) ## needed in b2.35
				    predictors2 <- predictors[-which(predictors %in%  predictors1)]
				    A1_s2 <- Reduce('+', lapply(Z1[s2], function(x) as.numeric(x) %o% as.numeric(x)))/ n_s2 ## from b2.27
				    iA1_s2 <- solve(A1_s2)
				    u1_s2 <- Reduce('+', mapply('*', Z1[s2], Y[s2], SIMPLIFY = FALSE)) /  n_s2
				    b2.5.2 <- as.numeric(iA1_s2 %*% u1_s2)
				    R1 <- Y - sapply(Z1, function(x) x %*% b2.5.2)  # b2 p.1025
				    b2.6.2 <- a2.13.b
				    b1.28 <- a2.15 
				    b1.29 <- iA1_s2 %*% (Reduce('+', mapply('*', R1[s2]^2
									    , lapply(Z1[s2], function(x) as.numeric(x) %o% as.numeric(x))
									    , SIMPLIFY=FALSE)) / n_s2 ^ 2) %*% iA1_s2

				  }
				  , stop(paste('unkown pred.type', pred.type))
				  )
			 }
			 out <- data.frame()
			 for (group in sort(unique(slot(object, "data")[, smallArea])[! is.na(unique(slot(object, "data")[, smallArea]))]))
			 {
			   if (VIMFOLD){ # cluster and non-cluster commons, temporary objects will be removed for cluster sampling
			     if (is.null(slot(object, "cluster")) || 
				 pred.type == "partially")
			     {
			       G <- slot(object, "data")[, smallArea] == group; G[is.na(G)] <- FALSE
			       eZ <- mapply(append, Z, as.numeric(G), SIMPLIFY = FALSE) ## a2 p.444
			       ieA_s2 <- solve(1 / n_s2 * Reduce('+', lapply(eZ[s2]
									     , function(x) as.numeric(x) %o% as.numeric(x))))  ## a2 p.444
			       eu_s2 <- Reduce('+', mapply('*', eZ[s2], Y[s2], SIMPLIFY = FALSE)) /  n_s2  ## a2 p.444
			       b2.28 <- ieA_s2 %*% eu_s2 ## the only one needed in cluster sampling, see b1 p.22 
			     }

			     switch(pred.type 
				    , 'partially' = {
				      eZ1 <- mapply(append, Z1, as.numeric(G), SIMPLIFY = FALSE) # b2 p.1026
				      eA1_s2 <- Reduce('+', lapply(eZ1[s2], function(x) as.numeric(x) %o% as.numeric(x)))/ n_s2
				      ieA1_s2 <- solve(eA1_s2) 
				      eu1_s2 <- Reduce('+', mapply('*', eZ1[s2], Y[s2], SIMPLIFY = FALSE)) /  n_s2 
				      b2.27 <- ieA1_s2 %*% eu1_s2 ## needed in cluster sampling, see b1 p.22
				      tmZ1_G <- as.numeric(
							   cbind(1
								 , subset(slot(object, "smallAreaMeans")[, predictors1]
									  , slot(object, "smallAreaMeans")[, smallArea] == group))
							   ) ## b2 p.1026
				      tmeZ1_G <- c(tmZ1_G, 1) ## b2 p.1027. b1 p.18 and p.22 are both in error. needed in cluster sampling b1.50 KLDUGE this may be in error!
				    }
				    , 'exhaustive' =  ## as below
				    , 'non-exhaustive' = {
				      ## do nothing
				    }
				    , stop(paste('unkown pred.type', pred.type))
				    )
			   }
			   if (! is.null(slot(object, "cluster")))
			   {# cluster sampling
			     if (is.factor(slot(object, "data")[, smallArea])){
			       smallarea <- tapply(as.character(slot(object, "data")[, smallArea]), slot(object, "data")[, slot(object, "cluster")], unique)
			     } else {
			       smallarea <- tapply(slot(object, "data")[, smallArea], slot(object, "data")[, slot(object, "cluster")], unique)
			     }
			     Gc <- smallarea == group; Gc[is.na(Gc)] <- FALSE
			     n_s1cGc <- sum(as.numeric(s1c & Gc))
			     n_s2cGc <- sum(as.numeric(s2c & Gc))
			     if (design){
			     p_cs <- Reduce('+',mapply('*', M[s2c & Gc]
						       , Y_c[s2c & Gc]
						       , SIMPLIFY=FALSE)) / sum(M[s2c & Gc])
			     var_cs <- 1 / sum(M[s2c & Gc])^2 * n_s2cGc / (n_s2cGc - 1) * sum((Y_c[s2c & Gc] * M[s2c & Gc] 
											       - p_cs * M[s2c & Gc])^2) 
			     result <- data.frame(smallArea = group
						  , p_cs = p_cs
						  , var_cs = var_cs
						  )
			     } else {
			     result <- data.frame(smallArea = group)
			     }
			     if (VIMFOLD){ # commons
			       ## extended model
			       tmp <- slot(object, "data")[, smallArea] == group; tmp[is.na(tmp)] <- FALSE
			       I_cGc <- tapply(as.numeric(tmp), slot(object, "data")[, slot(object, "cluster")], sum) / M ## a2 p.445: I_{c, Gc}(x) !:= Gc
			       eZ_c <- mapply(append, Z_c, I_cGc, SIMPLIFY = FALSE)
			       ieA_cs2c <- solve(Reduce('+',
							mapply('*'
							       , lapply(eZ_c[s2c], function(x) as.numeric(x) %o% as.numeric(x))
							       , M[s2c]
							       , SIMPLIFY = FALSE
							       )
							) / n_s2c # a1 p.24
			       )
			       a1.63 <- ieA_cs2c %*% Reduce('+', mapply('*', eZ_c[s2c]
									, Y_c[s2c] * M[s2c]
									, SIMPLIFY = FALSE)) / n_s2c
			       eR_c <- Y_c - sapply(eZ_c, function(x) x %*% a1.63) # a1 p.25
			       a1.64 <- ieA_cs2c %*% (Reduce('+'
							     , mapply('*', M[s2c]^2
								      , mapply('*'
									       , eR_c[s2c]^2
									       , lapply(eZ_c[s2c], function(x) as.numeric(x) %o% as.numeric(x))
									       , SIMPLIFY=FALSE)
								      , SIMPLIFY=FALSE)
							     ) / n_s2c^2) %*% ieA_cs2c
			       meZ_cs1cGc <- Reduce('+', mapply('*', M[s1c & Gc]
								, eZ_c[s1c & Gc]
								, SIMPLIFY=FALSE)) / sum(M[s1c & Gc]) # a1. p.25, b1 p.22 and c1 p.21
			     }
			     switch(pred.type # uncommons
				    , 'partially' = {
				      rm(G,eZ,ieA_s2, eu_s2) ## remove temporary objects
				      rm(eZ1, eA1_s2, ieA1_s2, eu1_s2, tmZ1_G) ## remove temporary objects
				      if (length(predictors1) == 1){
					## length 1, we need to tapply sum 
					Z1_c <- mapply(append
						       , mapply('/'
								, tapply( slot(object, "data")[, predictors1] *  as.numeric(include)
									 , slot(object, "data")[, slot(object, "cluster")], sum)
								, M
								, SIMPLIFY=FALSE)
						       , 1
						       , SIMPLIFY = FALSE
						       , after = 0)
				      } else {
					## length > 1, we need to by colSums
					Z1_c <- mapply(append
						       , mapply('/'
								,
								by( slot(object, "data")[, predictors1] *  as.numeric(include)
								   , slot(object, "data")[, slot(object, "cluster")], colSums, simplify = TRUE)
								, M
								, SIMPLIFY=FALSE)
						       , 1
						       , SIMPLIFY = FALSE
						       , after = 0)
				      }
				      eZ1_c <- mapply(append, Z1_c, I_cGc, SIMPLIFY = FALSE)
				      meZ1_cs1cGc <- Reduce('+', mapply('*', M[s1c & Gc]
									, eZ1_c[s1c & Gc]
									, SIMPLIFY=FALSE)) / sum(M[s1c & Gc]) # b1 p.22
				      ieA1_cs1c <- solve(Reduce('+',
							       mapply('*'
								      , lapply(eZ1_c[s1c], function(x) as.numeric(x) %o% as.numeric(x))
								      , M[s1c]
								      , SIMPLIFY = FALSE
								      )
							       ) / n_s1c # b1 p.22
				      )
				      ieA1_cs2c <- solve(Reduce('+',
							       mapply('*'
								      , lapply(eZ1_c[s2c], function(x) as.numeric(x) %o% as.numeric(x))
								      , M[s2c]
								      , SIMPLIFY = FALSE
								      )
							       ) / n_s2c # b1 p.22
				      )
				      b1.48 <- ieA1_cs2c %*% Reduce('+', mapply('*', eZ1_c[s2c]
									       , Y_c[s2c] * M[s2c]
									       , SIMPLIFY = FALSE)) / n_s2c
				      b1.49 <- a1.63
				      if(threePhase){ # we now estimate the smallAreaMeans from s0: c1 p.16. This is the essence of c1.
					  tmeZ1_G <- Reduce('+', mapply('*', M[Gc]
									   , eZ1_c[Gc]
									   , SIMPLIFY=FALSE)) / sum(M[Gc]) # a1. p.25, b1 p.22 and c1 p.21
				      }
				      b1.50 <- (tmeZ1_G - meZ1_cs1cGc) %*% b1.48 + t(b1.49) %*% meZ_cs1cGc 
				      w <- 'b1 p.22 gives strange formulae for R_c and R1_c, I replace hat{Y} with Y, resulting in formulae analogous to b2 p.18 with Zeta replaced by Zeta1. Confirmed by D.M.'
				      com <- c(com,w)
				      eR1_c <- Y_c - sapply(eZ1_c, function(x) x %*% b1.48) # b1 p.22
				      w <- 'c1 p.21/b1 p.22: In analogy to a1.25 I change hat{eR}_{1,c} to use the clustered version of the parameter estimate. Confirmed by D.M.'
				      com <- c(com,w)
				      b1.51.a <- ieA1_cs1c %*% (Reduce('+'
								      , mapply('*', M[s2c]^2
									       , mapply('*'
											, eR1_c[s2c]^2
											, lapply(eZ1_c[s2c], function(x) as.numeric(x) %o% as.numeric(x))
											, SIMPLIFY=FALSE)
									       , SIMPLIFY=FALSE)
								      ) / n_s2c^2) %*% ieA1_cs1c
				      eR_c <- Y_c - sapply(eZ_c, function(x) x %*% b1.49) # b1 p.22
				      w <- 'c1 p.21/b1 p.22: In analogy to a1.25 I change hat{eR}_{c} to use the clustered version of the parameter estimate. Confirmed by D.M.'
				      com <- c(com,w)
				      b1.51.b <- ieA_cs2c %*% (Reduce('+'
								      , mapply('*', M[s2c]^2
									       , mapply('*'
											, eR_c[s2c]^2
											, lapply(eZ_c[s2c], function(x) as.numeric(x) %o% as.numeric(x))
											, SIMPLIFY=FALSE)
									       , SIMPLIFY=FALSE)
								      ) / n_s2c^2) %*% ieA_cs2c
				      w <- 'b1.52 is skrewed, I replace hat{bar{Z}}_{G,1} with  hat{bar{Z}}_{c,G} and bar{Z}^(1) with bar{Zeta}^(1)_{G} in analogy with c2.24. Confirmed by D.M.'
				      com <- c(com,w)
				      b1.52 <- n_s2c/n_s1c * tmeZ1_G %*% b1.51.a %*% tmeZ1_G  + (1 - n_s2c/n_s1c) * meZ_cs1cGc %*% b1.51.b %*% meZ_cs1cGc 
				      if (threePhase){
					c1.53 <- b1.50
					n_s0cGc <- sum(as.numeric(Gc))
					COV_cs0G <- Reduce('+',
						      mapply('*'
							     , (M[Gc] / mean(M[Gc]))^2
							     , lapply(
								      lapply(eZ1_c[ Gc]
									     , function(x) x -  tmeZ1_G )
								      , function(x) as.numeric(x) %o% as.numeric(x))
							     , SIMPLIFY=FALSE
							     )
						      )  / (n_s0cGc * (n_s0cGc - 1))
					c1.55 <- b1.52 + t(b1.48) %*% COV_cs0G %*% b1.48 
					result <- cbind(result
							,  c1.53,  c1.55
							)
				      } else {
					result <- cbind(result
							,  b1.50,  b1.52
							)
				      }
				    }
				    , 'exhaustive' = {
				      tmZ_Gc <- as.numeric(
							   cbind(1
								 , subset(slot(object, "smallAreaMeans")[, predictors]
									  , slot(object, "smallAreaMeans")[, smallArea] == group))
							   ) ## KLUDGE: where's that from?
				      ## extended model
				      tmeZ_Gc <- c(tmZ_Gc, 1) # a1 p.18
				      a2.48 <- as.numeric(tmeZ_Gc %*% a1.63)
				      a2.49 <- as.numeric(tmeZ_Gc %*% a1.64 %*% tmeZ_Gc)
				      result <- cbind(result
						      ,  a2.48,  a2.49
						      )
				    } 
				    , 'non-exhaustive' = {
				      ## ## ## psynth
				      ## mean of the clustered Y m over Gc
				      a2.40 <- Reduce('+', mapply('*', M[s1c & Gc]
								  , Z_c[s1c & Gc]
								  , SIMPLIFY=FALSE)) / sum(M[s1c & Gc])
				      a2.41 <- Reduce('+',
						      mapply('*'
							     , (M[s1c & Gc] / mean(M[s1c & Gc]))^2
							     , lapply(
								      lapply(Z_c[s1c & Gc]
									     , function(x) x - a2.40)
								      , function(x) as.numeric(x) %o% as.numeric(x))
							     , SIMPLIFY=FALSE
							     )
						      )  / (n_s1cGc * (n_s1cGc - 1))
				      a2.46 <- as.numeric(meZ_cs1cGc %*% a1.63) # == a1.65
				      a1.67 <- Reduce('+',
						      mapply('*'
							     , (M[s1c & Gc] / mean(M[s1c & Gc]))^2
							     , lapply(
								      lapply(eZ_c[s1c & Gc]
									     , function(x) x - meZ_cs1cGc)
								      , function(x) as.numeric(x) %o% as.numeric(x))
							     , SIMPLIFY=FALSE
							     )
						      )  / (n_s1cGc * (n_s1cGc - 1))
				      a2.47 <- as.numeric(meZ_cs1cGc %*% a1.64 %*% meZ_cs1cGc + t(a1.63) %*% a1.67 %*% a1.63) # == a1.66
				      if (ALL){
					## ## ## psmall
					a2.42 <- as.numeric(a2.40 %*% a2.38)
					a2.43 <- as.numeric(a2.40 %*% a2.39 %*% a2.40 + a2.38 %*% a2.41 %*% a2.38)
					## ## ## psynth
					mR_s2cGc <- as.numeric(M[s2c & Gc] %*% R_c[s2c & Gc] / sum(M[s2c & Gc])) ## a2 p.445
					a2.44 <- as.numeric(a2.42 + mR_s2cGc)
					a2.45 <- as.numeric(a2.43 +
							    (M[s2c & Gc] / mean(M[s2c & Gc]))^2 %*% (R_c[s2c & Gc] - mR_s2cGc)^2
							    / (n_s1cGc * (n_s1cGc - 1)))
					result <- cbind(result
							, p_cGcpsynth = a2.42, var_cGcpsynth = a2.43
							, p_cGcpsmall = a2.44, var_cGcpsmall = a2.45
							)
				      } 
				      result <- cbind(result
						      ,  a2.46,  a2.47 
						      )
				      w <- 'KLUDGE Mandallaz calls a1.65/a2.46 hat{tilde{Y}}_sub{c,psynth} but I assume that this is an error and hat{tilde{Y}}_sub{c,Gc,psynth} is right. Confirmed by D.M.'
				      com <- c(com,w)

				    }
				    , stop(paste('unkown pred.type', pred.type))
				    )
			   }
			   else 
			   {# non-cluster sampling 
			       n_s2G <- sum(as.numeric(s2 & G))
			       n_s1G <- sum(as.numeric(s1 & G))
			       if (design){
			       result <- data.frame(smallArea = group
						    , p_srs  = mean(Y[s2 & G])
						    , var_srs = var(Y[s2 & G]) / n_s2G
						    )
			       } else {
				 result <- data.frame(smallArea = group)
			       }
			     if (VIMFOLD){ # commons
			       ## classical model
			       mR_s2G <- sum(R[G & s2]) / n_s2G 
			       ## extended model
			       eR <- Y - sapply(eZ, function(x) x %*% b2.28)  ## a2 p.444
			       a2.30 <- ieA_s2 %*% (Reduce('+', mapply('*', eR[s2]^2
								       , lapply(eZ[s2]
										, function(x) as.numeric(x) %o% as.numeric(x))
								       , SIMPLIFY=FALSE)
			       ) / n_s2 ^ 2
			       ) %*% ieA_s2 ## Sigma_theta_s2

			     }
			     switch(pred.type # commons to non-exhaustive & partially 
				    , 'exhaustive' = {    
				      ## do nothing
				    }
				    , 'non-exhaustive' = ## same as below
				    , 'partially' = {
				      if (length(predictors) == 1){
					mZ_s1G <- c(1, mean(slot(object, "data")[, c(predictors)][s1 & G])) ## a2 p.443, b1 p.15
				      } else {
					mZ_s1G <- c(1, colMeans(slot(object, "data")[, c(predictors)][s1 & G, ])) ## a2 p.443, b1 p.15
				      }
				      a2.34 <- Reduce('+', eZ[s1 & G])/n_s1G ## meZ_s1G
				    }
				    , stop(paste('unkown pred.type', pred.type))
				    )
			     switch(pred.type # uncommons 
				    , 'exhaustive' = {    
				      tmZ_G <- as.numeric(
							  cbind(1
								, subset(slot(object, "smallAreaMeans")[, predictors]
									 , slot(object, "smallAreaMeans")[, smallArea] == group))
							  ) ## a2 p.443
				      tmeZ_G <- c(tmZ_G, 1) ## a1 p.18
				      a2.31 <- tmeZ_G %*% b2.28  ## same as a2.32
				      a2.33 <- tmeZ_G %*% a2.30  %*% tmeZ_G
				      if(ALL){
					## ## ## synth
					a2.18 <- as.numeric(t(a2.13.b) %*% tmZ_G)
					a2.19 <- as.numeric(t(tmZ_G) %*% (a2.15 %*% tmZ_G))
					## ## ## small
					a2.20 <- as.numeric(a2.18 + mR_s2G)
					a2.21 <- as.numeric(a2.19 + var(R[G & s2]) / n_s2G)
					result <- cbind(result
							, p_Gsynth = a2.18, var_Gsynth = a2.19
							, p_Gsmall = a2.20, var_Gsmall = a2.21
							)
				      } 
				      result <- cbind(result
						      ,  a2.31,  a2.33
						      )

				    }
				    , 'non-exhaustive' = { 
				      a2.35 <- a2.34 %*% b2.28 
				      a2.37 <- Reduce('+', lapply(lapply(eZ[s1 & G], function(x) x - a2.34)
								  , function(x) as.numeric(x) %o% as.numeric(x)
								  )) / (n_s1G * (n_s1G - 1))
				      a2.36 <- as.numeric(t(a2.34) %*% a2.30 %*% a2.34 + t(b2.28) %*% a2.37 %*% b2.28)
				      if (ALL){
					## ## ## psynth
					a2.22 <- as.numeric(t(a2.13.b) %*% mZ_s1G)
					a2.24 <- Reduce('+', lapply(
								    lapply(Z[s1 & G]
									   , function(x) x - mZ_s1G)
								    , function(x) as.numeric(x) %o% as.numeric(x)
								    )) / (n_s1G * (n_s1G - 1))
					a2.23 <- as.numeric(mZ_s1G %*% a2.15 %*% mZ_s1G + a2.13.b %*% a2.24 %*% a2.13.b)
					## ## ## psmall
					a2.25 <- a2.22 + mR_s2G
					a2.26 <- a2.23 + var(R[G & s2]) / n_s2G

					result <- cbind(result
							,  a2.22,  a2.23
							, p_Gpsmall = a2.25, var_Gpsmall = a2.26
							)
				      } 
				      result <- cbind(result
						      ,  a2.35,  a2.36
						      )
				    }
				    , 'partially' = {
				      if (length(predictors2) == 0){ ## possible, if s1 is given.`
					mZ1_s1G <- mZ_s1G
				      } else {
					mZ1_s1G <- as.vector(mZ_s1G[-which(names(mZ_s1G) == predictors2)])
				      }
				      b2.29 <- a2.30
				      eR1 <- Y - sapply(eZ1, function(x) x %*% b2.27) # b1 p.18
				      ## eA1_s1 is never given, but by analogy with A1_s2 (from b2.27), A1_s1c (b1 p.21), eA1_s1c (b1 p.22):     
				      eA1_s1 <- Reduce('+', lapply(eZ1[s1], function(x) as.numeric(x) %o% as.numeric(x)))/ n_s1
				      ieA1_s1 <- solve(eA1_s1) 
				      b1.40.b <- ieA1_s1 %*% (Reduce('+', mapply('*', eR1[s2]^2
										 , lapply(eZ1[s2]
											  , function(x) as.numeric(x) %o% as.numeric(x))
										 , SIMPLIFY=FALSE)
				      ) / n_s2 ^ 2
				      ) %*% ieA1_s1  # Sigma_Gamma_s2 b2 p.1016
				      meZ1_s1G <- c(mZ1_s1G ,1) # b2 p.1027
				      if(threePhase){ # we now estimate the smallAreaMeans from s0: c1 p.16. This is the essence of c1.
					if (length(predictors1) == 1){
					  tmeZ1_G  <- c(1, mean(slot(object, "data")[G, predictors1]),  1) 
					} else {
					  tmeZ1_G  <- c(1, colMeans(slot(object, "data")[G, predictors1]),  1) 
					}
				      }
				      b2.30 <- (tmeZ1_G - meZ1_s1G) %*% b2.27 + a2.34 %*% b2.28
				      b2.31 <- n_s2/n_s1 * tmeZ1_G %*% b1.40.b %*% tmeZ1_G +  (1 - n_s2/n_s1) * a2.34 %*% b2.29 %*% a2.34
				      if (threePhase){
					c2.23 <- b2.30 ## we have replaced tmeZ1_G  with its estimate over s0!
					n_s0G <- sum(G) 

					c2.25 <- Reduce('+', lapply(
								    lapply(eZ1[G]
									   , function(x) x - tmeZ1_G)
								    , function(x) as.numeric(x) %o% as.numeric(x)
								    )) / (n_s0G * (n_s0G - 1))
					c2.24 <- b2.31  + t(b2.27) %*% c2.25 %*% b2.27
				      }
				      if (ALL && !threePhase){
					## ## ## psynth
					b2.34 <- (tmZ1_G - mZ1_s1G) %*% b2.5.2 + mZ_s1G %*%  b2.6.2 
					b2.35 <- n_s2/n_s1 * tmZ1_G  %*% b1.29 %*% tmZ1_G + (1 - n_s2/n_s1) *  mZ_s1G  %*% b1.28 %*% mZ_s1G 
					## ## ## psmall
					b2.24 <- b2.34 + mR_s2G 
					b2.23 <- var(R1[s2 & G]) / n_s2G + (1-n_s2G/n_s1G) * var(R[s2 & G]) / n_s2G
					result <- cbind(result
							, p_Gpsmallgreg = b2.24, var_extGpsmallgreg = b2.23
							, p_Gpsynthgreg = b2.34, var_Gpsynthgreg = b2.35
							)
				      } 
				      if (threePhase){
				      result <- cbind(result
						      ,  c2.23,  c2.24
						      )
				      } else {
					result <- cbind(result
							,  b2.30,  b2.31
							)
				      }
	
				    }
				    , stop(paste('unkown pred.type', pred.type))
				    )
			   }
			   out <- rbind(out, result)
			 }
			 comment(out) <- com
			 return(out)
		       }
		       )

