#' @title   Standardizes the matrix given as input
#' @description This function takes as input a matrix of numeric values and then transforms it
#'              so that each column has a mean of zero and a variance of one
#'
#' @name    standardize
#' @param   xx  Matrix or a data frame of numeric entries
#' @return      Matrix with columns that have mean zero and variance one
#' @examples \dontrun{
#' # I don't want you to run this
#' }
#' n<-450; x <- data.frame(cbind(rnorm(n, 162, 4), rnorm(n, 108, 2),
#' rnorm(n, 117, 3), rnorm(n, 36, 2), rnorm(n, 45, 2)))
#' p <- ncol(x)
#' x.stan <- standardize(x)
#' round(head(x),2)
#' round(head(x.stan),2)
#' round(rbind(apply(x, 2, mean), apply(x.stan, 2, mean)),2)
#' round(rbind(apply(x, 2, sd),apply(x.stan, 2, sd)),2)
#' par(mfrow=c(1,2))
#' boxplot(x[,1:min(5,p)], main='Original Data', col=rainbow(9))
#' boxplot(x.stan[,1:min(5,p)], main='PreProcessed Data', col=rainbow(7))
#' @export
standardize <-function(xx)
{
  n <- nrow(xx)
  p <- ncol(xx)
  aa  <- matrix(rep(apply(xx,2,mean), n), ncol=p, byrow=TRUE)
  bb  <- sqrt(matrix(rep(apply(xx,2,var), n), ncol=p, byrow=TRUE))
  return((xx-aa)/bb)
}


#' @title   Unitizes the matrix given as input
#'
#' @name    unitize
#' @param   xx Matrix or a data frame of numeric entries
#' @return     Matrix with columns that have mean zero and length one
#' @examples \dontrun{
#' # I don't want you to run this
#' }
#' n<-450; x <- data.frame(cbind(rnorm(n, 162, 4), rnorm(n, 108, 2),
#' rnorm(n, 117, 3), rnorm(n, 36, 2), rnorm(n, 45, 2)))
#' p <- ncol(x)
#' x.unit <- unitize(x)
#' round(head(x),2)
#' round(head(x.unit),2)
#' round(rbind(apply(x, 2, mean), apply(x.unit, 2, mean)),2)
#' round(rbind(apply(x, 2, sd),apply(x.unit, 2, sd)),2)
#' par(mfrow=c(1,2))
#' boxplot(x[,1:min(5,p)], main='Original Data', col=rainbow(9))
#' boxplot(x.unit[,1:min(5,p)], main='PreProcessed Data', col=rainbow(7))
#' @export
unitize <-function(xx)
{
  n <- nrow(xx)
  p <- ncol(xx)
  aa  <- matrix(rep(apply(xx,2,mean), n), ncol=p, byrow=TRUE)
  bb  <- sqrt((n-1)*matrix(rep(apply(xx,2,var), n), ncol=p, byrow=TRUE))
  return((xx-aa)/bb)
}


#' @title   Cubitizes the matrix given as input
#'
#' @name    cubitize
#' @param   xx Matrix or a data frame of numeric entries
#' @return     Matrix with columns that have minimum zero and maximum one
#' @examples \dontrun{
#' # I don't want you to run this
#' }
#' n<-450; x <- data.frame(cbind(rnorm(n, 162, 4), rnorm(n, 108, 2),
#' rnorm(n, 117, 3), rnorm(n, 36, 2), rnorm(n, 45, 2)))
#' p <- ncol(x)
#' x.cube <- cubitize(x)
#' round(head(x),2)
#' round(head(x.cube),2)
#' round(rbind(apply(x, 2, min), apply(x.cube, 2, min)),2)
#' round(rbind(apply(x, 2, max),apply(x.cube, 2, max)),2)
#' par(mfrow=c(1,2))
#' boxplot(x[,1:min(5,p)], main='Original Data', col=rainbow(9))
#' boxplot(x.cube[,1:min(5,p)], main='PreProcessed Data', col=rainbow(7))
#' @export
cubitize <-function(xx)
{
  n <- nrow(xx)
  p <- ncol(xx)
  aa  <- matrix(rep(apply(xx,2,min), n), ncol=p, byrow=TRUE)
  bb  <- matrix(rep(apply(xx,2,max), n), ncol=p, byrow=TRUE)
  return((xx-aa)/(bb-aa))
}


#' @title   Intervalizes the matrix given as input
#'
#' @name    intervalize
#' @param   xx Matrix or a data frame of numeric entries
#' @param   a  lower bound of the target interval
#' @param   b  upper bound of the target interval
#' @return     Matrix with columns that have minimum zero and maximum one
#' @examples \dontrun{
#' # I don't want you to run this
#' }
#' n<-450; x <- data.frame(cbind(rnorm(n, 162, 4), rnorm(n, 108, 2),
#' rnorm(n, 117, 3), rnorm(n, 36, 2), rnorm(n, 45, 2)))
#' p <- ncol(x)
#' x.inter <- intervalize(x,a=-1,b=1)
#' round(head(x),2)
#' round(head(x.inter),2)
#' round(rbind(apply(x, 2, min), apply(x.inter, 2, min)),2)
#' round(rbind(apply(x, 2, max),apply(x.inter, 2, max)),2)
#' par(mfrow=c(1,2))
#' boxplot(x[,1:min(5,p)], main='Original Data', col=rainbow(9))
#' boxplot(x.inter[,1:min(5,p)], main='PreProcessed Data', col=rainbow(7))
#' @export
intervalize <-function(xx, a=-1, b=1)
{
  n <- nrow(xx)
  p <- ncol(xx)
  aa  <- matrix(rep(apply(xx,2,min), n), ncol=p, byrow=TRUE)
  bb  <- matrix(rep(apply(xx,2,max), n), ncol=p, byrow=TRUE)
  alf <- (b-a)/(bb-aa)
  return(alf*xx + ((b+a-alf*(aa+bb))/2))
}
