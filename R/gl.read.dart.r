##' Import DarT data into R and convert it to a genlight object.
##'
##' This function is a wrapper for \code{\link{read.dart}} and
##' \code{\link{dart2genlight}} that converts a dart file into a genlight object in one step.
##'
##' The file of individual covariates is searched for the columns
##' \describe{
##' \item{\code{id}}{individual identifiers matching the genetic data in the dart object}
##' \item{\code{pop}}{population identifier for each individual}
##' \item{\code{lat}, \code{lon}}{spatial coordinates (perferably in decimal degrees WGS1984 format)}
##' }
##' Of these, only \code{id} is mandatory, but other functions in the
##' package may require metadata provided from DArT, Other covariates
##' contained in the file will be appended to individual covariate
##' dataframe in the genlight object.
##'
##' @title Import DarT data as genlight object.
##' @param filename DarT data in csv format.
##' @param ind.metafile name of csv file with covariates for each individual.
##' @param na.strings strings representing NAs.
##' @param skip number of lines before the row of column names. If \code{NULL} it is diagnosed from the file.
##' @param last.metric the name of the last column of preceeding the genetic data
##' @param nrows the DarT format.  If \code{NULL} it is diagnosed from the file.
##' @return A genlight object.
##' @seealso \code{\link{read.dart}}, \code{\link{dart2genlight}}
##' @author Bernd Gruber (bugs? Post to \url{https://groups.google.com/d/forum/dartr})
##' @export
gl.read.dart <- function(filename, ind.metafile=NULL, na.strings = "-", skip=NULL, last.metric="RepAvg",nrows=NULL) {
  dart <- utils.read.dart(filename = filename, na.strings=na.strings, skip=skip, last.metric = last.metric,nrows=nrows)
  utils.dart2genlight(dart, ind.metafile = ind.metafile)
}
