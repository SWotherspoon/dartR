#' Filter loci that represent secondary SNPs in a genlight \{adegenet\} object
#'
#' SNP datasets generated by DArT include fragments with more than one SNP and record them 
#' separately with the same CloneID (=AlleleID).
#' These multiple SNP loci within a fragment (secondaries) are likely to be linked, 
#' and so you may wish to remove secondaries.
#' This script filters out loci after ordering the genlight object on based on 
#' repeatability, avgPIC in that order (method="best") or at random (method="random")
#'
#' @param x -- name of the genlight object containing the SNP data [required]
#' @param method -- method of selecting SNP locus to retain, best or random [random]
#' @param v -- verbosity: 0, silent or fatal errors; 1, begin and end; 2, progress log ; 3, progress and results summary; 5, full report [default 2]
#' @return The reduced genlight, plus a summary
#' @export
#' @author Arthur Georges (Post to \url{https://groups.google.com/d/forum/dartr})
#' @examples
#' result <- gl.report.secondaries(testset.gl)
#' result2 <- gl.filter.secondaries(testset.gl)

# Last edit:25-Apr-18

gl.filter.secondaries <- function(x, method="random", v=2) {

  # ERROR CHECKING
  
  if(class(x)!="genlight") {
    cat("Fatal Error: genlight object required!\n"); stop("Execution terminated\n")
  }
  
  if (v < 0 | v > 5){
    cat("    Warning: verbosity must be an integer between 0 [silent] and 5 [full report], set to 2\n")
    v <- 2
  }
  
  if (method != "best" && method != "random"){
    cat("    Warning: method must be specified, set to \'random\'\n")
  }

  if (v > 0) {
    cat("Starting gl.filter.secondaries: Deleting all but one SNP per sequence tag\n")
  }
  
  if (v > 2) {cat("  Total number of SNP loci:",nLoc(x),"\n")}
  
# Sort the genlight object on AlleleID (asc), RepAvg (desc), AvgPIC (desc) 
  if (method == "best") {
    if (v > 1){cat("  Selecting one SNP per sequence tag based on best RepAvg and AvgPIC\n")}
    loc.order <- order(x@other$loc.metrics$AlleleID,-x@other$loc.metrics$RepAvg,-x@other$loc.metrics$AvgPIC)
    x <- x[, loc.order]
    x@other$loc.metrics <- x@other$loc.metrics[loc.order, ]
  } else {
    if (v > 1){cat("  Selecting one SNP per sequence tag at random\n")}
    n <- length(x@other$loc.metrics$AlleleID)
    index <- sample(1:(n+10000),size=n,replace=FALSE)
    x <- x[,order(index)]
    x@other$loc.metrics <- x@other$loc.metrics[order(index), ]
  }
# Extract the clone ID number
  a <- strsplit(as.character(x@other$loc.metrics$AlleleID),"\\|")
  b <- unlist(a)[ c(TRUE,FALSE,FALSE) ]
# Identify and remove secondaries from the genlight object, including the metadata
  x <- x[,duplicated(b)==FALSE]
  x@other$loc.metrics <- x@other$loc.metrics[duplicated(b)==FALSE,]

# Report secondaries from the genlight object
  if (v > 2) {
    if (is.na(table(duplicated(b))[2])) { 
      nsec <- 0
    } else {  
      nsec <- table(duplicated(b))[2]
    }
    cat("    Number of secondaries:",nsec,"\n")
    cat("    Number of loci after secondaries removed:",table(duplicated(b))[1],"\n")
  }
  
  if ( v > 0) {cat("gl.filter.secondaries completed\n")}
  
  return(x)
}  