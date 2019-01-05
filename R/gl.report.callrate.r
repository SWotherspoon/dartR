#' Report summary of Call Rate for loci or individuals
#'
#' SNP datasets generated by DArT have missing values primarily arising from failure to call a SNP because of a mutation
#' at one or both of the the restriction enzyme recognition sites. This script reports the number of missing values for each
#' of several percentiles. The script gl.filter.callrate() will filter out the loci with call rates below a specified threshold.
#'
#' A histogram and or a smearplot can be requested. Note that the smearplot is computationally intensive, and will take time to
#' execute on large datasets.
#'
#' @param x -- name of the genlight or genind object containing the SNP data [required]
#' @param method specify the type of report by locus (method="loc") or individual (method="ind") [default method="loc"]
#' @param plot specify if a histogram of call rate is to be produced [default FALSE]
#' @param smearplot if TRUE, will produce a smearplot of individuals against loci [default FALSE]
#' @return Tabulation of CallRate against Threshold
#' @importFrom adegenet glPlot
#' @importFrom graphics hist
#' @export
#' @author Arthur Georges (Post to \url{https://groups.google.com/d/forum/dartr})
#' @examples
#' gl.report.callrate(testset.gl)


gl.report.callrate <- function(x, method="loc", plot=FALSE, smearplot=FALSE) {

# ERROR CHECKING

  if(class(x)!="genlight") {
    cat("Fatal Error: genlight object required for gl.report.callrate!\n"); stop()
  }
  # Work around a bug in adegenet if genlight object is created by subsetting
  x@other$loc.metrics <- x@other$loc.metrics[1:nLoc(x),]

  if (method != "ind" & method != "loc") {
    cat("    Warning: method must be either \"loc\" or \"ind\", set to \"loc\" \n")
    method <- "loc"
  }

# FLAG SCRIPT START

    cat("Starting gl.report.callrate: Reporting distribution of Call Rate\n")

# RECALCULATE THE CALL RATE, BRING IT UP TO DATE IN CASE gl.recalc.metrics HAS NOT BEEN RUN

    x <- utils.recalc.callrate(x, v=1)

# FOR METHOD BASED ON LOCUS

  if(method == "loc") {

  # Plot a histogram of Call Rate

    if (plot) {
      par(mfrow = c(2, 1),pty="m")
      hist(x@other$loc.metrics$CallRate,
         main="Call Rate by Locus",
         xlab="Call Rate",
         border="blue",
         col="red",
         xlim=c(min(x@other$loc.metrics$CallRate),1),
         breaks=100)
    }
    if(smearplot){
      glPlot(x)
    }

  # Print out some statistics
    cat("Reporting Call Rate by Locus\n")
    cat("No. of loci =", nLoc(x), "\n")
    cat("No. of individuals =", nInd(x), "\n")
    cat("  Miniumum Call Rate: ",round(min(x@other$loc.metrics$CallRate),2),"\n")
    cat("  Maximum Call Rate: ",round(max(x@other$loc.metrics$CallRate),2),"\n")
    cat("  Average Call Rate: ",round(mean(x@other$loc.metrics$CallRate),3),"\n")
    cat("  Missing Rate Overall: ",round(sum(is.na(as.matrix(x)))/(nLoc(x)*nInd(x)),2),"\n\n")

  # Determine the loss of loci for a given filter cut-off
    retained <- array(NA,21)
    pc.retained <- array(NA,21)
    filtered <- array(NA,21)
    pc.filtered <- array(NA,21)
    percentile <- array(NA,21)
    crate <- x@other$loc.metrics$CallRate
    for (index in 1:21) {
      i <- (index-1)*5
      percentile[index] <- i/100
      retained[index] <- length(crate[crate>=percentile[index]])
      pc.retained[index] <- round(retained[index]*100/nLoc(x),1)
      filtered[index] <- nLoc(x) - retained[index]
      pc.filtered[index] <- 100 - pc.retained[index]
    }
    df <- cbind(percentile,retained,pc.retained,filtered,pc.filtered)
    df <- data.frame(df)
    colnames(df) <- c("Threshold", "Retained", "Percent", "Filtered", "Percent")
    df <- df[order(-df$Threshold),]
    rownames(df) <- NULL
    print(df)
  }

# FOR METHOD BASED ON INDIVIDUAL

  if(method == "ind") {

    # Calculate the call rate by individual
    ind.call.rate <- 1 - rowSums(is.na(as.matrix(x)))/nLoc(x)

    # Plot a histogram and smearplot of Call Rate

    if (plot) {
      par(mfrow = c(2, 1),pty="m")
      hist(ind.call.rate,
        main="Call Rate by Individual",
        xlab="Call Rate",
        border="blue",
        col="red",
        xlim=c(min(ind.call.rate),1),
        breaks=100)
    }
    if(smearplot){
      glPlot(x)
    }

    cat("Reporting Call Rate by Individual\n")
    cat("No. of loci =", nLoc(x), "\n")
    cat("No. of individuals =", nInd(x), "\n")
    cat("  Miniumum Call Rate: ",round(min(ind.call.rate),2),"\n")
    cat("  Maximum Call Rate: ",round(max(ind.call.rate),2),"\n")
    cat("  Average Call Rate: ",round(mean(ind.call.rate),3),"\n")
    cat("  Missing Rate Overall: ",round(sum(is.na(as.matrix(x)))/(nLoc(x)*nInd(x)),2),"\n\n")

    # Determine the loss of individuals for a given filter cut-off
    retained <- array(NA,21)
    pc.retained <- array(NA,21)
    filtered <- array(NA,21)
    pc.filtered <- array(NA,21)
    percentile <- array(NA,21)
    crate <- ind.call.rate
    for (index in 1:21) {
      i <- (index-1)*5
      percentile[index] <- i/100
      retained[index] <- length(crate[crate>=percentile[index]])
      pc.retained[index] <- round(retained[index]*100/nInd(x),1)
      filtered[index] <- nInd(x) - retained[index]
      pc.filtered[index] <- 100 - pc.retained[index]
    }
    df <- cbind(percentile,retained,pc.retained,filtered,pc.filtered)
    df <- data.frame(df)
    colnames(df) <- c("Threshold", "Retained", "Percent", "Filtered", "Percent")
    df <- df[order(-df$Threshold),]
    rownames(df) <- NULL
    print(df)
  }

# FLAG SCRIPT END
    cat("\ngl.report.callrate Completed\n")

  return(df)

}
