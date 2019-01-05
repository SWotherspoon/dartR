#' Filter loci or specimens in a genlight \{adegenet\} object based on call rate
#'
#' SNP datasets generated by DArT have missing values primarily arising from failure to call a SNP because of a mutation
#' at one or both of the the restriction enzyme recognition sites. This script filters out loci (or specimens) for which the call rate is
#' lower than a specified value. The script will also filter out loci (or specimens) in SilicoDArT (presence/absence) datasets where the call rate
#' is lower than the specified value. In this case, the data are missing owing to low coverage.
#'
#' Because this filter operates on call rate, and previously applied functions may not have recalculated locus metrics, this function recalculates Call Rate before filtering.
#' Recalculaton after filtering remains optional, with no recalculation as the default.
#'
#' Note that when filtering individuals on call rate, the initial call rate is calculated and compared against the threshold. After filtering, if mono.rm=TRUE, the removal of
#' monomorphic loci will alter the call rates. Some individuals with a call rate initially greater than the nominated threshold, and so retained, may come to have a call rate
#' lower than the threshold. If this is a problem, repeated iterations of this function will resolve the issue. This is done by setting mono.rm=TRUE and recursive=TRUE,
#' or it can be done manually.
#'
#' @param  x name of the genlight object containing the SNP data, or the genind object containing the SilocoDArT data [required]
#' @param method -- "loc" to specify that loci are to be filtered, "ind" to specify that specimens are to be filtered [default "loc"]
#' @param threshold -- threshold value below which loci will be removed [default 0.95]
#' @param plot specify if histograms of call rate, before and after, are to be produced [default FALSE]
#' @param bins -- number of bins to display in histograms [default 25]
#' @param mono.rm -- Remove monomorphic loci [default FALSE]
#' @param recalc -- Recalculate the locus metadata statistics if any individuals are deleted in the filtering [default FALSE]
#' @param recursive -- Repeatedly filter individuals on call rate, each time removing monomorphic loci. Only applies if method="ind" and mono.rm=TRUE [default FALSE]
#' @param v -- verbosity: 0, silent or fatal errors; 1, begin and end; 2, progress log ; 3, progress and results summary; 5, full report [default 2]
#' @return The reduced genlight or genind object, plus a summary
#' @export
#' @author Arthur Georges (Post to \url{https://groups.google.com/d/forum/dartr})
#' @examples
#' result <- gl.filter.callrate(testset.gl, plot=TRUE, method="ind", threshold=0.8, v=3)


 gl.filter.callrate <- function(x, method="loc", threshold=0.95, mono.rm=FALSE, recalc=FALSE, recursive=FALSE, plot=FALSE, bins=25, v=2) {

# ERROR CHECKING

   if(class(x)!="genlight") {
     cat("Fatal Error: genlight object required!\n"); stop("Execution terminated\n")
   }

   if (v < 0 | v > 5){
     cat("    Warning: verbosity must be an integer between 0 [silent] and 5 [full report], set to 2\n")
     v <- 2
   }

   if (method != "ind" & method != "loc") {
     cat("    Warning: method must be either \"loc\" or \"ind\", set to \"loc\" \n")
     method <- "loc"
   }

   if (threshold < 0 | threshold > 1){
     cat("    Warning: threshold must be an integer between 0 and 1, set to 0.95\n")
     threshold <- 0.95
   }

# FLAG SCRIPT START

   if (v >= 1) {cat("Starting gl.filter.callrate: Filtering on Call Rate\n")}
   if (v >= 3) {cat("Note: Missing values most commonly arise from restriction site mutation\n")}

# RECALCULATE THE CALL RATE, BRING IT UP TO DATE IN CASE gl.recalc.metrics HAS NOT BEEN RUN

   x <- utils.recalc.callrate(x, v=v)

# FOR METHOD BASED ON LOCUS

  if( method == "loc" ) {
    # Determine starting number of loci and individuals
    if (v >= 2) {cat("  Removing loci based on Call Rate, threshold =",threshold,"\n")}
    n0 <- nLoc(x)
    if (v >= 3) {cat("Initial no. of loci =", n0, "\n")}

    # Remove loci with NA count <= 1-threshold
      x2 <- x[ ,glNA(x,alleleAsUnit=FALSE)<=((1-threshold)*nInd(x))]
      x2@other$loc.metrics <- x@other$loc.metrics[glNA(x,alleleAsUnit=FALSE)<=((1-threshold)*nInd(x)),]
      if (v > 2) {cat ("  No. of loci deleted =", (n0-nLoc(x2)),"\n")}

      # Plot a histogram of Call Rate

      if (plot) {
        par(mfrow = c(2, 1),pty="m")
        hist(x@other$loc.metrics$CallRate,
             main="Call Rate by Locus\n[pre-filtering]",
             xlab="Call Rate",
             border="blue",
             col="red",
             xlim=c(min(x@other$loc.metrics$CallRate),1),
             breaks=bins
        )

       hist(x2@other$loc.metrics$CallRate,
             main="Call Rate by Locus\n[post-filtering]",
             xlab="Call Rate",
             border="blue",
             col="red",
             xlim=c(min(x2@other$loc.metrics$CallRate),1),
             breaks=bins
        )
      }

# FOR METHOD BASED ON INDIVIDUALS

  } else if ( method == "ind" ) {

    # Determine starting number of loci and individuals
    if (v > 1) {cat("  Removing individuals based on Call Rate, threshold =",threshold,"\n")}
      n0 <- nInd(x)
    if (v > 2) {cat("Initial no. of individuals =", n0, "\n")}

    # Calculate the individual call rate
      ind.call.rate <- 1 - rowSums(is.na(as.matrix(x)))/nLoc(x)
    # Store the initial call rate profile
      hold <- ind.call.rate
    # Check that there are some individuals left
      if (sum(ind.call.rate >= threshold) == 0) stop(paste("Maximum individual call rate =",max(ind.call.rate),". Nominated threshold of",threshold,"too stringent.\n No individuals remain.\n"))

      if (!recursive) {
    # Extract those individuals with a call rate greater or equal to the threshold
      x2 <- x[ind.call.rate >= threshold,]

    # for some reason that eludes me, this also (appropriately) filters the latlons and the covariates, but see above for locus filtering
        if (v > 2) {cat ("Filtering a genlight object\n  no. of individuals deleted =", (n0-nInd(x2)), "\nIndividuals retained =", nInd(x2),"\n")}

    # Report individuals that are excluded on call rate
      if (any(ind.call.rate <= threshold)) {
        x3 <- x[ind.call.rate <= threshold,]
        if (length(x3) > 0) {
          if (v >= 2) {
            cat("  No. of individuals deleted (CallRate <= ",threshold,":\n")
            cat(paste0(indNames(x3),"[",as.character(pop(x3)),"],"))
          }
            # Remove monomorphic loci
              if (mono.rm) {x2 <- gl.filter.monomorphs(x2,v=v)}
              if (recalc) { x2 <- gl.recalc.metrics(x2, v=v)}
        }
      }
    # Recalculate the callrate
      ind.call.rate <- 1 - rowSums(is.na(as.matrix(x2)))/nLoc(x2)
      # cat(min(ind.call.rate),"\n")

    } else { # If recursive

      # Recursively remove individuals
      for (i in 1:10) {
        # Recalculate the callrate
        ind.call.rate <- 1 - rowSums(is.na(as.matrix(x)))/nLoc(x)
        # Extract those individuals with a call rate greater or equal to the threshold
        x2 <- x[ind.call.rate >= threshold,]
        if (nInd(x2) == nInd(x)) {break}

        # for some reason that eludes me, this also (appropriately) filters the latlons and the covariates, but see above for locus filtering
        if (v > 2) {cat ("ITERATION",i,"\n  No. of individuals deleted =", (n0-nInd(x2)), "\n  No. of individuals retained =", nInd(x2),"\n")}

        # Report individuals that are excluded on call rate
        if (any(ind.call.rate <= threshold)) {
          x3 <- x[ind.call.rate <= threshold,]
          if (length(x3) > 0) {
            if (v >= 2) {
              cat("  List of individuals deleted (CallRate <= ",threshold,":\n")
              cat(paste0(indNames(x3),"[",as.character(pop(x3)),"],"))
            }
            # Remove monomorphic loci
            if (mono.rm) {x2 <- gl.filter.monomorphs(x2,v=v)}
            if (recalc) { x2 <- gl.recalc.metrics(x2, v=v)}
          }
        }

        x <- x2

      }
    }

      # Plot a histogram of Call Rate

      if (plot) {
        par(mfrow = c(2, 1),pty="m")

        hist(hold,
             main="Call Rate by Individual\n[pre-filtering]",
             xlab="Call Rate",
             border="blue",
             col="red",
             breaks=bins,
             xlim=c(min(hold),1)
        )

       hist(ind.call.rate,
             main="Call Rate by Individual\n[post-filtering]",
             xlab="Call Rate",
             border="blue",
             col="red",
             breaks=bins,
             xlim=c(min(ind.call.rate),1)
        )
      }
   }

# REPORT A SUMMARY
   if (v > 2) {
     cat("\nSummary of filtered dataset\n")
     cat(paste("  Call Rate >",threshold,"\n"))
     cat(paste("  No. of loci:",nLoc(x2),"\n"))
     cat(paste("  No. of individuals:", nInd(x2),"\n"))
     cat(paste("  No. of populations: ", length(levels(factor(pop(x2)))),"\n"))
   }

   if (v >= 2) {
     if (!recalc & method=="ind") {
       cat("Note: Locus metrics not recalculated\n")
     } else {
       cat("Note: Locus metrics recalculated\n")
     }
     if (!mono.rm & method=="ind") {
       cat("Note: Resultant monomorphic loci not deleted\n")
     } else{
       cat("Note: Resultant monomorphic loci deleted\n")
       if (!recursive) {cat("  Warning: Some individuals with a CallRate initially >=",threshold,"may have a CallRate lower than",threshold,"when call rate is recalculated after removing resultant monomorphic loci\n")}
     }
   }

   if ( v > 0) {cat("\ngl.filter.callrate completed\n")}

    return(x2)
 }
