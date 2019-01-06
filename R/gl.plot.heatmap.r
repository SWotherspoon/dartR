#' Represent a distance matrix as a heatmap
#'
#' The script plots a heat map to represent the distances in the distance or dissimilarity matrix
#'
#' @param D -- name of the distance matrix [required]
#' @param ncolors -- number of colors to display [default 5]
#' @param labels -- if TRUE, and the number of rows is <= 20, labels are added to the heatmap [default = TRUE]
#' @param labels.cex -- size of the labels [default = 1]
#' @param values -- if TRUE, and the number of rows is <= 20, distances are added to the body of the heatmap [default = TRUE]
#' @param values.cex -- size of the values to print inside each cell [default = 1]
#' @param legend -- if TRUE, a legend will be added to the plot [default = TRUE]
#' @param rank -- if TRUE, then the distance matrix will be reordered to group like with like, otherwise order will be displayed as given [default FALSE]
#' @param v -- verbosity: 0, silent or fatal errors; 1, begin and end; 2, progress log ; 3, progress and results summary; 5, full report [default 2]
#' @importFrom graphics image plot.new legend
#' @importFrom stats dist
#' @importFrom grDevices heat.colors
#' @return NULL
#' @export
#' @author Arthur Georges (Post to \url{https://groups.google.com/d/forum/dartr})
#' @examples
#'    gl <- testset.gl[1:10,]
#'    d <- dist(as.matrix(gl),upper=TRUE,diag=TRUE)
#'    gl.plot.heatmap(d)
#'    gl.plot.heatmap(d, ncolors=10, rank=TRUE, legend=TRUE)

gl.plot.heatmap <- function(D, ncolors=5, labels=TRUE, labels.cex=1, values=TRUE, values.cex=1, legend=TRUE, rank=FALSE, v=2){

# ERROR CHECKING

  if(class(D)!="dist") {
    cat("Fatal Error: distance matrix required!\n"); stop("Execution terminated\n")
  }

  if (v < 0 | v > 5){
    cat("    Warning: verbosity must be an integer between 0 [silent] and 5 [full report], set to 2\n")
    v <- 2
  }

  if (ncolors < 0){
    cat("    Warning: ncolors must be a positive integer, set to 5\n")
    ncolors <- 5
  }
  ncolors <- ncolors + 1 # to account for zero = white.

  if (max(D) == 0) {
    cat("    Warning: matrix contains no nonzero distances\n")
  }

# FLAG SCRIPT START

  if (v >= 1) {
    cat("Starting gl.plot.heatmap: Displaying distance matrix\n")
  }

# DO THE JOB

  # Convert the distance matrix to a numeric matrix
  x <- as.matrix(D) # Note converts to a full matrix, upper and lower
  #x <- x[1:10,1:10]
  dim <- ncol(x)

  # If the matrix is to be ordered on rank
  if(rank){
    x <- x[order(rowMeans(x),decreasing=TRUE),order(colMeans(x),decreasing=TRUE)]
  }

  # Check if labels and values can be plotted
  if (dim >= 30){
    if (labels){
      cat("    Warning: too many labels to display (more than 30); consider setting labels=FALSE\n")
      # labels=FALSE
    }
    if (values){
      cat("    Warning: too many cells to display values within (more than 30x30), conider setting values=FALSE\n")
      #values=FALSE
    }
  }

  # Hold the raw values
  vals <- x

  # Scale the values to fall between 0 and 1
  if (max(x) > 0){
    x <- 1-x/max(x)
  }

  # Invert the matrix so the diagonal runs top left to bottom right
  x <- apply(x, 2, rev)
  vals <- apply(vals,2,rev)

  # Plot the heat map
  #par(pty="s",mar=c(0,0,0,11),xpd=T)
  layout(matrix(c(1,2), 1, 2, byrow = TRUE),widths=c(0.85,0.15))
  par(mai=c(2,2,0,0),pty="s",xpd=T)
  # colours <- heat.colors(ncolors)
  colours <- c(heat.colors(ncolors)[2:ncolors],"#FFFFFFFF")
  image(1:dim, 1:dim, x, axes = FALSE, xlab="", ylab="", col=colours)

  # Add the labels
  if (labels) {
    axis(1, 1:dim, row.names(x), cex.axis=labels.cex, las=3)
    axis(2, 1:dim, colnames(x), cex.axis=labels.cex, las=1)
  }

  # Add the values
  if (values) {
    text(expand.grid(1:dim, 1:dim), sprintf("%0.1f", vals), cex=values.cex)
  }

  # Add the legend
  if (legend) {
    par(mai=c(0.2,0,0.5,0.1),pty="m",xpd=F)
    plot.new()
    bin <- (max(as.matrix(d))-min(as.matrix(d)))/ncolors
    series <- seq(from=0, to=max(d), by=bin)
    series=as.character(signif(series,2))
    series.offset <- series[2:length(series)]
    s <- array(NA,length(series)-1)
    for (i in 1:(length(series)-1)) {
      s[i] <- paste(series[i],"-",series[i+1])
    }
    #legend(x=10.7,y=10,legend=s, fill=rev(colours), title="Distance")
    legend("topright",legend=s, fill=rev(colours), title="Distance", cex=0.7)
  }

  # FLAG SCRIPT END

  if (v >= 1) {
    cat("Completed gl.plot.heatmap\n\n")
  }

  return()

}
