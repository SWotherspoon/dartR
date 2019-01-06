##' Plot a PCoA ordination.
##'
##' The factor scores are taken from the output of gl.pcoa() -- an
##' object of class glPca -- and the population assignments are taken
##' from from the original data file. The specimens are shown in a
##' bivariate plot optionally with adjacent labels and enclosing
##' ellipses. Population labels on the plot are shuffled so as not to
##' overlap (using package \{directlabels\}).  This can be a bit
##' clunky, as the labels may be some distance from the points to
##' which they refer, but it provides the opportunity for moving
##' labels around using graphics software (Adobe Illustrator).
#'
##' Any pair of axes can be specified from the ordination, provided
##' they are within the range of the nfactors value provided to
##' gl.pcoa(). Axes can be scaled to represent the proportion of
##' variation explained. In any case, the proportion of variation
##' explained by each axis is provided in the axis label.
##'
##'Points displayed in the ordination can be identified if the option
##' labels="interactive" is chosen, in which case the resultant plot
##' is ggplotly() friendly. Running ggplotyly() with no parameters
##' will replot the data and allow identification of points by moving
##' the mouse over them. Refer to the plotly package for further
##' information. Do not forget to load the library via
##' library(plotly).
##'
##' @title PCoA ordination plot
##' @param glPca a glPca object
##' @param gl a genlight object
##' @param scale logical indicating whether the x and y axes are to be scaled in proportion to \% variation explained
##' @param ellipse logical indicating to display tolerance ellipses for ach population
##' @param level quantiles for tolerance ellipses
##' @param labels the type of labelling to be applied.
##' @param xaxis the component to plot as the x axis
##' @param yaxis the component to plot as the y axis
##' @importFrom ggplot2 ggplot aes geom_point geom_hline geom_vline labs theme coord_fixed stat_ellipse element_text
##' @importFrom directlabels geom_dl
##' @return A ggplot object.
##' @author Arthur Georges (bugs? Post to \url{https://groups.google.com/d/forum/dartr})
##' @export
gl.pcoa.plot <- function(glPca, gl, scale=FALSE, ellipse=FALSE, level=0.95,
                         labels=c("pop","none", "ind", "interactive", "legend"),
                         xaxis=1, yaxis=2) {

  labels <- match.arg(labels)

  # Labels for the axes
  eig <- 100/sum(glPca$eig)*glPca$eig
  xlab <- paste("PCoA Axis", xaxis, "(",round(eig[xaxis],1),"%)")
  ylab <- paste("PCoA Axis", yaxis, "(",round(eig[yaxis],1),"%)")

  df <- data.frame(x=glPca$scores[,xaxis],
                   y=glPca$scores[,yaxis],
                   ind=as.character(indNames(gl)),
                   pop=as.character(pop(gl)))

  if (labels == "ind")
    p <- ggplot(df, aes_string(x="x",y="y",colour="pop",group="ind",label="ind"))+
      geom_point(size=2) +
      geom_dl(method="first.points")

  if (labels == "pop")
    p <- ggplot(df, aes_string(x="x",y="y",colour="pop",group="pop",label="pop"))+
      geom_point(size=2) +
      geom_dl(method="smart.grid")

  if (labels=="interactive" || labels=="ggplotly")
    p <- ggplot(df, aes_string(x="x",y="y",colour="pop",fill="ind"))+
      geom_point(size=2)

  if (labels == "legend")
    p <- ggplot(df, aes_string(x="x",y="y",colour="pop"))+
      geom_point(size=2)

  if (labels == "none")
    p <- ggplot(df, aes_string(x="x",y="y",colour="pop"))+
      geom_point(size=2)

  p <- p +
    labs(x=xlab, y=ylab) +
    geom_hline(yintercept=0) +
    geom_vline(xintercept=0) +
    theme(axis.title=element_text(face="bold.italic",size="20", color="black"),
          axis.text.x  = element_text(face="bold",angle=0, vjust=0.5, size=10),
          axis.text.y  = element_text(face="bold",angle=0, vjust=0.5, size=10),
          legend.title = element_text(colour="black", size=18, face="bold"),
          legend.text = element_text(colour="black", size = 16, face="bold"))
  if(!labels %in% c("pop","interactive","ggplotly","none"))
    p <- p + theme(legend.position="none")

  # Scale the axes in proportion to % explained, if requested
  if(scale) p <- p + coord_fixed(ratio=eig[yaxis]/eig[xaxis])
  # Add ellipses if requested
  if(ellipse) p <- p + stat_ellipse(type="norm", level=0.95)

  p
}
