##' Produce a scree plot from a PCoA analysis
##'
##' Plot the contribution each principal component makes to the total
##' variance, expressed as a percentage.
##' @title PCoA Scree Plot
##' @param glPca a glPca object
##' @param n number of components to display
##' @return a ggplot object.
##' @importFrom ggplot2 ggplot aes_string geom_point ggtitle labs
##' @author Arthur Georges (bugs? Post to \url{https://groups.google.com/d/forum/dartr})
##' @export
gl.pcoa.scree <- function(glPca,n=length(glPca$eig)) {

  ggplot(data.frame(x=seq_len(n),
                    y=(100/sum(glPca$eig))*glPca$eig[seq_len(n)]),
         aes_string(x="x",y="y"))+
    geom_point()+
    ggtitle("Scree plot for PCoA") +
    labs(x="Component", y="Percentage Contribution")
}
