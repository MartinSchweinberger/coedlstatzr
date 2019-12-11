#' @title Multiple ggplots in one Graphics Window
#'
#' @description This function allows the display of multiple ggplots in a single graphics window.
#' @param ... Further options passed to \pkg{ggplot} and \pkg{grid}
#' @param plotlist THIS NEEDS TO BE DOCUMENTED
#' @param file THIS NEEDS TO BE DOCUMENTED
#' @param cols THIS NEEDS TO BE DOCUMENTED
#' @param layout THIS NEEDS TO BE DOCUMENTED
#' @export
#' @keywords ggplot2, multiple plots, function
#' @return NULL
#' @import ggplot2 grid
#' @examples \dontrun{
#' #p1 <- ggplot(data, aes(var1, var2)) +
#' #geom_point()
#' #p2 <- ggplot(data, aes(var1, var2)) +
#' #geom_line()
#' #multiplot(p1, p2, cols = 1)
#' }
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  requireNamespace(ggplot2)
  requireNamespace(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
