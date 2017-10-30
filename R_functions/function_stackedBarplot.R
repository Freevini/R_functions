#  script for plotting ARISA
plotDataStackedLattice <- function (data, color = NULL){
  if(identical(color, NULL)) color <- hsv( 1:ncol(data)/ncol(data), s=0.8)
  print(barchart( data, stack=TRUE,  par.strip.text=list(cex=0.5),
            key=list(space="left", rectangles=list(col=color), text=list(colnames(data))), 
            col=color, xlab="relative proportion"))
}
