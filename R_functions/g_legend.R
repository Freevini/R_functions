###===============
##used to create only ggplot legends in a grid arrange plot
###===============

##-------------
##usage
##-------------

# 1. create a plot with legend(=plot1)
# plot1 = ggplot(diamonds, aes(clarity, fill = color)) + 
# geom_bar() + 
#   facet_wrap(~cut, nrow = 1)
# 2. Then create the g_legend(plot1)
#  legend <- g_legend(plot1)
# 3. then grid.arrange()
# grid.arrange(legend, plot1+ theme(legend.position = 'none'), 
#              ncol=2, nrow=1, widths=c(1/6,5/6))



library(gridExtra)
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}
