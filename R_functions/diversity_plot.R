diversity_plot <- function(d,response, explanatory1, explanatory2,
                      mycol=Pcolors, 
                      my_ylab="richness (sheldon)",
                      buffer=0.05){
  
  for_ggplot <- data.frame("resp" = d[,response],
                           "exp1" = d[,explanatory1],
                           "exp2" = d[,explanatory2])
  
  
  ggplot(for_ggplot, aes(x=exp2, y=resp))+
    stat_boxplot(geom = "errorbar", width = 0.5) +  
    geom_boxplot(aes( fill=exp2)) +
    scale_fill_manual(values=Pcolors)+
    facet_grid(.~exp1)+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
    theme(legend.position="none") +
    theme(axis.title.x=element_blank())+
    ylab(my_ylab)
}