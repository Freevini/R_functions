diversity_plot <- function(d,response, explanatory1, explanatory2,
                      my_ylab="richness (sheldon)",
                      buffer=0.05){
  
  for_ggplot <- data.frame("resp" = d[,response],
                           "exp1" = d[,explanatory1],
                           "exp2" = d[,explanatory2],
                           "Group"= interaction(d[,explanatory1],d[,explanatory2]),
                           "mycol"= d[,"color"])
  
  col <- as.character(d[,"color"])
  names(col) <- as.character(for_ggplot$Group)
  # It's recommended to use a named vector
  cols=c("Arabidopsis.low" = "#FEB24C",
         "Arabidopsis.medium" = "#FC4E2A",
         "Arabidopsis.high" = "#DE2D26",
         "Petunia.low" = "#7FCDBB",
         "Petunia.medium" = "#1D91C0",
         "Petunia.high" = "#253494" )
  #browser()
  
  #for_ggplot
  
  ggplot(for_ggplot, aes(x=exp2, y=resp, col=Group))+
 #   stat_boxplot(geom = "errorbar", width = 0.5) +  
    geom_jitter(aes( fill= Group)) +
    facet_grid(.~exp1, scales = "free")+
    scale_colour_manual(values=cols)+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
    theme(legend.position="none") +
    theme(axis.title.x=element_blank())+
    ylab(my_ylab)
 
  
  
  }