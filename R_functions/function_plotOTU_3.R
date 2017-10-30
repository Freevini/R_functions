plotOTU <- function(myOTU, taxon_names = labeltaxa, dat = log_counts, mydesign_01=design_01){
  # name and taxononmy of OTU
  name_OTU <-paste(myOTU,"is", taxon_names[names(taxon_names)==myOTU])
  counts<- dat[myOTU,]
  #browser()
  
  # prepare data for ggplot2
  for_ggplot <- data.frame(as.numeric(counts),
                       "plant"= mydesign_01[,"plant"],
                       "treatment" = mydesign_01[,"treatment"])
  colnames(for_ggplot)[1] <- "log_cpm"
  # stripchart
  
  print(ggplot(data=for_ggplot, aes(x=treatment, y=log_cpm, color= treatment))+
    geom_jitter(aes(fill=mydesign_01$treatment))+
      facet_grid(.~plant)+
    theme_bw()+
    theme(legend.position="none") +
    ggtitle(name_OTU)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    ylab("log2 cpm"))
  }