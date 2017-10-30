plotmodule_greedy <- function(networkFile=networkFile, pch_pet = pch_vector, names_size_pet = names_pch_vecor,
                       network_colour = colour_vector, names_colour = names_colour_vector,
                       size_pet = size_vector, size_names = names_size_vector, name_title = "Title" ){
  
  
  ##p-responsive OTUs
  ##all_colour_phosphate_dependence_pet
  
  ##css_nodes_pet_all <- names
  
  ## Perform cluster analysis using greedy clustering algorithm 
  cfg_pet <- cluster_fast_greedy(as.undirected(networkFile))
  
  ## Subset for top 20 biggest nodes
  pet_modules <- sort(table(membership(cfg_pet)),decr=T)
  pet_modules_20 <- pet_modules[1:20]
  
  sum(pet_modules_20)/sum(pet_modules)
  pet20_plot <- pet_modules_20
  names(pet20_plot) <- as.factor(1:20)
  
  
  
  ## Make vector of nodes in top 20 modules
  pet_modules_points <- membership(cfg_pet)[membership(cfg_pet) %in% names(pet_modules_20)]
  
  pet_points <- NULL
  for(i in pet_modules_points){
    petx <- which(names(pet_modules_20)==i)
    pet_points <- c(pet_points,petx)
  }
  names(pet_points) <- names(pet_modules_points)
  
  ###colour, size and shape 
  
  # 
  # pet_all_pch <- V(petunia_net)$shape
  # names(pet_all_pch) <- V(petunia_net)$name
  # pet_all_pch <- pet_all_pch[match(names(sort(pet_points)),names(pet_all_pch))]
  
  pet_all_pch <- names_colour
  
  names(pet_all_pch) <- names_colour 
  
  pet_all_pch <- gsub("bOTU.*",as.vector(16),pet_all_pch)
  pet_all_pch <- gsub("fOTU.*",as.numeric(15),pet_all_pch)
  # pet_all_pch <- pch_pet
  # names(pet_all_pch) <- names_size_pet
  pet_all_pch <- pet_all_pch[match(names(sort(pet_points)),names(pet_all_pch))]
  pet_all_pch <- as.numeric(pet_all_pch)
  
  
  
  pet_all_cols <- network_colour
  names(pet_all_cols) <- names_colour
  pet_all_cols <- pet_all_cols[match(names(sort(pet_points)),names(pet_all_cols))]
  
  
  pet_all_cex <- size_pet
  # pet_all_cex <- pet_all_cex/3
  
  pet_all_cex <- pet_all_cex[match(names(sort(pet_points)),size_names)]
  
  
  ##p-responsive nodes
  p_resonsive_nodes_pet_all <- names(pet_all_cols[pet_all_cols!="gray"])
  
  pet_modules_css <- table(factor(membership(cfg_pet)[p_resonsive_nodes_pet_all],levels=names(pet_modules)))
  pet_modules_css_20 <- pet_modules_css[names(pet_modules_20)]
  pet_css20_plot <- pet_modules_css_20
  names(pet_css20_plot) <- as.factor(1:20)
  
  
  ####---------
  ##### FIgure S14 Module histogram #####
  ####---------
  
  #pdf(paste0(output,"FigureS14.pdf"),height=10,width=10,paper="a4r")
  #par(mfrow=c(2,1))
  
  par(mar=c(4,4,1,1),las=1)
  plot(sequence(pet20_plot)~jitter(sort(pet_points),1.3),pch=pet_all_pch, col=pet_all_cols, cex=pet_all_cex,
       ann=F,axes=F,ylim=c(0,(pet20_plot[1]+5)),xlim=c(0,20))
  title(main=name_title)
  axis(1,at=1:20, labels=F, tcl=-0.15)
  staxlab(side=1,at=1:20,labels=paste(rep("M",20),names(pet_modules_20),sep=""),srt=45)
  axis(1,at=seq(1,20,1), tick=F, labels=paste(round(pet_css20_plot/pet20_plot*100,0),"%",sep=""),line=1.5,cex.axis=0.7)
  axis(2,at=seq(0,(pet20_plot[1]+5),10))
  text(-1.546517,-10.782334,"p-responsive \n OTUs\n in module:",xpd=NA)
  title(ylab="Number of nodes per module",line=2)
  #label_location <- locator(1)
  
  ##plot(sequence(pet20_plot)~sort(pet_points))
  
  
  
  #dev.off()
  
  
  
  
}

