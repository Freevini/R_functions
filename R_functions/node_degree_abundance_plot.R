###=================
##scatterplot (node degree vs. Abundance) with density plots on the axes
##19.05.2017
##Vincent Somerville
##This function is created to analyse both bacterial aswell as fungal community data
##This function is created for analysis of 3 treatment groups
###=================

##==============
##load packages
##==============
library("reshape2","ggplot2","gridExtra")

# function(log_OTU_matrix_16S=log_OTU_matrix_16S,log_OTU_matrix_ITS=log_OTU_matrix_ITS,Node_degree_all=Node_degree_all,Vertices_net_file=Vertices_net_file){
  
node_degree_abundance_plot <- function(log_OTU_matrix_16S=log_OTU_matrix_16S,log_OTU_matrix_ITS=log_OTU_matrix_ITS,Node_degree_all=Node_degree_all,Vertices_net_file=Vertices_net_file){

sum_log_OTU_matrix_16S <- rowSums(log_OTU_matrix_16S)
scatterplot_petunia_data <- cbind(abundance=sum_log_OTU_matrix_16S,node_degree=paste("b",names(sum_log_OTU_matrix_16S),sep = ""),phosphate=paste("b",names(sum_log_OTU_matrix_16S),sep = ""),taxon=rep("bacteria",length(sum_log_OTU_matrix_16S)))
rownames(scatterplot_petunia_data) <- paste("b",names(sum_log_OTU_matrix_16S),sep = "")

##==============
## add ITS
##==============

sum_log_OTU_matrix_ITS <- rowSums(log_OTU_matrix_ITS)
names(sum_log_OTU_matrix_ITS) <- paste("f",names(sum_log_OTU_matrix_ITS),sep = "")
scatterplot_petunia_data <- rbind(scatterplot_petunia_data,cbind(abundance=sum_log_OTU_matrix_ITS,node_degree=names(sum_log_OTU_matrix_ITS),phosphate=names(sum_log_OTU_matrix_ITS),taxon=rep("fungi",length(sum_log_OTU_matrix_ITS))))
all_rownames <- rownames(scatterplot_petunia_data)

##==============
##add node degree
##==============

for(i in names(Node_degree_all)){
  scatterplot_petunia_data[which(rownames(scatterplot_petunia_data)==i),2] <- Node_degree_all[i]
}
scatterplot_petunia_data[which(scatterplot_petunia_data[,2]==21),]

##==============
##add phosphate colour
##==============
for(i in 1:length(V(Vertices_net_file)$name)){
  scatterplot_petunia_data[which(rownames(scatterplot_petunia_data)==V(Vertices_net_file)$name[i]),3] <- V(Vertices_net_file)$color[i]
}

##==============
##shorten atrix to only occuring OTU
##==============
scatterplot_petunia_data <- scatterplot_petunia_data[match(V(Vertices_net_file)$name,rownames(scatterplot_petunia_data)),]
all_rownames <- rownames(scatterplot_petunia_data)


scatterplot_petunia_data <- as.data.frame(scatterplot_petunia_data, stringsAsFactors = FALSE)

scatterplot_petunia_data <- data.frame(lapply(scatterplot_petunia_data, type.convert))
rownames(scatterplot_petunia_data) <- all_rownames



##==============
###main scatterplot
##==============
fill <- as.character(levels(scatterplot_petunia_data$phosphate))

p1 <- ggplot(scatterplot_petunia_data,aes(x=node_degree,y=abundance,colour=factor(phosphate))) + geom_jitter(aes(fill=factor(phosphate),shape=taxon),size=4) +
  scale_x_continuous(expand=c(0.02,0)) +
  scale_y_log10(breaks=c(0.001,0.01,0.1,1,10,100),expand=c(0.02,0)) +
  scale_colour_manual(values = c(levels(scatterplot_petunia_data$phosphate)[1],levels(scatterplot_petunia_data$phosphate)[2],levels(scatterplot_petunia_data$phosphate)[3])) + ##change shape colour
  scale_shape_manual(values = c(19,15)) + 
    theme_bw() +
  theme(legend.position="none",
        plot.margin=unit(c(0,0,0,0),"points"),
        panel.border=element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour="black")
  )
##==============
###prepare side plots 
##==============
theme0 <- function(...) theme( legend.position = "none",
                               panel.background = element_blank(),
                               panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               panel.spacing = unit(0,"null"),
                               axis.ticks = element_blank(),
                               axis.text = element_blank(),
                               #axis.ticks.x = 
                               #axis.text.y = element_blank(),
                               axis.title.x = element_blank(),
                               axis.title.y = element_blank(),
                               axis.ticks.length = unit(0,"null"),
                               axis.ticks.margin = unit(0,"null"),
                               panel.border=element_rect(color=NA),...)



##==============
###first histogramm
##==============

p2 <- ggplot(scatterplot_petunia_data,aes(x=node_degree,colour=factor(phosphate),fill=factor(phosphate))) + 
  geom_density(aes(fill=factor(phosphate)),alpha=0.5,position = "fill") + 
  scale_x_continuous(breaks=NULL,expand=c(0.02,0)) +
  scale_y_continuous(breaks=NULL,expand=c(0.02,0),labels=percent) +
  scale_fill_manual(values = c(levels(scatterplot_petunia_data$phosphate)[1],levels(scatterplot_petunia_data$phosphate)[2],levels(scatterplot_petunia_data$phosphate)[3])) + ##change shape colour
  theme_bw() +
  theme0(plot.margin = unit(c(1,-0.6,0,2.8),"lines")) 

##==============
###second histogramm
##==============

p3 <- ggplot(scatterplot_petunia_data,aes(x=abundance,colour=factor(phosphate),fill=factor(phosphate))) + 
  geom_density(aes(fill=factor(phosphate)),alpha=0.5) + 
  coord_flip()  + 
  scale_x_log10(breaks=NULL,expand=c(0.02,0)) +
  scale_y_continuous(breaks=NULL,expand=c(0.02,0),labels=percent) +
  scale_fill_manual(values = c(levels(scatterplot_petunia_data$phosphate)[1],levels(scatterplot_petunia_data$phosphate)[2],levels(scatterplot_petunia_data$phosphate)[3])) + ##change shape colour
  theme_bw() +
  theme0(plot.margin = unit(c(-0.5,1,1.7,-0.1),"lines"))


##=======
##make legend
##=======

xx <- data.frame("phosphate" =rep(c("no P dependency","up in low P","up in high P"),2),
                 "color" = rep(levels(scatterplot_petunia_data$phosphate),2) )
xx$y <-rep(3:1,2)
xx$x <-c(1,1,1,2,2,2)


plot(xx$x, xx$y, col=as.character(xx$color), pch=c(19,19,19,15,15,15), cex=3,
     yaxt="n", xaxt="n", 
     ylab="", xlab="", 
     bty="n",
     ylim=c(0,5), xlim=c(-1,4), xpd=TRUE)
text(x=0, y=c(1:3), labels=rev(xx$phosphate))
text(y=4, x=c(1:2), labels=levels(scatterplot_petunia_data$taxon))

##==============
##arrange and plot
##==============

grid.arrange(arrangeGrob(p2,ncol=2,widths=c(3,1)),
             arrangeGrob(p1,p3,ncol=2,widths=c(3,1)),
             heights=c(1,3))
}