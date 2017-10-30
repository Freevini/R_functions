generate_label_df <- function(HSD, d, explanatory, response, buffer=0.2){
  
  Tukey.levels <- HSD[[explanatory]][,4]
  Tukey.labels <- multcompLetters(Tukey.levels, reversed=T, Letters=LETTERS)['Letters']
  
  plot.labels <- names(Tukey.labels[['Letters']])
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  plot.levels <- data.frame(plot.labels, labels = Tukey.labels[['Letters']],
                            stringsAsFactors = FALSE)
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space 
  # to buffer between upper quantile and label placement
  # boxplot.df <- ddply(d, explanatory, function (x) max(fivenum(x$y)) + 0.2) 
  ## does not work for me
  # improve using this bit
  #http://stackoverflow.com/questions/14758566/how-can-i-use-functions-returning-vectors-like-fivenum-with-ddply-or-aggregate
  xx <-aggregate(d[,response], list(d[,explanatory]) , function(x) summary(fivenum(x))) 
  boxplot.df <-data.frame(xx$Group.1,xx$x[,5],stringsAsFactors = FALSE)
  colnames(boxplot.df)<-c(explanatory,"max")
  boxplot.df$max <- boxplot.df$max+buffer
   
  # Merge coordinates with labels
  labels.df <- merge(plot.levels, boxplot.df, 
                     by.x = 'plot.labels', by.y = explanatory, sort = FALSE)
  
  return(labels.df)
}