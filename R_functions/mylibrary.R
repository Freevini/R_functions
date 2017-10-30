#####===================================
##my library function
##Vincent Somerville
##09.05.2017
##load and if necessary install and load the packages
#####===================================


mylibrary <- function(mypackage){
  for( i in mypackage){
    if(!require(i,character.only=TRUE)){
  
  install.packages(i, dependencies = TRUE)
  require(i,character.only = TRUE)
    }
  }
}

#####----------------
##running function
#####----------------

#mylibrary(c("vegan","animation"))

