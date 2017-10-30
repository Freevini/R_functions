compareNA <- function(v1,v2){
  ##function from r-cookbook.com
  same <- (v1 ==v2) 
  same[is.na(same)] <- FALSE
  return(same)
}