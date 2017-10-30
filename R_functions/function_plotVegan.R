plotVegan <- function(obj, dist.method="bray", binary=FALSE, ...){
  
for_vegan <- t(obj)
arab_dis <- vegdist(for_vegan, method=dist.method, binary=binary)
arab_mds <- cmdscale(arab_dis)

#browser()

plot(arab_mds, pch=16, ...)
}
