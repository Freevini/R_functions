own_gene2go <- function (feasibleGenes = NULL, gene2GO) 
{
  gene2GO <- gene2GO[!is.na(gene2GO)]
  gene2GO <- gene2GO[sapply(gene2GO, length) > 0]
  allGO <- unlist(gene2GO, use.names = FALSE)
  geneID <- rep(names(gene2GO), sapply(gene2GO, length))
  goodGO <- allGO %in% allGO
  return(split(geneID[goodGO], allGO[goodGO]))
}


