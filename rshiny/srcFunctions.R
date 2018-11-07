standardizeMixedData <- function(dataMat){
  resMat <- dataMat
  for(i in c(1:ncol(dataMat))){
    if(is.numeric(dataMat[,i])){
      if(nrow(as.matrix(table(dataMat[,i])))<3){
        resMat[, i] <-  as.factor(dataMat[, i])
      }
      else{
        resMat[, i] <-  as.numeric(as.matrix(scale(dataMat[, i])))
      } 
    } 
  }

 return(resMat)
}

nameMixedData <- function(dataMat){
  resMat <- dataMat
  for(i in c(1:ncol(dataMat))){
    if(is.factor(dataMat[,i])){
      levels(resMat[, i]) <- c(levels(resMat[, i]), paste0(names(dataMat)[i], ":", levels(resMat[, i])))
      resMat[, i] <-  as.factor(paste0(names(dataMat)[i], ":", dataMat[, i]))
    } 
  }
  
  return(resMat)
}