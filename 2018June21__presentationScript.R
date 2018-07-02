setwd("/home/richa/projects/eyerich/ad_remission/")
#pdf("plots_cyto.pdf")
# input data
suppressMessages({library(mice)})
imputedData <- readRDS("Data_LK/result_mice.Rds") # mice imputed data
dataMat <- complete(imputedData, action = 1)  #I used as covariates in the modelling. 
dataMat2 <- dataMat
names(dataMat) <- paste0("attribute_", 1:ncol(dataMat))
remissionStatus <- readRDS("Data_LK/persistent.Rds") # I used as outcome in my data
cytokines <- c('CCL17', 'CCL22', 'TIMP1', 'TIMP2', 'TIMP3', 'TIMP4', 'IL9', 'IP10', 'PDGFbb', 'MIP1b', 'RANTES', 'IL1ra', 'IL4', 'IL5', 'IL6', 'IL7', 'IL8', 'IL10', 'IL12', 'IL13', 'IL15', 'IL17', 'Eotaxin', 'FGFbasic', 'GCSF', 'GMCSF', 'INFg', 'MCP1', 'MIP1a', 'TNFa', 'VEGF')
expData <- dataMat[, which(names(dataMat2)%in%cytokines)]
quesData <- dataMat[, -which(names(dataMat2)%in%cytokines)]

# distance matrices
suppressMessages({library(cluster)})
quesDist <- daisy(quesData, metric = "gower")
cytoDist <- daisy(cytoData, metric = "gower")

# raw data visualization
suppressMessages({library(pheatmap)})
pheatmap(data.matrix(quesData[, which(sapply(1:ncol(quesData), 
          FUN=function(i) is.numeric(quesData[,i])))]), cluster_rows = F, cluster_cols = F, scale = "column")

pheatmap(data.matrix(quesData[, -which(sapply(1:ncol(quesData), 
                                              
            FUN=function(i) is.numeric(quesData[,i])))]), cluster_rows = F, cluster_cols = F)

# mds of distance matrix visual
mdsfit <- cmdscale(quesDist, eig = TRUE, k = 2)
x <- mdsfit$points[, 1]
y <- mdsfit$points[, 2]
plot(x, y, pch = 19)

# number of cluster optimization
suppressMessages({library(NbClust)})
  quesDist <- daisy(quesData, metric = "gower")
  cindexOpt <- NbClust(data=NULL, diss=quesDist, distance = NULL, min.nc = 2,
                       max.nc = 15, method = "ward.D2", index = "cindex")
plot(names(cindexOpt$All.index), cindexOpt$All.index, ylim=c(0, 1), xlab="k", ylab="c index")

# robustness analysis with pvclust
library(pvclust)
gowerDist <- function(mat){
  daisy(data.frame(t(mat)), metric = "gower")
}

pvcfit <- pvclust(data.matrix(t(quesData)), method.hclust="ward.D2",
               method.dist=gowerDist)
plot(pvcfit, hang=-1) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(pvcfit, alpha=.8)

# tsne visualization of clusters

## Resort to TSNE
library(Rtsne)
library(ggplot2)
library(tidyverse)
pamfit <- pam(quesDist, k = 5)
perps <- c(1, 3, 5, 10, 20, 30, 40)
# tsen on distance matrix
plotMats <- lapply(1:length(perps), FUN=function(p){
  tsne_obj <- Rtsne(quesDist, is_distance = TRUE,perplexity=perps[p], verbose=T, max_iter=10000 )
  tsne_data <- tsne_obj$Y %>%
    data.frame() %>%
    setNames(c("X", "Y")) %>%
    mutate(cluster = factor(pamfit$clustering)) %>%
    mutate(perplexity = perps[p])
})

plotMat <- do.call(rbind, plotMats)
ggplot(aes(x = X, y = Y), data = plotMat) +
  geom_point(aes(color = cluster))+
  facet_wrap(~ perplexity, scales = "free") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

# lasso models with cluster ids
## Characterize the groups
library(glmnet)
library(pheatmap)
pamfit <- pam(quesDist, k = 5)
cluster.ids <- fit$clustering
design.mat <- model.matrix(cluster.ids ~ data.matrix(cytoData))
glmfit <- glmnet(design.mat, cluster.ids, family = "multinomial", alpha = 0)
glmfit.cv <- cv.glmnet(design.mat, cluster.ids, family = "multinomial")
coef.mat <- do.call(cbind, coef(glmfit, s=glmfit.cv$lambda.1se))
#plot(fit.cv)
#title("Multinomial Family",line=2.5)
plotMat <- coef.mat[-c(1:2), ]
rownames(plotMat) <- names(cytoData)
colnames(plotMat) <- 1:5
pheatmap(plotMat, cluster_rows = F, cluster_cols = F, display_numbers = T )
# randomization
plotMats <- lapply(1:10, FUN=function(i){
  fcluster.ids <- sample(1:5, length(cluster.ids), replace = T)
  design.mat <- model.matrix(fcluster.ids ~ data.matrix(cytoData))
  fglmfit <- glmnet(design.mat, fcluster.ids, family = "multinomial", alpha = 0)
  fglmfit.cv <- cv.glmnet(design.mat, fcluster.ids, family = "multinomial")
  coef.mat <- do.call(cbind, coef(fglmfit, s=fglmfit.cv$lambda.1se))
  #plot(fit.cv)
  #title("Multinomial Family",line=2.5)
  plotMat <- coef.mat[-c(1:2), ]
  rownames(plotMat) <- names(cytoData)
  colnames(plotMat) <- 1:5
  return(plotMat)
})
phs <- lapply(plotMats, FUN=function(x)pheatmap(x, cluster_rows = F, cluster_cols = F, display_numbers = T )[[4]])
pdf(height=10, width=10, file="randomGLMNET.pdf")
plot_grid(plotlist = phs[1:4], nrow=2, ncol=2)
plot_grid(plotlist = phs[5:8], nrow=2, ncol=2)
plot_grid(plotlist = phs[9:10], nrow=2, ncol=2)
dev.off()

# external validation
pamfit <- pam(quesDist, k = 5)
hyperTest <- lapply(1:5, FUN=function(i) {
 clusOne <- which(pamfit$clustering==i)
  a <- length(which(remissionStatus[clusOne]==1))
  c <- length(which(remissionStatus[clusOne]==0))
  b <- length(which(remissionStatus==1))-a
  d <- length(which(remissionStatus==0))-c
 ftest <- fisher.test(matrix(c(a, b, c, d), byrow = T, nrow = 2), alternative = "two.sided")
  q <- a
  m <- length(which(remissionStatus==1))
  n <- length(which(remissionStatus==0))
  k <- length(clusOne)
  hyperLower <- phyper(q, m , n, k, lower.tail = F)
  hyperUpper <- phyper(q, m , n, k, lower.tail = T)
  
  return(list(hyperLower, hyperUpper))
})

# final cluster visualization
library(fpc)
plotcluster(quesDist, pamfit$cluster)
