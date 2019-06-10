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

getFAMD <- function(dataMat){
  par(mfrow=c(2,2))
  plotMat <- nameMixedData(dataMat)
  quesFAMD <- FAMD(plotMat, graph = F)
  plot(quesFAMD, choix="ind", title="Individual graph: Every entry is represented by the two components")
  plot(quesFAMD, choix="quanti", title="Quantitative variables: A correlation circle for the continuous features")
  plot(quesFAMD, choix="quali", title="Qualitative variables:  A correlation plot of the categorical features")
  plot(quesFAMD, choix="var", title = "Graph of the Variables: An assotiation plot for all features")
}

custom_river <- function(fitted_models,fulllst, levels = 8){
  nodes <- 'start'
  
  for(i in 2:levels)
    nodes <- c(nodes, paste0('Cl_', i, LETTERS[1:i]))
  a <- as.data.frame(table(factor(clusters(getModel(fitted_models, as.character(2)))[1:nrow(fulllst)], 
                                  levels = order(table(factor(clusters(getModel(fitted_models, '2'))[1:nrow(fulllst)], levels = 1:2)),decreasing = T),
                                  labels = 1:2)))[, 2]
  mov = a
  edges <- list( start= list( Cl_2A = mov[1], Cl_2B = mov[2]))
  for(i in 2:(levels-1)){
    
    a <- factor(clusters(getModel(fitted_models, as.character(i)))[1:nrow(fulllst)], 
                levels = order(table(factor(clusters(getModel(fitted_models, as.character(i)))[1:nrow(fulllst)], levels = 1:i)),decreasing = T),
                labels = 1:i)
    b <- factor(clusters(getModel(fitted_models, as.character(i+1)))[1:nrow(fulllst)], 
                levels = order(table(factor(clusters(getModel(fitted_models, as.character(i+1)))[1:nrow(fulllst)], levels = 1:(i+1))),decreasing = T),
                labels = 1:(i+1))
    mov <- t(table(a, b)) 
    listlist <- NULL
    for(j in 1:i) listlist[j] <- paste0('Cl_', i+1, LETTERS[1:(i+1)], '=mov[', 1:(i+1),',', j, ']', collapse=',')
    eval(parse(text = paste0(paste0('edges$Cl_', i, LETTERS[1:i]), '=list(', listlist, ')')))
  }
  # i think from here we can write outside of flexmix if-check
  node_xpos <- unlist(sapply(1:levels, function(i) rep(i, each = i)))
  
  ind <- 1
  for(i in 2:(levels)) ind <- c(ind, 2:(i+1))
  cols <- brewer.pal(levels+1, 'Set1')[ind]
  
  node_styles= list( start = list( col= cols[1] ))
  for(i in 2:length(nodes))
    eval(parse(text = paste0(paste0('node_styles$', nodes[i], ' =list(col=cols[', i, '])'))))
  
  
  r <- makeRiver( nodes, edges, node_xpos= node_xpos, node_styles= node_styles)
  ds <- default.style()
  ds[['textcex']] <- .7
  ds[['srt']] <- 45
  
  plot(r, lty = 0, default_style = ds, gravity = "bottom", 
       nsteps = 101, fix.pdf = F)

  
  
}
add.alpha <- function(col, alpha=1){
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}


long.psych.plot <- function(to_plot, ylab, cluster,v=4){
  plot(0, t = "n", xlim = c(1, v), ylim = range(-1*to_plot, na.rm = T), xaxt = "n",
       xlab = "time", ylab = ylab)
  axis(1, at = 1:v, labels = paste0("visit ",1:v))
  for(i in 1:nrow(to_plot))
    lines(na.omit(cbind(x = seq_along(-1*to_plot[i, ]), y = -1*to_plot[i, ])), col = add.alpha(as.numeric(cluster)[i] + 1, .2))
  cl_means <- apply(-1*to_plot, 2, function(i) tapply(i, cluster, mean, na.rm = T))
  cl_sds <- apply(-1*to_plot, 2, function(i) tapply(i, cluster, sd, na.rm = T))
  for(i in 1:length(unique(cluster))){
    polygon(c(1:v, v:1), c(cl_means[i, ] + cl_sds[i, ], rev(cl_means[i, ] - cl_sds[i, ])), density = 100,
            col = add.alpha((2:(length(unique(cluster)) + 1))[i], .4), border = add.alpha((2:(length(unique(cluster)) + 1))[i], .4))
  }
  for(i in 1:length(unique(cluster))){
    lines(cl_means[i, ], col = 1, lwd = 9)
    lines(cl_means[i, ], col = (2:(length(unique(cluster)) + 1))[i], lwd = 5)
  }
  points(rep(1:v, 2)[1:length(unique(cluster))], y = c(diag(cl_means), diag(cl_means[length(unique(cluster)):1, ]))[1:length(unique(cluster))], pch = 19, cex = 5, col = "black")
  points(rep(1:v, 2)[1:length(unique(cluster))], y = c(diag(cl_means), diag(cl_means[length(unique(cluster)):1, ]))[1:length(unique(cluster))], pch = 19, cex = 4, col = "white")
  text(rep(1:v, 2)[1:length(unique(cluster))], y = c(diag(cl_means), diag(cl_means[length(unique(cluster)):1, ]))[1:length(unique(cluster))], labels = LETTERS[1:length(unique(cluster))],
       cex = 1.7, col = (2:(length(unique(cluster)) + 1)))
}



flex_psy <- function(fulllst, ndim, nclus=8, ntime = 4){
  # fullst has the column ordering visit1_PC1,visit1_PC2, visit1_PC3,...
  Y1 <- unclass(as.vector(fulllst[, seq(1, ntime*ndim, ndim)])) # dim 1
  Y <- Y1
  for(i in 2:ndim){
    Y2 <- unclass(as.vector(fulllst[, seq(i, ntime*ndim, ndim)]))
    Y <- cbind(Y, Y2)
  }
  # Y is reordered cluster data with columns visit1_PC1,visit2_PC1,visit3_PC1,....
  
  # we take out the patients who missed the first visit
  nay <- is.na(Y1)
  Y <- Y[!nay, ]
  X <- 1:ntime
  X <- rep(X, each = nrow(fulllst))
  X <- X[!nay]
  grp <- rep(1:nrow(fulllst), ntime)
  grp <- grp[!nay]
  
  
  cls_dat <- data.frame(Y = Y, X = X, grp = grp)
  colnames(cls_dat)[1:ndim] <- paste0('Y', 1:ndim)
  mlist <- list()
  for(i in 1:ndim){
    eval(parse(text = paste0('mlist[[', i, ']] <- FLXMRmgcv(Y', i, '~ .)')))
  }
  eval(parse(text = paste0('fitted_models <- stepFlexmix(~s(X, k = ', ntime, ') | grp, data = cls_dat, k = 2:',nclus,', model=mlist, nrep = 5)')))
  save(fitted_models, file ='test.Rdata')
  return(fitted_models)
  
  
}