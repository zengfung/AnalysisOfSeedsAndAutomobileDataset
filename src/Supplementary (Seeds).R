library(MASS)
library(dendextend)
library(compiler)
library(ggplot2)
library(dplyr)
library(gridExtra)

# seeds dataset
seeds_data <- read.table("C:/Users/LZFun/Google Drive (lzfliew@ucdavis.edu)/2019-2020/Spring 2020/STA160/Midterm Project/Dataset/Seed/seeds_dataset.txt", quote='\"', comment.char="")
names(seeds_data) <- c("area", "perimeter", "compactness",
                      "kernel length", "kernel width",
                      "assymmetry coefficient", "kernel groove length",
                      "wheat variety")
seeds_data$`wheat variety` <- as.factor(seeds_data$`wheat variety`)
levels(seeds_data$`wheat variety`) <- c("kama", "rosa", "canadian")

##################################################################
##################################################################
## Single variable analysis

#A function returning DESS values from all branches if tree is cut a a fixed height.
#inputs: 
#tree= an hclust object created on "values"
#values=vector of values, on which histogram will be built
#kk=Number of clusters at the tree-height the tree will be cut.
#graph=Logical. Whether the histogram will be plotted or not.
DESS=function(tree,values,kk,graph){
  if(kk>1){  
    cut=cutree(tree,kk)
    ht=heights_per_k.dendrogram(as.dendrogram(tree))
    tree1.cut=cut(as.dendrogram(tree),min(ht[which(as.numeric(names(ht))==kk)])) #tree1.cut$lower has kk parts
    h=matrix(0,kk,3)
    colnames(h)=c("min","max","frequency")
    for(i in 1:kk){
      h[i,]=c(range(values[labels(tree1.cut$lower[[i]])]),length(labels(tree1.cut$lower[[i]])))
    }
    h=as.data.frame(h)
    diff_sum=rep(0,kk)
    threshold=(h$max-h$min)^2/3
    repeated=rep(0,1000)
    for(i in 1:kk){
      for(r in 1:1000){
        repeated[r]=sum((sort(runif(h$frequency[i],h$min[i],h$max[i]))-sort(values[values<=h$max[i]&values>=h$min[i]]))^2)
      }
      diff_sum[i]=mean(repeated)
    }
  }  
  if(kk==1){
    repeated=rep(0,1000)
    for(r in 1:1000)
      repeated[r]=sum((sort(values[labels(tree)])-sort(runif(length(values[labels(tree)]),min(values[labels(tree)]),max(values[labels(tree)]))))^2)
    diff_sum=mean(repeated)
    threshold=(max(values[labels(tree)])-min(values[labels(tree)]))^2/3
    graph=FALSE
    h=as.matrix(c(range(values[labels(tree)]),attr(tree,"members")),1,3)
    if(nrow(h)!=1)h=t(h)
    tree1.cut=tree
  }
  if(graph){
    plot(diff_sum,col=1,type="b",ylim=c(min(diff_sum,threshold)-0.5,max(diff_sum,threshold)+0.5))
    lines(threshold,col=2)
  }
  return(list("DESS"=diff_sum,"threshold"=threshold,"table"=h,"pruned.tree"=tree1.cut))
}

##############################################
#A function building histogram from range and frequency of bins given
#input:
#T= a data frame with three columns describing a set of histograms. T$min= left boundary, T$max=right boundary, T$frequency= bin freuency.
histfromtable=function(values, T, color = NULL){
  breaks = c(as.matrix(T[,1:2]))
  breaks = sort(breaks, decreasing = FALSE)
  
  r = range(c(T$max,T$min))
  
  plt = ggplot(data = NULL, aes(values, fill = color))
  plt + geom_histogram(col = "black", breaks = breaks) +
    ylim(0,round(max(T$frequency),2)+0.01) +
    xlim(r[1]-0.05,r[2]+0.05) +
    ylab("frequency") +
    xlab("data") +
    theme(legend.title = element_blank(),
          legend.box = "horizontal",
          legend.justification=c(1,0), 
          legend.position="none",  
          legend.background = element_blank(),
          legend.key = element_blank())
}
############################################
#A function building and plotting CDF, withh fitted lines for each branch, from range and frequency of bins given.
#input
#T= a data frame with three columns describing a set of histograms. T$min= left boundary, T$max=right boundary, T$frequency= bin freuency.
#values=vector of values, on which histogram will be built
cdffromtable=function(values,T){
  
  T=T[,c(3,1,2)]
  T=T[order(T$min),]
  y = c(0,T$frequency)
  p = y/sum(T$frequency)
  P = matrix(0,length(p),1)
  for(i in 1:length(P)){
    P[i,1] = sum(p[1:i])
  }
  
  COL = "red"
  
  plt = ggplot(data = NULL, aes(values)) + stat_ecdf(geom = "step")
  # plot(CDF,col="grey",main="empirical CDF")
  for(i in 1:(length(P)-1)){
    if(T[i,1]>1){
      plt = plt + 
        geom_line(aes_string(x = unlist(T[i,2:3]), y = P[i:(i+1)]), 
                  col = COL, lwd = 1) +
        geom_point(aes_string(x = unlist(T[i,2:3]), y = P[i:(i+1)]), 
                   col = COL, pch = 19)
      print(plt)
      #if(i < (length(P)-1)){
      # lines(c(T[i,3],T[(i+1),2]),rep(P[i+1],2),col=COL)
      #}
    }
    if(T[i,1]==1){
      plt = plt +
        geom_line(aes_string(x = c(T[i,3],T[(i+1),2]), y = rep(P[i+1],2)),
                  col = COL, lwd = 1) +
        geom_point(aes_string(x = c(T[i,3],T[(i+1),2]), y = rep(P[i+1],2)), 
                   col = COL, pch = 19)
      # lines(c(T[i,3],T[(i+1),2]),rep(P[i+1],2),col=COL)
      # points(T[i,3],P[i+1],pch=19,col=COL)
    }
  }
  plt
}


############################################
############################################
############################################

#A function that dynamically cuts a tree (not at fixed height) according to DESS threshold
#Application of algorithm 1
#inputs: 
#values=vector of values, on which histogram will be built
#epsilon: from the DESS criterion DESS < epsilon*(b_j-a_j)^2/3. Default value is 1 if missing
#L0= minimum DESS values which is tolerated in the bins. Default is 0.1* tree height if missing.
#Graph=Logical. Whether the histogram will be plotted or not. Default is FALSE.
histbyDESS=function(values,epsilon,L0,graph){ 
  if(missing(epsilon))epsilon=1
  if(missing(graph))graph=FALSE
  tree=hclust(dist(values))
  if(missing(L0))L0=0.1*attr(as.dendrogram(tree),"height")
  dess_list=c()
  threshold_list=c()
  table_list=c()
  tree_list=c()
  all.possible.trees=list()
  i=1
  sel=0
  all.possible.trees
  ht=heights_per_k.dendrogram(as.dendrogram(tree))
  ce_input=DESS(tree,values = values,kk=2,graph = FALSE)
  ce_input$DESS<L0 #if TRUE then break
  ce_input$DESS<2*ce_input$threshold #if TRUE then break
  new_trees=ce_input$pruned.tree$lower
  
  if(heights_per_k.dendrogram(new_trees[[1]])[1]>heights_per_k.dendrogram(new_trees[[2]])[1]){selected=1;not.selected=2}
  if(heights_per_k.dendrogram(new_trees[[1]])[1]<heights_per_k.dendrogram(new_trees[[2]])[1]){selected=2;not.selected=1}
  all.possible.trees[[i]]=new_trees[[not.selected]]
  i=i+1
  new_kk=2
  selected #this determines which branch of the pruned tree will be selected as the higest available one.
  height.selected=heights_per_k.dendrogram(new_trees[[selected]])[1]
  tree.selected=new_trees[[selected]]
  DESS.selected=ce_input$DESS[selected]
  table.selected=ce_input$table[selected,]
  threshold.selected=ce_input$threshold[selected]
  repeat{
    if(DESS.selected<max(L0,epsilon*threshold.selected)){ #if true put a stop sign on that node, collect the h table row
      sel=sel+1
      dess_list=c(dess_list,DESS.selected)
      threshold_list=c(threshold_list,threshold.selected)
      table_list=rbind(table_list,c(table.selected))
      tree_list[[sel]]= tree.selected
      #i=i-1
    }
    if(length(unlist(sapply(tree_list,labels)))==length(values))break
    #if false then go to the following step
    #find the second highest node
    if(DESS.selected>max(L0,epsilon*threshold.selected)){
      all.possible.trees[[i]]=cut(as.dendrogram(tree.selected),min(ht[which(names(ht)==new_kk)]))$lower[[1]]
      i=i+1
      all.possible.trees[[i]]=cut(as.dendrogram(tree.selected),min(ht[which(names(ht)==new_kk)]))$lower[[2]]
      i=i+1
    }
    
    heights=rep(0,length(all.possible.trees))
    for(j in 1: length(all.possible.trees)){
      heights[j]=attr(all.possible.trees[[j]],"height")
    }
    to.remove=which(heights==0)
    if(length(to.remove)>0){
      heights=heights[-to.remove]
      for(k in 1:length(to.remove)){
        sel=sel+1
        tree_list[sel]= all.possible.trees[to.remove[k]]
        all.possible.trees=all.possible.trees[-to.remove[k]]
        i=i-1
      }
      
    }
    
    selected=min(which(heights==max(heights)))
    not.selected=which(heights!=max(heights))
    new_trees=all.possible.trees
    new_kk=min(as.numeric(names(which(round(ht,5)==round(max(heights),5)))))
    ce_input=DESS(new_trees[[selected]],values=values,kk=1,graph = FALSE)
    DESS.selected=ce_input$DESS
    height.selected=max(heights)
    threshold.selected=ce_input$threshold
    table.selected=ce_input$table
    tree.selected=new_trees[[selected]]
    all.possible.trees=all.possible.trees[-selected]
    i=i-1
  }
  all.labels=sapply(tree_list,labels)
  all_heights=rep(0,length(tree_list))
  for(j in 1: length(tree_list)){
    all_heights[j]=attr(tree_list[[j]],"height")
  }
  full.table=matrix(0,length(all.labels),3)
  colnames(full.table)=c("min","max","frequency")
  for(i in 1:length(all.labels)){
    all.values=values[all.labels[[i]]]
    full.table[i,]=c(range(all.values),length(all.values))
  }
  full.table=as.data.frame(full.table)
  full.table=full.table[order(full.table$min),]
  if(graph==TRUE){
    par(mfrow=c(1,2))
    cdffromtable(values,full.table)
    histfromtable(values,full.table)
  }
  return(list("table"=full.table,"trees"=tree_list[as.numeric(rownames(full.table))]))
}
#Output:
# $table= a table containing maximum, minimum and frequency of each bin.
# $trees - a list all pruned branches from the main tree that constructed the histogram
histbyDESS=cmpfun(histbyDESS)
############################################
############################################
#M is an output from the histbyDESS() function.
plotDESS=function(values,M){
  all.dess=rep(0,length(M$trees))
  all.threshold=rep(0,length(M$trees))
  for(j in 1:length(all.dess)){
    all.dess[j]=DESS(M$trees[[j]],values=values,kk=1,graph = FALSE)$DESS
    all.threshold[j]=(M$table$max[j]-M$table$min[j])^2/3
  }
  
  plt = ggplot() + geom_line(aes(x = 1:length(all.dess), y=all.dess), col = "red") + 
    geom_line(aes(x = 1:length(all.threshold), y=all.threshold), col = "blue") + 
    geom_point(aes(x = 1:length(all.dess), y=all.dess), col = "red") + 
    geom_point(aes(x = 1:length(all.threshold), y=all.threshold), col = "blue") + 
    xlab("") + ylab("")
    
  return(list(plot = plt, all.dess=all.dess,all.threshold=all.threshold))
}
plotDESS=cmpfun(plotDESS)
#output
# $all.dess= A list of all DESS values calculated from the bins
# $all.threshold = A list of (b_j-a_j)^2/3 values from the bins

##############################################################
###############################################################

for (i in 1:7) {
  M3=histbyDESS(values=seeds_data[,i],epsilon=1,graph=TRUE)
  plot1 = cdffromtable(seeds_data[,i],M3$table)
  plot2 = histfromtable(values = seeds_data[,i], color = seeds_data$`wheat variety`,T = M3$table)
  plot3 = plotDESS(seeds_data[,i],M3)$plot
  grid.arrange(plot1, plot2, plot3, ncol = 3)
}

##################################################################
##################################################################

# obtaining mean and variance for each type of wheat variety
kama_data = seeds_data[which(seeds_data$"wheat variety" == "kama"), 1:7]
rosa_data = seeds_data[which(seeds_data$"wheat variety" == "rosa"), 1:7]
canadian_data = seeds_data[which(seeds_data$"wheat variety" == "canadian"), 1:7]

kama_mean = as.matrix(apply(kama_data, 2, mean), ncol = 1)
kama_var = as.matrix(apply(kama_data, 2, var), ncol = 1)

rosa_mean = as.matrix(apply(rosa_data, 2, mean), ncol = 1)
rosa_var = as.matrix(apply(rosa_data, 2, var), ncol = 1)

canadian_mean = as.matrix(apply(canadian_data, 2, mean), ncol = 1)
canadian_var = as.matrix(apply(canadian_data, 2, var), ncol = 1)

seeds_mean = as.matrix(apply(seeds_data[,1:7], 2, mean), ncol = 1)
seeds_var = as.matrix(apply(seeds_data[,1:7], 2, var), ncol = 1)

## principal component analysis (pca)
# pca of standardized data
seeds_pcstd = princomp(seeds_data[, 1:7], cor = TRUE)
summary(seeds_pcstd, loadings = TRUE)
# 1st 2 pc accounts for 89% of variability of standardized data

## linear discriminant analysis (lda) on pc1 and pc2
kama_pc = seeds_pcstd$scores[1:70,1:2]
rosa_pc = seeds_pcstd$scores[71:140,1:2]
canadian_pc = seeds_pcstd$scores[141:210,1:2]

kama_pcmean = colMeans(kama_pc)
rosa_pcmean = colMeans(rosa_pc)
canadian_pcmean = colMeans(canadian_pc)

S.u <- 69*(var(kama_pc)+var(rosa_pc)+var(canadian_pc))/207
w_1 <- solve(S.u)%*%(kama_pcmean - rosa_pcmean)
w0_1 <- -(kama_pcmean + rosa_pcmean)%*%w_1/2

w_2 <- solve(S.u)%*%(kama_pcmean - canadian_pcmean)
w0_2 <- -(kama_pcmean + canadian_pcmean)%*%w_2/2

# plot of pc1 vs pc2
par(mfrow = c(1,1))
plot(seeds_pcstd$scores[,1], seeds_pcstd$scores[,2], 
     xlab="PC 1", ylab="PC 2",  lwd=2, 
     col=seeds_data$`wheat variety`, pch = 20)
lines(seeds_pcstd$scores[,1],-(w_1[1]*seeds_pcstd$scores[,1]+w0_1)/w_1[2])
lines(seeds_pcstd$scores[,1],-(w_2[1]*seeds_pcstd$scores[,1]+w0_2)/w_2[2])
legend("bottomright",legend=levels(seeds_data$`wheat variety`),
       pch=20,col=1:3,cex=0.9)
# can see that there is some distinction between wheat variety

lda.obj <- lda(seeds_data$`wheat variety`~seeds_pcstd$scores[,1]+seeds_pcstd$scores[,2],prior=c(1,1,1)/3)
plda <- predict(object=lda.obj,newdata=seeds_data)

# apparent error rate
table(true = seeds_data$`wheat variety`, predicted = plda$class)

# expected actual error rate by Lachenbruch's holdout
cv.prediction <- c()
pc_data <- data.frame(seeds_pcstd$scores[,1:2],
                      `wheat` = seeds_data$`wheat variety`)

for (i in 1:nrow(seeds_data)) {
  holdout <- pc_data[-i,]
  
  holdout1 <- holdout[holdout$`wheat` == "kama",][,1:2]
  holdout2 <- holdout[holdout$`wheat` == "rosa",][,1:2]
  holdout3 <- holdout[holdout$`wheat` == "canadian",][,1:2]
  
  holdout1.means <- apply(holdout1, 2, mean)
  holdout2.means <- apply(holdout2, 2, mean)
  holdout3.means <- apply(holdout3, 2, mean)
  
  n1 <- nrow(holdout1)
  n2 <- nrow(holdout2)
  n3 <- nrow(holdout3)
  
  w1 <- (n1 - 1) * var(holdout1)
  w2 <- (n2 - 1) * var(holdout2)
  w3 <- (n3 - 1) * var(holdout3)
  
  sp1 <- 1 / (n1 + n2 +n3 - 3) * (w1 + w2 + w3)
  
  cutoff1 <- .5 * (holdout1.means - holdout2.means) %*% solve(sp1) %*% (holdout1.means + holdout2.means)
  cutoff2 <- .5 * (holdout2.means - holdout3.means) %*% solve(sp1) %*% (holdout2.means + holdout3.means)
  cutoff3 <- .5 * (holdout1.means - holdout3.means) %*% solve(sp1) %*% (holdout1.means + holdout3.means)
  
  ay1 <- (holdout1.means - holdout2.means) %*% solve(sp1) %*% as.numeric(seeds_pcstd$scores[i,1:2])
  ay2 <- (holdout2.means - holdout3.means) %*% solve(sp1) %*% as.numeric(seeds_pcstd$scores[i,1:2])
  ay3 <- (holdout1.means - holdout3.means) %*% solve(sp1) %*% as.numeric(seeds_pcstd$scores[i,1:2])

  if (ay1 >= cutoff1){
    group = ifelse(ay3 >= cutoff3, "kama", "canadian")
  } else if (ay2 >= cutoff2){
    group = ifelse(ay1 >= cutoff1, "kama", "rosa")
  } else{
    group = "canadian"
  }
  cv.prediction <- append(cv.prediction, group)
}

cv.prediction <- factor(cv.prediction, levels = c("kama", "rosa", "canadian"))

# expected actual error rate
table(pc_data$`wheat`, cv.prediction, dnn = c('true','predicted'))
# very similar to the apparent error rate due large data size

## hierarchical clustering with pc1 and pc2
hc.average =  hclust(dist(seeds_pcstd$scores[,1:2]), method = "average")
plot(hc.average , main = "Average Linkage", xlab="", sub ="", cex =.9)
hc.average.pred = as.factor(cutree(hc.average, 4))
clust.hc.average = ggplot(NULL,aes(seeds_pcstd$scores[,1],
                                  seeds_pcstd$scores[,2],
                                  color=hc.average.pred))
clust.hc.average + geom_point() +
  xlab("PC1") + ylab("PC2") +
  theme(legend.position = 'none')
