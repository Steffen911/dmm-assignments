source("util.R")

## load the document-term matrix
Ptilde <- as.matrix( read.csv("data/news.csv") )
P <- Ptilde/sum(Ptilde) ## replace word frequencies by "probabilities"

## the data is sorted by newsgroup; this is not exploited/known to NMF
## here we truncate values >0.0001 for improved visiblity
ggplotm(pmin(P,0.0001), format="", show.axis=FALSE, mid="black")
showcol(pmin(P,0.0001)) # alternative, faster but less flexible

############
## 1. NMF ##
############

## run NMF using GKL
r <- 4
lr.gkl <- lee01.gkl(P, r, reps=5)

## also compute SVD
P.svd <- svd(P)
rownames(P.svd$v) <- colnames(P)

## print results
with(lr.gkl,
     for (k in 1:nrow(R)) {
         print(rev(sort(R[k,]))[1:10])
         cat(strrep('-',80), "\n")
     })

with(P.svd,
     for (k in 1:r) {
         y=order(abs(v[,k]))
         print(rev(v[y,k])[1:10])
         cat(strrep('-',80), "\n")
     })

## look at the decomposition (here: Rtilde and V)
ggplotm(lr.gkl$R, format="", show.axis=FALSE, mid="black") # enlarge plot or subselect columns to see something
ggplotm(lr.gkl$R[,1:30], format="", rotate.labels=TRUE, mid="black") # first 30 columns

ggplotm(t(P.svd$v[,1:4]), format="", show.axis=FALSE, mid="black")
ggplotm(t(P.svd$v[1:30,1:4]), format="", rotate.labels=TRUE, mid="black") # first 30 columns

## look at the reconstruction
Phat <- lr.gkl$L %*% lr.gkl$R
summary(as.vector(Phat))
ggplotm(pmin(Phat, 0.0001), format="", show.axis=FALSE, mid="black")

Phat <- P.svd$u[,1:4] %*% diag(P.svd$d[1:4]) %*% t(P.svd$v[,1:4])
summary(as.vector(Phat))
ggplotm(pmax(pmin(Phat,0.0001),-0.0001), format="", show.axis=FALSE, mid="black")

## Gaussian NMF
r <- 4
lr.gnmf <- lee01.gnmf(P, r, reps=5)

## YOUR CODE HERE


#############
## 2. PLSA ##
#############

## computing the 3-matrix decompositions
r <- 4
lr.gkl <- lee01.gkl(P, r, reps=5)
lsr.gkl <- nmf.lsr(lr.gkl) ## result as: L %*% S %*% R
slr.gkl <- nmf.slr(lr.gkl) ## result as: S %*% L %*% R

## YOUR CODE HERE


###########################
## 3. NMF and clustering ##
###########################

## true labels (DO NOT USE for clustering)
## 1=sci.crypt
## 2=sci.med
## 3=sci.space
## 4=soc.religion.christian
labels <- rep(1:4, each=100)

## example clustering (k-means with k=4)
cluster <- kmeans(P, 4, nstart=100)$cluster

## to compute the confusion matrix between a clustering and the true labels, we
## first relabel every cluster so that cluster ids match labels to the extent
## possible. (Always do this.)
cm <- function(cluster.ids) {
    cluster.ids.matched <- match.labels(cluster.ids, labels)
    u = union(cluster.ids.matched, labels)
    t = table(factor(cluster.ids.matched, u), factor(labels, u))
    confusionMatrix(t)
}
cm(cluster)$overall["Accuracy"] ## just the accuracy of "cluster"
cm(cluster)                     ## confusion matrix and statistics of "cluster" (rows = predicted, columns = actual)

## YOUR CODE HERE


#####################
## 4. Beat the NMF ##
#####################

## YOUR CODE HERE

