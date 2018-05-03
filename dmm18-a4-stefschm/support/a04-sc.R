source("util.R")

## help function for plotting a matrix or a similarity matrix against black background
show <- function(M, ...) {
    ggplotm(as.matrix(M), mid="black", label.color=NA, show.axis=FALSE, ...)
}

## load digits data
## rows correspond to digits, columns to pixels (8x8 -> 64 columns)
D <- digits()

## visualize a digit (the 200-th one)
show( t(matrix(D[200,], 8, 8)) )

## these are the correct labels (data is organized in 50 rows per digit)
labels <- rep(0:9, each=50)

## show this "clustering" (each column = one cluster)
show.digits(labels)

## function to relabel cluster indicaters to match optimal clustering to the
## extent possible (try with and without)
match <- function(cluster) {
    match.labels(cluster+1, labels+1)-1
}

## compute a confusion matrix
cm <- function(cluster.ids) {
  cluster.ids.matched <- match.labels(cluster.ids, labels)
  u = union(cluster.ids.matched, labels)
  t = table(factor(cluster.ids.matched, u), factor(labels, u))
  confusionMatrix(t)
}

## run k-means clustering
cluster <- match( kmeans(D, 10, nstart=100)$cluster - 1 )

## let's see how good the clustering is with respect to the true numbers
cm(cluster)$overall["Accuracy"] # 82%
cm(cluster)

## visualize the clusters (each column is one cluster)
show.digits(cluster)

## just the errors
show.digits(cluster, cluster!=labels)

## print/plot cluster sizes
table(cluster)
barplot( table(cluster) )

## Some relevant functions for what follows
## - Compute full similarity graph: W <- fullgraph(D, sigma)
## - Compute eps graph: W <-epsgraph(D, eps, sigma)
## - Compute kNN graph: W <- knngraph(D, k, sigma, symmetric), where symmetric in { TRUE, FALSE }
## - Show histogram of similarities: hist(as.vector(W))
## - Show histogram of "large" similarities: hist(as.vector(W[W>0.1]))
## - Compute graph Laplacian: laplacian(W, type) where type in { "unnormalized", "symmetric", "walk" }
## - Eigenvalue/-vector computation: eigen(L)

## full similarity graph
W <- fullgraph(D, sigma=50)
show(W)

## note that we can already see the clusters here because our data is ordered nicely
## this is what our algorithms see, however
P <- as(sample(nrow(D)), "pMatrix") # random permutation matrix
show(P %*% W %*% t(P))

## corresponding unnormalized graph laplacian
L = laplacian(W, "unnormalized")
show(L)
show(pmin(L, 1))  ## truncate values >1

## cluster vector for testing (it's a k-means result)
cluster.test <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 1, 1, 2, 1, 1, 1, 1, 8, 1, 1,
                  1, 1, 8, 8, 8, 8, 8, 8, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 8, 1, 3,
                  1, 1, 3, 3, 3, 3, 3, 8, 1, 1, 1, 8, 1, 1, 8, 1, 1, 2, 2, 2, 2,
                  2, 2, 2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 3, 3, 2, 2, 2, 2, 2, 2, 2,
                  2, 2, 2, 2, 2, 3, 8, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                  2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 7, 3, 3, 3, 3,
                  3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 7, 3, 3, 3, 3, 3, 3,
                  3, 3, 3, 3, 3, 3, 2, 2, 3, 3, 3, 3, 9, 9, 9, 9, 1, 9, 9, 4, 4,
                  4, 4, 4, 4, 4, 9, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
                  4, 4, 4, 9, 4, 9, 9, 9, 9, 9, 9, 4, 6, 4, 4, 4, 4, 4, 4, 4, 9,
                  5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
                  5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 3, 5, 5, 5,
                  5, 5, 5, 5, 3, 3, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
                  6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
                  6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7,
                  7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
                  7, 1, 7, 7, 7, 7, 7, 7, 7, 7, 9, 9, 7, 7, 7, 7, 7, 7, 7, 7, 7,
                  9, 7, 8, 8, 8, 0, 0, 8, 8, 8, 8, 8, 8, 8, 8, 8, 5, 8, 8, 8, 8,
                  8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
                  8, 8, 8, 8, 8, 8, 6, 8, 3, 6, 9, 9, 9, 9, 9, 3, 9, 9, 9, 7, 9,
                  9, 9, 7, 9, 3, 3, 3, 3, 3, 9, 9, 1, 9, 9, 9, 9, 9, 9, 9, 3, 9,
                  9, 9, 8, 3, 8, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3)


#############################
## 1: COMPUTING CUT VALUES ##
#############################

## function to compute cluster assignment matrix
## input: cluster = vector of cluster identifiers (0-9), one for each row of D
## output: corresponding cluster assignment matrix
cam <- function(cluster) {
    C <- matrix(0, nrow(D), 10)
    colnames(C) <- 0:9

    ## YOUR CODE HERE

    C
}

## test your method
C <- cam(cluster.test)
colSums(C) # 51 34 49 84 34 44 54 50 58 42
colSums(C[1:100,]) ## 49 31  2  6  0  0  1  0 11  0
sort(unique(as.vector(C))) ## [1] 0 1
range(rowSums(C)) ## [1] 1 1

## compute the value of cut, ratio cut, and normalized cut given an adjacency
## matrix and a vector of cluster identifiers
cut <- function(W, cluster) {
    ## helpful matrices
    D <- diag( diag(L) )
    L <- laplacian(W, "unnormalized")
    C <- cam(cluster)

    ## YOUR CODE HERE (recall exercise 5.3 + solution)
    cut <- 0
    rcut <- 0
    ncut <- 0

    result <- c(cut, rcut, ncut)
    names(result) <- c("cut", "rcut", "ncut")
    result
}

## test your method
W <- fullgraph(D, sigma=50)
cut(W, cluster.test) ## 67421.076896  2705.907871     8.763376

## let's compare
compare <- function(W, cluster) {
    M <- rbind( cut(W, labels), cut(W, cluster) )
    rownames(M) <- c("label", "clustering")
    M
}
compare(W, cluster.test)

## or visually
barplot( compare(W, cluster), beside=TRUE, legend=c("labels", "clustering") )

############################
## 2: SIMILARITY GRAPHS   ##
############################

## YOUR CODE HERE


############################
## 3: SPECTRAL CLUSTERING ##
############################

## plot each digit's embeddings along eigenvectors of given eigenvalues numbers
plot.eigen <- function(L, which, xlim=c(-0.1,0.1), ylim=c(-0.1,0.1)) {
    E <- eigen(L)$vectors[,which]
    d <- data.frame(E)
    colnames(d) <- which
    plot(d, col=labels, xlim=xlim, ylim=ylim, asp=1)
}

plot.eigen( laplacian(W, "unnormalized"), nrow(D)-(0:9) )

## plot embeddings for a given digit
digit <- 3
ggplotm({ M <- eigen(L)$vectors[labels==digit,nrow(D)-(0:9)]; colnames(M) <- nrow(D)-0:9; M }, label.color=NA)

## YOUR CODE HERE

