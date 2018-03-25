## one file to rule them all
## install packages with: install.packages("Matrix") etc.
library(gplots)
library(Matrix)
library(plotrix)
library(grid)
library(clue)
library(ggplot2)
library(reshape)
library(caret)

## SVD #########################################################################

##' Takes a matrix (or its SVD decomposition) and returns the sum of the
##' specified components of its SVD decomposition
##' @param M     a matrix or the result of a call to svd()
##' @param which components to sum up (e.g. 1:2; default all)
svdcomp <- function(M, which=NULL) {
    if (is.matrix(M)) M <- svd(M)
    if (is.null(which)) which=1:length(M$d)
    D <- diag(M$d[which], length(which), length(which))
    M$u[,which,drop=FALSE] %*% D %*% t(M$v[,which, drop=FALSE])
}


## PLOTTING ####################################################################
##' Plot a matrix.
##'
##' @A the matrix
##' @low color for values smaller than midpoint
##' @high color for values larger than midpoint
##' @mid color for the midpoint
##' @midpoint value for midpoint
##' @show.grid whether to show grid lines between rows and columns
##' @show.legend whether to show the legend
##' @show.axis whether to show axis labels
##' @rotate.labels wheter to rotate column axis labels
##' @format format string for value labels (sprintf format)
##' @label.color color of value labels (set to NA to hide)
##' @ratio aspect ratio
##' @xlim x-axis limits (columns)
##' @ylim y-axis limits (rows)
##' 
##' @return a ggplot2 object (which can be further modified)
ggplotm <- function(A,
                    low="red", high="green", mid="white", midpoint=0,
                    show.grid = FALSE, show.legend=TRUE, show.axis=TRUE, rotate.labels=FALSE,
                    format="%.3g", label.color="black",
                    ratio=1, xlim=NULL, ylim=NULL) {                    
    X <- melt( A )
    levels1 <- rownames(A)
    if (is.null(levels1)) levels1 <- 1:nrow(A)
    levels2 <- colnames(A)
    if (is.null(levels2)) levels2 <- 1:ncol(A)
    X[,1] <- factor(X[,1], levels=rev(levels1))
    X[,2] <- factor(X[,2], levels=levels2)
    p <- ggplot(X, aes_string(x=colnames(X)[2], y=colnames(X)[1], fill="value",label="value")) +
        theme(panel.border = element_rect(colour = "black", fill=NA),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              axis.ticks.x=element_blank(), axis.ticks.y=element_blank()) +
       geom_tile(colour=if (show.grid) "black" else NA) + geom_text(label=sprintf(format, X[,3]), colour=label.color) +
       coord_fixed(ratio=ratio, xlim=xlim, ylim=ylim) +
           scale_fill_gradient2(low=low, high=high, mid=mid, midpoint=midpoint,
                                guide=if (show.legend) guide_colourbar(barheight=10, nbin=50) else "none") +
           scale_y_discrete(expand=c(0,0)) + scale_x_discrete(expand=c(0,0), position="top")
    if (!show.axis) {
        p <- p + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), 
                       axis.title.y=element_blank(), axis.text.y=element_blank())
    } else {
        if (is.null(names(dimnames(A)))) {
            p <- p + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
        }
         if (rotate.labels) {
            p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 0))
        }
    }
    p
}

##' Converts a vector of values into a color scale.
##' low values: blue, high values: red
##' BAD for visual perception; use redgreen instead
bluered <- function(x) {
    color.scale(x, cs1=c(0,1), cs2=c(0,0), cs3=c(1,0))
} 

##' Converts a vector of values into a color scale.
##' low values: red, 0 values: gray, high values: green
redgreen <- function(x) {
    ## based on perceived luminance between red/green
    color.scale(x, cs1=c(1,.95,0), cs2=c(0,.95, 0.299/0.587), cs3=c(0,.95,0), xrange=c(-1,1)*max(abs(x)))
} 

##' Converts matrix M to a grayscale image (in [0,1]). Negative values are
##' zeroed out. If there are entries >1, the entries are set to 1 (scale=F) or
##' the entire matrix is scaled down (scale=T).
imggray <- function(M, scale=F) {
    M[M<0] <- 0
    if (scale) {
        m <- max(M)
        if(m>0) M <- M/m
    } else {
        M[M > 1] <- 1
    }
    M
}

##' Plots the given matrix as a grayscale image
showgray <- function(M, scale=F) {
    grid.raster(imggray(as.matrix(M), scale))
} 

##' Plots the given matrix as a color image
showcol <- function(M, xrange=c(-1,1)*max(abs(M)), zerocol="black", legend=T, asp=1, ...) {
    ## compute colors
    M[M<xrange[1]] <- xrange[1]
    M[M>xrange[2]] <- xrange[2]
    cellcolors <- matrix("#000000", nrow(M), ncol(M))
    if (sum(M>0)>0)
        cellcolors[M>=0] <- color.scale(M[M>=0], xrange=c(0,xrange[2]), extremes=c(zerocol,"green"))
    if (sum(M<0)>0)
        cellcolors[M<0] <- color.scale(M[M<0], xrange=c(xrange[1],0), extremes=c("red",zerocol))

    ## compute legend
    legval<-c(seq(xrange[1],0,length.out=10)[1:9], seq(0,xrange[2],length.out=10))
    if (xrange[1]>=0) {
        legval<-seq(xrange[1],xrange[2],length.out=21)
    }
    
    legcol<-rep("#000000", length(legval))
    legcol[legval>=0]<-color.scale(legval[legval>=0], xrange=c(0,xrange[2]), extremes=c(zerocol,"green"))
    legcol[legval<0]<-color.scale(legval[legval<0], xrange=c(xrange[1],0), extremes=c("red", zerocol))

    xy <- par("usr")
    plot.din <- par("din")
    plot.pin <- par("pin")
    bottom.gap <- (xy[3] - xy[4]) * (plot.din[2] - plot.pin[2])/(2 * plot.pin[2])
    grx1 <- xy[1]
    gry1 <- bottom.gap * 0.95
    grx2 <- xy[1] + (xy[2] - xy[1])/4
    gry2 <- bottom.gap * 0.8

    color2D.matplot(M, cellcolors=cellcolors, asp=asp, border=NA, ...)
    if (legend) {
        if (xrange[1]<0) {
            color.legend(grx1, gry1, grx2, gry2, c(xrange[1],0,xrange[2]), rect.col=legcol)
        } else {
            color.legend(grx1, gry1, grx2, gry2, c(xrange[1],xrange[2]), rect.col=legcol)
        }
    }
}

##' Plots components of given decomposition LR (top-left to bottom-right)
##'
##' @L left matrix (m by r)
##' @R right matrix (r by n)
##' @r number of components to plot
##' @add add up components?
##' @scale to make things brighter; cut off values at fraction scale (<=1) of max
showcomps <- function(L, R, r=ncol(L), add=FALSE, scale=1) {
    m <- nrow(L)
    n <- ncol(R)
    rr <- as.integer(sqrt(r)+0.5)
    result <- matrix(0, m*rr, n*rr)
    x <- matrix(0, m, n)
    for (j in 1:rr) {
        for (i in 1:rr) {
            comp <- (j-1)*rr + i;
            if (comp <= r) {
                x.new <- L[,comp,drop=FALSE] %*% R[comp,,drop=FALSE]
                if (add) x <- x+x.new else x <- x.new
                result[((j-1)*m+1):(j*m), ((i-1)*n+1):(i*n)] <- x
            }
        }
    }
    showcol(result,xrange=c(-1,1)*max(abs(result))*scale, asp=1)
    for (i in 1:rr) {
        lines(c(0, rr*n), rep(m*(i-1), 2), col="white")
        lines(rep(n*(i-1), 2), c(0, rr*m), col="white")
    }
}

##' Plots the entries of a matrix.
##'
##' @param M the matrix
##' @param xrange smallest and largest value to plot (other values will be truncated)
##' @param zerocol which color to use for zero entries
##' @param legend whether to show the legend
##' @param show.values number of deciman digits to show (or FALSE to show no values)
##' @param ... additional arguments to color2D.matplot
plotm <- function(M, xrange=range(M), zerocol="white", legend=T, show.values=2, ...) {
    ## compute colors
    M[M<xrange[1]] <- xrange[1]
    M[M>xrange[2]] <- xrange[2]
    cellcolors <- matrix("#000000", nrow(M), ncol(M))
    if (sum(M>0)>0)
        cellcolors[M>=0] <- color.scale(M[M>=0], xrange=c(0,xrange[2]), extremes=c(zerocol,"green"))
    if (sum(M<0)>0)
        cellcolors[M<0] <- color.scale(M[M<0], xrange=c(xrange[1],0), extremes=c("red",zerocol))

    ## compute legend
    legval<-c(seq(xrange[1],0,length.out=10)[1:9], seq(0,xrange[2],length.out=10))
    if (xrange[1]>=0) {
        legval<-seq(xrange[1],xrange[2],length.out=21)
    }
    
    legcol<-rep("#000000", length(legval))
    legcol[legval>=0]<-color.scale(legval[legval>=0], xrange=c(0,xrange[2]), extremes=c(zerocol,"green"))
    legcol[legval<0]<-color.scale(legval[legval<0], xrange=c(xrange[1],0), extremes=c("red", zerocol))

    xy <- par("usr")
    plot.din <- par("din")
    plot.pin <- par("pin")
    bottom.gap <- (xy[3] - xy[4]) * (plot.din[2] - plot.pin[2])/(2 * 
                                                                 plot.pin[2])
    grx1 <- xy[1]
    gry1 <- bottom.gap * 0.95
    grx2 <- xy[1] + (xy[2] - xy[1])/4
    gry2 <- bottom.gap * 0.8

    color2D.matplot(M, cellcolors=cellcolors, show.values=show.values, ...)
    if (legend) {
        if (xrange[1]<0) {
            color.legend(grx1, gry1, grx2, gry2, c(xrange[1],0,xrange[2]), rect.col=legcol)
        } else {
            color.legend(grx1, gry1, grx2, gry2, c(xrange[1],xrange[2]), rect.col=legcol)
        }
    }
}
 

## CLUSTERING ##################################################################

##' Relabels the cluster labels of c1 to match the cluster lables of c2 as good
##' as possible.
##' @param c1 vector of cluster labels
##' @param c2 another vector of cluser labels
##' @return new cluster labels for c1
match.labels <- function (c1, c2) {
    matching <- minWeightBipartiteMatching(c1, c2)
    tmp <- c1
    sapply(1:length(matching),
                  function(i) { tmp[which(c1 == i)] <<- matching[i] })
    tmp
} 

## helper method
minWeightBipartiteMatching <- function(clusteringA, clusteringB) {
    idsA <- unique(clusteringA)  # distinct cluster ids in a
    idsB <- unique(clusteringB)  # distinct cluster ids in b
    nA <- length(clusteringA)  # number of instances in a
    nB <- length(clusteringB)  # number of instances in b
    if (length(idsA) != length(idsB) || nA != nB) {
        stop("number of cluster or number of instances do not match")
    }

    nC <- length(idsA)
    tupel <- c(1:nA)

    # computing the distance matrix
    assignmentMatrix <- matrix(rep(-1, nC * nC), nrow = nC)
    for (i in 1:nC) {
        tupelClusterI <- tupel[clusteringA == i]
        solRowI <- sapply(1:nC, function(i, clusterIDsB, tupelA_I) {
            nA_I <- length(tupelA_I)  # number of elements in cluster I
            tupelB_I <- tupel[clusterIDsB == i]
            nB_I <- length(tupelB_I)
            nTupelIntersect <- length(intersect(tupelA_I, tupelB_I))
            return((nA_I - nTupelIntersect) + (nB_I - nTupelIntersect))
        }, clusteringB, tupelClusterI)
        assignmentMatrix[i, ] <- solRowI
    }

    # optimization
    result <- solve_LSAP(assignmentMatrix, maximum = FALSE)
    attr(result, "assignmentMatrix") <- assignmentMatrix
    return(result)
}

## general nmi computation between the given cluster vectors
nmi <- function(x, opt) {
  p.x <- table(x)/length(x)
  p.y <- table(opt)/length(opt)
  if (length(p.x) != length(p.y)) {
    stop("Data has incorrect number of clusters")
  }
  p.xy <- table(x, opt)/length(x)

  H.xgy <- 0
  H.ygx <- 0
  H.xy <- 0
  for (i in 1:length(p.y)) {
    for (j in 1:length(p.y)) {
      if (p.xy[i,j] > 0) {
        H.xgy <- H.xgy + p.xy[i,j]*log(p.x[i]/p.xy[i,j])
        H.xy <- H.xy - p.xy[i,j]*log(p.xy[i,j])
      }
      if (p.xy[j,i] > 0) {
        H.ygx <- H.ygx + p.xy[j,i]*log(p.y[j]/p.xy[j,i])
      }
    }
  }
  I <- H.xy - H.xgy - H.ygx
  as.vector((H.xy - I)/H.xy)
}

## MATRIX COMPLETION ###########################################################

##' Computes the number of nonzeros for each row (or column)
##' @param indexes the row indexes (or column indexes) of each nonzero entry
##' @param l number of rows (or columns)
nnz <- function(indexes, l=max(indexes)) {    
    ## compute number of i's (the weird code makes sure that missing indexes
    ## are assigned a zero value
    n <- rep(0, l)
    com <- tapply(indexes, indexes, length)
    n[as.integer(names(com))] <- com
    n
} 


## NMF #########################################################################

## compute NMF with euclidean loss
lee01.gnmf <- function(D, r, max.epochs=500, rel=0.0001, reps=10) {
    cur.loss <- Inf
    result <- NA
    for (rep in 1:reps) {
        L0 <- matrix(runif(nrow(D)*r), nrow(D), r)
        R0 <- matrix(runif(r*ncol(D)), r, ncol(D))
        temp <- lee01.gnmf.LR(D, L0, R0, max.epochs, rel)
        old.loss <- cur.loss
        cur.loss <- norm(D-temp$L%*%temp$R, 'F')
        if (cur.loss < old.loss) result <- temp
    }

    ## order components by magnitude
    mag <- sapply(1:r, function(r) {
        crossprod(result$L[,r]) * crossprod(result$R[,r]) # frob norm of component
    })
    mag.order <- order(mag, decreasing=TRUE)
    cat("Component norms:", mag[mag.order],"\n")
    result$L <- matrix(result$L[,mag.order], nrow(D), r)
    result$R <- matrix(result$R[mag.order,], r, ncol(D))
    rownames(result$L) <- rownames(D)
    colnames(result$R) <- colnames(D)
    result
}

## helper method
lee01.gnmf.LR <- function(D, L, R, max.epochs, rel, eps=1E-9) {
    cur.loss <- norm(D-L%*%R, 'F')
    cat("Epoch 0 , loss", cur.loss, "\n")
    
    ## main loop
    for (epoch in 1:max.epochs) {
        R <- R * (t(L) %*% D + eps) / (crossprod(L) %*% R + eps)
        L <- L * (D %*% t(R) + eps)  / (L %*% tcrossprod(R) + eps)
        old.loss <- cur.loss
        cur.loss <- norm(D-L%*%R, 'F')
        cat("Epoch", epoch, ",", cur.loss, "\n")
        if (old.loss/cur.loss-1 < rel) break
    }
    
    list(L=L, R=R)
}

## compute the generlized KL divergence b/w v1 and v2
gkl.vector <- function(v1, v2) {
    kl <- v1 * log(v1/v2)
    kl[v1==0] <- 0
    gkl <- sum(kl) - sum(v1) + sum(v2)
    gkl
}

## compute the generlized KL divergence b/w D and LR
gkl <- function(D, L, R) {
    WH <- L %*% R
    kl <- D * log(D/WH)
    kl[D==0] <- 0
    gkl <- sum(kl) - sum(D) + sum(WH)
    gkl
}

## helper method
lee01.gkl.LR <- function(D, L, R, max.epochs, rel, eps=1E-9) {
    cur.loss <- gkl(D, L, R)
    cat("Epoch 0 , loss", cur.loss, "\n")
    
    ## main loop
    for (epoch in 1:max.epochs) {
        R <- R * ( diag(1/(colSums(L) + eps)) %*% (t(L) %*% (D / (L %*% R + eps)) ) )
        L <- L * ( ((D / (L %*% R + eps)) %*% t(R)) %*% diag(1/(rowSums(R) + eps)) )
        old.loss <- cur.loss
        cur.loss <- gkl(D, L, R)
        cat("Epoch", epoch, ",", cur.loss, "\n")
        if (old.loss/cur.loss-1 < rel) break
    }
    
    list(L=L, R=R)
}

## compute NMF with generalized KL loss
lee01.gkl <- function(D, r, max.epochs=500, rel=0.0001, reps=10) {
    cur.loss <- Inf
    result <- NA
    for (rep in 1:reps) {
        L0 <- matrix(runif(nrow(D)*r), nrow(D), r)
        R0 <- matrix(runif(r*ncol(D)), r, ncol(D))
        temp <- lee01.gkl.LR(D, L0, R0, max.epochs, rel)
        old.loss <- cur.loss
        cur.loss <- gkl(D, temp$L, temp$R)
        if (cur.loss < old.loss) result <- temp
    }
    
    rownames(result$L) <- rownames(D)
    colnames(result$R) <- colnames(D)
    result
}

## take LR and obtain factorization SLR, where
## - S is r-by-r diagonal
## - the columns of L each sum to 1
## - the rows of R each sum to 1
nmf.lsr <- function(result) {
    x <- colSums(result$L)
    y <- rowSums(result$R)
    L <- result$L %*% diag(1/x)
    rownames(L) = rownames(result$L)
    R <- diag(1/y) %*% result$R
    colnames(R) = colnames(result$R)
    S <- Diagonal(x=x*y)
    list(L=L, S=S, R=R)
}

## take LR and obtain factorization SLR, where
## - S is m-by-m diagonal
## - the rows of L each sum to 1
## - the rows of R each sum to 1
nmf.slr <- function(result) {
    y <- rowSums(result$R)
    R <- diag(1/y) %*% result$R
    L <- result$L %*% diag(y)
    x <- rowSums(L)
    L <- diag(1/x) %*% L
    L[is.nan(L)] <- 1./ncol(L) ## handle 0 rows
    S <- Diagonal(x=x)
    rownames(L) = rownames(result$L)
    colnames(R) = colnames(result$R)
    list(S=S, L=L, R=R)
} 


## SPECTRAL CLUSTERING #########################################################

## load (a sample of) the digit data
digits <- function() {
    D <- as.matrix(read.csv("data/digits.csv"))
    clusters <- D[,65]
    D <- D[order(clusters),1:64]
    clusters <- sort(clusters)

    ## retain 50 images per digit to make things faster
    indexes <- c()
    for (k in 0:9) {
        indexes = c(indexes, which(clusters==k)[1:50])
    }
    D <- D[indexes,]
    clusters <- clusters[indexes]
    D
} 

## plots a clustering of the digits
## "cluster" contains the cluster identifiers for each row of D
## "which" selects which digits to show digits (positions or indicator vector)
show.digits <- function(cluster, which=1:nrow(D)) {
    A <- NULL
    D <- D[which,]
    cluster <- cluster[which]
    for (i in 0:9) {
        Dc <- D[cluster==i,]
        if (is.null(nrow(Dc)) || nrow(Dc)==0) {
            A <- rbind(A, matrix(0, 8, if (is.null(A)) 8 else ncol(A)))
        } else {
            Acluster <- matrix(t(Dc), 8, nrow(Dc)*8)
            if (!is.null(A)) {
                if (ncol(Acluster)>ncol(A)) A <- cbind(A, matrix(0, nrow(A), ncol(Acluster)-ncol(A)))
                if (ncol(Acluster)<ncol(A)) Acluster <- cbind(Acluster, matrix(0, nrow(Acluster), ncol(A)-ncol(Acluster)))
            }
            A <- rbind(A, Acluster)
        }        
    }

    labels <- rep("", 80)
    labels[5 + (0:9)*8] <- 0:9
    suppressMessages(
        ggplotm(t(A), mid="black", label.color=NA) +
        theme(axis.title.x=element_blank(), axis.text.x=element_text(), 
              axis.title.y=element_blank(), axis.text.y=element_blank()) +
        scale_x_discrete(expand=c(0,0), position="top", labels=labels)
    )
}

## compute full neighborhood graph (returns adjacency matrix)
fullgraph <- function(data, sigma=1, method="euclidean") {
    d <- dist(data, method=method)
    d <- as.matrix(d)
    d <- exp(-d^2 / (2*sigma^2))
    for (i in 1:nrow(data)) d[i,i] <- 0
    Matrix(d)
}

## compute the eps-neighborhood graph (returns adjacency matrix)
epsgraph <- function(data, eps, sigma=1, method="euclidean", d=dist(data,method), tosim=function(x) exp(-x^2 / (2*sigma^2) )) {
    d <- as.matrix(d)
    i <- d>eps
    d[i] <- 0
    d[!i]=tosim(d[!i])
    for (i in 1:nrow(data)) d[i,i] <- 0
    Matrix(d)
}

## compute the knn graph (returns adjacency matrix)
knngraph <- function(data, k, sigma=1, symmetric=TRUE, method="euclidean", d=dist(data,method), tosim=function(x) exp(-x^2 / (2*sigma^2) )) {
    d <- as.matrix(d)
    r <- matrix(0, nrow(d), ncol(d))
    for (i in 1:nrow(data)) {
        o <- order(d[i,], decreasing=F)
        r[i,o[2:(k+1)]] <- 1
    }
    if (symmetric) {
        r <- matrix( as.double(r | t(r)), nrow(d), ncol(d) )
    } else {
        r <- matrix( as.double(r & t(r)), nrow(d), ncol(d) )
    }
    r[r>0] <- tosim(d[r>0])
    Matrix(r)
}

## compute the graph laplacian (W is an adjacency matrix)
laplacian <- function(W, type=c("unnormalized", "symmetric", "walk")) {
    D <- Diagonal(x=rowSums(W))
    L <- D - W
    switch(type[1],
           unnormalized = L,
           symmetric = sqrt(solve(D)) %*% L %*% sqrt(solve(D)),
           walk = solve(D) %*% L,
           stop("unknown type")
           )
}
