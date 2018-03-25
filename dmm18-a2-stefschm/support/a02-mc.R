## you may need to install these two packages; type:
## install.packages("grid")
## install.packages("Matrix")
## install.packages("jpeg")

library(grid)
library(Matrix)
library(jpeg)

source("util.R")


## read the sample
D = readMM("data/image_sample.mmc")
Dsummary <- summary(D)
names(Dsummary) <- c("i", "j", "d")
m <- nrow(D)               # number of rows
n <- ncol(D)               # number of columns
is <- Dsummary$i           # row of each revealed entry
js <- Dsummary$j           # column of each revealed entry
ds <- Dsummary$d           # value of each revealed entry
N <- length(is)            # number of revealed entries
Nis <- nnz(is, m)          # for each row, number of revealed entries
Njs <- nnz(js, n)          # for each column, number of revealed entries

## show the sample
showgray(D)

## randomly permute the rows and columns of the data
Prow <- as(sample(m), "pMatrix")
Pcol <- as(sample(n), "pMatrix")
Dperm <- Prow %*% D %*% Pcol

## this is how a matrix factorization algorithm sees the data
## -> all the "visible structure" is gone
showgray(Dperm)

## but it's still the same data (let's permute back)
showgray(solve(Prow) %*% Dperm %*% solve(Pcol))

## example usage: print the 10th revealed entry
cat("D[", is[10], ",", js[10], "]=", ds[10], "\n", sep="")

## pick a starting point (rank 10)
set.seed(1, kind="Mersenne-Twister")
r <- 10
L0.r10 <- matrix(rnorm(m*r), m, r)/sqrt(r)
R0.r10 <- matrix(rnorm(r*n), r, n)/sqrt(r)

## norm of starting point (should give 16.05334 and 18.04857)
norm(L0.r10, 'F')
norm(R0.r10, 'F')

## Compute the loss of factorization LR with L2 regularization (parameter lambda)

loss <- function(L, R, lambda) {
    ### YOUR CODE HERE ###
}

## loss of starting point with lambda=2 (should give 3865.816)
loss(L0.r10, R0.r10, 2)

## Compute gradient w.r.t. l_i and r_j of the local loss of the p-th revelead
## entry for factorization LR with L2 regularization (parameter lambda). Returns
## a list containing vector elements Li and Rj.
dlossp <- function(L, R, lambda, p) {
    i <- is[p]
    j <- js[p]
    d <- ds[p]

    ### YOUR CODE HERE ###
    ## create two length-r vectors dLi and dRj
    ## * dLi[k] contains the gradient of the local loss with respect to l_ik
    ## * dRi[k] contains the gradient of the local loss with respect to r_kj

    ## return the result as a list
    ## (elements can be accessed with x$Li or x$Ri, where x is the returned list)
    list(Li=dLi, Rj=dRj)
}

## test local gradient computation for 10th entry; should give:
## $Li
##  [1] -0.022076386  0.055660021  0.016555157  0.068076690  0.025315001
##  [6]  0.001815675  0.078699632  0.026119761  0.013711859  0.004034443
##
## $Rj
##  [1] -0.03494084  0.07994425  0.05441889  0.05581750  0.01939717 -0.01477114
##  [7]  0.04630433  0.02940866 -0.05088697  0.01276903
dlossp(L0.r10, R0.r10, 2, 10)

## Run a gradient descent epoch (L and R are starting points, eps is
## stepsize). Returns a list with matrix elements L and R, containing the
## updated factor matrices.
gdepoch <- function(L, R, lambda, eps) {
    ## create gradient matrices
    dL <- matrix(0, m, r)
    dR <- matrix(0, r, n)

    ### YOUR CODE HERE ###
    ## fill the gradient matrices using repeated calls to
    ## your dlossp function

    ### YOUR CODE HERE ###
    ## perform a gradient step on L and R with step size eps
    ## by using the gradient matrices

    ## return result
    list(L=L, R=R)
}

## test the gd epoch and print 5th row of L; should give
##  [1]  0.07822179  0.29110406  0.67363404 -0.27527854 -0.36139164  0.20011846
##  [7] -0.35296692  0.08101832 -0.01417779  0.01627573
gdepoch(L0.r10, R0.r10, 2, 0.01)$L[5,]

##' Runner for gradient descent (or stochastic gradient descent) for the
##' specified number of epochs.
##'
##' @param f function for running an epoch. Takes parameters L, R, lambda, and
##'          eps.
##' @param L0 starting point for L
##' @param R0 starting point for R
##' @param lambda regularization parameter
##' @param epoch number of epochs to run
##' @param Initial step size. Updated using bold driver.
##'
##' @return a list with matrix elements L and R, containing the
##'         updated factor matrices
runner <- function(f, L0, R0, lambda, epochs=50, eps=0.01) {
    LR <- list(L=L0, R=R0)
    cur.loss <- loss(LR$L, LR$R, lambda)
    for (epoch in 1:epochs) {
        cat("Epoch", epoch, ", loss:", cur.loss , "\n")
        LR <- f(LR$L, LR$R, lambda, eps)

        ## bold driver step size update
        old.loss <- cur.loss
        cur.loss <- loss(LR$L, LR$R, lambda)
        if (old.loss<cur.loss) {
            eps <- eps/2
        } else {
            eps <- eps*1.05
        }
    }
    LR
}

## example run
result.gd.r10.l2 <- runner(gdepoch, L0.r10, R0.r10, 2, epochs=5, eps=0.01)

## show result
showgray(result.gd.r10.l2$L %*% result.gd.r10.l2$R)

## nothing much to see, let's run more epochs
result.gd.r10.l2 <- runner(gdepoch, result.gd.r10.l2$L, result.gd.r10.l2$R, 2, epochs=10, eps=0.01)

## show result
showgray(result.gd.r10.l2$L %*% result.gd.r10.l2$R)

## getting better, run even longer (this takes a while)
result.gd.r10.l2 <- runner(gdepoch, result.gd.r10.l2$L, result.gd.r10.l2$R, 2, epochs=35, eps=0.01)

## show result
showgray(result.gd.r10.l2$L %*% result.gd.r10.l2$R)

## you can save all your variables as follows so that you do not
## have to repeat computations
## save(list = ls(all=TRUE), file = "mydata.RData")

## and load it back in
## load("mydata.RData")

## Run a stochastic gradient descent epoch. Uses same paramters and return
## values as gdepoch.
sgdepoch <- function(L, R, lambda, eps) {
    for (p in sample(length(is))) {
        i <- is[p]
        j <- js[p]

        ### YOUR CODE HERE ###
        ## perform a stochastic gradient descent step on revealed entry p
        ## with step size eps
        ## Use dlossp to compute the local gradient of entry (i,j), then update
        ## L[i,] and R[,j] using step size eps. Do not scale up this gradient
        ## by the number N of nonzero entries (i.e., proceed differntly as in
        ## the lecture slides)
    }

    ## return result
    list(L=L, R=R)
}

## test the sgd epoch and print 5th row of L; should give
## [1]  0.08011912  0.29800567  0.68218450 -0.27644483 -0.37742701  0.20463166
## [7] -0.36247519  0.07971791 -0.01823091  0.01838559
set.seed(2, kind="Mersenne-Twister")
sgdepoch(L0.r10, R0.r10, 2, 0.01)$L[5,]

## example run (this takes a while)
result.sgd.r10.l2 <- runner(sgdepoch, L0.r10, R0.r10, 2)

## show result
showgray(result.sgd.r10.l2$L %*% result.sgd.r10.l2$R)

## YOUR CODE HERE ##
## now try values of lambda, ranks, etc. as described in the
## assignment


