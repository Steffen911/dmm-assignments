source("util.R")

########################
## 1. Intuition on SVD
########################

M1 <- matrix(c(1,1,1,0,0,
               1,1,1,0,0,
               1,1,1,0,0,
               0,0,0,0,0,
               0,0,0,0,0
               ), 5, 5, byrow=T)

M2 <- matrix(c(0,0,0,0,0,
               0,2,1,2,0,
               0,2,1,2,0,
               0,2,1,2,0,
               0,0,0,0,0
               ), 5, 5, byrow=T)

M3 <- matrix(c(0,0,0,0,
               0,1,1,1,
               0,1,1,1,
               0,1,1,1,
               0,1,1,1
               ), 5, 4, byrow=T)

M4 <- matrix(c(1,1,1,0,0,
               1,1,1,0,0,
               1,1,1,0,0,
               0,0,0,1,1,
               0,0,0,1,1
               ), 5, 5, byrow=T)

M5 <- matrix(c(1,1,1,0,0,
               1,1,1,0,0,
               1,1,1,1,1,
               0,0,1,1,1,
               0,0,1,1,1
               ), 5, 5, byrow=T)

M6 <- matrix(c(1,1,1,1,1,
               1,1,1,1,1,
               1,1,0,1,1,
               1,1,1,1,1,
               1,1,1,1,1
               ), 5, 5, byrow=T)

## 1a
## YOUR PART


## 1b
## YOUR PART

## 1c
## You can use the function "svdcomp" and "ggplotm" from util.R.
## YOUR PART


## 1d
## A better method to compute the rank in R is rankMatrix()
## YOUR PART


############################
## 2 The SVD on Weather Data
############# ###############

## Load the data
climate <- as.matrix(read.csv("data/worldclim.csv"))
coord <- as.matrix(read.csv("data/worldclim_coordinates.csv"))

## Plot the coordinates
plot(coord, asp=1, pch=16)

## 2a
## Compute the mean value for each column of climate and
## remove the mean from every value in that column (zero-centered).
## Call result climate.zc
## YOUR PART


## Compute the standard deviation and normalize every column
## of climate.zc to unit standard deviation. Call result
## climate.normal
## YOUR PART


## Some helpful plots for 2a

par(mfcol=c(3,4))

for (i in 1:12) hist(climate[,i], xlab=colnames(climate)[i])

for (i in 12+(1:12)) hist(climate[,i], xlab=colnames(climate)[i])

for (i in 24+(1:12)) hist(climate[,i], xlab=colnames(climate)[i])

for (i in 36+(1:12)) hist(climate[,i], xlab=colnames(climate)[i])

par(mfcol=c(1,1))

## 2b Compute the SVD of the normalized climate data and store it in variable
## climate.svd.  What is the rank of the data?
## YOUR PART


## 2c
## Here is an example
plot(coord, col=redgreen(climate.svd$u[,1]), asp=1, pch=16)
## YOUR PART


## 2d
## Here is an example
plot(climate.svd$u[,c(1, 2)], col=redgreen(coord[,2]-mean(coord[,2])), asp=1, pch=16)
## YOUR PART

## 2e
## 2e(i) Guttman-Kaiser
## YOUR PART

## 2e(ii) 90% squared Frobenius norm
## YOUR PART

## 2e(iii) Scree test
## YOUR PART

## 2e(iv) entropy
## YOUR PART

## 2e(v) random flips
## Frobenius norm of A: norm(A, "F")
## 2-norm of A        : norm(A, "2") or svd(A,0,0)$d[1]
## Random signs       : sample(c(-1,1), nrow(A)*ncol(A), replace=T)
## YOUR PART

## 2e What, if any, of these would be your choice?
## YOUR PART


## 2f
## here is the empty plot that you need to fill (one line per choice of k +
## one line for the RSME between climate.normal and the noisy versions)

plot(NA, NA, xlim=c(0,2), ylim=c(0,2), xlab="epsilon", ylab="RMSE")

## YOUR PART

####################
## 3 SVD and k-means
####################

## Cluster the normalized climate data into 5 clusters using k-means and store
## the vector giving the cluster labels for each location. Use 100 maximum
## iterations and 10 restarts.
climate.cluster <- kmeans(climate.normal, 5, iter.max=100, nstart=10)$cluster


## 3a
## Plot the results to the map: use the cluster labels to give the color to each
## point.
plot(coord, col=climate.cluster, pch=16)

## 3b
## YOUR PART HERE

## 3c
plot(coord, col=climate.cluster, pch=16)
dev.new() ## new plot window, old one stays

## Compute the PCA scores (based on k), store in P (dimensions: nrow(climate.normal) x k)
## YOUR PART HERE

## cluster and visualize
P.cluster <- kmeans(P, 5, iter.max=100, nstart=10)$cluster
P.cluster <- match.labels(P.cluster, climate.cluster) ## this matches cluster colors as well as possible (try without)
plot(coord, col=P.cluster, pch=16)

