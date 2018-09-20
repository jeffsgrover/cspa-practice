# default: prcomp, kmeans, hclust, dist, cutree

# PCA on US arrest data
pr.out <- prcomp(USArrests, scale=TRUE)
# Plot first two components
biplot(pr.out, scale=0)
# PVE of each component
# First get variance
pr.var <- pr.out$sdev^2
# Then divide by total variance
pve <- pr.var/sum(pr.var)
plot(pve)

# K-means clustering on simulated data
set.seed(2)
x <- matrix(rnorm(100), ncol=2)
x[1:25,1] <- x[1:25,1]+3
x[1:25,2] <- x[1:25,2]-4

km.out2 <- kmeans(x, 2, nstart=20)
km.out3 <- kmeans(x, 3, nstart=20)

# Hierarchical clustering




