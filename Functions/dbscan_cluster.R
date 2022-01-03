dbscan_cluster <- function(data){
# data: a dataframe containing tagId, x, y, record_timestamp, date, ...
  
  library(fpc) # for dbscan clustering
  library(dbscan) # for KNN dbscan
  library(raster) # for distance calculation
  
  # Fitting DBScan clustering Model 
xy <- data[which(is.na(data$x)==FALSE),]

# dbscan::kNNdistplot(xy[,2:3], k =  5)
# abline(h = 0.15, lty = 2)

# to training dataset
set.seed(220)  # Setting seed
Dbscan_cl <- dbscan(xy[,2:3], eps = 0.15, MinPts = 5)
Dbscan_cl
# Checking cluster
# Dbscan_cl$cluster

# Table
clusters <- table(Dbscan_cl$cluster)
nb.clusters <- length(clusters)
nb.clusters

# Plotting Cluster
plot(Dbscan_cl,xy[,2:3], main = paste(tagId, "clustering", sep = " "))
# plot(Dbscan_cl, data, main = "Petal Width vs Sepal Length")

indx.out.points <- which(Dbscan_cl$cluster==0)

indx.stable.points <- which(Dbscan_cl$cluster==1)

corr.data <- xy
rownames(corr.data) <- NULL
for(k in 1:(nb.clusters-1)){
  indx.stable.points <- which(Dbscan_cl$cluster==k)
  # s <- min(indx.stable.points): max(indx.stable.points)
  # corr.data[s, 2] <- mean(xy[s, 2])
  # corr.data[s, 3] <- mean(xy[s, 3])
  corr.data$x[indx.stable.points] <- mean(xy$x[indx.stable.points])
  corr.data$y[indx.stable.points] <- mean(xy$y[indx.stable.points])
}

p1 <- plan +
  geom_point(data = xy[,2:3], aes(x, y), size=0.1, color="red")+
  ggtitle("Raw data")
print(p1)

# p1.stable <- plan + 
#   geom_point(data = xy[indx.stable.points,2:3], aes(x, y), size=0.1, color="green")+
#   ggtitle("Stabilized data")
# print(p1.stable)

p1.stable <- ggplot(data = corr.data[,2:3], aes(x, y, label=rownames(corr.data)))+
  geom_point(size=0.1,color="green")
p1.stable+geom_text(position = position_dodge(width=0.9),  size=2, hjust=1.5)


p1.stable.brut <- plan+ 
  geom_point(data = xy[,2:3], aes(x, y), size=0.1, color="red")+
  geom_point(data = corr.data[,2:3], aes(x, y), size=0.1, color="green")+
  scale_color_identity(guide = "legend")+
  ggtitle("Raw data vs stabilizes data")
print(p1.stable.brut)

return(corr.data)
}