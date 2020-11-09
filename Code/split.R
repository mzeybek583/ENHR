
## Split ENH

# Coder: Assist. Prof. Dr. Mustafa Zeybek
# Artvin Coruh University


# Load Libraries ----------------------------------------------------------


library(lidR)
library(dbscan)


# Load PCL Data -----------------------------------------------------------


las <- readLAS(files = "./Data/cross_section_data_voxel_output.las")

crs <- sp::CRS("+init=EPSG:5255")# Overwrite the CRS (but does not reproject)

projection(las) <- crs
sf::st_crs(las)$input

#plot(las)

las.enh = filter_poi(las, Classification == 14L)

#plot(las.enh)

df <- data.frame(x=las.enh@data$X, y=las.enh@data$Y, z=las.enh@data$Z) 

#Calculate and plot the k-Nearest Neighbor Distance
kNNdistplot(df, k = 3)
abline(h=.15, col = "red", lty=2)

# Parameters for DBSCAN
eps=.15


fr <- frNN(df, eps, search = "kdtree")

# Compute DBSCAN using fpc package
#library("fpc")

#cl <- dbscan::dbscan(df, eps = 0.2, minPts = 7)
#cl <- dbscan::dbscan(df, minPts = 7)
cl <- dbscan::dbscan(fr, minPts = 5)
cl
# i=1
# #### renumbering require!!
# for (i in 1:length(unique(cl$cluster))) {
#   print(i)
#   cl2 <- cl$cluster[cl$cluster == i]
#   if (length(cl2) <= 300 ) {
#     cl$cluster[cl$cluster == i] <- 0
#   }
# }

# Plot --------------------------------------------------------------------
## plot clusters and add noise (cluster 0) as crosses.
plot(df$x, df$y, col=cl$cluster, cex=0.1, xlab="Y", ylab="X")
points(df[cl$cluster==0,], pch = 3, col = "red")

class <- cl$cluster
df.out <- data.frame(X=df$x, Y=df$y, Z=df$z, Classification=class)


# Write to file -----------------------------------------------------------
dir.create(file.path("Data/", "Results"), showWarnings = FALSE)
write.csv(df.out, "./Data/Results/dbscan.out.txt")

