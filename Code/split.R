
## Split ENH

# Coder: Assist. Prof. Dr. Mustafa Zeybek
# Artvin Coruh University



# Load Libraries ----------------------------------------------------------


library(lidR)

las <- readLAS(files = "./Data/cross_section_data.las")

# Overwrite the CRS (but does not reproject)
crs <- sp::CRS("+init=EPSG:5255")
projection(las) <- crs
sf::st_crs(las)$input
plot(las)

las.enh = filter_poi(las, Classification == 14L)

plot(las.enh)


df <- data.frame(x=las.enh@data$X, y=las.enh@data$Y, z=las.enh@data$Z) 

library(dbscan)

kNNdistplot(df, k = 6)
abline(h=.2, col = "red", lty=2)

eps=.15

minpts = 100

fr <- frNN(df, eps)

cl <- dbscan(fr, minpts)
cl

## plot clusters and add noise (cluster 0) as crosses.
plot(df$y, df$z, col=cl$cluster)
points(df[cl$cluster==0,], pch = 3, col = "red")

class <- cl$cluster
df.out <- data.frame(X=df$x, Y=df$y, Z=df$z, Classification=class)


# Write to file -----------------------------------------------------------


dir.create(file.path("Data/", "Results"), showWarnings = FALSE)
write.csv(df.out, "./Data/Results/dbscan.out.csv")
