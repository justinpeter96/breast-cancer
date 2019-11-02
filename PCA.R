BCC <- read.csv(file="desktop/BCC.csv", sep=",")
library(Matrix)
library(matrixStats)
library(pca3d)
colnames(BCC)
par(mar=c(1,1,1,1))
Mpca<-prcomp(BCC, center = TRUE, scale. = TRUE)
pca3d(Mpca, group = BCC$Classification, legend = "topright", show.ellipses = TRUE, show.plane = FALSE)
pca3d(Mpca, group = BCC$Classification, legend = "topright", biplot = TRUE, biplot.vars = 2)
pca3d(Mpca, group = BCC$Classification, legend = 'bottomright', show.shadows = TRUE)
pca3d(Mpca, group = BCC$Classification, legend = 'bottomright', show.centroids = TRUE,show.group.labels = TRUE)

library(readr)
BCD<-read_csv("desktop/BCD.csv")
colnames(BCD) <- c("id", "diagnosis","radius_mean","texture_mean","perimeter_mean","area_mean","smoothness_mean","compactness_mean","concavity_mean","concave points_mean","symmetry_mean","fractal_dimension_mean","radius_se","texture_se","perimeter_se","area_se","smoothness_se","compactness_se","concavity_se","concave points_se","symmetry_se","fractal_dimension_se","radius_worst","texture_worst","perimeter_worst","area_worst","smoothness_worst","compactness_worst","concavity_worst","concave points_worst","symmetry_worst","fractal_dimension_worst")
BCD<-BCD[1:200,2:32]
Mpca<-prcomp(BCD, center = TRUE, scale. = TRUE)
library(pca3d)
pca3d(Mpca, group = BCD$diagnosis, legend = "topright", show.ellipses = TRUE, show.plane = FALSE)
pca3d(Mpca, group = BCD$diagnosis, legend = "topright", biplot = TRUE, biplot.vars = 2)
pca3d(Mpca, group = BCD$diagnosis, legend = 'bottomright', show.shadows = TRUE)
pca3d(Mpca, group = BCD$diagnosis, legend = 'bottomright', show.centroids = TRUE,show.group.labels = TRUE)



