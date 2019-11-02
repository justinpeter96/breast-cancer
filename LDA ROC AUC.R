library(readr)
BCD<-read_csv("desktop/BCD.csv")
colnames(BCD) <- c("id", "diagnosis","radius_mean","texture_mean","perimeter_mean","area_mean","smoothness_mean","compactness_mean","concavity_mean","concave points_mean","symmetry_mean","fractal_dimension_mean","radius_se","texture_se","perimeter_se","area_se","smoothness_se","compactness_se","concavity_se","concave points_se","symmetry_se","fractal_dimension_se","radius_worst","texture_worst","perimeter_worst","area_worst","smoothness_worst","compactness_worst","concavity_worst","concave points_worst","symmetry_worst","fractal_dimension_worst")
BCD.data <- as.matrix(BCD[,c(3:32)])
diagnosis <- as.numeric(BCD$diagnosis == "1")
BCD.pr <- prcomp(BCD.data, scale = TRUE, center = TRUE)
summary(BCD.pr)
BCD.pcs <- BCD.pr$x[,1:6]
BCD.pcst <- BCD.pcs
BCD.pcst <- cbind(BCD.pcs, diagnosis)
N <- nrow(BCD.pcst)
rvec <- runif(N)
BCD.pcst.train <- BCD.pcst[rvec < 0.75,]
BCD.pcst.test <- BCD.pcst[rvec >= 0.75,]
nrow(BCD.pcst.train)
nrow(BCD.pcst.test)
library(MASS)
BCD.pcst.train.df <- BCD.pcst.train
BCD.pcst.train.df <- as.data.frame(BCD.pcst.train)
BCD.lda <- lda(diagnosis ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6, data = BCD.pcst.train.df)
BCD.pcst.test.df <- BCD.pcst.test
BCD.pcst.test.df <- as.data.frame(BCD.pcst.test)
BCD.lda.predict <- predict(BCD.lda, newdata = BCD.pcst.test.df)
library("ROCR")
BCD.lda.predict.posteriors <- as.data.frame(BCD.lda.predict$posterior)
pred <- prediction(BCD.lda.predict.posteriors[,2], BCD.pcst.test.df$diagnosis)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values
plot(roc.perf)
abline(a=0, b= 1)
text(x=.4,y=.6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))

BCC <- read.csv(file="desktop/BCC.csv", sep=",")
BCC.data <- as.matrix(BCC[,c(1:9)])
classification <- as.numeric(BCC$Classification == "2")
BCC.pr <- prcomp(BCC.data, scale = TRUE, center = TRUE)
BCC.pcs <- BCC.pr$x[,1:4]
BCC.pcst <- BCC.pcs
BCC.pcst <- cbind(BCC.pcs, classification)
N <- nrow(BCC.pcst)
rvec <- runif(N)
BCC.pcst.train <- BCC.pcst[rvec < 0.75,]
BCC.pcst.test <- BCC.pcst[rvec >= 0.75,]
library(MASS)
BCC.pcst.train.df <- BCC.pcst.train
BCC.pcst.train.df <- as.data.frame(BCC.pcst.train)
BCC.lda <- lda(classification ~ PC1 + PC2 + PC3 + PC4, data = BCC.pcst.train.df)
BCC.pcst.test.df <- BCC.pcst.test
BCC.pcst.test.df <- as.data.frame(BCC.pcst.test)
BCC.lda.predict <- predict(BCC.lda, newdata = BCC.pcst.test.df)
library("ROCR")
BCC.lda.predict.posteriors <- as.data.frame(BCC.lda.predict$posterior)
pred <- prediction(BCC.lda.predict.posteriors[,2], BCC.pcst.test.df$classification)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values
plot(roc.perf)
abline(a=0, b= 1)
text(x=.4,y=.6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))

