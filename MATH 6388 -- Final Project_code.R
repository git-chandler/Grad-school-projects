## This project utilizes support vector machines to predict images of handwritten numbers.

## Column one is the verified number.  Columns 2:257 are the pixels that constitute the number.

rm(list=ls())
setwd("D:\\MATH\\MATH 6388 -- Machine Learning\\Projects\\Final Project")

install.packages(c("scatterplot3d",
                   "hexbin"))

library(scatterplot3d)
library(hexbin)

zipTrain = read.table("zip.train")
zipTest = read.table("zip.test")

View(zipTrain)
dim(zipTrain)
str(zipTrain)
zipTrain[1:10,]

View(zipTest)
dim(zipTest)
str(zipTest)
zipTest[1:10,]

## plot a few images
im1 <- matrix(as.numeric(zipTrain[2, 2:257]), nrow = 16, ncol = 16)
im2 <- matrix(as.numeric(zipTrain[6, 2:257]), nrow = 16, ncol = 16)
im3 <- matrix(as.numeric(zipTrain[4, 2:257]), nrow = 16, ncol = 16)
im4 <- matrix(as.numeric(zipTrain[22, 2:257]), nrow = 16, ncol = 16)

png(filename = "numbers.png")
par(mfrow=c(2,2))
image(t(apply(-im1, 1, rev)), col = gray((0:32)/32))  ## this is a 5
image(t(apply(-im2, 1, rev)), col = gray((0:32)/32))  ## this is a 6
image(t(apply(-im3, 1, rev)), col = gray((0:32)/32))  ## this is a 7
image(t(apply(-im4, 1, rev)), col = gray((0:32)/32))  ## this is an 8
dev.off()

## density plots
par(mfrow=c(1,1))
hist(zipTrain$V1, col = "red") ## outcome variable; notice the concentration at 0 and 1

par(mfrow=c(2,2))
plot(density(zipTrain[, 2]))
plot(density(zipTrain[, 57]))
plot(density(zipTrain[, 100]))
plot(density(zipTrain[, 200]))
## seems like the density for most variables is concentrated at
## -1.0 and +1.0

## qqplots
par(mfrow=c(1,1))
qqnorm(zipTrain$V1) ## outcome variable; approximately normal

par(mfrow=c(2,2))
qqnorm(zipTrain[, 2])
qqnorm(zipTrain[, 57])
qqnorm(zipTrain[, 100])
qqnorm(zipTrain[, 200])

## 2D scatterplots for the written number against 4 predictors
par(mfrow=c(2,2))
plot(zipTrain$V2, zipTrain$V1, col = rgb(0, 100, 0, maxColorValue = 255))
plot(zipTrain$V57, zipTrain$V1, col = rgb(139, 0, 0, 50, maxColorValue = 255))
plot(zipTrain$V100, zipTrain$V1, col = rgb(0, 0, 139, 50, maxColorValue = 255))
plot(zipTrain$V200, zipTrain$V1, col = rgb(255, 165, 0, maxColorValue = 255))

## 3D scatterplots for the written number against 8 predictors
par(mfrow=c(1,1))
scatterplot3d(zipTrain$V2, zipTrain$V54, zipTrain$V1, highlight.3d = TRUE)
scatterplot3d(zipTrain$V57, zipTrain$V87, zipTrain$V1, highlight.3d = TRUE)
scatterplot3d(zipTrain$V100, zipTrain$V195, zipTrain$V1, highlight.3d = TRUE)
scatterplot3d(zipTrain$V200, zipTrain$V218, zipTrain$V1, highlight.3d = TRUE)

## hexbin plot
par(mfrow=c(2,2))
plot(hexbin(zipTrain$V2, zipTrain$V1))
plot(hexbin(zipTrain$V57, zipTrain$V1))
plot(hexbin(zipTrain$V100, zipTrain$V1))
plot(hexbin(zipTrain$V200, zipTrain$V1))

sumStat = do.call(data.frame,
                  list(mean = apply(zipTrain, 2, mean),
                       stdev = apply(zipTrain, 2, sd),
                       median = apply(zipTrain, 2, median),
                       min = apply(zipTrain, 2, min),
                       max = apply(zipTrain, 2, max),
                       n = apply(zipTrain, 2, length)))
View(sumStat)

