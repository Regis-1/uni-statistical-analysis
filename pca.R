install.packages('clusterSim')
install.packages('psych')
library(clusterSim)
library(psych)
setwd('uni-statistical-analysis')

ds <- read_excel('data/R_pca.xlsx')
head(ds)
dsAsMat <- as.matrix(ds[,2:12])
colnames(dsAsMat) <- colnames(ds)[2:12]
head(dsAsMat)

pca <- princomp(dsAsMat)
summary(pca)
kmo <- KMO(dsAsMat)
kmo

plot(pca)
biplot(pca)
