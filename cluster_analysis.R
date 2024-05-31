install.packages('readxl')
install.packages('clusterSim')
install.packages('factoextra')
library(readxl)
library(clusterSim)
library(factoextra)
setwd('uni-statistical-analysis')

ds <- read.csv("data/R_skupienia.csv", header=TRUE, sep=';')
head(ds)

subSet <- ds[, 2:6]
ssAsMat <- as.matrix(subSet)
head(ssAsMat)

normalized = data.Normalization(ssAsMat, type="n1")
head(normalized)

kmOut <- kmeans(normalized, 2)
list(skupienia=kmOut$cluster, srodki=kmOut$centers, rozmiarGrupy=kmOut$size)
list(calkSumaKwad=kmOut$totss, calkSumaKwadWew=kmOut$tot.withinss, calkSumaKwadPomiedzy=kmOut$betweenss)

# dobor k - metoda lokcia
wss <- tibble::tibble(k=1:10, wcss=NA)
for (i in wss$k)
  wss$wcss[i] <- kmeans(x=normalized, centers=i)$tot.withinss
wss
plot(wss, type='b', xlab='Liczba skupien', ylab='Wewnatrzgrupowa suma kwadratow')

fviz <- factoextra::fviz_nbclust(normalized, FUN=kmeans, method='wss')
fviz$data
fviz

kmOut2 <- kmeans(normalized, 4)
list(skupienia=kmOut2$cluster, srodki=kmOut2$centers, rozmiarGrupy=kmOut2$size)
list(calkSumaKwad=kmOut2$totss, calkSumaKwadWew=kmOut2$tot.withinss, calkSumaKwadPomiedzy=kmOut2$betweenss)

cluster1 <- ssAsMat[kmOut2$cluster == 1, ]
cluster2 <- ssAsMat[kmOut2$cluster == 2, ]
sd_cluster1 <- apply(cluster1, 2, sd)
sd_cluster2 <- apply(cluster2, 2, sd)
sd_cluster1; sd_cluster2;
(100*(kmOut2$betweens/kmOut2$totss))
