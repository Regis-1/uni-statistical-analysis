library(readxl)
library(clusterSim)
library(glmnet)

ds <- read.csv("data/R_skupienia.csv", header=TRUE, sep=';')
head(ds)

A <- ds[, 2:6]
# niektore funkcje z clusterSim przyjmuja tylko macierz jako argument
M <- as.matrix(A)
# podglad macierzy
head(M)


# 2) normalizacja za pomoc¹ funkcji dataNormalization
B = data.Normalization(M, type="n1")
head(B)

# 3) miara odleg³oœci - Euklides, miejska, Braya-Curtisa,
dystans = dist(B, method="euclidean")
#head(dystans)

#Metoda k-œrednich
ksrednia <- kmeans(A, 5)
ksrednia$centers
ksrednia$size

plot(A, pch=ksrednia$cluster)
