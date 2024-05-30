# wczytywanie danych za pomoc¹ biblioteki readxl
library(readxl)
ds <- read_excel("data/R_liniowa.xlsx")

head(ds)
summary(ds)

cor(ds$sklepy, ds$linie_kol)
cor(ds$sklepy, ds$przystanki)
cor(ds$przystanki, ds$linie_kol)

mRegLiniowej <- lm(sklepy~przystanki+linie_kol, data=ds)
summary(mRegLiniowej)