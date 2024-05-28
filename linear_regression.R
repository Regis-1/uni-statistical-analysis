# wczytywanie danych za pomoc¹ biblioteki readxl
library(readxl)
ds <- read_excel("data/R_data2.xlsx")

head(ds)
#View(ds)

plot(sklepy~linie_kol, data=ds)
rSL <- lm(sklepy~linie_kol, data=ds)
summary(rSL)
abline(rSL, col='red')

plot(sklepy~muzea, data=ds)
rSM <- lm(sklepy~muzea, data=ds)
summary(rSM)
abline(rSM, col='red')

cor(
  ds$linie_kol,
  ds$muzea
)

summary(
  lm(sklepy~muzea+linie_kol, data=ds)
)