# wczytywanie danych za pomoc¹ biblioteki readxl
library(readxl)
dataSet <- read_excel("data/R_data.xlsx")

head(dataSet)
#View(dataSet)

plot(sr_cena_1m2~biblioteki_pub, data=dataSet)
regBudSrCena <- lm(sr_cena_1m2~biblioteki_pub, data=dataSet)
summary(regBudSrCena)
abline(regBudSrCena, col='red')

plot(sr_cena_1m2~imprezy_rozryw, data=dataSet)
regBudWydat <- lm(sr_cena_1m2~imprezy_rozryw, data=dataSet)
summary(regBudWydat)
abline(regBudWydat, col='red')

korDochStyp <- cor(
  dataSet$biblioteki_pub,
  dataSet$imprezy_rozryw)

regMulVar <- lm(
  sr_cena_1m2~imprezy_rozryw+biblioteki_pub,
  data=dataSet)

summary(regMulVar)