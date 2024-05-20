# wczytywanie danych za pomoc¹ biblioteki readxl
library(readxl)
dataSet <- read_excel("data/R_data.xlsx")

head(dataSet)
#View(dataSet)

# Analiza zmiennych niezale¿nych
## Dochody bud¿etów woj. ~ zgloszenia uprp
plot(zgloszenia_uprp~doch_budzet, data=dataSet)

regBudZglo <- lm(zgloszenia_uprp~doch_budzet, data=dataSet)
summary(regBudZglo)
abline(regBudZglo, col='red')

## Iloœæ styp_rektora ~ zgloszenia uprp
plot(zgloszenia_uprp~styp_rektora, data=dataSet)

regStypZglo <- lm(zgloszenia_uprp~styp_rektora, data=dataSet)
summary(regStypZglo)
abline(regStypZglo, col='red')

korDochStyp <- cor(
  dataSet$doch_budzet,
  dataSet$styp_rektora)

# multiple linear regression
# (Avg_flat_price ~ Flats_given_for_rental_or_sell + Students_count)
regMulVar <- lm(
  zgloszenia_uprp~styp_rektora+doch_budzet,
  data=dataSet)

summary(regMulVar)
