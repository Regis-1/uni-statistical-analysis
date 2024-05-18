# reding data with readxl library
library(readxl)
dataSet <- read_excel("data/R_data.xlsx")

head(dataSet)
View(dataSet)

# linear regression 
# (Flats_given_for_rental_or_sell ~ Avg_flat_price)
plot(Avg_flat_price~Flats_given_for_rental_or_sell, data=dataSet)

regFPriFGiv <- lm(Avg_flat_price~Flats_given_for_rental_or_sell, data=dataSet)
summary(regFPriFGiv)
abline(regFPriFGiv, col='red')

# linear regression
# (students_count ~ avg_flat_price)
plot(Avg_flat_price~Students_count, data=dataSet)

regFPriStud <- lm(Avg_flat_price~Students_count, data=dataSet)
summary(regFPriStud)
abline(regFPriStud, col='red')

# multiple linear regression
# (Avg_flat_price ~ Flats_given_for_rental_or_sell + Students_count)
regMulVar <- lm(
  Avg_flat_price~Flats_given_for_rental_or_sell+Students_count,
  data=dataSet)

summary(regMulVar)
