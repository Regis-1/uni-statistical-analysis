install.packages('readxl')
library(readxl)
setwd('uni-statistical-analysis')

ds <- read_excel("data/R_logistyczna.xlsx")

head(ds)
summary(ds)

cor(ds$absolwenci, ds$biblioteki)
cor(ds$absolwenci, ds$licea)
cor(ds$licea, ds$biblioteki)

mRegLogistycznej <- glm(pow25pat~absolwenci+licea, data=ds, family="binomial")
summary(mRegLogistycznej)

modelpraca <- glm(pow25pat~absolwenci, data=ds, family="binomial")
newdata <- data.frame(absolwenci=seq(min(ds$absolwenci), max(ds$absolwenci),len=500))
newdata$pow25pat = predict(modelpraca, newdata, type="response")
plot(pow25pat~absolwenci, data=ds, col="red")
lines(pow25pat~absolwenci, newdata, lwd=2)

modeldzietnosc <- glm(pow25pat~licea, data=ds, family="binomial")
newdata <- data.frame(licea=seq(min(ds$licea), max(ds$licea),len=500))
newdata$pow25pat = predict(modeldzietnosc, newdata, type="response")
plot(pow25pat~licea, data=ds, col="red")
lines(pow25pat~licea, newdata, lwd=2)
