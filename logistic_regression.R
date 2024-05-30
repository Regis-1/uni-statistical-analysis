library(readxl)
ds <- read_excel("data/R_logistyczna.xlsx")

head(ds)

mRegLogistycznej <- glm(wysBz~matury+wydatki, data=ds, family="binomial")
summary(mRegLogistycznej)


modelpraca <- glm(wysBz~praca, data=ds, family="binomial")
newdata <- data.frame(praca=seq(min(ds$praca), max(ds$praca),len=500))
newdata$wysBz = predict(modelpraca, newdata, type="response")
plot(wysBz ~ praca, data=ds, col="steelblue")
lines(wysBz~praca, newdata, lwd=2)

modeldzietnosc <- glm(wysBz~matury, data=ds, family="binomial")
newdata <- data.frame(matury=seq(min(ds$matury), max(ds$matury),len=500))
newdata$wysBz = predict(modeldzietnosc, newdata, type="response")
plot(wysBz ~ matury, data=ds, col="steelblue")
lines(wysBz~matury, newdata, lwd=2)
