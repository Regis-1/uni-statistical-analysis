library(readxl)
dataSet <- read_excel("data/R_logistic.xlsx")

head(dataSet)

xtabs(~ dataSet$duzy_doch_woj + dataSet$muzea, data=dataSet)
xtabs(~ dataSet$duzy_doch_woj + dataSet$biblioteki_pub, data=dataSet)

model <- glm(duzy_doch_woj~muzea+biblioteki_pub, data=dataSet, family="binomial")
summary(model)

modelmuzea <- glm(duzy_doch_woj~muzea, data=dataSet, family="binomial")
newdata <- data.frame(muzea=seq(min(dataSet$muzea), max(dataSet$muzea),len=500))
newdata$duzy_doch_woj = predict(modelmuzea, newdata, type="response")
plot(duzy_doch_woj ~ muzea, data=dataSet, col="red")
lines(duzy_doch_woj~muzea, newdata, lwd=2)

modelbiblioteki_pub <- glm(duzy_doch_woj~biblioteki_pub, data=dataSet, family="binomial")
newdata2 <- data.frame(biblioteki_pub=seq(min(dataSet$biblioteki_pub), max(dataSet$biblioteki_pub),len=500))
newdata2$duzy_doch_woj = predict(modelbiblioteki_pub, newdata2, type="response")
plot(duzy_doch_woj ~ biblioteki_pub, data=dataSet, col="red")
lines(duzy_doch_woj~biblioteki_pub, newdata2, lwd=2)
