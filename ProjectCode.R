#loading packages
library(leaps)
library(car)
library(ggplot2)
library(MASS)
library(lmtest)
library(nortest)
library(car)
library(Hmisc)

# read in data and getting sample
projectdata <- read.csv("ProjectData.csv", sep=',', header = T)
set.seed(38273) 
mydata <- projectdata[sample(1:nrow(projectdata), 150),]


#Selecting inital model
bestsub1=regsubsets(Score~Putt+ARG+APP+OTT+T2G,
                    data=mydata, nbest=10)
plot(bestsub1, scale="adjr2")
plot(bestsub1, scale="bic")
plot(bestsub1, scale="Cp")

#Automatic selection - Forward
null=lm(Score~1, data=mydata) #intercept only model
full=lm(Score~Putt+ARG+APP+OTT+T2G, data=mydata)#full model- Regresses y on all variables in dataset
step(null, scope=list(lower=null, upper=full),
     direction="forward")
#Automatic selection - backward
step(full, data=mydata, direction="backward")
#Automatic selection - Stepwise
step(null, scope = list(upper=full), data=mydata, direction="both")

linmodel = lm(Score~Putt+T2G, data = mydata)


#looking for outliers, multicollinearity
qqPlot(linmodel, main="QQ Plot")

#create new data set with the QQplot outliers removed
mydata2 = mydata[-c(71,140), ]
#Run regression on new data set and compare the two models
linmodel2 = lm(Score~Putt+T2G, data = mydata2)
summary(linmodel)
anova(linmodel)
summary(linmodel2)
anova(linmodel2)
#plot regression lines from the full and reduced data sets
ggplot(mydata, aes(Putt, T2G, Score)) +
  geom_point() +
  geom_smooth(method="lm", se=F, aes(color="With")) +
  geom_smooth(data = mydata[-c(71,140), ], method="lm", se=F, aes(color="Without QQ"))+
  scale_colour_manual(name='',values=c("red","green"))

vif(lm(Score ~Putt+T2G, data=mydata))


#Checking assumptions for model 1
summary(linmodel)
anova(linmodel)
#Normality check with QQ plot and Anderson-Darling Test
qqPlot(linmodel$residuals, main="Residuals QQ Plot")
ad.test(linmodel$residuals)
#Constant variance check with Res vs Fits plot and Breusch Pagan test
plot(linmodel$fitted,linmodel$residuals, ylab="Residuals", xlab="Fitted Values")
abline(0,0)
bptest(linmodel)#Breusch-Pagan for constant variance
#Linearity check
rcorr(as.matrix(mydata))
#Independence check
acf(linmodel$residuals)
dwtest(linmodel)


#Checking assumptions for updated model 
linmodel3 = lm(Score ~ Putt+ARG+OTT, data = mydata)
summary(linmodel3)
anova(linmodel3)
#Normality check with QQ plot and Anderson-Darling Test
qqPlot(linmodel3$residuals, main="Updated Residuals QQ Plot")
ad.test(linmodel3$residuals)
#Constant variance check with Res vs Fits plot and Breusch Pagan test
plot(linmodel3$fitted,linmodel3$residuals, ylab="Residuals", xlab="Fitted Values")
abline(0,0)
bptest(linmodel3)#Breusch-Pagan for constant variance
#Linearity check
rcorr(as.matrix(mydata))
#Independence check
acf(linmodel3$residuals)
dwtest(linmodel3)

#Confidence intervals for updated slopes
confint(linmodel3)


#Estimation and Prediction for updated linear model
predict(linmodel3, interval="confidence", se.fit=T, newdata=data.frame(Putt = 0, ARG = 0, OTT = 4))
predict(linmodel3, interval="prediction", se.fit=T, newdata=data.frame(Putt = 0, ARG = 0, OTT = 4))


#Logistic Model Selection and prediction
probit <- glm(Top25 ~ Putt + ARG + OTT, family = binomial(link = "probit"), data = mydata)
summary(probit)
anova(probit, test="Chisq")

logit <- glm(Top25 ~ Putt + ARG + OTT, family = binomial, data = mydata)
summary(logit)
anova(logit, test="Chisq")

confint(probit)

predict(probit, newdata=data.frame(Putt=0, ARG = 0, OTT = 4), type="response",
        se.fit=TRUE)
