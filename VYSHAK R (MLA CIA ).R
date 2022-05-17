read.csv("F:/Sem 4/MLA/CeoCompensation.csv")
myData = read.csv("F:/Sem 4/MLA/CeoCompensation.csv",stringsAsFactors=F)
print(head(myData)) 
mean = mean(myData$AGE )
print(mean)
mean = mean(myData$BACKGRD )
mean = mean(myData$COMP )
median = median(myData$Age)
print(median)
median = median(myData$BACKGRD)
median = median(myData$COMP)
mode = function(){return(sort(-table(myData$Age))[1])
}
mode() 
mode = function(){return(sort(-table(myData$BACKGRD))[1])
}
mode = function(){return(sort(-table(myData$COMP))[1])
}
sapply(  myData  [c(1,5,6)], mean)
sapply(  myData  [c(1,5,6)], median)
print(median(myData$COMP))
print(median(myData$EXPER))
print(median(myData$TENURE))
summary(myData [,c(1,5,6)])

Measure of Dispersion
sapply(myData [,c(1,5,6)], sd)

var
data(myData)
vari= myData$COMP 
var(vari)
vari= myData$BACKGRD 
Compensation",ylab="Tenure in company",x=myData$COMP y=myData$TENURE)
cor(myData$COMP,myData$BACKGRD)
plot(xlab="Compensation", ylab="Background" ,x=myData$COMP, y=myData$BACKGRD)
cor(myData$COMP,myData$TENURE)
plot(xlab="Compensations", y lab="Tenure in company", x =myData$COMP, y=myData$TENURE) 
abline(lm(myData$vari= myData$TENURE
sapply(myData[,c(1,5,6)], IQR)
skew_my= apply(myData[,c(1,5,6)], 2,apply)
install.packages("ggpubr")
install.packages("corrplot")

cor(myData$COMP,myData$TENURE)
plot(xlab="COMP~myData$TENURE, col="red")
cor(myData$COMP,myData$BACKGRD)
plot(xlab="Compensations",ylab="Background",x=myData$COMP, y=myData$BACKGRD) 
abline(lm(myData$COMP~myData$BACKGRD), col="red")

  REG
library(tidyverse)
library(ggpubr)
data("mydata", package = "datarium")
head(myData,5,5,6)
model=lm(COMP ~ TENURE, data = myData)
model
ggplot(myData, aes(COMP,TENURE)) +geom_point() +stat_smooth(method = lm)
summary(model)

input <- myData[,c("COMP","TENURE","EXPER")]
print(head(input))
model <- lm(COMP~TENURE+EXPER, data = input)
print(model)
cat("# # # # The Coefficient Values # # # ","\n")
a <- coef(model)
print(a)
XTENURE <- coef(model)
XEXPER <- coef(model)
print(XTENURE)
print(XEXPER)
Y = a+XTENURE+XEXPER

library(caret)
library(ISLR)
data(Default, package = "ISLR")

set.seed(430)
default_idx = createDataPartition(Default$default, p = 0.75, list = FALSE)
default_trn = Default[default_idx, ]
default_tst = Default[-default_idx, ]
default_glm_mod = train(form = default ~ .,data = default_trn,trControl = trainControl(method = "cv", number = 5),method = "glm",family = "binomial")
trainControl(method = "cv", number = 5)[1:3]
default_glm_mod
default_glm_mod$finalModel

library(caret)
library(klaR)

   
 BOOTSTRAP
data(mydata)
train_control <- trainControl(method="boot", number=100)
model <- train(Species~., data=myData, trControl=train_control, method="nb")
print(model)

data(mydata)
train_control <- trainControl(method="cv", number=10)
grid <- expand.grid(.fL=c(0), .usekernel=c(FALSE))
model <- train(Species~., data=myData, trControl=train_control, method="nb", tuneGrid=grid)
print(model)

LINEAR MODEL

linearMod <- lm(COMP ~ TENURE, data=myData)
print(linearMod)

lm(formula = COMP ~ TENURE + AGE * EXPER  , data = myData)

scatter.smooth(x=myData$COMP, y=myData$BACKGRD, main="COMP ~ BACKGRD")  

scatter.smooth(x=myData$COMP, y=myData$EXPER, main="COMP ~ EXPER") 
linearMod <- lm(COMP ~ TENURE, data=myData)
print(linearMod)
summary(linearMod)

AUTO CORRELLATION :

   (WITHOUT CORRELATION)
library(stats)
model <- lm(COMP~TENURE, data = myData)
acf(model$residuals, type = "correlation")

(WITH AUTO CORRELATION)
library(lmtest)
model <- lm(COMP~TENURE, data = myData)
lmtest::dwtest(model)

(Breusch-Godfrey Test to Check Autocorrelation)
library(lmtest)
model <- lm(COMP~TENURE, data = myData)
lmtest::bgtest(model, order = 3)

(LINEAR HYPOTHESIS)
library(car)
data(mydata)
m1 <- lm(COMP ~ TENURE + EXPER , data = myData)
m1
linearHypothesis(m1, c('TENURE + EXPER = 0'))
linearHypothesis(m1, c('TENURE = 0','EXPER = 0'))
linearHypothesis(m1, c('TENURE + EXPER = 0'))



