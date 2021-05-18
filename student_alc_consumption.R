#Understanding the factors that contribute to underage drinking

setwd("~/Documents/STAT 5607 Files/Mini-project")

dat <- read.csv("student-por.csv", header =T)

#--------------------------------------------------------------------------------------------------
# Descriptive information
#--------------------------------------------------------------------------------------------------

hist(dat$health, main="Distribution of Health",breaks=5,col="orange", xlab="Healthiness Level")
hist(dat$famrel, main="Distribution of Quality of Family Relationships",breaks=5,col="purple", xlab="Quality")
hist(dat$goout, main="Distribution of Going Out with Friends ",breaks=5,col="cyan", xlab="Going Out")
hist(dat$studytime, main="Distribution of Study Time ",breaks=4,col="blue", xlab="Study Time")

dat$DalcLog <- log(dat$Dalc)
hist(dat$DalcLog, breaks=5, col="green",main="Histogram of Log of Work Day Drinking", xlab="Workday Drinking (low to high)")
dat$WalcLog <- log(dat$Walc)
hist(dat$WalcLog, breaks=5, col="red",main="Histogram of Log of Weekend Drinking", xlab="Weekend Drinking (low to high)")
dat$studytimeLog <- log(dat$studytime)
hist(dat$WalcLog, breaks=5, col="purple",main="Histogram of Log of Study Time", xlab="Study Time")


summary(dat[,c(1,4,8,9,14,15,16,25,26,27,28,29,30,31,32,33)])
(dat[,c(1,4,8,9,14,15,16,25,26,27,28,29,30,31,32,33)])

tabSchool <- table(dat$school)
barplot(tabSchool, main="Distribution of Schools", xlab="School")

tabSex<- table(dat$sex)
barplot(tabSex, main="Distribution of Gender", xlab="Gender")

tabSchool <- table(dat3$school)
barplot(tabSchool, main="Distribution of Schools", xlab="School")

tabSchool <- table(dat3$school)
barplot(tabSchool, main="Distribution of Schools", xlab="School")


dat$school <- as.factor(dat$school)
dat$sex <- as.factor(dat$sex)
dat$address <- as.factor(dat$address)
dat$famsize <- as.factor(dat$famsize)
dat$Pstatus <- as.factor(dat$Pstatus)
dat$Mjob <- as.factor(dat$Mjob)
dat$Fjob <- as.factor(dat$Fjob)
dat$reason <- as.factor(dat$reason)
dat$guardian <- as.factor(dat$guardian)
dat$schoolsup <- as.factor(dat$schoolsup)
dat$famsup <- as.factor(dat$famsup)
dat$paid <- as.factor(dat$paid)
dat$activities <- as.factor(dat$activities)
dat$nursery <- as.factor(dat$nursery)
dat$higher <- as.factor(dat$higher)
dat$internet <- as.factor(dat$internet)
dat$romantic <- as.factor(dat$romantic)

dat$DalcLog <- log(dat$Dalc)
hist(dat$DalcLog, breaks=5, col="green",main="Histogram of Log of Work Day Drinking", xlab="Workday Drinking (low to high)")

#--------------------------------------------------------------------------------------------------
# Multiple Regression (Step 1)
#--------------------------------------------------------------------------------------------------
dat$guardian <- relevel(dat$guardian, "other")
levels(dat$guardian)

null <- lm(Walc ~ 1, data = dat)
full <- lm(Walc ~ ., data = dat)

forward <- step(null, scope=list(lower=null, upper=full), direction="forward")
summary(forward)
forward$anova

backward <- step(full, direction="backward")
summary(backward)
backward$anova

both <- step(null, scope = list(upper=full), direction="both")
summary(both)
both$anova

dat$guardian <- relevel(dat$guardian, "other")
levels(dat$guardian)


mod1 <- lm(WalcLog ~ DalcLog + goout + sex + studytimeLog+ famrel +health + guardian , data=dat )
summary(mod1)

AIC(mod1)
AIC(both)

#--------------------------------------------------------------------------------------------------
# Step 2: Outliers & Influential Cases
#--------------------------------------------------------------------------------------------------

res <- resid(mod1)
standRes <- rstandard(mod1)
cooks <- cooks.distance(mod1)
dffit <- dffits(mod1)

#Count outliers
sum(abs(standRes) > 3)

#Look at outlier cases
outliers <- dat[which(abs(standRes) > 3),]

#Influential Observations
sum(cooks > 1)
hist(dffit)
boxplot(dffit)

#--------------------------------------------------------------------------------------------------
# Model Assumptions
#--------------------------------------------------------------------------------------------------

install.packages("usdm")
library(usdm)

#Multicollinearity
quantVar <- dat[,c(27,25,29,34,36)]
cor(quantVar)
round(cor(quantVar),2)

predictors <- dat[,c(27,25,29,34,36)]
vif(predictors)

#Homoscedasticity & Linearity--Residual vs fitted plot
plot(mod1)	#Easiest way - look at first plot
plot(res ~ predict(mod1)) 

#Normally distributed errors
plot(mod1)	#Easiest way - look at second plot
qqnorm(res)
qqline(res)






#--------------------------------------------------------------------------------------------------
# ANCOVA
#--------------------------------------------------------------------------------------------------


tapply(dat$WalcLog, dat$school, mean) #looks at the mean for each school

tapply(dat$WalcLog, dat$school, sd)  #looks at the sd for each school

tapply(dat$WalcLog, dat$age, mean)

tapply(dat$WalcLog, dat$age, sd)

#Levene's test
library(car)
# Tests if the variances for each school is the same
# We want a high p-value for this test

leveneTest(dat$WalcLog, dat$school, center = median)

# We need to check that the covariate & independent variable (treatment) are independent

#Age and School
checkIndependenceModel <-aov(age ~ school, data = dat)

# High p-val: mean that there is  no difference in covariate across groups
# So we can assume the variables are independent. THUS it is appropriate to use this as a covariate in the analysis

summary(checkIndependenceModel)

#Free time and School
checkIndependenceModel <-aov(freetime ~ school, data = dat)

summary(checkIndependenceModel)

#Type II
mod2 <- Anova(mod1, type = "II")
mod2

#Look at regular means

tapply(dat$WalcLog, dat$school, mean)	

#Adjusted means

install.packages('effects')
library(effects)

adjustedMeans <- effect("school", mod1, se=TRUE)

#When adjusted for the covariate, we can see that the Number of drinks on the weekend is higher for MS than GP

summary(adjustedMeans)
