source("Rfunctions.R")
Default <- read.csv("Default.csv", stringsAsFactors=TRUE)
str(Default)
levels(Default$default)

#encoding No as 0 and Yes as 1 
Default$default <- 1*(Default$default=="Yes")

#estimating linear regression 
linear.model <- lm(Default$default ~ Default$balance)

#visualise data 
plot(Default$balance, Default$default)
#fitted linear regression
abline(linear.model, col="red")

summary(linear.model)

attach(Default)
#fit logistic regression model 
log.model <- glm(default~balance, data=Default, family = binomial)
summary(log.model)

#first plot the observations (as points)
plot(balance, default)
#to plot the estimated probability first define a vector x that takes values in the range of "balance"
x <- seq(from=min(balance), to=max(balance))
#obtain estimated coefficients using function
hat.beta <- coef(log.model)
hat.beta
#estimated probabilities from log. reg. formula
lines(x, (1 + exp(-hat.beta[1]- hat.beta[2]*x))^(-1), col="blue")

#compute the estimated probability of default for al the data in Default 
probs <- predict(log.model, newdata= Default, type="response")

#predict class 1 (default=Yes) if estimated probability > 0.5
class.pred <- 1*(probs>0.5)
#verify predictions 
head(cbind(probs, class.pred), 10)

#create truth table: rows represent actual class, column represents predicted 
truth.table <- table(default, class.pred)
truth.table

#total number of observations in truth.table
N <- sum(truth.table)
N
#missclassification error 
(truth.table[1,2]+truth.table[2,1])/N
#accuracy = proportion of correct predictions 
acc_1 <- (truth.table[1,1]+ truth.table[2,2])/N
#Accuracy = 1 - Misclassification error
#therefore... estimated error of 
1 - acc_1

#INTERPRETATION OF THRESHOLD
#first plot the observations (as points) 
plot(balance, default)
#plot estimated probabilities
lines(x, (1+exp(-hat.beta[1]-hat.beta[2]*x))^(-1), col="blue")
#plot classification threshold
#(staright horizontal line)
abline(h=0.5, col="green")
#plot partition of input space 
#(straight vertical line)
abline(v=-hat.beta[1]/hat.beta[2], col="red")

#estimate logistic regression with 2 predictors 
log.model2 <- glm(default~balance + income, data=Default, family=binomial)
#model summary 
summary(log.model2)

#Bˆ1 is: how much the log-odds of default change when balance (income) changes by one unit
#ASSUMING - all other predictors are constant 
#estimate probabilities 
probs2 <- predict(log.model2, data=Default, type="response")
#threshold of 0.5
#scatterplot of income against balance using default to colour data
plot(balance, income, col=as.factor(default))
#get estimated coefficients 
hb <- coef(log.model2)
hb
#define straight line using intercept and slope (threshold=0.5)
abline(a=-hb[1]/hb[3], b=-hb[2]/hb[3], col="blue")

#BLUE LINE = decision boundary - all combinations of balance and income is exactly = to 0.5. 
#right>0.5 left<0.5 
#logistic regression imposes a linear decision boundary in the input space
#changing threshold: 

#decision boundry for threshold=0.1
abline(a=(-hb[1]-log(9))/hb[3], b=-hb[2]/hb[3], col="green")

#only intercept changes (not slope)
#changes expected values (values inside the lines are now seen differently)


