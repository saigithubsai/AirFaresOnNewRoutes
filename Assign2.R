
# Set working directory to working file location

setwd("C:/Users/avala/OneDrive/Desktop/SPRING19/OR568/Assignments/Assignment2") 

# Read the csv file
dataset <- read.csv("AirfaresData.csv")

summary(dataset)

dataa <- dataset[, c(5,6,9:13,16:18)]
datab <- dataset[,c(7,8,14,15,18)]
df <- dataset[,c(5:18)]

summary(dataa)
summary(datab)
summary(datad)


#EXPLORING NUMERICAL PREDICTORS

#Correlation table

cor(dataa$FARE,dataa)

?aggregate

#visualizing predictors and FARE through scatter ploy

ggplot(dataa,aes(x = FARE,y = DISTANCE)) +
  geom_point()

ggplot(dataa,aes(x = FARE,y = COUPON)) +
  geom_point()

ggplot(dataa,aes(x = FARE,y = HI)) +
  geom_point()

pairs(dataa)


#DISTANCE seems to be best single predictor for FARE

#EXPLORING CATEGORICAL PREDICTORS
#mean value of FARE for each predictor

summaryVacation <-aggregate(FARE~VACATION, data = datab, FUN = mean) #47.5716
summarySW <-aggregate(FARE~SW, data = datab, FUN = mean) #89.80052
summarySLOT <-aggregate(FARE~SLOT, data = datab, FUN = mean) #35.2337
summaryGATE <-aggregate(FARE~GATE, data = datab, FUN = mean) #40.033

summaryVacation
summarySW
summarySLOT
summaryGATE

#SW seems best categorical predictor for predicting FARE as it has the largest difference in mean FARE values


#Splitting the data into training and Validation set
set.seed(12345)
library(caTools)
split = sample.split(df$FARE, SplitRatio = 0.6)
training_set = subset(df, split == TRUE)
validation_set = subset(df, split == FALSE)


#Regression model with first two predictors
regressor = lm(formula = FARE ~ DISTANCE + SW,
               data = training_set)
regressor


summary(regressor)

AIC(regressor)
BIC(regressor)
fitsummary = summary(regressor)
fitsummary$r.squared
fitsummary$adj.r.squared

residual<-residuals(regressor)
hist(residual,breaks=20)

layout(matrix(c(1,2,3,4),2,2))           # optional 4 graphs/page 
plot(regressor)

# Computing the MSE on validation dataset based on model fit with training data
# We use the "predict" function to compute the predicted value on validation set

PredBase<-predict(regressor, validation_set, se.fit=TRUE)  
PredBase
mean((validation_set[,"FARE"] - PredBase$fit)^2)



#Regression model with backward selection

backregressor = lm(formula = FARE ~ .,
                   data = training_set)

backward<-step(backregressor, direction='backward')
coefficients(backward)
summary(backward)


AIC(backward)
BIC(backward)
fitsummaryback = summary(backward)
fitsummaryback$r.squared
fitsummaryback$adj.r.squared


residualback<-residuals(backward)
hist(residualback,breaks=20)


layout(matrix(c(1,2,3,4),2,2))           
plot(backward)

# Computing the MSE on validation dataset based on model fit with training data
# We use the "predict" function to compute the predicted value on validation set

PredBaseback<-predict(backward, validation_set, se.fit=TRUE)  
PredBaseback
mean((validation_set[,"FARE"] - PredBaseback$fit)^2)
