#packages
library(dplyr)
library(e1071)
library(caret)
library(glmnet)
library(rpart)
library(randomForest)
library(rpart.plot)
library(adabag)
#import datasets
airlines<-read.csv(file = file.choose(),header = T) 
airports<-read.csv(file = file.choose(),header = T) 
flights<-read.csv(file = file.choose(),header = T)
planes<-read.csv(file = file.choose(),header = T)
weather<-read.csv(file = file.choose(),header = T)

#join tables
flights.airlines <- inner_join(flights, airlines, by = "carrier")
flights.weather <- inner_join(flights, weather, by = c("origin", "time_hour"))  
flights.planes <- inner_join(flights, planes, by = "tailnum")

#exploratory data analysis
summary(flights[,c(6,9)])

flights.planes <- flights.planes[!is.na(flights.planes$dep_delay) 
                                 & !is.na(flights.planes$arr_delay), ]
aggregate(flights.planes$dep_delay, 
          by = list(flights.planes$year.y), 
          FUN = mean, is.na = TRUE)

flights.sample <- flights[sample(nrow(flights), 1000), ]

plot(distance ~ air_time, flights.sample)

boxplot(dep_delay ~ month, data = flights.sample)

#multiple linear regression
flights.planes <- flights.planes[!is.na(flights.planes$dep_delay) 
                                 & !is.na(flights.planes$arr_delay), ]
flights.weather <- flights.weather[!is.na(flights.weather$dep_delay) 
                                   & !is.na(flights.weather$arr_delay), ]

m0 <- lm(dep_delay ~ temp + dewp + humid + wind_dir + 
           wind_speed + wind_gust + precip + pressure + visib, 
         data = flights.weather)
summary(m0)
m1 <- lm(dep_delay ~ temp + dewp + humid + wind_dir + 
           wind_speed + wind_gust + pressure + visib, 
         data = flights.weather)
summary(m1)

m2 <- lm(arr_delay ~ dep_delay + air_time + distance + temp + dewp + humid + wind_dir + 
           wind_speed + wind_gust + precip + pressure + visib, 
         data = flights.weather)
summary(m2)
m3 <- lm(arr_delay ~ dep_delay + air_time + distance + temp + dewp + humid + 
           wind_speed + wind_gust + precip + pressure + visib, 
         data = flights.weather)
summary(m3)

#logistic regression
flights.weather <- na.omit(flights.weather[, c(9,6,15,16,24,25,26,27,28,29,30,31,32)])

early.arrival <- c()
for (i in 1:length(flights.weather$arr_delay)){
  if (flights.weather$arr_delay[i] < 0) early.arrival[i] <- 1
  else early.arrival[i] <- 0
}
flights.weather <- cbind(flights.weather, early.arrival) 
flights.weather$early.arrival <- as.factor(flights.weather$early.arrival)
g_full <- glm(early.arrival ~ .-arr_delay-wind_dir-wind_gust-precip,  
              data = flights.weather, family = "binomial")summary(g_full)
summary(g_full)

#multiple linear prediction
set.seed(333)
inTrain<-createDataPartition(y=flights.weather$arr_delay, p=0.7, list=FALSE)
traindata <- flights.weather[inTrain, ]
testdata <- flights.weather[-inTrain, ]
m0 <- lm(arr_delay ~ .-early.arrival, data = traindata)
summary(m0)
mean((m0$fitted.values - traindata$arr_delay)^2)
mean((predict(m0, newdata = testdata) - testdata$arr_delay)^2)

#naive bayes prediction
arrival <- c()
for (i in 1:length(flights.weather$arr_delay)){
  if (flights.weather$arr_delay[i] < 0) arrival[i] <- "early"
  if (flights.weather$arr_delay[i] > 0) arrival[i] <- "late"
  if (flights.weather$arr_delay[i] == 0) arrival[i] <- "on time"
}
flights.weather <- cbind(flights.weather, arrival)
flights.weather$arrival <- as.factor(flights.weather$arrival)
set.seed(333)
inTrain<-createDataPartition(y=flights.weather$arr_delay, p=0.7, list=FALSE)
traindata <- flights.weather[inTrain, ]
testdata <- flights.weather[-inTrain, ]
NBfit <- naiveBayes(arrival ~.-arr_delay-early.arrival, 
                    laplace = 0, data = traindata)
pred <- predict(NBfit, testdata[, 2:13])
table(pred, testdata$arrival)

#logistic regression prediction
g_full <- glm(early.arrival ~ .-arr_delay-arrival,   
              data = traindata, family = "binomial")
summary(g_full)
pred <- ifelse(predict(g_full, testdata)>0.5,1,0)
acc <- confusionMatrix(as.factor(pred), as.factor(testdata$early.arrival))
acc

#cross validation
x <- model.matrix(early.arrival ~ .-arr_delay-arrival, flights.weather)[,-1]
y <- ifelse(flights.weather$early.arrival == 1, 1, 0)
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
model <- glmnet(x, y, alpha = 0, family = "binomial",lambda = cv.lasso$lambda.min)
coef(model)
probabilities <- predict(model,newx = x)
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
acc <- confusionMatrix(as.factor(predicted.classes), as.factor(flights.weather$early.arrival))
acc

#svm
fit1 <- svm(arrival ~ .-arr_delay-early.arrival, 
            data =traindata, type='C-classification', kernel='linear')  
fit.pred1 <- predict(fit1, testdata)
table(pred = fit.pred1, true = testdata [,15])
fit2 <- svm(arrival ~ .-arr_delay-early.arrival, 
            data = traindata, type='C-classification', kernel='radial')  
fit.pred2 <- predict(fit2, testdata)
table(pred = fit.pred2, true = testdata [,15])

#decision tree
fit <- rpart(early.arrival ~ .-arr_delay-arrival, method = "class", data = traindata)
printcp(fit)
summary(fit)
result <- predict(fit,testdata,type="class")
table(pred = result, true = testdata$early.arrival)

#random forest
fit <- randomForest(early.arrival ~ .-arr_delay-arrival, data = traindata, mtry = 4, ntree = 100)
print(fit)
importance(fit)
varImpPlot(fit)
RandomTreeresult <- ifelse(predict(fit, testdata, type = "class")>0.5, 1, 0)
table(pred = as.factor(RandomTreeresult), true = as.factor(testdata$early.arrival))

#adaboost
adaboost <- boosting(early.arrival ~ . -arr_delay-arrival, data = traindata, boos=FALSE, mfinal = 20, coeflearn = 'Breiman')
summary(adaboost)
adaboost$trees
adaboost$weights
adaboost$importance
importanceplot(adaboost)
pred<-predict(adaboost, testdata)
table(pred = pred$class, true = testdata$early.arrival)
