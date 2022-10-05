#packages
library(dplyr)
library(caret)
library(e1071)
library(glmnet)
library(rpart)
library(randomForest)
library(adabag)
library(arules)
library(arulesViz)
library(RColorBrewer)
library(cluster)
library(dbscan)
library(NbClust)
#import dataset
KickStarterData <- read.csv(file = file.choose(),header = T)

#general fundraising patterns of projects in different categories
category <- distinct(KickStarterData, category_parent, category) 
category <- category[order(category$category_parent), ]
num <- c()
for (i in 1:length(category$category)){
  num <- rbind(num, nrow(KickStarterData[KickStarterData$category == category$category[i],]))
}
data.frame(category$category_parent, category$category, num)

category_bankers <- aggregate(KickStarterData$backers,
                              by = list(KickStarterData$category),
                              FUN = length)
plot(x = 1:52, xlab = "category", y = category_bankers$x, type = "o")
text(x = 1:52, y = category_bankers$x+80, labels = category_bankers$Group.1, cex = 0.8)

goal_category_parent <- aggregate(KickStarterData$goal,
                                  by = list(KickStarterData$category_parent),
                                  FUN = mean)
pie(goal_category_parent$x, labels = goal_category_parent$Group.1)
pledged_category_parent <- aggregate(KickStarterData$pledged,
                                     by = list(KickStarterData$category_parent),
                                     FUN = mean)
pie(pledged_category_parent$x, labels = pledged_category_parent$Group.1)

#what factors can influence the fundraising performances of a project
successful_kickstarter <- KickStarterData[KickStarterData$Successful==TRUE,]
successful_category <- distinct(successful_kickstarter, category_parent, category)
successful_category <- successful_category[order(successful_category$category_parent), ]
perc <- c()
for (i in 1:length(successful_category$category)){
  n1 <- nrow(successful_kickstarter[successful_kickstarter$category 
                                    == successful_category$category[i],])
  n2 <- nrow(KickStarterData[KickStarterData$category == successful_category$category[i],])
  perc <- rbind(perc, n1/n2)
}
successful_category <- data.frame(successful_category$category_parent, 
                                  successful_category$category, perc)
barplot(successful_category$perc, horiz = T, las = 2,cex.names = 0.5,
        names.arg = successful_category$successful_category.category,
        col = c(1,rep(2,10),3,4,rep(5,4),6,rep(7,6),8,rep(9,4),rep(10,11),11,rep(12,8),rep(13,3),14))

#linear regression model
set.seed(123)
success.sample <- successful_kickstarter[sample(nrow(successful_kickstarter),1000),]
plot(x=success.sample$backers,success.sample$pledged, xlim = c(0,300), ylim = c(0,20000))
m0 <- lm(pledged ~ backers, data = success.sample)
abline(m0, col = "red")

m1 <- lm(pledged ~ backers+duration+goal+likes+owner_backing_count
         +owner_friends+reward_count, data = successful_kickstarter)
summary(m1)

#association analysis
backers.low <- c()
backers.median <- c()
backers.high <- c()
for (i in 1:nrow(KickStarterData)){
  if (KickStarterData$backers[i] <= 24){
    backers.low <- c(backers.low,1)
    backers.median <- c(backers.median,0)
    backers.high <- c(backers.high,0)
  }
  else if (KickStarterData$backers[i] <= 65){
    backers.low <- c(backers.low,0)
    backers.median <- c(backers.median,1)
    backers.high <- c(backers.high,0)
  }
  else{
    backers.low <- c(backers.low,0)
    backers.median <- c(backers.median,0)
    backers.high <- c(backers.high,1)
  }
}
#repeat with other variables to get the association matrix

trans <- as(association, "transactions")
summary(trans)
inspect(head(trans, 6))
LIST(head(trans, 6))
frequentItems <- eclat(trans, parameter = list(supp = 0.13, maxlen = 9, minlen = 5)) 
inspect(frequentItems)
rules <- apriori(trans, parameter = list(supp = 0.13, conf = 0.5, minlen = 5, maxlen = 9))
inspectDT(rules)  

#cluster analysis
US.data <- KickStarterData[KickStarterData$location.population != "#N/A" 
                           & KickStarterData$Country == "United States",]
US.data$location.population <- as.numeric(US.data$location.population)
US.data$Successful <- ifelse(US.data$Successful == TRUE, 1,0)
US.data <- US.data[,c(13,17,22)]
set.seed(100)
US.data <- US.data[sample(nrow(US.data),1000),]
plot(US.data$location.population, US.data$pledged, xlim = c(50000,1000000), ylim = c(1000,10000), col = US.data$Successful+1)
US.data <- US.data[,-3]
US.data <- scale(US.data)
kNNdistplot(US.data, k = 3)
abline(h=.25, col="red")
db<-dbscan(US.data, eps=0.25, MinPts = 3)
db
str(db)
plot(US.data, col = db$cluster+1, xlim = c(-0.75,-0.5), ylim = c(-0.45,0.5))

#predict the status of a crowdfunding project
KickStarterData$Successful <- as.factor(KickStarterData$Successful)
set.seed(333)
inTrain <- createDataPartition(y=KickStarterData$pledged, p=0.7, list=FALSE)
traindata <- KickStarterData[inTrain, ]
testdata <- KickStarterData[-inTrain, ]

#naive bayes prediction
KickStarter.sample <- KickStarterData[sample(nrow(KickStarterData),1000),]
pairs( ~ backers+duration+goal+likes+reward_count, data = KickStarter.sample)
NBfit <- naiveBayes(Successful ~ backers+duration+goal+likes+reward_count,
                    laplace = 0, data = traindata)
pred <- predict(NBfit, testdata)
table(pred, testdata$Successful)

#logistic lasso
x_train <- model.matrix(Successful ~ backers+duration+goal+likes+
                          owner_backing_count+owner_friends+reward_count, traindata)[,-1]
y_train <- ifelse(traindata$Successful==TRUE,1,0)
x_test <- model.matrix(Successful ~ backers+duration+goal+likes+
                         owner_backing_count+owner_friends+reward_count, testdata)[,-1]
y_test <- ifelse(testdata$Successful==TRUE,1,0)
cv.lasso <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 10)
fit_lasso <- glmnet(x_train, y_train, alpha = 1, lambda = cv.lasso$lambda.min)
coef(fit_lasso)
probabilities <- predict(fit_lasso, newx = x_test, s = cv.lasso$lambda.min)
pred_lasso <- ifelse(probabilities > 0.5, 1, 0)
acc <- confusionMatrix(as.factor(pred_lasso), as.factor(y_test))
acc

#svm
fit1 <- svm(Successful ~ backers+duration+goal+likes+reward_count,
            data = traindata, type='C-classification', kernel='linear')
fit.pred1 <- predict(fit1, testdata)
table(pred = fit.pred1, true = testdata$Successful) 
fit2 <- svm(Successful ~ backers+duration+goal+likes+reward_count,
            data = traindata, type='C-classification', kernel='radial')
fit.pred2 <- predict(fit2, testdata)
table(pred = fit.pred2, true = testdata$Successful)

#decision tree
fit <- rpart(Successful ~ backers+duration+goal+likes+reward_count, method = "class", data = traindata)
printcp(fit)
summary(fit)
result <- predict(fit, testdata, type="class")
table(pred = result, true = testdata$Successful)

#random forest
fit <- randomForest(Successful ~ backers+duration+goal+likes+reward_count, data = traindata, mtry = 4, ntree = 100)
print(fit)
importance(fit)
varImpPlot(fit)
RandomTreeresult <- predict(fit, testdata, type = "class")
table(pred = RandomTreeresult, true = testdata$Successful)

#adaboost
adaboost <- boosting(Successful ~ backers+duration+goal+likes+reward_count, 
                     data = traindata, boos = FALSE, mfinal = 20, coeflearn = 'Breiman')
summary(adaboost)
adaboost$trees
adaboost$weights
adaboost$importance
importanceplot(adaboost)
pred <- predict(adaboost, testdata)
table(pred = pred$class, true = testdata$Successful)

#predict the fundraising amount of a crowdfunding project 
#linear
summary(KickStarterData$pledged)
Kick.sample <- KickStarterData[KickStarterData$pledged >= 901 & KickStarterData$pledged <= 6030,]
set.seed(333)
inTrain <- createDataPartition(y=Kick.sample$pledged, p=0.7, list=FALSE)
traindata <- Kick.sample[inTrain, ]
testdata <- Kick.sample[-inTrain, ]
m0 <- lm(pledged ~ backers+duration+goal+likes+reward_count, data = traindata)
summary(m0)
mean((m0$fitted.values - traindata$pledged)^2)
mean((predict(m0, newdata = testdata) - testdata$pledged)^2)

#lasso
cv_lasso <- cv.glmnet(traindata.x, traindata.y, alpha = 1, nfolds = 5)
bestlam_lasso <- cv_lasso$lambda.1se
fit_lasso <- glmnet(traindata.x, traindata.y, alpha = 1)
coeff_lasso <- predict(fit_lasso, s = bestlam_lasso, type="coefficients")
y_lasso <- predict(fit_lasso, s = bestlam_lasso, newx = testdata.x)
err_lasso <- mean((y_lasso - testdata.y)^2)
#repeat using ridge regression
