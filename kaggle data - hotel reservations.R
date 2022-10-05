#Data reference: https://www.kaggle.com/jessemostipak/hotel-booking-demand

# The detailed information of variables:
# hotel: City Hotel or Resort Hotel
# is_canceled: Value indicating if the booking was canceled (1) or not (0)
# lead_time: Number of days that elapsed between the entering date of the booking into the PMS and the arrival date
# arrival_date_year: Year of arrival date
# arrival_date_month: Month of arrival date
# arrival_date_week_number: Week number of year for arrival date
# arrival_date_day_of_month: Day of arrival date
# stays_in_weekend_nights: Number of weekend nights (Saturday or Sunday) the guest stayed or booked to stay at the hotel
# stays_in_week_nights: Number of week nights (Monday to Friday) the guest stayed or booked to stay at the hotel
# adults: Number of adults
# children: Number of children
# babies: Number of babies
# meal: Type of meal booked. Categories are presented in standard hospitality meal packages: Undefined/SC – no meal package; BB – Bed & Breakfast; HB – Half board (breakfast and one other meal – usually dinner); FB – Full board (breakfast, lunch and dinner)
# country: Country of origin. Categories are represented in the ISO 3155–3:2013 format
# market_segment: Market segment designation. In categories, the term “TA” means “Travel Agents” and “TO” means “Tour Operators”
# distribution_channel: Booking distribution channel. The term “TA” means “Travel Agents” and “TO” means “Tour Operators”
# is_repeated_guest: Value indicating if the booking name was from a repeated guest (1) or not (0)
# previous_cancellations: Number of previous bookings that were cancelled by the customer prior to the current booking
# previous_bookings_not_canceled: Number of previous bookings not cancelled by the customer prior to the current booking
# reserved_room_type: Code of room type reserved. Code is presented instead of designation for anonymity reasons.
# assigned_room_type: Code for the type of room assigned to the booking. Sometimes the assigned room type differs from the reserved room type due to hotel operation reasons (e.g. overbooking) or by customer request. Code is presented instead of designation for anonymity reasons.
# booking_changes: Number of changes/amendments made to the booking from the moment the booking was entered on the PMS until the moment of check-in or cancellation
# deposit_type: Indication on if the customer made a deposit to guarantee the booking. This variable can assume three categories: No Deposit – no deposit was made; Non Refund – a deposit was made in the value of the total stay cost; Refundable – a deposit was made with a value under the total cost of stay.
# agent: ID of the travel agency that made the booking
# company: ID of the company/entity that made the booking or responsible for paying the booking. ID is presented instead of designation for anonymity reasons
# days_in_waiting_list: Number of days the booking was in the waiting list before it was confirmed to the customer
# customer_type: Type of booking, assuming one of four categories: Contract - when the booking has an allotment or other type of contract associated to it; Group – when the booking is associated to a group; Transient – when the booking is not part of a group or contract, and is not associated to other transient booking; Transient-party – when the booking is transient, but is associated to at least other transient booking
# adr: Average Daily Rate as defined by dividing the sum of all lodging transactions by the total number of staying nights
# required_car_parking_spaces: Number of car parking spaces required by the customer
# total_of_special_requests: Number of special requests made by the customer (e.g. twin bed or high floor)
# reservation_status: Reservation last status, assuming one of three categories: Canceled – booking was canceled by the customer; Check-Out – customer has checked in but already departed; No-Show – customer did not check-in and did inform the hotel of the reason why
# reservation_status_date: Date at which the last status was set. This variable can be used in conjunction with the ReservationStatus to understand when was the booking canceled or when did the customer checked-out of the hotel

library(dplyr); library(caret); library(e1071); library(glmnet); library(rpart); library(randomForest)
library(adabag); library(arules); library(arulesViz); library(RColorBrewer); library(fmsb); library(alr4)
library(cluster); library(dbscan); library(NbClust); library(recommenderlab); library(reshape)

#All the codes below are based on the data set after preprocessing

booking <- aggregate(Hotel$is_canceled, by = list(Hotel$hotel), FUN = length) 
cancel <- aggregate(Hotel$is_canceled, by = list(Hotel$hotel), FUN = sum) 
cancel$x/booking$x
barplot(cbind(booking[1,2],cancel[1,2],booking[2,2],cancel[2,2]), las = 0, beside = TRUE, 
        names.arg = c("City Hotel booking","City Hotel canceled","Resort Hotel booking","Resort Hotel canceled"),
        angle = c(15,-15,15,-15), density = c(10,5,10,5))

cityhotel <- Hotel[Hotel$hotel=="City Hotel",]
resorthotel <- Hotel[Hotel$hotel=="Resort Hotel",]
city.sample <- cityhotel[sample(nrow(cityhotel),1000),]
resort.sample <- cityhotel[sample(nrow(resorthotel),1000),]
plot(y = city.sample$lead_time, x = 1:1000)
plot(y = resort.sample$lead_time, x = 1:1000)

distinct(Hotel, arrival_date_year) #2015 2016 2017
month <- aggregate(Hotel$hotel, by = list(Hotel$arrival_date_month), FUN = length)
month <- month[c(5,4,8,1,9,7,6,2,12,11,10,3),]
barplot(month$x, names.arg = month$Group.1)
week <- aggregate(Hotel$hotel, by = list(Hotel$arrival_date_week_number), FUN = length)
barplot(week$x, names.arg = c(1:53))
day <- aggregate(Hotel$hotel, by = list(Hotel$arrival_date_day_of_month), FUN = length)
barplot(day$x, names.arg = c(1:31))

pairs( ~ stays_in_weekend_nights + stays_in_week_nights, data = Hotel)

citymeal <- aggregate(cityhotel$hotel, by = list(cityhotel$meal), FUN = length)
pie(citymeal$x, labels = citymeal$Group.1, main = "City Hotel")
resortmeal <- aggregate(resorthotel$hotel, by = list(resorthotel$meal), FUN = length)
pie(resortmeal$x, labels = resortmeal$Group.1, main = "Resort Hotel")

reservedroom <- aggregate(Hotel$hotel, by = list(Hotel$reserved_room_type), FUN = length)
reservedroom$x <- reservedroom$x/nrow(Hotel)*100
assignedroom <- aggregate(Hotel$hotel, by = list(Hotel$assigned_room_type), FUN = length)
assignedroom$x <- assignedroom$x/nrow(Hotel)*100
df1 <- data.frame(rbind(rep(80,10), rep(0,10), reservedroom$x))
colnames(df1) <- c("A","B","C","D","E","F","G","H","L","P")
radarchart(df1, title = "Reserved Room Type")
df2 <- data.frame(rbind(rep(80,12), rep(0,12), assignedroom$x))
colnames(df2) <- c("A","B","C","D","E","F","G","H","I","K","L","P")
radarchart(df2, title = "Assigned Room Type")

room_request_satisfied <- ifelse(Hotel$reserved_room_type == Hotel$assigned_room_type, 1, 0)
Hotel <- cbind(Hotel, room_request_satisfied)
nrow(Hotel[Hotel$reserved_room_type != Hotel$assigned_room_type,])/nrow(Hotel)

boxplot(adr ~ country, data = Hotel, ylim = c(0,350))

customeradr <- aggregate(Hotel$adr, by = list(Hotel$customer_type), FUN = median)
plot(customeradr$x, type = "o", xaxt = "n", pch = 16, lwd = 2, ylim = c(70,95))
axis(1, 1:4, customeradr$Group.1)
text(x = 1:4, y = customeradr$x+2, labels = customeradr$x)

set.seed(333)
inTrain <- createDataPartition(y=Hotel$adr, p=0.7, list=FALSE)
traindata <- Hotel[inTrain, ]
testdata <- Hotel[-inTrain, ]

m0 <- lm(is_canceled ~. - arrival_date_year – babies - market_segment - reserved_room_type -assigned_room_type-meal-hotel, data = traindata)
summary(m0)
Anova(m0)

Hotel$is_canceled <- as.factor(Hotel$is_canceled)
Hotel.sample <- Hotel[sample(nrow(Hotel),1000),]
pairs(~lead_time+stays_in_week_nights+adults+children+is_repeated_guest
      +room_request_satisfied+booking_changes+days_in_waiting_list
      +adr+total_of_special_requests, data = Hotel.sample)
NBfit <- naiveBayes(is_canceled ~ hotel+lead_time+arrival_date_month+stays_in_week_nights
                    +adults+children+meal+country+distribution_channel+is_repeated_guest
                    +room_request_satisfied+booking_changes+deposit_type+days_in_waiting_list
                    +customer_type+adr+total_of_special_requests,
                    laplace = 0, data = traindata)
pred <- predict(NBfit, testdata)
table(pred, testdata$is_canceled)

g_full <- glm(is_canceled ~ hotel+lead_time+arrival_date_month+stays_in_week_nights
              +adults+children+meal+country+is_repeated_guest
              +room_request_satisfied+booking_changes+deposit_type+days_in_waiting_list
              +customer_type+adr+total_of_special_requests,
              data = traindata, family = "binomial")
summary(g_full)
pred <- ifelse(predict(g_full, testdata)>0.5,1,0)
acc <- confusionMatrix(as.factor(pred), as.factor(testdata$is_canceled))
acc

x_train <- as.matrix(traindata[,c(3,8,9,10,11,12,17,20,22,24,25,26,27)])
y_train <- as.matrix(traindata$is_canceled)
x_test <- as.matrix(testdata[,c(3,8,9,10,11,12,17,20,22,24,25,26,27)])
y_test <- as.matrix(testdata$is_canceled)
cv.lasso <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 5)
fit_lasso <- glmnet(x_train, y_train, alpha = 1, lambda = cv.lasso$lambda.min)
coef(fit_lasso)
probabilities <- predict(fit_lasso, newx = x_test, s = cv.lasso$lambda.min)
pred_lasso <- ifelse(probabilities > 0.5, 1, 0)
acc <- confusionMatrix(as.factor(pred_lasso), as.factor(y_test))
acc

fit1 <- svm(is_canceled~lead_time+stays_in_weekend_nights+stays_in_week_nights
            +adults+children+babies+is_repeated_guest+booking_changes+days_in_waiting_list
            +adr+required_car_parking_spaces+total_of_special_requests+room_request_satisfied,
            data = traindata, type='C-classification', kernel='linear')
fit.pred1 <- predict(fit1, testdata)
table(pred = fit.pred1, true = testdata$is_canceled) 
fit2 <- svm(is_canceled~lead_time+stays_in_weekend_nights+stays_in_week_nights
            +adults+children+babies+is_repeated_guest+booking_changes+days_in_waiting_list
            +adr+required_car_parking_spaces+total_of_special_requests+room_request_satisfied,
            data = traindata, type='C-classification', kernel='radial')
fit.pred2 <- predict(fit2, testdata)
table(pred = fit.pred2, true = testdata$is_canceled)

fit <- rpart(is_canceled~lead_time+stays_in_weekend_nights+stays_in_week_nights
             +adults+children+babies+is_repeated_guest+booking_changes+days_in_waiting_list
             +adr+required_car_parking_spaces+total_of_special_requests+room_request_satisfied, 
             method = "class", data = traindata)
printcp(fit)
summary(fit)
result <- predict(fit, testdata, type="class")
table(pred = result, true = testdata$is_canceled)

fit <- randomForest(is_canceled~lead_time+stays_in_weekend_nights+stays_in_week_nights
                    +adults+children+babies+is_repeated_guest+booking_changes
                    +days_in_waiting_list+adr+required_car_parking_spaces
                    +total_of_special_requests+room_request_satisfied, 
                    data = traindata, mtry = 4, ntree = 100)
print(fit)
importance(fit)
varImpPlot(fit)
RandomTreeresult <- predict(fit, testdata, type = "class")
table(pred = RandomTreeresult, true = testdata$is_canceled)

adaboost <- boosting(is_canceled~lead_time+stays_in_weekend_nights+stays_in_week_nights
                     +adults+children+babies+is_repeated_guest+booking_changes
                     +days_in_waiting_list+adr+required_car_parking_spaces
                     +total_of_special_requests+room_request_satisfied, 
                     data = traindata, boos = FALSE, mfinal = 20, coeflearn = 'Breiman')
summary(adaboost)
adaboost$trees
adaboost$weights
adaboost$importance
importanceplot(adaboost)
pred <- predict(adaboost, testdata)
table(pred = pred$class, true = testdata$is_canceled)

Hotel <- Hotel[Hotel$adr <= 200 & Hotel$adr > 0,]
m0 <- lm(adr~.-is_canceled-arrival_date_year-arrival_date_week_number
         -arrival_date_day_of_month-market_segment-babies
         -days_in_waiting_list-assigned_room_type, 
         data = traindata)
Anova(m0)
summary(m0)
mean((m0$fitted.values - traindata$adr)^2)
mean((predict(m0, newdata = testdata) - testdata$adr)^2)
cbind(predict(m0, newdata = testdata), testdata$adr)[1:20,]

x_train <- as.matrix(traindata[,c(3,8,9,10,11,12,17,20,22,25,26,27)])
y_train <- as.matrix(traindata$adr)
x_test <- as.matrix(testdata[,c(3,8,9,10,11,12,17,20,22,25,26,27)])
y_test <- as.matrix(testdata$adr)
cv_ridge <- cv.glmnet(x_train, y_train, alpha = 0, nfolds = 5)
bestlam_ridge <- cv_ridge$lambda.1se
fit_ridge <- glmnet(x_train, y_train, alpha = 0)
coeff_ridge <- predict(fit_ridge, s = bestlam_ridge, type="coefficients")
y_ridge <- predict(fit_ridge, s = bestlam_ridge, newx = x_test)
err_ridge <- mean((y_ridge - y_test)^2)
cbind(y_ridge, y_test)[1:20,]
#repeat using lasso regression

is_canceled <- as.numeric(Hotel$is_canceled)-1
City_Hotel <- ifelse(Hotel$hotel == "City Hotel", 1, 0)
Resort_Hotel <- ifelse(Hotel$hotel == "Resort Hotel", 1, 0)
lead_time.long <- ifelse(Hotel$lead_time <= 79, 1, 0)
lead_time.short <- ifelse(Hotel$lead_time > 79, 1, 0)
children <- ifelse(Hotel$children > 0, 1, 0)
babies <- ifelse(Hotel$babies > 0, 1, 0)
is_repeated_guest <- Hotel$is_repeated_guest
booking_changes <- ifelse(Hotel$booking_changes > 0, 1, 0)
special_requests <- ifelse(Hotel$total_of_special_requests > 0, 1, 0)
require_car_parking_spaces <- ifelse(Hotel$required_car_parking_spaces > 0, 1, 0)
room_request_satisfied <- Hotel$room_request_satisfied
adr.high <- ifelse(Hotel$adr > 110, 1, 0)
adr.median <- ifelse(Hotel$adr <= 110 & Hotel$adr >= 70, 1, 0)
adr.low <- ifelse(Hotel$adr < 70, 1, 0)
association <- as.matrix(cbind(is_canceled,City_Hotel,Resort_Hotel,lead_time.long,
                               lead_time.short,children,babies,is_repeated_guest,
                               booking_changes,adr.high,adr.median,adr.low,
                               special_requests,room_request_satisfied,
                               require_car_parking_spaces))
trans <- as(association, "transactions")
summary(trans)
inspect(head(trans, 6))
LIST(head(trans, 6))
frequentItems <- eclat(trans, parameter = list(supp = 0.03, maxlen = 11, minlen = 5)) 
inspect(frequentItems)
rules <- apriori(trans, parameter = list(supp = 0.1, conf = 0.5, minlen = 2, maxlen = 11))
inspectDT(rules)  

Hotel.sample <- Hotel[sample(nrow(Hotel),500),]
Hotel.sample$market_segment <- as.factor(Hotel.sample$market_segment)
Hotel.sample$market_segment <- as.numeric(Hotel.sample$market_segment)
plot(x = 1:500, y = Hotel.sample$adr, pch = Hotel.sample$market_segment, ylim = c(0,200))
Hotel.sample <- cbind(1:500,Hotel.sample$adr)
Hotel.sample <- scale(Hotel.sample)
kNNdistplot(Hotel.sample, k = 7)
abline(h=.25, col="red")
db<-dbscan(Hotel.sample, eps=0.25, MinPts = 3)
db
str(db)
hullplot(Hotel.sample, db)

FRA <- Hotel[Hotel$country == "FRA",]
FRA.sample <- FRA[sample(nrow(FRA),500),]
FRA.sample <- FRA.sample[,c(1,5,10,11,12,13,18)]
FRA.sample$hotel <- as.numeric(as.factor(FRA.sample$hotel)) #City-1 Resort-2
FRA.sample$arrival_date_month <- as.numeric(FRA.sample$arrival_date_month)
FRA.sample$meal <- as.numeric(FRA.sample$meal) #BB-1 FB-2 HB-3 SC-4 U-5
FRA.sample$reserved_room_type <- as.numeric(as.factor(FRA.sample$reserved_room_type))
r <- FRA.sample
na <- rbind(sample(500,700,replace = TRUE), sample(7,700,replace = TRUE))
for (i in 1:700){
  row <- na[1,i]
  col <- na[2,i]
  r[row,col] <- NA
}
r <- as.matrix(r)
rownames(r) <- 1:500
rrm = as(r, "realRatingMatrix")
rec=Recommender(rrm[1:nrow(rrm)],method="UBCF", param=list(method="pearson",nn=2)) 
recom = predict(rec, rrm, type="ratings")
as(recom,"matrix")
summary(as(recom,"matrix"))
recom <- as(recom,"matrix")
meal_room <- recom[,c(6,7)]
result <- cbind(meal_room, FRA.sample[,c(6,7)])
colnames(result) <- c("premeal", "preroom", "meal", "room")
