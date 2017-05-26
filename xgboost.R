# copy the log to a text file
library('data.table')
train = fread('train.csv', stringsAsFactors = T)
#train = train[1:100000,]
test = fread('test.csv', stringsAsFactors = T)
#test = test[1:20000,]
submission = fread('sample_submission.csv', stringsAsFactors = T)

train[store_and_fwd_flag == "", store_and_fwd_flag:= "N"]
train[is.na(pickup_latitude), pickup_latitude:= 0.00000]
train[is.na(pickup_longitude), pickup_longitude:= 0.00000]
train[is.na(dropoff_latitude) , dropoff_latitude:= 0.00000]
train[is.na(dropoff_longitude), dropoff_longitude:= 0.00000]

train[payment_type == 'UNK' , payment_type:='CRD']
train$payment_type = factor(train$payment_type, levels = c('CRD','CSH','DIS','NOC'), labels = c(0,1,2,3))
train$store_and_fwd_flag = factor(train$store_and_fwd_flag, levels = c('Y','N'), labels = c(1,0))

train$pickup_longitude[train$pickup_longitude < -180] <- -180
train$pickup_longitude[train$pickup_longitude > 180] <- 180
train$pickup_latitude[train$pickup_latitude < -90] <- -90
train$pickup_latitude[train$pickup_latitude > 90] <- 90

train[dropoff_longitude< -180 , dropoff_longitude:=-180] 
train[dropoff_longitude > 180,dropoff_longitude := 180]
train[dropoff_latitude > 90,dropoff_latitude:=90]
train[dropoff_latitude < -90 , dropoff_latitude:= -90]

#new user column contains empty value
train[new_user == "", new_user := "NO"]

#removing negative values form tolls
train[tolls_amount < 0, tolls_amount:=0.00]

#removing NA values from tip amount with mode which is 0.00
train[tip_amount<0, tip_amount := 0.00][is.na(tip_amount),tip_amount:=0.00]

#mta_tax is 0.5,-0.5, 0.0
train[mta_tax ==-0.5 , mta_tax :=0.5]

#surcharge removing NA and negative 
train[is.na(surcharge),surcharge := 0.00]
train[surcharge < 0, surcharge := 0.00]

train$new_user = factor(train$new_user, labels = c(0,1))

train[vendor_id=="DST000532",vendor_id := "0"]
train[vendor_id=="DST000401",vendor_id := "1"]
train[vendor_id=="DST000543",vendor_id := "2"]
train[vendor_id=="DST000481",vendor_id := "3"]

intern = fread('intern.csv')
intern_test = fread('intern_test.csv')
train[, time:= intern$time]
train[,distance:= intern$distance]
test[, time:= intern_test$time]
test[,distance:= intern_test$distance]

#install.packages('geosphere')
library(sp)
library(geosphere)

pickup_list <- train[,.(pickup_longitude,pickup_latitude)]
dropoff_list <- train[,.(dropoff_longitude,dropoff_latitude)]

train$distance <- distVincentyEllipsoid(pickup_list[,c('pickup_longitude','pickup_latitude')], dropoff_list[,c('dropoff_longitude','dropoff_latitude')])


train$time = as.numeric(difftime(strptime(train[,dropoff_datetime],"%Y-%m-%d %H:%M:%S"),strptime(train[,pickup_datetime],"%Y-%m-%d %H:%M:%S")))

train$distance = round(train$distance,2)
test$distance = round(test$distance,2)
#train$dist_time_fare = train$fare_amount - train$tolls_amount - train$tip_amount - train$mta_tax - train$surcharge

# make a classifier using random forest
#install.packages('randomForest')
#manage train here
a = c(2:6,9,12:13,16:20)
train = train[, a,with=FALSE]

#View(new_dataset)
#install.packages('randomForest')
library(randomForest)
set.seed(123)
View(train)

#preprocessing of test set
test[store_and_fwd_flag == "", store_and_fwd_flag:= "N"]
test[is.na(pickup_latitude), pickup_latitude:= 0.00000]
test[is.na(pickup_longitude), pickup_longitude:= 0.00000]
test[is.na(dropoff_latitude) , dropoff_latitude:= 0.00000]
test[is.na(dropoff_longitude), dropoff_longitude:= 0.00000]

test[payment_type == 'UNK' , payment_type:='CRD']
test$payment_type = factor(test$payment_type, levels = c('CRD','CSH','DIS','NOC'), labels = c(0,1,2,3))
test$store_and_fwd_flag = factor(test$store_and_fwd_flag, levels = c('Y','N'), labels = c(1,0))

test$pickup_longitude[test$pickup_longitude < -180] <- -180
test$pickup_longitude[test$pickup_longitude > 180] <- 180
test$pickup_latitude[test$pickup_latitude < -90] <- -90
test$pickup_latitude[test$pickup_latitude > 90] <- 90

test[dropoff_longitude< -180 , dropoff_longitude:=-180] 
test[dropoff_longitude > 180,dropoff_longitude := 180]
test[dropoff_latitude > 90,dropoff_latitude:=90]
test[dropoff_latitude < -90 , dropoff_latitude:= -90]

#new user column contains empty value
test[new_user == "", new_user := "NO"]



test$new_user = factor(test$new_user, labels = c(0,1))


test[vendor_id=="DST000532",vendor_id := "0"]
test[vendor_id=="DST000401",vendor_id := "1"]
test[vendor_id=="DST000543",vendor_id := "2"]
test[vendor_id=="DST000481",vendor_id := "3"]


#removing negative values form tolls
test[tolls_amount < 0, tolls_amount:=0.00]

#removing NA values from tip amount with mode which is 0.00
test[tip_amount<0, tip_amount := 0.00][is.na(tip_amount),tip_amount:=0.00]

#mta_tax is 0.5,-0.5, 0.0
test[mta_tax ==-0.5 , mta_tax :=0.5]

#surcharge removing NA and negative 
test[is.na(surcharge),surcharge := 0.00]
test[surcharge < 0, surcharge := 0.00]

#install.packages('geosphere')
library(geosphere)

pickup_list <- test[,.(pickup_longitude,pickup_latitude)]
dropoff_list <- test[,.(dropoff_longitude,dropoff_latitude)]

test$distance <- distVincentyEllipsoid(pickup_list[,c('pickup_longitude','pickup_latitude')], dropoff_list[,c('dropoff_longitude','dropoff_latitude')])

test$time = as.numeric(difftime(strptime(test[,dropoff_datetime],"%Y-%m-%d %H:%M:%S"),strptime(test[,pickup_datetime],"%Y-%m-%d %H:%M:%S")))

#test$dist_time_fare = test$fare_amount - test$tolls_amount - test$tip_amount - test$mta_tax - test$surcharge

# make a classifier using random forest
#install.packages('randomForest')
#manage test here 
b = c(2:6,9,12:13,16:19)
test = test[, b,with=FALSE]

str(train)
#convert factors or characters to numeric to use the xgboost
train$vendor_id = as.numeric(as.character(train$vendor_id))
train$store_and_fwd_flag = as.numeric(as.character(train$store_and_fwd_flag))
train$payment_type = as.numeric(as.character(train$payment_type))
train$new_user = as.numeric(as.character(train$new_user))

test$vendor_id = as.numeric(as.character(test$vendor_id))
test$store_and_fwd_flag = as.numeric(as.character(test$store_and_fwd_flag))
test$payment_type = as.numeric(as.character(test$payment_type))
test$new_user = as.numeric(as.character(test$new_user))

library(randomForest)
set.seed(123)
View(test)

library(xgboost)
#xgb <- xgboost(data = as.matrix(train[,-11]), label = train$fare_amount, eta = 0.05, max_depth = 15, nrounds = 25, subsample = 0.5, colsample_bytree = 0.5, seed = 1)

#xgb <- xgboost(data = as.matrix(train[,-11]), label = train$fare_amount, eta = 0.1,nrounds = 1000)
xgb <- xgboost(data = as.matrix(train[,-11]), label = train$fare_amount, eta = 0.05,nrounds = 3000, max_depth = 10,subsample = 0.5,seed = 1)

y_pred <- predict(xgb, as.matrix(test))

# h20 package
library(h2o)
localH2O <- h2o.init(nthreads = -1)
h2o.init()

train.h2o <- as.h2o(train)
test.h2o <- as.h2o(test)
colnames(train.h2o)
indep <- c(1:10,12:13)
dep <-11

rforest.model <- h2o.randomForest(y = dep, x = indep, training_frame = train.h2o, ntrees =36)
y_pred <- as.data.frame(h2o.predict(rforest.model,test.h2o))

submission$fare_amount<- y_pred
fwrite(submission,file="sub10.csv")

#h2o.performance(rforest.model)
#regressor = randomForest(x = train[,1:11], y = train$fare_amount , ntree =36)
#y_pred = predict(regressor,newdata = test)
#y_pred = round(y_pred, 2)
#submission$fare_amount = y_pred
#fwrite(submission, file = "sub.csv")
#sorted <- train[,.N,by=list(passenger_count)]
#sorted[order(-N)]





#can do cheda khani with pessenger count
# c <- list(train, test)
#combin <- rbindlist(c)
#combin[, Product_Count := .N, by = Product_ID]
#c.train <- combin[1:nrow(train),]
# c.test <- combin[-(1:nrow(train)),]
#library(dummies)
# combin <- dummy.data.frame(combin, names = c("City_Category"), sep = "_")
#can do cheda khani with passengers


