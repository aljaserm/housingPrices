#  get the data
housingData <- read.csv("C:/Users/aljas/OneDrive/Documents/Development/R/HousingPricing/HousingPricing/House_Price.csv", header = TRUE)

#Observe the data
str(housingData)
summary(housingData)
hist(housingData$crime_rate)
pairs(~price + crime_rate + n_hot_rooms + rainfall, data = housingData)
barplot(table(housingData$bus_ter))

#Observations
# n_hot_rooms and rainfall has outliers
# n_hos_beds has missing values
# bus_ter is useless
# crime rate has some other functional relation with pricing

# Clean the data -- Start

# to clear hotel room outliers
n_hot_rooms_upperValue=3*quantile(housingData$n_hot_rooms, 0.99)
housingData$n_hot_rooms[housingData$n_hot_rooms>n_hot_rooms_upperValue] <- n_hot_rooms_upperValue
summary(housingData$n_hot_rooms)

# to clear rainfall outliers
rainfall_lowerValue= 0.3 *quantile(housingData$rainfall, 0.01)
housingData$rainfall[housingData$rainfall<rainfall_lowerValue] <- rainfall_lowerValue
summary(housingData$rainfall)

# handle missing info
mean(housingData$n_hos_beds)
mean(housingData$n_hos_beds,na.rm = TRUE)
which(is.na(housingData$n_hos_beds))
housingData$n_hos_beds[is.na(housingData$n_hos_beds)] <- mean(housingData$n_hos_beds,na.rm = TRUE)
summary(housingData$n_hos_beds)

# handle the realtion between Crime and price
pairs(~price + crime_rate, data = housingData)
plot(housingData$price, housingData$crime_rate)
housingData$crime_rate=log(1+housingData$crime_rate)

# remove unwated data
housingData$avg_Dist = (housingData$dist1+housingData$dist2 +housingData$dist3+ housingData$dist4)/4
housingDataNew <- housingData[,-7:-10]
housingData <- housingDataNew
rm(housingDataNew)

# Remove bus terminal
housingData <- housingData[,-14]

# replace to non-numeric values
#install.packages("dummies")
housingData <- dummy.data.frame(housingData)
housingData <- housingData[,-9]
housingData <- housingData[,-14]

#  correlation matrix
cor(housingData)
round(cor(housingData), 2)
housingData <- housingData[,-16]
# Clean the data -- End

# Linear Regresson model -- Start
# simple Linear Regresson model -- Start
simple_Model_forLR <- lm(price~room_num, data = housingData)
summary(simple_Model_forLR)
plot(housingData$room_num, housingData$price)
abline(simple_Model_forLR)
# simple Linear Regresson model -- End
# Multiple Linear Regresson model -- Start
Multiple_Model_forLR <- lm(price~.,data = housingData)
summary(Multiple_Model_forLR)
# Multiple Linear Regresson model -- End
# Linear Regresson model -- End

# Split the data to train and test 
install.packages("caTools")
set.seed(0)
split= sample.split(housingData, SplitRatio = 0.8)
trainingSet = subset(housingData, split == TRUE)
testSet= subset(housingData, split == FALSE)

training_a_LR = lm(price~., data = trainingSet)
train_a = predict(training_a_LR, trainingSet)
test_a = predict(training_a_LR, testSet)
mean((trainingSet$price - train_a)^2)
mean((testSet$price - test_a)^2)
