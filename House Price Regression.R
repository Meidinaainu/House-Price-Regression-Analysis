library(ggplot2)
library(lmtest)
library(car)
library(readxl)

data <- read_excel("D:/house_price.xlsx")
data.frame(data)
str(data)
head(data)

#Data Exploration
summary(data)
col_numeric <- sapply(data, is.numeric)
numericalVars <- data[,col_numeric]
corrMat <- cor(numericalVars)
corrMat
corrplot(corrMat, method = "ellipse")

par(mfrow = c(1,2))
hist(data$House_Price, main = "Price Frequency")
truehist(data$House_Price, main = "Price Frequency - normalized")

#Model
model <- lm(data$House_Price ~ data$Square_Footage + data$Num_Bedrooms + data$Num_Bathrooms + data$Year_Built + data$Lot_Size + data$Garage_Size + data$Neighborhood_Quality)
summary(model)

#Normality Test
residual <- residuals(model)
jarque.bera.test(residual)

#Multicollinearity Test
vif(model)

#Autocorrelation Test
dwtest(model)

#Heteroscedasticity Test
par(mfrow=c(2,2))
plot(model)
