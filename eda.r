setwd('E:/Programming/projects/Regression_house_price_pred')
library(ggplot2)
library(ggmap)
library(ggthemes)
library(corrgram)
library(corrplot)
library(tidyr)
library(dplyr)
library(caTools)

df <- read.csv('kc_house_data_original.csv', header = T)

##########################
#Exploratory Data Analysis

# missing values  check
any(is.na(df))

# check the structure of the data
str(df)

# Clean the date variable
df <- separate(data = df, col = date, into = c('date','junk'), sep = 'T')
df$junk <- NULL

# Use as.date to bring the date in a standard date format
df$date <- as.Date(df$date, format = "%Y%m%d")

# # added a feature for Month
# df$month <- sapply(df$date, function(x){format(x,"%B, %Y")})

# # Make waterfront a Nominal variable
# df$waterfront <- as.factor(df$waterfront)

######
#Plots

# Box plot for waterfront vs. house prices
ggplot(df, aes(x = as.factor(df$waterfront), y = df$price)) + 
  geom_boxplot(aes(fill = df$waterfront)) +
  theme_bw()

# Visualize the houses sold with google maps
my_location <- 'King County, WA, USA'
my_map <- get_map(location = my_location, source = 'google', maptype = 'roadmap')

ggmap(my_map) +
  geom_point(aes(x = long, y = lat), data = df, alpha = .5, color = 'darkred', size = 1)

# Make the correlation matrix
num.col <- sapply(df, is.numeric)
cor.data <- cor(df[,num.col])

# correlation plot for numerical variables
corrplot(cor.data, method = 'color')

corrgram(df, order = T, lower.panel = panel.shade, upper.panel = panel.pie, 
         text.panel = panel.txt)

# plots the distribution of the prices
ggplot(df, aes(x = price)) + 
  geom_histogram(bins = 50, alpha = .5, fill = 'blue') + 
  theme_minimal()

#test train split
set.seed(101)
sample <- sample.split(df$price,SplitRatio = .7)
train <- subset(df, sample == T)
test <- subset(df, sample == F)

#build the first basic model
model <- lm(price ~ ., train)
summary(model)

price.pred <- predict(model, test)
results <- cbind(price.pred, test$price)
results <- as.data.frame(results)
colnames(results) <- c('Prediction','Actual')
mse <- mean((results$Actual - results$Prediction)^2)

 



