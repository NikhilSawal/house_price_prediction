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
# df$month <- sapply(df$date, function(x){format(x,"%m")})
# df$month <- as.integer(df$month)

# Make waterfront a Nominal variable and remove id
df$waterfront <- as.factor(df$waterfront)
df <- df[, -1]

######
#Plots

# Box plot for waterfront vs. house prices
ggplot(df, aes(x = as.factor(df$waterfront), y = df$price)) + 
  geom_boxplot(aes(fill = as.factor(df$waterfront))) +
  labs(x = "Waterfront", y = "House Prices") +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

# Visualize the houses sold with google maps
my_location <- 'King County, WA, USA'
my_map <- get_map(location = my_location, source = 'google', maptype = 'roadmap')

ggmap(my_map) +
  geom_point(aes(x = long, y = lat), data = df, alpha = .5, color = 'darkred', size = 1)

# Make the correlation matrix
num.col <- sapply(df, is.numeric)
cor.data <- round(cor(df[,num.col]),2)

# correlation plot for numerical variables
corrplot(cor.data, method = 'number')

corrgram(df, order = T, lower.panel = panel.shade, upper.panel = panel.conf, 
         text.panel = panel.txt)

# plots the distribution of the prices
ggplot(df, aes(x = df$price)) + 
  geom_histogram(aes(fill = as.factor(df$waterfront)), color = 'black', bins = 50 ,alpha = .7) + 
  scale_x_continuous(labels = scales::comma) +
  scale_fill_manual(values = c('#ae4554','#faf7ea')) +
  labs(x = 'Price') +
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

#Function for residual plot distribution
model_residual_plots <- function(mod){
  
  res <- residuals(mod)
  res <- as.data.frame(res)
  
  print(ggplot(res, aes(res)) + 
    geom_histogram(fill = 'blue', alpha = .5, bins = 40) +
    scale_x_continuous(labels = scales::comma))
  print(plot(mod))
  
}

model.residuals(model)

#Normalize data
maxs <- apply(df[,-1], 2, max)
mins <- apply(df[,-1], 2, min)
scaled.data <- scale(df[,-1], center = mins, scale = maxs - mins)
scaled.data <- as.data.frame(scaled.data)
scaled.data <- cbind(df$date, scaled.data)
names(scaled.data)[1] <- paste("date")
