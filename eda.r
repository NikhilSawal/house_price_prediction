setwd('E:/Programming/projects/Regression_house_price_pred')
library(ggplot2)
library(ggmap)
library(corrplot)
library(tidyr)
library(dplyr)

df <- read.csv('kc_house_data_original.csv', header = T)

# check for missing values
any(is.na(df))

# check the structure of the data
str(df)

# Make waterfront a Nominal variable
df$waterfront <- as.numeric(df$waterfront)

# Clean the date variable
df <- separate(data = df, col = date, into = c('date','junk'), sep = 'T')
df$junk <- NULL

# Use as.date to bring the date in a standard date format
df$date <- as.Date(df$date, format = "%Y%m%d")

# added a feature for Month
df$month <- sapply(df$date, function(x){format(x,"%B, %Y")})

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

# plots the distribution of the prices
ggplot(df, aes(x = price)) + 
  geom_histogram(bins = 50, alpha = .5, fill = 'blue') + 
  theme_minimal()





