setwd('E:/Programming/projects/Regression_house_price_pred')
library(ggplot2)
library(ggmap)
library(corrplot)

df <- read.csv('kc_house_data_original.csv', header = T)

#check for missing values
any(is.na(df))

#check the structure of the data
str(df)

# Nominal variables
# waterfront, 
df$waterfront <- factor(df$waterfront)

my_location <- 'King County, WA, USA'
my_map <- get_map(location = my_location, source = 'google', maptype = 'roadmap')

ggmap(my_map) +
  geom_point(aes(x = long, y = lat), data = df, alpha = .5, color = 'darkred', size = 1)

#Make the correlation matrix
num.col <- sapply(df, is.numeric)
cor.data <- cor(df[,num.col])

#correlation plot for numerical variables
corrplot(cor.data, method = 'color')

#plots the distribution of the prices
ggplot(df, aes(x = price)) + 
  geom_histogram(bins = 50, alpha = .5, fill = 'blue') + 
  theme_minimal()

