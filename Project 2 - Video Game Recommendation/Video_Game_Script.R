knitr::opts_chunk$set(echo = TRUE, fig.align = "center", warning = FALSE)


#################### Packages #################### 
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(knitr)) install.packages("knitr")
if(!require(kableExtra)) install.packages("kableExtra")
if(!require(dplyr)) install.packages("dplyr")
if(!require(randomForest)) install.packages("randomForest")



#################### Libraries #################### 
library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)
library(knitr)
library(kableExtra)
library(dplyr)
library(randomForest)

#################### Data Sets #################### 

#Dataset originaly downloaded from https://www.kaggle.com/datasets/gregorut/videogamesales

vgsales_data <- read.csv(file = 'vgsales.csv')

# Changing data type of year.
vgsales_data <- vgsales_data  %>%
  mutate(Year = as.numeric(Year))



# Selecting the first 10 rows of the dataset.
top10 <- head(vgsales_data, 10)

# Kniting the table
knitr::kable(top10, caption="First 10 rows of dataset.", 
               booktabs = T) %>% 
  kable_styling(latex_options = c("striped", "HOLD_position", "scale_down"),
                position = "center")



# Selecting the bottom 10 rows of the dataset.
bottom10 <- tail(vgsales_data, 10)

# Kniting the table
knitr::kable(bottom10, caption="Bottom 10 rows of dataset.", 
               booktabs = T,
             row.names = FALSE) %>% 
  kable_styling(latex_options = c("striped", "HOLD_position", "scale_down"),
                position = "center")



# Calculating the total unit sales per grouping by name and slicing only the top 10.
top10name <- vgsales_data %>%
  group_by(Name) %>%
  summarise(sum = sum(Global_Sales)) %>%
  slice_max(sum, n=10) %>%
  ggplot(aes(x = reorder(Name, sum), sum)) +
  geom_bar(stat="identity", color = I("black"), fill = "darkseagreen4") +
  coord_flip() +
  labs(x = "Name", 
       y = "Global Sales Units in Millions") 

#Figure 1
top10name


#Sum of sales per Platform and slicing only the top 10.
top10platform <- vgsales_data %>%
  group_by(Platform) %>%
  summarise(total = sum(Global_Sales)) %>%
  slice_max(total, n=10) %>%
  ggplot(aes(x = reorder(Platform, total), total)) +
  geom_bar(stat="identity", color = I("black"), fill = "darkseagreen4") +
  coord_flip()  +
  labs(x = "Platform", 
       y = "Global Sales Unit in Millions") 

#Figure 2
top10platform


#Plot of Sales units per year , excluding NAs.
sales_year <- vgsales_data %>%
  group_by(Year) %>%
  summarise(total = sum(Global_Sales)) %>%
  filter(!is.na(Year)) %>%
  ggplot(aes(Year, total)) +
  geom_bar(stat="identity", color = I("black"), fill = "darkseagreen4") +
  labs(x = "Year", 
       y = "Global Sales Unit in Millions") 

#Figure 3
sales_year

#Plot of Sales units per year , excluding NAs and slicing the top 10.
top10year <- vgsales_data %>%
  group_by(Year) %>%
  summarise(total = sum(Global_Sales)) %>%
    slice_max(total, n=10) %>%
  ggplot(aes(x = reorder(Year, -total), total)) +
  geom_text(aes(label=total), vjust=-0.5) +
  geom_bar(stat="identity", color = I("black"), fill = "darkseagreen4") +
  labs(x = "Year", 
       y = "Global Sales Unit in Millions") 

#Figure 4
top10year



#Plot of Average Sales units per Genres.
avg_genre <- vgsales_data %>%
  group_by(Genre) %>%
  summarise(avg = sum(Global_Sales/n())) %>%
  ggplot(aes(x = reorder(Genre, avg), avg)) +
  geom_bar(stat="identity", color = I("black"), fill = "darkseagreen4") +
  coord_flip() +
  labs(x = "Genre", 
       y = "Average Sales Unit") 

#Figure 5
avg_genre



#Plot of Global Sales units per Genres that are higher than 25 mi.
total_genre <- vgsales_data %>%
  group_by(Year, Genre) %>%
  summarise(sales = sum(Global_Sales), .groups = "keep") %>%
  filter(sales >= 25 && is.na(Year) == FALSE) %>%
  ggplot(aes(Year, sales, colour=Genre, size=sales, alpha=0.5)) +
  geom_point() +
  scale_size(range = c(.1, 15)) +
  labs(x = "Release Year", 
       y = "Global Sales per Genre")
  

#Figure 6
total_genre



#Plot of Top 10 Total Sales units per Publishers
publisher_sales <- vgsales_data %>%
  group_by(Publisher) %>%
  summarise(total = sum(Global_Sales)) %>%
  slice_max(total, n=10) %>%
  ggplot(aes(x = reorder(Publisher, total), total)) +
  geom_bar(stat="identity", color = I("black"), fill = "darkseagreen4") +
  coord_flip() +
  labs(x = "Publisher", 
       y = "Top 10 Global Sales Unit in Millions") 

#Figure 7
publisher_sales



#Sales Quantity per Region grouped by Year and analyzed individualy
salesPerRegion <- vgsales_data %>%
  group_by(Year) %>%
  filter(!is.na(Year)) %>%
  summarise(nasales = sum(NA_Sales),
            eusales = sum(EU_Sales),
            jpsales = sum(JP_Sales),
            othersales = sum(Other_Sales)) %>%
  ggplot(aes(Year)) +
  geom_step(aes(y = nasales, color = "darkblue")) +
  geom_step(aes(y = eusales, color = "darkgreen")) +
  geom_step(aes(y = jpsales, color = "darkred") ) +
  geom_step(aes(y = othersales, color = "black"))  +
  scale_color_identity(name = "Sales Region",
                       breaks = c("darkblue", "darkgreen", "darkred", "black"),
                       labels = c("North America", "Europe", "Japan", "Others"),
                       guide = "legend")+
    labs(x = "Release Year", 
       y = "Sales per Region") 


#Figure 8
salesPerRegion



## Cleaning previous objects and the memory.
rm(avg_genre, bottom10, publisher_sales, sales_year, salesPerRegion, top10, top10name, top10platform, top10year, total_genre)
gc()

####################  Pre Processing #################### 

# Converting the Genre, Platform and Publisher to numeric.
vgsales_data$Genre <- unclass(factor(vgsales_data$Genre))
vgsales_data$Platform <- unclass(factor(vgsales_data$Platform))
vgsales_data$Publisher <- unclass(factor(vgsales_data$Publisher))

# Removing rows with null years.
vgsales_data <- vgsales_data %>% filter(!is.na(Year))

vgsales_data <- vgsales_data %>% filter(Global_Sales <= 7)


#################### train_set / test_set #################### 

# Feature and target definition
x <- vgsales_data %>% 
  select(Platform, Year, Genre, Publisher,NA_Sales, EU_Sales, JP_Sales,Other_Sales)
y <- vgsales_data$Global_Sales

# Split data into training and test sets for x and y
set.seed(1)
index <- createDataPartition(y, p=0.80, list=FALSE)

# Defining x_train and x_test.
x_train <- x[ index, ]
x_test <- x[-index, ]

#Defining y_train and y_test
y_train <- y[index]
y_test <- y[-index]

#Remove unused objects from memory.
rm(index)



#Simple average over Global Sales
mu_hat <- mean(y_train)

# RMSE 
naive_rmse <- round(RMSE(y_train, mu_hat),5)

#Ploting the simple average.
rmse_results <- data.frame(Method = "Simple Average",  RMSE = naive_rmse)

knitr::kable(rmse_results, 
               booktabs = T) %>% 
  kable_styling(latex_options = c("striped", "HOLD_position"),
                position = "center")

# Fiting the model
set.seed(1)
fit <- lm(y_train ~ Platform + Year + Genre + Publisher, data = x_train)

# Predicting the values based on the test_set.
y_hat <- predict(fit, x_test)

# Predict on the test set, round and format.
pred_rmse <- format(round(RMSE(y_test, y_hat),5), nsmall = 5)

# Adding the results to rmse_results.
rmse_results <- rmse_results %>%
  rbind(c("Linear Regression", pred_rmse))

knitr::kable(rmse_results, 
               booktabs = T) %>% 
  kable_styling(latex_options = c("striped", "HOLD_position"),
                position = "center")


# Fiting the model
set.seed(1)
fit <- lm(y_train ~ ., data = x_train)

# Predicting the values based on the test_set.
y_hat <- predict(fit, x_test)

# Predict on the test set, round and format.
pred_rmse <- format(round(RMSE(y_test, y_hat),5), nsmall = 5)

# Adding the results to rmse_results.
rmse_results <- rmse_results %>%
  rbind(c("Linear Regression (All)", pred_rmse))

knitr::kable(rmse_results, 
               booktabs = T) %>% 
  kable_styling(latex_options = c("striped", "HOLD_position"),
                position = "center")


# Fitting the random Forest
set.seed(1)
fit <- randomForest(x = x_train, y = y_train , maxnodes = 10, ntree = 10)

# Make prediction
predictions <- predict(fit, x_test)

# Predict on the test set, round and format.
pred_rmse <- format(round(RMSE(y_test, predictions),5), nsmall = 5)

# Adding the results to rmse_results.
rmse_results <- rmse_results %>%
  rbind(c("Random Forest Regression", pred_rmse))



# Ploting the results for fit.
fit



# Build scatter plot
predvsactuals <- ggplot() + 
  geom_jitter( aes(x=x_test$Year, y=y_test, color = 'red') ) + 
  geom_jitter( aes(x=x_test$Year , y = predictions, color = 'blue')) + 
scale_color_identity(name = "Comparision",
                       breaks = c("blue", "red"),
                       labels = c( "Predicted", "Real"),
                       guide = "legend")+
    labs(x = "Release Year", 
       y = "Sales") 

#Figure 9
predvsactuals

knitr::kable(rmse_results, 
               booktabs = T) %>% 
  kable_styling(latex_options = c("striped", "HOLD_position"),
                position = "center")


# Importance of variables
importance <- importance(fit) 

# Data Frame importance. 
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[,'IncNodePurity'], 0))


# Importance of predictors
predImp <- ggplot(varImportance, aes(x = reorder(Variables, Importance), 
                                   y = Importance, fill = Importance)) +
           geom_bar(stat='identity') + 
           labs(title = 'Importance of Predictors', x = 'Predictors', y = 'rmsle') +
           coord_flip() 

#Figure 10
predImp


# Fitting the random Forest
set.seed(1)
fit <- randomForest(x = x_train, y = y_train , maxnodes = 140, ntree = 1100)

# Make prediction
predictions <- predict(fit, x_test)

# Predict on the test set, round and format.
pred_rmse <- format(round(RMSE(y_test, predictions),5), nsmall = 5)

# Adding the results to rmse_results.
rmse_results <- rmse_results %>%
  rbind(c("Random Forest Tunning", pred_rmse))



# Ploting the results for fit.
fit


# Build scatter plot
predvsactuals <- ggplot() + 
  geom_jitter( aes(x=x_test$Year, y=y_test, color = 'red') ) + 
  geom_jitter( aes(x=x_test$Year , y = predictions, color = 'blue')) + 
  scale_color_identity(name = "Comparision",
                       breaks = c("blue", "red"),
                       labels = c( "Predicted", "Real"),
                       guide = "legend")+
  labs(x = "Release Year", 
       y = "Sales") 

#Figure 9
predvsactuals

# Ploting the errors vs the trees.
plot(fit)

knitr::kable(rmse_results, 
             booktabs = T) %>% 
  kable_styling(latex_options = c("striped", "HOLD_position"),
                position = "center")
