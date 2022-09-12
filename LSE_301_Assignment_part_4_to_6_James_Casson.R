Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 4. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 5. Include your insights and observations.

###############################################################################


# Import the tidyverse library.
library(tidyverse)

#im using file.choose() because in an actual work environment i will not always 
#know where my colleagues have their csv files saved.
f <- file.choose()
# Import a  CSV file.
sales <- read.csv((f), header=T)
# Print the dataframe.
sales
View(sales)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
R= subset(sales, select=c(Product,Platform,NA_Sales,EU_Sales,Global_Sales))


# View the data frame.
R
View(R)

# View the descriptive statistics.
summary(R)

# Return the structure of the data frame.
str(R)

# Check the type of the data frame.
typeof(R)

# Check the class of the data frame.
class(R)

# Check the dimensions of the data frame
dim(R)

#################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
#Create Scatterplots
qplot(NA_Sales, EU_Sales, data=R)
qplot(NA_Sales,Global_Sales, data=R)
qplot(EU_Sales,Global_Sales, data=R)

## 2b) Histograms
# Create histograms.
qplot(NA_Sales,bins=20, data=R)
qplot(EU_Sales,bins=20, data=R)
qplot(Global_Sales,bins=20, data=R)

## 2c) Boxplots
# Create boxplots.
qplot(NA_Sales, colour=I("red"),data=R, geom='boxplot')
qplot(EU_Sales, colour=I("red"),data=R, geom='boxplot')
qplot(Global_Sales, colour=I("red"),data=R, geom='boxplot')

# 3. Determine the impact on sales per product_id.

## 3a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
Sales_Impact <- sales %>% group_by(Product) %>% 
  summarise(across(.cols = c(NA_Sales, EU_Sales, Global_Sales),list(sum=sum)))         

# View the data frame.
head(as_tibble(Sales_Impact))
# Explore the data frame.
summary(Sales_Impact)


## 3b) Determine which plot is the best to compare game sales.
# Create scatterplots.
qplot(NA_Sales_sum, EU_Sales_sum, data=Sales_Impact)
qplot(NA_Sales_sum,Global_Sales_sum, data=Sales_Impact)
qplot(EU_Sales_sum,Global_Sales_sum, data=Sales_Impact)

# Create histograms.
qplot(NA_Sales_sum,bins=20, data=Sales_Impact)
qplot(EU_Sales_sum,bins=20, data=Sales_Impact)
qplot(Global_Sales_sum,bins=20, data=Sales_Impact)

# Create boxplots.
qplot(NA_Sales_sum, colour=I("red"),data=Sales_Impact, geom='boxplot')
qplot(EU_Sales_sum, colour=I("red"),data=Sales_Impact, geom='boxplot')
qplot(Global_Sales_sum, colour=I("red"),data=Sales_Impact, geom='boxplot')

###############################################################################

# 4. Observations and insights
# Your observations and insights here...
#boxplots are the best to compare game sales, followed by scatter plots
#box plots allow us to compare the outliers better as well as the main body of 
#the data. Scatterplots allow us to compare the relationships of the different
#data together and see if a line of correlation could fit.
###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 3. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 4. Include your insights and observations.

################################################################################


# 1. Load and explore the data

# View data frame created in Week 4.
R
View(R)
Sales_Impact
View(Sales_Impact)

# Check output: Determine the min, max, and mean values.
min(Sales_Impact$NA_Sales_sum)
min(Sales_Impact$EU_Sales_sum)
min(Sales_Impact$Global_Sales_sum)

max(Sales_Impact$NA_Sales_sum)
max(Sales_Impact$EU_Sales_sum)
max(Sales_Impact$Global_Sales_sum)

mean(Sales_Impact$NA_Sales_sum)
mean(Sales_Impact$EU_Sales_sum)
mean(Sales_Impact$Global_Sales_sum)

# View the descriptive statistics.
summary(Sales_Impact)


###############################################################################

# 2. Determine the normality of the data set.

## 2a) Create Q-Q Plots
# Create Q-Q Plots.
library("car")

qqnorm(Sales_Impact$NA_Sales_sum)
qqline(Sales_Impact$NA_Sales_sum)

qqnorm(Sales_Impact$EU_Sales_sum)
qqline(Sales_Impact$EU_Sales_sum)

qqnorm(Sales_Impact$Global_Sales_sum)
qqline(Sales_Impact$Global_Sales_sum)

## 2b) Perform Shapiro-Wilk test
# Install and import Moments.
install.packages("moments")
library(moments)

# Perform Shapiro-Wilk test.
shapiro.test(Sales_Impact$NA_Sales_sum)
shapiro.test(Sales_Impact$EU_Sales_sum)
shapiro.test(Sales_Impact$Global_Sales_sum)

#Determine the log of NA_Sales_sum, EU_Sales_sum, Global_Sales_sum
Sales_Impact$log_NA_Sales_sum = log1p(Sales_Impact$NA_Sales_sum)
Sales_Impact$log_EU_Sales_sum = log1p(Sales_Impact$EU_Sales_sum)
Sales_Impact$log_Global_Sales_sum = log1p(Sales_Impact$Global_Sales_sum)
head(as_tibble(Sales_Impact))
view(Sales_Impact)
#create Q-Q plots on the log values

qqnorm(Sales_Impact$log_NA_Sales_sum)
qqline(Sales_Impact$log_NA_Sales_sum)

qqnorm(Sales_Impact$log_EU_Sales_sum)
qqline(Sales_Impact$log_EU_Sales_sum)

qqnorm(Sales_Impact$log_Global_Sales_sum)
qqline(Sales_Impact$log_Global_Sales_sum)


#Perform shapiro_Wilk test on log values
shapiro.test(Sales_Impact$log_NA_Sales_sum)
shapiro.test(Sales_Impact$log_EU_Sales_sum)
shapiro.test(Sales_Impact$log_Global_Sales_sum)

## 2c) Determine Skewness and Kurtosis
#A skewness value less then -1 or greater then 1 is highly skewed
#Value between 0.5 and -0.5 indicates the distribution is symmetrical
#Kurtosis greater than 3, then the dataset has a heavier tail then a
#normal distribution (more in the tails)
#Kurtosis less than 3, then the dataset has a lighter tail then a
#normal distribution (less in the tails)
# Skewness and Kurtosis.
install.packages("propagate")
library(propagate)

skewness(Sales_Impact$NA_Sales_sum)
kurtosis(Sales_Impact$NA_Sales_sum)

skewness(Sales_Impact$EU_Sales_sum)
kurtosis(Sales_Impact$EU_Sales_sum)

skewness(Sales_Impact$Global_Sales_sum)
kurtosis(Sales_Impact$Global_Sales_sum)

#Skewness and Kurtosis of log values
skewness(Sales_Impact$log_NA_Sales_sum)
kurtosis(Sales_Impact$log_NA_Sales_sum)

skewness(Sales_Impact$log_EU_Sales_sum)
kurtosis(Sales_Impact$log_EU_Sales_sum)

skewness(Sales_Impact$log_Global_Sales_sum)
kurtosis(Sales_Impact$log_Global_Sales_sum)



## 2d) Determine correlation
# Determine correlation.
cor(Sales_Impact$Global_Sales_sum, Sales_Impact$NA_Sales_sum)
cor(Sales_Impact$Global_Sales_sum, Sales_Impact$EU_Sales_sum)
cor(Sales_Impact$EU_Sales_sum, Sales_Impact$NA_Sales_sum)


#Determine the correlation of the log values
cor(Sales_Impact$log_Global_Sales_sum, Sales_Impact$log_NA_Sales_sum)
cor(Sales_Impact$log_Global_Sales_sum, Sales_Impact$log_EU_Sales_sum)
cor(Sales_Impact$log_EU_Sales_sum, Sales_Impact$log_NA_Sales_sum)


###############################################################################

# 3. Plot the data
# Create plots to gain insights into data.
# Plot correlation matrix
qqplot(data=Sales_Impact, mapping~aes(Global_Sales_sum, NA_Sales_sum))+
  geom_point(colos="red", alpha=0.5, size=3)+geom_smooth(method="lm")
install.packages("corrplot")
corrplot::corrplot(cor(Sales_Impact), method=c("color"))



###############################################################################

# 4. Observations and insights
# Your observations and insights here...
#The data wasn't particularly reliable by itself, but the log version of the
#data was much more reliable, and it's a simple function meaning not much of 
#the dat was changed.
#NA_Sales and EU_Sales have a positive correlation with Global_Sales, meaning
#they could potentially accurately predict one given the other 2.

###############################################################################

###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explor the data
# View data frame created in Week 5.
view(Sales_Impact)

# Determine a summary of the data frame.
summary(Sales_Impact)

###############################################################################
library(dplyr)
# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
Sales_Impact_lnreg_reg1<-select(Sales_Impact, NA_Sales_sum, Global_Sales_sum)
str(Sales_Impact_lnreg_reg1)
Sales_Impact_lnreg_reg1 %>% cor()

Sales_Impact_lnreg_reg2<-select(Sales_Impact, EU_Sales_sum, Global_Sales_sum)
str(Sales_Impact_lnreg_reg2)
Sales_Impact_lnreg_reg2 %>% cor()

Sales_Impact_lnreg_reg3<-select(Sales_Impact, NA_Sales_sum, EU_Sales_sum)
str(Sales_Impact_lnreg_reg3)
Sales_Impact_lnreg_reg3 %>% cor()

## 2b) Create a plot (simple linear regression)
# Basic visualisation.
plot(Sales_Impact_lnreg_reg1)


plot(Sales_Impact_lnreg_reg2)


plot(Sales_Impact_lnreg_reg3)






###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.
mlr_data <- subset(Sales_Impact, select =-c(Product,log_NA_Sales_sum, log_EU_Sales_sum,
                                            log_Global_Sales_sum))
colnames(mlr_data)
str(mlr_data)
mlr_data %>% col()

# Multiple linear regression model.
model_mlr  <- lm(Global_Sales_sum~., data=mlr_data)
model_mlr
summary(model_mlr)
plot(mlr_data)

###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.

#These values were written down to keep easier track 
#NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
#NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
#NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
#NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
#NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.

#Example A

NA_Sales_sum <- c(34.02)
EU_Sales_sum <- c(23.80)
predict_data <- data.frame( NA_Sales_sum, EU_Sales_sum)

#predict
predict(model_mlr, newdata = predict_data)
#Predicted value is 68.05655, which is close to the observed value of 67.85

# Compare with observed values for a number of records.
#Example B

NA_Sales_sum <- c(3.93)
EU_Sales_sum <- c(1.56)
predict_data <- data.frame( NA_Sales_sum, EU_Sales_sum)

#predict
predict(model_mlr, newdata = predict_data)
#predicted value is 7.35674 

#Example C

NA_Sales_sum <- c(2.73)
EU_Sales_sum <- c(0.65)
predict_data <- data.frame( NA_Sales_sum, EU_Sales_sum)

#predict
predict(model_mlr, newdata = predict_data)
#Predicted value of 4.908353,which is a little bit over the observed value of 
#4.32

#Example D

NA_Sales_sum <- c(2.26)
EU_Sales_sum <- c(0.97)
predict_data <- data.frame( NA_Sales_sum, EU_Sales_sum)

#predict
predict(model_mlr, newdata = predict_data)
#Predicted value of 4.761039, which is close to the observed value of 6.12,
#however the observed value had a different NA_Sales_sum, which was 4.42.



#Example E

NA_Sales_sum <- c(22.08)
EU_Sales_sum <- c(0.52)
predict_data <- data.frame( NA_Sales_sum, EU_Sales_sum)

#predict
predict(model_mlr, newdata = predict_data)
#Predicted value of 26.62556, which is above the observed value of 23.21



###############################################################################

# 5. Observations and insights
# Your observations and insights here...
#This shows that the predicted value will either be very close to the observed
#value or just above it, which means taking it into account leads to accurately 
#predicting the market

###############################################################################
###############################################################################




