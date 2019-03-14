# Loading Packages
library(ggplot2)
library(dplyr)
library(caret)
library(sqldf)
library(plotly)
library(tidyverse)
library(readxl)
library(lubridate)
library(glmnet)
library(caTools)
library(treemap)
library(lattice) # for pairplots
library(ggcorrplot) # for pairplots
library(randomForest)

# https://stat545.com/bit001_dplyr-cheatsheet.html
# dplyr join functions

# Loading the dataset
file_list <- list.files(pattern = "*.csv")
df_list <- lapply(file_list, read_csv)
df_list

df <- df_list[1]

# Finding which columns have missing values
colnames(df)[colSums(is.na(df)) > 0] 

# Imputing mean in place of missing values for the entire column
impute_mean <- function(x){
  ifelse(is.na(x),
         round(mean(x,na.rm = TRUE),0),
         x)
}

# Imputing mean in a range of columns
for(i in ncol(df)){
  df[is.na(df[,i]),i] <- mean(df[,i],na.rm = TRUE)
}

# Imputing mean using which function
df[which(is.na(df$column)),'column'] <- mean(df$column,na.rm = TRUE)

# Imputing mean values for NA for unique combination of different columns
df <- df %>%
  group_by(Hospital_ID,Region_ID) %>%
  mutate(Hospital_employees = if_else(is.na(Hospital_employees),
                                      mean(Hospital_employees,na.rm = TRUE),
                                      Hospital_employees))

# Removing missing values from rows
df <- df[!is.na(df$column),]

# Dropping columns 
df <- df[,-c(1,2,3,4)]

# Encoding Categorical Data
df$column <- factor(df$column,
                    levels = c("Yes","No"),
                    labels = c(1,0))

# Gathering data - Combine several columns into 1
gather(data, key="new column",value ="name value column", -column to keep as it is )
# Spread is the opposite of gather

# Separate - 1 column to multiple columns
separate(rate, into = c("cases","pop"), sep = "/")

# Exploratory Data Analysis

# To count no of observations per category
mpg %>% count(class, wt=cyl)

# dplyr - Select multiple columns
select(origin, dest, ends_with("delay"), ends_with("time")))

# To identify duplicates within a set of columns
iris %>% count(Species, Petal.Width ) %>% filter(n >1)

# Joining data frames
a %>% left_join(b, by = c('a','b'))

# Factor commands to reorder, sort factor ggplots
fct_infreq - # Reorder factors levels by first appearance or frequency
fct_rev - # Reverse order of above
# example - fct_rev(fct_infreq(x))
  
fct_reorder - # Reorder factor levels by sorting along another variable
fct_reorder2 - # Reorders the factor by the y values associated with the largest x value - ex. widowed, married
# example - fct_reorder(x,y) reorder x based on y
  
fct_relevel -# Takes a factor f and move any unwanted level at the end in plot
  
# fct_recode allows you to change factor levels to break or combine 3 into 1
# fct_collapse allows you to collapse levels into one
gss_cat %>% count(partyid)
gss_cat %>% mutate(partyid = fct_collapse(partyid,
                                        "Major" = c("Strong democrat","Not str democrat"),
                                        "Minor" = c("Ind,near dem"))) %>%
  count(partyid)

# Often proportions are interesting
# prop.table(table(x,y)) - by row(1) and by column(2)

## Date Manipulations
# There are 3 ways to create a date/time
# 1. From Strings, From individual date-time components, From an existing date/time object

class(ymd("2018-11-02"))
ymd_hms("2017-01-31 20:11:59", tz='EST')

# To create date/time from multiple columns , use make_date for dates and make_datetime for datetime
library(nycflights13)
flights %>% 
  select(year, month, day, hour, minute) %>% 
  mutate(departure = make_datetime(year, month, day, hour, minute))

# You can pull out individual parts of the date with the accessor functions
# year(), month(), mday() (day of the month), yday() (day of the year), wday() (day of the week), hour(),
# minute(), and second().

# There’s an interesting pattern if we look at the average departure delay by minute within the hour.
flights_dt %>% 
  mutate(minute = minute(dep_time)) %>% 
  group_by(minute) %>% 
  summarise(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n()) %>% 
  ggplot(aes(minute, avg_delay)) +
  geom_line()


## ggplot
# Bar plots - to show relation between categorical variables
# Normal - showing counts
ggplotly(ggplot(mtcars,aes(x=factor(cyl),fill=factor(am))) +
  geom_bar() +
  labs(x='cyl',y='Count',title='Am for cyl type') + scale_fill_manual(values = c('light blue','red')))

# Normal - Proportion
ggplot(mtcars,aes(x=factor(cyl),y=..prop..,group=factor(am),fill=factor(am))) +
  geom_bar() +
  labs(x='cyl',y='Count',title='Am for cyl type') + scale_fill_manual(values = c('light blue','red'))

# Stacked - to show proportion of same height - 
# makes easier to compare proportions across groups
ggplot(mtcars,aes(x=factor(cyl),fill=factor(am))) + geom_bar(position = 'fill') +
  labs(x='cyl',y='Prop',title='Am for cyl type') +  theme_bw()

# Dodged - to show separate bars and compare between categories
ggplot(mtcars,aes(x=factor(cyl),fill=factor(am))) + geom_bar(position = 'dodge')+
  scale_x_discrete('Cylinders') + scale_y_continuous('Number') + scale_fill_discrete(labels=c('Manual','Auto'))

# Bar plot - Brewed color palette
ggplot(mtcars,aes(x=factor(cyl),fill=factor(am))) +
  geom_bar() + scale_fill_brewer(palette = 'RdBu')

# Bar plot - Reverse
df %>% filter(target==1) %>%
  ggplot(., aes(x=fct_rev(fct_infreq(job)),fill=factor(job))) + geom_bar() + coord_flip()

# Treemap - 
treemap(a,index = "job",vSize = "count")


## Histograms - to explain distribution of a continous variable
ggplot(iris, aes(x=Sepal.Width)) +
  geom_histogram(binwidth = 0.1,fill='yellow') + theme_bw()

# Adding categories to histogram
ggplot(mtcars, aes(x=mpg,fill=factor(cyl))) +
  geom_histogram(binwidth = 1,position = 'identity',alpha=0.7)


## Scatter plots
# Adding layers to a scatter plot by creating a small df and adding it as 
# a second geom_point function
iris_summary <- iris %>% group_by(Species) %>% summarise(Sepal_Length = mean(Sepal.Length),
                                                         Sepal_Width = mean(Sepal.Width))

ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width,col=Species))+
  geom_point() +
  geom_point(data=iris_summary, aes(x=Sepal_Length,y=Sepal_Width),
             shape=20,size=8) + geom_vline(xintercept = 5)
  theme_bw()
  
# To deal with overfitting of points, use jitter and alpha
ggplot(iris, aes(Sepal.Length,y=Sepal.Width,col=Species)) +
  geom_jitter(alpha=0.7)

# you can also color a column by giving some condition ex. col = displ <5
# If a column is to be specified as a part of color or shape or size mention
# inside aes or normal color

# Facet - To create multiple plots
# Facet_wrap - to facet your plot by a single variable
# Facet_grid - to facet by combination of variables

ggplot(mpg,aes(x=displ,y=hwy,col=class)) +
  geom_point() + facet_wrap(~class,nrow=5)

# geom_smooth - to create regression line with standard error and adds smoothness
ggplot(mpg,aes(displ,hwy,col=drv)) +
 geom_point() +
 geom_smooth(data=mpg,mapping=aes(linetype=drv))


## Line plots
ggplot(beaver1,aes(x=time,y=temp,col=factor(activ))) +
  geom_line()

# Box plots - to show distribution of categorical vs continous for outliers
ggplot(diamonds,aes(carat,price)) +
  geom_boxplot(aes(group=cut_width(carat,0.25)),outlier.alpha = 0.3,col='red')

ggplot(df,aes(job,age,fill=job)) +
           geom_boxplot() + theme(axis.text.x = element_text(angle=45,hjust = 1)) 


## Correlation plot
M <- cor(mtcars)
corrplot::corrplot(M, method="color", type='upper', order='hclust')
install.packages('ggcorrplot')
library(ggcorrplot)
ggcorrplot(M, hc.order = TRUE, type = "upper", lab = TRUE)




## Modeling ### ---------------------- Modeling

# Splitting into train and test
split <- sample.split(swiss$Fertility,SplitRatio = 0.8)
train <- subset(swiss, split == TRUE)
nrow(train)
test <- subset(swiss, split==FALSE)
nrow(test)

# Multiple Regression
train.control <- trainControl(method = 'cv', number = 10)
model <- train(Fertility ~ ., data=train, method = 'lm',
               trControl = train.control)
summary(model)
par(mfrow=c(2,2))
plot(model$finalModel) # Study 4 plots
# Residuals vs Fitted 
# This plot shows if residuals have non-linear patterns. There could be a non-linear relationship between predictor 
# variables and an outcome variable and the pattern could show up in this plot. (parabola)

# Normal Q-Q (Quantile-Quantile)
# This plot shows if residuals are normally distributed.
# Graphical tool to help us assess if a set of data plausibly came from some theoretical distribution such as 
# Normal or exponential.

# Scale-location - Spread location plot
# This plot shows if residuals are spread equally along the ranges of predictors.
# To check equal variance. It's good to see a horizontal line with equally spread points.

# Residual vs Leverage
# to show influential and points having high effect.
# We watch for outlying values at the upper right corner or at the lower right corner.
# Those spots are the places where cases can be influential against a regression.
# Look for cases outside a dashed line

# Predictions
s.pred <- predict(model, newdata = test)
#check MSE
mean((s.pred-test$)^2)

# Visualizing linear modeling results
#Visualization Linear modeling
temp <- test[,c("Likelihood_Recommend_H",'Age_Range_H',"Guest_Room_H",
                'Tranquility_H','Condition_Hotel_H',"Customer_SVC_H","Staff_Cared_H")]
#View(temp)
tempMelt <- melt(temp, id='Likelihood_Recommend_H')
#View(tempMelt)

#Heat Map - Visualization 05
visualization_05 <- ggplot(tempMelt, aes(x=Likelihood_Recommend_H,y=variable, fill=value)) + geom_tile() + scale_fill_gradient(low="yellow", high = "orange")

#Scatter Plot Visualization 06
visualization_06 <- ggplot(swiss, aes(x=Catholic, y=Fertility, size=Infant.Mortality, col=Education
                                     ))+geom_jitter(width = 0.5, height = 1) + geom_abline()



# Multi-collinearity
vif(model)

## Ridge and Lasso Regression ###
x <- model.matrix(Fertility ~ ., swiss)[,-1]
y <- swiss$Fertility
lambda <- 10^seq(10,-2,length=100)

train <- sample(1:nrow(x), 0.8*nrow(x))
test <- (-train)
ytest <- y[test]

# Ridge
ridge.mod <- glmnet(x[train,], y[train], alpha = 0, lambda = lambda)
# Find best lambda from our list via cross-validation
cv.out <- cv.glmnet(x[train,], y[train], alpha = 0)

bestlam <- cv.out$lambda.min

#make predictions
ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test,])
s.pred <- predict(swisslm, newdata = swiss[test,])
#check MSE
mean((s.pred-ytest)^2)
mean((ridge.pred-ytest)^2)

# a look at the coefficients
out = glmnet(x[train,],y[train],alpha = 0)
predict(ridge.mod, type = "coefficients", s = bestlam)[1:6,]

# Visualization
fit.ridge=glmnet(x,y,alpha=0)
plot(fit.ridge,xvar="lambda",label=TRUE)

cv.ridge=cv.glmnet(x,y,alpha=0)
plot(cv.ridge)


## Lasso ###
lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = lambda)
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test,])
mean((lasso.pred-ytest)^2)

lasso_coef <- predict(lasso.mod, type = "coefficients", s = bestlam)[1:6,]
lasso_coef

# Visualization
fit.lasso=glmnet(x,y,alpha=1)
plot(fit.lasso,xvar="lambda",label=TRUE)
#The deviance shows the percentage of deviance explained, (equivalent to r squared in case of regression)

plot(fit.lasso,xvar="dev",label=TRUE)
# A lot of the r squared was explained for quite heavily shrunk coefficients.
# And towards the end, with a relatively small increase in r squared from between 0.4 and 0.5, 
# coefficients grow very large. This may be an indication that the end of the path is overfitting.


## Elastic net ###
glmnet_grid <- expand.grid(alpha = c(0,  .1,  .2, .4, .6, .8, 1),
                           lambda = 10^seq(10,-2,length=100))
glmnet_ctrl <- trainControl(method = "cv", number = 10)
glmnet_fit <- train(Fertility ~ ., data = swiss,
                    method = "glmnet",
                    preProcess = c("center", "scale"),
                    tuneGrid = glmnet_grid,
                    trControl = glmnet_ctrl)
glmnet_fit



## Logistic Regression ##
# Log odds of the outcome is modeled as a linear combination of the predictor variables
# The logistic regression coefficients give the change in the log odds of the outcome for 
# a one unit increase in the predictor variable
# If nominal categorical variable - leave as it is, no need of dummying
# If ordinal - create dummy - convert to numeric
df <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
df <- BreastCancer
str(df)
View(df)

# remove id column
df <- df[,-1]

# convert factors to numeric
for(i in 1:9) {
  df[, i] <- as.numeric(as.character(df[, i]))
}

df$Class <- ifelse(df$Class == "malignant", 1, 0)
df$Class <- factor(df$Class, levels = c(0, 1))

# Contingency table between categorical and predictor to make sure that there are not 0 cells
table(df$Class)

split <- sample.split(df$Class,SplitRatio = 0.8)
train <- subset(df, split == TRUE)
nrow(train)
test <- subset(df, split==FALSE)
nrow(test)


table(train$Class)
table(test$Class)

## But we can see that the classes are imbalanced, hence we use down-sampling to get equal ratio
#'%ni%' <- Negate('%in%')  # define 'not in' func

#down_train <- downSample(x = train[, colnames(train) %ni% "Class"],
                   #      y = train$Class)

#table(down_train$Class)
#table(test$Class)
# removing missing values from x's
train$Bare.nuclei <- ifelse(is.na(train$Bare.nuclei),median(train$Bare.nuclei,na.rm = TRUE),train$Bare.nuclei)

train.control <- trainControl(method = 'cv', number = 10)
logistic_classifier2 <- train(Class ~ ., data=train, method = 'glm',
                              family = 'binomial',
                              trControl = train.control)
summary(logistic_classifier2)
exp(coef(logistic_classifier2$finalModel))

# for a one unit increase in gpa, the odds of being admitted to graduate school 
# (versus not being admitted) increase by a factor of 1.89.

prob_pred <- predict(logistic_classifier2$finalModel, type='response', newdata = test)
y_pred <- ifelse(prob_pred > .5, 1, 0)
cm = table(test[,"Class"], y_pred)
cm
confusionMatrix(cm)

# ROC
library(ROCR)
pr <- prediction(prob_pred, test$Class)

prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, colorize=TRUE)
abline(a=0,b=1)

acc.perf = performance(pr, measure = "acc")
plot(acc.perf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


## Decision Trees ###
library(rpart)
library(rpart.plot)

trctrl <- trainControl(method = "cv", number = 10)
set.seed(3333)
fit <- rpart(Class~., data = train, method = 'class',parms = list(split='gini'))
rpart.plot(fit,box.palette = "RdBu",shadow.col = "gray",nn=TRUE)

predict_unseen <-predict(fit, test,type='class')
table_mat <- table(test$Class, predict_unseen)
table_mat
confusionMatrix(table_mat)
varImp(fit)



## Random Forest ##
library(caret)
library(e1071)
library(randomForest)

# Steps involveD:
# Evaluate the model with the default setting
set.seed(1234)
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")
rf_default <- train(Class~.,
                    data = train,
                    method = "rf",
                    metric = "Accuracy",
                    trControl = trControl,
                    importance=TRUE)

rf_default

random_forest_model <- randomForest(Class~., data=train, mtry=5,ntree=500,importance=TRUE)
pred <- predict(random_forest_model,test,type='class')
table(pred,test$Class)
varImpPlot(random_forest_model)


fit_rf <- train(Class~.,
                train,
                method = "rf",
                metric = "Accuracy",
                tuneGrid = tuneGrid,
                trControl = trControl,
                importance = TRUE,ntree=500)

# Evaluate the model on the test dataset
prediction <-predict(fit_rf, test)
table_mat <- table(test$Class, prediction)
confusionMatrix(t)
plot(fit_rf)
varImp(fit_rf)

# Kappa = (observed accuracy - expected accuracy)/(1 - expected accuracy)
# kappa statistic is a measure of how closely the instances classified by the machine learning classifier
# matched the data labeled as ground truth, controlling for the accuracy of a random classifier as
# measured by the expected accuracy.



## K-Means Clustering ##
library(cluster)
library(factoextra)
x <- df[,4:5] # take all numeric columns
x <- scale(x)

# Determining optimal value of k
# Elbow method
fviz_nbclust(df, kmeans, method = "wss")

# Average Silhouette
fviz_nbclust(df, kmeans, method = "silhouette")

final <- kmeans(df, 4, nstart = 25)
print(final)
fviz_cluster(final, data = df)



## PCA ####
library(e1071)
df <- scale(df)
pca = preProcess(x = train[-14], method = 'pca', pcaComp = 2)
train <- predict(pca,train)
test <- predict(pca,test)

pca <- prcomp(df[,2:7])
pca
plot(pca, type = "l")
summary(pca)
library(ggfortify)
pca.plot <- autoplot(pca, data = train, colour = 'Group')
pca.plot


## Do Apriori
# Support - How frequently the itemset appears in the dataset - count(x)/total
# Confidence - How frequently the entire rule is true - count(x,y)/count(x)
# Lift - support(x,y)/support(x)*support(y)

# For apriri we need all variables as factors
# Removing ID column
df <- df[,-1]
str(df)

# Converting age from int to nominal categorical by binning the data in groups
df$age <- cut(df$age, breaks = c(0,10,20,50,max(df$age)),labels=c("child", "teens", "adults", "old"))

# support=0.2, confidence=0.5
ruleset= apriori(df, parameter = list(support=0.03, confidence= 0.95),appearance = list(rhs ="Class=1",default="lhs"))
inspect(ruleset)
strong_rules2 =ruleset[quality(ruleset)$lift > 1.74]
inspect(strong_rules2)

# visualize the results
plot(strong_rules2)
plot(strong_rules2, method="paracoord",control=list(type='items'))
plot(strong_rules2, method="grouped", interactive=F)



## Data Exploration and Preparation
# 1. Variable Identification
# Identifying Predictors and Target variable. Data types of them and if conversion or factoring required

# 2. Univariate analysis / Exploratory Data Analysis
# Explore variables one by one. Helps in detecting outliers
# If continous, we need to understand the central tendency and spread of the variable.(Histogram and boxplot)
# Categorical - Frequency table and proportion table(Bar plots)

# 3. Bi-variate analysis - To find the relationship between 2 variables
# Continous and Continous - Scatter plot and Correlation table
# Categorical and Categorical - Table or Stacked bar plots

# 4. Missing values treatment
# Deletion if more than 50% or imputing mean or median as per spread
# KNN

# 5. Outlier treatment - Increases the error variance, they can decrease normality
# Histograms, scatter and box plots - 1.5IQR, above 5th and 95th percentile
# How to deal with outliers?
# Deleting or trimming, Transforming or binning, imputing mean or treat separately

# 6. Variable Transformation and Creation / Feature Engineering 
# Scaling to standardize 
# Transforming complex non-linear relationships to linear - Log distribution
# Symmetric preferred over skewed - Log for right skewed and exp or square/cube root for left skewed
# Binning of variables in buckets - For categorical variables
# Creating derived or dummy variables

# 7. Modeling - Train and Test
# 8. Model Comparison
# 9. Evaluation metrics
# 10. Inference and Results






