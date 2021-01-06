setwd("C:/Users/abheer/Desktop/Data science/Financial_Risk_Analytics/Project FRA")
library(readxl)
company <- read_excel("raw-data.xlsx")
names(company)
attach(company)
str(company)

company$`Creditors turnover` = as.numeric(company$`Creditors turnover`)
company$`Debtors turnover` = as.numeric(company$`Debtors turnover`)
company$`Finished goods turnover` = as.numeric(company$`Finished goods turnover`)
company$`WIP turnover` = as.numeric(company$`WIP turnover`)
company$`Raw material turnover` = as.numeric(company$`Raw material turnover`)
company$`Shares outstanding` = as.numeric(company$`Shares outstanding`)
company$`Equity face value` = as.numeric(company$`Equity face value`)
company$`PE on BSE` = as.numeric(company$`PE on BSE`)

summary(company)


# Creating a new variable return_on_equity

company$return_on_equity <- company$`Profit after tax` / company$`Net worth`


# Creating a new variable debt_ratio

company$Total_capital_employed <- company$`Total assets`/ company$`Current liabilities & provisions`
company$debt_ratio <- company$Borrowings / company$Total_capital_employed

# Exploratory Data Analysis (EDA)

library(tidyverse)
library(ggplot2)
library(caret)
library(caretEnsemble)
library(psych)
library(Amelia)
library(GGally)
library(rpart)

# MIssing value teartment 

# Treating missing values 
library(mice)
library(VIM)

# Displaying a graph to detect any missing data in the dataset
missmap(company)



df <- company[,-c(1,22)]


df1 <- kNN(df, 
                variable = c(
                             "Total income","Change in stock",
                             "Total expenses","Profit after tax",
                             "PBDITA","PBT","Cash profit",
                             "PBDITA as % of total income",
                             "PBT as % of total income",
                             "PAT as % of total income",
                             "Cash profit as % of total income",
                             "Sales","Income from financial services",
                             "Other income",
                             "Total capital","Reserves and funds",
                             "Borrowings","Current liabilities & provisions",
                             "Deferred tax liability",
                             "Cumulative retained profits",
                             "Contingent liabilities",
                             "Net fixed assets","Investments",
                             "Current assets",
                             "Net working capital","Quick ratio (times)",
                             "Current ratio (times)",
                             "Cash to current liabilities (times)",
                             "Cash to average cost of sales per day",
                             "Creditors turnover","Debtors turnover",
                             "Finished goods turnover","WIP turnover",
                             "Raw material turnover","Shares outstanding",
                             "Equity face value","PE on BSE",
                             "return_on_equity",
                             "Total_capital_employed",
                             "debt_ratio"
                             ),
                k = 6)


missmap(df1)

df1$`Creditors turnover` = as.numeric(df1$`Creditors turnover`)
df1$`Debtors turnover` = as.numeric(df1$`Debtors turnover`)
df1$`Finished goods turnover` = as.numeric(df1$`Finished goods turnover`)
df1$`WIP turnover` = as.numeric(df1$`WIP turnover`)
df1$`Raw material turnover` = as.numeric(df1$`Raw material turnover`)
df1$`Shares outstanding` = as.numeric(df1$`Shares outstanding`)
df1$`Equity face value` = as.numeric(df1$`Equity face value`)
df1$`PE on BSE` = as.numeric(df1$`PE on BSE`)

summary(df1)

# Outlier treatment 

boxplot(df1$`Total assets`, horizontal = T)
bench1 = 1098.7 + 1.5 * IQR(df1$`Total assets`)
bench1
df1$`Total assets`[df1$`Total assets` > bench1] <- bench1
boxplot(df1$`Total assets`, horizontal = T)

boxplot(df1$`Net worth`, horizontal = T)
bench2 = 377.3 + 1.5 * IQR(df1$`Net worth`)
bench2
df1$`Net worth`[df1$`Net worth` > bench2] <- bench2
boxplot(df1$`Net worth`, horizontal = T)

boxplot(df1$`Total income`, horizontal = T)
bench3 = 1440.9 + 1.5 * IQR(df1$`Total income`)
bench3
df1$`Total income`[df1$`Total income` > bench3] <- bench3
boxplot(df1$`Total income`, horizontal = T)

boxplot(df1$`Change in stock`, horizontal = T)
bench4 = 13.60 + 1.5 * IQR(df1$`Change in stock`)
bench4
df1$`Change in stock`[df1$`Change in stock` > bench4] <- bench4
boxplot(df1$`Change in stock`, horizontal = T)

boxplot(df1$`Total expenses`, horizontal = T)
bench5 = 1284.6 + 1.5 * IQR(df1$`Total expenses`)
bench5
df1$`Total expenses`[df1$`Total expenses` > bench5] <- bench5
boxplot(df1$`Total expenses`, horizontal = T)

boxplot(df1$`Profit after tax`, horizontal = T)
bench6 = 48.1 + 1.5 * IQR(df1$`Profit after tax`)
bench6
df1$`Profit after tax`[df1$`Profit after tax` > bench6] <- bench6
boxplot(df1$`Profit after tax`, horizontal = T)

boxplot(df1$PBDITA, horizontal = T)
bench7 = 139.1 + 1.5 * IQR(df1$PBDITA)
bench7
df1$PBDITA[df1$PBDITA > bench7] <- bench7
boxplot(df1$PBDITA, horizontal = T)

boxplot(df1$PBT, horizontal = T)
bench8 = 67.5 + 1.5 * IQR(df1$PBT)
bench8
df1$PBT[df1$PBT > bench8] <- bench8
boxplot(df1$PBT, horizontal = T)

boxplot(df1$`Cash profit`, horizontal = T)
bench9 = 86.8 + 1.5 * IQR(df1$`Cash profit`)
bench9
df1$`Cash profit`[df1$`Cash profit` > bench9] <- bench9
boxplot(df1$`Cash profit`, horizontal = T)

boxplot(df1$`PBDITA as % of total income`, horizontal = T)
bench10 = 16.3 + 1.5 * IQR(df1$`PBDITA as % of total income`)
bench10
df1$`PBDITA as % of total income`[df1$`PBDITA as % of total income` > bench10] <- bench10
boxplot(df1$`PBDITA as % of total income`, horizontal = T)

boxplot(df1$`PBT as % of total income`, horizontal = T)
bench11 = 8.65 + 1.5 * IQR(df1$`PBT as % of total income`)
bench11
df1$`PBT as % of total income`[df1$`PBT as % of total income` > bench11] <- bench11
boxplot(df1$`PBT as % of total income`, horizontal = T)

boxplot(df1$`PAT as % of total income`, horizontal = T)
bench12 = 6.27 + 1.5 * IQR(df1$`PAT as % of total income`)
bench12
df1$`PAT as % of total income`[df1$`PAT as % of total income` > bench12] <- bench12
boxplot(df1$`PAT as % of total income`, horizontal = T)

boxplot(df1$`Cash profit as % of total income`, horizontal = T)
bench13 = 10.5  + 1.5 * IQR(df1$`Cash profit as % of total income`)
bench13
df1$`Cash profit as % of total income`[df1$`Cash profit as % of total income` > bench13] <- bench13
boxplot(df1$`Cash profit as % of total income`, horizontal = T)

boxplot(df1$`PAT as % of net worth`, horizontal = T)
bench14 = 20.19  + 1.5 * IQR(df1$`PAT as % of net worth`)
bench14
df1$`PAT as % of net worth`[df1$`PAT as % of net worth` > bench14] <- bench14
boxplot(df1$`PAT as % of net worth`, horizontal = T)

boxplot(df1$Sales, horizontal = T)
bench15 = 1314.7  + 1.5 * IQR(df1$Sales)
bench15
df1$Sales[df1$Sales > bench15] <- bench15
boxplot(df1$Sales, horizontal = T)

boxplot(df$`Income from financial services`, horizontal = T)
bench16 = 5.80  + 1.5 * IQR(df1$`Income from financial services`)
bench16
df1$`Income from financial services`[df1$`Income from financial services` > bench16] <- bench16
boxplot(df1$`Income from financial services`, horizontal = T)

boxplot(df1$`Other income`, horizontal = T)
bench17 = 2.90  + 1.5 * IQR(df1$`Other income`)
bench17
df1$`Other income`[df1$`Other income` > bench17] <- bench17
boxplot(df1$`Other income`, horizontal = T)

boxplot(df1$`Total capital`, horizontal = T)
bench18 = 100.3  + 1.5 * IQR(df1$`Total capital`)
bench18
df1$`Total capital`[df1$`Total capital` > bench18] <- bench18
boxplot(df1$`Total capital`, horizontal = T)

boxplot(df1$`Reserves and funds`, horizontal = T)
bench19 = 263.2  + 1.5 * IQR(df1$`Reserves and funds`)
bench19
df1$`Reserves and funds`[df1$`Reserves and funds` > bench19] <- bench19
boxplot(df1$`Reserves and funds`, horizontal = T)

boxplot(df1$Borrowings, horizontal = T)
bench20 = 303.5  + 1.5 * IQR(df1$Borrowings)
bench20
df1$Borrowings[df1$Borrowings > bench20] <- bench20
boxplot(df1$Borrowings, horizontal = T)

boxplot(df1$`Current liabilities & provisions`, horizontal = T)
bench21 = 249.1  + 1.5 * IQR(df1$`Current liabilities & provisions`)
bench21
df1$`Current liabilities & provisions`[df1$`Current liabilities & provisions` > bench21] <- bench21
boxplot(df1$`Current liabilities & provisions`, horizontal = T)

boxplot(df1$`Deferred tax liability`, horizontal = T)
bench22 = 32.9  + 1.5 * IQR(df1$`Deferred tax liability`)
bench22
df1$`Deferred tax liability`[df1$`Deferred tax liability` > bench22] <- bench22
boxplot(df1$`Deferred tax liability`, horizontal = T)

boxplot(df1$`Shareholders funds`, horizontal = T)
bench23 = 393.2  + 1.5 * IQR(df1$`Shareholders funds`)
bench23
df1$`Shareholders funds`[df1$`Shareholders funds` > bench23] <- bench23
boxplot(df1$`Shareholders funds`, horizontal = T)

boxplot(df1$`Cumulative retained profits`, horizontal = T)
bench24 = 199.4  + 1.5 * IQR(df1$`Cumulative retained profits`)
bench24
df1$`Cumulative retained profits`[df1$`Cumulative retained profits` > bench24] <- bench24
boxplot(df1$`Cumulative retained profits`, horizontal = T)

boxplot(df1$`Capital employed`, horizontal = T)
bench25 = 767.3  + 1.5 * IQR(df1$`Capital employed`)
bench25
df1$`Capital employed`[df1$`Capital employed` > bench25] <- bench25
boxplot(df1$`Capital employed`, horizontal = T)

boxplot(df1$`TOL/TNW`, horizontal = T)
bench26 = 2.830  + 1.5 * IQR(df1$`TOL/TNW`)
bench26
df1$`TOL/TNW`[df1$`TOL/TNW` > bench26] <- bench26
boxplot(df1$`TOL/TNW`, horizontal = T)

boxplot(df1$`Total term liabilities / tangible net worth`, horizontal = T)
bench27 = 1  + 1.5 * IQR(df1$`Total term liabilities / tangible net worth`)
bench27
df1$`Total term liabilities / tangible net worth`[df1$`Total term liabilities / tangible net worth` > bench27] <- bench27
boxplot(df1$`Total term liabilities / tangible net worth`, horizontal = T)

boxplot(df1$`Contingent liabilities / Net worth (%)`, horizontal = T)
bench28 = 30.76  + 1.5 * IQR(df1$`Contingent liabilities / Net worth (%)`)
bench28
df1$`Contingent liabilities / Net worth (%)`[df1$`Contingent liabilities / Net worth (%)` > bench28] <- bench28
boxplot(df1$`Contingent liabilities / Net worth (%)`, horizontal = T)

boxplot(df1$`Contingent liabilities`, horizontal = T)
bench29 = 94  + 1.5 * IQR(df1$`Contingent liabilities`)
bench29
df1$`Contingent liabilities`[df1$`Contingent liabilities` > bench29] <- bench29
boxplot(df1$`Contingent liabilities`, horizontal = T)

boxplot(df1$`Net fixed assets`, horizontal = T)
bench30 = 328.8  + 1.5 * IQR(df1$`Net fixed assets`)
bench30
df1$`Net fixed assets`[df1$`Net fixed assets` > bench30] <- bench30
boxplot(df1$`Net fixed assets`, horizontal = T)

boxplot(df1$Investments, horizontal = T)
bench31 = 23.65  + 1.5 * IQR(df1$Investments)
bench31
df1$Investments[df1$Investments > bench31] <- bench31
boxplot(df1$Investments, horizontal = T)

boxplot(df1$`Current assets`, horizontal = T)
bench32 = 485.9  + 1.5 * IQR(df1$`Current assets`)
bench32
df1$`Current assets`[df1$`Current assets` > bench32] <- bench32
boxplot(df1$`Current assets`, horizontal = T)

boxplot(df1$`Net working capital`, horizontal = T)
bench33 = 81.6  + 1.5 * IQR(df1$`Net working capital`)
bench33
df1$`Net working capital`[df1$`Net working capital` > bench33] <- bench33
boxplot(df1$`Net working capital`, horizontal = T)

boxplot(df1$`Quick ratio (times)`, horizontal = T)
bench34 = 1.060  + 1.5 * IQR(df1$`Quick ratio (times)`)
bench34
df1$`Quick ratio (times)`[df1$`Quick ratio (times)` > bench34] <- bench34
boxplot(df1$`Quick ratio (times)`, horizontal = T)

boxplot(df1$`Current ratio (times)`, horizontal = T)
bench35 = 1.740  + 1.5 * IQR(df1$`Current ratio (times)`)
bench35
df1$`Current ratio (times)`[df1$`Current ratio (times)` > bench35] <- bench35
boxplot(df1$`Current ratio (times)`, horizontal = T)

boxplot(df1$`Debt to equity ratio (times)`, horizontal = T)
bench36 = 1.75  + 1.5 * IQR(df1$`Debt to equity ratio (times)`)
bench36
df1$`Debt to equity ratio (times)`[df1$`Debt to equity ratio (times)` > bench36] <- bench36
boxplot(df1$`Debt to equity ratio (times)`, horizontal = T)

boxplot(df1$`Cash to current liabilities (times)`, horizontal = T)
bench37 = 0.2  + 1.5 * IQR(df1$`Cash to current liabilities (times)`)
bench37
df1$`Cash to current liabilities (times)`[df1$`Cash to current liabilities (times)` > bench37] <- bench37
boxplot(df1$`Cash to current liabilities (times)`, horizontal = T)

boxplot(df1$`Cash to average cost of sales per day`, horizontal = T)
bench38 = 21.27  + 1.5 * IQR(df1$`Cash to average cost of sales per day`)
bench38
df1$`Cash to average cost of sales per day`[df1$`Cash to average cost of sales per day` > bench38] <- bench38
boxplot(df1$`Cash to average cost of sales per day`, horizontal = T)

boxplot(df1$`Creditors turnover`, horizontal = T)
bench39 =  10.81 + 1.5 * IQR(df1$`Creditors turnover`)
bench39
df1$`Creditors turnover`[df1$`Creditors turnover` > bench39] <- bench39
boxplot(df1$`Creditors turnover`, horizontal = T)

boxplot(df1$`Debtors turnover`, horizontal = T)
bench40 = 10.88  + 1.5 * IQR(df1$`Debtors turnover`)
bench40
df1$`Debtors turnover`[df1$`Debtors turnover` > bench40] <- bench40
boxplot(df1$`Debtors turnover`, horizontal = T)

boxplot(df1$`Finished goods turnover`, horizontal = T)
bench41 = 47.54  + 1.5 * IQR(df1$`Finished goods turnover`)
bench41
df1$`Finished goods turnover`[df1$`Finished goods turnover` > bench41] <- bench41
boxplot(df1$`Finished goods turnover`, horizontal = T)

boxplot(df1$`WIP turnover`, horizontal = T)
bench42 = 19.490  + 1.5 * IQR(df1$`WIP turnover`)
bench42
df1$`WIP turnover`[df1$`WIP turnover` > bench42] <- bench42
boxplot(df1$`WIP turnover`, horizontal = T)

boxplot(df1$`Raw material turnover`, horizontal = T)
bench44 = 11.21  + 1.5 * IQR(df1$`Raw material turnover`)
bench44
df1$`Raw material turnover`[df1$`Raw material turnover` > bench44] <- bench44
boxplot(df1$`Raw material turnover`, horizontal = T)

boxplot(df1$`Shares outstanding`, horizontal = T)
bench45 =  8.570e+06 + 1.5 * IQR(df1$`Shares outstanding`)
bench45
df1$`Shares outstanding`[df1$`Shares outstanding` > bench45] <- bench45
boxplot(df1$`Shares outstanding`, horizontal = T)

boxplot(df1$`Equity face value`, horizontal = T)
bench46 = 10  + 1.5 * IQR(df1$`Equity face value`)
bench46
df1$`Equity face value`[df1$`Equity face value` > bench46] <- bench46
boxplot(df1$`Equity face value`, horizontal = T)

boxplot(df1$EPS, horizontal = T)
bench47 = 9.6  + 1.5 * IQR(df1$EPS)
bench47
df1$EPS[df1$EPS > bench47] <- bench47
boxplot(df1$EPS, horizontal = T)

boxplot(df1$`Adjusted EPS`, horizontal = T)
bench48 = 7.5  + 1.5 * IQR(df1$`Adjusted EPS`)
bench48
df1$`Adjusted EPS`[df1$`Adjusted EPS` > bench48] <- bench48
boxplot(df1$`Adjusted EPS`, horizontal = T)

boxplot(df1$`Total liabilities`, horizontal = T)
bench49 = 1098.7  + 1.5 * IQR(df1$`Total liabilities`)
bench49
df1$`Total liabilities`[df1$`Total liabilities` > bench49] <- bench49
boxplot(df1$`Total liabilities`, horizontal = T)

boxplot(df1$`PE on BSE`, horizontal = T)
bench50 = 14.41  + 1.5 * IQR(df1$`PE on BSE`)
bench50
df1$`PE on BSE`[df1$`PE on BSE` > bench50] <- bench50
boxplot(df1$`PE on BSE`, horizontal = T)

boxplot(df1$return_on_equity, horizontal = T)
bench51 = 0.19  + 1.5 * IQR(df1$return_on_equity)
bench51
df1$return_on_equity[df1$return_on_equity > bench51] <- bench51
boxplot(df1$return_on_equity, horizontal = T)

boxplot(df1$Total_capital_employed, horizontal = T)
bench52 = 8.653  + 1.5 * IQR(df1$Total_capital_employed)
bench52
df1$Total_capital_employed[df1$Total_capital_employed > bench52] <- bench52
boxplot(df1$Total_capital_employed, horizontal = T)

boxplot(df1$debt_ratio, horizontal = T)
bench53 = 65.46  + 1.5 * IQR(df1$debt_ratio)
bench53
df1$debt_ratio[df1$debt_ratio > bench53] <- bench53
boxplot(df1$debt_ratio, horizontal = T)

# Treating multicollinearity 

library(faraway)

mymodel = lm(`Networth Next Year` ~`Total assets` + `Net worth`+
               `Total income` +  `Change in stock` +
               `Total expenses` + `Profit after tax` +
               `PBDITA` + `PBT` + `Cash profit` +
               `PBDITA as % of total income` +
               `PBT as % of total income` +
               `PAT as % of total income` +
               `Cash profit as % of total income` +
               `Sales` + `Income from financial services` +
               `Other income` +
               `Total capital` + `Reserves and funds` +
               `Borrowings` + `Current liabilities & provisions` +
               `Deferred tax liability` +
               `Shareholders funds`+
               `Cumulative retained profits` +
               `Capital employed`+
               `TOL/TNW`+
               `Total term liabilities / tangible net worth`+
               `Contingent liabilities / Net worth (%)`+
               `Contingent liabilities` +
               `Net fixed assets` + `Investments` +
               `Current assets` +
               `Net working capital` + `Quick ratio (times)` +
               `Current ratio (times)` +
               `Debt to equity ratio (times)`+
               `Cash to current liabilities (times)` +
               `Cash to average cost of sales per day` +
               `Creditors turnover` + `Debtors turnover` +
               `Finished goods turnover` + `WIP turnover` +
               `Raw material turnover` + `Shares outstanding` +
               `Equity face value` +`EPS`+`Adjusted EPS` + 
               `Total liabilities`+
               `PE on BSE` +
               `return_on_equity` +
               `Total_capital_employed` +
               `debt_ratio`, data = df1)
summary(mymodel)

vif(mymodel)

summary(df1)

df2 <- df1[,-c(54:93)]
names(df2)

# Dropping variables with high vif values 

# Dropping variables with vif value higher than 8

# Dropping Profit after tax, PBT, Reserves and funds, capital employed
# and Total liablities 
df3 <- df2[,-c(7,9,20,26,49)]

# Again running multicollinearity test after dropping variables 

mymodel2 = lm(`Networth Next Year`~.,data = df3)

summary(mymodel2)

vif(mymodel2)

# Dropping variables with vif value higher than 4

# Dropping total assets, Total capital, Deferred tax liability, 
# Contingent liabilities, Quick ratio (times), 
# Current ratio (times), Cash to current liabilities (times), 
# Shares outstanding and Equity face value, 
names(df3)
df4 <- df3[,-c(2,17,20,26,31,32,34,41,42)]

# Again running multicollinearity test after dropping variables 

mymodel3 = lm(`Networth Next Year`~.,data = df4)

summary(mymodel3)

vif(mymodel3)

# Dropping variables with vif value higher than 4

# Dropping net fixed assets, 

names(df4)
df5 <- df4[,-c(23)]

# Again running multicollinearity test after dropping variables 

mymodel4 = lm(`Networth Next Year`~.,data = df5)

summary(mymodel4)

vif(mymodel4)

# There is no more multicollinearity withing the remaining variables 

names(df5)

# Univariate analysis 
attach(df5)
hist(`Net worth`, col = "Red")
hist(`Total income`, col = "Blue")
hist(`Change in stock`, col = "Yellow")
hist(`Total expenses`, col = "Orange")

hist(PBDITA, col = "Red")
hist(`Cash profit`, col = "Blue")
hist(`PBDITA as % of total income`, col = "Yellow")
hist(`PBT as % of total income`, col = "Orange")

hist(`PAT as % of total income`, col = "Red")
hist(`Cash profit as % of total income`, col = "Blue")
hist(`PAT as % of net worth`, col = "Yellow")
hist(Sales, col = "Orange")

hist(`Income from financial services`, col = "Red")
hist(`Other income`, col = "Blue")
hist(Borrowings, col = "Yellow")
hist(`Current liabilities & provisions`, col = "Orange")

hist(`Shareholders funds`, col = "Red")
hist(`Cumulative retained profits`, col = "Blue")
hist(`TOL/TNW`, col = "Yellow")
hist(`Total term liabilities / tangible net worth`, col = "Orange")

hist(`Contingent liabilities / Net worth (%)`, col = "Red")
hist(Investments, col = "Blue")
hist(`Current assets`, col = "Yellow")
hist(`Net working capital`, col = "Orange")

hist(`Debt to equity ratio (times)`, col = "Red")
hist(`Cash to average cost of sales per day`, col = "Blue")
hist(`Creditors turnover`, col = "Yellow")
hist(`Debtors turnover`, col = "Orange")

hist(`Finished goods turnover`, col = "Red")
hist(`WIP turnover`, col = "Blue")
hist(`Raw material turnover`, col = "Yellow")
hist(EPS, col = "Orange")

hist(`Adjusted EPS`, col = "Red")
hist(`PE on BSE`, col = "Blue")
hist(return_on_equity, col = "Yellow")
hist(Total_capital_employed, col = "Orange")
hist(debt_ratio, col = "Green")

# bi-variate analysis 

plot(Sales~PBDITA, data = df5)
plot(Sales~`Cash profit`, data = df5)
plot(Sales~`Cumulative retained profits`, data = df5)
plot(Sales~`Total expenses`, data = df5)




Default <- ifelse(df5$`Networth Next Year`>0,0,1)

summary(as.factor(Default))

243/(243+3298)

# This is an unbalances dataset as the number of default scenerios are 
# only 6.8%

# Dropping the Networth Next Year column 
df6 <- df5[,-c(1)]

names(df6)
default_dataset <- cbind(df6,Default)
names(default_dataset)

default_dataset$Default = as.factor(default_dataset$Default)

# Using the smote function to make the dataset evenly distrubuted 
library(DMwR)
Default.smote<- SMOTE(Default ~ ., default_dataset, 
                     perc.over = 700, perc.under=150)

table(Default.smote$Default)

1944/(2551+1944)


# 43% of the dataset consists of default scenerios

# Implementing logistic regression to develop a model 

Default.smote$Default = as.factor(Default.smote$Default)
default_logistic1 <- glm(Default~., data=Default.smote, 
                    family=binomial)

summary(default_logistic1)

# Building a 2nd iteration of the model comprising only of the 
# significant variables 

names(Default.smote)

# Dropping the non significant variables such as
# Net Worth, total expenses, PBDITA,
# PAT as % of total income, 
# PBT as % of total income, Income from financial services,
# Other income, Borrowings, Current liabilities & provisions,
# Shareholders funds, Contingent liabilities / Net worth (%),
# Investments, Current assets, Net working capital,
# Debtors turnover, Finished goods turnover, WIP turnover,
# EPS, Adjusted EPS, Total_capital_employed

Default.smote2 <- Default.smote[,-c(1,4,5,8,9,13:17,21:24,28:30,32,33,35:37)]

# Implementing logistic regression to develop a model 

Default.smote2$Default = as.factor(Default.smote2$Default)
default_logistic2 <- glm(Default~., data=Default.smote2, 
                         family=binomial)

# Using k-Fold to get the best suited method 

library(MASS)

k <- 10
folds <- cut(seq(1, nrow(Default.smote2)), breaks = k, labels = FALSE)
head(folds)
set.seed(1)
cv.qda <- sapply(1:k, FUN = function(i) {
  testID <- which(folds == 1, arr.ind = TRUE)
  test <- Default.smote2[testID, ]
  train <- Default.smote2[-testID, ]
  qdadf <- qda(Default ~ ., data = train)
  qda_pred <- predict(qdadf, test)
  cv_est_qda <- mean(qda_pred$class != test$Default)
  return(cv_est_qda)
}
)
cv.qda
mean(cv.qda)

summary(default_logistic2)

plot(as.factor(default_logistic2$y), default_logistic2$fitted.values)

# As shown in the model the boxplot has a very high distinctive and 
# predictive power as the boxplots differ in a larger manner 

validation_dataset <- read_excel("validation_data.xlsx")
names(Default.smote2)
names(validation_dataset)

validation_dataset_final <- validation_dataset[,c(5,6,11,12,15:17,27,29,30,39,41,42,46,52)]
names(validation_dataset_final)

validation_dataset_final2 <- cbind(validation_dataset_final,validation_dataset$`Default - 1`)
names(validation_dataset_final2)

names(validation_dataset_final2)[16] <- "Default"

validation_dataset$`Creditors turnover` = as.numeric(validation_dataset$`Creditors turnover`)
validation_dataset$`Raw material turnover` = as.numeric(validation_dataset$`Raw material turnover`)
validation_dataset$`PE on BSE` = as.numeric(validation_dataset$`PE on BSE`)

res <- predict(default_logistic2,validation_dataset,
               type = "response")

table(validation_dataset$`Default - 1`,res>0.5)

# Accuracy of the model 
(229+11)/(229+11+16+1)

# The accuracy is 0.92 as in 93.3%

library(ROCR)

ROCRPred <- prediction(res,validation_dataset$`Default - 1`)
ROCRPref <- performance(ROCRPred,"tpr","fpr")

plot(ROCRPref, colorize = TRUE, print.cutoffs.at=seq(0.1,by=0.1))

table(validation_dataset$`Default - 1`,res>0.3)

# With the implementation of ROC curve 
# Accuracy of the model is 8.8%
(219+12)/(219+12+26+0)

# Sensitiviy of the model is 89.3%
219/(219+26)

# Specificty of the model is 100%
12/(12+0)

# As it is bank default we have alloted high preference to increase 
# specificity as the defaults should be reduced 