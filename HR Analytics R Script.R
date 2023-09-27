# Change directory before run

# Install packages
# Un-comment the below part to install the packages that we are using

# install.packages("tidyverse")
# install.packages("haven")
# install.packages("readxl")
# install.packages("readr")
# install.packages("gridExtra")
# install.packages("showtext")
# install.packages("reshape2")
# install.packages("caret")
# install.packages("randomForest")
# install.packages("rpart")
# install.packages("rpart.plot")

# Read packages
library(tidyverse)
library(haven)
library(readxl)
library(readr)
library(gridExtra)
library(showtext)
library(reshape2)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)

showtext_auto()

# Read the data
#data <- read_csv("/Users/zhakaeric-macairm1/Downloads/ICT515 Source Code/HR_Analytics.csv")
data <- read_csv("/Users/branata.kurniawan/Documents/personal/ICT515/HR_Analytics.csv")
glimpse(data)

# ------

# Cleaning the data 
data <- na.omit(data)

# ------

# Data exploratory:
# average age by attrition
f1 <- data %>% group_by(Attrition) %>% summarize('avgAge'=round(mean(Age),2)) %>% 
  ggplot(aes(Attrition,avgAge,fill=Attrition))+geom_col(position=position_dodge())+geom_text(aes(y=avgAge+2,label=avgAge),color='black',size=5)+
  theme(text=element_text(size=10))+labs(title='Average Age by Attrition',x='Attrition',y='Average Age')
grid.arrange(f1,nrow=1,ncol=1)

# attrition and education
f2 <- data %>% group_by(Attrition) %>% summarize('avgEducationLevel'=round(mean(Education),2)) %>% 
  ggplot(aes(Attrition,avgEducationLevel,fill=Attrition))+geom_col(position=position_dodge())+geom_text(aes(y=avgEducationLevel+2,label=avgEducationLevel),color='black',size=5)+
  theme(text=element_text(size=10))+labs(title='Average Education Level by Attrition',x='Attrition',y='Average Education Level')
grid.arrange(f2,nrow=1,ncol=1)

# hourly rate by attrition
f3 <- data %>% group_by(Attrition) %>% summarize('avg'=round(mean(HourlyRate),2)) %>% 
  ggplot(aes(Attrition,avg,fill=Attrition))+geom_col(position=position_dodge())+geom_text(aes(y=avg+5,label=avg),color='black',size=5)+
  theme(text=element_text(size=10))+labs(title='Average Hourly Rate by Attrition',x='Attrition',y='Average Hourly Rate')
grid.arrange(f3,nrow=1,ncol=1)

# attrition and job satisfaction
f41 <- data %>% group_by(Attrition) %>% summarize('avg'=round(mean(JobSatisfaction),2)) %>% 
  ggplot(aes(Attrition,avg,fill=Attrition))+geom_col(position=position_dodge())+geom_text(aes(y=avg+0.2,label=avg),color='black',size=5)+
  theme(text=element_text(size=10))+labs(title='Average Job Satisfaction by Attrition',x='Attrition',y='Average Job Satisfaction')
grid.arrange(f41,nrow=1,ncol=1)

# attrition and job involvement
f42 <- data %>% group_by(Attrition) %>% summarize('avg'=round(mean(JobInvolvement),2)) %>% 
  ggplot(aes(Attrition,avg,fill=Attrition))+geom_col(position=position_dodge())+geom_text(aes(y=avg+0.2, label=avg),color='black',size=5)+ylim(0, 3)+
  theme(text=element_text(size=10))+labs(title='Average Job Involvement by Attrition',x='Attrition',y='Average Job Involvement')
grid.arrange(f42,nrow=1,ncol=1)

# satisfaction and involvement
f43 <- data %>% group_by(JobInvolvement) %>% summarize('avg'=round(mean(JobSatisfaction),2)) %>% 
  ggplot(aes(JobInvolvement,avg,fill=JobInvolvement))+geom_col(position=position_dodge())+geom_text(aes(y=avg+0.2,label=avg),color='black',size=5)+
  theme(text=element_text(size=10))+labs(title='Average Job Satisfaction by Job Involvement',x='Job Involvement',y='Average Job Satisfaction')
grid.arrange(f43,nrow=1,ncol=1)

# attrition and number of company works
f5 <- data %>% group_by(NumCompaniesWorked,Attrition) %>% summarize(n=n()) %>%
  ggplot(aes(Attrition,n,fill=Attrition))+geom_col(position=position_dodge())+geom_text(aes(y=n+20,label=n),color='black',size=5)+
  theme(text=element_text(size=10))+labs(title='Attrition by Number of Companies Work',x='')+facet_grid(.~NumCompaniesWorked)
grid.arrange(f5,nrow=1,ncol=1)

# attrition and salary over number of company works
f6 <- data %>% group_by(NumCompaniesWorked,Attrition) %>% summarize(n=n(),'avg'=round(mean(MonthlyIncome),2)) %>% 
  ggplot(aes(Attrition,avg,fill=Attrition))+geom_col(position=position_dodge())+geom_text(aes(y=avg+20,label=avg),color='black',size=2)+
  theme(text=element_text(size=10))+labs(title='Average Monthly Income by Attrition and Number of Companies Worked',x='',y='Average Monthly Income')+facet_grid(.~NumCompaniesWorked)
grid.arrange(f6,nrow=1,ncol=1)

# percent salary hike by job role and attrition
f7 <- data %>% group_by(Attrition) %>% summarize('avg'=round(mean(PercentSalaryHike),2)) %>% 
  ggplot(aes(Attrition,avg,fill=Attrition))+geom_col(position=position_dodge())+geom_text(aes(y=avg+20,label=avg),color='black',size=5)+
  theme(text=element_text(size=10))+labs(title='Average Salary Hike by Attrition',x='Attrition',y='Average Salary Hike')
grid.arrange(f7,nrow=1,ncol=1)

# attrition by department
f8 <- data %>% group_by(Department,Attrition) %>% summarize(n=n()) %>%
  ggplot(aes(Attrition,n,fill=Attrition))+geom_col(position=position_dodge())+geom_text(aes(y=n+20,label=n),color='black',size=5)+
  theme(text=element_text(size=10))+labs(title='Attrition by Department',x='')+facet_grid(.~Department)
grid.arrange(f8,nrow=1,ncol=1)

# work life balance and attrition
f9 <- data %>% group_by(WorkLifeBalance,Attrition) %>% summarize(n=n()) %>%
  ggplot(aes(Attrition,n,fill=Attrition))+geom_col(position=position_dodge())+geom_text(aes(y=n+20,label=n),color='black',size=5)+
  theme(text=element_text(size=10))+labs(title='Attrition by Work Life Balance',x='')+facet_grid(.~WorkLifeBalance)
grid.arrange(f9,nrow=1,ncol=1)

# years since last promotion and attrition
f10 <- data %>% group_by(Attrition) %>% summarize('avg'=round(mean(YearsSinceLastPromotion),2)) %>% 
  ggplot(aes(Attrition,avg,fill=Attrition))+geom_col(position=position_dodge())+geom_text(aes(y=avg+20,label=avg),color='black',size=5)+
  theme(text=element_text(size=10))+labs(title='Average Year Since Last Promotion by Attrition',x='Attrition',y='Average Year Since Last Promotion')
grid.arrange(f10,nrow=1,ncol=1)

# years in current role and attrition
f11 <- data %>% group_by(Attrition) %>% summarize('avgYearsInCurrentRole'=round(mean(YearsInCurrentRole), 2)) %>% 
  ggplot(aes(Attrition,avgYearsInCurrentRole,fill=Attrition))+geom_col(position=position_dodge())+geom_text(aes(y=avgYearsInCurrentRole+0.2,label=avgYearsInCurrentRole),color='black',size=5)+
  theme(text=element_text(size=10))+labs(title='Average Year in Current Role by Attrition',x='Attrition',y='Average Year In Current Role')
grid.arrange(f11,nrow=1,ncol=1)

# year with current manager and attrition
f12 <- data %>% group_by(Attrition) %>% summarize('avgYearsWithCurrManager'=round(mean(YearsWithCurrManager), 2)) %>% 
  ggplot(aes(Attrition,avgYearsWithCurrManager,fill=Attrition))+geom_col(position=position_dodge())+geom_text(aes(y=avgYearsWithCurrManager+0.2,label=avgYearsWithCurrManager),color='black',size=5)+
  theme(text=element_text(size=10))+labs(title='Average Year with Current Manager by Attrition',x='Attrition',y='Average Year With Current Manager')
grid.arrange(f12,nrow=1,ncol=1)

# attrition by monthly income 
dataMedian <- summarise(group_by(data, Attrition), MD = median(MonthlyIncome))
f13 <- ggplot(data, aes(x = Attrition, y = MonthlyIncome, fill = Attrition)) +
  geom_boxplot() +
  geom_text(data = dataMedian, aes(Attrition, MD, label = MD), 
            position = position_dodge(width = 0.8), size = 4, vjust = -0.5) +
  labs(x = "Attrition", y = "Monthly Income", title = "Average Monthly Income by Attrition") +
  theme_minimal()
grid.arrange(f13,nrow=1,ncol=1)

# attrition by over time
f14 <- data %>% group_by(OverTime,Attrition) %>% summarize(n=n()) %>%
  ggplot(aes(Attrition,n,fill=Attrition))+geom_col(position=position_dodge())+geom_text(aes(y=n+40,label=n),color='black',size=5)+
  theme(text=element_text(size=10))+labs(title='Attrition by Over Time',x='Overtime', y='Attrition Count')+facet_grid(.~OverTime)
grid.arrange(f14,nrow=1,ncol=1)

# attrition by age 
dataMedian <- summarise(group_by(data, Attrition), MD = median(Age))
f15 <- ggplot(data, aes(x = Attrition, y = Age, fill = Attrition)) +
  geom_boxplot() +
  geom_text(data = dataMedian, aes(Attrition, MD, label = MD), 
            position = position_dodge(width = 0.8), size = 4, vjust = -0.5) +
  labs(x = "Attrition", y = "Age", title = "Average Age by Attrition") +
  theme_minimal()
grid.arrange(f15,nrow=1,ncol=1)

# attrition by total working years 
dataMedian <- summarise(group_by(data, Attrition), MD = median(TotalWorkingYears))
f16 <- ggplot(data, aes(x = Attrition, y = TotalWorkingYears, fill = Attrition)) +
  geom_boxplot() +
  geom_text(data = dataMedian, aes(Attrition, MD, label = MD), 
            position = position_dodge(width = 0.8), size = 4, vjust = -0.5) +
  labs(x = "Attrition", y = "Total Working Years", title = "Average Total Working Years by Attrition") +
  theme_minimal()
grid.arrange(f16,nrow=1,ncol=1)

# attrition by daily rate
dataMedian <- summarise(group_by(data, Attrition), MD = median(DailyRate))
f17 <- ggplot(data, aes(x = Attrition, y = DailyRate, fill = Attrition)) +
  geom_boxplot() +
  geom_text(data = dataMedian, aes(Attrition, MD, label = MD), 
            position = position_dodge(width = 0.8), size = 4, vjust = -0.5) +
  labs(x = "Attrition", y = "Daily Rate", title = "Average Daily Rate by Attrition") +
  theme_minimal()
grid.arrange(f17,nrow=1,ncol=1)

# ------

# Preprocess the data
# Transform the variable from chr data type to factor
data <- data %>% mutate_if(is.character,as.factor) 

# Attrition
levels(data$Attrition) <- seq(length(levels(data$Attrition)))
data$Attrition <- as.numeric(data$Attrition)

# BusinessTravel
levels(data$BusinessTravel) <- seq(length(levels(data$BusinessTravel)))
data$BusinessTravel <- as.numeric(data$BusinessTravel)

# Department
levels(data$Department) <- seq(length(levels(data$Department)))
data$Department <- as.numeric(data$Department)

# EducationField
levels(data$EducationField) <- seq(length(levels(data$EducationField)))
data$EducationField <- as.numeric(data$EducationField)

# Gender
levels(data$Gender) <- seq(length(levels(data$Gender)))
data$Gender <- as.numeric(data$Gender)

# JobRole
levels(data$JobRole) <- seq(length(levels(data$JobRole)))
data$JobRole <- as.numeric(data$JobRole)

#MaritalStatus
levels(data$MaritalStatus) <- seq(length(levels(data$MaritalStatus)))
data$MaritalStatus <- as.numeric(data$MaritalStatus)

#Over18
levels(data$Over18) <- seq(length(levels(data$Over18)))
data$Over18 <- as.numeric(data$Over18)

# OverTime
levels(data$OverTime) <- seq(length(levels(data$OverTime)))
data$OverTime <- as.numeric(data$OverTime)

glimpse(data)

# ------

# Calculate the variable correlation value with Attrition
correlation_table <- cor(data %>% select(Attrition, everything())) %>% as.data.frame()
correlation_table <- correlation_table[, 1, drop=FALSE] %>% arrange(-Attrition)

# Drop the column without correlation
data <- data[, -which(names(data) == "EmployeeCount")]
data <- data[, -which(names(data) == "Over18")]
data <- data[, -which(names(data) == "StandardHours")]

# ------

# Modeling: Logistic Regression, Decision Tree, and Random Forest
# Defining the training and test data
# Set the random seed for reproducibility
set.seed(100)

performance <- c("Accuracy", "95% CI", "Kappa", "F1", "MSE", "MAE", "Precision Rate", "Recall Rate")

index <- sample(nrow(data), nrow(data)*0.8)
trainData <- data[index,]
testData <- data[-index,]

# Logistic Regression
set.seed(1030)

# Train the model using N-Fold Cross Validation
control <- trainControl(method="cv", number=5)
lr <- train(Attrition~., data=trainData, method="glm", trControl=control)

# Test the model
lrPredResult <- predict(lr,newdata=testData)

lrPredResult <- ifelse(lrPredResult>1.5,2,1)
lrPredResult %>% head

lrPredResultDf <- confusionMatrix(factor(testData$Attrition),factor(lrPredResult))

ci <- paste('(', round(lrPredResultDf$overall["AccuracyLower"], 4), ", ", round(lrPredResultDf$overall["AccuracyUpper"], 4), ")")
mse <- mean((lrPredResult - testData$Attrition)^2)
mae <- mean(abs(lrPredResult - testData$Attrition))

lrPerformanceResult <- c(
  round(lrPredResultDf$overall["Accuracy"], 4), 
  ci, 
  round(lrPredResultDf$overall["Kappa"], 4), 
  round(lrPredResultDf$byClass["F1"], 4), 
  round(mse, 4),
  round(mae, 4), 
  round(lrPredResultDf$byClass["Precision"], 4), 
  round(lrPredResultDf$byClass["Recall"], 4)
  )

# ------

# Random Forest
set.seed(109)

# Random Forest - 500
# Train the model using N-Fold Cross Validation
control <- trainControl(method = "cv", number = 5)
rf <- train(Attrition~., data=trainData, method="rf", trControl=control, tuneLength=3, ntree=500, type='classification')

# Test the model
rfPredResult <- predict(rf,newdata=testData)
rfPredResult <- ifelse(rfPredResult>1.5,2,1)
rfPredResult %>% head

rfPredResultDf500 <- confusionMatrix(factor(testData$Attrition), factor(rfPredResult))

ci <- paste('(', round(rfPredResultDf500$overall["AccuracyLower"], 4), ", ", round(rfPredResultDf500$overall["AccuracyUpper"], 4), ")")
mse <- mean((rfPredResult - testData$Attrition)^2)
mae <- mean(abs(rfPredResult - testData$Attrition))

rfPerformanceResult500 <- c(
  round(rfPredResultDf500$overall["Accuracy"], 4), 
  ci, 
  round(rfPredResultDf500$overall["Kappa"], 4), 
  round(rfPredResultDf500$byClass["F1"], 4), 
  round(mse, 4),
  round(mae, 4), 
  round(rfPredResultDf500$byClass["Precision"], 4), 
  round(rfPredResultDf500$byClass["Recall"], 4)
  )

plot(varImp(rf, scale=F), main = "Important Variables: RF 500 5 Fold CV")

# ------

# Random Forest - 750
# Train the model using N-Fold Cross Validation
control <- trainControl(method = "cv", number = 5)
rf <- train(Attrition~., data=trainData, method="rf", trControl=control, tuneLength=3, ntree=750, type='classification')

# Test the model
rfPredResult <- predict(rf,newdata=testData)
rfPredResult <- ifelse(rfPredResult>1.5,2,1)
rfPredResult %>% head

rfPredResultDf750 <- confusionMatrix(factor(testData$Attrition), factor(rfPredResult))

ci <- paste('(', round(rfPredResultDf750$overall["AccuracyLower"], 4), ", ", round(rfPredResultDf750$overall["AccuracyUpper"], 4), ")")
mse <- mean((rfPredResult - testData$Attrition)^2)
mae <- mean(abs(rfPredResult - testData$Attrition))

rfPerformanceResult750 <- c(
  round(rfPredResultDf750$overall["Accuracy"], 4), 
  ci, 
  round(rfPredResultDf750$overall["Kappa"], 4), 
  round(rfPredResultDf750$byClass["F1"], 4), 
  round(mse, 4),
  round(mae, 4), 
  round(rfPredResultDf750$byClass["Precision"], 4), 
  round(rfPredResultDf750$byClass["Recall"], 4)
)

plot(varImp(rf, scale=F), main = "Important Variables: RF 750 5 Fold CV")
plot(varImp(rf, scale=F), top = 5, main = "Top 5 Important Variables: RF 750 5 Fold CV")

# ------

# Random Forest - 1000
# Train the model using N-Fold Cross Validation
set.seed(110)
control <- trainControl(method = "cv", number = 5)
rf <- train(Attrition~., data=trainData, method="rf", trControl=control, tuneLength=3, ntree=1000, type='classification')

rfPredResult <- predict(rf,newdata=testData)
rfPredResult <- ifelse(rfPredResult>1.5,2,1)
rfPredResult %>% head

rfPredResultDf1000 <- confusionMatrix(factor(testData$Attrition), factor(rfPredResult))

ci <- paste('(', round(rfPredResultDf1000$overall["AccuracyLower"], 4), ", ", round(rfPredResultDf1000$overall["AccuracyUpper"], 4), ")")
mse <- mean((rfPredResult - testData$Attrition)^2)
mae <- mean(abs(rfPredResult - testData$Attrition))

rfPerformanceResult1000 <- c(
  round(rfPredResultDf1000$overall["Accuracy"], 4), 
  ci, 
  round(rfPredResultDf1000$overall["Kappa"], 4), 
  round(rfPredResultDf1000$byClass["F1"], 4), 
  round(mse, 4),
  round(mae, 4), 
  round(rfPredResultDf1000$byClass["Precision"], 4), 
  round(rfPredResultDf1000$byClass["Recall"], 4)
)

plot(varImp(rf, scale=F), main = "Important Variables: RF 1000 5 Fold CV")

# Print final performance data frame
finalPerformance <- data.frame(
  Performance=performance,
  LR=lrPerformanceResult,
  RF500=rfPerformanceResult500,
  RF750=rfPerformanceResult750,
  RF100=rfPerformanceResult1000
)