train <- read.csv(file.choose(), stringsAsFactors = F, na.strings = c("", "NA", " "))
test <- read.csv(file.choose(), stringsAsFactors = F, na.strings = c("", "NA", " "))
train
test
sample_submission <- read.csv(file.choose())
sample_submission

str(train)
str(test)
summary(train)
summary(test)

# Initial prediction model using gender("Sex")
table(train$Sex, train$Survived)
prop.table(table(train$Sex, train$Survived))

first_test <- test
first_test$Survived <- 0
first_test$Survived [test$Sex == "female"] <- 1
my_first_solution <- data.frame(PassengerId = test$PassengerId, Survived = first_test$Survived)
head(my_first_solution)
tail(my_first_solution)

write.csv(my_first_solution, file = "my_first_model.csv", row.names = FALSE)

# Another prediction model: Lazy Predictor
table(train$Survived)
prop.table(table(train$Survived))

second_test <- test
second_test$Survived <- 0

my_second_solution <- data.frame(PassengerId = test$PassengerId, Survived = second_test$Survived)
nrow(my_second_solution)
head(my_second_solution)
tail(my_second_solution)

write.csv(my_second_solution, file = "my_second_model.csv", row.names = FALSE)

# Preparing data after combining train and test
# return test
test <- read.csv(file.choose(), stringsAsFactors = F, na.strings = c("", "NA", " "))
test
new_test <- cbind(my_second_solution, test[-1])
new_test
complete_titanic <- rbind(train, new_test)
complete_titanic
head(complete_titanic)
tail(complete_titanic)
str(complete_titanic)
summary(complete_titanic)

# Survival rate
table(complete_titanic$Survived)
# Survival rate in proportion
prop.table(table(complete_titanic$Survived))

# Change Passenger class to factor
complete_titanic$Pclass <- as.factor(complete_titanic$Pclass)
str(complete_titanic)

# NA values treatment
sapply(complete_titanic, function(df)
  {
  sum(is.na(df)==T)/length(df)
})

# Using Amelia Package
#install.packages("Amelia")
library(Amelia)
missmap(complete_titanic, main = "Missing Map")

# Treating Age
complete_titanic$Age[is.na(complete_titanic$Age)] <- mean(complete_titanic$Age, na.rm = T)
sum(is.na(complete_titanic$Age))
#Treating Embarked
table(complete_titanic$Embarked, useNA = "always")
complete_titanic$Embarked[is.na(complete_titanic$Embarked)] <- "S"
sum(is.na(complete_titanic$Embarked))
table(complete_titanic$Embarked, useNA = "always")
# Treating Fare
complete_titanic$Fare[is.na(complete_titanic$Fare)] <- mean(complete_titanic$Fare, na.rm = T)
sum(is.na(complete_titanic$Fare))
# Drop Cabin
complete_titanic <- complete_titanic[-11]
complete_titanic

# Run below to see if there are other missing values
sapply(complete_titanic, function(df)
{
  sum(is.na(df)==T)/length(df)
})
missmap(complete_titanic, main = "Missing Map")

# Now split the cleaned data
train_cleaned <- complete_titanic[1:891,]
test_cleaned <- complete_titanic[892:1309,]

# Exploration
# Univariate
library(ggplot2)
xtabs(~Survived, train_cleaned)
# Categorical
ggplot(train_cleaned) + geom_bar(aes(x=Survived))
ggplot(train_cleaned) + geom_bar(aes(x=Sex))
ggplot(train_cleaned) + geom_bar(aes(x=Pclass))
# Numerical
ggplot(train_cleaned) + geom_histogram(aes(x=Fare), fill="white", colour="black")
ggplot(train_cleaned) + geom_boxplot(aes(x=factor(0), y=Fare)) + coord_flip()

ggplot(train_cleaned) + geom_histogram(aes(x=Age), fill="white", colour="black")
ggplot(train_cleaned) + geom_boxplot(aes(x=factor(0), y=Age)) + coord_flip()

# Bivariate

# categorical and categorical
xtabs(~Survived+Sex, train_cleaned)
ggplot(train_cleaned) + geom_bar(aes(x=Sex, fill=factor(Survived)))

xtabs(~Survived+Pclass, train_cleaned)
ggplot(train_cleaned) + geom_bar(aes(x=Pclass, fill=factor(Survived)))

xtabs(~Survived+Embarked, train_cleaned)
ggplot(train_cleaned) + geom_bar(aes(x=Embarked, fill=factor(Survived)))

# Numerical and Categorical
ggplot(train_cleaned) + geom_boxplot(aes(x=factor(Survived), y=Age))
ggplot(train_cleaned) + geom_histogram(aes(x=Age), fill="white", colour="black", bins = 50)

ggplot(train_cleaned) + geom_boxplot(aes(x=factor(Survived), y=Fare))
ggplot(train_cleaned) + geom_histogram(aes(x=Fare), fill="white", colour="black", bins = 50)

# Feature Engineering
# Create minor variable
complete_titanic$minor <- NA
complete_titanic$minor[complete_titanic$Age < 18] <- 1
complete_titanic$minor[complete_titanic$Age >= 18] <- 0
str(complete_titanic$minor)
ggplot(complete_titanic) + geom_bar(aes(x=minor))
# Create Title variable
complete_titanic$Title <- sapply(complete_titanic$Name, FUN=function(x) {strsplit(x, split = '[,.]')[[1]][2]})
complete_titanic$Title <- sub(' ', '', complete_titanic$Title)  # This removes the white spaces
table(complete_titanic$Title)
ggplot(complete_titanic) + geom_bar(aes(x=Title))
# Create Family Size
complete_titanic$Family <- complete_titanic$SibSp + complete_titanic$Parch + 1
table(complete_titanic$Family)
ggplot(complete_titanic) + geom_bar(aes(x=Family))

View(complete_titanic)

# Split data again
train_F <- complete_titanic[1:891,]
test_F <- complete_titanic[891:1309,]
train_F$Survived <- as.factor(train_F$Survived)
train_F$Embarked <- as.factor(train_F$Embarked)
train_F$Sex <- as.factor(train_F$Sex)
test_F$Embarked <- as.factor(test_F$Embarked)
test_F$Sex <- as.factor(test_F$Sex)

# Building Model
#1. Logistic Regression:
library(caTools)
set.seed(150)
split <- sample.split(train_F,SplitRatio = 0.75)
split

train.data <- subset(train_F, split=="TRUE")
test.data <- subset(train_F, split=="FALSE")
str(train.data)
str(test.data)

logit1 <- glm(Survived ~.,family = binomial(link = 'logit'), data = train.data[-c(1,4,9,12,13,14)])
summary(logit1) # AIC = 563.17 without feature engineering

logit2 <- glm(Survived ~.,family = binomial(link = 'logit'), data = train.data[-c(1,4,9,13)])
summary(logit2) # AIC = 546.26 with feature engineering # After dropping 13, AIC = 558.06

# Using anova function to see if logit1 and logit2 are the same
anova(logit1, logit2, test = "Chisq")
# Use model 2 that is significantly different from logit1
anova(logit2, test = "Chisq")

# Predict with logit2
fitted.results <- predict(logit2, newdata = test.data, type = 'response')
# The output from above, requires that we drop column 13

fitted.results <- ifelse(fitted.results > 0.5, 1.0)
#Evaluate model:
library(caret)
confusionMatrix(table(test.data$Survived, fitted.results))
# ROC-AUC curve:
# install.packages("ROCR")
library(ROCR)
ROCRPred <- prediction(fitted.results, test.data$Survived)
ROCRPerf <- performance(ROCRPred, measure = "tpr", x.measure = "fpr")
par(mfrow=c(1,1))
plot(ROCRPerf, colorize=TRUE, print.cutoffs.at = seq(0.1, by=0.1), main="ROC CURVE")
abline(a=0, b=1)

auc <- performance(ROCRPred, measure = "auc")
auc <- auc@y.values[[1]]
legend(.6,.4,auc,title = "AUC", cex=1)

my_prediction <- predict(logit2, test_F, type = "response")
my_prediction <- ifelse(my_prediction >0.5,1.0)
my_third_solution <- data.frame(PassengerId = test_F$PassengerId, Survived = my_prediction)

nrow(my_third_solution)

str(my_third_solution)

write.csv(my_third_solution, file = "My_Third_Solution.csv", row.names=FALSE)
