getwd()
#setwd()
train <- read.csv(file.choose(), stringsAsFactors = F, na.strings = c("", "NA", " "))
test <- read.csv(file.choose(), stringsAsFactors = F, na.strings = c("", "NA", " "))
train
test
sample_submission <- read.csv(file.choose())
sample_submission

str(train)
str(train)
summary(train)
summary(train)

# Initial prediction model using gender("Sex")
table(train$Sex, train$Survived)
prop.table(table(train$Sex, train$Survived))

first_test <- test
first_test$Survived <- 0
first_test$Survived [test$Sex == "female"] <- 1
my_first_submission <- data.frame(PassengerId = test$PassengerId, Survived = first_test$Survived)
head(my_first_submission)
