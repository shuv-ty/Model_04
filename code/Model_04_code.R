library(readxl)
credit_card <- read_excel("data/credit_card.xls")
View(credit_card)

#Import all important library

library(tidyverse)
library(dplyr)
library(caTools)

View(credit_card)
dim(credit_card)
str(credit_card)

#change the target variable into factor variable

credit_card$default <- as.factor(credit_card$default)

#Run GLM for default variable using all independent variable

model_check_01 <- glm(default ~ . , data = credit_card, family = binomial)
summary(model_check_01)

#drop those variable which are insignificant

credit_card <- credit_card %>% 
  select(-ID, -SEX, -EDUCATION, -AGE, -MARRIAGE, 
         -PAY_3, -PAY_6, -PAY_5, -PAY_6,
         -BILL_AMT1, -BILL_AMT2, -BILL_AMT3, -BILL_AMT4, -BILL_AMT5, BILL_AMT6,
         -PAY_AMT3, -PAY_AMT4, -PAY_AMT5, -PAY_AMT6)

view(credit_card)

#Run GLM for default variable using all independent variable (2ND TIME)

model_check_02 <- glm(default ~ . , data = credit_card, family = binomial)
summary(model_check_02)

#Now we will do A/B Testing


split <- sample.split(credit_card$default, SplitRatio = 0.8)

train <- subset(credit_card, split == TRUE)
test <- subset(credit_card, split == FALSE)

#Now train the GLM model in train data set

model <- glm(default ~. , data = train, family = binomial)
summary(model)

#Now Predict "default" based on model trained

prob <- predict(model, newdata = test, type = "response")

#set threshold value 
pred_class <- ifelse(prob > 0.6, 1, 0)

actual <- test$default
predicted <- pred_class

table(actual , predicted)

#check all the important evaluated data

library(caret)
confusionMatrix(table, positive = "1")

##########################
#accuracy is     [81%]       which is good, that can be improve after
#including other variables 


#plotting matrix

barplot(table,
        beside = TRUE,
        col = c("skyblue", "orange"),
        legend = TRUE,
        xlab = "Predicted",
        ylab = "Count",
        main = "Actual vs Predicted (Classification)")

















