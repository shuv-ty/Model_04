#Import all important library

library(tidyverse)
library(dplyr)
library(caTools)

View(credit_card)
dim(credit_card)
str(credit_card)

#change the target variablbe into factor variable

credit_card$default <- as.factor(credit_card$default)

#Run GLM for default variable using all independent variable

model_check <- glm(default ~ . , data = credit_card, family = binomial)
summary(model_check)

#drop those variable which are insignificant

credit_card <- credit_card %>% 
  select(-PAY_3, -PAY_6, -PAY_5, -PAY_6,
         -BILL_AMT1, -BILL_AMT2, -BILL_AMT3, -BILL_AMT4, -BILL_AMT5, BILL_AMT6,
         -PAY_AMT3, -PAY_AMT4, -PAY_AMT5, -PAY_AMT6)

credit_card <- credit_card %>% 
  select(-SEX, -EDUCATION, -MARRIAGE)

view(credit_card)

#Run GLM for default variable using all independent variable (2ND TIME)

model_check_02 <- glm(default ~ . , data = credit_card, family = binomial)
summary(model_check_02)

#Now we will do A/B Testing


split <- sample.split(credit_card$default, SplitRatio = 0.8)

train <- subset(credit_card, split == TRUE)
test <- subset(credit_card, split == FALSE)

#Now train the GLM model in train dataset



model <- glm(default ~. , data = train, family = binomial)
summary(model)

plot(model)

prob <- predict(model, newdata = test, type = "response")

pred_class <- ifelse(prob > 0.65, 1, 0)

actual <- test$default
predicted <- pred_class

table(actual , predicted)

library(caret)
confusionMatrix(table, positive = "1")

barplot(table,
        beside = TRUE,
        col = c("skyblue", "orange"),
        legend = TRUE,
        xlab = "Predicted",
        ylab = "Count",
        main = "Actual vs Predicted (Classification)")





