library(caret)
library(pROC)
library(pacman)
p_load(pacman, tidyverse, rio, formattable)

customGreen = "#71CA97"
customBlue = "#009dff"
customRed = "#ff7f7f"
# Prep Training and Test data.
set.seed(100)
trainDataIndex <- createDataPartition(dataset$DEATH_EVENT, p=0.7, list = F)  # 70% training data
train <- dataset[trainDataIndex, ]
test <- dataset[-trainDataIndex, ]
table(train$DEATH_EVENT)

DEATH_EVENT=test$DEATH_EVENT
model=glm(DEATH_EVENT~.,data=train,family = binomial(link = "logit"))
glm.probs <- predict(model , newdata = test , type = "response")
glm.pred <- rep(0, nrow(test))
glm.pred[glm.probs > .5] <- 1
table(glm.pred , DEATH_EVENT)
t1=tibble(Pred=c("Predicted_N0","Predicted_YES"),Actual_NO=c(50,10),Actual_YES=c(6,23))
mean(glm.pred == DEATH_EVENT)

test_con_mat = confusionMatrix(factor(glm.pred) , factor(DEATH_EVENT))

test_roc = roc(test$DEATH_EVENT ~glm.probs, plot = TRUE, print.auc= TRUE)

as.numeric(test_roc$auc)
formattable(
  t1,
  align = c("l", "c","c"),
  list("Pred" = formatter("span", style = ~style(color = "blue", font.weight = "bold")),
       "Actual_NO" = color_tile(customRed, customGreen),
       "Actual_YES" = color_tile(customRed, customGreen)))

