#70/30 split of Anne's clean data frame. 

set.seed(101)
house_split <- initial_split(clean_train)
house_train <- training(house_split)
house_test <- testing(house_split)

library(rpart)
library(rpart.plot)
library(ggplot2)
library(DescTools)
library(dplyr)

ggplot(house_train, aes(x=LogSalePrice)) +
  geom_histogram(bins=50)

#Regression Tree 1
rtree1 <- rpart(LogSalePrice ~ ., data = house_train, method = "anova")
rpart.plot(rtree1, cex = 0.8)
printcp(rtree1)
#variables actually used: BsmtFinSF1, CostPerSF, GarageArea, GarageType
# LogGrLivArea, Neighborhood, OverallQual, TotalSF
rsq.rpart(rtree1)
print(rtree1)
rtree1_pruned <- prune(rtree1, cp = 0.01) #cp chosen from CP Table (cp associated with smallest xerror)
rpart.plot(rtree1_pruned, cex = 0.8)
printcp(rtree1_pruned)

pred_rtree1 <- rtree1 %>% 
  predict(newdata = house_test, type = "prob") #error b/c there are two Condition2 factors that are not in the train data

ggplot(house_train, aes(x = Condition2)) +
  geom_bar()

ggplot(house_test, aes(x = Condition2)) +
  geom_bar()

#Regression Tree 2: same as rtree1, but excluding Condition2

house_train1 = subset(house_train, select = -c(Condition2))

rtree2 <- rpart(LogSalePrice ~ ., data = house_train1, method = "anova")

pred_rtree2 <- rtree2 %>% 
  predict(newdata = house_test, type = "prob")
  