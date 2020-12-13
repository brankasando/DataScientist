###### random sample split ######
library("rstudioapi")                                 # Load rstudioapi package
setwd(dirname(getActiveDocumentContext()$path))       # Set working directory to source file location

install.packages("tidyverse")
library("readr")
library("tidyverse")


##### Splitting sample ######
set.seed(123)

#Chose random numbers from 1 to number of rows of gl
index <- sample(1:nrow(df2), round(nrow(df2) * 0.65))

train <- df2[index, ]
test  <- df2[-index, ]


glimpse(train) #6,495
glimpse(test) #3,498



# ------ create model ------
train_model<-glm(bad ~ sexMale + divorced_single + children + self_empl + rent_1 + debt_1 + overdue_1  + amount + education +age
                 , family=binomial
                 , data=train)

summary(train_model)

#self-employed not significat, but amount yes
train_model1<-glm(bad ~ divorced_single + children + rent_1 + debt_1 + overdue_1 + amount ,family=binomial
                  , data=train)
summary(train_model1)


#for prediction
install.packages("modelr")
library(modelr)

#add column with predictions

train_lr <- mutate(train, p = predict(train_model1, type = "response"))
mean(train_lr$p)
summary(train_lr$p)
table(train_lr$bad)

train_lr$p_class <-cut(train_lr$p, 5)


merge(aggregate(bad ~ p_class, data = train_lr,function(x) c(length = length(x), sum = sum(x)))
      ,aggregate(p ~ p_class, data = train_lr, mean))

############# Model performance tradeoffs #####################

#threshold
train_lr <- mutate(train_lr, bad_predicted = ifelse(p<=0.04,0,1))

# Calculate the model's accuracy
mean(train_lr$bad == train_lr$bad_predicted)#0.8089299
table(ifelse(train_lr$bad==1,'Default True','Default False'), train_lr$bad_predicted)
# Sensitivity 0.5416667 - TruePositive / (TruePositive + FalseNegative)
91/(77+91)

#Specificity 0.8160266 - TrueNegative / (FalsePositive + TrueNegative)
5163 /(5163+1164)


install.packages("ROCR")
library(ROCR)
ROCRpred = prediction(train_lr$p, train_lr$bad)
ROCRperf = performance(ROCRpred, "tpr", "fpr")

plot(ROCRperf)
# Add colors
plot(ROCRperf, colorize=TRUE)
# Add threshold labels
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0.01,0.09,by=0.01), text.adj=c(-0.2,1.7))


#new threshold
train_lr1 <- mutate(train_lr, bad_predicted = ifelse(p<=0.02,0,1))

# Calculate the model's accuracy
mean(train_lr1$bad == train_lr1$bad_predicted) #0.6242763
table(ifelse(train_lr1$bad==1,'Default True','Default False'), train_lr1$bad_predicted)
# Sensitivity 0.8154762 - TruePositive / (TruePositive + FalseNegative)
137/(31   +137)

#Specificity 0.595701 - TrueNegative / (FalsePositive + TrueNegative)
3769  /(3769  +2558)



######## test model ########
test_lr <- mutate(test, p = predict(train_model1, type = "response",newdata = test))
test_lr$p <- predict(train_model1, type = "response", newdata = test)
summary(test_lr$p)
summary(test_lr$p)

p_class_test <-cut(test_lr$p, 5)
merge(aggregate(bad ~ p_class_test, data = test_lr,function(x) c(length = length(x), sum = sum(x)))
      ,aggregate(p ~ p_class_test, data = test_lr, mean))


test_lr <- mutate(test_lr, bad_predicted = ifelse(p<=0.02,0,1))
# Calculate the model's accuracy
mean(test_lr$bad == test_lr$bad_predicted) #0.6006289
table(ifelse(test_lr$bad==1,'Default True','Default False'), test_lr$bad_predicted)

# Sensitivity 0.7931034 - TruePositive / (TruePositive + FalseNegative)
69/(18+69)

#Specificity 0.5957197 - TrueNegative / (FalsePositive + TrueNegative)
2032 /(2032 + 1379)


####################################################################33

########### Random forest ###########
install.packages("randomForest")
library(randomForest)

train_rf<-mutate(train, bad=as.factor(bad))
test_rf<-mutate(test, bad=as.factor(bad))

model_rf <- randomForest(bad ~ sexMale + divorced_single + children + self_empl + rent_1 + debt_1 + overdue_1  + amount + education +age,data=train_rf, importance = TRUE)
model_rf

train_rf<-mutate(train_rf, p1 = predict(model_rf, train_rf,type = "class"))

table(ifelse(train_rf$bad==1,'Default True','Default False'), train_rf$p1)
mean(train_rf$bad == train_rf$p1) #0.9935335

# Sensitivity - TruePositive / (TruePositive + FalseNegative)
126/(42 +126) #0.75

#Specificity  - TrueNegative / (FalsePositive + TrueNegative)
6327/(6327)


#rf for test data
test_rf<-mutate(test_rf, p1 = predict(model_rf, test_rf,type = "class"))
mean(test_rf$bad==test_rf$p1) #0.9751286
table(ifelse(test_rf$bad==1,'Default True','Default False'), test_rf$p1)
#0 - Sensitivity - TruePositive / (TruePositive + FalseNegative)
#1 - Specificity  - TrueNegative / (FalsePositive + TrueNegative)
