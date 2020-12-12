library("rstudioapi")                                 # Load rstudioapi package
setwd(dirname(getActiveDocumentContext()$path))       # Set working directory to source file location

install.packages("tidyverse")
library("readr")
library("tidyverse")

# ------ import app ------
app<-read_csv("app.csv",col_types = cols (col_integer(), col_integer(), col_date(), col_double(), col_integer(), col_double()
                                          ,col_date(), col_character(), col_character(), col_integer(),col_logical(), col_character(), col_character(), col_double()))

glimpse(app)

#get duplicated id, if any
dupl_id<-app$id[duplicated(app$id)]
dupl_id
# -> 436 id which are duplicated, but we don't know how many times, nor if other columns are duplicated as well
dupl_values<-filter(app, id  %in%  dupl_id)
glimpse(dupl_values)
# -> it seems that only column income is different for each id

#get unique combination of id and income
unique(dupl_values[,c('id','income')])
# -> for each duplicated id there are 2 different values for income - 1 or other value
# -> we can assume that value which is not 1 is correct, therefor we will do filter accordingly

app<-filter(app, (id  %in%  dupl_id & income !=1) |  !(id  %in%  dupl_id))
(dupl_id<-app$id[duplicated(app$id)])
# ->no more duplicates


# ------ data analysis -APP ------
#how many NULL/NA values each column has
(na_count <- sapply(app, function(x) sum(is.na(x))))
(null_count <- sapply(app, function(x) sum(is.null(x))))

#->children has small number of NA, to be sure, let's check whether among those NA has significant number of bad=1
table(filter(app, is.na(children))$bad)

#since nb(bad=1) = 2, we can replace NA with 0
app$children[is.na(app$children)] <- 0




table(app$bad)
#we can there is clients much more clients with bad=0 (9745), than with bad=1 (255).

#not much difference among woman and man
table(app$sex)
aggregate(bad ~ sex, data = app, mean)

#divorced and single people has more tendency to default
aggregate(bad ~ family, data = app, mean)

#people who are renting their apartment has more tendency to default
aggregate(bad ~ rent, data = app, mean)

#self-employed has more tendency to default
aggregate(bad ~ employment, data = app, mean)

#not clear correlation between education and default
aggregate(bad ~ education, data = app, mean)

#to check if there is outliers in amount or income

hist(app$income)
boxplot(app$income)

hist(app$amount)
boxplot(app$amount)

ggplot(app, aes(income, amount)) +
  geom_point() +
  ggtitle("Gaph1: Correlation between Income and Life Amonut of credit")


ggplot(app,aes(x =  cut(app$income,c(-Inf,2792,3133, 5000,Inf),labels = c('[-Inf, 2792]','[2792,3133]','[3133,5000]','[5000,Inf]'))
               , y = amount)) +
  geom_boxplot() +
  xlab("Income") +
  ylab("Ammount")


#distribution for column children: most people have 0-2 children and they have lower prob to default
#there is only 1 person with 10 children and 6 persons with 7 children, they have mean(bad)=0, which
#might be just by accident, so it's better to exclude those values because it's logical that more children will has bad impact on
#default probability.

hist(app$children)
aggregate(bad ~ children, data = app, mean)
table(app$children)
boxplot(app$children)

app1<-filter(app, children <=6)
glimpse(app1)

#installments - no significant correlation
table(app1$installments)
aggregate(bad ~ installments, data = app1, mean)

#employment - self - emplyed has higer prob to default
table(app1$employment)
aggregate(bad ~ employment, data = app1, mean)



#to be sure that dates are between 2011-2012
summary(app1$date)

#to be sure that brithdate has reasonable values
summary(app1$birthdate)

#age of clients - almost all ages have equal part in data
app2<-(mutate(app1, age = as.double(difftime(date,birthdate, units="days")/365)))
glimpse(app2)
hist(app2$age)

# ------ import cb ------

cb<-read_csv("cb.csv",col_types = cols (col_integer(), col_double(), col_double(), col_integer()))
glimpse(cb)
#get duplicated id, if any
(dupl_cbid<-cb$id[duplicated(cb$id)]) # -> no duplicates
# ------ data analysis -CB ------
(na_count <- sapply(cb, function(x) sum(is.na(x))))
(null_count <- sapply(cb, function(x) sum(is.null(x))))

hist(cb$debt)

#we can see that distribution of mortgage is not balanced
hist(cb$mortgage)
#median is 0 -> half of clients has mortgage less or equal than 0, but according to box plot only 1 outlier
#->we can split this column into 3 classes according to median, 3rd quantile and max
summary(cb$mortgage)
boxplot(cb$mortgage)

#median of overdue is 0 -> half of clients has overdue less or equal than 0 days. Max is 83.
#->we can split this column into 3 classes according to median, 3rd quantile and max
summary(cb$overdue)
hist(cb$overdue)
table(cb)
boxplot(cb$overdue)

ovd <- df <- mutate(cb, overdue = cut(cb$overdue, c(-Inf, 0,40,Inf)))
table(ovd$overdue)

glimpse(cb) #5,616 rows

# ------ merging APP and CB ------

df<-left_join(app2, cb, by = "id")
glimpse(df)

#vidimo da ima 4,382 reda gde su debt, mortgage i overdue NA
(na_count <- sapply(df, function(x) sum(is.na(x))))

na_values<-filter(df, is.na(debt))
#medju NA vrednostima nema mnogo njih gde je bad=1
table(na_values$bad)



#we will replace NA values with 0 since there is no much bad=1 among them

df <-mutate(df, overdue = ifelse(is.na(df$overdue),0,df$overdue))
df <- mutate(df, overdue_1 = is.factor(ifelse(overdue<=25,1,0)))


df <-mutate(df, debt_1 = ifelse(is.na(df$debt),0,df$debt))
df <-mutate(df, mortgage_1 = ifelse(is.na(df$mortgage),0,df$mortgage))

glimpse(df)

# ------ examine colinearity ------
install.packages("corpcor")
library(corpcor)
cor2pcor(cov(cb))#->correlation between dept and mortgage -> we will exclude mortgage
df1<-select(df, c("-mortgage_1", "mortgage"))
cb1<-select(cb, -mortgage)
cor2pcor(cov(cb1)) #->ok

test1<-select(df,amount, income,age,installments,debt_1, overdue_1, children)
cor2pcor(cov(test1))

#to exclude age, income, installments

df2<-select(df, c(-age,-income,-installments))
test2<-select(df2,amount,debt_1, overdue)
cor2pcor(cov(test2))

#sex, family, rent (TRUE, FALES -> 1,0), education, employment are categorical variables -> we will do transformation accordingly
df2<-mutate(df2, sexMale = as.factor(ifelse(sex == 'male' ,1,0)))

df2$rent_1 <-as.factor(ifelse(df2$rent, 1, 0))
df2$education <-as.factor(df2$education)

df2<-mutate(df2, divorced_single = as.factor(ifelse(family == 'divorced' |family == 'single' ,1,0)))
df2<-mutate(df2, self_empl = as.factor(ifelse(employment == 'self-employed',1,0)))

# ------ test and train sample (divide by year) ------
install.packages("lubridate")
library(lubridate)
train <-filter(df2, year(date)==2011)
test <-filter(df2, year(date)==2012)
glimpse(train) #5,444
glimpse(test) #4984



# ------ create model ------
train_model<-glm(bad ~ sexMale + divorced_single + children + self_empl + rent_1 + debt_1 + overdue_1  + amount + education
                 , family=binomial
                 , data=train)

summary(train_model)

install.packages("car")
library(car)
vif(train_model)

summary(train_model)


train_model1<-glm(bad ~ sexMale + divorced_single + children + self_empl + rent_1 + debt_1 + overdue_1, family=binomial
                  , data=train)
summary(train_model1)


#for prediction
install.packages("modelr")
library(modelr)

#add column with predictions

train <- mutate(train, p = predict(train_model1, type = "response"))
p_class <-cut(train$p, 5)
table(p_class)
mean(train$p)


summary(train$p) #0.013519 0.022961
train <- mutate(train, bad_predicted = ifelse(p<=0.03,0,1))
table(train$bad_predicted)

table(train$bad)

# Calculate the model's accuracy
mean(train$bad == train$bad_predicted)


############# Model performance tradeoffs #####################

# Load the pROC package
install.packages("pROC")
library(pROC)

# Create a ROC curve
ROC <- roc(train$bad, train$p)

# Plot the ROC curve
plot(ROC, col = "blue")

# Calculate the area under the curve (AUC)
auc(ROC)

#########
table(test$bad)
glimpse(train)
train <- mutate(train, bad = as.numeric(bad))
table(train$bad, train_model1 > 0.5)

aggregate(p ~ bad , data = train, mean)

table(ifelse(train$bad==1,'Defaul True','Default False'), train$bad_predicted)
# Sensitivity
79/(49+76)

#Specificity
4169/(4169+1150)

install.packages("ROCR")
library(ROCR)
ROCRpred = prediction(train$p, train$bad)
ROCRperf = performance(ROCRpred, "tpr", "fpr")

plot(ROCRperf)
# Add colors
plot(ROCRperf, colorize=TRUE)
# Add threshold labels
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0.01,0.09,by=0.01), text.adj=c(-0.2,1.7))

#Sensitivity = TruePositive / (TruePositive + FalseNegative)
#Specificity = TrueNegative / (FalsePositive + TrueNegative)
#Where:

#Sensitivity = True Positive Rate
#Specificity = 1 ? False Positive Rate

train$class <- cut(train$p, c(0.005, 0.01, 0.02, 0.05, 0.1, 1))

aggregate(bad ~ class, data = train, function(x) c(length = length(x), sum = sum(x)))

######## test model ########
test<-mutate(test, married = as.factor(ifelse(family == 'married',1,0)))
test<-mutate(test, self_empl = as.factor(ifelse(employment == 'self-employed',1,0)))
test<-mutate(test, overdue_40 = as.factor(ifelse(overdue_1 == '(40, Inf]',1,0)))

test <- mutate(test, p = predict(train_model1, type = "response",newdata = test))
test$p <- predict(train_model1, type = "response", newdata = test)

p_class_test <-cut(test$p, 5)
table(p_class_test)
mean(test$p)


summary(test$p) #0.013519 0.022961
test <- mutate(test, bad_predicted = ifelse(p<=0.03,0,1))
table(test$bad_predicted)

table(test$bad)

# Calculate the model's accuracy
mean(test$bad == test$bad_predicted) #0.7879213

####################################################################33

########### Random forest ###########
install.packages("randomForest")
library(randomForest)

train1<-mutate(train, bad=as.factor(bad))
train2<-mutate(df, bad=as.factor(bad))
model_rf <- randomForest(bad ~ sex + married + children +rent + debt_1 + overdue_40 +self_empl , data = train1, importance = TRUE)
summary(model_rf)
varImpPlot(model_rf)
Imp(model_rf)

train_rf<-mutate(train1, p1 = predict(model_rf, train1,type = "class"))

predict<-predict(model_rf,train1,type = "class")

table(train_rf$p1, train_rf$bad)

mean(train_rf$bad == train_rf$p1)


#rf for test data

test <- mutate(test, prf = predict(model_rf    , type = "response",newdata = test))


mean(test$bad==test$prf)
