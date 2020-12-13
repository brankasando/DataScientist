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
glimpse(dupl_id)
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
255/9745

#not much difference among woman and man
table(app$sex)
aggregate(bad ~ sex, data = app, mean)
#transform sex
app<-mutate(app, sexMale = as.factor(ifelse(sex == 'male' ,1,0)))

#divorced and single people has more tendency to default
aggregate(bad ~ family, data = app, mean)

app<-mutate(app, divorced_single = as.factor(ifelse(family == 'divorced' |family == 'single' ,1,0)))


#people who are renting their apartment has more tendency to default
aggregate(bad ~ rent, data = app, mean)
app$rent_1 <-as.factor(ifelse(app$rent, 1, 0))


#self-employed has more tendency to default
aggregate(bad ~ employment, data = app, mean)
app<-mutate(app, self_empl = as.factor(ifelse(employment == 'self-employed',1,0)))

#not clear correlation between education and default
aggregate(bad ~ education, data = app, mean)
app$education <-as.factor(app$education)

#to check if there is outliers in amount or income

par(mfrow = c(2, 2))
hist(app$income)
boxplot(app$income)
hist(app$amount)
boxplot(app$amount)
par(mfrow = c(1, 1))


ggplot(app, aes(income, amount)) +
  geom_point() +
  ggtitle("Gaph1: Correlation between Income and Amonut of credit")

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
boxplot(cb$debt)
summary(cb$debt)

#we can see that distribution of mortgage is not balanced
hist(cb$mortgage)
boxplot(cb$mortgage)
summary(cb$mortgage)

summary(cb$overdue)
hist(cb$overdue)
boxplot(cb$overdue)


# ------ merging APP and CB ------

df<-left_join(app2, cb, by = "id")
glimpse(df)

#4,382 NA
(na_count <- sapply(df, function(x) sum(is.na(x))))

na_values<-filter(df, is.na(debt))
#not much with bad=1
table(na_values$bad)

#we will replace NA values with 0 since there is no much bad=1 among them

df <-mutate(df, overdue = ifelse(is.na(df$overdue),0,df$overdue))
df <- mutate(df, overdue_1 = cut(df$overdue, c(-Inf, 0,40,Inf)))
aggregate(bad ~ overdue_1, data = df, mean)
table(df$overdue_1)

df <-mutate(df, debt_1 = ifelse(is.na(df$debt),0,df$debt))
df <-mutate(df, mortgage_1 = ifelse(is.na(df$mortgage),0,df$mortgage))

glimpse(df)

# ------ examine colinearity ------
install.packages("corpcor")
library(corpcor)
cor2pcor(cov(cb))#->correlation between dept and mortgage -> we will exclude mortgage
cb1<-select(cb,  -mortgage)

cor2pcor(cov(cb1)) #->ok

#->excluding mortgage from df
df1<-select(df,  -mortgage)


test1<-select(df1,amount, income,age,installments,debt_1, children, overdue)
cor2pcor(cov(test1))

#to exclude  income, installments

df2<-select(df1, c(-income,-installments))
test2<-select(df2,amount,debt_1,children,age )
cor2pcor(cov(test2))



# ------ test and train sample (divide by year) ------
install.packages("lubridate")
library(lubridate)
train <-filter(df2, year(date)==2011)
test <-filter(df2, year(date)==2012)
glimpse(train) #5009
glimpse(test) #4984



# ------ create model ------
train_model<-glm(bad ~ sexMale + divorced_single + children + self_empl + rent_1 + debt_1 + overdue_1  + amount + education +age
                 , family=binomial
                 , data=train)

summary(train_model)


train_model1<-glm(bad ~ sexMale +  divorced_single + children + self_empl + rent_1 + debt_1 + overdue_1, family=binomial
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
mean(train_lr$bad == train_lr$bad_predicted)
table(ifelse(train_lr$bad==1,'Default True','Default False'), train_lr$bad_predicted)
# Sensitivity 0.4568966 - TruePositive / (TruePositive + FalseNegative)
53/(63+53)

#Specificity 0.8657265 - TrueNegative / (FalsePositive + TrueNegative)
4236  /(4236+657)


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
mean(train_lr1$bad == train_lr1$bad_predicted) # 0.6412458
table(ifelse(train_lr1$bad==1,'Default True','Default False'), train_lr1$bad_predicted)
# Sensitivity 0.7758621 - TruePositive / (TruePositive + FalseNegative)
90/(26+90)

#Specificity 0.6380544 - TrueNegative / (FalsePositive + TrueNegative)
3122  /(3122 +1771)



######## test model ########
test_lr <- mutate(test, p = predict(train_model1, type = "response",newdata = test))
test_lr$p <- predict(train_model1, type = "response", newdata = test)

summary(test_lr$p)
summary(train_lr$p)



p_class_test <-cut(test_lr$p, 5)
merge(aggregate(bad ~ p_class_test, data = test_lr,function(x) c(length = length(x), sum = sum(x)))
      ,aggregate(p ~ p_class_test, data = test_lr, mean))


test_lr <- mutate(test_lr, bad_predicted = ifelse(p<=0.02,0,1))
# Calculate the model's accuracy
mean(test_lr$bad == test_lr$bad_predicted) #0.6494783
table(ifelse(test_lr$bad==1,'Default True','Default False'), test_lr$bad_predicted)

# Sensitivity 0.7482014 - TruePositive / (TruePositive + FalseNegative)
104/(35+104)

#Specificity 0.646646- TrueNegative / (FalsePositive + TrueNegative)
3133   /(3133  +1712)


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
mean(train_rf$bad == train_rf$p1) #0.9960072

# Sensitivity  - TruePositive / (TruePositive + FalseNegative)
96/(20 +96) #0.8275862

#Specificity  - TrueNegative / (FalsePositive + TrueNegative)
4893/(4893)


#rf for test data
test_rf<-mutate(test_rf, p1 = predict(model_rf, test_rf,type = "class"))
mean(test_rf$bad==test_rf$p1) #0.9721108
table(ifelse(test_rf$bad==1,'Default True','Default False'), test_rf$p1)

