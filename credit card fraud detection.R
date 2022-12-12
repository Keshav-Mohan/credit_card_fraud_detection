#importing and installing libraries 
install.packages('ranger')
library(ranger)
install.packages('caret')
install.packages('ggplot2')
library(caret)
install.packages('data.tab')
library(data.table)

data=read.csv("creditcard.csv")

#Exploration
data.table(data)

summary(data)
table(data$Class)
names(data) 

#summary of amount
summary(data$Amount)
sd(data$Amount)
IQR(data$Amount)
var(data$Amount)

#manipulation 
data$Amount=scale(data$Amount) #normalization

data2=data[, -c(1)]
head(data2)

set.seed(22)
library(caTools)

sample_data=sample.split(data2$Class, SplitRatio=0.80)
train_data=subset(data2, sample_data==TRUE)
test_data=subset(data2,sample_data==FALSE)

dim(train_data)
dim(test_data)

#fir logit on data
logistic_model=glm(Class~., test_data, family = binomial())
summary(logistic_model)

plot(logistic_model)

logistic_model1=glm(Class~., train_data, family = binomial())
summary(logistic_model1)

plot(logistic_model1)

library(pROC)
lr.predict=predict(logistic_model1, test_data, probability=TRUE)
auc.gb=roc(test_data$Class, lr.predict, plot=TRUE, col='green')

#decision tree
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

decision_model=rpart(Class~., data, method = 'class')
predicted_value=predict(decision_model, data, type='class')
probability=predict(decision_model, data, type='prob')
rpart.plot(decision_model)

install.packages('neuralnet')
library(neuralnet)
NNmodel=neuralnet::neuralnet(Class~. , train_data, linear.output=FALSE)
plot(NNmodel)

predNN=compute(NNmodel,test_data)
resultNN=predNN$net.result
resultNN=ifelse(resultNN>0.6,1,0)