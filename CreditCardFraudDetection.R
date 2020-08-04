rm(list = ls())
cat("\14")
credit_card<-read.csv("creditcard.csv")

#look at the structure of the data
str(credit_card)

#Convert the class to a factor varaible
credit_card$Class<-factor(credit_card$Class,levels = c(0,1))

#get the summary of the data
summary(credit_card)

#count the missing values
sum(is.na(credit_card))

#get the distribution of fraud and legit cases
table(credit_card$Class)

#get the  proportion of distribution of fraud and legit cases
prop.table(table(credit_card$Class))

#EDA

labels<-c("legit","fraud")
labels<-paste(labels,round(100*prop.table(table(credit_card$Class)),2))
labels<-paste0(labels,"%")

pie(table(credit_card$Class),labels,col =c("orange","red"),
    main = "Pie chart of credit card transactions")

#we will take a subset of data ,so that the computation is faster
library(dplyr)
set.seed(1)
credit_card<-credit_card %>% sample_frac(0.1)

table(credit_card$Class)

library(ggplot2)

ggplot(data = credit_card,aes(x=V1,y=V2,col=Class))+
  geom_point()+
  theme_bw()+
  scale_color_manual(values = c('dodgerblue','red'))

#creating training and test set for model

library(caTools)

set.seed(123)

data_sample<-sample.split(credit_card$Class,SplitRatio = 0.80)

train_data<-subset(credit_card,data_sample==TRUE)
test_data<-subset(credit_card,data_sample==FALSE)

dim(train_data)
dim(test_data)


#Random Over Sampling(ROS)

table(train_data$Class)

n_legit<-22750
new_frac_legit<-0.50
new_n_total<-n_legit/new_frac_legit

install.packages('ROSE')
library(ROSE)

oversampling_result<-ovun.sample(Class~.,data = train_data,method = "over",N = new_n_total,seed = 209)

oversampling_credit<-oversampling_result$data

ggplot(data = oversampling_credit,aes(x=V1,y=V2,col=Class))+
  geom_point(position = position_jitter(width = 0.1))+
  theme_bw()+
  scale_color_manual(values = c('dodgerblue','red'))
#we will not use this for training our model since it only duplicates the fraud cases data.

#Random under Sampling(RUS)

table(train_data$Class)
n_fraud<-35
new_frac_fraud<-0.50
new_n_total<-n_fraud/new_frac_fraud

undersampling_result<-ovun.sample(Class~.,data = train_data,method = "under",N = new_n_total,seed = 2020)

undersampling_credit<-undersampling_result$data

ggplot(data = undersampling_credit,aes(x=V1,y=V2,col=Class))+
  geom_point()+
  theme_bw()+
  scale_color_manual(values = c("dodgerblue","red"))

#here we loose of data ,so we cant use this technique also

#Now we will see both 

n_new<-nrow(train_data)
fraction_fraud_new<-0.50

sampling_result<-ovun.sample(Class~.,data = train_data,method = "both",N = n_new,p = fraction_fraud_new,seed = 202)

sampling_credit<-sampling_result$data

table(sampling_credit$Class)
prop.table(table(sampling_credit$Class))

ggplot(data = sampling_credit,aes(x=V1,y=V2,col=Class))+
  geom_point(position = position_jitter(width = 0.1))+
  theme_bw()+
  scale_color_manual(values = c("dodgerblue","red"))
#still this method is creating the duplicate fraud cases use is not essential approach to follow 
#so we will look at the next methd

#SMOTE(Synthetic Minority OverSampling Technique)

install.packages("smotefamily")
library(smotefamily)

table(train_data$Class)

#Set the no of fraud and legitimate cases,and the desired percentage od legtimate cases 
n0<-22750
n1<-35
r0<-0.6

#Calculate the value of dup_size paramter of SMOTE
ntimes<-((1-r0)/r0)*(n0/n1)-1
smote_output<-SMOTE(X = train_data[,-c(1,31)],
                    target = train_data$Class,
                    K = 5,
                    dup_size = ntimes)

credit_smote<-smote_output$data
colnames(credit_smote)[30]<-"Class"
prop.table(table(credit_smote$Class))

#compare original data and Smote data using Scatter plot

#original data
ggplot(data = train_data,aes(x=V1,y=V2,col=Class))+
  geom_point()+
  theme_bw()+
  scale_color_manual(values = c("dodgerblue","red"))

#Smote data
ggplot(data = credit_smote,aes(x=V1,y=V2,col=Class))+
  geom_point()+
  theme_bw()+
  scale_color_manual(values = c("dodgerblue","red"))

#so now we have added syntetic points not duplicate points so we can use this data for our model

#Decision Tree model
library(rpart)
library(rpart.plot)

cart_model<-rpart(Class~.,credit_smote)
rpart.plot(cart_model,extra = 0,type = 5,tweak = 1.2)

#predict fraud classes
predicted_val<-predict(cart_model,test_data,type = 'class')

#Confusion Matrix
library(caret)

confusionMatrix(predicted_val,test_data$Class)

#decision tree without using Smote data
cart_model<-rpart(Class~.,train_data[-1])
rpart.plot(cart_model,extra = 0,tweak = 1.2,type = 5)

#predict the fraud classes
predicted_val<-predict(cart_model,test_data[-1],type = 'class')

confusionMatrix(predicted_val,test_data$Class)

#predicting on whole dataset
#without using smote model for whole dataset
predicted_val<-predict(cart_model,credit_card[-1],type = 'class')
confusionMatrix(predicted_val,credit_card$Class)

#Decision with smote Model for whole dataset
cart_model<-rpart(Class~.,credit_smote)
predicted_val<-predict(cart_model,credit_card[-1],type = 'class')

confusionMatrix(predicted_val,credit_card$Class)
