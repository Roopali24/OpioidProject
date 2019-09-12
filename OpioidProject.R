## Keeping Homeless Youth off Opioids
rm(list=ls())

library(openxlsx)
library(mice)
options(java.parameters = "-Xmx5g", memcache_for_speed=F)
library(bartMachine)
library(AUC)
library(caret)
setwd("~/Box Sync/Drug Project/Final Version")

## reading data
dat=read.xlsx("NewData_Final(Version4).xlsx")[,-1] ## Final cleaned data

## missing data
md.pattern(dat)
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(dat,2,pMiss) # proportion of missing data for each feature

dat[c(1:25,27:50,53,55:63,66:187,189:243)]=lapply(dat[c(1:25,27:50,53,55:63,66:187,189:243)],factor) # convert to factors
summary(dat)

## Imputation
init = mice(dat[,-243], maxit=0) 
meth = init$method
predM = init$predictorMatrix
set.seed(123)
imputed = mice(dat[,-243], method=meth, predictorMatrix=predM, nnet.MaxNWts = 50000) ## running this could take a while
imputed <- complete(imputed)
sapply(imputed, function(x) sum(is.na(x)))

#write.csv(imputed,"NewData_Imputed.csv") ## save the imputed data

## reading data
dat=read.csv("NewData_Imputed5.csv")[,-1] ## transformed imputed data according to our interventions
dat=dat[,-164] #removing gang_age_1 since there are still missing observation and the variable is irrelevant
dat[c(1:19,21:44,47,49:57,60:213)]=lapply(dat[c(1:19,21:44,47,49:57,60:213)],factor) # convert to factors
summary(dat)

## Training and Testing
set.seed(123)
n=round(nrow(dat)*.7)
train = sample(1:nrow(dat), size=n)
df.train=dat[train,]
df.test=dat[-train,]

## BART model
set_bart_machine_num_cores(4)
bm=bartMachineCV(X= df.train[,1:212], y=df.train[,213]) ##could take a while

## Test data evaluation
bm_test=1-predict(bm, df.test[,1:212], type="prob") # prob of being 1
RC=roc(bm_test,df.test$user)
AUC::auc(RC)
bm_test=1*(bm_test>0.35)
recall(table(factor(bm_test,c(0,1)),factor(df.test$user)), relevant='1')
F_meas(table(factor(bm_test,c(0,1)),factor(df.test$user)), relevant='1', beta=1)
a=table(factor(bm_test,c(0,1)),factor(df.test$user))
1-(a[1,2]+a[2,1])/nrow(df.test)
b=as.data.frame(cbind(RC$fpr, RC$tpr))

## Counterfactual Probabilities
prob=NULL # prob of being 1
prob=cbind(prob,1-predict(bm, df.test[,1:212], type="prob"))
setwd("~/Box Sync/Drug Project/Final Version/Counterfactual_Test_dataset")
df=read.csv("CounterfactualsTest_adhd_dx.csv")[,-1]
prob=cbind(prob,1-predict(bm, df[,1:212], type="prob"))
df=read.csv("CounterfactualsTest_condom_eff.csv")[,-1]
prob=cbind(prob,1-predict(bm, df[,1:212], type="prob"))
df=read.csv("CounterfactualsTest_condom_int.csv")[,-1]
prob=cbind(prob,1-predict(bm, df[,1:212], type="prob"))
df=read.csv("CounterfactualsTest_currentfc.csv")[,-1]
prob=cbind(prob,1-predict(bm, df[,1:212], type="prob"))
df=read.csv("CounterfactualsTest_education.csv")[,-1]
prob=cbind(prob,1-predict(bm, df[,1:212], type="prob"))
df=read.csv("CounterfactualsTest_giscale.csv")[,-1]
prob=cbind(prob,1-predict(bm, df[,1:212], type="prob"))
df=read.csv("CounterfactualsTest_inschool.csv")[,-1]
prob=cbind(prob,1-predict(bm, df[,1:212], type="prob"))
df=read.csv("CounterfactualsTest_prep_know.csv")[,-1]
prob=cbind(prob,1-predict(bm, df[,1:212], type="prob"))
df=read.csv("CounterfactualsTest_stress_street.csv")[,-1]
prob=cbind(prob,1-predict(bm, df[,1:212], type="prob"))
colnames(prob)=c("base","adhd_dx","condom_eff","condom_int","currentfc","education","giscale",
                 "inschool","prep_know","stress_street")
write.csv(prob,"CounterfactualProb.csv")

## Decision Trees
library(rpart)

tree.fit=rpart(factor(user) ~.,data=df.train, control=rpart.control(cp=0.01)) # 0.01 was chosen by looking at the cptable
tree_test=predict(tree.fit, newdata=df.test[,1:212], type="prob")
RC=roc(tree_test[,2],factor(df.test$user))
auc(RC)
a=as.data.frame(cbind(RC$fpr, RC$tpr))

setwd("~/Box Sync/Drug Project/Final Version/Counterfactual_Test_dataset")
prob=read.csv("CounterfactualProb.csv")[,-c(1:2)]
prob1=cbind(prob,tree_test[,2])
colnames(prob1)=c(colnames(prob),"base_DT")
write.csv(prob1,"DT_prob.csv") # saving counterfactuals with DT base probabilities

## Lasso Regression
library(glmnet)
library(dplyr)
x <- model.matrix(user~., df.train)[,-1]
y=as.numeric(df.train[,213]) - 1
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
model <- glmnet(x, y, alpha = 1, family = "binomial",
                lambda = cv.lasso$lambda.min)

x.test <- model.matrix(user~., df.test)[,-1]
prob <- model %>% predict(newx = x.test)
invlog = function(x){exp(x)/(1+exp(x))}
prob=as.vector(invlog(prob))
#lasso.pred <- predict(model, s = cv.lasso$lambda.min, newx = x.test)
RC=roc(prob,as.factor(df.test$user))
AUC::auc(RC)
c=as.data.frame(cbind(RC$fpr, RC$tpr))

## Roc curve plot
tt=seq(1,nrow(b),by=20)
b=b[tt,]
tt=seq(1,nrow(c),by=12)
c=c[tt,]

a=cbind(a,1)
colnames(a)=c("fpr","tpr","model")
b=cbind(b,2)
colnames(b)=c("fpr","tpr","model")
c=cbind(c,3)
colnames(c)=c("fpr","tpr","model")
cc=rbind(a,b,c)

ggplot(cc, aes(x=fpr, y=tpr, group=factor(model))) + ylab("True Positive Rate") + xlab("False Positive Rate") + 
  geom_line() + geom_point(aes(shape=factor(model)),size=3) + theme_bw() + geom_abline(aes(intercept=0, slope=1)) +
  theme(legend.text = element_text(size = 18,  family="serif"),legend.title=element_blank(), 
        axis.text=element_text(size=13), 
        axis.title=element_text(size=15,  family="serif"), legend.position = c(.95, .1),
        legend.justification = c("right", "bottom")) +
  scale_shape_manual(values=c(15, 16, 17),
                     labels = c("DT", "BART","Lasso"))


## Causality Inference for each pair of intervention
library(openxlsx)
options(java.parameters = "-Xmx5g", memcache_for_speed=F)
library(bartMachine)
library(AUC)
library(caret)

## reading data
rm(list=ls())
setwd("~/Box Sync/Drug Project/Final Version/Pairs/WholeData")
## We need to do this part for each pair. Following is an example for 1 case.
dat=read.csv("currentfc_prep_know.csv")[,-1]
colnames(dat)
dat=dat[,-164] #removing gang_age_1
dat[c(1:19,21:44,47,49:57,60:213)]=lapply(dat[c(1:19,21:44,47,49:57,60:213)],factor) # convert to factors
# careful while making factors because column orders are different

## Training and Testing
set.seed(123)
n=round(nrow(dat)*.7)
train = sample(1:nrow(dat), size=n)
df.train=dat[train,]
df.test=dat[-train,]
#setwd("~/Box Sync/Drug Project/Final Version/Pairs/TestData")
#write.csv(df.test,"currentfc_prep_know_test.csv")

## BART model
bm=bartMachineCV(X= df.train[,1:212], y=df.train[,213])

## Test data evaluation
bm_test=1-predict(bm, df.test[,1:212], type="prob") # prob of being 1
df=bm_test
bm_test=as.numeric(predict(bm, df.test[,1:212], type="class"))-1
df=cbind(df,bm_test)
setwd("~/Box Sync/Drug Project/Final Version/Pairs/Counterfactuals")
df_test=read.csv("currentfc_prep_know_test_c.csv")[,-1]
bm_test=1-predict(bm, df_test[,1:212], type="prob") # prob of being 1
df=cbind(df,bm_test)
bm_test=as.numeric(predict(bm, df_test[,1:212], type="class"))-1
df=as.data.frame(cbind(df,bm_test))
colnames(df)=c("Base_Prob","Base_Pred","Counter_Prob","Counter_Pred")
setwd("~/Box Sync/Drug Project/Final Version/Pairs/Predictions")
write.csv(df,"currentfc_prep_know_pred.csv")

## Creating 95% CI by bootstapping
table(df$Base_Pred)
table(df$Counter_Pred)

diff=NULL
for(i in 1:1000){
  sample1=rbinom(n=5000,size=1, prob= (49/54))
  sample2=rbinom(n=5000,size=1, prob= (5/54))
  diff=c(diff,mean(sample1)-mean(sample2))
}

mean(diff) - (qt(p=0.975, df= 999)*sd(diff)/sqrt(1000))
mean(diff) + (qt(p=0.975, df= 999)*sd(diff)/sqrt(1000))

