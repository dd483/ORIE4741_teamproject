library(class)
library(glmnet)
library(dplyr)
ds = read.csv("Police_Stop_Data_onehot.csv")

ds$citationIssued_1[ds$citationIssued == "YES"] = 1
ds$citationIssued_1[ds$citationIssued == "NO"] = 0
ds = ds[,5:75]
n=nrow(ds)


set.seed(1)
index = sample(1:n,n) #randomly generate indices to split train/test later


log.err = matrix(NA,nrow=1,ncol=5)
l.err = matrix(NA,1,5)
r.err = matrix(NA,1,5)
#5-fold CV
for (p in 1:5) {
  #split train and test
  ds.train = ds[-index[((n/5)*(p-1)+1):((n/5)*p)],] #get groups of 34 indices to get fold
  ds.test = ds[index[((n/5)*(p-1)+1):((n/5)*p)],]
  
  #split x and y
  train.x = as.matrix(ds.train[-c(71)])
  train.y = as.matrix(ds.train[c(71)])
  test.x = as.matrix(ds.test[-c(71)])
  test.y = as.matrix(ds.test[c(71)])
  
  #Logistic
  log = glm(citationIssued_1 ~ ., data=ds.train,family=binomial)
  predictions = (predict(log, ds.test[,], type='response') > 0.5)
  log.err[p] = mean(predictions != ds.test$citationIssued_1)
  
  #Lasso
  cv.lasso = cv.glmnet(train.x, train.y, alpha = 1, family = "binomial")
  bestlam.l = cv.lasso$lambda.min
  lasso.model = glmnet(train.x, train.y, alpha = 1, family = "binomial",lambda= bestlam.l)
  prob.l <- lasso.model %>%
    predict(newx = test.x)
  pred.l = ifelse(prob.l > 0.5, 1, 0)
  l.err[p] = mean(pred.l != test.y)
  
  #Ridge
  cv.ridge = cv.glmnet(train.x, train.y, alpha = 0, family = "binomial")                     
  bestlam.r = cv.ridge$lambda.min
  ridge.model = glmnet(train.x, train.y, alpha = 0, family = "binomial",lambda= bestlam.r)
  prob.r <- ridge.model %>%
    predict(newx = test.x)
  pred.r = ifelse(prob.r > 0.5, 1, 0)
  r.err[p] = mean(pred.r != test.y)
}  


test = cbind(ds.test,pred.r)
mis = test$s0 != test$citationIssued_1
onlymis = test[mis,]
table(onlymis$s0)

test = cbind(ds.test,pred.l)
mis = test$s0 != test$citationIssued_1
onlymis = test[mis,]
table(onlymis$s0)

test = cbind(ds.test,predictions)
mis = test$predictions != test$citationIssued_1
onlymis = test[mis,]
table(onlymis$predictions)

summary(log)
predict(lasso.model,type="coef")





