require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
require(DMwR)
require(glmnet)
require(dplyr)
require(methods)
require(rpart)
require(randomForest)
require(gbm)

ds=read.csv("Police_Use_of_Force_preprocessed.csv")
#ds = cbind(ds[7:18],ds[21:53],ds[65:95])
ds = cbind(ds[9],ds[11:15],ds[17:28],ds[33:38],ds[40:41])
ds = na.omit(ds)
n=nrow(ds)
set.seed(1)
index = sample(1:n,n)
ds$force2 = relevel(ds$ForceType, ref = "Bodily Force")

mult_err=matrix(NA,5,1)
for (p in 1:5) {
  ds.train = ds[-index[((n/5)*(p-1)+1):((n/5)*p)],] 
  ds.test = ds[index[((n/5)*(p-1)+1):((n/5)*p)],]
  bal.ds = SMOTE(ForceType ~., ds.train, perc.over = 2000, k = 5, perc.under = 4000)
  model = multinom(force2 ~ . -ForceType, data = bal.ds)
  
  #pred=predict(model, newdata = ds.test, "probs")
  pred=predict(model, newdata = ds.test, "class")
  print(table(pred,ds.test$ForceType))
  comp = pred!=ds.test$ForceType
  mult_err[p] = sum(comp)/length(comp)
}
mult_err

l_err=matrix(NA,5,1)
r_err=matrix(NA,5,1)
for (p in 1:5) {
  ds.train = ds[-index[((n/5)*(p-1)+1):((n/5)*p)],] 
  ds.test = ds[index[((n/5)*(p-1)+1):((n/5)*p)],]
  ds.train = SMOTE(ForceType ~., ds.train, perc.over = 2000, k = 5, perc.under = 4000)
  train.x = data.matrix(ds.train[-c(3)])
  #train.x = model.matrix( ~ ., as.data.frame(train.x))
  train.y = as.matrix(ds.train[c(3)])
  test.x = data.matrix(ds.test[-c(3)])
  #test.x = model.matrix( ~ ., as.data.frame(test.x))
  test.y = as.matrix(ds.test[c(3)])

  #traintest=rbind(ds.train,ds.test)
  #X = sparse.model.matrix(as.formula(paste("ForceType ~", paste(colnames(ds.train[,-c(3)]), sep = "", collapse=" +"))), data = traintest)
  #cv.lasso = cv.glmnet(X[-index[((n/5)*(p-1)+1):((n/5)*p)],], ds.train[,c(3)], alpha = 1, family = "multinomial",
                    #type.measure = "class",maxit=1000)
  #bestlam.l = cv.lasso$lambda.min
  #lasso.model = glmnet(X[-index[((n/5)*(p-1)+1):((n/5)*p)],], ds.train[,c(3)], alpha = 1, family = "multinomial",
                    #lambda= bestlam.l, type.measure = "class",maxit=1000)
  #pred = predict(lasso.model, newx=X[index[((n/5)*(p-1)+1):((n/5)*p)],], type="class")
  
  cv.lasso = cv.glmnet(train.x, train.y, alpha = 1, family = "multinomial",maxit=1000)
  bestlam.l = cv.lasso$lambda.min
  lasso.model = glmnet(train.x, train.y, alpha = 1, family = "multinomial",lambda= bestlam.l,maxit=1000)
  pred=predict(lasso.model,newx=test.x,type="class")
  print(table(pred,ds.test$ForceType))
  comp = pred!=ds.test$ForceType
  l_err[p] = sum(comp)/length(comp)
    
  cv.ridge = cv.glmnet(train.x, train.y, alpha = 0, family = "multinomial",maxit=1000)            
  bestlam.r = cv.ridge$lambda.min
  ridge.model = glmnet(train.x, train.y, alpha = 0, family = "multinomial",lambda= bestlam.r, maxit=1000)
  pred=predict(ridge.model,newx=test.x,type="class")
  print(table(pred,ds.test$ForceType))
  comp = pred!=ds.test$ForceType
  r_err[p] = sum(comp)/length(comp)
}
l_err
r_err

rptree.err = matrix(NA,nrow=1,ncol=5)
for (p in 1:5) {
  ds.train = ds[-index[((n/5)*(p-1)+1):((n/5)*p)],] 
  ds.test = ds[index[((n/5)*(p-1)+1):((n/5)*p)],]
  ds.train = SMOTE(ForceType ~., ds.train, perc.over = 2000, k = 5, perc.under = 4000)
  rptree = rpart(ForceType ~.-force2, data=ds.train,
                 control=rpart.control(minsplit=15,minbucket=5,cp=.005))
  pred=predict(rptree,ds.test,type="class")
  print(table(pred,ds.test$ForceType))
  comp = pred!=ds.test$ForceType
  rptree.err[p]=sum(comp)/length(comp)
}
rptree.err

rf_err=matrix(NA,5,1)
for (p in 1:5) {
  ds.train = ds[-index[((n/5)*(p-1)+1):((n/5)*p)],]
  ds.test = ds[index[((n/5)*(p-1)+1):((n/5)*p)],]
  ds.train = SMOTE(ForceType ~., ds.train, perc.over = 2000, k = 5, perc.under = 4000)
  bag=randomForest(ForceType~.-force2,data=ds.train,mtry=8,importance =TRUE)
  varImpPlot(bag)
  yhat.rf = predict(bag,newdata=ds.test) 
  print(table(yhat.rf,ds.test$ForceType))
  comp = pred!=ds.test$ForceType
  rf_err[p]= sum(comp)/length(comp)
}
rf_err

