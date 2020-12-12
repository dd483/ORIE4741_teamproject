library(class)
library(glmnet)
library(dplyr)
library(tree)
library(randomForest)
library(gbm)
library(rpart)
library(data.table)
ds = read.csv("Police_Stop_Data_onehot.csv")

ds$citationIssued_1[ds$citationIssued == "YES"] = 1
ds$citationIssued_1[ds$citationIssued == "NO"] = 0
ds = ds[,5:75]
ds = cbind(ds[1:2],ds[5:19],ds[31:71])
n=nrow(ds)
set.seed(1)
index = sample(1:n,n) #randomly generate indices to split train/test later


#Trees
citation=ifelse(ds$citationIssued_1==1,"Yes","No")
data_citation=data.frame(ds,citation)
tree.err = matrix(NA,nrow=1,ncol=5)
for (p in 1:5) {
  ds.train = data_citation[-index[((n/5)*(p-1)+1):((n/5)*p)],] 
  ds.test = data_citation[index[((n/5)*(p-1)+1):((n/5)*p)],]
  tree=tree(citation ~.-citationIssued_1,data = ds.train)  
  tree.pred=predict(tree,ds.test,type="class")
  print(table(tree.pred,ds.test$citation))
  plot(tree)
  text(tree)
  tree.err[p]=mean(tree.pred!= ds.test$citation)
}

summary(tree)
plot(tree)
text(tree)

tree.err



lmat=matrix(c(0,3,1,0),2,2)
rptree.err = matrix(NA,nrow=1,ncol=5)
for (p in 1:5) {
  ds.train = data_citation[-index[((n/5)*(p-1)+1):((n/5)*p)],] 
  ds.test = data_citation[index[((n/5)*(p-1)+1):((n/5)*p)],]
  rptree = rpart(citation ~. -citationIssued_1,data=ds.train,
                 parms=list(loss=lmat),
                 control=rpart.control(minsplit=15,minbucket=5,cp=.005))
  pred=predict(rptree,ds.test,type="class")
  print(table(pred,ds.test$citation))
  printcp(rptree)
  rptree.err[p]=mean(pred!= ds.test$citation)
}
rptree.err

dt=setDT(as.data.frame(rptree$variable.importance),keep.rownames = T)
plot(rptree$variable.importance[1:15],
     ylab="Variable Importance", xlab="",xaxt = "n", pch=20,cex.axis=.7)
axis(1,at=1:15,labels=dt$rn[1:15],las=2,cex.axis=.6)
summary(rptree)


#Random Forest
ds.train = data_citation[-index[((n/5)*(p-1)+1):((n/5)*p)],] 
ds.test = data_citation[index[((n/5)*(p-1)+1):((n/5)*p)],]
bag=randomForest(citation~.-citationIssued_1,data=ds.train,mtry=8,importance =TRUE)
bag
importance(bag)
varImpPlot(bag)
hist(treesize(bag))
yhat.bag = predict(bag,newdata=ds.test) 
table(yhat.bag,ds.test$citation)
bagerr=mean(yhat.bag!= ds.test$citation)
bagerr


rf_err=matrix(NA,5,1)
for (p in 1:5) {
  ds.train = data_citation[-index[((n/5)*(p-1)+1):((n/5)*p)],]
  ds.test = data_citation[index[((n/5)*(p-1)+1):((n/5)*p)],]
  bag=randomForest(citation~.-citationIssued_1,data=ds.train,mtry=8,importance =TRUE)
  varImpPlot(bag)
  yhat.rf = predict(bag,newdata=ds.test) 
  print(table(yhat.rf,ds.test$citation))
  rf_err[p]= mean(yhat.rf!= ds.test$citationIssued)
}
rf_err

#Boosting
boost_err=matrix(NA,5,1)
for (p in 1:5) {
  ds.train = ds[-index[((n/5)*(p-1)+1):((n/5)*p)],]
  ds.test = ds[index[((n/5)*(p-1)+1):((n/5)*p)],]
  boost=gbm(citationIssued_1~.-citationIssued_1,data=ds.train,distribution="bernoulli",n.trees=5000, interaction.depth=4,shrinkage =0.09,verbose =F)
  yhat.boost=(predict(boost,newdata=ds.test, n.trees=5000)>.5)
  print(table(yhat.boost,ds.test$citation))
  boost_err[p]=mean(yhat.boost!= ds.test$citationIssued_1)
}
boost_err





#Logistic
log.err = matrix(NA,nrow=1,ncol=5)
l.err = matrix(NA,1,5)
r.err = matrix(NA,1,5)
#5-fold CV
for (p in 1:5) {
  #split train and test
  ds.train = ds[-index[((n/5)*(p-1)+1):((n/5)*p)],]
  ds.test = ds[index[((n/5)*(p-1)+1):((n/5)*p)],]
  
  #split x and y
  train.x = as.matrix(ds.train[-c(58)])
  train.y = as.matrix(ds.train[c(58)])
  test.x = as.matrix(ds.test[-c(58)])
  test.y = as.matrix(ds.test[c(58)])
  
  #Logistic
  log = glm(citationIssued_1 ~ ., data=ds.train,family=binomial)
  predictions = (predict(log, ds.test[,], type='response') > 0.5)
  print(table(predictions,ds.test$citation))
  log.err[p] = mean(predictions != ds.test$citationIssued_1)
  
  #Lasso
  cv.lasso = cv.glmnet(train.x, train.y, alpha = 1, family = "binomial")
  bestlam.l = cv.lasso$lambda.min
  lasso.model = glmnet(train.x, train.y, alpha = 1, family = "binomial",lambda= bestlam.l)
  prob.l <- lasso.model %>%
    predict(newx = test.x)
  pred.l = ifelse(prob.l > 0.5, 1, 0)
  print(table(pred.l,ds.test$citation))
  l.err[p] = mean(pred.l != test.y)
  
  #Ridge
  cv.ridge = cv.glmnet(train.x, train.y, alpha = 0, family = "binomial")                     
  bestlam.r = cv.ridge$lambda.min
  ridge.model = glmnet(train.x, train.y, alpha = 0, family = "binomial",lambda= bestlam.r)
  prob.r <- ridge.model %>%
    predict(newx = test.x)
  pred.r = ifelse(prob.r > 0.5, 1, 0)
  print(table(pred.r,ds.test$citation))
  r.err[p] = mean(pred.r != test.y)
}  