file<-load("processed_text.RData")



### Random FOrest
library(randomForest)

Ytrainfac<-as.factor(Ytrain)
treecook<-randomForest(x=Xtrain,y=Ytrainfac,localImp=TRUE,ntrees=500)
tree.pred<-predict(treecook,Xtest,type="class")
tree.pred<-as.character(tree.pred)
tree.result<-data.frame(id=testID,cuisine=tree.pred)
write.csv(tree.result, "treecookresult.csv", quote = FALSE, row.names = FALSE)

install.packages("randomForestExplainer")
library(randomForestExplainer)

importance_frame<-measure_importance(treecook)
importance_frame
plot_multi_way_importance(importance_frame, size_measure = "no_of_nodes")

##XGBoost
library(xgboost)
library(tictoc)
library(doParallel)
registerDoParallel(cores=4)
##try with booster=gblinear too
Ytrainnum<-as.numeric(Ytrainfac)-1
xg.Xtrain<-xgb.DMatrix(Xtrain,missing=NA,label=Ytrainnum)

tic()
xgbcook <- xgboost(data = xg.Xtrain, 
                   
                 booster = 'gbtree',
                 eta = 0.2, # step size of each boosting step
                 nround = 925,
                 objective = "multi:softmax",
                 eval_metric="merror",
                 max_depth = 25,
                 subsample = 0.5,
                 num_class = 20,
                 colsample_bytree = 0.5)
toc()

xgbcookpred<-predict(xgbcook,Xtest,type="class")
as.character(as.factor(xgbcookpred))
xgb.result<-data.frame(id=testID,cuisine=xgbcookpred2)
write.csv(xgb.result, "xgbcookresult.csv", quote = FALSE, row.names = FALSE)

xgbcookpred2<-plyr::mapvalues(xgbcookpred, from= c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
                             to= c("brazilian","british","cajun_creole","chinese",
                                   "filipino","french","greek","indian","irish","italian","jamaican",
                                   "japanese","korean","mexican","moroccan","russian","southern_us",
                                   "spanish","thai","vietnamese"))




### SVM

library(caret)

svmcook<-train(x=Xtrain,y=Ytrain, method = "svmLinear3")
svm.pred<-predict(svmcook,Xtest)
svm.result<-data.frame(id=testID,cuisine=svm.pred)
write.csv(svm.result, "svmcookresult.csv", quote = FALSE, row.names = FALSE)

### data exploration

#organizing frequency of terms
freq <- colSums(as.matrix(Xtrain))
length(freq)

ord <- order(freq)
ord

#most and least frequent words
freq[head(ord)]
freq[tail(ord)]

#our table of 20 frequencies
head(table(freq),20)
tail(table(freq),20)

wf <- data.frame(word = names(freq), freq = freq)
head(wf)

#plot terms which appear atleast 10,000 times

library(ggplot2)
chart <- ggplot(subset(wf, freq >10000), aes(x = word, y = freq))
chart <- chart + geom_bar(stat = 'identity', color = 'black', fill = 'white')
chart <- chart + theme(axis.text.x=element_text(angle=45, hjust=1))
chart

findAssocs(ingredientsDTM, c('salt','pepper'), corlimit=0.30)
findAssocs(ingredientsDTM, c('cheese','onions'), corlimit=0.30)

#

df<-table(Ytrain)
df
df1<-as.data.frame(df)
ggplot(df1, aes(x=Ytrain))+
  geom_bar()


  



