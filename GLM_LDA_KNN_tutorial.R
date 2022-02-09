library(ISLR)
names(Smarket)
summary(Smarket)

pairs(Smarket,col=Smarket$Direction)

### logistic regression

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket,family=binomial)
summary(glm.fit)   ##none of coeficient are significant here but can make good predictions

glm.probs=predict(glm.fit,type="response")
glm.probs[1:5] ##close to 50% we dont expect strong predictions based on this data

glm.pred=ifelse(glm.probs>0.5,"Up","Down")
attach(Smarket)

table(glm.pred,Direction)
mean(glm.pred==Direction)  ##proportion of correctly fitted data

## make training and test set

train =Year<2005
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket,family=binomial, subset=train)
glm.probs = predict(glm.fit,newdata=Smarket[!train,],type="response")
glm.pred=ifelse(glm.probs>0.5,"Up","Down")
Direction.2005=Smarket$Direction[!train]

table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005) ## less accurate we might be overfitting the data


## Fit smaller model

glm.fit=glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fit,newdata=Smarket[!train,],type="response")
glm.pred=ifelse(glm.probs>0.5,"Up","Down")
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005) ## prediction improved to 56%

summary(glm.fit)

## part 2 

library(MASS)

## Linear Discrimination Analysis

lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=Year<2005)
lda.fit ## probabilities 50% up 50% down

plot(lda.fit)
Smarket.2005=subset(Smarket,Year==2005)
lda.pred=predict(lda.fit,Smarket.2005)
lda.pred[1:5,] ## not a matrix so this index does not work lets solve it
class(lda.pred) ## its list so lets use data.frame
data.frame(lda.pred)[1:5,]
table(lda.pred$class,Smarket.2005$Direction)
mean(lda.pred$class==Smarket.2005$Direction)

## nearest Neighbors

library(class)
?knn
attach(Smarket)
Xlag=cbind(Lag1,Lag2) ## make a metrix

train=Year<2005
knn.pred=knn(Xlag[train,],Xlag[!train,],Direction[train],k=1) ## k=1 means>>> 1 nearest neighbour
table(knn.pred,Direction[!train])
mean(knn.pred==Direction[!train]) ## exacctly 50% so k=1 was not too great
## lets try a bigger k k=2 & k=5

knn.pred=knn(Xlag[train,],Xlag[!train,],Direction[train],k=2) ## k=2
table(knn.pred,Direction[!train])
mean(knn.pred==Direction[!train]) ## 6% increase in prediction success

knn.pred=knn(Xlag[train,],Xlag[!train,],Direction[train],k=4) ## k=4
table(knn.pred,Direction[!train])
mean(knn.pred==Direction[!train]) ## 1% decdrease in prediction success


