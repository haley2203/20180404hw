library(FNN)
set.seed(1)
x <- sort(rnorm(100))
y<- 3+x^2 + rnorm(100)
plot(x, y, pch = 20)

par (mfrow = c(1,1), bg = 'white', col = 'black',
     col.main = 'black', col.axis = 'black', col.lab = 'black', 
     las = 2)

eval.n = 100
eval.point = seq(-3,3, length= eval.n)
plot(x, y, pch = 20)

#k=1
idx.mat<- knnx.index(x, eval.point , k = 5) 
idx.mat
dim(idx.mat)
yhat = rep(0,eval.n)
for (i in 1:eval.n){
   yhat[i]<-mean(y[idx.mat[i,]])

}
y
lines(eval.point , yhat, type= 'l', lty = 1, col = 'red')

#k=2
idx.mat<- knnx.index(x, eval.point , k = 5) 
dim(idx.mat)
yhat = rep(0,eval.n)
for (i in 1:eval.n){
   yhat[i]<-mean(y[idx.mat[i,]])
   
}
lines(eval.point , yhat, type= 'l', lty = 1, col = 'blue')

#k=30
idx.mat<- knnx.index(x, eval.point , k = 30) 
dim(idx.mat)
yhat = rep(0,eval.n)
for (i in 1:eval.n){
   yhat[i]<-mean(y[idx.mat[i,]])
   
}
lines(eval.point , yhat, type= 'l', lty = 30, col = 'chartreuse3')

legend("bottomright", c("k=1", "k=5","k=30"), col = c("red","blue","chartreuse3"),
       pch = 16)

#HW#
#k=1,k=5,k=30으로 plot라인을 하나의 그림에 그리고 레전드를 달기(컬러나 라인스타일로 구분)

rm(list=ls())

