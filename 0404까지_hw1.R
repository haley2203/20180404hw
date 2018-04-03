x=c(17, 16, 20, 24, 22, 15, 21, 18)
x

#1.(1)
x[x>=20]

#1.(2)
x[x>=20]=100
y = x
y

#2.(1)
x=matrix(0,5,5)
for(i in 1:5){
   for(j in 1:5){
      if(i==j){
         x[i,j]=i+2
      }
      else {
         x[i,j]=-1
      }  
   }
}
x
#2.(2)
y=x[,-5]
y
#2.(3)
yinfo=dim(y)
yinfo
#2.(4)
y[y==-1]=0
y1=y
y1

#3.(1)
rdata = read.table("c:/tmp/rowdata.txt",header = T,sep=",")
rdata
#3.(2)
is.na.data.frame(rdata)
#3.(3)
a=0
n=1
for(i in 1:101){
   if (is.na(rdata$v2[i])==F & is.na(rdata$v3[i])==F){
      a[n] = i
      n = n+1
   } 
}
a
#3.(4)
rdata1=rdata[a,]
rdata1

#4.(1)
vec1 = c(T,T,T,F,F,T,T,T)
mat = matrix(c(1,0,0,1),2,2)
vec2 = seq(0,1,length.out = 100)
l4 = 1
l5 = 2
l6 = 3
l7 = 4
temp = list(vec1,mat,vec2,l4,l5,l6,l7)
temp
#4.(2)
temp = temp[-2] #mat disappear
#4.(3)
temp[3]
#4.(4)
length(temp)

#5.(1)
a1 = -1:2
a2 = 1:2
a1;a2
a1 + a2

#5.(2)
a1 = -(1:2)
a2 = 1:2
a1;a2
a1 + a2

#5.(3)
a1 = matrix(0,2,2)
a2 = c(3,4)
a1;a2
a1 + a2

#5.(4)
a1 = matrix(1:4,2,2)
a1
a1[a1>2] = 0
a1
#5.(5)
a1 = 1:5
a1[-1] - a1[-length(a1)]
a1
#----------------------------
#R-Programming
#1
a=0
a[1]=1
a[2]=3
for(n in 1:18){
   a[n+2] = 0.9*a[n+1] - 0.1*a[n] +1
}
a[20]
#2
for(n in 1:100){
   a[n+2] = 0.9*a[n+1] - 0.1*a[n] +1
   if(a[n+2]>4){
      cat("n =",n+2,"\n")
      return(0)
   }
}
#3
A=matrix(runif(100),50,5)
head(A)
s_rowMean = function(x)
{
   if ( class(x) != "matrix") stop("this is not a matrix")
   v = rep(0, nrow(x))
   for ( i in 1:nrow(x))
   {
      v[i] = mean( x[i,] )
   }
   return(v)  
}
s_rowMean(A)

#4
tmp = rep(0, 10)
a = 10:1
idx = 1
for ( j in a){
   if (j<5)   {
      tmp[idx] = a[j]
      idx = idx + 1
   }
}
tmp

#5
x = matrix(rbinom(5000,10, 1/2),1000,5)
head(x)
class(x)
dim(x)
sid=matrix(0,dim(x)[1],1)
for(i in 1:dim(x)[1]){
   sid[i] = sample(1:10,1,replace=T)
}
x=cbind(x,sid)
head(x)
#6_1
m.mat = matrix(0,10,5)
for(j in 1:10){
   sid_j = x[(x[,6] == j),]
   m.mat[j,] = colMeans(sid_j)[1:5]
}
m.mat

#6_2 not finish
distance5 = function(x,y){ 
   pxy = as.numeric(x%*%y) 
   px = sqrt(as.numeric(x%*%x))
   py = sqrt(as.numeric(y%*%y)) 
   return(pxy/(px*py)) 
} 
distance5(x[1,1:5],m.mat[1,]) 

a = matrix(0, 1000, 10) 
for(i in 1:1000){ 
   for(j in 1:10) 
      a[i,j] = distance5(x[i,1:5],m.mat[j,]) 
} 
idist = a 
head(idist) 


#7 
ivec = rep(0, 1000) 
for(i in 1:1000){ 
   ivec[i] = min(idist[i,]) 
} 
head(ivec)

#8
set.seed(1)
a = list()
for (i in 1:1000){
   x = rpois(1,4)+1
   x = min(x,10)
   a[[i]] = sample(1:10, x)
}
head(a)
#8.(1)
b=c(rep(0,9))
for(j in 1:9){
   for(i in 1:1000){
      if(length(a[[i]]) == j+1){
         b[j] = b[j] + 1
      }
   }
}
b
sum(b)
#8.(2)
a[[1]]
a[[1]][1]
score=c(rep(0,10))
for(i in 1:1000){
   if(length(a[[i]])>=2 & length(a[[i]])<=3){
      score[a[[i]][1]] = score[a[[i]][1]] + 1 
   }
   else if(length(a[[i]])>=4 & length(a[[i]])<=6){
      score[a[[i]][1]] = score[a[[i]][1]] + 2 
      score[a[[i]][2]] = score[a[[i]][2]] + 1 
   }
   else if(length(a[[i]])>=7 & length(a[[i]])<=10){
      score[a[[i]][1]] = score[a[[i]][1]] + 3 
      score[a[[i]][2]] = score[a[[i]][2]] + 2 
      score[a[[i]][3]] = score[a[[i]][3]] + 1 
   }
}
score
max(score)

#9.(1)
set.seed(1)
m1 = 10
m2 = 5
num = 4

rbinom(1, 1, 1/2)
for(i in 1:num){
   if(rbinom(1,1,1/2) == 0){
      m1 = m1 - 1
      m2 = m2 + 1
   }
   else {
      m1 = m1 + 1
      m2 = m2 - 1
   }
   cat("num=",i,",m1=",m1,",m2=",m2,"\n")
}
m1;m2
#9.(2)
set.seed(1)
m1 = 10
m2 = 5
num = 1000
i=0
while(m1 > 0 & m2 > 0){
   i=i+1
   if(rbinom(1,1,1/2) == 0){
      m1 = m1 - 1
      m2 = m2 + 1
   }
   else {
      m1 = m1 + 1
      m2 = m2 - 1
   }
#   cat("num=",i,",m1=",m1,",m2=",m2,"\n")
   if(m1 == 0 | m2 == 0){
      cat("finish num=",i,'\n')
      if(m1 == 0){
         cat("B Win m2=",m2,"\n")
      }
      else if(m2 == 0){
         cat("A Win m1=",m1,"\n")
      }
      break
   }
}
m1;m2

#9.(3)
howmany=c(0,0)
for(k in 1:200){
   set.seed(k)
   m1 = 10
   m2 = 5
   while(m1>0 & m2>0){
      if(rbinom(1,1,1/2) == 0){
         m1 = m1 - 1
         m2 = m2 + 1
      }
      else {
         m1 = m1 + 1
         m2 = m2 - 1
      }
      #cat("num=",i,",m1=",m1,",m2=",m2,"\n")
      if(m1 == 0 | m2 == 0){
        # cat("finish num=",i,'\n')
         if(m1 == 0){
         #   cat("B Win m2=",m2,"\n")
            howmany[2] = howmany[2] + 1
         }
         else if(m2 == 0){
          #  cat("A Win m1=",m1,"\n")
            howmany[1] = howmany[1] + 1
         }
         break
      }
   }
}
m1;m2
howmany

#10
howmany=c(0,0)
for(k in 1:200){
   set.seed(k)
   m1 = 10
   m2 = 25
   while(m1>0 & m2>0){
      if(rbinom(1,1,1/2) == 0){
         m1 = m1 - 1
         m2 = m2 + 1
      }
      else {
         m1 = m1 + 1
         m2 = m2 - 1
      }
      #cat("num=",i,",m1=",m1,",m2=",m2,"\n")
      if(m1 == 0 | m2 == 0){
         #cat("finish num=",i,'\n')
         if(m1 == 0){
          #  cat("B Win m2=",m2,"\n")
            howmany[2] = howmany[2] + 1
         }
         else if(m2 == 0){
           # cat("A Win m1=",m1,"\n")
            howmany[1] = howmany[1] + 1
         }
         break
      }
   }
}
r=howmany[1]/(howmany[1]+howmany[2]) #m1=25
r
