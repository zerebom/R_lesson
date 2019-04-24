#大数の弱法則、中心極限定理

n<-c(10,50,250,1250);m<-10000;mu<-0
data <-numeric(m)
op<-par(mfrow=c(2,2))
for(j in 1:4){
  for(i in 1:m){
    data[i]<-mean(rnorm(n[j]))-mu
  }
  #"main" argment defined title of graph
  hist(data,breaks = seq(-2,2,length=50),main=paste('n=',n[j]))
}

#課題1

n<-c(2,50,250,1250);m<-10000;mu<-0;var<-1/3
data <-numeric(m)
#graphic paramater
op<-par(mfrow=c(2,2))
for(j in 1:4){
  for(i in 1:m){
    data[i]<-(mean(runif(n[j]))-mu)/(sqrt(var)/sqrt(n[j]))
  }
  #"main" argment defined title of graph
  hist(data,breaks = seq(min(data),max(data),length=50),main=paste('n=',n[j]))
}


#相関のある正規乱数の生成
n<-10000;rho<- 0.3
u<-rnorm(n);v<-rnorm(n)
x<-u
y<-rho*u+sqrt(1-rho*rho)*v
plot(x,y)

#不変推定量
n<-10
x<-rnorm(n)
var(x)
y<-(x-mean(x))*(x-mean(x))
mean(y)*n/(n-1)

#課題3
m<-1000
n<-c(5,50,100,1000)
y1<-numeric(m)
y2<-numeric(m)

variance <- function(x) var(x)*(length(x)-1)/length(x)   # 標本分散を求める関数を定義

for (j in 1:4) {
  for(i in 1:m){
    x<-rnorm(n[j],mean=1.0,sd=2)
    y1[i]<-var(x)
    y2[i]<-variance(x)
  }
  
  hist(y1,breaks=seq(0,max(y1),length=100),col='blue',main=paste('n=',n[j]))
  hist(y2,breaks=seq(0,max(y2),length=100),add=T,col='red')
  
}

#4.2あたり
count<-0
alpha<-0.01
for(i in 1:100){
  n<-10
  x<-rnorm(n,mean=1.0)
  z<-mean(x)*sqrt(n)
  {if(abs(z)>=qnorm(alpha/2,lower.tail=FALSE)){
    print("Rejected")
    conunt<-count+1
  }else{
    print('not rejected')
  }
  }
}

x<-c(-0.655,1.13,2.90,-0.339,1.969,-0.46,1.74,1.81)
t.test(x)


#課題7
qt(0.975,8,lower.tail = TRUE)

#課題8
help(t.test)