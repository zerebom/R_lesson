#‰Û‘è1

n<-c(2,50,250,1250);m<-10000;mu<-0;var<-1/3
data <-numeric(m)
#graphic paramater
op<-par(mfrow=c(2,2))
for(j in 1:4){
  for(i in 1:m){
    X=runif(n[j],min=-1.0,max=1.0)
    data[i]<-mean(X)/(sqrt(var)/sqrt(n[j]))
  }
  #"main" argment defined title of graph
  hist(data,breaks = seq(min(data),max(data),length=50),main=paste('n=',n[j]))
}


#‰Û‘è3
m<-1000
n<-c(5,50,100,1000)
y1<-numeric(m)
y2<-numeric(m)

variance <- function(x) var(x)*(length(x)-1)/length(x)   # •W–{•ªŽU‚ð‹‚ß‚éŠÖ”‚ð’è‹`

for (j in 1:4) {
  for(i in 1:m){
    x<-rnorm(n[j],mean=0,sd=1)
    y1[i]<-var(x)
    y2[i]<-variance(x)
  }
  
  hist(y1,breaks=seq(0,max(y1),length=100),col='blue',main=paste('n=',n[j]))
  hist(y2,breaks=seq(0,max(y2),length=100),add=T,col='red')
  
}

x<-c(-0.655,1.13,2.90,-0.339,1.969,-0.46,1.74,1.81)
t.test(x)

#‰Û‘è7
qt(0.975,7,lower.tail = TRUE)
t<-abs(qt(0.05/2,7))
SE<-sd(x)/sqrt(8)
lower<-mean(x)-t*SE
upper<-mean(x)+t*SE


#‰Û‘è8
help(t.test)