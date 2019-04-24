
#Chi-square distribution
#課題1
n<-c(3,5,10)
m<-100000
chi_sq<-numeric(m)
plot.new()
for(i in 1:3){
  for(j in 1:m){
  x<-rnorm(n[i])
  chi_sq[j]<-sum(x*x)}
  if(i==1){
    hist(chi_sq,breaks=seq(0,50,length=100),freq=F)
  }else{
    hist(chi_sq,breaks=seq(0,50,length=100),freq=F,add=T)  
  }
  
  curve(dchisq(x,n[i]),add=T)
}

#課題2
n<-c(10,8,5)
m<-10000
t_dist_vector<-numeric(m)
#正規分布の乱数
X<-rnorm(m)
#カイ2乗分布の乱数

for(i in 1:3){
  Y<-rchisq(m,n[i])
  
  t_dist_vector<-X/sqrt(Y/n[i])
  
  if (i==1){
    hist(t_dist_vector,breaks=seq(-20,20,length=100),freq=F)  
  }else{
    hist(t_dist_vector,breaks=seq(-20,20,length=100),freq=F,add=T)  
  }
  curve(dt(x,10),add=T)
  
}

#課題3
#一様分布の乱数
n<-c(3,7,10)
m<-10000
Y<-numeric(m)
for(i in 1:3){
  for(j in 1:m){
    Y[j]<-max(runif(n[i], min=0, max=1))
  }
  cat("n =", n[i],"\n")
  cat("var =", var(Y),"\n")
  cat("median =", median(Y),"\n")
  
} 




