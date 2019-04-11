
#Chi-square distribution
#課題1
n<-10
m<-100000
chi_sq<-numeric(m)

for(i in 1:m){
  x<-rnorm(n)
  chi_sq[i]<-sum(x*x)}

hist(chi_sq,breaks=seq(0,50,length=100),freq=F)
curve(dchisq(x,10),add=T)

#課題2
n<-10
m<-10000
t_dist_vector<-numeric(m)
#正規分布の乱数
X<-rnorm(m)
#カイ2乗分布の乱数
Y<-rchisq(m,n)

t_dist_vector<-X/sqrt(Y/n)

hist(t_dist_vector,breaks=seq(-10,10,length=100),freq=F)
curve(dt(x,10),add=T)

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




