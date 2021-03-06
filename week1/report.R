
#Chi-square distribution
#�ۑ�1
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

#�ۑ�2
n<-c(10,8,5)
m<-10000
t_dist_vector<-numeric(m)
#���K���z�̗���
X<-rnorm(m)
#�J�C2�敪�z�̗���

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

#�ۑ�3
#��l���z�̗���
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




