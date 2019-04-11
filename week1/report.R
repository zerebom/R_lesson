
n=10
m=100000
chi_sq<-numeric(m)

for(i in 1:m){
  x<-rnorm(n)
  chi_sq[i]<-sum(x*x)}
hist(chi_sq,freq=F)



