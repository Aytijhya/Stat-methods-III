library(dplyr)
n=20
X <- as.matrix(expand.grid(0:n, 0:n))
X <- X[rowSums(X) <= n,]
X=as.data.frame(X)
f=function(v){
  Y=as.matrix(expand.grid(0:(n-sum(v)), 0:(n-sum(v))))
  Y=Y[rowSums(Y) == n-sum(v),]
  if(sum(v) < n) {
    u=c(rep(v[1],nrow(Y)),rep(v[2],nrow(Y)))
    dim(u)=c(nrow(Y),2)
    return(cbind(u,Y))
  }
  else
    return(c(v,c(0,0)))
}
data=data.frame()
data=f(X[1,])
for (i in 2:nrow(X)) {
  data=rbind(data,f(X[i,]))
  print(i)
}
data=t(apply(data,1,as.numeric))
prob=apply(data,1,function(x) dmultinom(as.numeric(x), prob = rep(0.25,4)))
p=function(v) (sum(v^2)-10*sum(v)+100)/5
stat=apply(data,1,p)
data=cbind(data,stat,prob)
colnames(data)= c('a','b','c','d','stat','prob')
data=data %>% as.data.frame %>%arrange(stat) %>% mutate(cprob= cumsum(prob))

randomized=data[460:531,1:4]
rejected=data[532:1771,1:4]
a=(data[531,7]-0.95)/sum(as.vector(data[460:531,6]))
power=sum(apply(rejected,1,function(x) dmultinom(as.numeric(x), prob=c(0.3,0.3,0.3,0.1))) %>% as.vector)
power=power+a*sum(apply(randomized,1,function(x) dmultinom(as.numeric(x),prob= c(0.3,0.3,0.3,0.1))) %>% as.vector)
print(power)


