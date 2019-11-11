library(MASS)
data()
w =ChickWeight
head(w)
library(tidyverse)
w %>% ggplot(aes(y =weight,x =""))+geom_boxplot()+geom_point(position ='jitter',alpha =0.4)
library(data.table)
setDT(Orange)
Orange %>% ggplot(aes(x =age,y =circumference))+geom_point(alpha =0.3)


data(diamonds)
head(diamonds)
diamonds %>% ggplot(aes(x = log(price)))+geom_density()

P <-seq(0.05,0.99,0.05)
actual =quantile(log(diamonds$price),P)
theoretic = qnorm(P,mean =mean(log(diamonds$price)),sd =sd(log(diamonds$price)))
plot(actual,theoretic)
abline(0,1)


h<-function(y){
  #x<-x*2.54
  y <-y*2.54
  if(y>50){return("jinga")}
  else{return("Yah")}
  
}
h(3)
map(c(2,89,100),h)
reduce(c(23,103,56,100,78),(function(x) x>10))
