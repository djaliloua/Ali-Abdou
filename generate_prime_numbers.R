rm(list=ls())
library(tidyverse)
even <- function(n) {
    result <- c()
    for(i in seq(1,n)) {
        if(i%%2==0) {
            result[i] <- i
        }
    }
    return(result[!is.na(result)])
}
length(even(10))
even(15)

rep(seq(0,4),each=5)
rep(seq(1,5),5)
hist(c(8,12,23,43,23,12),col=rainbow(7))

x <- seq(1,20)
y <- x^2+2*x++cos(x)
plot(x,y,type='b')
?plot
require(stats)
plot(cars)
lines(lowess(cars))
colnames(cars)
lm <- lm(dist~speed,data=cars)
summary(lm)
plot(lm)
r <- lm$residuals
shapiro.test(r)
bartlett.test(lm)

gcd <- function(a,b) {
    if(a==0) {
        return(b)
    }
    if(a%%b!=0) {
        return(gcd(b,a%%b))
    } else {
        return(b)
    }
}




# Prime numbers

is_prime <- function(n) {
    if(n==2 | n==3) {
        return(T)
    }
    re <- c()
    for(i in 4:(n-1)) {
        re[i] <- gcd(n,i)
    }
    re <- re[!is.na(re)]
    if(all(re==1)) {
        return(T)
    } else {
        return(F)
    }
}
is_prime(2)
is_prime(11)
is_prime(22)

is_prime(101)

primes <- function(n) {
    p <- c()
    for(i in 2:n) {
        if(is_prime(i)==T) {
            p[i] <- i
        }
    }
    return(p[!is.na(p)])
}


primes(1000)
primes1 <- function(n) {
    p <- 2:n
    re <- p[which(map(p,is_prime))]
    return(re)
}
primes1(10)[[1]]

lm1 <- lm(log(dist)~speed,data=cars)
summary(lm1)
plot(lm1)
r1 <- lm1$residuals
shapiro.test(r1)
bartlett.test(dist~speed,data=cars)
ansari.test(cars[,'speed'],cars[,'dist'],conf.int = T)
x <- c(2,3,1,2,3,2,1,2,3,2,1,22,3)
factor(x)
levels(x)
which((1:8)%%2==0)
df <- data.frame(A=round(rnorm(100),2),Class=as.factor(rbinom(100,1,0.5)),C=round(rnorm(100,0,1),2))
 
df
df %>% 
    select(A,C,Class) %>% 
    group_by('Class') %>% 
    summarise(sum=n())
df %>% 
    select(A,C) %>% 
    ggplot(aes(x=A,y=C))+
    geom_point()
