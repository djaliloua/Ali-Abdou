rm(list=ls())

# Great Common Divisor between two integers a and b gcd(a,b)
# Recursion method
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




# Checking 

is_prime <- function(n) {
    if(n==2) {
        return(T)
    }
    re <- c()
    for(i in 2:(n-1)) {
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


primes(10) # 2 3 5 7
