#This assignment I would love to find the relation among Poisson, Exponential and Gamma Distribution


#Definition of three distributions:
#Poisson distribution is often referred to as the "distribution of rare events". It is also the distribution for count data.
#Exponential distribution is the probability distribution that describes the time between events in a Poisson process.
#Gamma Distribution arises naturally in processes for which the waiting times between events are relevant.


library(ggplot2)
library(qualityTools)
library(MASS)

lambda <- 5 # set lambda for 5 for now

#Histogram with curve of exponential distribution at lambda=5

x.gen <- rexp(1000, rate = lambda)# draw a random exponential sample with size 1000 and lambda = lambda
hist(x.gen, prob = TRUE)
x.est <- fitdistr(x.gen, "exponential")$estimate
curve(dexp(x, rate = x.est), add = TRUE, col = "red", lwd = 2)

      
      
#Part 1 :
#I want to show that the number of events divided by exponential waiting times are Poisson distributed.
      
      
# create a vector of exponential waiting times which total t <= Max with lambda = lam
#The first part of  the function is input check
set.seed(50)

wait.until <- function(Max,lam){
 if(!is.numeric(Max)) {
   stop("Max should be numeric")
 }
  if(Max <= 0){
  stop("Max should be positive")
 }
  if(!is.numeric(lam)) {
    stop("lam should be integer")
  }
  if((lam > 0) == FALSE) {
    stop("lam should be positive")
  }
   time = 0
   a = NULL
   while(time < Max){
   inter = rexp(1,lam)
   a = c(a,inter)
   time = time + inter
  }
     return(a[1:(length(a)-1)])  
  }
      

# now simulate the number of events 

poi.test <- function(rep, Max, lam){
  if(is.integer(rep) == FALSE) {
    stop("rep should be integer")
  }
    if((rep > 0) == FALSE){
      stop("rep should be positive")
    }
    if(is.numeric(Max) == FALSE){
      stop("Max should be numeric")
    }
    if((Max > 0) == FALSE){
      stop("Max should be positive")
    }
    if(is.numeric(lam) == FALSE){
      stop("lam should be numeric")
    } 
    if((lam >0) == FALSE){
      stop("lam should be positive")
    } 
     a = NULL
     for(i in 1:rep){
     q = wait.until(Max,lam)
     a = c(a,length(q))
   }
    return(a)
}

#Create a summation of 1000 exponential distributed event
  divexp <- poi.test(1000L,9,5)

#Calculate its mean and variance
  mean(divexp)
  var(divexp)
        
#Compare the graph I get from the sample above 
#to the graph I get from random poisson distribution sample.
    
  par(mfrow=c(1,2)) # set a two by two graph format
        
#Histogram with curve of the density function of sample above
   hist(divexp,prob=TRUE,mai = "Graph of the Sample")
   d <- density(divexp) # I use the density function since the distribution is unknown
   lines(density(divexp), col="blue", lwd=2)

#Histogram with curve of poisson distribution at lambda=45
    y.gen <- rpois(1000,45) #set the lambda equal to 45 to make the x-axis same as my sample
    hist(y.gen, prob = TRUE,main = "Graph of Poisson distribution")
    
    y.est <- fitdistr(y.gen, "poisson")$estimate
    curve(dpois(x,y.est), add =T, col = "red",from =20,to=70,n=51,lwd=2) # I use the dpois function since it is poison distributed
    mean(y.gen)
    var(y.gen)
    
#As we can see from the mean and the variance, the mean and the variance of the sample
#is 45.355 and 46.041 which is close to the mean and variance of poisson distribution 
#with n=9 and lambda=5, which has both mean and variance equal to 9*5=45.
#Also from comparison, the two graphs from different samples are pretty much alike.
#Then we can conclude that the number of events divided by exponential waiting times are Poisson distributed.
            
      
#Part 2:
            
#I want to check the time until kth event occurs follows gamma distribution.
         
#create a vector of exponential waiting counts which total count < k with lambda = lam
wait.for <- function(k, lam){
      
   if(is.integer(k) == FALSE){
     stop("k should be integer")
   } 
   if((k > 0) == FALSE){
     stop("k should be positive")
   } 
   if(is.numeric(lam) == FALSE){
     stop("lam should be numeric")
   } 
   if((lam >0) == FALSE){
     stop("lam should be positive")
   }
              
   time = 0
   count = 0
   a = NULL
   while(count < k){
   inter=rexp(1,lam)
   count = count + 1
   time = time+inter
   }
                  
    return(time)
} 
            
# now simulate the number of events
              
gam.test <-function(rep, max.e, lam ){
    
     if((is.integer(rep)) == FALSE){
       stop("rep should be integer")
     }
     if((rep > 0) == FALSE){
       stop("rep should be positive")
     }
     if(is.integer(max.e) == FALSE) {
       stop("max.e should be integer")
     }
     if((max.e > 0) == FALSE){
       stop("max.e should be positive")          
     } 
     if(is.numeric(lam) == FALSE){
       stop("lam should be numeric")
     } 
     if((lam >0) == FALSE) {
       stop("lam should be positive")
     }            
     a=NULL
     for (i in 1:rep){
     t = wait.for(max.e,lam)
     a = c(a,t)
                       
   }
                
    return(a)
}
              
sumexp <- gam.test(1000L,9L,5)
    
#Calculate its mean and variance
mean(sumexp)
var(sumexp)
                
#Compare the graph I get from the sample above 
#to the graph I get from random poisson distribution sample.
                  
par(mfrow=c(1,2)) # set a two by two graph format
                
#Histogram with curve of the density function of sample above
hist(sumexp,prob=TRUE,main ="Graph of the Sample")
d <- density(sumexp) # returns the density data 
lines(density(sumexp), col="blue", lwd=2)
                
#Histogram with curve of gamma distribution at lambda=5
z.gen <- rgamma(1000,9,5)
hist(z.gen, prob = TRUE,main="Graph of Gamma Distribution")
z.est <- fitdistr(z.gen, "gamma")$estimate
curve(dgamma(x,shape = z.est[1],rate = z.est[2]),add =TRUE, col = "red",lwd=2)
                  
                  
#As we can see from the mean and the variance, the mean and the variance of the sample
#is 1.76697 and 0.3345808 which is close to the mean and variance of gamma distribution 
#with alpha=9 and lambda=5, which has mean=9/5=1.8 and variance = 9/5^2 = 0.36.
#Also from comparison, the two graphs from different samples are pretty much alike.
#Then we can conclude that the number of events divided by exponential waiting times are Poisson distributed.