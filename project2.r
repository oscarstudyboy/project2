pvalues=function(p0,phatstar,n,N){
  result=replicate(N,mean(rbinom(n,1,p0))) 
  pvalue=mean(result<phatstar) 
  return(pvalue) 
}

#1: head, 2: tail
#####
#Distribution
#####
# 1. Binomial/Bernoulli
# 2. Proportion of head?
#coin.experiment <- sample(c(0,1), 50, replace = TRUE, prob = c(0.6, 0.4))
#given sample
results <- c(0,0,0,1,1,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,1,0,0,0,0,0,0)

mean(results == 1)
freq <- table(results)
freq.table <- freq/sum(freq)
mean(results)
median(results)
mean(results, trim = 0.1)
range(results)
var(results)
sd(results)
summary(results)#five number summary

barplot(freq.table, main = "Distribution", xlab = "Values", ylab = "probability")
boxplot(freq.table)

pvalues(0.4,0.16,50,1000)
# Should we reject John’s claim that p = 0.4?
# How strong is the evidence provided by the data for the rejection?
# What are the possible fairness values, i.e., proportion to have Heads?

####################
####What if john's claim is right
####################
phatstar = 0.16

virtual <- replicate(1000, rbinom(50, 1, 0.4))
phat50s = colSums(virtual)/50
hist(phat50s, prob = TRUE, main = "Histogram of Phat50s")
points(phatstar, 0, col = "red", pch = 20)

#####################
####Strenght of the argument
#####################

#question 5
pp=seq(0,1,by=0.01) 
pvalue_collection=rep(0,101) 
for(i in 1:101){ 
  pvalue_collection[i]=pvalues(pp[i],0.16,50,10000) 
} 
pvalue_collection
subset(pp, pvalue_collection >= 0.05)

#question 6
intuition_collection=rep(0,101) 
for (i in 1:101){
  if(abs(0.16-pp[i]) <= 2*sqrt(pp[i]*(1-pp[i])/50)){
    # print(pp[i]) # Debug line
    intuition_collection[i] <- pp[i]
  }
}
subset(intuition_collection, intuition_collection != 0)


########
#J_spoon_pot <- replicate(10000, mean(rbinom(50, 1, 0.4)))
#mean(J_spoon_pot)
#sd(J_spoon_pot)
#barplot(table(J_spoon_pot)/10000,main = "Distribution of phat for J-coin", xlab = "phat", ylab = "probability")
#points(phatstar, 0, col="red", pch=20)
#mean(J_spoon_pot<phatstar)
########

