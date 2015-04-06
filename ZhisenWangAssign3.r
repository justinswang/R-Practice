

name="Zhisen Wang"

##########################
###    Problem 1       ###
#############################################################################
# set up a vector for people with $5 bill, set all the elements as 1
# set up another vector for people with $5 bill, set all the elements as -1
# merge two vectors together
#
# use sample() to randomly create combinations
# use cumsum() to calculate, if there is any -1 means, there will be no change
#
# use ifelse() to set all the "unsuccessful" result as 0, "successful" ones as 1
# sum all ones, divied by total, will be the probability.
############################################################################

ticket.line<-function(n,sim.length)
      
{#f
   a= rep(1,n) #people with $5 bill
   b= rep(-1,n)#people with $10 bill
   c=c(a,b)
   
   x<-matrix(ncol = 2*n, nrow = sim.length)
   y<-matrix(ncol = 2*n, nrow = sim.length)
   z<- numeric(sim.length)
   
   
   for (i in 1:sim.length)
   {
         x[i,]<- sample(c,2*n,replace = F) #randomly to create samples
         y[i,]<-cumsum(x[i,]) #cumsum
         z[i]<-ifelse(any(y[i,]==-1),0,1)#the ones with -1 is "unsuccess"
         
   }
   
   w=(sum(z)/sim.length)#calculate the probability
   
   return(w)
      
      
}#f

ticket.line(4,10000)
ticket.line(5,10000)
ticket.line(6,10000)



##########################
###    Problem 2       ###
#############################################################################
# use combn(2*n, n) to find all the posibble combinations
# apply on the column of the matrix
# use function() to replace all the 1 to -1, according to (2*n, n)
#
#
# use cumsum() to calculate, if there is any -1 means, there will be no change
# use ifelse() to set all the "unsuccessful" result as 0, "successful" ones as 1
# sum all ones, divied by total, will be the probability.
#
############################################################################

ticket.line.perm<-function(n)
{
      ma<-matrix(ncol = choose(2*n,n), nrow = 2*n)
      y<-matrix(ncol = choose(2*n,n), nrow = 2*n)
      z<- numeric(choose(2*n,n))
      
      ma=apply(combn(2*n, n), 2, function(x){a=rep(1, 2*n);a[x]=-1;a}) 
      # find the all the possible combination, apply the to a by col
      
      
      for (i in 1:choose(2*n,n))
      { 
            y[,i]<-cumsum(ma[,i])  #cumsum
            z[i]<-ifelse(any(y[,i]==-1),0,1) #the ones with -1 is "unsuccess"
      }
      w=sum(z)/choose(2*n,n)
      
      return(w)
}


ticket.line.perm(4)
ticket.line.perm(5)
ticket.line.perm(6)





