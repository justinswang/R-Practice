

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

ticket.line<-function(n,change=0,sim.length)
      
{#f
      
      if (! is.numeric(n) ) { stop('argument n needs to be a numetic') } #error check
      if ( n < 1 ) { stop('argument n can not be less than 1') }
      if (! is.numeric(sim.length) ) { stop('argument sim.length needs to be a numeric') } #error check
      if ( sim.length < 1 ) { stop('argument sim.length can not be less than 1') }
      
      
   a= rep(1,n) #people with $5 bill
   b= rep(-1,n)#people with $10 bill
   d= rep(1,change)
   c=c(a,b)
   
   x<-matrix(ncol = 2*n + change, nrow = sim.length )
   y<-matrix(ncol = 2*n + change, nrow = sim.length )
   z<- numeric(sim.length)
   
   
   for (i in 1:sim.length)
   {
         x[i,]<-c(d, sample(c,2*n,replace = F)) #randomly to create samples
         y[i,]<-cumsum(x[i,]) #cumsum
         z[i]<-ifelse(any(y[i,]==-1),0,1)#the ones with -1 is "unsuccess"
         
   }
   
   w=(sum(z)/(sim.length+change))#calculate the probability
   
   return(w)
      
      
}#f

ticket.line(4,change=3,10000)
ticket.line(5,change=10,10000)
ticket.line(6,change=0,10000)


##########################
###    Problem 2       ###
#############################################################################
#   set n as length(class.v)
#   set original h as ("FALSE",NULL,NULL)
#   name the list as err.found, err.loc and new.class
#   set y as vector n 0s
#   set k=1 and l=1
#   
#   set the firt for loop for 1 to n
#   set the second for loop to calculate the distance 
#   rank all the distance as a
#   find the lowest 5 numbers expcept itself. start from 2 to 6
#       
#   sum(class.v[b]==1) > 2, this means at least three nearest points 
#   are coming from class 1. Our target points belong to class 1.
#   (sum(class.v[b]==1) < 3), this means at most two nearest points 
#   are coming from class 1. Our target points belong to class 2.
#     
#   Each time we find a error, k+1 and l+1 set err.found to "TRUE"
#
###################################################




k.nn<-function(k,v.data,t.data)
{#function start
      
      if ( ( k <= 0 )||( k != floor(k) ) )    { stop("k must be integer greater than 0.\n")}           
      if ( k = 1 )                            { stop("In this HW we only test when k is odd number >= 3.\n")}
      if ( !(is.matrix(t.data))||!(is.matrix(v.data)) ) { stop("Both t.data and v.data are required to be matrices.\n")}
      if ( ncol(v.data) !=  ncol(t.data) )              { stop("Number ofs columns in two data set matrixes are required to be the same.\n") } 
      
      n = dim(v.data)[1]
      m=dim(t.data)[1]
      
      y<-vector(mode='numeric',length=m)
      knn.out<-matrix(ncol = k, nrow = n)
      d<-vector(mode='numeric',length=k)
      
      
      for (i in 1:n)
      {# start 1st for
            
            
            for (j in 1:m)
            {#start 2nd for
                  
                  y[j]=sqrt((t.data[j,2]-v.data[i,2])^2 + (t.data[j,3] - v.data[i,3])^2)
                  #calculate the distance
                  
            }#end 2nd for
            
            a=rank(y) # rank the vector in order to find the lowest k
            
            for (h in 1:k)
            {#
                  
                  
                  d[h]<-which(a==h)
                  
            }#
            
            # find the indices of the lowest k distance
            
            
            knn.out[i,]<-d
            
            
            
      }#end for 1st for
      return(knn.out)
      
}#function end





vote<-function(class.id, knn.out)
{#function start
      n=nrow(knn.out)
      d<-vector(mode='numeric',length=k)
      v<-vector(mode='numeric',length=nrow(knn.out))
      p=0
      
      
      for (i in 1:n)
      {# first for
            
            
            b=knn.out[i,]
            
            
            
            if (  sum(class.id[b]==1) >= (k+1)/2 ) 
             # find out which class the point belongs to     
            {
                  v[i] = 1
            }
            
            if (  sum(class.id[b]==1) < (k+1)/2 )
            {
                  v[i] = 2
            }
            
            
            
      }#first for
      
      return(v)
      
      
}#function end
v.data = read.table( "validate.set.1(1).dat",header =T)
t.data = read.table("train.set.1(1).dat",header =T)
class.id=t.data$id


k=3
knn.out=k.nn(k,v.data,t.data)
v=vote(class.id,knn.out)
error.rate=1-sum(v==v.data$id)/nrow(v.data)
error.rate

k=9
knn.out=k.nn(k,v.data,t.data)
v=vote(class.id,knn.out)
error.rate=1-sum(v==v.data$id)/nrow(v.data)
error.rate

k=23
knn.out=k.nn(k,v.data,t.data)
v=vote(class.id,knn.out)
error.rate=1-sum(v==v.data$id)/nrow(v.data)
error.rate
