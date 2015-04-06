name="Zhisen Wang"

################
## Problem 1  ##
#####################################################
# set the length of x and y
# add m-1 0s after x, and reverse the sequence of x 
# add n-1 0s before y 
# wihtin for loop sum(a*b), after each loop add one
# zero before y and only take length(b). That is the 
# convolution.
####################################################


conv <- function(x,y)
{ # 
      if (! is.vector(x) ) { stop('argument x needs to be a vector') } #error check
      if (! is.vector(y) ) { stop('argument y needs to be a vector') } #error check
      
      n = length(x)#set the length of n
      m = length(y)#set the length of m
      z = numeric(n)
      
      b = c(x[n:1],rep(0,(m-1))) #add m-1 0s after x
      a = c( rep(0,(n-1)),y )# add n-1 0s before y
      
      for (i in 1:length(a))
      { # i 
            z[i]=sum(a*b)
            b = c(0,b)  # add one zero in front of b
            b = b[1:length(b)]  # keep the length of length(b)
            
      } # i 
      return(z)
} # 

x = c(rep(1/6,6))
y = c(rep(1/6,6))

for (i in 1:24)
{x= conv(x, y)}
x
sum(x)#check the correction
#P(79<=Y<=96)
sum(x[(79-24):(96-24)])
#P(70<=Y<=105)
sum(x[(105-24):(70-24)])



################
## Problem 2  ##
##################################################
# set binomal x2 and y2
# runing function conv()
# use round() to see if there is difference between
# the new z and dbinom(0:22,22,0.3)
###################################################
n2 = 10;
m2= 12
p = 0.3; 
x2 = dbinom(0:n2,n2,p) # pdf of values over support
y2 = dbinom(0:m2,m2,p)

z=conv(x2,y2)

round(z-dbinom(0:22,22,0.3))



################
## Problem 3  ##
################
n=0:100
y=cos(n*(pi/6)) + n/10
f=c(1,-sqrt(3),1)/(2-sqrt(3))
z=conv(f,y)
plot(y,type='l') #plot the input as a line
lines(z)
lines(c(0,n/10),col='blue')

