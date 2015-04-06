
name="Zhisen wang"


#########################################
######   Assignment 2  #######
#########################################
#################################################
#   we need to find n=dim(pts)[1], this way we can use "for" loop to 
#   calculate the distance between all the points to the center of the circle
#   
#   use "sqrt((pts[i,1]-cntr[1])^2 + (pts[i,2]-cntr[2])^2)<=r" to compare, 
#   all the points within circle, use cbind to assign to a new matrix.
#   
#   
#   drawing a circle,set r as the radius.
#   set xlim ylim xlab ylab
#   first set all the points to coler blue
#   then change the color of "in circle" points to red
#   drawing vertical and horizontal line across the center
#       
#
###################################################



in.circle<-function(pts,cntr,r)
      
{#function start
      
      n=dim(pts)[1] #set n 
      z=NULL
     
      
  for (i in 1:n)
      {#start for
            if (   sqrt((pts[i,1]-cntr[1])^2 + (pts[i,2]-cntr[2])^2)<=r  )
                  #calculate the distance between two points, compare to r
            {
                  z=rbind(z,pts[i,])
            }
      }#end for
      
      
  
  x.axis = c( min(c(pts[,1],cntr[1]-r)) - 1, max(c(pts[,1],cntr[1]+r)) + 1)
  #the min of x , should be the min of the ponits or the min of ccircle
  #max is the same
  
  y.axis = c( min(c(pts[,2],cntr[2]-r)) - 1, max(c(pts[,2],cntr[2]+r)) + 1)
  #the min of y , should be the min of the ponits or the min of ccircle
  #max is the same
  
  theta = seq(0,2*pi,length = 2000)
  x     = r*cos(theta) #here r is the radius
  y     = r*sin(theta)

  dev.new()
  plot(x+cntr[1],y+cntr[2],
       type = 'l', 
       xlim = x.axis, 
       ylim=y.axis,
       xlab="x",
       ylab="y",
       sub="Zhisen Wang")
  
  points(pts, col = "blue") #set all the points to blue
  points(z, col = "red")  #set the points within circle to red
  abline( v = cntr[1], h = cntr[2])
  
  return(z) #return z here
      
}#function end



set.seed(40)
x=runif(14,0,10)
x=round(x,3)
y=runif(14,0,10)
y=round(y,3)
pts=cbind(x,y)

cntr=c(4,5)
r=3.5

in.circle(pts,cntr,r)#testing


r=0.5   #error check (1) if there is no point within the circle
in.circle(pts,cntr,r)


r=10   #error check (2) if all the  points are within the circle
in.circle(pts,cntr,r)




