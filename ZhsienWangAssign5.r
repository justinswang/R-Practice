
name="Zhisen Wang"




#########################################
######   Assignment 5  #######
#########################################
#################################################
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



detect.missclass<-function(class.v,p)
{#function start

      n = length(class.v)
      h=list("FALSE",NULL,NULL)#set the defaut input
      names(h)=c("err.found","err.loc","new.class")#name them
      y<-vector(mode='numeric',length=n)
      
      k=1
      l=1
      
      for (i in 1:n)
      {# start 1st for
            
           
            for (j in 1:n)
            {#start 2nd for

                  y[j]=sqrt((p[j,1]-p[i,1])^2 + (p[j,2] - p[i,2])^2)
                  #calculate the distance
                  
            }#end 2nd for
            
            a=rank(y) # rank the vector in order to find the lowest 5
            
            b=c(which(a==2),which(a==3),which(a==4),which(a==5),which(a==6))
            # find the indices of the lowest 5 distance
            # since which==1 is the target point itself
            # so started on which==2
            

              if (  (sum(class.v[b]==1) > 2) && (class.v[i]==2) )
            #   sum(class.v[b]==1) > 2, this means at least three nearest points 
            #   are coming from class 1. Our target points belong to class 1.
            {     
                  h[[1]][1]="TRUE"
                  h[[2]][k]=i #add new loc on list
                  k=k+1
                  h[[3]][l]=1 #add new class on list
                  l=l+1
            }
            

              if (  (sum(class.v[b]==1) < 3) && (class.v[i]==1) )
            #   (sum(class.v[b]==1) < 3), this means at most two nearest points 
            #   are coming from class 1. Our target points belong to class 2.
            {     
                  h[[1]][1]="TRUE"
                  h[[2]][k]=i #add new loc on list
                  k=k+1
                  h[[3]][l]=2 #add new class on list
                  l=l+1
            }
            
            
      }#end for 1st for
      return(h)
}#function end


detect.missclass(class.vec,p)
