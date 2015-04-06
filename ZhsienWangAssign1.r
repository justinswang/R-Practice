
name="Zhisen wang"



#########################################
######   Assignment 1, problem 1  #######
#########################################
#################################################
#   set n as the length of in1
#   set m as the length of in2
#   define x as a vector with length of m+n
#   set i j k equal to 1
#   
#   Firstly, sort the elements while i<=n and j<=m
#   if in1[i]<=in2[j], assign the value x[k]=in1[i], at the same time
#   i plus 1, since one element already picked . Whereas, x[k]=in2[j], 
#   j plus 1, since one element already picked. Whenever, i or j plused 1
#   k need to plus 1, since one element has been assigned to x.
#       
#   Secondly, while i still less or equal to n, means that i is still left. 
#   we assign x[k]=in1[i], same, i plus 1, k plus 1
#     
#   Thirdly, while j still less or equal to m, means that j is still left. 
#   we assign x[k]=in2[j], same, j plus 1, k plus 1
#
#   we are now finished our sorting.
###################################################

merge.sort<-function(in1,in2)
{#function start
      
      n=length(in1)#length of in1
      m=length(in2)#length of in2
    
      x<-vector(mode='numeric',length=m+n)
      i=1
      j=1
      k=1
      
      while ( i<=n && j<=m) # sort in1 and in2 at same time
      {#first while start
            if (in1[i]<=in2[j]){
                  x[k]=in1[i]
                  i=i+1
            } else {
                  x[k]=in2[j]
                  j=j+1
            }
            k=k+1
      }#first while end
      
      while (i<=n){#if in1 is still left
            x[k]=in1[i]
            i=i+1
            k=k+1
      }#second while end
      while (j<=m){#if in2 is still left
            x[k]=in2[j]
            j=j+1
            k=k+1
      }#third while end

      

return(x)#now y is sort with ascending order
}#function end

merge.sort(c(1,2,3,4),c(1.5,3,5))

#error check
merge.sort(c(1,2,3,Inf),c(1.5,3,5))




#########################################
######   Assignment 1, problem 2  #######
#########################################
#################################################
#   set n as the length of x
#   set m as the length of bins
#   name a y for our result, pre-assigned value 0, with length of m+1
#   
#   
#   Firstly, set up for loop, i from 1 to n, if x[i]<=bins[1], add 1 count 
#   to y[1]. if x[i]<=bins[1], add 1 count to y[m+1], then we finished the 
#   assignment of first and last bin.
#       
#   Secondly, within the first loop, we set up a second for loop, j from 
#   1 to m-1(since we already assigned two). if (bins[j]<x[i]&x[i]<=bins[j+1]),
#   then add 1 count to y[j+1]
#
#
#   we are now finished our bins  assignment.
###################################################



bin.data<-function(x,bins)
{#fucntion start
      
      n=length(x)
      m=length(bins)
      y=rep(0,m+1) #name a y for our result
for (i in 1:n)
{#start first for !!!
      if (x[i]<=bins[1])  #for the first bin(-Inf,bin[1]]
            {#start first if 
            y[1]=y[1]+1
            }else{#start first else %%%
      if(x[i]>bins[m])  #for the last bin(bin[m],Inf]
      {
            y[m+1]=y[m+1]+1
            }else{#start second else @@@
for (j in 1:(m-1)) #assign all the values to the middle bins
{#start second for $$$
      if (bins[j]<x[i]&x[i]<=bins[j+1])
      {#start if within second for ---
            y[j+1]=y[j+1]+1
      }#end if within second for ---
}#end second for $$$
      
            }#end second else @@@
      }#end first else %%%
      
}#end first for !!!
return(y)
}#function end

x<-c(8.3,-2,2.3,7.9,2.5,2.51,8.5,-8.9,9.2)
bins<-c(2.5,5,7.8,9)
bin.data(x,bins)

#error check
x<-c(8.3,-2,2.3,Inf,2.5,2.51,8.5,-8.9,9.2)
bins<-c(2.5,5,7.8,9)
bin.data(x,bins)

