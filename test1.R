set.seed(124)
schtyp<-sample(0:1,20,replace=TRUE)
schtyp
is.factor(schtyp)
is.numeric(schtyp)
schtyp.f<-factor(schtyp,labels=c("private","public"))
schtyp.f
is.factor(schtyp.f)
ses <- c("low", "middle", "low", "low", "low", "low", "middle", "low", "middle",
         "middle", "middle", "middle", "middle", "high", "high", "low", "middle",
         "middle", "low", "high")
is.factor(ses)
is.character(ses)
ses.f<-factor(ses)
ses.f
levels(ses.f)
is.factor(ses.f)
ses.ff<-factor(ses,levels=c("low","middle","high"))
levels(ses.ff)
ses.order<-ordered(ses,levels=c("low","middle","high"))
ses.order
is.factor(ses.order)
ses.ff<-factor(ses.ff, levels=c(levels(ses.ff),"very.high"))
is.factor(ses.ff)
ses.ff[21]<-"very.high"
ses.ff
ses.ff.new<-ses.ff[ses.ff !="very.high"]
ses.ff.new
ses.ff.new<-factor(ses.ff.new)
ses.ff.new
levels(ses.ff.new)
ses.f<-ses.ff.new
read<-c(34,39,63,44,47,47,57,39,48,47,34,37,47,47,39,47,47,50,28,60)
combo<-data.frame(schtyp,schtyp.f,ses,ses.f,read)
combo
table(ses,schtyp)
table(ses.f,schtyp.f)
 library(lattice)
bwplot(schtyp~read|ses,data=combo,layout=c(2,2))
x<-c(1,2,3,4,5)
lapply(x,mean)b
k<-c("a","b","c")
sapply(k, paste,USE.NAMES=FALSE,1:5,sep="...")
sapply(k, paste,USE.NAMES=TRUE,1:5,sep="...")
sapply(k,paste,simplify=F,USE.NAMES=T,1:5,sep='...')
norm1<-rnorm(100)
norm1
mean(norm1)
set.seed(121343)
u<-rnorm(100)
hist(u)
hist(u,density=30)
