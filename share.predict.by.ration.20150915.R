######################################################################
#Project:  search engine market share prediction
#Author:   JX
#Version:  1.0
#Modify:   2015.04.02: add ajex number to cover the 2.16 drop
######################################################################



library(xlsx)
library(leaps) #add for leap
library(quantmod)
library(ggplot2) #add for ggplot
library(Hmisc) #describe
library(psych) #describe
library(pastecs) #stat.desc
library(dplyr)
library(reshape2)

par(mfrow=c(1,1))
mycolor=rainbow(20)


##########################################
#1# read the data
index=read.xlsx("index.xlsx",1);
##=============delete taobao data ==============
index=index[,-c(9,10,11)]
names(index)[2]="date"
# load the baidu quant
#load("pcindex.Rdata")
#index=cbind(index[1:7],pcindex$sum.pcindex,index[8:61])
#names(index)[8]="baidupc"
index.backup=index


index=index.backup
filter(index, date %in% as.Date(as.Date("2014-02-10"):as.Date("2015-9-14")))



##########################################
#2# define the macro
no.dim=dim(index)
no.var=no.dim[2]
no.sample=no.dim[1]

#define the collom position
begin=6
end=no.var
no.index=3
no.set=(end-begin+1)/no.index #18

#turn off the data to numeric
for (i in begin:end){
  for (j in 1:no.sample){
    # check the non numeric
    if (!is.numeric(index[j,i])){
      cat(i,j,index[j,i],"\n")
      index[j,i]=as.numeric(index[j,i])
    }

    if (is.na(index[j,i])){
      cat(i,j,index[j,i],"\n")
      if ((j==1)) index[j,i]=index[j+1,i]
      if (!(j==1)) index[j,i]=index[j-1,i]
    }
    
    if (index[j,i]<(mean(index[,i])/100)){
      cat(i,j,index[j,i],mean(index[,i]),"\n")
      index[j,i]=mean(index[,i])
    }
  }
  #cat(mean(index[,i]))
}


##### 04HUFZEEPr
index=index[order(index$date),]
# sum.baidu
sum.baidu=apply(index[seq(begin,end-2,no.index)],1,mean)
index=cbind(index,sum.baidu)

sum.qh=apply(index[seq(begin+1,end-1,no.index)],1,mean)
index=cbind(index,sum.qh)

sum.sogou=apply(index[seq(begin+2,end,no.index)],1,mean)
index=cbind(index,sum.sogou)

end=end+3
no.var=no.var+3





plot(index$date,index$sum.baidu,type="p",pch=1,cex=0.3,col=mycolor[1],pty=1,ylim=c( 10000, 300000))
legend("bottomleft",legend=c("baidu ", "qihu ", "sogou"),pch=c(1,1,1),col=mycolor[c(1,4,8)])
lines(index$date,index$sum.qh,type="p",pch=1,cex=0.3,col=mycolor[4],lty=2)
lines(index$date,index$sum.sogou,type="p",pch=1,cex=0.3,col=mycolor[8],lty=4)

#identify(index$date,index$sum.sogou)



#------------------------------------------------------------------
#modify Stage I
#------------------------------------------------------------------
# modify the sogou data for interruction
fsogou=index[265-20:265,"sum.sogou"]
asogou=index[266:266+20,"sum.sogou"]
fsg=index[1:265,"sum.sogou"]
asg=index[266:nrow(index),"sum.sogou"]
var(fsg)
var(asg)

if (0) {
  fsg=index[1:265,"sum.sogou"]*(var(asg)/var(fsg))^(1/2)
  mean(fsogou)
  mean(asogou)
  mean(fsg)
  mean(asg)
}

fsg=fsg-mean(fsg)+mean(asg)
mean(fsg)

index[1:265,"sum.sogou"]=fsg

fsg=index[1:265,"sum.sogou"]
asg=index[266:nrow(index),"sum.sogou"]
mean(fsg)
mean(asg)
var(fsg)
var(asg)

#------------------------------------------------------------------
#modify Stage II
#------------------------------------------------------------------
tail(index[,c("date","sum.baidu","sum.qh", "sum.sogou")],100)
tail(index[,c(2,grep("ogou",names(index)))],21)
filter(index[,c(2,grep("ogou",names(index)))],date %in% c(as.Date("2015-08-23"), as.Date("2015-08-24")))

(chdata=select(filter(index, sum.sogou>30000 & date>as.Date("2015-08-20")),date,sum.sogou))
(stdata=select(filter(index, date>(min(chdata$date,na.rm=T)-21) & date<min(chdata$date,na.rm=T)),date,sum.sogou))
modata=chdata
(modata$sum.sogou=chdata$sum.sogou*sd(stdata$sum.sogou)/sd(chdata$sum.sogou))
(modata$sum.sogou=modata$sum.sogou+mean(stdata$sum.sogou)-mean(modata$sum.sogou))

plot(chdata$date,chdata$sum.sogou,type="p",pch=1,cex=1,col=mycolor[1],pty=1,ylim=c( 10000, 100000),
     xlim=c(as.Date("2015-08-01"), as.Date("2015-10-01")))
legend("bottomleft",legend=c("origin ", "standard", "modified"),pch=c(1,1,1),col=mycolor[c(1,4,8)])
lines(stdata$date,stdata$sum.sogou,type="p",pch=1,cex=1,col=mycolor[4],lty=2)
lines(modata$date,modata$sum.sogou,type="p",pch=1,cex=1,col=mycolor[8],lty=4)

index[which(index$sum.sogou>30000 & index$date>as.Date("2015-08-20")),"sum.sogou"]=modata$sum.sogou






plot(index$date,index$sum.baidu,type="p",pch=1,cex=0.3,col=mycolor[1],pty=1,ylim=c( 10000, 300000))
legend("bottomleft",legend=c("baidu ", "qihu ", "sogou"),pch=c(1,1,1),col=mycolor[c(1,4,8)])
lines(index$date,index$sum.qh,type="p",pch=1,cex=0.3,col=mycolor[4],lty=2)
lines(index$date,index$sum.sogou,type="p",pch=1,cex=0.3,col=mycolor[8],lty=4)

#identify(index$date,index$sum.sogou)




##########################################
#3# define the switch
no.sample=nrow(index)
period=7
ma=30
lm.no=40
test.no=floor(no.sample/period)-lm.no
alpha=1


#############################################
#4# turn the data according to week and month
# calculate index in 7 day mean:period index
# smooth the data to 7 or 30 days average
period.index=index[1:floor(no.sample/period),]
for (i in 1:ceiling(no.sample/period)){
  location=(i-1)*period+1
  b=max(1,location-(ma-period))
  e=min(nrow(index),location+period-1)
  period.index[i,c(1,2)]=index[location,c(1,2)]
  #period.index[i,3:end]=apply(index[location:(location+period-1),3:end],2,mean)
  period.index[i,3:end]=apply(index[b:e,3:end],2,mean,na.rm=T)
  
}
tail(period.index)



#define the benchmark of index power
benchmarkday=as.Date("2014-12-22")
ib0=period.index[which(period.index$date==benchmarkday),"sum.baidu"]
iq0=period.index[which(period.index$date==benchmarkday),"sum.qh"]
is0=period.index[which(period.index$date==benchmarkday),"sum.sogou"]

#define the bechmark of share power coefficient
cb=1
cq=0.6
cs=0.1/(0.9/1.6)

#define the growth of index
gi=period.index[,c("date","sum.baidu","sum.qh","sum.sogou")]
gi$gb=gi$sum.baidu/ib0
gi$gq=gi$sum.qh/iq0
gi$gs=gi$sum.sogou/is0

gi$totshare=0.98+runif(nrow(gi),min=0,max=0.005)
  
  
gi$sb=gi$gb*cb/(gi$gb*cb+gi$gq*cq+gi$gs*cs)*gi$totshare
gi$sq=gi$gq*cq/(gi$gb*cb+gi$gq*cq+gi$gs*cs)*gi$totshare
gi$ss=gi$gs*cs/(gi$gb*cb+gi$gq*cq+gi$gs*cs)*gi$totshare

pgi=gi[which(gi$date==as.Date("2014-02-10")):nrow(gi),]
#pgi=gi[which(gi$date==as.Date("2014-03-24")):nrow(gi),]


plot(pgi$date,pgi$sb,type="p",pch=1,cex=0.3,col=mycolor[1],pty=1,ylim=c(0,1))
legend("topleft",legend=c("baidu", "qihu", "sogou"),pch=c(1,1,1),col=mycolor[c(1,2,4)])
lines(pgi$date,pgi$sq,type="p",pch=1,cex=0.3,col=mycolor[2],lty=2)
lines(pgi$date,pgi$ss,type="p",pch=1,cex=0.3,col=mycolor[4],lty=4)

pgi[,c("date","sb","sq","ss")]

write.xlsx(file=paste(Sys.Date(),"search engine market share.xlsx"),pgi[,c("date","sb","sq","ss")])







































##########################################
#5# have a look at the data

if (0){
 
  opar=par(no.readonly=TRUE)
  op <- par(mar = rep(0, 4))   
  
  target=pindex#define the target data
  for (i in 1:(no.set+1)){
    par(mfrow=c(7,1),mar=c(4,4,0,0))
    mycolor=rainbow(10)
    position=begin+(i-1)*no.index
    plot(target$Date,target$baidupc,type="b",col=mycolor[1],ylab="baidu pc")
    plot(target$Date,target$sb,type="b",col=mycolor[2],ylab="baidu share")
    plot(target$Date,target$sq,type="b",col=mycolor[3],ylab="360 share")
    plot(target$Date,target$ss,type="b",col=mycolor[4],ylab="sogou share")
    plot(target$Date,target[,begin+(i-1)*no.index],type="b",col=mycolor[5],xlab="baidu Date",ylab=names(index)[begin+(i-1)*no.index])
    plot(target$Date,target[,begin+(i-1)*no.index+1],type="b",col=mycolor[6],xlab="360 Date",ylab=names(index)[begin+(i-1)*no.index+1])
    plot(target$Date,target[,begin+(i-1)*no.index+2],type="b",col=mycolor[7],xlab="sogou Date",ylab=names(index)[begin+(i-1)*no.index+2])
    #plot(target$Date[c(1:60)],target$zhaopinb[c(1:60)],type="b",col=mycolor[6])
    #abline(index[2],zhaopinb)
    #abline(index[2],zhaopinq)
    #abline(index[2],zhaopins)
    #cor(target$baidupc,target$sb)
  
    if (0){
      jpeg(paste(i,".jpg",sep=""))
      #dev.off
    }
    dev.new()
  }
  
  par(opar)
}


################################################
#11# the real prediction with percent+LM method

#10.1 scale the data : sindex stand for the scaled index
sindex=period.index[,c(2,3,4,5,end-2,end-1,end)]
#use the history market share
weeks=dim(sindex)[1];

#####################may be this line should be modified################
sindex[2:weeks,c("sb","sq","ss")]=sindex[1:(weeks-1),c("sb","sq","ss")]

# scaled
if (1) {
  sindex$sum.baidu=scale(sindex$sum.baidu,scale=TRUE,center=TRUE)+10
  sindex$sum.qh=scale(sindex$sum.qh,scale=TRUE,center=TRUE)+10
  sindex$sum.sogou=scale(sindex$sum.sogou,scale=TRUE,center=TRUE)+10
  
  
  plot(sindex$Date,sindex$sum.baidu,type="b",pch=1,cex=1,col=mycolor[1],pty=1)
  legend("bottomleft",legend=c("baidu ", "qh ", "sogou"),lty=c(1,2,4),col=mycolor[c(1,2,4)])
  lines(sindex$Date,sindex$sum.qh,type="b",pch=1,col=mycolor[2],lty=2)
  lines(sindex$Date,sindex$sum.sogou,type="b",col=mycolor[4],lty=4)
  
  
}

sindex[,length(sindex)+1]=sindex$sb*sindex$sum.baidu/(sindex$sb*sindex$sum.baidu+sindex$sq*sindex$sum.qh+sindex$ss*sindex$sum.sogou)
names(sindex)[length(sindex)]="psb" #predict percent of baidu
sindex[,length(sindex)+1]=sindex$sq*sindex$sum.qh/(sindex$sb*sindex$sum.baidu+sindex$sq*sindex$sum.qh+sindex$ss*sindex$sum.sogou)
names(sindex)[length(sindex)]="psq" #predict percent of baidu
sindex[,length(sindex)+1]=sindex$ss*sindex$sum.sogou/(sindex$sb*sindex$sum.baidu+sindex$sq*sindex$sum.qh+sindex$ss*sindex$sum.sogou)
names(sindex)[length(sindex)]="pss" #predict percent of baidu

# means the final predict result
sindex[,length(sindex)+1]=sindex$sb
names(sindex)[length(sindex)]="fsb" #predict percent of baidu
sindex[,length(sindex)+1]=sindex$sq
names(sindex)[length(sindex)]="fsq" #predict percent of baidu
sindex[,length(sindex)+1]=sindex$ss
names(sindex)[length(sindex)]="fss" #predict percent of baidu

#produce the test data set for evaluation
stindex=sindex[(lm.no+1):(lm.no+test.no),]
spindex=sindex[1:lm.no,]


# start the linear migrite with spindex
sbfit=lm(sb~psb,spindex) #share baidu fit
#sbfit=lm(log(sb)~log(sum.baidu)+log(sum.qh)+log(sum.sogou),spindex) #share baidu fit
summary(sbfit)
sqfit=lm(sq~psq,spindex) #share baidu fit
#sqfit=lm(log(sq)~log(sum.baidu)+log(sum.qh)+log(sum.sogou),spindex) #share baidu fit
summary(sqfit)
ssfit=lm(ss~pss,spindex) #share baidu fit
#ssfit=lm(log(ss)~log(sum.baidu)+log(sum.qh)+log(sum.sogou),spindex) #share baidu fit
summary(ssfit)


for (i in 1:nrow(stindex)){

  stindex[i,"fsb"]=predict(sbfit,newdata=stindex[i,])
  stindex[i,"fsq"]=predict(sqfit,newdata=stindex[i,])
  stindex[i,"fss"]=predict(ssfit,newdata=stindex[i,])
  
  #scaled to 1
  sumfs=sum(stindex[i,c("fsb","fsq","fss")])
  stindex[i,c("fsb","fsq","fss")]=stindex[i,c("fsb","fsq","fss")]/sumfs
    
  #update the information
  if (!i==nrow(stindex)) {
    #------------------------------------------------------------------#
    #update sb sq ss    #this is the key assumption
    
    stindex[i+1,c("sb","sq","ss")]=stindex[1,c("sb","sq","ss")]
    stindex[i+1,c("sb","sq","ss")]=stindex[i,c("fsb","fsq","fss")]
    
    #------------------------------------------------------------------#
        
    #recalculate the psb, psq,pss 
    stindex[i+1,c("psb","psq","pss")]=stindex[i+1,c("sb","sq","ss")]*stindex[i+1,c("sum.baidu","sum.qh","sum.sogou")]/sum(stindex[i+1,c("sb","sq","ss")]*stindex[i+1,c("sum.baidu","sum.qh","sum.sogou")])
  }
}



par(mfrow=c(3,2),mar=c(4,4,0,0))
mycolor=rainbow(10)
plot(spindex$Date,spindex$sb,type="b",col=mycolor[1],lty=1,ylab="share baidu",xlab=paste(lm.no,"weeks learning"),ylim=c(0.4,0.8))
legend("topright",legend=c("real", "percent", "predict"),lty=c(1,1,1),col=mycolor[c(1,2,8)])
lines(spindex$Date,spindex$psb,type="l",col=mycolor[2])
lines(spindex$Date,fitted(sbfit),type="l",col=mycolor[8])
plot(stindex$Date,stindex$fsb,type="p",col=mycolor[1],ylim=c(0.4,0.8),ylab="share baidu",xlab=paste(test.no,"weeks predict"))
legend("topright",legend=c("predict"),lty=c(1),col=mycolor[c(1)])
#lines(stindex$Date,stindex$psb,type="l",col=mycolor[2])

plot(spindex$Date,spindex$sq,type="b",col=mycolor[1],lty=1,ylab="share 360",xlab=paste(lm.no,"weeks learning"),ylim=c(0.1,0.4))
#legend("topright",legend=c("real", "percent", "predict"),lty=c(1,1,1),col=mycolor[c(1,2,8)])
lines(spindex$Date,spindex$psq,type="l",col=mycolor[2])
lines(spindex$Date,fitted(sqfit),type="l",col=mycolor[8])
plot(stindex$Date,stindex$fsq,type="p",col=mycolor[1],ylim=c(0.1,0.4),ylab="share 360",xlab=paste(test.no,"weeks predict"))
#lines(stindex$Date,stindex$psq,type="l",col=mycolor[2])

plot(spindex$Date,spindex$ss,type="b",col=mycolor[1],lty=1,ylab="share sogou",xlab=paste(lm.no,"weeks learning"),ylim=c(0,0.2))
#legend("topright",legend=c("real", "percent", "predict"),lty=c(1,1,1),col=mycolor[c(1,2,8)])
lines(spindex$Date,spindex$pss,type="l",col=mycolor[2])
lines(spindex$Date,fitted(ssfit),type="l",col=mycolor[8])
plot(stindex$Date,stindex$fss,type="p",col=mycolor[1],ylim=c(0,0.2),ylab="share sogou",xlab=paste(test.no,"weeks predict"))
#lines(stindex$Date,stindex$pss,type="l",col=mycolor[2])

print("The predict shares are as followed")
print(stindex[c("fsb","fsq","fss")])
print(stindex[c("Date","fsb","fsq","fss")])