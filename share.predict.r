######################################################################
#Project:  search engine market share prediction
#Author:   JX
#Version:  1.0
#Modify:   2015.04.02: add ajex number to cover the 2.16 drop
######################################################################



library(xlsx)
library(leaps) #add for leap

  


##########################################
#1# read the data
index=read.xlsx("index.xlsx",1);
# load the baidu quant
#load("pcindex.Rdata")
#index=cbind(index[1:7],pcindex$sum.pcindex,index[8:61])
#names(index)[8]="baidupc"
index.backup=index
index=index.backup


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
index=index[order(index$Date),]
# sum.baidu
sum.baidu=apply(index[seq(begin,end-2,no.index)],1,mean)
index=cbind(index,sum.baidu)

sum.qh=apply(index[seq(begin+1,end-1,no.index)],1,mean)
index=cbind(index,sum.qh)

sum.sogou=apply(index[seq(begin+2,end,no.index)],1,mean)
index=cbind(index,sum.sogou)

end=end+3
no.var=no.var+3
index=index[-c(1,2,3,4),]



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
for (i in 1:floor(no.sample/period)){
  location=(i-1)*period+1
  b=max(1,location-(ma-period))
  e=min(nrow(index),location+period-1)
  period.index[i,c(1,2)]=index[location,c(1,2)]
  #period.index[i,3:end]=apply(index[location:(location+period-1),3:end],2,mean)
  period.index[i,3:end]=apply(index[b:e,3:end],2,mean,na.rm=T)
  
}
head(period.index)


#---------------------------------------------------------
# use the definite flow quantity instead of hotword index

  
if (T) {  
  # deal the NA to number, X is a collmn vector
  deal.na=function(x){
    x=as.numeric(x);
    l=length(x)
    for (i in 1:l){
      if (is.na(x[i])) {x[i]=x[i-1]}
      if (x[i]<(mean(x,na.rm=T)/1000)) x[i]=x[i-1]
    }
    return(x)
  }
  

  ajex=read.xlsx("ajex.xlsx",1);
  ajex=ajex[order(ajex$date),]
  #str(ajex)
  
  uvpv=data.frame(sapply(ajex[,2:ncol(ajex)],deal.na))
  ajex=cbind(date=ajex$date,uvpv)
  
  uvpv=ajex[,c("date","buv","bpv","suv","spv","quv","qpv")]
  #head(uvpv,30)
  
  par(mfrow=c(1,1),mar=c(4,4,0,0))
  mycolor=rainbow(10)
  plot(uvpv$date,uvpv$bpv,type="b",col=mycolor[1],lty=1,ylim=c( 1258812, 892000000))
  legend("bottomleft",legend=c("baidu pv", "so pv", "qihu pv"),lty=c(1,2,4),col=mycolor[c(1,2,4)])
  lines(uvpv$date,uvpv$spv,type="b",col=mycolor[2],lty=2)
  lines(uvpv$date,uvpv$qpv,type="b",col=mycolor[4],lty=4)
  
  plot(uvpv$date,uvpv$buv,type="b",col=mycolor[1],lty=1,ylim=c( 0, 100000000))
  legend("bottomleft",legend=c("baidu uv", "so uv", "qihu uv"),lty=c(1,2,4),col=mycolor[c(1,2,4)])
  lines(uvpv$date,uvpv$suv,type="b",col=mycolor[2],lty=2)
  lines(uvpv$date,uvpv$quv,type="b",col=mycolor[4],lty=4)
  
  plot(index$Date,index$sum.baidu,type="p",pch=1,cex=0.3,col=mycolor[1],pty=1,,ylim=c( 10000, 300000))
  legend("bottomleft",legend=c("baidu ", "so ", "qihu "),lty=c(1,2,4),col=mycolor[c(1,2,4)])
  lines(index$Date,index$sum.qh,type="p",pch=1,cex=0.3,col=mycolor[2],lty=2)
  lines(index$Date,index$sum.sogou,type="p",pch=1,cex=0.3,col=mycolor[4],lty=4)
  
  plot(period.index$Date,period.index$sum.baidu,type="p",pch=1,cex=0.3,col=mycolor[1],pty=1,,ylim=c( 10000, 300000))
  legend("bottomleft",legend=c("baidu ", "so ", "qihu "),lty=c(1,2,4),col=mycolor[c(1,2,4)])
  lines(period.index$Date,period.index$sum.qh,type="p",pch=1,cex=0.3,col=mycolor[2],lty=2)
  lines(period.index$Date,period.index$sum.sogou,type="p",pch=1,cex=0.3,col=mycolor[4],lty=4)


  scale.baidu=scale(period.index$sum.baidu,scale=TRUE,center=TRUE)+10
  scale.sogou=scale(period.index$sum.sogou,scale=TRUE,center=TRUE)+10
  scale.qh=scale(period.index$sum.qh,scale=TRUE,center=TRUE)+10
  
  uvpv$sbpv=scale(uvpv$bpv,scale=TRUE,center=TRUE)+10
  uvpv$sspv=scale(uvpv$spv,scale=TRUE,center=TRUE)+10
  uvpv$sqpv=scale(uvpv$qpv,scale=TRUE,center=TRUE)+10
  
  date=period.index$Date
  e=0
  for (i in 1:length(date)){
    location=which(uvpv$date==date[i])
    if (!length(location)==0) b=max(1,location-(ma-period))
    else b=max(1,e+1-(ma-period))
    e=min(nrow(uvpv),location+period-1)
        
    #for test with the sum of weighted alex number and the hotword number
    period.index[i,"sum.baidu"]=(1-alpha)*(sum(uvpv[b:e,"sbpv"])/(e-b+1))+alpha*scale.baidu[i]
    period.index[i,"sum.sogou"]=(1-alpha)*(sum(uvpv[b:e,"sspv"])/(e-b+1))+alpha*scale.sogou[i]
        
    period.index[i,"sum.qh"]=(1-1)*(sum(uvpv[b:e,"sqpv"])/(e-b+1))+1*scale.qh[i]    
  }
  
}










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
  
  
  plot(sindex$Date,sindex$sum.baidu,type="b",pch=1,cex=1,col=mycolor[1],pty=1,)
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