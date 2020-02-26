library("dummies")
library("AER")
library("plotly")
library('RColorBrewer')
library("data.table")
library("mlogit")
library("gmnl")
library("data.table")

#import and clean data
setwd("/Users/liutianyao/Desktop/Pricing Analytics/HW2")
kiwi_bubbles = read.csv('/Users/liutianyao/Desktop/Pricing Analytics/HW2/kiwi_bubbles_P2.csv')

kiwi_bubbles=kiwi_bubbles[!(kiwi_bubbles$price.KB==99),]
kiwi_bubbles=kiwi_bubbles[!(kiwi_bubbles$price.KR==99),]
kiwi_bubbles=kiwi_bubbles[!(kiwi_bubbles$price.MB==99),]


###Q3 (2)###

mlogitdata=mlogit.data(kiwi_bubbles,id="id",varying=4:7,choice="choice",shape="wide")

#Run MLE.
mle= gmnl(choice ~  price, data = mlogitdata)
summary(mle)

#demand function
demand=function(priceKB,priceKR,para){
  probKB=exp(para[1]+para[4]*priceKB)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
  probKR=exp(para[2]+para[4]*priceKR)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
  return(cbind(probKB,probKR))
}


#Unit cost
uc=0.5

#Profit function
profit=function(priceKB,priceKR,para){
  profitKB=demand(priceKB,priceKR,para)[,1]*(priceKB-uc)*1000
  profitKR=demand(priceKB,priceKR,para)[,2]*(priceKR-uc)*1000
  return(cbind(profitKB,profitKR))
}

#define para and pricespace
para=mle$coefficients
priceMB=1.43
aux=seq(1,3,0.1)
pricespace=expand.grid(aux,aux,1.43)

#profit
profitmat=matrix(0L,nrow(pricespace),1)
for (i in 1:nrow(pricespace)){
  profitmat[i]=sum(profit(pricespace[i,1],pricespace[i,2],para))  
}

#Draw figure
xaxis=list(title="P^{KB}")
yaxis=list(autorange = "reversed",title="P^{KR}")
zaxis=list(title="Profit")
p=plot_ly(x=pricespace[,1],y=pricespace[,2],z=as.numeric(profitmat),
          type="scatter3d",mode="markers",
          marker = list(color = as.numeric(profitmat), colorscale = c('#FFE1A1', '#683531'), showscale = TRUE))%>%
  layout(scene=list(xaxis=xaxis,yaxis=yaxis,zaxis=zaxis))%>%
  config(mathjax = 'cdn')
p

#optimal price
pricespace[profitmat==max(profitmat)]
#so the optimal price for KB and KR are 1.2 and 1.2



###Q3 (1)###
#calculate average prices
#calculate own price elasticity
KBavg = mean(kiwi_bubbles$price.KB)
KRavg = mean(kiwi_bubbles$price.KR)
MBavg = mean(kiwi_bubbles$price.MB)

probKB=exp(para[1]+para[4]*KBavg)/(1+exp(para[1]+para[4]*KBavg)+exp(para[2]+para[4]*KRavg)+exp(para[3]+para[4]*MBavg))
probKR=exp(para[2]+para[4]*KRavg)/(1+exp(para[1]+para[4]*KBavg)+exp(para[2]+para[4]*KRavg)+exp(para[3]+para[4]*MBavg))
probMB=exp(para[3]+para[4]*MBavg)/(1+exp(para[1]+para[4]*KBavg)+exp(para[2]+para[4]*KRavg)+exp(para[3]+para[4]*MBavg))

elaKB = (-para[4])*KBavg*(1-probKB) #4.257847
elaKR = (-para[4])*KRavg*(1-probKR) #4.13127
elaMB = (-para[4])*MBavg*(1-probMB) #4.069547

#calculate cross price elasticity
#KR on KB
elaKRKB = (-para[4])*KRavg*probKR #1.019923
#KR on MB
elaKRMB = (-para[4])*KRavg*probKR #1.019923
#KB on KR
elaKBKR = (-para[4])*KBavg*probKB #0.9054743
#KB on MB
elaKBMB = (-para[4])*KBavg*probKB #0.9054743
#MB on KB
elaMBKB = (-para[4])*MBavg*probMB #0.9601564
#MB on KR
elaMBKR = (-para[4])*MBavg*probMB #0.9601564
#cross elasticity一样,证明KB价格上升，consumer switch to KR and MB for equal proportion 
#independence from irrelevant alternatives





###Q4###
#Estimate single segment logit as a point of comparison

mle_noseg= gmnl(choice ~  price, data = mlogitdata)
summary(mle_noseg)

coef_noseg=mle_noseg$coefficients
#Load demographic data
demo=fread("demo_P2.csv",stringsAsFactors = F)

#Number of individuals
N = 283

#Clustering
demo_cluster = kmeans(x=demo[, 2:18], centers = 6, nstart = 1000)

# now combine cluster identity into the raw data
cluster_id = data.frame(id = demo$id)
cluster_id$cluster = demo_cluster$cluster
data = merge(kiwi_bubbles, cluster_id, by = "id", all.x = T)

# for those who don't fit in any cluster, group them into one additional cluster
data$cluster[is.na(data$cluster)] = 7

# segment share
seg.share = c( table(demo_cluster$cluster),N - sum(table(demo_cluster$cluster))) / N	

# just store the coefficients (you can store many other things)
coef.est = data.frame(segment = 1:7, intercept.KB = NA, intercept.KR = NA, 
                      intercept.MB = NA, price.coef = NA) 

#Write a for-loop. 
for (seg in 1:7) {
  # During each loop, pick subset of data of consumers from each segment.
  data.sub = subset(data, cluster == seg)
  
  #Using that data, the rest remains the same.
  mlogitdata=mlogit.data(data.sub,id="id",varying=4:7,choice="choice",shape="wide")
  
  #Run MLE.
  mle= gmnl(choice ~  price, data = mlogitdata)
  mle
  #Store the outcome in the coef.est matrix.
  coef.est[seg, 2:5] = mle$coefficients
}


#demand functions
demandKB=function(priceKB,priceKR,priceMB,para){
  probKB=exp(para[1]+para[4]*priceKB)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
  return(probKB)
}

demandKR=function(priceKB,priceKR,priceMB,para){
  probKR=exp(para[2]+para[4]*priceKR)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
  return(probKR)
}

demandMB=function(priceKB,priceKR,priceMB,para){
  probMB=exp(para[3]+para[4]*priceMB)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
  return(probMB)
}


pricespace=seq(0,2.0,0.01)
plot(pricespace,demandKB(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[1,2:5])),type='l',xlab='Prices',
     ylab='Probability of purchase',col="blue",lwd=20*seg.share[1],
     cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, ylim = c(0,1))
lines(pricespace,demandKB(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[5,2:5])),col="blue",lwd=20*seg.share[5])
lines(pricespace,demandKB(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[4,2:5])),col="blue",lwd=20*seg.share[4])
lines(pricespace,demandKB(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[3,2:5])),col="blue",lwd=20*seg.share[3])
lines(pricespace,demandKB(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[6,2:5])),col="blue",lwd=20*seg.share[6])
lines(pricespace,demandKB(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[2,2:5])),col="blue",lwd=20*seg.share[2])
lines(pricespace,demandKB(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[7,2:5])),col="blue",lwd=20*seg.share[7])


#calculate elasticity
#Calculate profit
#choice KB
agg_choiceKB=function(priceKB,priceKR,priceMB) {
  
  agg_choiceKB=seg.share[1]*demandKB(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))+
    seg.share[2]*demandKB(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))+
    seg.share[3]*demandKB(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))+
    seg.share[4]*demandKB(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))+
    seg.share[5]*demandKB(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))+ 
    seg.share[6]*demandKB(priceKB,priceKR,priceMB,as.numeric(coef.est[6,2:5]))+
    seg.share[7]*demandKB(priceKB,priceKR,priceMB,as.numeric(coef.est[7,2:5]))
  
  return(agg_choiceKB)
}
#choice KR
agg_choiceKR=function(priceKB,priceKR,priceMB) {
  
  agg_choiceKR=seg.share[1]*demandKR(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))+
    seg.share[2]*demandKR(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))+
    seg.share[3]*demandKR(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))+
    seg.share[4]*demandKR(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))+
    seg.share[5]*demandKR(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))+ 
    seg.share[6]*demandKR(priceKB,priceKR,priceMB,as.numeric(coef.est[6,2:5]))+
    seg.share[7]*demandKR(priceKB,priceKR,priceMB,as.numeric(coef.est[7,2:5]))
  
  return(agg_choiceKR)
}
#choice MB
agg_choiceMB=function(priceKB,priceKR,priceMB) {
  
  agg_choiceMB=seg.share[1]*demandMB(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))+
    seg.share[2]*demandMB(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))+
    seg.share[3]*demandMB(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))+
    seg.share[4]*demandMB(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))+
    seg.share[5]*demandMB(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))+ 
    seg.share[6]*demandMB(priceKB,priceKR,priceMB,as.numeric(coef.est[6,2:5]))+
    seg.share[7]*demandMB(priceKB,priceKR,priceMB,as.numeric(coef.est[7,2:5]))
  
  return(agg_choiceMB)
}

#aggregate probability of KB
plot(pricespace,agg_choiceKB(pricespace,mean(data$price.KR),mean(data$price.MB)),type='l',xlab='Prices',
     ylab='Choice probability',col="blue",lwd=2
     ,cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(pricespace,demandKB(pricespace,mean(data$price.KR),mean(data$price.MB),coef_noseg),col="orange",lwd=20*seg.share[4],lty=2)
legend(1.1, 0.9, legend=c("With segmentation", "Without segmentation"),
       col=c( "blue","orange"), lty=1, cex=1.1)

#KB own elasticity -4.286751
old = agg_choiceKB(mean(data$price.KB), mean(data$price.KR),mean(data$price.MB))
new = agg_choiceKB(mean(data$price.KB)*1.01, mean(data$price.KR),mean(data$price.MB))
segelaKB = ((new-old)/old)/0.01
#KR own elasticity -3.495968
old1= agg_choiceKR(mean(data$price.KB), mean(data$price.KR),mean(data$price.MB))
new1= agg_choiceKR(mean(data$price.KB), mean(data$price.KR)*1.01,mean(data$price.MB))
segelaKR = ((new1-old1)/old1)/0.01
#MB own elasticity -4.078755 
old2= agg_choiceMB(mean(data$price.KB), mean(data$price.KR),mean(data$price.MB))
new2= agg_choiceMB(mean(data$price.KB), mean(data$price.KR),mean(data$price.MB)*1.01)
segelaMB = ((new2-old2)/old2)/0.01
#KR-KB cross elasticity 1.016185 
old3= agg_choiceKB(mean(data$price.KB), mean(data$price.KR),mean(data$price.MB))
new3= agg_choiceKB(mean(data$price.KB), mean(data$price.KR)*1.01,mean(data$price.MB))
segelaKRKB = ((new3-old3)/old3)/0.01
#MB-KB cross elasticity 1.01548 
old4= agg_choiceKB(mean(data$price.KB), mean(data$price.KR),mean(data$price.MB))
new4= agg_choiceKB(mean(data$price.KB), mean(data$price.KR),mean(data$price.MB)*1.01)
segelaMBKB = ((new4-old4)/old4)/0.01
#KB-KR cross elasticity 0.7162632 
old5= agg_choiceKR(mean(data$price.KB), mean(data$price.KR),mean(data$price.MB))
new5= agg_choiceKR(mean(data$price.KB)*1.01, mean(data$price.KR),mean(data$price.MB))
segelaKBKR = ((new5-old5)/old5)/0.01
#MB-KR cross elasticity 0.8204181
old6= agg_choiceKR(mean(data$price.KB), mean(data$price.KR),mean(data$price.MB))
new6= agg_choiceKR(mean(data$price.KB), mean(data$price.KR),mean(data$price.MB)*1.01)
segelaMBKR = ((new6-old6)/old6)/0.01
#KB-MB cross elasticity 0.8127758
old7= agg_choiceMB(mean(data$price.KB), mean(data$price.KR),mean(data$price.MB))
new7= agg_choiceMB(mean(data$price.KB)*1.01, mean(data$price.KR),mean(data$price.MB))
segelaKBMB = ((new7-old7)/old7)/0.01
#KR-MB cross elasticity 0.9320796
old8= agg_choiceMB(mean(data$price.KB), mean(data$price.KR),mean(data$price.MB))
new8= agg_choiceMB(mean(data$price.KB), mean(data$price.KR)*1.01,mean(data$price.MB))
segelaKRMB = ((new7-old7)/old7)/0.01
#the most elastic one is KR-KB, the least elastic one is KB-KR 
#which means they are close substitutes, but customers are more loyal to KB than KR with a higher price
#the second smallest elasticity is KB-MB, which is good for us because raising KB's 
#price will not increase MB's demand so much,this reflect that KB's customers are less
#price-sensitive than MB


##customer segmentation and their preferences
#KB-KR comparison 
plot(coef.est[1,2]-coef.est[1,3],coef.est[1,5],cex=20*seg.share[1],xlim=c(-3,3),ylim=c(-9,-1.5),
     col = "chocolate",pch=16,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlab="beta_0^KB-beta_0^KR",ylab=("beta_1"))
points(coef.est[2,2]-coef.est[2,3],coef.est[2,5],cex=20*seg.share[2],col = "yellow",pch=16)
points(coef.est[3,2]-coef.est[3,3],coef.est[3,5],cex=20*seg.share[3],col = "skyblue",pch=16)
points(coef.est[4,2]-coef.est[4,3],coef.est[4,5],cex=20*seg.share[4],col = "darkgreen",pch=16)
points(coef.est[5,2]-coef.est[5,3],coef.est[5,5],cex=20*seg.share[5],col = "pink",pch=16)
points(coef.est[6,2]-coef.est[6,3],coef.est[6,5],cex=20*seg.share[6],col = "purple",pch=16)
points(coef.est[7,2]-coef.est[7,3],coef.est[7,5],cex=20*seg.share[7],col = "black",pch=16)
#KB-MB comparison 
plot(coef.est[1,2]-coef.est[1,4],coef.est[1,5],cex=20*seg.share[1],xlim=c(-3,3),ylim=c(-9,-1.5),
     col = "chocolate",pch=16,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlab="beta_0^KB-beta_0^MB",ylab=("beta_1"))
points(coef.est[2,2]-coef.est[2,4],coef.est[2,5],cex=20*seg.share[2],col = "yellow",pch=16)
points(coef.est[3,2]-coef.est[3,4],coef.est[3,5],cex=20*seg.share[3],col = "skyblue",pch=16)
points(coef.est[4,2]-coef.est[4,4],coef.est[4,5],cex=20*seg.share[4],col = "darkgreen",pch=16)
points(coef.est[5,2]-coef.est[5,4],coef.est[5,5],cex=20*seg.share[5],col = "pink",pch=16)
points(coef.est[6,2]-coef.est[6,4],coef.est[6,5],cex=20*seg.share[6],col = "purple",pch=16)
points(coef.est[7,2]-coef.est[7,4],coef.est[7,5],cex=20*seg.share[7],col = "black",pch=16)
#KR-MB comparison 
plot(coef.est[1,3]-coef.est[1,4],coef.est[1,5],cex=20*seg.share[1],xlim=c(-3,3),ylim=c(-9,-1.5),
     col = "chocolate",pch=16,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlab="beta_0^KR-beta_0^MB",ylab=("beta_1"))
points(coef.est[2,3]-coef.est[2,4],coef.est[2,5],cex=20*seg.share[2],col = "yellow",pch=16)
points(coef.est[3,3]-coef.est[3,4],coef.est[3,5],cex=20*seg.share[3],col = "skyblue",pch=16)
points(coef.est[4,3]-coef.est[4,4],coef.est[4,5],cex=20*seg.share[4],col = "darkgreen",pch=16)
points(coef.est[5,3]-coef.est[5,4],coef.est[5,5],cex=20*seg.share[5],col = "pink",pch=16)
points(coef.est[6,3]-coef.est[6,4],coef.est[6,5],cex=20*seg.share[6],col = "purple",pch=16)
points(coef.est[7,3]-coef.est[7,4],coef.est[7,5],cex=20*seg.share[7],col = "black",pch=16)

#calculate each segment's profit for KB
pricespace0 = seq(0.1, 2, 0.01)
profit1=1000*seg.share[1]*demandKB(pricespace0,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[1,2:5]))*(pricespace0-uc)
profit2=1000*seg.share[2]*demandKB(pricespace0,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[2,2:5]))*(pricespace0-uc)
profit3=1000*seg.share[3]*demandKB(pricespace0,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[3,2:5]))*(pricespace0-uc)
profit4=1000*seg.share[4]*demandKB(pricespace0,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[4,2:5]))*(pricespace0-uc)
profit5=1000*seg.share[5]*demandKB(pricespace0,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[5,2:5]))*(pricespace0-uc)
profit6=1000*seg.share[6]*demandKB(pricespace0,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[6,2:5]))*(pricespace0-uc)

pricespace0[profit1==max(profit1)]##0.99 and profit = 56.41933
pricespace0[profit2==max(profit2)]##0.98 and profit = 21.62776
pricespace0[profit3==max(profit3)]##1.05 and profit = 37.82587
pricespace0[profit4==max(profit4)]##0.99 and profit = 29.18509
pricespace0[profit5==max(profit5)]##1.01 and profit = 44.77759
pricespace0[profit6==max(profit6)]##0.94 and profit = 25.82988

# So we (KB) should target segment1 and segment 5


#Do not launch KB: if we estimate new coefficients#
kiwi_bubbles1=kiwi_bubbles[!(kiwi_bubbles$choice=='KB'),-5]
mlogitdata1=mlogit.data(kiwi_bubbles1,id="id",varying=4:6,choice="choice",shape="wide")
#Run MLE.
mle1= gmnl(choice ~  price, data = mlogitdata1)
summary(mle1)
para1 = mle1$coef


data1 = merge(kiwi_bubbles1, cluster_id, by = "id", all.x = T)

# for those who don't fit in any cluster, group them into one additional cluster
data1$cluster[is.na(data1$cluster)] = 7

# segment share
seg.share1 = c( table(demo_cluster$cluster),N - sum(table(demo_cluster$cluster))) / N 

# just store the coefficients (you can store many other things)
coef.est1 = data.frame(segment = 1:7, intercept.KR = NA, 
                       intercept.MB = NA, price.coef = NA)

#construct coef
for (seg in 1:7) {
  # During each loop, pick subset of data of consumers from each segment.
  data.sub1 = subset(data1, cluster == seg)
  
  #Using that data, the rest remains the same.
  mlogitdata1=mlogit.data(data.sub1,id="id",varying=4:6,choice="choice",shape="wide")
  
  #Run MLE.
  mle2= gmnl(choice ~  price, data = mlogitdata1)
  mle2
  #Store the outcome in the coef.est matrix.
  coef.est1[seg, 2:4] = mle2$coefficients
}

#demand function
demandKR1=function(priceKR,priceMB,para1){
  probKR1=exp(para1[1]+para1[3]*priceKR)/(1+exp(para1[1]+para1[3]*priceKR)+exp(para1[2]+para1[3]*priceMB))
  return(probKR1)
}

#aggregated choice probability
agg_choiceKR1=function(priceKR,priceMB) {
  
  agg_choiceKR1=seg.share1[1]*demandKR1(priceKR,priceMB,as.numeric(coef.est1[1,2:4]))+
    seg.share1[2]*demandKR1(priceKR,priceMB,as.numeric(coef.est1[2,2:4]))+
    seg.share1[3]*demandKR1(priceKR,priceMB,as.numeric(coef.est1[3,2:4]))+
    seg.share1[4]*demandKR1(priceKR,priceMB,as.numeric(coef.est1[4,2:4]))+
    seg.share1[5]*demandKR1(priceKR,priceMB,as.numeric(coef.est1[5,2:4]))+ 
    seg.share1[6]*demandKR1(priceKR,priceMB,as.numeric(coef.est1[6,2:4]))+
    seg.share1[7]*demandKR1(priceKR,priceMB,as.numeric(coef.est1[7,2:4]))
  
  return(agg_choiceKR1)
}

#profit=1000*agg_choiceKR1(pricespace,1.43)*(pricespace-uc)
pricing1 = data.frame(pricespace = seq(0,2,0.01), profit=NA)
pricing1$profit = 1000*agg_choiceKR1(pricing1$pricespace,1.43)*(pricing1$pricespace-uc)
pricing1[pricing1$profit == max(pricing1$profit),] #maximized profit is 317.4735 when price is 1.06

#Do not launch KB: if we use existing coefficients#
demandKR2=function(priceKR,priceMB,para){
  probKR2=exp(para[1]+para[3]*priceKR)/(1+exp(para[1]+para[3]*priceKR)+exp(para[2]+para[3]*priceMB))
  return(probKR2)
}

#aggregated choice probability
agg_choiceKR2=function(priceKR,priceMB) {
  
  agg_choiceKR2=seg.share[1]*demandKR2(priceKR,priceMB,as.numeric(coef.est[1,3:5]))+
    seg.share[2]*demandKR2(priceKR,priceMB,as.numeric(coef.est[2,3:5]))+
    seg.share[3]*demandKR2(priceKR,priceMB,as.numeric(coef.est[3,3:5]))+
    seg.share[4]*demandKR2(priceKR,priceMB,as.numeric(coef.est[4,3:5]))+
    seg.share[5]*demandKR2(priceKR,priceMB,as.numeric(coef.est[5,3:5]))+ 
    seg.share[6]*demandKR2(priceKR,priceMB,as.numeric(coef.est[6,3:5]))+
    seg.share[7]*demandKR2(priceKR,priceMB,as.numeric(coef.est[7,3:5]))
  
  return(agg_choiceKR2)
}

profit2=1000*agg_choiceKR2(pricespace,1.43)*(pricespace-uc)
pricing2 = data.frame(pricespace = seq(0,2,0.01), profit=NA)
pricing2$profit = 1000*agg_choiceKR2(pricing2$pricespace,1.43)*(pricing2$pricespace-uc)
pricing2[pricing2$profit == max(pricing2$profit),] #maximized profit is 295.7271 when price is 1.07
#MB's profit
demandMB2=function(priceKR,priceMB,para){
  probMB2=exp(para[2]+para[3]*priceMB)/(1+exp(para[1]+para[3]*priceKR)+exp(para[2]+para[3]*priceMB))
  return(probMB2)
}

agg_choiceMB2=function(priceKR,priceMB) {
  
  agg_choiceMB2=seg.share[1]*demandMB2(priceKR,priceMB,as.numeric(coef.est[1,3:5]))+
    seg.share[2]*demandMB2(priceKR,priceMB,as.numeric(coef.est[2,3:5]))+
    seg.share[3]*demandMB2(priceKR,priceMB,as.numeric(coef.est[3,3:5]))+
    seg.share[4]*demandMB2(priceKR,priceMB,as.numeric(coef.est[4,3:5]))+
    seg.share[5]*demandMB2(priceKR,priceMB,as.numeric(coef.est[5,3:5]))+ 
    seg.share[6]*demandMB2(priceKR,priceMB,as.numeric(coef.est[6,3:5]))+
    seg.share[7]*demandMB2(priceKR,priceMB,as.numeric(coef.est[7,3:5]))
  
  return(agg_choiceMB2)
}

profitmgo1 = 1000*agg_choiceMB2(1.07,1.43)*(1.43-uc) #109.7806

#if we launch KB#
pricespaceKB = seq(0,2,0.01)
pricespaceKR = seq(0,2,0.01)

aux=seq(0,2,0.1)
pricespaceKBKR=expand.grid(aux,aux,1.43)
pricespaceKBKR[,4:6]=NA

colnames(pricespaceKBKR) = c('pricespaceKB','pricespaceKR','priceMR','profitKB','profitKR','profit')

for(i in 1:441) {
  pricespaceKBKR$profitKB[i]=1000*agg_choiceKB(pricespaceKBKR$pricespaceKB[i],pricespaceKBKR$pricespaceKR[i],1.43)*(pricespaceKBKR$pricespaceKB[i]-uc)
  pricespaceKBKR$profitKR[i]=1000*agg_choiceKR(pricespaceKBKR$pricespaceKR[i],pricespaceKBKR$pricespaceKR[i],1.43)*(pricespaceKBKR$pricespaceKR[i]-uc)
  pricespaceKBKR$profit[i] = pricespaceKBKR$profitKB[i]+pricespaceKBKR$profitKR[i]
}
pricespaceKBKR[pricespaceKBKR$profit == max(pricespaceKBKR$profit),] 
#maximum profit is 424.4063 when priceKB=1, priceKR=1.3 when priceMR=1.43

profitmgo2 = 1000*agg_choiceMB(1,1.3,1.43)*(1.43-0.5) #85.3327









###Q5###
#conduct iteration to calculate Nash equilibrium
optKB = 1
optKR = 1.3
pricespace3 = seq(0, 2, 0.01)
profitmamgo = 1000*agg_choiceMB(optKB,optKR, pricespace3)*(pricespace3-uc)
max(profitmamgo) #171.3624
pricespace3[profitmamgo==max(profitmamgo)] #0.95

pricespaceKBKR1=expand.grid(aux,aux,0.95)
pricespaceKBKR1[,4:6]=NA

colnames(pricespaceKBKR1) = c('pricespaceKB','pricespaceKR','priceMR','profitKB','profitKR','profit')

for(i in 1:441) {
  pricespaceKBKR1$profitKB[i]=1000*agg_choiceKB(pricespaceKBKR1$pricespaceKB[i],pricespaceKBKR1$pricespaceKR[i],0.95)*(pricespaceKBKR1$pricespaceKB[i]-uc)
  pricespaceKBKR1$profitKR[i]=1000*agg_choiceKR(pricespaceKBKR1$pricespaceKR[i],pricespaceKBKR1$pricespaceKR[i],0.95)*(pricespaceKBKR1$pricespaceKR[i]-uc)
  pricespaceKBKR1$profit[i] = pricespaceKBKR1$profitKB[i]+pricespaceKBKR1$profitKR[i]
}
pricespaceKBKR1[pricespaceKBKR1$profit == max(pricespaceKBKR1$profit),]  #priceKB = 1 and priceKR=1.2, profitKBKR = 282.6778
#second interation
profitmamgo2 = 1000*agg_choiceMB(1,1.2, pricespace3)*(pricespace3-uc)
max(profitmamgo2) #163.5036
pricespace3[profitmamgo2==max(profitmamgo2)] #0.94

pricespaceKBKR2=expand.grid(aux,aux,0.94)
pricespaceKBKR2[,4:6]=NA

colnames(pricespaceKBKR2) = c('pricespaceKB','pricespaceKR','priceMR','profitKB','profitKR','profit')

for(i in 1:441) {
  pricespaceKBKR2$profitKB[i]=1000*agg_choiceKB(pricespaceKBKR2$pricespaceKB[i],pricespaceKBKR2$pricespaceKR[i],0.94)*(pricespaceKBKR2$pricespaceKB[i]-uc)
  pricespaceKBKR2$profitKR[i]=1000*agg_choiceKR(pricespaceKBKR2$pricespaceKR[i],pricespaceKBKR2$pricespaceKR[i],0.94)*(pricespaceKBKR2$pricespaceKR[i]-uc)
  pricespaceKBKR2$profit[i] = pricespaceKBKR2$profitKB[i]+pricespaceKBKR2$profitKR[i]
}
pricespaceKBKR2[pricespaceKBKR2$profit == max(pricespaceKBKR2$profit),]  #priceKB = 1 and priceKR=1.2, profitKBKR = 278.8207
# So, the Nash equilibrium is priceKB = 0.9, priceKR=1.2, and priceMB=0.94. Here, profitKBKR = 283.9205

#third interation
profitmamgo3 = 1000*agg_choiceMB(0.9,1.2, pricespace3)*(pricespace3-uc)
max(profitmamgo3) #144.9402
pricespace3[profitmamgo3==max(profitmamgo3)] #0.92

pricespaceKBKR3=expand.grid(aux,aux,0.92)
pricespaceKBKR3[,4:6]=NA

colnames(pricespaceKBKR3) = c('pricespaceKB','pricespaceKR','priceMR','profitKB','profitKR','profit')

for(i in 1:441) {
  pricespaceKBKR3$profitKB[i]=1000*agg_choiceKB(pricespaceKBKR3$pricespaceKB[i],pricespaceKBKR3$pricespaceKR[i],0.92)*(pricespaceKBKR3$pricespaceKB[i]-uc)
  pricespaceKBKR3$profitKR[i]=1000*agg_choiceKR(pricespaceKBKR3$pricespaceKR[i],pricespaceKBKR3$pricespaceKR[i],0.92)*(pricespaceKBKR3$pricespaceKR[i]-uc)
  pricespaceKBKR3$profit[i] = pricespaceKBKR3$profitKB[i]+pricespaceKBKR2$profitKR[i]
}
pricespaceKBKR3[pricespaceKBKR3$profit == max(pricespaceKBKR3$profit),]  #priceKB = 1 and priceKR=1.2, profitKBKR = 280.217
# So, the Nash equilibrium is priceKB = 0.9, priceKR=1.2, and priceMB=0.92. Here, profitKBKR = 280.217


##without KB launch, if we work the price war, price will drop heavily and we will get less profit
# round 0
profitMB3 =1000*agg_choiceMB2(1.07,pricespace)*(pricespace-uc) 
max(profitMB3)##200.1767 with price 0.99
pricespace[profitMB3 == max(profitMB3)]

profitKR4 = 1000*agg_choiceKR2(pricespace,0.99)*(pricespace-uc) 
max(profitKR4)##206.6413 with price 1
pricespace[profitKR4 == max(profitKR4)]

profitMB5 = 1000*agg_choiceMB2(1,pricespace)*(pricespace-uc) 
max(profitMB5)##183.5248 with price 0.97
pricespace[profitMB5 == max(profitMB5)]

profitKR6 = 1000*agg_choiceKR2(pricespace,0.97)*(pricespace-uc) 
max(profitKR6)##201.4765 with price 1
pricespace[profitKR6 == max(profitKR6)]









