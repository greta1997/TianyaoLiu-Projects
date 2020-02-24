###Q3
rm(list=ls())
yogurtFull = read.csv("/Users/liutianyao/Desktop/Analytics design/project2/survResponses.csv")
install.packages("ggplot2")
library(ggplot2)

#delete incomplete surveys
yogurt = yogurtFull[yogurtFull$V10 == 1,]

#assign different numbers to Q12 answers
yogurt[15:37][yogurt[15:37] == 0] <- 5
yogurt[15:37][yogurt[15:37] == 2] <- 0
yogurt[15:37][yogurt[15:37] == 1] <- 2
yogurtQ12 = yogurt[15:37]
#drop rows with missing values (NA)
yogurtQ12 = yogurtQ12[complete.cases(yogurtQ12), ]
colnames(yogurtQ12) = c('Almond','Banana','Black Cherry', 'Blueberry','Caramel', 
                        'Chai','Chocolate','Cinnamon','Coconut','Honey','Key Lime Pie',
                        'Lemon','Mango','Maple','Peach','Vanilla Banana','Pineapple','Plain', 'Pomegranate', 
                        'Raspberry','Strawberry','Strawberry Banana','Vanilla')
#calculate total frequency of each flavor
preSum = as.data.frame(colSums(yogurtQ12))
preSum = sort(colSums(yogurtQ12),decreasing = T)  
#calculate per person average frequency of each flavor
preAvg = as.data.frame(preSum/nrow(yogurtQ12))
names(preAvg)[1] = 'average_frequency'
preAvg$flavors = rownames(preAvg)
preAvg = as.data.frame(preAvg)

ggplot(data = preAvg, aes(x=reorder(flavors, -average_frequency),y=average_frequency)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=flavors), vjust=-0.3, size=3.5)+
  theme_minimal()


###Q4
#measReach: measures reach given set of options and data
yogurtNew = yogurtQ12
yogurtNew[yogurtNew==0] <- 0
yogurtNew[yogurtNew==2] <- 1
yogurtNew[yogurtNew==5] <- 1

measReach = function(data){
  if(is.null(dim(data))){ #if data is a vector
    ret = sum(data>0,na.rm=TRUE)/length(data)  #>0 assign 1, <=0 assign 0. (百分之多少人买了)
  } else if(ncol(data)==1){ #if data has only one column
    ret = sum(data>0,na.rm=TRUE)/length(data)
  }
  else { #if data has multiple columns
    ret = sum(apply(data>0,1,any),na.rm=TRUE)/nrow(data)   #作为一个整体算
    #1 indicates rows, 2 indicates columns
    # any: if any components of the vector is true (>0 here), the 
  }
}
#evalNext: evaluates the next set, nextSet using measure given existing set in data

evalNext = function(nextSet,set,data,measure=measReach){
  vals = numeric(length(nextSet)) #set up storage for return value
  for(k in 1:length(nextSet)){ #loop over the options in nextSet
    if(length(set)==0){         #if no existing options
      vals[k] = measure(data[,nextSet[k]]) 
    } else {                    #if existing options
      vals[k] = measure(data[,c(set,nextSet[k])])
    }
  }
  vals
}

#evalFull: creates optimal full evaluation starting from origSet and considering remaining options fullSet
evalFull = function(fullSet,data,origSet=numeric(0),measure=measReach){
  curSet = origSet; #the current set of included options
  remSet = fullSet[!(fullSet%in%origSet)]; #the remaining set of options to consider
  K = length(remSet)
  optVals = numeric(K); #create storage for the optimal values (optVals)
  ordSet = numeric(K); #create storage for ordered set
  for(i in 1:K){          #loop over the remaining set consider
    tmpVals = evalNext(remSet,curSet,data,measure); #calculate vector of next evaluations
    k = which.max(tmpVals) #pick the option that gives max measure, note will pick first case if a tie!
    optVals[i] = tmpVals[k] #add optimal value
    ordSet[i] = remSet[k]   #add index of option that creates optimal value
    curSet = c(curSet,ordSet[i]); #add optimal next option to current set
    remSet = remSet[-k];          #delete optimal next option from remaining set
  }
  #creaets a "TURF object" containing ordSet, optVals, origSet, origVal, measure, and pnames
  turf = list(ordSet=ordSet,optVals=optVals,origSet=origSet,origVal=measure(data[,origSet]),measure=measure,pnames=colnames(data))
  class(turf)="TURF" #makes the list into a TURF object so that can call plot.TURF
  turf  #return turf
}
#creates ggplot barplot for a turf object
plot.TURF=function(turf,...){
  if(class(turf)!="TURF"){
    cat("Object not a turf.")
  } else {
    df = with(turf,data.frame(vals = c(origVal,optVals),titles=paste(0:length(ordSet),c("Original",pnames[ordSet]),sep=":")))
    #with(turf,barplot(c(origVal,optVals),names.arg=c("Original",pnames[ordSet])))
    dodge = position_dodge(width=.75); ##to form constant dimensions positioning for all geom's
    gp = ggplot(df,aes(y=vals,x=titles))
    gp + geom_bar(position=dodge,stat="identity",fill='steelblue')
  }
}


brandsPurch = yogurtNew
turf = evalFull(c(1:23),brandsPurch,c(4, 10, 15, 18, 21, 23))  #fill six original flavors
plot(turf)
turf
#the result shows that we should launch the pinapple flavor because of its highest incremental reach
