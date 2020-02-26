rm(list=ls())

library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(grf)

# import dataset
email = read.csv('/Users/liutianyao/Desktop/Analytics design/project4/test_data_1904.csv')
summary(email)
dt1 = data.table(email)
dagg = dt1[,.(open = mean(open), click=mean(click), purch = mean(purch),seOpen = sd(open)/sqrt(.N), seClick=sd(click)/sqrt(.N), sePurch = sd(purch)/sqrt(.N),.N),by = .(group)]
dagg
sum(email$open)/nrow(subset(email,group=='email')) #79.58%
sum(email$click)/nrow(subset(email,open=='1')) #19.91%

######### Question 1 ################
summary(lm(purch~group,data=email))
# the effect size is $1.34
# If group is email instead of control, the purchase will increase 1.34. 
# This is becuase that we didn't segment and target with people who have purchased, so we cannot make effective predictions.

# Q2
summary(lm(purch~group+chard+sav_blanc+syrah+cab+past_purch+last_purch+visits,data=email)) #adding baseline variables as controls

# We slice based on baseline variables
email$recentPurch = (email$last_purch < median(email$last_purch))
email$pastPurch = (email$past_purch > median(email$past_purch))
email$purchaser = (email$purch > 0)
email$frequency = (email$visits > median(email$visits))
email$purch_chard = (email$chard > 0)
email$purch_sav = (email$sav_blanc > 0)
email$purch_syrah = (email$syrah > 0)
email$purch_cab = (email$cab >0)

#calculate past purchase percentile
percentile = as.data.frame(quantile(email$past_purch,seq(0,1,0.02)))
percentile$percentile = rownames(percentile)
colnames(percentile) = c('past_purch','percentile')

ggplot(data=percentile, aes(x = reorder(percentile, past_purch), y=past_purch)) +
  geom_bar(stat="identity")


#calculate last purchase percentile
percentile2 = as.data.frame(quantile(email$last_purch,seq(0,1,0.02)))
percentile2$percentile = rownames(percentile2)
colnames(percentile2) = c('last_purch','percentile')

ggplot(data=percentile2, aes(x = reorder(percentile, last_purch), y=last_purch)) +
  geom_bar(stat="identity")


#calculate visits percentile
percentile3 = as.data.frame(quantile(email$visits,seq(0,1,0.02)))
percentile3$percentile = rownames(percentile3)
colnames(percentile3) = c('visits','percentile')

ggplot(data=percentile3, aes(x = reorder(percentile, visits), y=visits)) +
  geom_bar(stat="identity")

#work with recent purchase
dt = data.table(email)
dagg1 = dt[,.(open = mean(open), click=mean(click), purch = mean(purch),seOpen = sd(open)/sqrt(.N), seClick=sd(click)/sqrt(.N), sePurch = sd(purch)/sqrt(.N),.N),by = .(group,recentPurch)]
dagg1

dodge1 = position_dodge(width=1); ##to form constant dimensions
ggplot(aes(fill=group,y=purch,x=recentPurch,ymax=purch+sePurch,ymin=purch-sePurch),data=dagg1)+
  geom_bar(position=dodge1,stat="identity") + 
  geom_errorbar(position=dodge1)
labs(x="Group",y="Purchases")

summary(lm(purch~group*recentPurch,data=email))
summary(lm(purch~recentPurch+group:recentPurch,data=email)) 
#recent buyer
sum(subset(email,recentPurch==TRUE)$open)/nrow(subset(subset(email,recentPurch==TRUE),group=='email')) #90.25% open rate
sum(subset(email,recentPurch==TRUE)$click)/nrow(subset(subset(email,recentPurch==TRUE),open=='1')) #16.50% CTR (click through rate)
sum(subset(email,recentPurch==TRUE & click=='1')$purchaser)/nrow(subset(subset(email,recentPurch==TRUE),click=='1')) #54.67% conversion rate

#not recent buyer
sum(subset(email,recentPurch==FALSE)$open)/nrow(subset(subset(email,recentPurch==FALSE),group=='email')) #68.84% open rate
sum(subset(email,recentPurch==FALSE)$click)/nrow(subset(subset(email,recentPurch==FALSE),open=='1')) #17.45% CTR
sum(subset(email,recentPurch==FALSE & click=='1')$purchaser)/nrow(subset(subset(email,recentPurch==FALSE),click=='1')) #28.23% conversion rate

#work with past purchase
dagg2 = dt[,.(open = mean(open), click=mean(click), purch = mean(purch),seOpen = sd(open)/sqrt(.N), seClick=sd(click)/sqrt(.N), sePurch = sd(purch)/sqrt(.N),.N),by = .(group,pastPurch)]
dagg2
summary(lm(purch~group*pastPurch,data=email))
summary(lm(purch~pastPurch + group:pastPurch,data=email)) 
#big volume buyer
sum(subset(email,pastPurch==TRUE)$open)/nrow(subset(subset(email,pastPurch==TRUE),group=='email')) #93.06% open rate 
sum(subset(email,pastPurch==TRUE)$click)/nrow(subset(subset(email,pastPurch==TRUE),open=='1')) #19.25% CTR
sum(subset(email,pastPurch==TRUE & click=='1')$purchaser)/nrow(subset(subset(email,pastPurch==TRUE),click=='1')) #48.60% conversion rate
#not big volume buyer
sum(subset(email,pastPurch==FALSE)$open)/nrow(subset(subset(email,pastPurch==FALSE),group=='email')) #65.96% open rate
sum(subset(email,pastPurch==FALSE)$click)/nrow(subset(subset(email,pastPurch==FALSE),open=='1')) #13.58% CTR
sum(subset(email,pastPurch==FALSE & click=='1')$purchaser)/nrow(subset(subset(email,pastPurch==FALSE),click=='1')) #31.40% conversion rate

##work with frequency
dagg3 = dt[,.(open = mean(open), click=mean(click), purch = mean(purch),seOpen = sd(open)/sqrt(.N), seClick=sd(click)/sqrt(.N), sePurch = sd(purch)/sqrt(.N),.N),by = .(group,frequency)]
dagg3
summary(lm(purch~group*frequency,data=email))
summary(lm(purch~frequency + group:frequency,data=email)) 
#frequent buyer
sum(subset(email,frequency==TRUE)$open)/nrow(subset(subset(email,frequency==TRUE),group=='email')) #82.72% open rate
sum(subset(email,frequency==TRUE)$click)/nrow(subset(subset(email,frequency==TRUE),open=='1')) #17.44% CTR
sum(subset(email,frequency==TRUE & click=='1')$purchaser)/nrow(subset(subset(email,frequency==TRUE),click=='1')) #45.70% conversion rate
#not frequent buyer
sum(subset(email,frequency==FALSE)$open)/nrow(subset(subset(email,frequency==FALSE),group=='email')) #76.68% open rate
sum(subset(email,frequency==FALSE)$click)/nrow(subset(subset(email,frequency==FALSE),open=='1')) #16.39% CTR
sum(subset(email,frequency==FALSE & click=='1')$purchaser)/nrow(subset(subset(email,frequency==FALSE),click=='1')) #39.95% conversion rate


##work with different brand
# Different wine categories
# chard
dagg5 = dt[,.(open = mean(open), click=mean(click), purch = mean(purch),seOpen = sd(open)/sqrt(.N), seClick=sd(click)/sqrt(.N), sePurch = sd(purch)/sqrt(.N),.N),by = .(group,purch_chard)]
dagg5
summary(lm(purch~purch_chard+group:purch_chard,data=email))
#chard buyer
sum(subset(email,purch_chard==TRUE)$open)/nrow(subset(subset(email,purch_chard==TRUE),group=='email')) #93.80% open rate
sum(subset(email,purch_chard==TRUE)$click)/nrow(subset(subset(email,purch_chard==TRUE),open=='1')) #16.16% CTR
sum(subset(email,purch_chard==TRUE & click=='1')$purchaser)/nrow(subset(subset(email,purch_chard==TRUE),click=='1')) #54.27% conversion rate

#not chard buyer
sum(subset(email,purch_chard==FALSE)$open)/nrow(subset(subset(email,purch_chard==FALSE),group=='email')) #73.50% open rate
sum(subset(email,purch_chard==FALSE)$click)/nrow(subset(subset(email,purch_chard==FALSE),open=='1')) #17.32% CTR
sum(subset(email,purch_chard==FALSE & click=='1')$purchaser)/nrow(subset(subset(email,purch_chard==FALSE),click=='1')) #37.11% conversion rate

# sav
dagg6 = dt[,.(open = mean(open), click=mean(click), purch = mean(purch),seOpen = sd(open)/sqrt(.N), seClick=sd(click)/sqrt(.N), sePurch = sd(purch)/sqrt(.N),.N),by = .(group,purch_sav)]
dagg6
summary(lm(purch~purch_sav+group:purch_sav,data=email))
#sav buyer
sum(subset(email,purch_sav==TRUE)$open)/nrow(subset(subset(email,purch_sav==TRUE),group=='email')) #88.84% open rate
sum(subset(email,purch_sav==TRUE)$click)/nrow(subset(subset(email,purch_sav==TRUE),open=='1')) #23.11% CTR
sum(subset(email,purch_sav==TRUE & click=='1')$purchaser)/nrow(subset(subset(email,purch_sav==TRUE),click=='1')) #45.80% conversion rate

#not sav buyer
sum(subset(email,purch_sav==FALSE)$open)/nrow(subset(subset(email,purch_sav==FALSE),group=='email')) #75.85% open rate
sum(subset(email,purch_sav==FALSE)$click)/nrow(subset(subset(email,purch_sav==FALSE),open=='1')) #13.99% CTR
sum(subset(email,purch_sav==FALSE & click=='1')$purchaser)/nrow(subset(subset(email,purch_sav==FALSE),click=='1')) #40.64% conversion rate

# syrah
dagg7 = dt[,.(open = mean(open), click=mean(click), purch = mean(purch),seOpen = sd(open)/sqrt(.N), seClick=sd(click)/sqrt(.N), sePurch = sd(purch)/sqrt(.N),.N),by = .(group,purch_syrah)]
dagg7
summary(lm(purch~purch_syrah+ group:purch_syrah,data=email))
#syrah buyer
sum(subset(email,purch_syrah==TRUE)$open)/nrow(subset(subset(email,purch_syrah==TRUE),group=='email')) #83.54% open rate
sum(subset(email,purch_syrah==TRUE)$click)/nrow(subset(subset(email,purch_syrah==TRUE),open=='1')) #17.21% CTR
sum(subset(email,purch_syrah==TRUE & click=='1')$purchaser)/nrow(subset(subset(email,purch_syrah==TRUE),click=='1')) #41.35% conversion rate

#not syrah buyer
sum(subset(email,purch_syrah==FALSE)$open)/nrow(subset(subset(email,purch_syrah==FALSE),group=='email')) #79.05% open rate
sum(subset(email,purch_syrah==FALSE)$click)/nrow(subset(subset(email,purch_syrah==FALSE),open=='1')) #16.87% CTR
sum(subset(email,purch_syrah==FALSE & click=='1')$purchaser)/nrow(subset(subset(email,purch_syrah==FALSE),click=='1')) #43.13% conversion rate

# cab
dagg8 = dt[,.(open = mean(open), click=mean(click), purch = mean(purch),seOpen = sd(open)/sqrt(.N), seClick=sd(click)/sqrt(.N), sePurch = sd(purch)/sqrt(.N),.N),by = .(group,purch_cab)]
dagg8
summary(lm(purch~purch_cab+group:purch_cab,data=email))
#cab buyer
sum(subset(email,purch_cab==TRUE)$open)/nrow(subset(subset(email,purch_cab==TRUE),group=='email')) #88.62% open rate
sum(subset(email,purch_cab==TRUE)$click)/nrow(subset(subset(email,purch_cab==TRUE),open=='1')) #21.64% CTR
sum(subset(email,purch_cab==TRUE & click=='1')$purchaser)/nrow(subset(subset(email,purch_cab==TRUE),click=='1')) #45.04% conversion rate

#not cab buyer
sum(subset(email,purch_cab==FALSE)$open)/nrow(subset(subset(email,purch_cab==FALSE),group=='email')) #75.91% open rate
sum(subset(email,purch_cab==FALSE)$click)/nrow(subset(subset(email,purch_cab==FALSE),open=='1')) #14.67% CTR
sum(subset(email,purch_cab==FALSE & click=='1')$purchaser)/nrow(subset(subset(email,purch_cab==FALSE),click=='1')) #41.41% conversion rate


######### Question 3 ################
# Causal Forests
# set treatment, response and baseline
email$groupnew = (email$group == 'email')*1
treat <- email$groupnew
response <- email$purch
baseline <- email[7:13]
# construct causal forests
tmp=proc.time()[3]
cf <- causal_forest(baseline, response, treat)
tmp = proc.time()[3]-tmp
print(cf)
# calculate average treatment effect
average_treatment_effect(cf, method="AIPW")
new_cust <- data.frame(chard=30.84, sav_blanc=0, syrah=15.09, cab=196.86, last_purch=79, visits=7, past_purch=242.79)
# calculate individual-level scores
email$score <- predict(cf, email[7:13], estimate.variance = TRUE)$predictions *0.3-0.1
# plot histogram
hist(email$score, main="Histogram of Individual Scores", xlab="Individual Scores", ylab="Frequency",col='skyblue')

# add targeting indicator
email$indicator = (email$score>0)*1
nrow(subset(email,email$score>0))/nrow(email) #target 55.79% of the total

#summary of the baseline variables of target and non-target group
summary(subset(email[7:13],email$indicator == '1'))
summary(subset(email[7:13],email$indicator == '0'))

#write csv
setwd('/Users/liutianyao/Desktop')
write.table(email[c('user_id', 'score', 'indicator')],'consumer_scores.csv')




