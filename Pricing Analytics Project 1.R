#Load packages lfd and data.table
rm(list = ls())
library("lfe")
library("data.table")
setwd("~/Desktop/project1")

#Load data
cardata=fread("cars.csv",stringsAsFactors = F)
iron = fread('iron_ore.csv',stringsAsFactors = F)

###Q4
#run a log-log regression with year as fixed effect and "li" as a control variable
reg4_0=felm(log(qu)~log(eurpr)+li|factor(ye)+factor(co)+factor(ma),data=cardata)
summary(reg4_0)

#We can add multiple FE separately, or interact them. This is how you add two separate fixed effects
#one for each year (same value across all car models) and the other for each car model (same value across all years)
reg4_1=felm(log(qu)~log(eurpr)+li|factor(ye):factor(co),data=cardata)
summary(reg4_1)
# the coefficient of log(p) is positive, which is not reasonable so we continue to try 
reg4_2=felm(log(qu)~log(eurpr)+li|factor(ye)+factor(co)+factor(ma),data=cardata)
summary(reg4_2)

# the coefficient of log(p) is negative, which does make sense, but ye and co are related so we use interraction
val=lm(ye~co, data = cardata)
summary(val)

reg4_3=felm(log(qu)~log(eurpr)+li|factor(ye):factor(co)+factor(ma),data=cardata)
summary(reg4_3)

###Q5
#Run a log-log regression with "tax" and 'unit price 'weight' as IV for log(P).

iron$ye=iron$year-1900
cardata1 = merge(iron, cardata, by = 'ye')

cardata1$mac = cardata1$unit_value_98*cardata1$we #cauculate total material cost per car

reg5=felm(log(qu)~li|factor(ye):factor(co)+factor(ma)|(log(eurpr)~tax+mac),data=cardata1)
summary(reg5)

summary(lm(log(qu)~factor(loc),data = cardata1))

#if you don't include any X variable in
#an IV regression, simply place "1" in place of X.
reg5_2=felm(log(qu)~1|factor(ye):factor(co)+factor(ma)|(log(eurpr)~tax+mac),data=cardata1)
summary(reg5_2)

#if you don't include fixed effects in 
#an IV regression, place "0" in place of factor variable.
reg5_3=felm(log(qu)~li|0|(log(eurpr)~tax+mac),data=cardata1)
summary(reg5_3)

###Q6
# We chose row 88 for analysis
a = cardata1[88,]

fe = getfe(reg5)
idc <- match('70.411',fe$idx)
fe_ye_co=fe[idc,1] # effect is 34.23707
idc2 <- match('2',fe$idx)
fe_market=fe[idc2,1] #effect is 1.44188
total_fe = fe_ye_co+fe_market #total fixed effect is 35.67895
total_fe

# log(Q) = beta1*log(price)+fe+cv
# log(Q) = log[(price)^beta1]+log(fe+cv)
# Q = P^beta * e^(fe+cv)
# P(optimal)  =  	2350.145
Q = a$eurpr^reg5$beta[2] * exp(1)^(total_fe+a$li*reg5$beta[1])
Q
# Q predicted as 2042.238

# profit = revenue-cost = (P-AVC)*Q
# = P * P^beta * e^(FE+CV) - AVC * P^beta * e^(FE+CV)
# = P^(beta+1) * e^(FE+CV) - AVC * P^beta * e^(FE+CV)
#d(Profit)/d(P) = reg4$beta[2]*P- AVC * P^(reg4$beta[2]-1) * e^(FE+CV)* reg4$beta[2]
#let left equal to 0 : 
#0 = P^(beta)*(beta+1)* e^(FE+CV)- AVC * P^(beta-1) * e^(FE+CV) * beta

# 0 = exp(1)^(total_fe++a$li*reg4$beta[1])*(reg4$beta[2]+1)*a$eurpr^reg4$beta[2]-reg4$beta[2]*a$eurpr^(reg4$beta[2]-1)*exp(1)^(total_fe+a$li*reg4$beta[1])* AVC
AVC1 = exp(1)^(total_fe+a$li*reg5$beta[1])*(reg5$beta[2]+1)*a$eurpr^reg5$beta[2]/reg5$beta[2]*a$eurpr^(reg5$beta[2]-1)*exp(1)^(total_fe+a$li*reg5$beta[1])
AVC1
#So the cost is equal to 780.0417.


###Q7 get rival's price
# Taking the rival effect into consideration, we included log(avgurprrival) in control variable. 
reg7_1=felm(log(qu)~li+log(avgurprrival)|factor(ye):factor(co)+factor(ma)|(log(eurpr)~tax+mac),data=cardata1)
summary(reg7_1)

#consider competitor's IV
reg8_2=felm(log(qu)~li|factor(ye):factor(co)+factor(ma):engdpc|(log(eurpr)~we+tax+loc+xexr)|(log(avgurprrival)~avgcyrival+avghprival+avgwerival+avglirival),data=cardata)
summary(reg8_2)

