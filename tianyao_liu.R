### Topic Modeling Homework ###

rm(list=ls())
### load required packages 
install.packages('tm')
library(tm)
install.packages('NMF')
library(NMF)
install.packages('ggplot2')
library(ggplot2)


### import the data 
docs<-Corpus(DirSource("/Users/liutianyao/Desktop/Social Media /HW4/fbtotal"))


### transformations and dtm building 
stp = c(stopwords("english"), stopwords("spanish"),stopwords("portuguese"))
control=list(tolower=T, removePunctuation=T, removeNumbers=T, stripWhitespace=T, 
             stopwords=stp)
dtm = DocumentTermMatrix(docs, control=control)

dtm = removeSparseTerms(dtm,0.97)


### get frequency 
freq = as.matrix(dtm)

freq_cauli = data.frame(freq[,'cauliflower'])
freq_cauli = cbind(rownames(freq_cauli),freq_cauli)

names(freq_cauli) = c('TimeOfPosts','Frequency')

### generate a list containing frequency of 'cauliflower' in each month
## Here I used a for loop to generate the list containing each month in the frequency table
l = list()
for (i in seq(1, nrow(freq_cauli))){
  a = unlist(strsplit(unlist(strsplit(row.names(freq_cauli)[i], "-")),'.csv')) #get year-month from each file name
  length3 <- length(unlist(strsplit(a[3],'')))
  if (length3 == 1){
    a[3] = paste('0',a[3],sep = '')
  } #add '0' before month 1 to 10 to fit the real time sequence for later use
  l[i] = paste(a[2],a[3],sep = '-') #Fill in the empty list with year-month
}

l
freq_cauli['TimeOfPosts'] = unlist(l)
freq_cauli <- freq_cauli[order(freq_cauli['TimeOfPosts']),]  #order the dataframe by time sequence to observe trend over time
freq_rank <- freq_cauli[order(freq_cauli['Frequency'],decreasing = TRUE),]  #have a clear look of the frequency ranking, with 2015-11 ranks first

###plot the trend over time using ggplot
ggplot(freq_cauli, 
       aes(x=TimeOfPosts, y=Frequency, group=1)) + 
  geom_line(linetype='longdash', color = 'blue') + 
  theme(axis.text.x=element_blank()) +
  ggtitle('Cauliflower Trend') +
  geom_point(color='purple', size=1)
  ## I found that there is an upward trend of the frequency of 'cauliflower' 
  ## mentioned in FB posts from 2011 to 2015.



###
### do the same on zoodle
freq_z = data.frame(freq[,'zoodle'])
freq_z = cbind(rownames(freq_z),freq_z)

names(freq_z) = c('TimeOfPosts','Frequency')

### generate a list containing frequency of 'zoodle' in each month
## Here I used a for loop to generate the list containing each month in the frequency table
l = list()
for (i in seq(1, nrow(freq_z))){
  a = unlist(strsplit(unlist(strsplit(row.names(freq_z)[i], "-")),'.csv')) #get year-month from each file name
  length3 <- length(unlist(strsplit(a[3],'')))
  if (length3 == 1){
    a[3] = paste('0',a[3],sep = '')
  } #add '0' before month 1 to 10 to fit the real time sequence for later use
  l[i] = paste(a[2],a[3],sep = '-') #Fill in the empty list with year-month
}

l
freq_z['TimeOfPosts'] = unlist(l)
freq_z <- freq_z[order(freq_z['TimeOfPosts']),]  #order the dataframe by time sequence to observe trend over time
freqz_rank <- freq_z[order(freq_z['Frequency'],decreasing = TRUE),]  #have a clear look of the frequency ranking, with 2015-7 ranks first

###plot the trend over time using ggplot
ggplot(freq_z, 
       aes(x=TimeOfPosts, y=Frequency, group=1)) + 
  geom_line(linetype='longdash', color = 'blue') + 
  theme(axis.text.x=element_blank()) +
  ggtitle('Zoodle Trend') +
  geom_point(color='purple', size=1)
## I found that there is an upward trend of the frequency of 'zoodle' 
## mentioned in FB posts from 2011 to 2015.