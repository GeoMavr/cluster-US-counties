
library('pgmm')
library('nnet')
library('class')
library('e1071')	#first install class and then this one
library('penalizedLDA')
library('MASS')
library('heplots')
library('tree')
library('mclust')
library("readxl")
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(NbClust)
library('cluster')
library('NbClust')
library(dplyr)
library(caret)
library(dplyr)

data1 <- read_excel("stat BA II project I.xlsx", sheet = "county_facts")
data1<-data1[complete.cases(data1),]
data<-data1[,4:54]

############ remove outliers ################
outliers <- function(x) {
  
  Q1 <- quantile(x, probs=.01)
  Q3 <- quantile(x, probs=.99)
  iqr = Q3-Q1
  
  upper_limit = Q3 + (iqr*1.5)
  lower_limit = Q1 - (iqr*1.5)
  
  x > upper_limit | x < lower_limit
}

remove_outliers <- function(df, cols = names(df)) {
  for (col in cols) {
    df <- df[!outliers(df[[col]]),]
  }
  df
}

data<-remove_outliers(data,c('PST045214','PST040210','PST120214','POP010210',
                             'AGE135214','AGE295214','AGE775214','SEX255214',
                             'RHI125214','RHI225214','RHI325214','RHI425214',
                             'RHI525214','RHI625214','RHI725214','RHI825214',
                             'POP715213','POP645213','POP815213','EDU635213',
                             'EDU685213','VET605213','LND110210','POP060210'))
###########################################
#standardize the data
scaled<-as.data.frame(scale(data))
cluster_data<-scaled[,c('PST045214','PST040210','PST120214','POP010210',
                      'AGE135214','AGE295214','AGE775214','SEX255214',
                      'RHI125214','RHI225214','RHI325214','RHI425214',
                      'RHI525214','RHI625214','RHI725214','RHI825214',
                      'POP715213','POP645213','POP815213','EDU635213',
                      'EDU685213','VET605213','LND110210','POP060210')]




#hierarchical clustering:
par(mfrow=c(2,2))
hc2 <- hclust(dist(cluster_data), method="complete")
plot(hc2$height,main='Complete Linkage',ylab='Height')


#	Hierarchical with other linkage
hc3 <- hclust(dist(cluster_data),method="ward.D")
plot(hc3$height,main='Ward Linkage',ylab='Height')

hc4 <- hclust(dist(cluster_data),method="single")

plot(hc4$height,main='Single Linkage',ylab='Height')


hc5 <- hclust(dist(cluster_data),method="centroid")

plot(hc5$height,main='Centroid Linkage',ylab='Height')

#plot silhouette 
par(mfrow=c(2,2))
plot(silhouette(cutree(hc2, k = 4), dist(cluster_data)),main ='complete',border=NA)
plot(silhouette(cutree(hc3, k = 4), dist(cluster_data)),main ='ward',border=NA)
plot(silhouette(cutree(hc4, k = 4), dist(cluster_data)),main ='single',border=NA)
plot(silhouette(cutree(hc5, k = 4), dist(cluster_data)),main ='centroid',border=NA)

####k-means

set.seed(123)
km4<-kmeans(cluster_data, centers = 4, nstart = 30)
plot(silhouette(km4$cluster, dist(cluster_data)),main ='ward',border=NA)

cluster<-km4$cluster
class1<-cutree(hc3, k = 4)
table(cluster,cutree(hc3, k = 4))
adjustedRandIndex(cluster,class1)



# function to compute total within-cluster sum of square 
set.seed(123)

fviz_nbclust(cluster_data, kmeans, method = "wss")


##### we wil try to explain the clusters based on the economic characteristics

demographic<-c('PST045214','PST040210','PST120214','POP010210',
               'AGE135214','AGE295214','AGE775214','SEX255214',
               'RHI125214','RHI225214','RHI325214','RHI425214',
               'RHI525214','RHI625214','RHI725214','RHI825214',
               'POP715213','POP645213','POP815213','EDU635213',
               'EDU685213','VET605213','LND110210','POP060210','cluster')

economic<-c('LFE305213','HSG010214','HSG445213','HSG096213','HSG495213','HSD410213',
            'HSD310213','INC910213','INC110213','PVY020213','BZA010213','BZA110213',
            'BZA115213','NES010213','SBO001207','SBO315207','SBO115207','SBO215207',
            'SBO515207','SBO415207','SBO015207','MAN450207','WTN220207','RTN130207',
            'RTN131207','AFN120207','BPS030214','cluster')

data$cluster<-km4$cluster #assign the cluster column to the dataset
scaled$cluster<-km4$cluster #assign the cluster column to the scale dataset

datanew<-data[,economic] # keep only economic related columns
dataold<-data[,demographic] # keep demographic related columns



#### manova
res.man <- manova(cbind(PST045214,PST040210,PST120214,POP010210,AGE135214,AGE295214,
                        AGE775214,SEX255214,RHI125214,RHI225214,RHI325214,RHI425214,
                        RHI525214,RHI625214,RHI725214,RHI825214,POP715213,POP645213,
                        POP815213,EDU635213,EDU685213,VET605213,LND110210,POP060210) ~ cluster, data = dataold)
summary(res.man)

### wilks for k-menas
clas1<-km4$cluster
demo<-as.matrix(dataold[,-25])
m <- manova(demo~clas1)
summary(m,test="Wilks")

mywilks<- summary(m,test="Wilks")$stats[1,2]


#### wilks for clustering
clas2<-cutree(hc3, k = 4)
demo<-as.matrix(dataold[,-25])
m <- manova(demo~clas2)
summary(m,test="Wilks")

mywilkshc3<- summary(m,test="Wilks")$stats[1,2]

###############

library("redux")
library("lessR")
library(ggplot2)
avgpercluster<-aggregate(dataold, list(dataold$cluster), mean) #calculate average column values per cluster
avgpercluster<-data.frame(t(avgpercluster))
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

avgcluster<-round_df(avgpercluster,2)
names(avgcluster) <- avgcluster[1,]
avgcluster<-avgcluster[-1,]
#reassign the cluster names for visualizations purposes
for(i in 1:nrow(dataold)){
  if (dataold[i,'cluster']==1){
    dataold$cluster[i]<-4
  } else if (dataold[i,'cluster']==2){
    dataold$cluster[i]<-3
  }
  else if (dataold[i,'cluster']==3){
    dataold$cluster[i]<-2
  }else{
    dataold$cluster[i]<-1
  }
}

clusters<-dataold$cluster
#clusplot(cluster_data,clusters, color=TRUE,labels=1,main = 'cluster plot')
datanew$cluster<-clusters
dataold<-as.data.frame(scale(dataold))
dataold$cluster<-clusters

############the following lines contai some visualizations related to cluster characteristics
library(ggpubr)
c1<-dataold[dataold$cluster==1,]

d1<-ggplot(data.frame(mean = colMeans(c1[,c('PST045214','SEX255214','AGE135214','AGE295214','AGE775214','EDU635213','EDU685213')]),
                      Demographic = names(c1[,c('PST045214','SEX255214','AGE135214','AGE295214','AGE775214','EDU635213','EDU685213')]))) +ggtitle('Cluster 1')+
  geom_col(aes(Demographic, mean)) +coord_flip()+ scale_x_discrete(labels = c("PST045214" = "Popoualtion2014","SEX255214" = "Female%",'AGE135214'='AgeUnder5','AGE295214'='AgeUnder18',
                                                                              'AGE775214'='AgeOver65','EDU635213'='HighschoolGrad','EDU685213'='BachelorGrad'))


d2<-ggplot(data.frame(mean = colMeans(c1[,c('RHI125214','RHI225214','RHI325214','RHI425214','RHI525214','RHI625214','RHI725214','RHI825214')]),
                      Demographic = names(c1[,c('RHI125214','RHI225214','RHI325214','RHI425214','RHI525214','RHI625214','RHI725214','RHI825214')]))) + ggtitle('Cluster 1')+
  geom_col(aes(Demographic, mean)) +coord_flip()+ scale_x_discrete(labels=c('RHI125214'='white%','RHI225214'='AfricanAmerican%','RHI325214'='AmericanIndian%',
                                                                            'RHI425214'='Asian%','RHI525214'='Hawaiian%','RHI625214'='Race2+','RHI725214'='Hispanic%','RHI825214'='whiteNOlatino' ))


d3<-ggplot(data.frame(mean = colMeans(c1[,c('POP715213','POP645213','POP815213','LND110210','POP060210')]),
                      Demographic = names(c1[,c('POP715213','POP645213','POP815213','LND110210','POP060210')]))) + ggtitle('Cluster 1')+
  geom_col(aes(Demographic, mean)) +coord_flip()+ scale_x_discrete(labels=c('POP715213'='SameHouse1+Year','POP645213'='ForeignBorn',
                                                                            'POP815213'='OtherLanguage','LND110210'='LandAreasqml','POP060210'='population/sqml'))




c2<-dataold[dataold$cluster==2,]

d4<-ggplot(data.frame(mean = colMeans(c2[,c('PST045214','SEX255214','AGE135214','AGE295214','AGE775214','EDU635213','EDU685213')]),
                      Demographic = names(c2[,c('PST045214','SEX255214','AGE135214','AGE295214','AGE775214','EDU635213','EDU685213')]))) + ggtitle('Cluster 2')+
  geom_col(aes(Demographic, mean)) +coord_flip()+ scale_x_discrete(labels = c("PST045214" = "Popoualtion2014","SEX255214" = "Female%",'AGE135214'='AgeUnder5','AGE295214'='AgeUnder18',
                                                                              'AGE775214'='AgeOver65','EDU635213'='HighschoolGrad','EDU685213'='BachelorGrad'))

d5<-ggplot(data.frame(mean = colMeans(c2[,c('RHI125214','RHI225214','RHI325214','RHI425214','RHI525214','RHI625214','RHI725214','RHI825214')]),
                      Demographic = names(c2[,c('RHI125214','RHI225214','RHI325214','RHI425214','RHI525214','RHI625214','RHI725214','RHI825214')]))) +ggtitle('Cluster 2')+
  geom_col(aes(Demographic, mean)) +coord_flip()+ scale_x_discrete(labels=c('RHI125214'='white%','RHI225214'='AfricanAmerican%','RHI325214'='AmericanIndian%',
                                                                            'RHI425214'='Asian%','RHI525214'='Hawaiian%','RHI625214'='Race2+','RHI725214'='Hispanic%','RHI825214'='whiteNOlatino' ))


d6<-ggplot(data.frame(mean = colMeans(c2[,c('POP715213','POP645213','POP815213','LND110210','POP060210')]),
                      Demographic = names(c2[,c('POP715213','POP645213','POP815213','LND110210','POP060210')]))) +ggtitle('Cluster 2')+
  geom_col(aes(Demographic, mean)) +coord_flip()+ scale_x_discrete(labels=c('POP715213'='SameHouse1+Year','POP645213'='ForeignBorn',
                                                                            'POP815213'='OtherLanguage','LND110210'='LandAreasqml','POP060210'='population/sqml'))



c3<-dataold[dataold$cluster==3,]

d7<-ggplot(data.frame(mean = colMeans(c3[,c('PST045214','SEX255214','AGE135214','AGE295214','AGE775214','EDU635213','EDU685213')]),
                      Demographic = names(c3[,c('PST045214','SEX255214','AGE135214','AGE295214','AGE775214','EDU635213','EDU685213')]))) +ggtitle('Cluster 3')+
  geom_col(aes(Demographic, mean)) +coord_flip()+ scale_x_discrete(labels = c("PST045214" = "Popoualtion2014","SEX255214" = "Female%",'AGE135214'='AgeUnder5','AGE295214'='AgeUnder18',
                                                                              'AGE775214'='AgeOver65','EDU635213'='HighschoolGrad','EDU685213'='BachelorGrad'))

d8<-ggplot(data.frame(mean = colMeans(c3[,c('RHI125214','RHI225214','RHI325214','RHI425214','RHI525214','RHI625214','RHI725214','RHI825214')]),
                      Demographic = names(c3[,c('RHI125214','RHI225214','RHI325214','RHI425214','RHI525214','RHI625214','RHI725214','RHI825214')]))) +ggtitle('Cluster 3')+
  geom_col(aes(Demographic, mean)) +coord_flip()+ scale_x_discrete(labels=c('RHI125214'='white%','RHI225214'='AfricanAmerican%','RHI325214'='AmericanIndian%',
                                                                            'RHI425214'='Asian%','RHI525214'='Hawaiian%','RHI625214'='Race2+','RHI725214'='Hispanic%','RHI825214'='whiteNOlatino' ))


d9<-ggplot(data.frame(mean = colMeans(c3[,c('POP715213','POP645213','POP815213','LND110210','POP060210')]),
                      Demographic = names(c3[,c('POP715213','POP645213','POP815213','LND110210','POP060210')]))) +ggtitle('Cluster 3')+
  geom_col(aes(Demographic, mean)) +coord_flip()+ scale_x_discrete(labels=c('POP715213'='SameHouse1+Year','POP645213'='ForeignBorn',
                                                                            'POP815213'='OtherLanguage','LND110210'='LandAreasqml','POP060210'='population/sqml'))




c4<-dataold[dataold$cluster==4,]

d10<-ggplot(data.frame(mean = colMeans(c4[,c('PST045214','SEX255214','AGE135214','AGE295214','AGE775214','EDU635213','EDU685213')]),
                       Demographic = names(c4[,c('PST045214','SEX255214','AGE135214','AGE295214','AGE775214','EDU635213','EDU685213')]))) +ggtitle('Cluster 4')+
  geom_col(aes(Demographic, mean)) +coord_flip()+ scale_x_discrete(labels = c("PST045214" = "Popoualtion2014","SEX255214" = "Female%",'AGE135214'='AgeUnder5','AGE295214'='AgeUnder18',
                                                                              'AGE775214'='AgeOver65','EDU635213'='HighschoolGrad','EDU685213'='BachelorGrad'))


d11<-ggplot(data.frame(mean = colMeans(c4[,c('RHI125214','RHI225214','RHI325214','RHI425214','RHI525214','RHI625214','RHI725214','RHI825214')]),
                       Demographic = names(c4[,c('RHI125214','RHI225214','RHI325214','RHI425214','RHI525214','RHI625214','RHI725214','RHI825214')]))) +ggtitle('Cluster 4')+
  geom_col(aes(Demographic, mean)) +coord_flip()+ scale_x_discrete(labels=c('RHI125214'='white%','RHI225214'='AfricanAmerican%','RHI325214'='AmericanIndian%',
                                                                            'RHI425214'='Asian%','RHI525214'='Hawaiian%','RHI625214'='Race2+','RHI725214'='Hispanic%','RHI825214'='whiteNOlatino' ))


d12<-ggplot(data.frame(mean = colMeans(c4[,c('POP715213','POP645213','POP815213','LND110210','POP060210')]),
                       Demographic = names(c4[,c('POP715213','POP645213','POP815213','LND110210','POP060210')]))) +ggtitle('Cluster 4')+
  geom_col(aes(Demographic, mean)) +coord_flip()+ scale_x_discrete(labels=c('POP715213'='SameHouse1+Year','POP645213'='ForeignBorn',
                                                                            'POP815213'='OtherLanguage','LND110210'='LandAreasqml','POP060210'='population/sqml'))

ggarrange(d1,d4,d7,d10,ncol = 2, nrow = 2)
ggarrange(d2,d5,d8,d11,ncol = 2, nrow = 2)
ggarrange(d3,d6,d9,d12,ncol = 2, nrow = 2)

#Visualizations of economic related variables

########### visualizations of housing units and homeonwership raye
table(data$cluster) 
library(ggplot2)
n1<-ggplot(datanew, aes(x=as.factor(cluster), y=HSG010214)) + geom_boxplot()+ggtitle('Housing units per cluster') +xlab('Cluster')
n2<-ggplot(datanew, aes(x=as.factor(cluster), y=HSG445213)) + geom_boxplot()+ggtitle('Homeownership rate per cluster')+xlab('Cluster')
ggarrange(n1,n2,ncol = 2, nrow = 1)

######## visualizations of poverty,median income and persons perhousehold 

n3<-ggplot(datanew, aes(x=as.factor(cluster), y=INC110213)) + geom_boxplot()+ggtitle('Median income per cluster')+xlab('Cluster')
n4<-ggplot(datanew, aes(x=as.factor(cluster), y=HSD310213)) + geom_boxplot()+ggtitle('Persons per household')+xlab('Cluster')
n5<-ggplot(datanew, aes(x=as.factor(cluster), y=PVY020213)) + geom_boxplot()+ggtitle('Poverty rate per cluster')+xlab('Cluster')
n6<-ggplot(datanew, aes(x=as.factor(cluster), y=HSD410213)) + geom_boxplot()+ggtitle('Households per cluster')+xlab('Cluster')
ggarrange(n3,n4,n5,n6,ncol = 2, nrow = 2)

##################### visualization of firms
require(reshape2)
firms <- melt(tapply(datanew$SBO001207, datanew$cluster,mean), varnames="cluster", value.name="mean")

n7<-ggplot(firms, aes(x=cluster,y=mean)) + geom_bar(position="dodge", stat="identity")+ggtitle('Total number of firms per cluster')+xlab('Cluster')


blackfirm<- melt(tapply(datanew$SBO315207, datanew$cluster,mean), varnames="cluster", value.name="mean")

n8<-ggplot(blackfirm, aes(x=cluster,y=mean)) + geom_bar(position="dodge", stat="identity")+ggtitle('Black-owned firms per cluster')+xlab('Cluster')


asianfirm <- melt(tapply(datanew$SBO215207, datanew$cluster,mean), varnames="cluster", value.name="mean")

n9<-ggplot(asianfirm, aes(x=cluster,y=mean)) + geom_bar(position="dodge", stat="identity")+ggtitle('Asian-owned firms per cluster')+xlab('Cluster')


hispanicfirm <- melt(tapply(datanew$SBO415207, datanew$cluster,mean), varnames="cluster", value.name="mean")

n10<-ggplot(hispanicfirm, aes(x=cluster,y=mean)) + geom_bar(position="dodge", stat="identity")+ggtitle('Hispanic-owned firms per cluster')+xlab('Cluster')

ggarrange(n7,n8,n9,n10,ncol = 2, nrow = 2)

################# visualizations of sales
retail_sales <- melt(tapply(datanew$RTN130207, datanew$cluster,mean), varnames="cluster", value.name="mean")

n11<-ggplot(retail_sales, aes(x=cluster,y=mean)) + geom_bar(position="dodge", stat="identity")+ggtitle('Retail sales per cluster')+xlab('Cluster')


accomodation_sales <- melt(tapply(datanew$AFN120207, datanew$cluster,mean), varnames="cluster", value.name="mean")

n12<-ggplot(accomodation_sales, aes(x=cluster,y=mean)) + geom_bar(position="dodge", stat="identity")+ggtitle('Accommodation and food services sales')+xlab('Cluster')


building_merits <- melt(tapply(datanew$BPS030214, datanew$cluster,mean), varnames="cluster", value.name="mean")

n13<-ggplot(building_merits, aes(x=cluster,y=mean)) + geom_bar(position="dodge", stat="identity")+ggtitle('Building merits')+xlab('Cluster')

ggarrange(n11,n12,n13,ncol = 3, nrow = 1)



