install.packages("cluster")
install.packages("fpc")

library(dplyr)
library(amap)
library(lubridate)
library(tidyr)
library(clusteval)
library(lubridate)
library(cluster)
library(fpc)
library(Hmisc)
library(ggplot2)
library(ggfortify)
library(readr)

#Preprocessing
dframe <- read.csv("hourly_2007.csv", header = TRUE, sep = "") #read the data
dim(dframe)                                                #prints the number of rows and the numbers of attributes
dframe<- subset(dframe, select=c(1,3,4,5,9,13))            #selecting attributes specified: STN,yearModa_hr,Temp,DewP,STP,WDSP
dim(dframe)
head(dframe)                                               #prints first few rows, we can see that the column names are messed up for the selected attributes

colnames(dframe)<-c("station","date","temp","dewpoint","stationpressure","windspeed") #assigning the proper column names for the selected attributes
head(dframe)

df<-dframe
df<-separate(df,date,c("d","hours"))  #separates date and hours
head(df)
#     station        d    hours  temp     dewpoint  stationpressure  windspeed
#1    690190   20070201     0    48.64     36.3           944.5       6.0
#2    690190   20070201     1    41.92     36.3           944.5       6.0

df<-transform(df, Year = substr(d, 1, 4), Month = substr(d, 5, 6),day = substr(d,7,8)) #separates date into year month and day
df
#     station    d         hours  temp    dewpoint  stationpressure   windspeed   Year   Month day
#1    690190     20070201     0   48.64     36.3           944.5       6.0        2007    02   01
#2    690190     20070201     1   41.92     36.3           944.5       6.0        2007    02   01

df <- subset(df, Month == "06") #select all the records with month = 06
head(df)
nrow(df)          #176544 rows slected for the month june(06) from 158 stations for the year 2007 data
table(df$station) #shows number of rows for june month for different stations

table(df$temp==999.9)                 #0 rows have 9999.9 value
table(df$dewpoint==9999.9)             #9456 rows have 9999.9 value
table(df$stationpressure==9999.9)      #86328 rows have 9999.9 value
table(df$windspeed==999.9)             #7224 rows have 999.9 value

#Replacing 9's in particular field with 0 value
df <- df %>% mutate(temp = replace(temp, temp == 999.9, 0))
df <- df %>% mutate(dewpoint = replace(dewpoint, dewpoint == 9999.9, 0));
df <- df %>% mutate(stationpressure = replace(stationpressure, stationpressure == 9999.9, 0));
df <- df %>% mutate(windspeed = replace(windspeed, windspeed == 999.9, 0));

table(df$temp==0)   #0
table(df$dewpoint==0) #9456
table(df$stationpressure==0)#86328
table(df$windspeed==0)#7224

dfgroup <- group_by(df,station) #grouping existing table data into station based grouped tables
dfgroup

#Taking monthly average of Temp,DewP,STP,WDSP for june month for all the station present in our data, ignoring the NA values
avg<-summarise(dfgroup,temp_avg=mean(temp,na.rm=TRUE),dp_avg=mean(dewpoint,na.rm=TRUE),
               sp_avg=mean(stationpressure,na.rm=TRUE),ws_avg=mean(windspeed,na.rm=TRUE))
avg
#   station temp_avg  dp_avg sp_avg ws_avg
#1  690190     79.9   66.1   475.   6.94
#2  720110     79.9   72.6   469.   3.94
#3  720151     79.9   51.2   434.   6.14

stations <-read.csv('stations.csv')        #Reading the stations.csv
stations
stations <- subset(stations,select = c(1))   #selecting first column which has station numbers
stations

avg_common<-subset(avg, station %in% stations$StationNumber)
avg_common     #selecting common stations between stations.csv and all stations in our june data, 2 stations were not part of stations.csv, remove those from our data
table(avg_common$station)

#scaling the data to normalize the weights
avg_common[,2:5] = scale(avg_common[,2:5])
avg_common
# 1  690190  -0.356   0.0732 -0.187   0.101 
# 2  720110   0.103   0.512  -0.203  -1.23  

#-------------------------------------------------------------------------------------------------------

#Kmeans clustering for year 2007, with same seed value=30 and k value from 2 to 8, and random centroid values
#Elbow method to find best k

#euclidean method for k = 2 to 8 and same seed value
set.seed(30)
clusters_euclidean_2007<-Kmeans(avg_common[,2:5],2, method = "euclidean",nstart =10,iter.max = 400)
cluster_euclidean_bestk<-clusters_euclidean_2007
cluster_euclidean_bestk
clusters_euclidean_2007$withinss #5.791232 6.016258
sum(clusters_euclidean_2007$withinss)#11.80749
plotcluster(avg_common, clusters_euclidean_2007$cluster,xlab="Cluster Analysis")
autoplot(cluster_euclidean_bestk,avg_common, frame=TRUE, annotations = c("=", "title:euclidean best k"))

set.seed(30)
clusters_euclidean_2007<-Kmeans(avg_common[,2:5],3, method = "euclidean",nstart =10,iter.max = 400)
clusters_euclidean_2007$withinss #5.791232 5.307244 1.720888
sum(clusters_euclidean_2007$withinss)#12.81936
clusters_euclidean_2007
plotcluster(avg_common, clusters_euclidean_2007$cluster,xlab="Cluster Analysis")
autoplot(clusters_euclidean_2007,avg_common, frame=TRUE)

set.seed(30)
clusters_euclidean_2007<-Kmeans(avg_common[,2:5],4, method = "euclidean",nstart =10,iter.max = 400)
clusters_euclidean_2007
clusters_euclidean_2007$withinss #5.7912324 1.5162975 2.6423463 0.4084578
sum(clusters_euclidean_2007$withinss)#10.35833
plotcluster(avg_common, clusters_euclidean_2007$cluster,xlab="Cluster Analysis")
autoplot(clusters_euclidean_2007,avg_common, frame=TRUE)

set.seed(30)
clusters_euclidean_2007<-Kmeans(avg_common[,2:5],5, method = "euclidean",nstart =10,iter.max = 400)
clusters_euclidean_2007
clusters_euclidean_2007$withinss #1.5422269 0.8436286 0.0740086 5.7912324 1.5463525
sum(clusters_euclidean_2007$withinss)#9.797449
plotcluster(avg_common, clusters_euclidean_2007$cluster,xlab="Cluster Analysis")
autoplot(clusters_euclidean_2007,avg_common, frame=TRUE)

set.seed(30)
clusters_euclidean_2007_7<-Kmeans(avg_common[,2:5],7, method = "euclidean",nstart =10, iter.max = 400)
clusters_euclidean_2007_7
clusters_euclidean_2007$withinss#1.5422269 0.8436286 0.0740086 5.7912324 1.5463525
sum(clusters_euclidean_2007$withinss)# 9.797449
plotcluster(avg_common, clusters_euclidean_2007_7$cluster,xlab="Cluster Analysis")
autoplot(clusters_euclidean_2007_7,avg_common, frame=TRUE)

#pearson method for k = 2 to 8
set.seed(30)
clusters_pearson_2007<-Kmeans(avg_common[,2:5],2, method = "pearson",nstart =5, iter.max = 400)
clusters_pearson_bestk<-clusters_pearson_2007
clusters_pearson_bestk
clusters_pearson_2007$withinss #0.1276273 0.4876751
sum(clusters_pearson_2007$withinss)#0.6153024
plotcluster(avg_common, clusters_pearson_2007$cluster,xlab="Cluster Analysis")
autoplot(clusters_pearson_bestk,avg_common, frame=TRUE)

set.seed(30)
clusters_pearson_2007<-Kmeans(avg_common[,2:5],3, method = "pearson",nstart =5,iter.max = 400)
clusters_pearson_2007_3<-clusters_pearson_2007
clusters_pearson_2007$withinss #0.03418694 0.07540285 0.39220610
sum(clusters_pearson_2007$withinss)#0.5017959
plotcluster(avg_common, clusters_pearson_2007$cluster,xlab="Cluster Analysis")
autoplot(clusters_pearson_2007,avg_common, frame=TRUE)

set.seed(30)
clusters_pearson_2007<-Kmeans(avg_common[,2:5],4, method = "pearson",nstart =5,iter.max = 400)
clusters_pearson_2007
clusters_pearson_2007$withinss #0.326676784 0.009338037 0.128464722 0.024357000
sum(clusters_pearson_2007$withinss)#0.4888365
plotcluster(avg_common, clusters_pearson_2007$cluster,xlab="Cluster Analysis")
autoplot(clusters_pearson_2007,avg_common, frame=TRUE)

set.seed(30)
clusters_pearson_2007<-Kmeans(avg_common[,2:5],5, method = "pearson", nstart =5, iter.max = 400)
clusters_pearson_2007
clusters_pearson_2007$withinss #0.131722179 0.027132364 0.025088755 0.009338037 0.088705130
sum(clusters_pearson_2007$withinss)#0.2819865
plotcluster(avg_common, clusters_pearson_2007$cluster,xlab="Cluster Analysis")
autoplot(clusters_pearson_2007,avg_common, frame=TRUE)

set.seed(30)
clusters_pearson_2007_7<-Kmeans(avg_common[,2:5],7, method = "pearson", nstart =5, iter.max = 400)
clusters_pearson_2007_7
clusters_pearson_2007_7$withinss #0.02462741 0.07734485 0.00262668 0.02063504 0.01601883 0.02248410 0.02713236
sum(clusters_pearson_2007_7$withinss)#0.1908693
plotcluster(avg_common, clusters_pearson_2007_7$cluster,xlab="Cluster Analysis")
autoplot(clusters_pearson_2007_7,avg_common, frame=TRUE)
#---------------------------------------------------------------------------------------------------

#Elbow method to verify the best k which we got through manual method

#Euclidean method: best k =2
wssplot <- function(data, nc=8, seed=30)
{
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(Kmeans(data,i, method = "euclidean",nstart =10,iter.max = 400)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab= "within groups sum of squares")
}
wssplot(avg_common[,2:5])

#Pearson method: best k =2
wssplot <- function(data, nc=8, seed=30)
{
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(Kmeans(data,centers=i, method = "pearson",nstart =5,iter.max = 400)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab= "within groups sum of squares")
}
wssplot(avg_common[,2:5])


#---------------------------------------------------------------------------------------------------------
#Using best k and random seed: 120
#from the above graphs, best k for euclidean = 2

#for euclidean bestk = 2
set.seed(120)
clusters_euclidean_2007<-Kmeans(avg_common[,2:5],2, method = "euclidean", nstart=10, iter.max = 400)
clusters_euclidean_2007_S20_2<-clusters_euclidean_2007
clusters_euclidean_2007_S20_2$withinss #5.791232 6.016258
sum(clusters_euclidean_2007_S20_2$withinss)#11.80749
plotcluster(avg_common, clusters_euclidean_2007_S20_2$cluster,xlab="Cluster Analysis")
autoplot(clusters_euclidean_2007,avg_common, frame=TRUE)

#from the above graphs, best k for pearson = 2
set.seed(120)
clusters_pearson_2007<-Kmeans(avg_common[,2:5],2, method = "pearson", nstart=10, iter.max = 400)
clusters_pearson_2007_S20<-clusters_pearson_2007
clusters_pearson_2007_S20
clusters_pearson_2007_S20$withinss # 0.2557943 0.3404261
sum(clusters_pearson_2007_S20$withinss)#0.5962204
plotcluster(avg_common, clusters_pearson_2007$cluster,xlab="Cluster Analysis")
autoplot(clusters_pearson_2007,avg_common, frame=TRUE)

##---------------------------------------------------------------------------------------------------------
#computing Jaccard similarity for k=7 clusters for euclidean vs pearson
jaccard_val_k7 = cluster_similarity(clusters_euclidean_2007_7$cluster,clusters_pearson_2007_7$cluster, similarity="jaccard", method="independence")
jaccard_val_k7 #0.3426327

#computing Jaccard similarity for best k clusters with same seed for pearson vs euclidean k=2
jaccard_val_s120_k2 = cluster_similarity(cluster_euclidean_bestk$cluster,clusters_pearson_bestk$cluster, similarity="jaccard", method="independence")
jaccard_val_s120_k2 #0.4899896

#computing Jaccard similarity for best k clusters with random seed for pearson vs euclidean k=2
jaccard_val_s120_k2 = cluster_similarity(clusters_euclidean_2007_S20_2$cluster,clusters_pearson_2007_S20$cluster, similarity="jaccard", method="independence")
jaccard_val_s120_k2 #0.4761782

##---------------------------------------------------------------------------------------------------------
#Visualization: some more plots
#all dimensions shown in one plot for euclidean
plot(avg_common[,2:5],col=cluster_euclidean_bestk$cluster)
#all dimensions shown in one plot for pearson
plot(avg_common[,2:5],col=clusters_pearson_bestk$cluster)

#map----------------------------------------------------------------------------------------------
library(ggmap)
ggmap::register_google(key = "AIzaSyCExUFws_-FpxeqH518H9K9xaMDyscCnGI")
ggmap_show_api_key()
has_google_key()
ss <- read.csv('stations.csv')
ss

#euclidean best k seed 30
avg_common$cluster <- as.factor(cluster_euclidean_bestk$cluster)
avg_common
mapeculidian <- merge.data.frame(avg_common,ss,by.x = "station", by.y = "StationNumber")
mapeculidian
qmplot(Lon, Lat, data = mapeculidian, maptype = "toner-hybrid", color = as.factor(cluster))

#euclidean best k seed 120
avg_common$cluster <- as.factor(clusters_euclidean_2007_S20_2$cluster)
avg_common
mapeculidian <- merge.data.frame(avg_common,ss,by.x = "station", by.y = "StationNumber")
mapeculidian
qmplot(Lon, Lat, data = mapeculidian, maptype = "toner-hybrid", color = as.factor(cluster))

#pearson best k seed 30
avg_common$cluster <- as.factor(clusters_pearson_bestk$cluster)
avg_common
mapeculidian <- merge.data.frame(avg_common,ss,by.x = "station", by.y = "StationNumber")
mapeculidian
qmplot(Lon, Lat, data = mapeculidian, maptype = "toner-hybrid", color = as.factor(cluster))

#pearson best k seed 120
avg_common$cluster <- as.factor(clusters_pearson_2007_S20$cluster)
avg_common
mapeculidian <- merge.data.frame(avg_common,ss,by.x = "station", by.y = "StationNumber")
mapeculidian
qmplot(Lon, Lat, data = mapeculidian, maptype = "toner-hybrid", color = as.factor(cluster))


