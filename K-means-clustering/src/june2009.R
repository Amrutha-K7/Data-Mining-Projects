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


#Preprocessing
dframe <- read.csv("hourly_2009.csv", header = TRUE, sep = "") #read the data
dim(dframe)                                                #prints the number of rows and the numbers of attributes
dframe<- subset(dframe, select=c(1,3,4,5,9,13))            #selecting attributes specified: STN,yearModa_hr,Temp,DewP,STP,WDSP
dim(dframe)
head(dframe)                                               #prints first few rows, we can see that the column names are messed up for the selected attributes

colnames(dframe)<-c("station","date","temp","dewpoint","stationpressure","windspeed") #assigning the proper column names for the selected attributes
head(dframe)

df<-dframe
df<-separate(df,date,c("d","hours"))  #separates date and hours
head(df)
# station        d   hours  temp dewpoint stationpressure windspeed
# 1  690190 20090201     0 48.96     25.6           948.9      14.9
# 2  690190 20090201     1 45.09     25.6           948.9      14.9
# 3  690190 20090201     2 48.75     25.6           948.9      14.9
# 4  690190 20090201     3 55.94     25.6           948.9      14.9

df<-transform(df, Year = substr(d, 1, 4), Month = substr(d, 5, 6),day = substr(d,7,8)) #separates date into year month and day
df
#     station    d         hours  temp    dewpoint  stationpressure   windspeed   Year   Month day
# 1    690190 20090201     0      48.96     25.6           948.9      14.9         2009    02  01
# 2    690190 20090201     1      45.09     25.6           948.9      14.9         2009    02  01

df <- subset(df, Month == "06") #select all the records with month = 06
head(df)
nrow(df)          #132576 rows slected for the month june(06) from 158 stations for the year 2008 data
table(df$station) #shows number of rows for june month for different stations

table(df$temp==999.9)                 #0 rows have 9999.9 value
table(df$dewpoint==9999.9)             #12240 rows have 9999.9 value
table(df$stationpressure==9999.9)      #34680 rows have 9999.9 value
table(df$windspeed==999.9)             #7296 rows have 999.9 value

#Replacing 9's in particular field with 0 value
df <- df %>% mutate(temp = replace(temp, temp == 999.9, 0))
df <- df %>% mutate(dewpoint = replace(dewpoint, dewpoint == 9999.9, 0));
df <- df %>% mutate(stationpressure = replace(stationpressure, stationpressure == 9999.9, 0));
df <- df %>% mutate(windspeed = replace(windspeed, windspeed == 999.9, 0));

table(df$temp==0)   #0
table(df$dewpoint==0) #12240 
table(df$stationpressure==0)#34680
table(df$windspeed==0)#7344 

dfgroup <- group_by(df,station) #grouping existing table data into station based grouped tables
dfgroup

#Taking monthly average of Temp,DewP,STP,WDSP for june month for all the station present in our data, ignoring the NA values
avg<-summarise(dfgroup,temp_avg=mean(temp,na.rm=TRUE),dp_avg=mean(dewpoint,na.rm=TRUE),
               sp_avg=mean(stationpressure,na.rm=TRUE),ws_avg=mean(windspeed,na.rm=TRUE))
avg
#   station temp_avg  dp_avg  sp_avg  ws_avg
#1  690190     83.2   62.9    948.    9.95
#2  720110     83.2   65.2    487.    5.94
#3  720151     83.2   53.8    868.    6.32

stations <-read.csv('stations.csv')        #Reading the stations.csv
stations
stations <- subset(stations,select = c(1))   #selecting first column which has station numbers
stations

avg_common<-subset(avg, station %in% stations$StationNumber)
avg_common     #selecting common stations between stations.csv and all stations in our june data, 2 stations were not part of stations.csv, remove those from our data

#-------------------------------------------------------------------------------------------------------

#Kmeans clustering for year 2009, with same seed value=60 and k value from 2 to 8, and random centroid values
#Elbow method to find best k

#euclidean method for k = 2 to 8 and same seed value
set.seed(30)
clusters_euclidean_2009<-Kmeans(avg_common[,2:5],2, method = "euclidean", iter.max = 400)
cluster_euclidean_bestk<-clusters_euclidean_2009
cluster_euclidean_bestk
clusters_euclidean_2009$withinss #521.6266 2629.4664
sum(clusters_euclidean_2009$withinss)#3151.093
plotcluster(avg_common, clusters_euclidean_2009$cluster,xlab="Cluster Analysis")
autoplot(clusters_euclidean_2009,avg_common, frame=TRUE)

set.seed(30)
clusters_euclidean_2009<-Kmeans(avg_common[,2:5],3, method = "euclidean", iter.max = 400)
clusters_euclidean_2009_3<-clusters_euclidean_2009
clusters_euclidean_2009
clusters_euclidean_2009$withinss #15.10438 10072.00444    96.49584
sum(clusters_euclidean_2009$withinss)
plotcluster(avg_common, clusters_euclidean_2009$cluster,xlab="Cluster Analysis")
autoplot(clusters_euclidean_2009,avg_common, frame=TRUE)

set.seed(30)
clusters_euclidean_2009<-Kmeans(avg_common[,2:5],4, method = "euclidean",iter.max = 400)
clusters_euclidean_2009
clusters_euclidean_2009$withinss #10072.00444    96.49584    44.04431   520.69927
plotcluster(avg_common, clusters_euclidean_2009$cluster,xlab="Cluster Analysis")

set.seed(30)
clusters_euclidean_2009<-Kmeans(avg_common[,2:5],5, method = "euclidean", iter.max = 400)
clusters_euclidean_2009
clusters_euclidean_2009$withinss #3138.2058  263.5682  135.3028 4700.6926  424.2264
plotcluster(avg_common, clusters_euclidean_2009$cluster,xlab="Cluster Analysis")

set.seed(30)
clusters_euclidean_2009_7<-Kmeans(avg_common[,2:5],7, method = "euclidean", iter.max = 400)
clusters_euclidean_2009_7
clusters_euclidean_2009$withinss#973.10017  1358.57637    44.04431    96.49584 10072.00444
plotcluster(avg_common, clusters_euclidean_2009_7$cluster,xlab="Cluster Analysis")

#pearson method for k = 2 to 8
set.seed(30)
clusters_pearson_2009<-Kmeans(avg_common[,2:5],2, method = "pearson", iter.max = 400)
clusters_pearson_bestk<-clusters_pearson_2009
clusters_pearson_2009$withinss #0.02060478 0.02071823
sum(clusters_pearson_2009$withinss)#0.04132302
plotcluster(avg_common, clusters_pearson_2009$cluster,xlab="Cluster Analysis")
autoplot(clusters_pearson_2009,avg_common, frame=TRUE)

set.seed(30)
clusters_pearson_2009<-Kmeans(avg_common[,2:5],3, method = "pearson", iter.max = 400)
clusters_pearson_2009_3<-clusters_pearson_2009
clusters_pearson_2009$withinss #0.000000e+00 2.065901e-02 3.845622e-11
plotcluster(avg_common, clusters_pearson_2009$cluster,xlab="Cluster Analysis")
autoplot(clusters_pearson_2009,avg_common, frame=TRUE)

set.seed(30)
clusters_pearson_2009<-Kmeans(avg_common[,2:5],4, method = "pearson",iter.max = 400)
clusters_pearson_2009
clusters_pearson_2009$withinss #5.742776e-07 7.618442e-06 1.044544e-02 1.290955e-11
plotcluster(avg_common, clusters_pearson_2009$cluster,xlab="Cluster Analysis")

set.seed(30)
clusters_pearson_2009<-Kmeans(avg_common[,2:5],5, method = "pearson", iter.max = 400)
clusters_pearson_2009
clusters_pearson_2009$withinss #1.887182e-13 5.742776e-07 7.618442e-06 1.044544e-02 8.632108e-11
plotcluster(avg_common, clusters_pearson_2009$cluster,xlab="Cluster Analysis")

set.seed(30)
clusters_pearson_2009_7<-Kmeans(avg_common[,2:5],7, method = "pearson", iter.max = 400)
clusters_pearson_2009_7
clusters_pearson_2009_7$withinss #8.467002e-12 3.010317e-13 5.742776e-07 7.618442e-06 1.044544e-02 5.053158e-13 5.169518e-12
plotcluster(avg_common, clusters_pearson_2009_7$cluster,xlab="Cluster Analysis")

#---------------------------------------------------------------------------------------------------------
#Using best k and random seed:
#from the above graphs, best k for euclidean = 2

#for euclidean = 2=bestk
set.seed(120)
clusters_euclidean_2009<-Kmeans(avg_common[,2:5],2, method = "euclidean",iter.max = 400)
clusters_euclidean_2009_S20_2<-clusters_euclidean_2009
clusters_euclidean_2009$withinss #2629.4664  521.6266
sum(clusters_euclidean_2009$withinss)#3151.093
plotcluster(avg_common, clusters_euclidean_2009_S20_2$cluster,xlab="Cluster Analysis")
autoplot(clusters_euclidean_2009,avg_common, frame=TRUE)

#from the above graphs, best k for pearson = 2
set.seed(120)
clusters_pearson_2009<-Kmeans(avg_common[,2:5],2, method = "pearson", iter.max = 400)
clusters_pearson_2009_S20<-clusters_pearson_2009
clusters_pearson_2009_S20$withinss #0.02071823 0.02060478
sum(clusters_pearson_2009_S20$withinss)#0.04132302
plotcluster(avg_common, clusters_pearson_2009$cluster,xlab="Cluster Analysis")
autoplot(clusters_pearson_2009,avg_common, frame=TRUE)


##---------------------------------------------------------------------------------------------------------
#Elbow method to verify the best k which we got through manual method

#Euclidean method: best k =2
wssplot <- function(data, nc=8, seed=30)
{
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(Kmeans(data,i, method = "euclidean",iter.max = 400)$withinss)}
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
    wss[i] <- sum(Kmeans(data,centers=i, method = "pearson",iter.max = 400)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab= "within groups sum of squares")
}
wssplot(avg_common[,2:5])

##---------------------------------------------------------------------------------------------------------
#computing Jaccard similarity for k=7 clusters for euclidean vs pearson
jaccard_val_k7 = cluster_similarity(clusters_euclidean_2009_7$cluster,clusters_pearson_2009_7$cluster, similarity="jaccard", method="independence")
jaccard_val_k7 #0.3728352

#computing Jaccard similarity for best k clusters for pearson vs respective euclidean k value = 2
jaccard_val_k2 = cluster_similarity(clusters_pearson_bestk$cluster,cluster_euclidean_bestk$cluster, similarity="jaccard", method="independence")
jaccard_val_k2 #0.8717847

#computing Jaccard similarity for best k clusters with random seed for pearson vs euclidean k=2
jaccard_val_s120_k2 = cluster_similarity(clusters_pearson_2009_S20$cluster,clusters_euclidean_2009_S20_2$cluster, similarity="jaccard", method="independence")
jaccard_val_s120_k2 #0.8717847

##---------------------------------------------------------------------------------------------------------
#Visualization: some more plots
#all dimensions shown in one plot for euclidean
plot(avg_common[,2:5],col=cluster_euclidean_bestk$cluster)
#all dimensions shown in one plot for pearson
plot(avg_common[,2:5],col=clusters_pearson_bestk$cluster)

#map--------------------------------------------------------------------------------------------------
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
avg_common$cluster <- as.factor(clusters_euclidean_2009_S20_2$cluster)
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
avg_common$cluster <- as.factor(clusters_pearson_2009_S20$cluster)
avg_common
mapeculidian <- merge.data.frame(avg_common,ss,by.x = "station", by.y = "StationNumber")
mapeculidian
qmplot(Lon, Lat, data = mapeculidian, maptype = "toner-hybrid", color = as.factor(cluster))
  

