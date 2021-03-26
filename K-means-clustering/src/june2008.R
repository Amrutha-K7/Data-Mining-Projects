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
dframe <- read.csv("hourly_2008.csv", header = TRUE, sep = "") #read the data
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
# 1  690190 20080201    0  40.94     12.3           955.2      11.8
# 2  690190 20080201    1  36.88     12.3           955.2      11.8
# 3  690190 20080201    2  52.79     12.3           955.2      11.8
# 4  690190 20080201    3  20.46     12.3           955.2      11.8

df<-transform(df, Year = substr(d, 1, 4), Month = substr(d, 5, 6),day = substr(d,7,8)) #separates date into year month and day
df
#     station    d         hours  temp    dewpoint  stationpressure   windspeed   Year   Month day
# 1    690190 20080201     0     40.94     12.3           955.2      11.8 2008      02   01
# 2    690190 20080201     1     36.88     12.3           955.2      11.8 2008      02   01

df <- subset(df, Month == "06") #select all the records with month = 06
head(df)
nrow(df)          #184872 rows slected for the month june(06) from 161 stations for the year 2008 data
table(df$station) #shows number of rows for june month for different stations

table(df$temp==999.9)                 #96 rows have 9999.9 value
table(df$dewpoint==9999.9)             #12240 rows have 9999.9 value
table(df$stationpressure==9999.9)      #87528 rows have 9999.9 value
table(df$windspeed==999.9)             #7200 rows have 999.9 value

#Replacing 9's in particular field with 0 value
df <- df %>% mutate(temp = replace(temp, temp == 999.9, 0))
df <- df %>% mutate(dewpoint = replace(dewpoint, dewpoint == 9999.9, 0));
df <- df %>% mutate(stationpressure = replace(stationpressure, stationpressure == 9999.9, 0));
df <- df %>% mutate(windspeed = replace(windspeed, windspeed == 999.9, 0));

table(df$temp==0)   #0
table(df$dewpoint==0) #12240 
table(df$stationpressure==0)#87528
table(df$windspeed==0)#7200 

dfgroup <- group_by(df,station) #grouping existing table data into station based grouped tables
dfgroup

#Taking monthly average of Temp,DewP,STP,WDSP for june month for all the station present in our data, ignoring the NA values
avg<-summarise(dfgroup,temp_avg=mean(temp,na.rm=TRUE),dp_avg=mean(dewpoint,na.rm=TRUE),
               sp_avg=mean(stationpressure,na.rm=TRUE),ws_avg=mean(windspeed,na.rm=TRUE))
avg
#   station temp_avg  dp_avg sp_avg ws_avg
# 1  690190     84.3   63.5   475.  12.5 
# 2  720110     84.3   70.2   487.   8.02
# 3  720151     84.3   50.2   434.   7.94
# 4  720261     84.3   64.8   491.   8.33

stations <-read.csv('stations.csv')        #Reading the stations.csv
stations
stations <- subset(stations,select = c(1))   #selecting first column which has station numbers
stations

avg_common<-subset(avg, station %in% stations$StationNumber)
avg_common     #selecting common stations between stations.csv and all stations in our june data, 2 stations were not part of stations.csv, remove those from our data
#  station  temp_avg dp_avg sp_avg ws_avg
#1  690190     84.3   63.5   475.   12.5 
#2  720110     84.3   70.2   487.   8.02
#3  720151     84.3   50.2   434.   7.94
#-------------------------------------------------------------------------------------------------------

#Kmeans clustering for year 2008, with same seed value=30 and k value from 2 to 8, and random centroid values
#Elbow method to find best k

#euclidean method for k = 2 to 8 and same seed value
set.seed(30)
clusters_euclidean_2008<-Kmeans(avg_common[,2:5],2, method = "euclidean", nstart=20, iter.max = 400)
clusters_euclidean_2008_2<-clusters_euclidean_2008
clusters_euclidean_2008
clusters_euclidean_2008$withinss #316.3532 36846.0241
sum(clusters_euclidean_2008$withinss) #37162.38
plotcluster(avg_common, clusters_euclidean_2008$cluster,xlab="Cluster Analysis")
autoplot(clusters_euclidean_2008,avg_common, frame=TRUE)

set.seed(30)
clusters_euclidean_2008<-Kmeans(avg_common[,2:5],3, method = "euclidean", nstart=20, iter.max = 400)
cluster_euclidean_bestk<-clusters_euclidean_2008
cluster_euclidean_bestk
clusters_euclidean_2008$withinss #316.35320    47.67817 38010.93745
sum(clusters_euclidean_2008$withinss) #38374.97
plotcluster(avg_common, clusters_euclidean_2008$cluster,xlab="Cluster Analysis")
autoplot(clusters_euclidean_2008,avg_common, frame=TRUE)

set.seed(30)
clusters_euclidean_2008<-Kmeans(avg_common[,2:5],4, method = "euclidean", nstart=24, iter.max = 400)
clusters_euclidean_2008_4<-clusters_euclidean_2008
clusters_euclidean_2008
clusters_euclidean_2008$withinss #237.77382    62.75552 36845.75032   184.32906
plotcluster(avg_common, clusters_euclidean_2008$cluster,xlab="Cluster Analysis")

set.seed(30)
clusters_euclidean_2008<-Kmeans(avg_common[,2:5],5, method = "euclidean", nstart=20, iter.max = 400)
clusters_euclidean_2008_5<-clusters_euclidean_2008
clusters_euclidean_2008
clusters_euclidean_2008$withinss #31055.77414   316.20435   173.04120   224.12493    88.45289
plotcluster(avg_common, clusters_euclidean_2008$cluster,xlab="Cluster Analysis")

set.seed(30)
clusters_euclidean_2008<-Kmeans(avg_common[,2:5],6, method = "euclidean", nstart=20, iter.max = 400)
clusters_euclidean_2008_6<-clusters_euclidean_2008
clusters_euclidean_2008
clusters_euclidean_2008$withinss #88.452890   316.204353   274.830663     2.937069    52.633354 31055.774139
plotcluster(avg_common, clusters_euclidean_2008$cluster,xlab="Cluster Analysis")

set.seed(30)
clusters_euclidean_2008<-Kmeans(avg_common[,2:5],7, method = "euclidean", nstart=20, iter.max = 400)
clusters_euclidean_2008_7<-clusters_euclidean_2008
clusters_euclidean_2008
clusters_euclidean_2008$withinss #184.329055    2.937069   52.633354  274.830663 1657.775963    0.000000  822.640464
plotcluster(avg_common, clusters_euclidean_2008$cluster,xlab="Cluster Analysis")

#pearson method for k = 2 to 8
set.seed(30)
clusters_pearson_2008<-Kmeans(avg_common[,2:5],2, method = "pearson", nstart=20, iter.max = 400)
clusters_pearson_2008_2<-clusters_pearson_2008
clusters_pearson_2008$withinss #2.280712e-11 4.175285e-04
sum(clusters_pearson_2008$withinss)#0.0004233448
plotcluster(avg_common, clusters_pearson_2008$cluster,xlab="Cluster Analysis")
autoplot(clusters_pearson_2008,avg_common, frame=TRUE)

set.seed(30)
clusters_pearson_2008<-Kmeans(avg_common[,2:5],3, method = "pearson", nstart=20, iter.max = 400)
clusters_pearson_bestk<-clusters_pearson_2008
clusters_pearson_2008$withinss #2.648233e-10 4.233448e-04 2.175211e-10
sum(clusters_pearson_2008$withinss)#0.0004233452
plotcluster(avg_common, clusters_pearson_2008$cluster,xlab="Cluster Analysis")
autoplot(clusters_pearson_2008,avg_common, frame=TRUE)

set.seed(30)
clusters_pearson_2008<-Kmeans(avg_common[,2:5],4, method = "pearson", nstart=20, iter.max = 400)
clusters_pearson_2008_4<-clusters_pearson_2008
clusters_pearson_2008$withinss #4.175285e-04 1.022004e-11 4.331808e-11 1.879164e-11
plotcluster(avg_common, clusters_pearson_2008$cluster,xlab="Cluster Analysis")

set.seed(30)
clusters_pearson_2008<-Kmeans(avg_common[,2:5],6, method = "pearson", nstart=20, iter.max = 400)
clusters_pearson_2008_6<-clusters_pearson_2008
clusters_pearson_2008$withinss # 4.175285e-04 4.406453e-11 4.892615e-13 3.378985e-11 2.424755e-11 1.383939e-17
plotcluster(avg_common, clusters_pearson_2008$cluster,xlab="Cluster Analysis")

set.seed(30)
clusters_pearson_2008<-Kmeans(avg_common[,2:5],7, method = "pearson", nstart=20, iter.max = 400)
clusters_pearson_2008_7<-clusters_pearson_2008
clusters_pearson_2008$withinss #3.071743e-11 0.000000e+00 5.686428e-07 5.248202e-06 0.000000e+00 2.993361e-10 3.146366e-11
plotcluster(avg_common, clusters_pearson_2008$cluster,xlab="Cluster Analysis")

#---------------------------------------------------------------------------------------------------------
#Using best k and random seed:
#from the above graphs, best k for euclidean = 3
set.seed(120)
clusters_euclidean_2008<-Kmeans(avg_common[,2:5],3, method = "euclidean", nstart=20, iter.max = 400)
clusters_euclidean_2008_S20<-clusters_euclidean_2008
clusters_euclidean_2008_S20$withinss #316.35320 38010.93745    47.67817
sum(clusters_euclidean_2008_S20$withinss)#38374.97
plotcluster(avg_common, clusters_euclidean_2008$cluster,xlab="Cluster Analysis")
autoplot(clusters_euclidean_2008,avg_common, frame=TRUE)

# for pearson = 3
set.seed(120)
clusters_pearson_2008<-Kmeans(avg_common[,2:5],3, method = "pearson", nstart=20, iter.max = 400)
clusters_pearson_2008_S20_3<-clusters_pearson_2008
clusters_pearson_2008_S20_3$withinss # 4.233448e-04 2.648233e-10 2.175211e-10
sum(clusters_pearson_2008_S20_3$withinss)#0.0004233452
plotcluster(avg_common, clusters_pearson_2008$cluster,xlab="Cluster Analysis")
autoplot(clusters_pearson_2008,avg_common, frame=TRUE)

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

jaccard_val_k7 = cluster_similarity(clusters_euclidean_2008_7$cluster,clusters_pearson_2008_7$cluster, similarity="jaccard", method="independence")
jaccard_val_k7 #0.6009539

#computing Jaccard similarity for best k clusters for euclidean vs respective pearson k value = 3
jaccard_val_k3 = cluster_similarity(cluster_euclidean_bestk$cluster,clusters_pearson_bestk$cluster, similarity="jaccard", method="independence")
jaccard_val_k3 #0.6515727


#computing Jaccard similarity for best k clusters with random seed for euclidean vs pearson k=3
jaccard_val_s120_k3 = cluster_similarity(clusters_euclidean_2008_S20$cluster,clusters_pearson_2008_S20_3$cluster, similarity="jaccard", method="independence")
jaccard_val_s120_k3 #0.6515727

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
avg_common$cluster <- as.factor(clusters_euclidean_2008_S20$cluster)
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
avg_common$cluster <- as.factor(clusters_pearson_2008_S20_3$cluster)
avg_common
mapeculidian <- merge.data.frame(avg_common,ss,by.x = "station", by.y = "StationNumber")
mapeculidian
qmplot(Lon, Lat, data = mapeculidian, maptype = "toner-hybrid", color = as.factor(cluster))
