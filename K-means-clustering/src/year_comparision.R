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


#2007 Preprocessing
dframe_2007 <- read.csv("hourly_2007.csv", header = TRUE, sep = "") #read the data
dim(dframe_2007)                                                #prints the number of rows and the numbers of attributes
dframe_2007<- subset(dframe_2007, select=c(1,3,4,5,9,13))            #selecting attributes specified: STN,yearModa_hr,Temp,DewP,STP,WDSP
dim(dframe_2007)
head(dframe_2007)                                               #prints first few rows, we can see that the column names are messed up for the selected attributes

colnames(dframe_2007)<-c("station","date","temp","dewpoint","stationpressure","windspeed") #assigning the proper column names for the selected attributes
head(dframe_2007)

df_2007<-dframe_2007
df_2007<-separate(df_2007,date,c("d","hours"))  #separates date and hours
head(df_2007)
#     station        d    hours  temp     dewpoint  stationpressure  windspeed
#1    690190   20070201     0    48.64     36.3           944.5       6.0
#2    690190   20070201     1    41.92     36.3           944.5       6.0

df_2007<-transform(df_2007, Year = substr(d, 1, 4), Month = substr(d, 5, 6),day = substr(d,7,8)) #separates date into year month and day
df_2007
#     station    d         hours  temp    dewpoint  stationpressure   windspeed   Year   Month day
#1    690190     20070201     0   48.64     36.3           944.5       6.0        2007    02   01
#2    690190     20070201     1   41.92     36.3           944.5       6.0        2007    02   01

df_2007 <- subset(df_2007, Month == "06") #select all the records with month = 06
head(df_2007)
nrow(df_2007)          #176544 rows slected for the month june(06) from 158 stations for the year 2007 data
table(df_2007$station) #shows number of rows for june month for different stations

table(df_2007$temp==999.9)                 #0 rows have 9999.9 value
table(df_2007$dewpoint==9999.9)             #9456 rows have 9999.9 value
table(df_2007$stationpressure==9999.9)      #86328 rows have 9999.9 value
table(df_2007$windspeed==999.9)             #7224 rows have 999.9 value

#Replacing 9's in particular field with 0 value
df_2007 <- df_2007 %>% mutate(temp = replace(temp, temp == 999.9, 0))
df_2007 <- df_2007 %>% mutate(dewpoint = replace(dewpoint, dewpoint == 9999.9, 0));
df_2007 <- df_2007 %>% mutate(stationpressure = replace(stationpressure, stationpressure == 9999.9, 0));
df_2007 <- df_2007 %>% mutate(windspeed = replace(windspeed, windspeed == 999.9, 0));

table(df_2007$temp==0)   #0
table(df_2007$dewpoint==0) #9456
table(df_2007$stationpressure==0)#86328
table(df_2007$windspeed==0)#7224

dfgroup_2007 <- group_by(df_2007,station) #grouping existing table data into station based grouped tables
dfgroup_2007

#Taking monthly average of Temp,DewP,STP,WDSP for june month for all the station present in our data, ignoring the NA values
avg_2007<-summarise(dfgroup_2007,temp_avg=mean(temp,na.rm=TRUE),dp_avg=mean(dewpoint,na.rm=TRUE),
               sp_avg=mean(stationpressure,na.rm=TRUE),ws_avg=mean(windspeed,na.rm=TRUE))
avg_2007
#   station temp_avg  dp_avg sp_avg ws_avg
#1  690190     79.9   66.1   475.   6.94
#2  720110     79.9   72.6   469.   3.94
#3  720151     79.9   51.2   434.   6.14


stations <-read.csv('stations.csv')        #Reading the stations.csv
stations
stations <- subset(stations,select = c(1))   #selecting first column which has station numbers
stations

avg_common_2007<-subset(avg_2007, station %in% stations$StationNumber)
avg_common_2007    #selecting common stations between stations.csv and all stations in our june data, 2 stations were not part of stations.csv, remove those from our data


##########################################################################################################################
#2008 Preprocessing
dframe_2008 <- read.csv("hourly_2008.csv", header = TRUE, sep = "") #read the data
dim(dframe_2008)                                                #prints the number of rows and the numbers of attributes
dframe_2008<- subset(dframe_2008, select=c(1,3,4,5,9,13))            #selecting attributes specified: STN,yearModa_hr,Temp,DewP,STP,WDSP
dim(dframe_2008)
head(dframe_2008)                                               #prints first few rows, we can see that the column names are messed up for the selected attributes

colnames(dframe_2008)<-c("station","date","temp","dewpoint","stationpressure","windspeed") #assigning the proper column names for the selected attributes
head(dframe_2008)

df_2008<-dframe_2008
df_2008<-separate(df_2008,date,c("d","hours"))  #separates date and hours
head(df_2008)
#     station        d      hours  temp     dewpoint  stationpressure  windspeed
# 1  690190        20080201     0 40.94     12.3           955.2      11.8
# 2  690190        20080201     1 36.88     12.3           955.2      11.8
# 3  690190        20080201     2 52.79     12.3           955.2      11.8

df_2008<-transform(df_2008, Year = substr(d, 1, 4), Month = substr(d, 5, 6),day = substr(d,7,8)) #separates date into year month and day
df_2008
#     station    d         hours  temp    dewpoint  stationpressure   windspeed   Year   Month day
# 1    690190 20080201     0      40.94     12.3           955.2      11.8 2008    02     01
# 2    690190 20080201     1      36.88     12.3           955.2      11.8 2008    02     01
# 3    690190 20080201     2      52.79     12.3           955.2      11.8 2008    02     01
# 4    690190 20080201     3      20.46     12.3           955.2      11.8 2008    02     01

df_2008 <- subset(df_2008, Month == "06") #select all the records with month = 06
head(df_2008)
nrow(df_2008)          #176544 rows slected for the month june(06) from 158 stations for the year 2007 data
table(df_2008$station) #shows number of rows for june month for different stations

table(df_2008$temp==999.9)                 #0 rows have 9999.9 value
table(df_2008$dewpoint==9999.9)             #9456 rows have 9999.9 value
table(df_2008$stationpressure==9999.9)      #86328 rows have 9999.9 value
table(df_2008$windspeed==999.9)             #7224 rows have 999.9 value

#Replacing 9's in particular field with 0 value
df_2008 <- df_2008 %>% mutate(temp = replace(temp, temp == 999.9, 0))
df_2008 <- df_2008 %>% mutate(dewpoint = replace(dewpoint, dewpoint == 9999.9, 0));
df_2008 <- df_2008 %>% mutate(stationpressure = replace(stationpressure, stationpressure == 9999.9, 0));
df_2008 <- df_2008 %>% mutate(windspeed = replace(windspeed, windspeed == 999.9, 0));

table(df_2008$temp==0)   #0
table(df_2008$dewpoint==0) #9456
table(df_2008$stationpressure==0)#86328
table(df_2008$windspeed==0)#7224

dfgroup_2008 <- group_by(df_2008,station) #grouping existing table data into station based grouped tables
dfgroup_2008

#Taking monthly average of Temp,DewP,STP,WDSP for june month for all the station present in our data, ignoring the NA values
avg_2008<-summarise(dfgroup_2008,temp_avg=mean(temp,na.rm=TRUE),dp_avg=mean(dewpoint,na.rm=TRUE),
                    sp_avg=mean(stationpressure,na.rm=TRUE),ws_avg=mean(windspeed,na.rm=TRUE))
avg_2008
#   station temp_avg  dp_avg sp_avg ws_avg
# 1  690190     84.3   63.5   475.  12.5 
# 2  720110     84.3   70.2   487.   8.02
# 3  720151     84.3   50.2   434.   7.94

avg_common_2008<-subset(avg_2008, station %in% stations$StationNumber)
avg_common_2008

#############################################################################################################
#2009 Preprocessing

dframe_2009 <- read.csv("hourly_2009.csv", header = TRUE, sep = "") #read the data
dim(dframe_2009)                                                #prints the number of rows and the numbers of attributes
dframe_2009<- subset(dframe_2009, select=c(1,3,4,5,9,13))            #selecting attributes specified: STN,yearModa_hr,Temp,DewP,STP,WDSP
dim(dframe_2009)
head(dframe_2009)                                               #prints first few rows, we can see that the column names are messed up for the selected attributes

colnames(dframe_2009)<-c("station","date","temp","dewpoint","stationpressure","windspeed") #assigning the proper column names for the selected attributes
head(dframe_2009)

df_2009<-dframe_2009
df_2009<-separate(df_2009,date,c("d","hours"))  #separates date and hours
head(df_2009)
#     station        d      hours  temp     dewpoint  stationpressure  windspeed
# 1  690190     20090201     0 48.96     25.6           948.9      14.9
# 2  690190     20090201     1 45.09     25.6           948.9      14.9
# 3  690190     20090201     2 48.75     25.6           948.9      14.9
# 4  690190     20090201     3 55.94     25.6           948.9      14.9

df_2009<-transform(df_2009, Year = substr(d, 1, 4), Month = substr(d, 5, 6),day = substr(d,7,8)) #separates date into year month and day
df_2009
#     station    d         hours  temp    dewpoint  stationpressure   windspeed   Year   Month day
# 1    690190 20090201     0      48.96     25.6           948.9      14.9 2009    02  01
# 2    690190 20090201     1      45.09     25.6           948.9      14.9 2009    02  01
# 3    690190 20090201     2      48.75     25.6           948.9      14.9 2009    02  01
# 4    690190 20090201     3      55.94     25.6           948.9      14.9 2009    02  01

df_2009 <- subset(df_2009, Month == "06") #select all the records with month = 06
head(df_2009)
nrow(df_2009)          #176544 rows slected for the month june(06) from 158 stations for the year 2007 data
table(df_2009$station) #shows number of rows for june month for different stations

table(df_2009$temp==999.9)                 #0 rows have 9999.9 value
table(df_2009$dewpoint==9999.9)             #12240 rows have 9999.9 value
table(df_2009$stationpressure==9999.9)      #34680  rows have 9999.9 value
table(df_2009$windspeed==999.9)             #7296  rows have 999.9 value

#Replacing 9's in particular field with 0 value
df_2009 <- df_2009 %>% mutate(temp = replace(temp, temp == 999.9, 0))
df_2009 <- df_2009 %>% mutate(dewpoint = replace(dewpoint, dewpoint == 9999.9, 0));
df_2009 <- df_2009 %>% mutate(stationpressure = replace(stationpressure, stationpressure == 9999.9, 0));
df_2009 <- df_2009 %>% mutate(windspeed = replace(windspeed, windspeed == 999.9, 0));

table(df_2009$temp==0)   #0
table(df_2009$dewpoint==0) #12240
table(df_2009$stationpressure==0)#34680 
table(df_2009$windspeed==0)#7344 

dfgroup_2009 <- group_by(df_2009,station) #grouping existing table data into station based grouped tables
dfgroup_2009

#Taking monthly average of Temp,DewP,STP,WDSP for june month for all the station present in our data, ignoring the NA values
avg_2009<-summarise(dfgroup_2009,temp_avg=mean(temp,na.rm=TRUE),dp_avg=mean(dewpoint,na.rm=TRUE),
                    sp_avg=mean(stationpressure,na.rm=TRUE),ws_avg=mean(windspeed,na.rm=TRUE))
avg_2009
#   station temp_avg  dp_avg sp_avg ws_avg
# 1  690190     83.2   62.9   948.   9.95
# 2  720110     83.2   65.2   487.   5.94
# 3  720151     83.2   53.8   868.   6.32
# 4  720261     83.2   65.4   981.   6.12

avg_common_2009<-subset(avg_2009, station %in% stations$StationNumber)
avg_common_2009
#---------------------------------------------------------------------------------------------------------
#keeping common data based on station for both 2007 vs 2008
avg_common_2007
avg_common_2008

avg_common_7wrt8<-subset(avg_common_2007, avg_common_2007$station %in% avg_common_2008$station)
avg_common_7wrt8  # data for stations in 2007 w.r.t 2008

avg_common_8wrt7<-subset(avg_common_2008, avg_common_2008$station %in% avg_common_2007$station)
avg_common_8wrt7 # data for stations in 2008 common w.r.t 2007

#kmeans cluster for year 2007 for best k (euclidean)
set.seed(30)
clusters_euclidean_2007_3<-Kmeans(avg_common_7wrt8[,2:5],3, method = "euclidean", nstart=20, iter.max = 400)
clusters_euclidean_2007_3
plotcluster(avg_common_7wrt8, clusters_euclidean_2007_3$cluster,xlab="Cluster Analysis")

set.seed(30)
clusters_euclidean_2007_2<-Kmeans(avg_common_7wrt8[,2:5],2, method = "euclidean", nstart=20, iter.max = 400)
clusters_euclidean_2007_2
plotcluster(avg_common_7wrt8, clusters_euclidean_2007_2$cluster,xlab="Cluster Analysis")

#kmeans cluster for year 2008 for best k (euclidean)
set.seed(30)
clusters_euclidean_2008_3<-Kmeans(avg_common_8wrt7[,2:5],3, method = "euclidean", nstart=20, iter.max = 400)
clusters_euclidean_2008_3
plotcluster(avg_common_8wrt7, clusters_euclidean_2008_3$cluster,xlab="Cluster Analysis")

set.seed(30)
clusters_euclidean_2008_2<-Kmeans(avg_common_8wrt7[,2:5],2, method = "euclidean", nstart=20, iter.max = 400)
clusters_euclidean_2008_2
plotcluster(avg_common_8wrt7, clusters_euclidean_2008_2$cluster,xlab="Cluster Analysis")



#-----------------------2007 euclidean vs 2008 euclidean-------------------------------------------
#jaccard for 2007 vs 2008 for euclidean metric with best k=2 for 2007
jaccard_val_78e = cluster_similarity(clusters_euclidean_2007_2$cluster,clusters_euclidean_2008_2$cluster, similarity="jaccard", method="independence")
jaccard_val_78e #0.9778934

#jaccard for 2007 vs 2008 for euclidean metric with best k=3 for 2008
jaccard_val_78e = cluster_similarity(clusters_euclidean_2008_3$cluster,clusters_euclidean_2007_3$cluster, similarity="jaccard", method="independence")
jaccard_val_78e #0.6907961

#--------------------------------------------------------------------------------------------------

#kmeans cluster for year 2007 for k=3 (pearson)
set.seed(30)
clusters_pearson_2007p<-Kmeans(avg_common_7wrt8[,2:5],3, method = "pearson", nstart=20, iter.max = 400)
clusters_pearson_2007p_3<-clusters_pearson_2007p
plotcluster(avg_common_7wrt8, clusters_pearson_2007p$cluster,xlab="Cluster Analysis")

#kmeans cluster for year 2007 for best k (pearson)
set.seed(30)
clusters_pearson_2007p<-Kmeans(avg_common_7wrt8[,2:5],2, method = "pearson", nstart=20, iter.max = 400)
clusters_pearson_2007p_2<-clusters_pearson_2007p
plotcluster(avg_common_7wrt8, clusters_pearson_2007p$cluster,xlab="Cluster Analysis")

#kmeans cluster for year 2008 for best k (pearson)
set.seed(30)
clusters_pearson_2008p<-Kmeans(avg_common_8wrt7[,2:5],3, method = "pearson", nstart=20, iter.max = 400)
clusters_pearson_2008p_3<-clusters_pearson_2008p
plotcluster(avg_common_8wrt7, clusters_pearson_2008p$cluster,xlab="Cluster Analysis")

#kmeans cluster for year 2008 for  k=2 (pearson)
set.seed(30)
clusters_pearson_2008p<-Kmeans(avg_common_8wrt7[,2:5],2, method = "pearson", nstart=20, iter.max = 400)
clusters_pearson_2008p_2<-clusters_pearson_2008p
plotcluster(avg_common_8wrt7, clusters_pearson_2008p$cluster,xlab="Cluster Analysis")

#-----------------------2007 pearson vs 2008 pearson-------------------------------------------


#jaccard for 2007 vs 2008 for pearson metric with best k for 2007
jaccard_val_78p_2 = cluster_similarity(clusters_pearson_2007p_2$cluster,clusters_pearson_2008p_2$cluster, similarity="jaccard", method="independence")
jaccard_val_78p_2 # 0.9563532

#jaccard for 2007 vs 2008 for pearson metric with best k for 2008
jaccard_val_78p_3 = cluster_similarity(clusters_pearson_2007p_3$cluster,clusters_pearson_2008p_3$cluster, similarity="jaccard", method="independence")
jaccard_val_78p_3 # 0.9166304

#---------------------------------------------------------------------------------------------------------
#keeping common data based on station for both 2008 vs 2009

avg_common_8wrt9<-subset(avg_common_2008, avg_common_2008$station %in% avg_common_2009$station)
avg_common_8wrt9  # data for stations in 2008 common w.r.t 2009

avg_common_9wrt8<-subset(avg_common_2009, avg_common_2009$station %in% avg_common_2008$station)
avg_common_9wrt8  # data for stations in 2008 common w.r.t 2009

#kmeans cluster for year 2008 for best k (euclidean)
set.seed(30)
clusters_euclidean_2008<-Kmeans(avg_common_8wrt9[,2:5],3, method = "euclidean", nstart=20, iter.max = 400)
clusters_euclidean_2008_3<-clusters_euclidean_2008
plotcluster(avg_common_8wrt9, clusters_euclidean_2008$cluster,xlab="Cluster Analysis")

#kmeans cluster for year 2008 for k=2 (euclidean)
set.seed(30)
clusters_euclidean_2008<-Kmeans(avg_common_8wrt9[,2:5],2, method = "euclidean", nstart=20, iter.max = 400)
clusters_euclidean_2008_2<-clusters_euclidean_2008
plotcluster(avg_common_8wrt9, clusters_euclidean_2008$cluster,xlab="Cluster Analysis")

#kmeans cluster for year 2009 for best k (euclidean)
set.seed(30)
clusters_euclidean_2009<-Kmeans(avg_common_9wrt8[,2:5],2, method = "euclidean", nstart=20, iter.max = 400)
clusters_euclidean_2009_2<-clusters_euclidean_2009
plotcluster(avg_common_9wrt8, clusters_euclidean_2009$cluster,xlab="Cluster Analysis")

#kmeans cluster for year 2009 for  k=3 (euclidean)
set.seed(30)
clusters_euclidean_2009<-Kmeans(avg_common_9wrt8[,2:5],3, method = "euclidean", nstart=20, iter.max = 400)
clusters_euclidean_2009_3<-clusters_euclidean_2009
plotcluster(avg_common_9wrt8, clusters_euclidean_2009$cluster,xlab="Cluster Analysis")

#--------------------------euclidean 2008 vs 2009-----------------------------------------------------------------
#jaccard for 2008 vs 2009 for euclidean metric with best k for 2008
jaccard_val_89e_3 = cluster_similarity(clusters_euclidean_2008_3$cluster,clusters_euclidean_2009_3$cluster, similarity="jaccard", method="independence")
jaccard_val_89e_3#0.3830949

#jaccard for 2008 vs 2009 for euclidean metric with best k for 2009
jaccard_val_89e_2 = cluster_similarity(clusters_euclidean_2008_2$cluster,clusters_euclidean_2009_2$cluster, similarity="jaccard", method="independence")
jaccard_val_89e_2#0.4376186
#-------------------------------------------------------------------------------------------

#kmeans cluster for year 2008 for best k (pearson)
set.seed(30)
clusters_pearson_2008p<-Kmeans(avg_common_8wrt9[,2:5],3, method = "pearson", nstart=20, iter.max = 400)
clusters_pearson_2008p_3<-clusters_pearson_2008p
plotcluster(avg_common_8wrt9, clusters_pearson_2008p$cluster,xlab="Cluster Analysis")

#kmeans cluster for year 2008 for  k=2 (pearson)
set.seed(30)
clusters_pearson_2008p<-Kmeans(avg_common_8wrt9[,2:5],2, method = "pearson", nstart=20, iter.max = 400)
clusters_pearson_2008p_2<-clusters_pearson_2008p
plotcluster(avg_common_8wrt9, clusters_pearson_2008p$cluster,xlab="Cluster Analysis")

#kmeans cluster for year 2009 for best k (pearson)
set.seed(30)
clusters_pearson_2009p<-Kmeans(avg_common_9wrt8[,2:5],2, method = "pearson", nstart=20, iter.max = 400)
clusters_pearson_2009p_2<-clusters_pearson_2009p
plotcluster(avg_common_9wrt8, clusters_pearson_2009p$cluster,xlab="Cluster Analysis")

#kmeans cluster for year 2009 for  k=3 (pearson)
set.seed(30)
clusters_pearson_2009p<-Kmeans(avg_common_9wrt8[,2:5],3, method = "pearson", nstart=20, iter.max = 400)
clusters_pearson_2009p_3<-clusters_pearson_2009p
plotcluster(avg_common_9wrt8, clusters_pearson_2009p$cluster,xlab="Cluster Analysis")

#--------------------------pearson 2008 vs 2009----------------------------------------------------
#jaccard for 2008 vs 2009 for pearson metric with best k for 2008
jaccard_val_89p_2 = cluster_similarity(clusters_pearson_2008p_2$cluster,clusters_pearson_2009p_2$cluster, similarity="jaccard", method="independence")
jaccard_val_89p_2 # 0.4639103

#jaccard for 2008 vs 2009 for pearson metric with best k for 2008
jaccard_val_89p_3 = cluster_similarity(clusters_pearson_2008p_3$cluster,clusters_pearson_2009p_3$cluster, similarity="jaccard", method="independence")
jaccard_val_89p_3 # 0.427918

#---------------------------------------------------------------------------------------------------------
#keeping common data based on station for both 2007 vs 2009
avg_common_7wrt9<-subset(avg_common_2007, avg_common_2007$station %in% avg_common_2009$station)
avg_common_7wrt9  # data for stations in 2008 common w.r.t 2009

avg_common_9wrt7<-subset(avg_common_2009, avg_common_2009$station %in% avg_common_2007$station)
avg_common_9wrt7  # data for stations in 2008 common w.r.t 2009


#kmeans cluster for year 2007 for k=2 (euclidean)
set.seed(30)
clusters_euclidean_2007<-Kmeans(avg_common_7wrt9[,2:5],2, method = "euclidean", nstart=20, iter.max = 400)
clusters_euclidean_2007_2<-clusters_euclidean_2007
plotcluster(avg_common_7wrt9, clusters_euclidean_2007$cluster,xlab="Cluster Analysis")

#kmeans cluster for year 2009 for best k (euclidean)
set.seed(30)
clusters_euclidean_2009<-Kmeans(avg_common_9wrt7[,2:5],2, method = "euclidean", nstart=20, iter.max = 400)
clusters_euclidean_2009_2<-clusters_euclidean_2009
plotcluster(avg_common_9wrt7, clusters_euclidean_2009$cluster,xlab="Cluster Analysis")

#--------------------------2007 vs 2009 euclidean----------------------------------
#jaccard for 2007 vs 2009 for euclidean metric with best k for 2009
jaccard_val_79e_2 = cluster_similarity(clusters_euclidean_2007_2$cluster,clusters_euclidean_2009_2$cluster, similarity="jaccard", method="independence")
jaccard_val_79e_2 #0.4505119

#-----------------------------------------------------------------------------------------
#kmeans cluster for year 2007 for  k=2 (pearson)
set.seed(30)
clusters_pearson_2007p<-Kmeans(avg_common_7wrt9[,2:5],2, method = "pearson", nstart=20, iter.max = 400)
clusters_pearson_2007p_2<-clusters_pearson_2007p
plotcluster(avg_common_7wrt9, clusters_pearson_2007p$cluster,xlab="Cluster Analysis")

#kmeans cluster for year 2009 for best k (pearson)
set.seed(30)
clusters_pearson_2009p<-Kmeans(avg_common_9wrt7[,2:5],2, method = "pearson", nstart=20, iter.max = 400)
clusters_pearson_2009p_2<-clusters_pearson_2009p
plotcluster(avg_common_9wrt7, clusters_pearson_2009p$cluster,xlab="Cluster Analysis")

#--------------------------2007 vs 2009 pearson----------------------------------
#jaccard for 2007 vs 2009 for pearson metric with best k for 2009
jaccard_val_79p_2 = cluster_similarity(clusters_pearson_2007p_2$cluster,clusters_pearson_2009p_2$cluster, similarity="jaccard", method="independence")
jaccard_val_79p_2 # 0.4820051

