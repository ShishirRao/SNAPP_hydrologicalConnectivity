library(sp)
library(rgdal)
library(gdalUtils)
library(raster)
library(ggplot2)
library(minpack.lm)
library(topmodel)
library(plyr)
library(lubridate)
library(caTools)
library(hydrostats)
library(IHA)
library(lfstat)
library(reshape2)
library(dplyr)
library(gridExtra)
library(geosphere)
library(reshape)
library(ggrepel)
library(factoextra)
library(cluster)


st.err <- function(x) {
  sd(x)/sqrt(length(x))
}

c.v <- function(x) {
  sd(x)/mean(x)
}


# Take the CWC master dataset, remove stations not necessary for analysis and extract their locations
West = read.csv("F:/wild/SNAPP/West flowing river - CWC/Analysis/West_16_3_18_V1_1.csv", header = TRUE)
West = read.csv("F:/wild/SNAPP/West flowing river - CWC/Analysis/West_19_6_18_V1_2_38Stations.csv", header = TRUE)
West = read.csv("F:/wild/SNAPP/West flowing river - CWC/Analysis/West_20_6_18_V1_2.csv", header = TRUE)
# Only stations which went through round 2 of data preprocessing.
West = (West[which(West$Round3 == "Yes"),])
nrow(West)
# only locations
head(West)
West = subset(West,select=c(Station.Name,latitude,longitude,State,Dammed.,Purpose,Catch.Area))
colnames(West) = c("Station","latitude","longitude","State","Dammed","Purpose","Area")
colnames(West)[1] = c("Station")

unique(West$Station)

Ref = c(West[West$Station=="Ashramam",]$longitude,West[West$Station=="Ashramam",]$latitude)
# Find the distance in kilometer from Ashramam station to every other location
for(i in 1:nrow(West)){
  Test = c(West[i,]$longitude,West[i,]$latitude)
  West$Distance[i] = distm(Ref,Test,distGeo) / 1000
}

###################### Rainfall ########################## 
setwd("F:/wild/SNAPP/West flowing river - CWC/Rainfall/Google EE results")
# Read the spatial meaans data set. Daily data is the mean of the all pixels in the basin
df = read.csv("CWC_1981_2016_meanPrecip.csv",header=T)
df = transform(df, date = colsplit(date, "T", names = c("date", "time")))
df = as.data.frame(cbind(df[,c(1,3)],df$date[[1]]))
colnames(df) = c("Station","MeanPrecip","Date")
df$Date = as.Date(df$Date)
df$Year = as.factor(as.numeric(strftime(df$Date, "%Y")))
df$MeanPrecip = as.numeric(as.character(df$MeanPrecip))

# Read the spatial sums data set. Daily data is the sum of the all pixels in the basin
# Larger basins will have larger values
df_sums = read.csv("CWC_1981_2016_sumPrecip.csv",header=T)
df_sums = transform(df_sums, date = colsplit(date, "T", names = c("date", "time")))
df_sums = as.data.frame(cbind(df_sums[,c(1,3)],df_sums$date[[1]]))
colnames(df_sums) = c("Station","MeanPrecip","Date")
df_sums$Date = as.Date(df_sums$Date)
df_sums$Year = as.factor(as.numeric(strftime(df_sums$Date, "%Y")))
df_sums$MeanPrecip = as.numeric(as.character(df_sums$MeanPrecip))

# Processing rainfall for spatial means
# Sum the spatial means over the year for each station
cwc_rain = aggregate(MeanPrecip~Year+Station,data = df,FUN = sum)
# Find the average across years for each station
cwc_mean = aggregate(MeanPrecip~Station, data = cwc_rain,FUN = mean)

#Find estimates of variance
cwc_se = aggregate(MeanPrecip~Station, data = cwc_rain,FUN = st.err)
colnames(cwc_se) = c("Station","se")
cwc_cv = aggregate(MeanPrecip~Station, data = cwc_rain,FUN = c.v)
colnames(cwc_cv) = c("Station","cv")
cwc_max = aggregate(MeanPrecip~Station, data = cwc_rain,FUN = max)
colnames(cwc_max) = c("Station","max")
cwc_min = aggregate(MeanPrecip~Station, data = cwc_rain,FUN = min)
colnames(cwc_min) = c("Station","min")

cwc_precipCompare = left_join(West,cwc_mean)
cwc_precipCompare = left_join(cwc_precipCompare,cwc_se)
cwc_precipCompare = left_join(cwc_precipCompare,cwc_cv)
cwc_precipCompare = left_join(cwc_precipCompare,cwc_max)
cwc_precipCompare = left_join(cwc_precipCompare,cwc_min)



# Processing rainfall
# Sum the spatial sums over the year for each station
cwc_rain = aggregate(MeanPrecip~Year+Station,data = df_sums,FUN = sum)
# Find the average across years for each station
cwc_sums = aggregate(MeanPrecip~Station, data = cwc_rain,FUN = mean)
colnames(cwc_sums) = c("Station","SumPrecip")
cwc_se_sum = aggregate(MeanPrecip~Station, data = cwc_rain,FUN = st.err)
colnames(cwc_se_sum) = c("Station","se_sum")
cwc_cv_sum = aggregate(MeanPrecip~Station, data = cwc_rain,FUN = c.v)
colnames(cwc_cv_sum) = c("Station","cv_sum")
cwc_max_sum = aggregate(MeanPrecip~Station, data = cwc_rain,FUN = max)
colnames(cwc_max_sum) = c("Station","max_sum")
cwc_min_sum = aggregate(MeanPrecip~Station, data = cwc_rain,FUN = min)
colnames(cwc_min_sum) = c("Station","min_sum")

cwc_precipCompare = left_join(cwc_precipCompare,cwc_sums)
cwc_precipCompare = left_join(cwc_precipCompare,cwc_se_sum)
cwc_precipCompare = left_join(cwc_precipCompare,cwc_cv_sum)
cwc_precipCompare = left_join(cwc_precipCompare,cwc_max_sum)
cwc_precipCompare = left_join(cwc_precipCompare,cwc_min_sum)

####################### Elevation ########################### 
setwd("F:/wild/SNAPP/West flowing river - CWC/Elevation/")
df_elev = read.csv("CWC_meanElev_8dd477134c629422cf98a9e549744dd0.csv",header=T)
df_elev = cbind(df_elev,read.csv("CWC_medianElev_ea84f79869fb47b1330493a3668c8ed7.csv",header=T))
df_elev = cbind(df_elev,read.csv("CWC_minMaxElev_07894e6ca0f84e6cd1b764ad31ba0720.csv",header=T))
df_elev$min[df_elev$min<0] = 0
df_elev$range = df_elev$max - df_elev$min
df_elev = subset(df_elev,select=c(Station,mean,median,range))
colnames(df_elev) = c("Station","elev_mean","elev_median","elev_range")
##############################################################

# add elev to the data set
cwc_precipCompare = left_join(cwc_precipCompare,df_elev)

###############################################################



pd = position_dodge(4)
ggplot(cwc_precipCompare,aes(Distance,SumPrecip,group=State)) + 
  geom_point(aes(shape=State),size = 2,position=pd) +
  #geom_errorbar(aes(ymin=min, ymax=max)) +
  geom_errorbar(aes(ymin=SumPrecip-cv_sum*SumPrecip, ymax=SumPrecip+cv_sum*SumPrecip),position=pd) +
  #geom_label(aes(label=abbreviate(Station)),size =4)+
  xlab("Distance (kms) from the South most CWC station (Ashramam)")+
  ylab("Average annual rainfall (mm) for the period 1981-2016 (mean +/- CV)")+
  geom_label_repel(aes(label = Station,color=Dammed),size = 3,
    box.padding = 0.35, point.padding = 0.5,
  segment.color = 'grey50',force = 2) +
  theme_classic(base_size = 12)+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=18,face="bold"),
        plot.title = element_text(size=20),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15))




#ggsave("CWC_RainfallCompare.jpg", plot = last_plot(), device = "jpg",path = "F:/wild/SNAPP/West flowing river - CWC/Results/Figures/",
#         scale = 3, width = NA, height = NA, units = c("in", "cm", "mm"),
#         dpi = 300, limitsize = TRUE)

###### Comparing the rainfall regimes of Haladi and Avershe

cwc_kar = aggregate(MeanPrecip~Year+Station,data = df[df$Station=='Haladi'|df$Station=='Avershe',],FUN = sum)
cwc_kar$Station = as.character(cwc_kar$Station)
boxplot(MeanPrecip~Station,data=cwc_kar)


############### K means clustering ####################
cwc_Kmean_data = cwc_precipCompare[complete.cases(cwc_precipCompare),]
cwc_Kmean_data = subset(cwc_Kmean_data,select=c(Station,Dammed,MeanPrecip,Area,elev_median,elev_range))
#cwc_Kmean_data = subset(cwc_Kmean_data,select=c(Station,Purpose,MeanPrecip,Area,elev_median,elev_range))
#cwc_Kmean_data = subset(cwc_Kmean_data,select=c(Station,Dammed,SumPrecip,elev_median,elev_range))
rownames(cwc_Kmean_data) = cwc_Kmean_data$Station
cwc_Kmean_data = cwc_Kmean_data[,-1]
#write.csv(cwc_Kmean_data,"F:/wild/SNAPP/West flowing river - CWC/Results/Intermediate_CSV_SHP_files/CWC_KMeans_Input.csv")



#cwc_Kmean_data = cwc_Kmean_data[(cwc_Kmean_data$Area<2000 & cwc_Kmean_data$MeanPrecip<4000),]
#cwc_Kmean_data = cwc_Kmean_data[cwc_Kmean_data$Area<2000,]

cwc_Kmean_data[!(cwc_Kmean_data$Area<2000 & cwc_Kmean_data$MeanPrecip<4000),]

# scale the data
#cwc_Kmean_data_scale = scale(cwc_Kmean_data)

# elbow method for finding the optimal k
fviz_nbclust(cwc_Kmean_data[,-1], kmeans, method = "wss")
fviz_nbclust(cwc_Kmean_data[,-1], kmeans, method = "silhouette")


# K means 
cwc_cluster = kmeans(cwc_Kmean_data[,-1],4,nstart = 50)

# This ratio should be high. Indicates total between cluster variation is high
# or in other words total within cluster variation is low.
cwc_cluster$betweenss / cwc_cluster$totss

# visualise it
fviz_cluster(cwc_cluster, data = cwc_Kmean_data[,-1])

# plot the data with any two of the axes and color code according to the cluster
cwc_cluster$cluster <- as.factor(cwc_cluster$cluster)

length(cwc_cluster$cluster)
ggplot(cwc_Kmean_data, aes(MeanPrecip,Area,color = cwc_cluster$cluster)) + 
#ggplot(cwc_Kmean_data, aes(SumPrecip, elev_median, color = cwc_cluster$cluster)) + 
  geom_point(aes(shape=Dammed),size = 2) + 
  geom_label_repel(aes(label = rownames(cwc_Kmean_data)),size = 2.5,
  box.padding = 0.2, point.padding = 0.2,
  segment.color = 'grey50',force = 2) +
  theme_classic(base_size = 12)+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=18,face="bold"),
        plot.title = element_text(size=20),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15))

#write.csv(cwc_cluster$centers,"F:/wild/SNAPP/West flowing river - CWC/Results/Intermediate_CSV_SHP_files/cluster_centers.csv")

nrow(cwc_Kmean_data)
summary(cwc_cluster$cluster)

summary(cwc_Kmean_data$Dammed)

KmeanSummary = cwc_Kmean_data
KmeanSummary = cbind(KmeanSummary,as.data.frame(cwc_cluster$cluster,col.names="Cluster"))

summarySE(KmeanSummary,measurevar = "MeanPrecip",groupvars = "cwc_cluster$cluster")
summarySE(KmeanSummary,measurevar = "Area",groupvars = "cwc_cluster$cluster")
summarySE(KmeanSummary,measurevar = "elev_median",groupvars = "cwc_cluster$cluster")
summarySE(KmeanSummary,measurevar = "elev_range",groupvars = "cwc_cluster$cluster")

#write.csv(cwc_cluster$cluster,"F:/wild/SNAPP/West flowing river - CWC/Results/Intermediate_CSV_SHP_files/cwc_clusters.csv")

#ggsave("CWC_cluster_DamType.jpg", plot = last_plot(), device = "jpg",path = "F:/wild/SNAPP/West flowing river - CWC/Results/Figures/",
#         scale = 2, width = NA, height = NA, units = c("in", "cm", "mm"),
#                dpi = 300, limitsize = TRUE)



set.seed(123)
gap_stat <- clusGap(cwc_Kmean_data, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
?clusGap
# Print the result
print(gap_stat, method = "firstmax")

fviz_gap_stat(gap_stat)
?fviz_gap_stat




######## April 3rd 2017 ###################
################# Now that we have the clusters, pick comparable pairs using proximity and data availability for the same period ###############


West = read.csv("F:/wild/SNAPP/West flowing river - CWC/Analysis/West_23_3_18_V1_1.csv", header = TRUE)
# Only stations which went through round 3 of data preprocessing.
West = (West[which(West$Round3 == "Yes"),])
West = subset(West,select=c(Station.Name,latitude,longitude,State,Dammed.,Purpose,Catch.Area,Cluster,start.year,end.year,Dam.built.year))
colnames(West) = c("Station","latitude","longitude","State","Dammed","Purpose","Area","Cluster","Start","End","DamYear")

########### Cluster 3 ############
clu = West[West$Cluster == "Clu_3",]

clu = clu[clu$Start<=clu$DamYear[clu$Station=="Haladi"] &
              clu$End>=clu$End[clu$Station=="Haladi"],]

Ref = c(clu[clu$Station=="Haladi",]$longitude,West[West$Station=="Haladi",]$latitude)
# Find the distance in kilometer from Haladi station to every other location
for(i in 1:nrow(clu)){
  Test = c(clu[i,]$longitude,clu[i,]$latitude)
  clu$Distance[i] = distm(Ref,Test,distGeo) / 1000
}

ggplot(clu,aes(x=Distance,y=Area)) + geom_point(aes(color=Dammed),size = 2) + 
  scale_color_manual(values=c("green","red"),labels = c("Dammed","Undammed"))+
  geom_label_repel(aes(label = Station),size = 2.5,
                   box.padding = 0.2, point.padding = 0.2,
                   segment.color = 'grey50',force = 2)+
  ggtitle("Cluster 3 - Haladi vs others")

?scale_color_manual

########### Cluster 1 ############
clu = West[West$Cluster == "Clu_1",]

clu = clu1[clu$Start<=clu$DamYear[clu$Station=="Pen"] &
              clu$End>=clu$End[clu$Station=="Pen"],]

Ref = c(clu[clu$Station=="Pen",]$longitude,clu[clu$Station=="Pen",]$latitude)
# Find the distance in kilometer from Haladi station to every other location
for(i in 1:nrow(clu)){
  Test = c(clu[i,]$longitude,clu[i,]$latitude)
  clu$Distance[i] = distm(Ref,Test,distGeo) / 1000
}

ggplot(clu,aes(x=Distance,y=Area)) + geom_point(aes(color=Dammed),size = 2) + 
  scale_color_manual(values=c("green","red"),labels = c("Dammed","Undammed"))+
  geom_label_repel(aes(label = Station),size = 2.5,
                   box.padding = 0.2, point.padding = 0.2,
                   segment.color = 'grey50',force = 2)+
  ggtitle("Cluster 1 - Pen vs others")

################# Cluster 2 #########################
clu = West[West$Cluster == "Clu_2",]

clu = clu[clu$Start<=clu$Start[clu$Station=="Malakkara"] &
            clu$End>=clu$End[clu$Station=="Malakkara"],]

Ref = c(clu[clu$Station=="Malakkara",]$longitude,clu[clu$Station=="Malakkara",]$latitude)
# Find the distance in kilometer from Haladi station to every other location
for(i in 1:nrow(clu)){
  Test = c(clu[i,]$longitude,clu[i,]$latitude)
  clu$Distance[i] = distm(Ref,Test,distGeo) / 1000
}

ggplot(clu,aes(x=Distance,y=Area)) + geom_point(aes(color=Dammed),size = 2) + 
  scale_color_manual(values=c("green","red"),labels = c("Dammed","Undammed"))+
  geom_label_repel(aes(label = Station),size = 2.5,
                   box.padding = 0.2, point.padding = 0.2,
                   segment.color = 'grey50',force = 2)+
  ggtitle("Cluster 2 - Malakkara vs others")


clu = West[West$Cluster == "Clu_2",]
clu = clu[clu$Start<=clu$Start[clu$Station=="Arangaly"],]

Ref = c(clu[clu$Station=="Arangaly",]$longitude,clu[clu$Station=="Arangaly",]$latitude)
# Find the distance in kilometer from Haladi station to every other location
for(i in 1:nrow(clu)){
  Test = c(clu[i,]$longitude,clu[i,]$latitude)
  clu$Distance[i] = distm(Ref,Test,distGeo) / 1000
}

ggplot(clu,aes(x=Distance,y=Area)) + geom_point(aes(color=Dammed),size = 2) + 
  scale_color_manual(values=c("green","red"),labels = c("Dammed","Undammed"))+
  geom_label_repel(aes(label = Station),size = 2.5,
                   box.padding = 0.2, point.padding = 0.2,
                   segment.color = 'grey50',force = 2)+
  ggtitle("Cluster 2 - Arangaly vs others")








