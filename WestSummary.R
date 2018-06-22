library(dplyr)
library(reshape2)

setwd("F:/wild/SNAPP/West flowing river - CWC/Analysis")

West = read.csv("West_11_1_18_V1_1.csv", header = TRUE)
West = read.csv("F:/wild/SNAPP/West flowing river - CWC/Analysis/West_16_3_18_V1_1.csv", header = TRUE)
West = read.csv("F:/wild/SNAPP/West flowing river - CWC/Analysis/West_23_3_18_V1_1.csv", header = TRUE)
West = read.csv("F:/wild/SNAPP/West flowing river - CWC/Analysis/West_19_6_18_V1_2_38Stations.csv", header = TRUE)
West = read.csv("F:/wild/SNAPP/West flowing river - CWC/Analysis/West_20_3_18_V1_2.csv", header = TRUE)

# Only stations which went through round 2 of data preprocessing
#West = (West[which(West$Round2 == "Yes"),])
West = (West[which(West$Round3 == "Yes"),])

#Read in the clusters names and combine with West
Clusters = read.csv("F:/wild/SNAPP/West flowing river - CWC/Results/Intermediate_CSV_SHP_files/cwc_clusters.csv",header=T)
colnames(Clusters) = c(colnames(West)[1],"Cluster")
West = left_join(West,Clusters)

nrow(West)

# get the basin ID
Basin_IDs = read.csv("F:/wild/SNAPP/West flowing river - CWC/Results/Intermediate_CSV_SHP_files/WestCWCStations_BasinID.csv", header = TRUE)
Basin_IDs = Basin_IDs[,-c(1,2)]
colnames (Basin_IDs) = c("Station.Name","Basin_IDs","Basin_Area")

West = left_join(West,Basin_IDs)

#write.csv(West,"F:/wild/SNAPP/West flowing river - CWC/Analysis/West_round2_shp_11_1_2018.csv")

#Find out how many basins are ungauged




# Temporal spread
min(West$start.year)
min(West$end.year) - max(West$end.year)
West$years = West$end.year - West$start.year
summary(West$years)

boxplot(West$years)

# spatial spread
boxplot(West$River)

#Basin ID's are more reliable
West$Basin_IDs = factor(West$Basin_IDs)
unique(West$Basin_IDs)

summary(West$Dammed.)

summary(West$Catch.Area)
mean(West$Catch.Area)

sd(West$Catch.Area) / sqrt(length(West$Catch.Area))

summary(West$Purpose)


# build a new dataset for visualising temporal spread

#create y axis with years from 1970 to 2016
Years = seq(min(West$start.year),max(West$end.year),1)
length(Years)

West_temporalSpread = data.frame(matrix(0, ncol = length(West$Station.Name), nrow = length(Years)))
rownames(West_temporalSpread) = Years
colnames(West_temporalSpread) = (as.character(West$Station.Name))

for(i in 1:ncol(West_temporalSpread))
{
  zero_top = zero_bottom = data_present = dammed = 0
  if(West$start.year[i] > min(West$start.year))
  {
    zero_top    = West$start.year[i] - min(West$start.year)
  }
  if(West$end.year[i]<max(West$end.year))
  {
    zero_bottom = max(West$end.year) - West$end.year[i]      
  }
  
  data_present = nrow(West_temporalSpread) - (zero_top + zero_bottom)
  
  #for dammed rivers find out the number of dammed years
  if(West$Dammed.[i] == "Yes")
  {
    if(West$Dam.built.year[i] > West$start.year[i])
    
    {
        dammed =  West$end.year[i] - West$Dam.built.year[i]
        undammed = data_present - dammed
    }
    # all the records are dammed discharge
    else
    {
        dammed = data_present
        undammed = 0 
    }
      
  }
  else
  {
      undammed = data_present
      dammed = 0
  }
  West_temporalSpread[,i] = (c(rep(0,zero_top),rep(1,undammed),rep(2,dammed),rep(0,zero_bottom)))
}

West_temporalSpread = t(West_temporalSpread)
West_temporalSpread = melt(West_temporalSpread)
colnames(West_temporalSpread)= c("Station.Name","Year","Value")

temp = left_join(West_temporalSpread,West)
West_temporalSpread = subset(temp,select = c("Station.Name","Year","Value","State","Cluster"))

West_temporalSpread$Value = as.factor(West_temporalSpread$Value)

ggplot(West_temporalSpread, aes(x = Year, y = Station.Name)) + 
  geom_tile(aes(color=Value,fill = Value),linetype = "dotted") +
  scale_fill_manual(values = c("grey85", "grey51","brown"),labels=c("No data","Undammed","Dammed"),guide = "legend")+
  scale_color_manual(values =c("black", "black","black"),guide=FALSE)+
  labs(x="Years", y="Station Name", title="Temporal spread of CWC discharge data") +
  theme_bw() + theme(axis.text.x=element_text(size=12, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=12),
                     plot.title=element_text(size=13))+
  scale_x_continuous(name="Years",breaks=seq(min(West$start.year),max(West$end.year),2))+
  theme(panel.grid = element_blank()) +facet_grid(State~.,space = "free",scales = "free")+
  theme(strip.text.y = element_text(angle = 0, hjust = 0))+
  theme(legend.position = "top")

West_temporalSpread$Cluster = factor(West_temporalSpread$Cluster )

  cluster_names <- list(
    '1'="Cluster 1",
    '2'="Cluster 2",
    '3'="Cluster 3",
    '4'="Cluster 4"
  )

  cluster_labeller <- function(variable,value){
    return(cluster_names[value])
  }
  
  
# with clusters instead of states
ggplot(West_temporalSpread, aes(x = Year, y = Station.Name)) + 
  geom_tile(aes(color=Value,fill = Value),linetype = "dotted") +
  scale_fill_manual(values = c("grey85", "grey51","grey20"),labels=c("No data","Undammed","Dammed"),guide = "legend")+
  scale_color_manual(values =c("black", "black","black"),guide=FALSE)+
  labs(x="Years", y="Station Name") +
  theme_bw() + theme(axis.text.x=element_text(size=12, angle=20, vjust=0.3),
                     axis.text.y=element_text(size=12),
                     plot.title=element_text(size=13))+
  scale_x_continuous(name="Years",breaks=seq(min(West$start.year),max(West$end.year),2))+
  theme(panel.grid = element_blank()) +facet_grid(Cluster~.,space = "free",scales = "free",labeller = cluster_labeller)+
  theme(strip.text.y = element_text(angle = 0, hjust = 0))+
  theme(legend.position = "bottom")

ggsave("K_means_Clustering_20_06_18.jpg", plot = last_plot(), device = "jpg",path = "F:/wild/SNAPP/West flowing river - CWC/Results/Figures/",
     scale = 1.9, width = 7, height = 5, units = c("in", "cm", "mm"),
     dpi = 400, limitsize = TRUE)



# Summarising the dammed stations.
dammed_stations = read.csv("F:/wild/SNAPP/West flowing river - CWC/Analysis/Dammed_Stations.csv",header=T)
colnames(dammed_stations)
dammed_stations = subset(dammed_stations,select=c("start.year","end.year","Station.Name","Stations.upstream","dam_name","Distance.from.the.dam.outlet"))

dam_summary = read.csv("F:/WIld/SNAPP/West flowing river - CWC/NRLD/Dams_WG_NRLD_consolidated.csv",header=T)
colnames(dam_summary)
dam_summary = subset(dam_summary,select = c("dam_name","Year_of_co","Height_abo","Area","Purpose2"))

dammed_stations = left_join(dammed_stations,dam_summary)
str(dammed_stations)
dammed_stations$start.year = as.numeric(dammed_stations$start.year)
dammed_stations$end.year = as.numeric(dammed_stations$end.year)
dammed_stations$Year_of_co = as.numeric(as.character(dammed_stations$Year_of_co))

dammed_stations$pre = dammed_stations$Year_of_co - dammed_stations$start.year
dammed_stations$pre[dammed_stations$pre<0] = 0

dammed_stations$post = ifelse(dammed_stations$Year_of_co<=dammed_stations$start.year,
                              dammed_stations$end.year - dammed_stations$start.year,
                              dammed_stations$end.year - dammed_stations$Year_of_co  )

dammed_stations = subset(dammed_stations,select = c(-1,-2))
dammed_stations$Area = dammed_stations$Area / 1000000 
colnames(dammed_stations) = c("Station Name","Stations upstream","Dam/reservoir name",
                              "Distance from the station to the dam outlet (km)","Year of dam commissioning",
                              "Height of the dam (m)","Area of the reservoir (sq km)","Purpose","Pre-dam discharge (years)",
                              "Post-dam discharge (years)")


write.csv(dammed_stations,"F:/WIld/SNAPP/West flowing river - CWC/Results/Intermediate_CSV_SHP_files/Dammed_summary_dam_characteristics_records.csv")
