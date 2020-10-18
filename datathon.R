
#Package to auto load missing
#https://rpubs.com/abdul_yunus/Kmeans_Clustering
packages<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}


packages(tidyverse) # data manipulation
packages(corrplot)
packages(gridExtra)
packages(GGally)
packages(cluster) # clustering algorithms 
packages(factoextra) # clustering algorithms & visualization
packages(ggfortify)
packages(rnaturalearth)
packages(rnaturalearthdata)
packages(sf)
packages(ggplot2)
packages(rgeos)
packages(cowplot)
packages(googleway)
packages(ggrepel)
packages(maps)

#https://www.kaggle.com/johnjdavisiv/us-counties-covid19-weather-sociohealth-data
setwd("C:\\dev\\tamu_datathon\\data")
df<-read.csv("US_counties_COVID19_health_weather_data.csv")

df$ID <- seq.int(nrow(df))

#Dinstinct
df2<-df %>% distinct(fips, .keep_all=TRUE)

#Only numeric
df3<-df2 %>% select_if(is.numeric)

#Replace NA with mean
for(i in 1:ncol(df3)){
  df3[is.na(df3[,i]), i] <- mean(df3[,i], na.rm = TRUE)
}

#Drop 0 variance cols
which(apply(df3, 2, var) == 0)
drop.cols <-c("tornado","lat","lon")
df4<- df3 %>% select(!drop.cols)

#Scale
df10<-scale(df4)
pc <- prcomp(df10)

plot(pc)


std_dev<-pc$sdev
pr_var<-std_dev^2

#Select the top 40 columns from PCA
gg<-dimnames(pc$rotation)[[1]]
feature_cols<-gg[1:40]

#Go back to df3 and start with that (numeric columns; has our ID)
df20<- df3 %>% select(feature_cols)
df21<- scale(df20)


#Check elbow method
fviz_nbclust(x = df21,FUNcluster = kmeans, method = 'wss' )

#Send to kmeans
set.seed(123)
K2 <- kmeans(df21, centers = 4, nstart = 25)

#Stitch together with original source
o<-order(K2$cluster)

#Synch cluster ids with row id
df.id.clusters<-data.frame(df3$ID[o],K2$cluster[o])
colnames(df.id.clusters)<-c("ID", "CLUSTER")
str(df.id.clusters)

#Join to city
df.final<- df %>% inner_join(df.id.clusters, KEEP= TRUE)
df.final.summary <- df.final %>% select("county" , "state", "lat" , "lon" , "CLUSTER")
df.final.summary$ID <- paste(tolower(df.final.summary$state),tolower(df.final.summary$county), sep=",")

#
# Plot clusters 1 and 2 on map
#
world <- ne_countries(scale = "medium", returnclass = "sf")
theme_set(theme_bw())
ggplot(data = world) +
  geom_sf() +
  geom_point(data = df.final[df.final$CLUSTER==1,], aes(x = lon, y = lat), size = 1, 
             shape = 23, fill = "darkred") + 
              coord_sf(xlim = c(-162, -60), ylim = c(19, 64), expand = FALSE)  + 
  geom_point(data = df.final[df.final$CLUSTER==2,], aes(x = lon, y = lat), size = 1, 
             shape = 23, fill = "darkblue") + 
  coord_sf(xlim = c(-162, -60), ylim = c(19, 64), expand = FALSE)
  
#
# Color code county by cluster
#
counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))
counties <- subset(counties, grepl("florida", counties$ID))
counties$area <- as.numeric(st_area(counties))

#Here we join the county shape file by the data
county_clusters<-counties %>% inner_join(df.final.summary)

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = county_clusters, aes(fill = CLUSTER)) +
  scale_fill_viridis_c(trans = "sqrt", alpha = .4) + 
  coord_sf(xlim = c(-140, -60), ylim = c(19, 64), expand = FALSE)


