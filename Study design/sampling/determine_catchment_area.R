set.seed(12345)
library(pracma)

### this is executed in the /report subdirectory, need to ..
path <- strsplit(getwd(), "/Study design/sampling")[[1]]

shops <- read.csv(paste(path,"stack surveys/data/agro_input_dealers.csv", sep ="/"))
farmers <- read.csv(paste(path,"stack surveys/data/farmers.csv", sep ="/"))
 
#merge each shop to all farmers 


 ### make in long format
connector <- reshape(farmers[c("id.agro1","id.agro2", "id.agro3","ID")], varying = c("id.agro1","id.agro2", "id.agro3"), direction="long", idvar="ID")[c("ID","id")]
names(connector) <- c("farmID", "shopID")
connector <- subset(connector, shopID != "n/a")
connector <- subset(connector, shopID != "")
### merge in GPS coordinates from the farmers


connector <- merge(connector, read.csv("/home/bjvca/data/projects/PIMMVC/data/raw_non_public/farmers_gps.csv")[c("ID","hh.maize._gps_latitude","hh.maize._gps_longitude")], by.x="farmID", by.y="ID")
names(connector) <- c("farmID","shopID","farmer_lat","farmer_long")
connector <- merge(connector, read.csv("/home/bjvca/data/projects/PIMMVC/data/raw_non_public/RawData_Shops_ids.csv")[c("id.agro","hh.maize._gps_latitude","hh.maize._gps_longitude")], by.x="shopID", by.y="id.agro")
names(connector) <- c("farmID","shopID","farmer_lat","farmer_long","shop_lat","shop_long")

connector$farmer_lat <- as.numeric(as.character(connector$farmer_lat))
connector$farmer_long <- as.numeric(as.character(connector$farmer_long))
connector$shop_lat <- as.numeric(as.character(connector$shop_lat))
connector$shop_long <- as.numeric(as.character(connector$shop_long))

connector <- subset(connector, !is.na(farmer_lat))

###now get distance between shops and farmers
connector$dist <- NA
for (i in 1:nrow(connector)) {
connector$dist[i] <- haversine(c(connector$farmer_lat[i] ,connector$farmer_long[i]),c(connector$shop_lat[i],connector$shop_long[1]))*1000
}

summary(aggregate(connector$dist, by=list(connector$farmID), mean)[,2])
