#read in raw data as expored from ONA

library(pracma)
library(leaflet)

path <- getwd()

shops <- rbind(read.csv(paste(path,"Baseline_DealerXX_2020_09_27_03_06_54_713799.csv", sep="/")),read.csv(paste(path,"Baseline_DealerXXXX_2020_09_30_04_27_16_594243.csv", sep="/")))



#create shop_ID

shops$shop_ID <- paste("AD",rownames(shops), sep="_")
shops$shop_ID <- factor(shops$shop_ID)

#Categorizing differnt input dealers into catchment areas
shops$catchmentID <- NA
counter <- 1

for (shop_1 in names(table(shops$shop_ID))) {

shops$catchmentID[shops$shop_ID == shop_1] <- counter


for (shop_2 in names(table(shops$shop_ID))) {
### key parameter is chosen here: distance to define a catchment area. Here we assume that if shops are less then 5 km apart, they serve the same catchment area
if ( haversine(c(shops$maize.owner.agree._gps_latitude[shops$shop_ID == shop_1] ,shops$maize.owner.agree._gps_longitude[shops$shop_ID == shop_1]),c(shops$maize.owner.agree._gps_latitude[shops$shop_ID == shop_2],shops$maize.owner.agree._gps_longitude[shops$shop_ID == shop_2])) < 2.5) {
if (is.na(shops$catchmentID[shops$shop_ID == shop_2])) {  ## if the shop has not been allocated to a catcchment area yet, create a new one
 shops$catchmentID[shops$shop_ID == shop_2] <- counter
} else {  ## if the shop is already part of a catchment area
## change ID of all shops in catchement area to a new catchment area
 shops$catchmentID[shops$catchmentID == shops$catchmentID[shops$shop_ID == shop_1]]  <- shops$catchmentID[shops$shop_ID == shop_2] 
}

}
}
counter <- counter + 1
}
dim(table(shops$catchmentID))


pal <- colorFactor(
  palette = 'Dark2',
  domain = shops$catchmentID
)


m <- leaflet() %>% setView(lat = 0.65, lng = 33.62, zoom=11)  %>%  addTiles(group="OSM") %>% addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",  group="Google", attribution = 'Google')  %>% addProviderTiles(providers$OpenTopoMap, group="Topography") %>% addCircleMarkers(data=shops, lng=~maize.owner.agree._gps_longitude, lat=~maize.owner.agree._gps_latitude,radius= 8, color=~pal(catchmentID), popup = ~as.character(catchmentID))   %>%  addLayersControl(baseGroups=c('OSM','Google','Topography'))

#prepare data for public release
##remove GPS coordinates
## remove metadata
#note: use "maize.owner.agree.id" to link records to seed quality test data (moisture, packageing, ...)


to_drop <- c("start","end","deviceid","simserial", "phonenumber", "subscriberid", "start.geopoint","X_start.geopoint_latitude", "X_start.geopoint_longitude","X_start.geopoint_altitude", "X_start.geopoint_precision","maize.owner.agree.eye","maize.owner.agree.id", "meta.instanceID","X_id", "X_uuid","X_submission_time", "X_tags","X_notes","X_version", "X_duration", "X_submitted_by", "X_total_media", "X_media_count","X_media_all_received", "X_xform_id" )   
 
shops <- shops[ , !(names(shops) %in% to_drop)]

##remove names and other data that can be used to id
to_drop <- c("maize.owner.agree.gps","maize.owner.agree._gps_longitude","maize.owner.agree._gps_latitude") 
shops <- shops[ , !(names(shops) %in% to_drop)]

#this links to pictures
to_drop <- c("maize.owner.agree.q13")
shops <- shops[ , !(names(shops) %in% to_drop)]

## remove villages where most customers are - this needs to be used for sampling of households
to_drop <- c("maize.owner.agree.catch_area.Village1", "maize.owner.agree.catch_area.Village2" , "maize.owner.agree.catch_area.Village3")  
shops <- shops[ , !(names(shops) %in% to_drop)]

## drop location, names and contact details
to_drop <- c("parish","village" ,"maize.owner.agree.dealer_name", "maize.owner.agree.surname", "maize.owner.agree.nickname", "maize.owner.agree.phone1", "maize.owner.agree.phone2", "maize.owner.agree.biz_name", "maize.owner.agree.family_name", "maize.owner.agree.market_name","enumerator","other_district")
 shops <- shops[ , !(names(shops) %in% to_drop)]

##convert district names and subcounty names to numeric codes 
i_dist <- 1
farmers$distID <- NULL
farmers$subID <- NULL

for (dist in names(table(shops$district))) {
	print(dist)
	i_sub <- 1
	for (sub in names(table(shops$sub[shops$district==dist]))) {
		print(sub)

		shops$subID[shops$district==dist & shops$sub == sub] <- i_sub
		i_sub <- i_sub + 1
	}
shops$distID[shops$district==dist ] <- i_dist
i_dist <- i_dist + 1
}
 
### BUT subcounty names was not pre-populated in the ODK app, so enumerators often spell differently/wrong, so better just delete (or manually correct first)
to_drop <- c("subID","district","sub")
 shops <- shops[ , !(names(shops) %in% to_drop)]
 
 
 # reorder catchement ID factor
 	i_catch <- 1
	for (catch in names(table(shops$catchmentID))) {

		shops$catchID[shops$catchmentID == catch] <- i_catch
		i_catch <- i_catch + 1
	}
 shops$catchmentID <- NULL
 
 ###write to public directory
 
 path <- strsplit(path, "/raw")[[1]]
 write.csv(shops,paste(path,"public/baseline_dealer.csv", sep="/"), row.names=FALSE)
 
 
 

