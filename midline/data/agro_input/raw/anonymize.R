#read in raw data as expored from ONA
#execute from project_root/midline/data/agro_input/raw

rm(list=ls())
library(htmlwidgets)
path <- getwd()

### reads in raw data (not public)
shops <- read.csv(paste(path,"Dealer_MidlineV4_2022_02_16_03_40_40_437618-1.csv", sep="/"))

### code to determine remnants
#need to get raw baseline data

path <- strsplit(path, "/midline/data/agro_input/raw")[[1]]

raw_agro_base <- read.csv(paste(path,"baseline/data/agro_input/raw/raw_agro_input.csv", sep="/"))

### get from base those that are not in shop
raw_agro_base$done <- 1
raw_agro_base$done[!(raw_agro_base$shop_ID %in% shops$shop_ID)] <- 0


library(leaflet)
to_plot <- raw_agro_base[c("shop_ID","maize.owner.agree._gps_latitude","maize.owner.agree._gps_longitude","done")]

pal <- colorFactor(c("red", "green"), domain = c(1,0))

m <- leaflet() %>% setView(lat = 0.6, lng = 33.5, zoom=9)  %>%  addTiles(group="OSM") %>% addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",  group="Google", attribution = 'Google') %>% addProviderTiles(providers$OpenTopoMap, group="Topography") %>% addCircleMarkers(data=to_plot, lng=~as.numeric(as.character(maize.owner.agree._gps_longitude)),  lat=~as.numeric(as.character(maize.owner.agree._gps_latitude)),radius= 3, color=~pal(done), label=~as.character(shop_ID), group="X_uuid")   %>%  addLayersControl(baseGroups=c('OSM','Google','Topography'))



### export remnants for revisit

raw_agro_base_sub <- subset(raw_agro_base, !(shop_ID %in% shops$shop_ID))

remnants <- raw_agro_base_sub[c("shop_ID","enumerator", "district","sub","parish", "village","maize.owner.agree.dealer_name","maize.owner.agree.surname","maize.owner.agree.nickname", "maize.owner.agree.phone1", "maize.owner.agree.phone2", "maize.owner.agree.biz_name", "maize.owner.agree.family_name","maize.owner.agree.market_name","maize.owner.agree.eye","maize.owner.agree._gps_latitude","maize.owner.agree._gps_longitude")]

path <- getwd()
saveWidget(m, file="AD_remnants_map.html") 

write.csv(remnants,paste(path,"AD_remnants.csv", sep="/"), row.names=FALSE)

moisture <- read.csv(paste(path,"Moisture_Midline_2022_02_20_05_23_34_994254.csv", sep="/"))


moisture$age <- difftime(strptime("01.02.2022", format = "%d.%m.%Y"),strptime(moisture$date_pack,format="%Y-%m-%d"),units="days")
moisture$age[moisture$date_pack=="n/a"] <- difftime(strptime("01.02.2022", format = "%d.%m.%Y"),strptime(moisture$exp[moisture$date_pack=="n/a"] ,format="%Y-%m-%d"),units="days")+180

moisture$age <- as.numeric(as.character(moisture$age))
shops$owner.agree.barcode <- as.numeric(as.character(shops$owner.agree.barcode))
moisture$barcode <- as.numeric(as.character(moisture$barcode))

##manually fix here to make sure all moisture measurements are used!
moisture <- moisture[c("barcode","id","reading","exp","date_pack","origin","cert","lot","verif","variety","other_var","company","age")]

moisture$barcode[is.na(moisture$barcode)] <- 251

moisture$barcode[moisture$barcode =="1216189"] <- 252
moisture$barcode[moisture$barcode =="44446680"] <- 134
moisture$barcode[moisture$barcode =="747116"] <- 258
moisture$barcode[moisture$id =="AD 25942"] <- 187
shops$owner.agree.barcode[shops$owner.agree.barcode=="16344495"] <- 298 


shops_check <- merge(shops, moisture,by.x="owner.agree.barcode",by.y="barcode")

moisture[!(moisture$barcode %in% shops_check$owner.agree.barcode),]

shops <- merge(shops, moisture,by.x="owner.agree.barcode",by.y="barcode", all.x=T)


to_drop <- c( "enumerator", "district", "sub","parish","village", "hh_namex","hh_name",   "shed", "phone1","q1" , "q1a","q1b","owner.consent", "owner.agree.respondent",  "q2",
"owner.consent","owner.agree.respondent","owner.agree.nickname" ,"owner.agree.phone1", "owner.agree.phone2",      "biz_name" ,"clearing" ,"training","start","end", "start.geopoint", "X_start.geopoint_latitude","X_start.geopoint_longitude","X_start.geopoint_altitude","X_start.geopoint_precision","owner.agree.gps",   "owner.agree._gps_latitude",   "owner.agree._gps_longitude" ,"owner.agree._gps_altitude",   "owner.agree._gps_precision", "owner.agree.id",   "meta.instanceID", "X_id",     "X_uuid" , "X_submission_time", "X_date_modified", "X_tags",    "X_notes" ,  "X_version",  "X_duration" , "X_submitted_by","X_total_media",  "X_media_count", "X_media_all_received" ,     "X_xform_id",   "id", "owner.agree.barcode")
 shops <- shops[ , !(names(shops) %in% to_drop)]
 
 path <- strsplit(path, "/raw")[[1]]
 write.csv(shops,paste(path,"public/midline_dealer.csv", sep="/"), row.names=FALSE)
