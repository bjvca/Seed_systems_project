##run in Seed_systems_project/endline/data/agro_input/raw/
rm(list=ls())
library(leaflet)
library(htmlwidgets)
path <- getwd()

path <- strsplit(path,"data/agro_input/raw")[[1]]

dealer_endline <- read.csv("latest_dealer.csv")

##visualize progress
dealer_base <- read.csv(paste(path,"/questionnaires/agro_input/to_upload_endline_shops.csv",sep=""))
dealer_base$done <- FALSE
dealer_base$done[(dealer_base$shop_ID %in% (names(table(dealer_endline$shop_ID))))] <- TRUE
pal <- colorFactor(c("red", "green"),dealer_base$done)
m <- leaflet() %>% setView(lat = 0.65, lng = 33.62, zoom=11)  %>%  addTiles(group="OSM") %>% addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",  group="Google", attribution = 'Google')  %>% addProviderTiles(providers$OpenTopoMap, group="Topography") %>% addCircleMarkers(data=dealer_base, lng=~maize.owner.agree._gps_longitude, lat=~maize.owner.agree._gps_latitude,radius= 2, color=~pal(done), popup = ~as.character(shop_ID) )   %>%  addLayersControl(baseGroups=c('OSM','Google','Topography'))
saveWidget(m, file="endline_progress.html") 
### merge in moisture measurements

moisture <- read.csv("latest_moisture.csv")


moisture$age <- difftime(strptime("20.07.2022", format = "%d.%m.%Y"),strptime(moisture$date_pack,format="%Y-%m-%d"),units="days")

moisture$age <- as.numeric(as.character(moisture$age))
dealer_endline$check.owner.agree.barcode <- as.numeric(as.character(dealer_endline$check.owner.agree.barcode))
moisture$barcode <- as.numeric(as.character(moisture$barcode))

##manually fix here to make sure all moisture measurements are used!

moisture$barcode[moisture$id=="AD 45 25"] <- 766
moisture$barcode[moisture$id=="AD 166 32"] <- 767
moisture$barcode[moisture$id=="AD 205 33"] <- 573
moisture$barcode[moisture$id=="AD 18456"] <- 511
moisture$barcode[moisture$id=="AD 84 27"] <- 679
moisture$barcode[moisture$id=="AD 205 33"] <- 572

#These have data in the moisture dataset, but not (yet) in dealer_endline
# moisture$barcode[!(moisture$barcode %in% dealer_endline$check.owner.agree.barcode)]
# 678 765 514 608 605 611 602 761 573
# moisture$id[!(moisture$barcode %in% dealer_endline$check.owner.agree.barcode)]
# "AD2340"     "AD-31452"   "AD 2642"    "AD 70 19"   " AD 250 16" "AD 160 30"  "AD 347 22"  "AD 14029"  


dealer_endline[dealer_endline$shop_ID=="AD_250",]

moisture <- moisture[c("barcode","id","reading","exp","date_pack","origin","cert","lot","verif","variety","other_var","company","age")]

dealer_endline <- merge(dealer_endline, moisture,by.x="check.owner.agree.barcode",by.y="barcode", all.x=T)

#just to try
summary(lm(reading~clearing, data=dealer_endline))
dealer_endline$check.owner.agree.longe5.q50 <- as.numeric(as.character(dealer_endline$check.owner.agree.longe5.q50 ))
dealer_endline$check.owner.agree.longe4.q62 <- as.numeric(as.character(dealer_endline$check.owner.agree.longe4.q62))
dealer_endline$check.owner.agree.long10h.q25 <- as.numeric(as.character(dealer_endline$check.owner.agree.long10h.q25))
dealer_endline$check.owner.agree.longe7H.q38 <- as.numeric(as.character(dealer_endline$check.owner.agree.longe7H.q38)) 

dealer_endline$check.owner.agree.longe5.q50[is.na(dealer_endline$check.owner.agree.longe5.q50) ] <- 0
dealer_endline$check.owner.agree.longe4.q62[is.na(dealer_endline$check.owner.agree.longe4.q62)] <- 0
dealer_endline$check.owner.agree.long10h.q25[is.na(dealer_endline$check.owner.agree.long10h.q25)] <- 0
dealer_endline$check.owner.agree.longe7H.q38[is.na(dealer_endline$check.owner.agree.longe7H.q38)] <- 0

#sum 
dealer_endline$kg_improved <- dealer_endline$check.owner.agree.longe5.q50 + dealer_endline$check.owner.agree.longe4.q62 + dealer_endline$check.owner.agree.long10h.q25 + dealer_endline$check.owner.agree.longe7H.q38

summary(lm(kg_improved~clearing ,data=dealer_endline))

to_drop <- c( "enumerator", "district", "sub","parish","village", "hh_namex","hh_name",   "shed", "phone1", "phone2", "phone3","lat","long","check.q1" , "check.q1a","check.q1b","check.owner.consent", "check.owner.agree.respondent",  "check.q2",
              "check.owner.consent","check.owner.agree.respondent","check.owner.agree.nickname" ,"check.owner.agree.phoneX1", "check.owner.agree.phoneX2",      "biz_name" ,"clearing" ,"training","start","end", "start.geopoint", "X_start.geopoint_latitude","X_start.geopoint_longitude","X_start.geopoint_altitude","X_start.geopoint_precision","check.owner.agree.gps",   "check.owner.agree._gps_latitude",   "check.owner.agree._gps_longitude" ,"check.owner.agree._gps_altitude",   "check.owner.agree._gps_precision", "check.owner.agree.id",   "meta.instanceID", "X_id",     "X_uuid" , "X_submission_time", "X_date_modified", "X_tags",    "X_notes" ,  "X_version",  "X_duration" , "X_submitted_by","X_total_media",  "X_media_count", "X_media_all_received" ,     "X_xform_id",   "id", "check.owner.agree.barcode", "check.owner.agree._gps_precision")
dealer_endline <- dealer_endline[ , !(names(dealer_endline) %in% to_drop)]
write.csv(dealer_endline,"/home/bjvca/data/projects/Seed_systems_project/endline/data/agro_input/public/dealer_endline.csv", row.names=FALSE)



