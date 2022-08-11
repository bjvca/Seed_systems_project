##run in Seed_systems_project/endline/data/agro_input/raw/
rm(list=ls())
library(leaflet)
library(htmlwidgets)
path <- getwd()

path <- strsplit(path,"data/agro_input/raw")[[1]]

dealer_endline <- read.csv("latest_dealer.csv")
dealer_endline$shop_ID[dealer_endline$X_uuid =="a78f6870-29b4-4e29-8820-aa2daf8ba8d0"] <- "AD_306"
dealer_endline$clearing[dealer_endline$X_uuid =="a78f6870-29b4-4e29-8820-aa2daf8ba8d0"] <- FALSE


###AD_39, AD_37  is duplicate, delete one instance - the other two 304 and 126 was one empty and one filled - empties deleted
dealer_endline <- dealer_endline[!(dealer_endline$X_uuid =="64cc523c-6816-48f5-929b-26e36944e744"),]
dealer_endline <- dealer_endline[!(dealer_endline$X_uuid =="0324c34f-78ff-47ef-9662-fda926fd44c1"),]
dealer_endline <- dealer_endline[!(dealer_endline$X_uuid =="89fafe9d-71ae-44d5-83f4-931040cffcae"),]
dealer_endline <- dealer_endline[!(dealer_endline$X_uuid =="f606e946-80c7-4abb-bbb8-ac1251ba5a8c"),]

### read data from moisture measuremment
moisture <- read.csv("latest_moisture.csv")

moisture$age <- difftime(strptime("10.08.2022", format = "%d.%m.%Y"),strptime(moisture$date_pack,format="%Y-%m-%d"),units="days")
moisture$age <- as.numeric(as.character(moisture$age))
dealer_endline$check.owner.agree.barcode <- as.numeric(as.character(dealer_endline$check.owner.agree.barcode))
dealer_endline$check.owner.agree.barcode[dealer_endline$X_uuid =="8569f346-5431-4be1-b9ed-057e761e9b2c"] <- 898989
moisture$barcode <- as.numeric(as.character(moisture$barcode))

##manually fix here to make sure all moisture measurements are used!

moisture$barcode[moisture$id=="AD 45 25"] <- 766
moisture$barcode[moisture$id=="AD 166 32"] <- 767
moisture$barcode[moisture$id=="AD 205 33"] <- 573
moisture$barcode[moisture$id=="AD 18456"] <- 511
moisture$barcode[moisture$id=="AD 84 27"] <- 679
moisture$barcode[moisture$id=="AD 205 33"] <- 572
moisture$barcode[moisture$id=="AD 22327"] <- 829
moisture$barcode[moisture$id=="AD 27 20"] <- 595
moisture$barcode[moisture$id=="AD 138 30"] <- 353
moisture$barcode[moisture$id=="AD 86 27"] <- 3288124
moisture$barcode[moisture$id=="AD 187 24"] <- 898989

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
dealer_endline$kg_improved[dealer_endline$kg_improved> 15000] <- NA 
summary(lm(kg_improved~clearing ,data=dealer_endline))

to_drop <- c( "enumerator", "district", "sub","parish","village", "hh_namex","hh_name",   "shed", "phone1", "phone2", "phone3","lat","long","check.q1" , "check.q1a","check.q1b","check.owner.consent", "check.owner.agree.respondent",  "check.q2",
              "check.owner.consent","check.owner.agree.respondent","check.owner.agree.nickname" ,"check.owner.agree.phoneX1", "check.owner.agree.phoneX2",      "biz_name" ,"clearing" ,"training","start","end", "start.geopoint", "X_start.geopoint_latitude","X_start.geopoint_longitude","X_start.geopoint_altitude","X_start.geopoint_precision","check.owner.agree.gps",   "check.owner.agree._gps_latitude",   "check.owner.agree._gps_longitude" ,"check.owner.agree._gps_altitude",   "check.owner.agree._gps_precision", "check.owner.agree.id",   "meta.instanceID", "X_id",     "X_uuid" , "X_submission_time", "X_date_modified", "X_tags",    "X_notes" ,  "X_version",  "X_duration" , "X_submitted_by","X_total_media",  "X_media_count", "X_media_all_received" ,     "X_xform_id",   "id", "check.owner.agree.barcode", "check.owner.agree._gps_precision")
dealer_endline <- dealer_endline[ , !(names(dealer_endline) %in% to_drop)]
write.csv(dealer_endline,"/home/bjvca/data/projects/Seed_systems_project/endline/data/agro_input/public/dealer_endline.csv", row.names=FALSE)



