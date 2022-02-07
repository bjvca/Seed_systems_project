#read in raw data as expored from ONA
#execute from project_root/midline/data/agro_input/raw

rm(list=ls())

path <- getwd()

### reads in raw data (not public)
shops <- read.csv(paste(path,"Dealer_MidlineV4_2022_02_07_03_51_37_380318.csv", sep="/"))
moisture <- read.csv(paste(path,"Moisture_Midline_2022_02_07_03_52_39_498233.csv", sep="/"))


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
