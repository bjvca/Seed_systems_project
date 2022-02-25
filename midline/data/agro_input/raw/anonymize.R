#read in raw data as expored from ONA
#execute from project_root/midline/data/agro_input/raw

rm(list=ls())
library(htmlwidgets)
path <- getwd()

### reads in raw data (not public)
shops <- read.csv(paste(path,"Dealer_MidlineV4_2022_02_16_03_40_40_437618-1.csv", sep="/"))
shops$shop_ID <- as.character(shops$shop_ID)
### five duplicate... four shops have ID == "n/a" and these n/a's also do not have "clearing" and "trainin" status... add here?
shops$shop_ID[shops$X_uuid =="a3faaf4d-2704-4b80-953a-b968579fe6b9"] <- "AD_131"
shops$clearing[shops$X_uuid =="a3faaf4d-2704-4b80-953a-b968579fe6b9"] <- TRUE
shops$training[shops$X_uuid =="a3faaf4d-2704-4b80-953a-b968579fe6b9"] <- FALSE

shops$shop_ID[shops$X_uuid =="3f435fb7-4832-4fb3-83bd-be1ae701db6e"] <- "AD_306"
shops$clearing[shops$X_uuid =="3f435fb7-4832-4fb3-83bd-be1ae701db6e"] <- FALSE
shops$training[shops$X_uuid =="3f435fb7-4832-4fb3-83bd-be1ae701db6e"] <- TRUE

shops$shop_ID[shops$X_uuid =="2c0f8acf-b804-4816-9545-ff36b332cddb"] <- "AD_65"
shops$clearing[shops$X_uuid =="2c0f8acf-b804-4816-9545-ff36b332cddb"] <- TRUE
shops$training[shops$X_uuid =="2c0f8acf-b804-4816-9545-ff36b332cddb"] <- FALSE

shops$shop_ID[shops$X_uuid =="dc8dc036-8ae8-41e7-a669-48af3e02da74"] <- "AD_270"
shops$clearing[shops$X_uuid =="dc8dc036-8ae8-41e7-a669-48af3e02da74"] <- TRUE
shops$training[shops$X_uuid =="dc8dc036-8ae8-41e7-a669-48af3e02da74"] <- FALSE

shops$shop_ID[shops$X_uuid =="f1efbe32-f4e9-44b1-b2dd-97a38c33897e"] <- "AD_279"
shops$clearing[shops$X_uuid =="f1efbe32-f4e9-44b1-b2dd-97a38c33897e"] <- TRUE
shops$training[shops$X_uuid =="f1efbe32-f4e9-44b1-b2dd-97a38c33897e"] <- FALSE

### this one seems to be a duplicate - wife was interviewed by a different enumerator later in the day...
shops <- subset(shops, X_uuid != "1686391d-9793-46bb-beb4-26922ac42dd7")


### just to try
shops$longe10h_kg <- as.numeric(as.character(shops$owner.agree.long10h.q25))
shops$longe10h_kg[is.na(shops$longe10h_kg)] <- 0

shops$longe7h_kg <-as.numeric(as.character(shops$owner.agree.longe7H.q38))
shops$longe7h_kg[is.na(shops$longe7h_kg)] <- 0

shops$longe5_kg <- as.numeric(as.character(shops$owner.agree.longe5.q50))
shops$longe5_kg[is.na(shops$longe5_kg)] <- 0

shops$longe4_kg <- as.numeric(as.character(shops$owner.agree.longe4.q62))
shops$longe4_kg[is.na(shops$longe4_kg)] <- 0

shops$tot_kg <- shops$longe10h_kg + shops$longe7h_kg+ shops$longe5_kg + shops$longe4_kg

summary(lm(tot_kg~clearing, data=shops))

#merge in moisture

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
moisture$barcode[moisture$id =="AD 11043"] <- 177
moisture$barcode[moisture$id =="AD 3448"] <- 320
shops$owner.agree.barcode[shops$owner.agree.barcode=="16344495"] <- 298
shops$owner.agree.barcode[shops$owner.agree.barcode=="11217770"] <- 318 


shops_check <- merge(shops, moisture,by.x="owner.agree.barcode",by.y="barcode")

moisture[!(moisture$barcode %in% shops_check$owner.agree.barcode),]
shops[!(shops$owner.agree.barcode %in% shops_check$owner.agree.barcode),]



shops <- merge(shops, moisture,by.x="owner.agree.barcode",by.y="barcode", all.x=T)


to_drop <- c( "enumerator", "district", "sub","parish","village", "hh_namex","hh_name",   "shed", "phone1","q1" , "q1a","q1b","owner.consent", "owner.agree.respondent",  "q2",
"owner.consent","owner.agree.respondent","owner.agree.nickname" ,"owner.agree.phone1", "owner.agree.phone2",      "biz_name" ,"clearing" ,"training","start","end", "start.geopoint", "X_start.geopoint_latitude","X_start.geopoint_longitude","X_start.geopoint_altitude","X_start.geopoint_precision","owner.agree.gps",   "owner.agree._gps_latitude",   "owner.agree._gps_longitude" ,"owner.agree._gps_altitude",   "owner.agree._gps_precision", "owner.agree.id",   "meta.instanceID", "X_id",     "X_uuid" , "X_submission_time", "X_date_modified", "X_tags",    "X_notes" ,  "X_version",  "X_duration" , "X_submitted_by","X_total_media",  "X_media_count", "X_media_all_received" ,     "X_xform_id",   "id", "owner.agree.barcode")
 shops <- shops[ , !(names(shops) %in% to_drop)]
 
 path <- strsplit(path, "/raw")[[1]]
 write.csv(shops,paste(path,"public/midline_dealer.csv", sep="/"), row.names=FALSE)
