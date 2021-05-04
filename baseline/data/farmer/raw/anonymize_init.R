#read in raw data as expored from ONA
#execute from /NWO seed system devt Uganda proposal development/baseline/data/farmer/raw/

rm(list=ls())
library(ggplot2)
library(reshape2)

set.seed(10042021)  #today's date

path <- getwd()

### reads in raw data (not public)
farmers <-read.csv(paste(path,"baseline_farmer_2021_04_25_15_03_43_633118.csv", sep="/"))

#charles changed this name?
farmers$village[farmers$village == "Buwaje"] <- "Buwagi"
### fix IDs
##one duplicate in village but only 9 farmers interviewed
farmers$farmer_ID[farmers$village == "Musakira" & farmers$farmer_ID == "F_2997" & farmers$X_id == "77127699"] <-  "F_2998"
farmers$farmer_ID[farmers$village == "Kasaka" & farmers$farmer_ID == "F_2516" & farmers$X_id == "77127934"] <-  "F_2512"
farmers$farmer_ID[farmers$village == "Kityelera" & farmers$farmer_ID == "F_1457" & farmers$X_id == "77154707"] <-  "F_1458"
farmers$farmer_ID[farmers$village == "Namusambya" & farmers$farmer_ID == "F_3461" & farmers$X_id == "77158044"] <-  "F_3463"
farmers$farmer_ID[farmers$village == "Bukaleba" & farmers$farmer_ID == "F_374" & farmers$X_id == "77221052"] <-  "F_375"
farmers$farmer_ID[farmers$village == "Nakalanga" & farmers$farmer_ID == "F_231" & farmers$X_id == "77260860"] <-  "F_232"
farmers$farmer_ID[farmers$village == "Naigobya" & farmers$farmer_ID == "F_1131" & farmers$X_id == "77310057"] <-  "F_1139"
farmers$farmer_ID[farmers$village == "Busanda" & farmers$farmer_ID == "F_1842" & farmers$X_id == "77353708"] <-  "F_1848"
farmers$farmer_ID[farmers$village == "Nawantale" & farmers$farmer_ID == "F_1481" & farmers$X_id == "77353775"] <-  "F_1488"
farmers$farmer_ID[farmers$village == "Namatooke" & farmers$farmer_ID == "F_2100" & farmers$X_id == "77386817"] <-  "F_2091"
farmers$farmer_ID[farmers$village == "Nabuguzi" & farmers$farmer_ID == "F_1645" & farmers$X_id == "77431542"] <-  "F_1641"
farmers$farmer_ID[farmers$village == "Matyama" & farmers$farmer_ID == "F_2185" & farmers$X_id == "77431611"] <-  "F_2184"
farmers$farmer_ID[farmers$village == "Namato" & farmers$farmer_ID == "F_1639" & farmers$X_id == "77471346"] <-  "F_1633"
farmers$farmer_ID[farmers$village == "Busenke" & farmers$farmer_ID == "F_3429" & farmers$X_id == "77471516"] <-  "F_3428"
farmers$farmer_ID[farmers$village == "Buyunga (Kaliro)" & farmers$farmer_ID == "F_918" & farmers$X_id == "77471605"] <-  "F_919"
farmers$farmer_ID[farmers$village == "Bumanya" & farmers$farmer_ID == "F_2697" & farmers$X_id == "77520869"] <-  "F_2691"
farmers$farmer_ID[farmers$village == "Nawandio" & farmers$farmer_ID == "F_2867" & farmers$X_id == "77521107"] <-  "F_2866"
farmers$farmer_ID[farmers$village == "Kisozi" & farmers$farmer_ID == "F_2245" & farmers$X_id == "77721638"] <-  "F_2242"

##two duplicates in village but only 8 farmers interviewed
farmers$farmer_ID[farmers$village == "Nasuti" & farmers$farmer_ID == "F_741" & farmers$X_id == "77310028"] <- "F_747"
farmers$farmer_ID[farmers$village == "Nasuti" & farmers$farmer_ID == "F_742" & farmers$X_id == "77310022"] <- "F_750"

farmers$farmer_ID[farmers$village == "Buwaaya" & farmers$farmer_ID == "F_2222" & farmers$X_id == "77127585"] <- "F_2228"
farmers$farmer_ID[farmers$village == "Buwaaya" & farmers$farmer_ID == "F_2225" & farmers$X_id == "77127666"] <- "F_2230"

farmers$farmer_ID[farmers$village == "Kigandaalo" & farmers$farmer_ID == "F_1051" & farmers$X_id == "77221038"] <- "F_1056"
farmers$farmer_ID[farmers$village == "Kigandaalo" & farmers$farmer_ID == "F_1055" & farmers$X_id == "77221059"] <- "F_1060"

##three duplicates in village but only 7 farmers interviewed
farmers$farmer_ID[farmers$village == "Bukowe" & farmers$farmer_ID == "F_146" & farmers$X_id == "77386572"] <- "F_141"
farmers$farmer_ID[farmers$village == "Bukowe" & farmers$farmer_ID == "F_147" & farmers$X_id == "77386603"] <- "F_142"
farmers$farmer_ID[farmers$village == "Bukowe" & farmers$farmer_ID == "F_148" & farmers$X_id == "77386580"] <- "F_144"

##in Mpungwe, 3 IDs are duplicated ( F_2148 F_2149 F_2150 ) but all 10 observations are there
## the duplicates are actually from Bulondo, where only 7 were recorded
farmers$farmer_ID[farmers$village == "Mpungwe" & farmers$farmer_ID == "F_2148" & farmers$X_id == "77127824"] <- "F_3231"
farmers$farmer_ID[farmers$village == "Mpungwe" & farmers$farmer_ID == "F_2149" & farmers$X_id == "77127603"] <- "F_3232"
farmers$farmer_ID[farmers$village == "Mpungwe" & farmers$farmer_ID == "F_2150" & farmers$X_id == "77127627"] <- "F_3234"
farmers$village[farmers$farmer_ID %in% c("F_3231","F_3232","F_3234")] <- "Bulondo"
farmers$sub[farmers$farmer_ID %in% c("F_3231","F_3232","F_3234")] <- "Mpungwe"
farmers$catchID[farmers$farmer_ID %in% c("F_3231","F_3232","F_3234")] <- 111
farmers$farmer_ID[farmers$village == "Kyeeya" & farmers$farmer_ID == "F_265" & farmers$X_id == "77145372"] <- "F_2475"
farmers$village[farmers$farmer_ID  == "F_2475"] <- "Isingo"

farmers$farmer_ID[farmers$village == "Bupala" & farmers$farmer_ID == "F_2316" & farmers$X_id == "77386635"] <- "F_3026"
farmers$village[farmers$farmer_ID  == "F_3026"] <- "Kalalu (Bugweri)"
farmers$sub[farmers$farmer_ID  == "F_3026"] <- "Idudi"
farmers$district[farmers$farmer_ID  == "F_3026"] <- "Bugweri"
farmers$catchID[farmers$farmer_ID  == "F_3026"] <- 9

farmers$farmer_ID[farmers$village == "Buwunga" & farmers$farmer_ID == "F_1892" & farmers$X_id == "77520637"] <- "F_1334"
farmers$village[farmers$farmer_ID  == "F_1334"] <- "Nakabale (Kaliro)"
farmers$sub[farmers$farmer_ID  == "F_1334"] <- "Kaliro Tc"
farmers$district[farmers$farmer_ID  == "F_1334"] <- "Kaliro"
farmers$catchID[farmers$farmer_ID  == "F_1334"] <- 61

farmers$farmer_ID[farmers$village == "Mutelele" & farmers$farmer_ID == "F_1965" & farmers$X_id == "77576784"] <- "F_1955"
farmers$village[farmers$farmer_ID  == "F_1955"] <- "Bululu"
farmers$sub[farmers$farmer_ID  == "F_1955"] <- "Muterere"
farmers$district[farmers$farmer_ID  == "F_1955"] <- "Bugiri"
farmers$catchID[farmers$farmer_ID  == "F_1955"] <- 98

farmers$farmer_ID[farmers$village == "Buyara" & farmers$farmer_ID == "F_1267" & farmers$X_id == "77703794"] <- "F_2587"
farmers$village[farmers$farmer_ID  == "F_2587"] <- "Buwagi"
farmers$sub[farmers$farmer_ID  == "F_2587"] <- "Budondo"
farmers$district[farmers$farmer_ID  == "F_2587"] <- "Jinja"
farmers$catchID[farmers$farmer_ID  == "F_2587"] <- 1


#this village was visited twice - I am removing it but maybe it can still be saved?
farmers <- farmers[!(farmers$village == "Namusambya" & (farmers$X_id %in% c("77386998",
"77387000",
"77387002",
"77387005",
"77387006",
"77387009",
"77387010",
"77387012",
"77387014",
"77387017"))),]

farmers <- farmers[!(farmers$village == "Bumoozi" &  farmers$farmer_ID == "F_736" & farmers$X_id == "77352290"),]
farmers <- farmers[!(farmers$village == "Nawasenga" &  farmers$farmer_ID == "F_639" & farmers$X_id == "77352536"),]
farmers <- farmers[!(farmers$village == "Nsozi" &  farmers$farmer_ID == "F_1109" & farmers$X_id == "77386760"),]
farmers <- farmers[!(farmers$village == "Bukola" &  farmers$farmer_ID == "F_1016" & farmers$X_id == "77409670"),]
farmers <- farmers[!(farmers$village == "Nabinyonyi" &  farmers$farmer_ID == "F_49" & farmers$X_id == "77409793"),]
farmers <- farmers[!(farmers$village == "Izira" &  farmers$farmer_ID == "F_721" & farmers$X_id == "77520701"),]
farmers <- farmers[!(farmers$village == "Kigusa" &  farmers$farmer_ID == "F_2641" & farmers$X_id == "77576761"),]
farmers <- farmers[!(farmers$village == "Mayuge Nilo" &  farmers$farmer_ID == "F_2740" & farmers$X_id == "77576821"),]
farmers <- farmers[!(farmers$village == "Nkoone" &  farmers$farmer_ID == "F_1475" & farmers$X_id == "77576896"),]
farmers <- farmers[!(farmers$village == "Kigusa" &  farmers$farmer_ID == "F_2644" & farmers$X_id == "77577060"),]
farmers <- farmers[!(farmers$village == "Budhaya" &  farmers$farmer_ID == "F_1097" & farmers$X_id == "77577117"),]
farmers <- farmers[!(farmers$village == "Maweito" &  farmers$farmer_ID == "F_3247" & farmers$X_id == "77706258"),]


farmers$farmer_ID[duplicated(farmers$farmer_ID)] 

test <- farmers[farmers$village == "Buyara",c("Check2.check.maize._gps_latitude", "Check2.check.maize._gps_longitude","farmer_ID")]
names(test) <- c("lat","long", "ID")
m <-  leaflet() %>% setView(lat = 0.65, lng = 33.62, zoom=11)  %>%  addTiles(group="OSM") %>% addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",  group="Google", attribution = 'Google')  %>% addProviderTiles(providers$OpenTopoMap, group="Topography") %>% addCircleMarkers(data=test, lng=~as.numeric(as.character(long)), lat=~as.numeric(as.character(lat)),radius= 8, popup = ~as.character(ID))   %>%  addLayersControl(baseGroups=c('OSM','Google','Topography')) 
#use this to investigate duplicates:
#farmers[farmers$farmer_ID == "F_1267",c("village")]
#table(farmers$farmer_ID[farmers$village == "Buwagi"])
#farmers[farmers$village == "Buyara" & farmers$farmer_ID == "F_1267",] 

#merge input dealers to which farmers (villages) are conntected - needed for clustering


path2 <- strsplit(path, "/farmer")[[1]][1]



shops <- read.csv(paste(path2,"agro_input/raw/villages_edited_final.csv", sep="/"))

farmers <- merge(farmers,shops[c("shop_ID", "district","sub","sampling_village")], by.x=c("district","sub","village"), by.y=c("district","sub","sampling_village") )



## drop location, names and contact details
to_drop <- c(
"start",                                        
"end",                                          
"deviceid",                                     
"simserial",                                    
"phonenumber",                                  
"subscriberid",                                 
"enumerator",                                   
"district",                                     
"sub",                                          
"village",                                                              
"Check2.check.consent",                         
"Check2.check.sig_consent",                     
"Check2.check.maize.q5",                        
 "Check2.check.maize.phone",                     
 "Check2.check.maize.phone2",   
 "Check2.check.maize.gps",                       
"Check2.check.maize._gps_latitude",             
"Check2.check.maize._gps_longitude",            
"Check2.check.maize._gps_altitude",             
"Check2.check.maize._gps_precision",            
"meta.instanceID",                              
"X_id",                                         
"X_uuid",                                       
"X_submission_time" ,                           
"X_date_modified",                              
"X_tags",                                      
"X_notes",                                      
"X_version",                                    
"X_duration",                                   
"X_submitted_by",                               
"X_total_media",                                
"X_media_count",                                
"X_media_all_received",                         
"X_xform_id")            
farmers <- farmers[ , !(names(farmers) %in% to_drop)]
 
#remove ID info for input dealers in rating module
to_drop <- c( paste(paste("Check2.check.maize.clear.shops",1:max(as.numeric(farmers$Check2.check.maize.clear.shops_count), na.rm=T),sep = "."),"calc_biz",sep=".."), 
paste(paste("Check2.check.maize.clear.shops",1:max(as.numeric(farmers$Check2.check.maize.clear.shops_count), na.rm=T),sep = "."),"dealer_name",sep=".."), 
paste(paste("Check2.check.maize.clear.shops",1:max(as.numeric(farmers$Check2.check.maize.clear.shops_count), na.rm=T),sep = "."),"nickname",sep=".."), 
paste(paste("Check2.check.maize.clear.shops",1:max(as.numeric(farmers$Check2.check.maize.clear.shops_count), na.rm=T),sep = "."),"market_name",sep=".."), 
paste(paste("Check2.check.maize.clear.shops",1:max(as.numeric(farmers$Check2.check.maize.clear.shops_count), na.rm=T),sep = "."),"eye",sep=".."), 
paste(paste("Check2.check.maize.clear.shops",1:max(as.numeric(farmers$Check2.check.maize.clear.shops_count), na.rm=T),sep = "."),"pic",sep=".."))
farmers <- farmers[ , !(names(farmers) %in% to_drop)]

#remove names for plot in random plot selection module
to_drop <-  c( paste(paste("Check2.check.maize.plot",1:max(as.numeric(farmers$Check2.check.maize.plot_count), na.rm=T),sep = "."),"plot_num",sep=".."), 
paste(paste("Check2.check.maize.plot",1:max(as.numeric(farmers$Check2.check.maize.plot_count), na.rm=T),sep = "."),"q28",sep=".."),
"Check2.check.maize.plot_calc1","Check2.check.maize.plot_calc2","Check2.check.maize.plot_select","Check2.check.maize.plot_select_name","Check2.check.maize.order","Check2.check.maize.clear.placeholder"
)
farmers <- farmers[ , !(names(farmers) %in% to_drop)]


in_long <- melt(farmers, id.vars = c("farmer_ID"))

###extract rating data from this dataset and save in a separate file called ratings_dyads
all_1 <- in_long[in_long$variable == "Check2.check.maize.clear.shops.1..calc_name",]
for (i in c(
"Check2.check.maize.clear.shops.1..q64",       
"Check2.check.maize.clear.shops.1..q65",       
"Check2.check.maize.clear.shops.1..q65b",      
"Check2.check.maize.clear.shops.1..q66",       
"Check2.check.maize.clear.shops.1..q67",       
"Check2.check.maize.clear.shops.1..q68a",      
"Check2.check.maize.clear.shops.1..q68b",      
"Check2.check.maize.clear.shops.1..q68c",      
"Check2.check.maize.clear.shops.1..q68d",      
"Check2.check.maize.clear.shops.1..q68e",      
"Check2.check.maize.clear.shops.1..q68f",      
"Check2.check.maize.clear.shops.1..q69a",      
"Check2.check.maize.clear.shops.1..q69b",      
"Check2.check.maize.clear.shops.1..q69c",      
"Check2.check.maize.clear.shops.1..q69d",      
"Check2.check.maize.clear.shops.1..q69e",      
"Check2.check.maize.clear.shops.1..q69f",      
"Check2.check.maize.clear.shops.1..q70",       
"Check2.check.maize.clear.shops.1..q71",       
"Check2.check.maize.clear.shops.1..q72",       
"Check2.check.maize.clear.shops.1..q73",       
"Check2.check.maize.clear.shops.1..q74",       
"Check2.check.maize.clear.shops.1..q75",       
"Check2.check.maize.clear.shops.1..q76")) {

all_1 <- cbind(all_1,in_long[in_long$variable == i,])
}
all_1 <- all_1[,c(1,seq(3,75, by=3))]

names(all_1) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","bought_last_season","general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating", "seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant")

### we need to do this 18 times
all_2 <- in_long[in_long$variable == "Check2.check.maize.clear.shops.2..calc_name",]
for (i in c(
"Check2.check.maize.clear.shops.2..q64",       
"Check2.check.maize.clear.shops.2..q65",       
"Check2.check.maize.clear.shops.2..q65b",      
"Check2.check.maize.clear.shops.2..q66",       
"Check2.check.maize.clear.shops.2..q67",       
"Check2.check.maize.clear.shops.2..q68a",      
"Check2.check.maize.clear.shops.2..q68b",      
"Check2.check.maize.clear.shops.2..q68c",      
"Check2.check.maize.clear.shops.2..q68d",      
"Check2.check.maize.clear.shops.2..q68e",      
"Check2.check.maize.clear.shops.2..q68f",      
"Check2.check.maize.clear.shops.2..q69a",      
"Check2.check.maize.clear.shops.2..q69b",      
"Check2.check.maize.clear.shops.2..q69c",      
"Check2.check.maize.clear.shops.2..q69d",      
"Check2.check.maize.clear.shops.2..q69e",      
"Check2.check.maize.clear.shops.2..q69f",      
"Check2.check.maize.clear.shops.2..q70",       
"Check2.check.maize.clear.shops.2..q71",       
"Check2.check.maize.clear.shops.2..q72",       
"Check2.check.maize.clear.shops.2..q73",       
"Check2.check.maize.clear.shops.2..q74",       
"Check2.check.maize.clear.shops.2..q75",       
"Check2.check.maize.clear.shops.2..q76")) {

all_2 <- cbind(all_2,in_long[in_long$variable == i,])
}
all_2 <- all_2[,c(1,seq(3,75, by=3))]

names(all_2) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","bought_last_season","general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating", "seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant")

### we need to do this 18 times
all_3 <- in_long[in_long$variable == "Check2.check.maize.clear.shops.3..calc_name",]
for (i in c(
"Check2.check.maize.clear.shops.3..q64",       
"Check2.check.maize.clear.shops.3..q65",       
"Check2.check.maize.clear.shops.3..q65b",      
"Check2.check.maize.clear.shops.3..q66",       
"Check2.check.maize.clear.shops.3..q67",       
"Check2.check.maize.clear.shops.3..q68a",      
"Check2.check.maize.clear.shops.3..q68b",      
"Check2.check.maize.clear.shops.3..q68c",      
"Check2.check.maize.clear.shops.3..q68d",      
"Check2.check.maize.clear.shops.3..q68e",      
"Check2.check.maize.clear.shops.3..q68f",      
"Check2.check.maize.clear.shops.3..q69a",      
"Check2.check.maize.clear.shops.3..q69b",      
"Check2.check.maize.clear.shops.3..q69c",      
"Check2.check.maize.clear.shops.3..q69d",      
"Check2.check.maize.clear.shops.3..q69e",      
"Check2.check.maize.clear.shops.3..q69f",      
"Check2.check.maize.clear.shops.3..q70",       
"Check2.check.maize.clear.shops.3..q71",       
"Check2.check.maize.clear.shops.3..q72",       
"Check2.check.maize.clear.shops.3..q73",       
"Check2.check.maize.clear.shops.3..q74",       
"Check2.check.maize.clear.shops.3..q75",       
"Check2.check.maize.clear.shops.3..q76")) {

all_3 <- cbind(all_3,in_long[in_long$variable == i,])
}
all_3 <- all_3[,c(1,seq(3,75, by=3))]

names(all_3) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","bought_last_season","general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating", "seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant")

### we need to do this 18 times
all_4 <- in_long[in_long$variable == "Check2.check.maize.clear.shops.4..calc_name",]
for (i in c(
"Check2.check.maize.clear.shops.4..q64",       
"Check2.check.maize.clear.shops.4..q65",       
"Check2.check.maize.clear.shops.4..q65b",      
"Check2.check.maize.clear.shops.4..q66",       
"Check2.check.maize.clear.shops.4..q67",       
"Check2.check.maize.clear.shops.4..q68a",      
"Check2.check.maize.clear.shops.4..q68b",      
"Check2.check.maize.clear.shops.4..q68c",      
"Check2.check.maize.clear.shops.4..q68d",      
"Check2.check.maize.clear.shops.4..q68e",      
"Check2.check.maize.clear.shops.4..q68f",      
"Check2.check.maize.clear.shops.4..q69a",      
"Check2.check.maize.clear.shops.4..q69b",      
"Check2.check.maize.clear.shops.4..q69c",      
"Check2.check.maize.clear.shops.4..q69d",      
"Check2.check.maize.clear.shops.4..q69e",      
"Check2.check.maize.clear.shops.4..q69f",      
"Check2.check.maize.clear.shops.4..q70",       
"Check2.check.maize.clear.shops.4..q71",       
"Check2.check.maize.clear.shops.4..q72",       
"Check2.check.maize.clear.shops.4..q73",       
"Check2.check.maize.clear.shops.4..q74",       
"Check2.check.maize.clear.shops.4..q75",       
"Check2.check.maize.clear.shops.4..q76")) {

all_4 <- cbind(all_4,in_long[in_long$variable == i,])
}
all_4 <- all_4[,c(1,seq(3,75, by=3))]

names(all_4) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","bought_last_season","general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating", "seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant")

### we need to do this 18 times
all_5 <- in_long[in_long$variable == "Check2.check.maize.clear.shops.5..calc_name",]
for (i in c(
"Check2.check.maize.clear.shops.5..q64",       
"Check2.check.maize.clear.shops.5..q65",       
"Check2.check.maize.clear.shops.5..q65b",      
"Check2.check.maize.clear.shops.5..q66",       
"Check2.check.maize.clear.shops.5..q67",       
"Check2.check.maize.clear.shops.5..q68a",      
"Check2.check.maize.clear.shops.5..q68b",      
"Check2.check.maize.clear.shops.5..q68c",      
"Check2.check.maize.clear.shops.5..q68d",      
"Check2.check.maize.clear.shops.5..q68e",      
"Check2.check.maize.clear.shops.5..q68f",      
"Check2.check.maize.clear.shops.5..q69a",      
"Check2.check.maize.clear.shops.5..q69b",      
"Check2.check.maize.clear.shops.5..q69c",      
"Check2.check.maize.clear.shops.5..q69d",      
"Check2.check.maize.clear.shops.5..q69e",      
"Check2.check.maize.clear.shops.5..q69f",      
"Check2.check.maize.clear.shops.5..q70",       
"Check2.check.maize.clear.shops.5..q71",       
"Check2.check.maize.clear.shops.5..q72",       
"Check2.check.maize.clear.shops.5..q73",       
"Check2.check.maize.clear.shops.5..q74",       
"Check2.check.maize.clear.shops.5..q75",       
"Check2.check.maize.clear.shops.5..q76")) {

all_5 <- cbind(all_5,in_long[in_long$variable == i,])
}
all_5 <- all_5[,c(1,seq(3,75, by=3))]

names(all_5) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","bought_last_season","general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating", "seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant")

### we need to do this 18 times
all_6 <- in_long[in_long$variable == "Check2.check.maize.clear.shops.6..calc_name",]
for (i in c(
"Check2.check.maize.clear.shops.6..q64",       
"Check2.check.maize.clear.shops.6..q65",       
"Check2.check.maize.clear.shops.6..q65b",      
"Check2.check.maize.clear.shops.6..q66",       
"Check2.check.maize.clear.shops.6..q67",       
"Check2.check.maize.clear.shops.6..q68a",      
"Check2.check.maize.clear.shops.6..q68b",      
"Check2.check.maize.clear.shops.6..q68c",      
"Check2.check.maize.clear.shops.6..q68d",      
"Check2.check.maize.clear.shops.6..q68e",      
"Check2.check.maize.clear.shops.6..q68f",      
"Check2.check.maize.clear.shops.6..q69a",      
"Check2.check.maize.clear.shops.6..q69b",      
"Check2.check.maize.clear.shops.6..q69c",      
"Check2.check.maize.clear.shops.6..q69d",      
"Check2.check.maize.clear.shops.6..q69e",      
"Check2.check.maize.clear.shops.6..q69f",      
"Check2.check.maize.clear.shops.6..q70",       
"Check2.check.maize.clear.shops.6..q71",       
"Check2.check.maize.clear.shops.6..q72",       
"Check2.check.maize.clear.shops.6..q73",       
"Check2.check.maize.clear.shops.6..q74",       
"Check2.check.maize.clear.shops.6..q75",       
"Check2.check.maize.clear.shops.6..q76")) {

all_6 <- cbind(all_6,in_long[in_long$variable == i,])
}
all_6 <- all_6[,c(1,seq(3,75, by=3))]

names(all_6) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","bought_last_season","general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating", "seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant")

### we need to do this 18 times
all_7 <- in_long[in_long$variable == "Check2.check.maize.clear.shops.7..calc_name",]
for (i in c(
"Check2.check.maize.clear.shops.7..q64",       
"Check2.check.maize.clear.shops.7..q65",       
"Check2.check.maize.clear.shops.7..q65b",      
"Check2.check.maize.clear.shops.7..q66",       
"Check2.check.maize.clear.shops.7..q67",       
"Check2.check.maize.clear.shops.7..q68a",      
"Check2.check.maize.clear.shops.7..q68b",      
"Check2.check.maize.clear.shops.7..q68c",      
"Check2.check.maize.clear.shops.7..q68d",      
"Check2.check.maize.clear.shops.7..q68e",      
"Check2.check.maize.clear.shops.7..q68f",      
"Check2.check.maize.clear.shops.7..q69a",      
"Check2.check.maize.clear.shops.7..q69b",      
"Check2.check.maize.clear.shops.7..q69c",      
"Check2.check.maize.clear.shops.7..q69d",      
"Check2.check.maize.clear.shops.7..q69e",      
"Check2.check.maize.clear.shops.7..q69f",      
"Check2.check.maize.clear.shops.7..q70",       
"Check2.check.maize.clear.shops.7..q71",       
"Check2.check.maize.clear.shops.7..q72",       
"Check2.check.maize.clear.shops.7..q73",       
"Check2.check.maize.clear.shops.7..q74",       
"Check2.check.maize.clear.shops.7..q75",       
"Check2.check.maize.clear.shops.7..q76")) {

all_7 <- cbind(all_7,in_long[in_long$variable == i,])
}
all_7 <- all_7[,c(1,seq(3,75, by=3))]

names(all_7) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","bought_last_season","general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating", "seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant")

### we need to do this 18 times
all_8 <- in_long[in_long$variable == "Check2.check.maize.clear.shops.8..calc_name",]
for (i in c(
"Check2.check.maize.clear.shops.8..q64",       
"Check2.check.maize.clear.shops.8..q65",       
"Check2.check.maize.clear.shops.8..q65b",      
"Check2.check.maize.clear.shops.8..q66",       
"Check2.check.maize.clear.shops.8..q67",       
"Check2.check.maize.clear.shops.8..q68a",      
"Check2.check.maize.clear.shops.8..q68b",      
"Check2.check.maize.clear.shops.8..q68c",      
"Check2.check.maize.clear.shops.8..q68d",      
"Check2.check.maize.clear.shops.8..q68e",      
"Check2.check.maize.clear.shops.8..q68f",      
"Check2.check.maize.clear.shops.8..q69a",      
"Check2.check.maize.clear.shops.8..q69b",      
"Check2.check.maize.clear.shops.8..q69c",      
"Check2.check.maize.clear.shops.8..q69d",      
"Check2.check.maize.clear.shops.8..q69e",      
"Check2.check.maize.clear.shops.8..q69f",      
"Check2.check.maize.clear.shops.8..q70",       
"Check2.check.maize.clear.shops.8..q71",       
"Check2.check.maize.clear.shops.8..q72",       
"Check2.check.maize.clear.shops.8..q73",       
"Check2.check.maize.clear.shops.8..q74",       
"Check2.check.maize.clear.shops.8..q75",       
"Check2.check.maize.clear.shops.8..q76")) {

all_8 <- cbind(all_8,in_long[in_long$variable == i,])
}
all_8 <- all_8[,c(1,seq(3,75, by=3))]

names(all_8) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","bought_last_season","general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating", "seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant")

### we need to do this 18 times
all_9 <- in_long[in_long$variable == "Check2.check.maize.clear.shops.9..calc_name",]
for (i in c(
"Check2.check.maize.clear.shops.9..q64",       
"Check2.check.maize.clear.shops.9..q65",       
"Check2.check.maize.clear.shops.9..q65b",      
"Check2.check.maize.clear.shops.9..q66",       
"Check2.check.maize.clear.shops.9..q67",       
"Check2.check.maize.clear.shops.9..q68a",      
"Check2.check.maize.clear.shops.9..q68b",      
"Check2.check.maize.clear.shops.9..q68c",      
"Check2.check.maize.clear.shops.9..q68d",      
"Check2.check.maize.clear.shops.9..q68e",      
"Check2.check.maize.clear.shops.9..q68f",      
"Check2.check.maize.clear.shops.9..q69a",      
"Check2.check.maize.clear.shops.9..q69b",      
"Check2.check.maize.clear.shops.9..q69c",      
"Check2.check.maize.clear.shops.9..q69d",      
"Check2.check.maize.clear.shops.9..q69e",      
"Check2.check.maize.clear.shops.9..q69f",      
"Check2.check.maize.clear.shops.9..q70",       
"Check2.check.maize.clear.shops.9..q71",       
"Check2.check.maize.clear.shops.9..q72",       
"Check2.check.maize.clear.shops.9..q73",       
"Check2.check.maize.clear.shops.9..q74",       
"Check2.check.maize.clear.shops.9..q75",       
"Check2.check.maize.clear.shops.9..q76")) {

all_9 <- cbind(all_9,in_long[in_long$variable == i,])
}
all_9 <- all_9[,c(1,seq(3,75, by=3))]

names(all_9) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","bought_last_season","general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating", "seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant")

### we need to do this 18 times
all_10 <- in_long[in_long$variable == "Check2.check.maize.clear.shops.10..calc_name",]
for (i in c(
"Check2.check.maize.clear.shops.10..q64",       
"Check2.check.maize.clear.shops.10..q65",       
"Check2.check.maize.clear.shops.10..q65b",      
"Check2.check.maize.clear.shops.10..q66",       
"Check2.check.maize.clear.shops.10..q67",       
"Check2.check.maize.clear.shops.10..q68a",      
"Check2.check.maize.clear.shops.10..q68b",      
"Check2.check.maize.clear.shops.10..q68c",      
"Check2.check.maize.clear.shops.10..q68d",      
"Check2.check.maize.clear.shops.10..q68e",      
"Check2.check.maize.clear.shops.10..q68f",      
"Check2.check.maize.clear.shops.10..q69a",      
"Check2.check.maize.clear.shops.10..q69b",      
"Check2.check.maize.clear.shops.10..q69c",      
"Check2.check.maize.clear.shops.10..q69d",      
"Check2.check.maize.clear.shops.10..q69e",      
"Check2.check.maize.clear.shops.10..q69f",      
"Check2.check.maize.clear.shops.10..q70",       
"Check2.check.maize.clear.shops.10..q71",       
"Check2.check.maize.clear.shops.10..q72",       
"Check2.check.maize.clear.shops.10..q73",       
"Check2.check.maize.clear.shops.10..q74",       
"Check2.check.maize.clear.shops.10..q75",       
"Check2.check.maize.clear.shops.10..q76")) {

all_10 <- cbind(all_10,in_long[in_long$variable == i,])
}
all_10 <- all_10[,c(1,seq(3,75, by=3))]

names(all_10) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","bought_last_season","general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating", "seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant")

### we need to do this 18 times
all_11 <- in_long[in_long$variable == "Check2.check.maize.clear.shops.11..calc_name",]
for (i in c(
"Check2.check.maize.clear.shops.11..q64",       
"Check2.check.maize.clear.shops.11..q65",       
"Check2.check.maize.clear.shops.11..q65b",      
"Check2.check.maize.clear.shops.11..q66",       
"Check2.check.maize.clear.shops.11..q67",       
"Check2.check.maize.clear.shops.11..q68a",      
"Check2.check.maize.clear.shops.11..q68b",      
"Check2.check.maize.clear.shops.11..q68c",      
"Check2.check.maize.clear.shops.11..q68d",      
"Check2.check.maize.clear.shops.11..q68e",      
"Check2.check.maize.clear.shops.11..q68f",      
"Check2.check.maize.clear.shops.11..q69a",      
"Check2.check.maize.clear.shops.11..q69b",      
"Check2.check.maize.clear.shops.11..q69c",      
"Check2.check.maize.clear.shops.11..q69d",      
"Check2.check.maize.clear.shops.11..q69e",      
"Check2.check.maize.clear.shops.11..q69f",      
"Check2.check.maize.clear.shops.11..q70",       
"Check2.check.maize.clear.shops.11..q71",       
"Check2.check.maize.clear.shops.11..q72",       
"Check2.check.maize.clear.shops.11..q73",       
"Check2.check.maize.clear.shops.11..q74",       
"Check2.check.maize.clear.shops.11..q75",       
"Check2.check.maize.clear.shops.11..q76")) {

all_11 <- cbind(all_11,in_long[in_long$variable == i,])
}
all_11 <- all_11[,c(1,seq(3,75, by=3))]

names(all_11) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","bought_last_season","general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating", "seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant")

### we need to do this 18 times
all_12 <- in_long[in_long$variable == "Check2.check.maize.clear.shops.12..calc_name",]
for (i in c(
"Check2.check.maize.clear.shops.12..q64",       
"Check2.check.maize.clear.shops.12..q65",       
"Check2.check.maize.clear.shops.12..q65b",      
"Check2.check.maize.clear.shops.12..q66",       
"Check2.check.maize.clear.shops.12..q67",       
"Check2.check.maize.clear.shops.12..q68a",      
"Check2.check.maize.clear.shops.12..q68b",      
"Check2.check.maize.clear.shops.12..q68c",      
"Check2.check.maize.clear.shops.12..q68d",      
"Check2.check.maize.clear.shops.12..q68e",      
"Check2.check.maize.clear.shops.12..q68f",      
"Check2.check.maize.clear.shops.12..q69a",      
"Check2.check.maize.clear.shops.12..q69b",      
"Check2.check.maize.clear.shops.12..q69c",      
"Check2.check.maize.clear.shops.12..q69d",      
"Check2.check.maize.clear.shops.12..q69e",      
"Check2.check.maize.clear.shops.12..q69f",      
"Check2.check.maize.clear.shops.12..q70",       
"Check2.check.maize.clear.shops.12..q71",       
"Check2.check.maize.clear.shops.12..q72",       
"Check2.check.maize.clear.shops.12..q73",       
"Check2.check.maize.clear.shops.12..q74",       
"Check2.check.maize.clear.shops.12..q75",       
"Check2.check.maize.clear.shops.12..q76")) {

all_12 <- cbind(all_12,in_long[in_long$variable == i,])
}
all_12 <- all_12[,c(1,seq(3,75, by=3))]

names(all_12) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","bought_last_season","general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating", "seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant")

### we need to do this 18 times
all_13 <- in_long[in_long$variable == "Check2.check.maize.clear.shops.13..calc_name",]
for (i in c(
"Check2.check.maize.clear.shops.13..q64",       
"Check2.check.maize.clear.shops.13..q65",       
"Check2.check.maize.clear.shops.13..q65b",      
"Check2.check.maize.clear.shops.13..q66",       
"Check2.check.maize.clear.shops.13..q67",       
"Check2.check.maize.clear.shops.13..q68a",      
"Check2.check.maize.clear.shops.13..q68b",      
"Check2.check.maize.clear.shops.13..q68c",      
"Check2.check.maize.clear.shops.13..q68d",      
"Check2.check.maize.clear.shops.13..q68e",      
"Check2.check.maize.clear.shops.13..q68f",      
"Check2.check.maize.clear.shops.13..q69a",      
"Check2.check.maize.clear.shops.13..q69b",      
"Check2.check.maize.clear.shops.13..q69c",      
"Check2.check.maize.clear.shops.13..q69d",      
"Check2.check.maize.clear.shops.13..q69e",      
"Check2.check.maize.clear.shops.13..q69f",      
"Check2.check.maize.clear.shops.13..q70",       
"Check2.check.maize.clear.shops.13..q71",       
"Check2.check.maize.clear.shops.13..q72",       
"Check2.check.maize.clear.shops.13..q73",       
"Check2.check.maize.clear.shops.13..q74",       
"Check2.check.maize.clear.shops.13..q75",       
"Check2.check.maize.clear.shops.13..q76")) {

all_13 <- cbind(all_13,in_long[in_long$variable == i,])
}
all_13 <- all_13[,c(1,seq(3,75, by=3))]

names(all_13) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","bought_last_season","general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating", "seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant")

### we need to do this 18 times
all_14 <- in_long[in_long$variable == "Check2.check.maize.clear.shops.14..calc_name",]
for (i in c(
"Check2.check.maize.clear.shops.14..q64",       
"Check2.check.maize.clear.shops.14..q65",       
"Check2.check.maize.clear.shops.14..q65b",      
"Check2.check.maize.clear.shops.14..q66",       
"Check2.check.maize.clear.shops.14..q67",       
"Check2.check.maize.clear.shops.14..q68a",      
"Check2.check.maize.clear.shops.14..q68b",      
"Check2.check.maize.clear.shops.14..q68c",      
"Check2.check.maize.clear.shops.14..q68d",      
"Check2.check.maize.clear.shops.14..q68e",      
"Check2.check.maize.clear.shops.14..q68f",      
"Check2.check.maize.clear.shops.14..q69a",      
"Check2.check.maize.clear.shops.14..q69b",      
"Check2.check.maize.clear.shops.14..q69c",      
"Check2.check.maize.clear.shops.14..q69d",      
"Check2.check.maize.clear.shops.14..q69e",      
"Check2.check.maize.clear.shops.14..q69f",      
"Check2.check.maize.clear.shops.14..q70",       
"Check2.check.maize.clear.shops.14..q71",       
"Check2.check.maize.clear.shops.14..q72",       
"Check2.check.maize.clear.shops.14..q73",       
"Check2.check.maize.clear.shops.14..q74",       
"Check2.check.maize.clear.shops.14..q75",       
"Check2.check.maize.clear.shops.14..q76")) {

all_14 <- cbind(all_14,in_long[in_long$variable == i,])
}
all_14 <- all_14[,c(1,seq(3,75, by=3))]

names(all_14) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","bought_last_season","general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating", "seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant")

### we need to do this 18 times
all_15 <- in_long[in_long$variable == "Check2.check.maize.clear.shops.15..calc_name",]
for (i in c(
"Check2.check.maize.clear.shops.15..q64",       
"Check2.check.maize.clear.shops.15..q65",       
"Check2.check.maize.clear.shops.15..q65b",      
"Check2.check.maize.clear.shops.15..q66",       
"Check2.check.maize.clear.shops.15..q67",       
"Check2.check.maize.clear.shops.15..q68a",      
"Check2.check.maize.clear.shops.15..q68b",      
"Check2.check.maize.clear.shops.15..q68c",      
"Check2.check.maize.clear.shops.15..q68d",      
"Check2.check.maize.clear.shops.15..q68e",      
"Check2.check.maize.clear.shops.15..q68f",      
"Check2.check.maize.clear.shops.15..q69a",      
"Check2.check.maize.clear.shops.15..q69b",      
"Check2.check.maize.clear.shops.15..q69c",      
"Check2.check.maize.clear.shops.15..q69d",      
"Check2.check.maize.clear.shops.15..q69e",      
"Check2.check.maize.clear.shops.15..q69f",      
"Check2.check.maize.clear.shops.15..q70",       
"Check2.check.maize.clear.shops.15..q71",       
"Check2.check.maize.clear.shops.15..q72",       
"Check2.check.maize.clear.shops.15..q73",       
"Check2.check.maize.clear.shops.15..q74",       
"Check2.check.maize.clear.shops.15..q75",       
"Check2.check.maize.clear.shops.15..q76")) {

all_15 <- cbind(all_15,in_long[in_long$variable == i,])
}
all_15 <- all_15[,c(1,seq(3,75, by=3))]

names(all_15) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","bought_last_season","general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating", "seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant")

### we need to do this 18 times
all_16 <- in_long[in_long$variable == "Check2.check.maize.clear.shops.16..calc_name",]
for (i in c(
"Check2.check.maize.clear.shops.16..q64",       
"Check2.check.maize.clear.shops.16..q65",       
"Check2.check.maize.clear.shops.16..q65b",      
"Check2.check.maize.clear.shops.16..q66",       
"Check2.check.maize.clear.shops.16..q67",       
"Check2.check.maize.clear.shops.16..q68a",      
"Check2.check.maize.clear.shops.16..q68b",      
"Check2.check.maize.clear.shops.16..q68c",      
"Check2.check.maize.clear.shops.16..q68d",      
"Check2.check.maize.clear.shops.16..q68e",      
"Check2.check.maize.clear.shops.16..q68f",      
"Check2.check.maize.clear.shops.16..q69a",      
"Check2.check.maize.clear.shops.16..q69b",      
"Check2.check.maize.clear.shops.16..q69c",      
"Check2.check.maize.clear.shops.16..q69d",      
"Check2.check.maize.clear.shops.16..q69e",      
"Check2.check.maize.clear.shops.16..q69f",      
"Check2.check.maize.clear.shops.16..q70",       
"Check2.check.maize.clear.shops.16..q71",       
"Check2.check.maize.clear.shops.16..q72",       
"Check2.check.maize.clear.shops.16..q73",       
"Check2.check.maize.clear.shops.16..q74",       
"Check2.check.maize.clear.shops.16..q75",       
"Check2.check.maize.clear.shops.16..q76")) {

all_16 <- cbind(all_16,in_long[in_long$variable == i,])
}
all_16 <- all_16[,c(1,seq(3,75, by=3))]

names(all_16) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","bought_last_season","general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating", "seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant")

### we need to do this 18 times
all_17 <- in_long[in_long$variable == "Check2.check.maize.clear.shops.17..calc_name",]
for (i in c(
"Check2.check.maize.clear.shops.17..q64",       
"Check2.check.maize.clear.shops.17..q65",       
"Check2.check.maize.clear.shops.17..q65b",      
"Check2.check.maize.clear.shops.17..q66",       
"Check2.check.maize.clear.shops.17..q67",       
"Check2.check.maize.clear.shops.17..q68a",      
"Check2.check.maize.clear.shops.17..q68b",      
"Check2.check.maize.clear.shops.17..q68c",      
"Check2.check.maize.clear.shops.17..q68d",      
"Check2.check.maize.clear.shops.17..q68e",      
"Check2.check.maize.clear.shops.17..q68f",      
"Check2.check.maize.clear.shops.17..q69a",      
"Check2.check.maize.clear.shops.17..q69b",      
"Check2.check.maize.clear.shops.17..q69c",      
"Check2.check.maize.clear.shops.17..q69d",      
"Check2.check.maize.clear.shops.17..q69e",      
"Check2.check.maize.clear.shops.17..q69f",      
"Check2.check.maize.clear.shops.17..q70",       
"Check2.check.maize.clear.shops.17..q71",       
"Check2.check.maize.clear.shops.17..q72",       
"Check2.check.maize.clear.shops.17..q73",       
"Check2.check.maize.clear.shops.17..q74",       
"Check2.check.maize.clear.shops.17..q75",       
"Check2.check.maize.clear.shops.17..q76")) {

all_17 <- cbind(all_17,in_long[in_long$variable == i,])
}
all_17 <- all_17[,c(1,seq(3,75, by=3))]

names(all_17) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","bought_last_season","general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating", "seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant")

### we need to do this 18 times
all_18 <- in_long[in_long$variable == "Check2.check.maize.clear.shops.18..calc_name",]
for (i in c(
"Check2.check.maize.clear.shops.18..q64",       
"Check2.check.maize.clear.shops.18..q65",       
"Check2.check.maize.clear.shops.18..q65b",      
"Check2.check.maize.clear.shops.18..q66",       
"Check2.check.maize.clear.shops.18..q67",       
"Check2.check.maize.clear.shops.18..q68a",      
"Check2.check.maize.clear.shops.18..q68b",      
"Check2.check.maize.clear.shops.18..q68c",      
"Check2.check.maize.clear.shops.18..q68d",      
"Check2.check.maize.clear.shops.18..q68e",      
"Check2.check.maize.clear.shops.18..q68f",      
"Check2.check.maize.clear.shops.18..q69a",      
"Check2.check.maize.clear.shops.18..q69b",      
"Check2.check.maize.clear.shops.18..q69c",      
"Check2.check.maize.clear.shops.18..q69d",      
"Check2.check.maize.clear.shops.18..q69e",      
"Check2.check.maize.clear.shops.18..q69f",      
"Check2.check.maize.clear.shops.18..q70",       
"Check2.check.maize.clear.shops.18..q71",       
"Check2.check.maize.clear.shops.18..q72",       
"Check2.check.maize.clear.shops.18..q73",       
"Check2.check.maize.clear.shops.18..q74",       
"Check2.check.maize.clear.shops.18..q75",       
"Check2.check.maize.clear.shops.18..q76")) {

all_18 <- cbind(all_18,in_long[in_long$variable == i,])
}
all_18 <- all_18[,c(1,seq(3,75, by=3))]

names(all_18) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","bought_last_season","general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating", "seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant")



all_shops <- rbind(all_1,all_2,all_3,all_4,all_5,all_6,all_7,all_8, all_9, all_10,all_11,all_12,all_13,all_14,all_15,all_16,all_17,all_18)
all_shops <- subset(all_shops, shop_ID!="n/a")

#remove ratings data from main base data file
for (i in 1:18) {

farmers[,
c(paste(paste("Check2.check.maize.clear.shops",i,sep="."),"calc_name",sep = ".."),
paste(paste("Check2.check.maize.clear.shops",i,sep="."),"q64",sep = ".."),       
paste(paste("Check2.check.maize.clear.shops",i,sep="."),"q65",sep = ".."),       
paste(paste("Check2.check.maize.clear.shops",i,sep="."),"q65b",sep = ".."),      
paste(paste("Check2.check.maize.clear.shops",i,sep="."),"q66",sep = ".."),       
paste(paste("Check2.check.maize.clear.shops",i,sep="."),"q67",sep = ".."),       
paste(paste("Check2.check.maize.clear.shops",i,sep="."),"q68a",sep = ".."),      
paste(paste("Check2.check.maize.clear.shops",i,sep="."),"q68b",sep = ".."),      
paste(paste("Check2.check.maize.clear.shops",i,sep="."),"q68c",sep = ".."),      
paste(paste("Check2.check.maize.clear.shops",i,sep="."),"q68d",sep = ".."),      
paste(paste("Check2.check.maize.clear.shops",i,sep="."),"q68e",sep = ".."),      
paste(paste("Check2.check.maize.clear.shops",i,sep="."),"q68f",sep = ".."),      
paste(paste("Check2.check.maize.clear.shops",i,sep="."),"q69a",sep = ".."),      
paste(paste("Check2.check.maize.clear.shops",i,sep="."),"q69b",sep = ".."),      
paste(paste("Check2.check.maize.clear.shops",i,sep="."),"q69c",sep = ".."),      
paste(paste("Check2.check.maize.clear.shops",i,sep="."),"q69d",sep = ".."),      
paste(paste("Check2.check.maize.clear.shops",i,sep="."),"q69e",sep = ".."),      
paste(paste("Check2.check.maize.clear.shops",i,sep="."),"q69f",sep = ".."),      
paste(paste("Check2.check.maize.clear.shops",i,sep="."),"q70",sep = ".."),       
paste(paste("Check2.check.maize.clear.shops",i,sep="."),"q71",sep = ".."),       
paste(paste("Check2.check.maize.clear.shops",i,sep="."),"q72",sep = ".."),       
paste(paste("Check2.check.maize.clear.shops",i,sep="."),"q73",sep = ".."),       
paste(paste("Check2.check.maize.clear.shops",i,sep="."),"q74",sep = ".."),       
paste(paste("Check2.check.maize.clear.shops",i,sep="."),"q75",sep = ".."),       
paste(paste("Check2.check.maize.clear.shops",i,sep="."),"q76",sep = ".."),
paste(paste("Check2.check.maize.clear.shops",i,sep="."),"pos",sep = ".."))] <- NULL
}






#write public dataset
path <- strsplit(path, "/raw")[[1]]
write.csv(farmers,paste(path,"public/baseline_farmers.csv", sep="/"), row.names=FALSE)
write.csv(all_shops,paste(path,"public/rating_dyads.csv", sep="/"), row.names=FALSE)


#number of customer reviews
reviews <- data.frame(cbind(tapply(as.numeric(all_shops$general_rating), all_shops$shop_ID,mean,na.rm=TRUE),
tapply(as.numeric(all_shops$location_rating), all_shops$shop_ID,mean,na.rm=TRUE),
tapply(as.numeric(all_shops$price_rating), all_shops$shop_ID,mean,na.rm=TRUE),
tapply(as.numeric(all_shops$quality_rating), all_shops$shop_ID,mean,na.rm=TRUE),
tapply(as.numeric(all_shops$stock_rating), all_shops$shop_ID,mean,na.rm=TRUE),
tapply(as.numeric(all_shops$reputation_rating), all_shops$shop_ID,mean,na.rm=TRUE),
tapply(all_shops$bought_at_dealer=="Yes" | all_shops$knows_other_customer=="Yes", all_shops$shop_ID,sum)))
names(reviews) <- c("general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","nr_reviews")

reviews$shop_ID <- rownames(reviews)

path <- strsplit(path, "/farmer")[[1]]
write.csv(reviews, paste(path, "agro_input/raw/shiny_app/reviews.csv",sep="/"), row.names=FALSE)


###update map
### change directory and then
setwd(paste(path, "agro_input/raw/shiny_app/",sep="/"))
rsconnect::deployApp(forceUpdate=TRUE)




