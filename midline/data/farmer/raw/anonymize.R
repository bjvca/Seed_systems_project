### anonymize midline data
#run in Seed_systems_project/midline/data/farmer/raw
#then run rating_calculation
#then run prepare_clearing
#ten run
library(reshape2)

mid <- read.csv("Farmer_MidlineV1_2022_01_22_09_30_12_083851.csv")

##investigate duplicates here!!!!!!

#also remove district, subcounty and village here -- we will use the encoded versions from this from the baseline data
to_drop <- c("start","end","deviceid","simserial","phonenumber","subscriberid","enumerator", "date","district","sub","village","hh_name","enumerator_base","phone1","phone2","farmer_name","q1","chess","new_phone","q3")             
mid <- mid[ , !(names(mid) %in% to_drop)]

to_drop <- c("check.consent","check.sig_consent","check.maize.q5","check.maize.phone1x","check.maize.phone2x","check.maize.q13")

mid <- mid[ , !(names(mid) %in% to_drop)]

#remove names for plot in random plot selection module
to_drop <- c("check.maize.plot.1..plot_num","check.maize.plot_count","check.maize.plot.1..q28","check.maize.plot.2..plot_num","check.maize.plot.2..q28","check.maize.plot.3..plot_num","check.maize.plot.3..q28","check.maize.plot_calc1","check.maize.plot_calc2","check.maize.plot_select_name","check.maize.order")

mid <- mid[ , !(names(mid) %in% to_drop)]

to_drop <- c("check.maize.clear.placeholder","check.maize.clear.shops_count")

mid <- mid[ , !(names(mid) %in% to_drop)]


## drop location, names and contact details
to_drop <- c("check.maize.video_shown",                
"check.maize.gps",                        
"check.maize._gps_latitude",              
"check.maize._gps_longitude",             
"check.maize._gps_altitude",              
"check.maize._gps_precision",            
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
mid <- mid[ , !(names(mid) %in% to_drop)]
 
#remove ID info for input dealers in rating module
to_drop <- c( paste(paste("check.maize.clear.shops",1:18,sep = "."),"calc_biz",sep=".."), 
paste(paste("check.maize.clear.shops",1:18,sep = "."),"dealer_name",sep=".."), 
paste(paste("check.maize.clear.shops",1:18,sep = "."),"nickname",sep=".."), 
paste(paste("check.maize.clear.shops",1:18,sep = "."),"market_name",sep=".."), 
paste(paste("check.maize.clear.shops",1:18,sep = "."),"eye",sep=".."), 
paste(paste("check.maize.clear.shops",1:18,sep = "."),"pic",sep=".."))
mid <- mid[ , !(names(mid) %in% to_drop)]

in_long <- melt(mid, id.vars = c("farmer_ID"))


###extract rating data from this dataset and save in a separate file called ratings_dyads
all_1 <- in_long[in_long$variable == "check.maize.clear.shops.1..calc_name",]
for (i in c(
"check.maize.clear.shops.1..q64",       
"check.maize.clear.shops.1..q65",       
"check.maize.clear.shops.1..q65b",      
"check.maize.clear.shops.1..q66",       
"check.maize.clear.shops.1..q67",
"check.maize.clear.shops.1..q67a",
"check.maize.clear.shops.1..q67b",             
"check.maize.clear.shops.1..q68a",      
"check.maize.clear.shops.1..q68b",      
"check.maize.clear.shops.1..q68c",      
"check.maize.clear.shops.1..q68d",      
"check.maize.clear.shops.1..q68e",      
"check.maize.clear.shops.1..q68f",      
"check.maize.clear.shops.1..q69a",      
"check.maize.clear.shops.1..q69b",      
"check.maize.clear.shops.1..q69c",      
"check.maize.clear.shops.1..q69d",      
"check.maize.clear.shops.1..q69e",      
"check.maize.clear.shops.1..q69f",      
"check.maize.clear.shops.1..q70",       
"check.maize.clear.shops.1..q71",       
"check.maize.clear.shops.1..q72",       
"check.maize.clear.shops.1..q73",       
"check.maize.clear.shops.1..q74",       
"check.maize.clear.shops.1..q75",       
"check.maize.clear.shops.1..q76")) {

all_1 <- cbind(all_1,in_long[in_long$variable == i,])
}
all_1 <- all_1[,c(1,seq(3,81, by=3))]

names(all_1) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","bought_last_season","knows_SA_rating","SA_rating","general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating", "seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant")

#we need to do this 18 times
#for 2
all_2 <- in_long[in_long$variable == "check.maize.clear.shops.2..calc_name",]
for (i in c(
"check.maize.clear.shops.2..q64",       
"check.maize.clear.shops.2..q65",       
"check.maize.clear.shops.2..q65b",      
"check.maize.clear.shops.2..q66",       
"check.maize.clear.shops.2..q67",
"check.maize.clear.shops.2..q67a",
"check.maize.clear.shops.2..q67b",             
"check.maize.clear.shops.2..q68a",      
"check.maize.clear.shops.2..q68b",      
"check.maize.clear.shops.2..q68c",      
"check.maize.clear.shops.2..q68d",      
"check.maize.clear.shops.2..q68e",      
"check.maize.clear.shops.2..q68f",      
"check.maize.clear.shops.2..q69a",      
"check.maize.clear.shops.2..q69b",      
"check.maize.clear.shops.2..q69c",      
"check.maize.clear.shops.2..q69d",      
"check.maize.clear.shops.2..q69e",      
"check.maize.clear.shops.2..q69f",      
"check.maize.clear.shops.2..q70",       
"check.maize.clear.shops.2..q71",       
"check.maize.clear.shops.2..q72",       
"check.maize.clear.shops.2..q73",       
"check.maize.clear.shops.2..q74",       
"check.maize.clear.shops.2..q75",       
"check.maize.clear.shops.2..q76")) {

all_2 <- cbind(all_2,in_long[in_long$variable == i,])
}
all_2 <- all_2[,c(1,seq(3,81, by=3))]

names(all_2) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","bought_last_season","knows_SA_rating","SA_rating","general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating", "seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant")

#for 3

all_3 <- in_long[in_long$variable == "check.maize.clear.shops.3..calc_name",]
for (i in c(
"check.maize.clear.shops.3..q64",       
"check.maize.clear.shops.3..q65",       
"check.maize.clear.shops.3..q65b",      
"check.maize.clear.shops.3..q66",       
"check.maize.clear.shops.3..q67",
"check.maize.clear.shops.3..q67a",
"check.maize.clear.shops.3..q67b",             
"check.maize.clear.shops.3..q68a",      
"check.maize.clear.shops.3..q68b",      
"check.maize.clear.shops.3..q68c",      
"check.maize.clear.shops.3..q68d",      
"check.maize.clear.shops.3..q68e",      
"check.maize.clear.shops.3..q68f",      
"check.maize.clear.shops.3..q69a",      
"check.maize.clear.shops.3..q69b",      
"check.maize.clear.shops.3..q69c",      
"check.maize.clear.shops.3..q69d",      
"check.maize.clear.shops.3..q69e",      
"check.maize.clear.shops.3..q69f",      
"check.maize.clear.shops.3..q70",       
"check.maize.clear.shops.3..q71",       
"check.maize.clear.shops.3..q72",       
"check.maize.clear.shops.3..q73",       
"check.maize.clear.shops.3..q74",       
"check.maize.clear.shops.3..q75",       
"check.maize.clear.shops.3..q76")) {

all_3 <- cbind(all_3,in_long[in_long$variable == i,])
}
all_3 <- all_3[,c(1,seq(3,81, by=3))]

names(all_3) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","bought_last_season","knows_SA_rating","SA_rating","general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating", "seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant")

#for 4

all_4 <- in_long[in_long$variable == "check.maize.clear.shops.4..calc_name",]
for (i in c(
"check.maize.clear.shops.4..q64",       
"check.maize.clear.shops.4..q65",       
"check.maize.clear.shops.4..q65b",      
"check.maize.clear.shops.4..q66",       
"check.maize.clear.shops.4..q67",
"check.maize.clear.shops.4..q67a",
"check.maize.clear.shops.4..q67b",             
"check.maize.clear.shops.4..q68a",      
"check.maize.clear.shops.4..q68b",      
"check.maize.clear.shops.4..q68c",      
"check.maize.clear.shops.4..q68d",      
"check.maize.clear.shops.4..q68e",      
"check.maize.clear.shops.4..q68f",      
"check.maize.clear.shops.4..q69a",      
"check.maize.clear.shops.4..q69b",      
"check.maize.clear.shops.4..q69c",      
"check.maize.clear.shops.4..q69d",      
"check.maize.clear.shops.4..q69e",      
"check.maize.clear.shops.4..q69f",      
"check.maize.clear.shops.4..q70",       
"check.maize.clear.shops.4..q71",       
"check.maize.clear.shops.4..q72",       
"check.maize.clear.shops.4..q73",       
"check.maize.clear.shops.4..q74",       
"check.maize.clear.shops.4..q75",       
"check.maize.clear.shops.4..q76")) {

all_4 <- cbind(all_4,in_long[in_long$variable == i,])
}
all_4 <- all_4[,c(1,seq(3,81, by=3))]

names(all_4) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","bought_last_season","knows_SA_rating","SA_rating","general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating", "seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant")

#for 5

all_5 <- in_long[in_long$variable == "check.maize.clear.shops.5..calc_name",]
for (i in c(
"check.maize.clear.shops.5..q64",       
"check.maize.clear.shops.5..q65",       
"check.maize.clear.shops.5..q65b",      
"check.maize.clear.shops.5..q66",       
"check.maize.clear.shops.5..q67",
"check.maize.clear.shops.5..q67a",
"check.maize.clear.shops.5..q67b",             
"check.maize.clear.shops.5..q68a",      
"check.maize.clear.shops.5..q68b",      
"check.maize.clear.shops.5..q68c",      
"check.maize.clear.shops.5..q68d",      
"check.maize.clear.shops.5..q68e",      
"check.maize.clear.shops.5..q68f",      
"check.maize.clear.shops.5..q69a",      
"check.maize.clear.shops.5..q69b",      
"check.maize.clear.shops.5..q69c",      
"check.maize.clear.shops.5..q69d",      
"check.maize.clear.shops.5..q69e",      
"check.maize.clear.shops.5..q69f",      
"check.maize.clear.shops.5..q70",       
"check.maize.clear.shops.5..q71",       
"check.maize.clear.shops.5..q72",       
"check.maize.clear.shops.5..q73",       
"check.maize.clear.shops.5..q74",       
"check.maize.clear.shops.5..q75",       
"check.maize.clear.shops.5..q76")) {

all_5 <- cbind(all_5,in_long[in_long$variable == i,])
}
all_5 <- all_5[,c(1,seq(3,81, by=3))]

names(all_5) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","bought_last_season","knows_SA_rating","SA_rating","general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating", "seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant")

#for 6

all_6 <- in_long[in_long$variable == "check.maize.clear.shops.6..calc_name",]
for (i in c(
"check.maize.clear.shops.6..q64",       
"check.maize.clear.shops.6..q65",       
"check.maize.clear.shops.6..q65b",      
"check.maize.clear.shops.6..q66",       
"check.maize.clear.shops.6..q67",
"check.maize.clear.shops.6..q67a",
"check.maize.clear.shops.6..q67b",             
"check.maize.clear.shops.6..q68a",      
"check.maize.clear.shops.6..q68b",      
"check.maize.clear.shops.6..q68c",      
"check.maize.clear.shops.6..q68d",      
"check.maize.clear.shops.6..q68e",      
"check.maize.clear.shops.6..q68f",      
"check.maize.clear.shops.6..q69a",      
"check.maize.clear.shops.6..q69b",      
"check.maize.clear.shops.6..q69c",      
"check.maize.clear.shops.6..q69d",      
"check.maize.clear.shops.6..q69e",      
"check.maize.clear.shops.6..q69f",      
"check.maize.clear.shops.6..q70",       
"check.maize.clear.shops.6..q71",       
"check.maize.clear.shops.6..q72",       
"check.maize.clear.shops.6..q73",       
"check.maize.clear.shops.6..q74",       
"check.maize.clear.shops.6..q75",       
"check.maize.clear.shops.6..q76")) {

all_6 <- cbind(all_6,in_long[in_long$variable == i,])
}
all_6 <- all_6[,c(1,seq(3,81, by=3))]

names(all_6) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","bought_last_season","knows_SA_rating","SA_rating","general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating", "seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant")

#for 7

all_7 <- in_long[in_long$variable == "check.maize.clear.shops.7..calc_name",]
for (i in c(
"check.maize.clear.shops.7..q64",       
"check.maize.clear.shops.7..q65",       
"check.maize.clear.shops.7..q65b",      
"check.maize.clear.shops.7..q66",       
"check.maize.clear.shops.7..q67",
"check.maize.clear.shops.7..q67a",
"check.maize.clear.shops.7..q67b",             
"check.maize.clear.shops.7..q68a",      
"check.maize.clear.shops.7..q68b",      
"check.maize.clear.shops.7..q68c",      
"check.maize.clear.shops.7..q68d",      
"check.maize.clear.shops.7..q68e",      
"check.maize.clear.shops.7..q68f",      
"check.maize.clear.shops.7..q69a",      
"check.maize.clear.shops.7..q69b",      
"check.maize.clear.shops.7..q69c",      
"check.maize.clear.shops.7..q69d",      
"check.maize.clear.shops.7..q69e",      
"check.maize.clear.shops.7..q69f",      
"check.maize.clear.shops.7..q70",       
"check.maize.clear.shops.7..q71",       
"check.maize.clear.shops.7..q72",       
"check.maize.clear.shops.7..q73",       
"check.maize.clear.shops.7..q74",       
"check.maize.clear.shops.7..q75",       
"check.maize.clear.shops.7..q76")) {

all_7 <- cbind(all_7,in_long[in_long$variable == i,])
}
all_7 <- all_7[,c(1,seq(3,81, by=3))]

names(all_7) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","bought_last_season","knows_SA_rating","SA_rating","general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating", "seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant")

#for 8

all_8 <- in_long[in_long$variable == "check.maize.clear.shops.8..calc_name",]
for (i in c(
"check.maize.clear.shops.8..q64",       
"check.maize.clear.shops.8..q65",       
"check.maize.clear.shops.8..q65b",      
"check.maize.clear.shops.8..q66",       
"check.maize.clear.shops.8..q67",
"check.maize.clear.shops.8..q67a",
"check.maize.clear.shops.8..q67b",             
"check.maize.clear.shops.8..q68a",      
"check.maize.clear.shops.8..q68b",      
"check.maize.clear.shops.8..q68c",      
"check.maize.clear.shops.8..q68d",      
"check.maize.clear.shops.8..q68e",      
"check.maize.clear.shops.8..q68f",      
"check.maize.clear.shops.8..q69a",      
"check.maize.clear.shops.8..q69b",      
"check.maize.clear.shops.8..q69c",      
"check.maize.clear.shops.8..q69d",      
"check.maize.clear.shops.8..q69e",      
"check.maize.clear.shops.8..q69f",      
"check.maize.clear.shops.8..q70",       
"check.maize.clear.shops.8..q71",       
"check.maize.clear.shops.8..q72",       
"check.maize.clear.shops.8..q73",       
"check.maize.clear.shops.8..q74",       
"check.maize.clear.shops.8..q75",       
"check.maize.clear.shops.8..q76")) {

all_8 <- cbind(all_8,in_long[in_long$variable == i,])
}
all_8 <- all_8[,c(1,seq(3,81, by=3))]

names(all_8) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","bought_last_season","knows_SA_rating","SA_rating","general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating", "seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant")

#for 9

all_9 <- in_long[in_long$variable == "check.maize.clear.shops.9..calc_name",]
for (i in c(
"check.maize.clear.shops.9..q64",       
"check.maize.clear.shops.9..q65",       
"check.maize.clear.shops.9..q65b",      
"check.maize.clear.shops.9..q66",       
"check.maize.clear.shops.9..q67",
"check.maize.clear.shops.9..q67a",
"check.maize.clear.shops.9..q67b",             
"check.maize.clear.shops.9..q68a",      
"check.maize.clear.shops.9..q68b",      
"check.maize.clear.shops.9..q68c",      
"check.maize.clear.shops.9..q68d",      
"check.maize.clear.shops.9..q68e",      
"check.maize.clear.shops.9..q68f",      
"check.maize.clear.shops.9..q69a",      
"check.maize.clear.shops.9..q69b",      
"check.maize.clear.shops.9..q69c",      
"check.maize.clear.shops.9..q69d",      
"check.maize.clear.shops.9..q69e",      
"check.maize.clear.shops.9..q69f",      
"check.maize.clear.shops.9..q70",       
"check.maize.clear.shops.9..q71",       
"check.maize.clear.shops.9..q72",       
"check.maize.clear.shops.9..q73",       
"check.maize.clear.shops.9..q74",       
"check.maize.clear.shops.9..q75",       
"check.maize.clear.shops.9..q76")) {

all_9 <- cbind(all_9,in_long[in_long$variable == i,])
}
all_9 <- all_9[,c(1,seq(3,81, by=3))]

names(all_9) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","bought_last_season","knows_SA_rating","SA_rating","general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating", "seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant")


#for 10

all_10 <- in_long[in_long$variable == "check.maize.clear.shops.10..calc_name",]
for (i in c(
"check.maize.clear.shops.10..q64",       
"check.maize.clear.shops.10..q65",       
"check.maize.clear.shops.10..q65b",      
"check.maize.clear.shops.10..q66",       
"check.maize.clear.shops.10..q67",
"check.maize.clear.shops.10..q67a",
"check.maize.clear.shops.10..q67b",             
"check.maize.clear.shops.10..q68a",      
"check.maize.clear.shops.10..q68b",      
"check.maize.clear.shops.10..q68c",      
"check.maize.clear.shops.10..q68d",      
"check.maize.clear.shops.10..q68e",      
"check.maize.clear.shops.10..q68f",      
"check.maize.clear.shops.10..q69a",      
"check.maize.clear.shops.10..q69b",      
"check.maize.clear.shops.10..q69c",      
"check.maize.clear.shops.10..q69d",      
"check.maize.clear.shops.10..q69e",      
"check.maize.clear.shops.10..q69f",      
"check.maize.clear.shops.10..q70",       
"check.maize.clear.shops.10..q71",       
"check.maize.clear.shops.10..q72",       
"check.maize.clear.shops.10..q73",       
"check.maize.clear.shops.10..q74",       
"check.maize.clear.shops.10..q75",       
"check.maize.clear.shops.10..q76")) {

all_10 <- cbind(all_10,in_long[in_long$variable == i,])
}
all_10 <- all_10[,c(1,seq(3,81, by=3))]

names(all_10) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","bought_last_season","knows_SA_rating","SA_rating","general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating", "seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant")


#for 11

all_11 <- in_long[in_long$variable == "check.maize.clear.shops.11..calc_name",]
for (i in c(
"check.maize.clear.shops.11..q64",       
"check.maize.clear.shops.11..q65",       
"check.maize.clear.shops.11..q65b",      
"check.maize.clear.shops.11..q66",       
"check.maize.clear.shops.11..q67",
"check.maize.clear.shops.11..q67a",
"check.maize.clear.shops.11..q67b",             
"check.maize.clear.shops.11..q68a",      
"check.maize.clear.shops.11..q68b",      
"check.maize.clear.shops.11..q68c",      
"check.maize.clear.shops.11..q68d",      
"check.maize.clear.shops.11..q68e",      
"check.maize.clear.shops.11..q68f",      
"check.maize.clear.shops.11..q69a",      
"check.maize.clear.shops.11..q69b",      
"check.maize.clear.shops.11..q69c",      
"check.maize.clear.shops.11..q69d",      
"check.maize.clear.shops.11..q69e",      
"check.maize.clear.shops.11..q69f",      
"check.maize.clear.shops.11..q70",       
"check.maize.clear.shops.11..q71",       
"check.maize.clear.shops.11..q72",       
"check.maize.clear.shops.11..q73",       
"check.maize.clear.shops.11..q74",       
"check.maize.clear.shops.11..q75",       
"check.maize.clear.shops.11..q76")) {

all_11 <- cbind(all_11,in_long[in_long$variable == i,])
}
all_11 <- all_11[,c(1,seq(3,81, by=3))]

names(all_11) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","bought_last_season","knows_SA_rating","SA_rating","general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating", "seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant")

#for 12

all_12 <- in_long[in_long$variable == "check.maize.clear.shops.12..calc_name",]
for (i in c(
"check.maize.clear.shops.12..q64",       
"check.maize.clear.shops.12..q65",       
"check.maize.clear.shops.12..q65b",      
"check.maize.clear.shops.12..q66",       
"check.maize.clear.shops.12..q67",
"check.maize.clear.shops.12..q67a",
"check.maize.clear.shops.12..q67b",             
"check.maize.clear.shops.12..q68a",      
"check.maize.clear.shops.12..q68b",      
"check.maize.clear.shops.12..q68c",      
"check.maize.clear.shops.12..q68d",      
"check.maize.clear.shops.12..q68e",      
"check.maize.clear.shops.12..q68f",      
"check.maize.clear.shops.12..q69a",      
"check.maize.clear.shops.12..q69b",      
"check.maize.clear.shops.12..q69c",      
"check.maize.clear.shops.12..q69d",      
"check.maize.clear.shops.12..q69e",      
"check.maize.clear.shops.12..q69f",      
"check.maize.clear.shops.12..q70",       
"check.maize.clear.shops.12..q71",       
"check.maize.clear.shops.12..q72",       
"check.maize.clear.shops.12..q73",       
"check.maize.clear.shops.12..q74",       
"check.maize.clear.shops.12..q75",       
"check.maize.clear.shops.12..q76")) {

all_12 <- cbind(all_12,in_long[in_long$variable == i,])
}
all_12 <- all_12[,c(1,seq(3,81, by=3))]

names(all_12) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","bought_last_season","knows_SA_rating","SA_rating","general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating", "seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant")

#for 13

all_13 <- in_long[in_long$variable == "check.maize.clear.shops.13..calc_name",]
for (i in c(
"check.maize.clear.shops.13..q64",       
"check.maize.clear.shops.13..q65",       
"check.maize.clear.shops.13..q65b",      
"check.maize.clear.shops.13..q66",       
"check.maize.clear.shops.13..q67",
"check.maize.clear.shops.13..q67a",
"check.maize.clear.shops.13..q67b",             
"check.maize.clear.shops.13..q68a",      
"check.maize.clear.shops.13..q68b",      
"check.maize.clear.shops.13..q68c",      
"check.maize.clear.shops.13..q68d",      
"check.maize.clear.shops.13..q68e",      
"check.maize.clear.shops.13..q68f",      
"check.maize.clear.shops.13..q69a",      
"check.maize.clear.shops.13..q69b",      
"check.maize.clear.shops.13..q69c",      
"check.maize.clear.shops.13..q69d",      
"check.maize.clear.shops.13..q69e",      
"check.maize.clear.shops.13..q69f",      
"check.maize.clear.shops.13..q70",       
"check.maize.clear.shops.13..q71",       
"check.maize.clear.shops.13..q72",       
"check.maize.clear.shops.13..q73",       
"check.maize.clear.shops.13..q74",       
"check.maize.clear.shops.13..q75",       
"check.maize.clear.shops.13..q76")) {

all_13 <- cbind(all_13,in_long[in_long$variable == i,])
}
all_13 <- all_13[,c(1,seq(3,81, by=3))]

names(all_13) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","bought_last_season","knows_SA_rating","SA_rating","general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating", "seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant")

#for 14

all_14 <- in_long[in_long$variable == "check.maize.clear.shops.14..calc_name",]
for (i in c(
"check.maize.clear.shops.14..q64",       
"check.maize.clear.shops.14..q65",       
"check.maize.clear.shops.14..q65b",      
"check.maize.clear.shops.14..q66",       
"check.maize.clear.shops.14..q67",
"check.maize.clear.shops.14..q67a",
"check.maize.clear.shops.14..q67b",             
"check.maize.clear.shops.14..q68a",     
"check.maize.clear.shops.14..q68b",      
"check.maize.clear.shops.14..q68c",      
"check.maize.clear.shops.14..q68d",      
"check.maize.clear.shops.14..q68e",      
"check.maize.clear.shops.14..q68f",      
"check.maize.clear.shops.14..q69a",      
"check.maize.clear.shops.14..q69b",      
"check.maize.clear.shops.14..q69c",      
"check.maize.clear.shops.14..q69d",      
"check.maize.clear.shops.14..q69e",      
"check.maize.clear.shops.14..q69f",      
"check.maize.clear.shops.14..q70",       
"check.maize.clear.shops.14..q71",       
"check.maize.clear.shops.14..q72",       
"check.maize.clear.shops.14..q73",       
"check.maize.clear.shops.14..q74",       
"check.maize.clear.shops.14..q75",       
"check.maize.clear.shops.14..q76")) {

all_14 <- cbind(all_14,in_long[in_long$variable == i,])
}
all_14 <- all_14[,c(1,seq(3,81, by=3))]

names(all_14) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","bought_last_season","knows_SA_rating","SA_rating","general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating", "seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant")

#for 15

all_15 <- in_long[in_long$variable == "check.maize.clear.shops.15..calc_name",]
for (i in c(
"check.maize.clear.shops.15..q64",       
"check.maize.clear.shops.15..q65",       
"check.maize.clear.shops.15..q65b",      
"check.maize.clear.shops.15..q66",       
"check.maize.clear.shops.15..q67",
"check.maize.clear.shops.15..q67a",
"check.maize.clear.shops.15..q67b",             
"check.maize.clear.shops.15..q68a",   
"check.maize.clear.shops.15..q68b",      
"check.maize.clear.shops.15..q68c",      
"check.maize.clear.shops.15..q68d",      
"check.maize.clear.shops.15..q68e",      
"check.maize.clear.shops.15..q68f",      
"check.maize.clear.shops.15..q69a",      
"check.maize.clear.shops.15..q69b",      
"check.maize.clear.shops.15..q69c",      
"check.maize.clear.shops.15..q69d",      
"check.maize.clear.shops.15..q69e",      
"check.maize.clear.shops.15..q69f",      
"check.maize.clear.shops.15..q70",       
"check.maize.clear.shops.15..q71",       
"check.maize.clear.shops.15..q72",       
"check.maize.clear.shops.15..q73",       
"check.maize.clear.shops.15..q74",       
"check.maize.clear.shops.15..q75",       
"check.maize.clear.shops.15..q76")) {

all_15 <- cbind(all_15,in_long[in_long$variable == i,])
}
all_15 <- all_15[,c(1,seq(3,81, by=3))]

names(all_15) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","bought_last_season","knows_SA_rating","SA_rating","general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating", "seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant")


#for 16

all_16 <- in_long[in_long$variable == "check.maize.clear.shops.16..calc_name",]
for (i in c(
"check.maize.clear.shops.16..q64",       
"check.maize.clear.shops.16..q65",       
"check.maize.clear.shops.16..q65b",      
"check.maize.clear.shops.16..q66",       
"check.maize.clear.shops.16..q67",
"check.maize.clear.shops.16..q67a",
"check.maize.clear.shops.16..q67b",             
"check.maize.clear.shops.16..q68a",    
"check.maize.clear.shops.16..q68b",      
"check.maize.clear.shops.16..q68c",      
"check.maize.clear.shops.16..q68d",      
"check.maize.clear.shops.16..q68e",      
"check.maize.clear.shops.16..q68f",      
"check.maize.clear.shops.16..q69a",      
"check.maize.clear.shops.16..q69b",      
"check.maize.clear.shops.16..q69c",      
"check.maize.clear.shops.16..q69d",      
"check.maize.clear.shops.16..q69e",      
"check.maize.clear.shops.16..q69f",      
"check.maize.clear.shops.16..q70",       
"check.maize.clear.shops.16..q71",       
"check.maize.clear.shops.16..q72",       
"check.maize.clear.shops.16..q73",       
"check.maize.clear.shops.16..q74",       
"check.maize.clear.shops.16..q75",       
"check.maize.clear.shops.16..q76")) {

all_16 <- cbind(all_16,in_long[in_long$variable == i,])
}
all_16 <- all_16[,c(1,seq(3,81, by=3))]

names(all_16) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","bought_last_season","knows_SA_rating","SA_rating","general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating", "seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant")


#for 17

all_17 <- in_long[in_long$variable == "check.maize.clear.shops.17..calc_name",]
for (i in c(
"check.maize.clear.shops.17..q64",       
"check.maize.clear.shops.17..q65",       
"check.maize.clear.shops.17..q65b",      
"check.maize.clear.shops.17..q66",       
"check.maize.clear.shops.17..q67",
"check.maize.clear.shops.17..q67a",
"check.maize.clear.shops.17..q67b",             
"check.maize.clear.shops.17..q68a",     
"check.maize.clear.shops.17..q68b",      
"check.maize.clear.shops.17..q68c",      
"check.maize.clear.shops.17..q68d",      
"check.maize.clear.shops.17..q68e",      
"check.maize.clear.shops.17..q68f",      
"check.maize.clear.shops.17..q69a",      
"check.maize.clear.shops.17..q69b",      
"check.maize.clear.shops.17..q69c",      
"check.maize.clear.shops.17..q69d",      
"check.maize.clear.shops.17..q69e",      
"check.maize.clear.shops.17..q69f",      
"check.maize.clear.shops.17..q70",       
"check.maize.clear.shops.17..q71",       
"check.maize.clear.shops.17..q72",       
"check.maize.clear.shops.17..q73",       
"check.maize.clear.shops.17..q74",       
"check.maize.clear.shops.17..q75",       
"check.maize.clear.shops.17..q76")) {

all_17 <- cbind(all_17,in_long[in_long$variable == i,])
}
all_17 <- all_17[,c(1,seq(3,81, by=3))]

names(all_17) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","bought_last_season","knows_SA_rating","SA_rating","general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating", "seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant")



#for 18

all_18 <- in_long[in_long$variable == "check.maize.clear.shops.18..calc_name",]
for (i in c(
"check.maize.clear.shops.18..q64",       
"check.maize.clear.shops.18..q65",       
"check.maize.clear.shops.18..q65b",      
"check.maize.clear.shops.18..q66",       
"check.maize.clear.shops.18..q67",
"check.maize.clear.shops.18..q67a",
"check.maize.clear.shops.18..q67b",             
"check.maize.clear.shops.18..q68a",    
"check.maize.clear.shops.18..q68b",      
"check.maize.clear.shops.18..q68c",      
"check.maize.clear.shops.18..q68d",      
"check.maize.clear.shops.18..q68e",      
"check.maize.clear.shops.18..q68f",      
"check.maize.clear.shops.18..q69a",      
"check.maize.clear.shops.18..q69b",      
"check.maize.clear.shops.18..q69c",      
"check.maize.clear.shops.18..q69d",      
"check.maize.clear.shops.18..q69e",      
"check.maize.clear.shops.18..q69f",      
"check.maize.clear.shops.18..q70",       
"check.maize.clear.shops.18..q71",       
"check.maize.clear.shops.18..q72",       
"check.maize.clear.shops.18..q73",       
"check.maize.clear.shops.18..q74",       
"check.maize.clear.shops.18..q75",       
"check.maize.clear.shops.18..q76")) {

all_18 <- cbind(all_18,in_long[in_long$variable == i,])
}
all_18 <- all_18[,c(1,seq(3,81, by=3))]

names(all_18) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","bought_last_season","knows_SA_rating","SA_rating","general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating", "seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant")

all_shops <- rbind(all_1,all_2,all_3,all_4,all_5,all_6,all_7,all_8, all_9, all_10,all_11,all_12,all_13,all_14,all_15,all_16,all_17,all_18)
all_shops <- subset(all_shops, shop_ID!="n/a")

#remove ratings data from main base data file
for (i in 1:18) {

mid[,
c(paste(paste("check.maize.clear.shops",i,sep="."),"calc_name",sep = ".."),
paste(paste("check.maize.clear.shops",i,sep="."),"q64",sep = ".."),       
paste(paste("check.maize.clear.shops",i,sep="."),"q65",sep = ".."),       
paste(paste("check.maize.clear.shops",i,sep="."),"q65b",sep = ".."),      
paste(paste("check.maize.clear.shops",i,sep="."),"q66",sep = ".."),       
paste(paste("check.maize.clear.shops",i,sep="."),"q67",sep = ".."),
paste(paste("check.maize.clear.shops",i,sep="."),"q67a",sep = ".."),   
paste(paste("check.maize.clear.shops",i,sep="."),"q67b",sep = ".."),      
paste(paste("check.maize.clear.shops",i,sep="."),"q68a",sep = ".."),      
paste(paste("check.maize.clear.shops",i,sep="."),"q68b",sep = ".."),      
paste(paste("check.maize.clear.shops",i,sep="."),"q68c",sep = ".."),      
paste(paste("check.maize.clear.shops",i,sep="."),"q68d",sep = ".."),      
paste(paste("check.maize.clear.shops",i,sep="."),"q68e",sep = ".."),      
paste(paste("check.maize.clear.shops",i,sep="."),"q68f",sep = ".."),      
paste(paste("check.maize.clear.shops",i,sep="."),"q69a",sep = ".."),      
paste(paste("check.maize.clear.shops",i,sep="."),"q69b",sep = ".."),      
paste(paste("check.maize.clear.shops",i,sep="."),"q69c",sep = ".."),      
paste(paste("check.maize.clear.shops",i,sep="."),"q69d",sep = ".."),      
paste(paste("check.maize.clear.shops",i,sep="."),"q69e",sep = ".."),      
paste(paste("check.maize.clear.shops",i,sep="."),"q69f",sep = ".."),      
paste(paste("check.maize.clear.shops",i,sep="."),"q70",sep = ".."),       
paste(paste("check.maize.clear.shops",i,sep="."),"q71",sep = ".."),       
paste(paste("check.maize.clear.shops",i,sep="."),"q72",sep = ".."),       
paste(paste("check.maize.clear.shops",i,sep="."),"q73",sep = ".."),       
paste(paste("check.maize.clear.shops",i,sep="."),"q74",sep = ".."),       
paste(paste("check.maize.clear.shops",i,sep="."),"q75",sep = ".."),       
paste(paste("check.maize.clear.shops",i,sep="."),"q76",sep = ".."),
paste(paste("check.maize.clear.shops",i,sep="."),"pos",sep = ".."))] <- NULL
}





write.csv(mid,"/home/bjvca/data/projects/Seed_systems_project/midline/data/farmer/public/midline.csv", row.names=FALSE)

write.csv(all_shops,"/home/bjvca/data/projects/Seed_systems_project/midline/data/farmer/public/midline_rating_dyads.csv", row.names=FALSE)
