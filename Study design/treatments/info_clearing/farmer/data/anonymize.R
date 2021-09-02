#run in /home/bjvca/Dropbox%20(IFPRI)/NWO%20seed%20system%20devt%20Uganda%20proposal%20development/Study%20design/treatments/info_clearing/farmer/data/

path <- getwd()

library(ggplot2)
library(reshape2)
library(leaflet)
library(dplyr)

### reads in raw data (not public)
farmers <- read.csv(paste(path,"farmer_dissemination_final.csv", sep="/"), stringsAsFactors=FALSE)

sum(duplicated(farmers$farmer_ID))
## 22 farmers are duplicates - how can these be saved? use gps coordinates to correct farmer_ID

farmers$farmer_ID[farmers$X_uuid == "f386391d-2872-4915-a3c8-544fc0ee7948"] <- "F_1007"
farmers <- farmers[!(farmers$X_uuid == "b0a9cc25-5042-4af0-9d99-31b61da83901"),] 
farmers$farmer_ID[farmers$X_uuid == "441c4c15-091e-40ac-b7c9-5e6c3063c807"] <- "F_800"
farmers <- farmers[!(farmers$X_uuid == "0e113efa-b140-410a-9450-14a0eda23d8c"),] 
farmers$farmer_ID[farmers$X_uuid == "fb2808ba-e3ef-4a51-8aee-48c98faaa3f2"] <- "F_3007"
farmers <- farmers[!(farmers$X_uuid == "6c30fc6b-ed86-4e9c-8ac8-69b649616574"),] 
farmers$farmer_ID[farmers$X_uuid == "fb50beac-2eca-4734-8b6f-62e462f1eab3"] <- "F_88"
farmers <- farmers[!(farmers$X_uuid == "005a3924-5824-43e9-a35a-1bf3088af071"),] 
farmers$farmer_ID[farmers$X_uuid == "cde95795-9c7c-4144-8212-81ced2bbf4be"] <- "F_3462"
farmers$farmer_ID[farmers$X_uuid == "17ff3dd2-a853-454c-977c-25175038c5fa"] <- "F_2661"
farmers$farmer_ID[farmers$X_uuid == "c08acedd-babd-4466-9e18-79a2fba88580"] <- "F_2526"
farmers$farmer_ID[farmers$X_uuid == "89775c9e-d95c-4a90-9581-07a0c2f9a724"] <- "F_2866"
farmers$farmer_ID[farmers$X_uuid == "027e830e-1b39-4f22-b18e-ef687d71cfed"] <- "F_1634"
farmers$farmer_ID[farmers$X_uuid == "33238b04-7f4e-4ec9-8ed0-0242ef61f8e9"] <- "F_1638"
farmers$farmer_ID[farmers$X_uuid == "4ba18ec8-1b54-45bb-a440-2ae6b263171a"] <- "F_1137"
farmers <- farmers[!(farmers$X_uuid == "efd2b78b-35d1-4486-90b9-73e6f1953221"),] 
farmers$farmer_ID[farmers$X_uuid == "461a9e97-3d0f-4a34-8dd0-c804d1f61696"] <- "F_1339"
farmers$farmer_ID[farmers$X_uuid == "09953523-f23d-4783-9faf-da88ea940e68"] <- "F_316"
farmers$farmer_ID[farmers$X_uuid == "5945f83d-e4ba-42db-9fd7-ffcbcdc37771"] <- "F_3159"
farmers$farmer_ID[farmers$X_uuid == "3ca29bf6-2e9e-4665-9376-4ed001de90e9"] <- "F_1213"
farmers$farmer_ID[farmers$X_uuid == "413d7fa7-b895-4274-9438-c0ad8422be8c"] <- "F_756"
farmers$farmer_ID[farmers$X_uuid == "bc7ea03c-44dd-4da8-9744-38bad3f806fc"] <- "F_700"

sum(duplicated(farmers$farmer_ID))  # all fixed

### extract information to be used as baseline data for non-treated farmers

farmers <- subset(farmers,Check2.check.clearing == FALSE)

in_long <- melt(farmers, id.vars = c("farmer_ID"))

###extract rating data from this dataset and save in a separate file called ratings_dyads
all_1 <- in_long[in_long$variable == "Check2.check.shops.1..calc_name",]
for (i in c(
"Check2.check.shops.1..q64",    #kd   
"Check2.check.shops.1..q65",       
"Check2.check.shops.1..q65b",      
"Check2.check.shops.1..q66",       
"Check2.check.shops.1..q70",       
"Check2.check.shops.1..q71",       
"Check2.check.shops.1..q72",       
"Check2.check.shops.1..q73",       
"Check2.check.shops.1..q74",       
"Check2.check.shops.1..q75",       
"Check2.check.shops.1..q76","catchID")) {

all_1 <- cbind(all_1,in_long[in_long$variable == i,])
}
all_1 <- all_1[,c(1,seq(3,39, by=3))]

names(all_1) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant","catchID")

### we need to do this 18 times

all_2 <- in_long[in_long$variable == "Check2.check.shops.2..calc_name",]
for (i in c(
"Check2.check.shops.2..q64",    #kd   
"Check2.check.shops.2..q65",       
"Check2.check.shops.2..q65b",      
"Check2.check.shops.2..q66",       
"Check2.check.shops.2..q70",       
"Check2.check.shops.2..q71",       
"Check2.check.shops.2..q72",       
"Check2.check.shops.2..q73",       
"Check2.check.shops.2..q74",       
"Check2.check.shops.2..q75",       
"Check2.check.shops.2..q76","catchID")) {

all_2 <- cbind(all_2,in_long[in_long$variable == i,])
}
all_2 <- all_2[,c(1,seq(3,39, by=3))]

names(all_2) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant","catchID")

all_3 <- in_long[in_long$variable == "Check2.check.shops.3..calc_name",]
for (i in c(
"Check2.check.shops.3..q64",    #kd   
"Check2.check.shops.3..q65",       
"Check2.check.shops.3..q65b",      
"Check2.check.shops.3..q66",       
"Check2.check.shops.3..q70",       
"Check2.check.shops.3..q71",       
"Check2.check.shops.3..q72",       
"Check2.check.shops.3..q73",       
"Check2.check.shops.3..q74",       
"Check2.check.shops.3..q75",       
"Check2.check.shops.3..q76","catchID")) {
all_3 <- cbind(all_3,in_long[in_long$variable == i,])
}
all_3 <- all_3[,c(1,seq(3,39, by=3))]

names(all_3) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant","catchID")

all_4 <- in_long[in_long$variable == "Check2.check.shops.4..calc_name",]
for (i in c(
"Check2.check.shops.4..q64",    #kd   
"Check2.check.shops.4..q65",       
"Check2.check.shops.4..q65b",      
"Check2.check.shops.4..q66",       
"Check2.check.shops.4..q70",       
"Check2.check.shops.4..q71",       
"Check2.check.shops.4..q72",       
"Check2.check.shops.4..q73",       
"Check2.check.shops.4..q74",       
"Check2.check.shops.4..q75",       
"Check2.check.shops.4..q76","catchID")) {

all_4 <- cbind(all_4,in_long[in_long$variable == i,])
}
all_4 <- all_4[,c(1,seq(3,39, by=3))]

names(all_4) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant","catchID")

all_5 <- in_long[in_long$variable == "Check2.check.shops.5..calc_name",]
for (i in c(
"Check2.check.shops.5..q64",    #kd   
"Check2.check.shops.5..q65",       
"Check2.check.shops.5..q65b",      
"Check2.check.shops.5..q66",       
"Check2.check.shops.5..q70",       
"Check2.check.shops.5..q71",       
"Check2.check.shops.5..q72",       
"Check2.check.shops.5..q73",       
"Check2.check.shops.5..q74",       
"Check2.check.shops.5..q75",       
"Check2.check.shops.5..q76","catchID")) {

all_5 <- cbind(all_5,in_long[in_long$variable == i,])
}
all_5 <- all_5[,c(1,seq(3,39, by=3))]

names(all_5) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant","catchID")

all_6 <- in_long[in_long$variable == "Check2.check.shops.6..calc_name",]
for (i in c(
"Check2.check.shops.6..q64",    #kd   
"Check2.check.shops.6..q65",       
"Check2.check.shops.6..q65b",      
"Check2.check.shops.6..q66",       
"Check2.check.shops.6..q70",       
"Check2.check.shops.6..q71",       
"Check2.check.shops.6..q72",       
"Check2.check.shops.6..q73",       
"Check2.check.shops.6..q74",       
"Check2.check.shops.6..q75",       
"Check2.check.shops.6..q76","catchID")) {

all_6 <- cbind(all_6,in_long[in_long$variable == i,])
}
all_6 <- all_6[,c(1,seq(3,39, by=3))]

names(all_6) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant","catchID")

all_7 <- in_long[in_long$variable == "Check2.check.shops.7..calc_name",]
for (i in c(
"Check2.check.shops.7..q64",    #kd   
"Check2.check.shops.7..q65",       
"Check2.check.shops.7..q65b",      
"Check2.check.shops.7..q66",       
"Check2.check.shops.7..q70",       
"Check2.check.shops.7..q71",       
"Check2.check.shops.7..q72",       
"Check2.check.shops.7..q73",       
"Check2.check.shops.7..q74",       
"Check2.check.shops.7..q75",       
"Check2.check.shops.7..q76","catchID")) {

all_7 <- cbind(all_7,in_long[in_long$variable == i,])
}
all_7 <- all_7[,c(1,seq(3,39, by=3))]

names(all_7) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant","catchID")

all_8 <- in_long[in_long$variable == "Check2.check.shops.8..calc_name",]
for (i in c(
"Check2.check.shops.8..q64",    #kd   
"Check2.check.shops.8..q65",       
"Check2.check.shops.8..q65b",      
"Check2.check.shops.8..q66",       
"Check2.check.shops.8..q70",       
"Check2.check.shops.8..q71",       
"Check2.check.shops.8..q72",       
"Check2.check.shops.8..q73",       
"Check2.check.shops.8..q74",       
"Check2.check.shops.8..q75",       
"Check2.check.shops.8..q76","catchID")) {

all_8 <- cbind(all_8,in_long[in_long$variable == i,])
}
all_8 <- all_8[,c(1,seq(3,39, by=3))]

names(all_8) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant","catchID")

all_9 <- in_long[in_long$variable == "Check2.check.shops.9..calc_name",]
for (i in c(
"Check2.check.shops.9..q64",    #kd   
"Check2.check.shops.9..q65",       
"Check2.check.shops.9..q65b",      
"Check2.check.shops.9..q66",       
"Check2.check.shops.9..q70",       
"Check2.check.shops.9..q71",       
"Check2.check.shops.9..q72",       
"Check2.check.shops.9..q73",       
"Check2.check.shops.9..q74",       
"Check2.check.shops.9..q75",       
"Check2.check.shops.9..q76","catchID")) {

all_9 <- cbind(all_9,in_long[in_long$variable == i,])
}
all_9 <- all_9[,c(1,seq(3,39, by=3))]

names(all_9) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant","catchID")

all_10 <- in_long[in_long$variable == "Check2.check.shops.10..calc_name",]
for (i in c(
"Check2.check.shops.10..q64",    #kd   
"Check2.check.shops.10..q65",       
"Check2.check.shops.10..q65b",      
"Check2.check.shops.10..q66",       
"Check2.check.shops.10..q70",       
"Check2.check.shops.10..q71",       
"Check2.check.shops.10..q72",       
"Check2.check.shops.10..q73",       
"Check2.check.shops.10..q74",       
"Check2.check.shops.10..q75",       
"Check2.check.shops.10..q76","catchID")) {

all_10 <- cbind(all_10,in_long[in_long$variable == i,])
}
all_10 <- all_10[,c(1,seq(3,39, by=3))]

names(all_10) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant","catchID")

all_11 <- in_long[in_long$variable == "Check2.check.shops.11..calc_name",]
for (i in c(
"Check2.check.shops.11..q64",    #kd   
"Check2.check.shops.11..q65",       
"Check2.check.shops.11..q65b",      
"Check2.check.shops.11..q66",       
"Check2.check.shops.11..q70",       
"Check2.check.shops.11..q71",       
"Check2.check.shops.11..q72",       
"Check2.check.shops.11..q73",       
"Check2.check.shops.11..q74",       
"Check2.check.shops.11..q75",       
"Check2.check.shops.11..q76","catchID")) {

all_11 <- cbind(all_11,in_long[in_long$variable == i,])
}
all_11 <- all_11[,c(1,seq(3,39, by=3))]

names(all_11) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant","catchID")

all_12 <- in_long[in_long$variable == "Check2.check.shops.12..calc_name",]
for (i in c(
"Check2.check.shops.12..q64",    #kd   
"Check2.check.shops.12..q65",       
"Check2.check.shops.12..q65b",      
"Check2.check.shops.12..q66",       
"Check2.check.shops.12..q70",       
"Check2.check.shops.12..q71",       
"Check2.check.shops.12..q72",       
"Check2.check.shops.12..q73",       
"Check2.check.shops.12..q74",       
"Check2.check.shops.12..q75",       
"Check2.check.shops.12..q76","catchID")) {

all_12 <- cbind(all_12,in_long[in_long$variable == i,])
}
all_12 <- all_12[,c(1,seq(3,39, by=3))]

names(all_12) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant","catchID")


all_13 <- in_long[in_long$variable == "Check2.check.shops.13..calc_name",]
for (i in c(
"Check2.check.shops.13..q64",    #kd   
"Check2.check.shops.13..q65",       
"Check2.check.shops.13..q65b",      
"Check2.check.shops.13..q66",       
"Check2.check.shops.13..q70",       
"Check2.check.shops.13..q71",       
"Check2.check.shops.13..q72",       
"Check2.check.shops.13..q73",       
"Check2.check.shops.13..q74",       
"Check2.check.shops.13..q75",       
"Check2.check.shops.13..q76","catchID")) {

all_13 <- cbind(all_13,in_long[in_long$variable == i,])
}
all_13 <- all_13[,c(1,seq(3,39, by=3))]

names(all_13) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant","catchID")

all_14 <- in_long[in_long$variable == "Check2.check.shops.14..calc_name",]
for (i in c(
"Check2.check.shops.14..q64",    #kd   
"Check2.check.shops.14..q65",       
"Check2.check.shops.14..q65b",      
"Check2.check.shops.14..q66",       
"Check2.check.shops.14..q70",       
"Check2.check.shops.14..q71",       
"Check2.check.shops.14..q72",       
"Check2.check.shops.14..q73",       
"Check2.check.shops.14..q74",       
"Check2.check.shops.14..q75",       
"Check2.check.shops.14..q76","catchID")) {

all_14 <- cbind(all_14,in_long[in_long$variable == i,])
}
all_14 <- all_14[,c(1,seq(3,39, by=3))]

names(all_14) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant","catchID")

all_15 <- in_long[in_long$variable == "Check2.check.shops.15..calc_name",]
for (i in c(
"Check2.check.shops.15..q64",    #kd   
"Check2.check.shops.15..q65",       
"Check2.check.shops.15..q65b",      
"Check2.check.shops.15..q66",       
"Check2.check.shops.15..q70",       
"Check2.check.shops.15..q71",       
"Check2.check.shops.15..q72",       
"Check2.check.shops.15..q73",       
"Check2.check.shops.15..q74",       
"Check2.check.shops.15..q75",       
"Check2.check.shops.15..q76","catchID")) {

all_15 <- cbind(all_15,in_long[in_long$variable == i,])
}
all_15 <- all_15[,c(1,seq(3,39, by=3))]

names(all_15) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant","catchID")

all_16 <- in_long[in_long$variable == "Check2.check.shops.16..calc_name",]
for (i in c(
"Check2.check.shops.16..q64",    #kd   
"Check2.check.shops.16..q65",       
"Check2.check.shops.16..q65b",      
"Check2.check.shops.16..q66",       
"Check2.check.shops.16..q70",       
"Check2.check.shops.16..q71",       
"Check2.check.shops.16..q72",       
"Check2.check.shops.16..q73",       
"Check2.check.shops.16..q74",       
"Check2.check.shops.16..q75",       
"Check2.check.shops.16..q76","catchID")) {
all_16 <- cbind(all_16,in_long[in_long$variable == i,])
}
all_16 <- all_16[,c(1,seq(3,39, by=3))]

names(all_16) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant","catchID")

all_17 <- in_long[in_long$variable == "Check2.check.shops.17..calc_name",]
for (i in c(
"Check2.check.shops.17..q64",    #kd   
"Check2.check.shops.17..q65",       
"Check2.check.shops.17..q65b",      
"Check2.check.shops.17..q66",       
"Check2.check.shops.17..q70",       
"Check2.check.shops.17..q71",       
"Check2.check.shops.17..q72",       
"Check2.check.shops.17..q73",       
"Check2.check.shops.17..q74",       
"Check2.check.shops.17..q75",       
"Check2.check.shops.17..q76","catchID")) {

all_17 <- cbind(all_17,in_long[in_long$variable == i,])
}
all_17 <- all_17[,c(1,seq(3,39, by=3))]

names(all_17) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant","catchID")

all_18 <- in_long[in_long$variable == "Check2.check.shops.18..calc_name",]
for (i in c(
"Check2.check.shops.18..q64",    #kd   
"Check2.check.shops.18..q65",       
"Check2.check.shops.18..q65b",      
"Check2.check.shops.18..q66",       
"Check2.check.shops.18..q70",       
"Check2.check.shops.18..q71",       
"Check2.check.shops.18..q72",       
"Check2.check.shops.18..q73",       
"Check2.check.shops.18..q74",       
"Check2.check.shops.18..q75",       
"Check2.check.shops.18..q76","catchID")) {

all_18 <- cbind(all_18,in_long[in_long$variable == i,])
}
all_18 <- all_18[,c(1,seq(3,39, by=3))]

names(all_18) <- c("farmer_ID","shop_ID","knows_dealer","bought_at_dealer", "duration_customer","knows_other_customer","refunds","gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods","small_quant","catchID")

all_shops <- rbind(all_1,all_2,all_3,all_4,all_5,all_6,all_7,all_8, all_9, all_10,all_11,all_12,all_13,all_14,all_15,all_16,all_17,all_18)
all_shops <- subset(all_shops, shop_ID!="n/a")


#### now get baseline data and add it 

baseline_dyads <- read.csv(paste(strsplit(path, "/Study design/treatments/info_clearing/farmer/data")[[1]], "baseline/data/farmer/public/rating_dyads.csv", sep="/"))


baseline_dyads <- rbind(all_shops,baseline_dyads[names(all_shops)])

write.csv(baseline_dyads,paste(path,"public/dealer_services_dyads.csv", sep="/"), row.names=FALSE)

