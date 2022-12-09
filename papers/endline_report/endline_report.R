rm(list=ls())

#df_ols_ --> df_ols_end_
#df_means_ --> df_means_end_

path <- getwd()
#path <- strsplit(path,"/papers/endline_report")[[1]]
#path <- strsplit(path,"/papers/clearinghouse_training_paper")[[1]]

baseline_dealers <- read.csv(paste(path,"/baseline/data/agro_input/public/baseline_dealer.csv",sep="/"), stringsAsFactors=TRUE)

baseline_farmers <- read.csv(paste(path,"/baseline/data/farmer/public/baseline_farmers.csv",sep="/"), stringsAsFactors=TRUE)
midline_farmers <- read.csv(paste(path,"/midline/data/farmer/public/midline.csv",sep="/"), stringsAsFactors=TRUE)
endline_farmers <- read.csv(paste(path,"/endline/data/farmer/public/endline.csv",sep="/"), stringsAsFactors=TRUE)

library(dplyr)
library(Jmisc)
endline_farmers <- rename_with(endline_farmers,toupper)

baseline_farmers <- merge(baseline_farmers,endline_farmers,by.x="farmer_ID",by.y="FARMER_ID",all.x=TRUE)

#merge in more data

###
#A# RATINGS (only CH treatment farmers rated CH treatment dealers at baseline)
###

#BASELINE

#farmers
rating_dyads <- read.csv(paste(path,"/baseline/data/farmer/public/rating_dyads.csv",sep="/"), stringsAsFactors=TRUE)
rating_dyads[rating_dyads=="n/a"] <- NA
rating_dyads[rating_dyads==98] <- NA

rating_dyads$bought_last_season <- ifelse(rating_dyads$bought_last_season=="Yes",1,0)

as_numeric <- c("bought_last_season","general_rating","location_rating","price_rating","quality_rating"
                ,"stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating"
                ,"seed_drought_rating","seed_disease_rating","seed_maturing_rating"
                ,"seed_germinate_rating")
rating_dyads[as_numeric] <- lapply(rating_dyads[as_numeric],function(x)as.numeric(as.character(x)))

rating_dyads_aggr_F <- aggregate(rating_dyads,by=list(rating_dyads$farmer_ID),FUN="mean",na.rm=T)
rating_dyads_aggr_F = rating_dyads_aggr_F[c("Group.1","bought_last_season","general_rating"
                                            ,"location_rating","price_rating","quality_rating"
                                            ,"stock_rating","reputation_rating"
                                            ,"seed_quality_general_rating","seed_yield_rating"
                                            ,"seed_drought_rating","seed_disease_rating"
                                            ,"seed_maturing_rating","seed_germinate_rating")]

baseline_farmers <- merge(baseline_farmers,rating_dyads_aggr_F,by.x="farmer_ID",by.y="Group.1",all.x=TRUE)

#dealers (again)
rating_dyads_aggr_D <- aggregate(rating_dyads,by=list(rating_dyads$shop_ID),FUN="mean",na.rm=T)
rating_dyads_aggr_D = rating_dyads_aggr_D[c("Group.1","bought_last_season","general_rating"
                                            ,"location_rating","price_rating","quality_rating"
                                            ,"stock_rating","reputation_rating"
                                            ,"seed_quality_general_rating","seed_yield_rating"
                                            ,"seed_drought_rating","seed_disease_rating"
                                            ,"seed_maturing_rating","seed_germinate_rating")]

baseline_dealers <- merge(baseline_dealers,rating_dyads_aggr_D,by.x="shop_ID",by.y="Group.1",all.x=TRUE)

baseline_dealers$general <- baseline_dealers$seed_quality_general_rating #because I've previously worked with reviews_seed by Bjorn with different variable names
baseline_dealers$yield <- baseline_dealers$seed_yield_rating
baseline_dealers$drought_resistent <- baseline_dealers$seed_drought_rating
baseline_dealers$disease_resistent <- baseline_dealers$seed_disease_rating
baseline_dealers$early_maturing <- baseline_dealers$seed_maturing_rating
baseline_dealers$germination <- baseline_dealers$seed_germinate_rating

#MIDLINE

#farmers
midline_rating_dyads <- read.csv(paste(path,"/midline/data/farmer/public/midline_rating_dyads.csv",sep="/"), stringsAsFactors=TRUE)

midline_rating_dyads[midline_rating_dyads=="n/a"] <- NA
midline_rating_dyads[midline_rating_dyads==98] <- NA

midline_rating_dyads$mid_bought_last_season <- ifelse(midline_rating_dyads$bought_last_season=="Yes",1,0)

names(midline_rating_dyads)[names(midline_rating_dyads) == "general_rating"] <- "mid_general_rating" #because same name in bl and ml
names(midline_rating_dyads)[names(midline_rating_dyads) == "location_rating"] <- "mid_location_rating"
names(midline_rating_dyads)[names(midline_rating_dyads) == "price_rating"] <- "mid_price_rating"
names(midline_rating_dyads)[names(midline_rating_dyads) == "quality_rating"] <- "mid_quality_rating"
names(midline_rating_dyads)[names(midline_rating_dyads) == "stock_rating"] <- "mid_stock_rating"
names(midline_rating_dyads)[names(midline_rating_dyads) == "reputation_rating"] <- "mid_reputation_rating"
names(midline_rating_dyads)[names(midline_rating_dyads) == "seed_quality_general_rating"] <- "mid_seed_quality_general_rating"
names(midline_rating_dyads)[names(midline_rating_dyads) == "seed_yield_rating"] <- "mid_seed_yield_rating"
names(midline_rating_dyads)[names(midline_rating_dyads) == "seed_drought_rating"] <- "mid_seed_drought_rating"
names(midline_rating_dyads)[names(midline_rating_dyads) == "seed_disease_rating"] <- "mid_seed_disease_rating"
names(midline_rating_dyads)[names(midline_rating_dyads) == "seed_maturing_rating"] <- "mid_seed_maturing_rating"
names(midline_rating_dyads)[names(midline_rating_dyads) == "seed_germinate_rating"] <- "mid_seed_germinate_rating"

as_numeric <- c("mid_bought_last_season","mid_general_rating","mid_location_rating","mid_price_rating","mid_quality_rating"
                ,"mid_stock_rating","mid_reputation_rating","mid_seed_quality_general_rating","mid_seed_yield_rating"
                ,"mid_seed_drought_rating","mid_seed_disease_rating","mid_seed_maturing_rating"
                ,"mid_seed_germinate_rating")
midline_rating_dyads[as_numeric] <- lapply(midline_rating_dyads[as_numeric],function(x)as.numeric(as.character(x)))

midline_rating_dyads_aggr_F <- aggregate(midline_rating_dyads,by=list(midline_rating_dyads$farmer_ID),FUN="mean",na.rm=T)
midline_rating_dyads_aggr_F = subset(midline_rating_dyads_aggr_F, select = c("Group.1","mid_bought_last_season","mid_general_rating"
                                            ,"mid_location_rating","mid_price_rating","mid_quality_rating"
                                            ,"mid_stock_rating","mid_reputation_rating"
                                            ,"mid_seed_quality_general_rating","mid_seed_yield_rating"
                                            ,"mid_seed_drought_rating","mid_seed_disease_rating"
                                            ,"mid_seed_maturing_rating","mid_seed_germinate_rating"))

baseline_farmers <- merge(baseline_farmers,midline_rating_dyads_aggr_F,by.x="farmer_ID",by.y="Group.1",all.x=TRUE)

#dealers
midline_rating_dyads_aggr_D <- aggregate(midline_rating_dyads,by=list(midline_rating_dyads$shop_ID),FUN="mean",na.rm=T)
midline_rating_dyads_aggr_D = subset(midline_rating_dyads_aggr_D, select = c("Group.1","mid_bought_last_season","mid_general_rating"
                                                                             ,"mid_location_rating","mid_price_rating","mid_quality_rating"
                                                                             ,"mid_stock_rating","mid_reputation_rating"
                                                                             ,"mid_seed_quality_general_rating","mid_seed_yield_rating"
                                                                             ,"mid_seed_drought_rating","mid_seed_disease_rating"
                                                                             ,"mid_seed_maturing_rating","mid_seed_germinate_rating"))

baseline_dealers <- merge(baseline_dealers,midline_rating_dyads_aggr_D,by.x="shop_ID",by.y="Group.1",all.x=TRUE)

#ENDLINE

#farmers
endline_rating_dyads <- read.csv(paste(path,"/endline/data/farmer/public/endline_rating_dyads.csv",sep="/"), stringsAsFactors=TRUE)

endline_rating_dyads[endline_rating_dyads=="n/a"] <- NA
endline_rating_dyads[endline_rating_dyads==98] <- NA

endline_rating_dyads$end_bought_last_season <- ifelse(endline_rating_dyads$bought_last_season=="Yes",1,0)

names(endline_rating_dyads)[names(endline_rating_dyads) == "general_rating"] <- "end_general_rating" #because same name in bl and ml
names(endline_rating_dyads)[names(endline_rating_dyads) == "location_rating"] <- "end_location_rating"
names(endline_rating_dyads)[names(endline_rating_dyads) == "price_rating"] <- "end_price_rating"
names(endline_rating_dyads)[names(endline_rating_dyads) == "quality_rating"] <- "end_quality_rating"
names(endline_rating_dyads)[names(endline_rating_dyads) == "stock_rating"] <- "end_stock_rating"
names(endline_rating_dyads)[names(endline_rating_dyads) == "reputation_rating"] <- "end_reputation_rating"
names(endline_rating_dyads)[names(endline_rating_dyads) == "seed_quality_general_rating"] <- "end_seed_quality_general_rating"
names(endline_rating_dyads)[names(endline_rating_dyads) == "seed_yield_rating"] <- "end_seed_yield_rating"
names(endline_rating_dyads)[names(endline_rating_dyads) == "seed_drought_rating"] <- "end_seed_drought_rating"
names(endline_rating_dyads)[names(endline_rating_dyads) == "seed_disease_rating"] <- "end_seed_disease_rating"
names(endline_rating_dyads)[names(endline_rating_dyads) == "seed_maturing_rating"] <- "end_seed_maturing_rating"
names(endline_rating_dyads)[names(endline_rating_dyads) == "seed_germinate_rating"] <- "end_seed_germinate_rating"

endline_rating_dyads$end_knows_dealer <- ifelse(endline_rating_dyads$knows_dealer=="Yes",1,0)
endline_rating_dyads$end_bought_at_dealer <- ifelse(endline_rating_dyads$bought_at_dealer=="Yes",1,0)
endline_rating_dyads$end_customer_years <- 2022 - as.numeric(as.character(substr(endline_rating_dyads$duration_customer,start=1,stop=4))) #endline data collection in 2022
endline_rating_dyads$end_knows_other_customer <- ifelse(endline_rating_dyads$knows_other_customer=="Yes",1,0)
endline_rating_dyads$end_refunds <- ifelse(endline_rating_dyads$refunds=="Yes",1,0)
endline_rating_dyads$end_gives_credit <- ifelse(endline_rating_dyads$gives_credit=="Yes",1,0)
endline_rating_dyads$end_gives_advice <- ifelse(endline_rating_dyads$gives_advice=="Yes",1,0)
endline_rating_dyads$end_delivers <- ifelse(endline_rating_dyads$delivers=="Yes",1,0)
endline_rating_dyads$end_after_sales_service <- ifelse(endline_rating_dyads$after_sales_service=="Yes",1,0)
endline_rating_dyads$end_payment_mehtods <- ifelse(endline_rating_dyads$payment_mehtods=="Yes",1,0)
endline_rating_dyads$end_small_quant <- ifelse(endline_rating_dyads$small_quant=="Yes",1,0)

as_numeric <- c("end_bought_last_season","end_general_rating","end_location_rating","end_price_rating","end_quality_rating"
                ,"end_stock_rating","end_reputation_rating","end_seed_quality_general_rating","end_seed_yield_rating"
                ,"end_seed_drought_rating","end_seed_disease_rating","end_seed_maturing_rating"
                ,"end_seed_germinate_rating"
                ,"end_knows_dealer","end_bought_at_dealer"
                ,"end_customer_years","end_knows_other_customer"
                ,"end_refunds","end_gives_credit"
                ,"end_gives_advice","end_delivers"
                ,"end_after_sales_service","end_payment_mehtods"
                ,"end_small_quant")
endline_rating_dyads[as_numeric] <- lapply(endline_rating_dyads[as_numeric],function(x)as.numeric(as.character(x)))

endline_rating_dyads_aggr_F <- aggregate(endline_rating_dyads,by=list(endline_rating_dyads$farmer_ID),FUN="mean",na.rm=T)

endline_rating_dyads_aggr_F = subset(endline_rating_dyads_aggr_F, select = c(Group.1,end_bought_last_season,end_general_rating
                                                                             ,end_location_rating,end_price_rating,end_quality_rating
                                                                             ,end_stock_rating,end_reputation_rating
                                                                             ,end_seed_quality_general_rating,end_seed_yield_rating
                                                                             ,end_seed_drought_rating,end_seed_disease_rating
                                                                             ,end_seed_maturing_rating,end_seed_germinate_rating
                                                                             ,end_knows_dealer,end_bought_at_dealer
                                                                             ,end_customer_years,end_knows_other_customer
                                                                             ,end_refunds,end_gives_credit
                                                                             ,end_gives_advice,end_delivers
                                                                             ,end_after_sales_service,end_payment_mehtods
                                                                             ,end_small_quant))

baseline_farmers <- merge(baseline_farmers,endline_rating_dyads_aggr_F,by.x="farmer_ID",by.y="Group.1",all.x=TRUE)

#dealers
endline_rating_dyads_aggr_D <- aggregate(endline_rating_dyads,by=list(endline_rating_dyads$shop_ID),FUN="mean",na.rm=T)
endline_rating_dyads_aggr_D = subset(endline_rating_dyads_aggr_D, select = c(Group.1,end_bought_last_season,end_general_rating
                                                                             ,end_location_rating,end_price_rating,end_quality_rating
                                                                             ,end_stock_rating,end_reputation_rating
                                                                             ,end_seed_quality_general_rating,end_seed_yield_rating
                                                                             ,end_seed_drought_rating,end_seed_disease_rating
                                                                             ,end_seed_maturing_rating,end_seed_germinate_rating
                                                                             ,end_knows_dealer,end_bought_at_dealer
                                                                             ,end_customer_years,end_knows_other_customer
                                                                             ,end_refunds,end_gives_credit
                                                                             ,end_gives_advice,end_delivers
                                                                             ,end_after_sales_service,end_payment_mehtods
                                                                             ,end_small_quant))

dealer_endline <- read.csv(paste(path,"/endline/data/agro_input/public/dealer_endline.csv",sep="/"), stringsAsFactors=TRUE)
#dealer_endline=subset(dealer_endline,!shop_ID=="AD_319")

names(dealer_endline)[names(dealer_endline) == "age"] <- "age_end" #because same name in bl and ml
names(dealer_endline)[names(dealer_endline) == "exp"] <- "exp_end"
names(dealer_endline)[names(dealer_endline) == "lot"] <- "lot_end"
names(dealer_endline)[names(dealer_endline) == "cert"] <- "cert_end"
names(dealer_endline)[names(dealer_endline) == "date"] <- "date_end"
names(dealer_endline)[names(dealer_endline) == "verif"] <- "verif_end"
names(dealer_endline)[names(dealer_endline) == "origin"] <- "origin_end"
names(dealer_endline)[names(dealer_endline) == "company"] <- "company_end"
names(dealer_endline)[names(dealer_endline) == "reading"] <- "reading_end"
names(dealer_endline)[names(dealer_endline) == "variety"] <- "variety_end"
names(dealer_endline)[names(dealer_endline) == "date_pack"] <- "date_pack_end"
names(dealer_endline)[names(dealer_endline) == "other_var"] <- "other_var_end"

baseline_dealers <- merge(baseline_dealers,endline_rating_dyads_aggr_D,by.x="shop_ID",by.y="Group.1",all.x=TRUE)
baseline_dealers <- merge(baseline_dealers,dealer_endline,by.x="shop_ID",by.y="shop_ID",all.x=TRUE)

endline_dealers <- merge(endline_rating_dyads_aggr_D,dealer_endline,by.x="Group.1",by.y="shop_ID",all=TRUE)

###
#B# SERVICES (CH treatment farmers were asked during baseline, CH control farmers were asked during CH rating dissemination)
###

#BASELINE
dealer_services_dyads <- read.csv(paste(path,"/Study design/treatments/info_clearing/farmer/data/public/dealer_services_dyads.csv",sep="/"), stringsAsFactors=TRUE)

dealer_services_dyads[dealer_services_dyads=="n/a"] <- NA
dealer_services_dyads[dealer_services_dyads==98] <- NA

dealer_services_dyads$knows_dealer <- ifelse(dealer_services_dyads$knows_dealer=="Yes",1,0)
dealer_services_dyads$bought_at_dealer<-ifelse(dealer_services_dyads$bought_at_dealer=="Yes",1,0)
dealer_services_dyads$customer_years <- 2021 - as.numeric(as.character(substr(dealer_services_dyads$duration_customer,start=1,stop=4))) #dealer baseline data collection in 2020, CH rating dissemination in 2021
dealer_services_dyads$knows_other_customer<-ifelse(dealer_services_dyads$knows_other_customer=="Yes",1,0)
dealer_services_dyads$refunds<-ifelse(dealer_services_dyads$refunds=="Yes",1,0)
dealer_services_dyads$gives_credit<-ifelse(dealer_services_dyads$gives_credit=="Yes",1,0)
dealer_services_dyads$gives_advice<-ifelse(dealer_services_dyads$gives_advice=="Yes",1,0)
dealer_services_dyads$delivers<-ifelse(dealer_services_dyads$delivers=="Yes",1,0)
dealer_services_dyads$after_sales_service<-ifelse(dealer_services_dyads$after_sales_service=="Yes",1,0)
dealer_services_dyads$payment_mehtods<-ifelse(dealer_services_dyads$payment_mehtods=="Yes",1,0)
dealer_services_dyads$small_quant<-ifelse(dealer_services_dyads$small_quant=="Yes",1,0)

as_numeric <- c("knows_dealer","bought_at_dealer","customer_years","knows_other_customer","refunds"
                ,"gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods"
                ,"small_quant")
dealer_services_dyads[as_numeric] <- lapply(dealer_services_dyads[as_numeric],function(x)as.numeric(as.character(x)))

#dealers
dealer_services_dyads_aggr_D <- aggregate(dealer_services_dyads,by=list(dealer_services_dyads$shop_ID),FUN="mean",na.rm=T)
dealer_services_dyads_aggr_D = dealer_services_dyads_aggr_D[c("Group.1","knows_dealer"
                                                              ,"bought_at_dealer","customer_years"
                                                              ,"knows_other_customer","refunds"
                                                              ,"gives_credit","gives_advice"
                                                              ,"delivers","after_sales_service"
                                                              ,"payment_mehtods","small_quant")]
baseline_dealers <- merge(baseline_dealers,dealer_services_dyads_aggr_D,by.x="shop_ID",by.y="Group.1",all.x=TRUE)

#farmers
dealer_services_dyads_aggr_F <- aggregate(dealer_services_dyads,by=list(dealer_services_dyads$farmer_ID),FUN="mean",na.rm=T)
dealer_services_dyads_aggr_F = dealer_services_dyads_aggr_F[c("Group.1","knows_dealer"
                                                              ,"bought_at_dealer","customer_years"
                                                              ,"knows_other_customer","refunds"
                                                              ,"gives_credit","gives_advice"
                                                              ,"delivers","after_sales_service"
                                                              ,"payment_mehtods","small_quant")]
baseline_farmers <- merge(baseline_farmers, dealer_services_dyads_aggr_F, by.x="farmer_ID", by.y="Group.1", all.x = TRUE)

#MIDLINE
midline_dealer_services_dyads <- read.csv(paste(path,"/midline/data/farmer/public/midline_dealer_services_dyads.csv",sep="/"), stringsAsFactors=TRUE)

midline_dealer_services_dyads[midline_dealer_services_dyads=="n/a"] <- NA
midline_dealer_services_dyads[midline_dealer_services_dyads==98] <- NA

midline_dealer_services_dyads$mid_knows_dealer <- ifelse(midline_dealer_services_dyads$knows_dealer=="Yes",1,0)
midline_dealer_services_dyads$mid_bought_at_dealer<-ifelse(midline_dealer_services_dyads$bought_at_dealer=="Yes",1,0)
midline_dealer_services_dyads$mid_customer_years <- 2022 - as.numeric(as.character(substr(midline_dealer_services_dyads$duration_customer,start=1,stop=4))) #CH rating dissemination in 2022
midline_dealer_services_dyads$mid_knows_other_customer<-ifelse(midline_dealer_services_dyads$knows_other_customer=="Yes",1,0)
midline_dealer_services_dyads$mid_refunds<-ifelse(midline_dealer_services_dyads$refunds=="Yes",1,0)
midline_dealer_services_dyads$mid_gives_credit<-ifelse(midline_dealer_services_dyads$gives_credit=="Yes",1,0)
midline_dealer_services_dyads$mid_gives_advice<-ifelse(midline_dealer_services_dyads$gives_advice=="Yes",1,0)
midline_dealer_services_dyads$mid_delivers<-ifelse(midline_dealer_services_dyads$delivers=="Yes",1,0)
midline_dealer_services_dyads$mid_after_sales_service<-ifelse(midline_dealer_services_dyads$after_sales_service=="Yes",1,0)
midline_dealer_services_dyads$mid_payment_mehtods<-ifelse(midline_dealer_services_dyads$payment_mehtods=="Yes",1,0)
midline_dealer_services_dyads$mid_small_quant<-ifelse(midline_dealer_services_dyads$small_quant=="Yes",1,0)

as_numeric <- c("mid_knows_dealer","mid_bought_at_dealer"
                ,"mid_customer_years","mid_knows_other_customer"
                ,"mid_refunds","mid_gives_credit"
                ,"mid_gives_advice","mid_delivers"
                ,"mid_after_sales_service","mid_payment_mehtods"
                ,"mid_small_quant")
midline_dealer_services_dyads[as_numeric] <- lapply(midline_dealer_services_dyads[as_numeric],function(x)as.numeric(as.character(x)))

#dealers
midline_dealer_services_dyads_aggr_D <- aggregate(midline_dealer_services_dyads,by=list(midline_dealer_services_dyads$shop_ID),FUN="mean",na.rm=T)
midline_dealer_services_dyads_aggr_D = subset(midline_dealer_services_dyads_aggr_D, select = c(Group.1,mid_knows_dealer,mid_bought_at_dealer
                                                                                               ,mid_customer_years,mid_knows_other_customer
                                                                                               ,mid_refunds,mid_gives_credit
                                                                                               ,mid_gives_advice,mid_delivers
                                                                                               ,mid_after_sales_service,mid_payment_mehtods
                                                                                               ,mid_small_quant))

midline_dealers <- read.csv(paste(path,"/midline/data/agro_input/public/midline_dealer.csv",sep="/"), stringsAsFactors=TRUE)

names(midline_dealers)[names(midline_dealers) == "age"] <- "age_mid" #because same name in bl and ml
names(midline_dealers)[names(midline_dealers) == "exp"] <- "exp_mid"
names(midline_dealers)[names(midline_dealers) == "lot"] <- "lot_mid"
names(midline_dealers)[names(midline_dealers) == "cert"] <- "cert_mid"
names(midline_dealers)[names(midline_dealers) == "date"] <- "date_mid"
names(midline_dealers)[names(midline_dealers) == "verif"] <- "verif_mid"
names(midline_dealers)[names(midline_dealers) == "origin"] <- "origin_mid"
names(midline_dealers)[names(midline_dealers) == "company"] <- "company_mid"
names(midline_dealers)[names(midline_dealers) == "reading"] <- "reading_mid"
names(midline_dealers)[names(midline_dealers) == "variety"] <- "variety_mid"
names(midline_dealers)[names(midline_dealers) == "date_pack"] <- "date_pack_mid"
names(midline_dealers)[names(midline_dealers) == "other_var"] <- "other_var_mid"

baseline_dealers <- merge(midline_dealer_services_dyads_aggr_D,baseline_dealers,by.x="Group.1",by.y="shop_ID",all=TRUE)
baseline_dealers <- merge(midline_dealers,baseline_dealers,by.x="shop_ID",by.y="Group.1",all=TRUE)

#farmers
midline_dealer_services_dyads_aggr_F <- aggregate(midline_dealer_services_dyads,by=list(midline_dealer_services_dyads$farmer_ID),FUN="mean",na.rm=T)
midline_dealer_services_dyads_aggr_F = subset(midline_dealer_services_dyads_aggr_F, select = c(Group.1,mid_knows_dealer,mid_bought_at_dealer
                                                                                               ,mid_customer_years,mid_knows_other_customer
                                                                                               ,mid_refunds,mid_gives_credit
                                                                                               ,mid_gives_advice,mid_delivers
                                                                                               ,mid_after_sales_service,mid_payment_mehtods
                                                                                               ,mid_small_quant))

midline_farmers <- merge(midline_dealer_services_dyads_aggr_F,midline_farmers,by.x="Group.1",by.y="farmer_ID",all=TRUE)

#ENDLINE: see above


###################################################
#####DESCRIPTIVE STATISTICS + DATA EXPLORATION#####
###################################################

###################################################
#####Descriptive statistics: agro-input dealer#####
###################################################

df_descriptives_dealer <- array(NA,dim=c(99,5))

###variable transformation###
baseline_dealers$maize.owner.agree.gender<-ifelse(baseline_dealers$maize.owner.agree.gender=="Male",1,0)

baseline_dealers$finished_primary[baseline_dealers$maize.owner.agree.educ=="a"] <- 0
baseline_dealers$finished_primary[baseline_dealers$maize.owner.agree.educ=="b"] <- 0
baseline_dealers$finished_primary[baseline_dealers$maize.owner.agree.educ=="c"] <- 1
baseline_dealers$finished_primary[baseline_dealers$maize.owner.agree.educ=="d"] <- 1
baseline_dealers$finished_primary[baseline_dealers$maize.owner.agree.educ=="e"] <- 1
baseline_dealers$finished_primary[baseline_dealers$maize.owner.agree.educ=="f"] <- 1
baseline_dealers$finished_primary[baseline_dealers$maize.owner.agree.educ=="g"] <- NA

baseline_dealers$prim <- FALSE
baseline_dealers$prim <- (baseline_dealers$maize.owner.agree.educ %in% c("c","d","e","f"))

baseline_dealers$finished_secondary[baseline_dealers$maize.owner.agree.educ=="a"] <- 0
baseline_dealers$finished_secondary[baseline_dealers$maize.owner.agree.educ=="b"] <- 0
baseline_dealers$finished_secondary[baseline_dealers$maize.owner.agree.educ=="c"] <- 0
baseline_dealers$finished_secondary[baseline_dealers$maize.owner.agree.educ=="d"] <- 0
baseline_dealers$finished_secondary[baseline_dealers$maize.owner.agree.educ=="e"] <- 1
baseline_dealers$finished_secondary[baseline_dealers$maize.owner.agree.educ=="f"] <- 1
baseline_dealers$finished_secondary[baseline_dealers$maize.owner.agree.educ=="g"] <- NA

baseline_dealers$maize.owner.agree.ownership<-ifelse(baseline_dealers$maize.owner.agree.ownership=="Yes",1,0)
baseline_dealers$maize.owner.agree.q5<-ifelse(baseline_dealers$maize.owner.agree.q5=="Yes",1,0)
baseline_dealers$years_shop <- 2020 - as.numeric(as.character(substr(baseline_dealers$maize.owner.agree.q8, start=1, stop=4)))
baseline_dealers$maize.owner.agree.q9.a<-ifelse(baseline_dealers$maize.owner.agree.q9.a=="True",1,0)
baseline_dealers$maize.owner.agree.q9.b<-ifelse(baseline_dealers$maize.owner.agree.q9.b=="True",1,0)
baseline_dealers$maize.owner.agree.q9.d<-ifelse(baseline_dealers$maize.owner.agree.q9.d=="True",1,0)
baseline_dealers$maize.owner.agree.q9.e<-ifelse(baseline_dealers$maize.owner.agree.q9.e=="True",1,0)
baseline_dealers$maize.owner.agree.q10<-ifelse(baseline_dealers$maize.owner.agree.q10=="Yes",1,0)
baseline_dealers$maize.owner.agree.q11<-ifelse(baseline_dealers$maize.owner.agree.q11=="Yes",1,0)

baseline_dealers$maize.owner.agree.train_ISSD[baseline_dealers$maize.owner.agree.train_ISSD==98] <- NA #because later too late (binary)
baseline_dealers$maize.owner.agree.train_ISSD<-ifelse(baseline_dealers$maize.owner.agree.train_ISSD=="Yes",1,0)

baseline_dealers$maize.owner.agree.q20<-ifelse(baseline_dealers$maize.owner.agree.q20=="Yes",1,0)
baseline_dealers$maize.owner.agree.q32<-ifelse(baseline_dealers$maize.owner.agree.q32=="Yes",1,0)
baseline_dealers$maize.owner.agree.q45<-ifelse(baseline_dealers$maize.owner.agree.q45=="Yes",1,0)
baseline_dealers$maize.owner.agree.q57<-ifelse(baseline_dealers$maize.owner.agree.q57=="Yes",1,0)
baseline_dealers$maize.owner.agree.temp.q69<-ifelse(baseline_dealers$maize.owner.agree.temp.q69=="Yes",1,0)
baseline_dealers$maize.owner.agree.temp.q71<-ifelse(baseline_dealers$maize.owner.agree.temp.q71=="Yes",1,0)
baseline_dealers$maize.owner.agree.temp.q72<-ifelse(baseline_dealers$maize.owner.agree.temp.q72=="Yes",1,0)
baseline_dealers$maize.owner.agree.temp.q73<-ifelse(baseline_dealers$maize.owner.agree.temp.q73=="Yes",1,0)
baseline_dealers$maize.owner.agree.temp.q74<-ifelse(baseline_dealers$maize.owner.agree.temp.q74=="Yes",1,0)
baseline_dealers$maize.owner.agree.temp.q75<-ifelse(baseline_dealers$maize.owner.agree.temp.q75=="Yes",1,0)
baseline_dealers$maize.owner.agree.temp.q76<-ifelse(baseline_dealers$maize.owner.agree.temp.q76=="Yes",1,0)

baseline_dealers$floor[baseline_dealers$maize.owner.agree.temp.q77=="Cement"] <- 1
baseline_dealers$floor[baseline_dealers$maize.owner.agree.temp.q77=="Tiles"] <- 1
baseline_dealers$floor[baseline_dealers$maize.owner.agree.temp.q77=="Mud"] <- 0

baseline_dealers$lighting[baseline_dealers$maize.owner.agree.temp.q78==1] <- 0
baseline_dealers$lighting[baseline_dealers$maize.owner.agree.temp.q78==2] <- 1
baseline_dealers$lighting[baseline_dealers$maize.owner.agree.temp.q78==3] <- 0

baseline_dealers$surface[baseline_dealers$maize.owner.agree.temp.q79==1] <- 0
baseline_dealers$surface[baseline_dealers$maize.owner.agree.temp.q79==2] <- 0
baseline_dealers$surface[baseline_dealers$maize.owner.agree.temp.q79==3] <- 0
baseline_dealers$surface[baseline_dealers$maize.owner.agree.temp.q79==4] <- 1
baseline_dealers$surface[baseline_dealers$maize.owner.agree.temp.q79==5] <- 1
baseline_dealers$surface[baseline_dealers$maize.owner.agree.temp.q79==96] <- NA #because 2 names

baseline_dealers$maize.owner.agree.temp.q80<-ifelse(baseline_dealers$maize.owner.agree.temp.q80=="Yes",1,0)
baseline_dealers$maize.owner.agree.temp.q81<-ifelse(baseline_dealers$maize.owner.agree.temp.q81=="Yes",1,0)
baseline_dealers$maize.owner.agree.q83.a<-ifelse(baseline_dealers$maize.owner.agree.q83.a=="True",1,0)
baseline_dealers$maize.owner.agree.q83.b<-ifelse(baseline_dealers$maize.owner.agree.q83.b=="True",1,0)
baseline_dealers$maize.owner.agree.q83.c<-ifelse(baseline_dealers$maize.owner.agree.q83.c=="True",1,0)
baseline_dealers$maize.owner.agree.q83.d<-ifelse(baseline_dealers$maize.owner.agree.q83.d=="True",1,0)
baseline_dealers$maize.owner.agree.q83.e<-ifelse(baseline_dealers$maize.owner.agree.q83.e=="True",1,0)
baseline_dealers$maize.owner.agree.q83.f<-ifelse(baseline_dealers$maize.owner.agree.q83.f=="True",1,0)
baseline_dealers$maize.owner.agree.q83.g<-ifelse(baseline_dealers$maize.owner.agree.q83.g=="True",1,0)
baseline_dealers$maize.owner.agree.q83.96<-ifelse(baseline_dealers$maize.owner.agree.q83.96=="True",1,0)

baseline_dealers$badpractice_expired <- 0
baseline_dealers$badpractice_expired[baseline_dealers$maize.owner.agree.q83.a==1] <- NA
baseline_dealers$badpractice_expired[baseline_dealers$maize.owner.agree.q83.96==1] <- 1
baseline_dealers$badpractice_expired[baseline_dealers$maize.owner.agree.q83.c==1] <- 1
baseline_dealers$badpractice_expired[baseline_dealers$maize.owner.agree.q83.d==1] <- 1
baseline_dealers$badpractice_expired[baseline_dealers$maize.owner.agree.q83.f==1] <- 1
baseline_dealers$badpractice_expired[baseline_dealers$maize.owner.agree.q83.g==1] <- 1
#there are 2 dealers who answered "This has never happened" and "Other"

baseline_dealers$goodpractice_expired[baseline_dealers$badpractice_expired==0] <- 1
baseline_dealers$goodpractice_expired[baseline_dealers$badpractice_expired==1] <- 0

baseline_dealers$alwaysexplains[baseline_dealers$maize.owner.agree.q85=="a"] <- 0
baseline_dealers$alwaysexplains[baseline_dealers$maize.owner.agree.q85=="b"] <- 0
baseline_dealers$alwaysexplains[baseline_dealers$maize.owner.agree.q85=="c"] <- 1

baseline_dealers$alwaysrecom[baseline_dealers$maize.owner.agree.q86=="a"] <- 0
baseline_dealers$alwaysrecom[baseline_dealers$maize.owner.agree.q86=="b"] <- 0
baseline_dealers$alwaysrecom[baseline_dealers$maize.owner.agree.q86=="c"] <- 1

baseline_dealers$extension[baseline_dealers$maize.owner.agree.q87=="1"] <- 0
baseline_dealers$extension[baseline_dealers$maize.owner.agree.q87=="2"] <- 1
baseline_dealers$extension[baseline_dealers$maize.owner.agree.q87=="3"] <- 1

baseline_dealers$maize.owner.agree.q88<-ifelse(baseline_dealers$maize.owner.agree.q88=="Yes",1,0)

baseline_dealers$q89_bin[baseline_dealers$maize.owner.agree.q89=="1"] <- 1
baseline_dealers$q89_bin[baseline_dealers$maize.owner.agree.q89=="2"] <- 0
baseline_dealers$q89_bin[baseline_dealers$maize.owner.agree.q89=="3"] <- 0
baseline_dealers$q89_bin[baseline_dealers$maize.owner.agree.q89=="4"] <- 0
baseline_dealers$q89_bin[baseline_dealers$maize.owner.agree.q89=="other"] <- NA

baseline_dealers$q90_bin[baseline_dealers$maize.owner.agree.q90=="a"] <- 1
baseline_dealers$q90_bin[baseline_dealers$maize.owner.agree.q90=="b"] <- 1
baseline_dealers$q90_bin[baseline_dealers$maize.owner.agree.q90=="c"] <- 0

baseline_dealers$q91_bin[baseline_dealers$maize.owner.agree.q91=="No"] <- 0
baseline_dealers$q91_bin[baseline_dealers$maize.owner.agree.q91=="Yes"] <- 1

baseline_dealers$q92_bin[baseline_dealers$maize.owner.agree.q92=="No"] <- 0
baseline_dealers$q92_bin[baseline_dealers$maize.owner.agree.q92=="Yes"] <- 1

baseline_dealers$q93_bin[baseline_dealers$maize.owner.agree.q93=="1"] <- 0
baseline_dealers$q93_bin[baseline_dealers$maize.owner.agree.q93=="2"] <- 1
baseline_dealers$q93_bin[baseline_dealers$maize.owner.agree.q93=="3"] <- 1

baseline_dealers$maize.owner.agree.q94[baseline_dealers$maize.owner.agree.q94==0] <- NA
baseline_dealers$maize.owner.agree.q94[baseline_dealers$maize.owner.agree.q94=="n/a"] <- NA
baseline_dealers$maize.owner.agree.q94 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.q94))

baseline_dealers$maize.owner.agree.q95[baseline_dealers$maize.owner.agree.q95==999] <- NA
baseline_dealers$maize.owner.agree.q95[baseline_dealers$maize.owner.agree.q95=="n/a"] <- NA
baseline_dealers$maize.owner.agree.q95 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.q95))

baseline_dealers$maize.owner.agree.q96<-ifelse(baseline_dealers$maize.owner.agree.q96=="Yes",1,0)
baseline_dealers$maize.owner.agree.q97.b<-ifelse(baseline_dealers$maize.owner.agree.q97.b=="True",1,0)

baseline_dealers$q98_binary[baseline_dealers$maize.owner.agree.q98=="a"] <- 0
baseline_dealers$q98_binary[baseline_dealers$maize.owner.agree.q98=="b"] <- 1
baseline_dealers$q98_binary[baseline_dealers$maize.owner.agree.q98=="c"] <- 1

###
baseline_dealers$maize.owner.agree.inspection.q114[baseline_dealers$maize.owner.agree.inspection.q114==98] <- NA #here because binary
baseline_dealers$maize.owner.agree.inspection.q114<-as.character(baseline_dealers$maize.owner.agree.inspection.q114)
baseline_dealers$maize.owner.agree.inspection.q114<-ifelse(baseline_dealers$maize.owner.agree.inspection.q114=="Yes",1,0)

baseline_dealers$maize.owner.agree.inspection.q115[baseline_dealers$maize.owner.agree.inspection.q115==98] <- NA #here because binary
baseline_dealers$maize.owner.agree.inspection.q115<-as.character(baseline_dealers$maize.owner.agree.inspection.q115)
baseline_dealers$maize.owner.agree.inspection.q115<-ifelse(baseline_dealers$maize.owner.agree.inspection.q115=="Yes",1,0)

baseline_dealers$maize.owner.agree.inspection.q116[baseline_dealers$maize.owner.agree.inspection.q116==98] <- NA #here because binary
baseline_dealers$maize.owner.agree.inspection.q116<-as.character(baseline_dealers$maize.owner.agree.inspection.q116)
baseline_dealers$maize.owner.agree.inspection.q116<-ifelse(baseline_dealers$maize.owner.agree.inspection.q116=="Yes",1,0)

baseline_dealers$maize.owner.agree.inspection.q118[baseline_dealers$maize.owner.agree.inspection.q118==98] <- NA
baseline_dealers$maize.owner.agree.inspection.q118<-as.character(baseline_dealers$maize.owner.agree.inspection.q118)
baseline_dealers$maize.owner.agree.inspection.q118<-ifelse(baseline_dealers$maize.owner.agree.inspection.q118=="Yes",1,0)

baseline_dealers$maize.owner.agree.inspection.q119[baseline_dealers$maize.owner.agree.inspection.q119==98] <- NA
baseline_dealers$maize.owner.agree.inspection.q119<-as.character(baseline_dealers$maize.owner.agree.inspection.q119)
baseline_dealers$maize.owner.agree.inspection.q119<-ifelse(baseline_dealers$maize.owner.agree.inspection.q119=="Yes",1,0)

baseline_dealers$maize.owner.agree.inspection.q120[baseline_dealers$maize.owner.agree.inspection.q120==98] <- NA
baseline_dealers$maize.owner.agree.inspection.q120<-as.character(baseline_dealers$maize.owner.agree.inspection.q120)
baseline_dealers$maize.owner.agree.inspection.q120<-ifelse(baseline_dealers$maize.owner.agree.inspection.q120=="Yes",1,0)

baseline_dealers$maize.owner.agree.inspection.q121<-ifelse(baseline_dealers$maize.owner.agree.inspection.q121=="Yes",1,0)
baseline_dealers$maize.owner.agree.inspection.q122<-ifelse(baseline_dealers$maize.owner.agree.inspection.q122=="Yes",1,0)

###

baseline_dealers$visible_expdate<-ifelse(baseline_dealers$exp=="n/a",0,1) #2 names

baseline_dealers$seed_expired <- 0
baseline_dealers$seed_expired[is.na(baseline_dealers$exp)] <- NA
baseline_dealers$seed_expired[baseline_dealers$exp=="n/a"] <- NA
baseline_dealers$seed_expired[is.na(baseline_dealers$date)] <- NA
baseline_dealers$seed_expired[baseline_dealers$date=="n/a"] <- NA
#1 shop has n/a for date but not for exp
baseline_dealers$date <- as.Date(baseline_dealers$date)
baseline_dealers$exp[baseline_dealers$exp=="n/a"] <- NA
baseline_dealers$exp <- as.Date(baseline_dealers$exp)
baseline_dealers$days_since_exp <- baseline_dealers$date - baseline_dealers$exp
baseline_dealers$seed_expired[baseline_dealers$days_since_exp > 0] <- 1

baseline_dealers$visible_packdate<-ifelse(baseline_dealers$date_pack=="n/a",0,1)

baseline_dealers$seedolderthan6m <- 0
baseline_dealers$seedolderthan6m[is.na(baseline_dealers$date_pack)] <- NA
baseline_dealers$seedolderthan6m[baseline_dealers$date_pack=="n/a"] <- NA
baseline_dealers$seedolderthan6m[is.na(baseline_dealers$date)] <- NA
baseline_dealers$date_pack <- as.Date(baseline_dealers$date_pack)
baseline_dealers$shelflife <- baseline_dealers$date - baseline_dealers$date_pack
baseline_dealers$seedolderthan6m[baseline_dealers$shelflife > 183] <- 1 #6x366/12

# #compare my "shelflife" with Bjorn's "age"
# #my "shelflife"
# summary(as.numeric(baseline_dealers$shelflife))
# # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# #  -15.00   32.25   58.50   62.40   77.75  261.00     194
# #Bjorn's "age" in baseline_dealers
# summary(baseline_dealers$age)
# # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# #  -15.00   33.00   52.00   66.35   79.00  259.96     159
# #Bjorn's "age" when using only his first line of code
# baseline_dealers$age2 <- difftime(strptime("01.10.2020", format = "%d.%m.%Y"),strptime(baseline_dealers$date_pack,format="%Y-%m-%d"),units="days")
# summary(as.numeric(baseline_dealers$age2))
# # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# #  -15.00   35.00   57.00   64.97   79.00  259.96     191
# #my "shelflife" after using 01/10/21
# baseline_dealers$shelflife2 <- as.Date("2020-10-01") - as.Date(baseline_dealers$date_pack)
# summary(as.numeric(baseline_dealers$shelflife2))
# #Bjorn's "age" when using only his first line of code & my "shelflife2" after using 01/10/21 MATCH
# sum(is.na(baseline_dealers$date_pack) & !is.na(baseline_dealers$exp))
# #32 shop have an NA for date_pack but no NA for exp
# #baseline_dealers$date_pack2 <- ifelse(is.na(baseline_dealers$date_pack), as.Date(baseline_dealers$exp), as.Date(baseline_dealers$date_pack))
# baseline_dealers$date_pack_incltransformedexp<-baseline_dealers$date_pack
# baseline_dealers$transformedexp <- baseline_dealers$exp - 180
# baseline_dealers$date_pack_incltransformedexp[is.na(baseline_dealers$date_pack)]<-baseline_dealers$transformedexp[is.na(baseline_dealers$date_pack)]
# baseline_dealers$shelflife3 <- as.Date("2020-10-01") - as.Date(baseline_dealers$date_pack_incltransformedexp)
# summary(as.numeric(baseline_dealers$age))
# summary(as.numeric(baseline_dealers$shelflife3))

baseline_dealers$date_pack_incltransformedexp <- baseline_dealers$date_pack
baseline_dealers$transformedexp <- baseline_dealers$exp - 183 #6x366/12
baseline_dealers$date_pack_incltransformedexp[is.na(baseline_dealers$date_pack)]<-baseline_dealers$transformedexp[is.na(baseline_dealers$date_pack)]
baseline_dealers$shelflife_Caro <- baseline_dealers$date - as.Date(baseline_dealers$date_pack_incltransformedexp)
baseline_dealers$shelflife_Caro[baseline_dealers$shelflife_Caro < 0] <- NA
baseline_dealers$shelflife_Caro <- as.numeric(as.character(baseline_dealers$shelflife_Caro))
#nobs: 348 obs - 159 NA's for Bjorn's "age" in baseline_dealers or my shelflife3 - 3 sum(is.na(baseline_dealers$date)) - 3 packaging date after interview

baseline_dealers$origin<-ifelse(baseline_dealers$origin=="Yes",1,0)
baseline_dealers$cert<-ifelse(baseline_dealers$cert=="Yes",1,0)
baseline_dealers$lot<-ifelse(baseline_dealers$lot=="Yes",1,0)
baseline_dealers$verif<-ifelse(baseline_dealers$verif=="Yes",1,0)

###loop###
variables <- c("maize.owner.agree.age","maize.owner.agree.gender","finished_primary","finished_secondary","maize.owner.agree.ownership"
               ,"maize.owner.agree.q3","maize.owner.agree.q4","maize.owner.agree.q5","maize.owner.agree.q6","maize.owner.agree.q7","years_shop"
               ,"maize.owner.agree.q9.a","maize.owner.agree.q9.b","maize.owner.agree.q9.d","maize.owner.agree.q9.e","maize.owner.agree.q10"
               ,"maize.owner.agree.q11","maize.owner.agree.train_ISSD","maize.owner.agree.nr_var","maize.owner.agree.q19"
               ,"maize.owner.agree.q20","maize.owner.agree.q32","maize.owner.agree.q44","maize.owner.agree.q45","maize.owner.agree.q57"
               ,"maize.owner.agree.temp.q69","maize.owner.agree.temp.q71","maize.owner.agree.temp.q72","maize.owner.agree.temp.q73"
               ,"maize.owner.agree.temp.q74","maize.owner.agree.temp.q75","maize.owner.agree.temp.q76","floor","lighting","surface"
               ,"maize.owner.agree.temp.q80","maize.owner.agree.temp.q81","maize.owner.agree.temp.q82","maize.owner.agree.q83.a"
               ,"goodpractice_expired","alwaysexplains","alwaysrecom","extension","maize.owner.agree.q88","q89_bin","q90_bin","q91_bin"
               ,"q92_bin","q93_bin","maize.owner.agree.q94","maize.owner.agree.q95","maize.owner.agree.q96","maize.owner.agree.q97.b"
               ,"q98_binary","maize.owner.agree.q99","maize.owner.agree.q100","maize.owner.agree.q101","maize.owner.agree.q102"
               ,"maize.owner.agree.q103","maize.owner.agree.inspection.q114","maize.owner.agree.inspection.q115"
               ,"maize.owner.agree.inspection.q116","maize.owner.agree.inspection.q117","maize.owner.agree.inspection.q118"
               ,"maize.owner.agree.inspection.q119","maize.owner.agree.inspection.q120","maize.owner.agree.inspection.q121"
               ,"maize.owner.agree.inspection.q122","maize.owner.agree.q70","reading","visible_expdate","seed_expired","visible_packdate"
               ,"seedolderthan6m","shelflife_Caro","origin","cert","lot","verif"
               ,"knows_dealer","bought_at_dealer","customer_years","knows_other_customer","refunds"
               ,"gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods"
               ,"small_quant"
               ,"general","yield","drought_resistent","disease_resistent","early_maturing"
               ,"germination")

baseline_dealers[variables] <- lapply(baseline_dealers[variables],function(x) replace(x,x==999,NA))
baseline_dealers[variables] <- lapply(baseline_dealers[variables],function(x) replace(x,x==98,NA))
baseline_dealers[variables] <- lapply(baseline_dealers[variables],function(x) replace(x,x==96,NA))
baseline_dealers[variables] <- lapply(baseline_dealers[variables],function(x) replace(x,x=="n/a",NA))

for (i in 1:length(variables)) {
  df_descriptives_dealer[i,1] <- sum(baseline_dealers[variables[i]], na.rm=T)/(nrow(baseline_dealers)-sum(is.na(baseline_dealers[variables[i]])))
  df_descriptives_dealer[i,2] <- min(baseline_dealers[variables[i]], na.rm=T)
  df_descriptives_dealer[i,3] <- max(baseline_dealers[variables[i]], na.rm=T)
  df_descriptives_dealer[i,4] <- sqrt(var(baseline_dealers[variables[i]], na.rm=T))
  df_descriptives_dealer[i,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers[variables[i]]))}

########################################
#####Descriptive statistics: farmer#####
########################################

df_descriptives_farmer <- array(NA,dim=c(130,5))

###variable transformation###

baseline_farmers[baseline_farmers==999] <- NA
#baseline_farmers[baseline_farmers==96] <- NA
baseline_farmers[, 4:135][baseline_farmers[, 4:135] == 96] <- NA #columns 4-94 only
#baseline_farmers[baseline_farmers==98] <- NA
baseline_farmers[, 4:135][baseline_farmers[, 4:135] == 98] <- NA
baseline_farmers[baseline_farmers=="n/a"] <- NA

baseline_farmers$Check2.check.maize.q15<-ifelse(baseline_farmers$Check2.check.maize.q15=="Male",1,0)
baseline_farmers$married<-ifelse(baseline_farmers$Check2.check.maize.q16=="a",1,0)
baseline_farmers$Check2.check.maize.q17[baseline_farmers$Check2.check.maize.q17=="g"] <- NA
baseline_farmers$noformaleducation<-ifelse(baseline_farmers$Check2.check.maize.q17=="a",1,0)

baseline_farmers$finishedprimary[baseline_farmers$Check2.check.maize.q17=="a"] <- 0
baseline_farmers$finishedprimary[baseline_farmers$Check2.check.maize.q17=="b"] <- 0
baseline_farmers$finishedprimary[baseline_farmers$Check2.check.maize.q17=="c"] <- 1
baseline_farmers$finishedprimary[baseline_farmers$Check2.check.maize.q17=="d"] <- 1
baseline_farmers$finishedprimary[baseline_farmers$Check2.check.maize.q17=="e"] <- 1
baseline_farmers$finishedprimary[baseline_farmers$Check2.check.maize.q17=="f"] <- 1
#baseline_farmers$finishedprimary[baseline_farmers$Check2.check.maize.q17=="g"] <- 0

baseline_farmers$finishedsecondary[baseline_farmers$Check2.check.maize.q17=="a"] <- 0
baseline_farmers$finishedsecondary[baseline_farmers$Check2.check.maize.q17=="b"] <- 0
baseline_farmers$finishedsecondary[baseline_farmers$Check2.check.maize.q17=="c"] <- 0
baseline_farmers$finishedsecondary[baseline_farmers$Check2.check.maize.q17=="d"] <- 0
baseline_farmers$finishedsecondary[baseline_farmers$Check2.check.maize.q17=="e"] <- 1
baseline_farmers$finishedsecondary[baseline_farmers$Check2.check.maize.q17=="f"] <- 1

baseline_farmers$goodroof[baseline_farmers$Check2.check.maize.q21=="a"] <- 0
baseline_farmers$goodroof[baseline_farmers$Check2.check.maize.q21=="b"] <- 1
baseline_farmers$goodroof[baseline_farmers$Check2.check.maize.q21=="c"] <- 1

baseline_farmers$yearsmaize <- 2021 - as.numeric(as.character(substr(baseline_farmers$Check2.check.maize.q23, start=1, stop=4)))
baseline_farmers$Check2.check.maize.q24<-ifelse(baseline_farmers$Check2.check.maize.q24=="Yes",1,0)
baseline_farmers$Check2.check.maize.q25a<-ifelse(baseline_farmers$Check2.check.maize.q25a=="Yes",1,0)

baseline_farmers$farmersavedseed[baseline_farmers$Check2.check.maize.q25b=="a"] <- 1
baseline_farmers$farmersavedseed[baseline_farmers$Check2.check.maize.q25b=="b"] <- 1
baseline_farmers$farmersavedseed[baseline_farmers$Check2.check.maize.q25b=="c"] <- 0
baseline_farmers$farmersavedseed[baseline_farmers$Check2.check.maize.q25b=="d"] <- 0
baseline_farmers$farmersavedseed[baseline_farmers$Check2.check.maize.q25b=="e"] <- 0
baseline_farmers$farmersavedseed[baseline_farmers$Check2.check.maize.q25b=="f"] <- 0
baseline_farmers$farmersavedseed[baseline_farmers$Check2.check.maize.q25b=="h"] <- 0
baseline_farmers$farmersavedseed[baseline_farmers$Check2.check.maize.q25b=="i"] <- 0

baseline_farmers$boughtfromagroinputshop[baseline_farmers$Check2.check.maize.q25b=="a"] <- 0
baseline_farmers$boughtfromagroinputshop[baseline_farmers$Check2.check.maize.q25b=="b"] <- 0
baseline_farmers$boughtfromagroinputshop[baseline_farmers$Check2.check.maize.q25b=="c"] <- 0
baseline_farmers$boughtfromagroinputshop[baseline_farmers$Check2.check.maize.q25b=="d"] <- 1
baseline_farmers$boughtfromagroinputshop[baseline_farmers$Check2.check.maize.q25b=="e"] <- 0
baseline_farmers$boughtfromagroinputshop[baseline_farmers$Check2.check.maize.q25b=="f"] <- 0
baseline_farmers$boughtfromagroinputshop[baseline_farmers$Check2.check.maize.q25b=="h"] <- 0
baseline_farmers$boughtfromagroinputshop[baseline_farmers$Check2.check.maize.q25b=="i"] <- 0

#baseline_farmers$boughtfromagroinputshop2[is.na(baseline_farmers$Check2.check.maize.q25b)] <- 0
baseline_farmers$boughtfromagroinputshop2[baseline_farmers$Check2.check.maize.q25b=="a"] <- 0
baseline_farmers$boughtfromagroinputshop2[baseline_farmers$Check2.check.maize.q25b=="b"] <- 0
baseline_farmers$boughtfromagroinputshop2[baseline_farmers$Check2.check.maize.q25b=="c"] <- 0
baseline_farmers$boughtfromagroinputshop2[baseline_farmers$Check2.check.maize.q25b=="d"] <- 1
baseline_farmers$boughtfromagroinputshop2[baseline_farmers$Check2.check.maize.q25b=="e"] <- 0
baseline_farmers$boughtfromagroinputshop2[baseline_farmers$Check2.check.maize.q25b=="f"] <- 0
baseline_farmers$boughtfromagroinputshop2[baseline_farmers$Check2.check.maize.q25b=="h"] <- 0
baseline_farmers$boughtfromagroinputshop2[baseline_farmers$Check2.check.maize.q25b=="i"] <- 0
baseline_farmers$boughtfromagroinputshop2[baseline_farmers$Check2.check.maize.q25a==0] <- 0

baseline_farmers$thirdormore_time_used[baseline_farmers$Check2.check.maize.q25c=="a"] <- 0
baseline_farmers$thirdormore_time_used[baseline_farmers$Check2.check.maize.q25c=="b"] <- 0
baseline_farmers$thirdormore_time_used[baseline_farmers$Check2.check.maize.q25c=="c"] <- 1
baseline_farmers$thirdormore_time_used[baseline_farmers$Check2.check.maize.q25c=="d"] <- 1
baseline_farmers$thirdormore_time_used[baseline_farmers$Check2.check.maize.q25c=="e"] <- 1
baseline_farmers$thirdormore_time_used[baseline_farmers$Check2.check.maize.q25c=="f"] <- 1

baseline_farmers$Check2.check.maize.q25d <- as.numeric((as.character(baseline_farmers$Check2.check.maize.q25d)))
baseline_farmers$Check2.check.maize.q25d2 <- baseline_farmers$Check2.check.maize.q25d
baseline_farmers$Check2.check.maize.q25d2[is.na(baseline_farmers$Check2.check.maize.q25d)] = 0

baseline_farmers$tooexpensive[baseline_farmers$Check2.check.maize.q25f=="1"] <- 0
baseline_farmers$tooexpensive[baseline_farmers$Check2.check.maize.q25f=="2"] <- 1
baseline_farmers$tooexpensive[baseline_farmers$Check2.check.maize.q25f=="3"] <- 0
baseline_farmers$tooexpensive[baseline_farmers$Check2.check.maize.q25f=="4"] <- 0
baseline_farmers$tooexpensive[baseline_farmers$Check2.check.maize.q25f=="5"] <- 0
baseline_farmers$tooexpensive[baseline_farmers$Check2.check.maize.q25f=="6"] <- NA
baseline_farmers$tooexpensive[baseline_farmers$Check2.check.maize.q25f=="n/a"] <- NA

baseline_farmers$notgoodquality[baseline_farmers$Check2.check.maize.q25f=="1"] <- 0
baseline_farmers$notgoodquality[baseline_farmers$Check2.check.maize.q25f=="2"] <- 0
baseline_farmers$notgoodquality[baseline_farmers$Check2.check.maize.q25f=="3"] <- 1
baseline_farmers$notgoodquality[baseline_farmers$Check2.check.maize.q25f=="4"] <- 0
baseline_farmers$notgoodquality[baseline_farmers$Check2.check.maize.q25f=="5"] <- 0
baseline_farmers$notgoodquality[baseline_farmers$Check2.check.maize.q25f=="6"] <- NA
baseline_farmers$notgoodquality[baseline_farmers$Check2.check.maize.q25f=="n/a"] <- NA

baseline_farmers$verygoodquality[baseline_farmers$Check2.check.maize.q25f_2=="1"] <- 0
baseline_farmers$verygoodquality[baseline_farmers$Check2.check.maize.q25f_2=="2"] <- 0
baseline_farmers$verygoodquality[baseline_farmers$Check2.check.maize.q25f_2=="3"] <- 1
baseline_farmers$verygoodquality[baseline_farmers$Check2.check.maize.q25f_2=="4"] <- 0
baseline_farmers$verygoodquality[baseline_farmers$Check2.check.maize.q25f_2=="5"] <- 0
baseline_farmers$verygoodquality[baseline_farmers$Check2.check.maize.q25f_2=="6"] <- NA
baseline_farmers$verygoodquality[baseline_farmers$Check2.check.maize.q25f_2=="n/a"] <- NA

baseline_farmers$yieldtoolow[baseline_farmers$Check2.check.maize.q25g==1] <- 1
baseline_farmers$yieldtoolow[baseline_farmers$Check2.check.maize.q25g=="2"] <- 0
baseline_farmers$yieldtoolow[baseline_farmers$Check2.check.maize.q25g=="3"] <- 0
baseline_farmers$yieldtoolow[baseline_farmers$Check2.check.maize.q25g=="4"] <- 0
baseline_farmers$yieldtoolow[baseline_farmers$Check2.check.maize.q25g=="5"] <- 0
baseline_farmers$yieldtoolow[baseline_farmers$Check2.check.maize.q25g=="6"] <- 0
baseline_farmers$yieldtoolow[baseline_farmers$Check2.check.maize.q25g=="7"] <- 0
baseline_farmers$yieldtoolow[baseline_farmers$Check2.check.maize.q25g=="8"] <- 0
baseline_farmers$yieldtoolow[baseline_farmers$Check2.check.maize.q25g=="9"] <- 0
baseline_farmers$yieldtoolow[baseline_farmers$Check2.check.maize.q25g=="10"] <- 0
baseline_farmers$yieldtoolow[baseline_farmers$Check2.check.maize.q25g==96] <- NA
baseline_farmers$yieldtoolow[baseline_farmers$Check2.check.maize.q25g=="n/a"] <- NA

baseline_farmers$lesspesttolerant[baseline_farmers$Check2.check.maize.q25g==1] <- 0
baseline_farmers$lesspesttolerant[baseline_farmers$Check2.check.maize.q25g=="2"] <- 0
baseline_farmers$lesspesttolerant[baseline_farmers$Check2.check.maize.q25g=="3"] <- 1
baseline_farmers$lesspesttolerant[baseline_farmers$Check2.check.maize.q25g=="4"] <- 0
baseline_farmers$lesspesttolerant[baseline_farmers$Check2.check.maize.q25g=="5"] <- 0
baseline_farmers$lesspesttolerant[baseline_farmers$Check2.check.maize.q25g=="6"] <- 0
baseline_farmers$lesspesttolerant[baseline_farmers$Check2.check.maize.q25g=="7"] <- 0
baseline_farmers$lesspesttolerant[baseline_farmers$Check2.check.maize.q25g=="8"] <- 0
baseline_farmers$lesspesttolerant[baseline_farmers$Check2.check.maize.q25g=="9"] <- 0
baseline_farmers$lesspesttolerant[baseline_farmers$Check2.check.maize.q25g=="10"] <- 0
baseline_farmers$lesspesttolerant[baseline_farmers$Check2.check.maize.q25g==96] <- NA
baseline_farmers$lesspesttolerant[baseline_farmers$Check2.check.maize.q25g=="n/a"] <- NA

baseline_farmers$lowgermination[baseline_farmers$Check2.check.maize.q25g==1] <- 0
baseline_farmers$lowgermination[baseline_farmers$Check2.check.maize.q25g=="2"] <- 0
baseline_farmers$lowgermination[baseline_farmers$Check2.check.maize.q25g=="3"] <- 0
baseline_farmers$lowgermination[baseline_farmers$Check2.check.maize.q25g=="4"] <- 0
baseline_farmers$lowgermination[baseline_farmers$Check2.check.maize.q25g=="5"] <- 0
baseline_farmers$lowgermination[baseline_farmers$Check2.check.maize.q25g=="6"] <- 0
baseline_farmers$lowgermination[baseline_farmers$Check2.check.maize.q25g=="7"] <- 1
baseline_farmers$lowgermination[baseline_farmers$Check2.check.maize.q25g=="8"] <- 0
baseline_farmers$lowgermination[baseline_farmers$Check2.check.maize.q25g=="9"] <- 0
baseline_farmers$lowgermination[baseline_farmers$Check2.check.maize.q25g=="10"] <- 0
baseline_farmers$lowgermination[baseline_farmers$Check2.check.maize.q25g==96] <- NA
baseline_farmers$lowgermination[baseline_farmers$Check2.check.maize.q25g=="n/a"] <- NA

baseline_farmers$Check2.check.maize.q25h<-ifelse(baseline_farmers$Check2.check.maize.q25h=="Yes",1,0)
baseline_farmers$Check2.check.maize.q26.Longe_5<-ifelse(baseline_farmers$Check2.check.maize.q26.Longe_5=="True",1,0)
baseline_farmers$Check2.check.maize.q26.Longe_7R_Kayongo.go<-ifelse(baseline_farmers$Check2.check.maize.q26.Longe_7R_Kayongo.go=="True",1,0)
baseline_farmers$Check2.check.maize.q26.Wema<-ifelse(baseline_farmers$Check2.check.maize.q26.Wema=="True",1,0)
baseline_farmers$q27bin<-ifelse(baseline_farmers$Check2.check.maize.q27==1,1,0)
baseline_farmers$Check2.check.maize.q30<-ifelse(baseline_farmers$Check2.check.maize.q30=="Yes",1,0)

baseline_farmers$Check2.check.maize.q30a.1<-ifelse(baseline_farmers$Check2.check.maize.q30a.1=="True",1,0)
baseline_farmers$Check2.check.maize.q30a.1[is.na(baseline_farmers$Check2.check.maize.q30a.1)] <- 0

baseline_farmers$Check2.check.maize.q30a.2<-ifelse(baseline_farmers$Check2.check.maize.q30a.2=="True",1,0)
baseline_farmers$Check2.check.maize.q30a.2[is.na(baseline_farmers$Check2.check.maize.q30a.2)] <- 0

baseline_farmers$Check2.check.maize.q30a.3<-ifelse(baseline_farmers$Check2.check.maize.q30a.3=="True",1,0)
baseline_farmers$Check2.check.maize.q30a.3[is.na(baseline_farmers$Check2.check.maize.q30a.3)] <- 0

baseline_farmers$Check2.check.maize.q30a.4<-ifelse(baseline_farmers$Check2.check.maize.q30a.4=="True",1,0)
baseline_farmers$Check2.check.maize.q30a.4[is.na(baseline_farmers$Check2.check.maize.q30a.4)] <- 0

baseline_farmers$Check2.check.maize.q30a.5<-ifelse(baseline_farmers$Check2.check.maize.q30a.5=="True",1,0)
baseline_farmers$Check2.check.maize.q30a.5[is.na(baseline_farmers$Check2.check.maize.q30a.5)] <- 0

baseline_farmers$Check2.check.maize.q30a.6<-ifelse(baseline_farmers$Check2.check.maize.q30a.6=="True",1,0)
baseline_farmers$Check2.check.maize.q30a.6[is.na(baseline_farmers$Check2.check.maize.q30a.6)] <- 0

baseline_farmers$Check2.check.maize.q30b <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q30b))

#hereq31
baseline_farmers$hybrid<-((baseline_farmers$Check2.check.maize.q31=="Longe_10H")|(baseline_farmers$Check2.check.maize.q31=="Longe_7H")|(baseline_farmers$Check2.check.maize.q31=="Longe_7R_Kayongo-go")|(baseline_farmers$Check2.check.maize.q31=="Bazooka")|(baseline_farmers$Check2.check.maize.q31=="Longe_6H")|(baseline_farmers$Check2.check.maize.q31=="Panner")|(baseline_farmers$Check2.check.maize.q31=="Wema")|(baseline_farmers$Check2.check.maize.q31=="KH_series"))
baseline_farmers$hybrid<-ifelse(baseline_farmers$hybrid=="TRUE",1,0)
baseline_farmers$hybrid[baseline_farmers$Check2.check.maize.q31=="Other_hybrid"] <- NA

baseline_farmers$OPV<-(baseline_farmers$Check2.check.maize.q31=="Longe_5")|(baseline_farmers$Check2.check.maize.q31=="Longe_4")
baseline_farmers$OPV<-ifelse(baseline_farmers$OPV=="TRUE",1,0)
baseline_farmers$OPV[baseline_farmers$Check2.check.maize.q31=="Other_hybrid"] <- NA

baseline_farmers$Land_Races<-(baseline_farmers$Check2.check.maize.q31=="Land_Races")
baseline_farmers$Land_Races<-ifelse(baseline_farmers$Land_Races=="TRUE",1,0)

baseline_farmers$improved<-((baseline_farmers$Check2.check.maize.q31=="Longe_10H")|(baseline_farmers$Check2.check.maize.q31=="Longe_7H")|(baseline_farmers$Check2.check.maize.q31=="Longe_7R_Kayongo-go")|(baseline_farmers$Check2.check.maize.q31=="Bazooka")|(baseline_farmers$Check2.check.maize.q31=="Longe_6H")|(baseline_farmers$Check2.check.maize.q31=="Panner")|(baseline_farmers$Check2.check.maize.q31=="Wema")|(baseline_farmers$Check2.check.maize.q31=="KH_series"|baseline_farmers$Check2.check.maize.q31=="Longe_5")|(baseline_farmers$Check2.check.maize.q31=="Longe_4")|(baseline_farmers$Check2.check.maize.q31=="Other_hybrid"))
baseline_farmers$improved<-ifelse(baseline_farmers$improved=="TRUE",1,0)

baseline_farmers$farmer_saved_seed<-((baseline_farmers$Check2.check.maize.q32=="a")|(baseline_farmers$Check2.check.maize.q32=="b"))
baseline_farmers$farmer_saved_seed<-ifelse(baseline_farmers$farmer_saved_seed=="TRUE",1,0)

baseline_farmers$Bought_from_agro_input_shop<-ifelse(baseline_farmers$Check2.check.maize.q32=="d",1,0)

baseline_farmers$hybridbutsaved <- NA
baseline_farmers$hybridbutsaved[baseline_farmers$hybrid == 1 & baseline_farmers$farmer_saved_seed == 1] <- 1
baseline_farmers$hybridbutsaved[baseline_farmers$hybrid == 1 & baseline_farmers$farmer_saved_seed == 0] <- 0
baseline_farmers$hybridbutsaved[baseline_farmers$hybrid == 0] <- 0

baseline_farmers$fourthormore_timeused<-((baseline_farmers$Check2.check.maize.q34=="d")|(baseline_farmers$Check2.check.maize.q34=="e")|(baseline_farmers$Check2.check.maize.q34=="f"))
baseline_farmers$fourthormore_timeused<-ifelse(baseline_farmers$fourthormore_timeused=="TRUE",1,0)
baseline_farmers$OPVbutfourthormore_timeused <- NA
baseline_farmers$OPVbutfourthormore_timeused[baseline_farmers$OPV==1 & baseline_farmers$farmer_saved_seed==1 & baseline_farmers$fourthormore_timeused==1] <- 1
baseline_farmers$OPVbutfourthormore_timeused[baseline_farmers$OPV==1 & baseline_farmers$farmer_saved_seed==1 & baseline_farmers$fourthormore_timeused==0] <- 0
baseline_farmers$OPVbutfourthormore_timeused[baseline_farmers$OPV==1 & baseline_farmers$farmer_saved_seed==0] <- 0
baseline_farmers$OPVbutfourthormore_timeused[baseline_farmers$OPV == 0] <- 0

baseline_farmers$adoption_onfield <- baseline_farmers$improved
baseline_farmers$adoption_onfield[baseline_farmers$hybridbutsaved==1] <- 0
baseline_farmers$adoption_onfield[baseline_farmers$OPVbutfourthormore_timeused==1] <- 0
#hereq31

baseline_farmers$Check2.check.maize.q36<-ifelse(baseline_farmers$Check2.check.maize.q36=="Yes",1,0)
baseline_farmers$Check2.check.maize.q36b<-ifelse(baseline_farmers$Check2.check.maize.q36b=="Yes",1,0)
baseline_farmers$Check2.check.maize.q37<-ifelse(baseline_farmers$Check2.check.maize.q37=="Yes",1,0)
baseline_farmers$Check2.check.maize.q39 <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q39))
baseline_farmers$Check2.check.maize.q39[baseline_farmers$Check2.check.maize.q32=="a"] <- 0
baseline_farmers$costforseed <- baseline_farmers$Check2.check.maize.q38*(as.numeric(as.character(baseline_farmers$Check2.check.maize.q39)))

baseline_farmers$correctseedspacing<-ifelse(baseline_farmers$Check2.check.maize.q40=="c",1,0)
baseline_farmers$correctseedspacing[is.na(baseline_farmers$Check2.check.maize.q40)] <- 0

baseline_farmers$Check2.check.maize.q41 <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q41))
baseline_farmers$correctnumberofseeds<-ifelse(baseline_farmers$Check2.check.maize.q41==1,1,0)
baseline_farmers$Check2.check.maize.q42<-ifelse(baseline_farmers$Check2.check.maize.q42=="Yes",1,0)
baseline_farmers$Check2.check.maize.q43<-ifelse(baseline_farmers$Check2.check.maize.q43=="Yes",1,0)
baseline_farmers$Check2.check.maize.q43a <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q43a))

baseline_farmers$Check2.check.maize.q44<-ifelse(baseline_farmers$Check2.check.maize.q44=="Yes",1,0)
baseline_farmers$Check2.check.maize.q44 <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q44))

baseline_farmers$Check2.check.maize.q44a <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q44a))

baseline_farmers$correctweeding<-((baseline_farmers$Check2.check.maize.q45>=3))
baseline_farmers$correctweeding<-ifelse(baseline_farmers$correctweeding=="TRUE",1,0)

baseline_farmers$Check2.check.maize.q46 <- (as.numeric(as.character(baseline_farmers$Check2.check.maize.q46)))

baseline_farmers$correcttimeweeding <- (baseline_farmers$Check2.check.maize.q46 <= 20 & baseline_farmers$Check2.check.maize.q46 >= 18)
baseline_farmers$correcttimeweeding<-ifelse(baseline_farmers$correcttimeweeding=="TRUE",1,0)

baseline_farmers$Check2.check.maize.q47<-ifelse(baseline_farmers$Check2.check.maize.q47=="Yes",1,0)
baseline_farmers$correctplanting<-ifelse(baseline_farmers$Check2.check.maize.q48==2,1,0)
baseline_farmers$Check2.check.maize.q49<-ifelse(baseline_farmers$Check2.check.maize.q49=="Yes",1,0)

#area if intercropped
baseline_farmers$area_intercropped <- baseline_farmers$Check2.check.maize.q29*baseline_farmers$Check2.check.maize.q30b/100
#area if not intercropped
baseline_farmers$area_not_intercropped <- baseline_farmers$Check2.check.maize.q29
#area both
baseline_farmers$area<-ifelse(baseline_farmers$Check2.check.maize.q30=="Yes",baseline_farmers$area_intercropped,baseline_farmers$area_not_intercropped)
#IGNORING INTERCROPPING BECAUSE WILBER SAYS SO.
#IF MAIZE IS INTERCROPPED, MAIZE IS OFTEN (ALMOST ALWAYS) THE MAIN CROP.
#INTERCROPPED OR NOT, THERE'S AN EQUAL NUMBER OF MAIZE CROPS.
baseline_farmers$yield_inkg <- baseline_farmers$Check2.check.maize.q50*baseline_farmers$Check2.check.maize.q51 #production in kg
baseline_farmers$landproductivity <- baseline_farmers$yield_inkg/baseline_farmers$Check2.check.maize.q29 #yield in kg per acre

baseline_farmers$yield_inUGX <- baseline_farmers$Check2.check.maize.q50*baseline_farmers$Check2.check.maize.q52
baseline_farmers$yield_indollar <- baseline_farmers$yield_inUGX/3561.51
baseline_farmers$landproductivity_inUGX <- baseline_farmers$yield_inUGX/baseline_farmers$Check2.check.maize.q29

baseline_farmers$landproductivity_indollar <- baseline_farmers$landproductivity_inUGX/3561.51

baseline_farmers$Check2.check.maize.q53<-ifelse(baseline_farmers$Check2.check.maize.q53=="Yes",1,0)
baseline_farmers$Check2.check.maize.q54 <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q54))
baseline_farmers$Check2.check.maize.q55 <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q55))

baseline_farmers$outputprice_indollar <- baseline_farmers$Check2.check.maize.q55/3561.51
baseline_farmers$outputprice_indollar <- as.numeric(as.character(baseline_farmers$outputprice_indollar))

baseline_farmers$revenueUGX <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q54))*baseline_farmers$Check2.check.maize.q55/1000
baseline_farmers$revenue_dollar <- baseline_farmers$revenueUGX/3561.51
baseline_farmers$Check2.check.maize.q56 <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q56))
baseline_farmers$Check2.check.maize.q57 <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q57))

#additional dollar measures
baseline_farmers$Check2.check.maize.q39 <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q39))
baseline_farmers$priceindollar <- baseline_farmers$Check2.check.maize.q39/3561.51
baseline_farmers$costforseed_dollar <- baseline_farmers$costforseed/3561.51
baseline_farmers$marketvaluedollars <- baseline_farmers$Check2.check.maize.q52/3561.51

###loop###
variables_farmer <- c("Check2.check.maize.q8","Check2.check.maize.q9","Check2.check.maize.q10","Check2.check.maize.q11","Check2.check.maize.q12"
                      ,"Check2.check.maize.q14","Check2.check.maize.q15","married","noformaleducation","finishedprimary","finishedsecondary"
                      ,"Check2.check.maize.q18","Check2.check.maize.q20","goodroof","Check2.check.maize.q22","yearsmaize"
                      ,"Check2.check.maize.q24","Check2.check.maize.q25a","farmersavedseed","boughtfromagroinputshop","thirdormore_time_used"
                      ,"Check2.check.maize.q25d","tooexpensive","notgoodquality","verygoodquality","yieldtoolow","lesspesttolerant"
                      ,"lowgermination","Check2.check.maize.q25h","Check2.check.maize.q26.Longe_5","Check2.check.maize.q26.Longe_7R_Kayongo.go"
                      ,"Check2.check.maize.q26.Wema","q27bin","Check2.check.maize.q27","Check2.check.maize.q29","Check2.check.maize.q30"
                      ,"Check2.check.maize.q30a.1","Check2.check.maize.q30a.2","Check2.check.maize.q30a.3","Check2.check.maize.q30a.4"
                      ,"Check2.check.maize.q30a.5","Check2.check.maize.q30a.6","Check2.check.maize.q30b","hybrid","OPV","Land_Races","improved"
                      ,"farmer_saved_seed","Bought_from_agro_input_shop","hybridbutsaved","OPVbutfourthormore_timeused","Check2.check.maize.q35a"
                      ,"Check2.check.maize.q35b","Check2.check.maize.q35c","Check2.check.maize.q35d","Check2.check.maize.q35e"
                      ,"Check2.check.maize.q35f","Check2.check.maize.q35g","Check2.check.maize.q35h","Check2.check.maize.q35i"
                      ,"Check2.check.maize.q35j","Check2.check.maize.q36","Check2.check.maize.q36b","Check2.check.maize.q37"
                      ,"Check2.check.maize.q38","Check2.check.maize.q39","costforseed","correctseedspacing","Check2.check.maize.q41"
                      ,"correctnumberofseeds","Check2.check.maize.q42","Check2.check.maize.q43","Check2.check.maize.q43a"
                      ,"Check2.check.maize.q44","Check2.check.maize.q44a","Check2.check.maize.q45","correctweeding","Check2.check.maize.q46"
                      ,"correcttimeweeding","Check2.check.maize.q47","correctplanting","Check2.check.maize.q49","Check2.check.maize.q50"
                      ,"Check2.check.maize.q51","yield_inkg","landproductivity","Check2.check.maize.q52","yield_inUGX"
                      ,"yield_indollar","landproductivity_inUGX","landproductivity_indollar","Check2.check.maize.q53","Check2.check.maize.q54"
                      ,"Check2.check.maize.q55","outputprice_indollar","revenueUGX","revenue_dollar","Check2.check.maize.q56"
                      ,"Check2.check.maize.q57","priceindollar","costforseed_dollar","marketvaluedollars"
                      ,"bought_last_season","general_rating"
                      ,"location_rating","price_rating","quality_rating"
                      ,"stock_rating","reputation_rating"
                      ,"seed_quality_general_rating","seed_yield_rating"
                      ,"seed_drought_rating","seed_disease_rating"
                      ,"seed_maturing_rating","seed_germinate_rating"
                      ,"knows_dealer","bought_at_dealer","customer_years","knows_other_customer")

for (i in 1:length(variables_farmer)) {
  df_descriptives_farmer[i,1] <- sum(baseline_farmers[variables_farmer[i]], na.rm=T)/(nrow(baseline_farmers)-sum(is.na(baseline_farmers[variables_farmer[i]])))
  df_descriptives_farmer[i,2] <- min(baseline_farmers[variables_farmer[i]], na.rm=T)
  df_descriptives_farmer[i,3] <- max(baseline_farmers[variables_farmer[i]], na.rm=T)
  df_descriptives_farmer[i,4] <- sqrt(var(baseline_farmers[variables_farmer[i]], na.rm=T))
  df_descriptives_farmer[i,5] <- nrow(baseline_farmers) - sum(is.na(baseline_farmers[variables_farmer[i]]))}

########################################
#####TESTS OF RANDOMIZATION BALANCE#####
########################################

####################################
#####Balance: agro-input dealer#####
####################################

#to use vcovCR
library(clubSandwich)
library(knitr)

#Bjorn's variable: amount of sold hybird/OPV maize seed during last season in kg
sel <- c("maize.owner.agree.long10h.q25", "maize.owner.agree.longe7h.q37", "maize.owner.agree.longe5.q50", "maize.owner.agree.longe4.q62")
baseline_dealers[sel] <- lapply(baseline_dealers[sel], function(x) as.numeric(as.character(x)) )
baseline_dealers[sel] <- lapply(baseline_dealers[sel], function(x) replace(x, x == 999,NA) )
baseline_dealers$tot_sold <- rowSums(baseline_dealers[sel], na.rm=T)
baseline_dealers$tot_sold[baseline_dealers$tot_sold > 80000] <- NA

#Bjorn's variable: amount of lost/wasted seed during last season in kg
sel <- c("maize.owner.agree.long10h.q27", "maize.owner.agree.longe7h.q39", "maize.owner.agree.longe5.q52", "maize.owner.agree.longe4.q64")
baseline_dealers[sel] <- lapply(baseline_dealers[sel], function(x) as.numeric(as.character(x)) )
baseline_dealers[sel] <- lapply(baseline_dealers[sel], function(x) replace(x, x == 999,NA) )
baseline_dealers$tot_lost <- rowSums(baseline_dealers[sel], na.rm=T)

#Bjorn's skill variable
baseline_dealers$maize.owner.agree.skill.q105_b<-ifelse(baseline_dealers$maize.owner.agree.skill.q105=="b",1,0)

###loop###

###
#1#
###

balance_dealer <- c("maize.owner.agree.age","maize.owner.agree.gender","finished_primary","maize.owner.agree.q3"
                    ,"maize.owner.agree.q6","years_shop","maize.owner.agree.q10","maize.owner.agree.nr_var"
                    ,"tot_sold","tot_lost","maize.owner.agree.temp.q71","maize.owner.agree.temp.q72"
                    ,"maize.owner.agree.q96","maize.owner.agree.skill.q105_b"
                    ,"maize.owner.agree.inspection.q115","reading","lot"
                    ,"refunds"
                    ,"gives_credit","after_sales_service"
                    ,"maize.owner.agree.q5"
                    ,"maize.owner.agree.q7"
                    ,"maize.owner.agree.ownership"
                    ,"maize.owner.agree.temp.q80"
                    ,"q93_bin"
                    ,"visible_packdate")

df_averages <- array(NA,dim=c(2,50))

for (i in 1:length(balance_dealer)){
  df_averages[1,i] <- sum(baseline_dealers[balance_dealer[i]], na.rm=T)/(nrow(baseline_dealers)-sum(is.na(baseline_dealers[balance_dealer[i]])))
  df_averages[2,i] <- sqrt(var(baseline_dealers[balance_dealer[i]], na.rm=T))}

###
#2#
###

df_ols <- array(NA,dim=c(3,3,50))

baseline_dealers$training_demeaned <- baseline_dealers$training - mean(baseline_dealers$training,na.rm = T)
baseline_dealers$clearing_demeaned <- baseline_dealers$clearing - mean(baseline_dealers$clearing,na.rm = T)
baseline_dealers$farmer_demeaned <- baseline_dealers$farmer - mean(baseline_dealers$farmer,na.rm = T)

for (i in 1:length(balance_dealer)){
  ols <- lm(as.formula(paste(balance_dealer[i],"training*clearing_demeaned*farmer_demeaned",sep="~")), data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  df_ols[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_ols[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_ols[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]}

###
#3#
###

for (i in 1:length(balance_dealer)){
  ols <- lm(as.formula(paste(balance_dealer[i],"training_demeaned*clearing*farmer_demeaned",sep="~")), data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  df_ols[1,2,i] <- coef_test(ols, vcov_cluster)$beta[3]
  df_ols[2,2,i] <- coef_test(ols, vcov_cluster)$SE[3]
  df_ols[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]}

###
#4#
###

for (i in 1:length(balance_dealer)){
  ols <- lm(as.formula(paste(balance_dealer[i],"training_demeaned*clearing_demeaned*farmer",sep="~")), data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  
  #farmer video treatment at village/shop level so no clustering needed
  df_ols[1,3,i] <- summary(ols)$coefficients[4,1]
  df_ols[2,3,i] <- summary(ols)$coefficients[4,2]
  df_ols[3,3,i] <- summary(ols)$coefficients[4,4]}

#difference in mean primary education: Bjorn counted g (Other) as 0, I as NA
#difference in mean tarmac road: Bjorn did baseline_dealers$maize.owner.agree.q3[baseline_dealers$maize.owner.agree.q3 < 1] <- 0

#########################
#####Balance: farmer#####
#########################

#no treatment indicator for dealer training in baseline_farmers
#treatments at shop level
treatments_shop_level <- read.csv(paste(path,"/baseline/data/agro_input/public/treats_shop_level.csv", sep="/"), stringsAsFactors = TRUE)
#treatments at CA level
trainingtreatment_CA_level <- data.frame(aggregate(treatments_shop_level$training, list(treatments_shop_level$catchID), mean))
names(trainingtreatment_CA_level) <- c("catchID","training")
baseline_farmers <- merge(baseline_farmers, trainingtreatment_CA_level, by.x="catchID", by.y="catchID")

###loop###
balance_farmer <- c("Check2.check.maize.q8","Check2.check.maize.q10","Check2.check.maize.q14","Check2.check.maize.q15","finishedprimary"
                    ,"Check2.check.maize.q18","Check2.check.maize.q20","Check2.check.maize.q22","Check2.check.maize.q25a"
                    ,"boughtfromagroinputshop2","Check2.check.maize.q25d2","Check2.check.maize.q25h","Check2.check.maize.q30a.1"
                    ,"adoption_onfield","Check2.check.maize.q35a","Check2.check.maize.q42","correctplanting","yield_inkg"
                    ,"landproductivity","Check2.check.maize.q53"
                    ,"yearsmaize"
                    ,"Check2.check.maize.q43")

###
#1#
###

df_averages_farmer <- array(NA,dim=c(2,50))

for (i in 1:length(balance_farmer)){
  df_averages_farmer[1,i] <- sum(baseline_farmers[balance_farmer[i]], na.rm=T)/(nrow(baseline_farmers)-sum(is.na(baseline_farmers[balance_farmer[i]])))
  df_averages_farmer[2,i] <- sqrt(var(baseline_farmers[balance_farmer[i]], na.rm=T))}

###
#2#
###

df_ols_end_farmer <- array(NA,dim=c(3,3,50))

baseline_farmers$training <- baseline_farmers$training
baseline_farmers$clearing <- baseline_farmers$Check2.check.maize.clearing
baseline_farmers$farmer <- baseline_farmers$Check2.check.maize.video_shown

baseline_farmers$training_demeaned <- baseline_farmers$training - mean(baseline_farmers$training,na.rm = T)
baseline_farmers$clearing_demeaned <- baseline_farmers$clearing - mean(baseline_farmers$clearing,na.rm = T)
baseline_farmers$farmer_demeaned <- baseline_farmers$farmer - mean(baseline_farmers$farmer,na.rm = T)

for (i in 1:length(balance_farmer)){
  ols <- lm(as.formula(paste(balance_farmer[i],"training*clearing_demeaned*farmer_demeaned",sep="~")), data=baseline_farmers) #* because of interactions
  #vcovCR for cluster-robust variance-covariance matrix
  vcov_cluster_catchID <- vcovCR(ols,cluster=baseline_farmers$catchID,type="CR0")
  
  #filling df_ols with training (Estimate, SE, p-val (Satt))
  df_ols_end_farmer[1,1,i] <- coef_test(ols, vcov_cluster_catchID)$beta[2]
  df_ols_end_farmer[2,1,i] <- coef_test(ols, vcov_cluster_catchID)$SE[2]
  df_ols_end_farmer[3,1,i] <- coef_test(ols, vcov_cluster_catchID)$p_Satt[2]}

###
#3#
###

for (i in 1:length(balance_farmer)){
  ols <- lm(as.formula(paste(balance_farmer[i],"training_demeaned*clearing*farmer_demeaned",sep="~")), data=baseline_farmers) #* because of interactions
  vcov_cluster_catchID <- vcovCR(ols,cluster=baseline_farmers$catchID,type="CR0")
  
  #filling df_ols with CH (Estimate, SE, p-val (Satt))
  df_ols_end_farmer[1,2,i] <- coef_test(ols, vcov_cluster_catchID)$beta[3]
  df_ols_end_farmer[2,2,i] <- coef_test(ols, vcov_cluster_catchID)$SE[3]
  df_ols_end_farmer[3,2,i] <- coef_test(ols, vcov_cluster_catchID)$p_Satt[3]}

###
#4#
###

for (i in 1:length(balance_farmer)){
  ols <- lm(as.formula(paste(balance_farmer[i],"training_demeaned*clearing_demeaned*farmer",sep="~")), data=baseline_farmers) #* because of interactions
  
  #filling df_ols with video (Estimate, SE, p-val (Satt))
  #randomization at village level ie. at shop level
  vcov_cluster_shop <- vcovCR(ols,cluster=baseline_farmers$shop_ID,type="CR0")
  
  df_ols_end_farmer[1,3,i] <- coef_test(ols, vcov_cluster_shop)$beta[4]
  df_ols_end_farmer[2,3,i] <- coef_test(ols, vcov_cluster_shop)$SE[4]
  df_ols_end_farmer[3,3,i] <- coef_test(ols, vcov_cluster_shop)$p_Satt[4]}

#difference in mean primary education: I first counted g (Other) as 0, now as NA
#difference in mean bought from dealer: correct if NA=0

###################################
#####TESTS OF SURVEY ATTRITION#####
###################################

######################################
#####Attrition: agro-input dealer#####
######################################

#MIDLINE

baseline_dealers$attrition_ind_D <- 0
baseline_dealers$attrition_ind_D[is.na(baseline_dealers$owner.agree.age)] <- 1

number_lostD <- sum(baseline_dealers$attrition_ind_D==1)
number_lostD_control <- sum(baseline_dealers$attrition_ind_D==1 & baseline_dealers$training == 0 & baseline_dealers$clearing == 0 & baseline_dealers$farmer == 0)
number_lostD_control_CH <- sum(baseline_dealers$attrition_ind_D==1 & baseline_dealers$clearing == 0)
number_lostD_training <- sum(baseline_dealers$attrition_ind_D==1 & baseline_dealers$training == 1)
number_lostD_clearing <- sum(baseline_dealers$attrition_ind_D==1 & baseline_dealers$clearing == 1)
number_lostD_farmer <- sum(baseline_dealers$attrition_ind_D==1 & baseline_dealers$farmer == 1)

number_allD <- sum(nrow(baseline_dealers))
number_allD_control <- sum(baseline_dealers$training == 0 & baseline_dealers$clearing == 0 & baseline_dealers$farmer == 0)
number_allD_control_CH <- sum(baseline_dealers$clearing == 0)
number_allD_training <- sum(baseline_dealers$training == 1)
number_allD_clearing <- sum(baseline_dealers$clearing == 1)
number_allD_farmer <- sum(baseline_dealers$farmer == 1)

perc_lostD <- sum(number_lostD/number_allD*100)
perc_lostD_control <- sum(number_lostD_control/number_allD_control*100)
perc_lostD_control_CH <- sum(number_lostD_control_CH/number_allD_control_CH*100)
perc_lostD_training <- sum(number_lostD_training/number_allD_training*100)
perc_lostD_clearing <- sum(number_lostD_clearing/number_allD_clearing*100)
perc_lostD_clearing_CH <- sum(number_lostD_clearing/number_allD_clearing*100)
perc_lostD_farmer <- sum(number_lostD_farmer/number_allD_farmer*100)

attrition_dealer <- c("attrition_ind_D")

###
#1#
###

df_averages_attritionD <- array(NA,dim=c(2,25))

for (i in 1:length(attrition_dealer)){
  df_averages_attritionD[1,i] <- sum(baseline_dealers[attrition_dealer[i]], na.rm=T)/(nrow(baseline_dealers)-sum(is.na(baseline_dealers[attrition_dealer[i]])))
  df_averages_attritionD[2,i] <- sqrt(var(baseline_dealers[attrition_dealer[i]], na.rm=T))}

###
#2#
###

df_ols_end_attritionD <- array(NA,dim=c(3,3,25))

for (i in 1:length(attrition_dealer)){
  ols <- lm(as.formula(paste(attrition_dealer[i],"training*clearing_demeaned*farmer_demeaned",sep="~")), data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  df_ols_end_attritionD[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_ols_end_attritionD[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_ols_end_attritionD[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]}

###
#3#
###

for (i in 1:length(attrition_dealer)){
  ols <- lm(as.formula(paste(attrition_dealer[i],"training_demeaned*clearing*farmer_demeaned",sep="~")), data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  df_ols_end_attritionD[1,2,i] <- coef_test(ols, vcov_cluster)$beta[3]
  df_ols_end_attritionD[2,2,i] <- coef_test(ols, vcov_cluster)$SE[3]
  df_ols_end_attritionD[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]}

###
#4#
###

for (i in 1:length(attrition_dealer)){
  ols <- lm(as.formula(paste(attrition_dealer[i],"training_demeaned*clearing_demeaned*farmer",sep="~")), data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  #farmer video treatment at village/shop level so no clustering needed
  df_ols_end_attritionD[1,3,i] <- summary(ols)$coefficients[4,1]
  df_ols_end_attritionD[2,3,i] <- summary(ols)$coefficients[4,2]
  df_ols_end_attritionD[3,3,i] <- summary(ols)$coefficients[4,4]}

#Chi-squared test p-value instead of p-value from regression?

table(baseline_dealers$attrition_ind_D,baseline_dealers$clearing)
chisq.test(table(baseline_dealers$attrition_ind_D,baseline_dealers$clearing))
#p-value = 0.0036

prop.test(table(baseline_dealers$clearing,baseline_dealers$attrition_ind_D==1))
#p-value = 0.0036

#but chisq.test and prop.test do not correct for clustering at catchment area level

#ENDLINE

baseline_dealers$attrition_ind_D_end <- 0
baseline_dealers$attrition_ind_D_end[baseline_dealers$checq=="No"] <- 1
baseline_dealers$attrition_ind_D_end[is.na(baseline_dealers$checq)] <- 0

number_lostD_end <- sum(baseline_dealers$attrition_ind_D_end==1)
number_lostD_control_end <- sum(baseline_dealers$attrition_ind_D_end==1 & baseline_dealers$training == 0 & baseline_dealers$clearing == 0 & baseline_dealers$farmer == 0)
number_lostD_control_CH_end <- sum(baseline_dealers$attrition_ind_D_end==1 & baseline_dealers$clearing == 0)
number_lostD_training_end <- sum(baseline_dealers$attrition_ind_D_end==1 & baseline_dealers$training == 1)
number_lostD_clearing_end <- sum(baseline_dealers$attrition_ind_D_end==1 & baseline_dealers$clearing == 1)
number_lostD_farmer_end <- sum(baseline_dealers$attrition_ind_D_end==1 & baseline_dealers$farmer == 1)

number_allD <- sum(nrow(baseline_dealers))
number_allD_control <- sum(baseline_dealers$training == 0 & baseline_dealers$clearing == 0 & baseline_dealers$farmer == 0)
number_allD_control_CH <- sum(baseline_dealers$clearing == 0)
number_allD_training <- sum(baseline_dealers$training == 1)
number_allD_clearing <- sum(baseline_dealers$clearing == 1)
number_allD_farmer <- sum(baseline_dealers$farmer == 1)

perc_lostD_end <- sum(number_lostD_end/number_allD*100)
perc_lostD_control_end <- sum(number_lostD_control_end/number_allD_control*100)
perc_lostD_control_CH_end <- sum(number_lostD_control_CH_end/number_allD_control_CH*100)
perc_lostD_training_end <- sum(number_lostD_training_end/number_allD_training*100)
perc_lostD_clearing_end <- sum(number_lostD_clearing_end/number_allD_clearing*100)
perc_lostD_clearing_CH_end <- sum(number_lostD_clearing_end/number_allD_clearing*100)
perc_lostD_farmer_end <- sum(number_lostD_farmer_end/number_allD_farmer*100)

attrition_dealer_end <- c("attrition_ind_D_end")

###
#1#
###

df_averages_attritionD_end <- array(NA,dim=c(2,25))

for (i in 1:length(attrition_dealer_end)){
  df_averages_attritionD_end[1,i] <- sum(baseline_dealers[attrition_dealer_end[i]], na.rm=T)/(nrow(baseline_dealers)-sum(is.na(baseline_dealers[attrition_dealer_end[i]])))
  df_averages_attritionD_end[2,i] <- sqrt(var(baseline_dealers[attrition_dealer_end[i]], na.rm=T))}

###
#2#
###

df_ols_end_attritionD_end <- array(NA,dim=c(3,3,25))

for (i in 1:length(attrition_dealer_end)){
  ols <- lm(as.formula(paste(attrition_dealer_end[i],"training*clearing_demeaned*farmer_demeaned",sep="~")), data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  df_ols_end_attritionD_end[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_ols_end_attritionD_end[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_ols_end_attritionD_end[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]}

###
#3#
###

for (i in 1:length(attrition_dealer_end)){
  ols <- lm(as.formula(paste(attrition_dealer_end[i],"training_demeaned*clearing*farmer_demeaned",sep="~")), data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  df_ols_end_attritionD_end[1,2,i] <- coef_test(ols, vcov_cluster)$beta[3]
  df_ols_end_attritionD_end[2,2,i] <- coef_test(ols, vcov_cluster)$SE[3]
  df_ols_end_attritionD_end[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]}

###
#4#
###

for (i in 1:length(attrition_dealer_end)){
  ols <- lm(as.formula(paste(attrition_dealer_end[i],"training_demeaned*clearing_demeaned*farmer",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  #farmer video treatment at village/shop level so no clustering needed
  df_ols_end_attritionD_end[1,3,i] <- summary(ols)$coefficients[4,1]
  df_ols_end_attritionD_end[2,3,i] <- summary(ols)$coefficients[4,2]
  df_ols_end_attritionD_end[3,3,i] <- summary(ols)$coefficients[4,4]}

#Chi-squared test p-value instead of p-value from regression?

table(baseline_dealers$attrition_ind_D_end,baseline_dealers$clearing)
chisq.test(table(baseline_dealers$attrition_ind_D_end,baseline_dealers$clearing))
#p-value = 0.05541

prop.test(table(baseline_dealers$clearing,baseline_dealers$attrition_ind_D_end==1))
#p-value = 0.05541

#but chisq.test and prop.test do not correct for clustering at catchment area level


################################################################################################################################################

###########################
#####Attrition: farmer#####
###########################

#MIDLINE

names(midline_farmers)[names(midline_farmers) == "clearing"] <- "mid_clearing" #because same name in bl and ml
names(midline_farmers)[names(midline_farmers) == "catchID"] <- "mid_catchID" #because same name in bl and ml

baseline_farmers <- merge(baseline_farmers,midline_farmers,by.x="farmer_ID",by.y="Group.1",all.x=T)

baseline_farmers$attrition_ind_F <- 0
baseline_farmers$attrition_ind_F[is.na(baseline_farmers$mid_catchID)] <- 1

number_lostF <- sum(baseline_farmers$attrition_ind_F==1)
number_lostF_control <- sum(baseline_farmers$attrition_ind_F==1 & baseline_farmers$training == 0 & baseline_farmers$Check2.check.maize.clearing == 0 & baseline_farmers$Check2.check.maize.video_shown == 0)
number_lostF_control_CH <- sum(baseline_farmers$attrition_ind_F==1 & baseline_farmers$Check2.check.maize.clearing == 0)
number_lostF_training <- sum(baseline_farmers$attrition_ind_F==1 & baseline_farmers$training == 1)
number_lostF_clearing <- sum(baseline_farmers$attrition_ind_F==1 & baseline_farmers$Check2.check.maize.clearing == 1)
number_lostF_farmer <- sum(baseline_farmers$attrition_ind_F==1 & baseline_farmers$Check2.check.maize.video_shown == 1)

number_allF <- sum(nrow(baseline_farmers))
number_allF_control <- sum(baseline_farmers$training == 0 & baseline_farmers$Check2.check.maize.clearing == 0 & baseline_farmers$Check2.check.maize.video_shown == 0)
number_allF_control_CH <- sum(baseline_farmers$Check2.check.maize.clearing == 0)
number_allF_training <- sum(baseline_farmers$training == 1)
number_allF_clearing <- sum(baseline_farmers$Check2.check.maize.clearing == 1)
number_allF_farmer <- sum(baseline_farmers$Check2.check.maize.video_shown == 1)

perc_lostF <- sum(number_lostF/number_allF*100)
perc_lostF_control <- sum(number_lostF_control/number_allF_control*100)
perc_lostF_control_CH <- sum(number_lostF_control_CH/number_allF_control_CH*100)
perc_lostF_training <- sum(number_lostF_training/number_allF_training*100)
perc_lostF_clearing <- sum(number_lostF_clearing/number_allF_clearing*100)
perc_lostF_farmer <- sum(number_lostF_farmer/number_allF_farmer*100)

attrition_farmer <- c("attrition_ind_F")

###
#1#
###

df_averages_attritionF <- array(NA,dim=c(2,25))

for (i in 1:length(attrition_farmer)){
  df_averages_attritionF[1,i] <- sum(baseline_farmers[attrition_farmer[i]], na.rm=T)/(nrow(baseline_farmers)-sum(is.na(baseline_farmers[attrition_farmer[i]])))
  df_averages_attritionF[2,i] <- sqrt(var(baseline_farmers[attrition_farmer[i]], na.rm=T))}

###
#2#
###

df_ols_end_attritionF <- array(NA,dim=c(3,3,25))

for (i in 1:length(attrition_farmer)){
  ols <- lm(as.formula(paste(attrition_farmer[i],"training*clearing_demeaned*farmer_demeaned",sep="~")),data=baseline_farmers) #* because of interactions
  #vcovCR for cluster-robust variance-covariance matrix
  vcov_cluster_catchID <- vcovCR(ols,cluster=baseline_farmers$catchID,type="CR0")

  #filling df_ols with training (Estimate, SE, p-val (Satt))
  df_ols_end_attritionF[1,1,i] <- coef_test(ols, vcov_cluster_catchID)$beta[2]
  df_ols_end_attritionF[2,1,i] <- coef_test(ols, vcov_cluster_catchID)$SE[2]
  df_ols_end_attritionF[3,1,i] <- coef_test(ols, vcov_cluster_catchID)$p_Satt[2]}

###
#3#
###

for (i in 1:length(attrition_farmer)){
  ols <- lm(as.formula(paste(attrition_farmer[i],"training_demeaned*clearing*farmer_demeaned",sep="~")),data=baseline_farmers) #* because of interactions
  #vcovCR for cluster-robust variance-covariance matrix
  vcov_cluster_catchID <- vcovCR(ols,cluster=baseline_farmers$catchID,type="CR0")

  #filling df_ols with CH (Estimate, SE, p-val (Satt))
  df_ols_end_attritionF[1,2,i] <- coef_test(ols, vcov_cluster_catchID)$beta[3]
  df_ols_end_attritionF[2,2,i] <- coef_test(ols, vcov_cluster_catchID)$SE[3]
  df_ols_end_attritionF[3,2,i] <- coef_test(ols, vcov_cluster_catchID)$p_Satt[3]}

###
#4#
###

for (i in 1:length(attrition_farmer)){
  ols <- lm(as.formula(paste(attrition_farmer[i],"training_demeaned*clearing_demeaned*farmer",sep="~")),data=baseline_farmers) #* because of interactions
  #vcovCR for cluster-robust variance-covariance matrix
  vcov_cluster_catchID <- vcovCR(ols,cluster=baseline_farmers$catchID,type="CR0")

  #filling df_ols with video (Estimate, SE, p-val (Satt))
  #randomization at village level ie. at shop level
  vcov_cluster_shop <- vcovCR(ols,cluster=baseline_farmers$shop_ID,type="CR0")

  df_ols_end_attritionF[1,3,i] <- coef_test(ols, vcov_cluster_shop)$beta[4]
  df_ols_end_attritionF[2,3,i] <- coef_test(ols, vcov_cluster_shop)$SE[4]
  df_ols_end_attritionF[3,3,i] <- coef_test(ols, vcov_cluster_shop)$p_Satt[4]}

#ENDLINE

baseline_farmers$attrition_ind_F_end <- 0
baseline_farmers$attrition_ind_F_end[is.na(baseline_farmers$TODAY)] <- 1

number_lostF_end <- sum(baseline_farmers$attrition_ind_F_end==1)
number_lostF_control_end <- sum(baseline_farmers$attrition_ind_F_end==1 & baseline_farmers$training == 0 & baseline_farmers$Check2.check.maize.clearing == 0 & baseline_farmers$Check2.check.maize.video_shown == 0)
number_lostF_control_CH_end <- sum(baseline_farmers$attrition_ind_F_end==1 & baseline_farmers$Check2.check.maize.clearing == 0)
number_lostF_training_end <- sum(baseline_farmers$attrition_ind_F_end==1 & baseline_farmers$training == 1)
number_lostF_clearing_end <- sum(baseline_farmers$attrition_ind_F_end==1 & baseline_farmers$Check2.check.maize.clearing == 1)
number_lostF_farmer_end <- sum(baseline_farmers$attrition_ind_F_end==1 & baseline_farmers$Check2.check.maize.video_shown == 1)

number_allF <- sum(nrow(baseline_farmers))
number_allF_control <- sum(baseline_farmers$training == 0 & baseline_farmers$Check2.check.maize.clearing == 0 & baseline_farmers$Check2.check.maize.video_shown == 0)
number_allF_control_CH <- sum(baseline_farmers$Check2.check.maize.clearing == 0)
number_allF_training <- sum(baseline_farmers$training == 1)
number_allF_clearing <- sum(baseline_farmers$Check2.check.maize.clearing == 1)
number_allF_farmer <- sum(baseline_farmers$Check2.check.maize.video_shown == 1)

perc_lostF_end <- sum(number_lostF_end/number_allF*100)
perc_lostF_control_end <- sum(number_lostF_control_end/number_allF_control*100)
perc_lostF_control_CH_end <- sum(number_lostF_control_CH_end/number_allF_control_CH*100)
perc_lostF_training_end <- sum(number_lostF_training_end/number_allF_training*100)
perc_lostF_clearing_end <- sum(number_lostF_clearing_end/number_allF_clearing*100)
perc_lostF_farmer_end <- sum(number_lostF_farmer_end/number_allF_farmer*100)

attrition_farmer_end <- c("attrition_ind_F_end")

###
#1#
###

df_averages_attritionF_end <- array(NA,dim=c(2,25))

for (i in 1:length(attrition_farmer_end)){
  df_averages_attritionF_end[1,i] <- sum(baseline_farmers[attrition_farmer_end[i]], na.rm=T)/(nrow(baseline_farmers)-sum(is.na(baseline_farmers[attrition_farmer_end[i]])))
  df_averages_attritionF_end[2,i] <- sqrt(var(baseline_farmers[attrition_farmer_end[i]], na.rm=T))}

###
#2#
###

df_ols_end_attritionF_end <- array(NA,dim=c(3,3,25))

for (i in 1:length(attrition_farmer_end)){
  ols <- lm(as.formula(paste(attrition_farmer_end[i],"training*clearing_demeaned*farmer_demeaned",sep="~")), data=baseline_farmers) #* because of interactions
  #vcovCR for cluster-robust variance-covariance matrix
  vcov_cluster_catchID <- vcovCR(ols,cluster=baseline_farmers$catchID,type="CR0")
  
  #filling df_ols with training (Estimate, SE, p-val (Satt))
  df_ols_end_attritionF_end[1,1,i] <- coef_test(ols, vcov_cluster_catchID)$beta[2]
  df_ols_end_attritionF_end[2,1,i] <- coef_test(ols, vcov_cluster_catchID)$SE[2]
  df_ols_end_attritionF_end[3,1,i] <- coef_test(ols, vcov_cluster_catchID)$p_Satt[2]}

###
#3#
###

for (i in 1:length(attrition_farmer_end)){
  ols <- lm(as.formula(paste(attrition_farmer_end[i],"training_demeaned*clearing*farmer_demeaned",sep="~")), data=baseline_farmers) #* because of interactions
  #vcovCR for cluster-robust variance-covariance matrix
  vcov_cluster_catchID <- vcovCR(ols,cluster=baseline_farmers$catchID,type="CR0")
  
  #filling df_ols with CH (Estimate, SE, p-val (Satt))
  df_ols_end_attritionF_end[1,2,i] <- coef_test(ols, vcov_cluster_catchID)$beta[3]
  df_ols_end_attritionF_end[2,2,i] <- coef_test(ols, vcov_cluster_catchID)$SE[3]
  df_ols_end_attritionF_end[3,2,i] <- coef_test(ols, vcov_cluster_catchID)$p_Satt[3]}

###
#4#
###

for (i in 1:length(attrition_farmer_end)){
  ols <- lm(as.formula(paste(attrition_farmer_end[i],"training_demeaned*clearing_demeaned*farmer",sep="~")), data=baseline_farmers) #* because of interactions
  #vcovCR for cluster-robust variance-covariance matrix
  vcov_cluster_catchID <- vcovCR(ols,cluster=baseline_farmers$catchID,type="CR0")
  
  #filling df_ols with video (Estimate, SE, p-val (Satt))
  #randomization at village level ie. at shop level
  vcov_cluster_shop <- vcovCR(ols,cluster=baseline_farmers$shop_ID,type="CR0")
  
  df_ols_end_attritionF_end[1,3,i] <- coef_test(ols, vcov_cluster_shop)$beta[4]
  df_ols_end_attritionF_end[2,3,i] <- coef_test(ols, vcov_cluster_shop)$SE[4]
  df_ols_end_attritionF_end[3,3,i] <- coef_test(ols, vcov_cluster_shop)$p_Satt[4]}










################################################################################################################################################################################
##### 1 ANALYSIS: Agro-input dealer - Primary###################################################################################################################################
################################################################################################################################################################################

trim <- function(var,dataset,trim_perc=.02){
  dataset[var][dataset[var]<quantile(dataset[var],c(trim_perc/2,1-(trim_perc/2)),na.rm=T)[1]|dataset[var]>quantile(dataset[var],c(trim_perc/2,1-(trim_perc/2)),na.rm=T)[2]] <- NA
  return(dataset)}

ihs <- function(x) {
  y <- log(x + sqrt(x ^ 2 + 1))
  return(y)}

baseline_dealers$training<-ifelse(baseline_dealers$training=="TRUE",1,0)
baseline_dealers$clearing<-ifelse(baseline_dealers$clearing=="TRUE",1,0)
baseline_dealers$farmer<-ifelse(baseline_dealers$farmer=="TRUE",1,0)

#Heterogeneity analyses
#1: Specialized agro-input shops
#baseline_dealers=subset(baseline_dealers,maize.owner.agree.q5=="1")
#delete baseline_dealers$mid_maize.owner.agree.q9_a from variables_motivation_mid

#2: More competitive catchment areas
baseline_dealers$small_catchID <- ifelse(baseline_dealers$catchID==16|baseline_dealers$catchID==18|baseline_dealers$catchID==19|
                                         baseline_dealers$catchID==33|baseline_dealers$catchID==34|baseline_dealers$catchID==36|
                                         baseline_dealers$catchID==42|baseline_dealers$catchID==45|baseline_dealers$catchID==46|
                                         baseline_dealers$catchID==48|baseline_dealers$catchID==53|baseline_dealers$catchID==63|
                                         baseline_dealers$catchID==65|baseline_dealers$catchID==66|baseline_dealers$catchID==67|
                                         baseline_dealers$catchID==73|baseline_dealers$catchID==79|baseline_dealers$catchID==80|
                                         baseline_dealers$catchID==87|baseline_dealers$catchID==89|baseline_dealers$catchID==90|
                                         baseline_dealers$catchID==91|baseline_dealers$catchID==92|baseline_dealers$catchID==93|
                                         baseline_dealers$catchID==95|baseline_dealers$catchID==98|baseline_dealers$catchID==101|
                                         baseline_dealers$catchID==103|baseline_dealers$catchID==106|baseline_dealers$catchID==107|
                                         baseline_dealers$catchID==108|baseline_dealers$catchID==109|baseline_dealers$catchID==110|
                                         baseline_dealers$catchID==112|baseline_dealers$catchID==116|baseline_dealers$catchID==118|
                                         baseline_dealers$catchID==120|baseline_dealers$catchID==121|baseline_dealers$catchID==122|
                                         baseline_dealers$catchID==124|baseline_dealers$catchID==125|baseline_dealers$catchID==126|
                                         baseline_dealers$catchID==127|baseline_dealers$catchID==128|baseline_dealers$catchID==129|
                                         baseline_dealers$catchID==130,1,0)
#baseline_dealers=subset(baseline_dealers,small_catchID=="0")

#3: Less competitive catchment areas
baseline_dealers$large_catchID <- ifelse(baseline_dealers$catchID==3|baseline_dealers$catchID==32|baseline_dealers$catchID==59,1,0)
# |baseline_dealers$catchID==64
#baseline_dealers=subset(baseline_dealers,large_catchID=="0")

#4:
reviews_seed <- read.csv(paste(path,"/baseline/data/agro_input/public/reviews_seed.csv",sep="/"), stringsAsFactors=TRUE)
reviews_seed = reviews_seed[c("catchID","shop_ID","score_corrected")]
baseline_dealers <- merge(baseline_dealers,reviews_seed,by.x=c("catchID","shop_ID"),by.y=c("catchID","shop_ID"),all.x=T)
baseline_dealers$notrated <- ifelse(is.na(baseline_dealers$score_corrected)&baseline_dealers$clearing==1,1,0)
#baseline_dealers=subset(baseline_dealers,notrated=="0")





#1. Cumulative quantity sold of a hybrid and a open-pollinated maize variety last season in kg
baseline_dealers$maize.owner.agree.long10h.q25_unadj <- baseline_dealers$maize.owner.agree.long10h.q25
baseline_dealers$maize.owner.agree.long10h.q25[baseline_dealers$maize.owner.agree.q20=="0"] <- 0

baseline_dealers$maize.owner.agree.longe7h.q37[baseline_dealers$maize.owner.agree.q32=="0"] <- 0

baseline_dealers$maize.owner.agree.longe5.q50_unadj <- baseline_dealers$maize.owner.agree.longe5.q50
baseline_dealers$maize.owner.agree.longe5.q50[baseline_dealers$maize.owner.agree.q45=="0"] <- 0

baseline_dealers$maize.owner.agree.longe4.q62[baseline_dealers$maize.owner.agree.q57=="0"] <- 0

baseline_dealers$check.owner.agree.long10h.q25[baseline_dealers$attrition_ind_D_end==1] <- NA
baseline_dealers$check.owner.agree.longe7H.q38[baseline_dealers$attrition_ind_D_end==1] <- NA
baseline_dealers$check.owner.agree.longe5.q50[baseline_dealers$attrition_ind_D_end==1] <- NA
baseline_dealers$check.owner.agree.longe4.q62[baseline_dealers$attrition_ind_D_end==1] <- NA
baseline_dealers$kg_improved[baseline_dealers$attrition_ind_D_end==1] <- NA

baseline_dealers$mid_maize.owner.agree.q20 <- NA
baseline_dealers$mid_maize.owner.agree.q20[baseline_dealers$check.owner.agree.q20=="Yes"] <- 1
baseline_dealers$mid_maize.owner.agree.q20[baseline_dealers$check.owner.agree.q20=="No"] <- 0

baseline_dealers$mid_maize.owner.agree.q32 <- NA
baseline_dealers$mid_maize.owner.agree.q32[baseline_dealers$check.owner.agree.q32=="Yes"] <- 1
baseline_dealers$mid_maize.owner.agree.q32[baseline_dealers$check.owner.agree.q32=="No"] <- 0

baseline_dealers$mid_maize.owner.agree.q45 <- NA
baseline_dealers$mid_maize.owner.agree.q45[baseline_dealers$check.owner.agree.q45=="Yes"] <- 1
baseline_dealers$mid_maize.owner.agree.q45[baseline_dealers$check.owner.agree.q45=="No"] <- 0

baseline_dealers$mid_maize.owner.agree.q57 <- NA
baseline_dealers$mid_maize.owner.agree.q57[baseline_dealers$check.owner.agree.q57=="Yes"] <- 1
baseline_dealers$mid_maize.owner.agree.q57[baseline_dealers$check.owner.agree.q57=="No"] <- 0

baseline_dealers$mid_maize.owner.agree.long10h.q25 <-baseline_dealers$check.owner.agree.long10h.q25 #x
baseline_dealers$mid_maize.owner.agree.long10h.q25[baseline_dealers$mid_maize.owner.agree.long10h.q25=="n/a"] <- NA #x
baseline_dealers$mid_maize.owner.agree.long10h.q25 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.long10h.q25)) #x
baseline_dealers$mid_maize.owner.agree.long10h.q25[baseline_dealers$mid_maize.owner.agree.long10h.q25==999]<-NA #x
baseline_dealers$mid_maize.owner.agree.long10h.q25_unadj <- baseline_dealers$mid_maize.owner.agree.long10h.q25
baseline_dealers$mid_maize.owner.agree.long10h.q25[baseline_dealers$mid_maize.owner.agree.q20=="0"] <- 0 #x

baseline_dealers$mid_maize.owner.agree.longe7h.q37 <- baseline_dealers$check.owner.agree.longe7H.q38 #x #different numbers in bl and ml
baseline_dealers$mid_maize.owner.agree.longe7h.q37[baseline_dealers$mid_maize.owner.agree.longe7h.q37=="n/a"] <- NA #x
baseline_dealers$mid_maize.owner.agree.longe7h.q37 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.longe7h.q37)) #x
baseline_dealers$mid_maize.owner.agree.longe7h.q37[baseline_dealers$mid_maize.owner.agree.longe7h.q37==999]<-NA #x
baseline_dealers$mid_maize.owner.agree.longe7h.q37[baseline_dealers$mid_maize.owner.agree.q32=="0"] <- 0 #x

baseline_dealers$mid_maize.owner.agree.longe5.q50 <- baseline_dealers$check.owner.agree.longe5.q50 #x
baseline_dealers$mid_maize.owner.agree.longe5.q50[baseline_dealers$mid_maize.owner.agree.longe5.q50=="n/a"] <- NA #x
baseline_dealers$mid_maize.owner.agree.longe5.q50 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.longe5.q50)) #x
baseline_dealers$mid_maize.owner.agree.longe5.q50[baseline_dealers$mid_maize.owner.agree.longe5.q50==999]<-NA #x
baseline_dealers$mid_maize.owner.agree.longe5.q50_unadj <- baseline_dealers$mid_maize.owner.agree.longe5.q50
baseline_dealers$mid_maize.owner.agree.longe5.q50[baseline_dealers$mid_maize.owner.agree.q45=="0"] <- 0 #x

baseline_dealers$mid_maize.owner.agree.longe4.q62 <- baseline_dealers$check.owner.agree.longe4.q62 #x
baseline_dealers$mid_maize.owner.agree.longe4.q62[baseline_dealers$mid_maize.owner.agree.longe4.q62=="n/a"] <- NA #x
baseline_dealers$mid_maize.owner.agree.longe4.q62 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.longe4.q62)) #x
baseline_dealers$mid_maize.owner.agree.longe4.q62[baseline_dealers$mid_maize.owner.agree.longe4.q62==999]<-NA #x
baseline_dealers$mid_maize.owner.agree.longe4.q62[baseline_dealers$mid_maize.owner.agree.q57=="0"] <- 0 #x

baseline_dealers$quantitysold <- baseline_dealers$maize.owner.agree.long10h.q25+baseline_dealers$maize.owner.agree.longe7h.q37+baseline_dealers$maize.owner.agree.longe5.q50+baseline_dealers$maize.owner.agree.longe4.q62
baseline_dealers <- trim("quantitysold",baseline_dealers,trim_perc=.02)
baseline_dealers$quantitysold <- ihs(baseline_dealers$quantitysold)

baseline_dealers$mid_quantitysold <- baseline_dealers$mid_maize.owner.agree.long10h.q25+baseline_dealers$mid_maize.owner.agree.longe7h.q37+baseline_dealers$mid_maize.owner.agree.longe5.q50+baseline_dealers$mid_maize.owner.agree.longe4.q62 #x
baseline_dealers <- trim("mid_quantitysold",baseline_dealers,trim_perc=.02) #x
baseline_dealers$mid_quantitysold <- ihs(baseline_dealers$mid_quantitysold)

#2. Sales prices of a hybrid and an open-pollinated maize variety at beginning of last season in UGX per kg
baseline_dealers$maize.owner.agree.long10h.q26[baseline_dealers$maize.owner.agree.long10h.q26=="n/a"]<-NA
baseline_dealers$maize.owner.agree.longe7h.q38[baseline_dealers$maize.owner.agree.longe7h.q38=="n/a"]<-NA
baseline_dealers$maize.owner.agree.longe5.q51[baseline_dealers$maize.owner.agree.longe5.q51=="n/a"]<-NA
baseline_dealers$maize.owner.agree.longe4.q63[baseline_dealers$maize.owner.agree.longe4.q63=="n/a"]<-NA

baseline_dealers$maize.owner.agree.long10h.q26 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.long10h.q26))
baseline_dealers$maize.owner.agree.longe7h.q38 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.longe7h.q38))
baseline_dealers$maize.owner.agree.longe5.q51 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.longe5.q51))
baseline_dealers$maize.owner.agree.longe4.q63 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.longe4.q63))

baseline_dealers$mid_maize.owner.agree.long10h.q26 <- baseline_dealers$check.owner.agree.long10h.q26 #x
baseline_dealers$mid_maize.owner.agree.longe7h.q38 <- baseline_dealers$check.owner.agree.longe7H.q39 #x #numer changed from bl to ml
baseline_dealers$mid_maize.owner.agree.longe5.q51 <-  baseline_dealers$check.owner.agree.longe5.q51 #x
baseline_dealers$mid_maize.owner.agree.longe4.q63 <-  baseline_dealers$check.owner.agree.longe4.q63 #x

baseline_dealers$mid_maize.owner.agree.long10h.q26[baseline_dealers$mid_maize.owner.agree.long10h.q26=="n/a"]<-NA #x
baseline_dealers$mid_maize.owner.agree.longe7h.q38[baseline_dealers$mid_maize.owner.agree.longe7h.q38=="n/a"]<-NA #x
baseline_dealers$mid_maize.owner.agree.longe5.q51[baseline_dealers$mid_maize.owner.agree.longe5.q51=="n/a"]<-NA #x
baseline_dealers$mid_maize.owner.agree.longe4.q63[baseline_dealers$mid_maize.owner.agree.longe4.q63=="n/a"]<-NA #x

baseline_dealers$mid_maize.owner.agree.long10h.q26 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.long10h.q26)) #x
baseline_dealers$mid_maize.owner.agree.longe7h.q38 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.longe7h.q38)) #x
baseline_dealers$mid_maize.owner.agree.longe5.q51 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.longe5.q51)) #x
baseline_dealers$mid_maize.owner.agree.longe4.q63 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.longe4.q63)) #x

baseline_dealers$av_salesprices <- rowMeans(baseline_dealers[c("maize.owner.agree.long10h.q26"
                                                               ,"maize.owner.agree.longe7h.q38"
                                                               ,"maize.owner.agree.longe5.q51"
                                                               ,"maize.owner.agree.longe4.q63")],na.rm = T)
baseline_dealers <- trim("av_salesprices",baseline_dealers,trim_perc=.02)

baseline_dealers$mid_av_salesprices <- rowMeans(baseline_dealers[c("mid_maize.owner.agree.long10h.q26"
                                                                   ,"mid_maize.owner.agree.longe7h.q38"
                                                                   ,"mid_maize.owner.agree.longe5.q51"
                                                                   ,"mid_maize.owner.agree.longe4.q63")],na.rm = T) #x
baseline_dealers <- trim("mid_av_salesprices",baseline_dealers,trim_perc=.02) #x

#3. Seed revenue in UGX: quantities sold * prices of hybrid and open-pollinated maize variety
baseline_dealers$revenue_long10h.q25 <- (baseline_dealers$maize.owner.agree.long10h.q25*baseline_dealers$maize.owner.agree.long10h.q26)
baseline_dealers$revenue_long10h.q25[baseline_dealers$maize.owner.agree.q20=="0"] <- 0

baseline_dealers$revenue_longe7h <- (baseline_dealers$maize.owner.agree.longe7h.q37*baseline_dealers$maize.owner.agree.longe7h.q38)
baseline_dealers$revenue_longe7h[baseline_dealers$maize.owner.agree.q32=="0"] <- 0

baseline_dealers$revenue_longe5 <- (baseline_dealers$maize.owner.agree.longe5.q50*baseline_dealers$maize.owner.agree.longe5.q51)
baseline_dealers$revenue_longe5[baseline_dealers$maize.owner.agree.q45=="0"] <- 0

baseline_dealers$revenue_longe4 <- (baseline_dealers$maize.owner.agree.longe4.q62*baseline_dealers$maize.owner.agree.longe4.q63)
baseline_dealers$revenue_longe4[baseline_dealers$maize.owner.agree.q57=="0"] <- 0

baseline_dealers$revenue <- (baseline_dealers$revenue_long10h.q25+baseline_dealers$revenue_longe7h
                             +baseline_dealers$revenue_longe5+baseline_dealers$revenue_longe4)

baseline_dealers$revenue <- baseline_dealers$revenue/1000000
baseline_dealers <- trim("revenue",baseline_dealers,trim_perc=.02)
baseline_dealers$revenue <- ihs(baseline_dealers$revenue)

baseline_dealers$mid_revenue_long10h.q25 <- (baseline_dealers$mid_maize.owner.agree.long10h.q25*baseline_dealers$mid_maize.owner.agree.long10h.q26) #x
baseline_dealers$mid_revenue_long10h.q25[baseline_dealers$mid_maize.owner.agree.q20=="0"] <- 0 #x

baseline_dealers$mid_revenue_longe7h <- (baseline_dealers$mid_maize.owner.agree.longe7h.q37*baseline_dealers$mid_maize.owner.agree.longe7h.q38) #x
baseline_dealers$mid_revenue_longe7h[baseline_dealers$mid_maize.owner.agree.q32=="0"] <- 0 #x

baseline_dealers$mid_revenue_longe5 <- (baseline_dealers$mid_maize.owner.agree.longe5.q50*baseline_dealers$mid_maize.owner.agree.longe5.q51) #x
baseline_dealers$mid_revenue_longe5[baseline_dealers$mid_maize.owner.agree.q45=="0"] <- 0 #x

baseline_dealers$mid_revenue_longe4 <- (baseline_dealers$mid_maize.owner.agree.longe4.q62*baseline_dealers$mid_maize.owner.agree.longe4.q63) #x
baseline_dealers$mid_revenue_longe4[baseline_dealers$mid_maize.owner.agree.q57=="0"] <- 0 #x

baseline_dealers$mid_revenue <- (baseline_dealers$mid_revenue_long10h.q25+baseline_dealers$mid_revenue_longe7h
                             +baseline_dealers$mid_revenue_longe5+baseline_dealers$mid_revenue_longe4) #x
baseline_dealers$mid_revenue <- baseline_dealers$mid_revenue/1000000 #x

baseline_dealers <- trim("mid_revenue",baseline_dealers,trim_perc=.02) #x
baseline_dealers$mid_revenue <- ihs(baseline_dealers$mid_revenue) #x


#4. Number of customers who bought maize seed on average day at beginning of last season
baseline_dealers$maize.owner.agree.q7[baseline_dealers$maize.owner.agree.q7==999] <- NA
baseline_dealers <- trim("maize.owner.agree.q7",baseline_dealers,trim_perc=.02)
baseline_dealers$maize.owner.agree.q7 <- ihs(baseline_dealers$maize.owner.agree.q7)

baseline_dealers$mid_maize.owner.agree.q7 <- baseline_dealers$check.owner.agree.q7 #x
baseline_dealers$mid_maize.owner.agree.q7[baseline_dealers$mid_maize.owner.agree.q7==999] <- NA #x
baseline_dealers$mid_maize.owner.agree.q7 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.q7)) #x
baseline_dealers <- trim("mid_maize.owner.agree.q7",baseline_dealers,trim_perc=.02) #x
baseline_dealers$mid_maize.owner.agree.q7 <- ihs(baseline_dealers$mid_maize.owner.agree.q7) #x

#5. Moisture content of random seed bag: now not controlling for baseline
baseline_dealers <- trim("reading",baseline_dealers,trim_perc=.02)

baseline_dealers$mid_reading <- baseline_dealers$reading_end #x
baseline_dealers$mid_reading_unadj <- baseline_dealers$reading_end #x
baseline_dealers$mid_reading <- as.numeric(as.character(baseline_dealers$mid_reading)) #x
baseline_dealers <- trim("mid_reading",baseline_dealers,trim_perc=.02) #x

#6. Index of capital-intensive seed handling and storage practices observed by enumerator
###Anderson, 2008: https://are.berkeley.edu/~mlanderson/pdf/Anderson%202008a.pdf p. 1485
#Is the roof leak-proof? yes=good
baseline_dealers$mid_maize.owner.agree.temp.q72 <- NA
baseline_dealers$mid_maize.owner.agree.temp.q72[baseline_dealers$check.owner.agree.temp.q72=="Yes"]<-1
baseline_dealers$mid_maize.owner.agree.temp.q72[baseline_dealers$check.owner.agree.temp.q72=="No"]<-0

#Is the roof insulated to keep heat out? yes=good
baseline_dealers$mid_maize.owner.agree.temp.q73 <- NA
baseline_dealers$mid_maize.owner.agree.temp.q73[baseline_dealers$check.owner.agree.temp.q73=="Yes"]<-1
baseline_dealers$mid_maize.owner.agree.temp.q73[baseline_dealers$check.owner.agree.temp.q73=="No"]<-0

#Are the walls insulated to keep the heat out? yes=good
baseline_dealers$mid_maize.owner.agree.temp.q74 <- NA
baseline_dealers$mid_maize.owner.agree.temp.q74[baseline_dealers$check.owner.agree.temp.q74=="Yes"]<-1
baseline_dealers$mid_maize.owner.agree.temp.q74[baseline_dealers$check.owner.agree.temp.q74=="No"]<-0

#Is the area ventilated? yes=good
baseline_dealers$mid_maize.owner.agree.temp.q75 <- NA
baseline_dealers$mid_maize.owner.agree.temp.q75[baseline_dealers$check.owner.agree.temp.q75=="Yes"]<-1
baseline_dealers$mid_maize.owner.agree.temp.q75[baseline_dealers$check.owner.agree.temp.q75=="No"]<-0

#Do you see any official certificates displayed in the shop (e.g. inspection, trainings, registration with association)? yes=good
baseline_dealers$mid_maize.owner.agree.temp.q81 <- NA
baseline_dealers$mid_maize.owner.agree.temp.q81[baseline_dealers$check.owner.agree.temp.q81=="Yes"]<-1
baseline_dealers$mid_maize.owner.agree.temp.q81[baseline_dealers$check.owner.agree.temp.q81=="No"]<-0

#What do you do with seed that have exceeded shelf live (expired)? yes=good
#(a This has never happened/b Return to supplier/c Sell at discount/d Given away/e Thrown away/f sell at normal price/g mix with other seed/96 Other)
baseline_dealers$mid_maize.owner.agree.q83.a<-baseline_dealers$check.owner.agree.q83.a #x
baseline_dealers$mid_maize.owner.agree.q83.b<-baseline_dealers$check.owner.agree.q83.b #x
baseline_dealers$mid_maize.owner.agree.q83.c<-baseline_dealers$check.owner.agree.q83.c #x
baseline_dealers$mid_maize.owner.agree.q83.d<-baseline_dealers$check.owner.agree.q83.d #x
baseline_dealers$mid_maize.owner.agree.q83.e<-baseline_dealers$check.owner.agree.q83.e #x
baseline_dealers$mid_maize.owner.agree.q83.f<-baseline_dealers$check.owner.agree.q83.f #x
baseline_dealers$mid_maize.owner.agree.q83.g<-baseline_dealers$check.owner.agree.q83.g #x
baseline_dealers$mid_maize.owner.agree.q83.96<-baseline_dealers$check.owner.agree.q83.96 #x

baseline_dealers$goodpractice_expired2 <- NA
baseline_dealers$goodpractice_expired2[baseline_dealers$maize.owner.agree.q83.a==1] <- 1 #x
baseline_dealers$goodpractice_expired2[baseline_dealers$maize.owner.agree.q83.b==1] <- 1 #x
baseline_dealers$goodpractice_expired2[baseline_dealers$maize.owner.agree.q83.e==1] <- 1 #x
baseline_dealers$goodpractice_expired2[baseline_dealers$maize.owner.agree.q83.c==1] <- 0 #x
baseline_dealers$goodpractice_expired2[baseline_dealers$maize.owner.agree.q83.d==1] <- 0 #x
baseline_dealers$goodpractice_expired2[baseline_dealers$maize.owner.agree.q83.f==1] <- 0 #x
baseline_dealers$goodpractice_expired2[baseline_dealers$maize.owner.agree.q83.g==1] <- 0 #x

baseline_dealers$mid_goodpractice_expired2 <- NA
baseline_dealers$mid_goodpractice_expired2[baseline_dealers$mid_maize.owner.agree.q83.a=="True"] <- 1 #x
baseline_dealers$mid_goodpractice_expired2[baseline_dealers$mid_maize.owner.agree.q83.b=="True"] <- 1 #x
baseline_dealers$mid_goodpractice_expired2[baseline_dealers$mid_maize.owner.agree.q83.e=="True"] <- 1 #x
baseline_dealers$mid_goodpractice_expired2[baseline_dealers$mid_maize.owner.agree.q83.c=="True"] <- 0 #x
baseline_dealers$mid_goodpractice_expired2[baseline_dealers$mid_maize.owner.agree.q83.d=="True"] <- 0 #x
baseline_dealers$mid_goodpractice_expired2[baseline_dealers$mid_maize.owner.agree.q83.f=="True"] <- 0 #x
baseline_dealers$mid_goodpractice_expired2[baseline_dealers$mid_maize.owner.agree.q83.g=="True"] <- 0 #x


###2. Demean and divide outcomes by control group standard deviation (normalizes outcomes to be on comparable scale)
#https://github.com/cdsamii/make_index/blob/master/r/index_comparison.R
#function to standardize columns of matrix, sgroup = control group = logical vector
matStand <- function(x, sgroup = rep(TRUE, nrow(x))){
  for(j in 1:ncol(x)){
    x[,j] <- (x[,j] - mean(x[sgroup,j],na.rm = T))/sd(x[sgroup,j],na.rm = T)
  }
  return(x)
}

###3. Define groupings/areas/domains of outcomes: each outcome is assigned to one of these areas
variables_practices_cap_mid <- cbind(baseline_dealers$mid_maize.owner.agree.temp.q72,baseline_dealers$mid_maize.owner.agree.temp.q73
                                     ,baseline_dealers$mid_maize.owner.agree.temp.q74,baseline_dealers$mid_maize.owner.agree.temp.q75
                                     ,baseline_dealers$mid_maize.owner.agree.temp.q81,baseline_dealers$mid_goodpractice_expired2) #x
variables_practices_cap_base <- cbind(baseline_dealers$maize.owner.agree.temp.q72,baseline_dealers$maize.owner.agree.temp.q73
                                      ,baseline_dealers$maize.owner.agree.temp.q74,baseline_dealers$maize.owner.agree.temp.q75
                                      ,baseline_dealers$maize.owner.agree.temp.q81,baseline_dealers$goodpractice_expired2)


###4. Create index: weighted average of outcomes for individual i in area j
###weight inputs (outcomes) by inverse of covariance matrix of transformed outcomes in area j
###simple way: set weight on each outcome equal to sum of its row entries in inverted covariance matrix for area j

#function that takes in data in matrix format and returns IC weights and ICW index
#wgts argument: weights can be incorporated
#revcols argument: takes vector indicating which columns should have reversed values (standardized values * -1) prior to construction of index
#because: For all outcomes, switch signs where necessary so that the positive direction always indicates a "better" outcome.
icwIndex <- function(	xmat,
                      #wgts=rep(1, nrow(xmat)), #nrow: number of rows present in xmat --> many 1s
                      revcols = NULL,
                      sgroup = rep(TRUE, nrow(xmat))){
  X <- matStand(xmat, sgroup)
  if(length(revcols)>0){
    X[,revcols] <-  -1*X[,revcols]
  }
  i.vec <- as.matrix(rep(1,ncol(xmat)))
  #Sx <- cov.wt(X, wt=wgts)[[1]]
  #list with estimates of the weighted covariance matrix and the mean of the data
  Sx <- cov(X,use = "pairwise.complete.obs")
  #cov: covariance of x and y if these are vectors/covariances between columns of x and columns of y are computed if these are matrices
  #use = "everything" produces NAs for the index.
  #use = "all.obs" produces an error.
  #use = "complete.obs" and use = "na.or.complete": works, NAs are handled by casewise deletion.
  #use = "pairwise.complete.obs": works, covariance between each pair of variables is computed using all complete pairs of observations on those variables
  weights <- solve(t(i.vec)%*%solve(Sx)%*%i.vec)%*%t(i.vec)%*%solve(Sx)
  index <- t(solve(t(i.vec)%*%solve(Sx)%*%i.vec)%*%t(i.vec)%*%solve(Sx)%*%t(X))
  return(list(weights = weights, index = index))
}

index_practices_cap_mid <- icwIndex(xmat=variables_practices_cap_mid) #x
baseline_dealers$index_practices_cap_mid <- index_practices_cap_mid$index #x

index_practices_cap_base <- icwIndex(xmat=variables_practices_cap_base)
baseline_dealers$index_practices_cap_base <- index_practices_cap_base$index

#7. Index of labor-intensive seed handling and storage practices observed by enumerator
#Are seed stored in a dedicated area, away from other merchandize? yes=good
baseline_dealers$mid_maize.owner.agree.temp.q69 <- baseline_dealers$check.owner.agree.temp.q69 #x
baseline_dealers$mid_maize.owner.agree.temp.q69[baseline_dealers$mid_maize.owner.agree.temp.q69=="n/a"]<-NA
baseline_dealers$mid_maize.owner.agree.temp.q69 <- ifelse(baseline_dealers$mid_maize.owner.agree.temp.q69=="Yes",1,0) #x

#Do you have a problem with rats or pests (insects, rats)? yes=BAD
baseline_dealers$mid_maize.owner.agree.temp.q71 <- baseline_dealers$check.owner.agree.temp.q71 #x
baseline_dealers$mid_maize.owner.agree.temp.q71[baseline_dealers$mid_maize.owner.agree.temp.q71=="n/a"]<-NA
baseline_dealers$mid_maize.owner.agree.temp.q71<-ifelse(baseline_dealers$mid_maize.owner.agree.temp.q71=="Yes",1,0) #x

#Lighting conditions in area where seed is stored? yes=good
#Wilberforce: ambient lighting condition is ideal for seed storage
baseline_dealers$mid_maize.owner.agree.temp.q78 <- baseline_dealers$check.owner.agree.temp.q78 #x
baseline_dealers$mid_maize.owner.agree.temp.q78[baseline_dealers$mid_maize.owner.agree.temp.q78=="n/a"]<-NA
baseline_dealers$mid_lighting[baseline_dealers$mid_maize.owner.agree.temp.q78==1] <- 0 #x
baseline_dealers$mid_lighting[baseline_dealers$mid_maize.owner.agree.temp.q78==2] <- 1 #x
baseline_dealers$mid_lighting[baseline_dealers$mid_maize.owner.agree.temp.q78==3] <- 0 #x

#On what surface are seed stored? yes=good
baseline_dealers$mid_maize.owner.agree.temp.q79 <- baseline_dealers$check.owner.agree.temp.q79 #x
baseline_dealers$mid_maize.owner.agree.temp.q79[baseline_dealers$mid_maize.owner.agree.temp.q79=="n/a"]<-NA
baseline_dealers$mid_surface[baseline_dealers$mid_maize.owner.agree.temp.q79==1] <- 0 #x
baseline_dealers$mid_surface[baseline_dealers$mid_maize.owner.agree.temp.q79==2] <- 0 #x
baseline_dealers$mid_surface[baseline_dealers$mid_maize.owner.agree.temp.q79==3] <- 0 #x
baseline_dealers$mid_surface[baseline_dealers$mid_maize.owner.agree.temp.q79==4] <- 1 #x
baseline_dealers$mid_surface[baseline_dealers$mid_maize.owner.agree.temp.q79==5] <- 1 #x
baseline_dealers$mid_surface[baseline_dealers$mid_maize.owner.agree.temp.q79==96] <- NA #because 2 names #x

#Do you see maize seed that is stored in open bags or open containers? yes=BAD
baseline_dealers$mid_maize.owner.agree.temp.q80 <- baseline_dealers$check.owner.agree.temp.q80 #x
baseline_dealers$mid_maize.owner.agree.temp.q80[baseline_dealers$mid_maize.owner.agree.temp.q80=="n/a"]<-NA
baseline_dealers$mid_maize.owner.agree.temp.q80<-ifelse(baseline_dealers$mid_maize.owner.agree.temp.q80=="Yes",1,0) #x

#On a scale of 1 to 5, rate this shop in terms of cleanness and professionality yes=good
baseline_dealers$mid_maize.owner.agree.temp.q82 <- baseline_dealers$check.owner.agree.temp.q82 #x
baseline_dealers$mid_maize.owner.agree.temp.q82[baseline_dealers$mid_maize.owner.agree.temp.q82=="n/a"]<-NA
baseline_dealers$mid_maize.owner.agree.temp.q82 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.temp.q82))

variables_practices_lab_mid <- cbind(baseline_dealers$mid_maize.owner.agree.temp.q69,baseline_dealers$mid_maize.owner.agree.temp.q71
                                     ,baseline_dealers$mid_lighting,baseline_dealers$mid_surface
                                     ,baseline_dealers$mid_maize.owner.agree.temp.q80,baseline_dealers$mid_maize.owner.agree.temp.q82) #x
variables_practices_lab_base <- cbind(baseline_dealers$maize.owner.agree.temp.q69,baseline_dealers$maize.owner.agree.temp.q71
                                      ,baseline_dealers$lighting,baseline_dealers$surface,baseline_dealers$maize.owner.agree.temp.q80
                                      ,baseline_dealers$maize.owner.agree.temp.q82)

index_practices_lab_mid <- icwIndex(xmat=variables_practices_lab_mid,revcols = c(2,5)) #x
baseline_dealers$index_practices_lab_mid <- index_practices_lab_mid$index #x

index_practices_lab_base <- icwIndex(xmat=variables_practices_lab_base,revcols = c(2,5))
baseline_dealers$index_practices_lab_base <- index_practices_lab_base$index

#8. Index of all seed handling and storage practices observed by enumerator
variables_practices_all_mid <- cbind(baseline_dealers$mid_maize.owner.agree.temp.q72,baseline_dealers$mid_maize.owner.agree.temp.q73
                                     ,baseline_dealers$mid_maize.owner.agree.temp.q74,baseline_dealers$mid_maize.owner.agree.temp.q75
                                     ,baseline_dealers$mid_maize.owner.agree.temp.q81,baseline_dealers$mid_goodpractice_expired2
                                     ,baseline_dealers$mid_maize.owner.agree.temp.q69,baseline_dealers$mid_maize.owner.agree.temp.q71
                                     ,baseline_dealers$mid_lighting,baseline_dealers$mid_surface
                                     ,baseline_dealers$mid_maize.owner.agree.temp.q80,baseline_dealers$mid_maize.owner.agree.temp.q82) #x
variables_practices_all_base <- cbind(baseline_dealers$maize.owner.agree.temp.q72,baseline_dealers$maize.owner.agree.temp.q73
                                      ,baseline_dealers$maize.owner.agree.temp.q74,baseline_dealers$maize.owner.agree.temp.q75
                                      ,baseline_dealers$maize.owner.agree.temp.q81,baseline_dealers$goodpractice_expired2
                                      ,baseline_dealers$maize.owner.agree.temp.q69,baseline_dealers$maize.owner.agree.temp.q71
                                      ,baseline_dealers$lighting,baseline_dealers$surface,baseline_dealers$maize.owner.agree.temp.q80
                                      ,baseline_dealers$maize.owner.agree.temp.q82)

index_practices_all_mid <- icwIndex(xmat=variables_practices_all_mid,revcols = c(8,11)) #x
baseline_dealers$index_practices_all_mid <- index_practices_all_mid$index #x

index_practices_all_base <- icwIndex(xmat=variables_practices_all_base,revcols = c(8,11))
baseline_dealers$index_practices_all_base <- index_practices_all_base$index

#9. Index of efforts of dealer and services offered by dealer
#When farmers buy seed, do you explain how the seed should be used (seed spacing, seed rate, complementary inputs) yes=good
baseline_dealers$mid_maize.owner.agree.q85 <- baseline_dealers$check.owner.agree.q85 #x
baseline_dealers$mid_alwaysexplains[baseline_dealers$mid_maize.owner.agree.q85=="a"] <- 0 #x
baseline_dealers$mid_alwaysexplains[baseline_dealers$mid_maize.owner.agree.q85=="b"] <- 0 #x
baseline_dealers$mid_alwaysexplains[baseline_dealers$mid_maize.owner.agree.q85=="c"] <- 1 #x

#When farmers buy seed, do you usually recommend complementary inputs (fertilizer, chemical,.) yes=good
baseline_dealers$mid_maize.owner.agree.q86 <- baseline_dealers$check.owner.agree.q86 #x
baseline_dealers$mid_alwaysrecom[baseline_dealers$mid_maize.owner.agree.q86=="a"] <- 0 #x
baseline_dealers$mid_alwaysrecom[baseline_dealers$mid_maize.owner.agree.q86=="b"] <- 0 #x
baseline_dealers$mid_alwaysrecom[baseline_dealers$mid_maize.owner.agree.q86=="c"] <- 1 #x

#Do you offer extension/training to your clients on how to use improved seed varieties? yes=good
baseline_dealers$mid_maize.owner.agree.q87 <- baseline_dealers$check.owner.agree.q87 #x
baseline_dealers$mid_extension[baseline_dealers$mid_maize.owner.agree.q87=="1"] <- 0 #x
baseline_dealers$mid_extension[baseline_dealers$mid_maize.owner.agree.q87=="2"] <- 1 #x
baseline_dealers$mid_extension[baseline_dealers$mid_maize.owner.agree.q87=="3"] <- 1 #x

#Did you offer discounts to clients that buy large quantities of maize seed during the second season of 2020? yes=good
baseline_dealers$mid_maize.owner.agree.q88 <- baseline_dealers$check.owner.agree.q88 #x
baseline_dealers$mid_maize.owner.agree.q88[baseline_dealers$mid_maize.owner.agree.q88=="n/a"]<-NA
baseline_dealers$mid_maize.owner.agree.q88<-ifelse(baseline_dealers$mid_maize.owner.agree.q88=="Yes",1,0) #x

#What is that smallest package of improved seed (OPV/hybird) that you stocked during this season (without repackaging) yes=good
baseline_dealers$mid_maize.owner.agree.q89 <- baseline_dealers$check.owner.agree.q89 #x
baseline_dealers$mid_q89_bin[baseline_dealers$mid_maize.owner.agree.q89=="1"] <- 1 #x
baseline_dealers$mid_q89_bin[baseline_dealers$mid_maize.owner.agree.q89=="2"] <- 0 #x
baseline_dealers$mid_q89_bin[baseline_dealers$mid_maize.owner.agree.q89=="3"] <- 0 #x
baseline_dealers$mid_q89_bin[baseline_dealers$mid_maize.owner.agree.q89=="4"] <- 0 #x
baseline_dealers$mid_q89_bin[baseline_dealers$mid_maize.owner.agree.q89=="other"] <- NA #x

#Do you provide seed on credit (pay after harvest)? yes=good
baseline_dealers$mid_maize.owner.agree.q93 <- baseline_dealers$check.owner.agree.q93 #x
baseline_dealers$mid_q93_bin[baseline_dealers$mid_maize.owner.agree.q93=="1"] <- 0 #x
baseline_dealers$mid_q93_bin[baseline_dealers$mid_maize.owner.agree.q93=="2"] <- 1 #x
baseline_dealers$mid_q93_bin[baseline_dealers$mid_maize.owner.agree.q93=="3"] <- 1 #x

#Since last season, did you receive any complaint from a customer that seed you sold was not good? yes=BAD
baseline_dealers$mid_maize.owner.agree.q96 <- baseline_dealers$check.owner.agree.q96 #x
baseline_dealers$mid_maize.owner.agree.q96[baseline_dealers$mid_maize.owner.agree.q96=="n/a"]<-NA
baseline_dealers$mid_maize.owner.agree.q96<-ifelse(baseline_dealers$mid_maize.owner.agree.q96=="Yes",1,0) #x

#What payment modalities do you accept?
baseline_dealers$mid_maize.owner.agree.q97.b <- baseline_dealers$check.owner.agree.q97.b #x
baseline_dealers$mid_maize.owner.agree.q97.b[baseline_dealers$mid_maize.owner.agree.q97.b=="n/a"]<-NA
baseline_dealers$mid_maize.owner.agree.q97.b<-ifelse(baseline_dealers$mid_maize.owner.agree.q97.b=="True",1,0) #x

variables_efforts_mid <- cbind(baseline_dealers$mid_alwaysexplains,baseline_dealers$mid_alwaysrecom,baseline_dealers$mid_extension
                               ,baseline_dealers$mid_maize.owner.agree.q88,baseline_dealers$mid_q93_bin
                               ,baseline_dealers$mid_maize.owner.agree.q96,baseline_dealers$mid_maize.owner.agree.q97.b) #x
variables_efforts_base <- cbind(baseline_dealers$alwaysexplains,baseline_dealers$alwaysrecom,baseline_dealers$extension
                                ,baseline_dealers$maize.owner.agree.q88,baseline_dealers$q93_bin
                                ,baseline_dealers$maize.owner.agree.q96,baseline_dealers$maize.owner.agree.q97.b)

index_efforts_mid <- icwIndex(xmat=variables_efforts_mid,revcols = c(6)) #x
baseline_dealers$index_efforts_mid <- index_efforts_mid$index #x

index_efforts_base <- icwIndex(xmat=variables_efforts_base,revcols = c(6))
baseline_dealers$index_efforts_base <- index_efforts_base$index

#10. Overall index of primary agro-input dealer outcome variables

variables_overall_prim_dealer_mid <- cbind(baseline_dealers$mid_quantitysold,baseline_dealers$mid_revenue
                                           ,baseline_dealers$mid_maize.owner.agree.q7
                                           ,baseline_dealers$index_practices_cap_mid,baseline_dealers$index_practices_lab_mid
                                           ,baseline_dealers$index_efforts_mid)
variables_overall_prim_dealer_base <- cbind(baseline_dealers$quantitysold,baseline_dealers$revenue
                                            ,baseline_dealers$maize.owner.agree.q7
                                            ,baseline_dealers$index_practices_cap_base,baseline_dealers$index_practices_lab_base
                                            ,baseline_dealers$index_efforts_base)

index_overall_prim_dealer_mid <- icwIndex(xmat=variables_overall_prim_dealer_mid) #x
baseline_dealers$index_overall_prim_dealer_mid <- index_overall_prim_dealer_mid$index #x

index_overall_prim_dealer_base <- icwIndex(xmat=variables_overall_prim_dealer_base)
baseline_dealers$index_overall_prim_dealer_base <- index_overall_prim_dealer_base$index

################################################################################################################################################################################

###
#1#
###

results_dealer_prim <- c("mid_quantitysold"                #1
                         ,"mid_av_salesprices"             #2
                         ,"mid_revenue"                    #3
                         ,"mid_maize.owner.agree.q7"       #4
                         ,"index_practices_cap_mid"        #5
                         ,"index_practices_lab_mid"        #6
                         ,"index_practices_all_mid"        #7
                         ,"index_efforts_mid"              #8 
                         ,"index_overall_prim_dealer_mid") #9
results_dealer_prim_base <- c("quantitysold"
                              ,"av_salesprices"
                              ,"revenue"
                              ,"maize.owner.agree.q7"
                              ,"index_practices_cap_base"
                              ,"index_practices_lab_base"
                              ,"index_practices_all_base"
                              ,"index_efforts_base"
                              ,"index_overall_prim_dealer_base")

baseline_dealers[results_dealer_prim_base] <- lapply(baseline_dealers[results_dealer_prim_base],function(x)x - mean(x,na.rm = T))

df_means_end_D_prim <- array(NA,dim=c(5,10))

for (i in 1:length(results_dealer_prim)){
  df_means_end_D_prim[1,i] <- sum(baseline_dealers[results_dealer_prim[i]], na.rm=T)/(nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_prim[i]])))
  df_means_end_D_prim[2,i] <- sqrt(var(baseline_dealers[results_dealer_prim[i]], na.rm=T))
  df_means_end_D_prim[3,i] <- nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_prim[i]]))-sum(is.na(baseline_dealers[results_dealer_prim_base[i]]))+sum(is.na(baseline_dealers[results_dealer_prim[i]])&is.na(baseline_dealers[results_dealer_prim_base[i]]))
  df_means_end_D_prim[4,i] <- min(baseline_dealers[results_dealer_prim[i]], na.rm=T)
  df_means_end_D_prim[5,i] <- max(baseline_dealers[results_dealer_prim[i]], na.rm=T)}

df_ols_end_D_prim <- array(NA,dim=c(3,3,10))

###
#2#
###

baseline_dealers$training_control[baseline_dealers$training==0] <- TRUE
baseline_dealers$training_control[baseline_dealers$training==1] <- FALSE

#6.
index_practices_cap_mid <- icwIndex(xmat=variables_practices_cap_mid,sgroup = baseline_dealers$training_control)
baseline_dealers$index_practices_cap_midT <- index_practices_cap_mid$index

index_practices_cap_base <- icwIndex(xmat=variables_practices_cap_base,sgroup = baseline_dealers$training_control)
baseline_dealers$index_practices_cap_baseT <- index_practices_cap_base$index

#7.
index_practices_lab_mid <- icwIndex(xmat=variables_practices_lab_mid,sgroup = baseline_dealers$training_control,revcols = c(2,5))
baseline_dealers$index_practices_lab_midT <- index_practices_lab_mid$index

index_practices_lab_base <- icwIndex(xmat=variables_practices_lab_base,sgroup = baseline_dealers$training_control,revcols = c(2,5))
baseline_dealers$index_practices_lab_baseT <- index_practices_lab_base$index

#8.
index_practices_all_mid <- icwIndex(xmat=variables_practices_all_mid,sgroup = baseline_dealers$training_control,revcols = c(8,11))
baseline_dealers$index_practices_all_midT <- index_practices_all_mid$index

index_practices_all_base <- icwIndex(xmat=variables_practices_all_base,sgroup = baseline_dealers$training_control,revcols = c(8,11))
baseline_dealers$index_practices_all_baseT <- index_practices_all_base$index

#9.
index_efforts_mid <- icwIndex(xmat=variables_efforts_mid,sgroup = baseline_dealers$training_control,revcols = c(6))
baseline_dealers$index_efforts_midT <- index_efforts_mid$index

index_efforts_base <- icwIndex(xmat=variables_efforts_base,sgroup = baseline_dealers$training_control,revcols = c(6))
baseline_dealers$index_efforts_baseT <- index_efforts_base$index

#10.
index_overall_prim_dealer_mid <- icwIndex(xmat=variables_overall_prim_dealer_mid,sgroup = baseline_dealers$training_control)
baseline_dealers$index_overall_prim_dealer_midT <- index_overall_prim_dealer_mid$index

index_overall_prim_dealer_base <- icwIndex(xmat=variables_overall_prim_dealer_base,sgroup = baseline_dealers$training_control)
baseline_dealers$index_overall_prim_dealer_baseT <- index_overall_prim_dealer_base$index

results_dealer_prim <- c("mid_quantitysold"
                         ,"mid_av_salesprices"
                         ,"mid_revenue"
                         ,"mid_maize.owner.agree.q7"
                         ,"index_practices_cap_midT"
                         ,"index_practices_lab_midT"
                         ,"index_practices_all_midT"
                         ,"index_efforts_midT"
                         ,"index_overall_prim_dealer_midT")
results_dealer_prim_base <- c("quantitysold"
                              ,"av_salesprices"
                              ,"revenue"
                              ,"maize.owner.agree.q7"
                              ,"index_practices_cap_baseT"
                              ,"index_practices_lab_baseT"
                              ,"index_practices_all_baseT"
                              ,"index_efforts_baseT"
                              ,"index_overall_prim_dealer_baseT")

baseline_dealers[results_dealer_prim_base] <- lapply(baseline_dealers[results_dealer_prim_base],function(x)x - mean(x,na.rm = T))

baseline_dealers$training_demeaned <- baseline_dealers$training - mean(baseline_dealers$training,na.rm = T)
baseline_dealers$clearing_demeaned <- baseline_dealers$clearing - mean(baseline_dealers$clearing,na.rm = T)
baseline_dealers$farmer_demeaned <- baseline_dealers$farmer - mean(baseline_dealers$farmer,na.rm = T)

for (i in 1:length(results_dealer_prim)){
  ols <- lm(as.formula(paste(paste(results_dealer_prim[i],"training*clearing_demeaned*farmer_demeaned",sep="~"),results_dealer_prim_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_prim[i],"training*clearing_demeaned*farmer_demeaned",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")

  df_ols_end_D_prim[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_ols_end_D_prim[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_ols_end_D_prim[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]}

###
#3#
###

baseline_dealers$clearing_control[baseline_dealers$clearing==0] <- TRUE
baseline_dealers$clearing_control[baseline_dealers$clearing==1] <- FALSE

#6.
index_practices_cap_mid <- icwIndex(xmat=variables_practices_cap_mid,sgroup = baseline_dealers$clearing_control)
baseline_dealers$index_practices_cap_midC <- index_practices_cap_mid$index

index_practices_cap_base <- icwIndex(xmat=variables_practices_cap_base,sgroup = baseline_dealers$clearing_control)
baseline_dealers$index_practices_cap_baseC <- index_practices_cap_base$index

#7.
index_practices_lab_mid <- icwIndex(xmat=variables_practices_lab_mid,sgroup = baseline_dealers$clearing_control,revcols = c(2,5))
baseline_dealers$index_practices_lab_midC <- index_practices_lab_mid$index

index_practices_lab_base <- icwIndex(xmat=variables_practices_lab_base,sgroup = baseline_dealers$clearing_control,revcols = c(2,5))
baseline_dealers$index_practices_lab_baseC <- index_practices_lab_base$index

#8.
index_practices_all_mid <- icwIndex(xmat=variables_practices_all_mid,sgroup = baseline_dealers$clearing_control,revcols = c(8,11))
baseline_dealers$index_practices_all_midC <- index_practices_all_mid$index

index_practices_all_base <- icwIndex(xmat=variables_practices_all_base,sgroup = baseline_dealers$clearing_control,revcols = c(8,11))
baseline_dealers$index_practices_all_baseC <- index_practices_all_base$index

#9.
index_efforts_mid <- icwIndex(xmat=variables_efforts_mid,sgroup = baseline_dealers$clearing_control,revcols = c(6))
baseline_dealers$index_efforts_midC <- index_efforts_mid$index

index_efforts_base <- icwIndex(xmat=variables_efforts_base,sgroup = baseline_dealers$clearing_control,revcols = c(6))
baseline_dealers$index_efforts_baseC <- index_efforts_base$index

#10.
index_overall_prim_dealer_mid <- icwIndex(xmat=variables_overall_prim_dealer_mid,sgroup = baseline_dealers$clearing_control)
baseline_dealers$index_overall_prim_dealer_midC <- index_overall_prim_dealer_mid$index

index_overall_prim_dealer_base <- icwIndex(xmat=variables_overall_prim_dealer_base,sgroup = baseline_dealers$clearing_control)
baseline_dealers$index_overall_prim_dealer_baseC <- index_overall_prim_dealer_base$index

results_dealer_prim <- c("mid_quantitysold"
                         ,"mid_av_salesprices"
                         ,"mid_revenue"
                         ,"mid_maize.owner.agree.q7"
                         ,"index_practices_cap_midC"
                         ,"index_practices_lab_midC"
                         ,"index_practices_all_midC"
                         ,"index_efforts_midC"
                         ,"index_overall_prim_dealer_midC")
results_dealer_prim_base <- c("quantitysold"
                              ,"av_salesprices"
                              ,"revenue"
                              ,"maize.owner.agree.q7"
                              ,"index_practices_cap_baseC"
                              ,"index_practices_lab_baseC"
                              ,"index_practices_all_baseC"
                              ,"index_efforts_baseC"
                              ,"index_overall_prim_dealer_baseC")

baseline_dealers[results_dealer_prim_base] <- lapply(baseline_dealers[results_dealer_prim_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_dealer_prim)){
  ols <- lm(as.formula(paste(paste(results_dealer_prim[i],"training_demeaned*clearing*farmer_demeaned",sep="~"),results_dealer_prim_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_prim[i],"training_demeaned*clearing*farmer_demeaned",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")

  df_ols_end_D_prim[1,2,i] <- coef_test(ols, vcov_cluster)$beta[3]
  df_ols_end_D_prim[2,2,i] <- coef_test(ols, vcov_cluster)$SE[3]
  df_ols_end_D_prim[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]}

###
#4#
###

baseline_dealers$farmer_control[baseline_dealers$farmer==0] <- TRUE
baseline_dealers$farmer_control[baseline_dealers$farmer==1] <- FALSE

#6.
index_practices_cap_mid <- icwIndex(xmat=variables_practices_cap_mid,sgroup = baseline_dealers$farmer_control)
baseline_dealers$index_practices_cap_midF <- index_practices_cap_mid$index

index_practices_cap_base <- icwIndex(xmat=variables_practices_cap_base,sgroup = baseline_dealers$farmer_control)
baseline_dealers$index_practices_cap_baseF <- index_practices_cap_base$index

#7.
index_practices_lab_mid <- icwIndex(xmat=variables_practices_lab_mid,sgroup = baseline_dealers$farmer_control,revcols = c(2,5))
baseline_dealers$index_practices_lab_midF <- index_practices_lab_mid$index

index_practices_lab_base <- icwIndex(xmat=variables_practices_lab_base,sgroup = baseline_dealers$farmer_control,revcols = c(2,5))
baseline_dealers$index_practices_lab_baseF <- index_practices_lab_base$index

#8.
index_practices_all_mid <- icwIndex(xmat=variables_practices_all_mid,sgroup = baseline_dealers$farmer_control,revcols = c(8,11))
baseline_dealers$index_practices_all_midF <- index_practices_all_mid$index

index_practices_all_base <- icwIndex(xmat=variables_practices_all_base,sgroup = baseline_dealers$farmer_control,revcols = c(8,11))
baseline_dealers$index_practices_all_baseF <- index_practices_all_base$index

#9.
index_efforts_mid <- icwIndex(xmat=variables_efforts_mid,sgroup = baseline_dealers$farmer_control,revcols = c(6))
baseline_dealers$index_efforts_midF <- index_efforts_mid$index

index_efforts_base <- icwIndex(xmat=variables_efforts_base,sgroup = baseline_dealers$farmer_control,revcols = c(6))
baseline_dealers$index_efforts_baseF <- index_efforts_base$index

#10.
index_overall_prim_dealer_mid <- icwIndex(xmat=variables_overall_prim_dealer_mid,sgroup = baseline_dealers$farmer_control)
baseline_dealers$index_overall_prim_dealer_midF <- index_overall_prim_dealer_mid$index

index_overall_prim_dealer_base <- icwIndex(xmat=variables_overall_prim_dealer_base,sgroup = baseline_dealers$farmer_control)
baseline_dealers$index_overall_prim_dealer_baseF <- index_overall_prim_dealer_base$index

results_dealer_prim <- c("mid_quantitysold"
                         ,"mid_av_salesprices"
                         ,"mid_revenue"
                         ,"mid_maize.owner.agree.q7"
                         ,"index_practices_cap_midF"
                         ,"index_practices_lab_midF"
                         ,"index_practices_all_midF"
                         ,"index_efforts_midF"
                         ,"index_overall_prim_dealer_midF")
results_dealer_prim_base <- c("quantitysold"
                              ,"av_salesprices"
                              ,"revenue"
                              ,"maize.owner.agree.q7"
                              ,"index_practices_cap_baseF"
                              ,"index_practices_lab_baseF"
                              ,"index_practices_all_baseF"
                              ,"index_efforts_baseF"
                              ,"index_overall_prim_dealer_baseF")

baseline_dealers[results_dealer_prim_base] <- lapply(baseline_dealers[results_dealer_prim_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_dealer_prim)){
  ols <- lm(as.formula(paste(paste(results_dealer_prim[i],"training_demeaned*clearing_demeaned*farmer",sep="~"),results_dealer_prim_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_prim[i],"training_demeaned*clearing_demeaned*farmer",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")

  #farmer video treatment at village/shop level so no clustering needed
  df_ols_end_D_prim[1,3,i] <- summary(ols)$coefficients[4,1]
  df_ols_end_D_prim[2,3,i] <- summary(ols)$coefficients[4,2]
  df_ols_end_D_prim[3,3,i] <- summary(ols)$coefficients[4,4]}

#Aker, Boumnijel, McClelland, Tierney (2012)

# ###Caro's cumbersome solution for baseline_dealers$mid_av_salesprices:
# #need mean correlation among outcomes other than outcome k (r_.k)
# #--> outcomes other than outcome k
# df_dealer_prim <- data.frame(baseline_dealers$mid_quantitysold,baseline_dealers$mid_revenue
#                              ,baseline_dealers$mid_maize.owner.agree.q7
#                              ,baseline_dealers$index_practices_cap_midF,baseline_dealers$index_practices_lab_midF
#                              ,baseline_dealers$index_practices_all_midF,baseline_dealers$index_efforts_midF)
# #no baseline_dealers$mid_av_salesprices
# #no baseline_dealers$index_overall_prim_dealer_midF
#
# #need mean correlation among outcomes other than outcome k (r_.k)
# #--> correlation among outcomes other than outcome k
# df_cor_dealer_prim <- cor(df_dealer_prim,use = "pairwise.complete.obs")
#
# #need mean correlation among outcomes other than outcome k (r_.k)
# #--> mean correlation among outcomes other than outcome k
# r_.k = (df_cor_dealer_prim[1,2]+df_cor_dealer_prim[1,3]+df_cor_dealer_prim[1,4]+df_cor_dealer_prim[1,5]+df_cor_dealer_prim[1,6]+df_cor_dealer_prim[1,7]
#         +df_cor_dealer_prim[1,8]+df_cor_dealer_prim[2,3]+df_cor_dealer_prim[2,4]+df_cor_dealer_prim[2,5]+df_cor_dealer_prim[2,6]+df_cor_dealer_prim[2,7]
#         +df_cor_dealer_prim[2,8]+df_cor_dealer_prim[3,4]+df_cor_dealer_prim[3,5]+df_cor_dealer_prim[3,6]+df_cor_dealer_prim[3,7]+df_cor_dealer_prim[3,8]
#         +df_cor_dealer_prim[4,5]+df_cor_dealer_prim[4,6]+df_cor_dealer_prim[4,7]+df_cor_dealer_prim[4,8]+df_cor_dealer_prim[5,6]+df_cor_dealer_prim[5,7]
#         +df_cor_dealer_prim[5,8]+df_cor_dealer_prim[6,7]+df_cor_dealer_prim[6,8]+df_cor_dealer_prim[7,8])/28
#
# #use formula
# M = 9
# g_k = M^(1-r_.k)
#
# p_k_T = df_ols_end_D_prim[3,1,2] #training
# p_k_CH = df_ols_end_D_prim[3,2,2] #CH
# p_k_F = df_ols_end_D_prim[3,3,2] #video
#
# padj_midquantitysold_T = 1-(1-p_k_T)^g_k
# padj_midquantitysold_CH = 1-(1-p_k_CH)^g_k
# padj_midquantitysold_F = 1-(1-p_k_F)^g_k

###Bjorn's function
adjust_p <- function(pval,df_outcome,outcome_k=1) {
  ## takes as input: pval (eg.0.231), the outcomes in the family (as a data.frame), the rank of the variable pval is for (default is the first in the df)
  if (outcome_k>0 & outcome_k<=dim(df_outcome)[2]) {
    cor_mat <- cor(df_outcome,use = "pairwise.complete.obs")[-outcome_k,-outcome_k]
    mean_cor_mat <- mean(cor_mat[lower.tri(cor_mat)])
    return(1-(1-pval)^(dim(df_outcome)[2]^(1-mean_cor_mat)))
  }
}

df_dealer_primT <- data.frame(baseline_dealers$mid_quantitysold,baseline_dealers$mid_av_salesprices,baseline_dealers$mid_revenue
                             ,baseline_dealers$mid_maize.owner.agree.q7,baseline_dealers$index_practices_cap_midT
                             ,baseline_dealers$index_practices_lab_midT,baseline_dealers$index_practices_all_midT
                             ,baseline_dealers$index_efforts_midT)
df_dealer_primC <- data.frame(baseline_dealers$mid_quantitysold,baseline_dealers$mid_av_salesprices,baseline_dealers$mid_revenue
                             ,baseline_dealers$mid_maize.owner.agree.q7,baseline_dealers$index_practices_cap_midC
                             ,baseline_dealers$index_practices_lab_midC,baseline_dealers$index_practices_all_midC
                             ,baseline_dealers$index_efforts_midC)
df_dealer_primF <- data.frame(baseline_dealers$mid_quantitysold,baseline_dealers$mid_av_salesprices,baseline_dealers$mid_revenue
                             ,baseline_dealers$mid_maize.owner.agree.q7,baseline_dealers$index_practices_cap_midF
                             ,baseline_dealers$index_practices_lab_midF,baseline_dealers$index_practices_all_midF
                             ,baseline_dealers$index_efforts_midF)
#no index_overall_prim_dealer_midF

#example
adjust_p(0.03,df_dealer_primT,9)

df_ols_end_D_prim_J <- array(NA,dim=c(3,3,10))

results_dealer_prim_J <- c("mid_quantitysold","mid_av_salesprices","mid_revenue","mid_maize.owner.agree.q7","index_practices_cap_mid"
                         ,"index_practices_lab_mid","index_practices_all_mid","index_efforts_mid")
#no index_overall_prim_dealer_midF

for (i in 1:length(results_dealer_prim_J)){
  df_ols_end_D_prim_J[3,1,i] <- adjust_p(df_ols_end_D_prim[3,1,i],df_dealer_primT,i)
  df_ols_end_D_prim_J[3,2,i] <- adjust_p(df_ols_end_D_prim[3,2,i],df_dealer_primC,i)
  df_ols_end_D_prim_J[3,3,i] <- adjust_p(df_ols_end_D_prim[3,3,i],df_dealer_primF,i)}










#############################################################################################################################################################################
##### 3 ANALYSIS: Agro-input dealer - Secondary: outcomes with baseline######################################################################################################
#############################################################################################################################################################################

#1. Number of maize varieties in stock last season (incl. hybrids, OPV, landraces)
baseline_dealers <- trim("maize.owner.agree.nr_var",baseline_dealers,trim_perc=.02)

baseline_dealers$mid_maize.owner.agree.nr_var <- baseline_dealers$check.owner.agree.nr_var #x
baseline_dealers$mid_maize.owner.agree.nr_var[baseline_dealers$mid_maize.owner.agree.nr_var=="n/a"] <- NA #x
baseline_dealers$mid_maize.owner.agree.nr_var <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.nr_var))
baseline_dealers <- trim("mid_maize.owner.agree.nr_var",baseline_dealers,trim_perc=.02) #x

#2. Number of hybrid maize varieties in stock last season
baseline_dealers <- trim("maize.owner.agree.q19",baseline_dealers,trim_perc=.02)

baseline_dealers$mid_maize.owner.agree.q19 <- baseline_dealers$check.owner.agree.q19 #x
baseline_dealers$mid_maize.owner.agree.q19[baseline_dealers$mid_maize.owner.agree.q19=="n/a"] <- NA #x
baseline_dealers$mid_maize.owner.agree.q19 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.q19))
baseline_dealers <- trim("mid_maize.owner.agree.q19",baseline_dealers,trim_perc=.02) #x

#3. Number of OP maize varieties in stock last season
baseline_dealers <- trim("maize.owner.agree.q44",baseline_dealers,trim_perc=.02)

baseline_dealers$mid_maize.owner.agree.q44 <- baseline_dealers$check.owner.agree.q44 #x
baseline_dealers$mid_maize.owner.agree.q44[baseline_dealers$mid_maize.owner.agree.q44=="n/a"] <- NA #x
baseline_dealers$mid_maize.owner.agree.q44 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.q44))
baseline_dealers <- trim("mid_maize.owner.agree.q44",baseline_dealers,trim_perc=.02) #x

#8. Index of dealer's self-ratings on location, price, product quality, stock & convenient quantities, reputation
baseline_dealers$mid_maize.owner.agree.q99 <- baseline_dealers$check.owner.agree.q99 #x
baseline_dealers$mid_maize.owner.agree.q99[baseline_dealers$mid_maize.owner.agree.q99=="n/a"] <- NA
baseline_dealers$mid_maize.owner.agree.q99 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.q99))

baseline_dealers$mid_maize.owner.agree.q100 <- baseline_dealers$check.owner.agree.q100 #x
baseline_dealers$mid_maize.owner.agree.q100[baseline_dealers$mid_maize.owner.agree.q100=="n/a"] <- NA
baseline_dealers$mid_maize.owner.agree.q100 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.q100))

baseline_dealers$mid_maize.owner.agree.q101 <- baseline_dealers$check.owner.agree.q101 #x
baseline_dealers$mid_maize.owner.agree.q101[baseline_dealers$mid_maize.owner.agree.q101=="n/a"] <- NA
baseline_dealers$mid_maize.owner.agree.q101 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.q101))

baseline_dealers$mid_maize.owner.agree.q102 <- baseline_dealers$check.owner.agree.q102 #x
baseline_dealers$mid_maize.owner.agree.q102[baseline_dealers$mid_maize.owner.agree.q102=="n/a"] <- NA
baseline_dealers$mid_maize.owner.agree.q102 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.q102))

baseline_dealers$mid_maize.owner.agree.q103 <- baseline_dealers$check.owner.agree.q103 #x
baseline_dealers$mid_maize.owner.agree.q103[baseline_dealers$mid_maize.owner.agree.q103=="n/a"] <- NA
baseline_dealers$mid_maize.owner.agree.q103 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.q103))

variables_selfratings_mid <- cbind(baseline_dealers$mid_maize.owner.agree.q99,baseline_dealers$mid_maize.owner.agree.q100
                                  ,baseline_dealers$mid_maize.owner.agree.q101,baseline_dealers$mid_maize.owner.agree.q102
                                  ,baseline_dealers$mid_maize.owner.agree.q103) #x
variables_selfratings_base <- cbind(baseline_dealers$maize.owner.agree.q99,baseline_dealers$maize.owner.agree.q100
                                   ,baseline_dealers$maize.owner.agree.q101,baseline_dealers$maize.owner.agree.q102
                                   ,baseline_dealers$maize.owner.agree.q103)

index_selfratings_mid <- icwIndex(xmat=variables_selfratings_mid) #x
baseline_dealers$index_selfratings_mid <- index_selfratings_mid$index #x

index_selfratings_base <- icwIndex(xmat=variables_selfratings_base)
baseline_dealers$index_selfratings_base <- index_selfratings_base$index #baseline index

#5. Index of dealer's efforts and services according to farmers (who (know someone who) bought seed there)
variables_servicesFARM_mid <- cbind(baseline_dealers$end_refunds,baseline_dealers$end_gives_credit,baseline_dealers$end_gives_advice
                               ,baseline_dealers$end_delivers,baseline_dealers$end_after_sales_service,baseline_dealers$end_payment_mehtods
                               ,baseline_dealers$end_small_quant) #x
variables_servicesFARM_base <- cbind(baseline_dealers$refunds,baseline_dealers$gives_credit,baseline_dealers$gives_advice
                                ,baseline_dealers$delivers,baseline_dealers$after_sales_service,baseline_dealers$payment_mehtods
                                ,baseline_dealers$small_quant)

index_servicesFARM_mid <- icwIndex(xmat=variables_servicesFARM_mid) #x
baseline_dealers$index_servicesFARM_mid <- index_servicesFARM_mid$index #x

index_servicesFARM_base <- icwIndex(xmat=variables_servicesFARM_base)
baseline_dealers$index_servicesFARM_base <- index_servicesFARM_base$index

#6. Index of dealer's knowledge about seed storage
baseline_dealers$maize.owner.agree.skill.q104_bin <- ifelse(baseline_dealers$maize.owner.agree.skill.q104=="b",1,0)
baseline_dealers$mid_maize.owner.agree.skill.q104 <- baseline_dealers$check.owner.agree.skill.q104 #x
baseline_dealers$mid_maize.owner.agree.skill.q104[baseline_dealers$mid_maize.owner.agree.skill.q104=="n/a"] <- NA
baseline_dealers$mid_maize.owner.agree.skill.q104_bin <- ifelse(baseline_dealers$mid_maize.owner.agree.skill.q104=="b",1,0) #x

baseline_dealers$maize.owner.agree.skill.q105_bin <- ifelse(baseline_dealers$maize.owner.agree.skill.q105=="b",1,0)
baseline_dealers$mid_maize.owner.agree.skill.q105 <- baseline_dealers$check.owner.agree.skill.q105 #x
baseline_dealers$mid_maize.owner.agree.skill.q105[baseline_dealers$mid_maize.owner.agree.skill.q105=="n/a"] <- NA
baseline_dealers$mid_maize.owner.agree.skill.q105_bin <- ifelse(baseline_dealers$mid_maize.owner.agree.skill.q105=="b",1,0) #x

baseline_dealers$maize.owner.agree.skill.q106_bin <- ifelse(baseline_dealers$maize.owner.agree.skill.q106=="c",1,0)
baseline_dealers$mid_maize.owner.agree.skill.q106 <- baseline_dealers$check.owner.agree.skill.q106 #x
baseline_dealers$mid_maize.owner.agree.skill.q106[baseline_dealers$mid_maize.owner.agree.skill.q106=="n/a"] <- NA
baseline_dealers$mid_maize.owner.agree.skill.q106_bin <- ifelse(baseline_dealers$mid_maize.owner.agree.skill.q106=="c",1,0) #x

baseline_dealers$maize.owner.agree.skill.q107_bin <- ifelse(baseline_dealers$maize.owner.agree.skill.q107=="b",1,0)
baseline_dealers$mid_maize.owner.agree.skill.q107 <- baseline_dealers$check.owner.agree.skill.q107 #x
baseline_dealers$mid_maize.owner.agree.skill.q107[baseline_dealers$mid_maize.owner.agree.skill.q107=="n/a"] <- NA
baseline_dealers$mid_maize.owner.agree.skill.q107_bin <- ifelse(baseline_dealers$mid_maize.owner.agree.skill.q107=="b",1,0) #x

baseline_dealers$maize.owner.agree.skill.q108_bin <- ifelse(baseline_dealers$maize.owner.agree.skill.q108=="c",1,0)
baseline_dealers$mid_maize.owner.agree.skill.q108 <- baseline_dealers$check.owner.agree.skill.q108 #x
baseline_dealers$mid_maize.owner.agree.skill.q108[baseline_dealers$mid_maize.owner.agree.skill.q108=="n/a"] <- NA
baseline_dealers$mid_maize.owner.agree.skill.q108_bin <- ifelse(baseline_dealers$mid_maize.owner.agree.skill.q108=="c",1,0) #x

variables_knowl_store_mid <- cbind(baseline_dealers$mid_maize.owner.agree.skill.q104_bin,baseline_dealers$mid_maize.owner.agree.skill.q105_bin
                                   ,baseline_dealers$mid_maize.owner.agree.skill.q106_bin,baseline_dealers$mid_maize.owner.agree.skill.q107_bin
                                   ,baseline_dealers$mid_maize.owner.agree.skill.q108_bin) #x
variables_knowl_store_base <- cbind(baseline_dealers$maize.owner.agree.skill.q104_bin,baseline_dealers$maize.owner.agree.skill.q105_bin
                                    ,baseline_dealers$maize.owner.agree.skill.q106_bin,baseline_dealers$maize.owner.agree.skill.q107_bin
                                    ,baseline_dealers$maize.owner.agree.skill.q108_bin)

index_knowl_store_mid <- icwIndex(xmat=variables_knowl_store_mid) #x
baseline_dealers$index_knowl_store_mid <- index_knowl_store_mid$index #x

index_knowl_store_base <- icwIndex(xmat=variables_knowl_store_base)
baseline_dealers$index_knowl_store_base <- index_knowl_store_base$index

#7. Index of dealer's knowledge about seed
baseline_dealers$maize.owner.agree.skill.q109_bin <- ifelse(baseline_dealers$maize.owner.agree.skill.q109=="a",1,0)
baseline_dealers$mid_maize.owner.agree.skill.q109 <- baseline_dealers$check.owner.agree.skill.q109 #x
baseline_dealers$mid_maize.owner.agree.skill.q109[baseline_dealers$mid_maize.owner.agree.skill.q109=="n/a"] <- NA
baseline_dealers$mid_maize.owner.agree.skill.q109_bin <- ifelse(baseline_dealers$mid_maize.owner.agree.skill.q109=="a",1,0) #x

#q110: OPVs can often be recycled for up to 3 yr without a significant loss in yield.
#but answer options Never, 4-5 times, 10-12 times, Don't know

baseline_dealers$maize.owner.agree.skill.q111_bin <- ifelse(baseline_dealers$maize.owner.agree.skill.q111=="a",1,0)
baseline_dealers$mid_maize.owner.agree.skill.q111 <- baseline_dealers$check.owner.agree.skill.q111 #x
baseline_dealers$mid_maize.owner.agree.skill.q111[baseline_dealers$mid_maize.owner.agree.skill.q111=="n/a"] <- NA
baseline_dealers$mid_maize.owner.agree.skill.q111_bin <- ifelse(baseline_dealers$mid_maize.owner.agree.skill.q111=="a",1,0) #x

baseline_dealers$maize.owner.agree.skill.q112_bin <- ifelse(baseline_dealers$maize.owner.agree.skill.q112=="c",1,0)
baseline_dealers$mid_maize.owner.agree.skill.q112 <- baseline_dealers$check.owner.agree.skill.q112 #x
baseline_dealers$mid_maize.owner.agree.skill.q112[baseline_dealers$mid_maize.owner.agree.skill.q112=="n/a"] <- NA
baseline_dealers$mid_maize.owner.agree.skill.q112_bin <- ifelse(baseline_dealers$mid_maize.owner.agree.skill.q112=="c",1,0) #x

baseline_dealers$maize.owner.agree.skill.q113_bin <- ifelse(baseline_dealers$maize.owner.agree.skill.q113=="c",1,0)
baseline_dealers$mid_maize.owner.agree.skill.q113 <- baseline_dealers$check.owner.agree.skill.q113 #x
baseline_dealers$mid_maize.owner.agree.skill.q113[baseline_dealers$mid_maize.owner.agree.skill.q113=="n/a"] <- NA
baseline_dealers$mid_maize.owner.agree.skill.q113_bin <- ifelse(baseline_dealers$mid_maize.owner.agree.skill.q113=="c",1,0) #x

variables_knowl_seed_mid <- cbind(baseline_dealers$mid_maize.owner.agree.skill.q109_bin,baseline_dealers$mid_maize.owner.agree.skill.q111_bin
                                  ,baseline_dealers$mid_maize.owner.agree.skill.q112_bin,baseline_dealers$mid_maize.owner.agree.skill.q113_bin) #x
variables_knowl_seed_base <- cbind(baseline_dealers$maize.owner.agree.skill.q109_bin,baseline_dealers$maize.owner.agree.skill.q111_bin
                                   ,baseline_dealers$maize.owner.agree.skill.q112_bin,baseline_dealers$maize.owner.agree.skill.q113_bin)

index_knowl_seed_mid <- icwIndex(xmat=variables_knowl_seed_mid) #x
baseline_dealers$index_knowl_seed_mid <- index_knowl_seed_mid$index #x

index_knowl_seed_base <- icwIndex(xmat=variables_knowl_seed_base)
baseline_dealers$index_knowl_seed_base <- index_knowl_seed_base$index

#4. Q121. Do you have equipment to monitor moisture in the seed?
baseline_dealers$mid_maize.owner.agree.inspection.q121 <- baseline_dealers$check.owner.agree.inspection.q121 #x
baseline_dealers$mid_maize.owner.agree.inspection.q121[baseline_dealers$mid_maize.owner.agree.inspection.q121=="n/a"] <- NA
baseline_dealers$mid_maize.owner.agree.inspection.q121<-ifelse(baseline_dealers$mid_maize.owner.agree.inspection.q121=="Yes",1,0) #x

#9. Overall index of secondary agro-input dealer outcome variables
variables_overallsec_mid <- cbind(baseline_dealers$mid_maize.owner.agree.q19,baseline_dealers$mid_maize.owner.agree.q44) #x
variables_overallsec_base <- cbind(baseline_dealers$maize.owner.agree.q19,baseline_dealers$maize.owner.agree.q44)

index_overallsec_mid <- icwIndex(xmat=variables_overallsec_mid) #x
baseline_dealers$index_overallsec_mid <- index_overallsec_mid$index #x

index_overallsec_base <- icwIndex(xmat=variables_overallsec_base)
baseline_dealers$index_overallsec_base <- index_overallsec_base$index #baseline index

################################################################################################################################################################################

###
#1#
###

results_dealer_sec <- c("mid_maize.owner.agree.nr_var",          #1
                        "mid_maize.owner.agree.q19",             #2
                        "mid_maize.owner.agree.q44",             #3
                        "mid_maize.owner.agree.inspection.q121", #4
                        "index_servicesFARM_mid",                #5
                        "index_knowl_store_mid",                 #6
                        "index_knowl_seed_mid",                  #7
                        "index_selfratings_mid",                 #8
                        "index_overallsec_mid")                  #9
results_dealer_sec_base <- c("maize.owner.agree.nr_var",
                             "maize.owner.agree.q19",
                             "maize.owner.agree.q44",
                             "maize.owner.agree.inspection.q121",
                             "index_servicesFARM_base",
                             "index_knowl_store_base",
                             "index_knowl_seed_base",
                             "index_selfratings_base",
                             "index_overallsec_base")

baseline_dealers[results_dealer_sec_base] <- lapply(baseline_dealers[results_dealer_sec_base],function(x)x - mean(x,na.rm = T))

df_means_end_D_sec <- array(NA,dim=c(5,12))

for (i in 1:length(results_dealer_sec)){
  df_means_end_D_sec[1,i] <- sum(baseline_dealers[results_dealer_sec[i]], na.rm=T)/(nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_sec[i]])))
  df_means_end_D_sec[2,i] <- sqrt(var(baseline_dealers[results_dealer_sec[i]], na.rm=T))
  df_means_end_D_sec[3,i] <- nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_sec[i]]))-sum(is.na(baseline_dealers[results_dealer_sec_base[i]]))+sum(is.na(baseline_dealers[results_dealer_sec[i]])&is.na(baseline_dealers[results_dealer_sec_base[i]]))
  df_means_end_D_sec[4,i] <- min(baseline_dealers[results_dealer_sec[i]], na.rm=T)
  df_means_end_D_sec[5,i] <- max(baseline_dealers[results_dealer_sec[i]], na.rm=T)}

###
#2#
###

df_ols_end_D_sec <- array(NA,dim=c(3,3,12))

baseline_dealers$training_control[baseline_dealers$training==0] <- TRUE
baseline_dealers$training_control[baseline_dealers$training==1] <- FALSE

#8.
index_selfratings_mid <- icwIndex(xmat=variables_selfratings_mid,sgroup = baseline_dealers$training_control)
baseline_dealers$index_selfratings_midT <- index_selfratings_mid$index

index_selfratings_base <- icwIndex(xmat=variables_selfratings_base,sgroup = baseline_dealers$training_control)
baseline_dealers$index_selfratings_baseT <- index_selfratings_base$index

#5.
index_servicesFARM_mid <- icwIndex(xmat=variables_servicesFARM_mid,sgroup = baseline_dealers$training_control)
baseline_dealers$index_servicesFARM_midT <- index_servicesFARM_mid$index

index_servicesFARM_base <- icwIndex(xmat=variables_servicesFARM_base,sgroup = baseline_dealers$training_control)
baseline_dealers$index_servicesFARM_baseT <- index_servicesFARM_base$index

#6.
index_knowl_store_mid <- icwIndex(xmat=variables_knowl_store_mid,sgroup = baseline_dealers$training_control)
baseline_dealers$index_knowl_store_midT <- index_knowl_store_mid$index

index_knowl_store_base <- icwIndex(xmat=variables_knowl_store_base,sgroup = baseline_dealers$training_control)
baseline_dealers$index_knowl_store_baseT <- index_knowl_store_base$index

#7.
index_knowl_seed_mid <- icwIndex(xmat=variables_knowl_seed_mid,sgroup = baseline_dealers$training_control)
baseline_dealers$index_knowl_seed_midT <- index_knowl_seed_mid$index

index_knowl_seed_base <- icwIndex(xmat=variables_knowl_seed_base,sgroup = baseline_dealers$training_control)
baseline_dealers$index_knowl_seed_baseT <- index_knowl_seed_base$index

#9.
index_overallsec_mid <- icwIndex(xmat=variables_overallsec_mid,sgroup = baseline_dealers$training_control)
baseline_dealers$index_overallsec_midT <- index_overallsec_mid$index

index_overallsec_base <- icwIndex(xmat=variables_overallsec_base,sgroup = baseline_dealers$training_control)
baseline_dealers$index_overallsec_baseT <- index_overallsec_base$index

results_dealer_sec <- c("mid_maize.owner.agree.nr_var"
                        ,"mid_maize.owner.agree.q19"
                        ,"mid_maize.owner.agree.q44"
                        ,"mid_maize.owner.agree.inspection.q121"
                        ,"index_servicesFARM_midT"
                        ,"index_knowl_store_midT"
                        ,"index_knowl_seed_midT"
                        ,"index_selfratings_midT"
                        ,"index_overallsec_midT")
results_dealer_sec_base <- c("maize.owner.agree.nr_var"
                             ,"maize.owner.agree.q19"
                             ,"maize.owner.agree.q44"
                             ,"maize.owner.agree.inspection.q121"
                             ,"index_servicesFARM_baseT"
                             ,"index_knowl_store_baseT"
                             ,"index_knowl_seed_baseT"
                             ,"index_selfratings_baseT"
                             ,"index_overallsec_baseT")

baseline_dealers[results_dealer_sec_base] <- lapply(baseline_dealers[results_dealer_sec_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_dealer_sec)){
  ols <- lm(as.formula(paste(paste(results_dealer_sec[i],"training*clearing_demeaned*farmer_demeaned",sep="~"),results_dealer_sec_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_sec[i],"training*clearing_demeaned*farmer_demeaned",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")

  df_ols_end_D_sec[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_ols_end_D_sec[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_ols_end_D_sec[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]}

###
#3#
###

baseline_dealers$clearing_control[baseline_dealers$clearing==0] <- TRUE
baseline_dealers$clearing_control[baseline_dealers$clearing==1] <- FALSE

#8.
index_selfratings_mid <- icwIndex(xmat=variables_selfratings_mid,sgroup = baseline_dealers$clearing_control)
baseline_dealers$index_selfratings_midC <- index_selfratings_mid$index

index_selfratings_base <- icwIndex(xmat=variables_selfratings_base,sgroup = baseline_dealers$clearing_control)
baseline_dealers$index_selfratings_baseC <- index_selfratings_base$index

#5.
index_servicesFARM_mid <- icwIndex(xmat=variables_servicesFARM_mid,sgroup = baseline_dealers$clearing_control)
baseline_dealers$index_servicesFARM_midC <- index_servicesFARM_mid$index

index_servicesFARM_base <- icwIndex(xmat=variables_servicesFARM_base,sgroup = baseline_dealers$clearing_control)
baseline_dealers$index_servicesFARM_baseC <- index_servicesFARM_base$index

#6.
index_knowl_store_mid <- icwIndex(xmat=variables_knowl_store_mid,sgroup = baseline_dealers$clearing_control)
baseline_dealers$index_knowl_store_midC <- index_knowl_store_mid$index

index_knowl_store_base <- icwIndex(xmat=variables_knowl_store_base,sgroup = baseline_dealers$clearing_control)
baseline_dealers$index_knowl_store_baseC <- index_knowl_store_base$index

#7.
index_knowl_seed_mid <- icwIndex(xmat=variables_knowl_seed_mid,sgroup = baseline_dealers$clearing_control)
baseline_dealers$index_knowl_seed_midC <- index_knowl_seed_mid$index

index_knowl_seed_base <- icwIndex(xmat=variables_knowl_seed_base,sgroup = baseline_dealers$clearing_control)
baseline_dealers$index_knowl_seed_baseC <- index_knowl_seed_base$index

#9.
index_overallsec_mid <- icwIndex(xmat=variables_overallsec_mid,sgroup = baseline_dealers$clearing_control)
baseline_dealers$index_overallsec_midC <- index_overallsec_mid$index

index_overallsec_base <- icwIndex(xmat=variables_overallsec_base,sgroup = baseline_dealers$clearing_control)
baseline_dealers$index_overallsec_baseC <- index_overallsec_base$index

results_dealer_sec <- c("mid_maize.owner.agree.nr_var"
                        ,"mid_maize.owner.agree.q19"
                        ,"mid_maize.owner.agree.q44"
                        ,"mid_maize.owner.agree.inspection.q121"
                        ,"index_servicesFARM_midC"
                        ,"index_knowl_store_midC"
                        ,"index_knowl_seed_midC"
                        ,"index_selfratings_midC"
                        ,"index_overallsec_midC")
results_dealer_sec_base <- c("maize.owner.agree.nr_var"
                             ,"maize.owner.agree.q19"
                             ,"maize.owner.agree.q44"
                             ,"maize.owner.agree.inspection.q121"
                             ,"index_servicesFARM_baseC"
                             ,"index_knowl_store_baseC"
                             ,"index_knowl_seed_baseC"
                             ,"index_selfratings_baseC"
                             ,"index_overallsec_baseC")

baseline_dealers[results_dealer_sec_base] <- lapply(baseline_dealers[results_dealer_sec_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_dealer_sec)){
  ols <- lm(as.formula(paste(paste(results_dealer_sec[i],"training_demeaned*clearing*farmer_demeaned",sep="~"),results_dealer_sec_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_sec[i],"training_demeaned*clearing*farmer_demeaned",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")

  df_ols_end_D_sec[1,2,i] <- coef_test(ols, vcov_cluster)$beta[3]
  df_ols_end_D_sec[2,2,i] <- coef_test(ols, vcov_cluster)$SE[3]
  df_ols_end_D_sec[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]}

###
#4#
###

baseline_dealers$farmer_control[baseline_dealers$farmer==0] <- TRUE
baseline_dealers$farmer_control[baseline_dealers$farmer==1] <- FALSE

#8.
index_selfratings_mid <- icwIndex(xmat=variables_selfratings_mid,sgroup = baseline_dealers$farmer_control)
baseline_dealers$index_selfratings_midF <- index_selfratings_mid$index

index_selfratings_base <- icwIndex(xmat=variables_selfratings_base,sgroup = baseline_dealers$farmer_control)
baseline_dealers$index_selfratings_baseF <- index_selfratings_base$index

#5.
index_servicesFARM_mid <- icwIndex(xmat=variables_servicesFARM_mid,sgroup = baseline_dealers$farmer_control)
baseline_dealers$index_servicesFARM_midF <- index_servicesFARM_mid$index

index_servicesFARM_base <- icwIndex(xmat=variables_servicesFARM_base,sgroup = baseline_dealers$farmer_control)
baseline_dealers$index_servicesFARM_baseF <- index_servicesFARM_base$index

#6.
index_knowl_store_mid <- icwIndex(xmat=variables_knowl_store_mid,sgroup = baseline_dealers$farmer_control)
baseline_dealers$index_knowl_store_midF <- index_knowl_store_mid$index

index_knowl_store_base <- icwIndex(xmat=variables_knowl_store_base,sgroup = baseline_dealers$farmer_control)
baseline_dealers$index_knowl_store_baseF <- index_knowl_store_base$index

#7.
index_knowl_seed_mid <- icwIndex(xmat=variables_knowl_seed_mid,sgroup = baseline_dealers$farmer_control)
baseline_dealers$index_knowl_seed_midF <- index_knowl_seed_mid$index

index_knowl_seed_base <- icwIndex(xmat=variables_knowl_seed_base,sgroup = baseline_dealers$farmer_control)
baseline_dealers$index_knowl_seed_baseF <- index_knowl_seed_base$index

#9.
index_overallsec_mid <- icwIndex(xmat=variables_overallsec_mid,sgroup = baseline_dealers$farmer_control)
baseline_dealers$index_overallsec_midF <- index_overallsec_mid$index

index_overallsec_base <- icwIndex(xmat=variables_overallsec_base,sgroup = baseline_dealers$farmer_control)
baseline_dealers$index_overallsec_baseF <- index_overallsec_base$index

results_dealer_sec <- c("mid_maize.owner.agree.nr_var"
                        ,"mid_maize.owner.agree.q19"
                        ,"mid_maize.owner.agree.q44"
                        ,"mid_maize.owner.agree.inspection.q121"
                        ,"index_servicesFARM_midF"
                        ,"index_knowl_store_midF"
                        ,"index_knowl_seed_midF"
                        ,"index_selfratings_midF"
                        ,"index_overallsec_midF")
results_dealer_sec_base <- c("maize.owner.agree.nr_var"
                             ,"maize.owner.agree.q19"
                             ,"maize.owner.agree.q44"
                             ,"maize.owner.agree.inspection.q121"
                             ,"index_servicesFARM_baseF"
                             ,"index_knowl_store_baseF"
                             ,"index_knowl_seed_baseF"
                             ,"index_selfratings_baseF"
                             ,"index_overallsec_baseF")

baseline_dealers[results_dealer_sec_base] <- lapply(baseline_dealers[results_dealer_sec_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_dealer_sec)){
  ols <- lm(as.formula(paste(paste(results_dealer_sec[i],"training_demeaned*clearing_demeaned*farmer",sep="~"),results_dealer_sec_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_sec[i],"training_demeaned*clearing_demeaned*farmer",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")

  #farmer video treatment at village/shop level so no clustering needed
  df_ols_end_D_sec[1,3,i] <- summary(ols)$coefficients[4,1]
  df_ols_end_D_sec[2,3,i] <- summary(ols)$coefficients[4,2]
  df_ols_end_D_sec[3,3,i] <- summary(ols)$coefficients[4,4]}

#Aker, Boumnijel, McClelland, Tierney (2012)

df_dealer_secT <- data.frame(baseline_dealers$mid_maize.owner.agree.nr_var,baseline_dealers$mid_maize.owner.agree.q19
                             ,baseline_dealers$mid_maize.owner.agree.q44,baseline_dealers$mid_maize.owner.agree.inspection.q121)
df_dealer_secC <- df_dealer_secT
df_dealer_secF <- df_dealer_secT
#no overall index
#all the same here

df_ols_end_D_sec_J <- array(NA,dim=c(3,3,10))

results_dealer_sec_J <- c("mid_maize.owner.agree.nr_var","mid_maize.owner.agree.q19","mid_maize.owner.agree.q44","mid_maize.owner.agree.inspection.q121")
#no overall index

for (i in 1:length(results_dealer_sec_J)){
  df_ols_end_D_sec_J[3,1,i] <- adjust_p(df_ols_end_D_sec[3,1,i],df_dealer_secT,i)
  df_ols_end_D_sec_J[3,2,i] <- adjust_p(df_ols_end_D_sec[3,2,i],df_dealer_secC,i)
  df_ols_end_D_sec_J[3,3,i] <- adjust_p(df_ols_end_D_sec[3,3,i],df_dealer_secF,i)}










###################################################################################################################################################################
##### 4 ANALYSIS: Agro-input dealer - Secondary: 9. Longe 10H######################################################################################################
###################################################################################################################################################################

baseline_dealers_save=baseline_dealers
baseline_dealers=subset(baseline_dealers,mid_maize.owner.agree.q20=="1")

#1. Did you have Longe 10H in stock in the second season of 2020. (q20)

#2. How much of Longe 10H was carried forward from the previous season (first season 2020) into the second season of 2020 (kg) (q21)
baseline_dealers$maize.owner.agree.long10h.q21[baseline_dealers$maize.owner.agree.long10h.q21=="n/a"] <- NA
baseline_dealers$maize.owner.agree.long10h.q21 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.long10h.q21))
baseline_dealers$maize.owner.agree.long10h.q21[baseline_dealers$maize.owner.agree.long10h.q21==999] <- NA
baseline_dealers$maize.owner.agree.long10h.q21_unadj <- baseline_dealers$maize.owner.agree.long10h.q21
baseline_dealers$maize.owner.agree.long10h.q21[baseline_dealers$maize.owner.agree.q20=="0"] <- 0
baseline_dealers <- trim("maize.owner.agree.long10h.q21",baseline_dealers,trim_perc=.02)
baseline_dealers$maize.owner.agree.long10h.q21 <- ihs(baseline_dealers$maize.owner.agree.long10h.q21)

baseline_dealers$mid_maize.owner.agree.long10h.q21 <- baseline_dealers$check.owner.agree.long10h.q21 #x
baseline_dealers$mid_maize.owner.agree.long10h.q21[baseline_dealers$mid_maize.owner.agree.long10h.q21=="n/a"] <- NA #x
baseline_dealers$mid_maize.owner.agree.long10h.q21 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.long10h.q21)) #x
baseline_dealers$mid_maize.owner.agree.long10h.q21[baseline_dealers$mid_maize.owner.agree.long10h.q21==999] <- NA #x
baseline_dealers$mid_maize.owner.agree.long10h.q21_unadj <- baseline_dealers$mid_maize.owner.agree.long10h.q21
baseline_dealers$mid_maize.owner.agree.long10h.q21[baseline_dealers$mid_maize.owner.agree.q20=="0"] <- 0 #x
baseline_dealers <- trim("mid_maize.owner.agree.long10h.q21",baseline_dealers,trim_perc=.02) #x
baseline_dealers$mid_maize.owner.agree.long10h.q21 <- ihs(baseline_dealers$mid_maize.owner.agree.long10h.q21) #x

#3. How much of Longe 10H was bought by you from any provider during the second season of 2020 (in kg) (q22)
baseline_dealers$maize.owner.agree.long10h.q22[baseline_dealers$maize.owner.agree.long10h.q22=="n/a"] <- NA
baseline_dealers$maize.owner.agree.long10h.q22[baseline_dealers$maize.owner.agree.long10h.q22==999] <- NA
baseline_dealers$maize.owner.agree.long10h.q22 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.long10h.q22))
baseline_dealers$maize.owner.agree.long10h.q22_unadj <- baseline_dealers$maize.owner.agree.long10h.q22
baseline_dealers$maize.owner.agree.long10h.q22[baseline_dealers$maize.owner.agree.q20=="0"] <- 0
baseline_dealers <- trim("maize.owner.agree.long10h.q22",baseline_dealers,trim_perc=.02)
baseline_dealers$maize.owner.agree.long10h.q22 <- ihs(baseline_dealers$maize.owner.agree.long10h.q22)

baseline_dealers$mid_maize.owner.agree.long10h.q22 <- baseline_dealers$check.owner.agree.long10h.q22 #x
baseline_dealers$mid_maize.owner.agree.long10h.q22[baseline_dealers$mid_maize.owner.agree.long10h.q22=="n/a"] <- NA #x
baseline_dealers$mid_maize.owner.agree.long10h.q22[baseline_dealers$mid_maize.owner.agree.long10h.q22==999] <- NA #x
baseline_dealers$mid_maize.owner.agree.long10h.q22 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.long10h.q22)) #x
baseline_dealers$mid_maize.owner.agree.long10h.q22_unadj <- baseline_dealers$mid_maize.owner.agree.long10h.q22
baseline_dealers$mid_maize.owner.agree.long10h.q22[baseline_dealers$mid_maize.owner.agree.q20=="0"] <- 0 #x
baseline_dealers <- trim("mid_maize.owner.agree.long10h.q22",baseline_dealers,trim_perc=.02) #x
baseline_dealers$mid_maize.owner.agree.long10h.q22 <- ihs(baseline_dealers$mid_maize.owner.agree.long10h.q22) #x

#4. What was the cost of Longe 10H per Kg from where you obtained it during the second season of 2020? (q24)
baseline_dealers$maize.owner.agree.long10h.q24[baseline_dealers$maize.owner.agree.long10h.q24=="n/a"] <- NA
baseline_dealers$maize.owner.agree.long10h.q24[baseline_dealers$maize.owner.agree.long10h.q24==999] <- NA
baseline_dealers$maize.owner.agree.long10h.q24 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.long10h.q24))
baseline_dealers <- trim("maize.owner.agree.long10h.q24",baseline_dealers,trim_perc=.02)
baseline_dealers$maize.owner.agree.long10h.q24 <- ihs(baseline_dealers$maize.owner.agree.long10h.q24)

baseline_dealers$mid_maize.owner.agree.long10h.q24 <- baseline_dealers$check.owner.agree.long10h.q24 #x
baseline_dealers$mid_maize.owner.agree.long10h.q24[baseline_dealers$mid_maize.owner.agree.long10h.q24=="n/a"] <- NA #x
baseline_dealers$mid_maize.owner.agree.long10h.q24[baseline_dealers$mid_maize.owner.agree.long10h.q24==999] <- NA #x
baseline_dealers$mid_maize.owner.agree.long10h.q24 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.long10h.q24)) #x
baseline_dealers <- trim("mid_maize.owner.agree.long10h.q24",baseline_dealers,trim_perc=.02) #x
baseline_dealers$mid_maize.owner.agree.long10h.q24 <- ihs(baseline_dealers$mid_maize.owner.agree.long10h.q24)

#5. Q25. Total quantity sold of ${carry} (Kg) over the second season of 2020
baseline_dealers <- trim("maize.owner.agree.long10h.q25",baseline_dealers,trim_perc=.02)
baseline_dealers$maize.owner.agree.long10h.q25 <- ihs(baseline_dealers$maize.owner.agree.long10h.q25)

baseline_dealers <- trim("mid_maize.owner.agree.long10h.q25",baseline_dealers,trim_perc=.02) #x
baseline_dealers$mid_maize.owner.agree.long10h.q25 <- ihs(baseline_dealers$mid_maize.owner.agree.long10h.q25) #x

#6. Sales price per kilogram of ${q25} at the beginning of the second season of 2020 (q26)
baseline_dealers <- trim("maize.owner.agree.long10h.q26",baseline_dealers,trim_perc=.02)
baseline_dealers$maize.owner.agree.long10h.q26 <- ihs(baseline_dealers$maize.owner.agree.long10h.q26)

baseline_dealers <- trim("mid_maize.owner.agree.long10h.q26",baseline_dealers,trim_perc=.02) #x
baseline_dealers$mid_maize.owner.agree.long10h.q26 <- ihs(baseline_dealers$mid_maize.owner.agree.long10h.q26) #x

#7. (h) How much of Longe10H was lost/wasted the second season of 2020 (kg) (q27)
baseline_dealers$sold_condition <- baseline_dealers$maize.owner.agree.long10h.q22_unadj+baseline_dealers$maize.owner.agree.long10h.q21_unadj-baseline_dealers$maize.owner.agree.long10h.q25_unadj
baseline_dealers$q27_isnot_asked <- 0
baseline_dealers$q27_isnot_asked[baseline_dealers$sold_condition<=0]<-1

#baseline_dealers$maize.owner.agree.long10h.q27[baseline_dealers$maize.owner.agree.long10h.q27=="n/a"] <- 0
baseline_dealers$maize.owner.agree.long10h.q27[baseline_dealers$maize.owner.agree.long10h.q27=="999"] <- NA
baseline_dealers$maize.owner.agree.long10h.q27 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.long10h.q27))
baseline_dealers$maize.owner.agree.long10h.q27[baseline_dealers$q27_isnot_asked==1] <- 0
baseline_dealers$maize.owner.agree.long10h.q27[baseline_dealers$maize.owner.agree.q20=="0"] <- 0
baseline_dealers <- trim("maize.owner.agree.long10h.q27",baseline_dealers,trim_perc=.02)
baseline_dealers$maize.owner.agree.long10h.q27 <- ihs(baseline_dealers$maize.owner.agree.long10h.q27)

baseline_dealers$mid_sold_condition <- baseline_dealers$mid_maize.owner.agree.long10h.q22_unadj+baseline_dealers$mid_maize.owner.agree.long10h.q21_unadj-baseline_dealers$mid_maize.owner.agree.long10h.q25_unadj
baseline_dealers$mid_q27_isnot_asked <- 0
baseline_dealers$mid_q27_isnot_asked[baseline_dealers$mid_sold_condition<=0]<-1

baseline_dealers$mid_maize.owner.agree.long10h.q27 <- baseline_dealers$check.owner.agree.long10h.q27 #x
#baseline_dealers$mid_maize.owner.agree.long10h.q27[baseline_dealers$mid_maize.owner.agree.long10h.q27=="n/a"] <- 0 #x
baseline_dealers$mid_maize.owner.agree.long10h.q27[baseline_dealers$mid_maize.owner.agree.long10h.q27=="999"] <- NA
baseline_dealers$mid_maize.owner.agree.long10h.q27 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.long10h.q27)) #x
baseline_dealers$mid_maize.owner.agree.long10h.q27[baseline_dealers$mid_q27_isnot_asked==1] <- 0
baseline_dealers$mid_maize.owner.agree.long10h.q27[baseline_dealers$mid_maize.owner.agree.q20=="0"] <- 0 #x
baseline_dealers <- trim("mid_maize.owner.agree.long10h.q27",baseline_dealers,trim_perc=.02) #x
baseline_dealers$mid_maize.owner.agree.long10h.q27 <- ihs(baseline_dealers$mid_maize.owner.agree.long10h.q27) #x

#not in variables_overall_Longe10H because 247 0's (2 non-0's) at baseline (242 0's and 6 non-0's at endline)

#8. Did you ever run out of Longe10H during the second season of 2020? (q29)
baseline_dealers$maize.owner.agree.long10h.q29[baseline_dealers$maize.owner.agree.long10h.q29=="n/a"] <- NA
baseline_dealers$maize.owner.agree.long10h.q29 <- ifelse(baseline_dealers$maize.owner.agree.long10h.q29=="Yes",1,0)

baseline_dealers$mid_maize.owner.agree.long10h.q29 <- baseline_dealers$check.owner.agree.long10h.q29 #x
baseline_dealers$mid_maize.owner.agree.long10h.q29[baseline_dealers$mid_maize.owner.agree.long10h.q29=="n/a"] <- NA #x
baseline_dealers$mid_maize.owner.agree.long10h.q29 <- ifelse(baseline_dealers$mid_maize.owner.agree.long10h.q29=="Yes",1,0) #x

#9. Estimate how often you ran out of stock for Longe10 H during the second season of 2020 (q30)
baseline_dealers$maize.owner.agree.long10h.q30 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.long10h.q30))
baseline_dealers$maize.owner.agree.long10h.q30[baseline_dealers$maize.owner.agree.long10h.q29=="0"] <- 0
baseline_dealers$maize.owner.agree.long10h.q30[baseline_dealers$maize.owner.agree.long10h.q30=="1"] <- 21.74 #Everyday: 21.74 working days per month
baseline_dealers$maize.owner.agree.long10h.q30[baseline_dealers$maize.owner.agree.long10h.q30=="2"] <- 4.34524 #once a week: 4,34524 weeks in a month
baseline_dealers$maize.owner.agree.long10h.q30[baseline_dealers$maize.owner.agree.long10h.q30=="3"] <- 1 #Once a month
baseline_dealers$maize.owner.agree.long10h.q30[baseline_dealers$maize.owner.agree.long10h.q30=="4"] <- 1/3 #once in a season
baseline_dealers$maize.owner.agree.long10h.q30[baseline_dealers$maize.owner.agree.long10h.q30=="5"] <- 0 #Never
baseline_dealers$maize.owner.agree.long10h.q30[baseline_dealers$maize.owner.agree.long10h.q30=="6"] <- NA #Never
baseline_dealers <- trim("maize.owner.agree.long10h.q30",baseline_dealers,trim_perc=.02)
baseline_dealers$maize.owner.agree.long10h.q30 <- ihs(baseline_dealers$maize.owner.agree.long10h.q30) #x

baseline_dealers$mid_maize.owner.agree.long10h.q30 <- baseline_dealers$check.owner.agree.long10h.q30 #x
baseline_dealers$mid_maize.owner.agree.long10h.q30 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.long10h.q30))
baseline_dealers$mid_maize.owner.agree.long10h.q30[baseline_dealers$mid_maize.owner.agree.long10h.q29=="0"] <- 0
baseline_dealers$mid_maize.owner.agree.long10h.q30[baseline_dealers$mid_maize.owner.agree.long10h.q30=="1"] <- 21.74 #Everyday: 21.74 working days per month
baseline_dealers$mid_maize.owner.agree.long10h.q30[baseline_dealers$mid_maize.owner.agree.long10h.q30=="2"] <- 4.34524 #once a week: 4,34524 weeks in a month
baseline_dealers$mid_maize.owner.agree.long10h.q30[baseline_dealers$mid_maize.owner.agree.long10h.q30=="3"] <- 1 #Once a month
baseline_dealers$mid_maize.owner.agree.long10h.q30[baseline_dealers$mid_maize.owner.agree.long10h.q30=="4"] <- 1/3 #once in a season
baseline_dealers$mid_maize.owner.agree.long10h.q30[baseline_dealers$mid_maize.owner.agree.long10h.q30=="5"] <- 0 #Never
baseline_dealers$mid_maize.owner.agree.long10h.q30[baseline_dealers$mid_maize.owner.agree.long10h.q30=="6"] <- NA #Never
baseline_dealers <- trim("mid_maize.owner.agree.long10h.q30",baseline_dealers,trim_perc=.02)
baseline_dealers$mid_maize.owner.agree.long10h.q30 <- ihs(baseline_dealers$mid_maize.owner.agree.long10h.q30) #x

#11. Overall index of secondary Longe10H agro-input dealer outcome variables
variables_overall_Longe10H_mid <- cbind(baseline_dealers$mid_maize.owner.agree.long10h.q21,baseline_dealers$mid_maize.owner.agree.long10h.q22
                                        ,baseline_dealers$mid_maize.owner.agree.long10h.q25) #x
variables_overall_Longe10H_base <- cbind(baseline_dealers$maize.owner.agree.long10h.q21,baseline_dealers$maize.owner.agree.long10h.q22
                                         ,baseline_dealers$maize.owner.agree.long10h.q25)

index_overall_Longe10H_mid <- icwIndex(xmat=variables_overall_Longe10H_mid, revcols = c(1)) #x
baseline_dealers$index_overall_Longe10H_mid <- index_overall_Longe10H_mid$index #x

index_overall_Longe10H_base <- icwIndex(xmat=variables_overall_Longe10H_base, revcols = c(1))
baseline_dealers$index_overall_Longe10H_base <- index_overall_Longe10H_base$index #baseline index

################################################################################################################################################################################

###
#1#
###

results_dealer_secL10H <- c("mid_maize.owner.agree.long10h.q21", #1
                            "mid_maize.owner.agree.long10h.q22", #2
                            "mid_maize.owner.agree.long10h.q24", #3
                            "mid_maize.owner.agree.long10h.q25", #4
                            "mid_maize.owner.agree.long10h.q26", #5
                            "mid_maize.owner.agree.long10h.q27", #6
                            "mid_maize.owner.agree.long10h.q30", #7
                            "index_overall_Longe10H_mid")        #8
results_dealer_secL10H_base <- c("maize.owner.agree.long10h.q21",
                                 "maize.owner.agree.long10h.q22",
                                 "maize.owner.agree.long10h.q24",
                                 "maize.owner.agree.long10h.q25",
                                 "maize.owner.agree.long10h.q26",
                                 "maize.owner.agree.long10h.q27",
                                 "maize.owner.agree.long10h.q30",
                                 "index_overall_Longe10H_base")

baseline_dealers[results_dealer_secL10H_base] <- lapply(baseline_dealers[results_dealer_secL10H_base],function(x)x - mean(x,na.rm = T))

df_means_end_D_secL10H <- array(NA,dim=c(3,11))

for (i in 1:length(results_dealer_secL10H)){
  df_means_end_D_secL10H[1,i] <- sum(baseline_dealers[results_dealer_secL10H[i]], na.rm=T)/(nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_secL10H[i]])))
  df_means_end_D_secL10H[2,i] <- sqrt(var(baseline_dealers[results_dealer_secL10H[i]], na.rm=T))
  df_means_end_D_secL10H[3,i] <- nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_secL10H[i]]))-sum(is.na(baseline_dealers[results_dealer_secL10H_base[i]]))+sum(is.na(baseline_dealers[results_dealer_secL10H[i]])&is.na(baseline_dealers[results_dealer_secL10H_base[i]]))}

###
#2#
###

baseline_dealers$training_control[baseline_dealers$training==0] <- TRUE
baseline_dealers$training_control[baseline_dealers$training==1] <- FALSE

#11.
index_overall_Longe10H_mid <- icwIndex(xmat=variables_overall_Longe10H_mid,sgroup = baseline_dealers$training_control, revcols = c(1))
baseline_dealers$index_overall_Longe10H_midT <- index_overall_Longe10H_mid$index

index_overall_Longe10H_base <- icwIndex(xmat=variables_overall_Longe10H_base,sgroup = baseline_dealers$training_control, revcols = c(1))
baseline_dealers$index_overall_Longe10H_baseT <- index_overall_Longe10H_base$index

df_ols_end_D_secL10H <- array(NA,dim=c(3,3,11))

results_dealer_secL10H <- c("mid_maize.owner.agree.long10h.q21"
                            ,"mid_maize.owner.agree.long10h.q22"
                            ,"mid_maize.owner.agree.long10h.q24"
                            ,"mid_maize.owner.agree.long10h.q25"
                            ,"mid_maize.owner.agree.long10h.q26"
                            ,"mid_maize.owner.agree.long10h.q27"
                            ,"mid_maize.owner.agree.long10h.q30"
                            ,"index_overall_Longe10H_midT")
results_dealer_secL10H_base <- c("maize.owner.agree.long10h.q21"
                                 ,"maize.owner.agree.long10h.q22"
                                 ,"maize.owner.agree.long10h.q24"
                                 ,"maize.owner.agree.long10h.q25"
                                 ,"maize.owner.agree.long10h.q26"
                                 ,"maize.owner.agree.long10h.q27"
                                 ,"maize.owner.agree.long10h.q30"
                                 ,"index_overall_Longe10H_baseT")

baseline_dealers[results_dealer_secL10H_base] <- lapply(baseline_dealers[results_dealer_secL10H_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_dealer_secL10H)){
  ols <- lm(as.formula(paste(paste(results_dealer_secL10H[i],"training*clearing_demeaned*farmer_demeaned",sep="~"),results_dealer_secL10H_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_secL10H[i],"training*clearing_demeaned*farmer_demeaned",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")

  df_ols_end_D_secL10H[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_ols_end_D_secL10H[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_ols_end_D_secL10H[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]}

###
#3#
###

baseline_dealers$clearing_control[baseline_dealers$clearing==0] <- TRUE
baseline_dealers$clearing_control[baseline_dealers$clearing==1] <- FALSE

#11.
index_overall_Longe10H_mid <- icwIndex(xmat=variables_overall_Longe10H_mid,sgroup = baseline_dealers$clearing_control, revcols = c(1))
baseline_dealers$index_overall_Longe10H_midC <- index_overall_Longe10H_mid$index

index_overall_Longe10H_base <- icwIndex(xmat=variables_overall_Longe10H_base,sgroup = baseline_dealers$clearing_control, revcols = c(1))
baseline_dealers$index_overall_Longe10H_baseC <- index_overall_Longe10H_base$index

results_dealer_secL10H <- c("mid_maize.owner.agree.long10h.q21"
                            ,"mid_maize.owner.agree.long10h.q22"
                            ,"mid_maize.owner.agree.long10h.q24"
                            ,"mid_maize.owner.agree.long10h.q25"
                            ,"mid_maize.owner.agree.long10h.q26"
                            ,"mid_maize.owner.agree.long10h.q27"
                            ,"mid_maize.owner.agree.long10h.q30"
                            ,"index_overall_Longe10H_midC")
results_dealer_secL10H_base <- c("maize.owner.agree.long10h.q21"
                                 ,"maize.owner.agree.long10h.q22"
                                 ,"maize.owner.agree.long10h.q24"
                                 ,"maize.owner.agree.long10h.q25"
                                 ,"maize.owner.agree.long10h.q26"
                                 ,"maize.owner.agree.long10h.q27"
                                 ,"maize.owner.agree.long10h.q30"
                                 ,"index_overall_Longe10H_baseC")

baseline_dealers[results_dealer_secL10H_base] <- lapply(baseline_dealers[results_dealer_secL10H_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_dealer_secL10H)){
  ols <- lm(as.formula(paste(paste(results_dealer_secL10H[i],"training_demeaned*clearing*farmer_demeaned",sep="~"),results_dealer_secL10H_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_secL10H[i],"training_demeaned*clearing*farmer_demeaned",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")

  df_ols_end_D_secL10H[1,2,i] <- coef_test(ols, vcov_cluster)$beta[3]
  df_ols_end_D_secL10H[2,2,i] <- coef_test(ols, vcov_cluster)$SE[3]
  df_ols_end_D_secL10H[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]}

###
#4#
###

baseline_dealers$farmer_control[baseline_dealers$farmer==0] <- TRUE
baseline_dealers$farmer_control[baseline_dealers$farmer==1] <- FALSE

#11.
index_overall_Longe10H_mid <- icwIndex(xmat=variables_overall_Longe10H_mid,sgroup = baseline_dealers$farmer_control, revcols = c(1))
baseline_dealers$index_overall_Longe10H_midF <- index_overall_Longe10H_mid$index

index_overall_Longe10H_base <- icwIndex(xmat=variables_overall_Longe10H_base,sgroup = baseline_dealers$farmer_control, revcols = c(1))
baseline_dealers$index_overall_Longe10H_baseF <- index_overall_Longe10H_base$index

results_dealer_secL10H <- c("mid_maize.owner.agree.long10h.q21"
                            ,"mid_maize.owner.agree.long10h.q22"
                            ,"mid_maize.owner.agree.long10h.q24"
                            ,"mid_maize.owner.agree.long10h.q25"
                            ,"mid_maize.owner.agree.long10h.q26"
                            ,"mid_maize.owner.agree.long10h.q27"
                            ,"mid_maize.owner.agree.long10h.q30"
                            ,"index_overall_Longe10H_midF")
results_dealer_secL10H_base <- c("maize.owner.agree.long10h.q21"
                                 ,"maize.owner.agree.long10h.q22"
                                 ,"maize.owner.agree.long10h.q24"
                                 ,"maize.owner.agree.long10h.q25"
                                 ,"maize.owner.agree.long10h.q26"
                                 ,"maize.owner.agree.long10h.q27"
                                 ,"maize.owner.agree.long10h.q30"
                                 ,"index_overall_Longe10H_baseF")

baseline_dealers[results_dealer_secL10H_base] <- lapply(baseline_dealers[results_dealer_secL10H_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_dealer_secL10H)){
  ols <- lm(as.formula(paste(paste(results_dealer_secL10H[i],"training_demeaned*clearing_demeaned*farmer",sep="~"),results_dealer_secL10H_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_secL10H[i],"training_demeaned*clearing_demeaned*farmer",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")

  #farmer video treatment at village/shop level so no clustering needed
  df_ols_end_D_secL10H[1,3,i] <- summary(ols)$coefficients[4,1]
  df_ols_end_D_secL10H[2,3,i] <- summary(ols)$coefficients[4,2]
  df_ols_end_D_secL10H[3,3,i] <- summary(ols)$coefficients[4,4]}

#Aker, Boumnijel, McClelland, Tierney (2012)

df_dealer_secL10HT <- data.frame(baseline_dealers$mid_maize.owner.agree.long10h.q21,baseline_dealers$mid_maize.owner.agree.long10h.q22
                                 ,baseline_dealers$mid_maize.owner.agree.long10h.q24,baseline_dealers$mid_maize.owner.agree.long10h.q25
                                 ,baseline_dealers$mid_maize.owner.agree.long10h.q26,baseline_dealers$mid_maize.owner.agree.long10h.q27
                                 ,baseline_dealers$mid_maize.owner.agree.long10h.q30
                                 )
df_dealer_secL10HC <- df_dealer_secL10HT
df_dealer_secL10HF <- df_dealer_secL10HT
#no overall index
#all the same here

df_ols_end_D_secL10H_J <- array(NA,dim=c(3,3,10))

results_dealer_secL10H_J <- c("mid_maize.owner.agree.long10h.q21","mid_maize.owner.agree.long10h.q22","mid_maize.owner.agree.long10h.q24"
                            ,"mid_maize.owner.agree.long10h.q25","mid_maize.owner.agree.long10h.q26","mid_maize.owner.agree.long10h.q27"
                            ,"mid_maize.owner.agree.long10h.q30")
#no overall index

for (i in 1:length(results_dealer_secL10H_J)){
  df_ols_end_D_secL10H_J[3,1,i] <- adjust_p(df_ols_end_D_secL10H[3,1,i],df_dealer_secL10HT,i)
  df_ols_end_D_secL10H_J[3,2,i] <- adjust_p(df_ols_end_D_secL10H[3,2,i],df_dealer_secL10HC,i)
  df_ols_end_D_secL10H_J[3,3,i] <- adjust_p(df_ols_end_D_secL10H[3,3,i],df_dealer_secL10HF,i)}










###################################################################################################################################################################
##### 4B ANALYSIS: Agro-input dealer - Secondary: 9. Longe 10H: not controlling for baseline ######################################################################
###################################################################################################################################################################

variables_overall_Longe10H_mid <- cbind(baseline_dealers$mid_maize.owner.agree.long10h.q21,
                                        baseline_dealers$mid_maize.owner.agree.long10h.q22,
                                        baseline_dealers$mid_maize.owner.agree.long10h.q25,
                                        baseline_dealers$mid_maize.owner.agree.long10h.q27,
                                        baseline_dealers$mid_maize.owner.agree.long10h.q30)

index_overall_Longe10H_mid <- icwIndex(xmat=variables_overall_Longe10H_mid, revcols = c(1,4,5)) #x
baseline_dealers$index_overall_Longe10H_mid <- index_overall_Longe10H_mid$index #x

################################################################################################################################################################################

###
#1#
###

results_dealer_secL10H_B <- c("index_overall_Longe10H_mid")

df_means_end_D_secL10H_B <- array(NA,dim=c(3,11))

for (i in 1:length(results_dealer_secL10H_B)){
  df_means_end_D_secL10H_B[1,i] <- sum(baseline_dealers[results_dealer_secL10H_B[i]], na.rm=T)/(nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_secL10H_B[i]])))
  df_means_end_D_secL10H_B[2,i] <- sqrt(var(baseline_dealers[results_dealer_secL10H_B[i]], na.rm=T))
  df_means_end_D_secL10H_B[3,i] <- nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_secL10H_B[i]]))}

###
#2#
###

baseline_dealers$training_control[baseline_dealers$training==0] <- TRUE
baseline_dealers$training_control[baseline_dealers$training==1] <- FALSE

index_overall_Longe10H_mid <- icwIndex(xmat=variables_overall_Longe10H_mid,sgroup = baseline_dealers$training_control, revcols = c(1,4,5))
baseline_dealers$index_overall_Longe10H_midT <- index_overall_Longe10H_mid$index

df_ols_end_D_secL10H_B <- array(NA,dim=c(3,3,11))

results_dealer_secL10H_B <- c("index_overall_Longe10H_midT")

for (i in 1:length(results_dealer_secL10H_B)){
  #ols <- lm(as.formula(paste(paste(results_dealer_secL10H_B[i],"training*clearing_demeaned*farmer_demeaned",sep="~"),results_dealer_secL10H_B_base[i],sep="+")),data=baseline_dealers)
  ols <- lm(as.formula(paste(results_dealer_secL10H_B[i],"training*clearing_demeaned*farmer_demeaned",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")

  df_ols_end_D_secL10H_B[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_ols_end_D_secL10H_B[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_ols_end_D_secL10H_B[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]}

###
#3#
###

baseline_dealers$clearing_control[baseline_dealers$clearing==0] <- TRUE
baseline_dealers$clearing_control[baseline_dealers$clearing==1] <- FALSE

index_overall_Longe10H_mid <- icwIndex(xmat=variables_overall_Longe10H_mid,sgroup = baseline_dealers$clearing_control, revcols = c(1,4,5))
baseline_dealers$index_overall_Longe10H_midC <- index_overall_Longe10H_mid$index

results_dealer_secL10H_B <- c("index_overall_Longe10H_midC")

for (i in 1:length(results_dealer_secL10H_B)){
  #ols <- lm(as.formula(paste(paste(results_dealer_secL10H_B[i],"training_demeaned*clearing*farmer_demeaned",sep="~"),results_dealer_secL10H_B_base[i],sep="+")),data=baseline_dealers)
  ols <- lm(as.formula(paste(results_dealer_secL10H_B[i],"training_demeaned*clearing*farmer_demeaned",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")

  df_ols_end_D_secL10H_B[1,2,i] <- coef_test(ols, vcov_cluster)$beta[3]
  df_ols_end_D_secL10H_B[2,2,i] <- coef_test(ols, vcov_cluster)$SE[3]
  df_ols_end_D_secL10H_B[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]}

###
#4#
###

baseline_dealers$farmer_control[baseline_dealers$farmer==0] <- TRUE
baseline_dealers$farmer_control[baseline_dealers$farmer==1] <- FALSE

index_overall_Longe10H_mid <- icwIndex(xmat=variables_overall_Longe10H_mid,sgroup = baseline_dealers$farmer_control, revcols = c(1,4,5))
baseline_dealers$index_overall_Longe10H_midF <- index_overall_Longe10H_mid$index

results_dealer_secL10H_B <- c("index_overall_Longe10H_midF")

for (i in 1:length(results_dealer_secL10H_B)){
  #ols <- lm(as.formula(paste(paste(results_dealer_secL10H_B[i],"training_demeaned*clearing_demeaned*farmer",sep="~"),results_dealer_secL10H_B_base[i],sep="+")),data=baseline_dealers)
  ols <- lm(as.formula(paste(results_dealer_secL10H_B[i],"training_demeaned*clearing_demeaned*farmer",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")

  #farmer video treatment at village/shop level so no clustering needed
  df_ols_end_D_secL10H_B[1,3,i] <- summary(ols)$coefficients[4,1]
  df_ols_end_D_secL10H_B[2,3,i] <- summary(ols)$coefficients[4,2]
  df_ols_end_D_secL10H_B[3,3,i] <- summary(ols)$coefficients[4,4]}

baseline_dealers=baseline_dealers_save










###################################################################################################################################################################
##### 5 ANALYSIS: Agro-input dealer - Secondary: Longe 5###########################################################################################################
###################################################################################################################################################################

baseline_dealers_save=baseline_dealers
baseline_dealers=subset(baseline_dealers,check.owner.agree.q45=="Yes")

#1. Q45. Did you have Longe 5 in 2020 In stock in the second season 2020?

#2. Q46. How much of Longe 5 was carried forward from the previous season (first season 2020) into the second season of 2020 (kg)
baseline_dealers$maize.owner.agree.longe5.q46[baseline_dealers$maize.owner.agree.longe5.q46=="n/a"] <- NA
baseline_dealers$maize.owner.agree.longe5.q46 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.longe5.q46))
baseline_dealers$maize.owner.agree.longe5.q46[baseline_dealers$maize.owner.agree.longe5.q46==999] <- NA
baseline_dealers$maize.owner.agree.longe5.q46_unadj <- baseline_dealers$maize.owner.agree.longe5.q46
baseline_dealers$maize.owner.agree.longe5.q46[baseline_dealers$maize.owner.agree.q45=="0"] <- 0
baseline_dealers <- trim("maize.owner.agree.longe5.q46",baseline_dealers,trim_perc=.02)
baseline_dealers$maize.owner.agree.longe5.q46<-ihs(baseline_dealers$maize.owner.agree.longe5.q46)

baseline_dealers$mid_maize.owner.agree.longe5.q46 <- baseline_dealers$check.owner.agree.longe5.q46 #x
baseline_dealers$mid_maize.owner.agree.longe5.q46[baseline_dealers$mid_maize.owner.agree.longe5.q46=="n/a"] <- NA #x
baseline_dealers$mid_maize.owner.agree.longe5.q46 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.longe5.q46)) #x
baseline_dealers$mid_maize.owner.agree.longe5.q46[baseline_dealers$mid_maize.owner.agree.longe5.q46==999] <- NA #x
baseline_dealers$mid_maize.owner.agree.longe5.q46_unadj <- baseline_dealers$mid_maize.owner.agree.longe5.q46
baseline_dealers$mid_maize.owner.agree.longe5.q46[baseline_dealers$midmaize.owner.agree.q45=="0"] <- 0 #x
baseline_dealers <- trim("mid_maize.owner.agree.longe5.q46",baseline_dealers,trim_perc=.02) #x
baseline_dealers$mid_maize.owner.agree.longe5.q46<-ihs(baseline_dealers$mid_maize.owner.agree.longe5.q46) #x

#3. Q47. How much of Longe 5 was bought by you from any provider during the second season of 2020 (in kg)
baseline_dealers$maize.owner.agree.longe5.q47[baseline_dealers$maize.owner.agree.longe5.q47=="n/a"] <- NA
baseline_dealers$maize.owner.agree.longe5.q47[baseline_dealers$maize.owner.agree.longe5.q47==999] <- NA
baseline_dealers$maize.owner.agree.longe5.q47 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.longe5.q47))
baseline_dealers$maize.owner.agree.longe5.q47_unadj <- baseline_dealers$maize.owner.agree.longe5.q47
baseline_dealers$maize.owner.agree.longe5.q47[baseline_dealers$maize.owner.agree.q45=="0"] <- 0
baseline_dealers <- trim("maize.owner.agree.longe5.q47",baseline_dealers,trim_perc=.02)
baseline_dealers$maize.owner.agree.longe5.q47 <- ihs(baseline_dealers$maize.owner.agree.longe5.q47)

baseline_dealers$mid_maize.owner.agree.longe5.q47 <- baseline_dealers$check.owner.agree.longe5.q47 #x
baseline_dealers$mid_maize.owner.agree.longe5.q47[baseline_dealers$mid_maize.owner.agree.longe5.q47=="n/a"] <- NA #x
baseline_dealers$mid_maize.owner.agree.longe5.q47[baseline_dealers$mid_maize.owner.agree.longe5.q47==999] <- NA #x
baseline_dealers$mid_maize.owner.agree.longe5.q47 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.longe5.q47)) #x
baseline_dealers$mid_maize.owner.agree.longe5.q47_unadj <- baseline_dealers$mid_maize.owner.agree.longe5.q47
baseline_dealers$mid_maize.owner.agree.longe5.q47[baseline_dealers$mid_maize.owner.agree.q45=="0"] <- 0 #x
baseline_dealers <- trim("mid_maize.owner.agree.longe5.q47",baseline_dealers,trim_perc=.02) #x
baseline_dealers$mid_maize.owner.agree.longe5.q47 <- ihs(baseline_dealers$mid_maize.owner.agree.longe5.q47) #x

#4. Q49. What was the cost of Longe 5 from where you obtained it during the second season of 2020? (ugx per kg)
baseline_dealers$maize.owner.agree.longe5.q49[baseline_dealers$maize.owner.agree.longe5.q49=="n/a"] <- NA
baseline_dealers$maize.owner.agree.longe5.q49[baseline_dealers$maize.owner.agree.longe5.q49==999] <- NA
baseline_dealers$maize.owner.agree.longe5.q49 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.longe5.q49))
baseline_dealers <- trim("maize.owner.agree.longe5.q49",baseline_dealers,trim_perc=.02)
baseline_dealers$maize.owner.agree.longe5.q49 <- ihs(baseline_dealers$maize.owner.agree.longe5.q49) #x

baseline_dealers$mid_maize.owner.agree.longe5.q49 <- baseline_dealers$check.owner.agree.longe5.q49 #x
baseline_dealers$mid_maize.owner.agree.longe5.q49[baseline_dealers$mid_maize.owner.agree.longe5.q49=="n/a"] <- NA #x
baseline_dealers$mid_maize.owner.agree.longe5.q49[baseline_dealers$mid_maize.owner.agree.longe5.q49==999] <- NA #x
baseline_dealers$mid_maize.owner.agree.longe5.q49 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.longe5.q49)) #x
baseline_dealers <- trim("mid_maize.owner.agree.longe5.q49",baseline_dealers,trim_perc=.02) #x
baseline_dealers$mid_maize.owner.agree.longe5.q49 <- ihs(baseline_dealers$mid_maize.owner.agree.longe5.q49) #x

#5. Q50. Total quantity sold of ${carry3} (Kg) over the second season of 2020
baseline_dealers <- trim("maize.owner.agree.longe5.q50",baseline_dealers,trim_perc=.02)
baseline_dealers$maize.owner.agree.longe5.q50 <- ihs(baseline_dealers$maize.owner.agree.longe5.q50)

baseline_dealers <- trim("mid_maize.owner.agree.longe5.q50",baseline_dealers,trim_perc=.02) #x
baseline_dealers$mid_maize.owner.agree.longe5.q50 <- ihs(baseline_dealers$mid_maize.owner.agree.longe5.q50) #x

#6. Q51. Sales price per kilogram of ${q50}  at the beginning of the second season of 2020
baseline_dealers <- trim("maize.owner.agree.longe5.q51",baseline_dealers,trim_perc=.02)
baseline_dealers$maize.owner.agree.longe5.q51 <- ihs(baseline_dealers$maize.owner.agree.longe5.q51)

baseline_dealers <- trim("mid_maize.owner.agree.longe5.q51",baseline_dealers,trim_perc=.02) #x
baseline_dealers$mid_maize.owner.agree.longe5.q51 <- ihs(baseline_dealers$mid_maize.owner.agree.longe5.q51) #x

#7. Q52. How much of Longe 5 was lost/wasted the second season of 2020 (kg)
baseline_dealers$sold3_condition <- baseline_dealers$maize.owner.agree.longe5.q47_unadj+baseline_dealers$maize.owner.agree.longe5.q46_unadj-baseline_dealers$maize.owner.agree.longe5.q50_unadj
baseline_dealers$q52_isnot_asked <- 0
baseline_dealers$q52_isnot_asked[baseline_dealers$sold3_condition<=0]<-1

baseline_dealers$maize.owner.agree.longe5.q52[baseline_dealers$maize.owner.agree.longe5.q52=="n/a"] <- NA
baseline_dealers$maize.owner.agree.longe5.q52[baseline_dealers$maize.owner.agree.q45=="0"] <- 0
baseline_dealers$maize.owner.agree.longe5.q52[baseline_dealers$q52_isnot_asked==1] <- 0
baseline_dealers <- trim("maize.owner.agree.longe5.q52",baseline_dealers,trim_perc=.02)
baseline_dealers$maize.owner.agree.longe5.q52 <- ihs(baseline_dealers$maize.owner.agree.longe5.q52)

baseline_dealers$mid_sold3_condition <- baseline_dealers$mid_maize.owner.agree.longe5.q47_unadj+baseline_dealers$mid_maize.owner.agree.longe5.q46_unadj-baseline_dealers$mid_maize.owner.agree.longe5.q50_unadj
baseline_dealers$mid_q52_isnot_asked <- 0
baseline_dealers$mid_q52_isnot_asked[baseline_dealers$mid_sold3_condition<=0]<-1

baseline_dealers$mid_maize.owner.agree.longe5.q52 <- baseline_dealers$check.owner.agree.longe5.q52 #x
#baseline_dealers$mid_maize.owner.agree.longe5.q52[baseline_dealers$mid_maize.owner.agree.longe5.q52=="n/a"] <- 0 #x
baseline_dealers$mid_maize.owner.agree.longe5.q52[baseline_dealers$mid_maize.owner.agree.longe5.q52==999] <- NA #x
baseline_dealers$mid_maize.owner.agree.longe5.q52 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.longe5.q52)) #x
baseline_dealers$mid_maize.owner.agree.longe5.q52[baseline_dealers$mid_maize.owner.agree.q45=="0"] <- 0 #x
baseline_dealers$mid_maize.owner.agree.longe5.q52[baseline_dealers$mid_q52_isnot_asked==1] <- 0
baseline_dealers <- trim("mid_maize.owner.agree.longe5.q52",baseline_dealers,trim_perc=.02) #x
baseline_dealers$mid_maize.owner.agree.longe5.q52 <- ihs(baseline_dealers$mid_maize.owner.agree.longe5.q52) #x

#8. Q54. Did you ever run out of this Longe 5 during the second season of 2020?
baseline_dealers$maize.owner.agree.longe5.q54[baseline_dealers$maize.owner.agree.longe5.q54=="n/a"] <- NA
baseline_dealers$maize.owner.agree.longe5.q54 <- ifelse(baseline_dealers$maize.owner.agree.longe5.q54=="Yes",1,0)

baseline_dealers$mid_maize.owner.agree.longe5.q54 <- baseline_dealers$check.owner.agree.longe5.q54 #x
baseline_dealers$mid_maize.owner.agree.longe5.q54[baseline_dealers$mid_maize.owner.agree.longe5.q54=="n/a"] <- NA #x
baseline_dealers$mid_maize.owner.agree.longe5.q54 <- ifelse(baseline_dealers$mid_maize.owner.agree.longe5.q54=="Yes",1,0) #x

#9. Q55. Estimate how often you ran out of stock for Longe 5 during the second season of 2020
baseline_dealers$maize.owner.agree.longe5.q55 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.longe5.q55))
baseline_dealers$maize.owner.agree.longe5.q55[baseline_dealers$maize.owner.agree.longe5.q54=="0"] <- 0
baseline_dealers$maize.owner.agree.longe5.q55[baseline_dealers$maize.owner.agree.longe5.q55=="1"] <- 21.74 #Everyday: 21.74 working days per month
baseline_dealers$maize.owner.agree.longe5.q55[baseline_dealers$maize.owner.agree.longe5.q55=="2"] <- 4.34524 #once a week: 4,34524 weeks in a month
baseline_dealers$maize.owner.agree.longe5.q55[baseline_dealers$maize.owner.agree.longe5.q55=="3"] <- 1 #Once a month
baseline_dealers$maize.owner.agree.longe5.q55[baseline_dealers$maize.owner.agree.longe5.q55=="4"] <- 1/3 #once in a season
baseline_dealers$maize.owner.agree.longe5.q55[baseline_dealers$maize.owner.agree.longe5.q55=="5"] <- 0 #Never
baseline_dealers$maize.owner.agree.longe5.q55[baseline_dealers$maize.owner.agree.longe5.q55=="6"] <- NA #Never
baseline_dealers <- trim("maize.owner.agree.longe5.q55",baseline_dealers,trim_perc=.02)
baseline_dealers$maize.owner.agree.longe5.q55 <- ihs(baseline_dealers$maize.owner.agree.longe5.q55) #x

baseline_dealers$mid_maize.owner.agree.longe5.q55 <- baseline_dealers$check.owner.agree.longe5.q55 #x
baseline_dealers$mid_maize.owner.agree.longe5.q55 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.longe5.q55))
baseline_dealers$mid_maize.owner.agree.longe5.q55[baseline_dealers$mid_maize.owner.agree.longe5.q54=="0"] <- 0
baseline_dealers$mid_maize.owner.agree.longe5.q55[baseline_dealers$mid_maize.owner.agree.longe5.q55=="1"] <- 21.74 #Everyday: 21.74 working days per month
baseline_dealers$mid_maize.owner.agree.longe5.q55[baseline_dealers$mid_maize.owner.agree.longe5.q55=="2"] <- 4.34524 #once a week: 4,34524 weeks in a month
baseline_dealers$mid_maize.owner.agree.longe5.q55[baseline_dealers$mid_maize.owner.agree.longe5.q55=="3"] <- 1 #Once a month
baseline_dealers$mid_maize.owner.agree.longe5.q55[baseline_dealers$mid_maize.owner.agree.longe5.q55=="4"] <- 1/3 #once in a season
baseline_dealers$mid_maize.owner.agree.longe5.q55[baseline_dealers$mid_maize.owner.agree.longe5.q55=="5"] <- 0 #Never
baseline_dealers$mid_maize.owner.agree.longe5.q55[baseline_dealers$mid_maize.owner.agree.longe5.q55=="6"] <- NA #Never
baseline_dealers <- trim("mid_maize.owner.agree.longe5.q55",baseline_dealers,trim_perc=.02)
baseline_dealers$mid_maize.owner.agree.longe5.q55 <- ihs(baseline_dealers$mid_maize.owner.agree.longe5.q55) #x

#11. Overall index of secondary Longe5 agro-input dealer outcome variables
variables_overall_Longe5_mid <- cbind(baseline_dealers$mid_maize.owner.agree.longe5.q46,
                                      baseline_dealers$mid_maize.owner.agree.longe5.q47,
                                      baseline_dealers$mid_maize.owner.agree.longe5.q50) #x
variables_overall_Longe5_base <- cbind(baseline_dealers$maize.owner.agree.longe5.q46,
                                       baseline_dealers$maize.owner.agree.longe5.q47,
                                       baseline_dealers$maize.owner.agree.longe5.q50)

index_overall_Longe5_mid <- icwIndex(xmat=variables_overall_Longe5_mid,revcols = c(1)) #x
baseline_dealers$index_overall_Longe5_mid <- index_overall_Longe5_mid$index #x

index_overall_Longe5_base <- icwIndex(xmat=variables_overall_Longe5_base,revcols = c(1))
baseline_dealers$index_overall_Longe5_base <- index_overall_Longe5_base$index

################################################################################################################################################################################

###
#1#
###

results_dealer_secL5 <- c("mid_maize.owner.agree.longe5.q46", #1
                          "mid_maize.owner.agree.longe5.q47", #2
                          "mid_maize.owner.agree.longe5.q49", #3
                          "mid_maize.owner.agree.longe5.q50", #4
                          "mid_maize.owner.agree.longe5.q51", #5
                          "mid_maize.owner.agree.longe5.q52", #6
                          "mid_maize.owner.agree.longe5.q55", #7
                          "index_overall_Longe5_mid")         #8
results_dealer_secL5_base <- c("maize.owner.agree.longe5.q46",
                               "maize.owner.agree.longe5.q47",
                               "maize.owner.agree.longe5.q49",
                               "maize.owner.agree.longe5.q50",
                               "maize.owner.agree.longe5.q51",
                               "maize.owner.agree.longe5.q52",
                               "maize.owner.agree.longe5.q55",
                               "index_overall_Longe5_base")

baseline_dealers[results_dealer_secL5_base] <- lapply(baseline_dealers[results_dealer_secL5_base],function(x)x - mean(x,na.rm = T))

df_means_end_D_secL5 <- array(NA,dim=c(3,11))

for (i in 1:length(results_dealer_secL5)){
  df_means_end_D_secL5[1,i] <- sum(baseline_dealers[results_dealer_secL5[i]], na.rm=T)/(nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_secL5[i]])))
  df_means_end_D_secL5[2,i] <- sqrt(var(baseline_dealers[results_dealer_secL5[i]], na.rm=T))
  df_means_end_D_secL5[3,i] <- nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_secL5[i]]))-sum(is.na(baseline_dealers[results_dealer_secL5_base[i]]))+sum(is.na(baseline_dealers[results_dealer_secL5[i]])&is.na(baseline_dealers[results_dealer_secL5_base[i]]))}

###
#2#
###

baseline_dealers$training_control[baseline_dealers$training==0] <- TRUE
baseline_dealers$training_control[baseline_dealers$training==1] <- FALSE

#11.
index_overall_Longe5_mid <- icwIndex(xmat=variables_overall_Longe5_mid,sgroup = baseline_dealers$training_control,revcols = c(1))
baseline_dealers$index_overall_Longe5_midT <- index_overall_Longe5_mid$index

index_overall_Longe5_base <- icwIndex(xmat=variables_overall_Longe5_base,sgroup = baseline_dealers$training_control,revcols = c(1))
baseline_dealers$index_overall_Longe5_baseT <- index_overall_Longe5_base$index

df_ols_end_D_secL5 <- array(NA,dim=c(3,3,11))

results_dealer_secL5 <- c("mid_maize.owner.agree.longe5.q46"
                          ,"mid_maize.owner.agree.longe5.q47"
                          ,"mid_maize.owner.agree.longe5.q49"
                          ,"mid_maize.owner.agree.longe5.q50"
                          ,"mid_maize.owner.agree.longe5.q51"
                          ,"mid_maize.owner.agree.longe5.q52"
                          ,"mid_maize.owner.agree.longe5.q55"
                          ,"index_overall_Longe5_midT")
results_dealer_secL5_base <- c("maize.owner.agree.longe5.q46"
                               ,"maize.owner.agree.longe5.q47"
                               ,"maize.owner.agree.longe5.q49"
                               ,"maize.owner.agree.longe5.q50"
                               ,"maize.owner.agree.longe5.q51"
                               ,"maize.owner.agree.longe5.q52"
                               ,"maize.owner.agree.longe5.q55"
                               ,"index_overall_Longe5_baseT")

baseline_dealers[results_dealer_secL5_base] <- lapply(baseline_dealers[results_dealer_secL5_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_dealer_secL5)){
  ols <- lm(as.formula(paste(paste(results_dealer_secL5[i],"training*clearing_demeaned*farmer_demeaned",sep="~"),results_dealer_secL5_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_secL5[i],"training*clearing_demeaned*farmer_demeaned",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")

  df_ols_end_D_secL5[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_ols_end_D_secL5[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_ols_end_D_secL5[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]}

###
#3#
###

baseline_dealers$clearing_control[baseline_dealers$clearing==0] <- TRUE
baseline_dealers$clearing_control[baseline_dealers$clearing==1] <- FALSE

#11.
index_overall_Longe5_mid <- icwIndex(xmat=variables_overall_Longe5_mid,sgroup = baseline_dealers$clearing_control,revcols = c(1))
baseline_dealers$index_overall_Longe5_midC <- index_overall_Longe5_mid$index

index_overall_Longe5_base <- icwIndex(xmat=variables_overall_Longe5_base,sgroup = baseline_dealers$clearing_control,revcols = c(1))
baseline_dealers$index_overall_Longe5_baseC <- index_overall_Longe5_base$index

results_dealer_secL5 <- c("mid_maize.owner.agree.longe5.q46"
                          ,"mid_maize.owner.agree.longe5.q47"
                          ,"mid_maize.owner.agree.longe5.q49"
                          ,"mid_maize.owner.agree.longe5.q50"
                          ,"mid_maize.owner.agree.longe5.q51"
                          ,"mid_maize.owner.agree.longe5.q52"
                          ,"mid_maize.owner.agree.longe5.q55"
                          ,"index_overall_Longe5_midC")
results_dealer_secL5_base <- c("maize.owner.agree.longe5.q46"
                               ,"maize.owner.agree.longe5.q47"
                               ,"maize.owner.agree.longe5.q49"
                               ,"maize.owner.agree.longe5.q50"
                               ,"maize.owner.agree.longe5.q51"
                               ,"maize.owner.agree.longe5.q52"
                               ,"maize.owner.agree.longe5.q55"
                               ,"index_overall_Longe5_baseC")

baseline_dealers[results_dealer_secL5_base] <- lapply(baseline_dealers[results_dealer_secL5_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_dealer_secL5)){
  ols <- lm(as.formula(paste(paste(results_dealer_secL5[i],"training_demeaned*clearing*farmer_demeaned",sep="~"),results_dealer_secL5_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_secL5[i],"training_demeaned*clearing*farmer_demeaned",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")

  df_ols_end_D_secL5[1,2,i] <- coef_test(ols, vcov_cluster)$beta[3]
  df_ols_end_D_secL5[2,2,i] <- coef_test(ols, vcov_cluster)$SE[3]
  df_ols_end_D_secL5[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]}

###
#4#
###

baseline_dealers$farmer_control[baseline_dealers$farmer==0] <- TRUE
baseline_dealers$farmer_control[baseline_dealers$farmer==1] <- FALSE

#11.
index_overall_Longe5_mid <- icwIndex(xmat=variables_overall_Longe5_mid,sgroup = baseline_dealers$farmer_control,revcols = c(1))
baseline_dealers$index_overall_Longe5_midF <- index_overall_Longe5_mid$index

index_overall_Longe5_base <- icwIndex(xmat=variables_overall_Longe5_base,sgroup = baseline_dealers$farmer_control,revcols = c(1))
baseline_dealers$index_overall_Longe5_baseF <- index_overall_Longe5_base$index

results_dealer_secL5 <- c("mid_maize.owner.agree.longe5.q46"
                          ,"mid_maize.owner.agree.longe5.q47"
                          ,"mid_maize.owner.agree.longe5.q49"
                          ,"mid_maize.owner.agree.longe5.q50"
                          ,"mid_maize.owner.agree.longe5.q51"
                          ,"mid_maize.owner.agree.longe5.q52"
                          ,"mid_maize.owner.agree.longe5.q55"
                          ,"index_overall_Longe5_midF")
results_dealer_secL5_base <- c("maize.owner.agree.longe5.q46"
                               ,"maize.owner.agree.longe5.q47"
                               ,"maize.owner.agree.longe5.q49"
                               ,"maize.owner.agree.longe5.q50"
                               ,"maize.owner.agree.longe5.q51"
                               ,"maize.owner.agree.longe5.q52"
                               ,"maize.owner.agree.longe5.q55"
                               ,"index_overall_Longe5_baseF")

baseline_dealers[results_dealer_secL5_base] <- lapply(baseline_dealers[results_dealer_secL5_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_dealer_secL5)){
  ols <- lm(as.formula(paste(paste(results_dealer_secL5[i],"training_demeaned*clearing_demeaned*farmer",sep="~"),results_dealer_secL5_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_secL5[i],"training_demeaned*clearing_demeaned*farmer",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")

  #farmer video treatment at village/shop level so no clustering needed
  df_ols_end_D_secL5[1,3,i] <- summary(ols)$coefficients[4,1]
  df_ols_end_D_secL5[2,3,i] <- summary(ols)$coefficients[4,2]
  df_ols_end_D_secL5[3,3,i] <- summary(ols)$coefficients[4,4]}

#Aker, Boumnijel, McClelland, Tierney (2012)
df_dealer_secL5T <- data.frame(baseline_dealers$mid_maize.owner.agree.longe5.q46
                               ,baseline_dealers$mid_maize.owner.agree.longe5.q47,baseline_dealers$mid_maize.owner.agree.longe5.q49
                                ,baseline_dealers$mid_maize.owner.agree.longe5.q50,baseline_dealers$mid_maize.owner.agree.longe5.q51
                                ,baseline_dealers$mid_maize.owner.agree.longe5.q52
                                ,baseline_dealers$mid_maize.owner.agree.longe5.q55)

df_dealer_secL5C <- df_dealer_secL5T
df_dealer_secL5F <- df_dealer_secL5T
#no overall index

df_ols_end_D_secL5_J <- array(NA,dim=c(3,3,11))

results_dealer_secL5_J <- c("mid_maize.owner.agree.longe5.q46"
                            ,"mid_maize.owner.agree.longe5.q47","mid_maize.owner.agree.longe5.q49"
                            ,"mid_maize.owner.agree.longe5.q50","mid_maize.owner.agree.longe5.q51"
                            ,"mid_maize.owner.agree.longe5.q52"
                            ,"mid_maize.owner.agree.longe5.q55")
#no overall index

for (i in 1:length(results_dealer_secL5_J)){
  df_ols_end_D_secL5_J[3,1,i] <- adjust_p(df_ols_end_D_secL5[3,1,i],df_dealer_secL5T,i)
  df_ols_end_D_secL5_J[3,2,i] <- adjust_p(df_ols_end_D_secL5[3,2,i],df_dealer_secL5C,i)
  df_ols_end_D_secL5_J[3,3,i] <- adjust_p(df_ols_end_D_secL5[3,3,i],df_dealer_secL5F,i)}










###################################################################################################################################################################
##### 5B ANALYSIS: Agro-input dealer - Secondary: Longe 5, not controlling for baseline ###########################################################################
###################################################################################################################################################################

#11. Overall index of secondary Longe5 agro-input dealer outcome variables
variables_overall_Longe5_mid <- cbind(baseline_dealers$mid_maize.owner.agree.longe5.q46,
                                      baseline_dealers$mid_maize.owner.agree.longe5.q47,
                                      baseline_dealers$mid_maize.owner.agree.longe5.q50,
                                      baseline_dealers$mid_maize.owner.agree.longe5.q52,
                                      baseline_dealers$mid_maize.owner.agree.longe5.q55) #x

index_overall_Longe5_mid <- icwIndex(xmat=variables_overall_Longe5_mid,revcols = c(1,4,5)) #x
baseline_dealers$index_overall_Longe5_mid <- index_overall_Longe5_mid$index #x

###
#1#
###

results_dealer_secL5_B <- c("index_overall_Longe5_mid")

df_means_end_D_secL5_B <- array(NA,dim=c(3,11))

for (i in 1:length(results_dealer_secL5_B)){
  df_means_end_D_secL5_B[1,i] <- sum(baseline_dealers[results_dealer_secL5_B[i]], na.rm=T)/(nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_secL5_B[i]])))
  df_means_end_D_secL5_B[2,i] <- sqrt(var(baseline_dealers[results_dealer_secL5_B[i]], na.rm=T))
  df_means_end_D_secL5_B[3,i] <- nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_secL5_B[i]]))}

###
#2#
###

baseline_dealers$training_control[baseline_dealers$training==0] <- TRUE
baseline_dealers$training_control[baseline_dealers$training==1] <- FALSE

#11.
index_overall_Longe5_mid <- icwIndex(xmat=variables_overall_Longe5_mid,sgroup = baseline_dealers$training_control,revcols = c(1,4,5))
baseline_dealers$index_overall_Longe5_midT <- index_overall_Longe5_mid$index

df_ols_end_D_secL5_B <- array(NA,dim=c(3,3,11))

results_dealer_secL5_B <- c("index_overall_Longe5_midT")

for (i in 1:length(results_dealer_secL5_B)){
  #ols <- lm(as.formula(paste(paste(results_dealer_secL5_B[i],"training*clearing_demeaned*farmer_demeaned",sep="~"),results_dealer_secL5_base[i],sep="+")),data=baseline_dealers)
  ols <- lm(as.formula(paste(results_dealer_secL5_B[i],"training*clearing_demeaned*farmer_demeaned",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")

  df_ols_end_D_secL5_B[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_ols_end_D_secL5_B[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_ols_end_D_secL5_B[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]}

###
#3#
###

baseline_dealers$clearing_control[baseline_dealers$clearing==0] <- TRUE
baseline_dealers$clearing_control[baseline_dealers$clearing==1] <- FALSE

#11.
index_overall_Longe5_mid <- icwIndex(xmat=variables_overall_Longe5_mid,sgroup = baseline_dealers$clearing_control,revcols = c(1,4,5))
baseline_dealers$index_overall_Longe5_midC <- index_overall_Longe5_mid$index

results_dealer_secL5_B <- c("index_overall_Longe5_midC")

for (i in 1:length(results_dealer_secL5_B)){
  #ols <- lm(as.formula(paste(paste(results_dealer_secL5_B[i],"training_demeaned*clearing*farmer_demeaned",sep="~"),results_dealer_secL5_base[i],sep="+")),data=baseline_dealers)
  ols <- lm(as.formula(paste(results_dealer_secL5_B[i],"training_demeaned*clearing*farmer_demeaned",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")

  df_ols_end_D_secL5_B[1,2,i] <- coef_test(ols, vcov_cluster)$beta[3]
  df_ols_end_D_secL5_B[2,2,i] <- coef_test(ols, vcov_cluster)$SE[3]
  df_ols_end_D_secL5_B[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]}

###
#4#
###

baseline_dealers$farmer_control[baseline_dealers$farmer==0] <- TRUE
baseline_dealers$farmer_control[baseline_dealers$farmer==1] <- FALSE

#11.
index_overall_Longe5_mid <- icwIndex(xmat=variables_overall_Longe5_mid,sgroup = baseline_dealers$farmer_control,revcols = c(1,4,5))
baseline_dealers$index_overall_Longe5_midF <- index_overall_Longe5_mid$index

results_dealer_secL5_B <- c("index_overall_Longe5_midF")

for (i in 1:length(results_dealer_secL5_B)){
  #ols <- lm(as.formula(paste(paste(results_dealer_secL5_B[i],"training_demeaned*clearing_demeaned*farmer",sep="~"),results_dealer_secL5_base[i],sep="+")),data=baseline_dealers)
  ols <- lm(as.formula(paste(results_dealer_secL5_B[i],"training_demeaned*clearing_demeaned*farmer",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")

  #farmer video treatment at village/shop level so no clustering needed
  df_ols_end_D_secL5_B[1,3,i] <- summary(ols)$coefficients[4,1]
  df_ols_end_D_secL5_B[2,3,i] <- summary(ols)$coefficients[4,2]
  df_ols_end_D_secL5_B[3,3,i] <- summary(ols)$coefficients[4,4]}

baseline_dealers=baseline_dealers_save










###################################################################################################################################################################
##### 6 ANALYSIS: Agro-input dealer - Secondary: 11. official #####################################################################################################
###################################################################################################################################################################

#1. Q114. Is this business registered as a seed dealer with UNADA (Uganda National Agro-input Dealers Association?
baseline_dealers$mid_maize.owner.agree.inspection.q114 <- baseline_dealers$check.owner.agree.inspection.q114 #x
baseline_dealers$mid_maize.owner.agree.inspection.q114[baseline_dealers$mid_maize.owner.agree.inspection.q114=="n/a"] <- NA
baseline_dealers$mid_maize.owner.agree.inspection.q114[baseline_dealers$mid_maize.owner.agree.inspection.q114==98] <- NA #here because binary #x
baseline_dealers$mid_maize.owner.agree.inspection.q114<-as.character(baseline_dealers$mid_maize.owner.agree.inspection.q114) #x
baseline_dealers$mid_maize.owner.agree.inspection.q114<-ifelse(baseline_dealers$mid_maize.owner.agree.inspection.q114=="Yes",1,0) #x
baseline_dealers$mid_maize.owner.agree.inspection.q114 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.inspection.q114)) #x

#2. Q115. Does this business have a trading license issued by local government?
baseline_dealers$mid_maize.owner.agree.inspection.q115 <- baseline_dealers$check.owner.agree.inspection.q115 #x
baseline_dealers$mid_maize.owner.agree.inspection.q115[baseline_dealers$mid_maize.owner.agree.inspection.q115=="n/a"] <- NA
baseline_dealers$mid_maize.owner.agree.inspection.q115[baseline_dealers$mid_maize.owner.agree.inspection.q115==98] <- NA #here because binary #x
baseline_dealers$mid_maize.owner.agree.inspection.q115<-as.character(baseline_dealers$mid_maize.owner.agree.inspection.q115) #x
baseline_dealers$mid_maize.owner.agree.inspection.q115<-ifelse(baseline_dealers$mid_maize.owner.agree.inspection.q115=="Yes",1,0) #x
baseline_dealers$mid_maize.owner.agree.inspection.q115 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.inspection.q115)) #x

#3. Q116. Is this business a member of any other professional association?
baseline_dealers$mid_maize.owner.agree.inspection.q116 <- baseline_dealers$check.owner.agree.inspection.q116 #x
baseline_dealers$mid_maize.owner.agree.inspection.q116[baseline_dealers$mid_maize.owner.agree.inspection.q116=="n/a"] <- NA
baseline_dealers$mid_maize.owner.agree.inspection.q116[baseline_dealers$mid_maize.owner.agree.inspection.q116==98] <- NA #here because binary #x
baseline_dealers$mid_maize.owner.agree.inspection.q116<-as.character(baseline_dealers$mid_maize.owner.agree.inspection.q116) #x
baseline_dealers$mid_maize.owner.agree.inspection.q116<-ifelse(baseline_dealers$mid_maize.owner.agree.inspection.q116=="Yes",1,0) #x
baseline_dealers$mid_maize.owner.agree.inspection.q116 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.inspection.q116)) #x

#4. Q117. How often were you inspected by DAO/MAAIF or UNADA last year (indicate 0 if no inspection happened).
baseline_dealers$maize.owner.agree.inspection.q117[baseline_dealers$maize.owner.agree.inspection.q117==999] <- NA
baseline_dealers$maize.owner.agree.inspection.q117 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.inspection.q117))
baseline_dealers <- trim("maize.owner.agree.inspection.q117",baseline_dealers,trim_perc=.02)
baseline_dealers$maize.owner.agree.inspection.q117 <- ihs(baseline_dealers$maize.owner.agree.inspection.q117)

baseline_dealers$mid_maize.owner.agree.inspection.q117 <- baseline_dealers$check.owner.agree.inspection.q117 #x
baseline_dealers$mid_maize.owner.agree.inspection.q117 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.inspection.q117))
baseline_dealers$mid_maize.owner.agree.inspection.q117[baseline_dealers$mid_maize.owner.agree.inspection.q117==999] <- NA
baseline_dealers <- trim("mid_maize.owner.agree.inspection.q117",baseline_dealers,trim_perc=.02) #x
baseline_dealers$mid_maize.owner.agree.inspection.q117 <- ihs(baseline_dealers$mid_maize.owner.agree.inspection.q117) #x

#5. Q118. Have you ever received a warning as a result of inspection if something was not up to standard?
baseline_dealers$mid_maize.owner.agree.inspection.q118 <- baseline_dealers$check.owner.agree.inspection.q118 #x
baseline_dealers$mid_maize.owner.agree.inspection.q118[baseline_dealers$mid_maize.owner.agree.inspection.q118==98] <- NA #x
baseline_dealers$mid_maize.owner.agree.inspection.q118[baseline_dealers$mid_maize.owner.agree.inspection.q118=="n/a"] <- NA #x
baseline_dealers$mid_maize.owner.agree.inspection.q118<-as.character(baseline_dealers$mid_maize.owner.agree.inspection.q118) #x
baseline_dealers$mid_maize.owner.agree.inspection.q118<-ifelse(baseline_dealers$mid_maize.owner.agree.inspection.q118=="Yes",1,0) #x
baseline_dealers$mid_maize.owner.agree.inspection.q118<-as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.inspection.q118)) #x

#6. Q119. Has some of your produce ever been confiscated after inspection?
baseline_dealers$mid_maize.owner.agree.inspection.q119 <- baseline_dealers$check.owner.agree.inspection.q119 #x
baseline_dealers$mid_maize.owner.agree.inspection.q119[baseline_dealers$mid_maize.owner.agree.inspection.q119==98] <- NA #x
baseline_dealers$mid_maize.owner.agree.inspection.q119[baseline_dealers$mid_maize.owner.agree.inspection.q119=="n/a"] <- NA #x
baseline_dealers$mid_maize.owner.agree.inspection.q119<-as.character(baseline_dealers$mid_maize.owner.agree.inspection.q119) #x
baseline_dealers$mid_maize.owner.agree.inspection.q119<-ifelse(baseline_dealers$mid_maize.owner.agree.inspection.q119=="Yes",1,0) #x
baseline_dealers$mid_maize.owner.agree.inspection.q119<-as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.inspection.q119)) #x

#8. Overall index of secondary OFFICIAL agro-input dealer outcome variables
variables_overall_off_mid <- cbind(baseline_dealers$mid_maize.owner.agree.inspection.q114,baseline_dealers$mid_maize.owner.agree.inspection.q115
                                   ,baseline_dealers$mid_maize.owner.agree.inspection.q116,baseline_dealers$mid_maize.owner.agree.inspection.q118
                                   ,baseline_dealers$mid_maize.owner.agree.inspection.q119,baseline_dealers$mid_maize.owner.agree.inspection.q117) #x
variables_overall_off_base <- cbind(baseline_dealers$maize.owner.agree.inspection.q114,baseline_dealers$maize.owner.agree.inspection.q115
                                    ,baseline_dealers$maize.owner.agree.inspection.q116,baseline_dealers$maize.owner.agree.inspection.q118
                                    ,baseline_dealers$maize.owner.agree.inspection.q119,baseline_dealers$maize.owner.agree.inspection.q117)

index_overall_off_mid <- icwIndex(xmat=variables_overall_off_mid,revcols = c(4,5)) #x
baseline_dealers$index_overall_off_mid <- index_overall_off_mid$index #x

index_overall_off_base <- icwIndex(xmat=variables_overall_off_base,revcols = c(4,5))
baseline_dealers$index_overall_off_base <- index_overall_off_base$index

################################################################################################################################################################################

###
#1#
###

results_dealer_sec_off <- c("mid_maize.owner.agree.inspection.q114", #1
                            "mid_maize.owner.agree.inspection.q115", #2
                            "mid_maize.owner.agree.inspection.q116", #3
                            "mid_maize.owner.agree.inspection.q117", #4
                            "mid_maize.owner.agree.inspection.q118", #5
                            "mid_maize.owner.agree.inspection.q119", #6
                            "index_overall_off_mid")                 #7
results_dealer_sec_off_base <- c("maize.owner.agree.inspection.q114",
                                 "maize.owner.agree.inspection.q115",
                                 "maize.owner.agree.inspection.q116",
                                 "maize.owner.agree.inspection.q117",
                                 "maize.owner.agree.inspection.q118",
                                 "maize.owner.agree.inspection.q119",
                                 "index_overall_off_base")

baseline_dealers[results_dealer_sec_off_base] <- lapply(baseline_dealers[results_dealer_sec_off_base],function(x)x - mean(x,na.rm = T))

df_means_end_D_sec_off <- array(NA,dim=c(3,11))

for (i in 1:length(results_dealer_sec_off)){
  df_means_end_D_sec_off[1,i] <- sum(baseline_dealers[results_dealer_sec_off[i]], na.rm=T)/(nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_sec_off[i]])))
  df_means_end_D_sec_off[2,i] <- sqrt(var(baseline_dealers[results_dealer_sec_off[i]], na.rm=T))
  df_means_end_D_sec_off[3,i] <- nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_sec_off[i]]))-sum(is.na(baseline_dealers[results_dealer_sec_off_base[i]]))+sum(is.na(baseline_dealers[results_dealer_sec_off[i]])&is.na(baseline_dealers[results_dealer_sec_off_base[i]]))}

###
#2#
###

baseline_dealers$training_control[baseline_dealers$training==0] <- TRUE
baseline_dealers$training_control[baseline_dealers$training==1] <- FALSE

#8.
index_overall_off_mid <- icwIndex(xmat=variables_overall_off_mid,sgroup = baseline_dealers$training_control,revcols = c(4,5))
baseline_dealers$index_overall_off_midT <- index_overall_off_mid$index

index_overall_off_base <- icwIndex(xmat=variables_overall_off_base,sgroup = baseline_dealers$training_control,revcols = c(4,5))
baseline_dealers$index_overall_off_baseT <- index_overall_off_base$index

df_ols_end_D_sec_off <- array(NA,dim=c(3,3,11))

results_dealer_sec_off <- c("mid_maize.owner.agree.inspection.q114"
                            ,"mid_maize.owner.agree.inspection.q115"
                            ,"mid_maize.owner.agree.inspection.q116"
                            ,"mid_maize.owner.agree.inspection.q117"
                            ,"mid_maize.owner.agree.inspection.q118"
                            ,"mid_maize.owner.agree.inspection.q119"
                            ,"index_overall_off_midT")
results_dealer_sec_off_base <- c("maize.owner.agree.inspection.q114"
                                 ,"maize.owner.agree.inspection.q115"
                                 ,"maize.owner.agree.inspection.q116"
                                 ,"maize.owner.agree.inspection.q117"
                                 ,"maize.owner.agree.inspection.q118"
                                 ,"maize.owner.agree.inspection.q119"
                                 ,"index_overall_off_baseT")

baseline_dealers[results_dealer_sec_off_base] <- lapply(baseline_dealers[results_dealer_sec_off_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_dealer_sec_off)){
  ols <- lm(as.formula(paste(paste(results_dealer_sec_off[i],"training*clearing_demeaned*farmer_demeaned",sep="~"),results_dealer_sec_off_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_sec_off[i],"training*clearing_demeaned*farmer_demeaned",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")

  df_ols_end_D_sec_off[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_ols_end_D_sec_off[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_ols_end_D_sec_off[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]}

###
#3#
###

baseline_dealers$clearing_control[baseline_dealers$clearing==0] <- TRUE
baseline_dealers$clearing_control[baseline_dealers$clearing==1] <- FALSE

#8.
index_overall_off_mid <- icwIndex(xmat=variables_overall_off_mid,sgroup = baseline_dealers$clearing_control,revcols = c(4,5))
baseline_dealers$index_overall_off_midC <- index_overall_off_mid$index

index_overall_off_base <- icwIndex(xmat=variables_overall_off_base,sgroup = baseline_dealers$clearing_control,revcols = c(4,5))
baseline_dealers$index_overall_off_baseC <- index_overall_off_base$index

results_dealer_sec_off <- c("mid_maize.owner.agree.inspection.q114"
                            ,"mid_maize.owner.agree.inspection.q115"
                            ,"mid_maize.owner.agree.inspection.q116"
                            ,"mid_maize.owner.agree.inspection.q117"
                            ,"mid_maize.owner.agree.inspection.q118"
                            ,"mid_maize.owner.agree.inspection.q119"
                            ,"index_overall_off_midC")
results_dealer_sec_off_base <- c("maize.owner.agree.inspection.q114"
                                 ,"maize.owner.agree.inspection.q115"
                                 ,"maize.owner.agree.inspection.q116"
                                 ,"maize.owner.agree.inspection.q117"
                                 ,"maize.owner.agree.inspection.q118"
                                 ,"maize.owner.agree.inspection.q119"
                                 ,"index_overall_off_baseC")

baseline_dealers[results_dealer_sec_off_base] <- lapply(baseline_dealers[results_dealer_sec_off_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_dealer_sec_off)){
  ols <- lm(as.formula(paste(paste(results_dealer_sec_off[i],"training_demeaned*clearing*farmer_demeaned",sep="~"),results_dealer_sec_off_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_sec_off[i],"training_demeaned*clearing*farmer_demeaned",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")

  df_ols_end_D_sec_off[1,2,i] <- coef_test(ols, vcov_cluster)$beta[3]
  df_ols_end_D_sec_off[2,2,i] <- coef_test(ols, vcov_cluster)$SE[3]
  df_ols_end_D_sec_off[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]}

###
#4#
###

baseline_dealers$farmer_control[baseline_dealers$farmer==0] <- TRUE
baseline_dealers$farmer_control[baseline_dealers$farmer==1] <- FALSE

#8.
index_overall_off_mid <- icwIndex(xmat=variables_overall_off_mid,sgroup = baseline_dealers$farmer_control,revcols = c(4,5))
baseline_dealers$index_overall_off_midF <- index_overall_off_mid$index

index_overall_off_base <- icwIndex(xmat=variables_overall_off_base,sgroup = baseline_dealers$farmer_control,revcols = c(4,5))
baseline_dealers$index_overall_off_baseF <- index_overall_off_base$index

results_dealer_sec_off <- c("mid_maize.owner.agree.inspection.q114"
                            ,"mid_maize.owner.agree.inspection.q115"
                            ,"mid_maize.owner.agree.inspection.q116"
                            ,"mid_maize.owner.agree.inspection.q117"
                            ,"mid_maize.owner.agree.inspection.q118"
                            ,"mid_maize.owner.agree.inspection.q119"
                            ,"index_overall_off_midF")
results_dealer_sec_off_base <- c("maize.owner.agree.inspection.q114"
                                 ,"maize.owner.agree.inspection.q115"
                                 ,"maize.owner.agree.inspection.q116"
                                 ,"maize.owner.agree.inspection.q117"
                                 ,"maize.owner.agree.inspection.q118"
                                 ,"maize.owner.agree.inspection.q119"
                                 ,"index_overall_off_baseF")

baseline_dealers[results_dealer_sec_off_base] <- lapply(baseline_dealers[results_dealer_sec_off_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_dealer_sec_off)){
  ols <- lm(as.formula(paste(paste(results_dealer_sec_off[i],"training_demeaned*clearing_demeaned*farmer",sep="~"),results_dealer_sec_off_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_sec_off[i],"training_demeaned*clearing_demeaned*farmer",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")

  #farmer video treatment at village/shop level so no clustering needed
  df_ols_end_D_sec_off[1,3,i] <- summary(ols)$coefficients[4,1]
  df_ols_end_D_sec_off[2,3,i] <- summary(ols)$coefficients[4,2]
  df_ols_end_D_sec_off[3,3,i] <- summary(ols)$coefficients[4,4]}

#Aker, Boumnijel, McClelland, Tierney (2012)
df_dealer_sec_offT <- data.frame(baseline_dealers$mid_maize.owner.agree.inspection.q114,baseline_dealers$mid_maize.owner.agree.inspection.q115
                                 ,baseline_dealers$mid_maize.owner.agree.inspection.q116,baseline_dealers$mid_maize.owner.agree.inspection.q117
                                 ,baseline_dealers$mid_maize.owner.agree.inspection.q118,baseline_dealers$mid_maize.owner.agree.inspection.q119)
df_dealer_sec_offC <- df_dealer_sec_offT
df_dealer_sec_offF <- df_dealer_sec_offT
#no overall index

df_ols_end_D_sec_off_J <- array(NA,dim=c(3,3,11))

results_dealer_sec_off_J <- c("mid_maize.owner.agree.inspection.q114","mid_maize.owner.agree.inspection.q115"
                            ,"mid_maize.owner.agree.inspection.q116","mid_maize.owner.agree.inspection.q117"
                            ,"mid_maize.owner.agree.inspection.q118","mid_maize.owner.agree.inspection.q119")
#no overall index

for (i in 1:length(results_dealer_sec_off_J)){
  df_ols_end_D_sec_off_J[3,1,i] <- adjust_p(df_ols_end_D_sec_off[3,1,i],df_dealer_sec_offT,i)
  df_ols_end_D_sec_off_J[3,2,i] <- adjust_p(df_ols_end_D_sec_off[3,2,i],df_dealer_sec_offC,i)
  df_ols_end_D_sec_off_J[3,3,i] <- adjust_p(df_ols_end_D_sec_off[3,3,i],df_dealer_sec_offF,i)}










###################################################################################################################################################################
##### 7 ANALYSIS: Agro-input dealer - Secondary: 11. seed bag #####################################################################################################
###################################################################################################################################################################

baseline_dealers_save=baseline_dealers
baseline_dealers=subset(baseline_dealers,!is.na(baseline_dealers$mid_reading_unadj))

#midline names:
#"age_mid" "exp_mid" "lot_mid" "cert_mid" "date_mid" "verif_mid" "origin_mid" "company_mid" "reading" "variety_mid" "date_pack_mid" "other_var_mid"

#1. Random seed bag shows expiry date
baseline_dealers$mid_exp <- baseline_dealers$exp_end #x
baseline_dealers$mid_visible_expdate<-ifelse(!is.na(baseline_dealers$mid_exp),1,0) #x

# sum(is.na(midline_dealers$exp) & !is.na(midline_dealers$reading))
# [1] 0
# meaning that if a seed bag was purchased, there was always an expiry date.

#2. Random seed bag shows packaging date
baseline_dealers$mid_date_pack <- baseline_dealers$date_pack_end #x
baseline_dealers$mid_visible_packdate<-ifelse(baseline_dealers$mid_date_pack=="n/a",0,1) #x

#3. Days since packaging date/expiry date minus 6 months
baseline_dealers$mid_date <- baseline_dealers$date_end #x
baseline_dealers$mid_date[baseline_dealers$mid_date=="n/a"] <- NA #x
baseline_dealers$mid_date <- as.Date(baseline_dealers$mid_date) #x

baseline_dealers$mid_exp <- as.Date(baseline_dealers$mid_exp) #x
baseline_dealers$mid_days_since_exp <- baseline_dealers$mid_date - baseline_dealers$mid_exp #x

baseline_dealers$mid_date_pack <- as.Date(baseline_dealers$mid_date_pack) #x
baseline_dealers$mid_shelflife <- baseline_dealers$mid_date - baseline_dealers$mid_date_pack #x

baseline_dealers$mid_date_pack_incltransformedexp <- baseline_dealers$mid_date_pack #x
baseline_dealers$mid_transformedexp <- baseline_dealers$mid_exp - 183 #6x366/12 #x
baseline_dealers$mid_date_pack_incltransformedexp[is.na(baseline_dealers$mid_date_pack)]<-baseline_dealers$mid_transformedexp[is.na(baseline_dealers$mid_date_pack)] #x

baseline_dealers$mid_shelflife_Caro <- baseline_dealers$mid_date - as.Date(baseline_dealers$mid_date_pack_incltransformedexp) #x
baseline_dealers$mid_shelflife_Caro[baseline_dealers$mid_shelflife_Caro < 0] <- NA #x
baseline_dealers$mid_shelflife_Caro <- as.numeric(as.character(baseline_dealers$mid_shelflife_Caro)) #x

baseline_dealers <- trim("mid_shelflife_Caro",baseline_dealers,trim_perc=.02) #x
baseline_dealers <- trim("shelflife_Caro",baseline_dealers,trim_perc=.02)

#Q4. Is the seed in the original bag without any signs of damage?
baseline_dealers$mid_origin <- baseline_dealers$origin_end
baseline_dealers$mid_origin<-ifelse(baseline_dealers$mid_origin=="Yes",1,0)

#4. Random seed bag shows lot number
baseline_dealers$mid_lot <- baseline_dealers$lot_end
baseline_dealers$mid_lot<-ifelse(baseline_dealers$mid_lot=="Yes",1,0)

#5. Overall index
variables_overall_bag_mid <- cbind(baseline_dealers$mid_reading,baseline_dealers$mid_visible_packdate,baseline_dealers$mid_shelflife_Caro,baseline_dealers$mid_origin,baseline_dealers$mid_lot) #x
variables_overall_bag_base <- cbind(baseline_dealers$reading,baseline_dealers$visible_packdate,baseline_dealers$shelflife_Caro,baseline_dealers$origin,baseline_dealers$lot)

index_overall_bag_mid <- icwIndex(xmat=variables_overall_bag_mid,revcols = c(1,3)) #x
baseline_dealers$index_overall_bag_mid <- index_overall_bag_mid$index #x

index_overall_bag_base <- icwIndex(xmat=variables_overall_bag_base,revcols = c(1,3))
baseline_dealers$index_overall_bag_base <- index_overall_bag_base$index

################################################################################################################################################################################

###
#1#
###

results_dealer_sec_bag <- c("mid_reading"
                            ,"mid_visible_packdate"
                            ,"mid_shelflife_Caro"
                            ,"mid_origin"
                            ,"mid_lot"
                            ,"index_overall_bag_mid")
results_dealer_sec_bag_base <- c("reading"
                                 ,"visible_packdate"
                                 ,"shelflife_Caro"
                                 ,"origin"
                                 ,"lot"
                                 ,"index_overall_bag_base")

baseline_dealers[results_dealer_sec_bag_base] <- lapply(baseline_dealers[results_dealer_sec_bag_base],function(x)x - mean(x,na.rm = T))

df_means_end_D_sec_bag <- array(NA,dim=c(3,11))

# for (i in 1:length(results_dealer_sec_bag)){
#   df_means_end_D_sec_bag[1,i] <- sum(baseline_dealers[results_dealer_sec_bag[i]], na.rm=T)/(nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_sec_bag[i]])))
#   df_means_end_D_sec_bag[2,i] <- sqrt(var(baseline_dealers[results_dealer_sec_bag[i]], na.rm=T))
#   df_means_end_D_sec_bag[3,i] <- nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_sec_bag[i]]))-sum(is.na(baseline_dealers[results_dealer_sec_bag_base[i]]))+sum(is.na(baseline_dealers[results_dealer_sec_bag[i]])&is.na(baseline_dealers[results_dealer_sec_bag_base[i]]))}

for (i in 1:length(results_dealer_sec_bag)){
  df_means_end_D_sec_bag[1,i] <- sum(baseline_dealers[results_dealer_sec_bag[i]], na.rm=T)/(nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_sec_bag[i]])))
  df_means_end_D_sec_bag[2,i] <- sqrt(var(baseline_dealers[results_dealer_sec_bag[i]], na.rm=T))
  df_means_end_D_sec_bag[3,i] <- nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_sec_bag[i]]))}


###
#2#
###

baseline_dealers$training_control[baseline_dealers$training==0] <- TRUE
baseline_dealers$training_control[baseline_dealers$training==1] <- FALSE

#5.
index_overall_bag_mid <- icwIndex(xmat=variables_overall_bag_mid,sgroup = baseline_dealers$training_control,revcols = c(1,3))
baseline_dealers$index_overall_bag_midT <- index_overall_bag_mid$index

index_overall_bag_base <- icwIndex(xmat=variables_overall_bag_base,sgroup = baseline_dealers$training_control,revcols = c(1,3))
baseline_dealers$index_overall_bag_baseT <- index_overall_bag_base$index

df_ols_end_D_sec_bag <- array(NA,dim=c(3,3,11))

results_dealer_sec_bag <- c("mid_reading"
                            ,"mid_visible_packdate"
                            ,"mid_shelflife_Caro"
                            ,"mid_origin"
                            ,"mid_lot"
                            ,"index_overall_bag_midT")
results_dealer_sec_bag_base <- c("reading"
                                 ,"visible_packdate"
                                 ,"shelflife_Caro"
                                 ,"origin"
                                 ,"lot"
                                 ,"index_overall_bag_baseT")

baseline_dealers[results_dealer_sec_bag_base] <- lapply(baseline_dealers[results_dealer_sec_bag_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_dealer_sec_bag)){
  #ols <- lm(as.formula(paste(paste(results_dealer_sec_bag[i],"training*clearing_demeaned*farmer_demeaned",sep="~"),results_dealer_sec_bag_base[i],sep="+")),data=baseline_dealers)
  ols <- lm(as.formula(paste(results_dealer_sec_bag[i],"training*clearing_demeaned*farmer_demeaned",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")

  df_ols_end_D_sec_bag[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_ols_end_D_sec_bag[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_ols_end_D_sec_bag[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]}

###
#3#
###

baseline_dealers$clearing_control[baseline_dealers$clearing==0] <- TRUE
baseline_dealers$clearing_control[baseline_dealers$clearing==1] <- FALSE

#5.
index_overall_bag_mid <- icwIndex(xmat=variables_overall_bag_mid,sgroup = baseline_dealers$clearing_control,revcols = c(1,3))
baseline_dealers$index_overall_bag_midC <- index_overall_bag_mid$index

index_overall_bag_base <- icwIndex(xmat=variables_overall_bag_base,sgroup = baseline_dealers$clearing_control,revcols = c(1,3))
baseline_dealers$index_overall_bag_baseC <- index_overall_bag_base$index

results_dealer_sec_bag <- c("mid_reading"
                            ,"mid_visible_packdate"
                            ,"mid_shelflife_Caro"
                            ,"mid_origin"
                            ,"mid_lot"
                            ,"index_overall_bag_midC")
results_dealer_sec_bag_base <- c("reading"
                                 ,"visible_packdate"
                                 ,"shelflife_Caro"
                                 ,"origin"
                                 ,"lot"
                                 ,"index_overall_bag_baseC")

baseline_dealers[results_dealer_sec_bag_base] <- lapply(baseline_dealers[results_dealer_sec_bag_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_dealer_sec_bag)){
  #ols <- lm(as.formula(paste(paste(results_dealer_sec_bag[i],"training_demeaned*clearing*farmer_demeaned",sep="~"),results_dealer_sec_bag_base[i],sep="+")),data=baseline_dealers)
  ols <- lm(as.formula(paste(results_dealer_sec_bag[i],"training_demeaned*clearing*farmer_demeaned",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")

  df_ols_end_D_sec_bag[1,2,i] <- coef_test(ols, vcov_cluster)$beta[3]
  df_ols_end_D_sec_bag[2,2,i] <- coef_test(ols, vcov_cluster)$SE[3]
  df_ols_end_D_sec_bag[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]}

###
#4#
###

baseline_dealers$farmer_control[baseline_dealers$farmer==0] <- TRUE
baseline_dealers$farmer_control[baseline_dealers$farmer==1] <- FALSE

#5.
index_overall_bag_mid <- icwIndex(xmat=variables_overall_bag_mid,sgroup = baseline_dealers$farmer_control,revcols = c(1,3))
baseline_dealers$index_overall_bag_midF <- index_overall_bag_mid$index

index_overall_bag_base <- icwIndex(xmat=variables_overall_bag_base,sgroup = baseline_dealers$farmer_control,revcols = c(1,3))
baseline_dealers$index_overall_bag_baseF <- index_overall_bag_base$index

results_dealer_sec_bag <- c("mid_reading"
                            ,"mid_visible_packdate"
                            ,"mid_shelflife_Caro"
                            ,"mid_origin"
                            ,"mid_lot"
                            ,"index_overall_bag_midF")
results_dealer_sec_bag_base <- c("reading"
                                 ,"visible_packdate"
                                 ,"shelflife_Caro"
                                 ,"origin"
                                 ,"lot"
                                 ,"index_overall_bag_baseF")

baseline_dealers[results_dealer_sec_bag_base] <- lapply(baseline_dealers[results_dealer_sec_bag_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_dealer_sec_bag)){
  #ols <- lm(as.formula(paste(paste(results_dealer_sec_bag[i],"training_demeaned*clearing_demeaned*farmer",sep="~"),results_dealer_sec_bag_base[i],sep="+")),data=baseline_dealers)
  ols <- lm(as.formula(paste(results_dealer_sec_bag[i],"training_demeaned*clearing_demeaned*farmer",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")

  #farmer video treatment at village/shop level so no clustering needed
  df_ols_end_D_sec_bag[1,3,i] <- summary(ols)$coefficients[4,1]
  df_ols_end_D_sec_bag[2,3,i] <- summary(ols)$coefficients[4,2]
  df_ols_end_D_sec_bag[3,3,i] <- summary(ols)$coefficients[4,4]}

#Aker, Boumnijel, McClelland, Tierney (2012)
df_dealer_sec_bagT <- data.frame(baseline_dealers$mid_reading,baseline_dealers$mid_visible_packdate,baseline_dealers$mid_shelflife_Caro,baseline_dealers$mid_origin,baseline_dealers$mid_lot)
df_dealer_sec_bagC <- df_dealer_sec_bagT
df_dealer_sec_bagF <- df_dealer_sec_bagT
#no overall index

df_ols_end_D_sec_bag_J <- array(NA,dim=c(3,3,11))

results_dealer_sec_bag_J <- c("mid_reading","mid_visible_packdate","mid_shelflife_Caro","mid_origin","mid_lot")
#no overall index

for (i in 1:length(results_dealer_sec_bag_J)){
  df_ols_end_D_sec_bag_J[3,1,i] <- adjust_p(df_ols_end_D_sec_bag[3,1,i],df_dealer_sec_bagT,i)
  df_ols_end_D_sec_bag_J[3,2,i] <- adjust_p(df_ols_end_D_sec_bag[3,2,i],df_dealer_sec_bagC,i)
  df_ols_end_D_sec_bag_J[3,3,i] <- adjust_p(df_ols_end_D_sec_bag[3,3,i],df_dealer_sec_bagF,i)}










###################################################################################################################################################################
##### 7B ANALYSIS: Agro-input dealer - Secondary: 11. seed bag, not controlling for baseline ######################################################################
###################################################################################################################################################################

###
#1#
###

results_dealer_sec_bag_B <- c("index_overall_bag_mid")

df_means_end_D_sec_bag_B <- array(NA,dim=c(3,11))

for (i in 1:length(results_dealer_sec_bag_B)){
  df_means_end_D_sec_bag_B[1,i] <- sum(baseline_dealers[results_dealer_sec_bag_B[i]], na.rm=T)/(nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_sec_bag_B[i]])))
  df_means_end_D_sec_bag_B[2,i] <- sqrt(var(baseline_dealers[results_dealer_sec_bag_B[i]], na.rm=T))
  df_means_end_D_sec_bag_B[3,i] <- nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_sec_bag_B[i]]))}

###
#2#
###

df_ols_end_D_sec_bag_B <- array(NA,dim=c(3,3,11))

results_dealer_sec_bag_B <- c("index_overall_bag_midT")

for (i in 1:length(results_dealer_sec_bag_B)){
  #ols <- lm(as.formula(paste(paste(results_dealer_sec_bag_B[i],"training*clearing_demeaned*farmer_demeaned",sep="~"),results_dealer_sec_bag_B_base[i],sep="+")),data=baseline_dealers)
  ols <- lm(as.formula(paste(results_dealer_sec_bag_B[i],"training*clearing_demeaned*farmer_demeaned",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")

  df_ols_end_D_sec_bag_B[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_ols_end_D_sec_bag_B[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_ols_end_D_sec_bag_B[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]}

###
#3#
###

results_dealer_sec_bag_B <- c("index_overall_bag_midC")

for (i in 1:length(results_dealer_sec_bag_B)){
  #ols <- lm(as.formula(paste(paste(results_dealer_sec_bag_B[i],"training_demeaned*clearing*farmer_demeaned",sep="~"),results_dealer_sec_bag_B_base[i],sep="+")),data=baseline_dealers)
  ols <- lm(as.formula(paste(results_dealer_sec_bag_B[i],"training_demeaned*clearing*farmer_demeaned",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")

  df_ols_end_D_sec_bag_B[1,2,i] <- coef_test(ols, vcov_cluster)$beta[3]
  df_ols_end_D_sec_bag_B[2,2,i] <- coef_test(ols, vcov_cluster)$SE[3]
  df_ols_end_D_sec_bag_B[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]}

###
#4#
###

results_dealer_sec_bag_B <- c("index_overall_bag_midF")

for (i in 1:length(results_dealer_sec_bag_B)){
  #ols <- lm(as.formula(paste(paste(results_dealer_sec_bag_B[i],"training_demeaned*clearing_demeaned*farmer",sep="~"),results_dealer_sec_bag_B_base[i],sep="+")),data=baseline_dealers)
  ols <- lm(as.formula(paste(results_dealer_sec_bag_B[i],"training_demeaned*clearing_demeaned*farmer",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")

  #farmer video treatment at village/shop level so no clustering needed
  df_ols_end_D_sec_bag_B[1,3,i] <- summary(ols)$coefficients[4,1]
  df_ols_end_D_sec_bag_B[2,3,i] <- summary(ols)$coefficients[4,2]
  df_ols_end_D_sec_bag_B[3,3,i] <- summary(ols)$coefficients[4,4]}

baseline_dealers=baseline_dealers_save










################################################################################################################################################################################
##### 2 ANALYSIS: Agro-input dealer - Primary and secondary: outcomes without baseline######################################################################################################
################################################################################################################################################################################

#1. Index of dealer's motivation and satisfaction OK!
#Do you see yourself working as an agro-input dealer 3 years from now? (q9a): yes=good
baseline_dealers$mid_maize.owner.agree.q9_a <- baseline_dealers$check.owner.agree.client.q9a #x
baseline_dealers$mid_maize.owner.agree.q9_a[baseline_dealers$mid_maize.owner.agree.q9_a==98] <- NA
baseline_dealers$mid_maize.owner.agree.q9_a[baseline_dealers$mid_maize.owner.agree.q9_a=="n/a"] <- NA
baseline_dealers$mid_maize.owner.agree.q9_a<-ifelse(baseline_dealers$mid_maize.owner.agree.q9_a=="Yes",1,0) #x

# #Do you think your job makes a positive difference in other's life? (q9b): yes=good
# baseline_dealers$mid_maize.owner.agree.q9.b <- baseline_dealers$owner.agree.client.q9b #x
# baseline_dealers$mid_maize.owner.agree.q9.b<-ifelse(baseline_dealers$mid_maize.owner.agree.q9.b=="Yes",1,0) #x
#
# table(baseline_dealers$mid_maize.owner.agree.q9.b)
# 0   1
# 4 302

#On a scale of 1 to 5, how likely are you to recommend working as an agro-input dealer to friends or family? (q9c) more=better
baseline_dealers$mid_maize.owner.agree.q9_c <- baseline_dealers$check.owner.agree.client.q9c #x
baseline_dealers$mid_maize.owner.agree.q9_c[baseline_dealers$mid_maize.owner.agree.q9_c=="n/a"] <- NA

#On a scale of 1 to 5, how happy do you feel when you come to work in the morning? (q9d) more=better
baseline_dealers$mid_maize.owner.agree.q9_d <- baseline_dealers$check.owner.agree.client.q9d #x
baseline_dealers$mid_maize.owner.agree.q9_d[baseline_dealers$mid_maize.owner.agree.q9_d=="n/a"] <- NA

baseline_dealers$mid_maize.owner.agree.q9_c <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.q9_c))
baseline_dealers$mid_maize.owner.agree.q9_d <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.q9_d))

variables_motivation_mid <- cbind(baseline_dealers$mid_maize.owner.agree.q9_a
                                  ,baseline_dealers$mid_maize.owner.agree.q9_c,baseline_dealers$mid_maize.owner.agree.q9_d) #x

index_motivation_mid <- icwIndex(xmat=variables_motivation_mid) #x
baseline_dealers$index_motivation_mid <- index_motivation_mid$index #x

#2. Index of shop's maize seed ratings by farmers OK!
baseline_dealers$mid_general<-baseline_dealers$end_seed_quality_general_rating
baseline_dealers$mid_yield<-baseline_dealers$end_seed_yield_rating
baseline_dealers$mid_drought_resistent<-baseline_dealers$end_seed_drought_rating
baseline_dealers$mid_disease_resistent<-baseline_dealers$end_seed_disease_rating
baseline_dealers$mid_early_maturing<-baseline_dealers$end_seed_maturing_rating
baseline_dealers$mid_germination<-baseline_dealers$end_seed_germinate_rating

variables_ratings_mid <- cbind(baseline_dealers$mid_general,baseline_dealers$mid_yield,baseline_dealers$mid_drought_resistent
                               ,baseline_dealers$mid_disease_resistent,baseline_dealers$mid_early_maturing,baseline_dealers$mid_germination)

index_ratings_mid <- icwIndex(xmat=variables_ratings_mid)
baseline_dealers$index_ratings_mid <- index_ratings_mid$index

#3. new index_overall_prim_dealer_mid incl. ratings OK!
variables_overall_prim_dealer_mid <- cbind(baseline_dealers$mid_quantitysold,baseline_dealers$mid_revenue
                                           ,baseline_dealers$mid_maize.owner.agree.q7,baseline_dealers$mid_reading
                                           ,baseline_dealers$index_practices_cap_mid,baseline_dealers$index_practices_lab_mid
                                           ,baseline_dealers$index_ratings_mid,baseline_dealers$index_efforts_mid)

index_overall_prim_dealer_mid <- icwIndex(xmat=variables_overall_prim_dealer_mid, revcols = c(4)) #x
baseline_dealers$index_overall_prim_dealer_mid <- index_overall_prim_dealer_mid$index #x

################################################################################################################################################################################

###
#1#
###

results_dealer_sec_nobase <- c("index_motivation_mid",          #1
                               "index_ratings_mid",             #2
                               "index_overall_prim_dealer_mid", #3
                               "index_overallsec_mid",          #4
                               "index_overall_off_mid",         #5
                               "mid_reading")                   #6

df_means_end_D_sec_nobase <- array(NA,dim=c(5,10))

for (i in 1:length(results_dealer_sec_nobase)){
  df_means_end_D_sec_nobase[1,i] <- sum(baseline_dealers[results_dealer_sec_nobase[i]], na.rm=T)/(nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_sec_nobase[i]])))
  df_means_end_D_sec_nobase[2,i] <- sqrt(var(baseline_dealers[results_dealer_sec_nobase[i]], na.rm=T))
  df_means_end_D_sec_nobase[3,i] <- nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_sec_nobase[i]]))
  df_means_end_D_sec_nobase[4,i] <- min(baseline_dealers[results_dealer_sec_nobase[i]], na.rm=T)
  df_means_end_D_sec_nobase[5,i] <- max(baseline_dealers[results_dealer_sec_nobase[i]], na.rm=T)}

###
#2#
###

df_ols_end_D_sec_nobase <- array(NA,dim=c(3,3,10))

baseline_dealers$training_control[baseline_dealers$training==0] <- TRUE
baseline_dealers$training_control[baseline_dealers$training==1] <- FALSE

#1.
index_motivation_mid <- icwIndex(xmat=variables_motivation_mid,sgroup = baseline_dealers$training_control)
baseline_dealers$index_motivation_midT <- index_motivation_mid$index

#2.
index_ratings_mid <- icwIndex(xmat=variables_ratings_mid,sgroup = baseline_dealers$training_control)
baseline_dealers$index_ratings_midT <- index_ratings_mid$index

#3.
index_overall_prim_dealer_mid <- icwIndex(xmat=variables_overall_prim_dealer_mid,sgroup = baseline_dealers$training_control, revcols = c(4))
baseline_dealers$index_overall_prim_dealer_midT <- index_overall_prim_dealer_mid$index

results_dealer_sec_nobase <- c("index_motivation_midT"
                               ,"index_ratings_midT"
                               ,"index_overall_prim_dealer_midT"
                               ,"index_overallsec_midT"
                               ,"index_overall_off_midT"
                               ,"mid_reading")

for (i in 1:length(results_dealer_sec_nobase)){
  ols <- lm(as.formula(paste(results_dealer_sec_nobase[i],"training*clearing_demeaned*farmer_demeaned",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")

  df_ols_end_D_sec_nobase[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_ols_end_D_sec_nobase[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_ols_end_D_sec_nobase[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]}

###
#3#
###

baseline_dealers$clearing_control[baseline_dealers$clearing==0] <- TRUE
baseline_dealers$clearing_control[baseline_dealers$clearing==1] <- FALSE

#1.
index_motivation_mid <- icwIndex(xmat=variables_motivation_mid,sgroup = baseline_dealers$clearing_control)
baseline_dealers$index_motivation_midC <- index_motivation_mid$index

#2.
index_ratings_mid <- icwIndex(xmat=variables_ratings_mid,sgroup = baseline_dealers$clearing_control)
baseline_dealers$index_ratings_midC <- index_ratings_mid$index

#3.
index_overall_prim_dealer_mid <- icwIndex(xmat=variables_overall_prim_dealer_mid,sgroup = baseline_dealers$clearing_control, revcols = c(4))
baseline_dealers$index_overall_prim_dealer_midC <- index_overall_prim_dealer_mid$index

results_dealer_sec_nobase <- c("index_motivation_midC"
                               ,"index_ratings_midC"
                               ,"index_overall_prim_dealer_midC"
                               ,"index_overallsec_midC"
                               ,"index_overall_off_midC"
                               ,"mid_reading")

for (i in 1:length(results_dealer_sec_nobase)){
  ols <- lm(as.formula(paste(results_dealer_sec_nobase[i],"training_demeaned*clearing*farmer_demeaned",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")

  df_ols_end_D_sec_nobase[1,2,i] <- coef_test(ols, vcov_cluster)$beta[3]
  df_ols_end_D_sec_nobase[2,2,i] <- coef_test(ols, vcov_cluster)$SE[3]
  df_ols_end_D_sec_nobase[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]}

###
#4#
###

baseline_dealers$farmer_control[baseline_dealers$farmer==0] <- TRUE
baseline_dealers$farmer_control[baseline_dealers$farmer==1] <- FALSE

#1.
index_motivation_mid <- icwIndex(xmat=variables_motivation_mid,sgroup = baseline_dealers$farmer_control)
baseline_dealers$index_motivation_midF <- index_motivation_mid$index

#2.
index_ratings_mid <- icwIndex(xmat=variables_ratings_mid,sgroup = baseline_dealers$farmer_control)
baseline_dealers$index_ratings_midF <- index_ratings_mid$index

#3.
index_overall_prim_dealer_mid <- icwIndex(xmat=variables_overall_prim_dealer_mid,sgroup = baseline_dealers$farmer_control, revcols = c(4))
baseline_dealers$index_overall_prim_dealer_midF <- index_overall_prim_dealer_mid$index

results_dealer_sec_nobase <- c("index_motivation_midF"
                               ,"index_ratings_midF"
                               ,"index_overall_prim_dealer_midF"
                               ,"index_overallsec_midF"
                               ,"index_overall_off_midF"
                               ,"mid_reading")

for (i in 1:length(results_dealer_sec_nobase)){
  ols <- lm(as.formula(paste(results_dealer_sec_nobase[i],"training_demeaned*clearing_demeaned*farmer",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")

  #farmer video treatment at village/shop level so no clustering needed
  df_ols_end_D_sec_nobase[1,3,i] <- summary(ols)$coefficients[4,1]
  df_ols_end_D_sec_nobase[2,3,i] <- summary(ols)$coefficients[4,2]
  df_ols_end_D_sec_nobase[3,3,i] <- summary(ols)$coefficients[4,4]}

#OK!










################################################################################################################################################################################
##### 8 ANALYSIS: Farmer - Primary##############################################################################################################################################
################################################################################################################################################################################

#Heterogeneity analyses
#2: Larger catchment areas
baseline_farmers$small_catchID <- ifelse(baseline_farmers$catchID==16|baseline_farmers$catchID==18|baseline_farmers$catchID==19|
                                           baseline_farmers$catchID==33|baseline_farmers$catchID==34|baseline_farmers$catchID==36|
                                           baseline_farmers$catchID==42|baseline_farmers$catchID==45|baseline_farmers$catchID==46|
                                           baseline_farmers$catchID==48|baseline_farmers$catchID==53|baseline_farmers$catchID==63|
                                           baseline_farmers$catchID==65|baseline_farmers$catchID==66|baseline_farmers$catchID==67|
                                           baseline_farmers$catchID==73|baseline_farmers$catchID==79|baseline_farmers$catchID==80|
                                           baseline_farmers$catchID==87|baseline_farmers$catchID==89|baseline_farmers$catchID==90|
                                           baseline_farmers$catchID==91|baseline_farmers$catchID==92|baseline_farmers$catchID==93|
                                           baseline_farmers$catchID==95|baseline_farmers$catchID==98|baseline_farmers$catchID==101|
                                           baseline_farmers$catchID==103|baseline_farmers$catchID==106|baseline_farmers$catchID==107|
                                           baseline_farmers$catchID==108|baseline_farmers$catchID==109|baseline_farmers$catchID==110|
                                           baseline_farmers$catchID==112|baseline_farmers$catchID==116|baseline_farmers$catchID==118|
                                           baseline_farmers$catchID==120|baseline_farmers$catchID==121|baseline_farmers$catchID==122|
                                           baseline_farmers$catchID==124|baseline_farmers$catchID==125|baseline_farmers$catchID==126|
                                           baseline_farmers$catchID==127|baseline_farmers$catchID==128|baseline_farmers$catchID==129|
                                           baseline_farmers$catchID==130,1,0)
#baseline_farmers=subset(baseline_farmers,small_catchID=="0")

#3: Less competitive catchment areas
baseline_farmers$large_catchID <- ifelse(baseline_farmers$catchID==3|baseline_farmers$catchID==32|baseline_farmers$catchID==59,1,0)
# |baseline_farmers$catchID==64
#baseline_farmers=subset(baseline_farmers,large_catchID=="0")




baseline_farmers[baseline_farmers==999] <- NA
baseline_farmers[, 4:372][baseline_farmers[, 4:372] == 96] <- NA #columns 4-372 only
baseline_farmers[, 4:372][baseline_farmers[, 4:372] == 98] <- NA
baseline_farmers[baseline_farmers=="n/a"] <- NA



#1. Q25a. Did you use any quality maize seed like **OPV or hybrid in **seed  the second season of **2020 (entoigo 2020)** for any of your plots?
baseline_farmers$mid_Check2.check.maize.q25a <- baseline_farmers$CHECK.MAIZE.Q25A
baseline_farmers$mid_Check2.check.maize.q25a<-ifelse(baseline_farmers$mid_Check2.check.maize.q25a=="Yes",1,0)

#2. q25b. Where did you obtain the maize seed used in the second season of **2020 (entoigo 2020)** on any of your plots?
baseline_farmers$agro <- ifelse(baseline_farmers$Check2.check.maize.q25b=="d",1,0)
baseline_farmers$agro[is.na(baseline_farmers$Check2.check.maize.q25b)] <- NA
baseline_farmers$agro[baseline_farmers$Check2.check.maize.q25a==0] = 0

baseline_farmers$mid_agro <- ifelse(baseline_farmers$CHECK.MAIZE.Q25B=="d",1,0)
baseline_farmers$mid_agro[is.na(baseline_farmers$CHECK.MAIZE.Q25B)] <- NA
baseline_farmers$mid_agro[baseline_farmers$CHECK.MAIZE.Q25A=="No"] = 0

#3. Q25d. How much quality maize seed (hybrid or OPV) did you buy from an input dealer in the second agricultural season of 2020? Record amount in **KG**
#baseline_farmers$Check2.check.maize.q25d[baseline_farmers$agro==0] = 0
baseline_farmers <- trim("Check2.check.maize.q25d",baseline_farmers,trim_perc=.05)
#baseline_farmers$Check2.check.maize.q25d <- ihs(baseline_farmers$Check2.check.maize.q25d)

#email to Bjorn (08/04) about farmer q25d

baseline_farmers$mid_Check2.check.maize.q25d <- baseline_farmers$CHECK.MAIZE.Q25D
baseline_farmers$mid_Check2.check.maize.q25d <- as.numeric((as.character(baseline_farmers$mid_Check2.check.maize.q25d)))
#baseline_farmers$mid_Check2.check.maize.q25d[baseline_farmers$mid_agro==0] = 0
baseline_farmers <- trim("mid_Check2.check.maize.q25d",baseline_farmers,trim_perc=.05)
#baseline_farmers$mid_Check2.check.maize.q25d <- ihs(baseline_farmers$mid_Check2.check.maize.q25d)

#4. services
variables_servicesF_mid <- cbind(baseline_farmers$end_refunds,baseline_farmers$end_gives_credit,baseline_farmers$end_gives_advice
                                 ,baseline_farmers$end_delivers,baseline_farmers$end_after_sales_service,baseline_farmers$end_payment_mehtods
                                 ,baseline_farmers$end_small_quant)
variables_servicesF_base <- cbind(baseline_farmers$refunds,baseline_farmers$gives_credit,baseline_farmers$gives_advice
                                 ,baseline_farmers$delivers,baseline_farmers$after_sales_service,baseline_farmers$payment_mehtods
                                 ,baseline_farmers$small_quant)

index_servicesF_mid <- icwIndex(xmat=variables_servicesF_mid)
baseline_farmers$index_servicesF_mid <- index_servicesF_mid$index

index_servicesF_base <- icwIndex(xmat=variables_servicesF_base)
baseline_farmers$index_servicesF_base <- index_servicesF_base$index

#5. practices
#q40 (Wilber: For q40, options b and c are technically correct. b if 2 seeds per hill are used and c if 1 seed per hill is used.)
baseline_farmers$correct_q40 <- 0
baseline_farmers$correct_q40[baseline_farmers$Check2.check.maize.q40=="b"] <- 1
baseline_farmers$correct_q40[baseline_farmers$Check2.check.maize.q40=="c"] <- 1

baseline_farmers$mid_Check2.check.maize.q40 <- baseline_farmers$CHECK.MAIZE.Q40
baseline_farmers$mid_correct_q40 <- 0
baseline_farmers$mid_correct_q40[baseline_farmers$mid_Check2.check.maize.q40=="b"] <- 1
baseline_farmers$mid_correct_q40[baseline_farmers$mid_Check2.check.maize.q40=="c"] <- 1
baseline_farmers$mid_correct_q40[baseline_farmers$attrition_ind_F_end==1] <- NA

#q41 (Wilber: The same applies to q41.)
baseline_farmers$correct_q41 <- ifelse(baseline_farmers$Check2.check.maize.q41=="1",1,0)
baseline_farmers$correct_q41[baseline_farmers$Check2.check.maize.q41=="2"] <- 1

baseline_farmers$mid_Check2.check.maize.q41 <- baseline_farmers$CHECK.MAIZE.Q41
baseline_farmers$mid_Check2.check.maize.q41 <- as.numeric(as.character(baseline_farmers$mid_Check2.check.maize.q41))
baseline_farmers$mid_correct_q41 <- ifelse(baseline_farmers$mid_Check2.check.maize.q41=="1",1,0)
baseline_farmers$mid_correct_q41[baseline_farmers$mid_Check2.check.maize.q41=="2"] <- 1
baseline_farmers$mid_correct_q41[baseline_farmers$attrition_ind_F_end==1] <- NA

#q42
baseline_farmers$correct_q42 <- NA
baseline_farmers$correct_q42[baseline_farmers$Check2.check.maize.q42=="1"] <- 1
baseline_farmers$correct_q42[baseline_farmers$Check2.check.maize.q42=="0"] <- 0

baseline_farmers$mid_Check2.check.maize.q42 <- baseline_farmers$CHECK.MAIZE.Q42
baseline_farmers$mid_Check2.check.maize.q42<-ifelse(baseline_farmers$mid_Check2.check.maize.q42=="Yes",1,0)
baseline_farmers$mid_correct_q42 <- NA
baseline_farmers$mid_correct_q42[baseline_farmers$mid_Check2.check.maize.q42=="1"] <- 1
baseline_farmers$mid_correct_q42[baseline_farmers$mid_Check2.check.maize.q42=="0"] <- 0

#q43
baseline_farmers$correct_q43 <- NA
baseline_farmers$correct_q43[baseline_farmers$Check2.check.maize.q43=="1"] <- 1
baseline_farmers$correct_q43[baseline_farmers$Check2.check.maize.q43=="0"] <- 0

baseline_farmers$mid_Check2.check.maize.q43 <- baseline_farmers$CHECK.MAIZE.Q43
baseline_farmers$mid_Check2.check.maize.q43<-ifelse(baseline_farmers$mid_Check2.check.maize.q43=="Yes",1,0)
baseline_farmers$mid_correct_q43 <- NA
baseline_farmers$mid_correct_q43[baseline_farmers$mid_Check2.check.maize.q43=="1"] <- 1
baseline_farmers$mid_correct_q43[baseline_farmers$mid_Check2.check.maize.q43=="0"] <- 0

#q44
baseline_farmers$correct_q44 <- NA
baseline_farmers$correct_q44[baseline_farmers$Check2.check.maize.q44=="1"] <- 1
baseline_farmers$correct_q44[baseline_farmers$Check2.check.maize.q44=="0"] <- 0

baseline_farmers$mid_Check2.check.maize.q44 <- baseline_farmers$CHECK.MAIZE.Q44
baseline_farmers$mid_Check2.check.maize.q44<-ifelse(baseline_farmers$mid_Check2.check.maize.q44=="Yes",1,0)
baseline_farmers$mid_Check2.check.maize.q44 <- as.numeric(as.character(baseline_farmers$mid_Check2.check.maize.q44))
baseline_farmers$mid_correct_q44 <- NA
baseline_farmers$mid_correct_q44[baseline_farmers$mid_Check2.check.maize.q44=="1"] <- 1
baseline_farmers$mid_correct_q44[baseline_farmers$mid_Check2.check.maize.q44=="0"] <- 0

#q45
baseline_farmers$correct_q45[baseline_farmers$Check2.check.maize.q45>=3] <- 1
baseline_farmers$correct_q45[baseline_farmers$Check2.check.maize.q45<3] <- 0

baseline_farmers$mid_Check2.check.maize.q45 <- baseline_farmers$CHECK.MAIZE.Q45
baseline_farmers$mid_Check2.check.maize.q45 <- as.numeric(as.character(baseline_farmers$mid_Check2.check.maize.q45))
baseline_farmers$mid_correct_q45[baseline_farmers$mid_Check2.check.maize.q45>=3] <- 1
baseline_farmers$mid_correct_q45[baseline_farmers$mid_Check2.check.maize.q45<3] <- 0

#q46
baseline_farmers$correct_q46 <- (baseline_farmers$Check2.check.maize.q46 <= 20)
baseline_farmers$correct_q46<-ifelse(baseline_farmers$correct_q46=="TRUE",1,0)

baseline_farmers$mid_Check2.check.maize.q46 <- baseline_farmers$CHECK.MAIZE.Q46
baseline_farmers$mid_Check2.check.maize.q46 <- (as.numeric(as.character(baseline_farmers$mid_Check2.check.maize.q46)))
baseline_farmers$mid_correct_q46 <- (baseline_farmers$mid_Check2.check.maize.q46 <= 20)
baseline_farmers$mid_correct_q46<-ifelse(baseline_farmers$mid_correct_q46=="TRUE",1,0)

#q47
baseline_farmers$correct_q47 <- NA
baseline_farmers$correct_q47[baseline_farmers$Check2.check.maize.q47=="1"] <- 1
baseline_farmers$correct_q47[baseline_farmers$Check2.check.maize.q47=="0"] <- 0

baseline_farmers$mid_Check2.check.maize.q47 <- baseline_farmers$CHECK.MAIZE.Q47
baseline_farmers$mid_Check2.check.maize.q47<-ifelse(baseline_farmers$mid_Check2.check.maize.q47=="Yes",1,0)
baseline_farmers$mid_correct_q47 <- NA
baseline_farmers$mid_correct_q47[baseline_farmers$mid_Check2.check.maize.q47=="1"] <- 1
baseline_farmers$mid_correct_q47[baseline_farmers$mid_Check2.check.maize.q47=="0"] <- 0

#q48
baseline_farmers$correct_q48 <- ifelse(baseline_farmers$Check2.check.maize.q48=="2",1,0)
baseline_farmers$correct_q48[is.na(baseline_farmers$Check2.check.maize.q48)] <- NA

baseline_farmers$mid_Check2.check.maize.q48 <- baseline_farmers$CHECK.MAIZE.Q48
baseline_farmers$mid_correct_q48 <- ifelse(baseline_farmers$mid_Check2.check.maize.q48=="2",1,0)
baseline_farmers$mid_correct_q48[is.na(baseline_farmers$mid_Check2.check.maize.q48)] <- NA

#q49
baseline_farmers$correct_q49 <- NA
baseline_farmers$correct_q49[baseline_farmers$Check2.check.maize.q49=="1"] <- 1
baseline_farmers$correct_q49[baseline_farmers$Check2.check.maize.q49=="0"] <- 0

baseline_farmers$mid_Check2.check.maize.q49 <- baseline_farmers$CHECK.MAIZE.Q49
baseline_farmers$mid_Check2.check.maize.q49<-ifelse(baseline_farmers$mid_Check2.check.maize.q49=="Yes",1,0)
baseline_farmers$mid_correct_q49 <- NA
baseline_farmers$mid_correct_q49[baseline_farmers$mid_Check2.check.maize.q49=="1"] <- 1
baseline_farmers$mid_correct_q49[baseline_farmers$mid_Check2.check.maize.q49=="0"] <- 0

variables_practices_mid <- cbind(baseline_farmers$mid_correct_q40,baseline_farmers$mid_correct_q41,baseline_farmers$mid_correct_q42
                                 ,baseline_farmers$mid_correct_q43,baseline_farmers$mid_correct_q44,baseline_farmers$mid_correct_q45
                                 ,baseline_farmers$mid_correct_q46,baseline_farmers$mid_correct_q47,baseline_farmers$mid_correct_q48
                                 ,baseline_farmers$mid_correct_q49)
variables_practices_base <- cbind(baseline_farmers$correct_q40,baseline_farmers$correct_q41,baseline_farmers$correct_q42
                                  ,baseline_farmers$correct_q43,baseline_farmers$correct_q44,baseline_farmers$correct_q45
                                  ,baseline_farmers$correct_q46,baseline_farmers$correct_q47,baseline_farmers$correct_q48
                                  ,baseline_farmers$correct_q49)

index_practices_mid <- icwIndex(xmat=variables_practices_mid)
baseline_farmers$index_practices_mid <- index_practices_mid$index

index_practices_base <- icwIndex(xmat=variables_practices_base)
baseline_farmers$index_practices_base <- index_practices_base$index

#Q25h. Do you think that maize seed that you can buy at agro-input dealer is counterfeit/adulterated? NEW ANSWER OPTION
baseline_farmers$mid_Check2.check.maize.q25h <- baseline_farmers$CHECK.MAIZE.Q25H
baseline_farmers$mid_Check2.check.maize.q25h<-ifelse(baseline_farmers$mid_Check2.check.maize.q25h=="Yes",1,0)

#farmer saved
baseline_farmers$mid_Check2.check.maize.q31 <- baseline_farmers$CHECK.MAIZE.Q31
baseline_farmers$mid_Land_Races<-ifelse(baseline_farmers$mid_Check2.check.maize.q31=="Land_Races",1,0)
baseline_farmers$mid_Land_Races[is.na(baseline_farmers$mid_Check2.check.maize.q31)]<-NA

#6. overall index
variables_overallprimF_mid <- cbind(baseline_farmers$mid_Check2.check.maize.q25a,baseline_farmers$mid_agro
                                    ,baseline_farmers$index_practices_mid
                                    ,baseline_farmers$mid_Land_Races)
variables_overallprimF_base <- cbind(baseline_farmers$Check2.check.maize.q25a,baseline_farmers$agro
                                     ,baseline_farmers$index_practices_base
                                     ,baseline_farmers$Land_Races)

index_overallprimF_mid <- icwIndex(xmat=variables_overallprimF_mid,revcols = c(4))
baseline_farmers$index_overallprimF_mid <- index_overallprimF_mid$index

index_overallprimF_base <- icwIndex(xmat=variables_overallprimF_base,revcols = c(4))
baseline_farmers$index_overallprimF_base <- index_overallprimF_base$index

################################################################################################################################################################################

###
#1#
###

results_farmer_prim <- c("mid_Check2.check.maize.q25a"  #1
                         ,"mid_agro"                    #2
                         ,"mid_Check2.check.maize.q25d" #3
                         ,"index_servicesF_mid"         #4
                         ,"index_practices_mid"         #5
                         ,"mid_Check2.check.maize.q25h" #6
                         ,"mid_Land_Races"              #7
                         ,"index_overallprimF_mid")     #8
results_farmer_prim_base <- c("Check2.check.maize.q25a"
                              ,"agro"
                              ,"Check2.check.maize.q25d"
                              ,"index_servicesF_base"
                              ,"index_practices_base"
                              ,"Check2.check.maize.q25h"
                              ,"Land_Races"
                              ,"index_overallprimF_base")

baseline_farmers[results_farmer_prim_base] <- lapply(baseline_farmers[results_farmer_prim_base],function(x)x - mean(x,na.rm = T))

df_means_end_F_prim <- array(NA,dim=c(5,8))

for (i in 1:length(results_farmer_prim)){
  df_means_end_F_prim[1,i] <- sum(baseline_farmers[results_farmer_prim[i]], na.rm=T)/(nrow(baseline_farmers)-sum(is.na(baseline_farmers[results_farmer_prim[i]])))
  df_means_end_F_prim[2,i] <- sqrt(var(baseline_farmers[results_farmer_prim[i]], na.rm=T))
  df_means_end_F_prim[3,i] <- nrow(baseline_farmers)-sum(is.na(baseline_farmers[results_farmer_prim[i]]))-sum(is.na(baseline_farmers[results_farmer_prim_base[i]]))+sum(is.na(baseline_farmers[results_farmer_prim[i]])&is.na(baseline_farmers[results_farmer_prim_base[i]]))
  df_means_end_F_prim[4,i] <- min(baseline_farmers[results_farmer_prim[i]], na.rm=T)
  df_means_end_F_prim[5,i] <- max(baseline_farmers[results_farmer_prim[i]], na.rm=T)}

###
#2#
###

baseline_farmers$training_control[baseline_farmers$training==0] <- TRUE
baseline_farmers$training_control[baseline_farmers$training==1] <- FALSE

#4.
index_servicesF_midT <- icwIndex(xmat=variables_servicesF_mid,sgroup = baseline_farmers$training_control)
baseline_farmers$index_servicesF_midT <- index_servicesF_midT$index

index_servicesF_baseT <- icwIndex(xmat=variables_servicesF_base,sgroup = baseline_farmers$training_control)
baseline_farmers$index_servicesF_baseT <- index_servicesF_baseT$index

#5.
index_practices_midT <- icwIndex(xmat=variables_practices_mid,sgroup = baseline_farmers$training_control)
baseline_farmers$index_practices_midT <- index_practices_midT$index

index_practices_baseT <- icwIndex(xmat=variables_practices_base,sgroup = baseline_farmers$training_control)
baseline_farmers$index_practices_baseT <- index_practices_baseT$index

#6.
index_overallprimF_midT <- icwIndex(xmat=variables_overallprimF_mid,sgroup = baseline_farmers$training_control,revcols = c(4))
baseline_farmers$index_overallprimF_midT <- index_overallprimF_midT$index

index_overallprimF_baseT <- icwIndex(xmat=variables_overallprimF_base,sgroup = baseline_farmers$training_control,revcols = c(4))
baseline_farmers$index_overallprimF_baseT <- index_overallprimF_baseT$index

df_ols_end_F_prim <- array(NA,dim=c(3,3,11))

results_farmer_prim <- c("mid_Check2.check.maize.q25a"
                         ,"mid_agro"
                         ,"mid_Check2.check.maize.q25d"
                         ,"index_servicesF_midT"
                         ,"index_practices_midT"
                         ,"mid_Check2.check.maize.q25h"
                         ,"mid_Land_Races"
                         ,"index_overallprimF_midT")
results_farmer_prim_base <- c("Check2.check.maize.q25a"
                              ,"agro"
                              ,"Check2.check.maize.q25d"
                              ,"index_servicesF_baseT"
                              ,"index_practices_baseT"
                              ,"Check2.check.maize.q25h"
                              ,"Land_Races"
                              ,"index_overallprimF_baseT")

baseline_farmers[results_farmer_prim_base] <- lapply(baseline_farmers[results_farmer_prim_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_farmer_prim)){
  ols <- lm(as.formula(paste(paste(results_farmer_prim[i],"training*clearing_demeaned*farmer_demeaned",sep="~"),results_farmer_prim_base[i],sep="+")),data=baseline_farmers)
  #ols <- lm(as.formula(paste(results_farmer_prim[i],"training*clearing_demeaned*farmer_demeaned",sep="~")),data=baseline_farmers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_farmers$catchID,type="CR0")

  df_ols_end_F_prim[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_ols_end_F_prim[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_ols_end_F_prim[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]}

###
#3#
###

baseline_farmers$clearing_control[baseline_farmers$clearing==0] <- TRUE
baseline_farmers$clearing_control[baseline_farmers$clearing==1] <- FALSE

#4.
index_servicesF_midC <- icwIndex(xmat=variables_servicesF_mid,sgroup = baseline_farmers$clearing_control)
baseline_farmers$index_servicesF_midC <- index_servicesF_midC$index

index_servicesF_baseC <- icwIndex(xmat=variables_servicesF_base,sgroup = baseline_farmers$clearing_control)
baseline_farmers$index_servicesF_baseC <- index_servicesF_baseC$index

#5.
index_practices_midC <- icwIndex(xmat=variables_practices_mid,sgroup = baseline_farmers$clearing_control)
baseline_farmers$index_practices_midC <- index_practices_midC$index

index_practices_baseC <- icwIndex(xmat=variables_practices_base,sgroup = baseline_farmers$clearing_control)
baseline_farmers$index_practices_baseC <- index_practices_baseC$index

#6.
index_overallprimF_midC <- icwIndex(xmat=variables_overallprimF_mid,sgroup = baseline_farmers$clearing_control,revcols = c(4))
baseline_farmers$index_overallprimF_midC <- index_overallprimF_midC$index

index_overallprimF_baseC <- icwIndex(xmat=variables_overallprimF_base,sgroup = baseline_farmers$clearing_control,revcols = c(4))
baseline_farmers$index_overallprimF_baseC <- index_overallprimF_baseC$index

results_farmer_prim <- c("mid_Check2.check.maize.q25a"
                         ,"mid_agro"
                         ,"mid_Check2.check.maize.q25d"
                         ,"index_servicesF_midC"
                         ,"index_practices_midC"
                         ,"mid_Check2.check.maize.q25h"
                         ,"mid_Land_Races"
                         ,"index_overallprimF_midC")
results_farmer_prim_base <- c("Check2.check.maize.q25a"
                              ,"agro"
                              ,"Check2.check.maize.q25d"
                              ,"index_servicesF_baseC"
                              ,"index_practices_baseC"
                              ,"Check2.check.maize.q25h"
                              ,"Land_Races"
                              ,"index_overallprimF_baseC")

baseline_farmers[results_farmer_prim_base] <- lapply(baseline_farmers[results_farmer_prim_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_farmer_prim)){
  ols <- lm(as.formula(paste(paste(results_farmer_prim[i],"training_demeaned*clearing*farmer_demeaned",sep="~"),results_farmer_prim_base[i],sep="+")),data=baseline_farmers)
  #ols <- lm(as.formula(paste(results_farmer_prim[i],"training_demeaned*clearing*farmer_demeaned",sep="~")),data=baseline_farmers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_farmers$catchID,type="CR0")

  df_ols_end_F_prim[1,2,i] <- coef_test(ols, vcov_cluster)$beta[3]
  df_ols_end_F_prim[2,2,i] <- coef_test(ols, vcov_cluster)$SE[3]
  df_ols_end_F_prim[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]}

###
#4#
###

baseline_farmers$farmer_control[baseline_farmers$farmer==0] <- TRUE
baseline_farmers$farmer_control[baseline_farmers$farmer==1] <- FALSE

#4.
index_servicesF_midF <- icwIndex(xmat=variables_servicesF_mid,sgroup = baseline_farmers$farmer_control)
baseline_farmers$index_servicesF_midF <- index_servicesF_midF$index

index_servicesF_baseF <- icwIndex(xmat=variables_servicesF_base,sgroup = baseline_farmers$farmer_control)
baseline_farmers$index_servicesF_baseF <- index_servicesF_baseF$index

#5.
index_practices_midF <- icwIndex(xmat=variables_practices_mid,sgroup = baseline_farmers$farmer_control)
baseline_farmers$index_practices_midF <- index_practices_midF$index

index_practices_baseF <- icwIndex(xmat=variables_practices_base,sgroup = baseline_farmers$farmer_control)
baseline_farmers$index_practices_baseF <- index_practices_baseF$index

#6.
index_overallprimF_midF <- icwIndex(xmat=variables_overallprimF_mid,sgroup = baseline_farmers$farmer_control,revcols = c(4))
baseline_farmers$index_overallprimF_midF <- index_overallprimF_midF$index

index_overallprimF_baseF <- icwIndex(xmat=variables_overallprimF_base,sgroup = baseline_farmers$farmer_control,revcols = c(4))
baseline_farmers$index_overallprimF_baseF <- index_overallprimF_baseF$index

results_farmer_prim <- c("mid_Check2.check.maize.q25a"
                         ,"mid_agro"
                         ,"mid_Check2.check.maize.q25d"
                         ,"index_servicesF_midF"
                         ,"index_practices_midF"
                         ,"mid_Check2.check.maize.q25h"
                         ,"mid_Land_Races"
                         ,"index_overallprimF_midF")
results_farmer_prim_base <- c("Check2.check.maize.q25a"
                              ,"agro"
                              ,"Check2.check.maize.q25d"
                              ,"index_servicesF_baseF"
                              ,"index_practices_baseF"
                              ,"Check2.check.maize.q25h"
                              ,"Land_Races"
                              ,"index_overallprimF_baseF")

baseline_farmers[results_farmer_prim_base] <- lapply(baseline_farmers[results_farmer_prim_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_farmer_prim)){
  ols <- lm(as.formula(paste(paste(results_farmer_prim[i],"training_demeaned*clearing_demeaned*farmer",sep="~"),results_farmer_prim_base[i],sep="+")),data=baseline_farmers)
  #ols <- lm(as.formula(paste(results_farmer_prim[i],"training_demeaned*clearing_demeaned*farmer",sep="~")),data=baseline_farmers)
  vcov_cluster_shop <- vcovCR(ols,cluster=baseline_farmers$shop_ID,type="CR0")

  #farmer video treatment at village/shop level so no clustering needed
  df_ols_end_F_prim[1,3,i] <- coef_test(ols, vcov_cluster_shop)$beta[4]
  df_ols_end_F_prim[2,3,i] <- coef_test(ols, vcov_cluster_shop)$SE[4]
  df_ols_end_F_prim[3,3,i] <- coef_test(ols, vcov_cluster_shop)$p_Satt[4]}

#transformation because otherwise no correlation between agro and q25d possible:
baseline_farmers$Check2.check.maize.q25d_save <- baseline_farmers$Check2.check.maize.q25d
baseline_farmers$mid_Check2.check.maize.q25d_save <- baseline_farmers$mid_Check2.check.maize.q25d

baseline_farmers$Check2.check.maize.q25d[baseline_farmers$agro==0] = 0
baseline_farmers$Check2.check.maize.q25d <- ihs(baseline_farmers$Check2.check.maize.q25d)

baseline_farmers$mid_Check2.check.maize.q25d[baseline_farmers$mid_agro==0] = 0
baseline_farmers$mid_Check2.check.maize.q25d <- ihs(baseline_farmers$mid_Check2.check.maize.q25d)

#Aker, Boumnijel, McClelland, Tierney (2012)
df_farmer_primT <- data.frame(baseline_farmers$mid_Check2.check.maize.q25a,baseline_farmers$mid_agro
                                 ,baseline_farmers$mid_Check2.check.maize.q25d,baseline_farmers$index_servicesF_midT
                                 ,baseline_farmers$index_practices_midT,baseline_farmers$mid_Check2.check.maize.q25h,baseline_farmers$mid_Land_Races)
df_farmer_primC <- data.frame(baseline_farmers$mid_Check2.check.maize.q25a,baseline_farmers$mid_agro
                              ,baseline_farmers$mid_Check2.check.maize.q25d,baseline_farmers$index_servicesF_midC
                              ,baseline_farmers$index_practices_midC,baseline_farmers$mid_Check2.check.maize.q25h,baseline_farmers$mid_Land_Races)
df_farmer_primF <- data.frame(baseline_farmers$mid_Check2.check.maize.q25a,baseline_farmers$mid_agro
                              ,baseline_farmers$mid_Check2.check.maize.q25d,baseline_farmers$index_servicesF_midF
                              ,baseline_farmers$index_practices_midF,baseline_farmers$mid_Check2.check.maize.q25h,baseline_farmers$mid_Land_Races)
#no overall index

df_ols_end_F_prim_J <- array(NA,dim=c(3,3,11))

results_farmer_prim_J <- c("mid_Check2.check.maize.q25a","mid_agro","mid_Check2.check.maize.q25d"
                           ,"index_servicesF_mid","index_practices_midF","mid_Check2.check.maize.q25h"
                           ,"mid_Land_Races")
#no overall index

for (i in 1:length(results_farmer_prim_J)){
  df_ols_end_F_prim_J[3,1,i] <- adjust_p(df_ols_end_F_prim[3,1,i],df_farmer_primT,i)
  df_ols_end_F_prim_J[3,2,i] <- adjust_p(df_ols_end_F_prim[3,2,i],df_farmer_primC,i)
  df_ols_end_F_prim_J[3,3,i] <- adjust_p(df_ols_end_F_prim[3,3,i],df_farmer_primF,i)}

baseline_farmers$Check2.check.maize.q25d <- baseline_farmers$Check2.check.maize.q25d_save
baseline_farmers$mid_Check2.check.maize.q25d <- baseline_farmers$mid_Check2.check.maize.q25d_save










################################################################################################################################################################################
##### 10 ANALYSIS: Farmer - Secondary ##########################################################################################################################################
################################################################################################################################################################################

#2. Q26. ${enumerator} : Ask the farmer to mention as many improved maize varieties that they are aware of
baseline_farmers$Check2.check.maize.q26.Longe_10H <- ifelse(baseline_farmers$Check2.check.maize.q26.Longe_10H=="True",1,0)
baseline_farmers$Check2.check.maize.q26.Longe_7H <- ifelse(baseline_farmers$Check2.check.maize.q26.Longe_7H=="True",1,0)
#baseline_farmers$Check2.check.maize.q26.Longe_7R_Kayongo.go <- ifelse(baseline_farmers$Check2.check.maize.q26.Longe_7R_Kayongo.go=="True",1,0) #because above
baseline_farmers$Check2.check.maize.q26.Bazooka <- ifelse(baseline_farmers$Check2.check.maize.q26.Bazooka=="True",1,0)
baseline_farmers$Check2.check.maize.q26.Longe_6H <- ifelse(baseline_farmers$Check2.check.maize.q26.Longe_6H=="True",1,0)
#baseline_farmers$Check2.check.maize.q26.Longe_5 <- ifelse(baseline_farmers$Check2.check.maize.q26.Longe_5=="True",1,0)
baseline_farmers$Check2.check.maize.q26.Longe_4 <- ifelse(baseline_farmers$Check2.check.maize.q26.Longe_4=="True",1,0)
baseline_farmers$Check2.check.maize.q26.Panner <- ifelse(baseline_farmers$Check2.check.maize.q26.Panner=="True",1,0)
#baseline_farmers$Check2.check.maize.q26.Wema <- ifelse(baseline_farmers$Check2.check.maize.q26.Wema=="True",1,0)
baseline_farmers$Check2.check.maize.q26.KH_series <- ifelse(baseline_farmers$Check2.check.maize.q26.KH_series=="True",1,0)
baseline_farmers$Check2.check.maize.q26.Land_Races <- ifelse(baseline_farmers$Check2.check.maize.q26.Land_Races=="True",1,0)
baseline_farmers$Check2.check.maize.q26.Other_hybrid <- ifelse(baseline_farmers$Check2.check.maize.q26.Other_hybrid=="True",1,0)

baseline_farmers$number_known <- (baseline_farmers$Check2.check.maize.q26.Longe_10H
                                  +baseline_farmers$Check2.check.maize.q26.Longe_7H
                                  +baseline_farmers$Check2.check.maize.q26.Longe_7R_Kayongo.go
                                  +baseline_farmers$Check2.check.maize.q26.Bazooka
                                  +baseline_farmers$Check2.check.maize.q26.Longe_6H
                                  +baseline_farmers$Check2.check.maize.q26.Longe_5
                                  +baseline_farmers$Check2.check.maize.q26.Longe_4
                                  +baseline_farmers$Check2.check.maize.q26.Panner
                                  +baseline_farmers$Check2.check.maize.q26.Wema
                                  +baseline_farmers$Check2.check.maize.q26.KH_series
                                  +baseline_farmers$Check2.check.maize.q26.Land_Races
                                  +baseline_farmers$Check2.check.maize.q26.Other_hybrid)

baseline_farmers <- trim("number_known",baseline_farmers,trim_perc=.05)

baseline_farmers$mid_Check2.check.maize.q26.Longe_10H <- baseline_farmers$CHECK.MAIZE.Q26.LONGE_10H
baseline_farmers$mid_Check2.check.maize.q26.Longe_7H <- baseline_farmers$CHECK.MAIZE.Q26.LONGE_7H
baseline_farmers$mid_Check2.check.maize.q26.Longe_7R_Kayongo.go <- baseline_farmers$CHECK.MAIZE.Q26.LONGE_7R_KAYONGO.GO
baseline_farmers$mid_Check2.check.maize.q26.Bazooka <- baseline_farmers$CHECK.MAIZE.Q26.BAZOOKA
baseline_farmers$mid_Check2.check.maize.q26.Longe_6H <- baseline_farmers$CHECK.MAIZE.Q26.LONGE_6H
baseline_farmers$mid_Check2.check.maize.q26.Longe_5 <- baseline_farmers$CHECK.MAIZE.Q26.LONGE_5
baseline_farmers$mid_Check2.check.maize.q26.Longe_4 <- baseline_farmers$CHECK.MAIZE.Q26.LONGE_4
baseline_farmers$mid_Check2.check.maize.q26.Panner <- baseline_farmers$CHECK.MAIZE.Q26.PANNER
baseline_farmers$mid_Check2.check.maize.q26.Wema <- baseline_farmers$CHECK.MAIZE.Q26.WEMA
baseline_farmers$mid_Check2.check.maize.q26.KH_series <- baseline_farmers$CHECK.MAIZE.Q26.KH_SERIES
baseline_farmers$mid_Check2.check.maize.q26.Land_Races <- baseline_farmers$CHECK.MAIZE.Q26.LAND_RACES
baseline_farmers$mid_Check2.check.maize.q26.Other_hybrid <- baseline_farmers$CHECK.MAIZE.Q26.OTHER_HYBRID

baseline_farmers$mid_Check2.check.maize.q26.Longe_10H <- ifelse(baseline_farmers$mid_Check2.check.maize.q26.Longe_10H=="True",1,0)
baseline_farmers$mid_Check2.check.maize.q26.Longe_7H <- ifelse(baseline_farmers$mid_Check2.check.maize.q26.Longe_7H=="True",1,0)
baseline_farmers$mid_Check2.check.maize.q26.Longe_7R_Kayongo.go <- ifelse(baseline_farmers$mid_Check2.check.maize.q26.Longe_7R_Kayongo.go=="True",1,0)
baseline_farmers$mid_Check2.check.maize.q26.Bazooka <- ifelse(baseline_farmers$mid_Check2.check.maize.q26.Bazooka=="True",1,0)
baseline_farmers$mid_Check2.check.maize.q26.Longe_6H <- ifelse(baseline_farmers$mid_Check2.check.maize.q26.Longe_6H=="True",1,0)
baseline_farmers$mid_Check2.check.maize.q26.Longe_5 <- ifelse(baseline_farmers$mid_Check2.check.maize.q26.Longe_5=="True",1,0)
baseline_farmers$mid_Check2.check.maize.q26.Longe_4 <- ifelse(baseline_farmers$mid_Check2.check.maize.q26.Longe_4=="True",1,0)
baseline_farmers$mid_Check2.check.maize.q26.Panner <- ifelse(baseline_farmers$mid_Check2.check.maize.q26.Panner=="True",1,0)
baseline_farmers$mid_Check2.check.maize.q26.Wema <- ifelse(baseline_farmers$mid_Check2.check.maize.q26.Wema=="True",1,0)
baseline_farmers$mid_Check2.check.maize.q26.KH_series <- ifelse(baseline_farmers$mid_Check2.check.maize.q26.KH_series=="True",1,0)
baseline_farmers$mid_Check2.check.maize.q26.Land_Races <- ifelse(baseline_farmers$mid_Check2.check.maize.q26.Land_Races=="True",1,0)
baseline_farmers$mid_Check2.check.maize.q26.Other_hybrid <- ifelse(baseline_farmers$mid_Check2.check.maize.q26.Other_hybrid=="True",1,0)

baseline_farmers$mid_number_known <- (baseline_farmers$mid_Check2.check.maize.q26.Longe_10H
                                  +baseline_farmers$mid_Check2.check.maize.q26.Longe_7H
                                  +baseline_farmers$mid_Check2.check.maize.q26.Longe_7R_Kayongo.go
                                  +baseline_farmers$mid_Check2.check.maize.q26.Bazooka
                                  +baseline_farmers$mid_Check2.check.maize.q26.Longe_6H
                                  +baseline_farmers$mid_Check2.check.maize.q26.Longe_5
                                  +baseline_farmers$mid_Check2.check.maize.q26.Longe_4
                                  +baseline_farmers$mid_Check2.check.maize.q26.Panner
                                  +baseline_farmers$mid_Check2.check.maize.q26.Wema
                                  +baseline_farmers$mid_Check2.check.maize.q26.KH_series
                                  +baseline_farmers$mid_Check2.check.maize.q26.Land_Races
                                  +baseline_farmers$mid_Check2.check.maize.q26.Other_hybrid)

baseline_farmers <- trim("mid_number_known",baseline_farmers,trim_perc=.05)

#3. Q64. Do you know **${calc_biz}**  or ${dealer_name} sometimes called ${nickname} located in ${market_name} market. The place can be described as: ${eye}
baseline_farmers$mid_knows_dealer <- baseline_farmers$end_knows_dealer

#4. Q67. Did you buy seed from  **${calc_biz}** in the last season (entoigo 2020)
baseline_farmers$bought_last_season[baseline_farmers$knows_dealer==0] <- 0
baseline_farmers$bought_last_season[baseline_farmers$bought_at_dealer==0] <- 0

baseline_farmers$mid_bought_last_season <- baseline_farmers$end_bought_last_season
baseline_farmers$mid_knows_dealer <- baseline_farmers$end_knows_dealer
baseline_farmers$mid_bought_at_dealer <- baseline_farmers$end_bought_at_dealer

baseline_farmers$mid_bought_last_season[baseline_farmers$mid_knows_dealer==0] <- 0
baseline_farmers$mid_bought_last_season[baseline_farmers$mid_bought_at_dealer==0] <- 0

#moved to "no baseline" because
# > table(baseline_farmers$bought_last_season[baseline_farmers$clearing==0])
# 0 
# 837

#5. overall index
variables_overallsecF_mid <- cbind(baseline_farmers$mid_number_known,baseline_farmers$mid_knows_dealer)
variables_overallsecF_base <- cbind(baseline_farmers$number_known,baseline_farmers$knows_dealer)

index_overallsecF_mid <- icwIndex(xmat=variables_overallsecF_mid)
baseline_farmers$index_overallsecF_mid <- index_overallsecF_mid$index

index_overallsecF_base <- icwIndex(xmat=variables_overallsecF_base)
baseline_farmers$index_overallsecF_base <- index_overallsecF_base$index

################################################################################################################################################################################

###
#1#
###

results_farmer_sec <- c("mid_number_known"
                        ,"mid_knows_dealer"
                        ,"index_overallsecF_mid")
results_farmer_sec_base <- c("number_known"
                             ,"knows_dealer"
                             ,"index_overallsecF_base")

baseline_farmers[results_farmer_sec_base] <- lapply(baseline_farmers[results_farmer_sec_base],function(x)x - mean(x,na.rm = T))

df_means_end_F_sec <- array(NA,dim=c(3,11))

for (i in 1:length(results_farmer_sec)){
  df_means_end_F_sec[1,i] <- sum(baseline_farmers[results_farmer_sec[i]], na.rm=T)/(nrow(baseline_farmers)-sum(is.na(baseline_farmers[results_farmer_sec[i]])))
  df_means_end_F_sec[2,i] <- sqrt(var(baseline_farmers[results_farmer_sec[i]], na.rm=T))
  df_means_end_F_sec[3,i] <- nrow(baseline_farmers)-sum(is.na(baseline_farmers[results_farmer_sec[i]]))-sum(is.na(baseline_farmers[results_farmer_sec_base[i]]))+sum(is.na(baseline_farmers[results_farmer_sec[i]])&is.na(baseline_farmers[results_farmer_sec_base[i]]))}

###
#2#
###

baseline_farmers$training_control[baseline_farmers$training==0] <- TRUE
baseline_farmers$training_control[baseline_farmers$training==1] <- FALSE

#5.
index_overallsecF_midT <- icwIndex(xmat=variables_overallsecF_mid,sgroup = baseline_farmers$training_control)
baseline_farmers$index_overallsecF_midT <- index_overallsecF_midT$index

index_overallsecF_baseT <- icwIndex(xmat=variables_overallsecF_base,sgroup = baseline_farmers$training_control)
baseline_farmers$index_overallsecF_baseT <- index_overallsecF_baseT$index

df_ols_end_F_sec <- array(NA,dim=c(3,3,11))

results_farmer_sec <- c("mid_number_known"
                        ,"mid_knows_dealer"
                        ,"index_overallsecF_midT")
results_farmer_sec_base <- c("number_known"
                             ,"knows_dealer"
                             ,"index_overallsecF_baseT")

baseline_farmers[results_farmer_sec_base] <- lapply(baseline_farmers[results_farmer_sec_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_farmer_sec)){
  ols <- lm(as.formula(paste(paste(results_farmer_sec[i],"training*clearing_demeaned*farmer_demeaned",sep="~"),results_farmer_sec_base[i],sep="+")),data=baseline_farmers)
  #ols <- lm(as.formula(paste(results_farmer_sec[i],"training*clearing_demeaned*farmer_demeaned",sep="~")),data=baseline_farmers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_farmers$catchID,type="CR0")

  df_ols_end_F_sec[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_ols_end_F_sec[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_ols_end_F_sec[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]}

###
#3#
###

baseline_farmers$clearing_control[baseline_farmers$clearing==0] <- TRUE
baseline_farmers$clearing_control[baseline_farmers$clearing==1] <- FALSE

#5.
index_overallsecF_midC <- icwIndex(xmat=variables_overallsecF_mid,sgroup = baseline_farmers$clearing_control)
baseline_farmers$index_overallsecF_midC <- index_overallsecF_midC$index

index_overallsecF_baseC <- icwIndex(xmat=variables_overallsecF_base,sgroup = baseline_farmers$clearing_control)
baseline_farmers$index_overallsecF_baseC <- index_overallsecF_baseC$index

results_farmer_sec <- c("mid_number_known"
                        ,"mid_knows_dealer"
                        ,"index_overallsecF_midC")
results_farmer_sec_base <- c("number_known"
                             ,"knows_dealer"
                             ,"index_overallsecF_baseC")

baseline_farmers[results_farmer_sec_base] <- lapply(baseline_farmers[results_farmer_sec_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_farmer_sec)){
  ols <- lm(as.formula(paste(paste(results_farmer_sec[i],"training_demeaned*clearing*farmer_demeaned",sep="~"),results_farmer_sec_base[i],sep="+")),data=baseline_farmers)
  #ols <- lm(as.formula(paste(results_farmer_sec[i],"training_demeaned*clearing*farmer_demeaned",sep="~")),data=baseline_farmers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_farmers$catchID,type="CR0")

  df_ols_end_F_sec[1,2,i] <- coef_test(ols, vcov_cluster)$beta[3]
  df_ols_end_F_sec[2,2,i] <- coef_test(ols, vcov_cluster)$SE[3]
  df_ols_end_F_sec[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]}

###
#4#
###

baseline_farmers$farmer_control[baseline_farmers$farmer==0] <- TRUE
baseline_farmers$farmer_control[baseline_farmers$farmer==1] <- FALSE

#5.
index_overallsecF_midF <- icwIndex(xmat=variables_overallsecF_mid,sgroup = baseline_farmers$farmer_control)
baseline_farmers$index_overallsecF_midF <- index_overallsecF_midF$index

index_overallsecF_baseF <- icwIndex(xmat=variables_overallsecF_base,sgroup = baseline_farmers$farmer_control)
baseline_farmers$index_overallsecF_baseF <- index_overallsecF_baseF$index

results_farmer_sec <- c("mid_number_known"
                        ,"mid_knows_dealer"
                        ,"index_overallsecF_midF")
results_farmer_sec_base <- c("number_known"
                             ,"knows_dealer"
                             ,"index_overallsecF_baseF")

baseline_farmers[results_farmer_sec_base] <- lapply(baseline_farmers[results_farmer_sec_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_farmer_sec)){
  ols <- lm(as.formula(paste(paste(results_farmer_sec[i],"training_demeaned*clearing_demeaned*farmer",sep="~"),results_farmer_sec_base[i],sep="+")),data=baseline_farmers)
  #ols <- lm(as.formula(paste(results_farmer_sec[i],"training_demeaned*clearing_demeaned*farmer*farmer",sep="~")),data=baseline_farmers)
  vcov_cluster_shop <- vcovCR(ols,cluster=baseline_farmers$shop_ID,type="CR0")

  #farmer video treatment at village/shop level so no clustering needed
  df_ols_end_F_sec[1,3,i] <- coef_test(ols, vcov_cluster_shop)$beta[4]
  df_ols_end_F_sec[2,3,i] <- coef_test(ols, vcov_cluster_shop)$SE[4]
  df_ols_end_F_sec[3,3,i] <- coef_test(ols, vcov_cluster_shop)$p_Satt[4]}

#Aker, Boumnijel, McClelland, Tierney (2012)
df_farmer_secT <- data.frame(baseline_farmers$mid_number_known,baseline_farmers$mid_knows_dealer)
df_farmer_secC <- df_farmer_secT
df_farmer_secF <- df_farmer_secT
#no overall index

df_ols_end_F_sec_J <- array(NA,dim=c(3,3,11))

results_farmer_sec_J <- c("mid_number_known","mid_knows_dealer")
#no overall index

for (i in 1:length(results_farmer_sec_J)){
  df_ols_end_F_sec_J[3,1,i] <- adjust_p(df_ols_end_F_sec[3,1,i],df_farmer_secT,i)
  df_ols_end_F_sec_J[3,2,i] <- adjust_p(df_ols_end_F_sec[3,2,i],df_farmer_secC,i)
  df_ols_end_F_sec_J[3,3,i] <- adjust_p(df_ols_end_F_sec[3,3,i],df_farmer_secF,i)}

#currently NAs because only two variables










################################################################################################################################################################################
##### 11 ANALYSIS: Farmer - Secondary: Adoption on plot ########################################################################################################################
################################################################################################################################################################################

#1. hybrid
baseline_farmers$mid_Check2.check.maize.q31 <- baseline_farmers$CHECK.MAIZE.Q31
baseline_farmers$mid_hybrid<-((baseline_farmers$mid_Check2.check.maize.q31=="Longe_10H")|(baseline_farmers$mid_Check2.check.maize.q31=="Longe_7H")|(baseline_farmers$mid_Check2.check.maize.q31=="Longe_7R_Kayongo-go")|(baseline_farmers$mid_Check2.check.maize.q31=="Bazooka")|(baseline_farmers$mid_Check2.check.maize.q31=="Longe_6H")|(baseline_farmers$mid_Check2.check.maize.q31=="Panner")|(baseline_farmers$mid_Check2.check.maize.q31=="Wema")|(baseline_farmers$mid_Check2.check.maize.q31=="KH_series"))
baseline_farmers$mid_hybrid<-ifelse(baseline_farmers$mid_hybrid=="TRUE",1,0)
baseline_farmers$mid_hybrid[baseline_farmers$mid_Check2.check.maize.q31=="Other_hybrid"] <- NA #because =Other hybrid or OPV

#2. OPV
baseline_farmers$mid_OPV<-(baseline_farmers$mid_Check2.check.maize.q31=="Longe_5")|(baseline_farmers$mid_Check2.check.maize.q31=="Longe_4")
baseline_farmers$mid_OPV<-ifelse(baseline_farmers$mid_OPV=="TRUE",1,0)
baseline_farmers$mid_OPV[baseline_farmers$mid_Check2.check.maize.q31=="Other_hybrid"] <- NA

#5. farmer saved
baseline_farmers$mid_Check2.check.maize.q32 <- baseline_farmers$CHECK.MAIZE.Q32
baseline_farmers$mid_farmer_saved_seed<-((baseline_farmers$mid_Check2.check.maize.q32=="a")|(baseline_farmers$mid_Check2.check.maize.q32=="b"))
baseline_farmers$mid_farmer_saved_seed<-ifelse(baseline_farmers$mid_farmer_saved_seed=="TRUE",1,0)

#6. agro
baseline_farmers$mid_Bought_from_agro_input_shop<-ifelse(baseline_farmers$mid_Check2.check.maize.q32=="d",1,0)

#7. adoption
baseline_farmers$mid_hybridbutsaved <- NA
baseline_farmers$mid_hybridbutsaved[baseline_farmers$mid_hybrid == 1 & baseline_farmers$mid_farmer_saved_seed == 1] <- 1
baseline_farmers$mid_hybridbutsaved[baseline_farmers$mid_hybrid == 1 & baseline_farmers$mid_farmer_saved_seed == 0] <- 0
baseline_farmers$mid_hybridbutsaved[baseline_farmers$mid_hybrid == 0] <- 0

baseline_farmers$mid_Check2.check.maize.q34 <- baseline_farmers$CHECK.MAIZE.Q34
baseline_farmers$mid_fourthormore_timeused<-((baseline_farmers$mid_Check2.check.maize.q34=="d")|(baseline_farmers$mid_Check2.check.maize.q34=="e")|(baseline_farmers$mid_Check2.check.maize.q34=="f"))
baseline_farmers$mid_fourthormore_timeused<-ifelse(baseline_farmers$mid_fourthormore_timeused=="TRUE",1,0)

baseline_farmers$mid_OPVbutfourthormore_timeused <- NA
baseline_farmers$mid_OPVbutfourthormore_timeused[baseline_farmers$mid_OPV==1 & baseline_farmers$mid_farmer_saved_seed==1 & baseline_farmers$mid_fourthormore_timeused==1] <- 1
baseline_farmers$mid_OPVbutfourthormore_timeused[baseline_farmers$mid_OPV==1 & baseline_farmers$mid_farmer_saved_seed==1 & baseline_farmers$mid_fourthormore_timeused==0] <- 0
baseline_farmers$mid_OPVbutfourthormore_timeused[baseline_farmers$mid_OPV==1 & baseline_farmers$mid_farmer_saved_seed==0] <- 0
baseline_farmers$mid_OPVbutfourthormore_timeused[baseline_farmers$mid_OPV == 0] <- 0

baseline_farmers$mid_improved<-((baseline_farmers$mid_Check2.check.maize.q31=="Longe_10H")|(baseline_farmers$mid_Check2.check.maize.q31=="Longe_7H")|(baseline_farmers$mid_Check2.check.maize.q31=="Longe_7R_Kayongo-go")|(baseline_farmers$mid_Check2.check.maize.q31=="Bazooka")|(baseline_farmers$mid_Check2.check.maize.q31=="Longe_6H")|(baseline_farmers$mid_Check2.check.maize.q31=="Panner")|(baseline_farmers$mid_Check2.check.maize.q31=="Wema")|(baseline_farmers$mid_Check2.check.maize.q31=="KH_series"|baseline_farmers$mid_Check2.check.maize.q31=="Longe_5")|(baseline_farmers$mid_Check2.check.maize.q31=="Longe_4")|(baseline_farmers$mid_Check2.check.maize.q31=="Other_hybrid"))
baseline_farmers$mid_improved<-ifelse(baseline_farmers$mid_improved=="TRUE",1,0)

baseline_farmers$mid_adoption_onfield <- baseline_farmers$mid_improved
baseline_farmers$mid_adoption_onfield[baseline_farmers$mid_hybridbutsaved==1] <- 0
baseline_farmers$mid_adoption_onfield[baseline_farmers$mid_OPVbutfourthormore_timeused==1] <- 0

#8. overall index
variables_overallsec_plotF_mid <- cbind(baseline_farmers$mid_hybrid,baseline_farmers$mid_OPV
                                        ,baseline_farmers$mid_farmer_saved_seed,baseline_farmers$mid_Bought_from_agro_input_shop)
variables_overallsec_plotF_base <- cbind(baseline_farmers$hybrid,baseline_farmers$OPV
                                         ,baseline_farmers$farmer_saved_seed,baseline_farmers$Bought_from_agro_input_shop)

index_overallsec_plotF_mid <- icwIndex(xmat=variables_overallsec_plotF_mid,revcols = c(3))
baseline_farmers$index_overallsec_plotF_mid <- index_overallsec_plotF_mid$index

index_overallsec_plotF_base <- icwIndex(xmat=variables_overallsec_plotF_base,revcols = c(3))
baseline_farmers$index_overallsec_plotF_base <- index_overallsec_plotF_base$index

################################################################################################################################################################################

###
#1#
###

results_farmer_sec_plot <- c("mid_hybrid"
                             ,"mid_OPV"
                             ,"mid_farmer_saved_seed"
                             ,"mid_Bought_from_agro_input_shop"
                             ,"mid_adoption_onfield"
                             ,"index_overallsec_plotF_mid")
results_farmer_sec_plot_base <- c("hybrid"
                                  ,"OPV"
                                  ,"farmer_saved_seed"
                                  ,"Bought_from_agro_input_shop"
                                  ,"adoption_onfield"
                                  ,"index_overallsec_plotF_base")

baseline_farmers[results_farmer_sec_plot_base] <- lapply(baseline_farmers[results_farmer_sec_plot_base],function(x)x - mean(x,na.rm = T))

df_means_end_F_sec_plot <- array(NA,dim=c(3,11))

for (i in 1:length(results_farmer_sec_plot)){
  df_means_end_F_sec_plot[1,i] <- sum(baseline_farmers[results_farmer_sec_plot[i]], na.rm=T)/(nrow(baseline_farmers)-sum(is.na(baseline_farmers[results_farmer_sec_plot[i]])))
  df_means_end_F_sec_plot[2,i] <- sqrt(var(baseline_farmers[results_farmer_sec_plot[i]], na.rm=T))
  df_means_end_F_sec_plot[3,i] <- nrow(baseline_farmers)-sum(is.na(baseline_farmers[results_farmer_sec_plot[i]]))-sum(is.na(baseline_farmers[results_farmer_sec_plot_base[i]]))+sum(is.na(baseline_farmers[results_farmer_sec_plot[i]])&is.na(baseline_farmers[results_farmer_sec_plot_base[i]]))}

###
#2#
###

baseline_farmers$training_control[baseline_farmers$training==0] <- TRUE
baseline_farmers$training_control[baseline_farmers$training==1] <- FALSE

#8.
index_overallsec_plotF_midT <- icwIndex(xmat=variables_overallsec_plotF_mid,sgroup = baseline_farmers$training_control,revcols = c(3))
baseline_farmers$index_overallsec_plotF_midT <- index_overallsec_plotF_midT$index

index_overallsec_plotF_baseT <- icwIndex(xmat=variables_overallsec_plotF_base,sgroup = baseline_farmers$training_control,revcols = c(3))
baseline_farmers$index_overallsec_plotF_baseT <- index_overallsec_plotF_baseT$index

df_ols_end_F_sec_plot <- array(NA,dim=c(3,3,11))

results_farmer_sec_plot <- c("mid_hybrid"
                             ,"mid_OPV"
                             ,"mid_farmer_saved_seed"
                             ,"mid_Bought_from_agro_input_shop"
                             ,"mid_adoption_onfield"
                             ,"index_overallsec_plotF_midT")
results_farmer_sec_plot_base <- c("hybrid"
                                  ,"OPV"
                                  ,"farmer_saved_seed"
                                  ,"Bought_from_agro_input_shop"
                                  ,"adoption_onfield"
                                  ,"index_overallsec_plotF_baseT")

baseline_farmers[results_farmer_sec_plot_base] <- lapply(baseline_farmers[results_farmer_sec_plot_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_farmer_sec_plot)){
  ols <- lm(as.formula(paste(paste(results_farmer_sec_plot[i],"training*clearing_demeaned*farmer_demeaned",sep="~"),results_farmer_sec_plot_base[i],sep="+")),data=baseline_farmers)
  #ols <- lm(as.formula(paste(results_farmer_sec_plot[i],"training*clearing_demeaned*farmer_demeaned",sep="~")),data=baseline_farmers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_farmers$catchID,type="CR0")

  df_ols_end_F_sec_plot[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_ols_end_F_sec_plot[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_ols_end_F_sec_plot[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]}

###
#3#
###

baseline_farmers$clearing_control[baseline_farmers$clearing==0] <- TRUE
baseline_farmers$clearing_control[baseline_farmers$clearing==1] <- FALSE

#8.
index_overallsec_plotF_midC <- icwIndex(xmat=variables_overallsec_plotF_mid,sgroup = baseline_farmers$clearing_control,revcols = c(3))
baseline_farmers$index_overallsec_plotF_midC <- index_overallsec_plotF_midC$index

index_overallsec_plotF_baseC <- icwIndex(xmat=variables_overallsec_plotF_base,sgroup = baseline_farmers$clearing_control,revcols = c(3))
baseline_farmers$index_overallsec_plotF_baseC <- index_overallsec_plotF_baseC$index

results_farmer_sec_plot <- c("mid_hybrid"
                             ,"mid_OPV"
                             ,"mid_farmer_saved_seed"
                             ,"mid_Bought_from_agro_input_shop"
                             ,"mid_adoption_onfield"
                             ,"index_overallsec_plotF_midC")
results_farmer_sec_plot_base <- c("hybrid"
                                  ,"OPV"
                                  ,"farmer_saved_seed"
                                  ,"Bought_from_agro_input_shop"
                                  ,"adoption_onfield"
                                  ,"index_overallsec_plotF_baseC")

baseline_farmers[results_farmer_sec_plot_base] <- lapply(baseline_farmers[results_farmer_sec_plot_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_farmer_sec_plot)){
  ols <- lm(as.formula(paste(paste(results_farmer_sec_plot[i],"training_demeaned*clearing*farmer_demeaned",sep="~"),results_farmer_sec_plot_base[i],sep="+")),data=baseline_farmers)
  #ols <- lm(as.formula(paste(results_farmer_sec_plot[i],"training_demeaned*clearing*farmer_demeaned",sep="~")),data=baseline_farmers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_farmers$catchID,type="CR0")

  df_ols_end_F_sec_plot[1,2,i] <- coef_test(ols, vcov_cluster)$beta[3]
  df_ols_end_F_sec_plot[2,2,i] <- coef_test(ols, vcov_cluster)$SE[3]
  df_ols_end_F_sec_plot[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]}

###
#4#
###

baseline_farmers$farmer_control[baseline_farmers$farmer==0] <- TRUE
baseline_farmers$farmer_control[baseline_farmers$farmer==1] <- FALSE

#8.
index_overallsec_plotF_midF <- icwIndex(xmat=variables_overallsec_plotF_mid,sgroup = baseline_farmers$farmer_control,revcols = c(3))
baseline_farmers$index_overallsec_plotF_midF <- index_overallsec_plotF_midF$index

index_overallsec_plotF_baseF <- icwIndex(xmat=variables_overallsec_plotF_base,sgroup = baseline_farmers$farmer_control,revcols = c(3))
baseline_farmers$index_overallsec_plotF_baseF <- index_overallsec_plotF_baseF$index

results_farmer_sec_plot <- c("mid_hybrid"
                             ,"mid_OPV"
                             ,"mid_farmer_saved_seed"
                             ,"mid_Bought_from_agro_input_shop"
                             ,"mid_adoption_onfield"
                             ,"index_overallsec_plotF_midF")
results_farmer_sec_plot_base <- c("hybrid"
                                  ,"OPV"
                                  ,"farmer_saved_seed"
                                  ,"Bought_from_agro_input_shop"
                                  ,"adoption_onfield"
                                  ,"index_overallsec_plotF_baseF")

baseline_farmers[results_farmer_sec_plot_base] <- lapply(baseline_farmers[results_farmer_sec_plot_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_farmer_sec_plot)){
  ols <- lm(as.formula(paste(paste(results_farmer_sec_plot[i],"training_demeaned*clearing_demeaned*farmer",sep="~"),results_farmer_sec_plot_base[i],sep="+")),data=baseline_farmers)
  #ols <- lm(as.formula(paste(results_farmer_sec_plot[i],"training_demeaned*clearing_demeaned*farmer",sep="~")),data=baseline_farmers)
  vcov_cluster_shop <- vcovCR(ols,cluster=baseline_farmers$shop_ID,type="CR0")

  #farmer video treatment at village/shop level so no clustering needed
  df_ols_end_F_sec_plot[1,3,i] <- coef_test(ols, vcov_cluster_shop)$beta[4]
  df_ols_end_F_sec_plot[2,3,i] <- coef_test(ols, vcov_cluster_shop)$SE[4]
  df_ols_end_F_sec_plot[3,3,i] <- coef_test(ols, vcov_cluster_shop)$p_Satt[4]}

#Aker, Boumnijel, McClelland, Tierney (2012)
df_farmer_sec_plotT <- data.frame(baseline_farmers$mid_hybrid,baseline_farmers$mid_OPV
                                 ,baseline_farmers$mid_farmer_saved_seed
                                 ,baseline_farmers$mid_Bought_from_agro_input_shop,baseline_farmers$mid_adoption_onfield)
df_farmer_sec_plotC <- df_farmer_sec_plotT
df_farmer_sec_plotF <- df_farmer_sec_plotT
#no overall index

df_ols_end_F_sec_plot_J <- array(NA,dim=c(3,3,11))

results_farmer_sec_plot_J <- c("mid_hybrid","mid_OPV","mid_farmer_saved_seed","mid_Bought_from_agro_input_shop","mid_adoption_onfield")
#no overall index

for (i in 1:length(results_farmer_sec_plot_J)){
  df_ols_end_F_sec_plot_J[3,1,i] <- adjust_p(df_ols_end_F_sec_plot[3,1,i],df_farmer_sec_plotT,i)
  df_ols_end_F_sec_plot_J[3,2,i] <- adjust_p(df_ols_end_F_sec_plot[3,2,i],df_farmer_sec_plotC,i)
  df_ols_end_F_sec_plot_J[3,3,i] <- adjust_p(df_ols_end_F_sec_plot[3,3,i],df_farmer_sec_plotF,i)}










################################################################################################################################################################################
##### 12 ANALYSIS: Farmer - Secondary: Seed on plot ############################################################################################################################
################################################################################################################################################################################

#1. seed on plot rating
baseline_farmers$mid_Check2.check.maize.q35a <- as.numeric(as.character(baseline_farmers$CHECK.MAIZE.Q35A))
baseline_farmers$mid_Check2.check.maize.q35b <- as.numeric(as.character(baseline_farmers$CHECK.MAIZE.Q35B))
baseline_farmers$mid_Check2.check.maize.q35c <- as.numeric(as.character(baseline_farmers$CHECK.MAIZE.Q35C))
baseline_farmers$mid_Check2.check.maize.q35d <- as.numeric(as.character(baseline_farmers$CHECK.MAIZE.Q35D))
baseline_farmers$mid_Check2.check.maize.q35e <- as.numeric(as.character(baseline_farmers$CHECK.MAIZE.Q35E))
baseline_farmers$mid_Check2.check.maize.q35f <- as.numeric(as.character(baseline_farmers$CHECK.MAIZE.Q35F)) #no g in endline
baseline_farmers$mid_Check2.check.maize.q35h <- as.numeric(as.character(baseline_farmers$CHECK.MAIZE.Q35H))
baseline_farmers$mid_Check2.check.maize.q35i <- as.numeric(as.character(baseline_farmers$CHECK.MAIZE.Q35I))
baseline_farmers$mid_Check2.check.maize.q35j <- as.numeric(as.character(baseline_farmers$CHECK.MAIZE.Q35J))

variables_ratingplot_mid <- cbind(baseline_farmers$mid_Check2.check.maize.q35a,baseline_farmers$mid_Check2.check.maize.q35b,baseline_farmers$mid_Check2.check.maize.q35c,
                                  baseline_farmers$mid_Check2.check.maize.q35d,baseline_farmers$mid_Check2.check.maize.q35e,
                                  baseline_farmers$mid_Check2.check.maize.q35j)
variables_ratingplot_base <- cbind(baseline_farmers$Check2.check.maize.q35a,baseline_farmers$Check2.check.maize.q35b,baseline_farmers$Check2.check.maize.q35c,
                                   baseline_farmers$Check2.check.maize.q35d,baseline_farmers$Check2.check.maize.q35e,
                                   baseline_farmers$Check2.check.maize.q35j)

index_ratingplot_mid <- icwIndex(xmat=variables_ratingplot_mid)
baseline_farmers$index_ratingplot_mid <- index_ratingplot_mid$index

index_ratingplot_base <- icwIndex(xmat=variables_ratingplot_base)
baseline_farmers$index_ratingplot_base <- index_ratingplot_base$index

#2. satisfied
baseline_farmers$mid_Check2.check.maize.q36 <- baseline_farmers$CHECK.MAIZE.Q36
baseline_farmers$mid_Check2.check.maize.q36<-ifelse(baseline_farmers$mid_Check2.check.maize.q36=="Yes",1,0)

#3. use again
baseline_farmers$mid_Check2.check.maize.q37 <- baseline_farmers$CHECK.MAIZE.Q37
baseline_farmers$mid_Check2.check.maize.q37<-ifelse(baseline_farmers$mid_Check2.check.maize.q37=="Yes",1,0)

#4. Q38. How much seed did you use  on **${plot_select_name}** in the second season (entoigo) of 2020? **(in kg)**?
baseline_farmers$Check2.check.maize.q38_untrimmed <- baseline_farmers$Check2.check.maize.q38
baseline_farmers <- trim("Check2.check.maize.q38",baseline_farmers,trim_perc=.05)

baseline_farmers$mid_Check2.check.maize.q38 <- as.numeric(as.character(baseline_farmers$CHECK.MAIZE.Q38))
baseline_farmers$mid_Check2.check.maize.q38_untrimmed <- baseline_farmers$mid_Check2.check.maize.q38
baseline_farmers <- trim("mid_Check2.check.maize.q38",baseline_farmers,trim_perc=.05)

#5. Q39. How much was the cost of 1 kg of this seed? (in UGX)
baseline_farmers$Check2.check.maize.q39[baseline_farmers$Check2.check.maize.q32=="a"] <- 0
baseline_farmers$Check2.check.maize.q39_untrimmed <- baseline_farmers$Check2.check.maize.q39
baseline_farmers <- trim("Check2.check.maize.q39",baseline_farmers,trim_perc=.05)

baseline_farmers$mid_Check2.check.maize.q39 <- baseline_farmers$CHECK.MAIZE.Q39
baseline_farmers$mid_Check2.check.maize.q39 <- as.numeric(as.character(baseline_farmers$mid_Check2.check.maize.q39))
baseline_farmers$mid_Check2.check.maize.q39[baseline_farmers$mid_Check2.check.maize.q32=="a"] <- 0
baseline_farmers$mid_Check2.check.maize.q39_untrimmed <- baseline_farmers$mid_Check2.check.maize.q39
baseline_farmers <- trim("mid_Check2.check.maize.q39",baseline_farmers,trim_perc=.05)

#6. cost
baseline_farmers$costforseed_new <- baseline_farmers$Check2.check.maize.q38_untrimmed*baseline_farmers$Check2.check.maize.q39_untrimmed

baseline_farmers$costforseed_new_untrimmed <- baseline_farmers$costforseed_new
baseline_farmers <- trim("costforseed_new",baseline_farmers,trim_perc=.05)
baseline_farmers$costforseed_new <- ihs(baseline_farmers$costforseed_new)

baseline_farmers$mid_costforseed_new <- baseline_farmers$mid_Check2.check.maize.q38_untrimmed*baseline_farmers$mid_Check2.check.maize.q39_untrimmed
baseline_farmers <- trim("mid_costforseed_new",baseline_farmers,trim_perc=.05)
baseline_farmers$mid_costforseed_new <- ihs(baseline_farmers$mid_costforseed_new)

#7. overall
variables_overall_seedonplot_mid <- cbind(baseline_farmers$index_ratingplot_mid,baseline_farmers$mid_Check2.check.maize.q36
                                          ,baseline_farmers$mid_Check2.check.maize.q37,baseline_farmers$mid_Check2.check.maize.q38
                                          ,baseline_farmers$mid_Check2.check.maize.q39)
variables_overall_seedonplot_base <- cbind(baseline_farmers$index_ratingplot_base,baseline_farmers$Check2.check.maize.q36
                                           ,baseline_farmers$Check2.check.maize.q37,baseline_farmers$Check2.check.maize.q38
                                           ,baseline_farmers$Check2.check.maize.q39)

index_overall_seedonplot_mid <- icwIndex(xmat=variables_overall_seedonplot_mid,revcols = c(4))
baseline_farmers$index_overall_seedonplot_mid <- index_overall_seedonplot_mid$index

index_overall_seedonplot_base <- icwIndex(xmat=variables_overall_seedonplot_base,revcols = c(4))
baseline_farmers$index_overall_seedonplot_base <- index_overall_seedonplot_base$index

################################################################################################################################################################################

###
#1#
###

results_farmer_sec_seed <- c("index_ratingplot_mid"
                             ,"mid_Check2.check.maize.q36"
                             ,"mid_Check2.check.maize.q37"
                             ,"mid_Check2.check.maize.q38"
                             ,"mid_Check2.check.maize.q39"
                             ,"mid_costforseed_new"
                             ,"index_overall_seedonplot_mid")
results_farmer_sec_seed_base <- c("index_ratingplot_base"
                                  ,"Check2.check.maize.q36"
                                  ,"Check2.check.maize.q37"
                                  ,"Check2.check.maize.q38"
                                  ,"Check2.check.maize.q39"
                                  ,"costforseed_new"
                                  ,"index_overall_seedonplot_base")

baseline_farmers[results_farmer_sec_seed_base] <- lapply(baseline_farmers[results_farmer_sec_seed_base],function(x)x - mean(x,na.rm = T))

df_means_end_F_sec_seed <- array(NA,dim=c(5,11))

for (i in 1:length(results_farmer_sec_seed)){
  df_means_end_F_sec_seed[1,i] <- sum(baseline_farmers[results_farmer_sec_seed[i]], na.rm=T)/(nrow(baseline_farmers)-sum(is.na(baseline_farmers[results_farmer_sec_seed[i]])))
  df_means_end_F_sec_seed[2,i] <- sqrt(var(baseline_farmers[results_farmer_sec_seed[i]], na.rm=T))
  df_means_end_F_sec_seed[3,i] <- nrow(baseline_farmers)-sum(is.na(baseline_farmers[results_farmer_sec_seed[i]]))-sum(is.na(baseline_farmers[results_farmer_sec_seed_base[i]]))+sum(is.na(baseline_farmers[results_farmer_sec_seed[i]])&is.na(baseline_farmers[results_farmer_sec_seed_base[i]]))
  df_means_end_F_sec_seed[4,i] <- min(baseline_farmers[results_farmer_sec_seed[i]], na.rm=T)
  df_means_end_F_sec_seed[5,i] <- max(baseline_farmers[results_farmer_sec_seed[i]], na.rm=T)}

###
#2#
###

baseline_farmers$training_control[baseline_farmers$training==0] <- TRUE
baseline_farmers$training_control[baseline_farmers$training==1] <- FALSE

#1.
index_ratingplot_midT <- icwIndex(xmat=variables_ratingplot_mid,sgroup = baseline_farmers$training_control)
baseline_farmers$index_ratingplot_midT <- index_ratingplot_midT$index

index_ratingplot_baseT <- icwIndex(xmat=variables_ratingplot_base,sgroup = baseline_farmers$training_control)
baseline_farmers$index_ratingplot_baseT <- index_ratingplot_baseT$index

#7.
index_overall_seedonplot_midT <- icwIndex(xmat=variables_overall_seedonplot_mid,sgroup = baseline_farmers$training_control,revcols = c(4))
baseline_farmers$index_overall_seedonplot_midT <- index_overall_seedonplot_midT$index

index_overall_seedonplot_baseT <- icwIndex(xmat=variables_overall_seedonplot_base,sgroup = baseline_farmers$training_control,revcols = c(4))
baseline_farmers$index_overall_seedonplot_baseT <- index_overall_seedonplot_baseT$index

df_ols_end_F_sec_seed <- array(NA,dim=c(3,3,11))

results_farmer_sec_seed <- c("index_ratingplot_midT"
                             ,"mid_Check2.check.maize.q36"
                             ,"mid_Check2.check.maize.q37"
                             ,"mid_Check2.check.maize.q38"
                             ,"mid_Check2.check.maize.q39"
                             ,"mid_costforseed_new"
                             ,"index_overall_seedonplot_midT")
results_farmer_sec_seed_base <- c("index_ratingplot_baseT"
                                  ,"Check2.check.maize.q36"
                                  ,"Check2.check.maize.q37"
                                  ,"Check2.check.maize.q38"
                                  ,"Check2.check.maize.q39"
                                  ,"costforseed_new"
                                  ,"index_overall_seedonplot_baseT")

baseline_farmers[results_farmer_sec_seed_base] <- lapply(baseline_farmers[results_farmer_sec_seed_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_farmer_sec_seed)){
  ols <- lm(as.formula(paste(paste(results_farmer_sec_seed[i],"training*clearing_demeaned*farmer_demeaned",sep="~"),results_farmer_sec_seed_base[i],sep="+")),data=baseline_farmers)
  #ols <- lm(as.formula(paste(results_farmer_sec_seed[i],"training*clearing_demeaned*farmer_demeaned",sep="~")),data=baseline_farmers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_farmers$catchID,type="CR0")

  df_ols_end_F_sec_seed[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_ols_end_F_sec_seed[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_ols_end_F_sec_seed[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]}

###
#3#
###

baseline_farmers$clearing_control[baseline_farmers$clearing==0] <- TRUE
baseline_farmers$clearing_control[baseline_farmers$clearing==1] <- FALSE

#1.
index_ratingplot_midC <- icwIndex(xmat=variables_ratingplot_mid,sgroup = baseline_farmers$clearing_control)
baseline_farmers$index_ratingplot_midC <- index_ratingplot_midC$index

index_ratingplot_baseC <- icwIndex(xmat=variables_ratingplot_base,sgroup = baseline_farmers$clearing_control)
baseline_farmers$index_ratingplot_baseC <- index_ratingplot_baseC$index

#7.
index_overall_seedonplot_midC <- icwIndex(xmat=variables_overall_seedonplot_mid,sgroup = baseline_farmers$clearing_control,revcols = c(4))
baseline_farmers$index_overall_seedonplot_midC <- index_overall_seedonplot_midC$index

index_overall_seedonplot_baseC <- icwIndex(xmat=variables_overall_seedonplot_base,sgroup = baseline_farmers$clearing_control,revcols = c(4))
baseline_farmers$index_overall_seedonplot_baseC <- index_overall_seedonplot_baseC$index

results_farmer_sec_seed <- c("index_ratingplot_midC"
                             ,"mid_Check2.check.maize.q36"
                             ,"mid_Check2.check.maize.q37"
                             ,"mid_Check2.check.maize.q38"
                             ,"mid_Check2.check.maize.q39"
                             ,"mid_costforseed_new"
                             ,"index_overall_seedonplot_midC")
results_farmer_sec_seed_base <- c("index_ratingplot_baseC"
                                  ,"Check2.check.maize.q36"
                                  ,"Check2.check.maize.q37"
                                  ,"Check2.check.maize.q38"
                                  ,"Check2.check.maize.q39"
                                  ,"costforseed_new"
                                  ,"index_overall_seedonplot_baseC")

baseline_farmers[results_farmer_sec_seed_base] <- lapply(baseline_farmers[results_farmer_sec_seed_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_farmer_sec_seed)){
  ols <- lm(as.formula(paste(paste(results_farmer_sec_seed[i],"training_demeaned*clearing*farmer_demeaned",sep="~"),results_farmer_sec_seed_base[i],sep="+")),data=baseline_farmers)
  #ols <- lm(as.formula(paste(results_farmer_sec_seed[i],"training_demeaned*clearing*farmer_demeaned",sep="~")),data=baseline_farmers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_farmers$catchID,type="CR0")

  df_ols_end_F_sec_seed[1,2,i] <- coef_test(ols, vcov_cluster)$beta[3]
  df_ols_end_F_sec_seed[2,2,i] <- coef_test(ols, vcov_cluster)$SE[3]
  df_ols_end_F_sec_seed[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]}

###
#4#
###

baseline_farmers$farmer_control[baseline_farmers$farmer==0] <- TRUE
baseline_farmers$farmer_control[baseline_farmers$farmer==1] <- FALSE

#1.
index_ratingplot_midF <- icwIndex(xmat=variables_ratingplot_mid,sgroup = baseline_farmers$farmer_control)
baseline_farmers$index_ratingplot_midF <- index_ratingplot_midF$index

index_ratingplot_baseF <- icwIndex(xmat=variables_ratingplot_base,sgroup = baseline_farmers$farmer_control)
baseline_farmers$index_ratingplot_baseF <- index_ratingplot_baseF$index

#7.
index_overall_seedonplot_midF <- icwIndex(xmat=variables_overall_seedonplot_mid,sgroup = baseline_farmers$farmer_control,revcols = c(4))
baseline_farmers$index_overall_seedonplot_midF <- index_overall_seedonplot_midF$index

index_overall_seedonplot_baseF <- icwIndex(xmat=variables_overall_seedonplot_base,sgroup = baseline_farmers$farmer_control,revcols = c(4))
baseline_farmers$index_overall_seedonplot_baseF <- index_overall_seedonplot_baseF$index

results_farmer_sec_seed <- c("index_ratingplot_midF"
                             ,"mid_Check2.check.maize.q36"
                             ,"mid_Check2.check.maize.q37"
                             ,"mid_Check2.check.maize.q38"
                             ,"mid_Check2.check.maize.q39"
                             ,"mid_costforseed_new"
                             ,"index_overall_seedonplot_midF")
results_farmer_sec_seed_base <- c("index_ratingplot_baseF"
                                  ,"Check2.check.maize.q36"
                                  ,"Check2.check.maize.q37"
                                  ,"Check2.check.maize.q38"
                                  ,"Check2.check.maize.q39"
                                  ,"costforseed_new"
                                  ,"index_overall_seedonplot_baseF")

baseline_farmers[results_farmer_sec_seed_base] <- lapply(baseline_farmers[results_farmer_sec_seed_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_farmer_sec_seed)){
  ols <- lm(as.formula(paste(paste(results_farmer_sec_seed[i],"training_demeaned*clearing_demeaned*farmer",sep="~"),results_farmer_sec_seed_base[i],sep="+")),data=baseline_farmers)
  #ols <- lm(as.formula(paste(results_farmer_sec_seed[i],"training_demeaned*clearing_demeaned*farmer",sep="~")),data=baseline_farmers)
  vcov_cluster_shop <- vcovCR(ols,cluster=baseline_farmers$shop_ID,type="CR0")

  #farmer video treatment at village/shop level so no clustering needed
  df_ols_end_F_sec_seed[1,3,i] <- coef_test(ols, vcov_cluster_shop)$beta[4]
  df_ols_end_F_sec_seed[2,3,i] <- coef_test(ols, vcov_cluster_shop)$SE[4]
  df_ols_end_F_sec_seed[3,3,i] <- coef_test(ols, vcov_cluster_shop)$p_Satt[4]}

#Aker, Boumnijel, McClelland, Tierney (2012)
df_farmer_sec_seedT <- data.frame(baseline_farmers$index_ratingplot_midT,baseline_farmers$mid_Check2.check.maize.q36
                                  ,baseline_farmers$mid_Check2.check.maize.q37,baseline_farmers$mid_Check2.check.maize.q38
                                  ,baseline_farmers$mid_Check2.check.maize.q39,baseline_farmers$mid_costforseed_new)
df_farmer_sec_seedC <- data.frame(baseline_farmers$index_ratingplot_midC,baseline_farmers$mid_Check2.check.maize.q36
                                  ,baseline_farmers$mid_Check2.check.maize.q37,baseline_farmers$mid_Check2.check.maize.q38
                                  ,baseline_farmers$mid_Check2.check.maize.q39,baseline_farmers$mid_costforseed_new)
df_farmer_sec_seedF <- data.frame(baseline_farmers$index_ratingplot_midF,baseline_farmers$mid_Check2.check.maize.q36
                                  ,baseline_farmers$mid_Check2.check.maize.q37,baseline_farmers$mid_Check2.check.maize.q38
                                  ,baseline_farmers$mid_Check2.check.maize.q39,baseline_farmers$mid_costforseed_new)
#no overall index

df_ols_end_F_sec_seed_J <- array(NA,dim=c(3,3,11))

results_farmer_sec_seed_J <- c("index_ratingplot_mid","mid_Check2.check.maize.q36","mid_Check2.check.maize.q37"
                               ,"mid_Check2.check.maize.q38","mid_Check2.check.maize.q39","mid_costforseed_new")
#no overall index

for (i in 1:length(results_farmer_sec_seed_J)){
  df_ols_end_F_sec_seed_J[3,1,i] <- adjust_p(df_ols_end_F_sec_seed[3,1,i],df_farmer_sec_seedT,i)
  df_ols_end_F_sec_seed_J[3,2,i] <- adjust_p(df_ols_end_F_sec_seed[3,2,i],df_farmer_sec_seedC,i)
  df_ols_end_F_sec_seed_J[3,3,i] <- adjust_p(df_ols_end_F_sec_seed[3,3,i],df_farmer_sec_seedF,i)}










################################################################################################################################################################################
##### 13 ANALYSIS: Farmer - Secondary: Yield etc. ##############################################################################################################################
################################################################################################################################################################################

#1. Production in kg
baseline_farmers$yield_inkg_untrimmed <- baseline_farmers$yield_inkg
baseline_farmers <- trim("yield_inkg",baseline_farmers,trim_perc=.05)

baseline_farmers$mid_Check2.check.maize.q50 <- as.numeric(as.character(baseline_farmers$CHECK.MAIZE.Q50))
baseline_farmers$mid_Check2.check.maize.q51 <- as.numeric(as.character(baseline_farmers$CHECK.MAIZE.Q51))
baseline_farmers$mid_yield_inkg <- baseline_farmers$mid_Check2.check.maize.q50*baseline_farmers$mid_Check2.check.maize.q51
baseline_farmers$mid_yield_inkg_untrimmed <- baseline_farmers$mid_yield_inkg
baseline_farmers <- trim("mid_yield_inkg",baseline_farmers,trim_perc=.05)

#2. yield
baseline_farmers$landproductivity_untrimmed <- baseline_farmers$landproductivity
baseline_farmers <- trim("landproductivity",baseline_farmers,trim_perc=.05)

baseline_farmers$mid_Check2.check.maize.q29 <- as.numeric(as.character(baseline_farmers$CHECK.MAIZE.Q29))
baseline_farmers$mid_landproductivity <- baseline_farmers$mid_yield_inkg_untrimmed/baseline_farmers$mid_Check2.check.maize.q29 #yield in kg per acre
baseline_farmers <- trim("mid_landproductivity",baseline_farmers,trim_perc=.05)

#4. amount sold
baseline_farmers$Check2.check.maize.q54[baseline_farmers$Check2.check.maize.q53==0] <- 0
baseline_farmers$soldinkg <- baseline_farmers$Check2.check.maize.q54*baseline_farmers$Check2.check.maize.q51
baseline_farmers <- trim("soldinkg",baseline_farmers,trim_perc=.05)
baseline_farmers$soldinkg <- ihs(baseline_farmers$soldinkg)

baseline_farmers$mid_Check2.check.maize.q53 <- baseline_farmers$CHECK.MAIZE.Q53
baseline_farmers$mid_Check2.check.maize.q53<-ifelse(baseline_farmers$mid_Check2.check.maize.q53=="Yes",1,0)
baseline_farmers$mid_Check2.check.maize.q54 <- baseline_farmers$CHECK.MAIZE.Q54
baseline_farmers$mid_Check2.check.maize.q54 <- as.numeric(as.character(baseline_farmers$mid_Check2.check.maize.q54))

baseline_farmers$mid_Check2.check.maize.q54[baseline_farmers$mid_Check2.check.maize.q53==0] <- 0
baseline_farmers$mid_soldinkg <- baseline_farmers$mid_Check2.check.maize.q54*baseline_farmers$mid_Check2.check.maize.q51
baseline_farmers <- trim("mid_soldinkg",baseline_farmers,trim_perc=.05)
baseline_farmers$mid_soldinkg <- ihs(baseline_farmers$mid_soldinkg)

#6. revenue
baseline_farmers$revenueUGX[baseline_farmers$Check2.check.maize.q53==0] <- 0
baseline_farmers <- trim("revenueUGX",baseline_farmers,trim_perc=.05)
baseline_farmers$revenueUGX <- ihs(baseline_farmers$revenueUGX)

baseline_farmers$mid_Check2.check.maize.q55 <- baseline_farmers$CHECK.MAIZE.Q55
baseline_farmers$mid_Check2.check.maize.q55 <- as.numeric(as.character(baseline_farmers$mid_Check2.check.maize.q55))

baseline_farmers$mid_revenueUGX <- baseline_farmers$mid_Check2.check.maize.q54*baseline_farmers$mid_Check2.check.maize.q55
baseline_farmers$mid_revenueUGX[baseline_farmers$mid_Check2.check.maize.q53==0] <- 0
baseline_farmers <- trim("mid_revenueUGX",baseline_farmers,trim_perc=.05)
baseline_farmers$mid_revenueUGX <- ihs(baseline_farmers$mid_revenueUGX)

#8.
variables_overall_yieldetc_mid <- cbind(baseline_farmers$mid_landproductivity
                                          ,baseline_farmers$mid_soldinkg,baseline_farmers$mid_revenueUGX)
variables_overall_yieldetc_base <- cbind(baseline_farmers$landproductivity
                                          ,baseline_farmers$soldinkg,baseline_farmers$revenueUGX)

index_overall_yieldetc_mid <- icwIndex(xmat=variables_overall_yieldetc_mid)
baseline_farmers$index_overall_yieldetc_mid <- index_overall_yieldetc_mid$index

index_overall_yieldetc_base <- icwIndex(xmat=variables_overall_yieldetc_base)
baseline_farmers$index_overall_yieldetc_base <- index_overall_yieldetc_base$index

################################################################################################################################################################################

###
#1#
###

results_farmer_sec_yieldetc <- c("mid_yield_inkg"
                                 ,"mid_landproductivity"
                                 ,"mid_soldinkg"
                                 ,"mid_revenueUGX"
                                 ,"index_overall_yieldetc_mid")
results_farmer_sec_yieldetc_base <- c("yield_inkg"
                                      ,"landproductivity"
                                      ,"soldinkg"
                                      ,"revenueUGX"
                                      ,"index_overall_yieldetc_base")

baseline_farmers[results_farmer_sec_yieldetc_base] <- lapply(baseline_farmers[results_farmer_sec_yieldetc_base],function(x)x - mean(x,na.rm = T))

df_means_end_F_sec_yieldetc <- array(NA,dim=c(5,11))

for (i in 1:length(results_farmer_sec_yieldetc)){
  df_means_end_F_sec_yieldetc[1,i] <- sum(baseline_farmers[results_farmer_sec_yieldetc[i]], na.rm=T)/(nrow(baseline_farmers)-sum(is.na(baseline_farmers[results_farmer_sec_yieldetc[i]])))
  df_means_end_F_sec_yieldetc[2,i] <- sqrt(var(baseline_farmers[results_farmer_sec_yieldetc[i]], na.rm=T))
  df_means_end_F_sec_yieldetc[3,i] <- nrow(baseline_farmers)-sum(is.na(baseline_farmers[results_farmer_sec_yieldetc[i]]))-sum(is.na(baseline_farmers[results_farmer_sec_yieldetc_base[i]]))+sum(is.na(baseline_farmers[results_farmer_sec_yieldetc[i]])&is.na(baseline_farmers[results_farmer_sec_yieldetc_base[i]]))
  df_means_end_F_sec_yieldetc[4,i] <- min(baseline_farmers[results_farmer_sec_yieldetc[i]], na.rm=T)
  df_means_end_F_sec_yieldetc[5,i] <- max(baseline_farmers[results_farmer_sec_yieldetc[i]], na.rm=T)}

###
#2#
###

baseline_farmers$training_control[baseline_farmers$training==0] <- TRUE
baseline_farmers$training_control[baseline_farmers$training==1] <- FALSE

#8.
index_overall_yieldetc_midT <- icwIndex(xmat=variables_overall_yieldetc_mid,sgroup = baseline_farmers$training_control)
baseline_farmers$index_overall_yieldetc_midT <- index_overall_yieldetc_midT$index

index_overall_yieldetc_baseT <- icwIndex(xmat=variables_overall_yieldetc_base,sgroup = baseline_farmers$training_control)
baseline_farmers$index_overall_yieldetc_baseT <- index_overall_yieldetc_baseT$index

df_ols_end_F_sec_yieldetc <- array(NA,dim=c(3,3,11))

results_farmer_sec_yieldetc <- c("mid_yield_inkg"
                                 ,"mid_landproductivity"
                                 ,"mid_soldinkg"
                                 ,"mid_revenueUGX"
                                 ,"index_overall_yieldetc_midT")
results_farmer_sec_yieldetc_base <- c("yield_inkg"
                                      ,"landproductivity"
                                      ,"soldinkg"
                                      ,"revenueUGX"
                                      ,"index_overall_yieldetc_baseT")

baseline_farmers[results_farmer_sec_yieldetc_base] <- lapply(baseline_farmers[results_farmer_sec_yieldetc_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_farmer_sec_yieldetc)){
  ols <- lm(as.formula(paste(paste(results_farmer_sec_yieldetc[i],"training*clearing_demeaned*farmer_demeaned",sep="~"),results_farmer_sec_yieldetc_base[i],sep="+")),data=baseline_farmers)
  #ols <- lm(as.formula(paste(results_farmer_sec_yieldetc[i],"training*clearing_demeaned*farmer_demeaned",sep="~")),data=baseline_farmers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_farmers$catchID,type="CR0")

  df_ols_end_F_sec_yieldetc[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_ols_end_F_sec_yieldetc[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_ols_end_F_sec_yieldetc[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]}

###
#3#
###

baseline_farmers$clearing_control[baseline_farmers$clearing==0] <- TRUE
baseline_farmers$clearing_control[baseline_farmers$clearing==1] <- FALSE

#8.
index_overall_yieldetc_midC <- icwIndex(xmat=variables_overall_yieldetc_mid,sgroup = baseline_farmers$clearing_control)
baseline_farmers$index_overall_yieldetc_midC <- index_overall_yieldetc_midC$index

index_overall_yieldetc_baseC <- icwIndex(xmat=variables_overall_yieldetc_base,sgroup = baseline_farmers$clearing_control)
baseline_farmers$index_overall_yieldetc_baseC <- index_overall_yieldetc_baseC$index

results_farmer_sec_yieldetc <- c("mid_yield_inkg"
                                 ,"mid_landproductivity"
                                 ,"mid_soldinkg"
                                 ,"mid_revenueUGX"
                                 ,"index_overall_yieldetc_midC")
results_farmer_sec_yieldetc_base <- c("yield_inkg"
                                      ,"landproductivity"
                                      ,"soldinkg"
                                      ,"revenueUGX"
                                      ,"index_overall_yieldetc_baseC")

baseline_farmers[results_farmer_sec_yieldetc_base] <- lapply(baseline_farmers[results_farmer_sec_yieldetc_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_farmer_sec_yieldetc)){
  ols <- lm(as.formula(paste(paste(results_farmer_sec_yieldetc[i],"training_demeaned*clearing*farmer_demeaned",sep="~"),results_farmer_sec_yieldetc_base[i],sep="+")),data=baseline_farmers)
  #ols <- lm(as.formula(paste(results_farmer_sec_yieldetc[i],"training_demeaned*clearing*farmer_demeaned",sep="~")),data=baseline_farmers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_farmers$catchID,type="CR0")

  df_ols_end_F_sec_yieldetc[1,2,i] <- coef_test(ols, vcov_cluster)$beta[3]
  df_ols_end_F_sec_yieldetc[2,2,i] <- coef_test(ols, vcov_cluster)$SE[3]
  df_ols_end_F_sec_yieldetc[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]}

###
#4#
###

baseline_farmers$farmer_control[baseline_farmers$farmer==0] <- TRUE
baseline_farmers$farmer_control[baseline_farmers$farmer==1] <- FALSE

#8.
index_overall_yieldetc_midF <- icwIndex(xmat=variables_overall_yieldetc_mid,sgroup = baseline_farmers$farmer_control)
baseline_farmers$index_overall_yieldetc_midF <- index_overall_yieldetc_midF$index

index_overall_yieldetc_baseF <- icwIndex(xmat=variables_overall_yieldetc_base,sgroup = baseline_farmers$farmer_control)
baseline_farmers$index_overall_yieldetc_baseF <- index_overall_yieldetc_baseF$index

results_farmer_sec_yieldetc <- c("mid_yield_inkg"
                                 ,"mid_landproductivity"
                                 ,"mid_soldinkg"
                                 ,"mid_revenueUGX"
                                 ,"index_overall_yieldetc_midF")
results_farmer_sec_yieldetc_base <- c("yield_inkg"
                                      ,"landproductivity"
                                      ,"soldinkg"
                                      ,"revenueUGX"
                                      ,"index_overall_yieldetc_baseF")

baseline_farmers[results_farmer_sec_yieldetc_base] <- lapply(baseline_farmers[results_farmer_sec_yieldetc_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_farmer_sec_yieldetc)){
  ols <- lm(as.formula(paste(paste(results_farmer_sec_yieldetc[i],"training_demeaned*clearing_demeaned*farmer",sep="~"),results_farmer_sec_yieldetc_base[i],sep="+")),data=baseline_farmers)
  #ols <- lm(as.formula(paste(results_farmer_sec_yieldetc[i],"training_demeaned*clearing_demeaned*farmer",sep="~")),data=baseline_farmers)
  vcov_cluster_shop <- vcovCR(ols,cluster=baseline_farmers$shop_ID,type="CR0")

  #farmer video treatment at village/shop level so no clustering needed
  df_ols_end_F_sec_yieldetc[1,3,i] <- coef_test(ols, vcov_cluster_shop)$beta[4]
  df_ols_end_F_sec_yieldetc[2,3,i] <- coef_test(ols, vcov_cluster_shop)$SE[4]
  df_ols_end_F_sec_yieldetc[3,3,i] <- coef_test(ols, vcov_cluster_shop)$p_Satt[4]}

#Aker, Boumnijel, McClelland, Tierney (2012)
df_farmer_sec_yieldetcT <- data.frame(baseline_farmers$mid_yield_inkg,baseline_farmers$mid_landproductivity
                                      ,baseline_farmers$mid_soldinkg,baseline_farmers$mid_revenueUGX)
df_farmer_sec_yieldetcC <- df_farmer_sec_yieldetcT
df_farmer_sec_yieldetcF <- df_farmer_sec_yieldetcT
#no overall index

df_ols_end_F_sec_yieldetc_J <- array(NA,dim=c(3,3,11))

results_farmer_sec_yieldetc_J <- c("mid_yield_inkg","mid_landproductivity"
                                   ,"mid_soldinkg","mid_revenueUGX")
#no overall index

for (i in 1:length(results_farmer_sec_yieldetc_J)){
  df_ols_end_F_sec_yieldetc_J[3,1,i] <- adjust_p(df_ols_end_F_sec_yieldetc[3,1,i],df_farmer_sec_yieldetcT,i)
  df_ols_end_F_sec_yieldetc_J[3,2,i] <- adjust_p(df_ols_end_F_sec_yieldetc[3,2,i],df_farmer_sec_yieldetcC,i)
  df_ols_end_F_sec_yieldetc_J[3,3,i] <- adjust_p(df_ols_end_F_sec_yieldetc[3,3,i],df_farmer_sec_yieldetcF,i)}










################################################################################################################################################################################
##### 9 ANALYSIS: Farmer - Primary and secondary: outcomes without baseline#####################################################################################################
################################################################################################################################################################################

#1. index of seed quality perception: average ratings of maize seed of all input dealers in catchment area (q68, q69 aggregated at household level) OK!
baseline_farmers$mid_seed_quality_general_rating=baseline_farmers$end_seed_quality_general_rating
baseline_farmers$mid_seed_yield_rating=baseline_farmers$end_seed_yield_rating
baseline_farmers$mid_seed_drought_rating=baseline_farmers$end_seed_drought_rating
baseline_farmers$mid_seed_disease_rating=baseline_farmers$end_seed_disease_rating
baseline_farmers$mid_seed_maturing_rating=baseline_farmers$end_seed_maturing_rating
baseline_farmers$mid_seed_germinate_rating=baseline_farmers$end_seed_germinate_rating

variables_ratingsF_mid <- cbind(baseline_farmers$mid_seed_quality_general_rating,baseline_farmers$mid_seed_yield_rating
                                ,baseline_farmers$mid_seed_drought_rating,baseline_farmers$mid_seed_disease_rating
                                ,baseline_farmers$mid_seed_maturing_rating,baseline_farmers$mid_seed_germinate_rating)

index_ratingsF_mid <- icwIndex(xmat=variables_ratingsF_mid)
baseline_farmers$index_ratingsF_mid <- index_ratingsF_mid$index

# baseline_farmers$index_ratingsF_mid <- rowMeans(baseline_farmers[c("mid_seed_quality_general_rating","mid_seed_yield_rating",
#                                                                     "mid_seed_drought_rating","mid_seed_disease_rating",
#                                                                     "mid_seed_maturing_rating","mid_seed_germinate_rating")],na.rm = T)

#2. Shop ratings OK!
baseline_farmers$mid_general_rating=baseline_farmers$end_general_rating
baseline_farmers$mid_location_rating=baseline_farmers$end_location_rating
baseline_farmers$mid_price_rating=baseline_farmers$end_price_rating
baseline_farmers$mid_quality_rating=baseline_farmers$end_quality_rating
baseline_farmers$mid_stock_rating=baseline_farmers$end_stock_rating
baseline_farmers$mid_reputation_rating=baseline_farmers$end_reputation_rating

variables_ratingsshopF_mid <- cbind(baseline_farmers$mid_general_rating,baseline_farmers$mid_location_rating
                                    ,baseline_farmers$mid_price_rating,baseline_farmers$mid_quality_rating
                                    ,baseline_farmers$mid_stock_rating,baseline_farmers$mid_reputation_rating)

index_ratingsshopF_mid <- icwIndex(xmat=variables_ratingsshopF_mid)
baseline_farmers$index_ratingsshopF_mid <- index_ratingsshopF_mid$index

#3. switching OK!
baseline_farmers$mid_farmerswitched<-ifelse(baseline_farmers$CHECK.MAIZE.Q25I=="2",1,0)
#baseline_farmers$mid_farmerswitched[baseline_farmers$CHECK.MAIZE.Q25I=="3"]<-NA
baseline_farmers$mid_farmerswitched[is.na(baseline_farmers$CHECK.MAIZE.Q25I)] <- 0
baseline_farmers$mid_farmerswitched[baseline_farmers$attrition_ind_F_end==1] <- NA

#4. Did you harvest as much maize from this plot as you expected? OK!
baseline_farmers$mid_Check2.check.maize.q51a <- baseline_farmers$CHECK.MAIZE.Q51A
baseline_farmers$mid_Check2.check.maize.q51a<-ifelse(baseline_farmers$mid_Check2.check.maize.q51a=="Yes",1,0)
baseline_farmers$mid_Check2.check.maize.q51a[baseline_farmers$attrition_ind_F_end==1] <- NA

#5. if q51a=no: Why did you not harvest as much maize as you expected? OK!
# a.	the weather was unfortunate
# b.	my planting material was poor
# c.	pests e.g. fall arm worm destroyed my harvest
# d.	my soil was not fertile
# e.	I did not plant on time !
# f.	I did not weed on time !
# g.	I did not manage pests e.g. fall arm worm well !
# h.	I did not apply complimentary inputs like fertilizer !
# i.	Others

baseline_farmers$mid_myownfault <- NA
baseline_farmers$mid_myownfault[baseline_farmers$mid_Check2.check.maize.q51a==1] <- 0

baseline_farmers$mid_Check2.check.maize.q51b <- baseline_farmers$CHECK.MAIZE.Q51B

baseline_farmers$mid_myownfault[baseline_farmers$mid_Check2.check.maize.q51b=="a"] <- 0
baseline_farmers$mid_myownfault[baseline_farmers$mid_Check2.check.maize.q51b=="b"] <- 0
baseline_farmers$mid_myownfault[baseline_farmers$mid_Check2.check.maize.q51b=="c"] <- 0
baseline_farmers$mid_myownfault[baseline_farmers$mid_Check2.check.maize.q51b=="d"] <- 0
baseline_farmers$mid_myownfault[baseline_farmers$mid_Check2.check.maize.q51b=="e"] <- 1
baseline_farmers$mid_myownfault[baseline_farmers$mid_Check2.check.maize.q51b=="f"] <- 1
baseline_farmers$mid_myownfault[baseline_farmers$mid_Check2.check.maize.q51b=="g"] <- 1
baseline_farmers$mid_myownfault[baseline_farmers$mid_Check2.check.maize.q51b=="h"] <- 1
baseline_farmers$mid_myownfault[baseline_farmers$mid_Check2.check.maize.q51b=="i"] <- NA

#6. skill questions OK!
baseline_farmers$q58_correct <- ifelse(baseline_farmers$CHECK.MAIZE.Q58=="c",1,0)
baseline_farmers$q58_correct[baseline_farmers$CHECK.MAIZE.Q58=="b"] <- 1 #robustness

baseline_farmers$q59_correct <- ifelse(baseline_farmers$CHECK.MAIZE.Q59=="c",1,0)

baseline_farmers$q60_correct <- ifelse(baseline_farmers$CHECK.MAIZE.Q60=="d",1,0)
baseline_farmers$q60_correct[baseline_farmers$CHECK.MAIZE.Q60=="c"] <- 1 #robustness

baseline_farmers$q61_correct <- ifelse(baseline_farmers$CHECK.MAIZE.Q61=="c",1,0)
baseline_farmers$q62_correct <- ifelse(baseline_farmers$CHECK.MAIZE.Q62=="c",1,0)
baseline_farmers$q63_correct <- ifelse(baseline_farmers$CHECK.MAIZE.Q63=="a",1,0) #not in index

variables_skills_mid <- cbind(baseline_farmers$q58_correct,baseline_farmers$q59_correct,baseline_farmers$q60_correct
                              ,baseline_farmers$q61_correct,baseline_farmers$q62_correct)

index_skillsF_mid <- icwIndex(xmat=variables_skills_mid)
baseline_farmers$index_skillsF_mid <- index_skillsF_mid$index

#7. Q56. How much did you keep for seed (record in kg)? OK!
baseline_farmers <- trim("Check2.check.maize.q56",baseline_farmers,trim_perc=.05)
baseline_farmers$Check2.check.maize.q56 <- ihs(baseline_farmers$Check2.check.maize.q56)

baseline_farmers$mid_Check2.check.maize.q56 <- baseline_farmers$CHECK.MAIZE.Q56
baseline_farmers$mid_Check2.check.maize.q56 <- as.numeric(as.character(baseline_farmers$mid_Check2.check.maize.q56))
baseline_farmers <- trim("mid_Check2.check.maize.q56",baseline_farmers,trim_perc=.05)
baseline_farmers$mid_Check2.check.maize.q56 <- ihs(baseline_farmers$mid_Check2.check.maize.q56)

#at baseline, q56 was only asked if	q53=Yes (Did you sell any maize that you harvested?) which was wrong, therefore no controlling for bl value

#new overallprimF index because of mid_farmerswitched OK!
variables_overallprimF_mid <- cbind(baseline_farmers$mid_Check2.check.maize.q25a,baseline_farmers$mid_agro
                                    ,baseline_farmers$mid_farmerswitched,baseline_farmers$mid_Land_Races
                                    ,baseline_farmers$index_ratingsF_mid,baseline_farmers$index_ratingsshopF_mid
                                    ,baseline_farmers$index_practices_mid)

index_overallprimF_mid <- icwIndex(xmat=variables_overallprimF_mid,revcols = c(4))
baseline_farmers$index_overallprimF_mid <- index_overallprimF_mid$index

#new index_overallsecF_mid because mid_bought_last_season & index_skillsF_mid OK!
variables_overallsecF_mid <- cbind(baseline_farmers$mid_number_known
                                   ,baseline_farmers$mid_knows_dealer
                                   ,baseline_farmers$mid_bought_last_season
                                   ,baseline_farmers$index_skillsF_mid)

index_overallsecF_mid <- icwIndex(xmat=variables_overallsecF_mid)
baseline_farmers$index_overallsecF_mid <- index_overallsecF_mid$index

#CREATE NEW index_overall_yieldetc_mid BECAUSE 3 MORE VARIABLES OK!
variables_overall_yieldetc_mid <- cbind(baseline_farmers$mid_landproductivity,baseline_farmers$mid_Check2.check.maize.q51a
                                        ,baseline_farmers$mid_myownfault,baseline_farmers$mid_soldinkg
                                        ,baseline_farmers$mid_revenueUGX,baseline_farmers$mid_Check2.check.maize.q56)

index_overall_yieldetc_mid <- icwIndex(xmat=variables_overall_yieldetc_mid,revcols=c(3,6))
baseline_farmers$index_overall_yieldetc_mid <- index_overall_yieldetc_mid$index

################################################################################################################################################################################

###
#1#
###

results_farmer_nobase <- c("index_ratingsF_mid"            #1
                           ,"index_ratingsshopF_mid"       #2
                           ,"mid_farmerswitched"           #3
                           ,"mid_Check2.check.maize.q51a"  #4
                           ,"mid_myownfault"               #5
                           ,"index_skillsF_mid"            #6
                           ,"index_overallprimF_mid"       #7
                           ,"index_overallsecF_mid"        #8
                           ,"index_overallsec_plotF_mid"   #9
                           ,"index_overall_seedonplot_mid" #10
                           ,"index_overall_yieldetc_mid"   #11
                           ,"mid_Check2.check.maize.q56"   #12
                           ,"mid_bought_last_season")      #13

df_means_end_F_nobase <- array(NA,dim=c(5,14))

for (i in 1:length(results_farmer_nobase)){
  df_means_end_F_nobase[1,i] <- sum(baseline_farmers[results_farmer_nobase[i]], na.rm=T)/(nrow(baseline_farmers)-sum(is.na(baseline_farmers[results_farmer_nobase[i]])))
  df_means_end_F_nobase[2,i] <- sqrt(var(baseline_farmers[results_farmer_nobase[i]], na.rm=T))
  df_means_end_F_nobase[3,i] <- nrow(baseline_farmers)-sum(is.na(baseline_farmers[results_farmer_nobase[i]]))
  df_means_end_F_nobase[4,i] <- min(baseline_farmers[results_farmer_nobase[i]], na.rm=T)
  df_means_end_F_nobase[5,i] <- max(baseline_farmers[results_farmer_nobase[i]], na.rm=T)}

###
#2#
###

df_ols_end_F_nobase <- array(NA,dim=c(3,3,13))

baseline_farmers$training_control[baseline_farmers$training==0] <- TRUE
baseline_farmers$training_control[baseline_farmers$training==1] <- FALSE

#1.
index_ratingsF_mid <- icwIndex(xmat=variables_ratingsF_mid,sgroup = baseline_farmers$training_control)
baseline_farmers$index_ratingsF_midT <- index_ratingsF_mid$index

#2.
index_ratingsshopF_mid <- icwIndex(xmat=variables_ratingsshopF_mid,sgroup = baseline_farmers$training_control)
baseline_farmers$index_ratingsshopF_midT <- index_ratingsshopF_mid$index

#skills
index_skillsF_midT <- icwIndex(xmat=variables_skills_mid,sgroup = baseline_farmers$training_control)
baseline_farmers$index_skillsF_midT <- index_skillsF_midT$index

#new overallprimF
index_overallprimF_midT <- icwIndex(xmat=variables_overallprimF_mid,sgroup = baseline_farmers$training_control,revcols = c(4))
baseline_farmers$index_overallprimF_midT <- index_overallprimF_midT$index

#new overallsecF
index_overallsecF_midT <- icwIndex(xmat=variables_overallsecF_mid,sgroup = baseline_farmers$training_control)
baseline_farmers$index_overallsecF_midT <- index_overallsecF_midT$index

#new overall_yieldetc
index_overall_yieldetc_midT <- icwIndex(xmat=variables_overall_yieldetc_mid,sgroup = baseline_farmers$training_control,revcols=c(3,6))
baseline_farmers$index_overall_yieldetc_midT <- index_overall_yieldetc_midT$index

results_farmer_nobase <- c("index_ratingsF_midT"
                           ,"index_ratingsshopF_midT"
                           ,"mid_farmerswitched"
                           ,"mid_Check2.check.maize.q51a"
                           ,"mid_myownfault"
                           ,"index_skillsF_midT"
                           ,"index_overallprimF_midT"
                           ,"index_overallsecF_midT"
                           ,"index_overallsec_plotF_midT"
                           ,"index_overall_seedonplot_midT"
                           ,"index_overall_yieldetc_midT"
                           ,"mid_Check2.check.maize.q56"
                           ,"mid_bought_last_season")

for (i in 1:length(results_farmer_nobase)){
  ols <- lm(as.formula(paste(results_farmer_nobase[i],"training*clearing_demeaned*farmer_demeaned",sep="~")),data=baseline_farmers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_farmers$catchID,type="CR0")

  df_ols_end_F_nobase[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_ols_end_F_nobase[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_ols_end_F_nobase[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]}

###
#3#
###

baseline_farmers$clearing_control[baseline_farmers$clearing==0] <- TRUE
baseline_farmers$clearing_control[baseline_farmers$clearing==1] <- FALSE

#1.
index_ratingsF_mid <- icwIndex(xmat=variables_ratingsF_mid,sgroup = baseline_farmers$clearing_control)
baseline_farmers$index_ratingsF_midC <- index_ratingsF_mid$index

#2.
index_ratingsshopF_mid <- icwIndex(xmat=variables_ratingsshopF_mid,sgroup = baseline_farmers$clearing_control)
baseline_farmers$index_ratingsshopF_midC <- index_ratingsshopF_mid$index

#skills
index_skillsF_midC <- icwIndex(xmat=variables_skills_mid,sgroup = baseline_farmers$clearing_control)
baseline_farmers$index_skillsF_midC <- index_skillsF_midC$index

#new overallprimF
index_overallprimF_midC <- icwIndex(xmat=variables_overallprimF_mid,sgroup = baseline_farmers$clearing_control,revcols = c(4))
baseline_farmers$index_overallprimF_midC <- index_overallprimF_midC$index

#new overallsecF
index_overallsecF_midC <- icwIndex(xmat=variables_overallsecF_mid,sgroup = baseline_farmers$clearing_control)
baseline_farmers$index_overallsecF_midC <- index_overallsecF_midC$index

#new overall_yieldetc
index_overall_yieldetc_midC <- icwIndex(xmat=variables_overall_yieldetc_mid,sgroup = baseline_farmers$clearing_control,revcols=c(3,6))
baseline_farmers$index_overall_yieldetc_midC <- index_overall_yieldetc_midC$index

results_farmer_nobase <- c("index_ratingsF_midC"
                           ,"index_ratingsshopF_midC"
                           ,"mid_farmerswitched"
                           ,"mid_Check2.check.maize.q51a"
                           ,"mid_myownfault"
                           ,"index_skillsF_midC"
                           ,"index_overallprimF_midC"
                           ,"index_overallsecF_midC"
                           ,"index_overallsec_plotF_midC"
                           ,"index_overall_seedonplot_midC"
                           ,"index_overall_yieldetc_midC"
                           ,"mid_Check2.check.maize.q56"
                           ,"mid_bought_last_season")

for (i in 1:length(results_farmer_nobase)){
  ols <- lm(as.formula(paste(results_farmer_nobase[i],"training_demeaned*clearing*farmer_demeaned",sep="~")),data=baseline_farmers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_farmers$catchID,type="CR0")

  df_ols_end_F_nobase[1,2,i] <- coef_test(ols, vcov_cluster)$beta[3]
  df_ols_end_F_nobase[2,2,i] <- coef_test(ols, vcov_cluster)$SE[3]
  df_ols_end_F_nobase[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]}

###
#4#
###

baseline_farmers$farmer_control[baseline_farmers$farmer==0] <- TRUE
baseline_farmers$farmer_control[baseline_farmers$farmer==1] <- FALSE

#1.
index_ratingsF_mid <- icwIndex(xmat=variables_ratingsF_mid,sgroup = baseline_farmers$farmer_control)
baseline_farmers$index_ratingsF_midF <- index_ratingsF_mid$index

#2.
index_ratingsshopF_mid <- icwIndex(xmat=variables_ratingsshopF_mid,sgroup = baseline_farmers$farmer_control)
baseline_farmers$index_ratingsshopF_midF <- index_ratingsshopF_mid$index

#skills
index_skillsF_midF <- icwIndex(xmat=variables_skills_mid,sgroup = baseline_farmers$farmer_control)
baseline_farmers$index_skillsF_midF <- index_skillsF_midF$index

#new overallprimF
index_overallprimF_midF <- icwIndex(xmat=variables_overallprimF_mid,sgroup = baseline_farmers$farmer_control,revcols = c(4))
baseline_farmers$index_overallprimF_midF <- index_overallprimF_midF$index

#new overallsecF
index_overallsecF_midF <- icwIndex(xmat=variables_overallsecF_mid,sgroup = baseline_farmers$farmer_control)
baseline_farmers$index_overallsecF_midF <- index_overallsecF_midF$index

#new overall_yieldetc
index_overall_yieldetc_midF <- icwIndex(xmat=variables_overall_yieldetc_mid,sgroup = baseline_farmers$farmer_control,revcols=c(3,6))
baseline_farmers$index_overall_yieldetc_midF <- index_overall_yieldetc_midF$index

results_farmer_nobase <- c("index_ratingsF_midF"
                           ,"index_ratingsshopF_midF"
                           ,"mid_farmerswitched"
                           ,"mid_Check2.check.maize.q51a"
                           ,"mid_myownfault"
                           ,"index_skillsF_midF"
                           ,"index_overallprimF_midF"
                           ,"index_overallsecF_midF"
                           ,"index_overallsec_plotF_midF"
                           ,"index_overall_seedonplot_midF"
                           ,"index_overall_yieldetc_midF"
                           ,"mid_Check2.check.maize.q56"
                           ,"mid_bought_last_season")

for (i in 1:length(results_farmer_nobase)){
  ols <- lm(as.formula(paste(results_farmer_nobase[i],"training_demeaned*clearing_demeaned*farmer",sep="~")),data=baseline_farmers)
  vcov_cluster_shop <- vcovCR(ols,cluster=baseline_farmers$shop_ID,type="CR0")

  #farmer video treatment at village/shop level so no clustering needed
  df_ols_end_F_nobase[1,3,i] <- coef_test(ols, vcov_cluster_shop)$beta[4]
  df_ols_end_F_nobase[2,3,i] <- coef_test(ols, vcov_cluster_shop)$SE[4]
  df_ols_end_F_nobase[3,3,i] <- coef_test(ols, vcov_cluster_shop)$p_Satt[4]}










#skill questions

###
#1#
###

results_skills <- c("q58_correct"         #1
                    ,"q59_correct"        #2
                    ,"q60_correct"        #3
                    ,"q61_correct"        #4
                    ,"q62_correct"        #5
                    ,"q63_correct"        #6
                    ,"index_skillsF_mid") #7

df_means_end_skills <- array(NA,dim=c(5,14))

for (i in 1:length(results_skills)){
  df_means_end_skills[1,i] <- sum(baseline_farmers[results_skills[i]], na.rm=T)/(nrow(baseline_farmers)-sum(is.na(baseline_farmers[results_skills[i]])))
  df_means_end_skills[2,i] <- sqrt(var(baseline_farmers[results_skills[i]], na.rm=T))
  df_means_end_skills[3,i] <- nrow(baseline_farmers)-sum(is.na(baseline_farmers[results_skills[i]]))
  df_means_end_skills[4,i] <- min(baseline_farmers[results_skills[i]], na.rm=T)
  df_means_end_skills[5,i] <- max(baseline_farmers[results_skills[i]], na.rm=T)}

###
#2#
###

df_ols_end_skills <- array(NA,dim=c(3,3,13))

results_skills <- c("q58_correct"         #1
                    ,"q59_correct"        #2
                    ,"q60_correct"        #3
                    ,"q61_correct"        #4
                    ,"q62_correct"        #5
                    ,"q63_correct"        #6
                    ,"index_skillsF_midT")

for (i in 1:length(results_skills)){
  ols <- lm(as.formula(paste(results_skills[i],"training*clearing_demeaned*farmer_demeaned",sep="~")),data=baseline_farmers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_farmers$catchID,type="CR0")
  
  df_ols_end_skills[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_ols_end_skills[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_ols_end_skills[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]}

###
#3#
###

results_skills <- c("q58_correct"         #1
                    ,"q59_correct"        #2
                    ,"q60_correct"        #3
                    ,"q61_correct"        #4
                    ,"q62_correct"        #5
                    ,"q63_correct"        #6
                    ,"index_skillsF_midC")

for (i in 1:length(results_skills)){
  ols <- lm(as.formula(paste(results_skills[i],"training_demeaned*clearing*farmer_demeaned",sep="~")),data=baseline_farmers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_farmers$catchID,type="CR0")
  
  df_ols_end_skills[1,2,i] <- coef_test(ols, vcov_cluster)$beta[3]
  df_ols_end_skills[2,2,i] <- coef_test(ols, vcov_cluster)$SE[3]
  df_ols_end_skills[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]}

###
#4#
###

results_skills <- c("q58_correct"         #1
                    ,"q59_correct"        #2
                    ,"q60_correct"        #3
                    ,"q61_correct"        #4
                    ,"q62_correct"        #5
                    ,"q63_correct"        #6
                    ,"index_skillsF_midF")

for (i in 1:length(results_skills)){
  ols <- lm(as.formula(paste(results_skills[i],"training_demeaned*clearing_demeaned*farmer",sep="~")),data=baseline_farmers)
  vcov_cluster_shop <- vcovCR(ols,cluster=baseline_farmers$shop_ID,type="CR0")
  
  df_ols_end_skills[1,3,i] <- coef_test(ols, vcov_cluster_shop)$beta[4]
  df_ols_end_skills[2,3,i] <- coef_test(ols, vcov_cluster_shop)$SE[4]
  df_ols_end_skills[3,3,i] <- coef_test(ols, vcov_cluster_shop)$p_Satt[4]}










# #OTHER CALCULATIONS
# #OC1. difference in ratings between dealers and farmers
# 
# #attention: only CH treated farmers
# 
# mean(baseline_dealers$maize.owner.agree.q101,na.rm = T) #4.045977
# mean(baseline_dealers$quality_rating,na.rm = T) #3.759996
# 
# t.test(baseline_dealers$maize.owner.agree.q101,baseline_dealers$quality_rating) #p-value < 2.2e-16
# #dealers rate their seed quality better than farmers do
# 
# 
# 
# #OC2. is improved seed worth it?
# mean(baseline_farmers$landproductivity_untrimmed[baseline_farmers$adoption_onfield==1],na.rm = T) #(harvested bags*kgs/bag)/area in acres
# mean(baseline_farmers$landproductivity_untrimmed[baseline_farmers$adoption_onfield==0],na.rm = T)
# 
# 589.4402/423.5282 #farmers who adopted had 39.1738% more yield per acre
# 
# mean(baseline_farmers$landproductivity_inUGX[baseline_farmers$adoption_onfield==1],na.rm = T) #(harvested bags*market value/bag)/area in acres
# mean(baseline_farmers$landproductivity_inUGX[baseline_farmers$adoption_onfield==0],na.rm = T)
# 
# 412705.3-298563.5 #farmers who adopted earned 114 141.8 UGX more per acre
# 
# baseline_farmers$costforseed_new_untrimmed_peracre <- baseline_farmers$costforseed_new_untrimmed/baseline_farmers$Check2.check.maize.q29
# 
# mean(baseline_farmers$costforseed_new_untrimmed_peracre[baseline_farmers$adoption_onfield==1],na.rm = T) #seed in kg*cost/kg
# mean(baseline_farmers$costforseed_new_untrimmed_peracre[baseline_farmers$adoption_onfield==0],na.rm = T)
# 
# 31699.39-1302.27 #farmers who adopted spent 30397.12 UGX more per acre
# 
# 114141.8/30397.12 #farmers who adopted almost quadrupled (3.75502) their investment
# 
# 
# 

#OC3. Are the seed quality ratings correlated with other measures of seed quality?

#DEALERS
#a) dependent variables

#score
reviews_seed <- read.csv(paste(path,"/endline/data/agro_input/public/reviews_seed.csv",sep="/"))
baseline_dealers <- merge(baseline_dealers,reviews_seed,by.x=c("catchID","shop_ID"),by.y=c("catchID","shop_ID"),all.x=T)
baseline_dealers$ratings1 <- baseline_dealers$score #this one

#score_corrected
baseline_dealers$ratings2 <- baseline_dealers$score_corrected.y

#ratings mean
baseline_dealers$ratings3 <- rowMeans(baseline_dealers[c("end_seed_quality_general_rating","end_seed_yield_rating","end_seed_drought_rating"
                                                         ,"end_seed_disease_rating","end_seed_maturing_rating","end_seed_germinate_rating")],na.rm = T)
#ratings index
baseline_dealers$ratings4 <- baseline_dealers$index_ratings_mid

#b) independent variables

#Shop only sells farm inputs
baseline_dealers$check.owner.agree.q5<-ifelse(baseline_dealers$check.owner.agree.q5=="Yes",1,0)

#midline quality indicators
midline_for.cor.ratings.quality <- read.csv(paste(path,"/midline/data/agro_input/public/midline_for.cor.ratings.quality.csv",sep="/"), stringsAsFactors=TRUE)
baseline_dealers <- merge(baseline_dealers,midline_for.cor.ratings.quality,by.x=c("catchID","shop_ID"),by.y=c("catchID","shop_ID"),all.x=T)

#Random seed bag shows packaging date
baseline_dealers$mid_date_pack <- baseline_dealers$date_pack_end #x
baseline_dealers$mid_visible_packdate<-ifelse(baseline_dealers$mid_date_pack=="n/a",0,1) #x

#Days since packaging date/expiry date minus 6 months
baseline_dealers$mid_exp <- baseline_dealers$exp_end #x
baseline_dealers$mid_visible_expdate<-ifelse(!is.na(baseline_dealers$mid_exp),1,0) #x

baseline_dealers$mid_date <- baseline_dealers$date_end #x
baseline_dealers$mid_date[baseline_dealers$mid_date=="n/a"] <- NA #x
baseline_dealers$mid_date <- as.Date(baseline_dealers$mid_date) #x
baseline_dealers$mid_exp <- as.Date(baseline_dealers$mid_exp) #x
baseline_dealers$mid_days_since_exp <- baseline_dealers$mid_date - baseline_dealers$mid_exp #x
baseline_dealers$mid_date_pack <- as.Date(baseline_dealers$mid_date_pack) #x
baseline_dealers$mid_shelflife <- baseline_dealers$mid_date - baseline_dealers$mid_date_pack #x
baseline_dealers$mid_date_pack_incltransformedexp <- baseline_dealers$mid_date_pack #x
baseline_dealers$mid_transformedexp <- baseline_dealers$mid_exp - 183 #6x366/12 #x
baseline_dealers$mid_date_pack_incltransformedexp[is.na(baseline_dealers$mid_date_pack)]<-baseline_dealers$mid_transformedexp[is.na(baseline_dealers$mid_date_pack)] #x
baseline_dealers$mid_shelflife_Caro <- baseline_dealers$mid_date - as.Date(baseline_dealers$mid_date_pack_incltransformedexp) #x
baseline_dealers$mid_shelflife_Caro[baseline_dealers$mid_shelflife_Caro < 0] <- NA #x
baseline_dealers$mid_shelflife_Caro <- as.numeric(as.character(baseline_dealers$mid_shelflife_Caro)) #x
baseline_dealers <- trim("mid_shelflife_Caro",baseline_dealers,trim_perc=.02) #x
baseline_dealers <- trim("shelflife_Caro",baseline_dealers,trim_perc=.02)

#Seed is in the original bag without any signs of damage
baseline_dealers$mid_origin <- baseline_dealers$origin_end
baseline_dealers$mid_origin<-ifelse(baseline_dealers$mid_origin=="Yes",1,0)

#Random seed bag shows lot number
baseline_dealers$mid_lot <- baseline_dealers$lot_end
baseline_dealers$mid_lot<-ifelse(baseline_dealers$mid_lot=="Yes",1,0)

quality_dealer <- c("midline_specialized.shop","midline_practices_lab","midline_practices_cap","midline_practices_all"
                    ,"midline_received.complaint","midline_received.warning","midline_bag.shows.packaging.date"
                    ,"midline_shelflife","midline_original.bag.without.damage","midline_bag.shows.lotnumber"
                    ,"midline_moisture","check.owner.agree.q5", "index_practices_lab_mid","index_practices_cap_mid","index_practices_all_mid"
                    ,"mid_maize.owner.agree.q96","mid_maize.owner.agree.inspection.q118","mid_visible_packdate","mid_shelflife_Caro"
                    ,"mid_origin","mid_lot","mid_reading")

df_ols_end_quality_dealer <- array(NA,dim=c(3,4,22))

for (i in 1:length(quality_dealer)){
  ols <- lm(as.formula(paste("ratings1",quality_dealer[i],sep="~")), data=baseline_dealers)
  
  df_ols_end_quality_dealer[1,1,i] <- summary(ols)$coefficients[2,1]
  df_ols_end_quality_dealer[2,1,i] <- summary(ols)$coefficients[2,2]
  df_ols_end_quality_dealer[3,1,i] <- summary(ols)$coefficients[2,4]}

for (i in 1:length(quality_dealer)){
  ols <- lm(as.formula(paste("ratings2",quality_dealer[i],sep="~")), data=baseline_dealers)
  
  df_ols_end_quality_dealer[1,2,i] <- summary(ols)$coefficients[2,1]
  df_ols_end_quality_dealer[2,2,i] <- summary(ols)$coefficients[2,2]
  df_ols_end_quality_dealer[3,2,i] <- summary(ols)$coefficients[2,4]}

for (i in 1:length(quality_dealer)){
  ols <- lm(as.formula(paste("ratings3",quality_dealer[i],sep="~")), data=baseline_dealers)
  
  df_ols_end_quality_dealer[1,3,i] <- summary(ols)$coefficients[2,1]
  df_ols_end_quality_dealer[2,3,i] <- summary(ols)$coefficients[2,2]
  df_ols_end_quality_dealer[3,3,i] <- summary(ols)$coefficients[2,4]}

for (i in 1:length(quality_dealer)){
  ols <- lm(as.formula(paste("ratings4",quality_dealer[i],sep="~")), data=baseline_dealers)
  
  df_ols_end_quality_dealer[1,4,i] <- summary(ols)$coefficients[2,1]
  df_ols_end_quality_dealer[2,4,i] <- summary(ols)$coefficients[2,2]
  df_ols_end_quality_dealer[3,4,i] <- summary(ols)$coefficients[2,4]}



#FARMERS
#attention: all seed, not only improved seed

baseline_farmers$yield_endline <- baseline_farmers$mid_landproductivity
baseline_farmers$plotratings_endline <- baseline_farmers$index_ratingplot_mid

#midline rankings
midline_for.cor.ratings.quality_farmers <- read.csv(paste(path,"/midline/data/farmer/public/midline_for.cor.ratings.quality_farmers.csv",sep="/"), stringsAsFactors=TRUE)
baseline_farmers <- merge(baseline_farmers,midline_for.cor.ratings.quality_farmers,by.x=c("farmer_ID"),by.y=c("farmer_ID"),all.x=T)

baseline_farmers$plotratings_midline <- baseline_farmers$plotratings_midline

quality_farmer <- c("plotratings_endline","plotratings_midline")

df_ols_end_quality_farmer <- array(NA,dim=c(3,5))

for (i in 1:length(quality_farmer)){
  ols <- lm(as.formula(paste("yield_endline",quality_farmer[i],sep="~")), data=baseline_farmers)
  
  df_ols_end_quality_farmer[1,i] <- summary(ols)$coefficients[2,1]
  df_ols_end_quality_farmer[2,i] <- summary(ols)$coefficients[2,2]
  df_ols_end_quality_farmer[3,i] <- summary(ols)$coefficients[2,4]}

summary(regression2 <- lm(baseline_farmers$yield_endline~baseline_farmers$plotratings_midline))

# #OC4. Do farmers who bought seed rate significantly different than farmers who didn't buy seed?
# 
# #attention: only CH treated farmers
# 
# # variables_ratingsF <- cbind(rating_dyads$seed_quality_general_rating,rating_dyads$seed_yield_rating
# #                                 ,rating_dyads$seed_drought_rating,rating_dyads$seed_disease_rating
# #                                 ,rating_dyads$seed_maturing_rating,rating_dyads$seed_germinate_rating)
# #
# # index_ratingsF <- icwIndex(xmat=variables_ratingsF)
# # rating_dyads$index_ratingsF <- index_ratingsF$index
# 
# #not straightforward to interpret, so:
# 
# #at rating_dyads level because baseline_farmers$bought_at_dealer and baseline_farmers$bought_last_season are averages
# 
# rating_dyads$index_ratingsF <- rowMeans(rating_dyads[c("seed_quality_general_rating","seed_yield_rating","seed_drought_rating"
#                                                        ,"seed_disease_rating","seed_maturing_rating","seed_germinate_rating")],na.rm = T)
# 
# table(rating_dyads$bought_at_dealer) #3536 No, 807 Yes
# 
# mean(rating_dyads$index_ratingsF[rating_dyads$bought_at_dealer=="Yes"],na.rm = T) #3.390949
# mean(rating_dyads$index_ratingsF[rating_dyads$bought_at_dealer=="No"],na.rm = T) #3.478922
# 
# summary(regression3 <- lm(rating_dyads$index_ratingsF~rating_dyads$bought_at_dealer))
# #farmers who never bought seed at dealer rate significantly better
# 
# rating_dyads$bought_last_season[rating_dyads$bought_at_dealer=="No"] <- 0
# 
# table(rating_dyads$bought_last_season) #3990 No, 353 Yes
# 
# mean(rating_dyads$index_ratingsF[rating_dyads$bought_last_season==1],na.rm = T) #3.354545
# mean(rating_dyads$index_ratingsF[rating_dyads$bought_last_season==0],na.rm = T) #3.438055
# 
# summary(regression4 <- lm(rating_dyads$index_ratingsF~rating_dyads$bought_last_season))
# #farmers who didn't buy seed at dealer last season rate significantly better
# 
# # variables_ratingsF <- cbind(baseline_farmers$seed_quality_general_rating,baseline_farmers$seed_yield_rating
# #                             ,baseline_farmers$seed_drought_rating,baseline_farmers$seed_disease_rating
# #                             ,baseline_farmers$seed_maturing_rating,baseline_farmers$seed_germinate_rating)
# #
# # index_ratingsF <- icwIndex(xmat=variables_ratingsF)
# # baseline_farmers$index_ratingsF <- index_ratingsF$index
# 
# #not straightforward to interpret, so:
# 
# baseline_farmers$index_ratingsF <- rowMeans(baseline_farmers[c("seed_quality_general_rating","seed_yield_rating","seed_drought_rating"
#                                                                ,"seed_disease_rating","seed_maturing_rating","seed_germinate_rating")],na.rm = T)
# 
# #baseline_farmers$agro==1 if farmer used quality maize seed bought at agro-input shop for any plot last season
# #attention: ANY agro-input shop
# mean(baseline_farmers$index_ratingsF[baseline_farmers$agro==1],na.rm = T) #3.371677
# mean(baseline_farmers$index_ratingsF[baseline_farmers$agro==0],na.rm = T) #3.425568
# 
# summary(regression4 <- lm(baseline_farmers$index_ratingsF~baseline_farmers$agro))
# #farmers who used quality maize seed bought at agro-input shop rate worse than those who didn't (but not significantly)
# 
# mean(baseline_farmers$index_ratingsF[baseline_farmers$Check2.check.maize.q25a==1],na.rm = T) #3.377495
# mean(baseline_farmers$index_ratingsF[baseline_farmers$Check2.check.maize.q25a==0],na.rm = T) #3.435884
# 
# summary(regression4 <- lm(baseline_farmers$index_ratingsF~baseline_farmers$Check2.check.maize.q25a))
# #farmers who adopt rate worse than those who don't (but not significantly)
# 
# 
# 
# #OC5. check yellow questions in "variables farmer" (C:\Users\u0127963\Desktop\PhD\Seed_systems_project_without_Bjorn\report)
# table(baseline_farmers$check.maize.q25f) #clearinghouse control group MIDLINE
# table(baseline_farmers$check.maize.q25fx) #clearinghouse treatment group MIDLINE
# 
# #62 farmers didn't buy seed at agro-input shop because it is too far way or in an inconvenient location
# #1024 farmers didn't buy seed at agro-input shop because it is too expensive
# #82 farmers didn't buy seed at agro-input shop because it isn't of good quality
# #15 farmers didn't buy seed at agro-input shop because it is always out of stock/doesn't sell the seed they are looking for
# #8 farmer didn't buy seed at agro-input shop because it sells only in inconvenient quantities
# #1 farmer didn't buy seed at agro-input shop because of other reasons
# #0 farmers didn't buy seed at agro-input shop because shop's SeedAdvisor rating was too low (only for clearinghouse treatment group)
# #=1192
# 
# table(baseline_farmers$check.maize.q25f_2)
# table(baseline_farmers$check.maize.q25f_2x)
# 
# #315 farmers bought seed at particular agro-input shop because it is close by or in an convenient location
# #70 farmers bought seed at particular agro-input shop because it is not very expensive
# #641 farmers bought seed at particular agro-input shop because seed there is of very good quality
# #87 farmers bought seed at particular agro-input shop because it always has a good stock of seed I am looking for
# #56 farmers bought seed at particular agro-input shop because it sells in convenient quantities
# #14 farmers bought seed at particular agro-input shop because it's SeedAdvisor rating was high
# 
# #Q67a. Do you know **${calc_biz}**'s SeedAdvisor rating?
# table(midline_rating_dyads$knows_dealer) #5081 know dealer, 5782 don't
# table(midline_rating_dyads$knows_SA_rating) #67 know rating, 929 don't
# #the next question you had to answer if you answered yes (if q67a=yes: What is  **${calc_biz}**'s SeedAdvisor rating?) might have influenced this answer
# table(midline_rating_dyads$SA_rating)
# 
# 
# 
# #OC6. heterogeneity analysis: only shops which only sell farm inputs (74.1%)
# #paste this before analysis:
# #baseline_dealers=subset(baseline_dealers,maize.owner.agree.q5=="1")
# #also had to change these 348's to 258's but that won't be necessary at endline:
# #e.g. baseline_dealers$mid_general=sample(na.omit(baseline_dealers$general),258,replace = T)
# 
# #new effects:
# #CH on transformed seed revenue in mln UGX (IHS)
# #training and CH on transformed quantity of Longe 10H sold last season in kg (IHS)
# #training on overall index Longe 10H
# #CH on shelflife (days since packaging date/expiry date minus 6 months)
# 
# 
# 
# #OC7. attrition: how many (un-)specialized dealers?
# sum(baseline_dealers$attrition_ind_D==1&baseline_dealers$maize.owner.agree.q5==1)
# #31 of 258 (12%) specialized shops left the sample
# sum(baseline_dealers$attrition_ind_D==1&baseline_dealers$maize.owner.agree.q5==0)
# #11 of 90 (12%) un-specialized shops left the sample
# 
# summary(regression5 <- lm(baseline_dealers$attrition_ind_D~baseline_dealers$maize.owner.agree.q5))
# #shops which only sell farm inputs are not significantly more/less likely to leave the sample
# 
# 
# 
# #OC8. intended to treat vs. treated
# sum(baseline_dealers$training==1 & baseline_dealers$attrition_ind_D==0)
# #147 dealers who didn't leave sample received training
# sum(baseline_dealers$training==0 & baseline_dealers$attrition_ind_D==0)
# #159 dealers who didn't leave sample didn't receive training
# 
# table(baseline_dealers$owner.agree.q11a)
# sum(baseline_dealers$training==1 & baseline_dealers$owner.agree.q11a=="Yes",na.rm = T)
# #120 of 147 (82%) dealers who were supposed to receive training, say that they were invited to our training
# 
# table(baseline_dealers$owner.agree.q11b)
# sum(baseline_dealers$training==1 & baseline_dealers$owner.agree.q11b=="Yes",na.rm = T)
# #105 of 147 (71%) dealers who were supposed to receive training, say that they attended our training
# 
# mean(as.numeric(as.character(baseline_dealers$owner.agree.q11c)),na.rm = T)
# #our training was rated 4.52/5 on average
# 
# #CH
# sum(baseline_dealers$clearing==1 & baseline_dealers$attrition_ind_D==0)
# #179 dealers who didn't leave sample received CH
# sum(baseline_dealers$clearing==0 & baseline_dealers$attrition_ind_D==0)
# #127 dealers who didn't leave sample didn't receive CH
# 
# table(baseline_dealers$owner.agree.q2a)
# sum(baseline_dealers$clearing==1 & baseline_dealers$owner.agree.q2a=="Yes",na.rm = T)
# #124 of 179 dealers (69%) who were supposed to receive certificate say that they did
# 
# table(baseline_dealers$owner.agree.q2b)
# sum(baseline_dealers$clearing==1 & baseline_dealers$owner.agree.q2b=="Yes",na.rm = T)
# #66 of 179 dealers (37%) who were supposed to receive certificate say that they know their shop's rating
# 
# 
# 
# #OC9. read Different-sized baskets of fruit: https://blogs.worldbank.org/impactevaluations/different-sized-baskets-fruit-how-unequally-sized-clusters-can-lead-your-power
# #"if you have a few clusters that are much larger than the rest, you may want to not include them in the experiment"
# #exclude catchment areas with more than 10 dealers/villages and redo analysis
# table(baseline_dealers$catchID)
# table(baseline_farmers$catchID)
# 
# # #paste this before analysis:
# # baseline_dealers$large_catchID <- ifelse(baseline_dealers$catchID==3|baseline_dealers$catchID==32|baseline_dealers$catchID==59,1,0)
# # #|baseline_dealers$catchID==64
# # baseline_dealers=subset(baseline_dealers,large_catchID=="0")
# 
# # baseline_farmers$large_catchID <- ifelse(baseline_farmers$catchID==3|baseline_farmers$catchID==32|baseline_farmers$catchID==59,1,0)
# # #|baseline_farmers$catchID==64
# # baseline_farmers=subset(baseline_farmers,large_catchID=="0")
# 
# # #then again, change all "sample(na.omit("
# # #nothing interesting happens
# 
# 
# 
# #OC10. different treatment stati
# #1. TRAINING
# #1.a training according to dealers
# baseline_dealers$say_that_attended_training <- NA
# baseline_dealers$say_that_attended_training[baseline_dealers$owner.agree.q11b=="Yes"] <- 1
# baseline_dealers$say_that_attended_training[baseline_dealers$training==0] <- 0
# #unreliable because at least 136 dealers attended the training (I checked their attendance) (here: 105)
# 
# #1.b training according to our attendance list
# #insert above analysis but after sign
# training_attendance <- read.csv(paste(path,"/Study design/treatments/training/training_attendance.csv", sep="/"), sep=";", stringsAsFactors=TRUE)
# training_attendance = subset(training_attendance, select = c("shop_ID","someone_attended"))
# baseline_dealers <- merge(baseline_dealers,training_attendance,by.x="shop_ID",by.y="shop_ID",all.x=TRUE)
# baseline_dealers$someone_attended[baseline_dealers$training==0] <- 0
# #baseline_dealers$training <- baseline_dealers$someone_attended
# 
# #now significant
# #Average sales price of 4 improved maize varieties last season in UGX/ kg
# #Index of all seed handling and storage practices observed by enumerator
# #Transformed amount of Longe 5 bought by shop from provider last season in kg (IHS)
# #Shop's products were confiscated after inspection
# 
# #2. CLEARINGHOUSE
# #general note: too complex because of combination of certificates to dealers, ratings to farmers in person, SMS...
# 
# #DEALERS
# #2.D.a first dissemination to dealers
# #2.D.a.1 first dissemination to dealers according to dealers
# baseline_dealers$say_that_certificate <- NA
# baseline_dealers$say_that_certificate[baseline_dealers$owner.agree.q2a=="Yes"] <- 1
# baseline_dealers$say_that_certificate[baseline_dealers$clearing==0] <- 0
# #unreliable because Richard says that 182 (at least 157) dealers received certificate (here: 124)
# 
# #2.D.a.2 first dissemination to dealers according to Richard
# #need to ask Richard
# 
# #2.D.b second dissemination to dealers
# #2.D.b.1 second dissemination to dealers according to dealers
# #see at endline
# 
# #2.D.a.2 second dissemination to dealers according to us
# #everyone in midline_dealers with clearing==1 received certificate (same visit)
# baseline_dealers$received_2nd_ml_certificate <- baseline_dealers$clearing
# baseline_dealers$received_2nd_ml_certificate[baseline_dealers$attrition_ind_D==1 & baseline_dealers$clearing==1] <- NA
# #doesn't make sense for midline analysis because dealers that left midline sample aren't in analysis anyway
# 
# #FARMERS
# #2.F.a first dissemination to farmers according to us
# farmer_dissemination_final <- read.csv("C:/Users/u0127963/Dropbox/NWO seed system devt Uganda proposal development/Study design/treatments/info_clearing/farmer/data/farmer_dissemination_final.csv") #54 missing
# 
# #2.F.b second dissemination to farmers according to us
# #not everyone in midline_farmers with clearing==1 received certificate (not same visit) but good proxy because almost same time (Jan and Feb)
# #midline dissemination dataset would be better here but data is not in dropbox but only on Bjorn's computer
# baseline_farmers$received_2nd_ml_rating <- baseline_farmers$clearing
# baseline_farmers$received_2nd_ml_rating[baseline_farmers$attrition_ind_F==1 & baseline_farmers$clearing==1] <- NA
# #doesn't make sense for midline analysis because farmers that left midline sample aren't in analysis anyway
# 
# #3. VIDEO
# #3.a first video shown
# #video shown during farmer baseline data collection and again during baseline rating dissemination/ service questions --> complete sample
# 
# #3.b second video shown
# #we showed the farmer video during midline rating dissemination/ service questions
# #not everyone in midline_farmers with video==1 saw video (not same visit) but good proxy because almost same time (Jan and Feb)
# #midline dissemination dataset would be better here but data is not in dropbox but only on Bjorn's computer
# baseline_farmers$watched_video <- baseline_farmers$Check2.check.maize.video_shown
# baseline_farmers$watched_video[baseline_farmers$attrition_ind_F==1 & baseline_farmers$Check2.check.maize.video_shown==1] <- NA
# #doesn't make sense for midline analysis because farmers that left midline sample aren't in analysis anyway
# 
# 
# 
# #OC11. CR0 to CR0
# #Average sales price of 4 improved maize varieties last season in UGX/ kg
# #Days since packaging date/expiry date minus 6 months
# #Overall index controlling for baseline (secondary outcome variables regarding seed bag)
# 
# ### saving results in matrices for papers
# df_ols_F_sec_plot_end <- df_ols_F_sec_plot 
# save(df_ols_F_sec_plot_end, file=paste(path,"papers/endline_report/output/df_ols_F_sec_plot_end.Rdata",sep="/"))
