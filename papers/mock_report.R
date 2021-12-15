rm(list=ls())

path <- getwd()
path <- strsplit(path,"/papers")[[1]]

baseline_dealers <- read.csv(paste(path,"/baseline/data/agro_input/public/baseline_dealer.csv",sep="/"))
baseline_farmers <- read.csv(paste(path,"/baseline/data/farmer/public/baseline_farmers.csv",sep="/"))

#merge in more data
#RATINGS (only CH treatment farmers rated CH treatment dealers at baseline)
#dealers
reviews_seed <- read.csv(paste(path,"/baseline/data/agro_input/public/reviews_seed.csv",sep="/"))
baseline_dealers <- merge(baseline_dealers,reviews_seed,by.x=c("catchID","shop_ID"),by.y=c("catchID","shop_ID"),all.x=T)

#farmers
rating_dyads <- read.csv(paste(path,"/baseline/data/farmer/public/rating_dyads.csv",sep="/"))

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

#SERVICES (CH treatment farmers were asked during baseline, CH control farmers were asked during CH rating dissemination)
dealer_services_dyads <- read.csv(paste(path,"/Study design/treatments/info_clearing/farmer/data/public/dealer_services_dyads.csv",sep="/"))

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
baseline_dealers$visible_expdate<-ifelse(baseline_dealers$exp=="n/a",0,1) #2 names

###

baseline_dealers$seed_expired <- 0
baseline_dealers$seed_expired[is.na(baseline_dealers$exp)] <- NA
baseline_dealers$seed_expired[baseline_dealers$exp=="n/a"] <- NA
baseline_dealers$seed_expired[is.na(baseline_dealers$date)] <- NA
baseline_dealers$seed_expired[baseline_dealers$date=="n/a"] <- NA
#1 shop has n/a for date but not for exp
baseline_dealers$date <- as.Date(baseline_dealers$date)
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
               ,"germination","nr_reviews")

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
baseline_farmers[, 4:94][baseline_farmers[, 4:94] == 96] <- NA
#baseline_farmers[baseline_farmers==98] <- NA
baseline_farmers[, 4:94][baseline_farmers[, 4:94] == 98] <- NA
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
baseline_farmers$OPVbutfourthormore_timeused <- NA
baseline_farmers$OPVbutfourthormore_timeused[baseline_farmers$OPV==1 & baseline_farmers$farmer_saved_seed==1 & baseline_farmers$fourthormore_timeused==1] <- 1
baseline_farmers$OPVbutfourthormore_timeused[baseline_farmers$OPV==1 & baseline_farmers$farmer_saved_seed==1 & baseline_farmers$fourthormore_timeused==0] <- 0
baseline_farmers$OPVbutfourthormore_timeused[baseline_farmers$OPV==1 & baseline_farmers$farmer_saved_seed==0] <- 0
baseline_farmers$OPVbutfourthormore_timeused[baseline_farmers$OPV == 0] <- 0

baseline_farmers$adoption_onfield <- baseline_farmers$improved
baseline_farmers$adoption_onfield[baseline_farmers$hybridbutsaved==1] <- 0
baseline_farmers$adoption_onfield[baseline_farmers$OPVbutfourthormore_timeused==1] <- 0

baseline_farmers$Check2.check.maize.q36<-ifelse(baseline_farmers$Check2.check.maize.q36=="Yes",1,0)
baseline_farmers$Check2.check.maize.q36b<-ifelse(baseline_farmers$Check2.check.maize.q36b=="Yes",1,0)
baseline_farmers$Check2.check.maize.q37<-ifelse(baseline_farmers$Check2.check.maize.q37=="Yes",1,0)
baseline_farmers$Check2.check.maize.q39 <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q39))
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

baseline_farmers$revenueUGX <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q54))*baseline_farmers$Check2.check.maize.q55
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

df_averages <- array(NA,dim=c(2,25))
df_ols <- array(NA,dim=c(3,3,25))

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
balance_dealer <- c("maize.owner.agree.age","maize.owner.agree.gender","finished_primary","maize.owner.agree.q3"
                    ,"maize.owner.agree.q6","years_shop","maize.owner.agree.q10","maize.owner.agree.nr_var"
                    ,"tot_sold","tot_lost","maize.owner.agree.temp.q71","maize.owner.agree.temp.q72"
                    ,"maize.owner.agree.q96","maize.owner.agree.skill.q105_b"
                    ,"maize.owner.agree.inspection.q115","reading","lot"
                    ,"refunds"
                    ,"gives_credit","after_sales_service")

for (i in 1:length(balance_dealer)){
  df_averages[1,i] <- sum(baseline_dealers[balance_dealer[i]], na.rm=T)/(nrow(baseline_dealers)-sum(is.na(baseline_dealers[balance_dealer[i]])))
  df_averages[2,i] <- sqrt(var(baseline_dealers[balance_dealer[i]], na.rm=T))
  
  formula1 <- as.formula(paste(balance_dealer[i],paste("training*clearing*farmer"),sep="~"))
  ols <- lm(formula1, data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  df_ols[1,1,i] <- coef_test(ols, vcov_cluster)[2,1]
  df_ols[2,1,i] <- coef_test(ols, vcov_cluster)[2,2]
  df_ols[3,1,i] <- coef_test(ols, vcov_cluster)[2,5]

  df_ols[1,2,i] <- coef_test(ols, vcov_cluster)[3,1]
  df_ols[2,2,i] <- coef_test(ols, vcov_cluster)[3,2]
  df_ols[3,2,i] <- coef_test(ols, vcov_cluster)[3,5]
  
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

df_averages_farmer <- array(NA,dim=c(2,20))
df_ols_farmer <- array(NA,dim=c(3,3,20))

###loop###
balance_farmer <- c("Check2.check.maize.q8","Check2.check.maize.q10","Check2.check.maize.q14","Check2.check.maize.q15","finishedprimary"
                    ,"Check2.check.maize.q18","Check2.check.maize.q20","Check2.check.maize.q22","Check2.check.maize.q25a"
                    ,"boughtfromagroinputshop2","Check2.check.maize.q25d2","Check2.check.maize.q25h","Check2.check.maize.q30a.1"
                    ,"adoption_onfield","Check2.check.maize.q35a","Check2.check.maize.q42","correctplanting","yield_inkg"
                    ,"landproductivity","Check2.check.maize.q53")

for (i in 1:length(balance_farmer)){
  df_averages_farmer[1,i] <- sum(baseline_farmers[balance_farmer[i]], na.rm=T)/(nrow(baseline_farmers)-sum(is.na(baseline_farmers[balance_farmer[i]])))
  df_averages_farmer[2,i] <- sqrt(var(baseline_farmers[balance_farmer[i]], na.rm=T))
  
  formula2 <- as.formula(paste(balance_farmer[i],paste("training*Check2.check.maize.clearing*Check2.check.maize.video_shown"),sep="~")) #* because of interactions
  ols <- lm(formula2, data=baseline_farmers)
  #vcovCR for cluster-robust variance-covariance matrix
  vcov_cluster_catchID <- vcovCR(ols,cluster=baseline_farmers$catchID,type="CR3")
  
  #filling df_ols with training (Estimate, SE, p-val (Satt))
  df_ols_farmer[1,1,i] <- coef_test(ols, vcov_cluster_catchID)[2,1]
  df_ols_farmer[2,1,i] <- coef_test(ols, vcov_cluster_catchID)[2,2]
  df_ols_farmer[3,1,i] <- coef_test(ols, vcov_cluster_catchID)[2,5]
  
  #filling df_ols with CH (Estimate, SE, p-val (Satt))
  df_ols_farmer[1,2,i] <- coef_test(ols, vcov_cluster_catchID)[3,1]
  df_ols_farmer[2,2,i] <- coef_test(ols, vcov_cluster_catchID)[3,2]
  df_ols_farmer[3,2,i] <- coef_test(ols, vcov_cluster_catchID)[3,5]
  
  #filling df_ols with video (Estimate, SE, p-val (Satt))
  #randomization at village level ie. at shop level
  vcov_cluster_shop <- vcovCR(ols,cluster=baseline_farmers$shop_ID,type="CR3")
  
  df_ols_farmer[1,3,i] <- coef_test(ols, vcov_cluster_shop)[4,1]
  df_ols_farmer[2,3,i] <- coef_test(ols, vcov_cluster_shop)[4,2]
  df_ols_farmer[3,3,i] <- coef_test(ols, vcov_cluster_shop)[4,5]}

#difference in mean primary education: I first counted g (Other) as 0, now as NA
#difference in mean bought from dealer: correct if NA=0

###################################
#####TESTS OF SURVEY ATTRITION#####
###################################

######################################
#####Attrition: agro-input dealer#####
######################################

df_averages_attritionD <- array(NA,dim=c(2,20))
df_ols_attritionD <- array(NA,dim=c(3,20))

#simulate random attrition
library(dplyr)
set.seed(10081996)
lostdealers <- sample_n(baseline_dealers,35)
lostdealers = lostdealers[c("shop_ID")]
lostdealers$attrition_ind_D <- 1
baseline_dealers <- merge(baseline_dealers, lostdealers, by.x="shop_ID", by.y="shop_ID", all.x = TRUE)
baseline_dealers$attrition_ind_D[is.na(baseline_dealers$attrition_ind_D)] <- 0

# baseline_dealers$attrition_ind_D <- 0
# baseline_dealers$attrition_ind_D[is.na(midline_dealers$q0)] <- 1

###loop###
attrition_dealer <- c("maize.owner.agree.age","maize.owner.agree.gender","finished_primary","maize.owner.agree.q3"
                    ,"maize.owner.agree.q6","years_shop","maize.owner.agree.q10","maize.owner.agree.nr_var"
                    ,"tot_sold","tot_lost","maize.owner.agree.temp.q71","maize.owner.agree.temp.q72","maize.owner.agree.temp.q81"
                    ,"alwaysexplains","q93_bin","maize.owner.agree.q96","maize.owner.agree.skill.q105_b"
                    ,"maize.owner.agree.inspection.q115","reading","lot")

for (i in 1:length(attrition_dealer)){
  df_averages_attritionD[1,i] <- sum(baseline_dealers[attrition_dealer[i]], na.rm=T)/(nrow(baseline_dealers)-sum(is.na(baseline_dealers[attrition_dealer[i]])))
  df_averages_attritionD[2,i] <- sqrt(var(baseline_dealers[attrition_dealer[i]], na.rm=T))
  
  formula1 <- as.formula(paste(attrition_dealer[i],paste("attrition_ind_D"),sep="~"))
  ols <- lm(formula1, data=baseline_dealers)

  #attrition at dealer level, so no clustering needed
  df_ols_attritionD[1,i] <- summary(ols)$coefficients[2,1]
  df_ols_attritionD[2,i] <- summary(ols)$coefficients[2,2]
  df_ols_attritionD[3,i] <- summary(ols)$coefficients[2,4]}

number_lostD <- sum(baseline_dealers$attrition_ind_D==1)
number_lostD_control <- sum(baseline_dealers$attrition_ind_D==1 & baseline_dealers$training == 0 & baseline_dealers$clearing == 0 & baseline_dealers$farmer == 0)
number_lostD_training <- sum(baseline_dealers$attrition_ind_D==1 & baseline_dealers$training == 1)
number_lostD_clearing <- sum(baseline_dealers$attrition_ind_D==1 & baseline_dealers$clearing == 1)
number_lostD_farmer <- sum(baseline_dealers$attrition_ind_D==1 & baseline_dealers$farmer == 1)

number_allD <- sum(nrow(baseline_dealers))
number_allD_control <- sum(baseline_dealers$training == 0 & baseline_dealers$clearing == 0 & baseline_dealers$farmer == 0)
number_allD_training <- sum(baseline_dealers$training == 1)
number_allD_clearing <- sum(baseline_dealers$clearing == 1)
number_allD_farmer <- sum(baseline_dealers$farmer == 1)

perc_lostD <- sum(number_lostD/number_allD*100)
perc_lostD_control <- sum(number_lostD_control/number_allD_control*100)
perc_lostD_training <- sum(number_lostD_training/number_allD_training*100)
perc_lostD_clearing <- sum(number_lostD_clearing/number_allD_clearing*100)
perc_lostD_farmer <- sum(number_lostD_farmer/number_allD_farmer*100)

###########################
#####Attrition: farmer#####
###########################

df_averages_attritionF <- array(NA,dim=c(2,20))
df_ols_attritionF <- array(NA,dim=c(3,20))

#simulate random attrition
lostfarmers <- sample_n(baseline_farmers,350)
lostfarmers = lostfarmers[c("farmer_ID")]
lostfarmers$attrition_ind_F <- 1
baseline_farmers <- merge(baseline_farmers, lostfarmers, by.x="farmer_ID", by.y="farmer_ID", all.x = TRUE)
baseline_farmers$attrition_ind_F[is.na(baseline_farmers$attrition_ind_F)] <- 0

# baseline_farmers$attrition_ind_F <- 0
# baseline_farmers$attrition_ind_F[is.na(midline_farmers$q0)] <- 1

###loop###
attrition_farmer <- c("Check2.check.maize.q8","Check2.check.maize.q10","Check2.check.maize.q14","Check2.check.maize.q15","finishedprimary"
                    ,"Check2.check.maize.q18","Check2.check.maize.q20","Check2.check.maize.q22","Check2.check.maize.q25a"
                    ,"boughtfromagroinputshop2","Check2.check.maize.q25d2","Check2.check.maize.q25h","Check2.check.maize.q30a.1"
                    ,"adoption_onfield","Check2.check.maize.q35a","Check2.check.maize.q42","correctplanting","yield_inkg"
                    ,"landproductivity","Check2.check.maize.q53")

for (i in 1:length(attrition_farmer)){
  df_averages_attritionF[1,i] <- sum(baseline_farmers[attrition_farmer[i]], na.rm=T)/(nrow(baseline_farmers)-sum(is.na(baseline_farmers[attrition_farmer[i]])))
  df_averages_attritionF[2,i] <- sqrt(var(baseline_farmers[attrition_farmer[i]], na.rm=T))
  
  formula1 <- as.formula(paste(attrition_farmer[i],paste("attrition_ind_F"),sep="~"))
  ols <- lm(formula1, data=baseline_farmers)
  
  #attrition at dealer level, so no clustering needed
  df_ols_attritionF[1,i] <- summary(ols)$coefficients[2,1]
  df_ols_attritionF[2,i] <- summary(ols)$coefficients[2,2]
  df_ols_attritionF[3,i] <- summary(ols)$coefficients[2,4]}

number_lostF <- sum(baseline_farmers$attrition_ind_F==1)
number_lostF <- sum(baseline_farmers$attrition_ind_F==1)
number_lostF_control <- sum(baseline_farmers$attrition_ind_F==1 & baseline_farmers$training == 0 & baseline_farmers$Check2.check.maize.clearing == 0 & baseline_farmers$Check2.check.maize.video_shown == 0)
number_lostF_training <- sum(baseline_farmers$attrition_ind_F==1 & baseline_farmers$training == 1)
number_lostF_clearing <- sum(baseline_farmers$attrition_ind_F==1 & baseline_farmers$Check2.check.maize.clearing == 1)
number_lostF_farmer <- sum(baseline_farmers$attrition_ind_F==1 & baseline_farmers$Check2.check.maize.video_shown == 1)

number_allF <- sum(nrow(baseline_farmers))
number_allF_control <- sum(baseline_farmers$training == 0 & baseline_farmers$Check2.check.maize.clearing == 0 & baseline_farmers$Check2.check.maize.video_shown == 0)
number_allF_training <- sum(baseline_farmers$training == 1)
number_allF_clearing <- sum(baseline_farmers$Check2.check.maize.clearing == 1)
number_allF_farmer <- sum(baseline_farmers$Check2.check.maize.video_shown == 1)

perc_lostF <- sum(number_lostF/number_allF*100)
perc_lostF_control <- sum(number_lostF_control/number_allF_control*100)
perc_lostF_training <- sum(number_lostF_training/number_allF_training*100)
perc_lostF_clearing <- sum(number_lostF_clearing/number_allF_clearing*100)
perc_lostF_farmer <- sum(number_lostF_farmer/number_allF_farmer*100)










################################################################################################################################################################################
##### 1 ANALYSIS: Agro-input dealer - Primary###################################################################################################################################
################################################################################################################################################################################

trim <- function(var,dataset,trim_perc=.01){
  dataset[var][dataset[var]<quantile(dataset[var],c(trim_perc/2,1-(trim_perc/2)),na.rm=T)[1]|dataset[var]>quantile(dataset[var],c(trim_perc/2,1-(trim_perc/2)),na.rm=T)[2]] <- NA
  return(dataset)}

baseline_dealers$training<-ifelse(baseline_dealers$training=="TRUE",1,0)
baseline_dealers$clearing<-ifelse(baseline_dealers$clearing=="TRUE",1,0)
baseline_dealers$farmer<-ifelse(baseline_dealers$farmer=="TRUE",1,0)

#1. Cumulative quantity sold of a hybrid and a open-pollinated maize variety last season in kg
baseline_dealers$maize.owner.agree.long10h.q25[baseline_dealers$maize.owner.agree.q20=="0"] <- 0
baseline_dealers <- trim("maize.owner.agree.long10h.q25",baseline_dealers,trim_perc=.01)

baseline_dealers$mid_maize.owner.agree.q20 <- baseline_dealers$maize.owner.agree.q20
#baseline_dealers$mid_maize.owner.agree.q20<-ifelse(baseline_dealers$mid_maize.owner.agree.q20=="Yes",1,0)
baseline_dealers$mid_maize.owner.agree.q32 <- baseline_dealers$maize.owner.agree.q32
#baseline_dealers$mid_maize.owner.agree.q32<-ifelse(mid_maize.owner.agree.q32=="Yes",1,0)
baseline_dealers$mid_maize.owner.agree.q45 <- baseline_dealers$maize.owner.agree.q45
#baseline_dealers$mid_maize.owner.agree.q45<-ifelse(baseline_dealers$mid_maize.owner.agree.q45=="Yes",1,0)
baseline_dealers$mid_maize.owner.agree.q57 <- baseline_dealers$maize.owner.agree.q57
#baseline_dealers$mid_maize.owner.agree.q57<-ifelse(baseline_dealers$mid_maize.owner.agree.q57=="Yes",1,0)

baseline_dealers$mid_maize.owner.agree.long10h.q25 <- (baseline_dealers$maize.owner.agree.long10h.q25+26.25481*baseline_dealers$training+26.25481*baseline_dealers$clearing+26.25481*baseline_dealers$farmer)
baseline_dealers$mid_maize.owner.agree.long10h.q25 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.long10h.q25))
baseline_dealers$mid_maize.owner.agree.long10h.q25[baseline_dealers$mid_maize.owner.agree.long10h.q25==999]<-NA
baseline_dealers$mid_maize.owner.agree.long10h.q25[baseline_dealers$mid_maize.owner.agree.q20=="0"] <- 0
baseline_dealers <- trim("mid_maize.owner.agree.long10h.q25",baseline_dealers,trim_perc=.01)

baseline_dealers$maize.owner.agree.longe7h.q37[baseline_dealers$maize.owner.agree.q32=="0"] <- 0
baseline_dealers <- trim("maize.owner.agree.longe7h.q37",baseline_dealers,trim_perc=.01)

baseline_dealers$mid_maize.owner.agree.longe7h.q37 <- (baseline_dealers$maize.owner.agree.longe7h.q37+3.583478*baseline_dealers$training+3.583478*baseline_dealers$clearing+3.583478*baseline_dealers$farmer)
baseline_dealers$mid_maize.owner.agree.longe7h.q37 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.longe7h.q37))
baseline_dealers$mid_maize.owner.agree.longe7h.q37[baseline_dealers$mid_maize.owner.agree.longe7h.q37==999]<-NA
baseline_dealers$mid_maize.owner.agree.longe7h.q37[baseline_dealers$mid_maize.owner.agree.q32=="0"] <- 0
baseline_dealers <- trim("mid_maize.owner.agree.longe7h.q37",baseline_dealers,trim_perc=.01)

baseline_dealers$maize.owner.agree.longe5.q50[baseline_dealers$maize.owner.agree.q45=="0"] <- 0
baseline_dealers <- trim("maize.owner.agree.longe5.q50",baseline_dealers,trim_perc=.01)

baseline_dealers$mid_maize.owner.agree.longe5.q50 <- (baseline_dealers$maize.owner.agree.longe5.q50+37.325*baseline_dealers$training+37.325*baseline_dealers$clearing+37.325*baseline_dealers$farmer)
baseline_dealers$mid_maize.owner.agree.longe5.q50 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.longe5.q50))
baseline_dealers$mid_maize.owner.agree.longe5.q50[baseline_dealers$mid_maize.owner.agree.longe5.q50==999]<-NA
baseline_dealers$mid_maize.owner.agree.longe5.q50[baseline_dealers$mid_maize.owner.agree.q45=="0"] <- 0
baseline_dealers <- trim("mid_maize.owner.agree.longe5.q50",baseline_dealers,trim_perc=.01)

baseline_dealers$maize.owner.agree.longe4.q62[baseline_dealers$maize.owner.agree.q57=="0"] <- 0
baseline_dealers <- trim("maize.owner.agree.longe4.q62",baseline_dealers,trim_perc=.01)

baseline_dealers$mid_maize.owner.agree.longe4.q62 <- (baseline_dealers$maize.owner.agree.longe4.q62+8.046957*baseline_dealers$training+8.046957*baseline_dealers$clearing+8.046957*baseline_dealers$farmer)
baseline_dealers$mid_maize.owner.agree.longe4.q62 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.longe4.q62))
baseline_dealers$mid_maize.owner.agree.longe4.q62[baseline_dealers$mid_maize.owner.agree.longe4.q62==999]<-NA
baseline_dealers$mid_maize.owner.agree.longe4.q62[baseline_dealers$mid_maize.owner.agree.q57=="0"] <- 0
baseline_dealers <- trim("mid_maize.owner.agree.longe4.q62",baseline_dealers,trim_perc=.01)

baseline_dealers$quantitysold <- baseline_dealers$maize.owner.agree.long10h.q25+baseline_dealers$maize.owner.agree.longe7h.q37+baseline_dealers$maize.owner.agree.longe5.q50+baseline_dealers$maize.owner.agree.longe4.q62
baseline_dealers$mid_quantitysold <- baseline_dealers$mid_maize.owner.agree.long10h.q25+baseline_dealers$mid_maize.owner.agree.longe7h.q37+baseline_dealers$mid_maize.owner.agree.longe5.q50+baseline_dealers$mid_maize.owner.agree.longe4.q62


#2. Sales prices of a hybrid and an open-pollinated maize variety at beginning of last season in UGX per kg
baseline_dealers$maize.owner.agree.long10h.q26[baseline_dealers$maize.owner.agree.long10h.q26=="n/a"]<-NA
baseline_dealers$maize.owner.agree.longe7h.q38[baseline_dealers$maize.owner.agree.longe7h.q38=="n/a"]<-NA
baseline_dealers$maize.owner.agree.longe5.q51[baseline_dealers$maize.owner.agree.longe5.q51=="n/a"]<-NA
baseline_dealers$maize.owner.agree.longe4.q63[baseline_dealers$maize.owner.agree.longe4.q63=="n/a"]<-NA

baseline_dealers$maize.owner.agree.long10h.q26 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.long10h.q26))
baseline_dealers$maize.owner.agree.longe7h.q38 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.longe7h.q38))
baseline_dealers$maize.owner.agree.longe5.q51 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.longe5.q51))
baseline_dealers$maize.owner.agree.longe4.q63 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.longe4.q63))

baseline_dealers$mid_maize.owner.agree.long10h.q26 <- (baseline_dealers$maize.owner.agree.long10h.q26+626.5756*baseline_dealers$training+626.5756*baseline_dealers$clearing+626.5756*baseline_dealers$farmer)
baseline_dealers$mid_maize.owner.agree.longe7h.q38 <- (baseline_dealers$maize.owner.agree.longe7h.q38+547.3214*baseline_dealers$training+547.3214*baseline_dealers$clearing+547.3214*baseline_dealers$farmer)
baseline_dealers$mid_maize.owner.agree.longe5.q51 <- (baseline_dealers$maize.owner.agree.longe5.q51+317.5649*baseline_dealers$training+317.5649*baseline_dealers$clearing+317.5649*baseline_dealers$farmer)
baseline_dealers$mid_maize.owner.agree.longe4.q63 <- (baseline_dealers$maize.owner.agree.longe4.q63+314.3478*baseline_dealers$training+314.3478*baseline_dealers$clearing+314.3478*baseline_dealers$farmer)

baseline_dealers$mid_maize.owner.agree.long10h.q26[baseline_dealers$mid_maize.owner.agree.long10h.q26=="n/a"]<-NA
baseline_dealers$mid_maize.owner.agree.longe7h.q38[baseline_dealers$mid_maize.owner.agree.longe7h.q38=="n/a"]<-NA
baseline_dealers$mid_maize.owner.agree.longe5.q51[baseline_dealers$mid_maize.owner.agree.longe5.q51=="n/a"]<-NA
baseline_dealers$mid_maize.owner.agree.longe4.q63[baseline_dealers$mid_maize.owner.agree.longe4.q63=="n/a"]<-NA

baseline_dealers$mid_maize.owner.agree.long10h.q26 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.long10h.q26))
baseline_dealers$mid_maize.owner.agree.longe7h.q38 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.longe7h.q38))
baseline_dealers$mid_maize.owner.agree.longe5.q51 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.longe5.q51))
baseline_dealers$mid_maize.owner.agree.longe4.q63 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.longe4.q63))

baseline_dealers <- trim("maize.owner.agree.long10h.q26",baseline_dealers,trim_perc=.01)
baseline_dealers <- trim("maize.owner.agree.longe7h.q38",baseline_dealers,trim_perc=.01)
baseline_dealers <- trim("maize.owner.agree.longe5.q51",baseline_dealers,trim_perc=.01)
baseline_dealers <- trim("maize.owner.agree.longe4.q63",baseline_dealers,trim_perc=.01)

baseline_dealers <- trim("mid_maize.owner.agree.long10h.q26",baseline_dealers,trim_perc=.01)
baseline_dealers <- trim("mid_maize.owner.agree.longe7h.q38",baseline_dealers,trim_perc=.01)
baseline_dealers <- trim("mid_maize.owner.agree.longe5.q51",baseline_dealers,trim_perc=.01)
baseline_dealers <- trim("mid_maize.owner.agree.longe4.q63",baseline_dealers,trim_perc=.01)

baseline_dealers$av_salesprices <- rowMeans(baseline_dealers[c("maize.owner.agree.long10h.q26"
                                                               ,"maize.owner.agree.longe7h.q38"
                                                               ,"maize.owner.agree.longe5.q51"
                                                               ,"maize.owner.agree.longe4.q63")],na.rm = T)
baseline_dealers$mid_av_salesprices <- rowMeans(baseline_dealers[c("mid_maize.owner.agree.long10h.q26"
                                                                   ,"mid_maize.owner.agree.longe7h.q38"
                                                                   ,"mid_maize.owner.agree.longe5.q51"
                                                                   ,"mid_maize.owner.agree.longe4.q63")],na.rm = T)


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

baseline_dealers$mid_revenue_long10h.q25 <- (baseline_dealers$mid_maize.owner.agree.long10h.q25*baseline_dealers$mid_maize.owner.agree.long10h.q26)
baseline_dealers$mid_revenue_long10h.q25[baseline_dealers$mid_maize.owner.agree.q20=="0"] <- 0

baseline_dealers$mid_revenue_longe7h <- (baseline_dealers$mid_maize.owner.agree.longe7h.q37*baseline_dealers$mid_maize.owner.agree.longe7h.q38)
baseline_dealers$mid_revenue_longe7h[baseline_dealers$mid_maize.owner.agree.q32=="0"] <- 0

baseline_dealers$mid_revenue_longe5 <- (baseline_dealers$mid_maize.owner.agree.longe5.q50*baseline_dealers$mid_maize.owner.agree.longe5.q51)
baseline_dealers$mid_revenue_longe5[baseline_dealers$mid_maize.owner.agree.q45=="0"] <- 0

baseline_dealers$mid_revenue_longe4 <- (baseline_dealers$mid_maize.owner.agree.longe4.q62*baseline_dealers$mid_maize.owner.agree.longe4.q63)
baseline_dealers$mid_revenue_longe4[baseline_dealers$mid_maize.owner.agree.q57=="0"] <- 0

baseline_dealers$mid_revenue <- (baseline_dealers$mid_revenue_long10h.q25+baseline_dealers$mid_revenue_longe7h
                             +baseline_dealers$mid_revenue_longe5+baseline_dealers$mid_revenue_longe4)


#4. Number of customers who bought maize seed on average day at beginning of last season
baseline_dealers <- trim("maize.owner.agree.q7",baseline_dealers,trim_perc=.01)
baseline_dealers$mid_maize.owner.agree.q7 <- (baseline_dealers$maize.owner.agree.q7+2.066*baseline_dealers$training+2.066*baseline_dealers$clearing+2.066*baseline_dealers$farmer)
baseline_dealers$mid_maize.owner.agree.q7 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.q7))
baseline_dealers <- trim("mid_maize.owner.agree.q7",baseline_dealers,trim_perc=.01)


#5. Moisture content of random seed bag
baseline_dealers <- trim("reading",baseline_dealers,trim_perc=.01)
baseline_dealers$mid_reading <- (baseline_dealers$reading-1.356*baseline_dealers$training-1.356*baseline_dealers$clearing-1.356*baseline_dealers$farmer)
baseline_dealers$mid_reading <- as.numeric(as.character(baseline_dealers$mid_reading))
baseline_dealers <- trim("mid_reading",baseline_dealers,trim_perc=.01)


#6. Index of capital-intensive seed handling and storage practices observed by enumerator
###Anderson, 2008: https://are.berkeley.edu/~mlanderson/pdf/Anderson%202008a.pdf p. 1485
###1. For all outcomes, switch signs where necessary so that the positive direction always indicates a "better" outcome.
#Is the roof leak-proof? yes=good
baseline_dealers$mid_maize.owner.agree.temp.q72 <- baseline_dealers$maize.owner.agree.temp.q72
#baseline_dealers$mid_maize.owner.agree.temp.q72 <- ifelse(baseline_dealers$mid_maize.owner.agree.temp.q72=="Yes",1,0)

#Is the roof insulated to keep heat out? yes=good
baseline_dealers$mid_maize.owner.agree.temp.q73 <- baseline_dealers$maize.owner.agree.temp.q73
#baseline_dealers$mid_maize.owner.agree.temp.q73 <- ifelse(baseline_dealers$mid_maize.owner.agree.temp.q73=="Yes",1,0)

#Are the walls insulated to keep the heat out? yes=good
baseline_dealers$mid_maize.owner.agree.temp.q74 <- baseline_dealers$maize.owner.agree.temp.q74
#baseline_dealers$mid_maize.owner.agree.temp.q74 <- ifelse(baseline_dealers$mid_maize.owner.agree.temp.q74=="Yes",1,0)

#Is the area ventilated? yes=good
baseline_dealers$mid_maize.owner.agree.temp.q75 <- baseline_dealers$maize.owner.agree.temp.q75
#baseline_dealers$mid_maize.owner.agree.temp.q75<-ifelse(baseline_dealers$mid_maize.owner.agree.temp.q75=="Yes",1,0)

#Do you see any official certificates displayed in the shop (e.g. inspection, trainings, registration with association)? yes=good
baseline_dealers$mid_maize.owner.agree.temp.q81 <- baseline_dealers$maize.owner.agree.temp.q81
#baseline_dealers$mid_maize.owner.agree.temp.q81<-ifelse(baseline_dealers$mid_maize.owner.agree.temp.q81=="Yes",1,0)

#What do you do with seed that have exceeded shelf live (expired)? yes=good
#(a This has never happened/b Return to supplier/c Sell at discount/d Given away/e Thrown away/f sell at normal price/g mix with other seed/96 Other)
baseline_dealers$mid_maize.owner.agree.q83.a<-baseline_dealers$maize.owner.agree.q83.a
baseline_dealers$mid_maize.owner.agree.q83.b<-baseline_dealers$maize.owner.agree.q83.b
baseline_dealers$mid_maize.owner.agree.q83.c<-baseline_dealers$maize.owner.agree.q83.c
baseline_dealers$mid_maize.owner.agree.q83.d<-baseline_dealers$maize.owner.agree.q83.d
baseline_dealers$mid_maize.owner.agree.q83.e<-baseline_dealers$maize.owner.agree.q83.e
baseline_dealers$mid_maize.owner.agree.q83.f<-baseline_dealers$maize.owner.agree.q83.f
baseline_dealers$mid_maize.owner.agree.q83.g<-baseline_dealers$maize.owner.agree.q83.g
baseline_dealers$mid_maize.owner.agree.q83.96<-baseline_dealers$maize.owner.agree.q83.96

baseline_dealers$goodpractice_expired2 <- 1
baseline_dealers$goodpractice_expired2[baseline_dealers$maize.owner.agree.q83.96==1] <- NA
baseline_dealers$goodpractice_expired2[baseline_dealers$maize.owner.agree.q83.c==1] <- 0
baseline_dealers$goodpractice_expired2[baseline_dealers$maize.owner.agree.q83.d==1] <- 0
baseline_dealers$goodpractice_expired2[baseline_dealers$maize.owner.agree.q83.f==1] <- 0
baseline_dealers$goodpractice_expired2[baseline_dealers$maize.owner.agree.q83.g==1] <- 0

baseline_dealers$mid_goodpractice_expired2 <- 1
baseline_dealers$mid_goodpractice_expired2[baseline_dealers$mid_maize.owner.agree.q83.96==1] <- NA
baseline_dealers$mid_goodpractice_expired2[baseline_dealers$mid_maize.owner.agree.q83.c==1] <- 0
baseline_dealers$mid_goodpractice_expired2[baseline_dealers$mid_maize.owner.agree.q83.d==1] <- 0
baseline_dealers$mid_goodpractice_expired2[baseline_dealers$mid_maize.owner.agree.q83.f==1] <- 0
baseline_dealers$mid_goodpractice_expired2[baseline_dealers$mid_maize.owner.agree.q83.g==1] <- 0

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
                                     ,baseline_dealers$mid_maize.owner.agree.temp.q81,baseline_dealers$mid_goodpractice_expired2)
variables_practices_cap_base <- cbind(baseline_dealers$maize.owner.agree.temp.q72,baseline_dealers$maize.owner.agree.temp.q73
                                      ,baseline_dealers$maize.owner.agree.temp.q74,baseline_dealers$maize.owner.agree.temp.q75
                                      ,baseline_dealers$maize.owner.agree.temp.q81,baseline_dealers$goodpractice_expired2)

###4. Create index: weighted average of outcomes for individual i in area j
###weight inputs (outcomes) by inverse of covariance matrix of transformed outcomes in area j
###simple way: set weight on each outcome equal to sum of its row entries in inverted covariance matrix for area j

#function that takes in data in matrix format and returns IC weights and ICW index
#wgts argument: weights can be incorporated
#revcols argument: takes vector indicating which columns should have reversed values (standardized values * -1) prior to construction of index
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

#7. Index of labor-intensive seed handling and storage practices observed by enumerator
###1. For all outcomes, switch signs where necessary so that the positive direction always indicates a "better" outcome.
#Are seed stored in a dedicated area, away from other merchandize? yes=good
baseline_dealers$mid_maize.owner.agree.temp.q69 <- baseline_dealers$maize.owner.agree.temp.q69
#baseline_dealers$mid_maize.owner.agree.temp.q69 <- ifelse(baseline_dealers$mid_maize.owner.agree.temp.q69=="Yes",1,0)

#Do you have a problem with rats or pests (insects, rats)? yes=BAD
baseline_dealers$maize.owner.agree.temp.q71_rev<-ifelse(baseline_dealers$maize.owner.agree.temp.q71==1,0,1)

baseline_dealers$mid_maize.owner.agree.temp.q71 <- baseline_dealers$maize.owner.agree.temp.q71
#baseline_dealers$mid_maize.owner.agree.temp.q71<-ifelse(baseline_dealers$mid_maize.owner.agree.temp.q71=="Yes",1,0)
baseline_dealers$mid_maize.owner.agree.temp.q71_rev<-ifelse(baseline_dealers$mid_maize.owner.agree.temp.q71==1,0,1)

#Lighting conditions in area where seed is stored? yes=good
#Wilberforce: ambient lighting condition is ideal for seed storage
baseline_dealers$mid_maize.owner.agree.temp.q78 <- baseline_dealers$maize.owner.agree.temp.q78
baseline_dealers$mid_lighting[baseline_dealers$mid_maize.owner.agree.temp.q78==1] <- 0
baseline_dealers$mid_lighting[baseline_dealers$mid_maize.owner.agree.temp.q78==2] <- 1
baseline_dealers$mid_lighting[baseline_dealers$mid_maize.owner.agree.temp.q78==3] <- 0

#On what surface are seed stored? yes=good
baseline_dealers$mid_maize.owner.agree.temp.q79 <- baseline_dealers$maize.owner.agree.temp.q79

baseline_dealers$mid_surface[baseline_dealers$mid_maize.owner.agree.temp.q79==1] <- 0
baseline_dealers$mid_surface[baseline_dealers$mid_maize.owner.agree.temp.q79==2] <- 0
baseline_dealers$mid_surface[baseline_dealers$mid_maize.owner.agree.temp.q79==3] <- 0
baseline_dealers$mid_surface[baseline_dealers$mid_maize.owner.agree.temp.q79==4] <- 1
baseline_dealers$mid_surface[baseline_dealers$mid_maize.owner.agree.temp.q79==5] <- 1
baseline_dealers$mid_surface[baseline_dealers$mid_maize.owner.agree.temp.q79==96] <- NA #because 2 names

#Do you see maize seed that is stored in open bags or open containers? yes=BAD
baseline_dealers$maize.owner.agree.temp.q80_rev <- ifelse(baseline_dealers$maize.owner.agree.temp.q80==1,0,1)

baseline_dealers$mid_maize.owner.agree.temp.q80 <- baseline_dealers$maize.owner.agree.temp.q80
#baseline_dealers$mid_maize.owner.agree.temp.q80<-ifelse(baseline_dealers$mid_maize.owner.agree.temp.q80=="Yes",1,0)
baseline_dealers$mid_maize.owner.agree.temp.q80_rev <- ifelse(baseline_dealers$mid_maize.owner.agree.temp.q80==1,0,1)

#On a scale of 1 to 5, rate this shop in terms of cleanness and professionality yes=good
baseline_dealers$mid_maize.owner.agree.temp.q82 <- baseline_dealers$maize.owner.agree.temp.q82

###3. Define groupings/areas/domains of outcomes: each outcome is assigned to one of these areas
variables_practices_lab_mid <- cbind(baseline_dealers$mid_maize.owner.agree.temp.q69,baseline_dealers$mid_maize.owner.agree.temp.q71_rev
                                     ,baseline_dealers$mid_lighting,baseline_dealers$mid_surface
                                     ,baseline_dealers$mid_maize.owner.agree.temp.q80_rev,baseline_dealers$mid_maize.owner.agree.temp.q82)
variables_practices_lab_base <- cbind(baseline_dealers$maize.owner.agree.temp.q69,baseline_dealers$maize.owner.agree.temp.q71_rev
                                      ,baseline_dealers$lighting,baseline_dealers$surface,baseline_dealers$maize.owner.agree.temp.q80_rev
                                      ,baseline_dealers$maize.owner.agree.temp.q82)


#8. Index of all seed handling and storage practices observed by enumerator
###3. Define groupings/areas/domains of outcomes: each outcome is assigned to one of these areas
variables_practices_all_mid <- cbind(baseline_dealers$mid_maize.owner.agree.temp.q72,baseline_dealers$mid_maize.owner.agree.temp.q73
                                     ,baseline_dealers$mid_maize.owner.agree.temp.q74,baseline_dealers$mid_maize.owner.agree.temp.q75
                                     ,baseline_dealers$mid_maize.owner.agree.temp.q81,baseline_dealers$mid_goodpractice_expired2
                                     ,baseline_dealers$mid_maize.owner.agree.temp.q69,baseline_dealers$mid_maize.owner.agree.temp.q71_rev
                                     ,baseline_dealers$mid_lighting,baseline_dealers$mid_surface
                                     ,baseline_dealers$mid_maize.owner.agree.temp.q80_rev,baseline_dealers$mid_maize.owner.agree.temp.q82)
variables_practices_all_base <- cbind(baseline_dealers$maize.owner.agree.temp.q72,baseline_dealers$maize.owner.agree.temp.q73
                                      ,baseline_dealers$maize.owner.agree.temp.q74,baseline_dealers$maize.owner.agree.temp.q75
                                      ,baseline_dealers$maize.owner.agree.temp.q81,baseline_dealers$goodpractice_expired2
                                      ,baseline_dealers$maize.owner.agree.temp.q69,baseline_dealers$maize.owner.agree.temp.q71_rev
                                      ,baseline_dealers$lighting,baseline_dealers$surface,baseline_dealers$maize.owner.agree.temp.q80_rev
                                      ,baseline_dealers$maize.owner.agree.temp.q82)


#9. Index of efforts of dealer and services offered by dealer
###1. For all outcomes, switch signs where necessary so that the positive direction always indicates a "better" outcome.
#When farmers buy seed, do you explain how the seed should be used (seed spacing, seed rate, complementary inputs) yes=good
baseline_dealers$mid_maize.owner.agree.q85 <- baseline_dealers$maize.owner.agree.q85
baseline_dealers$mid_alwaysexplains[baseline_dealers$mid_maize.owner.agree.q85=="a"] <- 0
baseline_dealers$mid_alwaysexplains[baseline_dealers$mid_maize.owner.agree.q85=="b"] <- 0
baseline_dealers$mid_alwaysexplains[baseline_dealers$mid_maize.owner.agree.q85=="c"] <- 1

#When farmers buy seed, do you usually recommend complementary inputs (fertilizer, chemical,.) yes=good
baseline_dealers$mid_maize.owner.agree.q86 <- baseline_dealers$maize.owner.agree.q86
baseline_dealers$mid_alwaysrecom[baseline_dealers$mid_maize.owner.agree.q86=="a"] <- 0
baseline_dealers$mid_alwaysrecom[baseline_dealers$mid_maize.owner.agree.q86=="b"] <- 0
baseline_dealers$mid_alwaysrecom[baseline_dealers$mid_maize.owner.agree.q86=="c"] <- 1

#Do you offer extension/training to your clients on how to use improved seed varieties? yes=good
baseline_dealers$mid_maize.owner.agree.q87 <- baseline_dealers$maize.owner.agree.q87
baseline_dealers$mid_extension[baseline_dealers$mid_maize.owner.agree.q87=="1"] <- 0
baseline_dealers$mid_extension[baseline_dealers$mid_maize.owner.agree.q87=="2"] <- 1
baseline_dealers$mid_extension[baseline_dealers$mid_maize.owner.agree.q87=="3"] <- 1

#Did you offer discounts to clients that buy large quantities of maize seed during the second season of 2020? yes=good
baseline_dealers$mid_maize.owner.agree.q88 <- baseline_dealers$maize.owner.agree.q88
#baseline_dealers$mid_maize.owner.agree.q88<-ifelse(baseline_dealers$mid_maize.owner.agree.q88=="Yes",1,0)

#What is that smallest package of improved seed (OPV/hybird) that you stocked during this season (without repackaging) yes=good
baseline_dealers$mid_maize.owner.agree.q89 <- baseline_dealers$maize.owner.agree.q89
baseline_dealers$mid_q89_bin[baseline_dealers$mid_maize.owner.agree.q89=="1"] <- 1
baseline_dealers$mid_q89_bin[baseline_dealers$mid_maize.owner.agree.q89=="2"] <- 0
baseline_dealers$mid_q89_bin[baseline_dealers$mid_maize.owner.agree.q89=="3"] <- 0
baseline_dealers$mid_q89_bin[baseline_dealers$mid_maize.owner.agree.q89=="4"] <- 0
baseline_dealers$mid_q89_bin[baseline_dealers$mid_maize.owner.agree.q89=="other"] <- NA

#Do you provide seed on credit (pay after harvest)? yes=good
baseline_dealers$mid_maize.owner.agree.q93 <- baseline_dealers$maize.owner.agree.q93
baseline_dealers$mid_q93_bin[baseline_dealers$mid_maize.owner.agree.q93=="1"] <- 0
baseline_dealers$mid_q93_bin[baseline_dealers$mid_maize.owner.agree.q93=="2"] <- 1
baseline_dealers$mid_q93_bin[baseline_dealers$mid_maize.owner.agree.q93=="3"] <- 1

#Since last season, did you receive any complaint from a customer that seed you sold was not good? yes=BAD
baseline_dealers$maize.owner.agree.q96_rev<-ifelse(baseline_dealers$maize.owner.agree.q96==1,0,1)

baseline_dealers$mid_maize.owner.agree.q96 <- baseline_dealers$maize.owner.agree.q96
#baseline_dealers$mid_maize.owner.agree.q96<-ifelse(baseline_dealers$mid_maize.owner.agree.q96=="Yes",1,0)
baseline_dealers$mid_maize.owner.agree.q96_rev<-ifelse(baseline_dealers$mid_maize.owner.agree.q96==1,0,1)

#What payment modalities do you accept?
baseline_dealers$mid_maize.owner.agree.q97.b <- baseline_dealers$maize.owner.agree.q97.b
#baseline_dealers$mid_maize.owner.agree.q97.b<-ifelse(baseline_dealers$mid_maize.owner.agree.q97.b=="True",1,0)

###3. Define groupings/areas/domains of outcomes: each outcome is assigned to one of these areas
variables_efforts_mid <- cbind(baseline_dealers$mid_alwaysexplains,baseline_dealers$mid_alwaysrecom,baseline_dealers$mid_extension
                               ,baseline_dealers$mid_maize.owner.agree.q88,baseline_dealers$mid_q89_bin,baseline_dealers$mid_q93_bin
                               ,baseline_dealers$mid_maize.owner.agree.q96_rev,baseline_dealers$mid_maize.owner.agree.q97.b)
variables_efforts_base <- cbind(baseline_dealers$alwaysexplains,baseline_dealers$alwaysrecom,baseline_dealers$extension
                                ,baseline_dealers$maize.owner.agree.q88,baseline_dealers$q89_bin,baseline_dealers$q93_bin
                                ,baseline_dealers$maize.owner.agree.q96_rev,baseline_dealers$maize.owner.agree.q97.b)


#10. Overall index of primary agro-input dealer outcome variables
###Anderson, 2008: https://are.berkeley.edu/~mlanderson/pdf/Anderson%202008a.pdf p. 1485
###1. For all outcomes, switch signs where necessary so that the positive direction always indicates a "better" outcome.
#1. YES: Cumulative quantity sold of a hybrid and a open-pollinated maize variety last season in kg: more=better
#2. NO: Sales prices of a hybrid and an open-pollinated maize variety at beginning of last season in UGX per kg
#3. NO: Seed revenue in UGX: quantities sold * prices of hybrid and open-pollinated maize variety: more=better
#4. YES: Number of customers who bought maize seed on average day at beginning of last season: more=better: 
#5. YES: Moisture content of random seed bag: less=better
baseline_dealers$reading_pos <- baseline_dealers$reading*-1
baseline_dealers$mid_reading_pos <- baseline_dealers$mid_reading*-1
#6. YES: Index of capital-intensive seed handling and storage practices observed by enumerator: more=better
#7. YES: Index of labor-intensive seed handling and storage practices observed by enumerator: more=better
#8. NO: Index of all seed handling and storage practices observed by enumerator: more=better
#9. YES: Index of efforts of dealer and services offered by dealer: more=better

###3. Define groupings/areas/domains of outcomes: each outcome is assigned to one of these areas
variables_overall_prim_dealer_mid <- cbind(baseline_dealers$mid_quantitysold,baseline_dealers$mid_maize.owner.agree.q7
                                           ,baseline_dealers$mid_reading_pos
                                           ,baseline_dealers$index_practices_cap_mid,baseline_dealers$index_practices_lab_mid
                                           ,baseline_dealers$index_efforts_mid)
variables_overall_prim_dealer_base <- cbind(baseline_dealers$quantitysold,baseline_dealers$maize.owner.agree.q7
                                            ,baseline_dealers$reading_pos
                                            ,baseline_dealers$index_practices_cap_base,baseline_dealers$index_practices_lab_base
                                            ,baseline_dealers$index_efforts_base)

################################################################################################################################################################################
###4. Create index: weighted average of outcomes for individual i in area j

###
#1#
###

#6.
index_practices_cap_mid <- icwIndex(xmat=variables_practices_cap_mid)
baseline_dealers$index_practices_cap_mid <- index_practices_cap_mid$index #midline index

index_practices_cap_base <- icwIndex(xmat=variables_practices_cap_base)
baseline_dealers$index_practices_cap_base <- index_practices_cap_base$index #baseline index

#7.
index_practices_lab_mid <- icwIndex(xmat=variables_practices_lab_mid)
baseline_dealers$index_practices_lab_mid <- index_practices_lab_mid$index

index_practices_lab_base <- icwIndex(xmat=variables_practices_lab_base)
baseline_dealers$index_practices_lab_base <- index_practices_lab_base$index

#8.
index_practices_all_mid <- icwIndex(xmat=variables_practices_all_mid)
baseline_dealers$index_practices_all_mid <- index_practices_all_mid$index

index_practices_all_base <- icwIndex(xmat=variables_practices_all_base)
baseline_dealers$index_practices_all_base <- index_practices_all_base$index

#9.
index_efforts_mid <- icwIndex(xmat=variables_efforts_mid)
baseline_dealers$index_efforts_mid <- index_efforts_mid$index

index_efforts_base <- icwIndex(xmat=variables_efforts_base)
baseline_dealers$index_efforts_base <- index_efforts_base$index

#10.
index_overall_prim_dealer_mid <- icwIndex(xmat=variables_overall_prim_dealer_mid)
baseline_dealers$index_overall_prim_dealer_mid <- index_overall_prim_dealer_mid$index

index_overall_prim_dealer_base <- icwIndex(xmat=variables_overall_prim_dealer_base)
baseline_dealers$index_overall_prim_dealer_base <- index_overall_prim_dealer_base$index

results_dealer_prim <- c("mid_quantitysold","mid_av_salesprices","mid_revenue","mid_maize.owner.agree.q7","mid_reading","index_practices_cap_mid"
                         ,"index_practices_lab_mid","index_practices_all_mid","index_efforts_mid"
                         ,"index_overall_prim_dealer_mid")
results_dealer_prim_base <- c("quantitysold","av_salesprices","revenue","maize.owner.agree.q7","reading","index_practices_cap_base"
                              ,"index_practices_lab_base","index_practices_all_base","index_efforts_base"
                              ,"index_overall_prim_dealer_base")

df_means_D_prim <- array(NA,dim=c(3,10))

for (i in 1:length(results_dealer_prim)){
  df_means_D_prim[1,i] <- sum(baseline_dealers[results_dealer_prim[i]], na.rm=T)/(nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_prim[i]])))
  df_means_D_prim[2,i] <- sqrt(var(baseline_dealers[results_dealer_prim[i]], na.rm=T))
  df_means_D_prim[3,i] <- nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_prim[i]]))-sum(is.na(baseline_dealers[results_dealer_prim_base[i]]))+sum(is.na(baseline_dealers[results_dealer_prim[i]])&is.na(baseline_dealers[results_dealer_prim_base[i]]))}

df_ols_D_prim <- array(NA,dim=c(3,3,10))

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
index_practices_lab_mid <- icwIndex(xmat=variables_practices_lab_mid,sgroup = baseline_dealers$training_control)
baseline_dealers$index_practices_lab_midT <- index_practices_lab_mid$index

index_practices_lab_base <- icwIndex(xmat=variables_practices_lab_base,sgroup = baseline_dealers$training_control)
baseline_dealers$index_practices_lab_baseT <- index_practices_lab_base$index

#8.
index_practices_all_mid <- icwIndex(xmat=variables_practices_all_mid,sgroup = baseline_dealers$training_control)
baseline_dealers$index_practices_all_midT <- index_practices_all_mid$index

index_practices_all_base <- icwIndex(xmat=variables_practices_all_base,sgroup = baseline_dealers$training_control)
baseline_dealers$index_practices_all_baseT <- index_practices_all_base$index

#9.
index_efforts_mid <- icwIndex(xmat=variables_efforts_mid,sgroup = baseline_dealers$training_control)
baseline_dealers$index_efforts_midT <- index_efforts_mid$index

index_efforts_base <- icwIndex(xmat=variables_efforts_base,sgroup = baseline_dealers$training_control)
baseline_dealers$index_efforts_baseT <- index_efforts_base$index

#10.
index_overall_prim_dealer_mid <- icwIndex(xmat=variables_overall_prim_dealer_mid,sgroup = baseline_dealers$training_control)
baseline_dealers$index_overall_prim_dealer_midT <- index_overall_prim_dealer_mid$index

index_overall_prim_dealer_base <- icwIndex(xmat=variables_overall_prim_dealer_base,sgroup = baseline_dealers$training_control)
baseline_dealers$index_overall_prim_dealer_baseT <- index_overall_prim_dealer_base$index

results_dealer_prim <- c("mid_quantitysold","mid_av_salesprices","mid_revenue","mid_maize.owner.agree.q7","mid_reading","index_practices_cap_midT"
                         ,"index_practices_lab_midT","index_practices_all_midT","index_efforts_midT"
                         ,"index_overall_prim_dealer_midT")
results_dealer_prim_base <- c("quantitysold","av_salesprices","revenue","maize.owner.agree.q7","reading","index_practices_cap_baseT"
                              ,"index_practices_lab_baseT","index_practices_all_baseT","index_efforts_baseT"
                              ,"index_overall_prim_dealer_baseT")

for (i in 1:length(results_dealer_prim)){
  ols <- lm(as.formula(paste(paste(results_dealer_prim[i],"training*clearing*farmer",sep="~"),results_dealer_prim_base[i],sep="+")),data=baseline_dealers)
  #ols <-  ols <- lm(as.formula(paste(paste(results_dealer_prim[i],"training*clearing*farmer",sep="~"),results_dealer_prim_base[i],sep="+")),data=baseline_dealers) lm(as.formula(paste(results_dealer_prim[i],"training*clearing*farmer",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")

  df_ols_D_prim[1,1,i] <- coef_test(ols, vcov_cluster)[2,1]
  df_ols_D_prim[2,1,i] <- coef_test(ols, vcov_cluster)[2,2]
  df_ols_D_prim[3,1,i] <- coef_test(ols, vcov_cluster)[2,5]}

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
index_practices_lab_mid <- icwIndex(xmat=variables_practices_lab_mid,sgroup = baseline_dealers$clearing_control)
baseline_dealers$index_practices_lab_midC <- index_practices_lab_mid$index

index_practices_lab_base <- icwIndex(xmat=variables_practices_lab_base,sgroup = baseline_dealers$clearing_control)
baseline_dealers$index_practices_lab_baseC <- index_practices_lab_base$index

#8.
index_practices_all_mid <- icwIndex(xmat=variables_practices_all_mid,sgroup = baseline_dealers$clearing_control)
baseline_dealers$index_practices_all_midC <- index_practices_all_mid$index

index_practices_all_base <- icwIndex(xmat=variables_practices_all_base,sgroup = baseline_dealers$clearing_control)
baseline_dealers$index_practices_all_baseC <- index_practices_all_base$index

#9.
index_efforts_mid <- icwIndex(xmat=variables_efforts_mid,sgroup = baseline_dealers$clearing_control)
baseline_dealers$index_efforts_midC <- index_efforts_mid$index

index_efforts_base <- icwIndex(xmat=variables_efforts_base,sgroup = baseline_dealers$clearing_control)
baseline_dealers$index_efforts_baseC <- index_efforts_base$index

#10.
index_overall_prim_dealer_mid <- icwIndex(xmat=variables_overall_prim_dealer_mid,sgroup = baseline_dealers$clearing_control)
baseline_dealers$index_overall_prim_dealer_midC <- index_overall_prim_dealer_mid$index

index_overall_prim_dealer_base <- icwIndex(xmat=variables_overall_prim_dealer_base,sgroup = baseline_dealers$clearing_control)
baseline_dealers$index_overall_prim_dealer_baseC <- index_overall_prim_dealer_base$index

results_dealer_prim <- c("mid_quantitysold","mid_av_salesprices","mid_revenue","mid_maize.owner.agree.q7","mid_reading","index_practices_cap_midC"
                         ,"index_practices_lab_midC","index_practices_all_midC","index_efforts_midC"
                         ,"index_overall_prim_dealer_midC")
results_dealer_prim_base <- c("quantitysold","av_salesprices","revenue","maize.owner.agree.q7","reading","index_practices_cap_baseC","index_practices_lab_baseC"
                              ,"index_practices_all_baseC","index_efforts_baseC","index_overall_prim_dealer_baseC")

for (i in 1:length(results_dealer_prim)){
  ols <- lm(as.formula(paste(paste(results_dealer_prim[i],"training*clearing*farmer",sep="~"),results_dealer_prim_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_prim[i],"training*clearing*farmer",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")

  df_ols_D_prim[1,2,i] <- coef_test(ols, vcov_cluster)[3,1]
  df_ols_D_prim[2,2,i] <- coef_test(ols, vcov_cluster)[3,2]
  df_ols_D_prim[3,2,i] <- coef_test(ols, vcov_cluster)[3,5]}

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
index_practices_lab_mid <- icwIndex(xmat=variables_practices_lab_mid,sgroup = baseline_dealers$farmer_control)
baseline_dealers$index_practices_lab_midF <- index_practices_lab_mid$index

index_practices_lab_base <- icwIndex(xmat=variables_practices_lab_base,sgroup = baseline_dealers$farmer_control)
baseline_dealers$index_practices_lab_baseF <- index_practices_lab_base$index

#8.
index_practices_all_mid <- icwIndex(xmat=variables_practices_all_mid,sgroup = baseline_dealers$farmer_control)
baseline_dealers$index_practices_all_midF <- index_practices_all_mid$index

index_practices_all_base <- icwIndex(xmat=variables_practices_all_base,sgroup = baseline_dealers$farmer_control)
baseline_dealers$index_practices_all_baseF <- index_practices_all_base$index

#9.
index_efforts_mid <- icwIndex(xmat=variables_efforts_mid,sgroup = baseline_dealers$farmer_control)
baseline_dealers$index_efforts_midF <- index_efforts_mid$index

index_efforts_base <- icwIndex(xmat=variables_efforts_base,sgroup = baseline_dealers$farmer_control)
baseline_dealers$index_efforts_baseF <- index_efforts_base$index

#10.
index_overall_prim_dealer_mid <- icwIndex(xmat=variables_overall_prim_dealer_mid,sgroup = baseline_dealers$farmer_control)
baseline_dealers$index_overall_prim_dealer_midF <- index_overall_prim_dealer_mid$index

index_overall_prim_dealer_base <- icwIndex(xmat=variables_overall_prim_dealer_base,sgroup = baseline_dealers$farmer_control)
baseline_dealers$index_overall_prim_dealer_baseF <- index_overall_prim_dealer_base$index

results_dealer_prim <- c("mid_quantitysold","mid_av_salesprices","mid_revenue","mid_maize.owner.agree.q7","mid_reading","index_practices_cap_midF"
                         ,"index_practices_lab_midF","index_practices_all_midF","index_efforts_midF"
                         ,"index_overall_prim_dealer_midF")
results_dealer_prim_base <- c("quantitysold","av_salesprices","revenue","maize.owner.agree.q7","reading","index_practices_cap_baseF","index_practices_lab_baseF"
                              ,"index_practices_all_baseF","index_efforts_baseF","index_overall_prim_dealer_baseF")

for (i in 1:length(results_dealer_prim)){
  ols <- lm(as.formula(paste(paste(results_dealer_prim[i],"training*clearing*farmer",sep="~"),results_dealer_prim_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_prim[i],"training*clearing*farmer",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")

  #farmer video treatment at village/shop level so no clustering needed
  df_ols_D_prim[1,3,i] <- summary(ols)$coefficients[4,1]
  df_ols_D_prim[2,3,i] <- summary(ols)$coefficients[4,2]
  df_ols_D_prim[3,3,i] <- summary(ols)$coefficients[4,4]}

#Aker, Boumnijel, McClelland, Tierney (2012)

# ###Caro's cumbersome solution for baseline_dealers$mid_av_salesprices:
# #need mean correlation among outcomes other than outcome k (r_.k)
# #--> outcomes other than outcome k
# df_dealer_prim <- data.frame(baseline_dealers$mid_quantitysold,baseline_dealers$mid_revenue
#                              ,baseline_dealers$mid_maize.owner.agree.q7,baseline_dealers$mid_reading
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
# p_k_T = df_ols_D_prim[3,1,2] #training
# p_k_CH = df_ols_D_prim[3,2,2] #CH
# p_k_F = df_ols_D_prim[3,3,2] #video
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
                             ,baseline_dealers$mid_maize.owner.agree.q7,baseline_dealers$mid_reading,baseline_dealers$index_practices_cap_midT
                             ,baseline_dealers$index_practices_lab_midT,baseline_dealers$index_practices_all_midT
                             ,baseline_dealers$index_efforts_midT)
df_dealer_primC <- data.frame(baseline_dealers$mid_quantitysold,baseline_dealers$mid_av_salesprices,baseline_dealers$mid_revenue
                             ,baseline_dealers$mid_maize.owner.agree.q7,baseline_dealers$mid_reading,baseline_dealers$index_practices_cap_midC
                             ,baseline_dealers$index_practices_lab_midC,baseline_dealers$index_practices_all_midC
                             ,baseline_dealers$index_efforts_midC)
df_dealer_primF <- data.frame(baseline_dealers$mid_quantitysold,baseline_dealers$mid_av_salesprices,baseline_dealers$mid_revenue
                             ,baseline_dealers$mid_maize.owner.agree.q7,baseline_dealers$mid_reading,baseline_dealers$index_practices_cap_midF
                             ,baseline_dealers$index_practices_lab_midF,baseline_dealers$index_practices_all_midF
                             ,baseline_dealers$index_efforts_midF)
#no index_overall_prim_dealer_midF

#example
adjust_p(0.03,df_dealer_primT,9)

df_ols_D_prim_J <- array(NA,dim=c(3,3,9))

results_dealer_prim_J <- c("mid_quantitysold","mid_av_salesprices","mid_revenue","mid_maize.owner.agree.q7","mid_reading","index_practices_cap_mid"
                         ,"index_practices_lab_mid","index_practices_all_mid","index_efforts_mid")
#no index_overall_prim_dealer_midF

for (i in 1:length(results_dealer_prim_J)){
  df_ols_D_prim_J[3,1,i] <- adjust_p(df_ols_D_prim[3,1,i],df_dealer_primT,i)
  df_ols_D_prim_J[3,2,i] <- adjust_p(df_ols_D_prim[3,2,i],df_dealer_primC,i)
  df_ols_D_prim_J[3,3,i] <- adjust_p(df_ols_D_prim[3,3,i],df_dealer_primF,i)}










################################################################################################################################################################################
##### 2 ANALYSIS: Agro-input dealer - Secondary: outcomes without baseline######################################################################################################
################################################################################################################################################################################

#1. Index of dealer's motivation and satisfaction
###Anderson, 2008: https://are.berkeley.edu/~mlanderson/pdf/Anderson%202008a.pdf p. 1485
###1. For all outcomes, switch signs where necessary so that the positive direction always indicates a "better" outcome.

#Do you see yourself working as an agro-input dealer 3 years from now? (q9a): yes=good
baseline_dealers$mid_maize.owner.agree.q9_a <- (rbinom(348, 1, 0.5)+0.1*baseline_dealers$training+0.1*baseline_dealers$clearing+0.1*baseline_dealers$farmer)

#Do you think your job makes a positive difference in other's life? (q9b): yes=good
baseline_dealers$mid_maize.owner.agree.q9.b <- (rbinom(348, 1, 0.5)+0.1*baseline_dealers$training+0.1*baseline_dealers$clearing+0.1*baseline_dealers$farmer)

#On a scale of 1 to 5, how likely are you to recommend working as an agro-input dealer to friends or family? (q9c) more=better
baseline_dealers$mid_maize.owner.agree.q9_c <- (rbinom(348, 1, 0.5)+0.1*baseline_dealers$training+0.1*baseline_dealers$clearing+0.1*baseline_dealers$farmer)

#On a scale of 1 to 5, how happy do you feel when you come to work in the morning? (q9d) more=better
baseline_dealers$mid_maize.owner.agree.q9_d <- (rbinom(348, 1, 0.5)+0.1*baseline_dealers$training+0.1*baseline_dealers$clearing+0.1*baseline_dealers$farmer)

###3. Define groupings/areas/domains of outcomes: each outcome is assigned to one of these areas
variables_motivation_mid <- cbind(baseline_dealers$mid_maize.owner.agree.q9_a,baseline_dealers$mid_maize.owner.agree.q9.b
                                  ,baseline_dealers$mid_maize.owner.agree.q9_c,baseline_dealers$mid_maize.owner.agree.q9_d)

################################################################################################################################################################################
###4. Create index: weighted average of outcomes for individual i in area j

###
#1#
###

#1.
index_motivation_mid <- icwIndex(xmat=variables_motivation_mid)
baseline_dealers$index_motivation_mid <- index_motivation_mid$index #midline index

results_dealer_sec_nobase <- c("index_motivation_mid")

df_means_D_sec_nobase <- array(NA,dim=c(3,10))

for (i in 1:length(results_dealer_sec_nobase)){
  df_means_D_sec_nobase[1,i] <- sum(baseline_dealers[results_dealer_sec_nobase[i]], na.rm=T)/(nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_sec_nobase[i]])))
  df_means_D_sec_nobase[2,i] <- sqrt(var(baseline_dealers[results_dealer_sec_nobase[i]], na.rm=T))
  df_means_D_sec_nobase[3,i] <- nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_sec_nobase[i]]))}

###
#2#
###

df_ols_D_sec_nobase <- array(NA,dim=c(3,3,10))

baseline_dealers$training_control[baseline_dealers$training==0] <- TRUE
baseline_dealers$training_control[baseline_dealers$training==1] <- FALSE

#1.
index_motivation_mid <- icwIndex(xmat=variables_motivation_mid,sgroup = baseline_dealers$training_control)
baseline_dealers$index_motivation_midT <- index_motivation_mid$index

results_dealer_sec_nobase <- c("index_motivation_mid")

for (i in 1:length(results_dealer_sec_nobase)){
  ols <- lm(as.formula(paste(results_dealer_sec_nobase[i],"training*clearing*farmer",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  df_ols_D_sec_nobase[1,1,i] <- coef_test(ols, vcov_cluster)[2,1]
  df_ols_D_sec_nobase[2,1,i] <- coef_test(ols, vcov_cluster)[2,2]
  df_ols_D_sec_nobase[3,1,i] <- coef_test(ols, vcov_cluster)[2,5]}

###
#3#
###

baseline_dealers$clearing_control[baseline_dealers$clearing==0] <- TRUE
baseline_dealers$clearing_control[baseline_dealers$clearing==1] <- FALSE

#1.
index_motivation_mid <- icwIndex(xmat=variables_motivation_mid,sgroup = baseline_dealers$clearing_control)
baseline_dealers$index_motivation_midC <- index_motivation_mid$index

results_dealer_sec_nobase <- c("index_motivation_mid")

for (i in 1:length(results_dealer_sec_nobase)){
  ols <- lm(as.formula(paste(results_dealer_sec_nobase[i],"training*clearing*farmer",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  df_ols_D_sec_nobase[1,2,i] <- coef_test(ols, vcov_cluster)[3,1]
  df_ols_D_sec_nobase[2,2,i] <- coef_test(ols, vcov_cluster)[3,2]
  df_ols_D_sec_nobase[3,2,i] <- coef_test(ols, vcov_cluster)[3,5]}

###
#4#
###

baseline_dealers$farmer_control[baseline_dealers$farmer==0] <- TRUE
baseline_dealers$farmer_control[baseline_dealers$farmer==1] <- FALSE

#1.
index_motivation_mid <- icwIndex(xmat=variables_motivation_mid,sgroup = baseline_dealers$farmer_control)
baseline_dealers$index_motivation_midF <- index_motivation_mid$index

results_dealer_sec_nobase <- c("index_motivation_mid")

for (i in 1:length(results_dealer_sec_nobase)){
  ols <- lm(as.formula(paste(results_dealer_sec_nobase[i],"training*clearing*farmer",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  #farmer video treatment at village/shop level so no clustering needed
  df_ols_D_sec_nobase[1,3,i] <- summary(ols)$coefficients[4,1]
  df_ols_D_sec_nobase[2,3,i] <- summary(ols)$coefficients[4,2]
  df_ols_D_sec_nobase[3,3,i] <- summary(ols)$coefficients[4,4]}










#############################################################################################################################################################################
##### 3 ANALYSIS: Agro-input dealer - Secondary: outcomes with baseline######################################################################################################
#############################################################################################################################################################################

#1. Number of maize varieties in stock last season (incl. hybrids, OPV, landraces)
baseline_dealers <- trim("maize.owner.agree.nr_var",baseline_dealers,trim_perc=.01)
baseline_dealers$mid_maize.owner.agree.nr_var <- (baseline_dealers$maize.owner.agree.nr_var+0.2916667*baseline_dealers$training+0.2916667*baseline_dealers$clearing+0.2916667*baseline_dealers$farmer)
baseline_dealers <- trim("mid_maize.owner.agree.nr_var",baseline_dealers,trim_perc=.01)

#2. Number of hybrid maize varieties in stock last season
baseline_dealers <- trim("maize.owner.agree.q19",baseline_dealers,trim_perc=.01)
baseline_dealers$mid_maize.owner.agree.q19 <- (baseline_dealers$maize.owner.agree.q19+0.1644509*baseline_dealers$training+0.1644509*baseline_dealers$clearing+0.1644509*baseline_dealers$farmer)
baseline_dealers <- trim("mid_maize.owner.agree.q19",baseline_dealers,trim_perc=.01)

#3. Number of OP maize varieties in stock last season
baseline_dealers <- trim("maize.owner.agree.q44",baseline_dealers,trim_perc=.01)
baseline_dealers$mid_maize.owner.agree.q44 <- (baseline_dealers$maize.owner.agree.q44+0.126513*baseline_dealers$training+0.126513*baseline_dealers$clearing+0.126513*baseline_dealers$farmer)
baseline_dealers <- trim("mid_maize.owner.agree.q44",baseline_dealers,trim_perc=.01)

#4. Index of dealer's self-ratings on location, price, product quality, stock & convenient quantities, reputation
###1. For all outcomes, switch signs where necessary so that the positive direction always indicates a "better" outcome.
baseline_dealers$mid_maize.owner.agree.q99 <- (baseline_dealers$maize.owner.agree.q99+0.3876437*baseline_dealers$training+0.3876437*baseline_dealers$clearing+0.3876437*baseline_dealers$farmer)
baseline_dealers$mid_maize.owner.agree.q100 <- (baseline_dealers$maize.owner.agree.q100+0.3922414*baseline_dealers$training+0.3922414*baseline_dealers$clearing+0.3922414*baseline_dealers$farmer)
baseline_dealers$mid_maize.owner.agree.q101 <- (baseline_dealers$maize.owner.agree.q101+0.4045977*baseline_dealers$training+0.4045977*baseline_dealers$clearing+0.4045977*baseline_dealers$farmer)
baseline_dealers$mid_maize.owner.agree.q102 <- (baseline_dealers$maize.owner.agree.q102+0.3583333*baseline_dealers$training+0.3583333*baseline_dealers$clearing+0.3583333*baseline_dealers$farmer)
baseline_dealers$mid_maize.owner.agree.q103 <- (baseline_dealers$maize.owner.agree.q103+0.4318966*baseline_dealers$training+0.4318966*baseline_dealers$clearing+0.4318966*baseline_dealers$farmer)

###3. Define groupings/areas/domains of outcomes: each outcome is assigned to one of these areas
variables_selfratings_mid <- cbind(baseline_dealers$mid_maize.owner.agree.q99,baseline_dealers$mid_maize.owner.agree.q100
                                  ,baseline_dealers$mid_maize.owner.agree.q101,baseline_dealers$mid_maize.owner.agree.q102
                                  ,baseline_dealers$mid_maize.owner.agree.q103)
variables_selfratings_base <- cbind(baseline_dealers$maize.owner.agree.q99,baseline_dealers$maize.owner.agree.q100
                                   ,baseline_dealers$maize.owner.agree.q101,baseline_dealers$maize.owner.agree.q102
                                   ,baseline_dealers$maize.owner.agree.q103)

#5. Index of dealer's efforts and services according to farmers (who (know someone who) bought seed there)
###1. For all outcomes, switch signs where necessary so that the positive direction always indicates a "better" outcome.
baseline_dealers$mid_refunds <- (baseline_dealers$refunds+0.0331466*baseline_dealers$training+0.0331466*baseline_dealers$clearing+0.0331466*baseline_dealers$farmer)
#baseline_dealers$mid_refunds<-ifelse(baseline_dealers$mid_refunds=="Yes",1,0)
baseline_dealers$mid_refunds<-as.numeric(as.character(baseline_dealers$mid_refunds))

baseline_dealers$mid_gives_credit <- (baseline_dealers$gives_credit+0.04098623*baseline_dealers$training+0.04098623*baseline_dealers$clearing+0.04098623*baseline_dealers$farmer)
#baseline_dealers$mid_gives_credit<-ifelse(baseline_dealers$mid_gives_credit=="Yes",1,0)
baseline_dealers$mid_gives_credit<-as.numeric(as.character(baseline_dealers$mid_gives_credit))

baseline_dealers$mid_gives_advice <- (baseline_dealers$gives_advice+0.07573887*baseline_dealers$training+0.07573887*baseline_dealers$clearing+0.07573887*baseline_dealers$farmer)
#baseline_dealers$mid_gives_advice<-ifelse(baseline_dealers$mid_gives_advice=="Yes",1,0)
baseline_dealers$mid_gives_advice<-as.numeric(as.character(baseline_dealers$mid_gives_advice))

baseline_dealers$mid_delivers <- (baseline_dealers$delivers+0.02347069*baseline_dealers$training+0.02347069*baseline_dealers$clearing+0.02347069*baseline_dealers$farmer)
#baseline_dealers$mid_delivers<-ifelse(baseline_dealers$mid_delivers=="Yes",1,0)
baseline_dealers$mid_delivers<-as.numeric(as.character(baseline_dealers$mid_delivers))

baseline_dealers$mid_after_sales_service <- (baseline_dealers$after_sales_service+0.02405475*baseline_dealers$training+0.02405475*baseline_dealers$clearing+0.02405475*baseline_dealers$farmer)
#baseline_dealers$mid_after_sales_service<-ifelse(baseline_dealers$mid_after_sales_service=="Yes",1,0)
baseline_dealers$mid_after_sales_service<-as.numeric(as.character(baseline_dealers$mid_after_sales_service))

baseline_dealers$mid_payment_mehtods <- (baseline_dealers$payment_mehtods+0.04195275*baseline_dealers$training+0.04195275*baseline_dealers$clearing+0.04195275*baseline_dealers$farmer)
#baseline_dealers$mid_payment_mehtods <- ifelse(baseline_dealers$mid_payment_mehtods=="Yes",1,0)
baseline_dealers$mid_payment_mehtods <- as.numeric(as.character(baseline_dealers$mid_payment_mehtods))

baseline_dealers$mid_small_quant <- (baseline_dealers$small_quant+0.08976569*baseline_dealers$training+0.08976569*baseline_dealers$clearing+0.08976569*baseline_dealers$farmer)
#baseline_dealers$mid_small_quant<-ifelse(baseline_dealers$mid_small_quant=="Yes",1,0)
baseline_dealers$mid_small_quant <- as.numeric(as.character(baseline_dealers$mid_small_quant))

###3. Define groupings/areas/domains of outcomes: each outcome is assigned to one of these areas
variables_servicesFARM_mid <- cbind(baseline_dealers$mid_refunds,baseline_dealers$mid_gives_credit,baseline_dealers$mid_gives_advice
                               ,baseline_dealers$mid_delivers,baseline_dealers$mid_after_sales_service,baseline_dealers$mid_payment_mehtods
                               ,baseline_dealers$mid_small_quant)
variables_servicesFARM_base <- cbind(baseline_dealers$refunds,baseline_dealers$gives_credit,baseline_dealers$gives_advice
                                ,baseline_dealers$delivers,baseline_dealers$after_sales_service,baseline_dealers$payment_mehtods
                                ,baseline_dealers$small_quant)

#6. Index of dealer's knowledge about seed storage
baseline_dealers$maize.owner.agree.skill.q104_bin <- ifelse(baseline_dealers$maize.owner.agree.skill.q104=="b",1,0)
baseline_dealers$mid_maize.owner.agree.skill.q104 <- baseline_dealers$maize.owner.agree.skill.q104
baseline_dealers$mid_maize.owner.agree.skill.q104_bin <- ifelse(baseline_dealers$mid_maize.owner.agree.skill.q104=="b",1,0)

baseline_dealers$maize.owner.agree.skill.q105_bin <- ifelse(baseline_dealers$maize.owner.agree.skill.q105=="b",1,0)
baseline_dealers$mid_maize.owner.agree.skill.q105 <- baseline_dealers$maize.owner.agree.skill.q105
baseline_dealers$mid_maize.owner.agree.skill.q105_bin <- ifelse(baseline_dealers$mid_maize.owner.agree.skill.q105=="b",1,0)

baseline_dealers$maize.owner.agree.skill.q106_bin <- ifelse(baseline_dealers$maize.owner.agree.skill.q106=="c",1,0)
baseline_dealers$mid_maize.owner.agree.skill.q106 <- baseline_dealers$maize.owner.agree.skill.q106
baseline_dealers$mid_maize.owner.agree.skill.q106_bin <- ifelse(baseline_dealers$mid_maize.owner.agree.skill.q106=="c",1,0)

baseline_dealers$maize.owner.agree.skill.q107_bin <- ifelse(baseline_dealers$maize.owner.agree.skill.q107=="b",1,0)
baseline_dealers$mid_maize.owner.agree.skill.q107 <- baseline_dealers$maize.owner.agree.skill.q107
baseline_dealers$mid_maize.owner.agree.skill.q107_bin <- ifelse(baseline_dealers$mid_maize.owner.agree.skill.q107=="b",1,0)

baseline_dealers$maize.owner.agree.skill.q108_bin <- ifelse(baseline_dealers$maize.owner.agree.skill.q108=="c",1,0)
baseline_dealers$mid_maize.owner.agree.skill.q108 <- baseline_dealers$maize.owner.agree.skill.q108
baseline_dealers$mid_maize.owner.agree.skill.q108_bin <- ifelse(baseline_dealers$mid_maize.owner.agree.skill.q108=="c",1,0)

###3. Define groupings/areas/domains of outcomes: each outcome is assigned to one of these areas
variables_knowl_store_mid <- cbind(baseline_dealers$mid_maize.owner.agree.skill.q104_bin,baseline_dealers$mid_maize.owner.agree.skill.q105_bin
                                   ,baseline_dealers$mid_maize.owner.agree.skill.q106_bin,baseline_dealers$mid_maize.owner.agree.skill.q107_bin
                                   ,baseline_dealers$mid_maize.owner.agree.skill.q108_bin)
variables_knowl_store_base <- cbind(baseline_dealers$maize.owner.agree.skill.q104_bin,baseline_dealers$maize.owner.agree.skill.q105_bin
                                    ,baseline_dealers$maize.owner.agree.skill.q106_bin,baseline_dealers$maize.owner.agree.skill.q107_bin
                                    ,baseline_dealers$maize.owner.agree.skill.q108_bin)

#7. Index of dealer's knowledge about seed
baseline_dealers$maize.owner.agree.skill.q109_bin <- ifelse(baseline_dealers$maize.owner.agree.skill.q109=="a",1,0)
baseline_dealers$mid_maize.owner.agree.skill.q109 <- baseline_dealers$maize.owner.agree.skill.q109
baseline_dealers$mid_maize.owner.agree.skill.q109_bin <- ifelse(baseline_dealers$mid_maize.owner.agree.skill.q109=="a",1,0)

baseline_dealers$maize.owner.agree.skill.q111_bin <- ifelse(baseline_dealers$maize.owner.agree.skill.q111=="a",1,0)
baseline_dealers$mid_maize.owner.agree.skill.q111 <- baseline_dealers$maize.owner.agree.skill.q111
baseline_dealers$mid_maize.owner.agree.skill.q111_bin <- ifelse(baseline_dealers$mid_maize.owner.agree.skill.q111=="a",1,0)

baseline_dealers$maize.owner.agree.skill.q112_bin <- ifelse(baseline_dealers$maize.owner.agree.skill.q112=="c",1,0)
baseline_dealers$mid_maize.owner.agree.skill.q112 <- baseline_dealers$maize.owner.agree.skill.q112
baseline_dealers$mid_maize.owner.agree.skill.q112_bin <- ifelse(baseline_dealers$mid_maize.owner.agree.skill.q112=="c",1,0)

baseline_dealers$maize.owner.agree.skill.q113_bin <- ifelse(baseline_dealers$maize.owner.agree.skill.q113=="c",1,0)
baseline_dealers$mid_maize.owner.agree.skill.q113 <- baseline_dealers$maize.owner.agree.skill.q113
baseline_dealers$mid_maize.owner.agree.skill.q113_bin <- ifelse(baseline_dealers$mid_maize.owner.agree.skill.q113=="c",1,0)

###3. Define groupings/areas/domains of outcomes: each outcome is assigned to one of these areas
variables_knowl_seed_mid <- cbind(baseline_dealers$mid_maize.owner.agree.skill.q109_bin,baseline_dealers$mid_maize.owner.agree.skill.q111_bin
                                  ,baseline_dealers$mid_maize.owner.agree.skill.q112_bin,baseline_dealers$mid_maize.owner.agree.skill.q113_bin)
variables_knowl_seed_base <- cbind(baseline_dealers$maize.owner.agree.skill.q109_bin,baseline_dealers$maize.owner.agree.skill.q111_bin
                                   ,baseline_dealers$maize.owner.agree.skill.q112_bin,baseline_dealers$maize.owner.agree.skill.q113_bin)

#8. Q121. Do you have equipment to monitor moisture in the seed?
baseline_dealers$mid_maize.owner.agree.inspection.q121<-baseline_dealers$maize.owner.agree.inspection.q121
#baseline_dealers$mid_maize.owner.agree.inspection.q121<-ifelse(baseline_dealers$mid_maize.owner.agree.inspection.q121=="Yes",1,0)

#9. Q122. Do you monitor temperature in your seed store?
baseline_dealers$mid_maize.owner.agree.inspection.q122<-baseline_dealers$maize.owner.agree.inspection.q122
#baseline_dealers$mid_maize.owner.agree.inspection.q122<-ifelse(baseline_dealers$mid_maize.owner.agree.inspection.q122=="Yes",1,0)

#10. Q70. Entert the temperature in the seed store (where seed is stored)
baseline_dealers <- trim("maize.owner.agree.q70",baseline_dealers,trim_perc=.01)
baseline_dealers$mid_maize.owner.agree.q70 <- (baseline_dealers$maize.owner.agree.q70-2.522537*baseline_dealers$training-2.522537*baseline_dealers$clearing-2.522537*baseline_dealers$farmer)
baseline_dealers <- trim("mid_maize.owner.agree.q70",baseline_dealers,trim_perc=.01)
baseline_dealers$mid_maize.owner.agree.q70_pos <- baseline_dealers$mid_maize.owner.agree.q70*-1
baseline_dealers$maize.owner.agree.q70_pos <- baseline_dealers$maize.owner.agree.q70*-1


#10. Overall index of secondary agro-input dealer outcome variables
###3. Define groupings/areas/domains of outcomes: each outcome is assigned to one of these areas
variables_overallsec_mid <- cbind(baseline_dealers$mid_maize.owner.agree.nr_var
                                  ,baseline_dealers$mid_maize.owner.agree.q19,baseline_dealers$mid_maize.owner.agree.q44
                                  ,baseline_dealers$index_selfratings_mid,baseline_dealers$index_servicesFARM_mid
                                  ,baseline_dealers$index_knowl_store_mid,baseline_dealers$index_knowl_seed_mid
                                  ,baseline_dealers$mid_maize.owner.agree.inspection.q122,baseline_dealers$mid_maize.owner.agree.q70_pos)
variables_overallsec_base <- cbind(baseline_dealers$maize.owner.agree.nr_var,baseline_dealers$maize.owner.agree.q19
                                   ,baseline_dealers$maize.owner.agree.q44,baseline_dealers$index_selfratings_base
                                   ,baseline_dealers$index_servicesFARM_base,baseline_dealers$index_knowl_store_base
                                   ,baseline_dealers$index_knowl_seed_base
                                   ,baseline_dealers$maize.owner.agree.inspection.q122,baseline_dealers$maize.owner.agree.q70_pos)

#dont forget to transform like bl
#dont forget to trim
#dont forget to simulate midline
#dont forget to trim

################################################################################################################################################################################
###4. Create index: weighted average of outcomes for individual i in area j

###
#1#
###

#4.
index_selfratings_mid <- icwIndex(xmat=variables_selfratings_mid)
baseline_dealers$index_selfratings_mid <- index_selfratings_mid$index #midline index

index_selfratings_base <- icwIndex(xmat=variables_selfratings_base)
baseline_dealers$index_selfratings_base <- index_selfratings_base$index #baseline index

#5.
index_servicesFARM_mid <- icwIndex(xmat=variables_servicesFARM_mid)
baseline_dealers$index_servicesFARM_mid <- index_servicesFARM_mid$index #midline index

index_servicesFARM_base <- icwIndex(xmat=variables_servicesFARM_base)
baseline_dealers$index_servicesFARM_base <- index_servicesFARM_base$index #baseline index

#6.
index_knowl_store_mid <- icwIndex(xmat=variables_knowl_store_mid)
baseline_dealers$index_knowl_store_mid <- index_knowl_store_mid$index #midline index

index_knowl_store_base <- icwIndex(xmat=variables_knowl_store_base)
baseline_dealers$index_knowl_store_base <- index_knowl_store_base$index #baseline index

#7.
index_knowl_seed_mid <- icwIndex(xmat=variables_knowl_seed_mid)
baseline_dealers$index_knowl_seed_mid <- index_knowl_seed_mid$index #midline index

index_knowl_seed_base <- icwIndex(xmat=variables_knowl_seed_base)
baseline_dealers$index_knowl_seed_base <- index_knowl_seed_base$index #baseline index

#8.
index_overallsec_mid <- icwIndex(xmat=variables_overallsec_mid)
baseline_dealers$index_overallsec_mid <- index_overallsec_mid$index #midline index

index_overallsec_base <- icwIndex(xmat=variables_overallsec_base)
baseline_dealers$index_overallsec_base <- index_overallsec_base$index #baseline index

results_dealer_sec <- c("mid_maize.owner.agree.nr_var","mid_maize.owner.agree.q19","mid_maize.owner.agree.q44","index_selfratings_mid","index_servicesFARM_mid"
                        ,"index_knowl_store_mid","index_knowl_seed_mid","mid_maize.owner.agree.inspection.q121","mid_maize.owner.agree.inspection.q122"
                        ,"mid_maize.owner.agree.q70","index_overallsec_mid")

results_dealer_sec_base <- c("maize.owner.agree.nr_var","maize.owner.agree.q19","maize.owner.agree.q44","index_selfratings_base","index_servicesFARM_base"
                             ,"index_knowl_store_base","index_knowl_seed_base","maize.owner.agree.inspection.q121","maize.owner.agree.inspection.q122"
                             ,"maize.owner.agree.q70","index_overallsec_base")

df_means_D_sec <- array(NA,dim=c(3,11))

for (i in 1:length(results_dealer_sec)){
  df_means_D_sec[1,i] <- sum(baseline_dealers[results_dealer_sec[i]], na.rm=T)/(nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_sec[i]])))
  df_means_D_sec[2,i] <- sqrt(var(baseline_dealers[results_dealer_sec[i]], na.rm=T))
  df_means_D_sec[3,i] <- nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_sec[i]]))-sum(is.na(baseline_dealers[results_dealer_sec_base[i]]))+sum(is.na(baseline_dealers[results_dealer_sec[i]])&is.na(baseline_dealers[results_dealer_sec_base[i]]))}

###
#2#
###

df_ols_D_sec <- array(NA,dim=c(3,3,11))

baseline_dealers$training_control[baseline_dealers$training==0] <- TRUE
baseline_dealers$training_control[baseline_dealers$training==1] <- FALSE

#4.
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

#8.
index_overallsec_mid <- icwIndex(xmat=variables_overallsec_mid,sgroup = baseline_dealers$training_control)
baseline_dealers$index_overallsec_midT <- index_overallsec_mid$index

index_overallsec_base <- icwIndex(xmat=variables_overallsec_base,sgroup = baseline_dealers$training_control)
baseline_dealers$index_overallsec_baseT <- index_overallsec_base$index

results_dealer_sec <- c("mid_maize.owner.agree.nr_var","mid_maize.owner.agree.q19","mid_maize.owner.agree.q44","index_selfratings_midT","index_servicesFARM_midT"
                        ,"index_knowl_store_midT","index_knowl_seed_midT","mid_maize.owner.agree.inspection.q121","mid_maize.owner.agree.inspection.q122"
                        ,"mid_maize.owner.agree.q70","index_overallsec_midT")

results_dealer_sec_base <- c("maize.owner.agree.nr_var","maize.owner.agree.q19","maize.owner.agree.q44","index_selfratings_baseT","index_servicesFARM_baseT"
                             ,"index_knowl_store_baseT","index_knowl_seed_baseT","maize.owner.agree.inspection.q121","maize.owner.agree.inspection.q122"
                             ,"maize.owner.agree.q70","index_overallsec_baseT")

for (i in 1:length(results_dealer_sec)){
  ols <- lm(as.formula(paste(paste(results_dealer_sec[i],"training*clearing*farmer",sep="~"),results_dealer_sec_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_sec[i],"training*clearing*farmer",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")

  df_ols_D_sec[1,1,i] <- coef_test(ols, vcov_cluster)[2,1]
  df_ols_D_sec[2,1,i] <- coef_test(ols, vcov_cluster)[2,2]
  df_ols_D_sec[3,1,i] <- coef_test(ols, vcov_cluster)[2,5]}

###
#3#
###

baseline_dealers$clearing_control[baseline_dealers$clearing==0] <- TRUE
baseline_dealers$clearing_control[baseline_dealers$clearing==1] <- FALSE

#4.
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

#8.
index_overallsec_mid <- icwIndex(xmat=variables_overallsec_mid,sgroup = baseline_dealers$clearing_control)
baseline_dealers$index_overallsec_midC <- index_overallsec_mid$index

index_overallsec_base <- icwIndex(xmat=variables_overallsec_base,sgroup = baseline_dealers$clearing_control)
baseline_dealers$index_overallsec_baseC <- index_overallsec_base$index

results_dealer_sec <- c("mid_maize.owner.agree.nr_var","mid_maize.owner.agree.q19","mid_maize.owner.agree.q44","index_selfratings_midC","index_servicesFARM_midC"
                        ,"index_knowl_store_midC","index_knowl_seed_midC","mid_maize.owner.agree.inspection.q121","mid_maize.owner.agree.inspection.q122"
                        ,"mid_maize.owner.agree.q70","index_overallsec_midC")

results_dealer_sec_base <- c("maize.owner.agree.nr_var","maize.owner.agree.q19","maize.owner.agree.q44","index_selfratings_baseC","index_servicesFARM_baseC"
                             ,"index_knowl_store_baseC","index_knowl_seed_baseC","maize.owner.agree.inspection.q121","maize.owner.agree.inspection.q122"
                             ,"maize.owner.agree.q70","index_overallsec_baseC")

for (i in 1:length(results_dealer_sec)){
  ols <- lm(as.formula(paste(paste(results_dealer_sec[i],"training*clearing*farmer",sep="~"),results_dealer_sec_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_sec[i],"training*clearing*farmer",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")

  df_ols_D_sec[1,2,i] <- coef_test(ols, vcov_cluster)[3,1]
  df_ols_D_sec[2,2,i] <- coef_test(ols, vcov_cluster)[3,2]
  df_ols_D_sec[3,2,i] <- coef_test(ols, vcov_cluster)[3,5]}

###
#4#
###

baseline_dealers$farmer_control[baseline_dealers$farmer==0] <- TRUE
baseline_dealers$farmer_control[baseline_dealers$farmer==1] <- FALSE

#4.
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

#8.
index_overallsec_mid <- icwIndex(xmat=variables_overallsec_mid,sgroup = baseline_dealers$farmer_control)
baseline_dealers$index_overallsec_midF <- index_overallsec_mid$index

index_overallsec_base <- icwIndex(xmat=variables_overallsec_base,sgroup = baseline_dealers$farmer_control)
baseline_dealers$index_overallsec_baseF <- index_overallsec_base$index

results_dealer_sec <- c("mid_maize.owner.agree.nr_var","mid_maize.owner.agree.q19","mid_maize.owner.agree.q44","index_selfratings_midF","index_servicesFARM_midF"
                        ,"index_knowl_store_midF","index_knowl_seed_midF","mid_maize.owner.agree.inspection.q121","mid_maize.owner.agree.inspection.q122"
                        ,"mid_maize.owner.agree.q70","index_overallsec_midF")

results_dealer_sec_base <- c("maize.owner.agree.nr_var","maize.owner.agree.q19","maize.owner.agree.q44","index_selfratings_baseF","index_servicesFARM_baseF"
                             ,"index_knowl_store_baseF","index_knowl_seed_baseF","maize.owner.agree.inspection.q121","maize.owner.agree.inspection.q122"
                             ,"maize.owner.agree.q70","index_overallsec_baseF")

for (i in 1:length(results_dealer_sec)){
  ols <- lm(as.formula(paste(paste(results_dealer_sec[i],"training*clearing*farmer",sep="~"),results_dealer_sec_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_sec[i],"training*clearing*farmer",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")

  #farmer video treatment at village/shop level so no clustering needed
  df_ols_D_sec[1,3,i] <- summary(ols)$coefficients[4,1]
  df_ols_D_sec[2,3,i] <- summary(ols)$coefficients[4,2]
  df_ols_D_sec[3,3,i] <- summary(ols)$coefficients[4,4]}










###################################################################################################################################################################
##### 4 ANALYSIS: Agro-input dealer - Secondary: 9. Longe 10H######################################################################################################
###################################################################################################################################################################

baseline_dealers_save=baseline_dealers
baseline_dealers=subset(baseline_dealers,maize.owner.agree.q20=="1")

#1. Did you have Longe 10H in stock in the second season of 2020. (q20)

#2. How much of Longe 10H was carried forward from the previous season (first season 2020) into the second season of 2020 (kg) (q21)
baseline_dealers$maize.owner.agree.long10h.q21[baseline_dealers$maize.owner.agree.long10h.q21=="n/a"] <- NA
baseline_dealers$maize.owner.agree.long10h.q21 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.long10h.q21))
baseline_dealers$maize.owner.agree.long10h.q21[baseline_dealers$maize.owner.agree.long10h.q21==999] <- NA
baseline_dealers$maize.owner.agree.long10h.q21[baseline_dealers$maize.owner.agree.q20=="0"] <- 0
baseline_dealers <- trim("maize.owner.agree.long10h.q21",baseline_dealers,trim_perc=.01)

baseline_dealers$mid_maize.owner.agree.long10h.q21 <- (baseline_dealers$maize.owner.agree.long10h.q21+0.2156522*baseline_dealers$training+0.2156522*baseline_dealers$clearing+0.2156522*baseline_dealers$farmer)
baseline_dealers$mid_maize.owner.agree.long10h.q21[baseline_dealers$mid_maize.owner.agree.long10h.q21=="n/a"] <- NA
baseline_dealers$mid_maize.owner.agree.long10h.q21 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.long10h.q21))
baseline_dealers$mid_maize.owner.agree.long10h.q21[baseline_dealers$mid_maize.owner.agree.long10h.q21==999] <- NA
baseline_dealers$mid_maize.owner.agree.long10h.q21[baseline_dealers$mid_maize.owner.agree.q20=="0"] <- 0
baseline_dealers <- trim("mid_maize.owner.agree.long10h.q21",baseline_dealers,trim_perc=.01)

#3. How much of Longe 10H was bought by you from any provider during the second season of 2020 (in kg) (q22)
baseline_dealers$maize.owner.agree.long10h.q22[baseline_dealers$maize.owner.agree.long10h.q22=="n/a"] <- NA
baseline_dealers$maize.owner.agree.long10h.q22[baseline_dealers$maize.owner.agree.long10h.q22==999] <- NA
baseline_dealers$maize.owner.agree.long10h.q22 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.long10h.q22))
baseline_dealers$maize.owner.agree.long10h.q22[baseline_dealers$maize.owner.agree.q20=="0"] <- 0
baseline_dealers <- trim("maize.owner.agree.long10h.q22",baseline_dealers,trim_perc=.01)

baseline_dealers$mid_maize.owner.agree.long10h.q22 <- (baseline_dealers$maize.owner.agree.long10h.q22+24.58912*baseline_dealers$training+24.58912*baseline_dealers$clearing+24.58912*baseline_dealers$farmer)
baseline_dealers$mid_maize.owner.agree.long10h.q22[baseline_dealers$mid_maize.owner.agree.long10h.q22=="n/a"] <- NA
baseline_dealers$mid_maize.owner.agree.long10h.q22[baseline_dealers$mid_maize.owner.agree.long10h.q22==999] <- NA
baseline_dealers$mid_maize.owner.agree.long10h.q22 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.long10h.q22))
baseline_dealers$mid_maize.owner.agree.long10h.q22[baseline_dealers$mid_maize.owner.agree.q20=="0"] <- 0
baseline_dealers <- trim("mid_maize.owner.agree.long10h.q22",baseline_dealers,trim_perc=.01)

#4. What was the cost of Longe 10H per Kg from where you obtained it during the second season of 2020? (q24)
baseline_dealers$maize.owner.agree.long10h.q24[baseline_dealers$maize.owner.agree.long10h.q24=="n/a"] <- NA
baseline_dealers$maize.owner.agree.long10h.q24[baseline_dealers$maize.owner.agree.long10h.q24==999] <- NA
baseline_dealers$maize.owner.agree.long10h.q24 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.long10h.q24))
baseline_dealers <- trim("maize.owner.agree.long10h.q24",baseline_dealers,trim_perc=.01)

baseline_dealers$mid_maize.owner.agree.long10h.q24 <- (baseline_dealers$maize.owner.agree.long10h.q24+508.7991*baseline_dealers$training+508.7991*baseline_dealers$clearing+508.7991*baseline_dealers$farmer)
baseline_dealers$mid_maize.owner.agree.long10h.q24[baseline_dealers$mid_maize.owner.agree.long10h.q24=="n/a"] <- NA
baseline_dealers$mid_maize.owner.agree.long10h.q24[baseline_dealers$mid_maize.owner.agree.long10h.q24==999] <- NA
baseline_dealers$mid_maize.owner.agree.long10h.q24 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.long10h.q24))
baseline_dealers <- trim("mid_maize.owner.agree.long10h.q24",baseline_dealers,trim_perc=.01)

#5. Q25. Total quantity sold of ${carry} (Kg) over the second season of 2020

#6. Sales price per kilogram of ${q25} at the beginning of the second season of 2020 (q26)

#7. (h) How much of Longe10H was lost/wasted the second season of 2020 (kg) (q27)
baseline_dealers$maize.owner.agree.long10h.q27[baseline_dealers$maize.owner.agree.long10h.q27=="n/a"] <- NA
baseline_dealers$maize.owner.agree.long10h.q27 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.long10h.q27))
baseline_dealers$maize.owner.agree.long10h.q27[baseline_dealers$maize.owner.agree.q20=="0"] <- 0
baseline_dealers <- trim("maize.owner.agree.long10h.q27",baseline_dealers,trim_perc=.01)

baseline_dealers$mid_maize.owner.agree.long10h.q27 <- (baseline_dealers$maize.owner.agree.long10h.q27-0.01878453*baseline_dealers$training-0.01878453*baseline_dealers$clearing-0.01878453*baseline_dealers$farmer)
baseline_dealers$mid_maize.owner.agree.long10h.q27[baseline_dealers$mid_maize.owner.agree.long10h.q27=="n/a"] <- NA
baseline_dealers$mid_maize.owner.agree.long10h.q27 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.long10h.q27))
baseline_dealers$mid_maize.owner.agree.long10h.q27[baseline_dealers$mid_maize.owner.agree.q20=="0"] <- 0
baseline_dealers <- trim("mid_maize.owner.agree.long10h.q27",baseline_dealers,trim_perc=.01)

#8. Did you ever run out of Longe10H during the second season of 2020? (q29)
baseline_dealers$maize.owner.agree.long10h.q29[baseline_dealers$maize.owner.agree.long10h.q29=="n/a"] <- NA
baseline_dealers$maize.owner.agree.long10h.q29 <- ifelse(baseline_dealers$maize.owner.agree.long10h.q29=="Yes",1,0)

baseline_dealers$mid_maize.owner.agree.long10h.q29 <- baseline_dealers$maize.owner.agree.long10h.q29
baseline_dealers$mid_maize.owner.agree.long10h.q29[baseline_dealers$mid_maize.owner.agree.long10h.q29=="n/a"] <- NA
#baseline_dealers$mid_maize.owner.agree.long10h.q29 <- ifelse(baseline_dealers$mid_maize.owner.agree.long10h.q29=="Yes",1,0)

#9. Estimate how often you ran out of stock for Longe10 H during the second season of 2020 (q30)
baseline_dealers$maize.owner.agree.long10h.q30[baseline_dealers$maize.owner.agree.long10h.q30=="n/a"] <- NA
baseline_dealers$maize.owner.agree.long10h.q30 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.long10h.q30))
baseline_dealers$maize.owner.agree.long10h.q30[baseline_dealers$maize.owner.agree.long10h.q29=="0"] <- 0
baseline_dealers <- trim("maize.owner.agree.long10h.q30",baseline_dealers,trim_perc=.01)

baseline_dealers$mid_maize.owner.agree.long10h.q30 <- (baseline_dealers$maize.owner.agree.long10h.q30+0.3287582*baseline_dealers$training+0.3287582*baseline_dealers$clearing+0.3287582*baseline_dealers$farmer)
baseline_dealers$mid_maize.owner.agree.long10h.q30[baseline_dealers$mid_maize.owner.agree.long10h.q30=="n/a"] <- NA
baseline_dealers$mid_maize.owner.agree.long10h.q30[baseline_dealers$mid_maize.owner.agree.long10h.q29=="0"] <- 0
baseline_dealers$mid_maize.owner.agree.long10h.q30 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.long10h.q30))
baseline_dealers$mid_maize.owner.agree.long10h.q30[baseline_dealers$mid_maize.owner.agree.long10h.q29=="0"] <- 0
baseline_dealers <- trim("mid_maize.owner.agree.long10h.q30",baseline_dealers,trim_perc=.01)

#10. How long (days) did it on average take to get restocked for Longe10H during the second season of 2020: (days) (q31)
baseline_dealers$maize.owner.agree.long10h.q31[baseline_dealers$maize.owner.agree.long10h.q31=="n/a"] <- NA
baseline_dealers$maize.owner.agree.long10h.q31[baseline_dealers$maize.owner.agree.long10h.q31==999] <- NA
baseline_dealers$maize.owner.agree.long10h.q31 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.long10h.q31))
baseline_dealers <- trim("maize.owner.agree.long10h.q31",baseline_dealers,trim_perc=.01)

baseline_dealers$mid_maize.owner.agree.long10h.q31 <- (baseline_dealers$maize.owner.agree.long10h.q31+1.331933*baseline_dealers$training+1.331933*baseline_dealers$clearing+1.331933*baseline_dealers$farmer)
baseline_dealers$mid_maize.owner.agree.long10h.q31[baseline_dealers$mid_maize.owner.agree.long10h.q31=="n/a"] <- NA
baseline_dealers$mid_maize.owner.agree.long10h.q31[baseline_dealers$mid_maize.owner.agree.long10h.q31==999] <- NA
baseline_dealers$mid_maize.owner.agree.long10h.q31 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.long10h.q31))
baseline_dealers <- trim("mid_maize.owner.agree.long10h.q31",baseline_dealers,trim_perc=.01)

#11. Overall index of secondary Longe10H agro-input dealer outcome variables
###1. For all outcomes, switch signs where necessary so that the positive direction always indicates a "better" outcome.

###3. Define groupings/areas/domains of outcomes: each outcome is assigned to one of these areas
variables_overall_Longe10H_mid <- cbind(baseline_dealers$mid_maize.owner.agree.long10h.q22
                                               ,baseline_dealers$mid_maize.owner.agree.long10h.q25)
variables_overall_Longe10H_base <- cbind(baseline_dealers$maize.owner.agree.long10h.q22
                                                ,baseline_dealers$maize.owner.agree.long10h.q25)





#dont forget to transform like bl
#dont forget to trim
#dont forget to simulate midline
#dont forget to trim

################################################################################################################################################################################
###4. Create index: weighted average of outcomes for individual i in area j

###
#1#
###

#4.
index_overall_Longe10H_mid <- icwIndex(xmat=variables_overall_Longe10H_mid)
baseline_dealers$index_overall_Longe10H_mid <- index_overall_Longe10H_mid$index #midline index

index_overall_Longe10H_base <- icwIndex(xmat=variables_overall_Longe10H_base)
baseline_dealers$index_overall_Longe10H_base <- index_overall_Longe10H_base$index #baseline index

results_dealer_secL10H <- c("mid_maize.owner.agree.long10h.q21","mid_maize.owner.agree.long10h.q22","mid_maize.owner.agree.long10h.q24"
                            ,"mid_maize.owner.agree.long10h.q25","mid_maize.owner.agree.long10h.q26","mid_maize.owner.agree.long10h.q27"
                            ,"mid_maize.owner.agree.long10h.q30","index_overall_Longe10H_mid")

results_dealer_secL10H_base <- c("maize.owner.agree.long10h.q21","maize.owner.agree.long10h.q22","maize.owner.agree.long10h.q24"
                                 ,"maize.owner.agree.long10h.q25","maize.owner.agree.long10h.q26","maize.owner.agree.long10h.q27"
                                 ,"maize.owner.agree.long10h.q30","index_overall_Longe10H_base")

df_means_D_secL10H <- array(NA,dim=c(3,11))

for (i in 1:length(results_dealer_secL10H)){
  df_means_D_secL10H[1,i] <- sum(baseline_dealers[results_dealer_secL10H[i]], na.rm=T)/(nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_secL10H[i]])))
  df_means_D_secL10H[2,i] <- sqrt(var(baseline_dealers[results_dealer_secL10H[i]], na.rm=T))
  df_means_D_secL10H[3,i] <- nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_secL10H[i]]))-sum(is.na(baseline_dealers[results_dealer_secL10H_base[i]]))+sum(is.na(baseline_dealers[results_dealer_secL10H[i]])&is.na(baseline_dealers[results_dealer_secL10H_base[i]]))}

###
#2#
###

baseline_dealers$training_control[baseline_dealers$training==0] <- TRUE
baseline_dealers$training_control[baseline_dealers$training==1] <- FALSE

#4.
index_overall_Longe10H_mid <- icwIndex(xmat=variables_overall_Longe10H_mid,sgroup = baseline_dealers$training_control)
baseline_dealers$index_overall_Longe10H_midT <- index_overall_Longe10H_mid$index

index_overall_Longe10H_base <- icwIndex(xmat=variables_overall_Longe10H_base,sgroup = baseline_dealers$training_control)
baseline_dealers$index_overall_Longe10H_baseT <- index_overall_Longe10H_base$index

df_ols_D_secL10H <- array(NA,dim=c(3,3,11))

results_dealer_secL10H <- c("mid_maize.owner.agree.long10h.q21","mid_maize.owner.agree.long10h.q22","mid_maize.owner.agree.long10h.q24"
                            ,"mid_maize.owner.agree.long10h.q25","mid_maize.owner.agree.long10h.q26","mid_maize.owner.agree.long10h.q27"
                            ,"mid_maize.owner.agree.long10h.q30","index_overall_Longe10H_midT")

results_dealer_secL10H_base <- c("maize.owner.agree.long10h.q21","maize.owner.agree.long10h.q22","maize.owner.agree.long10h.q24"
                                 ,"maize.owner.agree.long10h.q25","maize.owner.agree.long10h.q26","maize.owner.agree.long10h.q27"
                                 ,"maize.owner.agree.long10h.q30","index_overall_Longe10H_baseT")

for (i in 1:length(results_dealer_secL10H)){
  ols <- lm(as.formula(paste(paste(results_dealer_secL10H[i],"training*clearing*farmer",sep="~"),results_dealer_secL10H_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_secL10H[i],"training*clearing*farmer",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  df_ols_D_secL10H[1,1,i] <- coef_test(ols, vcov_cluster)[2,1]
  df_ols_D_secL10H[2,1,i] <- coef_test(ols, vcov_cluster)[2,2]
  df_ols_D_secL10H[3,1,i] <- coef_test(ols, vcov_cluster)[2,5]}

###
#3#
###

baseline_dealers$clearing_control[baseline_dealers$clearing==0] <- TRUE
baseline_dealers$clearing_control[baseline_dealers$clearing==1] <- FALSE

#4.
index_overall_Longe10H_mid <- icwIndex(xmat=variables_overall_Longe10H_mid,sgroup = baseline_dealers$clearing_control)
baseline_dealers$index_overall_Longe10H_midC <- index_overall_Longe10H_mid$index

index_overall_Longe10H_base <- icwIndex(xmat=variables_overall_Longe10H_base,sgroup = baseline_dealers$clearing_control)
baseline_dealers$index_overall_Longe10H_baseC <- index_overall_Longe10H_base$index

results_dealer_secL10H <- c("mid_maize.owner.agree.long10h.q21","mid_maize.owner.agree.long10h.q22","mid_maize.owner.agree.long10h.q24"
                            ,"mid_maize.owner.agree.long10h.q25","mid_maize.owner.agree.long10h.q26","mid_maize.owner.agree.long10h.q27"
                            ,"mid_maize.owner.agree.long10h.q30","index_overall_Longe10H_midC")

results_dealer_secL10H_base <- c("maize.owner.agree.long10h.q21","maize.owner.agree.long10h.q22","maize.owner.agree.long10h.q24"
                                 ,"maize.owner.agree.long10h.q25","maize.owner.agree.long10h.q26","maize.owner.agree.long10h.q27"
                                 ,"maize.owner.agree.long10h.q30","index_overall_Longe10H_baseC")

for (i in 1:length(results_dealer_secL10H)){
  ols <- lm(as.formula(paste(paste(results_dealer_secL10H[i],"training*clearing*farmer",sep="~"),results_dealer_secL10H_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_secL10H[i],"training*clearing*farmer",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  df_ols_D_secL10H[1,2,i] <- coef_test(ols, vcov_cluster)[3,1]
  df_ols_D_secL10H[2,2,i] <- coef_test(ols, vcov_cluster)[3,2]
  df_ols_D_secL10H[3,2,i] <- coef_test(ols, vcov_cluster)[3,5]}

###
#4#
###

baseline_dealers$farmer_control[baseline_dealers$farmer==0] <- TRUE
baseline_dealers$farmer_control[baseline_dealers$farmer==1] <- FALSE

#4.
index_overall_Longe10H_mid <- icwIndex(xmat=variables_overall_Longe10H_mid,sgroup = baseline_dealers$farmer_control)
baseline_dealers$index_overall_Longe10H_midF <- index_overall_Longe10H_mid$index

index_overall_Longe10H_base <- icwIndex(xmat=variables_overall_Longe10H_base,sgroup = baseline_dealers$farmer_control)
baseline_dealers$index_overall_Longe10H_baseF <- index_overall_Longe10H_base$index

results_dealer_secL10H <- c("mid_maize.owner.agree.long10h.q21","mid_maize.owner.agree.long10h.q22","mid_maize.owner.agree.long10h.q24"
                            ,"mid_maize.owner.agree.long10h.q25","mid_maize.owner.agree.long10h.q26","mid_maize.owner.agree.long10h.q27"
                            ,"mid_maize.owner.agree.long10h.q30","index_overall_Longe10H_midF")

results_dealer_secL10H_base <- c("maize.owner.agree.long10h.q21","maize.owner.agree.long10h.q22","maize.owner.agree.long10h.q24"
                                 ,"maize.owner.agree.long10h.q25","maize.owner.agree.long10h.q26","maize.owner.agree.long10h.q27"
                                 ,"maize.owner.agree.long10h.q30","index_overall_Longe10H_baseF")

for (i in 1:length(results_dealer_secL10H)){
  ols <- lm(as.formula(paste(paste(results_dealer_secL10H[i],"training*clearing*farmer",sep="~"),results_dealer_secL10H_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_secL10H[i],"training*clearing*farmer",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  #farmer video treatment at village/shop level so no clustering needed
  df_ols_D_secL10H[1,3,i] <- summary(ols)$coefficients[4,1]
  df_ols_D_secL10H[2,3,i] <- summary(ols)$coefficients[4,2]
  df_ols_D_secL10H[3,3,i] <- summary(ols)$coefficients[4,4]}

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

df_ols_D_secL10H_J <- array(NA,dim=c(3,3,10))

results_dealer_secL10H_J <- c("mid_maize.owner.agree.long10h.q21","mid_maize.owner.agree.long10h.q22","mid_maize.owner.agree.long10h.q24"
                            ,"mid_maize.owner.agree.long10h.q25","mid_maize.owner.agree.long10h.q26","mid_maize.owner.agree.long10h.q27"
                            ,"mid_maize.owner.agree.long10h.q30")
#no overall index

for (i in 1:length(results_dealer_secL10H_J)){
  df_ols_D_secL10H_J[3,1,i] <- adjust_p(df_ols_D_secL10H[3,1,i],df_dealer_secL10HT,i)
  df_ols_D_secL10H_J[3,2,i] <- adjust_p(df_ols_D_secL10H[3,2,i],df_dealer_secL10HC,i)
  df_ols_D_secL10H_J[3,3,i] <- adjust_p(df_ols_D_secL10H[3,3,i],df_dealer_secL10HF,i)}

baseline_dealers=baseline_dealers_save










###################################################################################################################################################################
##### 5 ANALYSIS: Agro-input dealer - Secondary: Longe 5###########################################################################################################
###################################################################################################################################################################

baseline_dealers_save=baseline_dealers
baseline_dealers=subset(baseline_dealers,maize.owner.agree.q45=="1")

#1. Q45. Did you have Longe 5 in 2020 In stock in the second season 2020?

#2. Q46. How much of Longe 5 was carried forward from the previous season (first season 2020) into the second season of 2020 (kg)
baseline_dealers$maize.owner.agree.longe5.q46[baseline_dealers$maize.owner.agree.longe5.q46=="n/a"] <- NA
baseline_dealers$maize.owner.agree.longe5.q46 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.longe5.q46))
baseline_dealers$maize.owner.agree.longe5.q46[baseline_dealers$maize.owner.agree.longe5.q46==999] <- NA
baseline_dealers$maize.owner.agree.longe5.q46[baseline_dealers$maize.owner.agree.q45=="0"] <- 0
baseline_dealers <- trim("maize.owner.agree.longe5.q46",baseline_dealers,trim_perc=.01)

baseline_dealers$mid_maize.owner.agree.longe5.q46 <- (baseline_dealers$maize.owner.agree.longe5.q46+0.5739884*baseline_dealers$training+0.5739884*baseline_dealers$clearing+0.5739884*baseline_dealers$farmer)
baseline_dealers$mid_maize.owner.agree.longe5.q46[baseline_dealers$mid_maize.owner.agree.longe5.q46=="n/a"] <- NA
baseline_dealers$mid_maize.owner.agree.longe5.q46 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.longe5.q46))
baseline_dealers$mid_maize.owner.agree.longe5.q46[baseline_dealers$mid_maize.owner.agree.longe5.q46==999] <- NA
baseline_dealers$mid_maize.owner.agree.longe5.q46[baseline_dealers$midmaize.owner.agree.q45=="0"] <- 0
baseline_dealers <- trim("mid_maize.owner.agree.longe5.q46",baseline_dealers,trim_perc=.01)

#3. Q47. How much of Longe 5 was bought by you from any provider during the second season of 2020 (in kg)
baseline_dealers$maize.owner.agree.longe5.q47[baseline_dealers$maize.owner.agree.longe5.q47=="n/a"] <- NA
baseline_dealers$maize.owner.agree.longe5.q47[baseline_dealers$maize.owner.agree.longe5.q47==999] <- NA
baseline_dealers$maize.owner.agree.longe5.q47 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.longe5.q47))
baseline_dealers$maize.owner.agree.longe5.q47[baseline_dealers$maize.owner.agree.q45=="0"] <- 0
baseline_dealers <- trim("maize.owner.agree.longe5.q47",baseline_dealers,trim_perc=.01)

baseline_dealers$mid_maize.owner.agree.longe5.q47 <- (baseline_dealers$maize.owner.agree.longe5.q47+41.37035*baseline_dealers$training+41.37035*baseline_dealers$clearing+41.37035*baseline_dealers$farmer)
baseline_dealers$mid_maize.owner.agree.longe5.q47[baseline_dealers$mid_maize.owner.agree.longe5.q47=="n/a"] <- NA
baseline_dealers$mid_maize.owner.agree.longe5.q47[baseline_dealers$mid_maize.owner.agree.longe5.q47==999] <- NA
baseline_dealers$mid_maize.owner.agree.longe5.q47 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.longe5.q47))
baseline_dealers$mid_maize.owner.agree.longe5.q47[baseline_dealers$mid_maize.owner.agree.q45=="0"] <- 0
baseline_dealers <- trim("mid_maize.owner.agree.longe5.q47",baseline_dealers,trim_perc=.01)

#4. Q49. What was the cost of Longe 5 from where you obtained it during the second season of 2020? (ugx per kg)
baseline_dealers$maize.owner.agree.longe5.q49[baseline_dealers$maize.owner.agree.longe5.q49=="n/a"] <- NA
baseline_dealers$maize.owner.agree.longe5.q49[baseline_dealers$maize.owner.agree.longe5.q49==999] <- NA
baseline_dealers$maize.owner.agree.longe5.q49 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.longe5.q49))
baseline_dealers <- trim("maize.owner.agree.longe5.q49",baseline_dealers,trim_perc=.01)

baseline_dealers$mid_maize.owner.agree.longe5.q49 <- (baseline_dealers$maize.owner.agree.longe5.q49+249.7138*baseline_dealers$training+249.7138*baseline_dealers$clearing+249.7138*baseline_dealers$farmer)
baseline_dealers$mid_maize.owner.agree.longe5.q49[baseline_dealers$mid_maize.owner.agree.longe5.q49=="n/a"] <- NA
baseline_dealers$mid_maize.owner.agree.longe5.q49[baseline_dealers$mid_maize.owner.agree.longe5.q49==999] <- NA
baseline_dealers$mid_maize.owner.agree.longe5.q49 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.longe5.q49))
baseline_dealers <- trim("mid_maize.owner.agree.longe5.q49",baseline_dealers,trim_perc=.01)

#5. Q50. Total quantity sold of ${carry3} (Kg) over the second season of 2020

#6. Q51. Sales price per kilogram of ${q50}  at the beginning of the second season of 2020

#7. Q52. How much of Longe 5 was lost/wasted the second season of 2020 (kg)
baseline_dealers$maize.owner.agree.longe5.q52[baseline_dealers$maize.owner.agree.longe5.q52=="n/a"] <- NA
baseline_dealers$maize.owner.agree.longe5.q52[baseline_dealers$maize.owner.agree.q45=="0"] <- 0
baseline_dealers <- trim("maize.owner.agree.longe5.q52",baseline_dealers,trim_perc=.01)

baseline_dealers$mid_maize.owner.agree.longe5.q52 <- (baseline_dealers$maize.owner.agree.longe5.q52+0.4370968*baseline_dealers$training+0.4370968*baseline_dealers$clearing+0.4370968*baseline_dealers$farmer)
baseline_dealers$mid_maize.owner.agree.longe5.q52[baseline_dealers$mid_maize.owner.agree.longe5.q52=="n/a"] <- NA
baseline_dealers$mid_maize.owner.agree.longe5.q52[baseline_dealers$mid_maize.owner.agree.longe5.q52==999] <- NA
baseline_dealers$mid_maize.owner.agree.longe5.q52 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.longe5.q52))
baseline_dealers$mid_maize.owner.agree.longe5.q52[baseline_dealers$mid_maize.owner.agree.q45=="0"] <- 0
baseline_dealers <- trim("mid_maize.owner.agree.longe5.q52",baseline_dealers,trim_perc=.01)

#8. Q54. Did you ever run out of this Longe 5 during the second season of 2020?
baseline_dealers$maize.owner.agree.longe5.q54[baseline_dealers$maize.owner.agree.longe5.q54=="n/a"] <- NA
baseline_dealers$maize.owner.agree.longe5.q54 <- ifelse(baseline_dealers$maize.owner.agree.longe5.q54=="Yes",1,0)

baseline_dealers$mid_maize.owner.agree.longe5.q54 <- baseline_dealers$maize.owner.agree.longe5.q54
baseline_dealers$mid_maize.owner.agree.longe5.q54[baseline_dealers$mid_maize.owner.agree.longe5.q54=="n/a"] <- NA
#baseline_dealers$mid_maize.owner.agree.longe5.q54 <- ifelse(baseline_dealers$mid_maize.owner.agree.longe5.q54=="Yes",1,0)

#9. Q55. Estimate how often you ran out of stock for Longe 5 during the second season of 2020
baseline_dealers$maize.owner.agree.longe5.q55[baseline_dealers$maize.owner.agree.longe5.q55=="n/a"] <- NA
baseline_dealers$maize.owner.agree.longe5.q55[baseline_dealers$maize.owner.agree.longe5.q54=="0"] <- 0
baseline_dealers$maize.owner.agree.longe5.q55 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.longe5.q55))
baseline_dealers$maize.owner.agree.longe5.q55[baseline_dealers$maize.owner.agree.longe5.q54=="0"] <- 0
baseline_dealers <- trim("maize.owner.agree.longe5.q55",baseline_dealers,trim_perc=.01)

baseline_dealers$mid_maize.owner.agree.longe5.q55 <- (baseline_dealers$maize.owner.agree.longe5.q55+0.3109589*baseline_dealers$training+0.3109589*baseline_dealers$clearing+0.3109589*baseline_dealers$farmer)
baseline_dealers$mid_maize.owner.agree.longe5.q55[baseline_dealers$mid_maize.owner.agree.longe5.q55=="n/a"] <- NA
baseline_dealers$mid_maize.owner.agree.longe5.q55 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.longe5.q55))
baseline_dealers$mid_maize.owner.agree.longe5.q55[baseline_dealers$mid_maize.owner.agree.longe5.q54=="0"] <- 0
baseline_dealers <- trim("mid_maize.owner.agree.longe5.q55",baseline_dealers,trim_perc=.01)

#10. Q56. How long (days) did it on average take to get restocked for Longe 5 during the second season of 2020: (days)
baseline_dealers$maize.owner.agree.longe5.q56[baseline_dealers$maize.owner.agree.longe5.q56=="n/a"] <- NA
baseline_dealers$maize.owner.agree.longe5.q56[baseline_dealers$maize.owner.agree.longe5.q56==999] <- NA
baseline_dealers$maize.owner.agree.longe5.q56 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.longe5.q56))
baseline_dealers <- trim("maize.owner.agree.longe5.q56",baseline_dealers,trim_perc=.01)

baseline_dealers$mid_maize.owner.agree.longe5.q56 <- (baseline_dealers$maize.owner.agree.longe5.q56+0.7563492*baseline_dealers$training+0.7563492*baseline_dealers$clearing+0.7563492*baseline_dealers$farmer)
baseline_dealers$mid_maize.owner.agree.longe5.q56[baseline_dealers$mid_maize.owner.agree.longe5.q56=="n/a"] <- NA
baseline_dealers$mid_maize.owner.agree.longe5.q56[baseline_dealers$mid_maize.owner.agree.longe5.q56==999] <- NA
baseline_dealers$mid_maize.owner.agree.longe5.q56 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.longe5.q56))
baseline_dealers <- trim("mid_maize.owner.agree.longe5.q56",baseline_dealers,trim_perc=.01)

#dont forget to transform like bl
#dont forget to trim
#dont forget to simulate midline
#dont forget to trim

#11. Overall index of secondary Longe5 agro-input dealer outcome variables
###1. For all outcomes, switch signs where necessary so that the positive direction always indicates a "better" outcome.

###3. Define groupings/areas/domains of outcomes: each outcome is assigned to one of these areas
variables_overall_Longe5_mid <- cbind(baseline_dealers$mid_maize.owner.agree.longe5.q47
                                        ,baseline_dealers$mid_maize.owner.agree.longe5.q50)
variables_overall_Longe5_base <- cbind(baseline_dealers$maize.owner.agree.longe5.q47
                                         ,baseline_dealers$maize.owner.agree.longe5.q50)






################################################################################################################################################################################
###4. Create index: weighted average of outcomes for individual i in area j

###
#1#
###

#4.
index_overall_Longe5_mid <- icwIndex(xmat=variables_overall_Longe5_mid)
baseline_dealers$index_overall_Longe5_mid <- index_overall_Longe5_mid$index #midline index

index_overall_Longe5_base <- icwIndex(xmat=variables_overall_Longe5_base)
baseline_dealers$index_overall_Longe5_base <- index_overall_Longe5_base$index #baseline index

results_dealer_secL5 <- c("mid_maize.owner.agree.longe5.q46","mid_maize.owner.agree.longe5.q47","mid_maize.owner.agree.longe5.q49"
                            ,"mid_maize.owner.agree.longe5.q50","mid_maize.owner.agree.longe5.q51","mid_maize.owner.agree.longe5.q52"
                            ,"mid_maize.owner.agree.longe5.q55","index_overall_Longe5_mid")

results_dealer_secL5_base <- c("maize.owner.agree.longe5.q46","maize.owner.agree.longe5.q47","maize.owner.agree.longe5.q49"
                                 ,"maize.owner.agree.longe5.q50","maize.owner.agree.longe5.q51","maize.owner.agree.longe5.q52"
                                 ,"maize.owner.agree.longe5.q55","index_overall_Longe5_base")

df_means_D_secL5 <- array(NA,dim=c(3,11))

for (i in 1:length(results_dealer_secL5)){
  df_means_D_secL5[1,i] <- sum(baseline_dealers[results_dealer_secL5[i]], na.rm=T)/(nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_secL5[i]])))
  df_means_D_secL5[2,i] <- sqrt(var(baseline_dealers[results_dealer_secL5[i]], na.rm=T))
  df_means_D_secL5[3,i] <- nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_secL5[i]]))-sum(is.na(baseline_dealers[results_dealer_secL5_base[i]]))+sum(is.na(baseline_dealers[results_dealer_secL5[i]])&is.na(baseline_dealers[results_dealer_secL5_base[i]]))}

###
#2#
###

baseline_dealers$training_control[baseline_dealers$training==0] <- TRUE
baseline_dealers$training_control[baseline_dealers$training==1] <- FALSE

#4.
index_overall_Longe5_mid <- icwIndex(xmat=variables_overall_Longe5_mid,sgroup = baseline_dealers$training_control)
baseline_dealers$index_overall_Longe5_midT <- index_overall_Longe5_mid$index

index_overall_Longe5_base <- icwIndex(xmat=variables_overall_Longe5_base,sgroup = baseline_dealers$training_control)
baseline_dealers$index_overall_Longe5_baseT <- index_overall_Longe5_base$index

df_ols_D_secL5 <- array(NA,dim=c(3,3,11))

results_dealer_secL5 <- c("mid_maize.owner.agree.longe5.q46","mid_maize.owner.agree.longe5.q47","mid_maize.owner.agree.longe5.q49"
                            ,"mid_maize.owner.agree.longe5.q50","mid_maize.owner.agree.longe5.q51","mid_maize.owner.agree.longe5.q52"
                            ,"mid_maize.owner.agree.longe5.q55","index_overall_Longe5_midT")

results_dealer_secL5_base <- c("maize.owner.agree.longe5.q46","maize.owner.agree.longe5.q47","maize.owner.agree.longe5.q49"
                                 ,"maize.owner.agree.longe5.q50","maize.owner.agree.longe5.q51","maize.owner.agree.longe5.q52"
                                 ,"maize.owner.agree.longe5.q55","index_overall_Longe5_baseT")

for (i in 1:length(results_dealer_secL5)){
  ols <- lm(as.formula(paste(paste(results_dealer_secL5[i],"training*clearing*farmer",sep="~"),results_dealer_secL5_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_secL5[i],"training*clearing*farmer",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  df_ols_D_secL5[1,1,i] <- coef_test(ols, vcov_cluster)[2,1]
  df_ols_D_secL5[2,1,i] <- coef_test(ols, vcov_cluster)[2,2]
  df_ols_D_secL5[3,1,i] <- coef_test(ols, vcov_cluster)[2,5]}

###
#3#
###

baseline_dealers$clearing_control[baseline_dealers$clearing==0] <- TRUE
baseline_dealers$clearing_control[baseline_dealers$clearing==1] <- FALSE

#4.
index_overall_Longe5_mid <- icwIndex(xmat=variables_overall_Longe5_mid,sgroup = baseline_dealers$clearing_control)
baseline_dealers$index_overall_Longe5_midC <- index_overall_Longe5_mid$index

index_overall_Longe5_base <- icwIndex(xmat=variables_overall_Longe5_base,sgroup = baseline_dealers$clearing_control)
baseline_dealers$index_overall_Longe5_baseC <- index_overall_Longe5_base$index

results_dealer_secL5 <- c("mid_maize.owner.agree.longe5.q46","mid_maize.owner.agree.longe5.q47","mid_maize.owner.agree.longe5.q49"
                            ,"mid_maize.owner.agree.longe5.q50","mid_maize.owner.agree.longe5.q51","mid_maize.owner.agree.longe5.q52"
                            ,"mid_maize.owner.agree.longe5.q55","index_overall_Longe5_midC")

results_dealer_secL5_base <- c("maize.owner.agree.longe5.q46","maize.owner.agree.longe5.q47","maize.owner.agree.longe5.q49"
                                 ,"maize.owner.agree.longe5.q50","maize.owner.agree.longe5.q51","maize.owner.agree.longe5.q52"
                                 ,"maize.owner.agree.longe5.q55","index_overall_Longe5_baseC")

for (i in 1:length(results_dealer_secL5)){
  ols <- lm(as.formula(paste(paste(results_dealer_secL5[i],"training*clearing*farmer",sep="~"),results_dealer_secL5_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_secL5[i],"training*clearing*farmer",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  df_ols_D_secL5[1,2,i] <- coef_test(ols, vcov_cluster)[3,1]
  df_ols_D_secL5[2,2,i] <- coef_test(ols, vcov_cluster)[3,2]
  df_ols_D_secL5[3,2,i] <- coef_test(ols, vcov_cluster)[3,5]}

###
#4#
###

baseline_dealers$farmer_control[baseline_dealers$farmer==0] <- TRUE
baseline_dealers$farmer_control[baseline_dealers$farmer==1] <- FALSE

#4.
index_overall_Longe5_mid <- icwIndex(xmat=variables_overall_Longe5_mid,sgroup = baseline_dealers$farmer_control)
baseline_dealers$index_overall_Longe5_midF <- index_overall_Longe5_mid$index

index_overall_Longe5_base <- icwIndex(xmat=variables_overall_Longe5_base,sgroup = baseline_dealers$farmer_control)
baseline_dealers$index_overall_Longe5_baseF <- index_overall_Longe5_base$index

results_dealer_secL5 <- c("mid_maize.owner.agree.longe5.q46","mid_maize.owner.agree.longe5.q47","mid_maize.owner.agree.longe5.q49"
                            ,"mid_maize.owner.agree.longe5.q50","mid_maize.owner.agree.longe5.q51","mid_maize.owner.agree.longe5.q52"
                            ,"mid_maize.owner.agree.longe5.q55","index_overall_Longe5_midF")

results_dealer_secL5_base <- c("maize.owner.agree.longe5.q46","maize.owner.agree.longe5.q47","maize.owner.agree.longe5.q49"
                                 ,"maize.owner.agree.longe5.q50","maize.owner.agree.longe5.q51","maize.owner.agree.longe5.q52"
                                 ,"maize.owner.agree.longe5.q55","index_overall_Longe5_baseF")

for (i in 1:length(results_dealer_secL5)){
  ols <- lm(as.formula(paste(paste(results_dealer_secL5[i],"training*clearing*farmer",sep="~"),results_dealer_secL5_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_secL5[i],"training*clearing*farmer",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  #farmer video treatment at village/shop level so no clustering needed
  df_ols_D_secL5[1,3,i] <- summary(ols)$coefficients[4,1]
  df_ols_D_secL5[2,3,i] <- summary(ols)$coefficients[4,2]
  df_ols_D_secL5[3,3,i] <- summary(ols)$coefficients[4,4]}


#Aker, Boumnijel, McClelland, Tierney (2012)
df_dealer_secL5T <- data.frame(baseline_dealers$mid_maize.owner.agree.longe5.q46
                               ,baseline_dealers$mid_maize.owner.agree.longe5.q47,baseline_dealers$mid_maize.owner.agree.longe5.q49
                                ,baseline_dealers$mid_maize.owner.agree.longe5.q50,baseline_dealers$mid_maize.owner.agree.longe5.q51
                                ,baseline_dealers$mid_maize.owner.agree.longe5.q52
                                ,baseline_dealers$mid_maize.owner.agree.longe5.q55)

df_dealer_secL5C <- df_dealer_secL5T
df_dealer_secL5F <- df_dealer_secL5T
#no overall index

df_ols_D_secL5_J <- array(NA,dim=c(3,3,11))

results_dealer_secL5_J <- c("mid_maize.owner.agree.longe5.q46"
                            ,"mid_maize.owner.agree.longe5.q47","mid_maize.owner.agree.longe5.q49"
                            ,"mid_maize.owner.agree.longe5.q50","mid_maize.owner.agree.longe5.q51"
                            ,"mid_maize.owner.agree.longe5.q52"
                            ,"mid_maize.owner.agree.longe5.q55")
#no overall index

for (i in 1:length(results_dealer_secL5_J)){
  df_ols_D_secL5_J[3,1,i] <- adjust_p(df_ols_D_secL5[3,1,i],df_dealer_secL5T,i)
  df_ols_D_secL5_J[3,2,i] <- adjust_p(df_ols_D_secL5[3,2,i],df_dealer_secL5C,i)
  df_ols_D_secL5_J[3,3,i] <- adjust_p(df_ols_D_secL5[3,3,i],df_dealer_secL5F,i)}

baseline_dealers=baseline_dealers_save










###################################################################################################################################################################
##### 6 ANALYSIS: Agro-input dealer - Secondary: 11. official #####################################################################################################
###################################################################################################################################################################

#1. Q114. Is this business registered as a seed dealer with UNADA (Uganda National Agro-input Dealers Association?
baseline_dealers$mid_maize.owner.agree.inspection.q114 <- baseline_dealers$maize.owner.agree.inspection.q114
baseline_dealers$mid_maize.owner.agree.inspection.q114[baseline_dealers$mid_maize.owner.agree.inspection.q114==98] <- NA #here because binary
baseline_dealers$mid_maize.owner.agree.inspection.q114<-as.character(baseline_dealers$mid_maize.owner.agree.inspection.q114)
#baseline_dealers$mid_maize.owner.agree.inspection.q114<-ifelse(baseline_dealers$mid_maize.owner.agree.inspection.q114=="Yes",1,0)
baseline_dealers$mid_maize.owner.agree.inspection.q114 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.inspection.q114))

#2. Q115. Does this business have a trading license issued by local government?
baseline_dealers$mid_maize.owner.agree.inspection.q115 <- baseline_dealers$maize.owner.agree.inspection.q115
baseline_dealers$mid_maize.owner.agree.inspection.q115[baseline_dealers$mid_maize.owner.agree.inspection.q115==98] <- NA #here because binary
baseline_dealers$mid_maize.owner.agree.inspection.q115<-as.character(baseline_dealers$mid_maize.owner.agree.inspection.q115)
#baseline_dealers$mid_maize.owner.agree.inspection.q115<-ifelse(baseline_dealers$mid_maize.owner.agree.inspection.q115=="Yes",1,0)
baseline_dealers$mid_maize.owner.agree.inspection.q115 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.inspection.q115))

#3. Q116. Is this business a member of any other professional association?
baseline_dealers$mid_maize.owner.agree.inspection.q116 <- baseline_dealers$maize.owner.agree.inspection.q116
baseline_dealers$mid_maize.owner.agree.inspection.q116[baseline_dealers$mid_maize.owner.agree.inspection.q116==98] <- NA #here because binary
baseline_dealers$mid_maize.owner.agree.inspection.q116<-as.character(baseline_dealers$mid_maize.owner.agree.inspection.q116)
#baseline_dealers$mid_maize.owner.agree.inspection.q116<-ifelse(baseline_dealers$mid_maize.owner.agree.inspection.q116=="Yes",1,0)
baseline_dealers$mid_maize.owner.agree.inspection.q116 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.inspection.q116))

#4. Q117. How often were you inspected by DAO/MAAIF or UNADA last year (indicate 0 if no inspection happened).
baseline_dealers <- trim("maize.owner.agree.inspection.q117",baseline_dealers,trim_perc=.01)
baseline_dealers$mid_maize.owner.agree.inspection.q117 <- (baseline_dealers$maize.owner.agree.inspection.q117+0.1621622*baseline_dealers$training+0.1621622*baseline_dealers$clearing+0.1621622*baseline_dealers$farmer)
baseline_dealers <- trim("mid_maize.owner.agree.inspection.q117",baseline_dealers,trim_perc=.01)

#5. Q118. Have you ever received a warning as a result of inspection if something was not up to standard?
baseline_dealers$mid_maize.owner.agree.inspection.q118<-baseline_dealers$maize.owner.agree.inspection.q118
baseline_dealers$mid_maize.owner.agree.inspection.q118[baseline_dealers$mid_maize.owner.agree.inspection.q118==98] <- NA
baseline_dealers$mid_maize.owner.agree.inspection.q118<-as.character(baseline_dealers$mid_maize.owner.agree.inspection.q118)
#baseline_dealers$mid_maize.owner.agree.inspection.q118<-ifelse(baseline_dealers$mid_maize.owner.agree.inspection.q118=="Yes",1,0)
baseline_dealers$mid_maize.owner.agree.inspection.q118<-as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.inspection.q118))

#6. Q119. Has some of your produce ever been confiscated after inspection?
baseline_dealers$mid_maize.owner.agree.inspection.q119<-baseline_dealers$maize.owner.agree.inspection.q119
baseline_dealers$mid_maize.owner.agree.inspection.q119[baseline_dealers$mid_maize.owner.agree.inspection.q119==98] <- NA
baseline_dealers$mid_maize.owner.agree.inspection.q119<-as.character(baseline_dealers$mid_maize.owner.agree.inspection.q119)
#baseline_dealers$mid_maize.owner.agree.inspection.q119<-ifelse(baseline_dealers$mid_maize.owner.agree.inspection.q119=="Yes",1,0)
baseline_dealers$mid_maize.owner.agree.inspection.q119<-as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.inspection.q119))

#7. Q120. Has this business ever been closed down due to quality issues after inspection?
baseline_dealers$mid_maize.owner.agree.inspection.q120 <- 0
baseline_dealers$mid_maize.owner.agree.inspection.q120[baseline_dealers$mid_maize.owner.agree.inspection.q120==98] <- NA
baseline_dealers$mid_maize.owner.agree.inspection.q120<-as.character(baseline_dealers$mid_maize.owner.agree.inspection.q120)
#baseline_dealers$mid_maize.owner.agree.inspection.q120<-ifelse(baseline_dealers$mid_maize.owner.agree.inspection.q120=="Yes",1,0)
baseline_dealers$mid_maize.owner.agree.inspection.q120 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.inspection.q120))

#8. Overall index of secondary OFFICIAL agro-input dealer outcome variables
###1. For all outcomes, switch signs where necessary so that the positive direction always indicates a "better" outcome.
baseline_dealers$maize.owner.agree.inspection.q118_pos <- baseline_dealers$maize.owner.agree.inspection.q118*-1
baseline_dealers$maize.owner.agree.inspection.q119_pos <- baseline_dealers$maize.owner.agree.inspection.q119*-1
baseline_dealers$maize.owner.agree.inspection.q120_pos <- baseline_dealers$maize.owner.agree.inspection.q120*-1

baseline_dealers$mid_maize.owner.agree.inspection.q118_pos <- baseline_dealers$mid_maize.owner.agree.inspection.q118*-1
baseline_dealers$mid_maize.owner.agree.inspection.q119_pos <- baseline_dealers$mid_maize.owner.agree.inspection.q119*-1
baseline_dealers$mid_maize.owner.agree.inspection.q120_pos <- baseline_dealers$mid_maize.owner.agree.inspection.q120*-1

###3. Define groupings/areas/domains of outcomes: each outcome is assigned to one of these areas
variables_overall_off_mid <- cbind(baseline_dealers$mid_maize.owner.agree.inspection.q114,baseline_dealers$mid_maize.owner.agree.inspection.q115
                                   ,baseline_dealers$mid_maize.owner.agree.inspection.q116,baseline_dealers$mid_maize.owner.agree.inspection.q118_pos
                                   ,baseline_dealers$mid_maize.owner.agree.inspection.q119_pos)
variables_overall_off_base <- cbind(baseline_dealers$maize.owner.agree.inspection.q114,baseline_dealers$maize.owner.agree.inspection.q11
                                    ,baseline_dealers$maize.owner.agree.inspection.q116,baseline_dealers$maize.owner.agree.inspection.q118_pos
                                    ,baseline_dealers$maize.owner.agree.inspection.q119_pos)


#dont forget to transform like bl
#dont forget to trim
#dont forget to simulate midline
#dont forget to trim

################################################################################################################################################################################
###4. Create index: weighted average of outcomes for individual i in area j

###
#1#
###

#4.
index_overall_off_mid <- icwIndex(xmat=variables_overall_off_mid)
baseline_dealers$index_overall_off_mid <- index_overall_off_mid$index #midline index

index_overall_off_base <- icwIndex(xmat=variables_overall_off_base)
baseline_dealers$index_overall_off_base <- index_overall_off_base$index #baseline index

results_dealer_sec_off <- c("mid_maize.owner.agree.inspection.q114","mid_maize.owner.agree.inspection.q115","mid_maize.owner.agree.inspection.q116","mid_maize.owner.agree.inspection.q117"
                            ,"mid_maize.owner.agree.inspection.q118","mid_maize.owner.agree.inspection.q119","mid_maize.owner.agree.inspection.q120"
                            ,"index_overall_off_mid")

results_dealer_sec_off_base <- c("maize.owner.agree.inspection.q114","maize.owner.agree.inspection.q115","maize.owner.agree.inspection.q116","maize.owner.agree.inspection.q117"
                                 ,"maize.owner.agree.inspection.q118","maize.owner.agree.inspection.q119","maize.owner.agree.inspection.q120"
                                 ,"index_overall_off_base")

df_means_D_sec_off <- array(NA,dim=c(3,11))

for (i in 1:length(results_dealer_sec_off)){
  df_means_D_sec_off[1,i] <- sum(baseline_dealers[results_dealer_sec_off[i]], na.rm=T)/(nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_sec_off[i]])))
  df_means_D_sec_off[2,i] <- sqrt(var(baseline_dealers[results_dealer_sec_off[i]], na.rm=T))
  df_means_D_sec_off[3,i] <- nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_sec_off[i]]))-sum(is.na(baseline_dealers[results_dealer_sec_off_base[i]]))+sum(is.na(baseline_dealers[results_dealer_sec_off[i]])&is.na(baseline_dealers[results_dealer_sec_off_base[i]]))}

###
#2#
###

baseline_dealers$training_control[baseline_dealers$training==0] <- TRUE
baseline_dealers$training_control[baseline_dealers$training==1] <- FALSE

#4.
index_overall_off_mid <- icwIndex(xmat=variables_overall_off_mid,sgroup = baseline_dealers$training_control)
baseline_dealers$index_overall_off_midT <- index_overall_off_mid$index

index_overall_off_base <- icwIndex(xmat=variables_overall_off_base,sgroup = baseline_dealers$training_control)
baseline_dealers$index_overall_off_baseT <- index_overall_off_base$index

df_ols_D_sec_off <- array(NA,dim=c(3,3,11))

results_dealer_sec_off <- c("mid_maize.owner.agree.inspection.q114","mid_maize.owner.agree.inspection.q115","mid_maize.owner.agree.inspection.q116","mid_maize.owner.agree.inspection.q117"
                            ,"mid_maize.owner.agree.inspection.q118","mid_maize.owner.agree.inspection.q119","mid_maize.owner.agree.inspection.q120"
                            ,"index_overall_off_midT")

results_dealer_sec_off_base <- c("maize.owner.agree.inspection.q114","maize.owner.agree.inspection.q115","maize.owner.agree.inspection.q116","maize.owner.agree.inspection.q117"
                                 ,"maize.owner.agree.inspection.q118","maize.owner.agree.inspection.q119","maize.owner.agree.inspection.q120"
                                 ,"index_overall_off_baseT")

for (i in 1:length(results_dealer_sec_off)){
  ols <- lm(as.formula(paste(paste(results_dealer_sec_off[i],"training*clearing*farmer",sep="~"),results_dealer_sec_off_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_sec_off[i],"training*clearing*farmer",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  df_ols_D_sec_off[1,1,i] <- coef_test(ols, vcov_cluster)[2,1]
  df_ols_D_sec_off[2,1,i] <- coef_test(ols, vcov_cluster)[2,2]
  df_ols_D_sec_off[3,1,i] <- coef_test(ols, vcov_cluster)[2,5]}

###
#3#
###

baseline_dealers$clearing_control[baseline_dealers$clearing==0] <- TRUE
baseline_dealers$clearing_control[baseline_dealers$clearing==1] <- FALSE

#4.
index_overall_off_mid <- icwIndex(xmat=variables_overall_off_mid,sgroup = baseline_dealers$clearing_control)
baseline_dealers$index_overall_off_midC <- index_overall_off_mid$index

index_overall_off_base <- icwIndex(xmat=variables_overall_off_base,sgroup = baseline_dealers$clearing_control)
baseline_dealers$index_overall_off_baseC <- index_overall_off_base$index

results_dealer_sec_off <- c("mid_maize.owner.agree.inspection.q114","mid_maize.owner.agree.inspection.q115","mid_maize.owner.agree.inspection.q116","mid_maize.owner.agree.inspection.q117"
                            ,"mid_maize.owner.agree.inspection.q118","mid_maize.owner.agree.inspection.q119","mid_maize.owner.agree.inspection.q120"
                            ,"index_overall_off_midC")

results_dealer_sec_off_base <- c("maize.owner.agree.inspection.q114","maize.owner.agree.inspection.q115","maize.owner.agree.inspection.q116","maize.owner.agree.inspection.q117"
                                 ,"maize.owner.agree.inspection.q118","maize.owner.agree.inspection.q119","maize.owner.agree.inspection.q120"
                                 ,"index_overall_off_baseC")

for (i in 1:length(results_dealer_sec_off)){
  ols <- lm(as.formula(paste(paste(results_dealer_sec_off[i],"training*clearing*farmer",sep="~"),results_dealer_sec_off_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_sec_off[i],"training*clearing*farmer",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  df_ols_D_sec_off[1,2,i] <- coef_test(ols, vcov_cluster)[3,1]
  df_ols_D_sec_off[2,2,i] <- coef_test(ols, vcov_cluster)[3,2]
  df_ols_D_sec_off[3,2,i] <- coef_test(ols, vcov_cluster)[3,5]}

###
#4#
###

baseline_dealers$farmer_control[baseline_dealers$farmer==0] <- TRUE
baseline_dealers$farmer_control[baseline_dealers$farmer==1] <- FALSE

#4.
index_overall_off_mid <- icwIndex(xmat=variables_overall_off_mid,sgroup = baseline_dealers$farmer_control)
baseline_dealers$index_overall_off_midF <- index_overall_off_mid$index

index_overall_off_base <- icwIndex(xmat=variables_overall_off_base,sgroup = baseline_dealers$farmer_control)
baseline_dealers$index_overall_off_baseF <- index_overall_off_base$index

results_dealer_sec_off <- c("mid_maize.owner.agree.inspection.q114","mid_maize.owner.agree.inspection.q115","mid_maize.owner.agree.inspection.q116","mid_maize.owner.agree.inspection.q117"
                            ,"mid_maize.owner.agree.inspection.q118","mid_maize.owner.agree.inspection.q119","mid_maize.owner.agree.inspection.q120"
                            ,"index_overall_off_midF")

results_dealer_sec_off_base <- c("maize.owner.agree.inspection.q114","maize.owner.agree.inspection.q115","maize.owner.agree.inspection.q116","maize.owner.agree.inspection.q117"
                                 ,"maize.owner.agree.inspection.q118","maize.owner.agree.inspection.q119","maize.owner.agree.inspection.q120"
                                 ,"index_overall_off_baseF")

for (i in 1:length(results_dealer_sec_off)){
  ols <- lm(as.formula(paste(paste(results_dealer_sec_off[i],"training*clearing*farmer",sep="~"),results_dealer_sec_off_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_sec_off[i],"training*clearing*farmer",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  #farmer video treatment at village/shop level so no clustering needed
  df_ols_D_sec_off[1,3,i] <- summary(ols)$coefficients[4,1]
  df_ols_D_sec_off[2,3,i] <- summary(ols)$coefficients[4,2]
  df_ols_D_sec_off[3,3,i] <- summary(ols)$coefficients[4,4]}

#Aker, Boumnijel, McClelland, Tierney (2012)
df_dealer_sec_offT <- data.frame(baseline_dealers$mid_maize.owner.agree.inspection.q114,baseline_dealers$mid_maize.owner.agree.inspection.q115
                                 ,baseline_dealers$mid_maize.owner.agree.inspection.q116,baseline_dealers$mid_maize.owner.agree.inspection.q117
                                 ,baseline_dealers$mid_maize.owner.agree.inspection.q118,baseline_dealers$mid_maize.owner.agree.inspection.q119
                                 )
df_dealer_sec_offC <- df_dealer_sec_offT
df_dealer_sec_offF <- df_dealer_sec_offT
#no overall index

df_ols_D_sec_off_J <- array(NA,dim=c(3,3,11))

results_dealer_sec_off_J <- c("mid_maize.owner.agree.inspection.q114","mid_maize.owner.agree.inspection.q115"
                            ,"mid_maize.owner.agree.inspection.q116","mid_maize.owner.agree.inspection.q117"
                            ,"mid_maize.owner.agree.inspection.q118","mid_maize.owner.agree.inspection.q119"
                            )
#no overall index

for (i in 1:length(results_dealer_sec_off_J)){
  df_ols_D_sec_off_J[3,1,i] <- adjust_p(df_ols_D_sec_off[3,1,i],df_dealer_sec_offT,i)
  df_ols_D_sec_off_J[3,2,i] <- adjust_p(df_ols_D_sec_off[3,2,i],df_dealer_sec_offC,i)
  df_ols_D_sec_off_J[3,3,i] <- adjust_p(df_ols_D_sec_off[3,3,i],df_dealer_sec_offF,i)}
