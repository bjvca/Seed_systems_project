rm(list=ls())

path <- getwd()
path <- strsplit(path,"/papers/clearinghouse_training_paper")[[1]]



#BASELINE
rating_dyads_baseline <- read.csv(paste(path,"/baseline/data/farmer/public/rating_dyads.csv", sep="/"), stringsAsFactors = TRUE)
rating_dyads_baseline=subset(rating_dyads_baseline,bought_last_season=="Yes")

reviews_seed_baseline <- read.csv(paste(path,"/baseline/data/agro_input/public/reviews_seed.csv", sep="/"), stringsAsFactors = TRUE)
names(reviews_seed_baseline)[names(reviews_seed_baseline) == "score_corrected"] <- "rat_at_baseline_of_dealer_farmer_chose_at_baseline"

rating_dyads_baseline <- merge(rating_dyads_baseline[,c("shop_ID","farmer_ID")]
                               ,reviews_seed_baseline[,c("shop_ID","rat_at_baseline_of_dealer_farmer_chose_at_baseline")]
                               ,by="shop_ID")



#MIDLINE
rating_dyads_midline <- read.csv(paste(path,"/midline/data/farmer/public/midline_rating_dyads.csv", sep="/"), stringsAsFactors = TRUE)
rating_dyads_midline=subset(rating_dyads_midline,bought_last_season=="Yes")

reviews_seed_midline <- read.csv(paste(path,"/midline/data/agro_input/public/reviews_seed.csv", sep="/"), stringsAsFactors = TRUE)
names(reviews_seed_midline)[names(reviews_seed_midline) == "score_corrected"] <- "rat_at_midline_of_dealer_farmer_chose_at_midline"

rating_dyads_midline <- merge(rating_dyads_midline[,c("shop_ID","farmer_ID")]
                              ,reviews_seed_midline[,c("shop_ID","rat_at_midline_of_dealer_farmer_chose_at_midline")]
                              ,by="shop_ID")

reviews_seed_baseline <- read.csv(paste(path,"/baseline/data/agro_input/public/reviews_seed.csv", sep="/"), stringsAsFactors = TRUE)
names(reviews_seed_baseline)[names(reviews_seed_baseline) == "score_corrected"] <- "rat_at_baseline_of_dealer_farmer_chose_at_midline"

rating_dyads_midline <- merge(rating_dyads_midline[,c("shop_ID","farmer_ID","rat_at_midline_of_dealer_farmer_chose_at_midline")]
                              ,reviews_seed_baseline[,c("shop_ID","rat_at_baseline_of_dealer_farmer_chose_at_midline")]
                              ,by="shop_ID")



#BASE & MIDLINE
rating_dyads_baseline_midline <- merge(rating_dyads_baseline[,c("shop_ID","farmer_ID","rat_at_baseline_of_dealer_farmer_chose_at_baseline")]
                                       ,rating_dyads_midline[,c("shop_ID","farmer_ID","rat_at_baseline_of_dealer_farmer_chose_at_midline")]
                                       ,by="farmer_ID")

mean(rating_dyads_baseline_midline$rat_at_baseline_of_dealer_farmer_chose_at_baseline,na.rm = T)
mean(rating_dyads_baseline_midline$rat_at_baseline_of_dealer_farmer_chose_at_midline,na.rm = T)

rating_dyads_baseline_midline$diff_between_rat_at_baseline <- (rating_dyads_baseline_midline$rat_at_baseline_of_dealer_farmer_chose_at_midline
                                                               - rating_dyads_baseline_midline$rat_at_baseline_of_dealer_farmer_chose_at_baseline)

rating_dyads_baseline_midline$shop_ID.x <- as.character(rating_dyads_baseline_midline$shop_ID.x)
rating_dyads_baseline_midline$shop_ID.y <- as.character(rating_dyads_baseline_midline$shop_ID.y)

sum(rating_dyads_baseline_midline$shop_ID.x==rating_dyads_baseline_midline$shop_ID.y)

rating_dyads_baseline_midline$farmer_switched <- ifelse(rating_dyads_baseline_midline$shop_ID.x==rating_dyads_baseline_midline$shop_ID.y,0,1)

mean(rating_dyads_baseline_midline$diff_between_rat_at_baseline[rating_dyads_baseline_midline$farmer_switched==1])
mean(rating_dyads_baseline_midline$diff_between_rat_at_baseline[rating_dyads_baseline_midline$farmer_switched==0])



#ENDLINE
rating_dyads_endline <- read.csv(paste(path,"/endline/data/farmer/public/endline_rating_dyads.csv", sep="/"), stringsAsFactors = TRUE)
rating_dyads_endline=subset(rating_dyads_endline,bought_last_season=="Yes")

#for CH treatment status
reviews_seed_endline <- read.csv(paste(path,"/endline/data/agro_input/public/reviews_seed.csv", sep="/"), stringsAsFactors = TRUE)
rating_dyads_endline <- merge(rating_dyads_endline[,c("shop_ID","farmer_ID")]
                              ,reviews_seed_endline[,c("shop_ID","clearing")]
                              ,by="shop_ID")

reviews_seed_midline <- read.csv(paste(path,"/midline/data/agro_input/public/reviews_seed.csv", sep="/"), stringsAsFactors = TRUE)
names(reviews_seed_midline)[names(reviews_seed_midline) == "score_corrected"] <- "rat_at_midline_of_dealer_farmer_chose_at_endline"

rating_dyads_endline <- merge(rating_dyads_endline[,c("shop_ID","farmer_ID","clearing")]
                              ,reviews_seed_midline[,c("shop_ID","rat_at_midline_of_dealer_farmer_chose_at_endline")]
                              ,by="shop_ID")

#online CH treated:
rating_dyads_endline=subset(rating_dyads_endline,clearing==T)



#MID & ENDLINE
rating_dyads_midline_endline <- merge(rating_dyads_midline[,c("shop_ID","farmer_ID","rat_at_midline_of_dealer_farmer_chose_at_midline")]
                                      ,rating_dyads_endline[,c("shop_ID","farmer_ID","rat_at_midline_of_dealer_farmer_chose_at_endline")]
                                      ,by="farmer_ID")

mean(rating_dyads_midline_endline$rat_at_midline_of_dealer_farmer_chose_at_midline,na.rm = T)
mean(rating_dyads_midline_endline$rat_at_midline_of_dealer_farmer_chose_at_endline,na.rm = T)

rating_dyads_midline_endline$diff_between_rat_at_midline <- (rating_dyads_midline_endline$rat_at_midline_of_dealer_farmer_chose_at_endline
                                                               - rating_dyads_midline_endline$rat_at_midline_of_dealer_farmer_chose_at_midline)

rating_dyads_midline_endline$shop_ID.x <- as.character(rating_dyads_midline_endline$shop_ID.x)
rating_dyads_midline_endline$shop_ID.y <- as.character(rating_dyads_midline_endline$shop_ID.y)

sum(rating_dyads_midline_endline$shop_ID.x==rating_dyads_midline_endline$shop_ID.y)

rating_dyads_midline_endline$farmer_switched <- ifelse(rating_dyads_midline_endline$shop_ID.x==rating_dyads_midline_endline$shop_ID.y,0,1)

mean(rating_dyads_midline_endline$diff_between_rat_at_midline[rating_dyads_midline_endline$farmer_switched==1],na.rm = T)
mean(rating_dyads_midline_endline$diff_between_rat_at_midline[rating_dyads_midline_endline$farmer_switched==0])



#Stacking BASE, MID & ENDLINE
rating_dyads_baseline_midline$difference <- rating_dyads_baseline_midline$diff_between_rat_at_baseline
rating_dyads_midline_endline$difference <- rating_dyads_midline_endline$diff_between_rat_at_midline


rating_dyads_baseline_midline_endline <- rbind(rating_dyads_baseline_midline[,c("difference","farmer_switched")]
                                               ,rating_dyads_midline_endline[,c("difference","farmer_switched")])

mean(rating_dyads_baseline_midline_endline$difference[rating_dyads_baseline_midline_endline$farmer_switched==1],na.rm = T)
mean(rating_dyads_baseline_midline_endline$difference[rating_dyads_baseline_midline_endline$farmer_switched==0])
