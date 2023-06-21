rm(list=ls())

path <- getwd()
path <- strsplit(path,"/papers/clearinghouse_training_paper")[[1]]



#BASELINE
rating_dyads_baseline <- read.csv(paste(path,"/baseline/data/farmer/public/rating_dyads.csv", sep="/"), stringsAsFactors = TRUE)
rating_dyads_baseline=subset(rating_dyads_baseline,bought_last_season=="Yes")

reviews_seed_baseline <- read.csv(paste(path,"/baseline/data/agro_input/public/reviews_seed.csv", sep="/"), stringsAsFactors = TRUE)

reviews_seed_baseline$score_corrected_standardized <- ((reviews_seed_baseline$score_corrected
                                                       - mean(reviews_seed_baseline$score_corrected,na.rm = T))
                                                       / sd(reviews_seed_baseline$score_corrected,na.rm = T))

names(reviews_seed_baseline)[names(reviews_seed_baseline) == "score_corrected_standardized"] <- "rat_at_baseline_of_dealer_farmer_chose_at_baseline"

rating_dyads_baseline <- merge(rating_dyads_baseline[,c("shop_ID","farmer_ID")]
                               ,reviews_seed_baseline[,c("shop_ID","rat_at_baseline_of_dealer_farmer_chose_at_baseline")]
                               ,by="shop_ID")



#MIDLINE
rating_dyads_midline <- read.csv(paste(path,"/midline/data/farmer/public/midline_rating_dyads.csv", sep="/"), stringsAsFactors = TRUE)
rating_dyads_midline=subset(rating_dyads_midline,bought_last_season=="Yes")

reviews_seed_midline <- read.csv(paste(path,"/midline/data/agro_input/public/reviews_seed.csv", sep="/"), stringsAsFactors = TRUE)

#reviews_seed_midline$score_corrected <- reviews_seed_midline$score #for option (c)

reviews_seed_midline$score_corrected_standardized <- ((reviews_seed_midline$score_corrected #for option (d)
                                                       - mean(reviews_seed_midline$score_corrected,na.rm = T))
                                                      / sd(reviews_seed_midline$score_corrected,na.rm = T))

#reviews_seed_midline$score_corrected_standardized <- reviews_seed_midline$score #for option (a)
#reviews_seed_midline$score_corrected_standardized <- reviews_seed_midline$score_corrected #for #option (b)


names(reviews_seed_midline)[names(reviews_seed_midline) == "score_corrected_standardized"] <- "rat_at_midline_of_dealer_farmer_chose_at_midline"

rating_dyads_midline <- merge(rating_dyads_midline[,c("shop_ID","farmer_ID")]
                              ,reviews_seed_midline[,c("shop_ID","rat_at_midline_of_dealer_farmer_chose_at_midline")]
                              ,by="shop_ID")

reviews_seed_baseline <- read.csv(paste(path,"/baseline/data/agro_input/public/reviews_seed.csv", sep="/"), stringsAsFactors = TRUE)

reviews_seed_baseline$score_corrected_standardized <- ((reviews_seed_baseline$score_corrected
                                                        - mean(reviews_seed_baseline$score_corrected,na.rm = T))
                                                       / sd(reviews_seed_baseline$score_corrected,na.rm = T))


names(reviews_seed_baseline)[names(reviews_seed_baseline) == "score_corrected_standardized"] <- "rat_at_baseline_of_dealer_farmer_chose_at_midline"

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

t.test(rating_dyads_baseline_midline$diff_between_rat_at_baseline[rating_dyads_baseline_midline$farmer_switched==1], mu=0, alternative = "greater")
#Bjorn says this is wrong way to do t.test (see correct way below)

#ENDLINE
rating_dyads_endline <- read.csv(paste(path,"/endline/data/farmer/public/endline_rating_dyads.csv", sep="/"), stringsAsFactors = TRUE)
rating_dyads_endline=subset(rating_dyads_endline,bought_last_season=="Yes")

#for CH treatment status
reviews_seed_endline <- read.csv(paste(path,"/endline/data/agro_input/public/reviews_seed.csv", sep="/"), stringsAsFactors = TRUE)
rating_dyads_endline <- merge(rating_dyads_endline[,c("shop_ID","farmer_ID")]
                              ,reviews_seed_endline[,c("shop_ID","clearing")]
                              ,by="shop_ID")

reviews_seed_midline <- read.csv(paste(path,"/midline/data/agro_input/public/reviews_seed.csv", sep="/"), stringsAsFactors = TRUE)

reviews_seed_midline$score_corrected_standardized <- ((reviews_seed_midline$score_corrected
                                                       - mean(reviews_seed_midline$score_corrected,na.rm = T))
                                                      / sd(reviews_seed_midline$score_corrected,na.rm = T))

names(reviews_seed_midline)[names(reviews_seed_midline) == "score_corrected_standardized"] <- "rat_at_midline_of_dealer_farmer_chose_at_endline"

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

t.test(rating_dyads_midline_endline$diff_between_rat_at_midline[rating_dyads_midline_endline$farmer_switched==1], mu=0, alternative = "greater")
#Bjorn says this is wrong way to do t.test (see correct way below)

#Stacking BASE, MID & ENDLINE
rating_dyads_baseline_midline$difference <- rating_dyads_baseline_midline$diff_between_rat_at_baseline
rating_dyads_midline_endline$difference <- rating_dyads_midline_endline$diff_between_rat_at_midline


rating_dyads_baseline_midline_endline <- rbind(rating_dyads_baseline_midline[,c("difference","farmer_switched")]
                                               ,rating_dyads_midline_endline[,c("difference","farmer_switched")])

mean(rating_dyads_baseline_midline_endline$difference[rating_dyads_baseline_midline_endline$farmer_switched==1],na.rm = T)
mean(rating_dyads_baseline_midline_endline$difference[rating_dyads_baseline_midline_endline$farmer_switched==0])

t.test(rating_dyads_baseline_midline_endline$difference[rating_dyads_baseline_midline_endline$farmer_switched==1], mu=0, alternative = "greater")
#Bjorn says this is wrong way to do t.test (see correct way below)









######## did farmers who chose high-rated dealers at midline have more yield at endline?

endline_farmers <- read.csv(paste(path,"/endline/data/farmer/public/endline.csv",sep="/"), stringsAsFactors=TRUE)

endline_farmers$check.maize.q50[endline_farmers$check.maize.q50==999]<-NA
endline_farmers$check.maize.q51[endline_farmers$check.maize.q51==999]<-NA
endline_farmers$check.maize.q29[endline_farmers$check.maize.q29==999]<-NA

endline_farmers$check.maize.q50<-as.numeric(as.character(endline_farmers$check.maize.q50))
endline_farmers$check.maize.q51<-as.numeric(as.character(endline_farmers$check.maize.q51))
endline_farmers$check.maize.q29<-as.numeric(as.character(endline_farmers$check.maize.q29))

trim <- function(var,dataset,trim_perc=.02){
  dataset[var][dataset[var]<quantile(dataset[var],c(trim_perc/2,1-(trim_perc/2)),na.rm=T)[1]|dataset[var]>quantile(dataset[var],c(trim_perc/2,1-(trim_perc/2)),na.rm=T)[2]] <- NA
  return(dataset)}

endline_farmers <- trim("check.maize.q50",endline_farmers,trim_perc=.05)
endline_farmers <- trim("check.maize.q51",endline_farmers,trim_perc=.05)
endline_farmers <- trim("check.maize.q29",endline_farmers,trim_perc=.05)

endline_farmers$yield_inkg <- endline_farmers$check.maize.q50*endline_farmers$check.maize.q51 #production in kg
endline_farmers$landproductivity_endline <- endline_farmers$yield_inkg/endline_farmers$check.maize.q29 #yield in kg per acre

rating_dyads_midline <- merge(rating_dyads_midline[,c("shop_ID","farmer_ID","rat_at_midline_of_dealer_farmer_chose_at_midline")]
                              ,endline_farmers[,c("farmer_ID","landproductivity_endline")]
                              ,by="farmer_ID")

rating_dyads_midline$rating_mean <- mean(rating_dyads_midline$rat_at_midline_of_dealer_farmer_chose_at_midline,na.rm = T)

rating_dyads_midline$rat_bin <- ifelse(rating_dyads_midline$rat_at_midline_of_dealer_farmer_chose_at_midline >= rating_dyads_midline$rating_mean,1,0)

table(rating_dyads_midline$rat_bin)

mean(rating_dyads_midline$landproductivity_endline[rating_dyads_midline$rat_bin==1],na.rm = T)
mean(rating_dyads_midline$landproductivity_endline[rating_dyads_midline$rat_bin==0],na.rm = T)

t.test(rating_dyads_midline$landproductivity_endline~rating_dyads_midline$rat_bin)

#3 options:
#score is option (a)
#score_corrected is option (b)
#score_standardized (c)
#score_corrected_standardized (d)
#correct above!!