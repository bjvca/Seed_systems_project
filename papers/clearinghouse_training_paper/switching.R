rm(list=ls())

path <- getwd()
path <- strsplit(path,"/papers/clearinghouse_training_paper")[[1]]

#BASELINE
rating_dyads_baseline <- read.csv(paste(path,"/baseline/data/farmer/public/rating_dyads.csv", sep="/"), stringsAsFactors = TRUE)
rating_dyads_baseline=subset(rating_dyads_baseline,bought_last_season=="Yes")

reviews_seed_baseline <- read.csv(paste(path,"/baseline/data/agro_input/public/reviews_seed.csv", sep="/"), stringsAsFactors = TRUE)
names(reviews_seed_baseline)[names(reviews_seed_baseline) == "score_corrected"] <- "score_corrected_baseline"

rating_dyads_baseline <- merge(rating_dyads_baseline,reviews_seed_baseline,by="shop_ID")

# rating_dyads_baseline[rating_dyads_baseline==98] <- NA
# 
# as_numeric <- c("seed_quality_general_rating","seed_yield_rating",
#                 "seed_drought_rating","seed_disease_rating",
#                 "seed_maturing_rating","seed_germinate_rating")
# rating_dyads_baseline[as_numeric] <- lapply(rating_dyads_baseline[as_numeric],function(x)as.numeric(as.character(x)))
# 
# rating_dyads_baseline$score_baseline <- rowMeans(rating_dyads_baseline[c("seed_quality_general_rating","seed_yield_rating",
#                                                                          "seed_drought_rating","seed_disease_rating",
#                                                                          "seed_maturing_rating","seed_germinate_rating")],na.rm = T)



#MIDLINE
rating_dyads_midline <- read.csv(paste(path,"/midline/data/farmer/public/midline_rating_dyads.csv", sep="/"), stringsAsFactors = TRUE)
rating_dyads_midline=subset(rating_dyads_midline,bought_last_season=="Yes")

reviews_seed_midline <- read.csv(paste(path,"/midline/data/agro_input/public/reviews_seed.csv", sep="/"), stringsAsFactors = TRUE)
names(reviews_seed_midline)[names(reviews_seed_midline) == "score_corrected"] <- "score_corrected_midline"

rating_dyads_midline <- merge(rating_dyads_midline,reviews_seed_midline,by="shop_ID")


# rating_dyads_midline[rating_dyads_midline==98] <- NA
# 
# as_numeric <- c("seed_quality_general_rating","seed_yield_rating",
#                 "seed_drought_rating","seed_disease_rating",
#                 "seed_maturing_rating","seed_germinate_rating")
# rating_dyads_midline[as_numeric] <- lapply(rating_dyads_midline[as_numeric],function(x)as.numeric(as.character(x)))
# 
# 
# rating_dyads_midline$score_midline <- rowMeans(rating_dyads_midline[c("seed_quality_general_rating","seed_yield_rating",
#                                                                       "seed_drought_rating","seed_disease_rating",
#                                                                       "seed_maturing_rating","seed_germinate_rating")],na.rm = T)



#BASE & MIDLINE
rating_dyads_baseline_midline <- merge(rating_dyads_baseline,rating_dyads_midline,by="farmer_ID")

# mean(rating_dyads_baseline_midline$score_baseline)
# #[1] 3.43193
# mean(rating_dyads_baseline_midline$score_midline)
# #[1] 3.524474

rating_dyads_baseline_midline$shop_ID.x <- as.character(rating_dyads_baseline_midline$shop_ID.x)
rating_dyads_baseline_midline$shop_ID.y <- as.character(rating_dyads_baseline_midline$shop_ID.y)

sum(rating_dyads_baseline_midline$shop_ID.x==rating_dyads_baseline_midline$shop_ID.y)

rating_dyads_baseline_midline$farmer_switched <- ifelse(rating_dyads_baseline_midline$shop_ID.x==rating_dyads_baseline_midline$shop_ID.y,0,1)

mean(rating_dyads_baseline_midline$score_corrected_midline[rating_dyads_baseline_midline$farmer_switched==1]) #[1] 3.58724
mean(rating_dyads_baseline_midline$score_corrected_midline[rating_dyads_baseline_midline$farmer_switched==0]) #[1] 3.590443



#ENDLINE
rating_dyads_endline <- read.csv(paste(path,"/endline/data/farmer/public/endline_rating_dyads.csv", sep="/"), stringsAsFactors = TRUE)
rating_dyads_endline=subset(rating_dyads_endline,bought_last_season=="Yes")

reviews_seed_endline <- read.csv(paste(path,"/endline/data/agro_input/public/reviews_seed.csv", sep="/"), stringsAsFactors = TRUE)
names(reviews_seed_endline)[names(reviews_seed_endline) == "score_corrected"] <- "score_corrected_endline"

rating_dyads_endline <- merge(rating_dyads_endline,reviews_seed_endline,by="shop_ID")

# rating_dyads_endline[rating_dyads_endline==98] <- NA
# 
# as_numeric <- c("seed_quality_general_rating","seed_yield_rating",
#                 "seed_drought_rating","seed_disease_rating",
#                 "seed_maturing_rating","seed_germinate_rating")
# rating_dyads_endline[as_numeric] <- lapply(rating_dyads_endline[as_numeric],function(x)as.numeric(as.character(x)))
# 
# rating_dyads_endline$score_endline <- rowMeans(rating_dyads_endline[c("seed_quality_general_rating","seed_yield_rating",
#                                                                       "seed_drought_rating","seed_disease_rating",
#                                                                       "seed_maturing_rating","seed_germinate_rating")],na.rm = T)

#online CH treated:
rating_dyads_endline=subset(rating_dyads_endline,clearing==T)



#MID & ENDLINE
rating_dyads_midline_endline <- merge(rating_dyads_midline,rating_dyads_endline,by="farmer_ID")

# mean(rating_dyads_midline_endline$score_midline)
# #[1] 3.547651
# mean(rating_dyads_midline_endline$score_endline)
# #[1] 3.554251

rating_dyads_midline_endline$shop_ID.x <- as.character(rating_dyads_midline_endline$shop_ID.x)
rating_dyads_midline_endline$shop_ID.y <- as.character(rating_dyads_midline_endline$shop_ID.y)

sum(rating_dyads_midline_endline$shop_ID.x==rating_dyads_midline_endline$shop_ID.y)

rating_dyads_midline_endline$farmer_switched <- ifelse(rating_dyads_midline_endline$shop_ID.x==rating_dyads_midline_endline$shop_ID.y,0,1)

mean(rating_dyads_midline_endline$score_corrected_endline[rating_dyads_midline_endline$farmer_switched==1]) #[1] 3.674058
mean(rating_dyads_midline_endline$score_corrected_endline[rating_dyads_midline_endline$farmer_switched==0]) #[1] 3.61726





#Bjorn's table
rating_dyads_baseline_midline$diff_rat.mid_rat.base <- (rating_dyads_baseline_midline$score_corrected_midline-rating_dyads_baseline_midline$score_corrected_baseline)
rating_dyads_midline_endline$diff_rat.end_rat.mid <- (rating_dyads_midline_endline$score_corrected_endline-rating_dyads_midline_endline$score_corrected_midline)

a <- mean(rating_dyads_baseline_midline$diff_rat.mid_rat.base[rating_dyads_baseline_midline$farmer_switched==1]) #[1] 3.674058
b <- mean(rating_dyads_baseline_midline$diff_rat.mid_rat.base[rating_dyads_baseline_midline$farmer_switched==0]) #[1] 3.674058

c <- mean(rating_dyads_midline_endline$diff_rat.end_rat.mid[rating_dyads_midline_endline$farmer_switched==1]) #[1] 3.674058
d <- mean(rating_dyads_midline_endline$diff_rat.end_rat.mid[rating_dyads_midline_endline$farmer_switched==0]) #[1] 3.674058
