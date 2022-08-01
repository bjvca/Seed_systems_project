#run in endeline/data/farmers/raw
#number of customer reviews
rm(list=ls())

path <- getwd()
path <- strsplit(path, "/farmer/raw")[[1]]

rating_dyads <- read.csv(paste(path,"farmer/public/endline_rating_dyads.csv", sep="/"), stringsAsFactors = FALSE)

rating_dyads[c("general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating") ] <- lapply(rating_dyads[c("general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating") ], function(x) as.numeric(as.character(x)) )

reviews <- data.frame(cbind(tapply(as.numeric(rating_dyads$general_rating), rating_dyads$shop_ID,mean,na.rm=TRUE),
tapply(as.numeric(rating_dyads$location_rating), rating_dyads$shop_ID,mean,na.rm=TRUE),
tapply(as.numeric(rating_dyads$price_rating), rating_dyads$shop_ID,mean,na.rm=TRUE),
tapply(as.numeric(rating_dyads$quality_rating), rating_dyads$shop_ID,mean,na.rm=TRUE),
tapply(as.numeric(rating_dyads$stock_rating), rating_dyads$shop_ID,mean,na.rm=TRUE),
tapply(as.numeric(rating_dyads$reputation_rating), rating_dyads$shop_ID,mean,na.rm=TRUE),
tapply(rating_dyads$bought_at_dealer=="Yes" | rating_dyads$knows_other_customer=="Yes", rating_dyads$shop_ID,sum)))
names(reviews) <- c("general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","nr_reviews")

reviews$shop_ID <- rownames(reviews)

#this is for the shiny app, maybe later
path_base <- strsplit(path, "/endline/data")[[1]]

treats_shop_level <- read.csv(paste(path_base,"baseline/data/agro_input/public/treats_shop_level.csv", sep="/"), stringsAsFactors = FALSE)

reviews <- merge(reviews, treats_shop_level, by.x="shop_ID", by.y="shop_ID")
summary(lm(general_rating~clearing,data=reviews))
path <- strsplit(path, "/farmer")[[1]]
write.csv(reviews, paste(path, "agro_input/raw/shiny_app/endline_reviews_general.csv",sep="/"), row.names=FALSE)

### now specifically for seed:

rating_dyads[c("seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating") ] <- lapply(rating_dyads[c("seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating") ], function(x) as.numeric(as.character(x)) )

rating_dyads[c("seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating") ] <- lapply(rating_dyads[c("seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating") ], function(x)replace(x, x == 98,NA) )

reviews <- data.frame(cbind(tapply(as.numeric(rating_dyads$quality_rating), rating_dyads$shop_ID,mean,na.rm=TRUE),
tapply(as.numeric(rating_dyads$seed_quality_general_rating), rating_dyads$shop_ID,mean,na.rm=TRUE),
tapply(as.numeric(rating_dyads$seed_yield_rating), rating_dyads$shop_ID,mean,na.rm=TRUE),
tapply(as.numeric(rating_dyads$seed_drought_rating), rating_dyads$shop_ID,mean,na.rm=TRUE),
tapply(as.numeric(rating_dyads$seed_disease_rating), rating_dyads$shop_ID,mean,na.rm=TRUE),
tapply(as.numeric(rating_dyads$seed_maturing_rating), rating_dyads$shop_ID,mean,na.rm=TRUE),
tapply(as.numeric(rating_dyads$seed_germinate_rating), rating_dyads$shop_ID,mean,na.rm=TRUE),
tapply(rating_dyads$bought_at_dealer=="Yes" | rating_dyads$knows_other_customer=="Yes", rating_dyads$shop_ID,sum)))
names(reviews) <- c("quality_rating","general","yield","drought_resistent","disease_resistent","early_maturing","germination","nr_reviews")

reviews$shop_ID <- rownames(reviews)

##missings or ratings based on few raters
# first calcuate catchement areas mean. For that, we need to merge catchment IDs in first - get from baseline data
path_base <- strsplit(path, "/endline/data")[[1]]

treats_shop_level <- read.csv(paste(path_base,"baseline/data/agro_input/public/treats_shop_level.csv", sep="/"), stringsAsFactors = FALSE)

reviews <- merge(reviews, treats_shop_level, by.x="shop_ID", by.y="shop_ID")

summary(lm(general~clearing,data=reviews))

#get subgroup means
sg_means <- aggregate(reviews[c("quality_rating","general","yield","drought_resistent","disease_resistent","early_maturing","germination") ], list(reviews$catchID), mean, na.rm=T)
names(sg_means) <- paste(names(sg_means),"av",sep="_")
names(sg_means)[names(sg_means) == "Group.1_av"] <- "catchID"

reviews <- merge(reviews, sg_means,by.x="catchID",by.y="catchID")
##not rated: CA average
#reviews[reviews$nr_reviews==0,c("general","yield","drought_resistent","disease_resistent","early_maturing","germination")] <- reviews[reviews$nr_reviews==0,c("general_av","yield_av","drought_resistent_av","disease_resistent_av","early_maturing_av","germination_av")]

##rated by 1: 33% 1 rating, 66% CA average
#reviews[reviews$nr_reviews==1,c("general","yield","drought_resistent","disease_resistent","early_maturing","germination")] <- .3333*reviews[reviews$nr_reviews==1,c("general","yield","drought_resistent","disease_resistent","early_maturing","germination")] + .6667*reviews[reviews$nr_reviews==1,c("general_av","yield_av","drought_resistent_av","disease_resistent_av","early_maturing_av","germination_av")]
##rated by 2: 66% 2 ratings, 33% CA average
#reviews[reviews$nr_reviews==2,c("general","yield","drought_resistent","disease_resistent","early_maturing","germination")] <- .6667*reviews[reviews$nr_reviews==2,c("general","yield","drought_resistent","disease_resistent","early_maturing","germination")] + .3333*reviews[reviews$nr_reviews==2,c("general_av","yield_av","drought_resistent_av","disease_resistent_av","early_maturing_av","germination_av")]
#score
reviews$score <-  rowMeans(reviews[c("quality_rating","general","yield","drought_resistent","disease_resistent","early_maturing","germination")],na.rm=T)




#######################################################
#differences between ratings of male & female dealers?#
#######################################################

#merge with dealer baseline data to get gender variable - should this be updated to gender of agro-input dealer at midline? Keep it simple and use baseline values
baseline_dealer <- read.csv(paste(path_base,"baseline/data/agro_input/public/baseline_dealer.csv", sep="/"), stringsAsFactors = FALSE)
baseline_dealer_with_score <- merge(reviews, baseline_dealer, by=c("catchID","shop_ID"))

#create gender dummy
baseline_dealer_with_score$genderdummy <- ifelse(baseline_dealer_with_score$maize.owner.agree.gender == "Male", 1, 0)
table(baseline_dealer_with_score$genderdummy)

#control for age, education, distance of to nearest tarmac road, distance to nearest murram road
summary(baseline_dealer_with_score$maize.owner.agree.age)
baseline_dealer_with_score$prim <- FALSE
baseline_dealer_with_score$prim <- (baseline_dealer_with_score$maize.owner.agree.educ %in% c("c","d","e","f"))
summary(baseline_dealer_with_score$prim)
baseline_dealer_with_score$maize.owner.agree.q3[baseline_dealer_with_score$maize.owner.agree.q3==999] <- NA
summary(baseline_dealer_with_score$maize.owner.agree.q3)
summary(baseline_dealer_with_score$maize.owner.agree.q4)

summary(lm(score~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4, data = baseline_dealer_with_score))



#additionally control for
#Q5. Is this a specialized agro-input shop that only sells farm inputs?
table(baseline_dealer_with_score$maize.owner.agree.q5)

#Q8. When was this agro-input shop established? (year)
baseline_dealer_with_score$years_shop <- 2020 - as.numeric(as.character(substr(baseline_dealer_with_score$maize.owner.agree.q8, start=1, stop=4)))
summary(baseline_dealer_with_score$years_shop)

#Q69. Are seed stored in a dedicated area, away from other merchandize?
table(baseline_dealer_with_score$maize.owner.agree.temp.q69)

#Q71. Do you have a problem with rats or pests (insects, rats)?
table(baseline_dealer_with_score$maize.owner.agree.temp.q71)

#Q72. Is the roof leak-proof?
table(baseline_dealer_with_score$maize.owner.agree.temp.q72)

#Q73. Is the roof insulated to keep heat out?
table(baseline_dealer_with_score$maize.owner.agree.temp.q73)

#Q74. Are the walls insulated to keep the heat out?
table(baseline_dealer_with_score$maize.owner.agree.temp.q74)

#Q75. Is the area ventilated
table(baseline_dealer_with_score$maize.owner.agree.temp.q75)

#Q76. Are the walls plastered?
table(baseline_dealer_with_score$maize.owner.agree.temp.q76)

#Q77. Material of floor in areas where seed is stored?
baseline_dealer_with_score$goodfloor <- FALSE
baseline_dealer_with_score$goodfloor <- (baseline_dealer_with_score$maize.owner.agree.temp.q77 %in% c("Cement","Tiles"))
table(baseline_dealer_with_score$goodfloor)

#Q78. Lighting conditions in area where seed is stored?
baseline_dealer_with_score$badlighting <- FALSE
baseline_dealer_with_score$badlighting <- (baseline_dealer_with_score$maize.owner.agree.temp.q78 %in% c("1"))
table(baseline_dealer_with_score$badlighting)

#Q79. On what surface are seed stored?
baseline_dealer_with_score$badstored <- FALSE
baseline_dealer_with_score$badstored <- (baseline_dealer_with_score$maize.owner.agree.temp.q79 %in% c("1", "2", "96"))
table(baseline_dealer_with_score$badstored)

#Q80. Do you see maize seed that is stored in open bags or open containers?
table(baseline_dealer_with_score$maize.owner.agree.temp.q80)

#Q81. Do you see any official certificates displayed in the store (eg that the shop was inspected ,that the owner attended trainings or that the business is registered with some association)
table(baseline_dealer_with_score$maize.owner.agree.temp.q81)

#Q82. On a scale of 1 to 5, rate this shop in terms of cleanness and professionality 1 poor 5 excellent
as.numeric(as.character(baseline_dealer_with_score$maize.owner.agree.temp.q82))
summary(baseline_dealer_with_score$maize.owner.agree.temp.q82)

# #Q92. When repackaging seed, do you keep track of expiry date (eg include it in the bag/write it on the bag)
# baseline_dealer_with_score$maize.owner.agree.q92[baseline_dealer_with_score$maize.owner.agree.q92=="n/a"] <- NA
# summary(baseline_dealer_with_score$maize.owner.agree.q92)

#Q96. Since last season, did you receive any complaint from a customer that seed you sold was not good?
table(baseline_dealer_with_score$maize.owner.agree.q96)

#Q70. Entert the temperature in the seed store (where seed is stored)
table(baseline_dealer_with_score$maize.owner.agree.q70)

# #moisture
# summary(baseline_dealer_with_score$reading)

model_corr <- lm(score~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_dealer_with_score)

#we correct the score by substracting the male divident from the score

baseline_dealer_with_score$score_corrected <- baseline_dealer_with_score$score - coefficients(model_corr)["genderdummy"]*baseline_dealer_with_score$genderdummy


## do this not only for aggregate score but also for all the components - just copy paste (also redo the averages for graphs)

model_corr <- lm(quality_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_dealer_with_score)

#we correct the score by substracting the male divident from the score

baseline_dealer_with_score$quality_rating_corrected <- baseline_dealer_with_score$quality_rating - coefficients(model_corr)["genderdummy"]*baseline_dealer_with_score$genderdummy

model_corr <- lm(general~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_dealer_with_score)

#we correct the score by substracting the male divident from the score

baseline_dealer_with_score$general_corrected <- baseline_dealer_with_score$general - coefficients(model_corr)["genderdummy"]*baseline_dealer_with_score$genderdummy

model_corr <- lm(yield~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_dealer_with_score)

#we correct the score by substracting the male divident from the score

baseline_dealer_with_score$yield_corrected <- baseline_dealer_with_score$yield - coefficients(model_corr)["genderdummy"]*baseline_dealer_with_score$genderdummy

model_corr <- lm(drought_resistent~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_dealer_with_score)

#we correct the score by substracting the male divident from the score

baseline_dealer_with_score$drought_resistent_corrected <- baseline_dealer_with_score$drought_resistent - coefficients(model_corr)["genderdummy"]*baseline_dealer_with_score$genderdummy

model_corr <- lm(disease_resistent~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_dealer_with_score)

#we correct the score by substracting the male divident from the score

baseline_dealer_with_score$disease_resistent_corrected <- baseline_dealer_with_score$disease_resistent - coefficients(model_corr)["genderdummy"]*baseline_dealer_with_score$genderdummy


model_corr <- lm(early_maturing~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_dealer_with_score)

#we correct the score by substracting the male divident from the score

baseline_dealer_with_score$early_maturing_corrected <- baseline_dealer_with_score$early_maturing - coefficients(model_corr)["genderdummy"]*baseline_dealer_with_score$genderdummy



model_corr <- lm(germination~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_dealer_with_score)

#we correct the score by substracting the male divident from the score

baseline_dealer_with_score$germination_corrected <- baseline_dealer_with_score$germination - coefficients(model_corr)["genderdummy"]*baseline_dealer_with_score$genderdummy



reviews <- merge(reviews,baseline_dealer_with_score[c("shop_ID","quality_rating_corrected","general_corrected","yield_corrected","drought_resistent_corrected","disease_resistent_corrected","early_maturing_corrected","germination_corrected","score_corrected")],by="shop_ID")
#get subgroup means
sg_means <- aggregate(reviews[c("quality_rating_corrected","general_corrected","yield_corrected","drought_resistent_corrected","disease_resistent_corrected","early_maturing_corrected","germination_corrected") ], list(reviews$catchID), mean, na.rm=T)
names(sg_means) <- paste(names(sg_means),"av",sep="_")
names(sg_means)[names(sg_means) == "Group.1_av"] <- "catchID"

reviews <- merge(reviews, sg_means,by.x="catchID",by.y="catchID")

write.csv(reviews, paste(path, "agro_input/public/reviews_seed.csv",sep="/"), row.names=FALSE)
write.csv(reviews, paste(path, "agro_input/raw/shiny_app_seed/reviews_seed.csv",sep="/"), row.names=FALSE)



