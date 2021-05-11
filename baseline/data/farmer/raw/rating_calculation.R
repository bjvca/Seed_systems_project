#number of customer reviews
rm(list=ls())

path <- getwd()
path <- strsplit(path, "/farmer/raw")[[1]]

rating_dyads <- read.csv(paste(path,"farmer/public/rating_dyads.csv", sep="/"), stringsAsFactors = FALSE)


reviews <- data.frame(cbind(tapply(as.numeric(rating_dyads$general_rating), rating_dyads$shop_ID,mean,na.rm=TRUE),
tapply(as.numeric(rating_dyads$location_rating), rating_dyads$shop_ID,mean,na.rm=TRUE),
tapply(as.numeric(rating_dyads$price_rating), rating_dyads$shop_ID,mean,na.rm=TRUE),
tapply(as.numeric(rating_dyads$quality_rating), rating_dyads$shop_ID,mean,na.rm=TRUE),
tapply(as.numeric(rating_dyads$stock_rating), rating_dyads$shop_ID,mean,na.rm=TRUE),
tapply(as.numeric(rating_dyads$reputation_rating), rating_dyads$shop_ID,mean,na.rm=TRUE),
tapply(rating_dyads$bought_at_dealer=="Yes" | rating_dyads$knows_other_customer=="Yes", rating_dyads$shop_ID,sum)))
names(reviews) <- c("general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","nr_reviews")

reviews$shop_ID <- rownames(reviews)

path <- strsplit(path, "/farmer")[[1]]
write.csv(reviews, paste(path, "agro_input/raw/shiny_app/reviews_general.csv",sep="/"), row.names=FALSE)

### now specifically for seed:

rating_dyads[c("seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating") ] <- lapply(rating_dyads[c("seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating") ], function(x) as.numeric(as.character(x)) )

rating_dyads[c("seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating") ] <- lapply(rating_dyads[c("seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating") ], function(x)replace(x, x == 98,NA) )


reviews <- data.frame(cbind(tapply(as.numeric(rating_dyads$seed_quality_general_rating), rating_dyads$shop_ID,mean,na.rm=TRUE),
tapply(as.numeric(rating_dyads$seed_yield_rating), rating_dyads$shop_ID,mean,na.rm=TRUE),
tapply(as.numeric(rating_dyads$seed_drought_rating), rating_dyads$shop_ID,mean,na.rm=TRUE),
tapply(as.numeric(rating_dyads$seed_disease_rating), rating_dyads$shop_ID,mean,na.rm=TRUE),
tapply(as.numeric(rating_dyads$seed_maturing_rating), rating_dyads$shop_ID,mean,na.rm=TRUE),
tapply(as.numeric(rating_dyads$seed_germinate_rating), rating_dyads$shop_ID,mean,na.rm=TRUE),
tapply(rating_dyads$bought_at_dealer=="Yes" | rating_dyads$knows_other_customer=="Yes", rating_dyads$shop_ID,sum)))
names(reviews) <- c("general","yield","drought_resistent","disease_resistent","early_maturing","germination","nr_reviews")

reviews$shop_ID <- rownames(reviews)

##missings or ratings based on few raters
# first calcuate catchement areas mean. For that, we need to merge catchment IDs in first
treats_shop_level <- read.csv(paste(path,"agro_input/public/treats_shop_level.csv", sep="/"), stringsAsFactors = FALSE)

reviews <- merge(reviews, treats_shop_level[c("shop_ID","catchID")], by.x="shop_ID", by.y="shop_ID")

#get subgroup means
sg_means <- aggregate(reviews[c("general","yield","drought_resistent","disease_resistent","early_maturing","germination") ], list(reviews$catchID), mean, na.rm=T)
names(sg_means) <- paste(names(sg_means),"av",sep="_")
names(sg_means)[names(sg_means) == "Group.1_av"] <- "catchID"

reviews <- merge(reviews, sg_means,by.x="catchID",by.y="catchID")
#not rated: CA average
reviews[reviews$nr_reviews==0,c("general","yield","drought_resistent","disease_resistent","early_maturing","germination")] <- reviews[reviews$nr_reviews==0,c("general_av","yield_av","drought_resistent_av","disease_resistent_av","early_maturing_av","germination_av")]

#rated by 1: 33% 1 rating, 66% CA average
reviews[reviews$nr_reviews==1,c("general","yield","drought_resistent","disease_resistent","early_maturing","germination")] <- .3333*reviews[reviews$nr_reviews==1,c("general","yield","drought_resistent","disease_resistent","early_maturing","germination")] + .6667*reviews[reviews$nr_reviews==1,c("general_av","yield_av","drought_resistent_av","disease_resistent_av","early_maturing_av","germination_av")]
#rated by 2: 66% 2 ratings, 33% CA average
reviews[reviews$nr_reviews==2,c("general","yield","drought_resistent","disease_resistent","early_maturing","germination")] <- .6667*reviews[reviews$nr_reviews==2,c("general","yield","drought_resistent","disease_resistent","early_maturing","germination")] + .3333*reviews[reviews$nr_reviews==2,c("general_av","yield_av","drought_resistent_av","disease_resistent_av","early_maturing_av","germination_av")]

path <- strsplit(path, "/farmer")[[1]]
write.csv(reviews, paste(path, "agro_input/raw/shiny_app/reviews_seed.csv",sep="/"), row.names=FALSE)


