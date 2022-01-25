#number of customer reviews
##execute in midline/data/agro_input/raw/
rm(list=ls())
library(dplyr)
library(stringr)
library(reshape2)

path <- getwd()
path <- strsplit(path, "/raw")[[1]]

#get reviews
reviews <- read.csv( paste(path, "public/reviews_seed.csv",sep="/"), stringsAsFactors = FALSE)

#by catchment ID, rank


reviews <- data.frame(reviews %>%
    group_by(catchID) %>%
    mutate(shop_rank = order(order(score_corrected,decreasing=TRUE))))
   
#### this is the sampling frame that was used to collect baseline data
path_base <- strsplit(path, "midline/data/agro_input")[[1]]
to_upload <- read.csv( paste(path_base, "baseline/data/agro_input/raw/to_upload.csv",sep="/"), stringsAsFactors = FALSE)
ODK_imp <- read.csv( paste(path_base, "baseline/data/agro_input/raw/ODK_imp.csv",sep="/"), stringsAsFactors = FALSE)

to_upload_clearing <- to_upload 

additions <- paste("score",1:18, sep="_")
scores <- data.frame(array(NA,c(dim(to_upload)[1],length(additions),1)))
names(scores) <- additions
to_upload_clearing <- cbind(to_upload_clearing, scores)
#for earch farmer

for (farmer in to_upload$farmer_ID) {

##get names form ODK_imp are used as they are manually corrected
to_upload_clearing$name_shop_1[to_upload_clearing$farmer_ID == farmer] <-  ODK_imp$maize.owner.agree.biz_name[ODK_imp$shop_ID ==  to_upload_clearing$ID_shop_1[to_upload_clearing$farmer_ID == farmer]]
to_upload_clearing$name_shop_2[to_upload_clearing$farmer_ID == farmer] <-  ODK_imp$maize.owner.agree.biz_name[ODK_imp$shop_ID ==  to_upload_clearing$ID_shop_2[to_upload_clearing$farmer_ID == farmer]]
to_upload_clearing$name_shop_3[to_upload_clearing$farmer_ID == farmer] <-  ODK_imp$maize.owner.agree.biz_name[ODK_imp$shop_ID ==  to_upload_clearing$ID_shop_3[to_upload_clearing$farmer_ID == farmer]]
to_upload_clearing$name_shop_4[to_upload_clearing$farmer_ID == farmer] <-  ODK_imp$maize.owner.agree.biz_name[ODK_imp$shop_ID ==  to_upload_clearing$ID_shop_4[to_upload_clearing$farmer_ID == farmer]]
to_upload_clearing$name_shop_5[to_upload_clearing$farmer_ID == farmer] <-  ODK_imp$maize.owner.agree.biz_name[ODK_imp$shop_ID ==  to_upload_clearing$ID_shop_5[to_upload_clearing$farmer_ID == farmer]]
to_upload_clearing$name_shop_6[to_upload_clearing$farmer_ID == farmer] <-  ODK_imp$maize.owner.agree.biz_name[ODK_imp$shop_ID ==  to_upload_clearing$ID_shop_6[to_upload_clearing$farmer_ID == farmer]]
to_upload_clearing$name_shop_7[to_upload_clearing$farmer_ID == farmer] <-  ODK_imp$maize.owner.agree.biz_name[ODK_imp$shop_ID ==  to_upload_clearing$ID_shop_7[to_upload_clearing$farmer_ID == farmer]]
to_upload_clearing$name_shop_8[to_upload_clearing$farmer_ID == farmer] <-  ODK_imp$maize.owner.agree.biz_name[ODK_imp$shop_ID ==  to_upload_clearing$ID_shop_8[to_upload_clearing$farmer_ID == farmer]]
to_upload_clearing$name_shop_9[to_upload_clearing$farmer_ID == farmer] <-  ODK_imp$maize.owner.agree.biz_name[ODK_imp$shop_ID ==  to_upload_clearing$ID_shop_9[to_upload_clearing$farmer_ID == farmer]]
to_upload_clearing$name_shop_10[to_upload_clearing$farmer_ID == farmer] <-  ODK_imp$maize.owner.agree.biz_name[ODK_imp$shop_ID ==  to_upload_clearing$ID_shop_10[to_upload_clearing$farmer_ID == farmer]]
to_upload_clearing$name_shop_11[to_upload_clearing$farmer_ID == farmer] <-  ODK_imp$maize.owner.agree.biz_name[ODK_imp$shop_ID ==  to_upload_clearing$ID_shop_11[to_upload_clearing$farmer_ID == farmer]]
to_upload_clearing$name_shop_12[to_upload_clearing$farmer_ID == farmer] <-  ODK_imp$maize.owner.agree.biz_name[ODK_imp$shop_ID ==  to_upload_clearing$ID_shop_12[to_upload_clearing$farmer_ID == farmer]]
to_upload_clearing$name_shop_13[to_upload_clearing$farmer_ID == farmer] <-  ODK_imp$maize.owner.agree.biz_name[ODK_imp$shop_ID ==  to_upload_clearing$ID_shop_13[to_upload_clearing$farmer_ID == farmer]]
to_upload_clearing$name_shop_14[to_upload_clearing$farmer_ID == farmer] <-  ODK_imp$maize.owner.agree.biz_name[ODK_imp$shop_ID ==  to_upload_clearing$ID_shop_14[to_upload_clearing$farmer_ID == farmer]]
to_upload_clearing$name_shop_15[to_upload_clearing$farmer_ID == farmer] <-  ODK_imp$maize.owner.agree.biz_name[ODK_imp$shop_ID ==  to_upload_clearing$ID_shop_15[to_upload_clearing$farmer_ID == farmer]]
to_upload_clearing$name_shop_16[to_upload_clearing$farmer_ID == farmer] <-  ODK_imp$maize.owner.agree.biz_name[ODK_imp$shop_ID ==  to_upload_clearing$ID_shop_16[to_upload_clearing$farmer_ID == farmer]]
to_upload_clearing$name_shop_17[to_upload_clearing$farmer_ID == farmer] <-  ODK_imp$maize.owner.agree.biz_name[ODK_imp$shop_ID ==  to_upload_clearing$ID_shop_17[to_upload_clearing$farmer_ID == farmer]]
to_upload_clearing$name_shop_18[to_upload_clearing$farmer_ID == farmer] <-  ODK_imp$maize.owner.agree.biz_name[ODK_imp$shop_ID ==  to_upload_clearing$ID_shop_18[to_upload_clearing$farmer_ID == farmer]]

#look upcatchment area
catch_revs <- reviews[reviews$catchID  == to_upload$catchID[to_upload$farmer_ID == farmer],]
if (dim(catch_revs)[1] >=1) {
for (i in c(1:dim(catch_revs)[1]) ) {
print(i)
#shop_IDs
to_upload_clearing[to_upload_clearing$farmer_ID == farmer, i+10] <-  catch_revs$shop_ID[catch_revs$shop_rank == i]
#pictures
to_upload_clearing[to_upload_clearing$farmer_ID == farmer, i+28] <-  ODK_imp$maize.owner.agree.q13[ODK_imp$shop_ID ==  catch_revs$shop_ID[catch_revs$shop_rank == i]]
#name_shop
to_upload_clearing[to_upload_clearing$farmer_ID == farmer, i+46] <-  ODK_imp$maize.owner.agree.biz_name[ODK_imp$shop_ID ==  catch_revs$shop_ID[catch_revs$shop_rank == i]]
#owner_name_shop_1
to_upload_clearing[to_upload_clearing$farmer_ID == farmer, i+64] <-  ODK_imp$maize.owner.agree.family_name[ODK_imp$shop_ID ==  catch_revs$shop_ID[catch_revs$shop_rank == i]]
#name_person_interviewed
to_upload_clearing[to_upload_clearing$farmer_ID == farmer, i+82] <-  ODK_imp$maize.owner.agree.dealer_name[ODK_imp$shop_ID ==  catch_revs$shop_ID[catch_revs$shop_rank == i]]
#nickname_person_interviewed_1
to_upload_clearing[to_upload_clearing$farmer_ID == farmer, i+100] <-  ODK_imp$maize.owner.agree.nickname[ODK_imp$shop_ID ==  catch_revs$shop_ID[catch_revs$shop_rank == i]]
#location_shop_1
to_upload_clearing[to_upload_clearing$farmer_ID == farmer, i+118] <-  ODK_imp$maize.owner.agree.market_name[ODK_imp$shop_ID ==  catch_revs$shop_ID[catch_revs$shop_rank == i]]
#escription_shop
to_upload_clearing[to_upload_clearing$farmer_ID == farmer, i+136] <-  ODK_imp$maize.owner.agree.eye[ODK_imp$shop_ID ==  catch_revs$shop_ID[catch_revs$shop_rank == i]]
#scores
to_upload_clearing[to_upload_clearing$farmer_ID == farmer, i+154] <-  catch_revs$score_corrected[catch_revs$shop_rank == i]
}
}
}



test2 <- to_upload_clearing
test2 <- test2[ c("farmer_ID",paste("ID_shop",seq(1:18), sep="_"))]
test2 <- data.frame(lapply( test2, function(x)  trimws(x, which = "both")))
test2 <- data.frame(lapply( test2,  function(x) str_replace_all(x, "[\r\n]" , " ")))
test2 <- data.frame(lapply( test2,  function(x) str_replace_all(x, "\\s+" , " ")))
#data needs to be in long form

test2 <- melt(test2, id.vars = c("farmer_ID"))
test2$variable <- NULL
names(test2) <- c("farmer_ID","shop_ID")
test2 <- subset(test2, !is.na(shop_ID))
test2$shop_name <- paste("shop",1:nrow(test2), sep = "_")

write.csv(test2,file="matcher_file_ranked.csv", row.names=FALSE)

ODK_imp <- merge(ODK_imp,reviews[c("shop_ID","score_corrected")],by.x="shop_ID", by.y="shop_ID", all.x=TRUE)
names(ODK_imp) <- c("shop_ID","catchID","biz_name","pic","maize.owner.agree.family_name","dealer_name","nickname","market_name","eye","score")
ODK_imp$scorex <-  NA
ODK_imp$scorex <- ifelse(ODK_imp$score > 3.468970, "excellent!", ifelse(ODK_imp$score > 3.287588, "very good!", ifelse(ODK_imp$score > 3.116589, "good!", "okay!")))
ODK_imp$scorex [is.na(ODK_imp$scorex)] <-  "not rated"

ODK_imp$rank <- rank(ODK_imp$score)
ODK_imp$rank[is.na(ODK_imp$score)] <- NA
ODK_imp$stars <- round(min(ODK_imp$score, na.rm=T)+ ODK_imp$rank*(max(ODK_imp$score, na.rm=T)-min(ODK_imp$score, na.rm=T))/max(ODK_imp$rank, na.rm=T),digits = 1)
ODK_imp$stars_pic <- ifelse(ODK_imp$stars > 4.75, "star_5.png", ifelse(ODK_imp$stars > 4.25, "stars_45.png", ifelse(ODK_imp$stars > 3.75, "stars_4.png", ifelse(ODK_imp$stars > 3.25, "stars_35.png", ifelse(ODK_imp$stars > 2.75, "stars_3.png", ifelse(ODK_imp$stars > 2.25, "stars_25.png", "stars_2.png"))))))
ODK_imp$stars_pic[is.na(ODK_imp$stars)] <-  "not_rated.png"
ODK_imp$rank <- NULL

write.csv(ODK_imp,file="ODK_imp_rated.csv", row.names=FALSE)

quantile(ODK_imp$score,c(.2,.4,.6,.8), na.rm=T)
#     20%      40%      60%      80% 
#3.116589 3.287588 3.468970 3.649582 
### to_upload_clearing should also have identifying information for the farmers extracted from baseline data collection

path <- strsplit(path, "/midline/data/agro_input")[[1]]

farmer_raw <- read.csv( paste(path, "baseline/data/farmer/raw/baseline_farmers_all_raw.csv",sep="/"), stringsAsFactors = FALSE)

to_upload_clearing <-merge(to_upload_clearing,farmer_raw[c("farmer_ID","enumerator","Check2.check.maize.q5","Check2.check.maize.phone","Check2.check.maize.phone2","Check2.check.maize._gps_latitude","Check2.check.maize._gps_longitude" )],by="farmer_ID")
names(to_upload_clearing)[names(to_upload_clearing) %in% c("Check2.check.maize.q5","Check2.check.maize.phone","Check2.check.maize.phone2","Check2.check.maize._gps_latitude","Check2.check.maize._gps_longitude" )] <- c("farmer_name","phone1","phone2","gps_latitude","gps_longitude" )
to_upload_clearing$shops <- to_upload_clearing$nr_shops_in_catch

write.csv(to_upload_clearing,file= paste(path,"midline/data/farmer/raw/to_upload_clearing.csv",sep="/"), row.names=FALSE)
