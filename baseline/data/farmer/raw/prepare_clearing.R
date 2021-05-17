#number of customer reviews
##execute in agro_input/raw/
rm(list=ls())
library(dplyr)

path <- getwd()

#get reviews
reviews <- read.csv( paste(path, "shiny_app_seed/reviews_seed.csv",sep="/"), stringsAsFactors = FALSE)

#by catchment ID, rank


reviews <- data.frame(reviews %>%
    group_by(catchID) %>%
    mutate(shop_rank = order(order(score,decreasing=TRUE))))
    



to_upload <- read.csv( paste(path, "to_upload.csv",sep="/"), stringsAsFactors = FALSE)

to_upload_new <- to_upload 
#for earch farmer

for (farmer in to_upload$farmer_ID) {
#look upcatchment area
catch_revs <- reviews[reviews$catchID  == to_upload$catchID[to_upload$farmer_ID == farmer],]
if (dim(catch_revs)[1] >=1) {
for (i in c(1:dim(catch_revs)[1]) ) {
print(i)
to_upload_new[to_upload_new$farmer_ID == farmer, i+10] <-  catch_revs$shop_ID[catch_revs$shop_rank == i]
to_upload_new[to_upload_new$farmer_ID == farmer, i+28] <-  catch_revs$score[catch_revs$shop_rank == i]
}
}
}

to_upload_new[to_upload_new$catchID==3,]
