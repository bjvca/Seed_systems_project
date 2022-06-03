#dealer rankings by farmers

rm(list=ls())

path <- getwd()
path <- strsplit(path, "/registered_report")[[1]]
rating_dyads <- read.csv(paste(path,"data/farmer/public/rating_dyads.csv", sep="/"), stringsAsFactors = TRUE)

#Q64. Do you know **${calc_biz}**  or ${dealer_name} sometimes called
#${nickname} located in ${market_name} market. The place can be 
#described as: ${eye}

table(rating_dyads$knows_dealer)
7660/(7660+4343)


