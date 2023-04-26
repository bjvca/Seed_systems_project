rm(list=ls())

library(ggplot2)

path <- getwd()
path <- strsplit(path,"/papers/clearinghouse_training_paper")[[1]]

trim <- function(var,dataset,trim_perc=.02){
  dataset[var][dataset[var]<quantile(dataset[var],c(trim_perc/2,1-(trim_perc/2)),na.rm=T)[1]|dataset[var]>quantile(dataset[var],c(trim_perc/2,1-(trim_perc/2)),na.rm=T)[2]] <- NA
  return(dataset)}


baseline_dealers <- read.csv(paste(path,"/baseline/data/agro_input/public/baseline_dealer.csv",sep="/"), stringsAsFactors=TRUE)

baseline_dealers$maize.owner.agree.q7[baseline_dealers$maize.owner.agree.q7==999] <- NA
baseline_dealers <- trim("maize.owner.agree.q7",baseline_dealers,trim_perc=.02)
#baseline_dealers$maize.owner.agree.q7 <- ihs(baseline_dealers$maize.owner.agree.q7)

baseline_dealers$customers_baseline <- baseline_dealers$maize.owner.agree.q7


midline_dealers <- read.csv(paste(path,"/midline/data/agro_input/public/midline_dealer.csv",sep="/"), stringsAsFactors=TRUE)

midline_dealers$owner.agree.q7[midline_dealers$owner.agree.q7==999] <- NA #x
midline_dealers$owner.agree.q7 <- as.numeric(as.character(midline_dealers$owner.agree.q7)) #x
#midline_dealers$owner.agree.q7 <- ihs(midline_dealers$owner.agree.q7) #x
midline_dealers <- trim("owner.agree.q7",midline_dealers,trim_perc=.02) #x

midline_dealers$customers_midline <- midline_dealers$owner.agree.q7


endline_dealers <- read.csv(paste(path,"/endline/data/agro_input/public/dealer_endline.csv",sep="/"), stringsAsFactors=TRUE)

endline_dealers$check.owner.agree.q7[endline_dealers$check.owner.agree.q7==999] <- NA #x
endline_dealers$check.owner.agree.q7 <- as.numeric(as.character(endline_dealers$check.owner.agree.q7)) #x
endline_dealers <- trim("check.owner.agree.q7",endline_dealers,trim_perc=.02) #x
#endline_dealers$check.owner.agree.q7 <- ihs(endline_dealers$check.owner.agree.q7) #x

endline_dealers$customers_endline <- endline_dealers$check.owner.agree.q7


reviews_seed_baseline <- read.csv(paste(path,"/baseline/data/agro_input/public/reviews_seed.csv", sep="/"), stringsAsFactors = TRUE)
reviews_seed_baseline$rating_baseline <- reviews_seed_baseline$score_corrected


reviews_seed_midline <- read.csv(paste(path,"/midline/data/agro_input/public/reviews_seed.csv", sep="/"), stringsAsFactors = TRUE)
reviews_seed_midline$rating_midline <- reviews_seed_midline$score_corrected


reviews_seed_endline <- read.csv(paste(path,"/endline/data/agro_input/public/reviews_seed.csv", sep="/"), stringsAsFactors = TRUE)
reviews_seed_endline$rating_endline <- reviews_seed_endline$score_corrected


all <- merge(baseline_dealers[,c("shop_ID","customers_baseline","clearing")]
                               ,midline_dealers[,c("shop_ID","customers_midline")]
                               ,by="shop_ID",all.x=TRUE)

all <- merge(all[,c("shop_ID","customers_baseline","clearing","customers_midline")]
             ,endline_dealers[,c("shop_ID","customers_endline")]
             ,by="shop_ID",all.x=TRUE)

all <- merge(all[,c("shop_ID","customers_baseline","clearing","customers_midline","customers_endline")]
             ,reviews_seed_baseline[,c("shop_ID","rating_baseline")]
             ,by="shop_ID",all.x=TRUE)

all <- merge(all[,c("shop_ID","customers_baseline","clearing","customers_midline","customers_endline","rating_baseline")]
             ,reviews_seed_midline[,c("shop_ID","rating_midline")]
             ,by="shop_ID",all.x=TRUE)

all <- merge(all[,c("shop_ID","customers_baseline","clearing","customers_midline","customers_endline","rating_baseline","rating_midline")]
             ,reviews_seed_endline[,c("shop_ID","rating_endline")]
             ,by="shop_ID",all.x=TRUE)


#Bjorn's idea

ggplot(all,aes(x=rating_baseline,y=customers_baseline)) +
  geom_point() +
  geom_smooth(method='lm')
summary(lm(all$customers_baseline~all$rating_baseline)) #2.158

ggplot(all,aes(x=rating_midline,y=customers_midline)) +
  geom_point() +
  geom_smooth(method='lm')
summary(lm(all$customers_midline~all$rating_midline)) #-1.658

ggplot(all,aes(x=rating_endline,y=customers_endline)) +
  geom_point() +
  geom_smooth(method='lm')
summary(lm(all$customers_endline~all$rating_endline)) #1.745

all_CH_treated=subset(all,clearing==T)

ggplot(all_CH_treated,aes(x=rating_endline,y=customers_endline)) +
  geom_point() +
  geom_smooth(method='lm')
summary(lm(all_CH_treated$customers_endline~all_CH_treated$rating_endline)) #3.193

all_CH_control=subset(all,clearing==F)

ggplot(all_CH_control,aes(x=rating_endline,y=customers_endline)) +
  geom_point() +
  geom_smooth(method='lm')
summary(lm(all_CH_control$customers_endline~all_CH_control$rating_endline)) #-0.4889


#Caro's idea

ggplot(all,aes(x=rating_baseline,y=customers_midline)) +
  geom_point() +
  geom_smooth(method='lm')
summary(lm(all$customers_midline~all$rating_baseline)) #-6.888

ggplot(all,aes(x=rating_midline,y=customers_endline)) +
  geom_point() +
  geom_smooth(method='lm')
summary(lm(all$customers_endline~all$rating_midline)) #2.865