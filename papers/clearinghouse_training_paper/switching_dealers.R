rm(list=ls())

library(ggplot2)

path <- getwd()
path <- strsplit(path,"/papers/clearinghouse_training_paper")[[1]]

trim <- function(var,dataset,trim_perc=.02){
  dataset[var][dataset[var]<quantile(dataset[var],c(trim_perc/2,1-(trim_perc/2)),na.rm=T)[1]|dataset[var]>quantile(dataset[var],c(trim_perc/2,1-(trim_perc/2)),na.rm=T)[2]] <- NA
  return(dataset)}

########

baseline_dealers <- read.csv(paste(path,"/baseline/data/agro_input/public/baseline_dealer.csv",sep="/"), stringsAsFactors=TRUE)

baseline_dealers$maize.owner.agree.q7[baseline_dealers$maize.owner.agree.q7==999] <- NA
baseline_dealers <- trim("maize.owner.agree.q7",baseline_dealers,trim_perc=.02)
#baseline_dealers$maize.owner.agree.q7 <- ihs(baseline_dealers$maize.owner.agree.q7)

CA_mean <- aggregate(baseline_dealers$maize.owner.agree.q7,list(baseline_dealers$catchID),FUN=mean,na.rm=T)
CA_mean$catchID <- CA_mean$Group.1
CA_mean$cust_CA_mean <- CA_mean$x

CA_sd <- aggregate(baseline_dealers$maize.owner.agree.q7,list(baseline_dealers$catchID),FUN=sd,na.rm=T)
CA_sd$catchID <- CA_sd$Group.1
CA_sd$cust_CA_sd <- CA_sd$x

baseline_dealers <- merge(baseline_dealers[,c("catchID","shop_ID","maize.owner.agree.q7","clearing")]
             ,CA_mean[,c("catchID","cust_CA_mean")]
             ,by="catchID",all.x=TRUE)

baseline_dealers <- merge(baseline_dealers[,c("catchID","shop_ID","maize.owner.agree.q7","cust_CA_mean","clearing")]
                               ,CA_sd[,c("catchID","cust_CA_sd")]
                               ,by="catchID",all.x=TRUE)

baseline_dealers$cust_stand <- ((baseline_dealers$maize.owner.agree.q7
                                                       - baseline_dealers$cust_CA_mean)
                                                      / baseline_dealers$cust_CA_sd)

baseline_dealers$customers_baseline <- baseline_dealers$cust_stand

#baseline_dealers$customers_baseline <- baseline_dealers$maize.owner.agree.q7

########

midline_dealers <- read.csv(paste(path,"/midline/data/agro_input/public/midline_dealer.csv",sep="/"), stringsAsFactors=TRUE)

midline_dealers$owner.agree.q7[midline_dealers$owner.agree.q7==999] <- NA #x
midline_dealers$owner.agree.q7 <- as.numeric(as.character(midline_dealers$owner.agree.q7)) #x
#midline_dealers$owner.agree.q7 <- ihs(midline_dealers$owner.agree.q7) #x
midline_dealers <- trim("owner.agree.q7",midline_dealers,trim_perc=.02) #x

#midline dealers has no catch ID

midline_dealers <- merge(midline_dealers[,c("shop_ID","owner.agree.q7")]
                         ,baseline_dealers[,c("shop_ID","catchID")]
                         ,by="shop_ID",all.x=TRUE)

CA_mean <- aggregate(midline_dealers$owner.agree.q7,list(midline_dealers$catchID),FUN=mean,na.rm=T)
CA_mean$catchID <- CA_mean$Group.1
CA_mean$cust_CA_mean <- CA_mean$x

CA_sd <- aggregate(midline_dealers$owner.agree.q7,list(midline_dealers$catchID),FUN=sd,na.rm=T)
CA_sd$catchID <- CA_sd$Group.1
CA_sd$cust_CA_sd <- CA_sd$x

midline_dealers <- merge(midline_dealers[,c("catchID","shop_ID","owner.agree.q7")]
                          ,CA_mean[,c("catchID","cust_CA_mean")]
                          ,by="catchID",all.x=TRUE)

midline_dealers <- merge(midline_dealers[,c("catchID","shop_ID","owner.agree.q7","cust_CA_mean")]
                          ,CA_sd[,c("catchID","cust_CA_sd")]
                          ,by="catchID",all.x=TRUE)

midline_dealers$cust_stand <- ((midline_dealers$owner.agree.q7
                                 - midline_dealers$cust_CA_mean)
                                / midline_dealers$cust_CA_sd)

midline_dealers$customers_midline <- midline_dealers$cust_stand

#midline_dealers$customers_midline <- midline_dealers$owner.agree.q7

########

endline_dealers <- read.csv(paste(path,"/endline/data/agro_input/public/dealer_endline.csv",sep="/"), stringsAsFactors=TRUE)

endline_dealers$check.owner.agree.q7[endline_dealers$check.owner.agree.q7==999] <- NA #x
endline_dealers$check.owner.agree.q7 <- as.numeric(as.character(endline_dealers$check.owner.agree.q7)) #x
endline_dealers <- trim("check.owner.agree.q7",endline_dealers,trim_perc=.02) #x
#endline_dealers$check.owner.agree.q7 <- ihs(endline_dealers$check.owner.agree.q7) #x

#endline dealers has no catch ID

endline_dealers <- merge(endline_dealers[,c("shop_ID","check.owner.agree.q7")]
                         ,baseline_dealers[,c("shop_ID","catchID")]
                         ,by="shop_ID",all.x=TRUE)

CA_mean <- aggregate(endline_dealers$check.owner.agree.q7,list(endline_dealers$catchID),FUN=mean,na.rm=T)
CA_mean$catchID <- CA_mean$Group.1
CA_mean$cust_CA_mean <- CA_mean$x

CA_sd <- aggregate(endline_dealers$check.owner.agree.q7,list(endline_dealers$catchID),FUN=sd,na.rm=T)
CA_sd$catchID <- CA_sd$Group.1
CA_sd$cust_CA_sd <- CA_sd$x

endline_dealers <- merge(endline_dealers[,c("catchID","shop_ID","check.owner.agree.q7")]
                          ,CA_mean[,c("catchID","cust_CA_mean")]
                          ,by="catchID",all.x=TRUE)

endline_dealers <- merge(endline_dealers[,c("catchID","shop_ID","check.owner.agree.q7","cust_CA_mean")]
                          ,CA_sd[,c("catchID","cust_CA_sd")]
                          ,by="catchID",all.x=TRUE)

endline_dealers$cust_stand <- ((endline_dealers$check.owner.agree.q7
                                 - endline_dealers$cust_CA_mean)
                                / endline_dealers$cust_CA_sd)

endline_dealers$customers_endline <- endline_dealers$cust_stand

#endline_dealers$customers_endline <- endline_dealers$check.owner.agree.q7

########

reviews_seed_baseline <- read.csv(paste(path,"/baseline/data/agro_input/public/reviews_seed.csv", sep="/"), stringsAsFactors = TRUE)

CA_mean <- aggregate(reviews_seed_baseline$score_corrected,list(reviews_seed_baseline$catchID),FUN=mean,na.rm=T)
CA_mean$catchID <- CA_mean$Group.1
CA_mean$score_corrected_CA_mean <- CA_mean$x

CA_sd <- aggregate(reviews_seed_baseline$score_corrected,list(reviews_seed_baseline$catchID),FUN=sd,na.rm=T)
CA_sd$catchID <- CA_sd$Group.1
CA_sd$score_corrected_CA_sd <- CA_sd$x

reviews_seed_baseline <- merge(reviews_seed_baseline[,c("catchID","shop_ID","score_corrected")]
             ,CA_mean[,c("catchID","score_corrected_CA_mean")]
             ,by="catchID",all.x=TRUE)

reviews_seed_baseline <- merge(reviews_seed_baseline[,c("catchID","shop_ID","score_corrected","score_corrected_CA_mean")]
                               ,CA_sd[,c("catchID","score_corrected_CA_sd")]
                               ,by="catchID",all.x=TRUE)

reviews_seed_baseline$score_corrected_stand <- ((reviews_seed_baseline$score_corrected
                                                       - reviews_seed_baseline$score_corrected_CA_mean)
                                                      / reviews_seed_baseline$score_corrected_CA_sd)

reviews_seed_baseline$rating_baseline <- reviews_seed_baseline$score_corrected_stand

#reviews_seed_baseline$rating_baseline <- reviews_seed_baseline$score_corrected

########

reviews_seed_midline <- read.csv(paste(path,"/midline/data/agro_input/public/reviews_seed.csv", sep="/"), stringsAsFactors = TRUE)

CA_mean <- aggregate(reviews_seed_midline$score_corrected,list(reviews_seed_midline$catchID),FUN=mean,na.rm=T)
CA_mean$catchID <- CA_mean$Group.1
CA_mean$score_corrected_CA_mean <- CA_mean$x

CA_sd <- aggregate(reviews_seed_midline$score_corrected,list(reviews_seed_midline$catchID),FUN=sd,na.rm=T)
CA_sd$catchID <- CA_sd$Group.1
CA_sd$score_corrected_CA_sd <- CA_sd$x

reviews_seed_midline <- merge(reviews_seed_midline[,c("catchID","shop_ID","score_corrected")]
                               ,CA_mean[,c("catchID","score_corrected_CA_mean")]
                               ,by="catchID",all.x=TRUE)

reviews_seed_midline <- merge(reviews_seed_midline[,c("catchID","shop_ID","score_corrected","score_corrected_CA_mean")]
                               ,CA_sd[,c("catchID","score_corrected_CA_sd")]
                               ,by="catchID",all.x=TRUE)

reviews_seed_midline$score_corrected_stand <- ((reviews_seed_midline$score_corrected
                                                 - reviews_seed_midline$score_corrected_CA_mean)
                                                / reviews_seed_midline$score_corrected_CA_sd)

reviews_seed_midline$rating_midline <- reviews_seed_midline$score_corrected_stand

#reviews_seed_midline$rating_midline <- reviews_seed_midline$score_corrected

########

reviews_seed_endline <- read.csv(paste(path,"/endline/data/agro_input/public/reviews_seed.csv", sep="/"), stringsAsFactors = TRUE)

CA_mean <- aggregate(reviews_seed_endline$score_corrected,list(reviews_seed_endline$catchID),FUN=mean,na.rm=T)
CA_mean$catchID <- CA_mean$Group.1
CA_mean$score_corrected_CA_mean <- CA_mean$x

CA_sd <- aggregate(reviews_seed_endline$score_corrected,list(reviews_seed_endline$catchID),FUN=sd,na.rm=T)
CA_sd$catchID <- CA_sd$Group.1
CA_sd$score_corrected_CA_sd <- CA_sd$x

reviews_seed_endline <- merge(reviews_seed_endline[,c("catchID","shop_ID","score_corrected")]
                              ,CA_mean[,c("catchID","score_corrected_CA_mean")]
                              ,by="catchID",all.x=TRUE)

reviews_seed_endline <- merge(reviews_seed_endline[,c("catchID","shop_ID","score_corrected","score_corrected_CA_mean")]
                              ,CA_sd[,c("catchID","score_corrected_CA_sd")]
                              ,by="catchID",all.x=TRUE)

reviews_seed_endline$score_corrected_stand <- ((reviews_seed_endline$score_corrected
                                                - reviews_seed_endline$score_corrected_CA_mean)
                                               / reviews_seed_endline$score_corrected_CA_sd)

reviews_seed_endline$rating_endline <- reviews_seed_endline$score_corrected_stand

#reviews_seed_endline$rating_endline <- reviews_seed_endline$score_corrected

########

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


#Average ratings

all$average_rating <- ((all$rating_baseline+all$rating_midline+all$rating_endline)/3)

all$average_rating <- rowMeans(all[c("rating_baseline"
                                     ,"rating_midline"
                                     ,"rating_endline")],na.rm = T)

ggplot(all,aes(x=average_rating,y=customers_baseline)) +
  geom_point() +
  geom_smooth(method='lm')
summary(lm(all$customers_baseline~all$average_rating))

ggplot(all,aes(x=average_rating,y=customers_midline)) +
  geom_point() +
  geom_smooth(method='lm')
summary(lm(all$customers_midline~all$average_rating))

ggplot(all,aes(x=average_rating,y=customers_endline)) +
  geom_point() +
  geom_smooth(method='lm')
summary(lm(all$customers_endline~all$average_rating))

all_CH_treated=subset(all,clearing==T)

ggplot(all_CH_treated,aes(x=average_rating,y=customers_endline)) +
  geom_point() +
  geom_smooth(method='lm')
summary(lm(all_CH_treated$customers_endline~all_CH_treated$average_rating))

all_CH_control=subset(all,clearing==F)

ggplot(all_CH_control,aes(x=average_rating,y=customers_endline)) +
  geom_point() +
  geom_smooth(method='lm')
summary(lm(all_CH_control$customers_endline~all_CH_control$average_rating))