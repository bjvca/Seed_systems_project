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

baseline_dealers$customers_baseline_save <- baseline_dealers$maize.owner.agree.q7
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

midline_dealers$customers_midline_save <- midline_dealers$owner.agree.q7

midline_dealers$customers_midline <- midline_dealers$cust_stand

#midline_dealers$customers_midline <- midline_dealers$owner.agree.q7

########

endline_dealers <- read.csv(paste(path,"/endline/data/agro_input/public/dealer_endline.csv",sep="/"), stringsAsFactors=TRUE)

endline_dealers$check.owner.agree.q7[endline_dealers$check.owner.agree.q7==999] <- NA #x
endline_dealers$check.owner.agree.q7 <- as.numeric(as.character(endline_dealers$check.owner.agree.q7)) #x
endline_dealers <- trim("check.owner.agree.q7",endline_dealers,trim_perc=.02) #x
#endline_dealers$check.owner.agree.q7 <- ihs(endline_dealers$check.owner.agree.q7) #x
#endline_dealers$check.owner.agree.q7[endline_dealers$check.owner.agree.q7>40] <- NA #x #here1


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

endline_dealers$customers_endline_save <- endline_dealers$check.owner.agree.q7
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

all <- merge(baseline_dealers[,c("shop_ID","customers_baseline","clearing","customers_baseline_save")]
                               ,midline_dealers[,c("shop_ID","customers_midline","customers_midline_save")]
                               ,by="shop_ID",all.x=TRUE)

all <- merge(all[,c("shop_ID","customers_baseline","clearing","customers_midline","customers_baseline_save","customers_midline_save")]
             ,endline_dealers[,c("shop_ID","customers_endline","customers_endline_save")]
             ,by="shop_ID",all.x=TRUE)

all <- merge(all[,c("shop_ID","customers_baseline","clearing","customers_midline","customers_endline","customers_baseline_save","customers_endline_save","customers_midline_save")]
             ,reviews_seed_baseline[,c("shop_ID","rating_baseline")]
             ,by="shop_ID",all.x=TRUE)

all <- merge(all[,c("shop_ID","customers_baseline","clearing","customers_midline","customers_endline","rating_baseline","customers_baseline_save","customers_endline_save","customers_midline_save")]
             ,reviews_seed_midline[,c("shop_ID","rating_midline")]
             ,by="shop_ID",all.x=TRUE)

all <- merge(all[,c("shop_ID","customers_baseline","clearing","customers_midline","customers_endline","rating_baseline","rating_midline","customers_baseline_save","customers_endline_save","customers_midline_save")]
             ,reviews_seed_endline[,c("shop_ID","rating_endline")]
             ,by="shop_ID",all.x=TRUE)


#Bjorn's idea

ggplot(all,aes(x=rating_baseline,y=customers_baseline)) +
  geom_point() +
  geom_smooth(method='lm')
summary(lm(all$customers_baseline~all$rating_baseline))

ggplot(all,aes(x=rating_midline,y=customers_midline)) +
  geom_point() +
  geom_smooth(method='lm')
summary(lm(all$customers_midline~all$rating_midline))

ggplot(all,aes(x=rating_endline,y=customers_endline)) +
  geom_point() +
  geom_smooth(method='lm')
summary(lm(all$customers_endline~all$rating_endline))

all_CH_treated=subset(all,clearing==T)

ggplot(all_CH_treated,aes(x=rating_endline,y=customers_endline)) +
  geom_point() +
  geom_smooth(method='lm')
summary(lm(all_CH_treated$customers_endline~all_CH_treated$rating_endline))

#here2

all_CH_control=subset(all,clearing==F)

ggplot(all_CH_control,aes(x=rating_endline,y=customers_endline)) +
  geom_point() +
  geom_smooth(method='lm')
summary(lm(all_CH_control$customers_endline~all_CH_control$rating_endline))


#Caro's idea

#THIS PLOT IS IN PAPER
ggplot(all,aes(x=rating_baseline,y=customers_midline)) +
  geom_point() +
  xlab("Standardized shop rating at baseline") + ylab("Standardized number of maize seed customers at midline") +
  geom_smooth(method='lm')
summary(lm(all$customers_midline~all$rating_baseline))

ggplot(all,aes(x=rating_baseline,y=customers_endline)) +
  geom_point() +
  geom_smooth(method='lm')
summary(lm(all$customers_endline~all$rating_baseline))

#THIS PLOT IS IN PAPER
ggplot(all,aes(x=rating_midline,y=customers_endline)) +
  geom_point() +
  xlab("Standardized shop rating at midline") + ylab("Standardized number of maize seed customers at endline") +
  geom_smooth(method='lm')
summary(lm(all$customers_endline~all$rating_midline))


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

#Robert's comment

all$cust_end_min_base <- (all$customers_endline-all$customers_baseline)

ggplot(all,aes(x=rating_midline,y=cust_end_min_base)) +
  geom_point() +
  xlab("Standardized shop rating at midline") + ylab("Difference between # customers at base- and endline (standardized)") +
  geom_smooth(method='lm')
summary(lm(all$cust_end_min_base~all$rating_midline))

all$cust_end_min_base_notstand <- (all$customers_endline_save-all$customers_baseline_save)

ggplot(all,aes(x=rating_midline,y=cust_end_min_base_notstand)) +
  geom_point() +
  xlab("Standardized shop rating at midline") + ylab("Difference between # customers at base- and endline (not standardized)") +
  geom_smooth(method='lm')
summary(lm(all$cust_end_min_base_notstand~all$rating_midline))

###Bjorn's improvement

all$cust_mid_min_base <- (all$customers_midline-all$customers_baseline)

ggplot(all,aes(x=rating_baseline,y=cust_mid_min_base)) +
  geom_point() +
  xlab("Standardized shop rating at baseline") + ylab("Difference between number of customers base- and midline (standardized)") +
  geom_smooth(method='lm')
summary(lm(all$cust_mid_min_base~all$rating_baseline))

all$cust_mid_min_base_notstand <- (all$customers_midline_save-all$customers_baseline_save)

ggplot(all,aes(x=rating_baseline,y=cust_mid_min_base_notstand)) +
  geom_point() +
  xlab("Standardized shop rating at baseline") + ylab("Difference between number of customers base- and midline (not standardized)") +
  geom_smooth(method='lm')
summary(lm(all$cust_mid_min_base_notstand~all$rating_baseline))

all$cust_end_min_mid <- (all$customers_endline-all$customers_midline)
all$cust_end_min_mid_notstand <- (all$customers_endline_save-all$customers_midline_save)

#all$cust_end_min_mid <- all$cust_end_min_mid_notstand
#all$cust_mid_min_base <- all$cust_mid_min_base_notstand

#stacked
part1 <- all[,c("rating_baseline","cust_mid_min_base")]
part1$rating_tmin1 <- part1$rating_baseline
part1$diff_cust_t_and_tmin1 <- part1$cust_mid_min_base
part1 <- part1[,c("rating_tmin1","diff_cust_t_and_tmin1")]

part2 <- all[,c("rating_midline","cust_end_min_mid")]
part2$rating_tmin1 <- part2$rating_midline
part2$diff_cust_t_and_tmin1 <- part2$cust_end_min_mid
part2 <- part2[,c("rating_tmin1","diff_cust_t_and_tmin1")]

stacked <- rbind(part1,part2)

ggplot(stacked,aes(x=rating_tmin1,y=diff_cust_t_and_tmin1)) +
  geom_point() +
  xlab("Standardized shop rating at t-1") + ylab("Difference between number of customers t and t-1 (standardized)") +
  geom_smooth(method='lm')
summary(lm(stacked$diff_cust_t_and_tmin1~stacked$rating_tmin1))

sum(stacked$rating_tmin1>=-0.70711 & stacked$rating_tmin1<=-0.70710 & !is.na(stacked$diff_cust_t_and_tmin1),na.rm = T) #24 if stand. 31 if not stand
sum(stacked$rating_tmin1>=0.70710 & stacked$rating_tmin1<=0.70711 & !is.na(stacked$diff_cust_t_and_tmin1),na.rm = T) #23 if stand. 35 if not stand

#NOTSTAND
table(stacked$diff_cust_t_and_tmin1[stacked$rating_tmin1>=-0.70711 & stacked$rating_tmin1<=-0.70710])

#-78 -50 -15 -10  -7  -4  -1   0   1   4   5   7  15  25  32  50  70  95 
#1   1   2   3   1   2   3   5   2   1   3   1   1   1   1   1   1   1  #31

#STAND
table(stacked$diff_cust_t_and_tmin1[stacked$rating_tmin1>=-0.70711 & stacked$rating_tmin1<=-0.70710])

#-1.414213    -0.6816843 -1.110223e-16        0   1.110223e-16      1.414213 
#3            1          2                    11  3                 4  #24

