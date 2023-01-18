

baseline_dealers$mid_maize.owner.agree.long10h.q25 <- ihs(baseline_dealers$mid_maize.owner.agree.long10h.q25) #x
baseline_dealers <- trim("mid_maize.owner.agree.long10h.q25",baseline_dealers,trim_perc=.02) #x

#6. Sales price per kilogram of ${q25} at the beginning of the second season of 2020 (q26)
baseline_dealers$maize.owner.agree.long10h.q26 <- ihs(baseline_dealers$maize.owner.agree.long10h.q26)
baseline_dealers <- trim("maize.owner.agree.long10h.q26",baseline_dealers,trim_perc=.02)

baseline_dealers$mid_maize.owner.agree.long10h.q26 <- ihs(baseline_dealers$mid_maize.owner.agree.long10h.q26) #x
baseline_dealers <- trim("mid_maize.owner.agree.long10h.q26",baseline_dealers,trim_perc=.02) #x

#7. (h) How much of Longe10H was lost/wasted the second season of 2020 (kg) (q27)
baseline_dealers$maize.owner.agree.long10h.q27[baseline_dealers$maize.owner.agree.long10h.q27=="n/a"] <- 0
baseline_dealers$maize.owner.agree.long10h.q27 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.long10h.q27))
baseline_dealers$maize.owner.agree.long10h.q27[baseline_dealers$maize.owner.agree.q20=="0"] <- 0
baseline_dealers$maize.owner.agree.long10h.q27 <- ihs(baseline_dealers$maize.owner.agree.long10h.q27)
baseline_dealers <- trim("maize.owner.agree.long10h.q27",baseline_dealers,trim_perc=.02)

baseline_dealers$mid_maize.owner.agree.long10h.q27 <- baseline_dealers$owner.agree.long10h.q27 #x
baseline_dealers$mid_maize.owner.agree.long10h.q27[baseline_dealers$mid_maize.owner.agree.long10h.q27=="n/a"] <- 0 #x
baseline_dealers$mid_maize.owner.agree.long10h.q27 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.long10h.q27)) #x
baseline_dealers$mid_maize.owner.agree.long10h.q27[baseline_dealers$mid_maize.owner.agree.q20=="0"] <- 0 #x
baseline_dealers$mid_maize.owner.agree.long10h.q27 <- ihs(baseline_dealers$mid_maize.owner.agree.long10h.q27) #x
baseline_dealers <- trim("mid_maize.owner.agree.long10h.q27",baseline_dealers,trim_perc=.02) #x

#8. Did you ever run out of Longe10H during the second season of 2020? (q29)
baseline_dealers$maize.owner.agree.long10h.q29[baseline_dealers$maize.owner.agree.long10h.q29=="n/a"] <- NA
baseline_dealers$maize.owner.agree.long10h.q29 <- ifelse(baseline_dealers$maize.owner.agree.long10h.q29=="Yes",1,0)

baseline_dealers$mid_maize.owner.agree.long10h.q29 <- baseline_dealers$owner.agree.long10h.q29 #x
baseline_dealers$mid_maize.owner.agree.long10h.q29[baseline_dealers$mid_maize.owner.agree.long10h.q29=="n/a"] <- NA #x
baseline_dealers$mid_maize.owner.agree.long10h.q29 <- ifelse(baseline_dealers$mid_maize.owner.agree.long10h.q29=="Yes",1,0) #x

#9. Estimate how often you ran out of stock for Longe10 H during the second season of 2020 (q30)
baseline_dealers$maize.owner.agree.long10h.q30 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.long10h.q30))
baseline_dealers$maize.owner.agree.long10h.q30[baseline_dealers$maize.owner.agree.long10h.q29=="0"] <- 0
baseline_dealers$maize.owner.agree.long10h.q30[baseline_dealers$maize.owner.agree.long10h.q30=="1"] <- 21.74 #Everyday: 21.74 working days per month
baseline_dealers$maize.owner.agree.long10h.q30[baseline_dealers$maize.owner.agree.long10h.q30=="2"] <- 4.34524 #once a week: 4,34524 weeks in a month
baseline_dealers$maize.owner.agree.long10h.q30[baseline_dealers$maize.owner.agree.long10h.q30=="3"] <- 1 #Once a month
baseline_dealers$maize.owner.agree.long10h.q30[baseline_dealers$maize.owner.agree.long10h.q30=="4"] <- 1/3 #once in a season
baseline_dealers$maize.owner.agree.long10h.q30[baseline_dealers$maize.owner.agree.long10h.q30=="5"] <- 0 #Never
baseline_dealers$maize.owner.agree.long10h.q30[baseline_dealers$maize.owner.agree.long10h.q30=="6"] <- NA #Never
baseline_dealers <- trim("maize.owner.agree.long10h.q30",baseline_dealers,trim_perc=.02)

baseline_dealers$mid_maize.owner.agree.long10h.q30 <- baseline_dealers$owner.agree.long10h.q30 #x
baseline_dealers$mid_maize.owner.agree.long10h.q30 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.long10h.q30))
baseline_dealers$mid_maize.owner.agree.long10h.q30[baseline_dealers$mid_maize.owner.agree.long10h.q29=="0"] <- 0
baseline_dealers$mid_maize.owner.agree.long10h.q30[baseline_dealers$mid_maize.owner.agree.long10h.q30=="1"] <- 21.74 #Everyday: 21.74 working days per month
baseline_dealers$mid_maize.owner.agree.long10h.q30[baseline_dealers$mid_maize.owner.agree.long10h.q30=="2"] <- 4.34524 #once a week: 4,34524 weeks in a month
baseline_dealers$mid_maize.owner.agree.long10h.q30[baseline_dealers$mid_maize.owner.agree.long10h.q30=="3"] <- 1 #Once a month
baseline_dealers$mid_maize.owner.agree.long10h.q30[baseline_dealers$mid_maize.owner.agree.long10h.q30=="4"] <- 1/3 #once in a season
baseline_dealers$mid_maize.owner.agree.long10h.q30[baseline_dealers$mid_maize.owner.agree.long10h.q30=="5"] <- 0 #Never
baseline_dealers$mid_maize.owner.agree.long10h.q30[baseline_dealers$mid_maize.owner.agree.long10h.q30=="6"] <- NA #Never
baseline_dealers <- trim("mid_maize.owner.agree.long10h.q30",baseline_dealers,trim_perc=.02)

#11. Overall index of secondary Longe10H agro-input dealer outcome variables
variables_overall_Longe10H_mid <- cbind(baseline_dealers$mid_maize.owner.agree.long10h.q21,baseline_dealers$mid_maize.owner.agree.long10h.q22
                                        ,baseline_dealers$mid_maize.owner.agree.long10h.q25) #x
variables_overall_Longe10H_base <- cbind(baseline_dealers$maize.owner.agree.long10h.q21,baseline_dealers$maize.owner.agree.long10h.q22
                                         ,baseline_dealers$maize.owner.agree.long10h.q25)

index_overall_Longe10H_mid <- icwIndex(xmat=variables_overall_Longe10H_mid, revcols = c(1)) #x
baseline_dealers$index_overall_Longe10H_mid <- index_overall_Longe10H_mid$index #x

index_overall_Longe10H_base <- icwIndex(xmat=variables_overall_Longe10H_base, revcols = c(1))
baseline_dealers$index_overall_Longe10H_base <- index_overall_Longe10H_base$index #baseline index

################################################################################################################################################################################

###
#1#
###

results_dealer_secL10H <- c("mid_maize.owner.agree.long10h.q21","mid_maize.owner.agree.long10h.q22","mid_maize.owner.agree.long10h.q24"
                            ,"mid_maize.owner.agree.long10h.q25","mid_maize.owner.agree.long10h.q26","mid_maize.owner.agree.long10h.q27"
                            ,"mid_maize.owner.agree.long10h.q30","index_overall_Longe10H_mid")

results_dealer_secL10H_base <- c("maize.owner.agree.long10h.q21","maize.owner.agree.long10h.q22","maize.owner.agree.long10h.q24"
                                 ,"maize.owner.agree.long10h.q25","maize.owner.agree.long10h.q26","maize.owner.agree.long10h.q27"
                                 ,"maize.owner.agree.long10h.q30","index_overall_Longe10H_base")

baseline_dealers[results_dealer_secL10H_base] <- lapply(baseline_dealers[results_dealer_secL10H_base],function(x)x - mean(x,na.rm = T))

df_means_D_secL10H <- array(NA,dim=c(3,11))

for (i in 1:length(results_dealer_secL10H)){
  df_means_D_secL10H[1,i] <- sum(baseline_dealers[results_dealer_secL10H[i]], na.rm=T)/(nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_secL10H[i]])))
  df_means_D_secL10H[2,i] <- sqrt(var(baseline_dealers[results_dealer_secL10H[i]], na.rm=T))
  df_means_D_secL10H[3,i] <- nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_secL10H[i]]))-sum(is.na(baseline_dealers[results_dealer_secL10H_base[i]]))+sum(is.na(baseline_dealers[results_dealer_secL10H[i]])&is.na(baseline_dealers[results_dealer_secL10H_base[i]]))}

save(df_means_D_secL10H,file=paste(path,"papers/clearinghouse_training_paper/output_CH_training/df_means_D_secL10H.Rdata",sep="/"))

###
#2#
###

baseline_dealers$training_control[baseline_dealers$training==0] <- TRUE
baseline_dealers$training_control[baseline_dealers$training==1] <- FALSE

#11.
index_overall_Longe10H_mid <- icwIndex(xmat=variables_overall_Longe10H_mid,sgroup = baseline_dealers$training_control, revcols = c(1))
baseline_dealers$index_overall_Longe10H_midT <- index_overall_Longe10H_mid$index

index_overall_Longe10H_base <- icwIndex(xmat=variables_overall_Longe10H_base,sgroup = baseline_dealers$training_control, revcols = c(1))
baseline_dealers$index_overall_Longe10H_baseT <- index_overall_Longe10H_base$index

df_ols_D_secL10H <- array(NA,dim=c(3,3,11))

results_dealer_secL10H <- c("mid_maize.owner.agree.long10h.q21","mid_maize.owner.agree.long10h.q22","mid_maize.owner.agree.long10h.q24"
                            ,"mid_maize.owner.agree.long10h.q25","mid_maize.owner.agree.long10h.q26","mid_maize.owner.agree.long10h.q27"
                            ,"mid_maize.owner.agree.long10h.q30","index_overall_Longe10H_midT")

results_dealer_secL10H_base <- c("maize.owner.agree.long10h.q21","maize.owner.agree.long10h.q22","maize.owner.agree.long10h.q24"
                                 ,"maize.owner.agree.long10h.q25","maize.owner.agree.long10h.q26","maize.owner.agree.long10h.q27"
                                 ,"maize.owner.agree.long10h.q30","index_overall_Longe10H_baseT")

baseline_dealers[results_dealer_secL10H_base] <- lapply(baseline_dealers[results_dealer_secL10H_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_dealer_secL10H)){
  ols <- lm(as.formula(paste(paste(results_dealer_secL10H[i],"training*clearing_demeaned*farmer_demeaned",sep="~"),results_dealer_secL10H_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_secL10H[i],"training*clearing_demeaned*farmer_demeaned",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  df_ols_D_secL10H[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_ols_D_secL10H[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_ols_D_secL10H[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]}

###
#3#
###

baseline_dealers$clearing_control[baseline_dealers$clearing==0] <- TRUE
baseline_dealers$clearing_control[baseline_dealers$clearing==1] <- FALSE

#11.
index_overall_Longe10H_mid <- icwIndex(xmat=variables_overall_Longe10H_mid,sgroup = baseline_dealers$clearing_control, revcols = c(1))
baseline_dealers$index_overall_Longe10H_midC <- index_overall_Longe10H_mid$index

index_overall_Longe10H_base <- icwIndex(xmat=variables_overall_Longe10H_base,sgroup = baseline_dealers$clearing_control, revcols = c(1))
baseline_dealers$index_overall_Longe10H_baseC <- index_overall_Longe10H_base$index

results_dealer_secL10H <- c("mid_maize.owner.agree.long10h.q21","mid_maize.owner.agree.long10h.q22","mid_maize.owner.agree.long10h.q24"
                            ,"mid_maize.owner.agree.long10h.q25","mid_maize.owner.agree.long10h.q26","mid_maize.owner.agree.long10h.q27"
                            ,"mid_maize.owner.agree.long10h.q30","index_overall_Longe10H_midC")

results_dealer_secL10H_base <- c("maize.owner.agree.long10h.q21","maize.owner.agree.long10h.q22","maize.owner.agree.long10h.q24"
                                 ,"maize.owner.agree.long10h.q25","maize.owner.agree.long10h.q26","maize.owner.agree.long10h.q27"
                                 ,"maize.owner.agree.long10h.q30","index_overall_Longe10H_baseC")

baseline_dealers[results_dealer_secL10H_base] <- lapply(baseline_dealers[results_dealer_secL10H_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_dealer_secL10H)){
  ols <- lm(as.formula(paste(paste(results_dealer_secL10H[i],"training_demeaned*clearing*farmer_demeaned",sep="~"),results_dealer_secL10H_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_secL10H[i],"training_demeaned*clearing*farmer_demeaned",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  df_ols_D_secL10H[1,2,i] <- coef_test(ols, vcov_cluster)$beta[3]
  df_ols_D_secL10H[2,2,i] <- coef_test(ols, vcov_cluster)$SE[3]
  df_ols_D_secL10H[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]}

###
#4#
###

baseline_dealers$farmer_control[baseline_dealers$farmer==0] <- TRUE
baseline_dealers$farmer_control[baseline_dealers$farmer==1] <- FALSE

#11.
index_overall_Longe10H_mid <- icwIndex(xmat=variables_overall_Longe10H_mid,sgroup = baseline_dealers$farmer_control, revcols = c(1))
baseline_dealers$index_overall_Longe10H_midF <- index_overall_Longe10H_mid$index

index_overall_Longe10H_base <- icwIndex(xmat=variables_overall_Longe10H_base,sgroup = baseline_dealers$farmer_control, revcols = c(1))
baseline_dealers$index_overall_Longe10H_baseF <- index_overall_Longe10H_base$index

results_dealer_secL10H <- c("mid_maize.owner.agree.long10h.q21","mid_maize.owner.agree.long10h.q22","mid_maize.owner.agree.long10h.q24"
                            ,"mid_maize.owner.agree.long10h.q25","mid_maize.owner.agree.long10h.q26","mid_maize.owner.agree.long10h.q27"
                            ,"mid_maize.owner.agree.long10h.q30","index_overall_Longe10H_midF")

results_dealer_secL10H_base <- c("maize.owner.agree.long10h.q21","maize.owner.agree.long10h.q22","maize.owner.agree.long10h.q24"
                                 ,"maize.owner.agree.long10h.q25","maize.owner.agree.long10h.q26","maize.owner.agree.long10h.q27"
                                 ,"maize.owner.agree.long10h.q30","index_overall_Longe10H_baseF")

for (i in 1:length(results_dealer_secL10H)){
  ols <- lm(as.formula(paste(paste(results_dealer_secL10H[i],"training_demeaned*clearing_demeaned*farmer",sep="~"),results_dealer_secL10H_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_secL10H[i],"training_demeaned*clearing_demeaned*farmer",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  #farmer video treatment at village/shop level so no clustering needed
  df_ols_D_secL10H[1,3,i] <- summary(ols)$coefficients[4,1]
  df_ols_D_secL10H[2,3,i] <- summary(ols)$coefficients[4,2]
  df_ols_D_secL10H[3,3,i] <- summary(ols)$coefficients[4,4]}

save(df_ols_D_secL10H,file=paste(path,"papers/clearinghouse_training_paper/output_CH_training/df_ols_D_secL10H.Rdata",sep="/"))

baseline_dealers[results_dealer_secL10H_base] <- lapply(baseline_dealers[results_dealer_secL10H_base],function(x)x - mean(x,na.rm = T))

#Aker, Boumnijel, McClelland, Tierney (2012)

df_dealer_secL10HT <- data.frame(baseline_dealers$mid_maize.owner.agree.long10h.q21,baseline_dealers$mid_maize.owner.agree.long10h.q22
                                 ,baseline_dealers$mid_maize.owner.agree.long10h.q24,baseline_dealers$mid_maize.owner.agree.long10h.q25
                                 ,baseline_dealers$mid_maize.owner.agree.long10h.q26,baseline_dealers$mid_maize.owner.agree.long10h.q27
                                 ,baseline_dealers$mid_maize.owner.agree.long10h.q30
                                 )
df_dealer_secL10HC <- df_dealer_secL10HT
df_dealer_secL10HF <- df_dealer_secL10HT
#no overall index
#all the same here

df_ols_D_secL10H_J <- array(NA,dim=c(3,3,10))

results_dealer_secL10H_J <- c("mid_maize.owner.agree.long10h.q21","mid_maize.owner.agree.long10h.q22","mid_maize.owner.agree.long10h.q24"
                            ,"mid_maize.owner.agree.long10h.q25","mid_maize.owner.agree.long10h.q26","mid_maize.owner.agree.long10h.q27"
                            ,"mid_maize.owner.agree.long10h.q30")
#no overall index

for (i in 1:length(results_dealer_secL10H_J)){
  df_ols_D_secL10H_J[3,1,i] <- adjust_p(df_ols_D_secL10H[3,1,i],df_dealer_secL10HT,i)
  df_ols_D_secL10H_J[3,2,i] <- adjust_p(df_ols_D_secL10H[3,2,i],df_dealer_secL10HC,i)
  df_ols_D_secL10H_J[3,3,i] <- adjust_p(df_ols_D_secL10H[3,3,i],df_dealer_secL10HF,i)}










###################################################################################################################################################################
##### 4B ANALYSIS: Agro-input dealer - Secondary: 9. Longe 10H: not controlling for baseline ######################################################################
###################################################################################################################################################################

#results_dealer_secL10H_B
#df_means_D_secL10H_B
#df_ols_D_secL10H_B

variables_overall_Longe10H_mid <- cbind(baseline_dealers$mid_maize.owner.agree.long10h.q21,baseline_dealers$mid_maize.owner.agree.long10h.q22,baseline_dealers$mid_maize.owner.agree.long10h.q24
                                         ,baseline_dealers$mid_maize.owner.agree.long10h.q25)

################################################################################################################################################################################

###
#1#
###

results_dealer_secL10H_B <- c("index_overall_Longe10H_mid")

df_means_D_secL10H_B <- array(NA,dim=c(3,11))

for (i in 1:length(results_dealer_secL10H_B)){
  df_means_D_secL10H_B[1,i] <- sum(baseline_dealers[results_dealer_secL10H_B[i]], na.rm=T)/(nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_secL10H_B[i]])))
  df_means_D_secL10H_B[2,i] <- sqrt(var(baseline_dealers[results_dealer_secL10H_B[i]], na.rm=T))
  df_means_D_secL10H_B[3,i] <- nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_secL10H_B[i]]))}

###
#2#
###

df_ols_D_secL10H_B <- array(NA,dim=c(3,3,11))

results_dealer_secL10H_B <- c("index_overall_Longe10H_midT")

for (i in 1:length(results_dealer_secL10H_B)){
  #ols <- lm(as.formula(paste(paste(results_dealer_secL10H_B[i],"training*clearing_demeaned*farmer_demeaned",sep="~"),results_dealer_secL10H_B_base[i],sep="+")),data=baseline_dealers)
  ols <- lm(as.formula(paste(results_dealer_secL10H_B[i],"training*clearing_demeaned*farmer_demeaned",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  df_ols_D_secL10H_B[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_ols_D_secL10H_B[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_ols_D_secL10H_B[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]}

###
#3#
###

results_dealer_secL10H_B <- c("index_overall_Longe10H_midC")

for (i in 1:length(results_dealer_secL10H_B)){
  #ols <- lm(as.formula(paste(paste(results_dealer_secL10H_B[i],"training_demeaned*clearing*farmer_demeaned",sep="~"),results_dealer_secL10H_B_base[i],sep="+")),data=baseline_dealers)
  ols <- lm(as.formula(paste(results_dealer_secL10H_B[i],"training_demeaned*clearing*farmer_demeaned",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  df_ols_D_secL10H_B[1,2,i] <- coef_test(ols, vcov_cluster)$beta[3]
  df_ols_D_secL10H_B[2,2,i] <- coef_test(ols, vcov_cluster)$SE[3]
  df_ols_D_secL10H_B[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]}

###
#4#
###

results_dealer_secL10H_B <- c("index_overall_Longe10H_midF")

for (i in 1:length(results_dealer_secL10H_B)){
  #ols <- lm(as.formula(paste(paste(results_dealer_secL10H_B[i],"training_demeaned*clearing_demeaned*farmer",sep="~"),results_dealer_secL10H_B_base[i],sep="+")),data=baseline_dealers)
  ols <- lm(as.formula(paste(results_dealer_secL10H_B[i],"training_demeaned*clearing_demeaned*farmer",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  #farmer video treatment at village/shop level so no clustering needed
  df_ols_D_secL10H_B[1,3,i] <- summary(ols)$coefficients[4,1]
  df_ols_D_secL10H_B[2,3,i] <- summary(ols)$coefficients[4,2]
  df_ols_D_secL10H_B[3,3,i] <- summary(ols)$coefficients[4,4]}

baseline_dealers=baseline_dealers_save










###################################################################################################################################################################
##### 5 ANALYSIS: Agro-input dealer - Secondary: Longe 5###########################################################################################################
###################################################################################################################################################################

baseline_dealers_save=baseline_dealers
baseline_dealers=subset(baseline_dealers,mid_maize.owner.agree.q45=="1")

#1. Q45. Did you have Longe 5 in 2020 In stock in the second season 2020?

#2. Q46. How much of Longe 5 was carried forward from the previous season (first season 2020) into the second season of 2020 (kg)
baseline_dealers$maize.owner.agree.longe5.q46[baseline_dealers$maize.owner.agree.longe5.q46=="n/a"] <- NA
baseline_dealers$maize.owner.agree.longe5.q46 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.longe5.q46))
baseline_dealers$maize.owner.agree.longe5.q46[baseline_dealers$maize.owner.agree.longe5.q46==999] <- NA
baseline_dealers$maize.owner.agree.longe5.q46[baseline_dealers$maize.owner.agree.q45=="0"] <- 0
baseline_dealers$maize.owner.agree.longe5.q46<-ihs(baseline_dealers$maize.owner.agree.longe5.q46)
baseline_dealers <- trim("maize.owner.agree.longe5.q46",baseline_dealers,trim_perc=.02)

baseline_dealers$mid_maize.owner.agree.longe5.q46 <- baseline_dealers$owner.agree.longe5.q46 #x
baseline_dealers$mid_maize.owner.agree.longe5.q46[baseline_dealers$mid_maize.owner.agree.longe5.q46=="n/a"] <- NA #x
baseline_dealers$mid_maize.owner.agree.longe5.q46 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.longe5.q46)) #x
baseline_dealers$mid_maize.owner.agree.longe5.q46[baseline_dealers$mid_maize.owner.agree.longe5.q46==999] <- NA #x
baseline_dealers$mid_maize.owner.agree.longe5.q46[baseline_dealers$midmaize.owner.agree.q45=="0"] <- 0 #x
baseline_dealers$mid_maize.owner.agree.longe5.q46<-ihs(baseline_dealers$mid_maize.owner.agree.longe5.q46) #x
baseline_dealers <- trim("mid_maize.owner.agree.longe5.q46",baseline_dealers,trim_perc=.02) #x

#3. Q47. How much of Longe 5 was bought by you from any provider during the second season of 2020 (in kg)
baseline_dealers$maize.owner.agree.longe5.q47[baseline_dealers$maize.owner.agree.longe5.q47=="n/a"] <- NA
baseline_dealers$maize.owner.agree.longe5.q47[baseline_dealers$maize.owner.agree.longe5.q47==999] <- NA
baseline_dealers$maize.owner.agree.longe5.q47 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.longe5.q47))
baseline_dealers$maize.owner.agree.longe5.q47[baseline_dealers$maize.owner.agree.q45=="0"] <- 0
baseline_dealers$maize.owner.agree.longe5.q47 <- ihs(baseline_dealers$maize.owner.agree.longe5.q47)
baseline_dealers <- trim("maize.owner.agree.longe5.q47",baseline_dealers,trim_perc=.02)

baseline_dealers$mid_maize.owner.agree.longe5.q47 <- baseline_dealers$owner.agree.longe5.q47 #x
baseline_dealers$mid_maize.owner.agree.longe5.q47[baseline_dealers$mid_maize.owner.agree.longe5.q47=="n/a"] <- NA #x
baseline_dealers$mid_maize.owner.agree.longe5.q47[baseline_dealers$mid_maize.owner.agree.longe5.q47==999] <- NA #x
baseline_dealers$mid_maize.owner.agree.longe5.q47 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.longe5.q47)) #x
baseline_dealers$mid_maize.owner.agree.longe5.q47[baseline_dealers$mid_maize.owner.agree.q45=="0"] <- 0 #x
baseline_dealers$mid_maize.owner.agree.longe5.q47 <- ihs(baseline_dealers$mid_maize.owner.agree.longe5.q47) #x
baseline_dealers <- trim("mid_maize.owner.agree.longe5.q47",baseline_dealers,trim_perc=.02) #x

#4. Q49. What was the cost of Longe 5 from where you obtained it during the second season of 2020? (ugx per kg)
baseline_dealers$maize.owner.agree.longe5.q49[baseline_dealers$maize.owner.agree.longe5.q49=="n/a"] <- NA
baseline_dealers$maize.owner.agree.longe5.q49[baseline_dealers$maize.owner.agree.longe5.q49==999] <- NA
baseline_dealers$maize.owner.agree.longe5.q49 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.longe5.q49))
baseline_dealers <- trim("maize.owner.agree.longe5.q49",baseline_dealers,trim_perc=.02)

baseline_dealers$mid_maize.owner.agree.longe5.q49 <- baseline_dealers$owner.agree.longe5.q49 #x
baseline_dealers$mid_maize.owner.agree.longe5.q49[baseline_dealers$mid_maize.owner.agree.longe5.q49=="n/a"] <- NA #x
baseline_dealers$mid_maize.owner.agree.longe5.q49[baseline_dealers$mid_maize.owner.agree.longe5.q49==999] <- NA #x
baseline_dealers$mid_maize.owner.agree.longe5.q49 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.longe5.q49)) #x
baseline_dealers <- trim("mid_maize.owner.agree.longe5.q49",baseline_dealers,trim_perc=.02) #x

#5. Q50. Total quantity sold of ${carry3} (Kg) over the second season of 2020
baseline_dealers$maize.owner.agree.longe5.q50 <- ihs(baseline_dealers$maize.owner.agree.longe5.q50)
baseline_dealers <- trim("maize.owner.agree.longe5.q50",baseline_dealers,trim_perc=.02)

baseline_dealers$mid_maize.owner.agree.longe5.q50 <- ihs(baseline_dealers$mid_maize.owner.agree.longe5.q50) #x
baseline_dealers <- trim("mid_maize.owner.agree.longe5.q50",baseline_dealers,trim_perc=.02) #x

#6. Q51. Sales price per kilogram of ${q50}  at the beginning of the second season of 2020
baseline_dealers$maize.owner.agree.longe5.q51 <- ihs(baseline_dealers$maize.owner.agree.longe5.q51)
baseline_dealers <- trim("maize.owner.agree.longe5.q51",baseline_dealers,trim_perc=.02)

baseline_dealers$mid_maize.owner.agree.longe5.q51 <- ihs(baseline_dealers$mid_maize.owner.agree.longe5.q51) #x
baseline_dealers <- trim("mid_maize.owner.agree.longe5.q51",baseline_dealers,trim_perc=.02) #x

#7. Q52. How much of Longe 5 was lost/wasted the second season of 2020 (kg)
baseline_dealers$maize.owner.agree.longe5.q52[baseline_dealers$maize.owner.agree.longe5.q52=="n/a"] <- NA
baseline_dealers$maize.owner.agree.longe5.q52[baseline_dealers$maize.owner.agree.q45=="0"] <- 0
baseline_dealers$maize.owner.agree.longe5.q52 <- ihs(baseline_dealers$maize.owner.agree.longe5.q52)
baseline_dealers <- trim("maize.owner.agree.longe5.q52",baseline_dealers,trim_perc=.02)

baseline_dealers$mid_maize.owner.agree.longe5.q52 <- baseline_dealers$owner.agree.longe5.q52 #x
baseline_dealers$mid_maize.owner.agree.longe5.q52[baseline_dealers$mid_maize.owner.agree.longe5.q52=="n/a"] <- 0 #x
baseline_dealers$mid_maize.owner.agree.longe5.q52[baseline_dealers$mid_maize.owner.agree.longe5.q52==999] <- NA #x
baseline_dealers$mid_maize.owner.agree.longe5.q52 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.longe5.q52)) #x
baseline_dealers$mid_maize.owner.agree.longe5.q52[baseline_dealers$mid_maize.owner.agree.q45=="0"] <- 0 #x
baseline_dealers$mid_maize.owner.agree.longe5.q52 <- ihs(baseline_dealers$mid_maize.owner.agree.longe5.q52) #x
baseline_dealers <- trim("mid_maize.owner.agree.longe5.q52",baseline_dealers,trim_perc=.02) #x

#8. Q54. Did you ever run out of this Longe 5 during the second season of 2020?
baseline_dealers$maize.owner.agree.longe5.q54[baseline_dealers$maize.owner.agree.longe5.q54=="n/a"] <- NA
baseline_dealers$maize.owner.agree.longe5.q54 <- ifelse(baseline_dealers$maize.owner.agree.longe5.q54=="Yes",1,0)

baseline_dealers$mid_maize.owner.agree.longe5.q54 <- baseline_dealers$owner.agree.longe5.q54 #x
baseline_dealers$mid_maize.owner.agree.longe5.q54[baseline_dealers$mid_maize.owner.agree.longe5.q54=="n/a"] <- NA #x
baseline_dealers$mid_maize.owner.agree.longe5.q54 <- ifelse(baseline_dealers$mid_maize.owner.agree.longe5.q54=="Yes",1,0) #x

#9. Q55. Estimate how often you ran out of stock for Longe 5 during the second season of 2020
baseline_dealers$maize.owner.agree.longe5.q55 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.longe5.q55))
baseline_dealers$maize.owner.agree.longe5.q55[baseline_dealers$maize.owner.agree.longe5.q54=="0"] <- 0
baseline_dealers$maize.owner.agree.longe5.q55[baseline_dealers$maize.owner.agree.longe5.q55=="1"] <- 21.74 #Everyday: 21.74 working days per month
baseline_dealers$maize.owner.agree.longe5.q55[baseline_dealers$maize.owner.agree.longe5.q55=="2"] <- 4.34524 #once a week: 4,34524 weeks in a month
baseline_dealers$maize.owner.agree.longe5.q55[baseline_dealers$maize.owner.agree.longe5.q55=="3"] <- 1 #Once a month
baseline_dealers$maize.owner.agree.longe5.q55[baseline_dealers$maize.owner.agree.longe5.q55=="4"] <- 1/3 #once in a season
baseline_dealers$maize.owner.agree.longe5.q55[baseline_dealers$maize.owner.agree.longe5.q55=="5"] <- 0 #Never
baseline_dealers$maize.owner.agree.longe5.q55[baseline_dealers$maize.owner.agree.longe5.q55=="6"] <- NA #Never
baseline_dealers <- trim("maize.owner.agree.longe5.q55",baseline_dealers,trim_perc=.02)

baseline_dealers$mid_maize.owner.agree.longe5.q55 <- baseline_dealers$owner.agree.longe5.q55 #x
baseline_dealers$mid_maize.owner.agree.longe5.q55 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.longe5.q55))
baseline_dealers$mid_maize.owner.agree.longe5.q55[baseline_dealers$mid_maize.owner.agree.longe5.q54=="0"] <- 0
baseline_dealers$mid_maize.owner.agree.longe5.q55[baseline_dealers$mid_maize.owner.agree.longe5.q55=="1"] <- 21.74 #Everyday: 21.74 working days per month
baseline_dealers$mid_maize.owner.agree.longe5.q55[baseline_dealers$mid_maize.owner.agree.longe5.q55=="2"] <- 4.34524 #once a week: 4,34524 weeks in a month
baseline_dealers$mid_maize.owner.agree.longe5.q55[baseline_dealers$mid_maize.owner.agree.longe5.q55=="3"] <- 1 #Once a month
baseline_dealers$mid_maize.owner.agree.longe5.q55[baseline_dealers$mid_maize.owner.agree.longe5.q55=="4"] <- 1/3 #once in a season
baseline_dealers$mid_maize.owner.agree.longe5.q55[baseline_dealers$mid_maize.owner.agree.longe5.q55=="5"] <- 0 #Never
baseline_dealers$mid_maize.owner.agree.longe5.q55[baseline_dealers$mid_maize.owner.agree.longe5.q55=="6"] <- NA #Never
baseline_dealers <- trim("mid_maize.owner.agree.longe5.q55",baseline_dealers,trim_perc=.02)

#11. Overall index of secondary Longe5 agro-input dealer outcome variables
variables_overall_Longe5_mid <- cbind(baseline_dealers$mid_maize.owner.agree.longe5.q46,baseline_dealers$mid_maize.owner.agree.longe5.q47,baseline_dealers$mid_maize.owner.agree.longe5.q49
                                        ,baseline_dealers$mid_maize.owner.agree.longe5.q50) #x
variables_overall_Longe5_base <- cbind(baseline_dealers$maize.owner.agree.longe5.q46,baseline_dealers$maize.owner.agree.longe5.q47,baseline_dealers$maize.owner.agree.longe5.q49
                                         ,baseline_dealers$maize.owner.agree.longe5.q50)

index_overall_Longe5_mid <- icwIndex(xmat=variables_overall_Longe5_mid,revcols = c(1)) #x
baseline_dealers$index_overall_Longe5_mid <- index_overall_Longe5_mid$index #x

index_overall_Longe5_base <- icwIndex(xmat=variables_overall_Longe5_base,revcols = c(1))
baseline_dealers$index_overall_Longe5_base <- index_overall_Longe5_base$index

################################################################################################################################################################################

###
#1#
###

results_dealer_secL5 <- c("mid_maize.owner.agree.longe5.q46","mid_maize.owner.agree.longe5.q47","mid_maize.owner.agree.longe5.q49"
                            ,"mid_maize.owner.agree.longe5.q50","mid_maize.owner.agree.longe5.q51","mid_maize.owner.agree.longe5.q52"
                            ,"mid_maize.owner.agree.longe5.q55","index_overall_Longe5_mid")

results_dealer_secL5_base <- c("maize.owner.agree.longe5.q46","maize.owner.agree.longe5.q47","maize.owner.agree.longe5.q49"
                                 ,"maize.owner.agree.longe5.q50","maize.owner.agree.longe5.q51","maize.owner.agree.longe5.q52"
                                 ,"maize.owner.agree.longe5.q55","index_overall_Longe5_base")

baseline_dealers[results_dealer_secL5_base] <- lapply(baseline_dealers[results_dealer_secL5_base],function(x)x - mean(x,na.rm = T))

df_means_D_secL5 <- array(NA,dim=c(3,11))

for (i in 1:length(results_dealer_secL5)){
  df_means_D_secL5[1,i] <- sum(baseline_dealers[results_dealer_secL5[i]], na.rm=T)/(nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_secL5[i]])))
  df_means_D_secL5[2,i] <- sqrt(var(baseline_dealers[results_dealer_secL5[i]], na.rm=T))
  df_means_D_secL5[3,i] <- nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_secL5[i]]))-sum(is.na(baseline_dealers[results_dealer_secL5_base[i]]))+sum(is.na(baseline_dealers[results_dealer_secL5[i]])&is.na(baseline_dealers[results_dealer_secL5_base[i]]))}

###
#2#
###

baseline_dealers$training_control[baseline_dealers$training==0] <- TRUE
baseline_dealers$training_control[baseline_dealers$training==1] <- FALSE

#11.
index_overall_Longe5_mid <- icwIndex(xmat=variables_overall_Longe5_mid,sgroup = baseline_dealers$training_control,revcols = c(1))
baseline_dealers$index_overall_Longe5_midT <- index_overall_Longe5_mid$index

index_overall_Longe5_base <- icwIndex(xmat=variables_overall_Longe5_base,sgroup = baseline_dealers$training_control,revcols = c(1))
baseline_dealers$index_overall_Longe5_baseT <- index_overall_Longe5_base$index

df_ols_D_secL5 <- array(NA,dim=c(3,3,11))

results_dealer_secL5 <- c("mid_maize.owner.agree.longe5.q46","mid_maize.owner.agree.longe5.q47","mid_maize.owner.agree.longe5.q49"
                            ,"mid_maize.owner.agree.longe5.q50","mid_maize.owner.agree.longe5.q51","mid_maize.owner.agree.longe5.q52"
                            ,"mid_maize.owner.agree.longe5.q55","index_overall_Longe5_midT")

results_dealer_secL5_base <- c("maize.owner.agree.longe5.q46","maize.owner.agree.longe5.q47","maize.owner.agree.longe5.q49"
                                 ,"maize.owner.agree.longe5.q50","maize.owner.agree.longe5.q51","maize.owner.agree.longe5.q52"
                                 ,"maize.owner.agree.longe5.q55","index_overall_Longe5_baseT")

baseline_dealers[results_dealer_secL5_base] <- lapply(baseline_dealers[results_dealer_secL5_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_dealer_secL5)){
  ols <- lm(as.formula(paste(paste(results_dealer_secL5[i],"training*clearing_demeaned*farmer_demeaned",sep="~"),results_dealer_secL5_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_secL5[i],"training*clearing_demeaned*farmer_demeaned",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  df_ols_D_secL5[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_ols_D_secL5[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_ols_D_secL5[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]}

###
#3#
###

baseline_dealers$clearing_control[baseline_dealers$clearing==0] <- TRUE
baseline_dealers$clearing_control[baseline_dealers$clearing==1] <- FALSE

#11.
index_overall_Longe5_mid <- icwIndex(xmat=variables_overall_Longe5_mid,sgroup = baseline_dealers$clearing_control,revcols = c(1))
baseline_dealers$index_overall_Longe5_midC <- index_overall_Longe5_mid$index

index_overall_Longe5_base <- icwIndex(xmat=variables_overall_Longe5_base,sgroup = baseline_dealers$clearing_control,revcols = c(1))
baseline_dealers$index_overall_Longe5_baseC <- index_overall_Longe5_base$index

results_dealer_secL5 <- c("mid_maize.owner.agree.longe5.q46","mid_maize.owner.agree.longe5.q47","mid_maize.owner.agree.longe5.q49"
                            ,"mid_maize.owner.agree.longe5.q50","mid_maize.owner.agree.longe5.q51","mid_maize.owner.agree.longe5.q52"
                            ,"mid_maize.owner.agree.longe5.q55","index_overall_Longe5_midC")

results_dealer_secL5_base <- c("maize.owner.agree.longe5.q46","maize.owner.agree.longe5.q47","maize.owner.agree.longe5.q49"
                                 ,"maize.owner.agree.longe5.q50","maize.owner.agree.longe5.q51","maize.owner.agree.longe5.q52"
                                 ,"maize.owner.agree.longe5.q55","index_overall_Longe5_baseC")

baseline_dealers[results_dealer_secL5_base] <- lapply(baseline_dealers[results_dealer_secL5_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_dealer_secL5)){
  ols <- lm(as.formula(paste(paste(results_dealer_secL5[i],"training_demeaned*clearing*farmer_demeaned",sep="~"),results_dealer_secL5_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_secL5[i],"training_demeaned*clearing*farmer_demeaned",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  df_ols_D_secL5[1,2,i] <- coef_test(ols, vcov_cluster)$beta[3]
  df_ols_D_secL5[2,2,i] <- coef_test(ols, vcov_cluster)$SE[3]
  df_ols_D_secL5[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]}

###
#4#
###

baseline_dealers$farmer_control[baseline_dealers$farmer==0] <- TRUE
baseline_dealers$farmer_control[baseline_dealers$farmer==1] <- FALSE

#11.
index_overall_Longe5_mid <- icwIndex(xmat=variables_overall_Longe5_mid,sgroup = baseline_dealers$farmer_control,revcols = c(1))
baseline_dealers$index_overall_Longe5_midF <- index_overall_Longe5_mid$index

index_overall_Longe5_base <- icwIndex(xmat=variables_overall_Longe5_base,sgroup = baseline_dealers$farmer_control,revcols = c(1))
baseline_dealers$index_overall_Longe5_baseF <- index_overall_Longe5_base$index

results_dealer_secL5 <- c("mid_maize.owner.agree.longe5.q46","mid_maize.owner.agree.longe5.q47","mid_maize.owner.agree.longe5.q49"
                            ,"mid_maize.owner.agree.longe5.q50","mid_maize.owner.agree.longe5.q51","mid_maize.owner.agree.longe5.q52"
                            ,"mid_maize.owner.agree.longe5.q55","index_overall_Longe5_midF")

results_dealer_secL5_base <- c("maize.owner.agree.longe5.q46","maize.owner.agree.longe5.q47","maize.owner.agree.longe5.q49"
                                 ,"maize.owner.agree.longe5.q50","maize.owner.agree.longe5.q51","maize.owner.agree.longe5.q52"
                                 ,"maize.owner.agree.longe5.q55","index_overall_Longe5_baseF")

baseline_dealers[results_dealer_secL5_base] <- lapply(baseline_dealers[results_dealer_secL5_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_dealer_secL5)){
  ols <- lm(as.formula(paste(paste(results_dealer_secL5[i],"training_demeaned*clearing_demeaned*farmer",sep="~"),results_dealer_secL5_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_secL5[i],"training_demeaned*clearing_demeaned*farmer",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  #farmer video treatment at village/shop level so no clustering needed
  df_ols_D_secL5[1,3,i] <- summary(ols)$coefficients[4,1]
  df_ols_D_secL5[2,3,i] <- summary(ols)$coefficients[4,2]
  df_ols_D_secL5[3,3,i] <- summary(ols)$coefficients[4,4]}


#Aker, Boumnijel, McClelland, Tierney (2012)
df_dealer_secL5T <- data.frame(baseline_dealers$mid_maize.owner.agree.longe5.q46
                               ,baseline_dealers$mid_maize.owner.agree.longe5.q47,baseline_dealers$mid_maize.owner.agree.longe5.q49
                                ,baseline_dealers$mid_maize.owner.agree.longe5.q50,baseline_dealers$mid_maize.owner.agree.longe5.q51
                                ,baseline_dealers$mid_maize.owner.agree.longe5.q52
                                ,baseline_dealers$mid_maize.owner.agree.longe5.q55)

df_dealer_secL5C <- df_dealer_secL5T
df_dealer_secL5F <- df_dealer_secL5T
#no overall index

df_ols_D_secL5_J <- array(NA,dim=c(3,3,11))

results_dealer_secL5_J <- c("mid_maize.owner.agree.longe5.q46"
                            ,"mid_maize.owner.agree.longe5.q47","mid_maize.owner.agree.longe5.q49"
                            ,"mid_maize.owner.agree.longe5.q50","mid_maize.owner.agree.longe5.q51"
                            ,"mid_maize.owner.agree.longe5.q52"
                            ,"mid_maize.owner.agree.longe5.q55")
#no overall index

for (i in 1:length(results_dealer_secL5_J)){
  df_ols_D_secL5_J[3,1,i] <- adjust_p(df_ols_D_secL5[3,1,i],df_dealer_secL5T,i)
  df_ols_D_secL5_J[3,2,i] <- adjust_p(df_ols_D_secL5[3,2,i],df_dealer_secL5C,i)
  df_ols_D_secL5_J[3,3,i] <- adjust_p(df_ols_D_secL5[3,3,i],df_dealer_secL5F,i)}










###################################################################################################################################################################
##### 5B ANALYSIS: Agro-input dealer - Secondary: Longe 5, not controlling for baseline ###########################################################################
###################################################################################################################################################################

################################################################################################################################################################################

###
#1#
###

results_dealer_secL5_B <- c("index_overall_Longe5_mid")

df_means_D_secL5_B <- array(NA,dim=c(3,11))

for (i in 1:length(results_dealer_secL5_B)){
  df_means_D_secL5_B[1,i] <- sum(baseline_dealers[results_dealer_secL5_B[i]], na.rm=T)/(nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_secL5_B[i]])))
  df_means_D_secL5_B[2,i] <- sqrt(var(baseline_dealers[results_dealer_secL5_B[i]], na.rm=T))
  df_means_D_secL5_B[3,i] <- nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_secL5_B[i]]))}

###
#2#
###

df_ols_D_secL5_B <- array(NA,dim=c(3,3,11))

results_dealer_secL5_B <- c("index_overall_Longe5_midT")

for (i in 1:length(results_dealer_secL5_B)){
  #ols <- lm(as.formula(paste(paste(results_dealer_secL5_B[i],"training*clearing_demeaned*farmer_demeaned",sep="~"),results_dealer_secL5_base[i],sep="+")),data=baseline_dealers)
  ols <- lm(as.formula(paste(results_dealer_secL5_B[i],"training*clearing_demeaned*farmer_demeaned",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  df_ols_D_secL5_B[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_ols_D_secL5_B[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_ols_D_secL5_B[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]}

###
#3#
###

results_dealer_secL5_B <- c("index_overall_Longe5_midC")

for (i in 1:length(results_dealer_secL5_B)){
  #ols <- lm(as.formula(paste(paste(results_dealer_secL5_B[i],"training_demeaned*clearing*farmer_demeaned",sep="~"),results_dealer_secL5_base[i],sep="+")),data=baseline_dealers)
  ols <- lm(as.formula(paste(results_dealer_secL5_B[i],"training_demeaned*clearing*farmer_demeaned",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  df_ols_D_secL5_B[1,2,i] <- coef_test(ols, vcov_cluster)$beta[3]
  df_ols_D_secL5_B[2,2,i] <- coef_test(ols, vcov_cluster)$SE[3]
  df_ols_D_secL5_B[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]}

###
#4#
###

results_dealer_secL5_B <- c("index_overall_Longe5_midF")

for (i in 1:length(results_dealer_secL5_B)){
  #ols <- lm(as.formula(paste(paste(results_dealer_secL5_B[i],"training_demeaned*clearing_demeaned*farmer",sep="~"),results_dealer_secL5_base[i],sep="+")),data=baseline_dealers)
  ols <- lm(as.formula(paste(results_dealer_secL5_B[i],"training_demeaned*clearing_demeaned*farmer",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  #farmer video treatment at village/shop level so no clustering needed
  df_ols_D_secL5_B[1,3,i] <- summary(ols)$coefficients[4,1]
  df_ols_D_secL5_B[2,3,i] <- summary(ols)$coefficients[4,2]
  df_ols_D_secL5_B[3,3,i] <- summary(ols)$coefficients[4,4]}

baseline_dealers=baseline_dealers_save










###################################################################################################################################################################
##### 6 ANALYSIS: Agro-input dealer - Secondary: 11. official #####################################################################################################
###################################################################################################################################################################

#1. Q114. Is this business registered as a seed dealer with UNADA (Uganda National Agro-input Dealers Association?
baseline_dealers$mid_maize.owner.agree.inspection.q114 <- baseline_dealers$owner.agree.inspection.q114 #x
baseline_dealers$mid_maize.owner.agree.inspection.q114[baseline_dealers$mid_maize.owner.agree.inspection.q114==98] <- NA #here because binary #x
baseline_dealers$mid_maize.owner.agree.inspection.q114<-as.character(baseline_dealers$mid_maize.owner.agree.inspection.q114) #x
baseline_dealers$mid_maize.owner.agree.inspection.q114<-ifelse(baseline_dealers$mid_maize.owner.agree.inspection.q114=="Yes",1,0) #x
baseline_dealers$mid_maize.owner.agree.inspection.q114 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.inspection.q114)) #x

#2. Q115. Does this business have a trading license issued by local government?
baseline_dealers$mid_maize.owner.agree.inspection.q115 <- baseline_dealers$owner.agree.inspection.q115 #x
baseline_dealers$mid_maize.owner.agree.inspection.q115[baseline_dealers$mid_maize.owner.agree.inspection.q115==98] <- NA #here because binary #x
baseline_dealers$mid_maize.owner.agree.inspection.q115<-as.character(baseline_dealers$mid_maize.owner.agree.inspection.q115) #x
baseline_dealers$mid_maize.owner.agree.inspection.q115<-ifelse(baseline_dealers$mid_maize.owner.agree.inspection.q115=="Yes",1,0) #x
baseline_dealers$mid_maize.owner.agree.inspection.q115 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.inspection.q115)) #x

#3. Q116. Is this business a member of any other professional association?
baseline_dealers$mid_maize.owner.agree.inspection.q116 <- baseline_dealers$owner.agree.inspection.q116 #x
baseline_dealers$mid_maize.owner.agree.inspection.q116[baseline_dealers$mid_maize.owner.agree.inspection.q116==98] <- NA #here because binary #x
baseline_dealers$mid_maize.owner.agree.inspection.q116<-as.character(baseline_dealers$mid_maize.owner.agree.inspection.q116) #x
baseline_dealers$mid_maize.owner.agree.inspection.q116<-ifelse(baseline_dealers$mid_maize.owner.agree.inspection.q116=="Yes",1,0) #x
baseline_dealers$mid_maize.owner.agree.inspection.q116 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.inspection.q116)) #x

#4. Q117. How often were you inspected by DAO/MAAIF or UNADA last year (indicate 0 if no inspection happened).
baseline_dealers$maize.owner.agree.inspection.q117 <- ihs(baseline_dealers$maize.owner.agree.inspection.q117)
baseline_dealers <- trim("maize.owner.agree.inspection.q117",baseline_dealers,trim_perc=.02)

baseline_dealers$mid_maize.owner.agree.inspection.q117 <- baseline_dealers$owner.agree.inspection.q117 #x
baseline_dealers$mid_maize.owner.agree.inspection.q117 <- ihs(baseline_dealers$mid_maize.owner.agree.inspection.q117) #x
baseline_dealers <- trim("mid_maize.owner.agree.inspection.q117",baseline_dealers,trim_perc=.02) #x

#5. Q118. Have you ever received a warning as a result of inspection if something was not up to standard?
baseline_dealers$mid_maize.owner.agree.inspection.q118 <- baseline_dealers$owner.agree.inspection.q118 #x
baseline_dealers$mid_maize.owner.agree.inspection.q118[baseline_dealers$mid_maize.owner.agree.inspection.q118==98] <- NA #x
baseline_dealers$mid_maize.owner.agree.inspection.q118<-as.character(baseline_dealers$mid_maize.owner.agree.inspection.q118) #x
baseline_dealers$mid_maize.owner.agree.inspection.q118<-ifelse(baseline_dealers$mid_maize.owner.agree.inspection.q118=="Yes",1,0) #x
baseline_dealers$mid_maize.owner.agree.inspection.q118<-as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.inspection.q118)) #x

#6. Q119. Has some of your produce ever been confiscated after inspection?
baseline_dealers$mid_maize.owner.agree.inspection.q119 <- baseline_dealers$owner.agree.inspection.q119 #x
baseline_dealers$mid_maize.owner.agree.inspection.q119[baseline_dealers$mid_maize.owner.agree.inspection.q119==98] <- NA #x
baseline_dealers$mid_maize.owner.agree.inspection.q119<-as.character(baseline_dealers$mid_maize.owner.agree.inspection.q119) #x
baseline_dealers$mid_maize.owner.agree.inspection.q119<-ifelse(baseline_dealers$mid_maize.owner.agree.inspection.q119=="Yes",1,0) #x
baseline_dealers$mid_maize.owner.agree.inspection.q119<-as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.inspection.q119)) #x

#8. Overall index of secondary OFFICIAL agro-input dealer outcome variables
variables_overall_off_mid <- cbind(baseline_dealers$mid_maize.owner.agree.inspection.q114,baseline_dealers$mid_maize.owner.agree.inspection.q115
                                   ,baseline_dealers$mid_maize.owner.agree.inspection.q116,baseline_dealers$mid_maize.owner.agree.inspection.q118
                                   ,baseline_dealers$mid_maize.owner.agree.inspection.q119) #x
variables_overall_off_base <- cbind(baseline_dealers$maize.owner.agree.inspection.q114,baseline_dealers$maize.owner.agree.inspection.q115
                                    ,baseline_dealers$maize.owner.agree.inspection.q116,baseline_dealers$maize.owner.agree.inspection.q118
                                    ,baseline_dealers$maize.owner.agree.inspection.q119)

index_overall_off_mid <- icwIndex(xmat=variables_overall_off_mid,revcols = c(4,5)) #x
baseline_dealers$index_overall_off_mid <- index_overall_off_mid$index #x

index_overall_off_base <- icwIndex(xmat=variables_overall_off_base,revcols = c(4,5))
baseline_dealers$index_overall_off_base <- index_overall_off_base$index

################################################################################################################################################################################

###
#1#
###

results_dealer_sec_off <- c("mid_maize.owner.agree.inspection.q114","mid_maize.owner.agree.inspection.q115","mid_maize.owner.agree.inspection.q116","mid_maize.owner.agree.inspection.q117"
                            ,"mid_maize.owner.agree.inspection.q118","mid_maize.owner.agree.inspection.q119"
                            ,"index_overall_off_mid")

results_dealer_sec_off_base <- c("maize.owner.agree.inspection.q114","maize.owner.agree.inspection.q115","maize.owner.agree.inspection.q116","maize.owner.agree.inspection.q117"
                                 ,"maize.owner.agree.inspection.q118","maize.owner.agree.inspection.q119"
                                 ,"index_overall_off_base")

baseline_dealers[results_dealer_sec_off_base] <- lapply(baseline_dealers[results_dealer_sec_off_base],function(x)x - mean(x,na.rm = T))

df_means_D_sec_off <- array(NA,dim=c(3,11))

for (i in 1:length(results_dealer_sec_off)){
  df_means_D_sec_off[1,i] <- sum(baseline_dealers[results_dealer_sec_off[i]], na.rm=T)/(nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_sec_off[i]])))
  df_means_D_sec_off[2,i] <- sqrt(var(baseline_dealers[results_dealer_sec_off[i]], na.rm=T))
  df_means_D_sec_off[3,i] <- nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_sec_off[i]]))-sum(is.na(baseline_dealers[results_dealer_sec_off_base[i]]))+sum(is.na(baseline_dealers[results_dealer_sec_off[i]])&is.na(baseline_dealers[results_dealer_sec_off_base[i]]))}

###
#2#
###

baseline_dealers$training_control[baseline_dealers$training==0] <- TRUE
baseline_dealers$training_control[baseline_dealers$training==1] <- FALSE

#8.
index_overall_off_mid <- icwIndex(xmat=variables_overall_off_mid,sgroup = baseline_dealers$training_control,revcols = c(4,5))
baseline_dealers$index_overall_off_midT <- index_overall_off_mid$index

index_overall_off_base <- icwIndex(xmat=variables_overall_off_base,sgroup = baseline_dealers$training_control,revcols = c(4,5))
baseline_dealers$index_overall_off_baseT <- index_overall_off_base$index

df_ols_D_sec_off <- array(NA,dim=c(3,3,11))

results_dealer_sec_off <- c("mid_maize.owner.agree.inspection.q114","mid_maize.owner.agree.inspection.q115","mid_maize.owner.agree.inspection.q116","mid_maize.owner.agree.inspection.q117"
                            ,"mid_maize.owner.agree.inspection.q118","mid_maize.owner.agree.inspection.q119"
                            ,"index_overall_off_midT")

results_dealer_sec_off_base <- c("maize.owner.agree.inspection.q114","maize.owner.agree.inspection.q115","maize.owner.agree.inspection.q116","maize.owner.agree.inspection.q117"
                                 ,"maize.owner.agree.inspection.q118","maize.owner.agree.inspection.q119"
                                 ,"index_overall_off_baseT")

baseline_dealers[results_dealer_sec_off_base] <- lapply(baseline_dealers[results_dealer_sec_off_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_dealer_sec_off)){
  ols <- lm(as.formula(paste(paste(results_dealer_sec_off[i],"training*clearing_demeaned*farmer_demeaned",sep="~"),results_dealer_sec_off_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_sec_off[i],"training*clearing_demeaned*farmer_demeaned",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  df_ols_D_sec_off[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_ols_D_sec_off[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_ols_D_sec_off[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]}

###
#3#
###

baseline_dealers$clearing_control[baseline_dealers$clearing==0] <- TRUE
baseline_dealers$clearing_control[baseline_dealers$clearing==1] <- FALSE

#8.
index_overall_off_mid <- icwIndex(xmat=variables_overall_off_mid,sgroup = baseline_dealers$clearing_control,revcols = c(4,5))
baseline_dealers$index_overall_off_midC <- index_overall_off_mid$index

index_overall_off_base <- icwIndex(xmat=variables_overall_off_base,sgroup = baseline_dealers$clearing_control,revcols = c(4,5))
baseline_dealers$index_overall_off_baseC <- index_overall_off_base$index

results_dealer_sec_off <- c("mid_maize.owner.agree.inspection.q114","mid_maize.owner.agree.inspection.q115","mid_maize.owner.agree.inspection.q116","mid_maize.owner.agree.inspection.q117"
                            ,"mid_maize.owner.agree.inspection.q118","mid_maize.owner.agree.inspection.q119"
                            ,"index_overall_off_midC")

results_dealer_sec_off_base <- c("maize.owner.agree.inspection.q114","maize.owner.agree.inspection.q115","maize.owner.agree.inspection.q116","maize.owner.agree.inspection.q117"
                                 ,"maize.owner.agree.inspection.q118","maize.owner.agree.inspection.q119"
                                 ,"index_overall_off_baseC")

baseline_dealers[results_dealer_sec_off_base] <- lapply(baseline_dealers[results_dealer_sec_off_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_dealer_sec_off)){
  ols <- lm(as.formula(paste(paste(results_dealer_sec_off[i],"training_demeaned*clearing*farmer_demeaned",sep="~"),results_dealer_sec_off_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_sec_off[i],"training_demeaned*clearing*farmer_demeaned",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  df_ols_D_sec_off[1,2,i] <- coef_test(ols, vcov_cluster)$beta[3]
  df_ols_D_sec_off[2,2,i] <- coef_test(ols, vcov_cluster)$SE[3]
  df_ols_D_sec_off[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]}

###
#4#
###

baseline_dealers$farmer_control[baseline_dealers$farmer==0] <- TRUE
baseline_dealers$farmer_control[baseline_dealers$farmer==1] <- FALSE

#8.
index_overall_off_mid <- icwIndex(xmat=variables_overall_off_mid,sgroup = baseline_dealers$farmer_control,revcols = c(4,5))
baseline_dealers$index_overall_off_midF <- index_overall_off_mid$index

index_overall_off_base <- icwIndex(xmat=variables_overall_off_base,sgroup = baseline_dealers$farmer_control,revcols = c(4,5))
baseline_dealers$index_overall_off_baseF <- index_overall_off_base$index

results_dealer_sec_off <- c("mid_maize.owner.agree.inspection.q114","mid_maize.owner.agree.inspection.q115","mid_maize.owner.agree.inspection.q116","mid_maize.owner.agree.inspection.q117"
                            ,"mid_maize.owner.agree.inspection.q118","mid_maize.owner.agree.inspection.q119"
                            ,"index_overall_off_midF")

results_dealer_sec_off_base <- c("maize.owner.agree.inspection.q114","maize.owner.agree.inspection.q115","maize.owner.agree.inspection.q116","maize.owner.agree.inspection.q117"
                                 ,"maize.owner.agree.inspection.q118","maize.owner.agree.inspection.q119"
                                 ,"index_overall_off_baseF")

baseline_dealers[results_dealer_sec_off_base] <- lapply(baseline_dealers[results_dealer_sec_off_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_dealer_sec_off)){
  ols <- lm(as.formula(paste(paste(results_dealer_sec_off[i],"training_demeaned*clearing_demeaned*farmer",sep="~"),results_dealer_sec_off_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_sec_off[i],"training_demeaned*clearing_demeaned*farmer",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  #farmer video treatment at village/shop level so no clustering needed
  df_ols_D_sec_off[1,3,i] <- summary(ols)$coefficients[4,1]
  df_ols_D_sec_off[2,3,i] <- summary(ols)$coefficients[4,2]
  df_ols_D_sec_off[3,3,i] <- summary(ols)$coefficients[4,4]}

#Aker, Boumnijel, McClelland, Tierney (2012)
df_dealer_sec_offT <- data.frame(baseline_dealers$mid_maize.owner.agree.inspection.q114,baseline_dealers$mid_maize.owner.agree.inspection.q115
                                 ,baseline_dealers$mid_maize.owner.agree.inspection.q116,baseline_dealers$mid_maize.owner.agree.inspection.q117
                                 ,baseline_dealers$mid_maize.owner.agree.inspection.q118,baseline_dealers$mid_maize.owner.agree.inspection.q119
                                 )
df_dealer_sec_offC <- df_dealer_sec_offT
df_dealer_sec_offF <- df_dealer_sec_offT
#no overall index

df_ols_D_sec_off_J <- array(NA,dim=c(3,3,11))

results_dealer_sec_off_J <- c("mid_maize.owner.agree.inspection.q114","mid_maize.owner.agree.inspection.q115"
                            ,"mid_maize.owner.agree.inspection.q116","mid_maize.owner.agree.inspection.q117"
                            ,"mid_maize.owner.agree.inspection.q118","mid_maize.owner.agree.inspection.q119"
                            )
#no overall index

for (i in 1:length(results_dealer_sec_off_J)){
  df_ols_D_sec_off_J[3,1,i] <- adjust_p(df_ols_D_sec_off[3,1,i],df_dealer_sec_offT,i)
  df_ols_D_sec_off_J[3,2,i] <- adjust_p(df_ols_D_sec_off[3,2,i],df_dealer_sec_offC,i)
  df_ols_D_sec_off_J[3,3,i] <- adjust_p(df_ols_D_sec_off[3,3,i],df_dealer_sec_offF,i)}










###################################################################################################################################################################
##### 7 ANALYSIS: Agro-input dealer - Secondary: 11. seed bag #####################################################################################################
###################################################################################################################################################################

baseline_dealers_save=baseline_dealers
baseline_dealers=subset(baseline_dealers,!is.na(baseline_dealers$mid_reading_unadj))

#midline names:
#"age_mid" "exp_mid" "lot_mid" "cert_mid" "date_mid" "verif_mid" "origin_mid" "company_mid" "reading" "variety_mid" "date_pack_mid" "other_var_mid"

#1. Random seed bag shows expiry date
baseline_dealers$mid_exp <- baseline_dealers$exp_mid #x
baseline_dealers$mid_visible_expdate<-ifelse(!is.na(baseline_dealers$mid_exp),1,0) #x

# sum(is.na(midline_dealers$exp) & !is.na(midline_dealers$reading))
# [1] 0
# meaning that if a seed bag was purchased, there was always an expiry date.

#2. Random seed bag shows packaging date
baseline_dealers$mid_date_pack <- baseline_dealers$date_pack_mid #x
baseline_dealers$mid_visible_packdate<-ifelse(baseline_dealers$mid_date_pack=="n/a",0,1) #x

#3. Days since packaging date/expiry date minus 6 months
baseline_dealers$mid_date <- baseline_dealers$date_mid #x
baseline_dealers$mid_date[baseline_dealers$mid_date=="n/a"] <- NA #x
baseline_dealers$mid_date <- as.Date(baseline_dealers$mid_date) #x

baseline_dealers$mid_exp <- as.Date(baseline_dealers$mid_exp) #x
baseline_dealers$mid_days_since_exp <- baseline_dealers$mid_date - baseline_dealers$mid_exp #x

baseline_dealers$mid_date_pack <- as.Date(baseline_dealers$mid_date_pack) #x
baseline_dealers$mid_shelflife <- baseline_dealers$mid_date - baseline_dealers$mid_date_pack #x

baseline_dealers$mid_date_pack_incltransformedexp <- baseline_dealers$mid_date_pack #x
baseline_dealers$mid_transformedexp <- baseline_dealers$mid_exp - 183 #6x366/12 #x
baseline_dealers$mid_date_pack_incltransformedexp[is.na(baseline_dealers$mid_date_pack)]<-baseline_dealers$mid_transformedexp[is.na(baseline_dealers$mid_date_pack)] #x

baseline_dealers$mid_shelflife_Caro <- baseline_dealers$mid_date - as.Date(baseline_dealers$mid_date_pack_incltransformedexp) #x
baseline_dealers$mid_shelflife_Caro[baseline_dealers$mid_shelflife_Caro < 0] <- NA #x
baseline_dealers$mid_shelflife_Caro <- as.numeric(as.character(baseline_dealers$mid_shelflife_Caro)) #x

baseline_dealers <- trim("mid_shelflife_Caro",baseline_dealers,trim_perc=.02) #x
baseline_dealers <- trim("shelflife_Caro",baseline_dealers,trim_perc=.02)

#4. Random seed bag shows lot number
baseline_dealers$mid_lot <- baseline_dealers$lot_mid
baseline_dealers$mid_lot<-ifelse(baseline_dealers$mid_lot=="Yes",1,0)

#5. Overall index
variables_overall_bag_mid <- cbind(baseline_dealers$mid_visible_packdate,baseline_dealers$mid_shelflife_Caro,baseline_dealers$mid_lot) #x
#no baseline_dealers$mid_visible_expdate because all same value (1)
variables_overall_bag_base <- cbind(baseline_dealers$visible_packdate,baseline_dealers$shelflife_Caro,baseline_dealers$lot)

index_overall_bag_mid <- icwIndex(xmat=variables_overall_bag_mid,revcols = c(2)) #x
baseline_dealers$index_overall_bag_mid <- index_overall_bag_mid$index #x

index_overall_bag_base <- icwIndex(xmat=variables_overall_bag_base,revcols = c(2))
baseline_dealers$index_overall_bag_base <- index_overall_bag_base$index

################################################################################################################################################################################

###
#1#
###

results_dealer_sec_bag <- c("mid_visible_packdate","mid_shelflife_Caro","mid_lot","index_overall_bag_mid","mid_visible_expdate")

results_dealer_sec_bag_base <- c("visible_packdate","shelflife_Caro","lot","index_overall_bag_base","visible_expdate")

baseline_dealers[results_dealer_sec_bag_base] <- lapply(baseline_dealers[results_dealer_sec_bag_base],function(x)x - mean(x,na.rm = T))

df_means_D_sec_bag <- array(NA,dim=c(3,11))

for (i in 1:length(results_dealer_sec_bag)){
  df_means_D_sec_bag[1,i] <- sum(baseline_dealers[results_dealer_sec_bag[i]], na.rm=T)/(nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_sec_bag[i]])))
  df_means_D_sec_bag[2,i] <- sqrt(var(baseline_dealers[results_dealer_sec_bag[i]], na.rm=T))
  df_means_D_sec_bag[3,i] <- nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_sec_bag[i]]))-sum(is.na(baseline_dealers[results_dealer_sec_bag_base[i]]))+sum(is.na(baseline_dealers[results_dealer_sec_bag[i]])&is.na(baseline_dealers[results_dealer_sec_bag_base[i]]))}

###
#2#
###

baseline_dealers$training_control[baseline_dealers$training==0] <- TRUE
baseline_dealers$training_control[baseline_dealers$training==1] <- FALSE

#5.
index_overall_bag_mid <- icwIndex(xmat=variables_overall_bag_mid,sgroup = baseline_dealers$training_control,revcols = c(2))
baseline_dealers$index_overall_bag_midT <- index_overall_bag_mid$index

index_overall_bag_base <- icwIndex(xmat=variables_overall_bag_base,sgroup = baseline_dealers$training_control,revcols = c(2))
baseline_dealers$index_overall_bag_baseT <- index_overall_bag_base$index

df_ols_D_sec_bag <- array(NA,dim=c(3,3,11))

results_dealer_sec_bag <- c("mid_visible_packdate","mid_shelflife_Caro","mid_lot","index_overall_bag_midT","mid_visible_expdate")

results_dealer_sec_bag_base <- c("visible_packdate","shelflife_Caro","lot","index_overall_bag_baseT","visible_expdate")

baseline_dealers[results_dealer_sec_bag_base] <- lapply(baseline_dealers[results_dealer_sec_bag_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_dealer_sec_bag)){
  ols <- lm(as.formula(paste(paste(results_dealer_sec_bag[i],"training*clearing_demeaned*farmer_demeaned",sep="~"),results_dealer_sec_bag_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_sec_bag[i],"training*clearing_demeaned*farmer_demeaned",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  df_ols_D_sec_bag[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_ols_D_sec_bag[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_ols_D_sec_bag[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]}

###
#3#
###

baseline_dealers$clearing_control[baseline_dealers$clearing==0] <- TRUE
baseline_dealers$clearing_control[baseline_dealers$clearing==1] <- FALSE

#5.
index_overall_bag_mid <- icwIndex(xmat=variables_overall_bag_mid,sgroup = baseline_dealers$clearing_control,revcols = c(2))
baseline_dealers$index_overall_bag_midC <- index_overall_bag_mid$index

index_overall_bag_base <- icwIndex(xmat=variables_overall_bag_base,sgroup = baseline_dealers$clearing_control,revcols = c(2))
baseline_dealers$index_overall_bag_baseC <- index_overall_bag_base$index

results_dealer_sec_bag <- c("mid_visible_packdate","mid_shelflife_Caro","mid_lot","index_overall_bag_midC","mid_visible_expdate")

results_dealer_sec_bag_base <- c("visible_packdate","shelflife_Caro","lot","index_overall_bag_baseC","visible_expdate")

baseline_dealers[results_dealer_sec_bag_base] <- lapply(baseline_dealers[results_dealer_sec_bag_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_dealer_sec_bag)){
  ols <- lm(as.formula(paste(paste(results_dealer_sec_bag[i],"training_demeaned*clearing*farmer_demeaned",sep="~"),results_dealer_sec_bag_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_sec_bag[i],"training_demeaned*clearing*farmer_demeaned",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  df_ols_D_sec_bag[1,2,i] <- coef_test(ols, vcov_cluster)$beta[3]
  df_ols_D_sec_bag[2,2,i] <- coef_test(ols, vcov_cluster)$SE[3]
  df_ols_D_sec_bag[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]}

###
#4#
###

baseline_dealers$farmer_control[baseline_dealers$farmer==0] <- TRUE
baseline_dealers$farmer_control[baseline_dealers$farmer==1] <- FALSE

#5.
index_overall_bag_mid <- icwIndex(xmat=variables_overall_bag_mid,sgroup = baseline_dealers$farmer_control,revcols = c(2))
baseline_dealers$index_overall_bag_midF <- index_overall_bag_mid$index

index_overall_bag_base <- icwIndex(xmat=variables_overall_bag_base,sgroup = baseline_dealers$farmer_control,revcols = c(2))
baseline_dealers$index_overall_bag_baseF <- index_overall_bag_base$index

results_dealer_sec_bag <- c("mid_visible_packdate","mid_shelflife_Caro","mid_lot","index_overall_bag_midF","mid_visible_expdate")

results_dealer_sec_bag_base <- c("visible_packdate","shelflife_Caro","lot","index_overall_bag_baseF","visible_expdate")

baseline_dealers[results_dealer_sec_bag_base] <- lapply(baseline_dealers[results_dealer_sec_bag_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_dealer_sec_bag)){
  ols <- lm(as.formula(paste(paste(results_dealer_sec_bag[i],"training_demeaned*clearing_demeaned*farmer",sep="~"),results_dealer_sec_bag_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_sec_bag[i],"training_demeaned*clearing_demeaned*farmer",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  #farmer video treatment at village/shop level so no clustering needed
  df_ols_D_sec_bag[1,3,i] <- summary(ols)$coefficients[4,1]
  df_ols_D_sec_bag[2,3,i] <- summary(ols)$coefficients[4,2]
  df_ols_D_sec_bag[3,3,i] <- summary(ols)$coefficients[4,4]}

#Aker, Boumnijel, McClelland, Tierney (2012)
df_dealer_sec_bagT <- data.frame(baseline_dealers$mid_visible_packdate,baseline_dealers$mid_shelflife_Caro,baseline_dealers$mid_lot)
df_dealer_sec_bagC <- df_dealer_sec_bagT
df_dealer_sec_bagF <- df_dealer_sec_bagT
#no overall index

df_ols_D_sec_bag_J <- array(NA,dim=c(3,3,11))

results_dealer_sec_bag_J <- c("mid_visible_packdate","mid_shelflife_Caro","mid_lot")
#no overall index

for (i in 1:length(results_dealer_sec_bag_J)){
  df_ols_D_sec_bag_J[3,1,i] <- adjust_p(df_ols_D_sec_bag[3,1,i],df_dealer_sec_bagT,i)
  df_ols_D_sec_bag_J[3,2,i] <- adjust_p(df_ols_D_sec_bag[3,2,i],df_dealer_sec_bagC,i)
  df_ols_D_sec_bag_J[3,3,i] <- adjust_p(df_ols_D_sec_bag[3,3,i],df_dealer_sec_bagF,i)}










###################################################################################################################################################################
##### 7B ANALYSIS: Agro-input dealer - Secondary: 11. seed bag, not controlling for baseline ######################################################################
###################################################################################################################################################################

################################################################################################################################################################################

###
#1#
###

results_dealer_sec_bag_B <- c("index_overall_bag_mid")

df_means_D_sec_bag_B <- array(NA,dim=c(3,11))

for (i in 1:length(results_dealer_sec_bag_B)){
  df_means_D_sec_bag_B[1,i] <- sum(baseline_dealers[results_dealer_sec_bag_B[i]], na.rm=T)/(nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_sec_bag_B[i]])))
  df_means_D_sec_bag_B[2,i] <- sqrt(var(baseline_dealers[results_dealer_sec_bag_B[i]], na.rm=T))
  df_means_D_sec_bag_B[3,i] <- nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_sec_bag_B[i]]))}

###
#2#
###

df_ols_D_sec_bag_B <- array(NA,dim=c(3,3,11))

results_dealer_sec_bag_B <- c("index_overall_bag_midT")

for (i in 1:length(results_dealer_sec_bag_B)){
  #ols <- lm(as.formula(paste(paste(results_dealer_sec_bag_B[i],"training*clearing_demeaned*farmer_demeaned",sep="~"),results_dealer_sec_bag_B_base[i],sep="+")),data=baseline_dealers)
  ols <- lm(as.formula(paste(results_dealer_sec_bag_B[i],"training*clearing_demeaned*farmer_demeaned",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  df_ols_D_sec_bag_B[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_ols_D_sec_bag_B[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_ols_D_sec_bag_B[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]}

###
#3#
###

results_dealer_sec_bag_B <- c("index_overall_bag_midC")

for (i in 1:length(results_dealer_sec_bag_B)){
  #ols <- lm(as.formula(paste(paste(results_dealer_sec_bag_B[i],"training_demeaned*clearing*farmer_demeaned",sep="~"),results_dealer_sec_bag_B_base[i],sep="+")),data=baseline_dealers)
  ols <- lm(as.formula(paste(results_dealer_sec_bag_B[i],"training_demeaned*clearing*farmer_demeaned",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  df_ols_D_sec_bag_B[1,2,i] <- coef_test(ols, vcov_cluster)$beta[3]
  df_ols_D_sec_bag_B[2,2,i] <- coef_test(ols, vcov_cluster)$SE[3]
  df_ols_D_sec_bag_B[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]}

###
#4#
###

results_dealer_sec_bag_B <- c("index_overall_bag_midF")

for (i in 1:length(results_dealer_sec_bag_B)){
  #ols <- lm(as.formula(paste(paste(results_dealer_sec_bag_B[i],"training_demeaned*clearing_demeaned*farmer",sep="~"),results_dealer_sec_bag_B_base[i],sep="+")),data=baseline_dealers)
  ols <- lm(as.formula(paste(results_dealer_sec_bag_B[i],"training_demeaned*clearing_demeaned*farmer",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  #farmer video treatment at village/shop level so no clustering needed
  df_ols_D_sec_bag_B[1,3,i] <- summary(ols)$coefficients[4,1]
  df_ols_D_sec_bag_B[2,3,i] <- summary(ols)$coefficients[4,2]
  df_ols_D_sec_bag_B[3,3,i] <- summary(ols)$coefficients[4,4]}










###################################################################################################################################################################
##### 7H ANALYSIS: Agro-input dealer - Secondary: 11. seed bag ####################################################################################################
##### Heterogeneity analyses: Shops that were rated
###################################################################################################################################################################

baseline_dealers=subset(baseline_dealers,notrated=="0")

variables_overall_bag_mid <- cbind(baseline_dealers$mid_visible_packdate,baseline_dealers$mid_shelflife_Caro,baseline_dealers$mid_lot) #x
variables_overall_bag_base <- cbind(baseline_dealers$visible_packdate,baseline_dealers$shelflife_Caro,baseline_dealers$lot)

index_overall_bag_mid <- icwIndex(xmat=variables_overall_bag_mid,revcols = c(2)) #x
baseline_dealers$index_overall_bag_mid <- index_overall_bag_mid$index #x

index_overall_bag_base <- icwIndex(xmat=variables_overall_bag_base,revcols = c(2))
baseline_dealers$index_overall_bag_base <- index_overall_bag_base$index

################################################################################################################################################################################

###
#1#
###

results_dealer_sec_bag <- c("mid_visible_packdate","mid_shelflife_Caro","mid_lot","index_overall_bag_mid","mid_visible_expdate")

results_dealer_sec_bag_base <- c("visible_packdate","shelflife_Caro","lot","index_overall_bag_base","visible_expdate")

baseline_dealers[results_dealer_sec_bag_base] <- lapply(baseline_dealers[results_dealer_sec_bag_base],function(x)x - mean(x,na.rm = T))

df_means_D_sec_bag_het3 <- array(NA,dim=c(3,11))

for (i in 1:length(results_dealer_sec_bag)){
  df_means_D_sec_bag_het3[1,i] <- sum(baseline_dealers[results_dealer_sec_bag[i]], na.rm=T)/(nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_sec_bag[i]])))
  df_means_D_sec_bag_het3[2,i] <- sqrt(var(baseline_dealers[results_dealer_sec_bag[i]], na.rm=T))
  df_means_D_sec_bag_het3[3,i] <- nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_sec_bag[i]]))-sum(is.na(baseline_dealers[results_dealer_sec_bag_base[i]]))+sum(is.na(baseline_dealers[results_dealer_sec_bag[i]])&is.na(baseline_dealers[results_dealer_sec_bag_base[i]]))}

###
#2#
###

baseline_dealers$training_control[baseline_dealers$training==0] <- TRUE
baseline_dealers$training_control[baseline_dealers$training==1] <- FALSE

#5.
index_overall_bag_mid <- icwIndex(xmat=variables_overall_bag_mid,sgroup = baseline_dealers$training_control,revcols = c(2))
baseline_dealers$index_overall_bag_midT <- index_overall_bag_mid$index

index_overall_bag_base <- icwIndex(xmat=variables_overall_bag_base,sgroup = baseline_dealers$training_control,revcols = c(2))
baseline_dealers$index_overall_bag_baseT <- index_overall_bag_base$index

df_ols_D_sec_bag_het3 <- array(NA,dim=c(3,3,11))

results_dealer_sec_bag <- c("mid_visible_packdate","mid_shelflife_Caro","mid_lot","index_overall_bag_midT","mid_visible_expdate")

results_dealer_sec_bag_base <- c("visible_packdate","shelflife_Caro","lot","index_overall_bag_baseT","visible_expdate")

baseline_dealers[results_dealer_sec_bag_base] <- lapply(baseline_dealers[results_dealer_sec_bag_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_dealer_sec_bag)){
  ols <- lm(as.formula(paste(paste(results_dealer_sec_bag[i],"training*clearing_demeaned*farmer_demeaned",sep="~"),results_dealer_sec_bag_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_sec_bag[i],"training*clearing_demeaned*farmer_demeaned",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  df_ols_D_sec_bag_het3[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_ols_D_sec_bag_het3[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_ols_D_sec_bag_het3[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]}

###
#3#
###

baseline_dealers$clearing_control[baseline_dealers$clearing==0] <- TRUE
baseline_dealers$clearing_control[baseline_dealers$clearing==1] <- FALSE

#5.
index_overall_bag_mid <- icwIndex(xmat=variables_overall_bag_mid,sgroup = baseline_dealers$clearing_control,revcols = c(2))
baseline_dealers$index_overall_bag_midC <- index_overall_bag_mid$index

index_overall_bag_base <- icwIndex(xmat=variables_overall_bag_base,sgroup = baseline_dealers$clearing_control,revcols = c(2))
baseline_dealers$index_overall_bag_baseC <- index_overall_bag_base$index

results_dealer_sec_bag <- c("mid_visible_packdate","mid_shelflife_Caro","mid_lot","index_overall_bag_midC","mid_visible_expdate")

results_dealer_sec_bag_base <- c("visible_packdate","shelflife_Caro","lot","index_overall_bag_baseC","visible_expdate")

baseline_dealers[results_dealer_sec_bag_base] <- lapply(baseline_dealers[results_dealer_sec_bag_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_dealer_sec_bag)){
  ols <- lm(as.formula(paste(paste(results_dealer_sec_bag[i],"training_demeaned*clearing*farmer_demeaned",sep="~"),results_dealer_sec_bag_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_sec_bag[i],"training_demeaned*clearing*farmer_demeaned",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  df_ols_D_sec_bag_het3[1,2,i] <- coef_test(ols, vcov_cluster)$beta[3]
  df_ols_D_sec_bag_het3[2,2,i] <- coef_test(ols, vcov_cluster)$SE[3]
  df_ols_D_sec_bag_het3[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]}

###
#4#
###

baseline_dealers$farmer_control[baseline_dealers$farmer==0] <- TRUE
baseline_dealers$farmer_control[baseline_dealers$farmer==1] <- FALSE

#5.
index_overall_bag_mid <- icwIndex(xmat=variables_overall_bag_mid,sgroup = baseline_dealers$farmer_control,revcols = c(2))
baseline_dealers$index_overall_bag_midF <- index_overall_bag_mid$index

index_overall_bag_base <- icwIndex(xmat=variables_overall_bag_base,sgroup = baseline_dealers$farmer_control,revcols = c(2))
baseline_dealers$index_overall_bag_baseF <- index_overall_bag_base$index

results_dealer_sec_bag <- c("mid_visible_packdate","mid_shelflife_Caro","mid_lot","index_overall_bag_midF","mid_visible_expdate")

results_dealer_sec_bag_base <- c("visible_packdate","shelflife_Caro","lot","index_overall_bag_baseF","visible_expdate")

baseline_dealers[results_dealer_sec_bag_base] <- lapply(baseline_dealers[results_dealer_sec_bag_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_dealer_sec_bag)){
  ols <- lm(as.formula(paste(paste(results_dealer_sec_bag[i],"training_demeaned*clearing_demeaned*farmer",sep="~"),results_dealer_sec_bag_base[i],sep="+")),data=baseline_dealers)
  #ols <- lm(as.formula(paste(results_dealer_sec_bag[i],"training_demeaned*clearing_demeaned*farmer",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  #farmer video treatment at village/shop level so no clustering needed
  df_ols_D_sec_bag_het3[1,3,i] <- summary(ols)$coefficients[4,1]
  df_ols_D_sec_bag_het3[2,3,i] <- summary(ols)$coefficients[4,2]
  df_ols_D_sec_bag_het3[3,3,i] <- summary(ols)$coefficients[4,4]}

#Aker, Boumnijel, McClelland, Tierney (2012)
df_dealer_sec_bagT <- data.frame(baseline_dealers$mid_visible_packdate,baseline_dealers$mid_shelflife_Caro,baseline_dealers$mid_lot)
df_dealer_sec_bagC <- df_dealer_sec_bagT
df_dealer_sec_bagF <- df_dealer_sec_bagT
#no overall index

df_ols_D_sec_bag_het3_J <- array(NA,dim=c(3,3,11))

results_dealer_sec_bag_J <- c("mid_visible_packdate","mid_shelflife_Caro","mid_lot")
#no overall index

for (i in 1:length(results_dealer_sec_bag_J)){
  df_ols_D_sec_bag_het3_J[3,1,i] <- adjust_p(df_ols_D_sec_bag_het3[3,1,i],df_dealer_sec_bagT,i)
  df_ols_D_sec_bag_het3_J[3,2,i] <- adjust_p(df_ols_D_sec_bag_het3[3,2,i],df_dealer_sec_bagC,i)
  df_ols_D_sec_bag_het3_J[3,3,i] <- adjust_p(df_ols_D_sec_bag_het3[3,3,i],df_dealer_sec_bagF,i)}

baseline_dealers=baseline_dealers_save










################################################################################################################################################################################
##### 2 ANALYSIS: Agro-input dealer - Primary and secondary: outcomes without baseline######################################################################################################
################################################################################################################################################################################

#1. Index of dealer's motivation and satisfaction
#Do you see yourself working as an agro-input dealer 3 years from now? (q9a): yes=good
baseline_dealers$mid_maize.owner.agree.q9_a <- baseline_dealers$owner.agree.client.q9a #x
baseline_dealers$mid_maize.owner.agree.q9_a<-ifelse(baseline_dealers$mid_maize.owner.agree.q9_a=="Yes",1,0) #x

# #Do you think your job makes a positive difference in other's life? (q9b): yes=good
# baseline_dealers$mid_maize.owner.agree.q9.b <- baseline_dealers$owner.agree.client.q9b #x
# baseline_dealers$mid_maize.owner.agree.q9.b<-ifelse(baseline_dealers$mid_maize.owner.agree.q9.b=="Yes",1,0) #x
# 
# table(baseline_dealers$mid_maize.owner.agree.q9.b)
# 0   1 
# 4 302 

#On a scale of 1 to 5, how likely are you to recommend working as an agro-input dealer to friends or family? (q9c) more=better
baseline_dealers$mid_maize.owner.agree.q9_c <- baseline_dealers$owner.agree.client.q9c #x

#On a scale of 1 to 5, how happy do you feel when you come to work in the morning? (q9d) more=better
baseline_dealers$mid_maize.owner.agree.q9_d <- baseline_dealers$owner.agree.client.q9d #x

variables_motivation_mid <- cbind(baseline_dealers$mid_maize.owner.agree.q9_a,baseline_dealers$mid_maize.owner.agree.q9.b
                                  ,baseline_dealers$mid_maize.owner.agree.q9_c,baseline_dealers$mid_maize.owner.agree.q9_d) #x

index_motivation_mid <- icwIndex(xmat=variables_motivation_mid) #x
baseline_dealers$index_motivation_mid <- index_motivation_mid$index #x

#2. Index of shop's maize seed ratings by farmers
#NOT IN MIDLINE

nrowD <- nrow(baseline_dealers)

baseline_dealers$mid_general=sample(na.omit(baseline_dealers$general),nrowD,replace = T)

baseline_dealers$mid_yield=sample(na.omit(baseline_dealers$yield),nrowD,replace = T)

baseline_dealers$mid_drought_resistent=sample(na.omit(baseline_dealers$drought_resistent),nrowD,replace = T)

baseline_dealers$mid_disease_resistent=sample(na.omit(baseline_dealers$disease_resistent),nrowD,replace = T)

baseline_dealers$mid_early_maturing=sample(na.omit(baseline_dealers$early_maturing),nrowD,replace = T)

baseline_dealers$mid_germination=sample(na.omit(baseline_dealers$germination),nrowD,replace = T)

variables_ratings_mid <- cbind(baseline_dealers$mid_general,baseline_dealers$mid_yield,baseline_dealers$mid_drought_resistent
                               ,baseline_dealers$mid_disease_resistent,baseline_dealers$mid_early_maturing,baseline_dealers$mid_germination)

#2.
index_ratings_mid <- icwIndex(xmat=variables_ratings_mid)
baseline_dealers$index_ratings_mid <- index_ratings_mid$index

#3. new index_overall_prim_dealer_mid incl. ratings
variables_overall_prim_dealer_mid <- cbind(baseline_dealers$mid_quantitysold,baseline_dealers$mid_revenue
                                           ,baseline_dealers$mid_maize.owner.agree.q7
                                           
                                           ,baseline_dealers$index_practices_cap_mid,baseline_dealers$index_practices_lab_mid
                                           ,baseline_dealers$index_efforts_mid) #x
#                                           ,baseline_dealers$index_ratings_mid) #to do after endline
# no ,baseline_dealers$mid_reading bc nobs at midline

index_overall_prim_dealer_mid <- icwIndex(xmat=variables_overall_prim_dealer_mid) #x
baseline_dealers$index_overall_prim_dealer_mid <- index_overall_prim_dealer_mid$index #x

################################################################################################################################################################################

###
#1#
###

results_dealer_sec_nobase <- c("index_motivation_mid","index_ratings_mid"
                               ,"index_overall_prim_dealer_mid","index_overallsec_mid"
                               ,"index_overall_off_mid","mid_reading")

df_means_D_sec_nobase <- array(NA,dim=c(3,10))

for (i in 1:length(results_dealer_sec_nobase)){
  df_means_D_sec_nobase[1,i] <- sum(baseline_dealers[results_dealer_sec_nobase[i]], na.rm=T)/(nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_sec_nobase[i]])))
  df_means_D_sec_nobase[2,i] <- sqrt(var(baseline_dealers[results_dealer_sec_nobase[i]], na.rm=T))
  df_means_D_sec_nobase[3,i] <- nrow(baseline_dealers)-sum(is.na(baseline_dealers[results_dealer_sec_nobase[i]]))}

save(df_means_D_sec_nobase,file=paste(path,"papers/clearinghouse_training_paper/output_CH_training/df_means_D_sec_nobase.Rdata",sep="/"))

###
#2#
###

df_ols_D_sec_nobase <- array(NA,dim=c(3,3,10))

baseline_dealers$training_control[baseline_dealers$training==0] <- TRUE
baseline_dealers$training_control[baseline_dealers$training==1] <- FALSE

#1.
index_motivation_mid <- icwIndex(xmat=variables_motivation_mid,sgroup = baseline_dealers$training_control)
baseline_dealers$index_motivation_midT <- index_motivation_mid$index

#2.
index_ratings_mid <- icwIndex(xmat=variables_ratings_mid,sgroup = baseline_dealers$training_control)
baseline_dealers$index_ratings_midT <- index_ratings_mid$index

#3.
index_overall_prim_dealer_mid <- icwIndex(xmat=variables_overall_prim_dealer_mid,sgroup = baseline_dealers$training_control)
baseline_dealers$index_overall_prim_dealer_midT <- index_overall_prim_dealer_mid$index

results_dealer_sec_nobase <- c("index_motivation_midT","index_ratings_midT"
                               ,"index_overall_prim_dealer_midT","index_overallsec_midT"
                               ,"index_overall_off_midT","mid_reading")

for (i in 1:length(results_dealer_sec_nobase)){
  ols <- lm(as.formula(paste(results_dealer_sec_nobase[i],"training*clearing_demeaned*farmer_demeaned",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  df_ols_D_sec_nobase[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_ols_D_sec_nobase[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_ols_D_sec_nobase[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]}

###
#3#
###

baseline_dealers$clearing_control[baseline_dealers$clearing==0] <- TRUE
baseline_dealers$clearing_control[baseline_dealers$clearing==1] <- FALSE

#1.
index_motivation_mid <- icwIndex(xmat=variables_motivation_mid,sgroup = baseline_dealers$clearing_control)
baseline_dealers$index_motivation_midC <- index_motivation_mid$index

#2.
index_ratings_mid <- icwIndex(xmat=variables_ratings_mid,sgroup = baseline_dealers$clearing_control)
baseline_dealers$index_ratings_midC <- index_ratings_mid$index

#3.
index_overall_prim_dealer_mid <- icwIndex(xmat=variables_overall_prim_dealer_mid,sgroup = baseline_dealers$clearing_control)
baseline_dealers$index_overall_prim_dealer_midC <- index_overall_prim_dealer_mid$index

results_dealer_sec_nobase <- c("index_motivation_midC","index_ratings_midC"
                               ,"index_overall_prim_dealer_midC","index_overallsec_midC"
                               ,"index_overall_off_midC","mid_reading")

for (i in 1:length(results_dealer_sec_nobase)){
  ols <- lm(as.formula(paste(results_dealer_sec_nobase[i],"training_demeaned*clearing*farmer_demeaned",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  df_ols_D_sec_nobase[1,2,i] <- coef_test(ols, vcov_cluster)$beta[3]
  df_ols_D_sec_nobase[2,2,i] <- coef_test(ols, vcov_cluster)$SE[3]
  df_ols_D_sec_nobase[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]}

###
#4#
###

baseline_dealers$farmer_control[baseline_dealers$farmer==0] <- TRUE
baseline_dealers$farmer_control[baseline_dealers$farmer==1] <- FALSE

#1.
index_motivation_mid <- icwIndex(xmat=variables_motivation_mid,sgroup = baseline_dealers$farmer_control)
baseline_dealers$index_motivation_midF <- index_motivation_mid$index

#2.
index_ratings_mid <- icwIndex(xmat=variables_ratings_mid,sgroup = baseline_dealers$farmer_control)
baseline_dealers$index_ratings_midF <- index_ratings_mid$index

#3.
index_overall_prim_dealer_mid <- icwIndex(xmat=variables_overall_prim_dealer_mid,sgroup = baseline_dealers$farmer_control)
baseline_dealers$index_overall_prim_dealer_midF <- index_overall_prim_dealer_mid$index

results_dealer_sec_nobase <- c("index_motivation_midF","index_ratings_midF"
                               ,"index_overall_prim_dealer_midF","index_overallsec_midF"
                               ,"index_overall_off_midF","mid_reading")

for (i in 1:length(results_dealer_sec_nobase)){
  ols <- lm(as.formula(paste(results_dealer_sec_nobase[i],"training_demeaned*clearing_demeaned*farmer",sep="~")),data=baseline_dealers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dealers$catchID,type="CR3")
  
  #farmer video treatment at village/shop level so no clustering needed
  df_ols_D_sec_nobase[1,3,i] <- summary(ols)$coefficients[4,1]
  df_ols_D_sec_nobase[2,3,i] <- summary(ols)$coefficients[4,2]
  df_ols_D_sec_nobase[3,3,i] <- summary(ols)$coefficients[4,4]}

save(df_ols_D_sec_nobase,file=paste(path,"papers/clearinghouse_training_paper/output_CH_training/df_ols_D_sec_nobase.Rdata",sep="/"))










################################################################################################################################################################################
##### 8 ANALYSIS: Farmer - Primary##############################################################################################################################################
################################################################################################################################################################################

#Heterogeneity analyses
#2: Larger catchment areas
baseline_farmers$small_catchID <- ifelse(baseline_farmers$catchID==16|baseline_farmers$catchID==18|baseline_farmers$catchID==19|
                                           baseline_farmers$catchID==33|baseline_farmers$catchID==34|baseline_farmers$catchID==36|
                                           baseline_farmers$catchID==42|baseline_farmers$catchID==45|baseline_farmers$catchID==46|
                                           baseline_farmers$catchID==48|baseline_farmers$catchID==53|baseline_farmers$catchID==63|
                                           baseline_farmers$catchID==65|baseline_farmers$catchID==66|baseline_farmers$catchID==67|
                                           baseline_farmers$catchID==73|baseline_farmers$catchID==79|baseline_farmers$catchID==80|
                                           baseline_farmers$catchID==87|baseline_farmers$catchID==89|baseline_farmers$catchID==90|
                                           baseline_farmers$catchID==91|baseline_farmers$catchID==92|baseline_farmers$catchID==93|
                                           baseline_farmers$catchID==95|baseline_farmers$catchID==98|baseline_farmers$catchID==101|
                                           baseline_farmers$catchID==103|baseline_farmers$catchID==106|baseline_farmers$catchID==107|
                                           baseline_farmers$catchID==108|baseline_farmers$catchID==109|baseline_farmers$catchID==110|
                                           baseline_farmers$catchID==112|baseline_farmers$catchID==116|baseline_farmers$catchID==118|
                                           baseline_farmers$catchID==120|baseline_farmers$catchID==121|baseline_farmers$catchID==122|
                                           baseline_farmers$catchID==124|baseline_farmers$catchID==125|baseline_farmers$catchID==126|
                                           baseline_farmers$catchID==127|baseline_farmers$catchID==128|baseline_farmers$catchID==129|
                                           baseline_farmers$catchID==130,1,0)
#baseline_farmers=subset(baseline_farmers,small_catchID=="0")

#3: Less competitive catchment areas
baseline_farmers$large_catchID <- ifelse(baseline_farmers$catchID==3|baseline_farmers$catchID==32|baseline_farmers$catchID==59,1,0)
# |baseline_farmers$catchID==64
#baseline_farmers=subset(baseline_farmers,large_catchID=="0")



baseline_farmers[baseline_farmers==999] <- NA
#baseline_farmers[baseline_farmers==96] <- NA
baseline_farmers[, 4:270][baseline_farmers[, 4:270] == 96] <- NA #columns 4-246 only
#baseline_farmers[baseline_farmers==98] <- NA
baseline_farmers[, 4:270][baseline_farmers[, 4:270] == 98] <- NA
baseline_farmers[baseline_farmers=="n/a"] <- NA

#1. Q25a. Did you use any quality maize seed like **OPV or hybrid in **seed  the second season of **2020 (entoigo 2020)** for any of your plots?
baseline_farmers$mid_Check2.check.maize.q25a <- baseline_farmers$check.maize.q25a
baseline_farmers$mid_Check2.check.maize.q25a<-ifelse(baseline_farmers$mid_Check2.check.maize.q25a=="Yes",1,0)

#2. q25b. Where did you obtain the maize seed used in the second season of **2020 (entoigo 2020)** on any of your plots?
baseline_farmers$agro <- ifelse(baseline_farmers$Check2.check.maize.q25b=="d",1,0)
baseline_farmers$agro[is.na(baseline_farmers$Check2.check.maize.q25b)] <- NA
baseline_farmers$agro[baseline_farmers$Check2.check.maize.q25a==0] = 0

baseline_farmers$mid_agro <- ifelse(baseline_farmers$check.maize.q25b=="d",1,0)
baseline_farmers$mid_agro[is.na(baseline_farmers$check.maize.q25b)] <- NA
baseline_farmers$mid_agro[baseline_farmers$check.maize.q25a=="No"] = 0

#3. Q25d. How much quality maize seed (hybrid or OPV) did you buy from an input dealer in the second agricultural season of 2020? Record amount in **KG**
#baseline_farmers$Check2.check.maize.q25d[baseline_farmers$agro==0] = 0
baseline_farmers$Check2.check.maize.q25d <- ihs(baseline_farmers$Check2.check.maize.q25d)

baseline_farmers <- trim("Check2.check.maize.q25d",baseline_farmers,trim_perc=.05)

#email to Bjorn (08/04) about farmer q25d 

baseline_farmers$mid_Check2.check.maize.q25d <- baseline_farmers$check.maize.q25d
baseline_farmers$mid_Check2.check.maize.q25d <- as.numeric((as.character(baseline_farmers$mid_Check2.check.maize.q25d)))
#baseline_farmers$mid_Check2.check.maize.q25d[baseline_farmers$mid_agro==0] = 0
baseline_farmers$mid_Check2.check.maize.q25d <- ihs(baseline_farmers$mid_Check2.check.maize.q25d)
baseline_farmers <- trim("mid_Check2.check.maize.q25d",baseline_farmers,trim_perc=.05)

#4. services
variables_servicesF_mid <- cbind(baseline_farmers$mid_refunds,baseline_farmers$mid_gives_credit,baseline_farmers$mid_gives_advice
                                 ,baseline_farmers$mid_delivers,baseline_farmers$mid_after_sales_service,baseline_farmers$mid_payment_mehtods
                                 ,baseline_farmers$mid_small_quant)
variables_servicesF_base <- cbind(baseline_farmers$refunds,baseline_farmers$gives_credit,baseline_farmers$gives_advice
                                 ,baseline_farmers$delivers,baseline_farmers$after_sales_service,baseline_farmers$payment_mehtods
                                 ,baseline_farmers$small_quant)

index_servicesF_mid <- icwIndex(xmat=variables_servicesF_mid)
baseline_farmers$index_servicesF_mid <- index_servicesF_mid$index

index_servicesF_base <- icwIndex(xmat=variables_servicesF_base)
baseline_farmers$index_servicesF_base <- index_servicesF_base$index

#5. practices
#q40 (Wilber: For q40, options b and c are technically correct. b if 2 seeds per hill are used and c if 1 seed per hill is used.)
baseline_farmers$correct_q40 <- 0
baseline_farmers$correct_q40[baseline_farmers$Check2.check.maize.q40=="b"] <- 1
baseline_farmers$correct_q40[baseline_farmers$Check2.check.maize.q40=="c"] <- 1

baseline_farmers$mid_Check2.check.maize.q40 <- baseline_farmers$check.maize.q40
baseline_farmers$mid_correct_q40 <- 0
baseline_farmers$mid_correct_q40[baseline_farmers$mid_Check2.check.maize.q40=="b"] <- 1
baseline_farmers$mid_correct_q40[baseline_farmers$mid_Check2.check.maize.q40=="c"] <- 1

#q41 (Wilber: The same applies to q41.)
baseline_farmers$correct_q41 <- ifelse(baseline_farmers$Check2.check.maize.q41=="1",1,0)
baseline_farmers$correct_q41[baseline_farmers$Check2.check.maize.q41=="2"] <- 1

baseline_farmers$mid_Check2.check.maize.q41 <- baseline_farmers$check.maize.q41
baseline_farmers$mid_Check2.check.maize.q41 <- as.numeric(as.character(baseline_farmers$mid_Check2.check.maize.q41))
baseline_farmers$mid_correct_q41 <- ifelse(baseline_farmers$mid_Check2.check.maize.q41=="1",1,0)
baseline_farmers$mid_correct_q41[baseline_farmers$mid_Check2.check.maize.q41=="2"] <- 1

#q42
baseline_farmers$correct_q42 <- NA
baseline_farmers$correct_q42[baseline_farmers$Check2.check.maize.q42=="1"] <- 1
baseline_farmers$correct_q42[baseline_farmers$Check2.check.maize.q42=="0"] <- 0

baseline_farmers$mid_Check2.check.maize.q42 <- baseline_farmers$check.maize.q42
baseline_farmers$mid_Check2.check.maize.q42<-ifelse(baseline_farmers$mid_Check2.check.maize.q42=="Yes",1,0)
baseline_farmers$mid_correct_q42 <- NA
baseline_farmers$mid_correct_q42[baseline_farmers$mid_Check2.check.maize.q42=="1"] <- 1
baseline_farmers$mid_correct_q42[baseline_farmers$mid_Check2.check.maize.q42=="0"] <- 0

#q43
baseline_farmers$correct_q43 <- NA
baseline_farmers$correct_q43[baseline_farmers$Check2.check.maize.q43=="1"] <- 1
baseline_farmers$correct_q43[baseline_farmers$Check2.check.maize.q43=="0"] <- 0

baseline_farmers$mid_Check2.check.maize.q43 <- baseline_farmers$check.maize.q43
baseline_farmers$mid_Check2.check.maize.q43<-ifelse(baseline_farmers$mid_Check2.check.maize.q43=="Yes",1,0)
baseline_farmers$mid_correct_q43 <- NA
baseline_farmers$mid_correct_q43[baseline_farmers$mid_Check2.check.maize.q43=="1"] <- 1
baseline_farmers$mid_correct_q43[baseline_farmers$mid_Check2.check.maize.q43=="0"] <- 0

#q44
baseline_farmers$correct_q44 <- NA
baseline_farmers$correct_q44[baseline_farmers$Check2.check.maize.q44=="1"] <- 1
baseline_farmers$correct_q44[baseline_farmers$Check2.check.maize.q44=="0"] <- 0

baseline_farmers$mid_Check2.check.maize.q44 <- baseline_farmers$check.maize.q44
baseline_farmers$mid_Check2.check.maize.q44<-ifelse(baseline_farmers$mid_Check2.check.maize.q44=="Yes",1,0)
baseline_farmers$mid_Check2.check.maize.q44 <- as.numeric(as.character(baseline_farmers$mid_Check2.check.maize.q44))
baseline_farmers$mid_correct_q44 <- NA
baseline_farmers$mid_correct_q44[baseline_farmers$mid_Check2.check.maize.q44=="1"] <- 1
baseline_farmers$mid_correct_q44[baseline_farmers$mid_Check2.check.maize.q44=="0"] <- 0

#q45
baseline_farmers$correct_q45[baseline_farmers$Check2.check.maize.q45>=3] <- 1
baseline_farmers$correct_q45[baseline_farmers$Check2.check.maize.q45<3] <- 0

baseline_farmers$mid_Check2.check.maize.q45 <- baseline_farmers$check.maize.q45
baseline_farmers$mid_Check2.check.maize.q45 <- as.numeric(as.character(baseline_farmers$mid_Check2.check.maize.q45))
baseline_farmers$mid_correct_q45[baseline_farmers$mid_Check2.check.maize.q45>=3] <- 1
baseline_farmers$mid_correct_q45[baseline_farmers$mid_Check2.check.maize.q45<3] <- 0

#q46
baseline_farmers$correct_q46 <- (baseline_farmers$Check2.check.maize.q46 <= 20)
baseline_farmers$correct_q46<-ifelse(baseline_farmers$correct_q46=="TRUE",1,0)

baseline_farmers$mid_Check2.check.maize.q46 <- baseline_farmers$check.maize.q46
baseline_farmers$mid_Check2.check.maize.q46 <- (as.numeric(as.character(baseline_farmers$mid_Check2.check.maize.q46)))
baseline_farmers$mid_correct_q46 <- (baseline_farmers$mid_Check2.check.maize.q46 <= 20)
baseline_farmers$mid_correct_q46<-ifelse(baseline_farmers$mid_correct_q46=="TRUE",1,0)

#q47
baseline_farmers$correct_q47 <- NA
baseline_farmers$correct_q47[baseline_farmers$Check2.check.maize.q47=="1"] <- 1
baseline_farmers$correct_q47[baseline_farmers$Check2.check.maize.q47=="0"] <- 0

baseline_farmers$mid_Check2.check.maize.q47 <- baseline_farmers$check.maize.q47
baseline_farmers$mid_Check2.check.maize.q47<-ifelse(baseline_farmers$mid_Check2.check.maize.q47=="Yes",1,0)
baseline_farmers$mid_correct_q47 <- NA
baseline_farmers$mid_correct_q47[baseline_farmers$mid_Check2.check.maize.q47=="1"] <- 1
baseline_farmers$mid_correct_q47[baseline_farmers$mid_Check2.check.maize.q47=="0"] <- 0

#q48
baseline_farmers$correct_q48 <- ifelse(baseline_farmers$Check2.check.maize.q48=="2",1,0)
baseline_farmers$correct_q48[is.na(baseline_farmers$Check2.check.maize.q48)] <- NA

baseline_farmers$mid_Check2.check.maize.q48 <- baseline_farmers$check.maize.q48
baseline_farmers$mid_correct_q48 <- ifelse(baseline_farmers$mid_Check2.check.maize.q48=="2",1,0)
baseline_farmers$mid_correct_q48[is.na(baseline_farmers$mid_Check2.check.maize.q48)] <- NA


#q49
baseline_farmers$correct_q49 <- NA
baseline_farmers$correct_q49[baseline_farmers$Check2.check.maize.q49=="1"] <- 1
baseline_farmers$correct_q49[baseline_farmers$Check2.check.maize.q49=="0"] <- 0

baseline_farmers$mid_Check2.check.maize.q49 <- baseline_farmers$check.maize.q49
baseline_farmers$mid_Check2.check.maize.q49<-ifelse(baseline_farmers$mid_Check2.check.maize.q49=="Yes",1,0)
baseline_farmers$mid_correct_q49 <- NA
baseline_farmers$mid_correct_q49[baseline_farmers$mid_Check2.check.maize.q49=="1"] <- 1
baseline_farmers$mid_correct_q49[baseline_farmers$mid_Check2.check.maize.q49=="0"] <- 0

variables_practices_mid <- cbind(baseline_farmers$mid_correct_q40,baseline_farmers$mid_correct_q41,baseline_farmers$mid_correct_q42
                                 ,baseline_farmers$mid_correct_q43,baseline_farmers$mid_correct_q44,baseline_farmers$mid_correct_q45
                                 ,baseline_farmers$mid_correct_q46,baseline_farmers$mid_correct_q47,baseline_farmers$mid_correct_q48
                                 ,baseline_farmers$mid_correct_q49)
variables_practices_base <- cbind(baseline_farmers$correct_q40,baseline_farmers$correct_q41,baseline_farmers$correct_q42
                                  ,baseline_farmers$correct_q43,baseline_farmers$correct_q44,baseline_farmers$correct_q45
                                  ,baseline_farmers$correct_q46,baseline_farmers$correct_q47,baseline_farmers$correct_q48
                                  ,baseline_farmers$correct_q49)

index_practices_mid <- icwIndex(xmat=variables_practices_mid)
baseline_farmers$index_practices_mid <- index_practices_mid$index

index_practices_base <- icwIndex(xmat=variables_practices_base)
baseline_farmers$index_practices_base <- index_practices_base$index

#Q25h. Do you think that maize seed that you can buy at agro-input dealer is counterfeit/adulterated? NEW ANSWER OPTION
baseline_farmers$mid_Check2.check.maize.q25h <- baseline_farmers$check.maize.q25h
baseline_farmers$mid_Check2.check.maize.q25h<-ifelse(baseline_farmers$mid_Check2.check.maize.q25h=="Yes",1,0)

#farmer saved
baseline_farmers$mid_Check2.check.maize.q31 <- baseline_farmers$check.maize.q31
baseline_farmers$mid_Land_Races<-ifelse(baseline_farmers$mid_Check2.check.maize.q31=="Land_Races",1,0)

#6. overall index
variables_overallprimF_mid <- cbind(baseline_farmers$mid_Check2.check.maize.q25a,baseline_farmers$mid_agro
                                 ,baseline_farmers$mid_Check2.check.maize.q25d,baseline_farmers$index_practices_mid
                                 ,baseline_farmers$mid_Check2.check.maize.q25h,baseline_farmers$mid_Land_Races)
variables_overallprimF_base <- cbind(baseline_farmers$Check2.check.maize.q25a,baseline_farmers$agro
                                  ,baseline_farmers$Check2.check.maize.q25d,baseline_farmers$index_practices_base
                                  ,baseline_farmers$Check2.check.maize.q25h,baseline_farmers$Land_Races)

index_overallprimF_mid <- icwIndex(xmat=variables_overallprimF_mid,revcols = c(5,6))
baseline_farmers$index_overallprimF_mid <- index_overallprimF_mid$index

index_overallprimF_base <- icwIndex(xmat=variables_overallprimF_base,revcols = c(5,6))
baseline_farmers$index_overallprimF_base <- index_overallprimF_base$index

################################################################################################################################################################################

###
#1#
###

results_farmer_prim <- c("mid_Check2.check.maize.q25a","mid_agro","mid_Check2.check.maize.q25d"
                         ,"index_servicesF_mid","index_practices_mid","mid_Check2.check.maize.q25h"
                         ,"mid_Land_Races","index_overallprimF_mid")

results_farmer_prim_base <- c("Check2.check.maize.q25a","agro","Check2.check.maize.q25d"
                              ,"index_servicesF_base","index_practices_base","Check2.check.maize.q25h"
                              ,"Land_Races","index_overallprimF_base")

baseline_farmers[results_farmer_prim_base] <- lapply(baseline_farmers[results_farmer_prim_base],function(x)x - mean(x,na.rm = T))

df_means_F_prim <- array(NA,dim=c(3,8))

for (i in 1:length(results_farmer_prim)){
  df_means_F_prim[1,i] <- sum(baseline_farmers[results_farmer_prim[i]], na.rm=T)/(nrow(baseline_farmers)-sum(is.na(baseline_farmers[results_farmer_prim[i]])))
  df_means_F_prim[2,i] <- sqrt(var(baseline_farmers[results_farmer_prim[i]], na.rm=T))
  df_means_F_prim[3,i] <- nrow(baseline_farmers)-sum(is.na(baseline_farmers[results_farmer_prim[i]]))-sum(is.na(baseline_farmers[results_farmer_prim_base[i]]))+sum(is.na(baseline_farmers[results_farmer_prim[i]])&is.na(baseline_farmers[results_farmer_prim_base[i]]))}

###
#2#
###

baseline_farmers$training_control[baseline_farmers$training==0] <- TRUE
baseline_farmers$training_control[baseline_farmers$training==1] <- FALSE

#4.
index_servicesF_midT <- icwIndex(xmat=variables_servicesF_mid,sgroup = baseline_farmers$training_control)
baseline_farmers$index_servicesF_midT <- index_servicesF_midT$index

index_servicesF_baseT <- icwIndex(xmat=variables_servicesF_base,sgroup = baseline_farmers$training_control)
baseline_farmers$index_servicesF_baseT <- index_servicesF_baseT$index

#5.
index_practices_midT <- icwIndex(xmat=variables_practices_mid,sgroup = baseline_farmers$training_control)
baseline_farmers$index_practices_midT <- index_practices_midT$index

index_practices_baseT <- icwIndex(xmat=variables_practices_base,sgroup = baseline_farmers$training_control)
baseline_farmers$index_practices_baseT <- index_practices_baseT$index

#6.
index_overallprimF_midT <- icwIndex(xmat=variables_overallprimF_mid,sgroup = baseline_farmers$training_control,revcols = c(5,6))
baseline_farmers$index_overallprimF_midT <- index_overallprimF_midT$index

index_overallprimF_baseT <- icwIndex(xmat=variables_overallprimF_base,sgroup = baseline_farmers$training_control,revcols = c(5,6))
baseline_farmers$index_overallprimF_baseT <- index_overallprimF_baseT$index

df_ols_F_prim <- array(NA,dim=c(3,3,11))

results_farmer_prim <- c("mid_Check2.check.maize.q25a","mid_agro","mid_Check2.check.maize.q25d"
                         ,"index_servicesF_midT","index_practices_midT","mid_Check2.check.maize.q25h"
                         ,"mid_Land_Races","index_overallprimF_midT")

results_farmer_prim_base <- c("Check2.check.maize.q25a","agro","Check2.check.maize.q25d"
                              ,"index_servicesF_baseT","index_practices_baseT","Check2.check.maize.q25h"
                              ,"Land_Races","index_overallprimF_baseT")

baseline_farmers[results_farmer_prim_base] <- lapply(baseline_farmers[results_farmer_prim_base],function(x)x - mean(x,na.rm = T))

baseline_farmers$training_demeaned <- baseline_farmers$training - mean(baseline_farmers$training,na.rm = T)
baseline_farmers$clearing_demeaned <- baseline_farmers$clearing - mean(baseline_farmers$clearing,na.rm = T)
baseline_farmers$farmer_demeaned <- baseline_farmers$farmer - mean(baseline_farmers$farmer,na.rm = T)

for (i in 1:length(results_farmer_prim)){
  ols <- lm(as.formula(paste(paste(results_farmer_prim[i],"training*clearing_demeaned*farmer_demeaned",sep="~"),results_farmer_prim_base[i],sep="+")),data=baseline_farmers)
  #ols <- lm(as.formula(paste(results_farmer_prim[i],"training*clearing_demeaned*farmer_demeaned",sep="~")),data=baseline_farmers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_farmers$catchID,type="CR0")

  df_ols_F_prim[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_ols_F_prim[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_ols_F_prim[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]}

###
#3#
###

baseline_farmers$clearing_control[baseline_farmers$clearing==0] <- TRUE
baseline_farmers$clearing_control[baseline_farmers$clearing==1] <- FALSE

#4.
index_servicesF_midC <- icwIndex(xmat=variables_servicesF_mid,sgroup = baseline_farmers$clearing_control)
baseline_farmers$index_servicesF_midC <- index_servicesF_midC$index

index_servicesF_baseC <- icwIndex(xmat=variables_servicesF_base,sgroup = baseline_farmers$clearing_control)
baseline_farmers$index_servicesF_baseC <- index_servicesF_baseC$index

#5.
index_practices_midC <- icwIndex(xmat=variables_practices_mid,sgroup = baseline_farmers$clearing_control)
baseline_farmers$index_practices_midC <- index_practices_midC$index

index_practices_baseC <- icwIndex(xmat=variables_practices_base,sgroup = baseline_farmers$clearing_control)
baseline_farmers$index_practices_baseC <- index_practices_baseC$index

#6.
index_overallprimF_midC <- icwIndex(xmat=variables_overallprimF_mid,sgroup = baseline_farmers$clearing_control,revcols = c(5,6))
baseline_farmers$index_overallprimF_midC <- index_overallprimF_midC$index

index_overallprimF_baseC <- icwIndex(xmat=variables_overallprimF_base,sgroup = baseline_farmers$clearing_control,revcols = c(5,6))
baseline_farmers$index_overallprimF_baseC <- index_overallprimF_baseC$index

results_farmer_prim <- c("mid_Check2.check.maize.q25a","mid_agro","mid_Check2.check.maize.q25d"
                         ,"index_servicesF_midC","index_practices_midC","mid_Check2.check.maize.q25h"
                         ,"mid_Land_Races","index_overallprimF_midC")

results_farmer_prim_base <- c("Check2.check.maize.q25a","agro","Check2.check.maize.q25d"
                              ,"index_servicesF_baseC","index_practices_baseC","Check2.check.maize.q25h"
                              ,"Land_Races","index_overallprimF_baseC")

baseline_farmers[results_farmer_prim_base] <- lapply(baseline_farmers[results_farmer_prim_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_farmer_prim)){
  ols <- lm(as.formula(paste(paste(results_farmer_prim[i],"training_demeaned*clearing*farmer_demeaned",sep="~"),results_farmer_prim_base[i],sep="+")),data=baseline_farmers)
  #ols <- lm(as.formula(paste(results_farmer_prim[i],"training_demeaned*clearing*farmer_demeaned",sep="~")),data=baseline_farmers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_farmers$catchID,type="CR0")

  df_ols_F_prim[1,2,i] <- coef_test(ols, vcov_cluster)$beta[3]
  df_ols_F_prim[2,2,i] <- coef_test(ols, vcov_cluster)$SE[3]
  df_ols_F_prim[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]}

###
#4#
###

baseline_farmers$farmer_control[baseline_farmers$farmer==0] <- TRUE
baseline_farmers$farmer_control[baseline_farmers$farmer==1] <- FALSE

#4.
index_servicesF_midF <- icwIndex(xmat=variables_servicesF_mid,sgroup = baseline_farmers$farmer_control)
baseline_farmers$index_servicesF_midF <- index_servicesF_midF$index

index_servicesF_baseF <- icwIndex(xmat=variables_servicesF_base,sgroup = baseline_farmers$farmer_control)
baseline_farmers$index_servicesF_baseF <- index_servicesF_baseF$index

#5.
index_practices_midF <- icwIndex(xmat=variables_practices_mid,sgroup = baseline_farmers$farmer_control)
baseline_farmers$index_practices_midF <- index_practices_midF$index

index_practices_baseF <- icwIndex(xmat=variables_practices_base,sgroup = baseline_farmers$farmer_control)
baseline_farmers$index_practices_baseF <- index_practices_baseF$index

#6.
index_overallprimF_midF <- icwIndex(xmat=variables_overallprimF_mid,sgroup = baseline_farmers$farmer_control,revcols = c(5,6))
baseline_farmers$index_overallprimF_midF <- index_overallprimF_midF$index

index_overallprimF_baseF <- icwIndex(xmat=variables_overallprimF_base,sgroup = baseline_farmers$farmer_control,revcols = c(5,6))
baseline_farmers$index_overallprimF_baseF <- index_overallprimF_baseF$index

results_farmer_prim <- c("mid_Check2.check.maize.q25a","mid_agro","mid_Check2.check.maize.q25d"
                         ,"index_servicesF_midF","index_practices_midF","mid_Check2.check.maize.q25h"
                         ,"mid_Land_Races","index_overallprimF_midF")

results_farmer_prim_base <- c("Check2.check.maize.q25a","agro","Check2.check.maize.q25d"
                              ,"index_servicesF_baseF","index_practices_baseF","Check2.check.maize.q25h"
                              ,"Land_Races","index_overallprimF_baseF")

baseline_farmers[results_farmer_prim_base] <- lapply(baseline_farmers[results_farmer_prim_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_farmer_prim)){
  ols <- lm(as.formula(paste(paste(results_farmer_prim[i],"training_demeaned*clearing_demeaned*farmer",sep="~"),results_farmer_prim_base[i],sep="+")),data=baseline_farmers)
  #ols <- lm(as.formula(paste(results_farmer_prim[i],"training_demeaned*clearing_demeaned*farmer",sep="~")),data=baseline_farmers)
  vcov_cluster_shop <- vcovCR(ols,cluster=baseline_farmers$shop_ID,type="CR0")

  #farmer video treatment at village/shop level so no clustering needed
  df_ols_F_prim[1,3,i] <- coef_test(ols, vcov_cluster_shop)$beta[4]
  df_ols_F_prim[2,3,i] <- coef_test(ols, vcov_cluster_shop)$SE[4]
  df_ols_F_prim[3,3,i] <- coef_test(ols, vcov_cluster_shop)$p_Satt[4]}

#transformation because otherwise no correlation between agro and q25d possible:
baseline_farmers$Check2.check.maize.q25d_save <- baseline_farmers$Check2.check.maize.q25d
baseline_farmers$mid_Check2.check.maize.q25d_save <- baseline_farmers$mid_Check2.check.maize.q25d

baseline_farmers$Check2.check.maize.q25d[baseline_farmers$agro==0] = 0
baseline_farmers$Check2.check.maize.q25d <- ihs(baseline_farmers$Check2.check.maize.q25d)

baseline_farmers$mid_Check2.check.maize.q25d[baseline_farmers$mid_agro==0] = 0
baseline_farmers$mid_Check2.check.maize.q25d <- ihs(baseline_farmers$mid_Check2.check.maize.q25d)

#Aker, Boumnijel, McClelland, Tierney (2012)
df_farmer_primT <- data.frame(baseline_farmers$mid_Check2.check.maize.q25a,baseline_farmers$mid_agro
                                 ,baseline_farmers$mid_Check2.check.maize.q25d,baseline_farmers$index_servicesF_midT
                                 ,baseline_farmers$index_practices_midT,baseline_farmers$mid_Check2.check.maize.q25h,baseline_farmers$mid_Land_Races)
df_farmer_primC <- data.frame(baseline_farmers$mid_Check2.check.maize.q25a,baseline_farmers$mid_agro
                              ,baseline_farmers$mid_Check2.check.maize.q25d,baseline_farmers$index_servicesF_midC
                              ,baseline_farmers$index_practices_midC,baseline_farmers$mid_Check2.check.maize.q25h,baseline_farmers$mid_Land_Races)
df_farmer_primF <- data.frame(baseline_farmers$mid_Check2.check.maize.q25a,baseline_farmers$mid_agro
                              ,baseline_farmers$mid_Check2.check.maize.q25d,baseline_farmers$index_servicesF_midF
                              ,baseline_farmers$index_practices_midF,baseline_farmers$mid_Check2.check.maize.q25h,baseline_farmers$mid_Land_Races)
#no overall index

df_ols_F_prim_J <- array(NA,dim=c(3,3,11))

results_farmer_prim_J <- c("mid_Check2.check.maize.q25a","mid_agro","mid_Check2.check.maize.q25d"
                           ,"index_servicesF_mid","index_practices_midF","mid_Check2.check.maize.q25h"
                           ,"mid_Land_Races")
#no overall index

for (i in 1:length(results_farmer_prim_J)){
  df_ols_F_prim_J[3,1,i] <- adjust_p(df_ols_F_prim[3,1,i],df_farmer_primT,i)
  df_ols_F_prim_J[3,2,i] <- adjust_p(df_ols_F_prim[3,2,i],df_farmer_primC,i)
  df_ols_F_prim_J[3,3,i] <- adjust_p(df_ols_F_prim[3,3,i],df_farmer_primF,i)}

baseline_farmers$Check2.check.maize.q25d <- baseline_farmers$Check2.check.maize.q25d_save
baseline_farmers$mid_Check2.check.maize.q25d <- baseline_farmers$mid_Check2.check.maize.q25d_save











################################################################################################################################################################################
##### 8H ANALYSIS: Farmer - Primary#############################################################################################################################################
##### Heterogeneity analyses: More competitive catchment areas #################################################################################################################
################################################################################################################################################################################

#2: Larger catchment areas
baseline_farmers$small_catchID <- ifelse(baseline_farmers$catchID==16|baseline_farmers$catchID==18|baseline_farmers$catchID==19|
                                           baseline_farmers$catchID==33|baseline_farmers$catchID==34|baseline_farmers$catchID==36|
                                           baseline_farmers$catchID==42|baseline_farmers$catchID==45|baseline_farmers$catchID==46|
                                           baseline_farmers$catchID==48|baseline_farmers$catchID==53|baseline_farmers$catchID==63|
                                           baseline_farmers$catchID==65|baseline_farmers$catchID==66|baseline_farmers$catchID==67|
                                           baseline_farmers$catchID==73|baseline_farmers$catchID==79|baseline_farmers$catchID==80|
                                           baseline_farmers$catchID==87|baseline_farmers$catchID==89|baseline_farmers$catchID==90|
                                           baseline_farmers$catchID==91|baseline_farmers$catchID==92|baseline_farmers$catchID==93|
                                           baseline_farmers$catchID==95|baseline_farmers$catchID==98|baseline_farmers$catchID==101|
                                           baseline_farmers$catchID==103|baseline_farmers$catchID==106|baseline_farmers$catchID==107|
                                           baseline_farmers$catchID==108|baseline_farmers$catchID==109|baseline_farmers$catchID==110|
                                           baseline_farmers$catchID==112|baseline_farmers$catchID==116|baseline_farmers$catchID==118|
                                           baseline_farmers$catchID==120|baseline_farmers$catchID==121|baseline_farmers$catchID==122|
                                           baseline_farmers$catchID==124|baseline_farmers$catchID==125|baseline_farmers$catchID==126|
                                           baseline_farmers$catchID==127|baseline_farmers$catchID==128|baseline_farmers$catchID==129|
                                           baseline_farmers$catchID==130,1,0)

baseline_farmers_save=baseline_farmers
baseline_farmers=subset(baseline_farmers,small_catchID=="0")

variables_servicesF_mid <- cbind(baseline_farmers$mid_refunds,baseline_farmers$mid_gives_credit,baseline_farmers$mid_gives_advice
                                 ,baseline_farmers$mid_delivers,baseline_farmers$mid_after_sales_service,baseline_farmers$mid_payment_mehtods
                                 ,baseline_farmers$mid_small_quant)
variables_servicesF_base <- cbind(baseline_farmers$refunds,baseline_farmers$gives_credit,baseline_farmers$gives_advice
                                  ,baseline_farmers$delivers,baseline_farmers$after_sales_service,baseline_farmers$payment_mehtods
                                  ,baseline_farmers$small_quant)

index_servicesF_mid <- icwIndex(xmat=variables_servicesF_mid)
baseline_farmers$index_servicesF_mid <- index_servicesF_mid$index

index_servicesF_base <- icwIndex(xmat=variables_servicesF_base)
baseline_farmers$index_servicesF_base <- index_servicesF_base$index

variables_practices_mid <- cbind(baseline_farmers$mid_correct_q40,baseline_farmers$mid_correct_q41,baseline_farmers$mid_correct_q42
                                 ,baseline_farmers$mid_correct_q43,baseline_farmers$mid_correct_q44,baseline_farmers$mid_correct_q45
                                 ,baseline_farmers$mid_correct_q46,baseline_farmers$mid_correct_q47,baseline_farmers$mid_correct_q48
                                 ,baseline_farmers$mid_correct_q49)
variables_practices_base <- cbind(baseline_farmers$correct_q40,baseline_farmers$correct_q41,baseline_farmers$correct_q42
                                  ,baseline_farmers$correct_q43,baseline_farmers$correct_q44,baseline_farmers$correct_q45
                                  ,baseline_farmers$correct_q46,baseline_farmers$correct_q47,baseline_farmers$correct_q48
                                  ,baseline_farmers$correct_q49)

index_practices_mid <- icwIndex(xmat=variables_practices_mid)
baseline_farmers$index_practices_mid <- index_practices_mid$index

index_practices_base <- icwIndex(xmat=variables_practices_base)
baseline_farmers$index_practices_base <- index_practices_base$index

variables_overallprimF_mid <- cbind(baseline_farmers$mid_Check2.check.maize.q25a,baseline_farmers$mid_agro
                                    ,baseline_farmers$mid_Check2.check.maize.q25d,baseline_farmers$index_practices_mid
                                    ,baseline_farmers$mid_Check2.check.maize.q25h,baseline_farmers$mid_Land_Races)
variables_overallprimF_base <- cbind(baseline_farmers$Check2.check.maize.q25a,baseline_farmers$agro
                                     ,baseline_farmers$Check2.check.maize.q25d,baseline_farmers$index_practices_base
                                     ,baseline_farmers$Check2.check.maize.q25h,baseline_farmers$Land_Races)

index_overallprimF_mid <- icwIndex(xmat=variables_overallprimF_mid,revcols = c(5,6))
baseline_farmers$index_overallprimF_mid <- index_overallprimF_mid$index

index_overallprimF_base <- icwIndex(xmat=variables_overallprimF_base,revcols = c(5,6))
baseline_farmers$index_overallprimF_base <- index_overallprimF_base$index

################################################################################################################################################################################

###
#1#
###

results_farmer_prim <- c("mid_Check2.check.maize.q25a","mid_agro","mid_Check2.check.maize.q25d"
                         ,"index_servicesF_mid","index_practices_mid","mid_Check2.check.maize.q25h"
                         ,"mid_Land_Races","index_overallprimF_mid")

results_farmer_prim_base <- c("Check2.check.maize.q25a","agro","Check2.check.maize.q25d"
                              ,"index_servicesF_base","index_practices_base","Check2.check.maize.q25h"
                              ,"Land_Races","index_overallprimF_base")

baseline_farmers[results_farmer_prim_base] <- lapply(baseline_farmers[results_farmer_prim_base],function(x)x - mean(x,na.rm = T))

df_means_F_prim_het2 <- array(NA,dim=c(3,8))

for (i in 1:length(results_farmer_prim)){
  df_means_F_prim_het2[1,i] <- sum(baseline_farmers[results_farmer_prim[i]], na.rm=T)/(nrow(baseline_farmers)-sum(is.na(baseline_farmers[results_farmer_prim[i]])))
  df_means_F_prim_het2[2,i] <- sqrt(var(baseline_farmers[results_farmer_prim[i]], na.rm=T))
  df_means_F_prim_het2[3,i] <- nrow(baseline_farmers)-sum(is.na(baseline_farmers[results_farmer_prim[i]]))-sum(is.na(baseline_farmers[results_farmer_prim_base[i]]))+sum(is.na(baseline_farmers[results_farmer_prim[i]])&is.na(baseline_farmers[results_farmer_prim_base[i]]))}

###
#2#
###

baseline_farmers$training_control[baseline_farmers$training==0] <- TRUE
baseline_farmers$training_control[baseline_farmers$training==1] <- FALSE

#4.
index_servicesF_midT <- icwIndex(xmat=variables_servicesF_mid,sgroup = baseline_farmers$training_control)
baseline_farmers$index_servicesF_midT <- index_servicesF_midT$index

index_servicesF_baseT <- icwIndex(xmat=variables_servicesF_base,sgroup = baseline_farmers$training_control)
baseline_farmers$index_servicesF_baseT <- index_servicesF_baseT$index

#5.
index_practices_midT <- icwIndex(xmat=variables_practices_mid,sgroup = baseline_farmers$training_control)
baseline_farmers$index_practices_midT <- index_practices_midT$index

index_practices_baseT <- icwIndex(xmat=variables_practices_base,sgroup = baseline_farmers$training_control)
baseline_farmers$index_practices_baseT <- index_practices_baseT$index

#6.
index_overallprimF_midT <- icwIndex(xmat=variables_overallprimF_mid,sgroup = baseline_farmers$training_control,revcols = c(5,6))
baseline_farmers$index_overallprimF_midT <- index_overallprimF_midT$index

index_overallprimF_baseT <- icwIndex(xmat=variables_overallprimF_base,sgroup = baseline_farmers$training_control,revcols = c(5,6))
baseline_farmers$index_overallprimF_baseT <- index_overallprimF_baseT$index

df_ols_F_prim_het2 <- array(NA,dim=c(3,3,11))

results_farmer_prim <- c("mid_Check2.check.maize.q25a","mid_agro","mid_Check2.check.maize.q25d"
                         ,"index_servicesF_midT","index_practices_midT","mid_Check2.check.maize.q25h"
                         ,"mid_Land_Races","index_overallprimF_midT")

results_farmer_prim_base <- c("Check2.check.maize.q25a","agro","Check2.check.maize.q25d"
                              ,"index_servicesF_baseT","index_practices_baseT","Check2.check.maize.q25h"
                              ,"Land_Races","index_overallprimF_baseT")

baseline_farmers[results_farmer_prim_base] <- lapply(baseline_farmers[results_farmer_prim_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_farmer_prim)){
  ols <- lm(as.formula(paste(paste(results_farmer_prim[i],"training*clearing_demeaned*farmer_demeaned",sep="~"),results_farmer_prim_base[i],sep="+")),data=baseline_farmers)
  #ols <- lm(as.formula(paste(results_farmer_prim[i],"training*clearing_demeaned*farmer_demeaned",sep="~")),data=baseline_farmers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_farmers$catchID,type="CR0")
  
  df_ols_F_prim_het2[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_ols_F_prim_het2[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_ols_F_prim_het2[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]}

###
#3#
###

baseline_farmers$clearing_control[baseline_farmers$clearing==0] <- TRUE
baseline_farmers$clearing_control[baseline_farmers$clearing==1] <- FALSE

#4.
index_servicesF_midC <- icwIndex(xmat=variables_servicesF_mid,sgroup = baseline_farmers$clearing_control)
baseline_farmers$index_servicesF_midC <- index_servicesF_midC$index

index_servicesF_baseC <- icwIndex(xmat=variables_servicesF_base,sgroup = baseline_farmers$clearing_control)
baseline_farmers$index_servicesF_baseC <- index_servicesF_baseC$index

#5.
index_practices_midC <- icwIndex(xmat=variables_practices_mid,sgroup = baseline_farmers$clearing_control)
baseline_farmers$index_practices_midC <- index_practices_midC$index

index_practices_baseC <- icwIndex(xmat=variables_practices_base,sgroup = baseline_farmers$clearing_control)
baseline_farmers$index_practices_baseC <- index_practices_baseC$index

#6.
index_overallprimF_midC <- icwIndex(xmat=variables_overallprimF_mid,sgroup = baseline_farmers$clearing_control,revcols = c(5,6))
baseline_farmers$index_overallprimF_midC <- index_overallprimF_midC$index

index_overallprimF_baseC <- icwIndex(xmat=variables_overallprimF_base,sgroup = baseline_farmers$clearing_control,revcols = c(5,6))
baseline_farmers$index_overallprimF_baseC <- index_overallprimF_baseC$index

results_farmer_prim <- c("mid_Check2.check.maize.q25a","mid_agro","mid_Check2.check.maize.q25d"
                         ,"index_servicesF_midC","index_practices_midC","mid_Check2.check.maize.q25h"
                         ,"mid_Land_Races","index_overallprimF_midC")

results_farmer_prim_base <- c("Check2.check.maize.q25a","agro","Check2.check.maize.q25d"
                              ,"index_servicesF_baseC","index_practices_baseC","Check2.check.maize.q25h"
                              ,"Land_Races","index_overallprimF_baseC")

baseline_farmers[results_farmer_prim_base] <- lapply(baseline_farmers[results_farmer_prim_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_farmer_prim)){
  ols <- lm(as.formula(paste(paste(results_farmer_prim[i],"training_demeaned*clearing*farmer_demeaned",sep="~"),results_farmer_prim_base[i],sep="+")),data=baseline_farmers)
  #ols <- lm(as.formula(paste(results_farmer_prim[i],"training_demeaned*clearing*farmer_demeaned",sep="~")),data=baseline_farmers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_farmers$catchID,type="CR0")
  
  df_ols_F_prim_het2[1,2,i] <- coef_test(ols, vcov_cluster)$beta[3]
  df_ols_F_prim_het2[2,2,i] <- coef_test(ols, vcov_cluster)$SE[3]
  df_ols_F_prim_het2[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]}

###
#4#
###

baseline_farmers$farmer_control[baseline_farmers$farmer==0] <- TRUE
baseline_farmers$farmer_control[baseline_farmers$farmer==1] <- FALSE

#4.
index_servicesF_midF <- icwIndex(xmat=variables_servicesF_mid,sgroup = baseline_farmers$farmer_control)
baseline_farmers$index_servicesF_midF <- index_servicesF_midF$index

index_servicesF_baseF <- icwIndex(xmat=variables_servicesF_base,sgroup = baseline_farmers$farmer_control)
baseline_farmers$index_servicesF_baseF <- index_servicesF_baseF$index

#5.
index_practices_midF <- icwIndex(xmat=variables_practices_mid,sgroup = baseline_farmers$farmer_control)
baseline_farmers$index_practices_midF <- index_practices_midF$index

index_practices_baseF <- icwIndex(xmat=variables_practices_base,sgroup = baseline_farmers$farmer_control)
baseline_farmers$index_practices_baseF <- index_practices_baseF$index

#6.
index_overallprimF_midF <- icwIndex(xmat=variables_overallprimF_mid,sgroup = baseline_farmers$farmer_control,revcols = c(5,6))
baseline_farmers$index_overallprimF_midF <- index_overallprimF_midF$index

index_overallprimF_baseF <- icwIndex(xmat=variables_overallprimF_base,sgroup = baseline_farmers$farmer_control,revcols = c(5,6))
baseline_farmers$index_overallprimF_baseF <- index_overallprimF_baseF$index

results_farmer_prim <- c("mid_Check2.check.maize.q25a","mid_agro","mid_Check2.check.maize.q25d"
                         ,"index_servicesF_midF","index_practices_midF","mid_Check2.check.maize.q25h"
                         ,"mid_Land_Races","index_overallprimF_midF")

results_farmer_prim_base <- c("Check2.check.maize.q25a","agro","Check2.check.maize.q25d"
                              ,"index_servicesF_baseF","index_practices_baseF","Check2.check.maize.q25h"
                              ,"Land_Races","index_overallprimF_baseF")

baseline_farmers[results_farmer_prim_base] <- lapply(baseline_farmers[results_farmer_prim_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_farmer_prim)){
  ols <- lm(as.formula(paste(paste(results_farmer_prim[i],"training_demeaned*clearing_demeaned*farmer",sep="~"),results_farmer_prim_base[i],sep="+")),data=baseline_farmers)
  #ols <- lm(as.formula(paste(results_farmer_prim[i],"training_demeaned*clearing_demeaned*farmer",sep="~")),data=baseline_farmers)
  vcov_cluster_shop <- vcovCR(ols,cluster=baseline_farmers$shop_ID,type="CR0")
  
  #farmer video treatment at village/shop level so no clustering needed
  df_ols_F_prim_het2[1,3,i] <- coef_test(ols, vcov_cluster_shop)$beta[4]
  df_ols_F_prim_het2[2,3,i] <- coef_test(ols, vcov_cluster_shop)$SE[4]
  df_ols_F_prim_het2[3,3,i] <- coef_test(ols, vcov_cluster_shop)$p_Satt[4]}

#Aker, Boumnijel, McClelland, Tierney (2012)
df_farmer_primT <- data.frame(baseline_farmers$mid_Check2.check.maize.q25a,baseline_farmers$mid_agro
                              ,baseline_farmers$mid_Check2.check.maize.q25d,baseline_farmers$index_servicesF_midT
                              ,baseline_farmers$index_practices_midT,baseline_farmers$mid_Check2.check.maize.q25h,baseline_farmers$mid_Land_Races)
df_farmer_primC <- data.frame(baseline_farmers$mid_Check2.check.maize.q25a,baseline_farmers$mid_agro
                              ,baseline_farmers$mid_Check2.check.maize.q25d,baseline_farmers$index_servicesF_midC
                              ,baseline_farmers$index_practices_midC,baseline_farmers$mid_Check2.check.maize.q25h,baseline_farmers$mid_Land_Races)
df_farmer_primF <- data.frame(baseline_farmers$mid_Check2.check.maize.q25a,baseline_farmers$mid_agro
                              ,baseline_farmers$mid_Check2.check.maize.q25d,baseline_farmers$index_servicesF_midF
                              ,baseline_farmers$index_practices_midF,baseline_farmers$mid_Check2.check.maize.q25h,baseline_farmers$mid_Land_Races)
#no overall index

df_ols_F_prim_het2_J <- array(NA,dim=c(3,3,11))

results_farmer_prim_J <- c("mid_Check2.check.maize.q25a","mid_agro","mid_Check2.check.maize.q25d"
                           ,"index_servicesF_mid","index_practices_midF","mid_Check2.check.maize.q25h"
                           ,"mid_Land_Races")
#no overall index

for (i in 1:length(results_farmer_prim_J)){
  df_ols_F_prim_het2_J[3,1,i] <- adjust_p(df_ols_F_prim_het2[3,1,i],df_farmer_primT,i)
  df_ols_F_prim_het2_J[3,2,i] <- adjust_p(df_ols_F_prim_het2[3,2,i],df_farmer_primC,i)
  df_ols_F_prim_het2_J[3,3,i] <- adjust_p(df_ols_F_prim_het2[3,3,i],df_farmer_primF,i)}

baseline_farmers=baseline_farmers_save












################################################################################################################################################################################
##### 10 ANALYSIS: Farmer - Secondary ##########################################################################################################################################
################################################################################################################################################################################

#2. Q26. ${enumerator} : Ask the farmer to mention as many improved maize varieties that they are aware of
baseline_farmers$Check2.check.maize.q26.Longe_10H <- ifelse(baseline_farmers$Check2.check.maize.q26.Longe_10H=="True",1,0)
baseline_farmers$Check2.check.maize.q26.Longe_7H <- ifelse(baseline_farmers$Check2.check.maize.q26.Longe_7H=="True",1,0)
#baseline_farmers$Check2.check.maize.q26.Longe_7R_Kayongo.go <- ifelse(baseline_farmers$Check2.check.maize.q26.Longe_7R_Kayongo.go=="True",1,0) #because above
baseline_farmers$Check2.check.maize.q26.Bazooka <- ifelse(baseline_farmers$Check2.check.maize.q26.Bazooka=="True",1,0)
baseline_farmers$Check2.check.maize.q26.Longe_6H <- ifelse(baseline_farmers$Check2.check.maize.q26.Longe_6H=="True",1,0)
#baseline_farmers$Check2.check.maize.q26.Longe_5 <- ifelse(baseline_farmers$Check2.check.maize.q26.Longe_5=="True",1,0)
baseline_farmers$Check2.check.maize.q26.Longe_4 <- ifelse(baseline_farmers$Check2.check.maize.q26.Longe_4=="True",1,0)
baseline_farmers$Check2.check.maize.q26.Panner <- ifelse(baseline_farmers$Check2.check.maize.q26.Panner=="True",1,0)
#baseline_farmers$Check2.check.maize.q26.Wema <- ifelse(baseline_farmers$Check2.check.maize.q26.Wema=="True",1,0)
baseline_farmers$Check2.check.maize.q26.KH_series <- ifelse(baseline_farmers$Check2.check.maize.q26.KH_series=="True",1,0)
baseline_farmers$Check2.check.maize.q26.Land_Races <- ifelse(baseline_farmers$Check2.check.maize.q26.Land_Races=="True",1,0)
baseline_farmers$Check2.check.maize.q26.Other_hybrid <- ifelse(baseline_farmers$Check2.check.maize.q26.Other_hybrid=="True",1,0)

baseline_farmers$number_known <- (baseline_farmers$Check2.check.maize.q26.Longe_10H
                                  +baseline_farmers$Check2.check.maize.q26.Longe_7H
                                  +baseline_farmers$Check2.check.maize.q26.Longe_7R_Kayongo.go
                                  +baseline_farmers$Check2.check.maize.q26.Bazooka
                                  +baseline_farmers$Check2.check.maize.q26.Longe_6H
                                  +baseline_farmers$Check2.check.maize.q26.Longe_5
                                  +baseline_farmers$Check2.check.maize.q26.Longe_4
                                  +baseline_farmers$Check2.check.maize.q26.Panner
                                  +baseline_farmers$Check2.check.maize.q26.Wema
                                  +baseline_farmers$Check2.check.maize.q26.KH_series
                                  +baseline_farmers$Check2.check.maize.q26.Land_Races
                                  +baseline_farmers$Check2.check.maize.q26.Other_hybrid)

baseline_farmers$mid_Check2.check.maize.q26.Longe_10H <- baseline_farmers$check.maize.q26.Longe_10H
baseline_farmers$mid_Check2.check.maize.q26.Longe_7H <- baseline_farmers$check.maize.q26.Longe_7H
baseline_farmers$mid_Check2.check.maize.q26.Longe_7R_Kayongo.go <- baseline_farmers$check.maize.q26.Longe_7R_Kayongo.go
baseline_farmers$mid_Check2.check.maize.q26.Bazooka <- baseline_farmers$check.maize.q26.Bazooka
baseline_farmers$mid_Check2.check.maize.q26.Longe_6H <- baseline_farmers$check.maize.q26.Longe_6H
baseline_farmers$mid_Check2.check.maize.q26.Longe_5 <- baseline_farmers$check.maize.q26.Longe_5
baseline_farmers$mid_Check2.check.maize.q26.Longe_4 <- baseline_farmers$check.maize.q26.Longe_4
baseline_farmers$mid_Check2.check.maize.q26.Panner <- baseline_farmers$check.maize.q26.Panner
baseline_farmers$mid_Check2.check.maize.q26.Wema <- baseline_farmers$check.maize.q26.Wema
baseline_farmers$mid_Check2.check.maize.q26.KH_series <- baseline_farmers$check.maize.q26.KH_series
baseline_farmers$mid_Check2.check.maize.q26.Land_Races <- baseline_farmers$check.maize.q26.Land_Races
baseline_farmers$mid_Check2.check.maize.q26.Other_hybrid <- baseline_farmers$check.maize.q26.Other_hybrid

baseline_farmers$mid_Check2.check.maize.q26.Longe_10H <- ifelse(baseline_farmers$mid_Check2.check.maize.q26.Longe_10H=="True",1,0)
baseline_farmers$mid_Check2.check.maize.q26.Longe_7H <- ifelse(baseline_farmers$mid_Check2.check.maize.q26.Longe_7H=="True",1,0)
baseline_farmers$mid_Check2.check.maize.q26.Longe_7R_Kayongo.go <- ifelse(baseline_farmers$mid_Check2.check.maize.q26.Longe_7R_Kayongo.go=="True",1,0)
baseline_farmers$mid_Check2.check.maize.q26.Bazooka <- ifelse(baseline_farmers$mid_Check2.check.maize.q26.Bazooka=="True",1,0)
baseline_farmers$mid_Check2.check.maize.q26.Longe_6H <- ifelse(baseline_farmers$mid_Check2.check.maize.q26.Longe_6H=="True",1,0)
baseline_farmers$mid_Check2.check.maize.q26.Longe_5 <- ifelse(baseline_farmers$mid_Check2.check.maize.q26.Longe_5=="True",1,0)
baseline_farmers$mid_Check2.check.maize.q26.Longe_4 <- ifelse(baseline_farmers$mid_Check2.check.maize.q26.Longe_4=="True",1,0)
baseline_farmers$mid_Check2.check.maize.q26.Panner <- ifelse(baseline_farmers$mid_Check2.check.maize.q26.Panner=="True",1,0)
baseline_farmers$mid_Check2.check.maize.q26.Wema <- ifelse(baseline_farmers$mid_Check2.check.maize.q26.Wema=="True",1,0)
baseline_farmers$mid_Check2.check.maize.q26.KH_series <- ifelse(baseline_farmers$mid_Check2.check.maize.q26.KH_series=="True",1,0)
baseline_farmers$mid_Check2.check.maize.q26.Land_Races <- ifelse(baseline_farmers$mid_Check2.check.maize.q26.Land_Races=="True",1,0)
baseline_farmers$mid_Check2.check.maize.q26.Other_hybrid <- ifelse(baseline_farmers$mid_Check2.check.maize.q26.Other_hybrid=="True",1,0)

baseline_farmers$mid_number_known <- (baseline_farmers$mid_Check2.check.maize.q26.Longe_10H
                                  +baseline_farmers$mid_Check2.check.maize.q26.Longe_7H
                                  +baseline_farmers$mid_Check2.check.maize.q26.Longe_7R_Kayongo.go
                                  +baseline_farmers$mid_Check2.check.maize.q26.Bazooka
                                  +baseline_farmers$mid_Check2.check.maize.q26.Longe_6H
                                  +baseline_farmers$mid_Check2.check.maize.q26.Longe_5
                                  +baseline_farmers$mid_Check2.check.maize.q26.Longe_4
                                  +baseline_farmers$mid_Check2.check.maize.q26.Panner
                                  +baseline_farmers$mid_Check2.check.maize.q26.Wema
                                  +baseline_farmers$mid_Check2.check.maize.q26.KH_series
                                  +baseline_farmers$mid_Check2.check.maize.q26.Land_Races
                                  +baseline_farmers$mid_Check2.check.maize.q26.Other_hybrid)

#3. Q64. Do you know **${calc_biz}**  or ${dealer_name} sometimes called ${nickname} located in ${market_name} market. The place can be described as: ${eye}

#4. Q67. Did you buy seed from  **${calc_biz}** in the last season (entoigo 2020)
baseline_farmers$bought_last_season[baseline_farmers$knows_dealer==0] <- 0
baseline_farmers$bought_last_season[baseline_farmers$bought_at_dealer==0] <- 0

baseline_farmers$mid_bought_last_season[baseline_farmers$mid_knows_dealer==0] <- 0
baseline_farmers$mid_bought_last_season[baseline_farmers$mid_bought_at_dealer==0] <- 0

#5. overall index
variables_overallsecF_mid <- cbind(baseline_farmers$mid_number_known
                                   ,baseline_farmers$mid_knows_dealer)
variables_overallsecF_base <- cbind(baseline_farmers$number_known
                                    ,baseline_farmers$knows_dealer)

index_overallsecF_mid <- icwIndex(xmat=variables_overallsecF_mid)
baseline_farmers$index_overallsecF_mid <- index_overallsecF_mid$index

index_overallsecF_base <- icwIndex(xmat=variables_overallsecF_base)
baseline_farmers$index_overallsecF_base <- index_overallsecF_base$index

################################################################################################################################################################################

###
#1#
###

results_farmer_sec <- c("mid_number_known","mid_knows_dealer","mid_bought_last_season","index_overallsecF_mid")

results_farmer_sec_base <- c("number_known","knows_dealer","bought_last_season","index_overallsecF_base")

baseline_farmers[results_farmer_sec_base] <- lapply(baseline_farmers[results_farmer_sec_base],function(x)x - mean(x,na.rm = T))

df_means_F_sec <- array(NA,dim=c(3,11))

for (i in 1:length(results_farmer_sec)){
  df_means_F_sec[1,i] <- sum(baseline_farmers[results_farmer_sec[i]], na.rm=T)/(nrow(baseline_farmers)-sum(is.na(baseline_farmers[results_farmer_sec[i]])))
  df_means_F_sec[2,i] <- sqrt(var(baseline_farmers[results_farmer_sec[i]], na.rm=T))
  df_means_F_sec[3,i] <- nrow(baseline_farmers)-sum(is.na(baseline_farmers[results_farmer_sec[i]]))-sum(is.na(baseline_farmers[results_farmer_sec_base[i]]))+sum(is.na(baseline_farmers[results_farmer_sec[i]])&is.na(baseline_farmers[results_farmer_sec_base[i]]))}

###
#2#
###

baseline_farmers$training_control[baseline_farmers$training==0] <- TRUE
baseline_farmers$training_control[baseline_farmers$training==1] <- FALSE

#5.
index_overallsecF_midT <- icwIndex(xmat=variables_overallsecF_mid,sgroup = baseline_farmers$training_control)
baseline_farmers$index_overallsecF_midT <- index_overallsecF_midT$index

index_overallsecF_baseT <- icwIndex(xmat=variables_overallsecF_base,sgroup = baseline_farmers$training_control)
baseline_farmers$index_overallsecF_baseT <- index_overallsecF_baseT$index

df_ols_F_sec <- array(NA,dim=c(3,3,11))

results_farmer_sec <- c("mid_number_known","mid_knows_dealer","mid_bought_last_season","index_overallsecF_midT")

results_farmer_sec_base <- c("number_known","knows_dealer","bought_last_season","index_overallsecF_baseT")

baseline_farmers[results_farmer_sec_base] <- lapply(baseline_farmers[results_farmer_sec_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_farmer_sec)){
  ols <- lm(as.formula(paste(paste(results_farmer_sec[i],"training*clearing_demeaned*farmer_demeaned",sep="~"),results_farmer_sec_base[i],sep="+")),data=baseline_farmers)
  #ols <- lm(as.formula(paste(results_farmer_sec[i],"training*clearing_demeaned*farmer_demeaned",sep="~")),data=baseline_farmers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_farmers$catchID,type="CR0")
  
  df_ols_F_sec[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_ols_F_sec[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_ols_F_sec[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]}

###
#3#
###

baseline_farmers$clearing_control[baseline_farmers$clearing==0] <- TRUE
baseline_farmers$clearing_control[baseline_farmers$clearing==1] <- FALSE

#5.
index_overallsecF_midC <- icwIndex(xmat=variables_overallsecF_mid,sgroup = baseline_farmers$clearing_control)
baseline_farmers$index_overallsecF_midC <- index_overallsecF_midC$index

index_overallsecF_baseC <- icwIndex(xmat=variables_overallsecF_base,sgroup = baseline_farmers$clearing_control)
baseline_farmers$index_overallsecF_baseC <- index_overallsecF_baseC$index

results_farmer_sec <- c("mid_number_known","mid_knows_dealer","mid_bought_last_season","index_overallsecF_midC")

results_farmer_sec_base <- c("number_known","knows_dealer","bought_last_season","index_overallsecF_baseC")

baseline_farmers[results_farmer_sec_base] <- lapply(baseline_farmers[results_farmer_sec_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_farmer_sec)){
  ols <- lm(as.formula(paste(paste(results_farmer_sec[i],"training_demeaned*clearing*farmer_demeaned",sep="~"),results_farmer_sec_base[i],sep="+")),data=baseline_farmers)
  #ols <- lm(as.formula(paste(results_farmer_sec[i],"training_demeaned*clearing*farmer_demeaned",sep="~")),data=baseline_farmers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_farmers$catchID,type="CR0")
  
  df_ols_F_sec[1,2,i] <- coef_test(ols, vcov_cluster)$beta[3]
  df_ols_F_sec[2,2,i] <- coef_test(ols, vcov_cluster)$SE[3]
  df_ols_F_sec[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]}

###
#4#
###

baseline_farmers$farmer_control[baseline_farmers$farmer==0] <- TRUE
baseline_farmers$farmer_control[baseline_farmers$farmer==1] <- FALSE

#5.
index_overallsecF_midF <- icwIndex(xmat=variables_overallsecF_mid,sgroup = baseline_farmers$farmer_control)
baseline_farmers$index_overallsecF_midF <- index_overallsecF_midF$index

index_overallsecF_baseF <- icwIndex(xmat=variables_overallsecF_base,sgroup = baseline_farmers$farmer_control)
baseline_farmers$index_overallsecF_baseF <- index_overallsecF_baseF$index

results_farmer_sec <- c("mid_number_known","mid_knows_dealer","mid_bought_last_season","index_overallsecF_midF")

results_farmer_sec_base <- c("number_known","knows_dealer","bought_last_season","index_overallsecF_baseF")

baseline_farmers[results_farmer_sec_base] <- lapply(baseline_farmers[results_farmer_sec_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_farmer_sec)){
  ols <- lm(as.formula(paste(paste(results_farmer_sec[i],"training_demeaned*clearing_demeaned*farmer",sep="~"),results_farmer_sec_base[i],sep="+")),data=baseline_farmers)
  #ols <- lm(as.formula(paste(results_farmer_sec[i],"training_demeaned*clearing_demeaned*farmer",sep="~")),data=baseline_farmers)
  vcov_cluster_shop <- vcovCR(ols,cluster=baseline_farmers$shop_ID,type="CR0")
  
  #farmer video treatment at village/shop level so no clustering needed
  df_ols_F_sec[1,3,i] <- coef_test(ols, vcov_cluster_shop)$beta[4]
  df_ols_F_sec[2,3,i] <- coef_test(ols, vcov_cluster_shop)$SE[4]
  df_ols_F_sec[3,3,i] <- coef_test(ols, vcov_cluster_shop)$p_Satt[4]}

#Aker, Boumnijel, McClelland, Tierney (2012)
df_farmer_secT <- data.frame(baseline_farmers$mid_number_known,baseline_farmers$mid_knows_dealer)
df_farmer_secC <- df_farmer_secT
df_farmer_secF <- df_farmer_secT
#no overall index

df_ols_F_sec_J <- array(NA,dim=c(3,3,11))

results_farmer_sec_J <- c("mid_number_known","mid_knows_dealer")
#no overall index

for (i in 1:length(results_farmer_sec_J)){
  df_ols_F_sec_J[3,1,i] <- adjust_p(df_ols_F_sec[3,1,i],df_farmer_secT,i)
  df_ols_F_sec_J[3,2,i] <- adjust_p(df_ols_F_sec[3,2,i],df_farmer_secC,i)
  df_ols_F_sec_J[3,3,i] <- adjust_p(df_ols_F_sec[3,3,i],df_farmer_secF,i)}

#currently NAs because only two variables










################################################################################################################################################################################
##### 11 ANALYSIS: Farmer - Secondary: Adoption on plot ########################################################################################################################
################################################################################################################################################################################

#1. hybrid
baseline_farmers$mid_Check2.check.maize.q31 <- baseline_farmers$check.maize.q31
baseline_farmers$mid_hybrid<-((baseline_farmers$mid_Check2.check.maize.q31=="Longe_10H")|(baseline_farmers$mid_Check2.check.maize.q31=="Longe_7H")|(baseline_farmers$mid_Check2.check.maize.q31=="Longe_7R_Kayongo-go")|(baseline_farmers$mid_Check2.check.maize.q31=="Bazooka")|(baseline_farmers$mid_Check2.check.maize.q31=="Longe_6H")|(baseline_farmers$mid_Check2.check.maize.q31=="Panner")|(baseline_farmers$mid_Check2.check.maize.q31=="Wema")|(baseline_farmers$mid_Check2.check.maize.q31=="KH_series"))
baseline_farmers$mid_hybrid<-ifelse(baseline_farmers$mid_hybrid=="TRUE",1,0)
baseline_farmers$mid_hybrid[baseline_farmers$mid_Check2.check.maize.q31=="Other_hybrid"] <- NA #because =Other hybrid or OPV

#2. OPV
baseline_farmers$mid_OPV<-(baseline_farmers$mid_Check2.check.maize.q31=="Longe_5")|(baseline_farmers$mid_Check2.check.maize.q31=="Longe_4")
baseline_farmers$mid_OPV<-ifelse(baseline_farmers$mid_OPV=="TRUE",1,0)
baseline_farmers$mid_OPV[baseline_farmers$mid_Check2.check.maize.q31=="Other_hybrid"] <- NA

#5. farmer saved
baseline_farmers$mid_Check2.check.maize.q32 <- baseline_farmers$check.maize.q32
baseline_farmers$mid_farmer_saved_seed<-((baseline_farmers$mid_Check2.check.maize.q32=="a")|(baseline_farmers$mid_Check2.check.maize.q32=="b"))
baseline_farmers$mid_farmer_saved_seed<-ifelse(baseline_farmers$mid_farmer_saved_seed=="TRUE",1,0)

#6. agro
baseline_farmers$mid_Bought_from_agro_input_shop<-ifelse(baseline_farmers$mid_Check2.check.maize.q32=="d",1,0)

#7. adoption
baseline_farmers$mid_hybridbutsaved <- NA
baseline_farmers$mid_hybridbutsaved[baseline_farmers$mid_hybrid == 1 & baseline_farmers$mid_farmer_saved_seed == 1] <- 1
baseline_farmers$mid_hybridbutsaved[baseline_farmers$mid_hybrid == 1 & baseline_farmers$mid_farmer_saved_seed == 0] <- 0
baseline_farmers$mid_hybridbutsaved[baseline_farmers$mid_hybrid == 0] <- 0

baseline_farmers$mid_Check2.check.maize.q34 <- baseline_farmers$check.maize.q34
baseline_farmers$mid_fourthormore_timeused<-((baseline_farmers$mid_Check2.check.maize.q34=="d")|(baseline_farmers$mid_Check2.check.maize.q34=="e")|(baseline_farmers$mid_Check2.check.maize.q34=="f"))
baseline_farmers$mid_fourthormore_timeused<-ifelse(baseline_farmers$mid_fourthormore_timeused=="TRUE",1,0)

baseline_farmers$mid_OPVbutfourthormore_timeused <- NA
baseline_farmers$mid_OPVbutfourthormore_timeused[baseline_farmers$mid_OPV==1 & baseline_farmers$mid_farmer_saved_seed==1 & baseline_farmers$mid_fourthormore_timeused==1] <- 1
baseline_farmers$mid_OPVbutfourthormore_timeused[baseline_farmers$mid_OPV==1 & baseline_farmers$mid_farmer_saved_seed==1 & baseline_farmers$mid_fourthormore_timeused==0] <- 0
baseline_farmers$mid_OPVbutfourthormore_timeused[baseline_farmers$mid_OPV==1 & baseline_farmers$mid_farmer_saved_seed==0] <- 0
baseline_farmers$mid_OPVbutfourthormore_timeused[baseline_farmers$mid_OPV == 0] <- 0

baseline_farmers$mid_improved<-((baseline_farmers$mid_Check2.check.maize.q31=="Longe_10H")|(baseline_farmers$mid_Check2.check.maize.q31=="Longe_7H")|(baseline_farmers$mid_Check2.check.maize.q31=="Longe_7R_Kayongo-go")|(baseline_farmers$mid_Check2.check.maize.q31=="Bazooka")|(baseline_farmers$mid_Check2.check.maize.q31=="Longe_6H")|(baseline_farmers$mid_Check2.check.maize.q31=="Panner")|(baseline_farmers$mid_Check2.check.maize.q31=="Wema")|(baseline_farmers$mid_Check2.check.maize.q31=="KH_series"|baseline_farmers$mid_Check2.check.maize.q31=="Longe_5")|(baseline_farmers$mid_Check2.check.maize.q31=="Longe_4")|(baseline_farmers$mid_Check2.check.maize.q31=="Other_hybrid"))
baseline_farmers$mid_improved<-ifelse(baseline_farmers$mid_improved=="TRUE",1,0)

baseline_farmers$mid_adoption_onfield <- baseline_farmers$mid_improved
baseline_farmers$mid_adoption_onfield[baseline_farmers$mid_hybridbutsaved==1] <- 0
baseline_farmers$mid_adoption_onfield[baseline_farmers$mid_OPVbutfourthormore_timeused==1] <- 0

#8. overall index
variables_overallsec_plotF_mid <- cbind(baseline_farmers$mid_hybrid,baseline_farmers$mid_OPV
                                        ,baseline_farmers$mid_farmer_saved_seed,baseline_farmers$mid_Bought_from_agro_input_shop)
variables_overallsec_plotF_base <- cbind(baseline_farmers$hybrid,baseline_farmers$OPV
                                         ,baseline_farmers$farmer_saved_seed,baseline_farmers$Bought_from_agro_input_shop)

index_overallsec_plotF_mid <- icwIndex(xmat=variables_overallsec_plotF_mid,revcols = c(3))
baseline_farmers$index_overallsec_plotF_mid <- index_overallsec_plotF_mid$index

index_overallsec_plotF_base <- icwIndex(xmat=variables_overallsec_plotF_base,revcols = c(3))
baseline_farmers$index_overallsec_plotF_base <- index_overallsec_plotF_base$index

################################################################################################################################################################################

###
#1#
###

results_farmer_sec_plot <- c("mid_hybrid","mid_OPV","mid_farmer_saved_seed"
                             ,"mid_Bought_from_agro_input_shop","mid_adoption_onfield","index_overallsec_plotF_mid")

results_farmer_sec_plot_base <- c("hybrid","OPV","farmer_saved_seed"
                                  ,"Bought_from_agro_input_shop","adoption_onfield","index_overallsec_plotF_base")

baseline_farmers[results_farmer_sec_plot_base] <- lapply(baseline_farmers[results_farmer_sec_plot_base],function(x)x - mean(x,na.rm = T))

df_means_F_sec_plot <- array(NA,dim=c(3,11))

for (i in 1:length(results_farmer_sec_plot)){
  df_means_F_sec_plot[1,i] <- sum(baseline_farmers[results_farmer_sec_plot[i]], na.rm=T)/(nrow(baseline_farmers)-sum(is.na(baseline_farmers[results_farmer_sec_plot[i]])))
  df_means_F_sec_plot[2,i] <- sqrt(var(baseline_farmers[results_farmer_sec_plot[i]], na.rm=T))
  df_means_F_sec_plot[3,i] <- nrow(baseline_farmers)-sum(is.na(baseline_farmers[results_farmer_sec_plot[i]]))-sum(is.na(baseline_farmers[results_farmer_sec_plot_base[i]]))+sum(is.na(baseline_farmers[results_farmer_sec_plot[i]])&is.na(baseline_farmers[results_farmer_sec_plot_base[i]]))}

###
#2#
###

baseline_farmers$training_control[baseline_farmers$training==0] <- TRUE
baseline_farmers$training_control[baseline_farmers$training==1] <- FALSE

#8.
index_overallsec_plotF_midT <- icwIndex(xmat=variables_overallsec_plotF_mid,sgroup = baseline_farmers$training_control,revcols = c(3))
baseline_farmers$index_overallsec_plotF_midT <- index_overallsec_plotF_midT$index

index_overallsec_plotF_baseT <- icwIndex(xmat=variables_overallsec_plotF_base,sgroup = baseline_farmers$training_control,revcols = c(3))
baseline_farmers$index_overallsec_plotF_baseT <- index_overallsec_plotF_baseT$index

df_ols_F_sec_plot <- array(NA,dim=c(3,3,11))

results_farmer_sec_plot <- c("mid_hybrid","mid_OPV","mid_farmer_saved_seed"
                             ,"mid_Bought_from_agro_input_shop","mid_adoption_onfield","index_overallsec_plotF_midT")

results_farmer_sec_plot_base <- c("hybrid","OPV","farmer_saved_seed"
                                  ,"Bought_from_agro_input_shop","adoption_onfield","index_overallsec_plotF_baseT")

baseline_farmers[results_farmer_sec_plot_base] <- lapply(baseline_farmers[results_farmer_sec_plot_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_farmer_sec_plot)){
  ols <- lm(as.formula(paste(paste(results_farmer_sec_plot[i],"training*clearing_demeaned*farmer_demeaned",sep="~"),results_farmer_sec_plot_base[i],sep="+")),data=baseline_farmers)
  #ols <- lm(as.formula(paste(results_farmer_sec_plot[i],"training*clearing_demeaned*farmer_demeaned",sep="~")),data=baseline_farmers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_farmers$catchID,type="CR0")
  
  df_ols_F_sec_plot[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_ols_F_sec_plot[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_ols_F_sec_plot[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]}

###
#3#
###

baseline_farmers$clearing_control[baseline_farmers$clearing==0] <- TRUE
baseline_farmers$clearing_control[baseline_farmers$clearing==1] <- FALSE

#8.
index_overallsec_plotF_midC <- icwIndex(xmat=variables_overallsec_plotF_mid,sgroup = baseline_farmers$clearing_control,revcols = c(3))
baseline_farmers$index_overallsec_plotF_midC <- index_overallsec_plotF_midC$index

index_overallsec_plotF_baseC <- icwIndex(xmat=variables_overallsec_plotF_base,sgroup = baseline_farmers$clearing_control,revcols = c(3))
baseline_farmers$index_overallsec_plotF_baseC <- index_overallsec_plotF_baseC$index

results_farmer_sec_plot <- c("mid_hybrid","mid_OPV","mid_farmer_saved_seed"
                             ,"mid_Bought_from_agro_input_shop","mid_adoption_onfield","index_overallsec_plotF_midC")

results_farmer_sec_plot_base <- c("hybrid","OPV","farmer_saved_seed"
                                  ,"Bought_from_agro_input_shop","adoption_onfield","index_overallsec_plotF_baseC")

baseline_farmers[results_farmer_sec_plot_base] <- lapply(baseline_farmers[results_farmer_sec_plot_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_farmer_sec_plot)){
  ols <- lm(as.formula(paste(paste(results_farmer_sec_plot[i],"training_demeaned*clearing*farmer_demeaned",sep="~"),results_farmer_sec_plot_base[i],sep="+")),data=baseline_farmers)
  #ols <- lm(as.formula(paste(results_farmer_sec_plot[i],"training_demeaned*clearing*farmer_demeaned",sep="~")),data=baseline_farmers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_farmers$catchID,type="CR0")
  
  df_ols_F_sec_plot[1,2,i] <- coef_test(ols, vcov_cluster)$beta[3]
  df_ols_F_sec_plot[2,2,i] <- coef_test(ols, vcov_cluster)$SE[3]
  df_ols_F_sec_plot[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]}

###
#4#
###

baseline_farmers$farmer_control[baseline_farmers$farmer==0] <- TRUE
baseline_farmers$farmer_control[baseline_farmers$farmer==1] <- FALSE

#8.
index_overallsec_plotF_midF <- icwIndex(xmat=variables_overallsec_plotF_mid,sgroup = baseline_farmers$farmer_control,revcols = c(3))
baseline_farmers$index_overallsec_plotF_midF <- index_overallsec_plotF_midF$index

index_overallsec_plotF_baseF <- icwIndex(xmat=variables_overallsec_plotF_base,sgroup = baseline_farmers$farmer_control,revcols = c(3))
baseline_farmers$index_overallsec_plotF_baseF <- index_overallsec_plotF_baseF$index

results_farmer_sec_plot <- c("mid_hybrid","mid_OPV","mid_farmer_saved_seed"
                             ,"mid_Bought_from_agro_input_shop","mid_adoption_onfield","index_overallsec_plotF_midF")

results_farmer_sec_plot_base <- c("hybrid","OPV","farmer_saved_seed"
                                  ,"Bought_from_agro_input_shop","adoption_onfield","index_overallsec_plotF_baseF")

baseline_farmers[results_farmer_sec_plot_base] <- lapply(baseline_farmers[results_farmer_sec_plot_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_farmer_sec_plot)){
  ols <- lm(as.formula(paste(paste(results_farmer_sec_plot[i],"training_demeaned*clearing_demeaned*farmer",sep="~"),results_farmer_sec_plot_base[i],sep="+")),data=baseline_farmers)
  #ols <- lm(as.formula(paste(results_farmer_sec_plot[i],"training_demeaned*clearing_demeaned*farmer",sep="~")),data=baseline_farmers)
  vcov_cluster_shop <- vcovCR(ols,cluster=baseline_farmers$shop_ID,type="CR0")
  
  #farmer video treatment at village/shop level so no clustering needed
  df_ols_F_sec_plot[1,3,i] <- coef_test(ols, vcov_cluster_shop)$beta[4]
  df_ols_F_sec_plot[2,3,i] <- coef_test(ols, vcov_cluster_shop)$SE[4]
  df_ols_F_sec_plot[3,3,i] <- coef_test(ols, vcov_cluster_shop)$p_Satt[4]}

#Aker, Boumnijel, McClelland, Tierney (2012)
df_farmer_sec_plotT <- data.frame(baseline_farmers$mid_hybrid,baseline_farmers$mid_OPV
                                 ,baseline_farmers$mid_farmer_saved_seed
                                 ,baseline_farmers$mid_Bought_from_agro_input_shop,baseline_farmers$mid_adoption_onfield)
df_farmer_sec_plotC <- df_farmer_sec_plotT
df_farmer_sec_plotF <- df_farmer_sec_plotT
#no overall index

df_ols_F_sec_plot_J <- array(NA,dim=c(3,3,11))

results_farmer_sec_plot_J <- c("mid_hybrid","mid_OPV","mid_farmer_saved_seed","mid_Bought_from_agro_input_shop","mid_adoption_onfield")
#no overall index

for (i in 1:length(results_farmer_sec_plot_J)){
  df_ols_F_sec_plot_J[3,1,i] <- adjust_p(df_ols_F_sec_plot[3,1,i],df_farmer_sec_plotT,i)
  df_ols_F_sec_plot_J[3,2,i] <- adjust_p(df_ols_F_sec_plot[3,2,i],df_farmer_sec_plotC,i)
  df_ols_F_sec_plot_J[3,3,i] <- adjust_p(df_ols_F_sec_plot[3,3,i],df_farmer_sec_plotF,i)}










################################################################################################################################################################################
##### 12 ANALYSIS: Farmer - Secondary: Seed on plot ############################################################################################################################
################################################################################################################################################################################

#1. seed on plot rating
baseline_farmers$mid_Check2.check.maize.q35a <- baseline_farmers$check.maize.q35a
baseline_farmers$mid_Check2.check.maize.q35b <- baseline_farmers$check.maize.q35b
baseline_farmers$mid_Check2.check.maize.q35c <- baseline_farmers$check.maize.q35c
baseline_farmers$mid_Check2.check.maize.q35d <- baseline_farmers$check.maize.q35d
baseline_farmers$mid_Check2.check.maize.q35e <- baseline_farmers$check.maize.q35e
baseline_farmers$mid_Check2.check.maize.q35f <- baseline_farmers$check.maize.q35f
#baseline_farmers$mid_Check2.check.maize.q35g <- baseline_farmers$Check2.check.maize.q35g
baseline_farmers$mid_Check2.check.maize.q35h <- baseline_farmers$check.maize.q35h
baseline_farmers$mid_Check2.check.maize.q35i <- baseline_farmers$check.maize.q35i
baseline_farmers$mid_Check2.check.maize.q35j <- baseline_farmers$check.maize.q35j

variables_ratingplot_mid <- cbind(baseline_farmers$mid_Check2.check.maize.q35a,baseline_farmers$mid_Check2.check.maize.q35b,baseline_farmers$mid_Check2.check.maize.q35c,
                                  baseline_farmers$mid_Check2.check.maize.q35d,baseline_farmers$mid_Check2.check.maize.q35e
                                  ,baseline_farmers$mid_Check2.check.maize.q35j)
variables_ratingplot_base <- cbind(baseline_farmers$Check2.check.maize.q35a,baseline_farmers$Check2.check.maize.q35b,baseline_farmers$Check2.check.maize.q35c,
                                   baseline_farmers$Check2.check.maize.q35d,baseline_farmers$Check2.check.maize.q35e
                                   ,baseline_farmers$Check2.check.maize.q35j)

index_ratingplot_mid <- icwIndex(xmat=variables_ratingplot_mid)
baseline_farmers$index_ratingplot_mid <- index_ratingplot_mid$index

index_ratingplot_base <- icwIndex(xmat=variables_ratingplot_base)
baseline_farmers$index_ratingplot_base <- index_ratingplot_base$index

#2. satisfied
baseline_farmers$mid_Check2.check.maize.q36 <- baseline_farmers$check.maize.q36
baseline_farmers$mid_Check2.check.maize.q36<-ifelse(baseline_farmers$mid_Check2.check.maize.q36=="Yes",1,0)

#3. use again
baseline_farmers$mid_Check2.check.maize.q37 <- baseline_farmers$check.maize.q37
baseline_farmers$mid_Check2.check.maize.q37<-ifelse(baseline_farmers$mid_Check2.check.maize.q37=="Yes",1,0)

#4. Q38. How much seed did you use  on **${plot_select_name}** in the second season (entoigo) of 2020? **(in kg)**?
baseline_farmers$Check2.check.maize.q38_untrimmed <- baseline_farmers$Check2.check.maize.q38
baseline_farmers <- trim("Check2.check.maize.q38",baseline_farmers,trim_perc=.05)

baseline_farmers$mid_Check2.check.maize.q38 <- as.numeric(as.character(baseline_farmers$check.maize.q38))
baseline_farmers$mid_Check2.check.maize.q38_untrimmed <- baseline_farmers$mid_Check2.check.maize.q38
baseline_farmers <- trim("mid_Check2.check.maize.q38",baseline_farmers,trim_perc=.05)

#5. Q39. How much was the cost of 1 kg of this seed? (in UGX)
baseline_farmers$Check2.check.maize.q39[baseline_farmers$Check2.check.maize.q32=="a"] <- 0
baseline_farmers$Check2.check.maize.q39_untrimmed <- baseline_farmers$Check2.check.maize.q39
baseline_farmers <- trim("Check2.check.maize.q39",baseline_farmers,trim_perc=.05)

baseline_farmers$mid_Check2.check.maize.q39 <- baseline_farmers$check.maize.q39
baseline_farmers$mid_Check2.check.maize.q39 <- as.numeric(as.character(baseline_farmers$mid_Check2.check.maize.q39))
baseline_farmers$mid_Check2.check.maize.q39[baseline_farmers$mid_Check2.check.maize.q32=="a"] <- 0
baseline_farmers$mid_Check2.check.maize.q39_untrimmed <- baseline_farmers$mid_Check2.check.maize.q39
baseline_farmers <- trim("mid_Check2.check.maize.q39",baseline_farmers,trim_perc=.05)

#6. cost
baseline_farmers$costforseed_new <- baseline_farmers$Check2.check.maize.q38_untrimmed*baseline_farmers$Check2.check.maize.q39_untrimmed

baseline_farmers$costforseed_new_untrimmed <- baseline_farmers$costforseed_new
baseline_farmers$costforseed_new <- ihs(baseline_farmers$costforseed_new)
baseline_farmers <- trim("costforseed_new",baseline_farmers,trim_perc=.05)

baseline_farmers$mid_costforseed_new <- baseline_farmers$mid_Check2.check.maize.q38_untrimmed*baseline_farmers$mid_Check2.check.maize.q39_untrimmed
baseline_farmers$mid_costforseed_new <- ihs(baseline_farmers$mid_costforseed_new)
baseline_farmers <- trim("mid_costforseed_new",baseline_farmers,trim_perc=.05)

#7. overall
variables_overall_seedonplot_mid <- cbind(baseline_farmers$index_ratingplot_mid,baseline_farmers$mid_Check2.check.maize.q36
                                          ,baseline_farmers$mid_Check2.check.maize.q37,baseline_farmers$mid_Check2.check.maize.q38
                                          ,baseline_farmers$mid_Check2.check.maize.q39)
variables_overall_seedonplot_base <- cbind(baseline_farmers$index_ratingplot_base,baseline_farmers$Check2.check.maize.q36
                                           ,baseline_farmers$Check2.check.maize.q37,baseline_farmers$Check2.check.maize.q38
                                           ,baseline_farmers$Check2.check.maize.q39)

index_overall_seedonplot_mid <- icwIndex(xmat=variables_overall_seedonplot_mid,revcols = c(4))
baseline_farmers$index_overall_seedonplot_mid <- index_overall_seedonplot_mid$index

index_overall_seedonplot_base <- icwIndex(xmat=variables_overall_seedonplot_base,revcols = c(4))
baseline_farmers$index_overall_seedonplot_base <- index_overall_seedonplot_base$index

################################################################################################################################################################################

###
#1#
###

results_farmer_sec_seed <- c("index_ratingplot_mid","mid_Check2.check.maize.q36","mid_Check2.check.maize.q37"
                             ,"mid_Check2.check.maize.q38","mid_Check2.check.maize.q39","mid_costforseed_new"
                             ,"index_overall_seedonplot_mid")

results_farmer_sec_seed_base <- c("index_ratingplot_base","Check2.check.maize.q36","Check2.check.maize.q37"
                                  ,"Check2.check.maize.q38","Check2.check.maize.q39","costforseed_new"
                                  ,"index_overall_seedonplot_base")

baseline_farmers[results_farmer_sec_seed_base] <- lapply(baseline_farmers[results_farmer_sec_seed_base],function(x)x - mean(x,na.rm = T))

df_means_F_sec_seed <- array(NA,dim=c(3,11))

for (i in 1:length(results_farmer_sec_seed)){
  df_means_F_sec_seed[1,i] <- sum(baseline_farmers[results_farmer_sec_seed[i]], na.rm=T)/(nrow(baseline_farmers)-sum(is.na(baseline_farmers[results_farmer_sec_seed[i]])))
  df_means_F_sec_seed[2,i] <- sqrt(var(baseline_farmers[results_farmer_sec_seed[i]], na.rm=T))
  df_means_F_sec_seed[3,i] <- nrow(baseline_farmers)-sum(is.na(baseline_farmers[results_farmer_sec_seed[i]]))-sum(is.na(baseline_farmers[results_farmer_sec_seed_base[i]]))+sum(is.na(baseline_farmers[results_farmer_sec_seed[i]])&is.na(baseline_farmers[results_farmer_sec_seed_base[i]]))}

###
#2#
###

baseline_farmers$training_control[baseline_farmers$training==0] <- TRUE
baseline_farmers$training_control[baseline_farmers$training==1] <- FALSE

#1.
index_ratingplot_midT <- icwIndex(xmat=variables_ratingplot_mid,sgroup = baseline_farmers$training_control)
baseline_farmers$index_ratingplot_midT <- index_ratingplot_midT$index

index_ratingplot_baseT <- icwIndex(xmat=variables_ratingplot_base,sgroup = baseline_farmers$training_control)
baseline_farmers$index_ratingplot_baseT <- index_ratingplot_baseT$index

#7.
index_overall_seedonplot_midT <- icwIndex(xmat=variables_overall_seedonplot_mid,sgroup = baseline_farmers$training_control,revcols = c(4))
baseline_farmers$index_overall_seedonplot_midT <- index_overall_seedonplot_midT$index

index_overall_seedonplot_baseT <- icwIndex(xmat=variables_overall_seedonplot_base,sgroup = baseline_farmers$training_control,revcols = c(4))
baseline_farmers$index_overall_seedonplot_baseT <- index_overall_seedonplot_baseT$index

df_ols_F_sec_seed <- array(NA,dim=c(3,3,11))

results_farmer_sec_seed <- c("index_ratingplot_midT","mid_Check2.check.maize.q36","mid_Check2.check.maize.q37"
                             ,"mid_Check2.check.maize.q38","mid_Check2.check.maize.q39","mid_costforseed_new"
                             ,"index_overall_seedonplot_midT")

results_farmer_sec_seed_base <- c("index_ratingplot_baseT","Check2.check.maize.q36","Check2.check.maize.q37"
                                  ,"Check2.check.maize.q38","Check2.check.maize.q39","costforseed_new"
                                  ,"index_overall_seedonplot_baseT")

baseline_farmers[results_farmer_sec_seed_base] <- lapply(baseline_farmers[results_farmer_sec_seed_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_farmer_sec_seed)){
  ols <- lm(as.formula(paste(paste(results_farmer_sec_seed[i],"training*clearing_demeaned*farmer_demeaned",sep="~"),results_farmer_sec_seed_base[i],sep="+")),data=baseline_farmers)
  #ols <- lm(as.formula(paste(results_farmer_sec_seed[i],"training*clearing_demeaned*farmer_demeaned",sep="~")),data=baseline_farmers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_farmers$catchID,type="CR0")
  
  df_ols_F_sec_seed[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_ols_F_sec_seed[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_ols_F_sec_seed[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]}

###
#3#
###

baseline_farmers$clearing_control[baseline_farmers$clearing==0] <- TRUE
baseline_farmers$clearing_control[baseline_farmers$clearing==1] <- FALSE

#1.
index_ratingplot_midC <- icwIndex(xmat=variables_ratingplot_mid,sgroup = baseline_farmers$clearing_control)
baseline_farmers$index_ratingplot_midC <- index_ratingplot_midC$index

index_ratingplot_baseC <- icwIndex(xmat=variables_ratingplot_base,sgroup = baseline_farmers$clearing_control)
baseline_farmers$index_ratingplot_baseC <- index_ratingplot_baseC$index

#7.
index_overall_seedonplot_midC <- icwIndex(xmat=variables_overall_seedonplot_mid,sgroup = baseline_farmers$clearing_control,revcols = c(4))
baseline_farmers$index_overall_seedonplot_midC <- index_overall_seedonplot_midC$index

index_overall_seedonplot_baseC <- icwIndex(xmat=variables_overall_seedonplot_base,sgroup = baseline_farmers$clearing_control,revcols = c(4))
baseline_farmers$index_overall_seedonplot_baseC <- index_overall_seedonplot_baseC$index

results_farmer_sec_seed <- c("index_ratingplot_midC","mid_Check2.check.maize.q36","mid_Check2.check.maize.q37"
                             ,"mid_Check2.check.maize.q38","mid_Check2.check.maize.q39","mid_costforseed_new"
                             ,"index_overall_seedonplot_midC")

results_farmer_sec_seed_base <- c("index_ratingplot_baseC","Check2.check.maize.q36","Check2.check.maize.q37"
                                  ,"Check2.check.maize.q38","Check2.check.maize.q39","costforseed_new",
                                  "index_overall_seedonplot_baseC")

baseline_farmers[results_farmer_sec_seed_base] <- lapply(baseline_farmers[results_farmer_sec_seed_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_farmer_sec_seed)){
  ols <- lm(as.formula(paste(paste(results_farmer_sec_seed[i],"training_demeaned*clearing*farmer_demeaned",sep="~"),results_farmer_sec_seed_base[i],sep="+")),data=baseline_farmers)
  #ols <- lm(as.formula(paste(results_farmer_sec_seed[i],"training_demeaned*clearing*farmer_demeaned",sep="~")),data=baseline_farmers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_farmers$catchID,type="CR0")
  
  df_ols_F_sec_seed[1,2,i] <- coef_test(ols, vcov_cluster)$beta[3]
  df_ols_F_sec_seed[2,2,i] <- coef_test(ols, vcov_cluster)$SE[3]
  df_ols_F_sec_seed[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]}

###
#4#
###

baseline_farmers$farmer_control[baseline_farmers$farmer==0] <- TRUE
baseline_farmers$farmer_control[baseline_farmers$farmer==1] <- FALSE

#1.
index_ratingplot_midF <- icwIndex(xmat=variables_ratingplot_mid,sgroup = baseline_farmers$farmer_control)
baseline_farmers$index_ratingplot_midF <- index_ratingplot_midF$index

index_ratingplot_baseF <- icwIndex(xmat=variables_ratingplot_base,sgroup = baseline_farmers$farmer_control)
baseline_farmers$index_ratingplot_baseF <- index_ratingplot_baseF$index

#7.
index_overall_seedonplot_midF <- icwIndex(xmat=variables_overall_seedonplot_mid,sgroup = baseline_farmers$farmer_control,revcols = c(4))
baseline_farmers$index_overall_seedonplot_midF <- index_overall_seedonplot_midF$index

index_overall_seedonplot_baseF <- icwIndex(xmat=variables_overall_seedonplot_base,sgroup = baseline_farmers$farmer_control,revcols = c(4))
baseline_farmers$index_overall_seedonplot_baseF <- index_overall_seedonplot_baseF$index

results_farmer_sec_seed <- c("index_ratingplot_midF","mid_Check2.check.maize.q36","mid_Check2.check.maize.q37"
                             ,"mid_Check2.check.maize.q38","mid_Check2.check.maize.q39","mid_costforseed_new"
                             ,"index_overall_seedonplot_midF")

results_farmer_sec_seed_base <- c("index_ratingplot_baseF","Check2.check.maize.q36","Check2.check.maize.q37"
                                  ,"Check2.check.maize.q38","Check2.check.maize.q39","costforseed_new"
                                  ,"index_overall_seedonplot_baseF")

baseline_farmers[results_farmer_sec_seed_base] <- lapply(baseline_farmers[results_farmer_sec_seed_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_farmer_sec_seed)){
  ols <- lm(as.formula(paste(paste(results_farmer_sec_seed[i],"training_demeaned*clearing_demeaned*farmer",sep="~"),results_farmer_sec_seed_base[i],sep="+")),data=baseline_farmers)
  #ols <- lm(as.formula(paste(results_farmer_sec_seed[i],"training_demeaned*clearing_demeaned*farmer",sep="~")),data=baseline_farmers)
  vcov_cluster_shop <- vcovCR(ols,cluster=baseline_farmers$shop_ID,type="CR0")
  
  #farmer video treatment at village/shop level so no clustering needed
  df_ols_F_sec_seed[1,3,i] <- coef_test(ols, vcov_cluster_shop)$beta[4]
  df_ols_F_sec_seed[2,3,i] <- coef_test(ols, vcov_cluster_shop)$SE[4]
  df_ols_F_sec_seed[3,3,i] <- coef_test(ols, vcov_cluster_shop)$p_Satt[4]}

#Aker, Boumnijel, McClelland, Tierney (2012)
df_farmer_sec_seedT <- data.frame(baseline_farmers$index_ratingplot_midT,baseline_farmers$mid_Check2.check.maize.q36
                                  ,baseline_farmers$mid_Check2.check.maize.q37,baseline_farmers$mid_Check2.check.maize.q38
                                  ,baseline_farmers$mid_Check2.check.maize.q39,baseline_farmers$mid_costforseed_new)
df_farmer_sec_seedC <- data.frame(baseline_farmers$index_ratingplot_midC,baseline_farmers$mid_Check2.check.maize.q36
                                  ,baseline_farmers$mid_Check2.check.maize.q37,baseline_farmers$mid_Check2.check.maize.q38
                                  ,baseline_farmers$mid_Check2.check.maize.q39,baseline_farmers$mid_costforseed_new)
df_farmer_sec_seedF <- data.frame(baseline_farmers$index_ratingplot_midF,baseline_farmers$mid_Check2.check.maize.q36
                                  ,baseline_farmers$mid_Check2.check.maize.q37,baseline_farmers$mid_Check2.check.maize.q38
                                  ,baseline_farmers$mid_Check2.check.maize.q39,baseline_farmers$mid_costforseed_new)
#no overall index

df_ols_F_sec_seed_J <- array(NA,dim=c(3,3,11))

results_farmer_sec_seed_J <- c("index_ratingplot_mid","mid_Check2.check.maize.q36","mid_Check2.check.maize.q37"
                               ,"mid_Check2.check.maize.q38","mid_Check2.check.maize.q39","mid_costforseed_new")
#no overall index

for (i in 1:length(results_farmer_sec_seed_J)){
  df_ols_F_sec_seed_J[3,1,i] <- adjust_p(df_ols_F_sec_seed[3,1,i],df_farmer_sec_seedT,i)
  df_ols_F_sec_seed_J[3,2,i] <- adjust_p(df_ols_F_sec_seed[3,2,i],df_farmer_sec_seedC,i)
  df_ols_F_sec_seed_J[3,3,i] <- adjust_p(df_ols_F_sec_seed[3,3,i],df_farmer_sec_seedF,i)}










################################################################################################################################################################################
##### 13 ANALYSIS: Farmer - Secondary: Yield etc. ##############################################################################################################################
################################################################################################################################################################################

#1. Production in kg
baseline_farmers$yield_inkg_untrimmed <- baseline_farmers$yield_inkg
baseline_farmers <- trim("yield_inkg",baseline_farmers,trim_perc=.05)

baseline_farmers$mid_Check2.check.maize.q50 <- as.numeric(as.character(baseline_farmers$check.maize.q50))
baseline_farmers$mid_Check2.check.maize.q51 <- as.numeric(as.character(baseline_farmers$check.maize.q51))
baseline_farmers$mid_yield_inkg <- baseline_farmers$mid_Check2.check.maize.q50*baseline_farmers$mid_Check2.check.maize.q51
baseline_farmers$mid_yield_inkg_untrimmed <- baseline_farmers$mid_yield_inkg
baseline_farmers <- trim("mid_yield_inkg",baseline_farmers,trim_perc=.05)

#2. yield
baseline_farmers$landproductivity_untrimmed <- baseline_farmers$landproductivity
baseline_farmers <- trim("landproductivity",baseline_farmers,trim_perc=.05)

baseline_farmers$mid_Check2.check.maize.q29 <- as.numeric(as.character(baseline_farmers$check.maize.q29))
baseline_farmers$mid_landproductivity <- baseline_farmers$mid_yield_inkg_untrimmed/baseline_farmers$mid_Check2.check.maize.q29 #yield in kg per acre
baseline_farmers <- trim("mid_landproductivity",baseline_farmers,trim_perc=.05)

#4. amount sold
baseline_farmers$Check2.check.maize.q54[baseline_farmers$Check2.check.maize.q53==0] <- 0
baseline_farmers$soldinkg <- baseline_farmers$Check2.check.maize.q54*baseline_farmers$Check2.check.maize.q51
baseline_farmers$soldinkg <- ihs(baseline_farmers$soldinkg)
baseline_farmers <- trim("soldinkg",baseline_farmers,trim_perc=.05)

baseline_farmers$mid_Check2.check.maize.q53 <- baseline_farmers$check.maize.q53
baseline_farmers$mid_Check2.check.maize.q53<-ifelse(baseline_farmers$mid_Check2.check.maize.q53=="Yes",1,0)
baseline_farmers$mid_Check2.check.maize.q54 <- baseline_farmers$check.maize.q54
baseline_farmers$mid_Check2.check.maize.q54 <- as.numeric(as.character(baseline_farmers$mid_Check2.check.maize.q54))

baseline_farmers$mid_Check2.check.maize.q54[baseline_farmers$mid_Check2.check.maize.q53==0] <- 0
baseline_farmers$mid_soldinkg <- baseline_farmers$mid_Check2.check.maize.q54*baseline_farmers$mid_Check2.check.maize.q51
baseline_farmers$mid_soldinkg <- ihs(baseline_farmers$mid_soldinkg)
baseline_farmers <- trim("mid_soldinkg",baseline_farmers,trim_perc=.05)

#6. revenue
baseline_farmers$revenueUGX[baseline_farmers$Check2.check.maize.q53==0] <- 0
baseline_farmers$revenueUGX <- ihs(baseline_farmers$revenueUGX)
baseline_farmers <- trim("revenueUGX",baseline_farmers,trim_perc=.05)

baseline_farmers$mid_Check2.check.maize.q55 <- baseline_farmers$check.maize.q55
baseline_farmers$mid_Check2.check.maize.q55 <- as.numeric(as.character(baseline_farmers$mid_Check2.check.maize.q55))

baseline_farmers$mid_revenueUGX <- baseline_farmers$mid_Check2.check.maize.q54*baseline_farmers$mid_Check2.check.maize.q55
baseline_farmers$mid_revenueUGX[baseline_farmers$mid_Check2.check.maize.q53==0] <- 0
baseline_farmers$mid_revenueUGX <- ihs(baseline_farmers$mid_revenueUGX)
baseline_farmers <- trim("mid_revenueUGX",baseline_farmers,trim_perc=.05)

#8.
variables_overall_yieldetc_mid <- cbind(baseline_farmers$mid_landproductivity
                                          ,baseline_farmers$mid_soldinkg,baseline_farmers$mid_revenueUGX)
variables_overall_yieldetc_base <- cbind(baseline_farmers$landproductivity
                                          ,baseline_farmers$soldinkg,baseline_farmers$revenueUGX)

index_overall_yieldetc_mid <- icwIndex(xmat=variables_overall_yieldetc_mid)
baseline_farmers$index_overall_yieldetc_mid <- index_overall_yieldetc_mid$index

index_overall_yieldetc_base <- icwIndex(xmat=variables_overall_yieldetc_base)
baseline_farmers$index_overall_yieldetc_base <- index_overall_yieldetc_base$index

################################################################################################################################################################################

###
#1#
###

results_farmer_sec_yieldetc <- c("mid_yield_inkg","mid_landproductivity","mid_soldinkg"
                                 ,"mid_revenueUGX","index_overall_yieldetc_mid")

results_farmer_sec_yieldetc_base <- c("yield_inkg","landproductivity","soldinkg"
                                      ,"revenueUGX","index_overall_yieldetc_base")

baseline_farmers[results_farmer_sec_yieldetc_base] <- lapply(baseline_farmers[results_farmer_sec_yieldetc_base],function(x)x - mean(x,na.rm = T))

df_means_F_sec_yieldetc <- array(NA,dim=c(3,11))

for (i in 1:length(results_farmer_sec_yieldetc)){
  df_means_F_sec_yieldetc[1,i] <- sum(baseline_farmers[results_farmer_sec_yieldetc[i]], na.rm=T)/(nrow(baseline_farmers)-sum(is.na(baseline_farmers[results_farmer_sec_yieldetc[i]])))
  df_means_F_sec_yieldetc[2,i] <- sqrt(var(baseline_farmers[results_farmer_sec_yieldetc[i]], na.rm=T))
  df_means_F_sec_yieldetc[3,i] <- nrow(baseline_farmers)-sum(is.na(baseline_farmers[results_farmer_sec_yieldetc[i]]))-sum(is.na(baseline_farmers[results_farmer_sec_yieldetc_base[i]]))+sum(is.na(baseline_farmers[results_farmer_sec_yieldetc[i]])&is.na(baseline_farmers[results_farmer_sec_yieldetc_base[i]]))}

###
#2#
###

baseline_farmers$training_control[baseline_farmers$training==0] <- TRUE
baseline_farmers$training_control[baseline_farmers$training==1] <- FALSE

#8.
index_overall_yieldetc_midT <- icwIndex(xmat=variables_overall_yieldetc_mid,sgroup = baseline_farmers$training_control)
baseline_farmers$index_overall_yieldetc_midT <- index_overall_yieldetc_midT$index

index_overall_yieldetc_baseT <- icwIndex(xmat=variables_overall_yieldetc_base,sgroup = baseline_farmers$training_control)
baseline_farmers$index_overall_yieldetc_baseT <- index_overall_yieldetc_baseT$index

df_ols_F_sec_yieldetc <- array(NA,dim=c(3,3,11))

results_farmer_sec_yieldetc <- c("mid_yield_inkg","mid_landproductivity","mid_soldinkg"
                                 ,"mid_revenueUGX","index_overall_yieldetc_midT")

results_farmer_sec_yieldetc_base <- c("yield_inkg","landproductivity","soldinkg"
                                      ,"revenueUGX","index_overall_yieldetc_baseT")

baseline_farmers[results_farmer_sec_yieldetc_base] <- lapply(baseline_farmers[results_farmer_sec_yieldetc_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_farmer_sec_yieldetc)){
  ols <- lm(as.formula(paste(paste(results_farmer_sec_yieldetc[i],"training*clearing_demeaned*farmer_demeaned",sep="~"),results_farmer_sec_yieldetc_base[i],sep="+")),data=baseline_farmers)
  #ols <- lm(as.formula(paste(results_farmer_sec_yieldetc[i],"training*clearing_demeaned*farmer_demeaned",sep="~")),data=baseline_farmers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_farmers$catchID,type="CR0")
  
  df_ols_F_sec_yieldetc[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_ols_F_sec_yieldetc[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_ols_F_sec_yieldetc[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]}

###
#3#
###

baseline_farmers$clearing_control[baseline_farmers$clearing==0] <- TRUE
baseline_farmers$clearing_control[baseline_farmers$clearing==1] <- FALSE

#8.
index_overall_yieldetc_midC <- icwIndex(xmat=variables_overall_yieldetc_mid,sgroup = baseline_farmers$clearing_control)
baseline_farmers$index_overall_yieldetc_midC <- index_overall_yieldetc_midC$index

index_overall_yieldetc_baseC <- icwIndex(xmat=variables_overall_yieldetc_base,sgroup = baseline_farmers$clearing_control)
baseline_farmers$index_overall_yieldetc_baseC <- index_overall_yieldetc_baseC$index

results_farmer_sec_yieldetc <- c("mid_yield_inkg","mid_landproductivity","mid_soldinkg"
                                 ,"mid_revenueUGX","index_overall_yieldetc_midC")

results_farmer_sec_yieldetc_base <- c("yield_inkg","landproductivity","soldinkg"
                                      ,"revenueUGX","index_overall_yieldetc_baseC")

baseline_farmers[results_farmer_sec_yieldetc_base] <- lapply(baseline_farmers[results_farmer_sec_yieldetc_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_farmer_sec_yieldetc)){
  ols <- lm(as.formula(paste(paste(results_farmer_sec_yieldetc[i],"training_demeaned*clearing*farmer_demeaned",sep="~"),results_farmer_sec_yieldetc_base[i],sep="+")),data=baseline_farmers)
  #ols <- lm(as.formula(paste(results_farmer_sec_yieldetc[i],"training_demeaned*clearing*farmer_demeaned",sep="~")),data=baseline_farmers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_farmers$catchID,type="CR0")
  
  df_ols_F_sec_yieldetc[1,2,i] <- coef_test(ols, vcov_cluster)$beta[3]
  df_ols_F_sec_yieldetc[2,2,i] <- coef_test(ols, vcov_cluster)$SE[3]
  df_ols_F_sec_yieldetc[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]}

###
#4#
###

baseline_farmers$farmer_control[baseline_farmers$farmer==0] <- TRUE
baseline_farmers$farmer_control[baseline_farmers$farmer==1] <- FALSE

#8.
index_overall_yieldetc_midF <- icwIndex(xmat=variables_overall_yieldetc_mid,sgroup = baseline_farmers$farmer_control)
baseline_farmers$index_overall_yieldetc_midF <- index_overall_yieldetc_midF$index

index_overall_yieldetc_baseF <- icwIndex(xmat=variables_overall_yieldetc_base,sgroup = baseline_farmers$farmer_control)
baseline_farmers$index_overall_yieldetc_baseF <- index_overall_yieldetc_baseF$index

results_farmer_sec_yieldetc <- c("mid_yield_inkg","mid_landproductivity","mid_soldinkg"
                                 ,"mid_revenueUGX","index_overall_yieldetc_midF")

results_farmer_sec_yieldetc_base <- c("yield_inkg","landproductivity","soldinkg"
                                      ,"revenueUGX","index_overall_yieldetc_baseF")

baseline_farmers[results_farmer_sec_yieldetc_base] <- lapply(baseline_farmers[results_farmer_sec_yieldetc_base],function(x)x - mean(x,na.rm = T))

for (i in 1:length(results_farmer_sec_yieldetc)){
  ols <- lm(as.formula(paste(paste(results_farmer_sec_yieldetc[i],"training_demeaned*clearing_demeaned*farmer",sep="~"),results_farmer_sec_yieldetc_base[i],sep="+")),data=baseline_farmers)
  #ols <- lm(as.formula(paste(results_farmer_sec_yieldetc[i],"training_demeaned*clearing_demeaned*farmer",sep="~")),data=baseline_farmers)
  vcov_cluster_shop <- vcovCR(ols,cluster=baseline_farmers$shop_ID,type="CR0")
  
  #farmer video treatment at village/shop level so no clustering needed
  df_ols_F_sec_yieldetc[1,3,i] <- coef_test(ols, vcov_cluster_shop)$beta[4]
  df_ols_F_sec_yieldetc[2,3,i] <- coef_test(ols, vcov_cluster_shop)$SE[4]
  df_ols_F_sec_yieldetc[3,3,i] <- coef_test(ols, vcov_cluster_shop)$p_Satt[4]}

#Aker, Boumnijel, McClelland, Tierney (2012)
df_farmer_sec_yieldetcT <- data.frame(baseline_farmers$mid_yield_inkg,baseline_farmers$mid_landproductivity
                                      ,baseline_farmers$mid_soldinkg,baseline_farmers$mid_revenueUGX)
df_farmer_sec_yieldetcC <- df_farmer_sec_yieldetcT
df_farmer_sec_yieldetcF <- df_farmer_sec_yieldetcT
#no overall index

df_ols_F_sec_yieldetc_J <- array(NA,dim=c(3,3,11))

results_farmer_sec_yieldetc_J <- c("mid_yield_inkg","mid_landproductivity"
                                   ,"mid_soldinkg","mid_revenueUGX")
#no overall index

for (i in 1:length(results_farmer_sec_yieldetc_J)){
  df_ols_F_sec_yieldetc_J[3,1,i] <- adjust_p(df_ols_F_sec_yieldetc[3,1,i],df_farmer_sec_yieldetcT,i)
  df_ols_F_sec_yieldetc_J[3,2,i] <- adjust_p(df_ols_F_sec_yieldetc[3,2,i],df_farmer_sec_yieldetcC,i)
  df_ols_F_sec_yieldetc_J[3,3,i] <- adjust_p(df_ols_F_sec_yieldetc[3,3,i],df_farmer_sec_yieldetcF,i)}










################################################################################################################################################################################
##### 9 ANALYSIS: Farmer - Primary and secondary: outcomes without baseline#####################################################################################################
################################################################################################################################################################################

#1. index of seed quality perception: average ratings of maize seed of all input dealers in catchment area (q68, q69 aggregated at household level)

nrowF <- nrow(baseline_farmers)

baseline_farmers$mid_seed_quality_general_rating=sample(na.omit(baseline_farmers$seed_quality_general_rating),nrowF,replace = T)
baseline_farmers$mid_seed_quality_general_rating <- as.numeric(as.character(baseline_farmers$mid_seed_quality_general_rating))

baseline_farmers$mid_seed_yield_rating=sample(na.omit(baseline_farmers$seed_yield_rating),nrowF,replace = T)
baseline_farmers$mid_seed_yield_rating <- as.numeric(as.character(baseline_farmers$mid_seed_yield_rating))

baseline_farmers$mid_seed_drought_rating=sample(na.omit(baseline_farmers$seed_drought_rating),nrowF,replace = T)
baseline_farmers$mid_seed_drought_rating <- as.numeric(as.character(baseline_farmers$mid_seed_drought_rating))

baseline_farmers$mid_seed_disease_rating=sample(na.omit(baseline_farmers$seed_disease_rating),nrowF,replace = T)
baseline_farmers$mid_seed_disease_rating <- as.numeric(as.character(baseline_farmers$mid_seed_disease_rating))

baseline_farmers$mid_seed_maturing_rating=sample(na.omit(baseline_farmers$seed_maturing_rating),nrowF,replace = T)
baseline_farmers$mid_seed_maturing_rating <- as.numeric(as.character(baseline_farmers$mid_seed_maturing_rating))

baseline_farmers$mid_seed_germinate_rating=sample(na.omit(baseline_farmers$seed_germinate_rating),nrowF,replace = T)
baseline_farmers$mid_seed_germinate_rating <- as.numeric(as.character(baseline_farmers$mid_seed_germinate_rating))

variables_ratingsF_mid <- cbind(baseline_farmers$mid_seed_quality_general_rating,baseline_farmers$mid_seed_yield_rating
                                ,baseline_farmers$mid_seed_drought_rating,baseline_farmers$mid_seed_disease_rating
                                ,baseline_farmers$mid_seed_maturing_rating,baseline_farmers$mid_seed_germinate_rating)

index_ratingsF_mid <- icwIndex(xmat=variables_ratingsF_mid)
baseline_farmers$index_ratingsF_mid <- index_ratingsF_mid$index

#2. Shop ratings
baseline_farmers$mid_general_rating=sample(na.omit(baseline_farmers$general_rating),nrowF,replace = T)
baseline_farmers$mid_general_rating <- as.numeric(as.character(baseline_farmers$mid_general_rating))

baseline_farmers$mid_location_rating=sample(na.omit(baseline_farmers$location_rating),nrowF,replace = T)
baseline_farmers$mid_location_rating <- as.numeric(as.character(baseline_farmers$mid_location_rating))

baseline_farmers$mid_price_rating=sample(na.omit(baseline_farmers$price_rating),nrowF,replace = T)
baseline_farmers$mid_price_rating <- as.numeric(as.character(baseline_farmers$mid_price_rating))

baseline_farmers$mid_quality_rating=sample(na.omit(baseline_farmers$quality_rating),nrowF,replace = T)
baseline_farmers$mid_quality_rating <- as.numeric(as.character(baseline_farmers$mid_quality_rating))

baseline_farmers$mid_stock_rating=sample(na.omit(baseline_farmers$stock_rating),nrowF,replace = T)
baseline_farmers$mid_stock_rating <- as.numeric(as.character(baseline_farmers$mid_stock_rating))

baseline_farmers$mid_reputation_rating=sample(na.omit(baseline_farmers$reputation_rating),nrowF,replace = T)
baseline_farmers$mid_reputation_rating <- as.numeric(as.character(baseline_farmers$mid_reputation_rating))

variables_ratingsshopF_mid <- cbind(baseline_farmers$mid_general_rating,baseline_farmers$mid_location_rating
                                    ,baseline_farmers$mid_price_rating,baseline_farmers$mid_quality_rating
                                    ,baseline_farmers$mid_stock_rating,baseline_farmers$mid_reputation_rating)

index_ratingsshopF_mid <- icwIndex(xmat=variables_ratingsshopF_mid)
baseline_farmers$index_ratingsshopF_mid <- index_ratingsshopF_mid$index

#3. switching
baseline_farmers$mid_farmerswitched<-ifelse(baseline_farmers$check.maize.q25i=="2",1,0)
baseline_farmers$mid_farmerswitched[is.na(baseline_farmers$check.maize.q25i)] <- 0

#4. Did you harvest as much maize from this **${plot_select_name}** plot in the second season (entoigo) of 2020 (including maize that was consumed) as you expected?
baseline_farmers$mid_Check2.check.maize.q51a <- baseline_farmers$check.maize.q51a
baseline_farmers$mid_Check2.check.maize.q51a<-ifelse(baseline_farmers$mid_Check2.check.maize.q51a=="Yes",1,0)

#5. if q51a=no: Why did you not harvest as much maize as you expected?
# a.	the weather was unfortunate
# b.	my planting material was poor
# c.	pests e.g. fall arm worm destroyed my harvest
# d.	my soil was not fertile
# e.	I did not plant on time !
# f.	I did not weed on time !
# g.	I did not manage pests e.g. fall arm worm well !
# h.	I did not apply complimentary inputs like fertilizer !
# i.	Others

baseline_farmers$mid_myownfault <- NA
baseline_farmers$mid_myownfault[baseline_farmers$mid_Check2.check.maize.q51a==1] <- 0

baseline_farmers$mid_Check2.check.maize.q51b <- baseline_farmers$check.maize.q51b

baseline_farmers$mid_myownfault[baseline_farmers$mid_Check2.check.maize.q51b=="a"] <- 0
baseline_farmers$mid_myownfault[baseline_farmers$mid_Check2.check.maize.q51b=="b"] <- 0
baseline_farmers$mid_myownfault[baseline_farmers$mid_Check2.check.maize.q51b=="c"] <- 0
baseline_farmers$mid_myownfault[baseline_farmers$mid_Check2.check.maize.q51b=="d"] <- 0
baseline_farmers$mid_myownfault[baseline_farmers$mid_Check2.check.maize.q51b=="e"] <- 1
baseline_farmers$mid_myownfault[baseline_farmers$mid_Check2.check.maize.q51b=="f"] <- 1
baseline_farmers$mid_myownfault[baseline_farmers$mid_Check2.check.maize.q51b=="g"] <- 1
baseline_farmers$mid_myownfault[baseline_farmers$mid_Check2.check.maize.q51b=="h"] <- 1
baseline_farmers$mid_myownfault[baseline_farmers$mid_Check2.check.maize.q51b=="i"] <- NA

#6. skill questions
baseline_farmers$index_skillsF_mid=sample(na.omit(baseline_farmers$index_practices_mid),nrowF,replace = T) #took any index as basis
#placeholder #to do endline (mind: T, CH, F have different indices)

#7. Q56. How much did you keep for seed (record in kg)?
baseline_farmers$Check2.check.maize.q56 <- ihs(baseline_farmers$Check2.check.maize.q56)
baseline_farmers <- trim("Check2.check.maize.q56",baseline_farmers,trim_perc=.05)

baseline_farmers$mid_Check2.check.maize.q56 <- baseline_farmers$check.maize.q56
baseline_farmers$mid_Check2.check.maize.q56 <- as.numeric(as.character(baseline_farmers$mid_Check2.check.maize.q56))
baseline_farmers$mid_Check2.check.maize.q56 <- ihs(baseline_farmers$mid_Check2.check.maize.q56)
baseline_farmers <- trim("mid_Check2.check.maize.q56",baseline_farmers,trim_perc=.05)

#XXXXX

#new overallprimF index because of mid_farmerswitched
variables_overallprimF_mid <- cbind(baseline_farmers$mid_Check2.check.maize.q25a,baseline_farmers$mid_agro
                                    ,baseline_farmers$mid_Check2.check.maize.q25d,baseline_farmers$mid_farmerswitched
                                    ,baseline_farmers$index_practices_mid,baseline_farmers$mid_Check2.check.maize.q25h
                                    ,baseline_farmers$mid_Land_Races)
#                                   ,baseline_farmers$index_ratingsF_mid,baseline_farmers$index_ratingsshopF_mid) #to do after endline (also: add (+) sign to tables)

index_overallprimF_mid <- icwIndex(xmat=variables_overallprimF_mid,revcols = c(6,7))
baseline_farmers$index_overallprimF_mid <- index_overallprimF_mid$index

#new overallsecF index because of mid_knows_dealer & index_skillsF_mid
variables_overallsecF_mid <- cbind(baseline_farmers$mid_number_known
                                   ,baseline_farmers$mid_knows_dealer)
                                   # ,baseline_farmers$mid_bought_last_season,index_skillsF_mid) #to do endline

index_overallsecF_mid <- icwIndex(xmat=variables_overallsecF_mid)
baseline_farmers$index_overallsecF_mid <- index_overallsecF_mid$index

#CREATE NEW INDEX BECAUSE 3 MORE VARIABLES
variables_overall_yieldetc_mid <- cbind(baseline_farmers$mid_landproductivity,baseline_farmers$mid_Check2.check.maize.q51a
                                        ,baseline_farmers$mid_Check2.check.maize.q51b,baseline_farmers$mid_soldinkg
                                        ,baseline_farmers$mid_revenueUGX,baseline_farmers$mid_Check2.check.maize.q56)

index_overall_yieldetc_mid <- icwIndex(xmat=variables_overall_yieldetc_mid,revcols=c(3,6))
baseline_farmers$index_overall_yieldetc_mid <- index_overall_yieldetc_mid$index

################################################################################################################################################################################

###
#1#
###

results_farmer_nobase <- c("index_ratingsF_mid","index_ratingsshopF_mid","mid_farmerswitched"
                           ,"mid_Check2.check.maize.q51a","mid_myownfault","index_skillsF_mid"
                           ,"index_overallprimF_mid","index_overallsecF_mid","index_overallsec_plotF_mid"
                           ,"index_overall_seedonplot_mid","index_overall_yieldetc_mid","mid_Check2.check.maize.q56")

df_means_F_nobase <- array(NA,dim=c(3,12))

for (i in 1:length(results_farmer_nobase)){
  df_means_F_nobase[1,i] <- sum(baseline_farmers[results_farmer_nobase[i]], na.rm=T)/(nrow(baseline_farmers)-sum(is.na(baseline_farmers[results_farmer_nobase[i]])))
  df_means_F_nobase[2,i] <- sqrt(var(baseline_farmers[results_farmer_nobase[i]], na.rm=T))
  df_means_F_nobase[3,i] <- nrow(baseline_farmers)-sum(is.na(baseline_farmers[results_farmer_nobase[i]]))}

###
#2#
###

df_ols_F_nobase <- array(NA,dim=c(3,3,12))

baseline_farmers$training_control[baseline_farmers$training==0] <- TRUE
baseline_farmers$training_control[baseline_farmers$training==1] <- FALSE

#1.
index_ratingsF_mid <- icwIndex(xmat=variables_ratingsF_mid,sgroup = baseline_farmers$training_control)
baseline_farmers$index_ratingsF_midT <- index_ratingsF_mid$index

#2.
index_ratingsshopF_mid <- icwIndex(xmat=variables_ratingsshopF_mid,sgroup = baseline_farmers$training_control)
baseline_farmers$index_ratingsshopF_midT <- index_ratingsshopF_mid$index

#new overallprimF
index_overallprimF_midT <- icwIndex(xmat=variables_overallprimF_mid,sgroup = baseline_farmers$training_control,revcols = c(6,7))
baseline_farmers$index_overallprimF_midT <- index_overallprimF_midT$index

#new overallsecF
index_overallsecF_midT <- icwIndex(xmat=variables_overallsecF_mid,sgroup = baseline_farmers$training_control)
baseline_farmers$index_overallsecF_midT <- index_overallsecF_midT$index

#new overall_yieldetc
index_overall_yieldetc_midT <- icwIndex(xmat=variables_overall_yieldetc_mid,sgroup = baseline_farmers$training_control,revcols=c(3,6))
baseline_farmers$index_overall_yieldetc_midT <- index_overall_yieldetc_midT$index

results_farmer_nobase <- c("index_ratingsF_midT","index_ratingsshopF_midT","mid_farmerswitched"
                           ,"mid_Check2.check.maize.q51a","mid_myownfault","index_skillsF_mid"
                           ,"index_overallprimF_midT","index_overallsecF_midT","index_overallsec_plotF_midT"
                           ,"index_overall_seedonplot_midT","index_overall_yieldetc_midT","mid_Check2.check.maize.q56")

for (i in 1:length(results_farmer_nobase)){
  ols <- lm(as.formula(paste(results_farmer_nobase[i],"training*clearing_demeaned*farmer_demeaned",sep="~")),data=baseline_farmers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_farmers$catchID,type="CR0")

  df_ols_F_nobase[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_ols_F_nobase[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_ols_F_nobase[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]}

###
#3#
###

baseline_farmers$clearing_control[baseline_farmers$clearing==0] <- TRUE
baseline_farmers$clearing_control[baseline_farmers$clearing==1] <- FALSE

#1.
index_ratingsF_mid <- icwIndex(xmat=variables_ratingsF_mid,sgroup = baseline_farmers$clearing_control)
baseline_farmers$index_ratingsF_midC <- index_ratingsF_mid$index

#2.
index_ratingsshopF_mid <- icwIndex(xmat=variables_ratingsshopF_mid,sgroup = baseline_farmers$clearing_control)
baseline_farmers$index_ratingsshopF_midC <- index_ratingsshopF_mid$index

#new overallprimF
index_overallprimF_midC <- icwIndex(xmat=variables_overallprimF_mid,sgroup = baseline_farmers$clearing_control,revcols = c(6,7))
baseline_farmers$index_overallprimF_midC <- index_overallprimF_midC$index

#new overallsecF
index_overallsecF_midC <- icwIndex(xmat=variables_overallsecF_mid,sgroup = baseline_farmers$clearing_control)
baseline_farmers$index_overallsecF_midC <- index_overallsecF_midC$index

#new overall_yieldetc
index_overall_yieldetc_midC <- icwIndex(xmat=variables_overall_yieldetc_mid,sgroup = baseline_farmers$clearing_control,revcols=c(3,6))
baseline_farmers$index_overall_yieldetc_midC <- index_overall_yieldetc_midC$index

results_farmer_nobase <- c("index_ratingsF_midC","index_ratingsshopF_midC","mid_farmerswitched"
                           ,"mid_Check2.check.maize.q51a","mid_myownfault","index_skillsF_mid"
                           ,"index_overallprimF_midC","index_overallsecF_midC","index_overallsec_plotF_midC"
                           ,"index_overall_seedonplot_midC","index_overall_yieldetc_midC","mid_Check2.check.maize.q56")

for (i in 1:length(results_farmer_nobase)){
  ols <- lm(as.formula(paste(results_farmer_nobase[i],"training_demeaned*clearing*farmer_demeaned",sep="~")),data=baseline_farmers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_farmers$catchID,type="CR0")

  df_ols_F_nobase[1,2,i] <- coef_test(ols, vcov_cluster)$beta[3]
  df_ols_F_nobase[2,2,i] <- coef_test(ols, vcov_cluster)$SE[3]
  df_ols_F_nobase[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]}

###
#4#
###

baseline_farmers$farmer_control[baseline_farmers$farmer==0] <- TRUE
baseline_farmers$farmer_control[baseline_farmers$farmer==1] <- FALSE

#1.
index_ratingsF_mid <- icwIndex(xmat=variables_ratingsF_mid,sgroup = baseline_farmers$farmer_control)
baseline_farmers$index_ratingsF_midF <- index_ratingsF_mid$index

#2.
index_ratingsshopF_mid <- icwIndex(xmat=variables_ratingsshopF_mid,sgroup = baseline_farmers$farmer_control)
baseline_farmers$index_ratingsshopF_midF <- index_ratingsshopF_mid$index

#new overallprimF
index_overallprimF_midF <- icwIndex(xmat=variables_overallprimF_mid,sgroup = baseline_farmers$farmer_control,revcols = c(6,7))
baseline_farmers$index_overallprimF_midF <- index_overallprimF_midF$index

#new overallsecF
index_overallsecF_midF <- icwIndex(xmat=variables_overallsecF_mid,sgroup = baseline_farmers$farmer_control)
baseline_farmers$index_overallsecF_midF <- index_overallsecF_midF$index

#new overall_yieldetc
index_overall_yieldetc_midF <- icwIndex(xmat=variables_overall_yieldetc_mid,sgroup = baseline_farmers$farmer_control,revcols=c(3,6))
baseline_farmers$index_overall_yieldetc_midF <- index_overall_yieldetc_midF$index

results_farmer_nobase <- c("index_ratingsF_midF","index_ratingsshopF_midF","mid_farmerswitched"
                           ,"mid_Check2.check.maize.q51a","mid_myownfault","index_skillsF_mid"
                           ,"index_overallprimF_midF","index_overallsecF_midF","index_overallsec_plotF_midF"
                           ,"index_overall_seedonplot_midF","index_overall_yieldetc_midF","mid_Check2.check.maize.q56")

for (i in 1:length(results_farmer_nobase)){
  ols <- lm(as.formula(paste(results_farmer_nobase[i],"training_demeaned*clearing_demeaned*farmer",sep="~")),data=baseline_farmers)
  vcov_cluster_shop <- vcovCR(ols,cluster=baseline_farmers$shop_ID,type="CR0")

  #farmer video treatment at village/shop level so no clustering needed
  df_ols_F_nobase[1,3,i] <- coef_test(ols, vcov_cluster_shop)$beta[4]
  df_ols_F_nobase[2,3,i] <- coef_test(ols, vcov_cluster_shop)$SE[4]
  df_ols_F_nobase[3,3,i] <- coef_test(ols, vcov_cluster_shop)$p_Satt[4]}









################################################################################################################################################################################
##### 9H ANALYSIS: Farmer - Primary and secondary: outcomes without baseline#####################################################################################################
##### Heterogeneity analyses: More competitive catchment areas
################################################################################################################################################################################

baseline_farmers_save=baseline_farmers
baseline_farmers=subset(baseline_farmers,small_catchID=="0")

variables_ratingsF_mid <- cbind(baseline_farmers$mid_seed_quality_general_rating,baseline_farmers$mid_seed_yield_rating
                                ,baseline_farmers$mid_seed_drought_rating,baseline_farmers$mid_seed_disease_rating
                                ,baseline_farmers$mid_seed_maturing_rating,baseline_farmers$mid_seed_germinate_rating)

index_ratingsF_mid <- icwIndex(xmat=variables_ratingsF_mid)
baseline_farmers$index_ratingsF_mid <- index_ratingsF_mid$index

variables_ratingsshopF_mid <- cbind(baseline_farmers$mid_general_rating,baseline_farmers$mid_location_rating
                                    ,baseline_farmers$mid_price_rating,baseline_farmers$mid_quality_rating
                                    ,baseline_farmers$mid_stock_rating,baseline_farmers$mid_reputation_rating)

index_ratingsshopF_mid <- icwIndex(xmat=variables_ratingsshopF_mid)
baseline_farmers$index_ratingsshopF_mid <- index_ratingsshopF_mid$index

variables_overallprimF_mid <- cbind(baseline_farmers$mid_Check2.check.maize.q25a,baseline_farmers$mid_agro
                                    ,baseline_farmers$mid_Check2.check.maize.q25d,baseline_farmers$mid_farmerswitched
                                    ,baseline_farmers$index_practices_mid,baseline_farmers$mid_Check2.check.maize.q25h
                                    ,baseline_farmers$mid_Land_Races)
#                                   ,baseline_farmers$index_ratingsF_mid,baseline_farmers$index_ratingsshopF_mid) #to do after endline (also: add (+) sign to tables)

index_overallprimF_mid <- icwIndex(xmat=variables_overallprimF_mid,revcols = c(6,7))
baseline_farmers$index_overallprimF_mid <- index_overallprimF_mid$index

variables_overallsecF_mid <- cbind(baseline_farmers$mid_number_known
                                   ,baseline_farmers$mid_knows_dealer)
# ,baseline_farmers$mid_bought_last_season,index_skillsF_mid) #to do endline

index_overallsecF_mid <- icwIndex(xmat=variables_overallsecF_mid)
baseline_farmers$index_overallsecF_mid <- index_overallsecF_mid$index

variables_overall_yieldetc_mid <- cbind(baseline_farmers$mid_landproductivity,baseline_farmers$mid_Check2.check.maize.q51a
                                        ,baseline_farmers$mid_Check2.check.maize.q51b,baseline_farmers$mid_soldinkg
                                        ,baseline_farmers$mid_revenueUGX,baseline_farmers$mid_Check2.check.maize.q56)

index_overall_yieldetc_mid <- icwIndex(xmat=variables_overall_yieldetc_mid,revcols=c(3,6))
baseline_farmers$index_overall_yieldetc_mid <- index_overall_yieldetc_mid$index

################################################################################################################################################################################

###
#1#
###

results_farmer_nobase <- c("index_ratingsF_mid","index_ratingsshopF_mid","mid_farmerswitched"
                           ,"mid_Check2.check.maize.q51a","mid_myownfault","index_skillsF_mid"
                           ,"index_overallprimF_mid","index_overallsecF_mid","index_overallsec_plotF_mid"
                           ,"index_overall_seedonplot_mid","index_overall_yieldetc_mid","mid_Check2.check.maize.q56")

df_means_F_nobase_het2 <- array(NA,dim=c(3,12))

for (i in 1:length(results_farmer_nobase)){
  df_means_F_nobase_het2[1,i] <- sum(baseline_farmers[results_farmer_nobase[i]], na.rm=T)/(nrow(baseline_farmers)-sum(is.na(baseline_farmers[results_farmer_nobase[i]])))
  df_means_F_nobase_het2[2,i] <- sqrt(var(baseline_farmers[results_farmer_nobase[i]], na.rm=T))
  df_means_F_nobase_het2[3,i] <- nrow(baseline_farmers)-sum(is.na(baseline_farmers[results_farmer_nobase[i]]))}

###
#2#
###

df_ols_F_nobase_het2 <- array(NA,dim=c(3,3,12))

baseline_farmers$training_control[baseline_farmers$training==0] <- TRUE
baseline_farmers$training_control[baseline_farmers$training==1] <- FALSE

#1.
index_ratingsF_mid <- icwIndex(xmat=variables_ratingsF_mid,sgroup = baseline_farmers$training_control)
baseline_farmers$index_ratingsF_midT <- index_ratingsF_mid$index

#2.
index_ratingsshopF_mid <- icwIndex(xmat=variables_ratingsshopF_mid,sgroup = baseline_farmers$training_control)
baseline_farmers$index_ratingsshopF_midT <- index_ratingsshopF_mid$index

#new overallprimF
index_overallprimF_midT <- icwIndex(xmat=variables_overallprimF_mid,sgroup = baseline_farmers$training_control,revcols = c(6,7))
baseline_farmers$index_overallprimF_midT <- index_overallprimF_midT$index

#new overallsecF
index_overallsecF_midT <- icwIndex(xmat=variables_overallsecF_mid,sgroup = baseline_farmers$training_control)
baseline_farmers$index_overallsecF_midT <- index_overallsecF_midT$index

#new overall_yieldetc
index_overall_yieldetc_midT <- icwIndex(xmat=variables_overall_yieldetc_mid,sgroup = baseline_farmers$training_control,revcols=c(3,6))
baseline_farmers$index_overall_yieldetc_midT <- index_overall_yieldetc_midT$index

results_farmer_nobase <- c("index_ratingsF_midT","index_ratingsshopF_midT","mid_farmerswitched"
                           ,"mid_Check2.check.maize.q51a","mid_myownfault","index_skillsF_mid"
                           ,"index_overallprimF_midT","index_overallsecF_midT","index_overallsec_plotF_midT"
                           ,"index_overall_seedonplot_midT","index_overall_yieldetc_midT","mid_Check2.check.maize.q56")

for (i in 1:length(results_farmer_nobase)){
  ols <- lm(as.formula(paste(results_farmer_nobase[i],"training*clearing_demeaned*farmer_demeaned",sep="~")),data=baseline_farmers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_farmers$catchID,type="CR0")
  
  df_ols_F_nobase_het2[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_ols_F_nobase_het2[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_ols_F_nobase_het2[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]}

###
#3#
###

baseline_farmers$clearing_control[baseline_farmers$clearing==0] <- TRUE
baseline_farmers$clearing_control[baseline_farmers$clearing==1] <- FALSE

#1.
index_ratingsF_mid <- icwIndex(xmat=variables_ratingsF_mid,sgroup = baseline_farmers$clearing_control)
baseline_farmers$index_ratingsF_midC <- index_ratingsF_mid$index

#2.
index_ratingsshopF_mid <- icwIndex(xmat=variables_ratingsshopF_mid,sgroup = baseline_farmers$clearing_control)
baseline_farmers$index_ratingsshopF_midC <- index_ratingsshopF_mid$index

#new overallprimF
index_overallprimF_midC <- icwIndex(xmat=variables_overallprimF_mid,sgroup = baseline_farmers$clearing_control,revcols = c(6,7))
baseline_farmers$index_overallprimF_midC <- index_overallprimF_midC$index

#new overallsecF
index_overallsecF_midC <- icwIndex(xmat=variables_overallsecF_mid,sgroup = baseline_farmers$clearing_control)
baseline_farmers$index_overallsecF_midC <- index_overallsecF_midC$index

#new overall_yieldetc
index_overall_yieldetc_midC <- icwIndex(xmat=variables_overall_yieldetc_mid,sgroup = baseline_farmers$clearing_control,revcols=c(3,6))
baseline_farmers$index_overall_yieldetc_midC <- index_overall_yieldetc_midC$index

results_farmer_nobase <- c("index_ratingsF_midC","index_ratingsshopF_midC","mid_farmerswitched"
                           ,"mid_Check2.check.maize.q51a","mid_myownfault","index_skillsF_mid"
                           ,"index_overallprimF_midC","index_overallsecF_midC","index_overallsec_plotF_midC"
                           ,"index_overall_seedonplot_midC","index_overall_yieldetc_midC","mid_Check2.check.maize.q56")

for (i in 1:length(results_farmer_nobase)){
  ols <- lm(as.formula(paste(results_farmer_nobase[i],"training_demeaned*clearing*farmer_demeaned",sep="~")),data=baseline_farmers)
  vcov_cluster <- vcovCR(ols,cluster=baseline_farmers$catchID,type="CR0")
  
  df_ols_F_nobase_het2[1,2,i] <- coef_test(ols, vcov_cluster)$beta[3]
  df_ols_F_nobase_het2[2,2,i] <- coef_test(ols, vcov_cluster)$SE[3]
  df_ols_F_nobase_het2[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]}

###
#4#
###

baseline_farmers$farmer_control[baseline_farmers$farmer==0] <- TRUE
baseline_farmers$farmer_control[baseline_farmers$farmer==1] <- FALSE

#1.
index_ratingsF_mid <- icwIndex(xmat=variables_ratingsF_mid,sgroup = baseline_farmers$farmer_control)
baseline_farmers$index_ratingsF_midF <- index_ratingsF_mid$index

#2.
index_ratingsshopF_mid <- icwIndex(xmat=variables_ratingsshopF_mid,sgroup = baseline_farmers$farmer_control)
baseline_farmers$index_ratingsshopF_midF <- index_ratingsshopF_mid$index

#new overallprimF
index_overallprimF_midF <- icwIndex(xmat=variables_overallprimF_mid,sgroup = baseline_farmers$farmer_control,revcols = c(6,7))
baseline_farmers$index_overallprimF_midF <- index_overallprimF_midF$index

#new overallsecF
index_overallsecF_midF <- icwIndex(xmat=variables_overallsecF_mid,sgroup = baseline_farmers$farmer_control)
baseline_farmers$index_overallsecF_midF <- index_overallsecF_midF$index

#new overall_yieldetc
index_overall_yieldetc_midF <- icwIndex(xmat=variables_overall_yieldetc_mid,sgroup = baseline_farmers$farmer_control,revcols=c(3,6))
baseline_farmers$index_overall_yieldetc_midF <- index_overall_yieldetc_midF$index

results_farmer_nobase <- c("index_ratingsF_midF","index_ratingsshopF_midF","mid_farmerswitched"
                           ,"mid_Check2.check.maize.q51a","mid_myownfault","index_skillsF_mid"
                           ,"index_overallprimF_midF","index_overallsecF_midF","index_overallsec_plotF_midF"
                           ,"index_overall_seedonplot_midF","index_overall_yieldetc_midF","mid_Check2.check.maize.q56")

for (i in 1:length(results_farmer_nobase)){
  ols <- lm(as.formula(paste(results_farmer_nobase[i],"training_demeaned*clearing_demeaned*farmer",sep="~")),data=baseline_farmers)
  vcov_cluster_shop <- vcovCR(ols,cluster=baseline_farmers$shop_ID,type="CR0")
  
  #farmer video treatment at village/shop level so no clustering needed
  df_ols_F_nobase_het2[1,3,i] <- coef_test(ols, vcov_cluster_shop)$beta[4]
  df_ols_F_nobase_het2[2,3,i] <- coef_test(ols, vcov_cluster_shop)$SE[4]
  df_ols_F_nobase_het2[3,3,i] <- coef_test(ols, vcov_cluster_shop)$p_Satt[4]}

baseline_farmers=baseline_farmers_save











# #OTHER CALCULATIONS
# #OC1. difference in ratings between dealers and farmers
# 
# #attention: only CH treated farmers
# 
# mean(baseline_dealers$maize.owner.agree.q101,na.rm = T) #4.045977
# mean(baseline_dealers$quality_rating,na.rm = T) #3.759996
# 
# t.test(baseline_dealers$maize.owner.agree.q101,baseline_dealers$quality_rating) #p-value < 2.2e-16
# #dealers rate their seed quality better than farmers do
# 
# 
# 
# #OC2. is improved seed worth it?
# mean(baseline_farmers$landproductivity_untrimmed[baseline_farmers$adoption_onfield==1],na.rm = T) #(harvested bags*kgs/bag)/area in acres
# mean(baseline_farmers$landproductivity_untrimmed[baseline_farmers$adoption_onfield==0],na.rm = T)
# 
# 589.4402/423.5282 #farmers who adopted had 39.1738% more yield per acre
# 
# mean(baseline_farmers$landproductivity_inUGX[baseline_farmers$adoption_onfield==1],na.rm = T) #(harvested bags*market value/bag)/area in acres
# mean(baseline_farmers$landproductivity_inUGX[baseline_farmers$adoption_onfield==0],na.rm = T)
# 
# 412705.3-298563.5 #farmers who adopted earned 114 141.8 UGX more per acre
# 
# baseline_farmers$costforseed_new_untrimmed_peracre <- baseline_farmers$costforseed_new_untrimmed/baseline_farmers$Check2.check.maize.q29
# 
# mean(baseline_farmers$costforseed_new_untrimmed_peracre[baseline_farmers$adoption_onfield==1],na.rm = T) #seed in kg*cost/kg
# mean(baseline_farmers$costforseed_new_untrimmed_peracre[baseline_farmers$adoption_onfield==0],na.rm = T)
# 
# 31699.39-1302.27 #farmers who adopted spent 30397.12 UGX more per acre
# 
# 114141.8/30397.12 #farmers who adopted almost quadrupled (3.75502) their investment
# 
# 
# 
# #OC3. Are the seed quality ratings correlated with other measures of seed quailty?
# 
# #attention: only CH treated dealers
# 
# # variables_ratingsD <- cbind(baseline_dealers$seed_quality_general_rating,baseline_dealers$seed_yield_rating
# #                             ,baseline_dealers$seed_drought_rating,baseline_dealers$seed_disease_rating
# #                             ,baseline_dealers$seed_maturing_rating,baseline_dealers$seed_germinate_rating)
# # 
# # index_ratingsD <- icwIndex(xmat=variables_ratingsD)
# # baseline_dealers$index_ratingsD <- index_ratingsD$index
# # 
# # baseline_dealers$index_ratingsD <- rowMeans(baseline_dealers[c("seed_quality_general_rating","seed_yield_rating","seed_drought_rating"
# #                                                                ,"seed_disease_rating","seed_maturing_rating","seed_germinate_rating")],na.rm = T)
# 
# #DEALERS
# # reviews_seed <- read.csv(paste(path,"/baseline/data/agro_input/public/reviews_seed.csv",sep="/"))
# # baseline_dealers <- merge(baseline_dealers,reviews_seed,by.x=c("catchID","shop_ID"),by.y=c("catchID","shop_ID"),all.x=T)
# 
# baseline_dealers$index_ratingsD <- baseline_dealers$score_corrected
# 
# #Shop only sells farm inputs
# cor(baseline_dealers$index_ratingsD,baseline_dealers$maize.owner.agree.q5,use = "pairwise.complete.obs")
# #good: positive correlation of 0.12
# 
# #Index of labor-intensive seed handling and storage practices observed by enumerator
# cor(baseline_dealers$index_ratingsD,baseline_dealers$index_practices_lab_base,use = "pairwise.complete.obs")
# #neutral: positive correlation of 0.01 (too weak)
# 
# #Index of capital-intensive seed handling and storage practices observed by enumerator
# cor(baseline_dealers$index_ratingsD,baseline_dealers$index_practices_cap_base,use = "pairwise.complete.obs")
# #good: positive correlation of 0.13
# 
# #Index of all seed handling and storage practices observed by enumerator
# cor(baseline_dealers$index_ratingsD,baseline_dealers$index_practices_all_base,use = "pairwise.complete.obs")
# #neutral: positive correlation of 0.06
# 
# #Shop received seed related complaint from customer
# cor(baseline_dealers$index_ratingsD,baseline_dealers$maize.owner.agree.q96,use = "pairwise.complete.obs")
# #good: negative correlation of -0.12
# 
# #Moisture in random seed bag in percent
# cor(baseline_dealers$index_ratingsD,baseline_dealers$reading,use = "pairwise.complete.obs")
# #neutral: negative correlation of -0.01
# 
# #Random seed bag shows lot number
# cor(baseline_dealers$index_ratingsD,baseline_dealers$lot,use = "pairwise.complete.obs")
# #neutral: positive correlation of 0.04
# 
# #Random seed bag shows packaging date
# cor(baseline_dealers$index_ratingsD,baseline_dealers$visible_packdate,use = "pairwise.complete.obs")
# #neutral: positive correlation 0.08
# 
# #Days since packaging date/expiry date minus 6 months
# cor(baseline_dealers$index_ratingsD,baseline_dealers$shelflife_Caro,use = "pairwise.complete.obs")
# #bad: positive correlation of 0.01 but should be negative
# 
# #Shop received a warning after inspection
# cor(baseline_dealers$index_ratingsD,baseline_dealers$maize.owner.agree.inspection.q118,use = "pairwise.complete.obs")
# #bad: positive correlation of 0.05 but should be negative
# 
# summary(regression <- lm(baseline_dealers$index_ratingsD~baseline_dealers$maize.owner.agree.q5
#                          +baseline_dealers$index_practices_cap_base+baseline_dealers$maize.owner.agree.q96
#                          +baseline_dealers$lot+baseline_dealers$visible_packdate))
# 
# #FARMERS
# #attention: any seed, not specific seed
# #Index of farmer's ratings of seed used on randomly selected maize field last season
# #Yield in kg/acre (production/area)
# cor(baseline_farmers$index_ratingplot_base,baseline_farmers$landproductivity,use = "pairwise.complete.obs")
# #very good: positive correlation of 0.21
# 
# summary(regression2 <- lm(baseline_farmers$landproductivity~baseline_farmers$index_ratingplot_base))
# 
# 
# 
# #OC4. Do farmers who bought seed rate significantly different than farmers who didn't buy seed?
# 
# #attention: only CH treated farmers
# 
# # variables_ratingsF <- cbind(rating_dyads$seed_quality_general_rating,rating_dyads$seed_yield_rating
# #                                 ,rating_dyads$seed_drought_rating,rating_dyads$seed_disease_rating
# #                                 ,rating_dyads$seed_maturing_rating,rating_dyads$seed_germinate_rating)
# # 
# # index_ratingsF <- icwIndex(xmat=variables_ratingsF)
# # rating_dyads$index_ratingsF <- index_ratingsF$index
# 
# #not straightforward to interpret, so:
# 
# #at rating_dyads level because baseline_farmers$bought_at_dealer and baseline_farmers$bought_last_season are averages
# 
# rating_dyads$index_ratingsF <- rowMeans(rating_dyads[c("seed_quality_general_rating","seed_yield_rating","seed_drought_rating"
#                                                        ,"seed_disease_rating","seed_maturing_rating","seed_germinate_rating")],na.rm = T)
# 
# table(rating_dyads$bought_at_dealer) #3536 No, 807 Yes
# 
# mean(rating_dyads$index_ratingsF[rating_dyads$bought_at_dealer=="Yes"],na.rm = T) #3.390949
# mean(rating_dyads$index_ratingsF[rating_dyads$bought_at_dealer=="No"],na.rm = T) #3.478922
# 
# summary(regression3 <- lm(rating_dyads$index_ratingsF~rating_dyads$bought_at_dealer))
# #farmers who never bought seed at dealer rate significantly better
# 
# rating_dyads$bought_last_season[rating_dyads$bought_at_dealer=="No"] <- 0
# 
# table(rating_dyads$bought_last_season) #3990 No, 353 Yes
# 
# mean(rating_dyads$index_ratingsF[rating_dyads$bought_last_season==1],na.rm = T) #3.354545
# mean(rating_dyads$index_ratingsF[rating_dyads$bought_last_season==0],na.rm = T) #3.438055
# 
# summary(regression4 <- lm(rating_dyads$index_ratingsF~rating_dyads$bought_last_season))
# #farmers who didn't buy seed at dealer last season rate significantly better
# 
# # variables_ratingsF <- cbind(baseline_farmers$seed_quality_general_rating,baseline_farmers$seed_yield_rating
# #                             ,baseline_farmers$seed_drought_rating,baseline_farmers$seed_disease_rating
# #                             ,baseline_farmers$seed_maturing_rating,baseline_farmers$seed_germinate_rating)
# # 
# # index_ratingsF <- icwIndex(xmat=variables_ratingsF)
# # baseline_farmers$index_ratingsF <- index_ratingsF$index
# 
# #not straightforward to interpret, so:
# 
# baseline_farmers$index_ratingsF <- rowMeans(baseline_farmers[c("seed_quality_general_rating","seed_yield_rating","seed_drought_rating"
#                                                                ,"seed_disease_rating","seed_maturing_rating","seed_germinate_rating")],na.rm = T)
# 
# #baseline_farmers$agro==1 if farmer used quality maize seed bought at agro-input shop for any plot last season
# #attention: ANY agro-input shop
# mean(baseline_farmers$index_ratingsF[baseline_farmers$agro==1],na.rm = T) #3.371677
# mean(baseline_farmers$index_ratingsF[baseline_farmers$agro==0],na.rm = T) #3.425568
# 
# summary(regression4 <- lm(baseline_farmers$index_ratingsF~baseline_farmers$agro))
# #farmers who used quality maize seed bought at agro-input shop rate worse than those who didn't (but not significantly)
# 
# mean(baseline_farmers$index_ratingsF[baseline_farmers$Check2.check.maize.q25a==1],na.rm = T) #3.377495
# mean(baseline_farmers$index_ratingsF[baseline_farmers$Check2.check.maize.q25a==0],na.rm = T) #3.435884
# 
# summary(regression4 <- lm(baseline_farmers$index_ratingsF~baseline_farmers$Check2.check.maize.q25a))
# #farmers who adopt rate worse than those who don't (but not significantly)
# 
# 
# 
# #OC5. check yellow questions in "variables farmer" (C:\Users\u0127963\Desktop\PhD\Seed_systems_project_without_Bjorn\report)
# table(baseline_farmers$check.maize.q25f) #clearinghouse control group MIDLINE
# table(baseline_farmers$check.maize.q25fx) #clearinghouse treatment group MIDLINE
# 
# #62 farmers didn't buy seed at agro-input shop because it is too far way or in an inconvenient location
# #1024 farmers didn't buy seed at agro-input shop because it is too expensive
# #82 farmers didn't buy seed at agro-input shop because it isn't of good quality
# #15 farmers didn't buy seed at agro-input shop because it is always out of stock/doesn't sell the seed they are looking for
# #8 farmer didn't buy seed at agro-input shop because it sells only in inconvenient quantities
# #1 farmer didn't buy seed at agro-input shop because of other reasons
# #0 farmers didn't buy seed at agro-input shop because shop's SeedAdvisor rating was too low (only for clearinghouse treatment group)
# #=1192
# 
# table(baseline_farmers$check.maize.q25f_2)
# table(baseline_farmers$check.maize.q25f_2x)
# 
# #315 farmers bought seed at particular agro-input shop because it is close by or in an convenient location
# #70 farmers bought seed at particular agro-input shop because it is not very expensive
# #641 farmers bought seed at particular agro-input shop because seed there is of very good quality
# #87 farmers bought seed at particular agro-input shop because it always has a good stock of seed I am looking for
# #56 farmers bought seed at particular agro-input shop because it sells in convenient quantities
# #14 farmers bought seed at particular agro-input shop because it's SeedAdvisor rating was high
# 
# #Q67a. Do you know **${calc_biz}**'s SeedAdvisor rating?
# table(midline_rating_dyads$knows_dealer) #5081 know dealer, 5782 don't
# table(midline_rating_dyads$knows_SA_rating) #67 know rating, 929 don't
# #the next question you had to answer if you answered yes (if q67a=yes: What is  **${calc_biz}**'s SeedAdvisor rating?) might have influenced this answer
# table(midline_rating_dyads$SA_rating)
# 
# 
# 
# #OC6. heterogeneity analysis: only shops which only sell farm inputs (74.1%)
# #paste this before analysis:
# #baseline_dealers=subset(baseline_dealers,maize.owner.agree.q5=="1")
# #also had to change these 348's to 258's but that won't be necessary at endline:
# #e.g. baseline_dealers$mid_general=sample(na.omit(baseline_dealers$general),258,replace = T)
# 
# #new effects:
# #CH on transformed seed revenue in mln UGX (IHS)
# #training and CH on transformed quantity of Longe 10H sold last season in kg (IHS)
# #training on overall index Longe 10H
# #CH on shelflife (days since packaging date/expiry date minus 6 months)
# 
# 
# 
# #OC7. attrition: how many (un-)specialized dealers?
# sum(baseline_dealers$attrition_ind_D==1&baseline_dealers$maize.owner.agree.q5==1)
# #31 of 258 (12%) specialized shops left the sample
# sum(baseline_dealers$attrition_ind_D==1&baseline_dealers$maize.owner.agree.q5==0)
# #11 of 90 (12%) un-specialized shops left the sample
# 
# summary(regression5 <- lm(baseline_dealers$attrition_ind_D~baseline_dealers$maize.owner.agree.q5))
# #shops which only sell farm inputs are not significantly more/less likely to leave the sample
# 
# 
# 
# #OC8. intended to treat vs. treated
# sum(baseline_dealers$training==1 & baseline_dealers$attrition_ind_D==0)
# #147 dealers who didn't leave sample received training
# sum(baseline_dealers$training==0 & baseline_dealers$attrition_ind_D==0)
# #159 dealers who didn't leave sample didn't receive training
# 
# table(baseline_dealers$owner.agree.q11a)
# sum(baseline_dealers$training==1 & baseline_dealers$owner.agree.q11a=="Yes",na.rm = T)
# #120 of 147 (82%) dealers who were supposed to receive training, say that they were invited to our training
# 
# table(baseline_dealers$owner.agree.q11b)
# sum(baseline_dealers$training==1 & baseline_dealers$owner.agree.q11b=="Yes",na.rm = T)
# #105 of 147 (71%) dealers who were supposed to receive training, say that they attended our training
# 
# mean(as.numeric(as.character(baseline_dealers$owner.agree.q11c)),na.rm = T)
# #our training was rated 4.52/5 on average
# 
# #CH
# sum(baseline_dealers$clearing==1 & baseline_dealers$attrition_ind_D==0)
# #179 dealers who didn't leave sample received CH
# sum(baseline_dealers$clearing==0 & baseline_dealers$attrition_ind_D==0)
# #127 dealers who didn't leave sample didn't receive CH
# 
# table(baseline_dealers$owner.agree.q2a)
# sum(baseline_dealers$clearing==1 & baseline_dealers$owner.agree.q2a=="Yes",na.rm = T)
# #124 of 179 dealers (69%) who were supposed to receive certificate say that they did
# 
# table(baseline_dealers$owner.agree.q2b)
# sum(baseline_dealers$clearing==1 & baseline_dealers$owner.agree.q2b=="Yes",na.rm = T)
# #66 of 179 dealers (37%) who were supposed to receive certificate say that they know their shop's rating
# 
# 
# 
# #OC9. read Different-sized baskets of fruit: https://blogs.worldbank.org/impactevaluations/different-sized-baskets-fruit-how-unequally-sized-clusters-can-lead-your-power
# #"if you have a few clusters that are much larger than the rest, you may want to not include them in the experiment"
# #exclude catchment areas with more than 10 dealers/villages and redo analysis
# table(baseline_dealers$catchID)
# table(baseline_farmers$catchID)
# 
# # #paste this before analysis:
# # baseline_dealers$large_catchID <- ifelse(baseline_dealers$catchID==3|baseline_dealers$catchID==32|baseline_dealers$catchID==59,1,0)
# # #|baseline_dealers$catchID==64
# # baseline_dealers=subset(baseline_dealers,large_catchID=="0")
# 
# # baseline_farmers$large_catchID <- ifelse(baseline_farmers$catchID==3|baseline_farmers$catchID==32|baseline_farmers$catchID==59,1,0)
# # #|baseline_farmers$catchID==64
# # baseline_farmers=subset(baseline_farmers,large_catchID=="0")
# 
# # #then again, change all "sample(na.omit("
# # #nothing interesting happens
# 
# 
# 
# #OC10. different treatment stati
# #1. TRAINING
# #1.a training according to dealers
# baseline_dealers$say_that_attended_training <- NA
# baseline_dealers$say_that_attended_training[baseline_dealers$owner.agree.q11b=="Yes"] <- 1
# baseline_dealers$say_that_attended_training[baseline_dealers$training==0] <- 0
# #unreliable because at least 136 dealers attended the training (I checked their attendance) (here: 105)
# 
# #1.b training according to our attendance list
# #insert above analysis but after sign
# training_attendance <- read.csv(paste(path,"/Study design/treatments/training/training_attendance.csv", sep="/"), sep=";", stringsAsFactors = TRUE)
# training_attendance = subset(training_attendance, select = c("shop_ID","someone_attended"))
# baseline_dealers <- merge(baseline_dealers,training_attendance,by.x="shop_ID",by.y="shop_ID",all.x=TRUE)
# baseline_dealers$someone_attended[baseline_dealers$training==0] <- 0
# #baseline_dealers$training <- baseline_dealers$someone_attended
# 
# #now significant
# #Average sales price of 4 improved maize varieties last season in UGX/ kg
# #Index of all seed handling and storage practices observed by enumerator
# #Transformed amount of Longe 5 bought by shop from provider last season in kg (IHS)
# #Shop's products were confiscated after inspection
# 
# #2. CLEARINGHOUSE
# #general note: too complex because of combination of certificates to dealers, ratings to farmers in person, SMS...
# 
# #DEALERS
# #2.D.a first dissemination to dealers
# #2.D.a.1 first dissemination to dealers according to dealers
# baseline_dealers$say_that_certificate <- NA
# baseline_dealers$say_that_certificate[baseline_dealers$owner.agree.q2a=="Yes"] <- 1
# baseline_dealers$say_that_certificate[baseline_dealers$clearing==0] <- 0
# #unreliable because Richard says that 182 (at least 157) dealers received certificate (here: 124)
# 
# #2.D.a.2 first dissemination to dealers according to Richard
# #need to ask Richard
# 
# #2.D.b second dissemination to dealers
# #2.D.b.1 second dissemination to dealers according to dealers
# #see at endline
# 
# #2.D.a.2 second dissemination to dealers according to us
# #everyone in midline_dealers with clearing==1 received certificate (same visit)
# baseline_dealers$received_2nd_ml_certificate <- baseline_dealers$clearing
# baseline_dealers$received_2nd_ml_certificate[baseline_dealers$attrition_ind_D==1 & baseline_dealers$clearing==1] <- NA
# #doesn't make sense for midline analysis because dealers that left midline sample aren't in analysis anyway
# 
# #FARMERS
# #2.F.a first dissemination to farmers according to us
# farmer_dissemination_final <- read.csv("C:/Users/u0127963/Dropbox/NWO seed system devt Uganda proposal development/Study design/treatments/info_clearing/farmer/data/farmer_dissemination_final.csv", stringsAsFactors = TRUE) #54 missing
# 
# #2.F.b second dissemination to farmers according to us
# #not everyone in midline_farmers with clearing==1 received certificate (not same visit) but good proxy because almost same time (Jan and Feb)
# #midline dissemination dataset would be better here but data is not in dropbox but only on Bjorn's computer
# baseline_farmers$received_2nd_ml_rating <- baseline_farmers$clearing
# baseline_farmers$received_2nd_ml_rating[baseline_farmers$attrition_ind_F==1 & baseline_farmers$clearing==1] <- NA
# #doesn't make sense for midline analysis because farmers that left midline sample aren't in analysis anyway
# 
# #3. VIDEO
# #3.a first video shown
# #video shown during farmer baseline data collection and again during baseline rating dissemination/ service questions --> complete sample
# 
# #3.b second video shown
# #we showed the farmer video during midline rating dissemination/ service questions
# #not everyone in midline_farmers with video==1 saw video (not same visit) but good proxy because almost same time (Jan and Feb)
# #midline dissemination dataset would be better here but data is not in dropbox but only on Bjorn's computer
# baseline_farmers$watched_video <- baseline_farmers$Check2.check.maize.video_shown
# baseline_farmers$watched_video[baseline_farmers$attrition_ind_F==1 & baseline_farmers$Check2.check.maize.video_shown==1] <- NA
# #doesn't make sense for midline analysis because farmers that left midline sample aren't in analysis anyway
# 
# 
# 
# #OC11. CR0 to CR0
# #Average sales price of 4 improved maize varieties last season in UGX/ kg
# #Days since packaging date/expiry date minus 6 months
# #Overall index controlling for baseline (secondary outcome variables regarding seed bag)
# 
# #Farmer planted local land race maize seed on this field

###create CSV
baseline_dealers$midline_specialized.shop <- ifelse(baseline_dealers$owner.agree.q5=="Yes",1,0)
baseline_dealers$midline_practices_lab <- baseline_dealers$index_practices_lab_mid
baseline_dealers$midline_practices_cap <- baseline_dealers$index_practices_cap_mid
baseline_dealers$midline_practices_all <- baseline_dealers$index_practices_all_mid
baseline_dealers$midline_received.complaint <- baseline_dealers$mid_maize.owner.agree.q96
baseline_dealers$midline_received.warning <- baseline_dealers$mid_maize.owner.agree.inspection.q118

  baseline_dealers$mid_date_pack <- baseline_dealers$date_pack_mid #x
  baseline_dealers$mid_visible_packdate <- ifelse(baseline_dealers$mid_date_pack=="n/a",0,1) #x

baseline_dealers$midline_bag.shows.packaging.date <- baseline_dealers$mid_visible_packdate

baseline_dealers$mid_exp <- baseline_dealers$exp_mid #x
baseline_dealers$mid_visible_expdate<-ifelse(!is.na(baseline_dealers$mid_exp),1,0) #x

  baseline_dealers$mid_date <- baseline_dealers$date_mid #x
  baseline_dealers$mid_date[baseline_dealers$mid_date=="n/a"] <- NA #x
  baseline_dealers$mid_date <- as.Date(baseline_dealers$mid_date) #x

  baseline_dealers$mid_exp <- as.Date(baseline_dealers$mid_exp) #x
  baseline_dealers$mid_days_since_exp <- baseline_dealers$mid_date - baseline_dealers$mid_exp #x

  baseline_dealers$mid_date_pack <- as.Date(baseline_dealers$mid_date_pack) #x
  baseline_dealers$mid_shelflife <- baseline_dealers$mid_date - baseline_dealers$mid_date_pack #x

  baseline_dealers$mid_date_pack_incltransformedexp <- baseline_dealers$mid_date_pack #x
  baseline_dealers$mid_transformedexp <- baseline_dealers$mid_exp - 183 #6x366/12 #x
  baseline_dealers$mid_date_pack_incltransformedexp[is.na(baseline_dealers$mid_date_pack)]<-baseline_dealers$mid_transformedexp[is.na(baseline_dealers$mid_date_pack)] #x

  baseline_dealers$mid_shelflife_Caro <- baseline_dealers$mid_date - as.Date(baseline_dealers$mid_date_pack_incltransformedexp) #x
  baseline_dealers$mid_shelflife_Caro[baseline_dealers$mid_shelflife_Caro < 0] <- NA #x
  baseline_dealers$mid_shelflife_Caro <- as.numeric(as.character(baseline_dealers$mid_shelflife_Caro)) #x

  baseline_dealers <- trim("mid_shelflife_Caro",baseline_dealers,trim_perc=.02) #x
  baseline_dealers <- trim("shelflife_Caro",baseline_dealers,trim_perc=.02)

baseline_dealers$midline_shelflife <- baseline_dealers$mid_shelflife_Caro

  baseline_dealers$mid_lot <- baseline_dealers$lot_mid
  baseline_dealers$mid_lot<-ifelse(baseline_dealers$mid_lot=="Yes",1,0)

baseline_dealers$midline_bag.shows.lotnumber <- baseline_dealers$mid_lot

baseline_dealers$midline_moisture <- baseline_dealers$mid_reading

baseline_dealers$midline_original.bag.without.damage <- ifelse(baseline_dealers$origin_mid=="Yes",1,0)

midline_for.cor.ratings.quality = subset(baseline_dealers, select = c("shop_ID",
                                                                      "catchID",
                                                                      "midline_specialized.shop",
                                                                      "midline_practices_lab",
                                                                      "midline_practices_cap",
                                                                      "midline_practices_all",
                                                                      "midline_received.complaint",
                                                                      "midline_received.warning",
                                                                      "midline_bag.shows.packaging.date",
                                                                      "midline_shelflife",
                                                                      "midline_bag.shows.lotnumber",
                                                                      "midline_moisture",
                                                                      "midline_original.bag.without.damage"))

write.csv(midline_for.cor.ratings.quality,paste(path,"/midline/data/agro_input/public/midline_for.cor.ratings.quality.csv",sep="/"), row.names = T)

# df <- data.frame(baseline_dealers$shop_ID,
#                  baseline_dealers$catchID,
#                  baseline_dealers$midline_specialized.shop,
#                  baseline_dealers$midline_practices_lab,
#                  baseline_dealers$midline_practices_cap,
#                  baseline_dealers$midline_practices_all,
#                  baseline_dealers$midline_received.complaint,
#                  baseline_dealers$midline_received.warning,
#                  baseline_dealers$midline_bag.shows.packaging.date,
#                  baseline_dealers$midline_shelflife,
#                  baseline_dealers$midline_bag.shows.lotnumber,
#                  baseline_dealers$midline_moisture,
#                  baseline_dealers$midline_original.bag.without.damage)

baseline_farmers$plotratings_midline <- baseline_farmers$index_ratingplot_mid

midline_for.cor.ratings.quality_farmers = subset(baseline_farmers, select = c("farmer_ID",
                                                                      "plotratings_midline"))

write.csv(midline_for.cor.ratings.quality_farmers,paste(path,"/midline/data/farmer/public/midline_for.cor.ratings.quality_farmers.csv",sep="/"), row.names = T)

### saving results in matrices for papers
df_ols_F_sec_plot_mid <- df_ols_F_sec_plot 
df_means_F_sec_plot_mid <- df_means_F_sec_plot
save(df_ols_F_sec_plot_mid, file=paste(path,"papers/midline_report/output/df_ols_F_sec_plot_mid.Rdata",sep="/"))
save(df_means_F_sec_plot_mid, file=paste(path,"papers/midline_report/output/df_means_F_sec_plot_mid.Rdata",sep="/"))
