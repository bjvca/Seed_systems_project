rm(list = ls())

path <- getwd()
path <- strsplit(path, "/papers")[[1]]

baseline_dealers <- read.csv(paste(path,"/baseline/data/agro_input/public/baseline_dealer.csv", sep = "/"))
baseline_farmers <- read.csv(paste(path,"/baseline/data/farmer/public/baseline_farmers.csv", sep = "/"))

###################################################
#####Descriptive statistics: agro-input dealer#####
###################################################

df_descriptives_dealer <- array(NA,dim=c(168,5))

baseline_dealers$maize.owner.agree.age[baseline_dealers$maize.owner.agree.age==999] <- NA

df_descriptives_dealer[1,1] <- mean(baseline_dealers$maize.owner.agree.age, na.rm=T)
df_descriptives_dealer[1,2] <- min(baseline_dealers$maize.owner.agree.age, na.rm=T)
df_descriptives_dealer[1,3] <- max(baseline_dealers$maize.owner.agree.age, na.rm=T)
df_descriptives_dealer[1,4] <- sd(baseline_dealers$maize.owner.agree.age, na.rm=T)
df_descriptives_dealer[1,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.age))

baseline_dealers$maize.owner.agree.gender<-ifelse(baseline_dealers$maize.owner.agree.gender=="Male",1,0)

df_descriptives_dealer[2,1] <- mean(baseline_dealers$maize.owner.agree.gender)
df_descriptives_dealer[2,2] <- min(baseline_dealers$maize.owner.agree.gender)
df_descriptives_dealer[2,3] <- max(baseline_dealers$maize.owner.agree.gender)
df_descriptives_dealer[2,4] <- sd(baseline_dealers$maize.owner.agree.gender)
df_descriptives_dealer[2,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.gender))

baseline_dealers$finished_primary[baseline_dealers$maize.owner.agree.educ=="a"] <- 0
baseline_dealers$finished_primary[baseline_dealers$maize.owner.agree.educ=="b"] <- 0
baseline_dealers$finished_primary[baseline_dealers$maize.owner.agree.educ=="c"] <- 1
baseline_dealers$finished_primary[baseline_dealers$maize.owner.agree.educ=="d"] <- 1
baseline_dealers$finished_primary[baseline_dealers$maize.owner.agree.educ=="e"] <- 1
baseline_dealers$finished_primary[baseline_dealers$maize.owner.agree.educ=="f"] <- 1
baseline_dealers$finished_primary[baseline_dealers$maize.owner.agree.educ=="g"] <- NA

df_descriptives_dealer[3,1] <- mean(baseline_dealers$finished_primary, na.rm=T)
df_descriptives_dealer[3,2] <- min(baseline_dealers$finished_primary, na.rm=T)
df_descriptives_dealer[3,3] <- max(baseline_dealers$finished_primary, na.rm=T)
df_descriptives_dealer[3,4] <- sd(baseline_dealers$finished_primary, na.rm=T)
df_descriptives_dealer[3,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$finished_primary))

baseline_dealers$finished_secondary[baseline_dealers$maize.owner.agree.educ=="a"] <- 0
baseline_dealers$finished_secondary[baseline_dealers$maize.owner.agree.educ=="b"] <- 0
baseline_dealers$finished_secondary[baseline_dealers$maize.owner.agree.educ=="c"] <- 0
baseline_dealers$finished_secondary[baseline_dealers$maize.owner.agree.educ=="d"] <- 0
baseline_dealers$finished_secondary[baseline_dealers$maize.owner.agree.educ=="e"] <- 1
baseline_dealers$finished_secondary[baseline_dealers$maize.owner.agree.educ=="f"] <- 1
baseline_dealers$finished_secondary[baseline_dealers$maize.owner.agree.educ=="g"] <- NA

df_descriptives_dealer[4,1] <- mean(baseline_dealers$finished_secondary, na.rm=T)
df_descriptives_dealer[4,2] <- min(baseline_dealers$finished_secondary, na.rm=T)
df_descriptives_dealer[4,3] <- max(baseline_dealers$finished_secondary, na.rm=T)
df_descriptives_dealer[4,4] <- sd(baseline_dealers$finished_secondary, na.rm=T)
df_descriptives_dealer[4,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$finished_secondary))

baseline_dealers$maize.owner.agree.ownership<-ifelse(baseline_dealers$maize.owner.agree.ownership=="Yes",1,0)

df_descriptives_dealer[5,1] <- mean(baseline_dealers$maize.owner.agree.ownership)
df_descriptives_dealer[5,2] <- min(baseline_dealers$maize.owner.agree.ownership)
df_descriptives_dealer[5,3] <- max(baseline_dealers$maize.owner.agree.ownership)
df_descriptives_dealer[5,4] <- sd(baseline_dealers$maize.owner.agree.ownership)
df_descriptives_dealer[5,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.ownership))

baseline_dealers$maize.owner.agree.q3[baseline_dealers$maize.owner.agree.q3==999] <- NA

df_descriptives_dealer[6,1] <- mean(baseline_dealers$maize.owner.agree.q3, na.rm=T)
df_descriptives_dealer[6,2] <- min(baseline_dealers$maize.owner.agree.q3, na.rm=T)
df_descriptives_dealer[6,3] <- max(baseline_dealers$maize.owner.agree.q3, na.rm=T)
df_descriptives_dealer[6,4] <- sd(baseline_dealers$maize.owner.agree.q3, na.rm=T)
df_descriptives_dealer[6,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.q3))

df_descriptives_dealer[7,1] <- mean(baseline_dealers$maize.owner.agree.q4)
df_descriptives_dealer[7,2] <- min(baseline_dealers$maize.owner.agree.q4)
df_descriptives_dealer[7,3] <- max(baseline_dealers$maize.owner.agree.q4)
df_descriptives_dealer[7,4] <- sd(baseline_dealers$maize.owner.agree.q4)
df_descriptives_dealer[7,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.q4))

baseline_dealers$maize.owner.agree.q5<-ifelse(baseline_dealers$maize.owner.agree.q5=="Yes",1,0)

df_descriptives_dealer[8,1] <- mean(baseline_dealers$maize.owner.agree.q5)
df_descriptives_dealer[8,2] <- min(baseline_dealers$maize.owner.agree.q5)
df_descriptives_dealer[8,3] <- max(baseline_dealers$maize.owner.agree.q5)
df_descriptives_dealer[8,4] <- sd(baseline_dealers$maize.owner.agree.q5)
df_descriptives_dealer[8,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.q5))

baseline_dealers$maize.owner.agree.q6[baseline_dealers$maize.owner.agree.q6==999] <- NA

df_descriptives_dealer[9,1] <- mean(baseline_dealers$maize.owner.agree.q6, na.rm=T)
df_descriptives_dealer[9,2] <- min(baseline_dealers$maize.owner.agree.q6, na.rm=T)
df_descriptives_dealer[9,3] <- max(baseline_dealers$maize.owner.agree.q6, na.rm=T)
df_descriptives_dealer[9,4] <- sd(baseline_dealers$maize.owner.agree.q6, na.rm=T)
df_descriptives_dealer[9,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.q6))

baseline_dealers$maize.owner.agree.q7[baseline_dealers$maize.owner.agree.q7==999] <- NA

df_descriptives_dealer[10,1] <- mean(baseline_dealers$maize.owner.agree.q7, na.rm=T)
df_descriptives_dealer[10,2] <- min(baseline_dealers$maize.owner.agree.q7, na.rm=T)
df_descriptives_dealer[10,3] <- max(baseline_dealers$maize.owner.agree.q7, na.rm=T)
df_descriptives_dealer[10,4] <- sd(baseline_dealers$maize.owner.agree.q7, na.rm=T)
df_descriptives_dealer[10,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.q7))

baseline_dealers$years_shop <- 2020 - as.numeric(as.character(substr(baseline_dealers$maize.owner.agree.q8, start=1, stop=4)))

df_descriptives_dealer[11,1] <- mean(baseline_dealers$years_shop)
df_descriptives_dealer[11,2] <- min(baseline_dealers$years_shop)
df_descriptives_dealer[11,3] <- max(baseline_dealers$years_shop)
df_descriptives_dealer[11,4] <- sd(baseline_dealers$years_shop)
df_descriptives_dealer[11,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$years_shop))

baseline_dealers$maize.owner.agree.q9.a<-ifelse(baseline_dealers$maize.owner.agree.q9.a=="True",1,0)

df_descriptives_dealer[12,1] <- mean(baseline_dealers$maize.owner.agree.q9.a)
df_descriptives_dealer[12,2] <- min(baseline_dealers$maize.owner.agree.q9.a)
df_descriptives_dealer[12,3] <- max(baseline_dealers$maize.owner.agree.q9.a)
df_descriptives_dealer[12,4] <- sd(baseline_dealers$maize.owner.agree.q9.a)
df_descriptives_dealer[12,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.q9.a))

baseline_dealers$maize.owner.agree.q9.b<-ifelse(baseline_dealers$maize.owner.agree.q9.b=="True",1,0)

df_descriptives_dealer[13,1] <- mean(baseline_dealers$maize.owner.agree.q9.b)
df_descriptives_dealer[13,2] <- min(baseline_dealers$maize.owner.agree.q9.b)
df_descriptives_dealer[13,3] <- max(baseline_dealers$maize.owner.agree.q9.b)
df_descriptives_dealer[13,4] <- sd(baseline_dealers$maize.owner.agree.q9.b)
df_descriptives_dealer[13,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.q9.b))

baseline_dealers$maize.owner.agree.q9.d<-ifelse(baseline_dealers$maize.owner.agree.q9.d=="True",1,0)

df_descriptives_dealer[14,1] <- mean(baseline_dealers$maize.owner.agree.q9.d)
df_descriptives_dealer[14,2] <- min(baseline_dealers$maize.owner.agree.q9.d)
df_descriptives_dealer[14,3] <- max(baseline_dealers$maize.owner.agree.q9.d)
df_descriptives_dealer[14,4] <- sd(baseline_dealers$maize.owner.agree.q9.d)
df_descriptives_dealer[14,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.q9.d))

baseline_dealers$maize.owner.agree.q9.e<-ifelse(baseline_dealers$maize.owner.agree.q9.e=="True",1,0)

df_descriptives_dealer[15,1] <- mean(baseline_dealers$maize.owner.agree.q9.e)
df_descriptives_dealer[15,2] <- min(baseline_dealers$maize.owner.agree.q9.e)
df_descriptives_dealer[15,3] <- max(baseline_dealers$maize.owner.agree.q9.e)
df_descriptives_dealer[15,4] <- sd(baseline_dealers$maize.owner.agree.q9.e)
df_descriptives_dealer[15,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.q9.e))

baseline_dealers$maize.owner.agree.q10<-ifelse(baseline_dealers$maize.owner.agree.q10=="Yes",1,0)

df_descriptives_dealer[16,1] <- mean(baseline_dealers$maize.owner.agree.q10)
df_descriptives_dealer[16,2] <- min(baseline_dealers$maize.owner.agree.q10)
df_descriptives_dealer[16,3] <- max(baseline_dealers$maize.owner.agree.q10)
df_descriptives_dealer[16,4] <- sd(baseline_dealers$maize.owner.agree.q10)
df_descriptives_dealer[16,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.q10))

baseline_dealers$maize.owner.agree.q11<-ifelse(baseline_dealers$maize.owner.agree.q11=="Yes",1,0)

df_descriptives_dealer[17,1] <- mean(baseline_dealers$maize.owner.agree.q11)
df_descriptives_dealer[17,2] <- min(baseline_dealers$maize.owner.agree.q11)
df_descriptives_dealer[17,3] <- max(baseline_dealers$maize.owner.agree.q11)
df_descriptives_dealer[17,4] <- sd(baseline_dealers$maize.owner.agree.q11)
df_descriptives_dealer[17,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.q11))

baseline_dealers$maize.owner.agree.train_ISSD[baseline_dealers$maize.owner.agree.train_ISSD==98] <- NA
baseline_dealers$maize.owner.agree.train_ISSD<-ifelse(baseline_dealers$maize.owner.agree.train_ISSD=="Yes",1,0)

df_descriptives_dealer[18,1] <- mean(baseline_dealers$maize.owner.agree.train_ISSD, na.rm=T)
df_descriptives_dealer[18,2] <- min(baseline_dealers$maize.owner.agree.train_ISSD, na.rm=T)
df_descriptives_dealer[18,3] <- max(baseline_dealers$maize.owner.agree.train_ISSD, na.rm=T)
df_descriptives_dealer[18,4] <- sd(baseline_dealers$maize.owner.agree.train_ISSD, na.rm=T)
df_descriptives_dealer[18,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.train_ISSD))

df_descriptives_dealer[19,1] <- mean(baseline_dealers$maize.owner.agree.nr_var)
df_descriptives_dealer[19,2] <- min(baseline_dealers$maize.owner.agree.nr_var)
df_descriptives_dealer[19,3] <- max(baseline_dealers$maize.owner.agree.nr_var)
df_descriptives_dealer[19,4] <- sd(baseline_dealers$maize.owner.agree.nr_var)
df_descriptives_dealer[19,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.nr_var))

df_descriptives_dealer[20,1] <- mean(baseline_dealers$maize.owner.agree.q19)
df_descriptives_dealer[20,2] <- min(baseline_dealers$maize.owner.agree.q19)
df_descriptives_dealer[20,3] <- max(baseline_dealers$maize.owner.agree.q19)
df_descriptives_dealer[20,4] <- sd(baseline_dealers$maize.owner.agree.q19)
df_descriptives_dealer[20,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.q19))

baseline_dealers$maize.owner.agree.q20<-ifelse(baseline_dealers$maize.owner.agree.q20=="Yes",1,0)

df_descriptives_dealer[21,1] <- mean(baseline_dealers$maize.owner.agree.q20)
df_descriptives_dealer[21,2] <- min(baseline_dealers$maize.owner.agree.q20)
df_descriptives_dealer[21,3] <- max(baseline_dealers$maize.owner.agree.q20)
df_descriptives_dealer[21,4] <- sd(baseline_dealers$maize.owner.agree.q20)
df_descriptives_dealer[21,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.q20))

baseline_dealers$maize.owner.agree.q32<-ifelse(baseline_dealers$maize.owner.agree.q32=="Yes",1,0)

df_descriptives_dealer[22,1] <- mean(baseline_dealers$maize.owner.agree.q32)
df_descriptives_dealer[22,2] <- min(baseline_dealers$maize.owner.agree.q32)
df_descriptives_dealer[22,3] <- max(baseline_dealers$maize.owner.agree.q32)
df_descriptives_dealer[22,4] <- sd(baseline_dealers$maize.owner.agree.q32)
df_descriptives_dealer[22,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.q32))

df_descriptives_dealer[23,1] <- mean(baseline_dealers$maize.owner.agree.q44)
df_descriptives_dealer[23,2] <- min(baseline_dealers$maize.owner.agree.q44)
df_descriptives_dealer[23,3] <- max(baseline_dealers$maize.owner.agree.q44)
df_descriptives_dealer[23,4] <- sd(baseline_dealers$maize.owner.agree.q44)
df_descriptives_dealer[23,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.q44))

baseline_dealers$maize.owner.agree.q45<-ifelse(baseline_dealers$maize.owner.agree.q45=="Yes",1,0)

df_descriptives_dealer[24,1] <- mean(baseline_dealers$maize.owner.agree.q45)
df_descriptives_dealer[24,2] <- min(baseline_dealers$maize.owner.agree.q45)
df_descriptives_dealer[24,3] <- max(baseline_dealers$maize.owner.agree.q45)
df_descriptives_dealer[24,4] <- sd(baseline_dealers$maize.owner.agree.q45)
df_descriptives_dealer[24,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.q45))

baseline_dealers$maize.owner.agree.q57<-ifelse(baseline_dealers$maize.owner.agree.q57=="Yes",1,0)

df_descriptives_dealer[25,1] <- mean(baseline_dealers$maize.owner.agree.q57)
df_descriptives_dealer[25,2] <- min(baseline_dealers$maize.owner.agree.q57)
df_descriptives_dealer[25,3] <- max(baseline_dealers$maize.owner.agree.q57)
df_descriptives_dealer[25,4] <- sd(baseline_dealers$maize.owner.agree.q57)
df_descriptives_dealer[25,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.q57))

baseline_dealers$maize.owner.agree.temp.q69<-ifelse(baseline_dealers$maize.owner.agree.temp.q69=="Yes",1,0)

df_descriptives_dealer[26,1] <- mean(baseline_dealers$maize.owner.agree.temp.q69)
df_descriptives_dealer[26,2] <- min(baseline_dealers$maize.owner.agree.temp.q69)
df_descriptives_dealer[26,3] <- max(baseline_dealers$maize.owner.agree.temp.q69)
df_descriptives_dealer[26,4] <- sd(baseline_dealers$maize.owner.agree.temp.q69)
df_descriptives_dealer[26,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.temp.q69))

baseline_dealers$maize.owner.agree.temp.q71<-ifelse(baseline_dealers$maize.owner.agree.temp.q71=="Yes",1,0)

df_descriptives_dealer[27,1] <- mean(baseline_dealers$maize.owner.agree.temp.q71)
df_descriptives_dealer[27,2] <- min(baseline_dealers$maize.owner.agree.temp.q71)
df_descriptives_dealer[27,3] <- max(baseline_dealers$maize.owner.agree.temp.q71)
df_descriptives_dealer[27,4] <- sd(baseline_dealers$maize.owner.agree.temp.q71)
df_descriptives_dealer[27,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.temp.q71))

baseline_dealers$maize.owner.agree.temp.q72<-ifelse(baseline_dealers$maize.owner.agree.temp.q72=="Yes",1,0)

df_descriptives_dealer[28,1] <- mean(baseline_dealers$maize.owner.agree.temp.q72)
df_descriptives_dealer[28,2] <- min(baseline_dealers$maize.owner.agree.temp.q72)
df_descriptives_dealer[28,3] <- max(baseline_dealers$maize.owner.agree.temp.q72)
df_descriptives_dealer[28,4] <- sd(baseline_dealers$maize.owner.agree.temp.q72)
df_descriptives_dealer[28,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.temp.q72))

baseline_dealers$maize.owner.agree.temp.q73<-ifelse(baseline_dealers$maize.owner.agree.temp.q73=="Yes",1,0)

df_descriptives_dealer[29,1] <- mean(baseline_dealers$maize.owner.agree.temp.q73)
df_descriptives_dealer[29,2] <- min(baseline_dealers$maize.owner.agree.temp.q73)
df_descriptives_dealer[29,3] <- max(baseline_dealers$maize.owner.agree.temp.q73)
df_descriptives_dealer[29,4] <- sd(baseline_dealers$maize.owner.agree.temp.q73)
df_descriptives_dealer[29,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.temp.q73))

baseline_dealers$maize.owner.agree.temp.q74<-ifelse(baseline_dealers$maize.owner.agree.temp.q74=="Yes",1,0)

df_descriptives_dealer[30,1] <- mean(baseline_dealers$maize.owner.agree.temp.q74)
df_descriptives_dealer[30,2] <- min(baseline_dealers$maize.owner.agree.temp.q74)
df_descriptives_dealer[30,3] <- max(baseline_dealers$maize.owner.agree.temp.q74)
df_descriptives_dealer[30,4] <- sd(baseline_dealers$maize.owner.agree.temp.q74)
df_descriptives_dealer[30,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.temp.q74))

baseline_dealers$maize.owner.agree.temp.q75<-ifelse(baseline_dealers$maize.owner.agree.temp.q75=="Yes",1,0)

df_descriptives_dealer[31,1] <- mean(baseline_dealers$maize.owner.agree.temp.q75)
df_descriptives_dealer[31,2] <- min(baseline_dealers$maize.owner.agree.temp.q75)
df_descriptives_dealer[31,3] <- max(baseline_dealers$maize.owner.agree.temp.q75)
df_descriptives_dealer[31,4] <- sd(baseline_dealers$maize.owner.agree.temp.q75)
df_descriptives_dealer[31,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.temp.q75))

baseline_dealers$maize.owner.agree.temp.q76<-ifelse(baseline_dealers$maize.owner.agree.temp.q76=="Yes",1,0)

df_descriptives_dealer[32,1] <- mean(baseline_dealers$maize.owner.agree.temp.q76)
df_descriptives_dealer[32,2] <- min(baseline_dealers$maize.owner.agree.temp.q76)
df_descriptives_dealer[32,3] <- max(baseline_dealers$maize.owner.agree.temp.q76)
df_descriptives_dealer[32,4] <- sd(baseline_dealers$maize.owner.agree.temp.q76)
df_descriptives_dealer[32,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.temp.q76))

baseline_dealers$floor[baseline_dealers$maize.owner.agree.temp.q77==96] <- NA
baseline_dealers$floor[baseline_dealers$maize.owner.agree.temp.q77=="Cement"] <- 1
baseline_dealers$floor[baseline_dealers$maize.owner.agree.temp.q77=="Tiles"] <- 1
baseline_dealers$floor[baseline_dealers$maize.owner.agree.temp.q77=="Mud"] <- 0

df_descriptives_dealer[33,1] <- mean(baseline_dealers$floor, na.rm=T)
df_descriptives_dealer[33,2] <- min(baseline_dealers$floor, na.rm=T)
df_descriptives_dealer[33,3] <- max(baseline_dealers$floor, na.rm=T)
df_descriptives_dealer[33,4] <- sd(baseline_dealers$floor, na.rm=T)
df_descriptives_dealer[33,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$floor))

baseline_dealers$lighting[baseline_dealers$maize.owner.agree.temp.q78==1] <- 0
baseline_dealers$lighting[baseline_dealers$maize.owner.agree.temp.q78==2] <- 1
baseline_dealers$lighting[baseline_dealers$maize.owner.agree.temp.q78==3] <- 1

df_descriptives_dealer[34,1] <- mean(baseline_dealers$lighting)
df_descriptives_dealer[34,2] <- min(baseline_dealers$lighting)
df_descriptives_dealer[34,3] <- max(baseline_dealers$lighting)
df_descriptives_dealer[34,4] <- sd(baseline_dealers$lighting)
df_descriptives_dealer[34,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$lighting))

baseline_dealers$surface[baseline_dealers$maize.owner.agree.temp.q79==1] <- 0
baseline_dealers$surface[baseline_dealers$maize.owner.agree.temp.q79==2] <- 0
baseline_dealers$surface[baseline_dealers$maize.owner.agree.temp.q79==3] <- 1
baseline_dealers$surface[baseline_dealers$maize.owner.agree.temp.q79==4] <- 1
baseline_dealers$surface[baseline_dealers$maize.owner.agree.temp.q79==5] <- 1
baseline_dealers$surface[baseline_dealers$maize.owner.agree.temp.q79==96] <- NA

df_descriptives_dealer[35,1] <- mean(baseline_dealers$surface, na.rm=T)
df_descriptives_dealer[35,2] <- min(baseline_dealers$surface, na.rm=T)
df_descriptives_dealer[35,3] <- max(baseline_dealers$surface, na.rm=T)
df_descriptives_dealer[35,4] <- sd(baseline_dealers$surface, na.rm=T)
df_descriptives_dealer[35,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$surface))

baseline_dealers$maize.owner.agree.temp.q80<-ifelse(baseline_dealers$maize.owner.agree.temp.q80=="Yes",1,0)

df_descriptives_dealer[36,1] <- mean(baseline_dealers$maize.owner.agree.temp.q80)
df_descriptives_dealer[36,2] <- min(baseline_dealers$maize.owner.agree.temp.q80)
df_descriptives_dealer[36,3] <- max(baseline_dealers$maize.owner.agree.temp.q80)
df_descriptives_dealer[36,4] <- sd(baseline_dealers$maize.owner.agree.temp.q80)
df_descriptives_dealer[36,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.temp.q80))

baseline_dealers$maize.owner.agree.temp.q81<-ifelse(baseline_dealers$maize.owner.agree.temp.q81=="Yes",1,0)

df_descriptives_dealer[37,1] <- mean(baseline_dealers$maize.owner.agree.temp.q81)
df_descriptives_dealer[37,2] <- min(baseline_dealers$maize.owner.agree.temp.q81)
df_descriptives_dealer[37,3] <- max(baseline_dealers$maize.owner.agree.temp.q81)
df_descriptives_dealer[37,4] <- sd(baseline_dealers$maize.owner.agree.temp.q81)
df_descriptives_dealer[37,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.temp.q81))

df_descriptives_dealer[38,1] <- mean(baseline_dealers$maize.owner.agree.temp.q82)
df_descriptives_dealer[38,2] <- min(baseline_dealers$maize.owner.agree.temp.q82)
df_descriptives_dealer[38,3] <- max(baseline_dealers$maize.owner.agree.temp.q82)
df_descriptives_dealer[38,4] <- sd(baseline_dealers$maize.owner.agree.temp.q82)
df_descriptives_dealer[38,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.temp.q82))

baseline_dealers$maize.owner.agree.q83.a<-ifelse(baseline_dealers$maize.owner.agree.q83.a=="True",1,0)
baseline_dealers$maize.owner.agree.q83.b<-ifelse(baseline_dealers$maize.owner.agree.q83.b=="True",1,0)
baseline_dealers$maize.owner.agree.q83.c<-ifelse(baseline_dealers$maize.owner.agree.q83.c=="True",1,0)
baseline_dealers$maize.owner.agree.q83.d<-ifelse(baseline_dealers$maize.owner.agree.q83.d=="True",1,0)
baseline_dealers$maize.owner.agree.q83.e<-ifelse(baseline_dealers$maize.owner.agree.q83.e=="True",1,0)
baseline_dealers$maize.owner.agree.q83.f<-ifelse(baseline_dealers$maize.owner.agree.q83.f=="True",1,0)
baseline_dealers$maize.owner.agree.q83.g<-ifelse(baseline_dealers$maize.owner.agree.q83.g=="True",1,0)
baseline_dealers$maize.owner.agree.q83.96<-ifelse(baseline_dealers$maize.owner.agree.q83.96=="True",1,0)

df_descriptives_dealer[39,1] <- mean(baseline_dealers$maize.owner.agree.q83.a)
df_descriptives_dealer[39,2] <- min(baseline_dealers$maize.owner.agree.q83.a)
df_descriptives_dealer[39,3] <- max(baseline_dealers$maize.owner.agree.q83.a)
df_descriptives_dealer[39,4] <- sd(baseline_dealers$maize.owner.agree.q83.a)
df_descriptives_dealer[39,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.q83.a))

baseline_dealers$badpractice_expired <- 0
baseline_dealers$badpractice_expired[baseline_dealers$maize.owner.agree.q83.a==1] <- NA
baseline_dealers$badpractice_expired[baseline_dealers$maize.owner.agree.q83.96==1] <- 1
baseline_dealers$badpractice_expired[baseline_dealers$maize.owner.agree.q83.c==1] <- 1
baseline_dealers$badpractice_expired[baseline_dealers$maize.owner.agree.q83.d==1] <- 1
baseline_dealers$badpractice_expired[baseline_dealers$maize.owner.agree.q83.f==1] <- 1
baseline_dealers$badpractice_expired[baseline_dealers$maize.owner.agree.q83.g==1] <- 1

#there are 2 dealers who answered "This has never happened" and "Other"

baseline_dealers$goodpractice_expired[baseline_dealers$badpractice_expired==0] <- 1
baseline_dealers$goodpractice_expired[baseline_dealers$badpractice_expired==1] <- 0

df_descriptives_dealer[40,1] <- mean(baseline_dealers$goodpractice_expired, na.rm=T)
df_descriptives_dealer[40,2] <- min(baseline_dealers$goodpractice_expired, na.rm=T)
df_descriptives_dealer[40,3] <- max(baseline_dealers$goodpractice_expired, na.rm=T)
df_descriptives_dealer[40,4] <- sd(baseline_dealers$goodpractice_expired, na.rm=T)
df_descriptives_dealer[40,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$goodpractice_expired))

baseline_dealers$alwaysexplains[baseline_dealers$maize.owner.agree.q85=="a"] <- 0
baseline_dealers$alwaysexplains[baseline_dealers$maize.owner.agree.q85=="b"] <- 0
baseline_dealers$alwaysexplains[baseline_dealers$maize.owner.agree.q85=="c"] <- 1

df_descriptives_dealer[41,1] <- mean(baseline_dealers$alwaysexplains)
df_descriptives_dealer[41,2] <- min(baseline_dealers$alwaysexplains)
df_descriptives_dealer[41,3] <- max(baseline_dealers$alwaysexplains)
df_descriptives_dealer[41,4] <- sd(baseline_dealers$alwaysexplains)
df_descriptives_dealer[41,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$alwaysexplains))

baseline_dealers$alwaysrecom[baseline_dealers$maize.owner.agree.q86=="a"] <- 0
baseline_dealers$alwaysrecom[baseline_dealers$maize.owner.agree.q86=="b"] <- 0
baseline_dealers$alwaysrecom[baseline_dealers$maize.owner.agree.q86=="c"] <- 1

df_descriptives_dealer[42,1] <- mean(baseline_dealers$alwaysrecom)
df_descriptives_dealer[42,2] <- min(baseline_dealers$alwaysrecom)
df_descriptives_dealer[42,3] <- max(baseline_dealers$alwaysrecom)
df_descriptives_dealer[42,4] <- sd(baseline_dealers$alwaysrecom)
df_descriptives_dealer[42,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$alwaysrecom))

baseline_dealers$extension[baseline_dealers$maize.owner.agree.q87=="1"] <- 0
baseline_dealers$extension[baseline_dealers$maize.owner.agree.q87=="2"] <- 0
baseline_dealers$extension[baseline_dealers$maize.owner.agree.q87=="3"] <- 1

df_descriptives_dealer[43,1] <- mean(baseline_dealers$extension)
df_descriptives_dealer[43,2] <- min(baseline_dealers$extension)
df_descriptives_dealer[43,3] <- max(baseline_dealers$extension)
df_descriptives_dealer[43,4] <- sd(baseline_dealers$extension)
df_descriptives_dealer[43,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$extension))

baseline_dealers$maize.owner.agree.q88<-ifelse(baseline_dealers$maize.owner.agree.q88=="Yes",1,0)

df_descriptives_dealer[44,1] <- mean(baseline_dealers$maize.owner.agree.q88)
df_descriptives_dealer[44,2] <- min(baseline_dealers$maize.owner.agree.q88)
df_descriptives_dealer[44,3] <- max(baseline_dealers$maize.owner.agree.q88)
df_descriptives_dealer[44,4] <- sd(baseline_dealers$maize.owner.agree.q88)
df_descriptives_dealer[44,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.q88))

baseline_dealers$q89_bin[baseline_dealers$maize.owner.agree.q89=="1"] <- 1
baseline_dealers$q89_bin[baseline_dealers$maize.owner.agree.q89=="2"] <- 0
baseline_dealers$q89_bin[baseline_dealers$maize.owner.agree.q89=="3"] <- 0
baseline_dealers$q89_bin[baseline_dealers$maize.owner.agree.q89=="4"] <- 0
baseline_dealers$q89_bin[baseline_dealers$maize.owner.agree.q89=="other"] <- NA

df_descriptives_dealer[45,1] <- mean(baseline_dealers$q89_bin, na.rm=T)
df_descriptives_dealer[45,2] <- min(baseline_dealers$q89_bin, na.rm=T)
df_descriptives_dealer[45,3] <- max(baseline_dealers$q89_bin, na.rm=T)
df_descriptives_dealer[45,4] <- sd(baseline_dealers$q89_bin, na.rm=T)
df_descriptives_dealer[45,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$q89_bin))

baseline_dealers$q90_bin[baseline_dealers$maize.owner.agree.q90=="a"] <- 1
baseline_dealers$q90_bin[baseline_dealers$maize.owner.agree.q90=="b"] <- 1
baseline_dealers$q90_bin[baseline_dealers$maize.owner.agree.q90=="c"] <- 0

df_descriptives_dealer[46,1] <- mean(baseline_dealers$q90_bin)
df_descriptives_dealer[46,2] <- min(baseline_dealers$q90_bin)
df_descriptives_dealer[46,3] <- max(baseline_dealers$q90_bin)
df_descriptives_dealer[46,4] <- sd(baseline_dealers$q90_bin)
df_descriptives_dealer[46,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$q90_bin))

baseline_dealers$q91_bin[baseline_dealers$maize.owner.agree.q91=="n/a"] <- NA
baseline_dealers$q91_bin[baseline_dealers$maize.owner.agree.q91=="No"] <- 0
baseline_dealers$q91_bin[baseline_dealers$maize.owner.agree.q91=="Yes"] <- 1

df_descriptives_dealer[47,1] <- mean(baseline_dealers$q91_bin, na.rm=T)
df_descriptives_dealer[47,2] <- min(baseline_dealers$q91_bin, na.rm=T)
df_descriptives_dealer[47,3] <- max(baseline_dealers$q91_bin, na.rm=T)
df_descriptives_dealer[47,4] <- sd(baseline_dealers$q91_bin, na.rm=T)
df_descriptives_dealer[47,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$q91_bin))

baseline_dealers$q92_bin[baseline_dealers$maize.owner.agree.q92=="n/a"] <- NA
baseline_dealers$q92_bin[baseline_dealers$maize.owner.agree.q92=="No"] <- 0
baseline_dealers$q92_bin[baseline_dealers$maize.owner.agree.q92=="Yes"] <- 1

df_descriptives_dealer[48,1] <- mean(baseline_dealers$q92_bin, na.rm=T)
df_descriptives_dealer[48,2] <- min(baseline_dealers$q92_bin, na.rm=T)
df_descriptives_dealer[48,3] <- max(baseline_dealers$q92_bin, na.rm=T)
df_descriptives_dealer[48,4] <- sd(baseline_dealers$q92_bin, na.rm=T)
df_descriptives_dealer[48,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$q92_bin))

baseline_dealers$q93_bin[baseline_dealers$maize.owner.agree.q93=="1"] <- 0
baseline_dealers$q93_bin[baseline_dealers$maize.owner.agree.q93=="2"] <- 1
baseline_dealers$q93_bin[baseline_dealers$maize.owner.agree.q93=="3"] <- 1

df_descriptives_dealer[49,1] <- mean(baseline_dealers$q93_bin)
df_descriptives_dealer[49,2] <- min(baseline_dealers$q93_bin)
df_descriptives_dealer[49,3] <- max(baseline_dealers$q93_bin)
df_descriptives_dealer[49,4] <- sd(baseline_dealers$q93_bin)
df_descriptives_dealer[49,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$q93_bin))

baseline_dealers$maize.owner.agree.q94[baseline_dealers$maize.owner.agree.q94==999] <- NA
baseline_dealers$maize.owner.agree.q94[baseline_dealers$maize.owner.agree.q94=="n/a"] <- NA
baseline_dealers$maize.owner.agree.q94[baseline_dealers$maize.owner.agree.q94==0] <- NA

df_descriptives_dealer[50,1] <- mean(as.numeric(as.character(baseline_dealers$maize.owner.agree.q94)), na.rm=T)
df_descriptives_dealer[50,2] <- min(as.numeric(as.character(baseline_dealers$maize.owner.agree.q94)), na.rm=T)
df_descriptives_dealer[50,3] <- max(as.numeric(as.character(baseline_dealers$maize.owner.agree.q94)), na.rm=T)
df_descriptives_dealer[50,4] <- sd(as.numeric(as.character(baseline_dealers$maize.owner.agree.q94)), na.rm=T)
df_descriptives_dealer[50,5] <- nrow(baseline_dealers) - sum(is.na(as.numeric(as.character(baseline_dealers$maize.owner.agree.q94))))

baseline_dealers$maize.owner.agree.q95[baseline_dealers$maize.owner.agree.q95==999] <- NA
baseline_dealers$maize.owner.agree.q95[baseline_dealers$maize.owner.agree.q95=="n/a"] <- NA

df_descriptives_dealer[51,1] <- mean(as.numeric(as.character(baseline_dealers$maize.owner.agree.q95)), na.rm=T)
df_descriptives_dealer[51,2] <- min(as.numeric(as.character(baseline_dealers$maize.owner.agree.q95)), na.rm=T)
df_descriptives_dealer[51,3] <- max(as.numeric(as.character(baseline_dealers$maize.owner.agree.q95)), na.rm=T)
df_descriptives_dealer[51,4] <- sd(as.numeric(as.character(baseline_dealers$maize.owner.agree.q95)), na.rm=T)
df_descriptives_dealer[51,5] <- nrow(baseline_dealers) - sum(is.na(as.numeric(as.character(baseline_dealers$maize.owner.agree.q95))))

baseline_dealers$maize.owner.agree.q96<-ifelse(baseline_dealers$maize.owner.agree.q96=="Yes",1,0)

df_descriptives_dealer[52,1] <- mean(baseline_dealers$maize.owner.agree.q96)
df_descriptives_dealer[52,2] <- min(baseline_dealers$maize.owner.agree.q96)
df_descriptives_dealer[52,3] <- max(baseline_dealers$maize.owner.agree.q96)
df_descriptives_dealer[52,4] <- sd(baseline_dealers$maize.owner.agree.q96)
df_descriptives_dealer[52,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.q96))

baseline_dealers$maize.owner.agree.q97.b<-ifelse(baseline_dealers$maize.owner.agree.q97.b=="True",1,0)

df_descriptives_dealer[53,1] <- mean(baseline_dealers$maize.owner.agree.q97.b)
df_descriptives_dealer[53,2] <- min(baseline_dealers$maize.owner.agree.q97.b)
df_descriptives_dealer[53,3] <- max(baseline_dealers$maize.owner.agree.q97.b)
df_descriptives_dealer[53,4] <- sd(baseline_dealers$maize.owner.agree.q97.b)
df_descriptives_dealer[53,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.q97.b))

baseline_dealers$q98_binary[baseline_dealers$maize.owner.agree.q98=="a"] <- 0
baseline_dealers$q98_binary[baseline_dealers$maize.owner.agree.q98=="b"] <- 1
baseline_dealers$q98_binary[baseline_dealers$maize.owner.agree.q98=="c"] <- 1

df_descriptives_dealer[54,1] <- mean(baseline_dealers$q98_binary)
df_descriptives_dealer[54,2] <- min(baseline_dealers$q98_binary)
df_descriptives_dealer[54,3] <- max(baseline_dealers$q98_binary)
df_descriptives_dealer[54,4] <- sd(baseline_dealers$q98_binary)
df_descriptives_dealer[54,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$q98_binary))

df_descriptives_dealer[55,1] <- mean(baseline_dealers$maize.owner.agree.q99)
df_descriptives_dealer[55,2] <- min(baseline_dealers$maize.owner.agree.q99)
df_descriptives_dealer[55,3] <- max(baseline_dealers$maize.owner.agree.q99)
df_descriptives_dealer[55,4] <- sd(baseline_dealers$maize.owner.agree.q99)
df_descriptives_dealer[55,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.q99))

df_descriptives_dealer[56,1] <- mean(baseline_dealers$maize.owner.agree.q100)
df_descriptives_dealer[56,2] <- min(baseline_dealers$maize.owner.agree.q100)
df_descriptives_dealer[56,3] <- max(baseline_dealers$maize.owner.agree.q100)
df_descriptives_dealer[56,4] <- sd(baseline_dealers$maize.owner.agree.q100)
df_descriptives_dealer[56,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.q100))

df_descriptives_dealer[57,1] <- mean(baseline_dealers$maize.owner.agree.q101)
df_descriptives_dealer[57,2] <- min(baseline_dealers$maize.owner.agree.q101)
df_descriptives_dealer[57,3] <- max(baseline_dealers$maize.owner.agree.q101)
df_descriptives_dealer[57,4] <- sd(baseline_dealers$maize.owner.agree.q101)
df_descriptives_dealer[57,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.q101))

df_descriptives_dealer[58,1] <- mean(baseline_dealers$maize.owner.agree.q102)
df_descriptives_dealer[58,2] <- min(baseline_dealers$maize.owner.agree.q102)
df_descriptives_dealer[58,3] <- max(baseline_dealers$maize.owner.agree.q102)
df_descriptives_dealer[58,4] <- sd(baseline_dealers$maize.owner.agree.q102)
df_descriptives_dealer[58,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.q102))

df_descriptives_dealer[59,1] <- mean(baseline_dealers$maize.owner.agree.q103)
df_descriptives_dealer[59,2] <- min(baseline_dealers$maize.owner.agree.q103)
df_descriptives_dealer[59,3] <- max(baseline_dealers$maize.owner.agree.q103)
df_descriptives_dealer[59,4] <- sd(baseline_dealers$maize.owner.agree.q103)
df_descriptives_dealer[59,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.q103))

baseline_dealers$maize.owner.agree.inspection.q114[baseline_dealers$maize.owner.agree.inspection.q114==98] <- NA
baseline_dealers$maize.owner.agree.inspection.q114 <- as.character(baseline_dealers$maize.owner.agree.inspection.q114)
baseline_dealers$maize.owner.agree.inspection.q114<-ifelse(baseline_dealers$maize.owner.agree.inspection.q114=="Yes",1,0)

df_descriptives_dealer[60,1] <- mean(baseline_dealers$maize.owner.agree.inspection.q114, na.rm=T)
df_descriptives_dealer[60,2] <- min(baseline_dealers$maize.owner.agree.inspection.q114, na.rm=T)
df_descriptives_dealer[60,3] <- max(baseline_dealers$maize.owner.agree.inspection.q114, na.rm=T)
df_descriptives_dealer[60,4] <- sd(baseline_dealers$maize.owner.agree.inspection.q114, na.rm=T)
df_descriptives_dealer[60,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.inspection.q114))

baseline_dealers$maize.owner.agree.inspection.q115[baseline_dealers$maize.owner.agree.inspection.q115==98] <- NA
baseline_dealers$maize.owner.agree.inspection.q115 <- as.character(baseline_dealers$maize.owner.agree.inspection.q115)
baseline_dealers$maize.owner.agree.inspection.q115<-ifelse(baseline_dealers$maize.owner.agree.inspection.q115=="Yes",1,0)

df_descriptives_dealer[61,1] <- mean(baseline_dealers$maize.owner.agree.inspection.q115, na.rm=T)
df_descriptives_dealer[61,2] <- min(baseline_dealers$maize.owner.agree.inspection.q115, na.rm=T)
df_descriptives_dealer[61,3] <- max(baseline_dealers$maize.owner.agree.inspection.q115, na.rm=T)
df_descriptives_dealer[61,4] <- sd(baseline_dealers$maize.owner.agree.inspection.q115, na.rm=T)
df_descriptives_dealer[61,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.inspection.q115))

baseline_dealers$maize.owner.agree.inspection.q116[baseline_dealers$maize.owner.agree.inspection.q116==98] <- NA
baseline_dealers$maize.owner.agree.inspection.q116 <- as.character(baseline_dealers$maize.owner.agree.inspection.q116)
baseline_dealers$maize.owner.agree.inspection.q116<-ifelse(baseline_dealers$maize.owner.agree.inspection.q116=="Yes",1,0)

df_descriptives_dealer[62,1] <- mean(baseline_dealers$maize.owner.agree.inspection.q116, na.rm=T)
df_descriptives_dealer[62,2] <- min(baseline_dealers$maize.owner.agree.inspection.q116, na.rm=T)
df_descriptives_dealer[62,3] <- max(baseline_dealers$maize.owner.agree.inspection.q116, na.rm=T)
df_descriptives_dealer[62,4] <- sd(baseline_dealers$maize.owner.agree.inspection.q116, na.rm=T)
df_descriptives_dealer[62,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.inspection.q116))

baseline_dealers$maize.owner.agree.inspection.q117[baseline_dealers$maize.owner.agree.inspection.q117==999] <- NA

df_descriptives_dealer[63,1] <- mean(baseline_dealers$maize.owner.agree.inspection.q117, na.rm=T)
df_descriptives_dealer[63,2] <- min(baseline_dealers$maize.owner.agree.inspection.q117, na.rm=T)
df_descriptives_dealer[63,3] <- max(baseline_dealers$maize.owner.agree.inspection.q117, na.rm=T)
df_descriptives_dealer[63,4] <- sd(baseline_dealers$maize.owner.agree.inspection.q117, na.rm=T)
df_descriptives_dealer[63,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.inspection.q117))

baseline_dealers$maize.owner.agree.inspection.q118[baseline_dealers$maize.owner.agree.inspection.q118==98] <- NA
baseline_dealers$maize.owner.agree.inspection.q118 <- as.character(baseline_dealers$maize.owner.agree.inspection.q118)
baseline_dealers$maize.owner.agree.inspection.q118<-ifelse(baseline_dealers$maize.owner.agree.inspection.q118=="Yes",1,0)

df_descriptives_dealer[64,1] <- mean(baseline_dealers$maize.owner.agree.inspection.q118, na.rm=T)
df_descriptives_dealer[64,2] <- min(baseline_dealers$maize.owner.agree.inspection.q118, na.rm=T)
df_descriptives_dealer[64,3] <- max(baseline_dealers$maize.owner.agree.inspection.q118, na.rm=T)
df_descriptives_dealer[64,4] <- sd(baseline_dealers$maize.owner.agree.inspection.q118, na.rm=T)
df_descriptives_dealer[64,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.inspection.q118))

baseline_dealers$maize.owner.agree.inspection.q119[baseline_dealers$maize.owner.agree.inspection.q119==98] <- NA
baseline_dealers$maize.owner.agree.inspection.q119 <- as.character(baseline_dealers$maize.owner.agree.inspection.q119)
baseline_dealers$maize.owner.agree.inspection.q119<-ifelse(baseline_dealers$maize.owner.agree.inspection.q119=="Yes",1,0)

df_descriptives_dealer[65,1] <- mean(baseline_dealers$maize.owner.agree.inspection.q119, na.rm=T)
df_descriptives_dealer[65,2] <- min(baseline_dealers$maize.owner.agree.inspection.q119, na.rm=T)
df_descriptives_dealer[65,3] <- max(baseline_dealers$maize.owner.agree.inspection.q119, na.rm=T)
df_descriptives_dealer[65,4] <- sd(baseline_dealers$maize.owner.agree.inspection.q119, na.rm=T)
df_descriptives_dealer[65,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.inspection.q119))

baseline_dealers$maize.owner.agree.inspection.q120[baseline_dealers$maize.owner.agree.inspection.q120==98] <- NA
baseline_dealers$maize.owner.agree.inspection.q120 <- as.character(baseline_dealers$maize.owner.agree.inspection.q120)
baseline_dealers$maize.owner.agree.inspection.q120<-ifelse(baseline_dealers$maize.owner.agree.inspection.q120=="Yes",1,0)

df_descriptives_dealer[66,1] <- mean(baseline_dealers$maize.owner.agree.inspection.q120, na.rm=T)
df_descriptives_dealer[66,2] <- min(baseline_dealers$maize.owner.agree.inspection.q120, na.rm=T)
df_descriptives_dealer[66,3] <- max(baseline_dealers$maize.owner.agree.inspection.q120, na.rm=T)
df_descriptives_dealer[66,4] <- sd(baseline_dealers$maize.owner.agree.inspection.q120, na.rm=T)
df_descriptives_dealer[66,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.inspection.q120))

baseline_dealers$maize.owner.agree.inspection.q121<-ifelse(baseline_dealers$maize.owner.agree.inspection.q121=="Yes",1,0)

df_descriptives_dealer[67,1] <- mean(baseline_dealers$maize.owner.agree.inspection.q121)
df_descriptives_dealer[67,2] <- min(baseline_dealers$maize.owner.agree.inspection.q121)
df_descriptives_dealer[67,3] <- max(baseline_dealers$maize.owner.agree.inspection.q121)
df_descriptives_dealer[67,4] <- sd(baseline_dealers$maize.owner.agree.inspection.q121)
df_descriptives_dealer[67,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.inspection.q121))

baseline_dealers$maize.owner.agree.inspection.q122<-ifelse(baseline_dealers$maize.owner.agree.inspection.q122=="Yes",1,0)

df_descriptives_dealer[68,1] <- mean(baseline_dealers$maize.owner.agree.inspection.q122)
df_descriptives_dealer[68,2] <- min(baseline_dealers$maize.owner.agree.inspection.q122)
df_descriptives_dealer[68,3] <- max(baseline_dealers$maize.owner.agree.inspection.q122)
df_descriptives_dealer[68,4] <- sd(baseline_dealers$maize.owner.agree.inspection.q122)
df_descriptives_dealer[68,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.inspection.q122))

baseline_dealers$maize.owner.agree.q70[baseline_dealers$maize.owner.agree.q70==999] <- NA

df_descriptives_dealer[69,1] <- mean(baseline_dealers$maize.owner.agree.q70, na.rm=T)
df_descriptives_dealer[69,2] <- min(baseline_dealers$maize.owner.agree.q70, na.rm=T)
df_descriptives_dealer[69,3] <- max(baseline_dealers$maize.owner.agree.q70, na.rm=T)
df_descriptives_dealer[69,4] <- sd(baseline_dealers$maize.owner.agree.q70, na.rm=T)
df_descriptives_dealer[69,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$maize.owner.agree.q70))

df_descriptives_dealer[70,1] <- mean(baseline_dealers$reading, na.rm=T)
df_descriptives_dealer[70,2] <- min(baseline_dealers$reading, na.rm=T)
df_descriptives_dealer[70,3] <- max(baseline_dealers$reading, na.rm=T)
df_descriptives_dealer[70,4] <- sd(baseline_dealers$reading, na.rm=T)
df_descriptives_dealer[70,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$reading))

baseline_dealers$visible_expdate<-ifelse(baseline_dealers$exp=="n/a",0,1)

df_descriptives_dealer[71,1] <- mean(baseline_dealers$visible_expdate, na.rm=T)
df_descriptives_dealer[71,2] <- min(baseline_dealers$visible_expdate, na.rm=T)
df_descriptives_dealer[71,3] <- max(baseline_dealers$visible_expdate, na.rm=T)
df_descriptives_dealer[71,4] <- sd(baseline_dealers$visible_expdate, na.rm=T)
df_descriptives_dealer[71,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$visible_expdate))

baseline_dealers$seed_expired <- 0
baseline_dealers$seed_expired[is.na(baseline_dealers$exp)] <- NA
baseline_dealers$seed_expired[baseline_dealers$exp=="n/a"] <- NA
baseline_dealers$seed_expired[is.na(baseline_dealers$date)] <- NA
baseline_dealers$seed_expired[baseline_dealers$date=="n/a"] <- NA
#1 shop has n/a for date but not for exp
baseline_dealers$date <- as.Date(baseline_dealers$date)
baseline_dealers$exp <- as.Date(baseline_dealers$exp)
baseline_dealers$days_since_exp <- baseline_dealers$date - baseline_dealers$exp
baseline_dealers$seed_expired[baseline_dealers$days_since_exp > 0] <- 1

df_descriptives_dealer[72,1] <- mean(baseline_dealers$seed_expired, na.rm=T)
df_descriptives_dealer[72,2] <- min(baseline_dealers$seed_expired, na.rm=T)
df_descriptives_dealer[72,3] <- max(baseline_dealers$seed_expired, na.rm=T)
df_descriptives_dealer[72,4] <- sd(baseline_dealers$seed_expired, na.rm=T)
df_descriptives_dealer[72,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$seed_expired))

baseline_dealers$visible_packdate<-ifelse(baseline_dealers$date_pack=="n/a",0,1)

df_descriptives_dealer[73,1] <- mean(baseline_dealers$visible_packdate, na.rm=T)
df_descriptives_dealer[73,2] <- min(baseline_dealers$visible_packdate, na.rm=T)
df_descriptives_dealer[73,3] <- max(baseline_dealers$visible_packdate, na.rm=T)
df_descriptives_dealer[73,4] <- sd(baseline_dealers$visible_packdate, na.rm=T)
df_descriptives_dealer[73,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$visible_packdate))

baseline_dealers$seedolderthan6m <- 0
baseline_dealers$seedolderthan6m[is.na(baseline_dealers$date_pack)] <- NA
baseline_dealers$seedolderthan6m[baseline_dealers$date_pack=="n/a"] <- NA
baseline_dealers$seedolderthan6m[is.na(baseline_dealers$date)] <- NA
baseline_dealers$date_pack <- as.Date(baseline_dealers$date_pack)
baseline_dealers$shelflife <- baseline_dealers$date - baseline_dealers$date_pack
baseline_dealers$seedolderthan6m[baseline_dealers$shelflife > 183] <- 1 #6x366/12

df_descriptives_dealer[74,1] <- mean(baseline_dealers$seedolderthan6m, na.rm=T)
df_descriptives_dealer[74,2] <- min(baseline_dealers$seedolderthan6m, na.rm=T)
df_descriptives_dealer[74,3] <- max(baseline_dealers$seedolderthan6m, na.rm=T)
df_descriptives_dealer[74,4] <- sd(baseline_dealers$seedolderthan6m, na.rm=T)
df_descriptives_dealer[74,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$seedolderthan6m))

# #compare my "shelflife" with Bjorn's "age"
# 
# #my "shelflife"
# summary(as.numeric(baseline_dealers$shelflife))
# # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# #  -15.00   32.25   58.50   62.40   77.75  261.00     194
# 
# #Bjorn's "age" in baseline_dealers
# summary(baseline_dealers$age)
# # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# #  -15.00   33.00   52.00   66.35   79.00  259.96     159
# 
# #Bjorn's "age" when using only his first line of code
# baseline_dealers$age2 <- difftime(strptime("01.10.2020", format = "%d.%m.%Y"),strptime(baseline_dealers$date_pack,format="%Y-%m-%d"),units="days")
# summary(as.numeric(baseline_dealers$age2))
# # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# #  -15.00   35.00   57.00   64.97   79.00  259.96     191
# 
# #my "shelflife" after using 01/10/21
# baseline_dealers$shelflife2 <- as.Date("2020-10-01") - as.Date(baseline_dealers$date_pack)
# summary(as.numeric(baseline_dealers$shelflife2))
# 
# #Bjorn's "age" when using only his first line of code & my "shelflife2" after using 01/10/21 MATCH
# 
# sum(is.na(baseline_dealers$date_pack) & !is.na(baseline_dealers$exp))
# #32 shop have an NA for date_pack but no NA for exp
# 
# #baseline_dealers$date_pack2 <- ifelse(is.na(baseline_dealers$date_pack), as.Date(baseline_dealers$exp), as.Date(baseline_dealers$date_pack))
# 
# baseline_dealers$date_pack_incltransformedexp<-baseline_dealers$date_pack
# baseline_dealers$transformedexp <- baseline_dealers$exp - 180
# baseline_dealers$date_pack_incltransformedexp[is.na(baseline_dealers$date_pack)]<-baseline_dealers$transformedexp[is.na(baseline_dealers$date_pack)]
# baseline_dealers$shelflife3 <- as.Date("2020-10-01") - as.Date(baseline_dealers$date_pack_incltransformedexp)
# 
# summary(as.numeric(baseline_dealers$age))
# summary(as.numeric(baseline_dealers$shelflife3))

baseline_dealers$date_pack_incltransformedexp <- baseline_dealers$date_pack
baseline_dealers$transformedexp <- baseline_dealers$exp - 183 #6x366/12
baseline_dealers$date_pack_incltransformedexp[is.na(baseline_dealers$date_pack)]<-baseline_dealers$transformedexp[is.na(baseline_dealers$date_pack)]
baseline_dealers$shelflife_Caro <- baseline_dealers$date - as.Date(baseline_dealers$date_pack_incltransformedexp)
baseline_dealers$shelflife_Caro[baseline_dealers$shelflife_Caro < 0] <- NA

df_descriptives_dealer[75,1] <- mean(baseline_dealers$shelflife_Caro, na.rm=T)
df_descriptives_dealer[75,2] <- min(baseline_dealers$shelflife_Caro, na.rm=T)
df_descriptives_dealer[75,3] <- max(baseline_dealers$shelflife_Caro, na.rm=T)
df_descriptives_dealer[75,4] <- sd(baseline_dealers$shelflife_Caro, na.rm=T)
df_descriptives_dealer[75,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$shelflife_Caro))
#nobs: 348 obs - 159 NA's for Bjorn's "age" in baseline_dealers or my shelflife3 - 3 sum(is.na(baseline_dealers$date)) - 3 packaging date after interview

baseline_dealers$origin<-ifelse(baseline_dealers$origin=="Yes",1,0)

df_descriptives_dealer[76,1] <- mean(baseline_dealers$origin, na.rm=T)
df_descriptives_dealer[76,2] <- min(baseline_dealers$origin, na.rm=T)
df_descriptives_dealer[76,3] <- max(baseline_dealers$origin, na.rm=T)
df_descriptives_dealer[76,4] <- sd(baseline_dealers$origin, na.rm=T)
df_descriptives_dealer[76,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$origin))

baseline_dealers$cert<-ifelse(baseline_dealers$cert=="Yes",1,0)

df_descriptives_dealer[77,1] <- mean(baseline_dealers$cert, na.rm=T)
df_descriptives_dealer[77,2] <- min(baseline_dealers$cert, na.rm=T)
df_descriptives_dealer[77,3] <- max(baseline_dealers$cert, na.rm=T)
df_descriptives_dealer[77,4] <- sd(baseline_dealers$cert, na.rm=T)
df_descriptives_dealer[77,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$cert))

baseline_dealers$lot<-ifelse(baseline_dealers$lot=="Yes",1,0)

df_descriptives_dealer[78,1] <- mean(baseline_dealers$lot, na.rm=T)
df_descriptives_dealer[78,2] <- min(baseline_dealers$lot, na.rm=T)
df_descriptives_dealer[78,3] <- max(baseline_dealers$lot, na.rm=T)
df_descriptives_dealer[78,4] <- sd(baseline_dealers$lot, na.rm=T)
df_descriptives_dealer[78,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$lot))

baseline_dealers$verif<-ifelse(baseline_dealers$verif=="Yes",1,0)

df_descriptives_dealer[79,1] <- mean(baseline_dealers$verif, na.rm=T)
df_descriptives_dealer[79,2] <- min(baseline_dealers$verif, na.rm=T)
df_descriptives_dealer[79,3] <- max(baseline_dealers$verif, na.rm=T)
df_descriptives_dealer[79,4] <- sd(baseline_dealers$verif, na.rm=T)
df_descriptives_dealer[79,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers$verif))

# #path in beginning; "C:/Users/u0127963/Desktop/PhD/Seed_systems_project/papers"
# 
# path <- paste(path,"/papers")
# 
# #path in end: "C:/Users/u0127963/Desktop/PhD/Seed_systems_project"
# 
# path