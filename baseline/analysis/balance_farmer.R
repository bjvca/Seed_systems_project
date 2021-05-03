#balance table farmers

rm(list=ls())

#for Caro
setwd("C:/Users/u0127963/Desktop/PhD/Seed_systems_project/baseline/registered_report")
path <- getwd()
path <- strsplit(path, "/registered_report")[[1]]
baseline_farmers <- read.csv(paste(path,"data/farmer/public/baseline_farmers.csv", sep="/"), stringsAsFactors = TRUE)

#no treatment indicator for dealer training in baseline_farmers
#treatments at shop level
treatments_shop_level <- read.csv(paste(path,"data/agro_input/public/treats_shop_level.csv", sep="/"), stringsAsFactors = TRUE)
#treatments at CA level
trainingtreatment_CA_level <- data.frame(aggregate(treatments_shop_level$training, list(treatments_shop_level$catchID), mean))
names(trainingtreatment_CA_level) <- c("catchID","training")
baseline_farmers <- merge(baseline_farmers, trainingtreatment_CA_level, by.x="catchID", by.y="catchID")

#empty tables with NAs
#10 columns, 2 rows
df_averages <- array(NA,dim=c(2,10))
#2 columns, 3 rows, 10 times
df_ols <- array(NA,dim=c(3,3,10))

###FILLING df_averages
# 1. age of household head - years (q14)
baseline_farmers$Check2.check.maize.q14[baseline_farmers$Check2.check.maize.q14 == 999] <- NA
#hist(baseline_farmers$Check2.check.maize.q14)
summary(baseline_farmers$Check2.check.maize.q14)
#plot(density(baseline_farmers$Check2.check.maize.q14, na.rm=T))
table(baseline_farmers$Check2.check.maize.q14)

df_averages[1,1] <- mean(baseline_farmers$Check2.check.maize.q14, na.rm=T)
df_averages[2,1] <- sd(baseline_farmers$Check2.check.maize.q14, na.rm=T)

# 2. household head has finished primary education? - 1 is yes (q17)
baseline_farmers$primaryeducation <- FALSE
baseline_farmers$primaryeducation <- (baseline_farmers$Check2.check.maize.q17 %in% c("c","d","e","f"))

baseline_farmers$primaryeducation[baseline_farmers$primaryeducation == 999] <- NA
summary(baseline_farmers$primaryeducation)
table(baseline_farmers$primaryeducation)

df_averages[1,2] <- mean(baseline_farmers$primaryeducation=="TRUE", na.rm=T)
df_averages[2,2] <- sd(baseline_farmers$primaryeducation=="TRUE", na.rm=T)

# 3. gender of household head - 1 is male (q15)
baseline_farmers$Check2.check.maize.q15[baseline_farmers$Check2.check.maize.q15 == 999] <- NA
summary(baseline_farmers$Check2.check.maize.q15)
table(baseline_farmers$Check2.check.maize.q15)

df_averages[1,3] <- mean(baseline_farmers$Check2.check.maize.q15=="Male", na.rm=T)
df_averages[2,3] <- sd(baseline_farmers$Check2.check.maize.q15=="Male", na.rm=T)

# 4. household size - number of people in household (including interviewee)/number of individuals that eats on a regular basis in the house (q18)
baseline_farmers$Check2.check.maize.q18[baseline_farmers$Check2.check.maize.q18 == 999] <- NA
#hist(baseline_farmers$Check2.check.maize.q18)
summary(baseline_farmers$Check2.check.maize.q18)
#plot(density(baseline_farmers$Check2.check.maize.q18, na.rm=T))
table(baseline_farmers$Check2.check.maize.q18)

df_averages[1,4] <- mean(baseline_farmers$Check2.check.maize.q18, na.rm=T)
df_averages[2,4] <- sd(baseline_farmers$Check2.check.maize.q18, na.rm=T)

# 5. distance of homestead to nearest agro-input shop selling maize seed - km (q10)
baseline_farmers$Check2.check.maize.q10[baseline_farmers$Check2.check.maize.q10 == 999] <- NA
#hist(baseline_farmers$Check2.check.maize.q10)
summary(baseline_farmers$Check2.check.maize.q10)
#plot(density(baseline_farmers$Check2.check.maize.q10, na.rm=T))
table(baseline_farmers$Check2.check.maize.q10)

df_averages[1,5] <- mean(baseline_farmers$Check2.check.maize.q10, na.rm=T)
df_averages[2,5] <- sd(baseline_farmers$Check2.check.maize.q10, na.rm=T)

# 1. *purchased* quality maize seed like OPV or hybrid in the last season
# for any plot - 1 is yes (q25a, q25b aggregated at household level)
# 2. bought quality maize seed like OPV or hybrid from agro-input shop
# in the last season for any plot - 1 is yes (q25a, q25b aggregated at
#                                             household level)
# 3. quantity of quality maize seed (hybrid or OPV) bought from an input
# dealer in the last season - kg (q25d aggregated at household level)
# 4. index of seed quality perception: average ratings of maize seed of all
# input dealers in catchment area (based on q69a-q69f aggregated at
#                                  household level)
# 5. index of dealer (effort) perception (based on q68b-q68f, q70-q76)
# 6. share of farmers switching to different dealer (q67)
# 7. index of farmers practices: interaction between adoption on that plot
# (q31, (q32)) and farmers practices on that plot (q40-q49)

# 6. quantity of quality maize seed bought from an input dealer in kg
table(baseline_farmers$Check2.check.maize.q25d)
baseline_farmers$Check2.check.maize.q25d[baseline_farmers$Check2.check.maize.q25d == "n/a"] <- NA
baseline_farmers$Check2.check.maize.q25d[baseline_farmers$Check2.check.maize.q25d == 999] <- NA
baseline_farmers$Check2.check.maize.q25d <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q25d))
baseline_farmers$Check2.check.maize.q25d[is.na(baseline_farmers$Check2.check.maize.q25d)] = 0

#hist(baseline_farmers$Check2.check.maize.q25d)
summary(baseline_farmers$Check2.check.maize.q25d)
#boxplot(baseline_farmers$Check2.check.maize.q25d)
#plot(density(baseline_farmers$Check2.check.maize.q25d))

df_averages[1,6] <- mean(baseline_farmers$Check2.check.maize.q25d, na.rm=T)
df_averages[2,6] <- sd(baseline_farmers$Check2.check.maize.q25d, na.rm=T)

# 7. has used any quality maize seed like OPV or hybrid in the last season on any of your #plots - 1 is yes (q25a)
baseline_farmers$Check2.check.maize.q25a[baseline_farmers$Check2.check.maize.q25a == 999] <- NA
baseline_farmers$Check2.check.maize.q25a[baseline_farmers$Check2.check.maize.q25a == 98] <- NA
summary(baseline_farmers$Check2.check.maize.q25a)
table(baseline_farmers$Check2.check.maize.q25a)

df_averages[1,7] <- mean(baseline_farmers$Check2.check.maize.q25a=="Yes", na.rm=T)
df_averages[2,7] <- sd(baseline_farmers$Check2.check.maize.q25a=="Yes", na.rm=T)

# 8. do you think that maize seed that you can buy at agro-input dealer is counterfeit/adulterated? - 1 is yes (q25h)
baseline_farmers$Check2.check.maize.q25h[baseline_farmers$Check2.check.maize.q25h == 999] <- NA
baseline_farmers$Check2.check.maize.q25h[baseline_farmers$Check2.check.maize.q25h == 98] <- NA
summary(baseline_farmers$Check2.check.maize.q25h)
table(baseline_farmers$Check2.check.maize.q25h)

df_averages[1,8] <- mean(baseline_farmers$Check2.check.maize.q25h=="Yes", na.rm=T)
df_averages[2,8] <- sd(baseline_farmers$Check2.check.maize.q25h=="Yes", na.rm=T)

# 9. has bought quality maize seed like OPV or hybrid in the last season from agro-input shop for any of your #plots - 1 is yes (q25a, q25b aggregated at household level)
baseline_farmers$fromagroinputshop <- FALSE
baseline_farmers$fromagroinputshop <- (baseline_farmers$Check2.check.maize.q25a=="Yes" & baseline_farmers$Check2.check.maize.q25b=="d")
summary(baseline_farmers$fromagroinputshop)
table(baseline_farmers$fromagroinputshop)

df_averages[1,9] <- mean(baseline_farmers$fromagroinputshop=="TRUE", na.rm=T)
df_averages[2,9] <- sd(baseline_farmers$fromagroinputshop=="TRUE", na.rm=T)

# 10. maize yields in the last season on a randomly chosen #plot - estimated production/estimated size of the #plot (q29, q30a,q30b, q50, q51)
#Q29. What is the area of this **${#plot_select_name}** maize field during the second season of **2020 (entoigo 2020)**? **ACRES** 
baseline_farmers$Check2.check.maize.q29[baseline_farmers$Check2.check.maize.q29 == 999] <- NA
#hist(baseline_farmers$Check2.check.maize.q29)
summary(baseline_farmers$Check2.check.maize.q29)
#plot(density(baseline_farmers$Check2.check.maize.q29, na.rm=T))
table(baseline_farmers$Check2.check.maize.q29)

#Q50. How many bags of maize did you harvest from this **${_select_name}**  in the second season (entoigo) of 2020?(including maize that was consumed)?
baseline_farmers$Check2.check.maize.q50[baseline_farmers$Check2.check.maize.q50 == 999] <- NA
#baseline_farmers$Check2.check.maize.q50[baseline_farmers$Check2.check.maize.q50 == 250] <- NA
#250 bags is an outlier
#hist(baseline_farmers$Check2.check.maize.q50)
summary(baseline_farmers$Check2.check.maize.q50)
#plot(density(baseline_farmers$Check2.check.maize.q50, na.rm=T))
table(baseline_farmers$Check2.check.maize.q50)

#Q51. How many kgs is in one bag?
baseline_farmers$Check2.check.maize.q51[baseline_farmers$Check2.check.maize.q51 == 999] <- NA
#hist(baseline_farmers$Check2.check.maize.q51)
summary(baseline_farmers$Check2.check.maize.q51)
#plot(density(baseline_farmers$Check2.check.maize.q51, na.rm=T))
table(baseline_farmers$Check2.check.maize.q51)

# #TAKING INTERCROPPING INTO ACCOUNT
# #Q30. Was this **${#plot_select_name}** plot intercropped in the second season (entoigo) of 2020?
# baseline_farmers$Check2.check.maize.q30[baseline_farmers$Check2.check.maize.q30 == 999] <- NA
# summary(baseline_farmers$Check2.check.maize.q30)
# table(baseline_farmers$Check2.check.maize.q30)
# 
# #Q30b. Estimate percentage allocated to maize on **${plot_select_name}** in the second season (entoigo) of 2020? **[percentage %]**
# baseline_farmers$Check2.check.maize.q30b[baseline_farmers$Check2.check.maize.q30b == 999] <- NA
# baseline_farmers$Check2.check.maize.q30b[baseline_farmers$Check2.check.maize.q30b == "n/a"] <- NA
# baseline_farmers$Check2.check.maize.q30b <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q30b))
# #hist(baseline_farmers$Check2.check.maize.q30b)
# summary(baseline_farmers$Check2.check.maize.q30b)
# #(density(baseline_farmers$Check2.check.maize.q30b, na.rm=T))
# table(baseline_farmers$Check2.check.maize.q30b)
# 
# #area if intercropped
# baseline_farmers$area_intercropped <- baseline_farmers$Check2.check.maize.q29*baseline_farmers$Check2.check.maize.q30b/100
# #hist(baseline_farmers$area_intercropped)
# summary(baseline_farmers$area_intercropped)
# #plot(density(baseline_farmers$area_intercropped, na.rm=T))
# table(baseline_farmers$area_intercropped)
# 
# #area if not intercropped
# baseline_farmers$area_not_intercropped <- baseline_farmers$Check2.check.maize.q29
# 
# #area both
# baseline_farmers$area<-ifelse(baseline_farmers$Check2.check.maize.q30=="Yes",baseline_farmers$area_intercropped,baseline_farmers$area_not_intercropped)
# #hist(baseline_farmers$area)
# summary(baseline_farmers$area)
# #plot(density(baseline_farmers$area, na.rm=T))
# table(baseline_farmers$area)
# baseline_farmers$yield_kgperacre[baseline_farmers$yield_kgperacre == 140000] <- NA
# #outlier

#IGNORING INTERCROPPING BECAUSE WILBER SAYS SO.
#IF MAIZE IS INTERCROPPED, MAIZE IS OFTEN (ALMOST ALWAYS) THE MAIN CROP.
#INTERCROPPED OR NOT, THERE'S AN EQUAL NUMBER OF MAIZE CROPS.
baseline_farmers$area <- baseline_farmers$Check2.check.maize.q29

#production in kg
baseline_farmers$production_kg <- baseline_farmers$Check2.check.maize.q50*baseline_farmers$Check2.check.maize.q51
#hist(baseline_farmers$production_kg)
summary(baseline_farmers$production_kg)
#plot(density(baseline_farmers$production_kg, na.rm=T))
table(baseline_farmers$production_kg)

#yield in kg per acre
baseline_farmers$yield_kgperacre <- baseline_farmers$production_kg/baseline_farmers$area
#hist(baseline_farmers$yield_kgperacre)
summary(baseline_farmers$yield_kgperacre)
#plot(density(baseline_farmers$yield_kgperacre, na.rm=T))
table(baseline_farmers$yield_kgperacre)

df_averages[1,10] <- mean(baseline_farmers$yield_kgperacre, na.rm=T)
df_averages[2,10] <- sd(baseline_farmers$yield_kgperacre, na.rm=T)








###FILLING df_ols

# 1. age of household head - years (q14)
#* because of interactions
ols <- lm(Check2.check.maize.q14~training*Check2.check.maize.clearing*Check2.check.maize.video_shown, data=baseline_farmers)

#to use vcovCR
library(clubSandwich)

#vcovCR for cluster-robust variance-covariance matrix
#CR0 is the original form of the sandwich estimator (Liang & Zeger, 1986), which does not make any small-sample correction.
vcov_cluster <- vcovCR(ols, cluster=baseline_farmers$catchID, type = "CR0")

#coef_test [1,...] = Intercept
#coef_test [2,...] = training
#coef_test [3,...] = CH
#coef_test [4,...] = video
#coef_test [5,...] = training*CH
#coef_test [6,...] = training*video
#coef_test [7,...] = CH*video
#coef_test [8,...] = training*CH*video

#filling df_ols with training (Estimate, SE, p-val (Satt))
df_ols[1,1,1] <- coef_test(ols, vcov_cluster)[2,1]
df_ols[2,1,1] <- coef_test(ols, vcov_cluster)[2,2]
df_ols[3,1,1] <- coef_test(ols, vcov_cluster)[2,5]

#filling df_ols with CH (Estimate, SE, p-val (Satt))
df_ols[1,2,1] <- coef_test(ols, vcov_cluster)[3,1]
df_ols[2,2,1] <- coef_test(ols, vcov_cluster)[3,2]
df_ols[3,2,1] <- coef_test(ols, vcov_cluster)[3,5]

#filling df_ols with video (Estimate, SE, p-val (Satt))
df_ols[1,3,1] <- coef_test(ols, vcov_cluster)[4,1]
df_ols[2,3,1] <- coef_test(ols, vcov_cluster)[4,2]
df_ols[3,3,1] <- coef_test(ols, vcov_cluster)[4,5]

# 2. household head has finished primary education? - 1 is yes (q17)
ols <- lm((baseline_farmers$primaryeducation=="TRUE")~training*Check2.check.maize.clearing*baseline_farmers$Check2.check.maize.video_shown, data=baseline_farmers)
vcov_cluster <- vcovCR(ols, cluster=baseline_farmers$catchID, type = "CR0")

df_ols[1,1,2] <- coef_test(ols, vcov_cluster)[2,1]
df_ols[2,1,2] <- coef_test(ols, vcov_cluster)[2,2]
df_ols[3,1,2] <- coef_test(ols, vcov_cluster)[2,5]

df_ols[1,2,2] <- coef_test(ols, vcov_cluster)[3,1]
df_ols[2,2,2] <- coef_test(ols, vcov_cluster)[3,2]
df_ols[3,2,2] <- coef_test(ols, vcov_cluster)[3,5]

df_ols[1,3,2] <- coef_test(ols, vcov_cluster)[4,1]
df_ols[2,3,2] <- coef_test(ols, vcov_cluster)[4,2]
df_ols[3,3,2] <- coef_test(ols, vcov_cluster)[4,5]

# 3. gender of household head - 1 is male (q15)
ols <- lm((Check2.check.maize.q15=="Male")~training*Check2.check.maize.clearing*Check2.check.maize.video_shown, data=baseline_farmers)
vcov_cluster <- vcovCR(ols, cluster=baseline_farmers$catchID, type = "CR0")

df_ols[1,1,3] <- coef_test(ols, vcov_cluster)[2,1]
df_ols[2,1,3] <- coef_test(ols, vcov_cluster)[2,2]
df_ols[3,1,3] <- coef_test(ols, vcov_cluster)[2,5]

df_ols[1,2,3] <- coef_test(ols, vcov_cluster)[3,1]
df_ols[2,2,3] <- coef_test(ols, vcov_cluster)[3,2]
df_ols[3,2,3] <- coef_test(ols, vcov_cluster)[3,5]

df_ols[1,3,3] <- coef_test(ols, vcov_cluster)[4,1]
df_ols[2,3,3] <- coef_test(ols, vcov_cluster)[4,2]
df_ols[3,3,3] <- coef_test(ols, vcov_cluster)[4,5]

# 4. household size - number of people in household (including interviewee)/number of individuals that eats on a regular basis in the house (q18)
ols <- lm(baseline_farmers$Check2.check.maize.q18~training*Check2.check.maize.clearing*Check2.check.maize.video_shown, data=baseline_farmers)
vcov_cluster <- vcovCR(ols, cluster=baseline_farmers$catchID, type = "CR0")

df_ols[1,1,4] <- coef_test(ols, vcov_cluster)[2,1]
df_ols[2,1,4] <- coef_test(ols, vcov_cluster)[2,2]
df_ols[3,1,4] <- coef_test(ols, vcov_cluster)[2,5]

df_ols[1,2,4] <- coef_test(ols, vcov_cluster)[3,1]
df_ols[2,2,4] <- coef_test(ols, vcov_cluster)[3,2]
df_ols[3,2,4] <- coef_test(ols, vcov_cluster)[3,5]

df_ols[1,3,4] <- coef_test(ols, vcov_cluster)[4,1]
df_ols[2,3,4] <- coef_test(ols, vcov_cluster)[4,2]
df_ols[3,3,4] <- coef_test(ols, vcov_cluster)[4,5]

# 5. distance of homestead to nearest agro-input shop selling maize seed - km (q10)
ols <- lm(baseline_farmers$Check2.check.maize.q10~training*Check2.check.maize.clearing*Check2.check.maize.video_shown, data=baseline_farmers)
vcov_cluster <- vcovCR(ols, cluster=baseline_farmers$catchID, type = "CR0")

df_ols[1,1,5] <- coef_test(ols, vcov_cluster)[2,1]
df_ols[2,1,5] <- coef_test(ols, vcov_cluster)[2,2]
df_ols[3,1,5] <- coef_test(ols, vcov_cluster)[2,5]

df_ols[1,2,5] <- coef_test(ols, vcov_cluster)[3,1]
df_ols[2,2,5] <- coef_test(ols, vcov_cluster)[3,2]
df_ols[3,2,5] <- coef_test(ols, vcov_cluster)[3,5]

df_ols[1,3,5] <- coef_test(ols, vcov_cluster)[4,1]
df_ols[2,3,5] <- coef_test(ols, vcov_cluster)[4,2]
df_ols[3,3,5] <- coef_test(ols, vcov_cluster)[4,5]

# 6. quantity of quality maize seed bought from an input dealer in kg
ols <- lm((Check2.check.maize.q25d)~training*Check2.check.maize.clearing*baseline_farmers$Check2.check.maize.video_shown, data=baseline_farmers)
vcov_cluster <- vcovCR(ols, cluster=baseline_farmers$catchID, type = "CR0")

df_ols[1,1,6] <- coef_test(ols, vcov_cluster)[2,1]
df_ols[2,1,6] <- coef_test(ols, vcov_cluster)[2,2]
df_ols[3,1,6] <- coef_test(ols, vcov_cluster)[2,5]

df_ols[1,2,6] <- coef_test(ols, vcov_cluster)[3,1]
df_ols[2,2,6] <- coef_test(ols, vcov_cluster)[3,2]
df_ols[3,2,6] <- coef_test(ols, vcov_cluster)[3,5]

df_ols[1,3,6] <- coef_test(ols, vcov_cluster)[4,1]
df_ols[2,3,6] <- coef_test(ols, vcov_cluster)[4,2]
df_ols[3,3,6] <- coef_test(ols, vcov_cluster)[4,5]

# 7. has used any quality maize seed like OPV or hybrid in the last season on any of your plots - 1 is yes (q25a)
ols <- lm(baseline_farmers$Check2.check.maize.q25a=="Yes"~training*Check2.check.maize.clearing*Check2.check.maize.video_shown, data=baseline_farmers)
vcov_cluster <- vcovCR(ols, cluster=baseline_farmers$catchID, type = "CR0")

df_ols[1,1,7] <- coef_test(ols, vcov_cluster)[2,1]
df_ols[2,1,7] <- coef_test(ols, vcov_cluster)[2,2]
df_ols[3,1,7] <- coef_test(ols, vcov_cluster)[2,5]

df_ols[1,2,7] <- coef_test(ols, vcov_cluster)[3,1]
df_ols[2,2,7] <- coef_test(ols, vcov_cluster)[3,2]
df_ols[3,2,7] <- coef_test(ols, vcov_cluster)[3,5]

df_ols[1,3,7] <- coef_test(ols, vcov_cluster)[4,1]
df_ols[2,3,7] <- coef_test(ols, vcov_cluster)[4,2]
df_ols[3,3,7] <- coef_test(ols, vcov_cluster)[4,5]

# 8. do you think that maize seed that you can buy at agro-input dealer is counterfeit/adulterated? - 1 is yes (q25h)
ols <- lm(baseline_farmers$Check2.check.maize.q25h=="Yes"~training*Check2.check.maize.clearing*Check2.check.maize.video_shown, data=baseline_farmers)
vcov_cluster <- vcovCR(ols, cluster=baseline_farmers$catchID, type = "CR0")

df_ols[1,1,8] <- coef_test(ols, vcov_cluster)[2,1]
df_ols[2,1,8] <- coef_test(ols, vcov_cluster)[2,2]
df_ols[3,1,8] <- coef_test(ols, vcov_cluster)[2,5]

df_ols[1,2,8] <- coef_test(ols, vcov_cluster)[3,1]
df_ols[2,2,8] <- coef_test(ols, vcov_cluster)[3,2]
df_ols[3,2,8] <- coef_test(ols, vcov_cluster)[3,5]

df_ols[1,3,8] <- coef_test(ols, vcov_cluster)[4,1]
df_ols[2,3,8] <- coef_test(ols, vcov_cluster)[4,2]
df_ols[3,3,8] <- coef_test(ols, vcov_cluster)[4,5]

# 9. has bought quality maize seed like OPV or hybrid in the last season from agro-input shop for any of your plots - 1 is yes (q25a, q25b aggregated at household level)
ols <- lm(baseline_farmers$fromagroinputshop=="TRUE"~training*Check2.check.maize.clearing*Check2.check.maize.video_shown, data=baseline_farmers)
vcov_cluster <- vcovCR(ols, cluster=baseline_farmers$catchID, type = "CR0")

df_ols[1,1,9] <- coef_test(ols, vcov_cluster)[2,1]
df_ols[2,1,9] <- coef_test(ols, vcov_cluster)[2,2]
df_ols[3,1,9] <- coef_test(ols, vcov_cluster)[2,5]

df_ols[1,2,9] <- coef_test(ols, vcov_cluster)[3,1]
df_ols[2,2,9] <- coef_test(ols, vcov_cluster)[3,2]
df_ols[3,2,9] <- coef_test(ols, vcov_cluster)[3,5]

df_ols[1,3,9] <- coef_test(ols, vcov_cluster)[4,1]
df_ols[2,3,9] <- coef_test(ols, vcov_cluster)[4,2]
df_ols[3,3,9] <- coef_test(ols, vcov_cluster)[4,5]

# 10. maize yields in the last season on a randomly chosen plot - estimated production/estimated size of the plot (q29, q30a,q30b, q50, q51)
ols <- lm(baseline_farmers$yield_kgperacre~training*Check2.check.maize.clearing*Check2.check.maize.video_shown, data=baseline_farmers)
vcov_cluster <- vcovCR(ols, cluster=baseline_farmers$catchID, type = "CR0")

df_ols[1,1,10] <- coef_test(ols, vcov_cluster)[2,1]
df_ols[2,1,10] <- coef_test(ols, vcov_cluster)[2,2]
df_ols[3,1,10] <- coef_test(ols, vcov_cluster)[2,5]

df_ols[1,2,10] <- coef_test(ols, vcov_cluster)[3,1]
df_ols[2,2,10] <- coef_test(ols, vcov_cluster)[3,2]
df_ols[3,2,10] <- coef_test(ols, vcov_cluster)[3,5]

df_ols[1,3,10] <- coef_test(ols, vcov_cluster)[4,1]
df_ols[2,3,10] <- coef_test(ols, vcov_cluster)[4,2]
df_ols[3,3,10] <- coef_test(ols, vcov_cluster)[4,5]
