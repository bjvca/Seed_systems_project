### balance table for pre-registered/final report
#rm(list=ls())
library(clubSandwich)
#path <- getwd()
#path <- strsplit(path, "/analysis")[[1]]
shops <- read.csv(paste(path,"data/agro_input/public/baseline_dealer.csv", sep="/"))
df_averages <- array(NA,dim=c(2,10))
df_ols <- array(NA,dim=c(3,2,10))
 
#1. age of the person interviewed (most knowledgeable person) - years
#2. education level of the interviewed (most knowledgeable person) - 1 if finished primary education.
#3. gender of the person interviewed (most knowledgeable person) - 1 is male
#4. number of years the business has been in operation (today - year the business was founded)
#5. distance of agro-input dealer to nearest tarmac road - km
#6. On an average day during the week, how many customers come here and buys something related to agriculture (eg seed, fertilizer, farm tools such as hoes,...)? (number of customers)
#7. quantity of hybird/OPV maize seed sold during the last season - kg
#8. quantity of seed that was lost/wasted during the last season - kg
#9. Did you or anyone else who works in this store ever receive any training on handling and storage of maize seed? - 1 is yes
#10. input dealer rating score - average of all farmers that scored the particular input dealer - overall + components

 
## some quick balance tests
shops$maize.owner.agree.age[shops$maize.owner.agree.age == 999] <- NA

df_averages[1,1] <- mean(shops$maize.owner.agree.age, na.rm=T)
df_averages[2,1] <- sd(shops$maize.owner.agree.age, na.rm=T)
 
ols <- lm(maize.owner.agree.age~training*clearing, data=shops)
vcov_cluster <- vcovCR(ols, cluster =shops$catchID, type = "CR0")

df_ols[1,1,1] <- coef_test(ols, vcov_cluster)[2,1]
df_ols[2,1,1] <- coef_test(ols, vcov_cluster)[2,2]
df_ols[3,1,1] <- coef_test(ols, vcov_cluster)[2,5]

df_ols[1,2,1] <- coef_test(ols, vcov_cluster)[3,1]
df_ols[2,2,1] <- coef_test(ols, vcov_cluster)[3,2]
df_ols[3,2,1] <- coef_test(ols, vcov_cluster)[3,5]

df_averages[1,2] <- mean(shops$maize.owner.agree.gender=="Male", na.rm=T)
df_averages[2,2] <- sd(shops$maize.owner.agree.gender=="Male", na.rm=T)

ols <- lm((maize.owner.agree.gender=="Male")~training*clearing, data=shops)
vcov_cluster <- vcovCR(ols, cluster =shops$catchID, type = "CR0")


df_ols[1,1,2] <- coef_test(ols, vcov_cluster)[2,1]
df_ols[2,1,2] <- coef_test(ols, vcov_cluster)[2,2]
df_ols[3,1,2] <- coef_test(ols, vcov_cluster)[2,5]

df_ols[1,2,2] <- coef_test(ols, vcov_cluster)[3,1]
df_ols[2,2,2] <- coef_test(ols, vcov_cluster)[3,2]
df_ols[3,2,2] <- coef_test(ols, vcov_cluster)[3,5]

shops$prim <- FALSE
shops$prim <- (shops$maize.owner.agree.educ %in% c("c","d","e","f"))

df_averages[1,3] <- mean(shops$prim, na.rm=T)
df_averages[2,3] <- sd(shops$prim, na.rm=T)

ols <- lm(prim~training*clearing, data=shops)
vcov_cluster <- vcovCR(ols, cluster =shops$catchID, type = "CR0")


df_ols[1,1,3] <- coef_test(ols, vcov_cluster)[2,1]
df_ols[2,1,3] <- coef_test(ols, vcov_cluster)[2,2]
df_ols[3,1,3] <- coef_test(ols, vcov_cluster)[2,5]

df_ols[1,2,3] <- coef_test(ols, vcov_cluster)[3,1]
df_ols[2,2,3] <- coef_test(ols, vcov_cluster)[3,2]
df_ols[3,2,3] <- coef_test(ols, vcov_cluster)[3,5]

shops$years_shop <- 2020 - as.numeric(as.character(substr(shops$maize.owner.agree.q8, start=1, stop=4)))

df_averages[1,4] <- mean(shops$years_shop, na.rm=T)
df_averages[2,4] <- sd(shops$years_shop, na.rm=T)

ols <- lm(years_shop~training*clearing, data=shops)
vcov_cluster <- vcovCR(ols, cluster =shops$catchID, type = "CR0")


df_ols[1,1,4] <- coef_test(ols, vcov_cluster)[2,1]
df_ols[2,1,4] <- coef_test(ols, vcov_cluster)[2,2]
df_ols[3,1,4] <- coef_test(ols, vcov_cluster)[2,5]

df_ols[1,2,4] <- coef_test(ols, vcov_cluster)[3,1]
df_ols[2,2,4] <- coef_test(ols, vcov_cluster)[3,2]
df_ols[3,2,4] <- coef_test(ols, vcov_cluster)[3,5]

shops$maize.owner.agree.q3[shops$maize.owner.agree.q3 == 999] <- NA
shops$maize.owner.agree.q3[shops$maize.owner.agree.q3 < 1] <- 0

df_averages[1,5] <- mean(shops$maize.owner.agree.q3, na.rm=T)
df_averages[2,5] <- sd(shops$maize.owner.agree.q3, na.rm=T)

ols <- lm(maize.owner.agree.q3~training*clearing, data=shops)
vcov_cluster <- vcovCR(ols, cluster =shops$catchID, type = "CR0")


df_ols[1,1,5] <- coef_test(ols, vcov_cluster)[2,1]
df_ols[2,1,5] <- coef_test(ols, vcov_cluster)[2,2]
df_ols[3,1,5] <- coef_test(ols, vcov_cluster)[2,5]

df_ols[1,2,5] <- coef_test(ols, vcov_cluster)[3,1]
df_ols[2,2,5] <- coef_test(ols, vcov_cluster)[3,2]
df_ols[3,2,5] <- coef_test(ols, vcov_cluster)[3,5]

shops$maize.owner.agree.q6[shops$maize.owner.agree.q6 == 999] <- NA

df_averages[1,6] <- mean(shops$maize.owner.agree.q6, na.rm=T)
df_averages[2,6] <- sd(shops$maize.owner.agree.q6, na.rm=T)

ols <- lm(maize.owner.agree.q6~training*clearing, data=shops)
vcov_cluster <- vcovCR(ols, cluster =shops$catchID, type = "CR0")


df_ols[1,1,6] <- coef_test(ols, vcov_cluster)[2,1]
df_ols[2,1,6] <- coef_test(ols, vcov_cluster)[2,2]
df_ols[3,1,6] <- coef_test(ols, vcov_cluster)[2,5]

df_ols[1,2,6] <- coef_test(ols, vcov_cluster)[3,1]
df_ols[2,2,6] <- coef_test(ols, vcov_cluster)[3,2]
df_ols[3,2,6] <- coef_test(ols, vcov_cluster)[3,5]

##7. quantity of hybird/OPV maize seed sold during the last season - kg
sel <- c("maize.owner.agree.long10h.q25", "maize.owner.agree.longe7h.q37", "maize.owner.agree.longe5.q50", "maize.owner.agree.longe4.q62")
shops[sel] <- lapply(shops[sel], function(x) as.numeric(as.character(x)) )
shops[sel] <- lapply(shops[sel], function(x) replace(x, x == 999,NA) )

shops$tot_sold <- rowSums(shops[sel], na.rm=T)
shops$tot_sold[shops$tot_sold > 80000] <- NA
df_averages[1,7] <- mean(shops$tot_sold, na.rm=T)
df_averages[2,7] <- sd(shops$tot_sold, na.rm=T)

ols <- lm(tot_sold~training*clearing, data=shops)
vcov_cluster <- vcovCR(ols, cluster =shops$catchID, type = "CR0")


df_ols[1,1,7] <- coef_test(ols, vcov_cluster)[2,1]
df_ols[2,1,7] <- coef_test(ols, vcov_cluster)[2,2]
df_ols[3,1,7] <- coef_test(ols, vcov_cluster)[2,5]

df_ols[1,2,7] <- coef_test(ols, vcov_cluster)[3,1]
df_ols[2,2,7] <- coef_test(ols, vcov_cluster)[3,2]
df_ols[3,2,7] <- coef_test(ols, vcov_cluster)[3,5]

# 8. quantity of seed that was lost/wasted during the last season - kg
sel <- c("maize.owner.agree.long10h.q27", "maize.owner.agree.longe7h.q39", "maize.owner.agree.longe5.q52", "maize.owner.agree.longe4.q64")
shops[sel] <- lapply(shops[sel], function(x) as.numeric(as.character(x)) )
shops[sel] <- lapply(shops[sel], function(x) replace(x, x == 999,NA) )

shops$tot_lost <- rowSums(shops[sel], na.rm=T)

df_averages[1,8] <- mean(shops$tot_lost, na.rm=T)
df_averages[2,8] <- sd(shops$tot_lost, na.rm=T)

ols <- lm(tot_lost~training*clearing, data=shops)
vcov_cluster <- vcovCR(ols, cluster =shops$catchID, type = "CR0")


df_ols[1,1,8] <- coef_test(ols, vcov_cluster)[2,1]
df_ols[2,1,8] <- coef_test(ols, vcov_cluster)[2,2]
df_ols[3,1,8] <- coef_test(ols, vcov_cluster)[2,5]

df_ols[1,2,8] <- coef_test(ols, vcov_cluster)[3,1]
df_ols[2,2,8] <- coef_test(ols, vcov_cluster)[3,2]
df_ols[3,2,8] <- coef_test(ols, vcov_cluster)[3,5]

## Did you or anyone else who works in this store ever receive any training on handling and storage of maize seed? 



df_averages[1,9] <- mean(shops$maize.owner.agree.q10=="Yes", na.rm=T)
df_averages[2,9] <- sd(shops$maize.owner.agree.q10=="Yes", na.rm=T)

ols <- lm((maize.owner.agree.q10=="Yes")~training*clearing, data=shops)
vcov_cluster <- vcovCR(ols, cluster =shops$catchID, type = "CR0")


df_ols[1,1,9] <- coef_test(ols, vcov_cluster)[2,1]
df_ols[2,1,9] <- coef_test(ols, vcov_cluster)[2,2]
df_ols[3,1,9] <- coef_test(ols, vcov_cluster)[2,5]

df_ols[1,2,9] <- coef_test(ols, vcov_cluster)[3,1]
df_ols[2,2,9] <- coef_test(ols, vcov_cluster)[3,2]
df_ols[3,2,9] <- coef_test(ols, vcov_cluster)[3,5]



