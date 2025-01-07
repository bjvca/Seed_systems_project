rm(list=ls())
library(clubSandwich)
#df_ols_ --> df_ols_end_
#df_means_ --> df_means_end_

matStand <- function(x, sgroup = rep(TRUE, nrow(x))){
  for(j in 1:ncol(x)){
    x[,j] <- (x[,j] - mean(x[sgroup,j],na.rm = T))/sd(x[sgroup,j],na.rm = T)
  }
  return(x)
}

icwIndex <- function(	xmat,
                      #wgts=rep(1, nrow(xmat)), #nrow: number of rows present in xmat --> many 1s
                      revcols = NULL,
                      sgroup = rep(TRUE, nrow(xmat))){
  X <- matStand(xmat, sgroup)
  if(length(revcols)>0){
    X[,revcols] <-  -1*X[,revcols]
  }
  i.vec <- as.matrix(rep(1,ncol(xmat)))
  #Sx <- cov.wt(X, wt=wgts)[[1]]
  #list with estimates of the weighted covariance matrix and the mean of the data
  Sx <- cov(X,use = "pairwise.complete.obs")
  #cov: covariance of x and y if these are vectors/covariances between columns of x and columns of y are computed if these are matrices
  #use = "everything" produces NAs for the index.
  #use = "all.obs" produces an error.
  #use = "complete.obs" and use = "na.or.complete": works, NAs are handled by casewise deletion.
  #use = "pairwise.complete.obs": works, covariance between each pair of variables is computed using all complete pairs of observations on those variables
  weights <- solve(t(i.vec)%*%solve(Sx)%*%i.vec)%*%t(i.vec)%*%solve(Sx)
  index <- t(solve(t(i.vec)%*%solve(Sx)%*%i.vec)%*%t(i.vec)%*%solve(Sx)%*%t(X))
  return(list(weights = weights, index = index))
}

trim <- function(var,dataset,trim_perc=.02){
  dataset[var][dataset[var]<quantile(dataset[var],c(trim_perc/2,1-(trim_perc/2)),na.rm=T)[1]|dataset[var]>quantile(dataset[var],c(trim_perc/2,1-(trim_perc/2)),na.rm=T)[2]] <- NA
  return(dataset)}

ihs <- function(x) {
  y <- log(x + sqrt(x ^ 2 + 1))
  return(y)}

path <- getwd()
path <- strsplit(path,"papers/learning_failures/code")[[1]]
#path <- strsplit(path,"/papers/endline_report")[[1]]
#path <- strsplit(path,"/papers/clearinghouse_training_paper")[[1]]

baseline_farmers <- read.csv(paste(path,"/baseline/data/farmer/public/baseline_farmers.csv",sep="/"), stringsAsFactors=TRUE)
midline_farmers <- read.csv(paste(path,"/midline/data/farmer/public/midline.csv",sep="/"), stringsAsFactors=TRUE)
endline_farmers <- read.csv(paste(path,"/endline/data/farmer/public/endline.csv",sep="/"), stringsAsFactors=TRUE)

baseline_farmers[, 4:98][baseline_farmers[, 4:98] == 999] <- NA
#baseline_farmers[, 4:98][baseline_farmers[, 4:98] == 96] <- NA
baseline_farmers[, 4:98][baseline_farmers[, 4:98] == 98] <- NA
baseline_farmers[, 4:98][baseline_farmers[, 4:98] == "n/a"] <- NA

midline_farmers[, 4:72][midline_farmers[, 4:72] == 999] <- NA
#midline_farmers[, 4:72][midline_farmers[, 4:72] == 96] <- NA
midline_farmers[, 4:72][midline_farmers[, 4:72] == 98] <- NA
midline_farmers[, 4:72][midline_farmers[, 4:72] == "n/a"] <- NA

endline_farmers[, 4:76][endline_farmers[, 4:76] == 999] <- NA
#endline_farmers[, 4:76][endline_farmers[, 4:76] == 96] <- NA
endline_farmers[, 4:76][endline_farmers[, 4:76] == 98] <- NA
endline_farmers[, 4:76][endline_farmers[, 4:76] == "n/a"] <- NA

#no treatment indicator for dealer training in baseline_farmers
#treatments at shop level
treatments_shop_level <- read.csv(paste(path,"/baseline/data/agro_input/public/treats_shop_level.csv", sep="/"), stringsAsFactors = TRUE)
#treatments at CA level
trainingtreatment_CA_level <- data.frame(aggregate(treatments_shop_level$training, list(treatments_shop_level$catchID), mean))
names(trainingtreatment_CA_level) <- c("catchID","training")
baseline_farmers <- merge(baseline_farmers, trainingtreatment_CA_level, by.x="catchID", by.y="catchID")


### treatment status:
baseline_farmers$treatment <- baseline_farmers$Check2.check.maize.video_shown
baseline_farmers$clearing <- baseline_farmers$Check2.check.maize.clearing

baseline_farmers$adoption_any <- baseline_farmers$Check2.check.maize.q25a
midline_farmers$mid_adoption_any <- midline_farmers$check.maize.q25a
endline_farmers$end_adoption_any <- endline_farmers$check.maize.q25a

baseline_farmers$adoption_any[baseline_farmers$adoption_any == "n/a"] <- NA
midline_farmers$mid_adoption_any[midline_farmers$mid_adoption_any == "n/a"] <- NA
endline_farmers$end_adoption_any[endline_farmers$end_adoption_any == "n/a"] <- NA



### create adoption_on_field variable for baseline
baseline_farmers$hybrid<-((baseline_farmers$Check2.check.maize.q31=="Longe_10H")|(baseline_farmers$Check2.check.maize.q31=="Longe_7H")|(baseline_farmers$Check2.check.maize.q31=="Longe_7R_Kayongo-go")|(baseline_farmers$Check2.check.maize.q31=="Bazooka")|(baseline_farmers$Check2.check.maize.q31=="Longe_6H")|(baseline_farmers$Check2.check.maize.q31=="Panner")|(baseline_farmers$Check2.check.maize.q31=="Wema")|(baseline_farmers$Check2.check.maize.q31=="KH_series"))
baseline_farmers$hybrid<-ifelse(baseline_farmers$hybrid=="TRUE",1,0)
baseline_farmers$hybrid[baseline_farmers$Check2.check.maize.q31=="Other_hybrid"] <- NA

baseline_farmers$OPV<-(baseline_farmers$Check2.check.maize.q31=="Longe_5")|(baseline_farmers$Check2.check.maize.q31=="Longe_4")
baseline_farmers$OPV<-ifelse(baseline_farmers$OPV=="TRUE",1,0)
baseline_farmers$OPV[baseline_farmers$Check2.check.maize.q31=="Other_hybrid"] <- NA

baseline_farmers$Land_Races<-(baseline_farmers$Check2.check.maize.q31=="Land_Races")
baseline_farmers$Land_Races<-ifelse(baseline_farmers$Land_Races=="TRUE",1,0)

baseline_farmers$improved<-((baseline_farmers$Check2.check.maize.q31=="Longe_10H")|(baseline_farmers$Check2.check.maize.q31=="Longe_7H")|(baseline_farmers$Check2.check.maize.q31=="Longe_7R_Kayongo-go")|(baseline_farmers$Check2.check.maize.q31=="Bazooka")|(baseline_farmers$Check2.check.maize.q31=="Longe_6H")|(baseline_farmers$Check2.check.maize.q31=="Panner")|(baseline_farmers$Check2.check.maize.q31=="Wema")|(baseline_farmers$Check2.check.maize.q31=="KH_series"|baseline_farmers$Check2.check.maize.q31=="Longe_5")|(baseline_farmers$Check2.check.maize.q31=="Longe_4")|(baseline_farmers$Check2.check.maize.q31=="Other_hybrid"))
baseline_farmers$improved<-ifelse(baseline_farmers$improved=="TRUE",1,0)

baseline_farmers$farmer_saved_seed<-((baseline_farmers$Check2.check.maize.q32=="a")|(baseline_farmers$Check2.check.maize.q32=="b"))
baseline_farmers$farmer_saved_seed<-ifelse(baseline_farmers$farmer_saved_seed=="TRUE",1,0)

baseline_farmers$Bought_from_agro_input_shop<-ifelse(baseline_farmers$Check2.check.maize.q32=="d",1,0)

baseline_farmers$hybridbutsaved <- NA
baseline_farmers$hybridbutsaved[baseline_farmers$hybrid == 1 & baseline_farmers$farmer_saved_seed == 1] <- 1
baseline_farmers$hybridbutsaved[baseline_farmers$hybrid == 1 & baseline_farmers$farmer_saved_seed == 0] <- 0
baseline_farmers$hybridbutsaved[baseline_farmers$hybrid == 0] <- 0

baseline_farmers$OPVbutsaved <- NA
baseline_farmers$OPVbutsaved[baseline_farmers$OPV == 1 & baseline_farmers$farmer_saved_seed == 1] <- 1
baseline_farmers$OPVbutsaved[baseline_farmers$OPV == 1 & baseline_farmers$farmer_saved_seed == 0] <- 0
baseline_farmers$OPVbutsaved[baseline_farmers$OPV == 0] <- 0

# baseline_farmers$fourthormore_timeused<-((baseline_farmers$Check2.check.maize.q34=="d")|(baseline_farmers$Check2.check.maize.q34=="e")|(baseline_farmers$Check2.check.maize.q34=="f"))
# baseline_farmers$fourthormore_timeused<-ifelse(baseline_farmers$fourthormore_timeused=="TRUE",1,0)
# 
# baseline_farmers$OPVbutfourthormore_timeused <- NA
# baseline_farmers$OPVbutfourthormore_timeused[baseline_farmers$OPV==1 & baseline_farmers$farmer_saved_seed==1 & baseline_farmers$fourthormore_timeused==1] <- 1
# baseline_farmers$OPVbutfourthormore_timeused[baseline_farmers$OPV==1 & baseline_farmers$farmer_saved_seed==1 & baseline_farmers$fourthormore_timeused==0] <- 0
# baseline_farmers$OPVbutfourthormore_timeused[baseline_farmers$OPV==1 & baseline_farmers$farmer_saved_seed==0] <- 0

baseline_farmers$adoption_onfield <- baseline_farmers$improved
baseline_farmers$adoption_onfield[baseline_farmers$hybridbutsaved==1] <- 0
#baseline_farmers$adoption_onfield[baseline_farmers$OPVbutfourthormore_timeused==1] <- 0
baseline_farmers$adoption_onfield[baseline_farmers$OPVbutsaved==1] <- 0

##correct spacing at baseline
baseline_farmers$Check2.check.maize.q40[baseline_farmers$Check2.check.maize.q40 == "n/a"] <- NA
baseline_farmers$correct_spacing <- baseline_farmers$Check2.check.maize.q40 == "b" | baseline_farmers$Check2.check.maize.q40 == "c"

midline_farmers$check.maize.q40[midline_farmers$check.maize.q40 == "n/a"] <- NA
midline_farmers$mid_correct_spacing <- midline_farmers$check.maize.q40 == "b" | midline_farmers$check.maize.q40 == "c"

endline_farmers$check.maize.q40[endline_farmers$check.maize.q40 == "n/a"] <- NA
endline_farmers$end_correct_spacing <- endline_farmers$check.maize.q40 == "b" | endline_farmers$check.maize.q40 == "c"

baseline_farmers$Check2.check.maize.q41[baseline_farmers$Check2.check.maize.q41 == "n/a"] <- NA
baseline_farmers$Check2.check.maize.q41[baseline_farmers$Check2.check.maize.q41 == 999] <- NA
baseline_farmers$correct_seed_rate <- baseline_farmers$Check2.check.maize.q41==1 |  baseline_farmers$Check2.check.maize.q41==2 

midline_farmers$check.maize.q41[midline_farmers$check.maize.q41== "n/a"] <- NA
midline_farmers$check.maize.q41[midline_farmers$check.maize.q41== 999] <- NA
midline_farmers$mid_correct_seed_rate <- midline_farmers$check.maize.q41==1 |  midline_farmers$check.maize.q41==2 

endline_farmers$check.maize.q41[endline_farmers$check.maize.q41== "n/a"] <- NA
endline_farmers$check.maize.q41[endline_farmers$check.maize.q41== 999] <- NA
endline_farmers$end_correct_seed_rate <- endline_farmers$check.maize.q41==1 |  endline_farmers$check.maize.q41==2 



#fertilizer use at baseline
baseline_farmers$Check2.check.maize.q42[baseline_farmers$Check2.check.maize.q42==98] <- NA
baseline_farmers$organic_use  <- baseline_farmers$Check2.check.maize.q42=="Yes"

midline_farmers$check.maize.q42[midline_farmers$check.maize.q42==98] <- NA
midline_farmers$mid_organic_use  <- midline_farmers$check.maize.q42=="Yes"

endline_farmers$check.maize.q42[endline_farmers$check.maize.q42==98] <- NA
endline_farmers$end_organic_use  <- endline_farmers$check.maize.q42=="Yes"

baseline_farmers$DAP_use <- NA
baseline_farmers$DAP_use <- baseline_farmers$Check2.check.maize.q43=="Yes"
baseline_farmers$DAP_use[baseline_farmers$Check2.check.maize.q43=="98"] <- NA

midline_farmers$mod_DAP_use <- NA
midline_farmers$mid_DAP_use <- midline_farmers$check.maize.q43=="Yes"
midline_farmers$mid_DAP_use[midline_farmers$check.maize.q43=="98"] <- NA

endline_farmers$end_DAP_use <- NA
endline_farmers$end_DAP_use <- endline_farmers$check.maize.q43=="Yes"
endline_farmers$end_DAP_use[endline_farmers$check.maize.q43=="98"] <- NA

baseline_farmers$Urea_use <- NA
baseline_farmers$Urea_use <- baseline_farmers$Check2.check.maize.q44=="Yes"
baseline_farmers$Urea_use[baseline_farmers$Check2.check.maize.q44=="98"] <- NA

midline_farmers$mid_Urea_use <- NA
midline_farmers$mid_Urea_use <- midline_farmers$check.maize.q44=="Yes"
midline_farmers$mid_Urea_use[midline_farmers$check.maize.q44=="98"] <- NA

endline_farmers$end_Urea_use <- NA
endline_farmers$end_Urea_use <- endline_farmers$check.maize.q44=="Yes"
endline_farmers$end_Urea_use[endline_farmers$check.maize.q44=="98"] <- NA

baseline_farmers$pesticide_use <- NA
baseline_farmers$pesticide_use <- baseline_farmers$Check2.check.maize.q47=="Yes"
baseline_farmers$pesticide_use[baseline_farmers$Check2.check.maize.q47=="98"] <- NA

midline_farmers$mid_pesticide_use <- NA
midline_farmers$mid_pesticide_use <- midline_farmers$check.maize.q47=="Yes"
midline_farmers$mid_pesticide_use[midline_farmers$check.maize.q47=="98"] <- NA

endline_farmers$end_pesticide_use <- NA
endline_farmers$end_pesticide_use <- endline_farmers$check.maize.q47=="Yes"
endline_farmers$end_pesticide_use[endline_farmers$check.maize.q47=="98"] <- NA

##weeding nr
baseline_farmers$Check2.check.maize.q45[baseline_farmers$Check2.check.maize.q45 == "999"] <- NA
baseline_farmers$times_weeding<-as.numeric(as.character(baseline_farmers$Check2.check.maize.q45))

midline_farmers$check.maize.q45[midline_farmers$check.maize.q45 == "999"] <- NA
midline_farmers$mid_times_weeding<-as.numeric(as.character(midline_farmers$check.maize.q45))

endline_farmers$Check2.check.maize.q45[endline_farmers$check.maize.q45 == "999"] <- NA
endline_farmers$end_times_weeding<-as.numeric(as.character(endline_farmers$check.maize.q45))
##resowing
baseline_farmers$resowing <- NA
baseline_farmers$resowing <- baseline_farmers$Check2.check.maize.q49=="Yes"
baseline_farmers$resowing[baseline_farmers$Check2.check.maize.q49=="98"] <- NA

midline_farmers$mid_resowing <- NA
midline_farmers$mid_resowing <- midline_farmers$check.maize.q49=="Yes"
midline_farmers$mid_resowing[midline_farmers$check.maize.q49=="98"] <- NA

endline_farmers$end_resowing <- NA
endline_farmers$end_resowing <- endline_farmers$check.maize.q49=="Yes"
endline_farmers$end_resowing[endline_farmers$check.maize.q49=="98"] <- NA


baseline_farmers$correctweeding <- NA
baseline_farmers$Check2.check.maize.q45 <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q45))
baseline_farmers$correctweeding <-  (baseline_farmers$Check2.check.maize.q45 >=3)


##weeding at correct time
baseline_farmers$time_weed <- NA
baseline_farmers$Check2.check.maize.q46 <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q46))
baseline_farmers$time_weed <-  (baseline_farmers$Check2.check.maize.q46 <= 20)

midline_farmers$mid_time_weed <- NA
midline_farmers$check.maize.q46 <- as.numeric(as.character(midline_farmers$check.maize.q46))
midline_farmers$mid_time_weed <-  (midline_farmers$check.maize.q46 <= 20)

endline_farmers$end_time_weed <- NA
endline_farmers$check.maize.q46 <- as.numeric(as.character(endline_farmers$check.maize.q46))
endline_farmers$end_time_weed <-  (endline_farmers$check.maize.q46 <= 20)

#planting at correct time
baseline_farmers$time_plant <- NA
baseline_farmers$Check2.check.maize.q48 <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q48))
baseline_farmers$time_plant <-  (baseline_farmers$Check2.check.maize.q48 =="2")

midline_farmers$mid_time_plant <- NA
midline_farmers$check.maize.q48 <- as.numeric(as.character(midline_farmers$check.maize.q48))
midline_farmers$mid_time_plant <-  (midline_farmers$check.maize.q48 =="2")

endline_farmers$end_time_plant <- NA
endline_farmers$check.maize.q48 <- as.numeric(as.character(endline_farmers$check.maize.q48))
endline_farmers$end_time_plant <-  (endline_farmers$check.maize.q48 =="2")

#create adoption_onfield variable for midline

#1. hybrid
midline_farmers$mid_Check2.check.maize.q31 <- midline_farmers$check.maize.q31
midline_farmers$mid_hybrid<-((midline_farmers$mid_Check2.check.maize.q31=="Longe_10H")|(midline_farmers$mid_Check2.check.maize.q31=="Longe_7H")|(midline_farmers$mid_Check2.check.maize.q31=="Longe_7R_Kayongo-go")|(midline_farmers$mid_Check2.check.maize.q31=="Bazooka")|(midline_farmers$mid_Check2.check.maize.q31=="Longe_6H")|(midline_farmers$mid_Check2.check.maize.q31=="Panner")|(midline_farmers$mid_Check2.check.maize.q31=="Wema")|(midline_farmers$mid_Check2.check.maize.q31=="KH_series"))
midline_farmers$mid_hybrid<-ifelse(midline_farmers$mid_hybrid=="TRUE",1,0)
midline_farmers$mid_hybrid[midline_farmers$mid_Check2.check.maize.q31=="Other_hybrid"] <- NA #because =Other hybrid or OPV

#2. OPV
midline_farmers$mid_OPV<-(midline_farmers$mid_Check2.check.maize.q31=="Longe_5")|(midline_farmers$mid_Check2.check.maize.q31=="Longe_4")
midline_farmers$mid_OPV<-ifelse(midline_farmers$mid_OPV=="TRUE",1,0)
midline_farmers$mid_OPV[midline_farmers$mid_Check2.check.maize.q31=="Other_hybrid"] <- NA

#5. farmer saved
midline_farmers$mid_Check2.check.maize.q32 <- midline_farmers$check.maize.q32
midline_farmers$mid_farmer_saved_seed<-((midline_farmers$mid_Check2.check.maize.q32=="a")|(midline_farmers$mid_Check2.check.maize.q32=="b"))
midline_farmers$mid_farmer_saved_seed<-ifelse(midline_farmers$mid_farmer_saved_seed=="TRUE",1,0)

#6. agro
midline_farmers$mid_Bought_from_agro_input_shop<-ifelse(midline_farmers$mid_Check2.check.maize.q32=="d",1,0)

#7. adoption
midline_farmers$mid_hybridbutsaved <- NA
midline_farmers$mid_hybridbutsaved[midline_farmers$mid_hybrid == 1 & midline_farmers$mid_farmer_saved_seed == 1] <- 1
midline_farmers$mid_hybridbutsaved[midline_farmers$mid_hybrid == 1 & midline_farmers$mid_farmer_saved_seed == 0] <- 0
midline_farmers$mid_hybridbutsaved[midline_farmers$mid_hybrid == 0] <- 0

midline_farmers$mid_OPVbutsaved <- NA
midline_farmers$mid_OPVbutsaved[midline_farmers$mid_OPV == 1 & midline_farmers$mid_farmer_saved_seed == 1] <- 1
midline_farmers$mid_OPVbutsaved[midline_farmers$mid_OPV == 1 & midline_farmers$mid_farmer_saved_seed == 0] <- 0
midline_farmers$mid_OPVbutsaved[midline_farmers$mid_OPV == 0] <- 0

# midline_farmers$mid_Check2.check.maize.q34 <- midline_farmers$check.maize.q34
# midline_farmers$mid_fourthormore_timeused<-((midline_farmers$mid_Check2.check.maize.q34=="d")|(midline_farmers$mid_Check2.check.maize.q34=="e")|(midline_farmers$mid_Check2.check.maize.q34=="f"))
# midline_farmers$mid_fourthormore_timeused<-ifelse(midline_farmers$mid_fourthormore_timeused=="TRUE",1,0)
# 
# midline_farmers$mid_OPVbutfourthormore_timeused <- NA
# midline_farmers$mid_OPVbutfourthormore_timeused[midline_farmers$mid_OPV==1 & midline_farmers$mid_farmer_saved_seed==1 & midline_farmers$mid_fourthormore_timeused==1] <- 1
# midline_farmers$mid_OPVbutfourthormore_timeused[midline_farmers$mid_OPV==1 & midline_farmers$mid_farmer_saved_seed==1 & midline_farmers$mid_fourthormore_timeused==0] <- 0
# midline_farmers$mid_OPVbutfourthormore_timeused[midline_farmers$mid_OPV==1 & midline_farmers$mid_farmer_saved_seed==0] <- 0
# midline_farmers$mid_OPVbutfourthormore_timeused[midline_farmers$mid_OPV == 0] <- 0

midline_farmers$mid_improved<-((midline_farmers$mid_Check2.check.maize.q31=="Longe_10H")|(midline_farmers$mid_Check2.check.maize.q31=="Longe_7H")|(midline_farmers$mid_Check2.check.maize.q31=="Longe_7R_Kayongo-go")|(midline_farmers$mid_Check2.check.maize.q31=="Bazooka")|(midline_farmers$mid_Check2.check.maize.q31=="Longe_6H")|(midline_farmers$mid_Check2.check.maize.q31=="Panner")|(midline_farmers$mid_Check2.check.maize.q31=="Wema")|(midline_farmers$mid_Check2.check.maize.q31=="KH_series"|midline_farmers$mid_Check2.check.maize.q31=="Longe_5")|(midline_farmers$mid_Check2.check.maize.q31=="Longe_4")|(midline_farmers$mid_Check2.check.maize.q31=="Other_hybrid"))
midline_farmers$mid_improved<-ifelse(midline_farmers$mid_improved=="TRUE",1,0)

midline_farmers$mid_adoption_onfield <- midline_farmers$mid_improved
midline_farmers$mid_adoption_onfield[midline_farmers$mid_hybridbutsaved==1] <- 0
#midline_farmers$mid_adoption_onfield[midline_farmers$mid_OPVbutfourthormore_timeused==1] <- 0
midline_farmers$mid_adoption_onfield[midline_farmers$mid_OPVbutsaved==1] <- 0

#fertilizer use at midline
midline_farmers$mid_DAP_use <- NA
midline_farmers$mid_DAP_use <- midline_farmers$check.maize.q43=="Yes"
midline_farmers$mid_DAP_use[midline_farmers$check.maize.q43=="98"] <- NA
midline_farmers$mid_DAP_use[midline_farmers$check.maize.q43=="n/a"] <- NA

midline_farmers$check.maize.q45[midline_farmers$check.maize.q45 == "n/a"] <- NA
midline_farmers$check.maize.q45[midline_farmers$check.maize.q45 == "999"] <- NA
midline_farmers$check.maize.q45 <- as.numeric(as.character(midline_farmers$check.maize.q45))
midline_farmers$mid_correctweeding<-((midline_farmers$check.maize.q45>=3))
midline_farmers$mid_correctweeding<-ifelse(midline_farmers$mid_correctweeding=="TRUE",1,0)

#create adoption on field variable for endline
#1. hybrid
endline_farmers$end_Check2.check.maize.q31 <- endline_farmers$check.maize.q31
endline_farmers$end_hybrid<-((endline_farmers$end_Check2.check.maize.q31=="Longe_10H")|(endline_farmers$end_Check2.check.maize.q31=="Longe_7H")|(endline_farmers$end_Check2.check.maize.q31=="Longe_7R_Kayongo-go")|(endline_farmers$end_Check2.check.maize.q31=="Bazooka")|(endline_farmers$end_Check2.check.maize.q31=="Longe_6H")|(endline_farmers$end_Check2.check.maize.q31=="Panner")|(endline_farmers$end_Check2.check.maize.q31=="Wema")|(endline_farmers$end_Check2.check.maize.q31=="KH_series"))
endline_farmers$end_hybrid<-ifelse(endline_farmers$end_hybrid=="TRUE",1,0)
endline_farmers$end_hybrid[endline_farmers$end_Check2.check.maize.q31=="Other_hybrid"] <- NA #because =Other hybrid or OPV

#2. OPV
endline_farmers$end_OPV<-(endline_farmers$end_Check2.check.maize.q31=="Longe_5")|(endline_farmers$end_Check2.check.maize.q31=="Longe_4")
endline_farmers$end_OPV<-ifelse(endline_farmers$end_OPV=="TRUE",1,0)
endline_farmers$end_OPV[endline_farmers$end_Check2.check.maize.q31=="Other_hybrid"] <- NA

#5. farmer saved
endline_farmers$end_Check2.check.maize.q32 <- endline_farmers$check.maize.q32
endline_farmers$end_farmer_saved_seed<-((endline_farmers$end_Check2.check.maize.q32=="a")|(endline_farmers$end_Check2.check.maize.q32=="b"))
endline_farmers$end_farmer_saved_seed<-ifelse(endline_farmers$end_farmer_saved_seed=="TRUE",1,0)

#6. agro
endline_farmers$end_Bought_from_agro_input_shop<-ifelse(endline_farmers$end_Check2.check.maize.q32=="d",1,0)

#7. adoption
endline_farmers$end_hybridbutsaved <- NA
endline_farmers$end_hybridbutsaved[endline_farmers$end_hybrid == 1 & endline_farmers$end_farmer_saved_seed == 1] <- 1
endline_farmers$end_hybridbutsaved[endline_farmers$end_hybrid == 1 & endline_farmers$end_farmer_saved_seed == 0] <- 0
endline_farmers$end_hybridbutsaved[endline_farmers$end_hybrid == 0] <- 0

endline_farmers$end_OPVbutsaved <- NA
endline_farmers$end_OPVbutsaved[endline_farmers$end_OPV == 1 & endline_farmers$end_farmer_saved_seed == 1] <- 1
endline_farmers$end_OPVbutsaved[endline_farmers$end_OPV == 1 & endline_farmers$end_farmer_saved_seed == 0] <- 0
endline_farmers$end_OPVbutsaved[endline_farmers$end_OPV == 0] <- 0

# endline_farmers$end_Check2.check.maize.q34 <- endline_farmers$check.maize.q34
# endline_farmers$end_fourthormore_timeused<-((endline_farmers$end_Check2.check.maize.q34=="d")|(endline_farmers$end_Check2.check.maize.q34=="e")|(endline_farmers$end_Check2.check.maize.q34=="f"))
# endline_farmers$end_fourthormore_timeused<-ifelse(endline_farmers$end_fourthormore_timeused=="TRUE",1,0)
# 
# endline_farmers$end_OPVbutfourthormore_timeused <- NA
# endline_farmers$end_OPVbutfourthormore_timeused[endline_farmers$end_OPV==1 & endline_farmers$end_farmer_saved_seed==1 & endline_farmers$end_fourthormore_timeused==1] <- 1
# endline_farmers$end_OPVbutfourthormore_timeused[endline_farmers$end_OPV==1 & endline_farmers$end_farmer_saved_seed==1 & endline_farmers$end_fourthormore_timeused==0] <- 0
# endline_farmers$end_OPVbutfourthormore_timeused[endline_farmers$end_OPV==1 & endline_farmers$end_farmer_saved_seed==0] <- 0
# endline_farmers$end_OPVbutfourthormore_timeused[endline_farmers$end_OPV == 0] <- 0

endline_farmers$end_improved<-((endline_farmers$end_Check2.check.maize.q31=="Longe_10H")|(endline_farmers$end_Check2.check.maize.q31=="Longe_7H")|(endline_farmers$end_Check2.check.maize.q31=="Longe_7R_Kayongo-go")|(endline_farmers$end_Check2.check.maize.q31=="Bazooka")|(endline_farmers$end_Check2.check.maize.q31=="Longe_6H")|(endline_farmers$end_Check2.check.maize.q31=="Panner")|(endline_farmers$end_Check2.check.maize.q31=="Wema")|(endline_farmers$end_Check2.check.maize.q31=="KH_series"|endline_farmers$end_Check2.check.maize.q31=="Longe_5")|(endline_farmers$end_Check2.check.maize.q31=="Longe_4")|(endline_farmers$end_Check2.check.maize.q31=="Other_hybrid"))
endline_farmers$end_improved<-ifelse(endline_farmers$end_improved=="TRUE",1,0)

endline_farmers$end_adoption_onfield <- endline_farmers$end_improved
endline_farmers$end_adoption_onfield[endline_farmers$end_hybridbutsaved==1] <- 0
#endline_farmers$end_adoption_onfield[endline_farmers$end_OPVbutfourthormore_timeused==1] <- 0
endline_farmers$end_adoption_onfield[endline_farmers$end_OPVbutsaved==1] <- 0

## farmer saved

baseline_farmers$farmer_saved_seed<- (baseline_farmers$Check2.check.maize.q32=="a")
baseline_farmers$farmer_saved_seed<-ifelse(baseline_farmers$farmer_saved_seed=="TRUE",1,0)

midline_farmers$mid_farmer_saved_seed<- (midline_farmers$check.maize.q32=="a")
midline_farmers$mid_farmer_saved_seed<-ifelse(midline_farmers$mid_farmer_saved_seed=="TRUE",1,0)

endline_farmers$end_farmer_saved_seed<- (endline_farmers$check.maize.q32=="a")
endline_farmers$end_farmer_saved_seed<-ifelse(endline_farmers$end_farmer_saved_seed=="TRUE",1,0)
### bought at agro
baseline_farmers$Bought_from_agro_input_shop<-ifelse(baseline_farmers$Check2.check.maize.q32=="d",1,0)
midline_farmers$mid_Bought_from_agro_input_shop<-ifelse(midline_farmers$check.maize.q32=="d",1,0)
endline_farmers$end_Bought_from_agro_input_shop<-ifelse(endline_farmers$check.maize.q32=="d",1,0)


#fertilizer use at midline
endline_farmers$end_DAP_use <- NA
endline_farmers$end_DAP_use <- endline_farmers$check.maize.q43=="Yes"
endline_farmers$end_DAP_use[endline_farmers$check.maize.q43=="98"] <- NA
endline_farmers$end_DAP_use[endline_farmers$check.maize.q43=="n/a"] <- NA

endline_farmers$check.maize.q45[endline_farmers$check.maize.q45 == "n/a"] <- NA
endline_farmers$check.maize.q45[endline_farmers$check.maize.q45 == "999"] <- NA
endline_farmers$check.maize.q45 <- as.numeric(as.character(endline_farmers$check.maize.q45))
endline_farmers$end_correctweeding<-((endline_farmers$check.maize.q45>=3))
endline_farmers$end_correctweeding<-ifelse(endline_farmers$end_correctweeding=="TRUE",1,0)

endline_farmers$end_pesticide_use <- NA
endline_farmers$end_pesticide_use <- endline_farmers$check.maize.q47=="Yes"
endline_farmers$end_pesticide_use[endline_farmers$check.maize.q47=="98"] <- NA

endline_farmers$check.maize.q58[endline_farmers$check.maize.q58=="n/a"] <- NA
endline_farmers$q58_correct <- ifelse(endline_farmers$check.maize.q58 %in% c("b","c"),1,0)
##only correct answer
endline_farmers$q58_correct <- ifelse(endline_farmers$check.maize.q58 %in% c("b"),1,0)

endline_farmers$check.maize.q59[endline_farmers$check.maize.q59=="n/a"] <- NA
endline_farmers$q59_correct <- ifelse(endline_farmers$check.maize.q59=="c",1,0)

endline_farmers$check.maize.q60[endline_farmers$check.maize.q60=="n/a"] <- NA
endline_farmers$q60_correct <- ifelse(endline_farmers$check.maize.q60%in%c("c","d"),1,0)
### only correct answer
endline_farmers$q60_correct <- ifelse(endline_farmers$check.maize.q60%in%c("c"),1,0)

endline_farmers$check.maize.q61[endline_farmers$check.maize.q61=="n/a"] <- NA
endline_farmers$q61_correct <- ifelse(endline_farmers$check.maize.q61=="c",1,0)

endline_farmers$check.maize.q62[endline_farmers$check.maize.q62=="n/a"] <- NA
endline_farmers$q62_correct <- ifelse(endline_farmers$check.maize.q62=="c",1,0)


endline_farmers$check.maize.q63[endline_farmers$check.maize.q63=="n/a"] <- NA
endline_farmers$q63_correct <- ifelse(endline_farmers$check.maize.q63=="a",1,0)
endline_farmers$q63_correct[endline_farmers$check.maize.q63=="c"] <- 1 #Roberts comment


#now make a panel

all <- merge(merge(baseline_farmers[c("farmer_ID","shop_ID","treatment","adoption_any","adoption_onfield","correct_spacing","correct_seed_rate","organic_use","DAP_use","Urea_use","correctweeding","times_weeding","pesticide_use","resowing")],midline_farmers[c("farmer_ID","mid_adoption_any","mid_adoption_onfield","mid_DAP_use", "mid_correctweeding")]),endline_farmers[c("farmer_ID","end_adoption_any","end_adoption_onfield","end_DAP_use","end_correctweeding","end_pesticide_use","q58_correct","q59_correct","q60_correct","q61_correct")])
##always adopters
mean(all$adoption_onfield==1 & all$mid_adoption_onfield==1 & all$end_adoption_onfield==1, na.rm=T)
#never adopters
mean(all$adoption_onfield==0 & all$mid_adoption_onfield==0 & all$end_adoption_onfield==0, na.rm=T)
#disadopters
mean((all$adoption_onfield==1 & all$mid_adoption_onfield==1 & all$end_adoption_onfield==0) | (all$adoption_onfield==1 & all$mid_adoption_onfield==0 & all$end_adoption_onfield==0), na.rm=T)
#adopters
mean((all$adoption_onfield==0 & all$mid_adoption_onfield==0 & all$end_adoption_onfield==1) | (all$adoption_onfield==0 & all$mid_adoption_onfield==1 & all$end_adoption_onfield==1), na.rm=T)
#adopt/disadopt
mean((all$adoption_onfield==0 & all$mid_adoption_onfield==1 & all$end_adoption_onfield==0) | (all$adoption_onfield==1 & all$mid_adoption_onfield==0 & all$end_adoption_onfield==1), na.rm=T)

#transition matrix

trans_mat_1_2 <- merge(baseline_farmers[c("farmer_ID","adoption_onfield")], midline_farmers[c("farmer_ID","mid_adoption_onfield")], by="farmer_ID")
trans_mat_2_3 <- merge(midline_farmers[c("farmer_ID","mid_adoption_onfield")], endline_farmers[c("farmer_ID","end_adoption_onfield")], by="farmer_ID")

names(trans_mat_1_2) <- c("farmer_ID","adoption_onfield_t_1","adoption_onfield_t")
names(trans_mat_2_3) <- c("farmer_ID","adoption_onfield_t_1","adoption_onfield_t")
trans_mat <- rbind(trans_mat_1_2,trans_mat_2_3)

prop.table(table(trans_mat$adoption_onfield_t==1 & trans_mat$adoption_onfield_t_1==1))

prop.table(table(trans_mat$adoption_onfield_t==0 & trans_mat$adoption_onfield_t_1==0))
prop.table(table(trans_mat$adoption_onfield_t==1 & trans_mat$adoption_onfield_t_1==0))
#disadopt
prop.table(table(trans_mat$adoption_onfield_t==0 & trans_mat$adoption_onfield_t_1==1))
# some farmers who adopt at baseline disadopt after notc

# in control group, do we see that farmers that disadopt use less fertilizer than farmers who consistently  adopt?

##start by doing this on the panel

all$disadopter <- (all$adoption_onfield==1 & all$mid_adoption_onfield==1 & all$end_adoption_onfield==0) | (all$adoption_onfield==1 & all$mid_adoption_onfield==0 & all$end_adoption_onfield==0)
all$always_adopter <- (all$adoption_onfield==1 & all$mid_adoption_onfield==1 & all$end_adoption_onfield==1)
all_nt <- subset(all, treatment == FALSE & (disadopter | always_adopter))

#hypothesis 1
summary(lm(correct_spacing~disadopter, all_nt))
summary(lm(correct_seed_rate~disadopter, all_nt))
summary(lm(organic_use~disadopter, all_nt))
summary(lm(DAP_use~disadopter, all_nt))
summary(lm(Urea_use~disadopter, all_nt))
summary(lm(times_weeding~disadopter, all_nt))
summary(lm(pesticide_use~disadopter, all_nt))
summary(lm(resowing~disadopter, all_nt))

##hypothesis 2
### balance
ols <- lm(adoption_onfield~treatment,all)
vcov_cluster_shop <- vcovCR(ols,cluster=all$shop_ID,type="CR0")
coef_test(ols, vcov_cluster_shop)

### original regression
ols <- lm(mid_adoption_onfield~treatment,all)
vcov_cluster_shop <- vcovCR(ols,cluster=all$shop_ID,type="CR0")
coef_test(ols, vcov_cluster_shop)
### key regression
ols <- lm(mid_adoption_onfield~treatment,all[all$adoption_onfield==1,])
vcov_cluster_shop <- vcovCR(ols,cluster=all[all$adoption_onfield==1,]$shop_ID,type="CR0")
coef_test(ols, vcov_cluster_shop)
### does this also work for the second season?
ols <- lm(end_adoption_onfield~treatment,all[all$adoption_onfield==0 & all$mid_adoption_onfield==1,])
vcov_cluster_shop <- vcovCR(ols,cluster=all[all$adoption_onfield==0 & all$mid_adoption_onfield==1,]$shop_ID,type="CR0")
coef_test(ols, vcov_cluster_shop)
### effect is not there for always adopters (they know this already)
ols <- lm(end_adoption_onfield~treatment,all[all$adoption_onfield==1 & all$mid_adoption_onfield==1,])
vcov_cluster_shop <- vcovCR(ols,cluster=all[all$adoption_onfield==1 & all$mid_adoption_onfield==1,]$shop_ID,type="CR0")
coef_test(ols, vcov_cluster_shop)


## hypothesis 3
ols <- lm(mid_adoption_onfield~treatment,all[all$adoption_onfield==0,])
vcov_cluster_shop <- vcovCR(ols,cluster=all[all$adoption_onfield==0,]$shop_ID,type="CR0")
coef_test(ols, vcov_cluster_shop)
###hypothesis 4
summary(lm(end_DAP_use~treatment, all[all$adoption_onfield==1 & all$mid_adoption_onfield==0 & all$end_adoption_onfield==1,]))
summary(lm(end_pesticide_use~treatment, all[all$adoption_onfield==1 & all$mid_adoption_onfield==0 & all$end_adoption_onfield==1,]))


#keep control group
t1 <-table(all$mid_DAP_use[all$mid_adoption_onfield==1 & all$end_adoption_onfield==0 & all$treatment==FALSE])
t2 <- table(all$mid_DAP_use[all$mid_adoption_onfield==1 & all$end_adoption_onfield==1  & all$treatment==FALSE])
prop.test(x=c(t1[2],t2[2]), n=c(t1[1]+t1[2],t2[1]+t2[2]))

t1 <-table(all$DAP_use[all$adoption_onfield==1 & all$mid_adoption_onfield==0  & all$treatment==FALSE])
t2 <-table(all$DAP_use[all$adoption_onfield==1 & all$mid_adoption_onfield==1 & all$treatment==FALSE])
prop.test(x=c(t1[2],t2[2]), n=c(t1[1]+t1[2],t2[1]+t2[2]))

t1 <- table(all$mid_correctweeding[all$mid_adoption_onfield==1 & all$end_adoption_onfield==0 & all$treatment==FALSE])
t2 <- table(all$mid_correctweeding[all$mid_adoption_onfield==1 & all$end_adoption_onfield==1  & all$treatment==FALSE])
prop.test(x=c(t1[2],t2[2]), n=c(t1[1]+t1[2],t2[1]+t2[2]))

t1 <-table(all$correctweeding[all$adoption_onfield==1 & all$mid_adoption_onfield==0  & all$treatment==FALSE])
t2 <-table(all$correctweeding[all$adoption_onfield==1 & all$mid_adoption_onfield==1 & all$treatment==FALSE])
prop.test(x=c(t1[2],t2[2]), n=c(t1[1]+t1[2],t2[1]+t2[2]))

# Farmers that were treated that use seed at baseline are more likely to disadopt 
summary(lm(mid_adoption_onfield~treatment, data=all[all$adoption_onfield==1,]))
# farmers that use inputs at baseline are less likely to disadrop
summary(lm(mid_adoption_onfield~treatment*DAP_use, data=all[all$adoption_onfield==1,]))
summary(lm(mid_adoption_onfield~treatment*correctweeding, data=all[all$adoption_onfield==1,]))

#ugh, seems to be the opposite, disadoption as a response to treatment is actually higher among farmers that use inputs

#3. Farmers that were treated that do not adopt at baseline are more likely to adopt at midline than control farmers
summary(lm(mid_adoption_onfield~treatment, data=all[all$adoption_onfield==0,]))

#4. Treated farmers that disadopt at midline are more likely to adopt at endline (suggestive as treatment may have affected comparison group)
summary(lm(end_adoption_onfield~treatment, data=all[all$mid_adoption_onfield==0,]))

#5. Treated farmers that adopt at baseline, disadopt at midline and adopt at endline use more inputs at endline than at baseline.
t1 <- table(all[all$adoption_onfield==1 & all$mid_adoption_onfield==0 & all$end_adoption_onfield==1 & all$treatment==TRUE,"DAP_use"])
t2 <- table(all[all$adoption_onfield==1 & all$mid_adoption_onfield==0 & all$end_adoption_onfield==1 & all$treatment==TRUE,"end_DAP_use"])
prop.test(x=c(t1[2],t2[2]), n=c(t1[1]+t1[2],t2[1]+t2[2]))

t1 <- table(all[all$adoption_onfield==1 & all$mid_adoption_onfield==0 & all$end_adoption_onfield==1 & all$treatment==TRUE,"correctweeding"])
t2 <- table(all[all$adoption_onfield==1 & all$mid_adoption_onfield==0 & all$end_adoption_onfield==1 & all$treatment==TRUE,"end_correctweeding"])
prop.test(x=c(t1[2],t2[2]), n=c(t1[1]+t1[2],t2[1]+t2[2]))

#6. Treated farmers that adopt at baseline, disadopt at midline and adopt at endline have more knowledge than control (suggestive as treatment may have affected comparison group)
summary(lm(q58_correct~treatment, data=all[all$adoption_onfield==1 & all$mid_adoption_onfield==0 & all$end_adoption_onfield==1,]))

summary(lm(q58_correct~treatment, data=all))

summary(lm(q59_correct~treatment, data=all[all$adoption_onfield==1 & all$mid_adoption_onfield==0 & all$end_adoption_onfield==1,]))
summary(lm(q59_correct~treatment, data=all[all$adoption_onfield==1 ,]))

summary(lm(q59_correct~treatment, data=all))

summary(lm(q60_correct~treatment, data=all[all$adoption_onfield==1 & all$mid_adoption_onfield==0 & all$end_adoption_onfield==1,]))
summary(lm(q60_correct~treatment, data=all[all$adoption_onfield==1 ,]))
summary(lm(q60_correct~treatment, data=all))

summary(lm(q61_correct~treatment, data=all[all$adoption_onfield==1 & all$mid_adoption_onfield==0 & all$end_adoption_onfield==1,]))
summary(lm(q61_correct~treatment, data=all[all$adoption_onfield==1 ,]))
summary(lm(q61_correct~treatment, data=all))

###answer all questions again but now using "on any field"


mean(all$adoption_any=="Yes" & all$mid_adoption_any=="Yes" & all$end_adoption_any=="Yes", na.rm=T)
mean(all$adoption_any=="No" & all$mid_adoption_any=="No" & all$end_adoption_any=="No", na.rm=T)
mean((all$adoption_any=="Yes" & all$mid_adoption_any=="Yes" & all$end_adoption_any=="No") | (all$adoption_any=="Yes" & all$mid_adoption_any=="No" & all$end_adoption_any=="No"), na.rm=T)
mean((all$adoption_any=="No" & all$mid_adoption_any=="Yes" & all$end_adoption_any=="Yes") | (all$adoption_any=="No" & all$mid_adoption_any=="No" & all$end_adoption_any=="Yes"), na.rm=T)
mean((all$adoption_any=="No" & all$mid_adoption_any=="Yes" & all$end_adoption_any=="No") | (all$adoption_any=="Yes" & all$mid_adoption_any=="No" & all$end_adoption_any=="Yes"), na.rm=T)

# Farmers that were treated that use seed at baseline are more likely to disadopt 
summary(lm((mid_adoption_any=="Yes")~treatment, data=all[all$adoption_any=="Yes",]))
# farmers that use inputs at baseline are less likely to disadrop
summary(lm((mid_adoption_any=="Yes")~treatment*DAP_use, data=all[all$adoption_any=="Yes",]))
summary(lm((mid_adoption_any=="Yes")~treatment*correctweeding, data=all[all$adoption_any=="Yes",]))

###Caro's code to create figure by treatment group:

# all=subset(all,treatment==FALSE)
# 
# midline_farmers <- merge(midline_farmers
#                          ,baseline_farmers[,c("farmer_ID","Check2.check.maize.video_shown")]
#                          ,by="farmer_ID")
# 
# endline_farmers <- merge(endline_farmers
#                          ,baseline_farmers[,c("farmer_ID","Check2.check.maize.video_shown")]
#                          ,by="farmer_ID")
# 
# baseline_farmers=subset(baseline_farmers,Check2.check.maize.video_shown==FALSE)
# 
# midline_farmers=subset(midline_farmers,Check2.check.maize.video_shown==FALSE)
# 
# endline_farmers=subset(endline_farmers,Check2.check.maize.video_shown==FALSE)

###########################alluvial graphs########################################
library(easyalluvial)
library(tidyverse)

stack_1 <- baseline_farmers[c("farmer_ID","adoption_onfield")]
stack_1$round <- "baseline" 
stack_2 <- midline_farmers[c("farmer_ID","mid_adoption_onfield")]
stack_2$round <- "midline" 
names(stack_2) <- c("farmer_ID","adoption_onfield","round")
stack_3 <- endline_farmers[c("farmer_ID","end_adoption_onfield")]
stack_3$round <- "endline" 
names(stack_3) <- c("farmer_ID","adoption_onfield","round")

alluvial_frame <- rbind(stack_1, stack_2, stack_3)
alluvial_frame <- subset(alluvial_frame, !is.na(alluvial_frame$adoption_onfield))

### make panel ba

to_drop <- names(table(alluvial_frame$farmer_ID))[table(alluvial_frame$farmer_ID)!=3]
alluvial_frame <- subset(alluvial_frame, !(farmer_ID %in% to_drop))

alluvial_frame$farmer_ID <- as.character(alluvial_frame$farmer_ID)
col_vector = c('green', 'red')
alluvial_frame$round[alluvial_frame$round == "baseline"] <- "survey 1"
alluvial_frame$round[alluvial_frame$round == "midline"] <- "survey 2"
alluvial_frame$round[alluvial_frame$round == "endline"] <- "survey 3"
alluvial_long(alluvial_frame
              , key = round
              , value = adoption_onfield
              , id = farmer_ID 
              , order_levels_key =c("survey 1","survey 2","survey 3")
              ,bins = 2
              ,bin_labels = c("nonadoption","adoption")
              , fill_by = 'value'
              , col_vector_flow = col_vector
              , col_vector_value = col_vector
) +theme(axis.text=element_text(size=18),
         axis.title=element_text(size=14,face="bold"))

prop.table(table(alluvial_frame$adoption_onfield[alluvial_frame$round=='baseline']))
prop.table(table(alluvial_frame$adoption_onfield[alluvial_frame$round=='midline']))
prop.table(table(alluvial_frame$adoption_onfield[alluvial_frame$round=='endline']))

prop.table(table(all$adoption_onfield ==1))
prop.table(table(all$adoption_onfield ==1 & all$mid_adoption_onfield==0))
prop.table(table(all$adoption_onfield ==1 & all$mid_adoption_onfield==1  & all$end_adoption_onfield==0))

prop.table(table(all$adoption_onfield ==0))
prop.table(table(all$adoption_onfield ==0 & all$mid_adoption_onfield==1))
prop.table(table(all$adoption_onfield ==0 & all$mid_adoption_onfield==0  & all$end_adoption_onfield==1))

prop.table(table(all$adoption_onfield ==1 & all$mid_adoption_onfield==0  & all$end_adoption_onfield==1))

prop.table(table(all$adoption_onfield ==0 & all$mid_adoption_onfield==1  & all$end_adoption_onfield==0))

##prepare first variable - adoption at household level

baseline_farmers$agro <- ifelse(baseline_farmers$Check2.check.maize.q25b=="d",1,0)
baseline_farmers$agro[is.na(baseline_farmers$Check2.check.maize.q25b)] <- NA
baseline_farmers$agro[baseline_farmers$Check2.check.maize.q25a==0] = 0

midline_farmers$mid_agro <- ifelse(midline_farmers$check.maize.q25b=="d",1,0)
midline_farmers$mid_agro[is.na(midline_farmers$check.maize.q25b)] <- NA
midline_farmers$mid_agro[midline_farmers$check.maize.q25a=="No"] = 0

endline_farmers$end_agro <- ifelse(endline_farmers$check.maize.q25b=="d",1,0)
endline_farmers$end_agro[is.na(endline_farmers$check.maize.q25b)] <- NA
endline_farmers$end_agro[endline_farmers$check.maize.q25a=="No"] = 0

baseline_farmers$Land_Races<-(baseline_farmers$Check2.check.maize.q31=="Land_Races")
baseline_farmers$Land_Races<-ifelse(baseline_farmers$Land_Races=="TRUE",1,0)

midline_farmers$mid_Land_Races<-(midline_farmers$check.maize.q31=="Land_Races")
midline_farmers$mid_Land_Races<-ifelse(midline_farmers$mid_Land_Races=="TRUE",1,0)

endline_farmers$end_Land_Races<-(endline_farmers$check.maize.q31=="Land_Races")
endline_farmers$end_Land_Races<-ifelse(endline_farmers$end_Land_Races=="TRUE",1,0)

baseline_farmers$farmer_saved_seed<-((baseline_farmers$Check2.check.maize.q32=="a")|(baseline_farmers$Check2.check.maize.q32=="b"))
baseline_farmers$farmer_saved_seed<-ifelse(baseline_farmers$farmer_saved_seed=="TRUE",1,0)

midline_farmers$mid_farmer_saved_seed<-((midline_farmers$check.maize.q32=="a")|(midline_farmers$check.maize.q32=="b"))
midline_farmers$mid_farmer_saved_seed<-ifelse(midline_farmers$mid_farmer_saved_seed=="TRUE",1,0)

endline_farmers$end_farmer_saved_seed<-((endline_farmers$check.maize.q32=="a")|(endline_farmers$check.maize.q32=="b"))
endline_farmers$end_farmer_saved_seed<-ifelse(endline_farmers$end_farmer_saved_seed=="TRUE",1,0)


baseline_farmers$Bought_from_agro_input_shop<-ifelse(baseline_farmers$Check2.check.maize.q32=="d",1,0)

#make panel

#here2
all <- merge(merge(baseline_farmers[c("farmer_ID","shop_ID","Check2.check.maize.q18","Check2.check.maize.q20","Check2.check.maize.q21","correct_spacing", "correct_seed_rate", "organic_use", "DAP_use", "Urea_use", "times_weeding", "pesticide_use", "resowing","time_plant","time_weed","treatment","clearing","training","adoption_onfield", "Land_Races","farmer_saved_seed","Bought_from_agro_input_shop","Check2.check.maize.q9","Check2.check.maize.q57")],midline_farmers[c("farmer_ID","mid_adoption_onfield","mid_Land_Races","mid_farmer_saved_seed","mid_Bought_from_agro_input_shop")]),endline_farmers[c("farmer_ID","end_adoption_onfield","end_Land_Races", "end_farmer_saved_seed","end_Bought_from_agro_input_shop")])

##always adopters
##demean orthogonal treatments
all$clearing <- all$clearing - mean(all$clearing)
all$training <- all$training - mean(all$training)
##  collect results
xmat_base <- cbind(all$adoption_onfield,all$farmer_saved_seed,all$Bought_from_agro_input_shop)
index_base <- icwIndex(xmat=xmat_base,revcols = c(2))
all$index_base <- index_base$index


mean_base <-  array(NA,dim=c(4,2,5))
#loop here over outcomes
outcomes <- c("adoption_onfield","Land_Races","farmer_saved_seed","Bought_from_agro_input_shop","index_base" )
for (i in 1:length(outcomes)) {
  mean_base[1,1,i] <- mean(unlist(all[outcomes[i]]), na.rm=TRUE)
  mean_base[2,1,i] <- sd(unlist(all[outcomes[i]]), na.rm=TRUE)
  ols <- lm(as.formula(paste(outcomes[i],"treatment*clearing*training", sep="~")),data=all)
  
  vcov_cluster <- vcovCR(ols,cluster=all$shop_ID,type="CR0")
  coef_test(ols, vcov_cluster)
  mean_base[1,2,i] <- coef_test(ols, vcov_cluster)$beta[2]
  mean_base[2,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  mean_base[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  mean_base[4,2,i] <- nobs(ols)
}


save(mean_base, file=paste(path,"papers/learning_failures/code/output/mean_base.Rdata",sep="/")) 

xmat_mid <- cbind(all$mid_adoption_onfield,all$mid_farmer_saved_seed,all$mid_Bought_from_agro_input_shop)
index_mid <- icwIndex(xmat=xmat_mid,revcols = c(2))
all$index_mid <- index_mid$index

### instead of conditioning on baseline adoption, try to condition on baseline 
all$Check2.check.maize.q57[all$Check2.check.maize.q57>99] <- NA
ll <- quantile(all[all$adoption_onfield==1,]$Check2.check.maize.q57, c(.33,.66),na.rm=T)[1]
ul <- quantile(all[all$adoption_onfield==1,]$Check2.check.maize.q57, c(.33,.66),na.rm=T)[2]

all$overest <- all$Check2.check.maize.q57>=ul
all$underest <- all$Check2.check.maize.q57<ll
all$corest <- all$Check2.check.maize.q57>=ll & all$Check2.check.maize.q57<ul


mid_adoption <-  array(NA,dim=c(4,3,5)) #here
#loop here over outcomes c("mid_adoption_onfield","mid_Land_Races","mid_Bought_from_agro_input_shop")
outcomes <- c("mid_adoption_onfield","mid_Land_Races","mid_farmer_saved_seed", "mid_Bought_from_agro_input_shop","index_mid")
outcomes_base <- c("adoption_onfield","Land_Races","farmer_saved_seed","Bought_from_agro_input_shop","index_base" )

#"correct_spacing", "correct_seed_rate", "organic_use", "DAP_use", "Urea_use", "times_weeding", "pesticide_use", "resowing","time_plant","time_weed"
xmat_base <- cbind(all$correct_spacing, all$correct_seed_rate, all$organic_use, all$DAP_use, all$Urea_use, all$times_weeding, all$pesticide_use, all$resowing, all$time_plant, all$time_weed)
index_base <- icwIndex(xmat=xmat_base)
all$compl_index <- index_base$index

all$selector_2 <- all$overest == TRUE
all$selector_1 <- all$compl_index < 0

for (i in 1:length(outcomes)) {
 
ols <- lm(as.formula(paste(outcomes[i],paste("treatment*clearing*training",outcomes_base[i],sep="+"), sep="~")),data=all)

vcov_cluster <- vcovCR(ols,cluster=all$shop_ID,type="CR0")
coef_test(ols, vcov_cluster)
mid_adoption[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
mid_adoption[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
mid_adoption[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
mid_adoption[4,1,i] <- nobs(ols)

ols <- lm(as.formula(paste(outcomes[i],paste("treatment*clearing*training",outcomes_base[i],sep="+"), sep="~")),data=all[all$selector_1==1,])
vcov_cluster <- vcovCR(ols,cluster=all[all$selector_1==1,]$shop_ID,type="CR0")
coef_test(ols, vcov_cluster)
mid_adoption[1,2,i] <- coef_test(ols, vcov_cluster)$beta[2]
mid_adoption[2,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
mid_adoption[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
mid_adoption[4,2,i] <- nobs(ols)

ols <- lm(as.formula(paste(outcomes[i],paste("treatment*clearing*training",outcomes_base[i],sep="+"), sep="~")),data=all[all$selector_1==1 & all$selector_2==1,])
vcov_cluster <- vcovCR(ols,cluster=all[all$selector_1==1 & all$selector_2==1 ,]$shop_ID,type="CR0")
coef_test(ols, vcov_cluster)
mid_adoption[1,3,i] <- coef_test(ols, vcov_cluster)$beta[2]
mid_adoption[2,3,i] <- coef_test(ols, vcov_cluster)$SE[2]
mid_adoption[3,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
mid_adoption[4,3,i] <- nobs(ols)
}







xmat_end <- cbind(all$end_adoption_onfield,all$end_farmer_saved_seed,all$end_Bought_from_agro_input_shop)
index_end <- icwIndex(xmat=xmat_end,revcols = c(2))
all$index_end <- index_end$index

end_adoption <-  array(NA,dim=c(4,2,5))
#loop here over outcomes c("mid_adoption_onfield","mid_Land_Races","mid_farmer_saved_seed","end_Bought_from_agro_input_shop")
outcomes <- c("end_adoption_onfield","end_Land_Races","end_farmer_saved_seed","end_Bought_from_agro_input_shop","index_end")
outcomes_base <- c("adoption_onfield","Land_Races","farmer_saved_seed","Bought_from_agro_input_shop","index_base" )
for (i in 1:length(outcomes)) {
  
  ols <- lm(as.formula(paste(outcomes[i],paste("treatment*clearing*training",outcomes_base[i],sep="+"), sep="~")),data=all)
  
  vcov_cluster <- vcovCR(ols,cluster=all$shop_ID,type="CR0")
  coef_test(ols, vcov_cluster)
  end_adoption[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  end_adoption[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  end_adoption[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  end_adoption[4,1,i] <- nobs(ols)
    
  ols <- lm(as.formula(paste(outcomes[i],paste("treatment*clearing*training",outcomes_base[i],sep="+"), sep="~")),data=all[all$selector_1==1, ])
  vcov_cluster <- vcovCR(ols,cluster=all[all$selector_1==1 ,]$shop_ID,type="CR0")
  coef_test(ols, vcov_cluster)
  end_adoption[1,2,i] <- coef_test(ols, vcov_cluster)$beta[2]
  end_adoption[2,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  end_adoption[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  end_adoption[4,2,i] <- nobs(ols) 
}



save(mid_adoption, file=paste(path,"papers/learning_failures/code/output/mid_adoption.Rdata",sep="/")) 
save(end_adoption, file=paste(path,"papers/learning_failures/code/output/end_adoption.Rdata",sep="/")) 

#### knowledge
all <- merge(merge(baseline_farmers[c("farmer_ID","shop_ID","correct_spacing", "correct_seed_rate", "organic_use", "DAP_use", "Urea_use", "times_weeding", "pesticide_use", "resowing","time_plant","time_weed","treatment","clearing","training","adoption_onfield","Check2.check.maize.q57")],midline_farmers[c("farmer_ID","mid_adoption_onfield")]),endline_farmers[c("farmer_ID","q58_correct","q59_correct","q60_correct","q61_correct","q62_correct","q63_correct")])

##demean orthogonal treatments
all$clearing <- all$clearing - mean(all$clearing)
all$training <- all$training - mean(all$training)


xmat_end <- cbind(all$q58_correct,all$q59_correct,all$q60_correct)
index_end <- icwIndex(xmat=xmat_end)
all$index_end <- index_end$index

all_nt <- subset(all, treatment==FALSE)
mean_know <-  array(NA,dim=c(4,2,7))
#loop here over outcomes
outcomes <- c("q58_correct","q59_correct","q60_correct","q61_correct","q62_correct","q63_correct","index_end")
for (i in 1:length(outcomes)) {
  mean_know[1,1,i] <- mean(unlist(all_nt[outcomes[i]]), na.rm=TRUE)
  mean_know[2,1,i] <- sd(unlist(all_nt[outcomes[i]]), na.rm=TRUE)
  ols <- lm(as.formula(paste(outcomes[i],"treatment*clearing*training", sep="~")),data=all)
  
  vcov_cluster <- vcovCR(ols,cluster=all$shop_ID,type="CR0")
  coef_test(ols, vcov_cluster)
  mean_know[1,2,i] <- coef_test(ols, vcov_cluster)$beta[2]
  mean_know[2,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  mean_know[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  mean_know[4,2,i] <- nobs(ols)
}

### instead of conditioning on baseline adoption, try to condition on baseline 
all$Check2.check.maize.q57[all$Check2.check.maize.q57>99] <- NA
ll <- quantile(all[all$adoption_onfield==1,]$Check2.check.maize.q57, c(.33,.66),na.rm=T)[1]
ul <- quantile(all[all$adoption_onfield==1,]$Check2.check.maize.q57, c(.33,.66),na.rm=T)[2]

all$overest <- all$Check2.check.maize.q57>=ul
all$underest <- all$Check2.check.maize.q57<ll
all$corest <- all$Check2.check.maize.q57>=ll & all$Check2.check.maize.q57<ul
#"correct_spacing", "correct_seed_rate", "organic_use", "DAP_use", "Urea_use", "times_weeding", "pesticide_use", "resowing","time_plant","time_weed"
xmat_base <- cbind(all$correct_spacing, all$correct_seed_rate, all$organic_use, all$DAP_use, all$Urea_use, all$times_weeding, all$pesticide_use, all$resowing, all$time_plant, all$time_weed)
index_base <- icwIndex(xmat=xmat_base)
all$compl_index <- index_base$index

all$selector_2 <- all$overest == TRUE
all$selector_1 <- all$compl_index < 0

end_knowledge <-  array(NA,dim=c(4,3,7))
#loop here over outcomes c("mid_adoption_onfield","mid_Land_Races","mid_farmer_saved_seed","end_Bought_from_agro_input_shop")
outcomes <- c("q58_correct","q59_correct","q60_correct","q61_correct","q62_correct","q63_correct","index_end")
for (i in 1:length(outcomes)) {
  
  ols <- lm(as.formula(paste(outcomes[i],"treatment*clearing*training", sep="~")),data=all)
  
  vcov_cluster <- vcovCR(ols,cluster=all$shop_ID,type="CR0")
  coef_test(ols, vcov_cluster)
  end_knowledge[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  end_knowledge[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  end_knowledge[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  end_knowledge[4,1,i] <- nobs(ols)
  
  ols <- lm(as.formula(paste(outcomes[i],"treatment*clearing*training", sep="~")),data=all[all$selector_1==1,])
  vcov_cluster <- vcovCR(ols,cluster=all[all$selector_1==1,]$shop_ID,type="CR0")
  coef_test(ols, vcov_cluster)
  end_knowledge[1,2,i] <- coef_test(ols, vcov_cluster)$beta[2]
  end_knowledge[2,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  end_knowledge[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  end_knowledge[4,2,i] <- nobs(ols) 
  
  
  ols <- lm(as.formula(paste(outcomes[i],"treatment*clearing*training", sep="~")),data=all[all$selector_2==1 & all$selector_1==1,])
  vcov_cluster <- vcovCR(ols,cluster=all[all$selector_2==1 & all$selector_1==1,]$shop_ID,type="CR0")
  coef_test(ols, vcov_cluster)
  end_knowledge[1,3,i] <- coef_test(ols, vcov_cluster)$beta[2]
  end_knowledge[2,3,i] <- coef_test(ols, vcov_cluster)$SE[2]
  end_knowledge[3,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  end_knowledge[4,3,i] <- nobs(ols) 
}


save(mean_know, file=paste(path,"papers/learning_failures/code/output/mean_know.Rdata",sep="/")) 
save(end_knowledge, file=paste(path,"papers/learning_failures/code/output/end_knowledge.Rdata",sep="/")) 

### now for inputs and practices

#c("end_correct_spacing", "end_correct_seed_rate", "end_organic_use", "end_DAP_use", "end_Urea_use", "end_times_weeding", "end_pesticide_use", "end_resowing")

all <- merge(merge(baseline_farmers[c("farmer_ID","shop_ID","Check2.check.maize.q18","Check2.check.maize.q20","Check2.check.maize.q21","correct_spacing", "correct_seed_rate", "organic_use", "DAP_use", "Urea_use", "times_weeding", "pesticide_use", "resowing","time_plant","time_weed","treatment","clearing","training","adoption_onfield", "correct_spacing", "correct_seed_rate", "organic_use", "DAP_use", "Urea_use", "times_weeding", "pesticide_use", "resowing","time_plant","time_weed", "Check2.check.maize.q57")],midline_farmers[c("farmer_ID","mid_adoption_onfield","mid_correct_spacing", "mid_correct_seed_rate", "mid_organic_use", "mid_DAP_use", "mid_Urea_use", "mid_times_weeding", "mid_pesticide_use", "mid_resowing","mid_time_plant","mid_time_weed")]),endline_farmers[c("farmer_ID","end_adoption_onfield","end_correct_spacing", "end_correct_seed_rate", "end_organic_use", "end_DAP_use", "end_Urea_use", "end_times_weeding", "end_pesticide_use", "end_resowing","end_time_plant","end_time_weed")])


##demean orthogonal treatments
all$clearing <- all$clearing - mean(all$clearing)
all$training <- all$training - mean(all$training)


xmat_base <- cbind(all$correct_spacing, all$correct_seed_rate, all$organic_use, all$DAP_use, all$Urea_use, all$times_weeding, all$pesticide_use, all$resowing, all$time_plant, all$time_weed)
index_base <- icwIndex(xmat=xmat_base)
all$index_base <- index_base$index
### instead of conditioning on baseline adoption, try to condition on baseline 
all$Check2.check.maize.q57[all$Check2.check.maize.q57>99] <- NA
ll <- quantile(all[all$adoption_onfield==1,]$Check2.check.maize.q57, c(.33,.66),na.rm=T)[1]
ul <- quantile(all[all$adoption_onfield==1,]$Check2.check.maize.q57, c(.33,.66),na.rm=T)[2]

all$overest <- all$Check2.check.maize.q57>=ul
all$underest <- all$Check2.check.maize.q57<ll
all$corest <- all$Check2.check.maize.q57>=ll & all$Check2.check.maize.q57<ul


mean_pract <-  array(NA,dim=c(4,2,12))
#loop here over outcomes
outcomes <- c("correct_spacing", "correct_seed_rate", "organic_use", "DAP_use", "Urea_use", "times_weeding", "pesticide_use", "resowing","index_base","time_plant","time_weed", "index_base")
for (i in 1:length(outcomes)) {
  mean_pract[1,1,i] <- mean(unlist(all[outcomes[i]]), na.rm=TRUE)
  mean_pract[2,1,i] <- sd(unlist(all[outcomes[i]]), na.rm=TRUE)
  ols <- lm(as.formula(paste(outcomes[i],"treatment*clearing*training", sep="~")),data=all)
  
  vcov_cluster <- vcovCR(ols,cluster=all$shop_ID,type="CR0")
  coef_test(ols, vcov_cluster)
  mean_pract[1,2,i] <- coef_test(ols, vcov_cluster)$beta[2]
  mean_pract[2,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  mean_pract[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  mean_pract[4,2,i] <- nobs(ols)
}

xmat_mid <- cbind(all$mid_organic_use, all$mid_DAP_use, all$mid_Urea_use, all$mid_times_weeding, all$mid_pesticide_use, all$mid_resowing, all$mid_time_weed)
index_mid <- icwIndex(xmat=xmat_mid)
all$index_mid <- index_mid$index

#"correct_spacing", "correct_seed_rate", "organic_use", "DAP_use", "Urea_use", "times_weeding", "pesticide_use", "resowing","time_plant","time_weed"
xmat_base <- cbind(all$correct_spacing, all$correct_seed_rate, all$organic_use, all$DAP_use, all$Urea_use, all$times_weeding, all$pesticide_use, all$resowing, all$time_plant, all$time_weed)
index_base <- icwIndex(xmat=xmat_base)
all$compl_index <- index_base$index

all$selector_2 <- all$overest == TRUE
all$selector_1 <- all$compl_index < 0

mid_practices <-  array(NA,dim=c(4,3,12))
#loop here over outcomes c("mid_adoption_onfield","mid_Land_Races","mid_farmer_saved_seed","mid_Bought_from_agro_input_shop")
outcomes <- c("mid_correct_spacing", "mid_correct_seed_rate", "mid_organic_use", "mid_DAP_use", "mid_Urea_use", "mid_times_weeding", "mid_pesticide_use", "mid_resowing","index_mid","mid_time_plant", "mid_time_weed", "index_mid")
outcomes_base <- c("correct_spacing", "correct_seed_rate", "organic_use", "DAP_use", "Urea_use", "times_weeding", "pesticide_use", "resowing","index_base","time_plant","time_weed","index_base")

for (i in 1:length(outcomes)) {
  
  ols <- lm(as.formula(paste(outcomes[i],paste("treatment*clearing*training",outcomes_base[i],sep="+"), sep="~")),data=all)
  
  vcov_cluster <- vcovCR(ols,cluster=all$shop_ID,type="CR0")
  coef_test(ols, vcov_cluster)
  mid_practices[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  mid_practices[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  mid_practices[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  mid_practices[4,1,i] <- nobs(ols)
  
  ols <- lm(as.formula(paste(outcomes[i],paste("treatment*clearing*training",outcomes_base[i],sep="+"), sep="~")),data=all[all$selector_1==1,])
  vcov_cluster <- vcovCR(ols,cluster=all[all$selector_1==1,]$shop_ID,type="CR0")
  coef_test(ols, vcov_cluster)
  mid_practices[1,2,i] <- coef_test(ols, vcov_cluster)$beta[2]
  mid_practices[2,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  mid_practices[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  mid_practices[4,2,i] <- nobs(ols) 
  
  ols <- lm(as.formula(paste(outcomes[i],paste("treatment*clearing*training",outcomes_base[i],sep="+"), sep="~")),data=all[all$selector_2==1 & all$selector_1==1,])
  vcov_cluster <- vcovCR(ols,cluster=all[all$selector_2==1  & all$selector_1==1,]$shop_ID,type="CR0")
  coef_test(ols, vcov_cluster)
  mid_practices[1,3,i] <- coef_test(ols, vcov_cluster)$beta[2]
  mid_practices[2,3,i] <- coef_test(ols, vcov_cluster)$SE[2]
  mid_practices[3,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  mid_practices[4,3,i] <- nobs(ols) 
  

}


xmat_end <- cbind(all$end_correct_spacing, all$end_correct_seed_rate, all$end_organic_use, all$end_DAP_use, all$end_Urea_use, all$end_times_weeding, all$end_pesticide_use, all$end_resowing, all$end_time_plant, all$end_time_weed)
index_end <- icwIndex(xmat=xmat_end)
all$index_end <- index_end$index

end_practices <-  array(NA,dim=c(4,3,11))
#loop here over outcomes c("mid_adoption_onfield","mid_Land_Races","mid_farmer_saved_seed","end_Bought_from_agro_input_shop")
outcomes <- c("end_correct_spacing", "end_correct_seed_rate", "end_organic_use", "end_DAP_use", "end_Urea_use", "end_times_weeding", "end_pesticide_use", "end_resowing","index_end","end_time_plant","end_time_weed")
outcomes_base <- c("correct_spacing", "correct_seed_rate", "organic_use", "DAP_use", "Urea_use", "times_weeding", "pesticide_use", "resowing","index_base","time_plant","time_weed")

for (i in 1:length(outcomes)) {
  
  ols <- lm(as.formula(paste(outcomes[i],paste("treatment*clearing*training",outcomes_base[i],sep="+"), sep="~")),data=all)
  
  vcov_cluster <- vcovCR(ols,cluster=all$shop_ID,type="CR0")
  coef_test(ols, vcov_cluster)
  end_practices[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  end_practices[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  end_practices[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  end_practices[4,1,i] <- nobs(ols)
  
  ols <- lm(as.formula(paste(outcomes[i],paste("treatment*clearing*training",outcomes_base[i],sep="+"), sep="~")),data=all[all$selector_1==1,])
  vcov_cluster <- vcovCR(ols,cluster=all[all$selector_1==1 ,]$shop_ID,type="CR0")
  coef_test(ols, vcov_cluster) 
  end_practices[1,2,i] <- coef_test(ols, vcov_cluster)$beta[2]
  end_practices[2,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  end_practices[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  end_practices[4,2,i] <- nobs(ols) 
  
  ols <- lm(as.formula(paste(outcomes[i],paste("treatment*clearing*training",outcomes_base[i],sep="+"), sep="~")),data=all[all$selector_2==1  & all$selector_1==1,])
  vcov_cluster <- vcovCR(ols,cluster=all[all$selector_2==1  & all$selector_1==1,]$shop_ID,type="CR0")
  coef_test(ols, vcov_cluster) 
  end_practices[1,3,i] <- coef_test(ols, vcov_cluster)$beta[2]
  end_practices[2,3,i] <- coef_test(ols, vcov_cluster)$SE[2]
  end_practices[3,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  end_practices[4,3,i] <- nobs(ols) 
  
  
}

save(mean_pract, file=paste(path,"papers/learning_failures/code/output/mean_pract.Rdata",sep="/")) 
save(mid_practices, file=paste(path,"papers/learning_failures/code/output/mid_practices.Rdata",sep="/")) 
save(end_practices, file=paste(path,"papers/learning_failures/code/output/end_practices.Rdata",sep="/")) 

#expectations

baseline_farmers$Check2.check.maize.q57[baseline_farmers$Check2.check.maize.q57 == "999"] <- NA
baseline_farmers$Check2.check.maize.q57[baseline_farmers$Check2.check.maize.q57 > 50] <- NA
baseline_farmers$expectations <- baseline_farmers$Check2.check.maize.q57

midline_farmers$check.maize.q51a[midline_farmers$check.maize.q51a == "n/a"] <- NA
midline_farmers$mid_expectations_met <- midline_farmers$check.maize.q51a== "Yes"

endline_farmers$check.maize.q51a[endline_farmers$check.maize.q51a == "n/a"] <- NA
endline_farmers$end_expectations_met <- endline_farmers$check.maize.q51a== "Yes"




### mismanagement

midline_farmers$mid_myownfault <- NA
midline_farmers$mid_myownfault[midline_farmers$mid_expectations_met==TRUE] <- 0

midline_farmers$mid_myownfault[midline_farmers$check.maize.q51b%in%c("a","b","c","d","96")] <- 0
midline_farmers$mid_myownfault[midline_farmers$check.maize.q51b%in%c("e","f","g","h")] <- 1

endline_farmers$end_myownfault <- NA
endline_farmers$end_myownfault[endline_farmers$end_expectations_met==TRUE] <- 0

endline_farmers$end_myownfault[endline_farmers$check.maize.q51b%in%c("a","b","c","d","96")] <- 0
endline_farmers$end_myownfault[endline_farmers$check.maize.q51b%in%c("e","f","g","h")] <- 1

## poductivity
baseline_farmers$Check2.check.maize.q50 <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q50))
baseline_farmers$check.maize.q51 <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q51))
baseline_farmers$yield_inkg <- baseline_farmers$Check2.check.maize.q50*baseline_farmers$Check2.check.maize.q51
baseline_farmers$yield_inkg_untrimmed <- baseline_farmers$yield_inkg
baseline_farmers <- trim("yield_inkg",baseline_farmers,trim_perc=.05)

baseline_farmers$Check2.check.maize.q29 <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q29))
baseline_farmers$landproductivity <- baseline_farmers$yield_inkg/baseline_farmers$Check2.check.maize.q29 

baseline_farmers$landproductivity_untrimmed <- baseline_farmers$landproductivity
baseline_farmers <- trim("landproductivity",baseline_farmers,trim_perc=.05)

midline_farmers$check.maize.q50 <- as.numeric(as.character(midline_farmers$check.maize.q50))
midline_farmers$check.maize.q51 <- as.numeric(as.character(midline_farmers$check.maize.q51))
midline_farmers$mid_yield_inkg <- midline_farmers$check.maize.q50*midline_farmers$check.maize.q51
midline_farmers$mid_yield_inkg_untrimmed <- midline_farmers$mid_yield_inkg
midline_farmers <- trim("mid_yield_inkg",midline_farmers,trim_perc=.05)

midline_farmers$check.maize.q29 <- as.numeric(as.character(midline_farmers$check.maize.q29))
midline_farmers$mid_landproductivity <- midline_farmers$mid_yield_inkg/midline_farmers$check.maize.q29 

midline_farmers$mid_landproductivity_untrimmed <- midline_farmers$mid_landproductivity
midline_farmers <- trim("mid_landproductivity",midline_farmers,trim_perc=.05)

endline_farmers$check.maize.q50[endline_farmers$check.maize.q50 ==999] <- NA 
endline_farmers$check.maize.q50 <- as.numeric(as.character(endline_farmers$check.maize.q50))
endline_farmers$check.maize.q51 <- as.numeric(as.character(endline_farmers$check.maize.q51))
endline_farmers$end_yield_inkg <- endline_farmers$check.maize.q50*endline_farmers$check.maize.q51
endline_farmers$end_yield_inkg_untrimmed <- endline_farmers$end_yield_inkg
endline_farmers <- trim("end_yield_inkg",endline_farmers,trim_perc=.05)

endline_farmers$check.maize.q29 <- as.numeric(as.character(endline_farmers$check.maize.q29))
endline_farmers$end_landproductivity <- endline_farmers$end_yield_inkg/endline_farmers$check.maize.q29 

endline_farmers$end_landproductivity_untrimmed <- endline_farmers$end_landproductivity
endline_farmers <- trim("end_landproductivity",endline_farmers,trim_perc=.05)

baseline_farmers$Check2.check.maize.q54 <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q54))
baseline_farmers$Check2.check.maize.q54[baseline_farmers$Check2.check.maize.q53=="No"] <- 0
baseline_farmers$Check2.check.maize.q51 <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q51))
baseline_farmers$soldinkg <- baseline_farmers$Check2.check.maize.q54*baseline_farmers$Check2.check.maize.q51
baseline_farmers$Check2.check.maize.q55 <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q55))
baseline_farmers$revenueUGX <- baseline_farmers$Check2.check.maize.q54*baseline_farmers$Check2.check.maize.q55
baseline_farmers$revenueUGX[baseline_farmers$Check2.check.maize.q53=="No"] <- 0

midline_farmers$check.maize.q54 <- as.numeric(as.character(midline_farmers$check.maize.q54))
midline_farmers$check.maize.q54[midline_farmers$check.maize.q53=="No"] <- 0
midline_farmers$check.maize.q51 <- as.numeric(as.character(midline_farmers$check.maize.q51))
midline_farmers$mid_soldinkg <- midline_farmers$check.maize.q54*midline_farmers$check.maize.q51
midline_farmers$check.maize.q55 <- as.numeric(as.character(midline_farmers$check.maize.q55))
midline_farmers$mid_revenueUGX <- midline_farmers$check.maize.q54*midline_farmers$check.maize.q55
midline_farmers$mid_revenueUGX[midline_farmers$check.maize.q53=="No"] <- 0

endline_farmers$check.maize.q54 <- as.numeric(as.character(endline_farmers$check.maize.q54))
endline_farmers$check.maize.q54[endline_farmers$check.maize.q53=="No"] <- 0
endline_farmers$check.maize.q51 <- as.numeric(as.character(endline_farmers$check.maize.q51))
endline_farmers$end_soldinkg <- endline_farmers$check.maize.q54*endline_farmers$check.maize.q51
endline_farmers$check.maize.q55 <- as.numeric(as.character(endline_farmers$check.maize.q55))
endline_farmers$end_revenueUGX <- endline_farmers$check.maize.q54*endline_farmers$check.maize.q55
endline_farmers$end_revenueUGX[endline_farmers$check.maize.q53=="No"] <- 0


all <- merge(merge(baseline_farmers[c("farmer_ID","shop_ID","correct_spacing", "correct_seed_rate", "organic_use", "DAP_use", "Urea_use", "times_weeding", "pesticide_use", "resowing","time_plant","time_weed","treatment","clearing","training","adoption_onfield","expectations","yield_inkg","landproductivity","Check2.check.maize.q57","soldinkg","revenueUGX" )],midline_farmers[c("farmer_ID","mid_expectations_met","mid_myownfault","mid_yield_inkg","mid_landproductivity","mid_soldinkg","mid_revenueUGX", "mid_adoption_onfield","mid_Land_Races","mid_farmer_saved_seed", "mid_Bought_from_agro_input_shop")]),endline_farmers[c("farmer_ID","end_expectations_met","end_myownfault","end_yield_inkg","end_landproductivity","end_soldinkg","end_revenueUGX")])
##demean orthogonal treatments
all$clearing <- all$clearing - mean(all$clearing)
all$training <- all$training - mean(all$training)

xmat_base <- cbind(all$expectations, all$yield_inkg, all$landproductivity)
index_base <- icwIndex(xmat=xmat_base)
all$index_base <- index_base$index

all_nt <- subset(all, treatment==FALSE)
mean_expectations <-  array(NA,dim=c(4,2,6))
#loop here over outcomes
outcomes <- c("mid_expectations_met","yield_inkg","landproductivity","soldinkg","revenueUGX","index_base")
for (i in 1:length(outcomes)) {
  mean_expectations[1,1,i] <- mean(unlist(all_nt[outcomes[i]]), na.rm=TRUE)
  mean_expectations[2,1,i] <- sd(unlist(all_nt[outcomes[i]]), na.rm=TRUE)
  ols <- lm(as.formula(paste(outcomes[i],"treatment*clearing*training", sep="~")),data=all)
  
  vcov_cluster <- vcovCR(ols,cluster=all$shop_ID,type="CR0")
  coef_test(ols, vcov_cluster)
  mean_expectations[1,2,i] <- coef_test(ols, vcov_cluster)$beta[2]
  mean_expectations[2,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  mean_expectations[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  mean_expectations[4,2,i] <- nobs(ols)
}
xmat_mid <- cbind(all$mid_expectations_met)
index_mid <- icwIndex(xmat=xmat_mid)
all$index_mid <- index_mid$index

### instead of conditioning on baseline adoption, try to condition on baseline 
all$Check2.check.maize.q57[all$Check2.check.maize.q57>99] <- NA
ll <- quantile(all[all$adoption_onfield==1,]$Check2.check.maize.q57, c(.33,.66),na.rm=T)[1]
ul <- quantile(all[all$adoption_onfield==1,]$Check2.check.maize.q57, c(.33,.66),na.rm=T)[2]

all$overest <- all$Check2.check.maize.q57>=ul
all$underest <- all$Check2.check.maize.q57<ll
all$corest <- all$Check2.check.maize.q57>=ll & all$Check2.check.maize.q57<ul

#"correct_spacing", "correct_seed_rate", "organic_use", "DAP_use", "Urea_use", "times_weeding", "pesticide_use", "resowing","time_plant","time_weed"
xmat_base <- cbind(all$correct_spacing, all$correct_seed_rate, all$organic_use, all$DAP_use, all$Urea_use, all$times_weeding, all$pesticide_use, all$resowing, all$time_plant, all$time_weed)
index_base <- icwIndex(xmat=xmat_base)
all$compl_index <- index_base$index

all$selector_2 <- all$overest == TRUE
all$selector_1 <- all$compl_index < 0

mid_expectations <-  array(NA,dim=c(4,4,7))
#loop here over outcomes c
outcomes <- c("mid_expectations_met","mid_myownfault","mid_yield_inkg","mid_landproductivity","mid_soldinkg","mid_revenueUGX","index_mid")
for (i in 1:length(outcomes)) {
  
  ols <- lm(as.formula(paste(outcomes[i],"treatment*clearing*training", sep="~")),data=all)
  
  vcov_cluster <- vcovCR(ols,cluster=all$shop_ID,type="CR0")
  coef_test(ols, vcov_cluster)
  mid_expectations[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  mid_expectations[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  mid_expectations[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  mid_expectations[4,1,i] <- nobs(ols)
  
  ols <- lm(as.formula(paste(outcomes[i],"treatment*clearing*training", sep="~")),data=all[all$selector_1==1,])
  vcov_cluster <- vcovCR(ols,cluster=all[all$selector_1==1,]$shop_ID,type="CR0")
  coef_test(ols, vcov_cluster)
  mid_expectations[1,2,i] <- coef_test(ols, vcov_cluster)$beta[2]
  mid_expectations[2,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  mid_expectations[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  mid_expectations[4,2,i] <- nobs(ols) 
  
  ols <- lm(as.formula(paste(outcomes[i],"treatment*clearing*training", sep="~")),data=all[all$selector_1==1 & all$selector_2==1,])
  vcov_cluster <- vcovCR(ols,cluster=all[all$selector_1==1  & all$selector_2==1,]$shop_ID,type="CR0")
  coef_test(ols, vcov_cluster)
  mid_expectations[1,3,i] <- coef_test(ols, vcov_cluster)$beta[2]
  mid_expectations[2,3,i] <- coef_test(ols, vcov_cluster)$SE[2]
  mid_expectations[3,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  mid_expectations[4,3,i] <- nobs(ols) 
  #####this is per reviewer request - look at effect on yield for farmers that don't use complementary practices at baseline and adopt after the treatment
  ### c("mid_adoption_onfield","mid_Land_Races","mid_farmer_saved_seed", "mid_Bought_from_agro_input_shop","index_mid")
  ols <- lm(as.formula(paste(outcomes[i],"treatment*clearing*training", sep="~")),data=all[all$selector_1==1 &  all$mid_Bought_from_agro_input_shop == 1,])
  vcov_cluster <- vcovCR(ols,cluster=all[all$selector_1==1  & all$mid_Bought_from_agro_input_shop ==1,]$shop_ID,type="CR0")
  coef_test(ols, vcov_cluster)
  mid_expectations[1,4,i] <- coef_test(ols, vcov_cluster)$beta[2]
  mid_expectations[2,4,i] <- coef_test(ols, vcov_cluster)$SE[2]
  mid_expectations[3,4,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  mid_expectations[4,4,i] <- nobs(ols) 
  
  }

xmat_end <- cbind(all$end_expectations_met, all$end_yield_inkg, all$end_landproductivity)
index_end <- icwIndex(xmat=xmat_end)
all$index_end <- index_end$index

end_expectations <-  array(NA,dim=c(4,4,7))
#loop here over outcomes c
outcomes <- c("end_expectations_met","end_myownfault","end_yield_inkg","end_landproductivity","end_soldinkg","end_revenueUGX","index_end")
for (i in 1:length(outcomes)) {
  
  ols <- lm(as.formula(paste(outcomes[i],"treatment*clearing*training", sep="~")),data=all)
  
  vcov_cluster <- vcovCR(ols,cluster=all$shop_ID,type="CR0")
  coef_test(ols, vcov_cluster)
  end_expectations[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  end_expectations[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  end_expectations[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  end_expectations[4,1,i] <- nobs(ols)
  
  ols <- lm(as.formula(paste(outcomes[i],"treatment*clearing*training", sep="~")),data=all[all$selector_1==1,])
  vcov_cluster <- vcovCR(ols,cluster=all[all$selector_1==1,]$shop_ID,type="CR0")
  coef_test(ols, vcov_cluster)
  end_expectations[1,2,i] <- coef_test(ols, vcov_cluster)$beta[2]
  end_expectations[2,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  end_expectations[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  end_expectations[4,2,i] <- nobs(ols) 
  
  
  
}
save(mean_expectations, file=paste(path,"papers/learning_failures/code/output/mean_expectations.Rdata",sep="/")) 
save(mid_expectations, file=paste(path,"papers/learning_failures/code/output/mid_expectations.Rdata",sep="/")) 
save(end_expectations, file=paste(path,"papers/learning_failures/code/output/end_expectations.Rdata",sep="/")) 

#
all <- merge(merge(baseline_farmers[c("farmer_ID","shop_ID","treatment","clearing","training","adoption_onfield","Check2.check.maize.q36","Check2.check.maize.q37" )],midline_farmers[c("farmer_ID","mid_adoption_onfield")]),endline_farmers[c("farmer_ID","end_expectations_met","end_myownfault","end_yield_inkg","end_landproductivity")])
##demean orthogonal treatments


### create forest plot
# Load and attach the package
library(ggforestplot)



# Filter only associations to BMI for the first 30 biomarkers of the example
# dataset
df <- data.frame(matrix(ncol = 4, nrow = 0))
x <- c("outcome", "ATE", "se","pval")
colnames(df) <- x
df[1,1] <- "knowledge"
df[1,2:4] <- end_knowledge[1:3,1,7]

df[2,1] <- "adoption"
df[2,2:4] <- mid_adoption[1:3,1,5]

df[3,1] <- "complementary input use"
df[3,2:4] <- mid_practices[1:3,1,12]

df[4,1] <- "correct expectations"
df[4,2:4] <- mid_expectations[1:3,1,7]




# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier


df$outcome <- factor(df$outcome, levels = c("correct expectations","complementary input use", "adoption","knowledge"))
# Plot
zp1 <- ggplot(df, aes(x=outcome, y=ATE)) 
zp1 <- zp1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
zp1 <- zp1 + geom_linerange(aes(x = outcome, ymin = ATE - se*interval1,
                                ymax = ATE + se*interval1),
                            lwd = 1, position = position_dodge(width = 1/2))
zp1 <- zp1 + geom_pointrange(aes(x = outcome, y = ATE, ymin = ATE - se*interval2,
                                 ymax = ATE + se*interval2),
                             lwd = 1/2, position = position_dodge(width = 1/2),
                             shape = 21, fill = "WHITE")
zp1 <- zp1 + coord_flip() 

print(zp1)  # The trick to these is position_dodge().
ggsave("forrest.png", height =3, width = 4,dpi=300)
