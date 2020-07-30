set.seed(12345)
library(pracma)
library(randomizr)  

#trimming: function for trimming a variable in a dataset - replaces with NA
trim <- function(var, dataset, trim_perc=.05) {
  dataset[var][dataset[var] < quantile(dataset[var],c(trim_perc/2,1-(trim_perc/2)), na.rm=T)[1] | dataset[var] > quantile(dataset[var], c(trim_perc/2,1-(trim_perc/2)),na.rm=T)[2] ] <- NA
  return(dataset)
}


### this is executed in the /report subdirectory, need to ..
path <- strsplit(getwd(), "/Study design")[[1]]

shops <- read.csv(paste(path,"stack surveys/data/agro_input_dealers.csv", sep ="/"))
farmers <- read.csv(paste(path,"stack surveys/data/farmers.csv", sep ="/"))
 
#merge each shop to all farmers 


 ### make in long format
connector <- reshape(farmers[c("id.agro1","id.agro2", "id.agro3","ID")], varying = c("id.agro1","id.agro2", "id.agro3"), direction="long", idvar="ID")
#connector <- subset(connector, time == "agro1")
connector <- connector[c("ID","id")]
names(connector) <- c("farmID", "shopID")
connector <- subset(connector, shopID != "n/a")
connector <- subset(connector, shopID != "")
### merge in GPS coordinates from the farmers


connector <- merge(connector, read.csv("/home/bjvca/data/projects/PIMMVC/data/raw_non_public/farmers_gps.csv")[c("ID","hh.maize._gps_latitude","hh.maize._gps_longitude")], by.x="farmID", by.y="ID")
names(connector) <- c("farmID","shopID","farmer_lat","farmer_long")
connector <- merge(connector, read.csv("/home/bjvca/data/projects/PIMMVC/data/raw_non_public/RawData_Shops_ids.csv")[c("id.agro","hh.maize._gps_latitude","hh.maize._gps_longitude")], by.x="shopID", by.y="id.agro")
names(connector) <- c("farmID","shopID","farmer_lat","farmer_long","shop_lat","shop_long")

connector$farmer_lat <- as.numeric(as.character(connector$farmer_lat))
connector$farmer_long <- as.numeric(as.character(connector$farmer_long))
connector$shop_lat <- as.numeric(as.character(connector$shop_lat))
connector$shop_long <- as.numeric(as.character(connector$shop_long))

connector <- subset(connector, !is.na(farmer_lat))

###now get distance between shops and farmers
connector$dist <- NA
for (i in 1:nrow(connector)) {
connector$dist[i] <- haversine(c(connector$farmer_lat[i] ,connector$farmer_long[i]),c(connector$shop_lat[i],connector$shop_long[1]))*1000
}

summary(aggregate(connector$dist, by=list(connector$farmID), mean)[,2])
### median distance of catchment area is 16km
summary(aggregate(connector$dist, by=list(connector$farmID), min)[,2])
### even if we define catchement by minimum distance we still get median of >12 km

shops <- read.csv("/home/bjvca/data/projects/PIMMVC/data/raw_non_public/RawData_Shops_ids.csv")[c("id.agro","hh.maize._gps_latitude","hh.maize._gps_longitude")]
names(shops) <- c("shopID","shop_lat","shop_long")

### we found that a few locations are taken at the hotel where enumerators stay - remove these
shops$shop_lat[shops$shop_lat  > 0.604217379 &
shops$shop_lat  < 0.613710477 &
shops$shop_long > 33.47659906 &
shops$shop_long < 33.4922358] <- NA

shops$shop_long[is.na(shops$shop_lat) &
shops$shop_long > 33.47659906 &
shops$shop_long < 33.4922358]   <- NA

shops <- subset(shops, !is.na(shop_long)) 
shops$shopID <- factor(shops$shopID)

#So how many catchment areas can we define when shops need to be at least 16 km apart?
shops$catchmentID <- NA
counter <- 1

for (shop_1 in names(table(shops$shopID))) {
shops$catchmentID[shops$shopID == shop_1] <- counter
for (shop_2 in names(table(shops$shopID))) {
### key parameter is chosen here: distance to define a catchment area. Here we assume that if shops are less then 5 km apart, they serve the same catchment area
if ( haversine(c(shops$shop_lat[shops$shopID == shop_1] ,shops$shop_long[shops$shopID == shop_1]),c(shops$shop_lat[shops$shopID == shop_2],shops$shop_long[shops$shopID == shop_2])) < 5) {
if (is.na(shops$catchmentID[shops$shopID == shop_2])) {
 shops$catchmentID[shops$shopID == shop_2] <- counter
} else {
 shops$catchmentID[shops$shopID == shop_1]  <- shops$catchmentID[shops$shopID == shop_2] 
}

}
}
counter <- counter + 1
}
dim(table(shops$catchmentID))
library(leaflet)

pal <- colorFactor(
  palette = 'Dark2',
  domain = shops$catchmentID
)


m <- leaflet() %>% setView(lat = 0.65, lng = 33.62, zoom=11)  %>%  addTiles(group="OSM") %>% addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",  group="Google", attribution = 'Google')  %>% addProviderTiles(providers$OpenTopoMap, group="Topography") %>% addCircleMarkers(data=shops, lng=~shop_long, lat=~shop_lat,radius= 8, color=~pal(catchmentID), popup = ~as.character(catchmentID))   %>%  addLayersControl(baseGroups=c('OSM','Google','Topography'))


shops <- shops[c("shopID","catchmentID")]
write.csv(shops, file = paste(path,"stack surveys/data/work/shops.csv", sep ="/") )

#Now run power analysis agains -  copied from power.R

####################################################
 #######Power analysis for the standard design########
 ######Y0 not normal distribution but real data#######
 ################cluster randomization################
 #####################################################
 
 set.seed(12345)
path <- strsplit(getwd(), "/Study design")[[1]]
library(doParallel)
library(ggplot2)


stack_dealers <- read.csv(paste(path,"stack surveys/data/agro_input_dealers.csv", sep ="/"))
stack_farmers <- read.csv(paste(path,"stack surveys/data/farmers.csv", sep ="/"))
shops <- read.csv(file = paste(path,"stack surveys/data/work/shops.csv", sep ="/") )
#transform
stack_farmers$hh.maize.maizen.q64[stack_farmers$hh.maize.maizen.q64==999] <- NA
stack_farmers$hh.maize.maizen.q65[stack_farmers$hh.maize.maizen.q65==999] <- NA
stack_farmers$hh.maize.maizen.q46b[stack_farmers$hh.maize.maizen.q46b==999] <- NA
stack_farmers$hh.maize.maizen.q46b[stack_farmers$hh.maize.maizen.q46b=="n/a"] <- NA
stack_farmers$hh.maize.plot_select_area[stack_farmers$hh.maize.plot_select_area=="n/a"] <- NA
stack_farmers$hh.maize.plot_select_area <- as.numeric(as.character(stack_farmers$hh.maize.plot_select_area))

#make yield variable
stack_farmers$yield_kg <- stack_farmers$hh.maize.maizen.q64*stack_farmers$hh.maize.maizen.q65 #64. bags of maize harvested * 65. kgs in one bag
stack_farmers$yield_kg_per_acre <- stack_farmers$yield_kg/stack_farmers$hh.maize.plot_select_area #area of maize field in acres (particular plot)
#percentage allocated to maize q46b excluded because 454 NA's

#trimming: function for trimming a variable in a dataset - replaces with NA
trim <- function(var, dataset, trim_perc=.05) {
  dataset[var][dataset[var] < quantile(dataset[var],c(trim_perc/2,1-(trim_perc/2)), na.rm=T)[1] | dataset[var] > quantile(dataset[var], c(trim_perc/2,1-(trim_perc/2)),na.rm=T)[2] ] <- NA
  return(dataset)
}

stack_farmers <- trim("yield_kg_per_acre", stack_farmers)

#standard deviations and means
sd(stack_farmers$yield_kg_per_acre, na.rm=TRUE)
mean(stack_farmers$yield_kg_per_acre, na.rm=TRUE)
### prepare variables that will be used to assess power
###QUANTITY SOLD###
#transform
stack_dealers$hh.maize.seed.1..q22[stack_dealers$hh.maize.seed.1..q22==999] <- NA
stack_dealers$hh.maize.seed.1..q22[stack_dealers$hh.maize.seed.1..q22=="n/a"] <- NA
stack_dealers$hh.maize.seed.2..q22[stack_dealers$hh.maize.seed.2..q22==999] <- NA
stack_dealers$hh.maize.seed.2..q22[stack_dealers$hh.maize.seed.2..q22=="n/a"] <- NA
stack_dealers$hh.maize.seed.3..q22[stack_dealers$hh.maize.seed.3..q22=="n/a"] <- NA
stack_dealers$hh.maize.seed.1..q22 <- as.numeric(as.character(stack_dealers$hh.maize.seed.1..q22))
stack_dealers$hh.maize.seed.2..q22 <- as.numeric(as.character(stack_dealers$hh.maize.seed.2..q22))
stack_dealers$hh.maize.seed.3..q22 <- as.numeric(as.character(stack_dealers$hh.maize.seed.3..q22))

#make quantity sold variable - this is kg sold of top 3 hybrids (assuming missing means no hybrids are sold)
stack_dealers$quantitysold <- rowSums(cbind(stack_dealers$hh.maize.seed.1..q22,stack_dealers$hh.maize.seed.2..q22,stack_dealers$hh.maize.seed.3..q22), na.rm=T) #not good because 65 of 78 NA's (67 after trimming)
#stack_dealers$quantitysold <- stack_dealers$hh.maize.seed.1..q22+stack_dealers$hh.maize.seed.2..q22 #not good because 46 of 78 NA's (48 after trimming)
#stack_dealers$quantitysold <- stack_dealers$hh.maize.seed.1..q22 #only 12 NA's, 15 after trimming

stack_dealers <- trim("quantitysold", stack_dealers, trim_perc=.05)

#standard deviations and means
sd(stack_dealers$quantitysold, na.rm=TRUE)
mean(stack_dealers$quantitysold, na.rm=TRUE)

###REPUTATION###
#make reputation variable
ratings <- merge(merge(aggregate(as.numeric(as.character(stack_farmers$hh.maize.agro1.q108l)),list(stack_farmers$id.agro1), FUN=mean, na.rm=T), aggregate(as.numeric(as.character(stack_farmers$hh.maize.agro2.q109l)),list(stack_farmers$id.agro2), FUN=mean, na.rm=T), by = "Group.1", all=T)
                 ,
                 aggregate(as.numeric(as.character(stack_farmers$hh.maize.agro3.q111l)),list(stack_farmers$id.agro3), FUN=mean, na.rm=T), by = "Group.1", all=T)
ratings <- subset(ratings,Group.1!="n/a")

ratings$reputation_av_farmers <- rowMeans(ratings[,2:4], na.rm=T)
stack_dealers <- merge(stack_dealers, ratings[,c(1,5)],by.x="id.agro" ,by.y="Group.1",all.x=T)

#standard deviations and means
sd(ratings$reputation_av_farmers, na.rm=TRUE)
mean(ratings$reputation_av_farmers, na.rm=TRUE)

###INPUT USE###
#transform
stack_farmers$hh.maize.maizen.q48[stack_farmers$hh.maize.maizen.q48==98] <- NA
stack_farmers$hh.maize.maizen.q48[stack_farmers$hh.maize.maizen.q48=="l"] <- NA #made "other" NA

#make input use variable
stack_farmers$inputuse_binary <- (stack_farmers$hh.maize.maizen.q48 == "a") | (stack_farmers$hh.maize.maizen.q48 == "b")| (stack_farmers$hh.maize.maizen.q48 == "c")| (stack_farmers$hh.maize.maizen.q48 == "d")| (stack_farmers$hh.maize.maizen.q48 == "e")| (stack_farmers$hh.maize.maizen.q48 == "f")| (stack_farmers$hh.maize.maizen.q48 == "g")| (stack_farmers$hh.maize.maizen.q48 == "h")| (stack_farmers$hh.maize.maizen.q48 == "i")| (stack_farmers$hh.maize.maizen.q48 == "j")

#standard deviations and means
sd(stack_farmers$inputuse_binary, na.rm=TRUE)
mean(stack_farmers$inputuse_binary, na.rm=TRUE)
###SEED QUALITY###
#transform
stack_farmers$hh.maize.agro1.q108j[stack_farmers$hh.maize.agro1.q108j=="n/a"] <- NA
stack_farmers$hh.maize.agro1.q108j <- as.numeric(as.character(stack_farmers$hh.maize.agro1.q108j))

#make seed quality variable
stack_farmers$seedquality_binary <- (stack_farmers$hh.maize.agro1.q108j > 4) #median is 4

#standard deviations and means
sd(stack_farmers$seedquality_binary, na.rm=TRUE)
mean(stack_farmers$seedquality_binary, na.rm=TRUE)

#merge in catchment ID
stack_dealers <- merge(stack_dealers,shops[c("shopID","catchmentID")], by.x="id.agro",by.y="shopID")
 
 #interventions & randomization at the level of the catchment area = level of the input dealer (ID) = level of the cluster
 
 
 possible.ns <- seq(from=75, to=125, by=5) # The sample sizes we'll be considering
 powers <- rep(NA, length(possible.ns))           # Empty object to collect simulation estimates
 alpha <- 0.05                                    # Standard significance level
 sims <- 500                                      # Number of simulations to conduct for each N
 
 #### Outer loop to vary the number of subjects ####
 for (j in 1:length(possible.ns)){
   N <- possible.ns[j]                              # Pick the jth value for N
 
   significant.experiments <- rep(NA, sims)         # Empty object to count significant experiments
 
   #### Inner loop to conduct experiments "sims" times over for each N ####
   for (i in 1:sims){             # control potential outcome
     #sample_dta <- stack_farmers[c("id.agro","yield_kg_per_acre")][sample(nrow(stack_farmers), size = N, replace = TRUE),]             # control potential outcome - is now a data frame with 2 vars
     sample_dta <- data.frame(sample(names(table(stack_dealers$catchmentID)), size=N, replace =T))
     sample_dta$Z.sim <- 0
     sample_dta$Z.sim[1:round(length(sample_dta$Z.sim)/2)] <- 1 
     sample_dta$new_catch <- rownames(sample_dta) 
     names(sample_dta) <- c("catchID","Z.sim","new_catch")
     sample_dta <- merge(sample_dta, stack_dealers, by.x="catchID",by.y="catchmentID")
     sample_dta <- sample_dta[c("new_catch","quantitysold","Z.sim")]          # control potential outcome - is now a data frame with 2 vars
     names(sample_dta) <- c("cluster_ID","Y0","Z.sim")
     tau <- 158.8215                           # Hypothesize treatment effect .35 cohen d
     #tau <- 136.131      # Hypothesize treatment effect .3 cohen d
     #tau <-  90.75516
     
     sample_dta$Y1 <- sample_dta$Y0 + tau                                 # treatment potential outcome
     #randomize(stack_farmers, group = c("1", "0"), block = stack_farmers$id_inputdealer)
     #Z.sim <- rbinom(n=N, size=1, prob=.5)          # Do a random assignment
     #Z.sim <- cluster_ra(clusters = stack_farmers$id_inputdealer)
     sample_dta$Y.sim <- sample_dta$Y1*sample_dta$Z.sim + sample_dta$Y0*(1-sample_dta$Z.sim)               # Reveal outcomes according to assignment
     fit.sim <- lm(Y.sim ~ Z.sim, data= sample_dta)                   # Do analysis (Simple regression)
     p.value <- summary(fit.sim)$coefficients[2,4]  # Extract p-values
     significant.experiments[i] <- (p.value <= alpha) # Determine significance according to p <= 0.05
   }
 
   powers[j] <- mean(significant.experiments)       # store average success rate (power) for each N
 }
 df <- data.frame(cbind(possible.ns, powers))
 png((paste(path,"Study design/power_quantitysold.png", sep ="/")), units="px", height=3200, width= 3200, res=600)
 ggplot(df, aes(x = possible.ns, y = powers)) + geom_hline(yintercept = .8, colour =  "red", size=1) + geom_smooth(se=FALSE) + labs(y="power", x = "number of catchment areas") + ylim(0.6, 0.9) + annotate(geom="text", x=80, y=0.79, label="target power value of 0.8", color="red")
 dev.off()
 #### do this also for scores:
  #interventions & randomization at the level of the catchment area = level of the input dealer (ID) = level of the cluster
 
 
 possible.ns <- seq(from=75, to=125, by=5) # The sample sizes we'll be considering
 powers <- rep(NA, length(possible.ns))           # Empty object to collect simulation estimates
 alpha <- 0.05                                    # Standard significance level
 sims <- 500                                      # Number of simulations to conduct for each N
 stack_dealers_reputation_av_farmers <- subset(stack_dealers, !is.na(reputation_av_farmers))
 
 #### Outer loop to vary the number of subjects ####
 for (j in 1:length(possible.ns)){
   N <- possible.ns[j]                              # Pick the jth value for N
 
   significant.experiments <- rep(NA, sims)         # Empty object to count significant experiments
 
   #### Inner loop to conduct experiments "sims" times over for each N ####
   for (i in 1:sims){             # control potential outcome
     #sample_dta <- stack_farmers[c("id.agro","yield_kg_per_acre")][sample(nrow(stack_farmers), size = N, replace = TRUE),]             # control potential outcome - is now a data frame with 2 vars
     sample_dta <- data.frame(sample(names(table(stack_dealers_reputation_av_farmers$catchmentID)), size=N, replace =T))
     sample_dta$Z.sim <- 0
     sample_dta$Z.sim[1:round(length(sample_dta$Z.sim)/2)] <- 1 
     sample_dta$new_catch <- rownames(sample_dta) 
     names(sample_dta) <- c("catchID","Z.sim","new_catch")
     sample_dta <- merge(sample_dta, stack_dealers_reputation_av_farmers, by.x="catchID",by.y="catchmentID")
     sample_dta <- sample_dta[c("new_catch","reputation_av_farmers","Z.sim")]          # control potential outcome - is now a data frame with 2 vars
     names(sample_dta) <- c("cluster_ID","Y0","Z.sim")
	tau <-  0.2151055 
     sample_dta$Y1 <- sample_dta$Y0 + tau                                 # treatment potential outcome
     #randomize(stack_farmers, group = c("1", "0"), block = stack_farmers$id_inputdealer)
     #Z.sim <- rbinom(n=N, size=1, prob=.5)          # Do a random assignment
     #Z.sim <- cluster_ra(clusters = stack_farmers$id_inputdealer)
     sample_dta$Y.sim <- sample_dta$Y1*sample_dta$Z.sim + sample_dta$Y0*(1-sample_dta$Z.sim)               # Reveal outcomes according to assignment
     fit.sim <- lm(Y.sim ~ Z.sim, data= sample_dta)                   # Do analysis (Simple regression)
     p.value <- summary(fit.sim)$coefficients[2,4]  # Extract p-values
     significant.experiments[i] <- (p.value <= alpha) # Determine significance according to p <= 0.05
   }
 
   powers[j] <- mean(significant.experiments)       # store average success rate (power) for each N
 }
 df <- data.frame(cbind(possible.ns, powers))
 png((paste(path,"Study design/power_reputation_av_farmers.png", sep ="/")), units="px", height=3200, width= 3200, res=600)
 ggplot(df, aes(x = possible.ns, y = powers)) + geom_hline(yintercept = .8, colour =  "red", size=1) + geom_smooth(se=FALSE) + labs(y="power", x = "number of catchment areas") + ylim(0.6, 0.9) + annotate(geom="text", x=80, y=0.79, label="target power value of 0.8", color="red")
dev.off()
 
 
 ### to caluclate average number of input dealers that correspond to a given N 
 N <- 112
nr_shops <- rep(NA, sims)
    for (i in 1:sims){             # control potential outcome
     #sample_dta <- stack_farmers[c("id.agro","yield_kg_per_acre")][sample(nrow(stack_farmers), size = N, replace = TRUE),]             # control potential outcome - is now a data frame with 2 vars
     sample_dta <- data.frame(sample(names(table(stack_dealers$catchmentID)), size=N, replace =T))
     sample_dta$Z.sim <- 0
     sample_dta$Z.sim[1:round(length(sample_dta$Z.sim)/2)] <- 1 
     sample_dta$new_catch <- rownames(sample_dta) 
     names(sample_dta) <- c("catchID","Z.sim","new_catch")
     sample_dta <- merge(sample_dta, stack_dealers, by.x="catchID",by.y="catchmentID")
     nr_shops[i] <- dim(sample_dta)[1]
     }
  summary(nr_shops)   
     #mean is 318
     
     
     #Now that we have numner of catchment areas and input dearlers, turn to farmers
     
     #interventions & randomization at the level of the catchment area = level of the input dealer (ID) = level of the cluster


#possible.ns <- 112
#possible.fs <- seq(from=20, to=40, by=1)
#powers <- rep(NA, length(possible.ns))
#alpha <- 0.05
#sims <- 200

# #yield_kg_per_acre
# 

# cl <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
# registerDoParallel(cl)
# #1st loop
# 

#for (f in 1:length(possible.fs)){
#     F <- possible.fs[f]
#     print(possible.fs[f]) #print something to show that we are still making progress
# 
#     significant.experiments <- foreach(i = 1:sims,.combine=rbind,.packages=c("doParallel")) %dopar% {
#     sample_dta <-  data.frame(sample(names(table(stack_dealers$catchmentID)), size=N, replace =T))
#     sample_dta$Z.sim <- 0
#     sample_dta$Z.sim[1:round(length(sample_dta$Z.sim)/2)] <- 1 
#     sample_dta$new_catch <- rownames(sample_dta) 
#     names(sample_dta) <- c("catchID","Z.sim","new_catch")
#     sample_dta_dealer <- merge(sample_dta, stack_dealers, by.x="catchID",by.y="catchmentID")
#     sample_dta_farmers <- merge(sample_dta_dealer, stack_farmers, by.x="id.agro",by.y="id.agro1")
# 
#       #4th loop
#       clusters1 <- foreach(k = 1:length(sample_dta$new_catch),.combine=rbind) %dopar% {
#        temp <- sample_dta_farmers[sample_dta_farmers$catchID == sample_dta$catchID[k],]
#       temp <- temp[sample(nrow(temp), size=F, replace = TRUE),]
# 
#       clusters1 <- return(temp)  #here we get the treatment in again
#        }
#     clusters1$Y0 <- clusters1$yield_kg_per_acre
#     tau <- 56.91028
#     clusters1$Y1 <- clusters1$Y0 + tau
#     clusters1$Y.sim <- clusters1$Y1*clusters1$Z.sim + clusters1$Y0*(1-clusters1$Z.sim)
#     fit.sim <- lm(Y.sim ~ Z.sim, data=clusters1)
#     p.value <- summary(fit.sim)$coefficients[2,4]
#     significant.experiments <- (p.value <= alpha)
#   }
# 
#   powers[f] <- mean(significant.experiments)
# 
# }
# 

#df <- data.frame(cbind(possible.fs, powers))
## write.csv(df,(paste(path,"Study design/power_yield_kg_per_acre.csv", sep ="/")), row.names = FALSE)
## 
##df <- read.csv(paste(path,"Study design/power_yield_kg_per_acre.csv", sep ="/"))
#png((paste(path,"Study design/power_yield_kg_per_acre.png", sep ="/")), units="px", height=3200, width= 3200, res=600)
##ggplot(df, aes(x = possible.ns, y = powers)) + geom_line() + geom_hline(yintercept = .8, colour =  "red", size=1)
#ggplot(df, aes(x = possible.fs, y = powers)) + geom_hline(yintercept = .8, colour =  "red", size=1) + geom_smooth(se=FALSE)+ labs(y="power", x = "number of farmers per catchment area") + ylim(0.75, .825) + annotate(geom="text", x=22, y=0.798, label="target power value of 0.8", color="red")

#dev.off()


#### now for input use
#possible.ns <- 112
#possible.fs <- seq(from=20, to=40, by=1)
#powers <- rep(NA, length(possible.ns))
#alpha <- 0.05
#sims <- 200

#cl <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
#registerDoParallel(cl)
# 

#for (f in 1:length(possible.fs)){
#     F <- possible.fs[f]
#     print(possible.fs[f]) #print something to show that we are still making progress
# 
#     significant.experiments <- foreach(i = 1:sims,.combine=rbind,.packages=c("doParallel")) %dopar% {
#     sample_dta <-  data.frame(sample(names(table(stack_dealers$catchmentID)), size=N, replace =T))
#     sample_dta$Z.sim <- 0
#     sample_dta$Z.sim[1:round(length(sample_dta$Z.sim)/2)] <- 1 
#     sample_dta$new_catch <- rownames(sample_dta) 
#     names(sample_dta) <- c("catchID","Z.sim","new_catch")
#     sample_dta_dealer <- merge(sample_dta, stack_dealers, by.x="catchID",by.y="catchmentID")
#     sample_dta_farmers <- merge(sample_dta_dealer, stack_farmers, by.x="id.agro",by.y="id.agro1")
# 
#       #4th loop
#       clusters1 <- foreach(k = 1:length(sample_dta$new_catch),.combine=rbind) %dopar% {
#        temp <- sample_dta_farmers[sample_dta_farmers$catchID == sample_dta$catchID[k],]
#       temp <- temp[sample(nrow(temp), size=F, replace = TRUE),]
# 
#       clusters1 <- return(temp)  #here we get the treatment in again
#        }
#     clusters1$Y0 <- clusters1$inputuse_binary
#     tau <-  0.065
#     clusters1$Y1 <- clusters1$Y0 + tau
#     clusters1$Y.sim <- clusters1$Y1*clusters1$Z.sim + clusters1$Y0*(1-clusters1$Z.sim)
#     fit.sim <- lm(Y.sim ~ Z.sim, data=clusters1)
#     p.value <- summary(fit.sim)$coefficients[2,4]
#     significant.experiments <- (p.value <= alpha)
#   }
# 
#   powers[f] <- mean(significant.experiments)
# 
# }
## 

#df <- data.frame(cbind(possible.fs, powers))
## write.csv(df,(paste(path,"Study design/power_yield_kg_per_acre.csv", sep ="/")), row.names = FALSE)
## 
##df <- read.csv(paste(path,"Study design/power_yield_kg_per_acre.csv", sep ="/"))
#png((paste(path,"Study design/power_inputuse_binary.png", sep ="/")), units="px", height=3200, width= 3200, res=600)
##ggplot(df, aes(x = possible.ns, y = powers)) + geom_line() + geom_hline(yintercept = .8, colour =  "red", size=1)
#ggplot(df, aes(x = possible.fs, y = powers)) + geom_hline(yintercept = .8, colour =  "red", size=1) + geom_smooth(se=FALSE)+ labs(y="power", x = "number of farmers per catchment area") + ylim(0, 1) + annotate(geom="text", x=5, y=0.775, label="target power value of 0.8", color="red")
#dev.off()

#### now for input use
#possible.ns <- 112
#possible.fs <- seq(from=20, to=40, by=1)
#powers <- rep(NA, length(possible.ns))
#alpha <- 0.05
#sims <- 200

#cl <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
#registerDoParallel(cl)
# 

#for (f in 1:length(possible.fs)){
#     F <- possible.fs[f]
#     print(possible.fs[f]) #print something to show that we are still making progress
# 
#     significant.experiments <- foreach(i = 1:sims,.combine=rbind,.packages=c("doParallel")) %dopar% {
#     sample_dta <-  data.frame(sample(names(table(stack_dealers$catchmentID)), size=N, replace =T))
#     sample_dta$Z.sim <- 0
#     sample_dta$Z.sim[1:round(length(sample_dta$Z.sim)/2)] <- 1 
#     sample_dta$new_catch <- rownames(sample_dta) 
#     names(sample_dta) <- c("catchID","Z.sim","new_catch")
#     sample_dta_dealer <- merge(sample_dta, stack_dealers, by.x="catchID",by.y="catchmentID")
#     sample_dta_farmers <- merge(sample_dta_dealer, stack_farmers, by.x="id.agro",by.y="id.agro1")
# 
#       #4th loop
#       clusters1 <- foreach(k = 1:length(sample_dta$new_catch),.combine=rbind) %dopar% {
#        temp <- sample_dta_farmers[sample_dta_farmers$catchID == sample_dta$catchID[k],]
#       temp <- temp[sample(nrow(temp), size=F, replace = TRUE),]
# 
#       clusters1 <- return(temp)  #here we get the treatment in again
#        }
#     clusters1$Y0 <- clusters1$seedquality_binary
#     tau <-  0.065
#     clusters1$Y1 <- clusters1$Y0 + tau
#     clusters1$Y.sim <- clusters1$Y1*clusters1$Z.sim + clusters1$Y0*(1-clusters1$Z.sim)
#     fit.sim <- lm(Y.sim ~ Z.sim, data=clusters1)
#     p.value <- summary(fit.sim)$coefficients[2,4]
#     significant.experiments <- (p.value <= alpha)
#   }
# 
#   powers[f] <- mean(significant.experiments)
# 
# }
## 

#df <- data.frame(cbind(possible.fs, powers))
## write.csv(df,(paste(path,"Study design/power_yield_kg_per_acre.csv", sep ="/")), row.names = FALSE)
## 
##df <- read.csv(paste(path,"Study design/power_yield_kg_per_acre.csv", sep ="/"))
#png((paste(path,"Study design/seedquality_binary.png", sep ="/")), units="px", height=3200, width= 3200, res=600)
##ggplot(df, aes(x = possible.ns, y = powers)) + geom_line() + geom_hline(yintercept = .8, colour =  "red", size=1)
#ggplot(df, aes(x = possible.fs, y = powers)) + geom_hline(yintercept = .8, colour =  "red", size=1) + geom_smooth(se=FALSE)+ labs(y="power", x = "number of farmers per catchment area") + ylim(0, 1) + annotate(geom="text", x=5, y=0.775, label="target power value of 0.8", color="red")
#dev.off()

##### x farmers per input dealer
###yield
 set.seed(12345)
N <- 112
possible.fs <- seq(from=5, to=20, by=1)
powers <- rep(NA, length(possible.fs))
alpha <- 0.05
sims <- 200

 #yield_kg_per_acre
 

 cl <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
 registerDoParallel(cl)
 #1st loop
 

for (f in 1:length(possible.fs)){
     F <- possible.fs[f]
     print(possible.fs[f]) #print something to show that we are still making progress
 
     significant.experiments <- foreach(i = 1:sims,.combine=rbind,.packages=c("doParallel")) %dopar% {
     sample_dta <-  data.frame(sample(names(table(stack_dealers$catchmentID)), size=N, replace =T))
     sample_dta$Z.sim <- 0
     sample_dta$Z.sim[1:round(length(sample_dta$Z.sim)/2)] <- 1 
     sample_dta$new_catch <- rownames(sample_dta) 
     names(sample_dta) <- c("catchID","Z.sim","new_catch")
     sample_dta_dealer <- merge(sample_dta, stack_dealers, by.x="catchID",by.y="catchmentID")
     sample_dta_farmers <- merge(sample_dta_dealer, stack_farmers, by.x="id.agro",by.y="id.agro1")
 
       #4th loop
       clusters1 <- foreach(k = 1:length(sample_dta_dealer$id.agro),.combine=rbind) %dopar% {
        temp <- sample_dta_farmers[sample_dta_farmers$id.agro == sample_dta_dealer$id.agro[k],]
       temp <- temp[sample(nrow(temp), size=F, replace = TRUE),]
 
       clusters1 <- return(temp)  #here we get the treatment in again
        }
     clusters1$Y0 <- clusters1$yield_kg_per_acre
     tau <- 56.91028
     clusters1$Y1 <- clusters1$Y0 + tau
     clusters1$Y.sim <- clusters1$Y1*clusters1$Z.sim + clusters1$Y0*(1-clusters1$Z.sim)
     fit.sim <- lm(Y.sim ~ Z.sim, data=clusters1)
     p.value <- summary(fit.sim)$coefficients[2,4]
     significant.experiments <- (p.value <= alpha)
   }
 
   powers[f] <- mean(significant.experiments)
 
 }
 

df <- data.frame(cbind(possible.fs, powers))
write.csv(df,(paste(path,"Study design/power_yield_kg_per_acre.csv", sep ="/")), row.names = FALSE)
# 
#df <- read.csv(paste(path,"Study design/power_yield_kg_per_acre.csv", sep ="/"))
png((paste(path,"Study design/power_yield_kg_per_acre.png", sep ="/")), units="px", height=3200, width= 3200, res=600)
#ggplot(df, aes(x = possible.ns, y = powers)) + geom_line() + geom_hline(yintercept = .8, colour =  "red", size=1)
ggplot(df, aes(x = possible.fs, y = powers)) + geom_hline(yintercept = .8, colour =  "red", size=1) + geom_smooth(se=FALSE)+ labs(y="power", x = "number of farmers per input dealer") + ylim(.1, .975) + annotate(geom="text", x=4, y=0.775, label="target power value of 0.8", color="red")

dev.off()

##### x farmers per input dealer
###input use
 set.seed(12345)
N <- 112
possible.fs <- seq(from=1, to=10, by=1)
powers <- rep(NA, length(possible.fs))
alpha <- 0.05
sims <- 200

 #yield_kg_per_acre
 

 cl <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
 registerDoParallel(cl)
 #1st loop
 

for (f in 1:length(possible.fs)){
     F <- possible.fs[f]
     print(possible.fs[f]) #print something to show that we are still making progress
 
     significant.experiments <- foreach(i = 1:sims,.combine=rbind,.packages=c("doParallel")) %dopar% {
     sample_dta <-  data.frame(sample(names(table(stack_dealers$catchmentID)), size=N, replace =T))
     sample_dta$Z.sim <- 0
     sample_dta$Z.sim[1:round(length(sample_dta$Z.sim)/2)] <- 1 
     sample_dta$new_catch <- rownames(sample_dta) 
     names(sample_dta) <- c("catchID","Z.sim","new_catch")
     sample_dta_dealer <- merge(sample_dta, stack_dealers, by.x="catchID",by.y="catchmentID")
     sample_dta_farmers <- merge(sample_dta_dealer, stack_farmers, by.x="id.agro",by.y="id.agro1")
 
       #4th loop
       clusters1 <- foreach(k = 1:length(sample_dta_dealer$id.agro),.combine=rbind) %dopar% {
        temp <- sample_dta_farmers[sample_dta_farmers$id.agro == sample_dta_dealer$id.agro[k],]
       temp <- temp[sample(nrow(temp), size=F, replace = TRUE),]
 
       clusters1 <- return(temp)  #here we get the treatment in again
        }
     clusters1$Y0 <- clusters1$inputuse_binary
     clusters1$Y1 <- clusters1$Y0
     clusters1$Y1[!is.na(clusters1$Y0) & clusters1$Y1 == 0] <- rbinom(n=length(clusters1$Y0[!is.na(clusters1$Y0) & clusters1$Y1 == 0] ), size=1, prob=(0.065*length(clusters1$Y0[!is.na(clusters1$Y0)])/length(clusters1$Y0[!is.na(clusters1$Y0) & clusters1$Y1 == 0] )))
     clusters1$Y.sim <- clusters1$Y1*clusters1$Z.sim + clusters1$Y0*(1-clusters1$Z.sim)
     fit.sim <- lm(Y.sim ~ Z.sim, data=clusters1)
     p.value <- summary(fit.sim)$coefficients[2,4]
     significant.experiments <- (p.value <= alpha)
   }
 
   powers[f] <- mean(significant.experiments)
 
 }
 

df <- data.frame(cbind(possible.fs, powers))
write.csv(df,(paste(path,"Study design/power_inputuse_binary.csv", sep ="/")), row.names = FALSE)
# 
#df <- read.csv(paste(path,"Study design/power_yield_kg_per_acre.csv", sep ="/"))
png((paste(path,"Study design/power_inputuse_binary.png", sep ="/")), units="px", height=3200, width= 3200, res=600)
#ggplot(df, aes(x = possible.ns, y = powers)) + geom_line() + geom_hline(yintercept = .8, colour =  "red", size=1)
ggplot(df, aes(x = possible.fs, y = powers)) + geom_hline(yintercept = .8, colour =  "red", size=1) + geom_smooth(se=FALSE)+ labs(y="power", x = "number of farmers per input dealer") + ylim(0, 1) + annotate(geom="text", x=5, y=0.775, label="target power value of 0.8", color="red")
dev.off()

 set.seed(12345)
possible.ns <- 112
possible.fs <- seq(from=1, to=10, by=1)
powers <- rep(NA, length(possible.fs))
alpha <- 0.05
sims <- 200

 #yield_kg_per_acre
 

 cl <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
 registerDoParallel(cl)
 #1st loop
 

for (f in 1:length(possible.fs)){
     F <- possible.fs[f]
     print(possible.fs[f]) #print something to show that we are still making progress
 
     significant.experiments <- foreach(i = 1:sims,.combine=rbind,.packages=c("doParallel")) %dopar% {
     sample_dta <-  data.frame(sample(names(table(stack_dealers$catchmentID)), size=N, replace =T))
     sample_dta$Z.sim <- 0
     sample_dta$Z.sim[1:round(length(sample_dta$Z.sim)/2)] <- 1 
     sample_dta$new_catch <- rownames(sample_dta) 
     names(sample_dta) <- c("catchID","Z.sim","new_catch")
     sample_dta_dealer <- merge(sample_dta, stack_dealers, by.x="catchID",by.y="catchmentID")
     sample_dta_farmers <- merge(sample_dta_dealer, stack_farmers, by.x="id.agro",by.y="id.agro1")
 
       #4th loop
       clusters1 <- foreach(k = 1:length(sample_dta_dealer$id.agro),.combine=rbind) %dopar% {
        temp <- sample_dta_farmers[sample_dta_farmers$id.agro == sample_dta_dealer$id.agro[k],]
       temp <- temp[sample(nrow(temp), size=F, replace = TRUE),]
 
       clusters1 <- return(temp)  #here we get the treatment in again
        }
     clusters1$Y0 <- clusters1$seedquality_binary
     clusters1$Y1 <- clusters1$Y0 
     clusters1$Y1[!is.na(clusters1$Y0) & clusters1$Y1 == 0] <- rbinom(n=length(clusters1$Y0[!is.na(clusters1$Y0) & clusters1$Y1 == 0] ), size=1, prob=(0.0887512*length(clusters1$Y0[!is.na(clusters1$Y0)])/length(clusters1$Y0[!is.na(clusters1$Y0) & clusters1$Y1 == 0] )))
     clusters1$Y.sim <- clusters1$Y1*clusters1$Z.sim + clusters1$Y0*(1-clusters1$Z.sim)
     fit.sim <- lm(Y.sim ~ Z.sim, data=clusters1)
     p.value <- summary(fit.sim)$coefficients[2,4]
     significant.experiments <- (p.value <= alpha)
   }
 
   powers[f] <- mean(significant.experiments)
 
 }
 

df <- data.frame(cbind(possible.fs, powers))
write.csv(df,(paste(path,"Study design/power_seedquality_binary.csv", sep ="/")), row.names = FALSE)
# 
#df <- read.csv(paste(path,"Study design/power_yield_kg_per_acre.csv", sep ="/"))
png((paste(path,"Study design/power_seedquality_binary.png", sep ="/")), units="px", height=3200, width= 3200, res=600)
#ggplot(df, aes(x = possible.ns, y = powers)) + geom_line() + geom_hline(yintercept = .8, colour =  "red", size=1)
ggplot(df, aes(x = possible.fs, y = powers)) + geom_hline(yintercept = .8, colour =  "red", size=1) + geom_smooth(se=FALSE)+ labs(y="power", x = "number of farmers per input dealer") + ylim(0, 1) + annotate(geom="text", x=5, y=0.775, label="target power value of 0.8", color="red")
dev.off()
