rm(list=ls())
#install.packages("randomizr")
#install.packages("fabricatr")
#install.packages("data.table")
#install.packages("mvnfast")
library(randomizr)    #randomizr package for complete random assignment
library(fabricatr)
library(data.table)
library(mvnfast)

######################################################
########Power analysis for the standard design########
######################################################

if (Sys.info()['sysname'] =="Windows") {
  path <- "C:/users/u0127963/Desktop/PhD/Seed_systems_project"
} else {
  path <- "/home/bjvca/Dropbox (IFPRI)/Seed_systems_project"
}
### this is executed in the /report subdirectory, need to ..
path <- strsplit(getwd(), "/Study design")[[1]]

stack_dealers <- read.csv(paste(path,"stack surveys/data/agro_input_dealers.csv", sep ="/"))
stack_farmers <- read.csv(paste(path,"stack surveys/data/farmers.csv", sep ="/"))

###changing the level of randomization
stack_farmers$id.agro1[stack_farmers$id.agro1==""] <- NA
stack_farmers$id.agro1[stack_farmers$id.agro1=="n/a"] <- NA
stack_farmers$id.agro1 <- as.character(stack_farmers$id.agro1)

stack_farmers$id.agro2[stack_farmers$id.agro2==""] <- NA
stack_farmers$id.agro2[stack_farmers$id.agro2=="n/a"] <- NA
stack_farmers$id.agro2 <- as.character(stack_farmers$id.agro2)

stack_farmers$id.agro3[stack_farmers$id.agro3==""] <- NA
stack_farmers$id.agro3[stack_farmers$id.agro3=="n/a"] <- NA
stack_farmers$id.agro3 <- as.character(stack_farmers$id.agro3)

stack_farmers$id_inputdealer <- ifelse(is.na(stack_farmers$id.agro1), ifelse(is.na(stack_farmers$id.agro2), ifelse(is.na(stack_farmers$id.agro3), NA, stack_farmers$id.agro3), stack_farmers$id.agro2), stack_farmers$id.agro1)
sum(is.na(stack_farmers$id_inputdealer))

#stack_farmers[complete.cases(stack_farmers[ , 794:794]),]

#Z.sim <- cluster_ra(clusters = stack_farmers$id_inputdealer)

#Z <- cluster_ra(clusters = stack_farmers$id_inputdealer)
#table(Z.sim, complete.cases(stack_farmers$id_inputdealer))
#table(Z.sim, stack_farmers$id_inputdealer[complete.cases(stack_farmers$id_inputdealer)])
#table(Z.sim, stack_farmers$id_inputdealer[!is.na(stack_farmers$id_inputdealer)])

#stack_farmers$yield_kg_per_acre <- stack_farmers$yield_kg_per_acre[!is.na(stack_farmers$id_inputdealer)]
#stack_farmers[complete.cases(stack_farmers[ , 792]),]
#na.omit(stack_farmers$id_inputdealer)
#stack_farmers$id_inputdealer[complete.cases(stack_farmers$id_inputdealer), ]

stack_farmers <- subset(stack_farmers, !is.na(id_inputdealer))

###YIELD###
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

###INPUT USE###
#transform
stack_farmers$hh.maize.maizen.q48[stack_farmers$hh.maize.maizen.q48==98] <- NA
stack_farmers$hh.maize.maizen.q48[stack_farmers$hh.maize.maizen.q48=="l"] <- NA #made "other" NA

#make input use variable
stack_farmers$inputuse_binary <- (stack_farmers$hh.maize.maizen.q48 == "a") | (stack_farmers$hh.maize.maizen.q48 == "b")| (stack_farmers$hh.maize.maizen.q48 == "c")| (stack_farmers$hh.maize.maizen.q48 == "d")| (stack_farmers$hh.maize.maizen.q48 == "e")| (stack_farmers$hh.maize.maizen.q48 == "f")| (stack_farmers$hh.maize.maizen.q48 == "g")| (stack_farmers$hh.maize.maizen.q48 == "h")| (stack_farmers$hh.maize.maizen.q48 == "i")| (stack_farmers$hh.maize.maizen.q48 == "j")

#standard deviations and means
sd(stack_farmers$inputuse_binary, na.rm=TRUE)
mean(stack_farmers$inputuse_binary, na.rm=TRUE)

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

#make quantity sold variable
#stack_dealers$quantitysold <- stack_dealers$hh.maize.seed.1..q22+stack_dealers$hh.maize.seed.2..q22+stack_dealers$hh.maize.seed.3..q22 #not good because 65 of 78 NA's (67 after trimming)
#stack_dealers$quantitysold <- stack_dealers$hh.maize.seed.1..q22+stack_dealers$hh.maize.seed.2..q22 #not good because 46 of 78 NA's (48 after trimming)
stack_dealers$quantitysold <- stack_dealers$hh.maize.seed.1..q22 #only 12 NA's, 15 after trimming

stack_dealers <- trim("quantitysold", stack_dealers)

#standard deviations and means
sd(stack_dealers$quantitysold, na.rm=TRUE)
mean(stack_dealers$quantitysold, na.rm=TRUE)

###REPUTATION###
#make reputation variable
stack_dealers$reputation <- (stack_dealers$hh.maize.q83 > 4) #median is 5

#standard deviations and means
sd(stack_dealers$reputation, na.rm=TRUE)
mean(stack_dealers$reputation, na.rm=TRUE)

###SEED QUALITY###
#transform
stack_farmers$hh.maize.agro1.q108j[stack_farmers$hh.maize.agro1.q108j=="n/a"] <- NA
stack_farmers$hh.maize.agro1.q108j <- as.numeric(as.character(stack_farmers$hh.maize.agro1.q108j))

#make seed quality variable
stack_farmers$seedquality_binary <- (stack_farmers$hh.maize.agro1.q108j > 4) #median is 4

#standard deviations and means
sd(stack_farmers$seedquality_binary, na.rm=TRUE)
mean(stack_farmers$seedquality_binary, na.rm=TRUE)

# ##################################################
# possible.ns <- seq(from=3500, to=5000, by=100)     # The sample sizes we'll be considering
# powers <- rep(NA, length(possible.ns))           # Empty object to collect simulation estimates
# alpha <- 0.05                                    # Standard significance level
# sims <- 500                                      # Number of simulations to conduct for each N
# 
# #### Outer loop to vary the number of subjects ####
# for (j in 1:length(possible.ns)){
#   N <- possible.ns[j]                              # Pick the jth value for N
# 
#   significant.experiments <- rep(NA, sims)         # Empty object to count significant experiments
# 
#   #### Inner loop to conduct experiments "sims" times over for each N ####
#   for (i in 1:sims){
#     Y0 <-  rnorm(n=N, mean=265.4762, sd=302.7211)              # control potential outcome
#     tau <- Y0*0.1                                # Hypothesize treatment effect
#     Y1 <- Y0 + tau                                 # treatment potential outcome
#     Z.sim <- rbinom(n=N, size=1, prob=.5)          # Do a random assignment
#     Y.sim <- Y1*Z.sim + Y0*(1-Z.sim)               # Reveal outcomes according to assignment
#     fit.sim <- lm(Y.sim ~ Z.sim)                   # Do analysis (Simple regression)
#     p.value <- summary(fit.sim)$coefficients[2,4]  # Extract p-values
#     significant.experiments[i] <- (p.value <= alpha) # Determine significance according to p <= 0.05
#   }
# 
#   powers[j] <- mean(significant.experiments)       # store average success rate (power) for each N
# }
# plot(possible.ns, powers, ylim=c(0,1))
# cbind(possible.ns, powers)
# 
######################################################
########Power analysis for the standard design########
#######Y0 not normal distribution but real data#######
######################################################

possible.ns <- seq(from=500, to=1500, by=100)     # The sample sizes we'll be considering
powers <- rep(NA, length(possible.ns))           # Empty object to collect simulation estimates
alpha <- 0.05                                    # Standard significance level
sims <- 500                                      # Number of simulations to conduct for each N

#### Outer loop to vary the number of subjects ####
for (j in 1:length(possible.ns)){
  N <- possible.ns[j]                              # Pick the jth value for N

  significant.experiments <- rep(NA, sims)         # Empty object to count significant experiments

  #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){             # control potential outcome
    Y0 <- sample(stack_farmers$yield_kg_per_acre, size = N, replace = TRUE)             # control potential outcome
    tau <- 36.38                          # Hypothesize treatment effect
    Y1 <- Y0 + tau                                 # treatment potential outcome
    #randomize(stack_farmers, group = c("1", "0"), block = stack_farmers$id_inputdealer)
    #Z.sim <- rbinom(n=N, size=1, prob=.5)          # Do a random assignment
    #Z.sim <- cluster_ra(clusters = stack_farmers$id_inputdealer)
    Z.sim <- cluster_ra(clusters = stack_farmers$id_inputdealer)
    Y.sim <- Y1*Z.sim + Y0*(1-Z.sim)               # Reveal outcomes according to assignment
    fit.sim <- lm(Y.sim ~ Z.sim)                   # Do analysis (Simple regression)
    p.value <- summary(fit.sim)$coefficients[2,4]  # Extract p-values
    significant.experiments[i] <- (p.value <= alpha) # Determine significance according to p <= 0.05
  }

  powers[j] <- mean(significant.experiments)       # store average success rate (power) for each N
}
plot(possible.ns, powers, ylim=c(0,1))
cbind(possible.ns, powers)

# #####################################################
# #######Power analysis for covariate control##########
# #####################################################
# 
# possible.ns <- seq(from=500, to=1500, by=100)     # The sample sizes we'll be considering
# powers <- rep(NA, length(possible.ns))           # Empty object to collect simulation estimates
# alpha <- 0.05                                    # Standard significance level
# sims <- 500                                      # Number of simulations to conduct for each N
# 
# #### Outer loop to vary the number of subjects ####
# for (j in 1:length(possible.ns)){
#   N <- possible.ns[j]                              # Pick the jth value for N
# 
#   significant.experiments <- rep(NA, sims)         # Empty object to count significant experiments
# 
#   #### Inner loop to conduct experiments "sims" times over for each N ####
#   for (i in 1:sims){             # control potential outcome
#     Y0 <- sample(stack_dealers$quantitysold, size = N, replace = TRUE)             # control potential outcome
# 
#     #create baseline data
#     quantitysold_baseline <- correlate(given = Y0, rho = 0.7, draw_count, mean = mean(Y0, na.rm=TRUE))
# 
#     tau <- 60.54422                                    # Hypothesize treatment effect
#     Y1 <- Y0 + tau                                 # treatment potential outcome
#     Z.sim <- rbinom(n=N, size=1, prob=.5)          # Do a random assignment
#     Y.sim <- Y1*Z.sim + Y0*(1-Z.sim)               # Reveal outcomes according to assignment
#     fit.sim <- lm(Y.sim ~ Z.sim + quantitysold_baseline)                   # Do analysis (Simple regression)
#    #fit.sim <- lm(Y.sim ~ information +base_out, data=baseline_sim) # Do analysis (Simple regression)
#     p.value <- summary(fit.sim)$coefficients[2,4]  # Extract p-values
#     significant.experiments[i] <- (p.value <= alpha) # Determine significance according to p <= 0.05
#   }
# 
#   powers[j] <- mean(significant.experiments)       # store average success rate (power) for each N
# }
# plot(possible.ns, powers, ylim=c(0,1))
# cbind(possible.ns, powers)
# 
# #correlation?
# stack_dealers$quantitysold_baseline <- as.numeric(stack_dealers$quantitysold +  rnorm(length(stack_dealers$quantitysold), mean = 0, sd = sd(stack_dealers$quantitysold, na.rm=T)))
# cor(stack_dealers$quantitysold,stack_dealers$quantitysold_baseline,  method = "pearson", use = "complete.obs")
# 
####binary
# 
# possible.ns <- seq(from=500, to=2500, by=100)     # The sample sizes we'll be considering
# powers <- rep(NA, length(possible.ns))           # Empty object to collect simulation estimates
# alpha <- 0.05                                    # Standard significance level
# sims <- 500                                      # Number of simulations to conduct for each N
# 
# #### Outer loop to vary the number of subjects ####
# for (j in 1:length(possible.ns)){
#   N <- possible.ns[j]                              # Pick the jth value for N
# 
#   significant.experiments <- rep(NA, sims)         # Empty object to count significant experiments
# 
#   #### Inner loop to conduct experiments "sims" times over for each N ####
#   for (i in 1:sims){             # control potential outcome
#     Y0 <- sample(stack_farmers$seedquality_binary, size = N, replace = TRUE)             # control potential outcome
# 
#     #create baseline data
#     seedquality_binary_baseline <- correlate(given = Y0, rho = 0.7, draw_binary, prob = mean(Y0, na.rm=TRUE))
#     
#     tau <- 0.08759494                                    # Hypothesize treatment effect
#     Y1 <- Y0 + tau                                 # treatment potential outcome
#     Z.sim <- rbinom(n=N, size=1, prob=.5)          # Do a random assignment
#     Y.sim <- Y1*Z.sim + Y0*(1-Z.sim)               # Reveal outcomes according to assignment
#     fit.sim <- lm(Y.sim ~ Z.sim + seedquality_binary_baseline)                   # Do analysis (Simple regression)
#     #fit.sim <- lm(Y.sim ~ information +base_out, data=baseline_sim) # Do analysis (Simple regression)
#     p.value <- summary(fit.sim)$coefficients[2,4]  # Extract p-values
#     significant.experiments[i] <- (p.value <= alpha) # Determine significance according to p <= 0.05
#   }
# 
#   powers[j] <- mean(significant.experiments)       # store average success rate (power) for each N
# }
# plot(possible.ns, powers, ylim=c(0,1), type = "l")
# abline(h = .8, col = "red")
# 
# cbind(possible.ns, powers)
# 
# ######################################################
# ########Power analysis for multiple treatments########
# #############real data but no covartiates#############
# ######################################################
# 
# possible.ns <- seq(from=5000, to=15000, by=1000)     # The sample sizes we'll be considering
# power.atleastone <- rep(NA, length(possible.ns))
# power.bothtreatments <- rep(NA, length(possible.ns))
# alpha <- 0.1  #(one-tailed test at .05 level)
# sims <- 100                                      # Number of simulations to conduct for each N
# 
# #### Outer loop to vary the number of subjects ####
# for (j in 1:length(possible.ns)){
#   N <- possible.ns[j]                              # Pick the jth value for N
# 
#   p.T1vsC <- rep(NA, sims)
#   p.T2vsC <- rep(NA, sims)
#   p.T3vsC <- rep(NA, sims)
#   c.T1vsC <- rep(NA, sims)
#   c.T2vsC <- rep(NA, sims)
#   c.T3vsC <- rep(NA, sims)
# 
#   #### Inner loop to conduct experiments "sims" times over for each N ####
#   for (i in 1:sims){             # control potential outcome
#     Y0 <- sample(stack_farmers$seedquality_binary, size = N, replace = TRUE)             # control potential outcome
#     tau_1 <- 0.08759494                         # Hypothesize treatment effect
#     tau_2 <- 0.08759494
#     tau_3 <- 0.08759494
#     Y1 <- Y0 + tau_1
#     Y2 <- Y0 + tau_2
#     Y3 <- Y0 + tau_3
#     Z.sim <- complete_ra(N=N, num_arms=4)
#     Y.sim <- Y0*(Z.sim=="T4") + Y1*(Z.sim=="T1") + Y2*(Z.sim=="T2") + Y3*(Z.sim=="T3")
#     frame.sim <- data.frame(Y.sim, Z.sim)
#     fit.T1vsC.sim <- lm(Y.sim ~ Z.sim=="T1", data=subset(frame.sim, Z.sim=="T1" | Z.sim=="T4"))
#     fit.T2vsC.sim <- lm(Y.sim ~ Z.sim=="T2", data=subset(frame.sim, Z.sim=="T2" | Z.sim=="T4"))
#     fit.T3vsC.sim <- lm(Y.sim ~ Z.sim=="T3", data=subset(frame.sim, Z.sim=="T3" | Z.sim=="T4"))
# 
#     ### Need to capture coefficients and pvalues (one-tailed tests, so signs are important)
#     c.T1vsC[i] <- summary(fit.T1vsC.sim)$coefficients[2,1]
#     c.T2vsC[i] <- summary(fit.T2vsC.sim)$coefficients[2,1]
#     c.T3vsC[i] <- summary(fit.T3vsC.sim)$coefficients[2,1]
#     p.T1vsC[i] <- summary(fit.T1vsC.sim)$coefficients[2,4]
#     p.T2vsC[i] <- summary(fit.T2vsC.sim)$coefficients[2,4]
#     p.T3vsC[i] <- summary(fit.T3vsC.sim)$coefficients[2,4]
#   }
#   power.atleastone[j] <- mean(c.T1vsC>0 & c.T2vsC>0 & c.T3vsC>0 & (p.T1vsC < alpha/2 | p.T2vsC < alpha/2 | p.T3vsC < alpha/2))
#   power.bothtreatments[j] <- mean(c.T1vsC>0 & c.T2vsC>0 & c.T3vsC>0 & p.T1vsC < alpha/2 & p.T2vsC < alpha/2 & p.T3vsC < alpha/2)
#   print(j)
# }
# 
# #plot(possible.ns, power.atleastone, ylim=c(0,1))
# 
# cbind(possible.ns, power.atleastone)
# cbind(possible.ns, power.bothtreatments)
# #cbind(possible.ns, power.fullranking)