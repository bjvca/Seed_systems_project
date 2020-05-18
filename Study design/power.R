rm(list=ls())

######################################################
########Power analysis for the standard design########
######################################################

if (Sys.info()['sysname'] =="Windows") {
  path <- "C:/users/u0127963/Desktop/PhD/Seed_systems_project"
} else {
  path <- "/home/bjvca/Dropbox (IFPRI)/Seed_systems_project"
}

stack_dealers <- read.csv(paste(path,"stack surveys/data/agro_input_dealers.csv", sep ="/"))
stack_farmers <- read.csv(paste(path,"stack surveys/data/farmers.csv", sep ="/"))

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
stack_dealers$quantitysold <- stack_dealers$hh.maize.seed.1..q22+stack_dealers$hh.maize.seed.2..q22+stack_dealers$hh.maize.seed.3..q22

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
# possible.ns <- seq(from=2000, to=3000, by=100)     # The sample sizes we'll be considering
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
#     Y0 <-  rnorm(n=N, mean=926.9231, sd=733.1028)              # control potential outcome
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
# ######################################################
# ########Power analysis for the standard design########
# #######Y0 not normal distribution but real data#######
# ######################################################
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
#     tau <- 0.08759494                         # Hypothesize treatment effect
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
########Power analysis for covariate control##########
######################################################
# 
# possible.ns <- seq(from=1000, to=5000, by=500)     # The sample sizes we'll be considering
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
#     quantitysold_baseline <- as.numeric(Y0 +  rnorm(length(Y0), mean = 0, sd = sd(stack_dealers$quantitysold, na.rm=T)))
# 
#     tau <- 146.6206                                   # Hypothesize treatment effect
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
###binary
#stack_farmers$seedquality_binary_baseline <- as.numeric(stack_farmers$seedquality_binary +  rbinom(n=length(stack_farmers$seedquality_binary), size=1, prob=0.2) >= 1)
#cor(stack_farmers$seedquality_binary,stack_farmers$seedquality_binary_baseline,  use = "complete.obs")
#change prob until cor is what we want

possible.ns <- seq(from=1000, to=4000, by=200)     # The sample sizes we'll be considering
powers <- rep(NA, length(possible.ns))           # Empty object to collect simulation estimates
alpha <- 0.05                                    # Standard significance level
sims <- 500                                      # Number of simulations to conduct for each N

#### Outer loop to vary the number of subjects ####
for (j in 1:length(possible.ns)){
  N <- possible.ns[j]                              # Pick the jth value for N
  
  significant.experiments <- rep(NA, sims)         # Empty object to count significant experiments
  
  #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){             # control potential outcome
    Y0 <- sample(stack_farmers$seedquality_binary, size = N, replace = TRUE)             # control potential outcome
    
    #create baseline data
    seedquality_binary_baseline <- as.numeric(Y0 +  rbinom(n=length(Y0), size=1, prob=0.75) >= 1)
    
    tau <- 0.08759494                                   # Hypothesize treatment effect
    Y1 <- Y0 + tau                                 # treatment potential outcome
    Z.sim <- rbinom(n=N, size=1, prob=.5)          # Do a random assignment
    Y.sim <- Y1*Z.sim + Y0*(1-Z.sim)               # Reveal outcomes according to assignment
    fit.sim <- lm(Y.sim ~ Z.sim + seedquality_binary_baseline)                   # Do analysis (Simple regression)
    #fit.sim <- lm(Y.sim ~ information +base_out, data=baseline_sim) # Do analysis (Simple regression)
    p.value <- summary(fit.sim)$coefficients[2,4]  # Extract p-values
    significant.experiments[i] <- (p.value <= alpha) # Determine significance according to p <= 0.05
  }
  
  powers[j] <- mean(significant.experiments)       # store average success rate (power) for each N
}
plot(possible.ns, powers, ylim=c(0,1))
cbind(possible.ns, powers)
