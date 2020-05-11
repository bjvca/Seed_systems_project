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

#standard deviations and means
stack_dealers$hh.maize.seed.1..q22 <- as.numeric(as.character(stack_dealers$hh.maize.seed.1..q22))
sd(stack_dealers$hh.maize.seed.1..q22, na.rm=TRUE)
mean(stack_dealers$hh.maize.seed.1..q22, na.rm=TRUE)

#stack_dealers$hh.maize.q83_binary <- (stack_dealers$hh.maize.q83 > 4)
#sd(stack_dealers$hh.maize.q83_binary, na.rm=TRUE)
#mean(stack_dealers$hh.maize.q83_binary, na.rm=TRUE)

stack_farmers$hh.maize.maizen.q64[stack_farmers$hh.maize.maizen.q64==999] <- NA
sd(stack_farmers$hh.maize.maizen.q64, na.rm=TRUE)
mean(stack_farmers$hh.maize.maizen.q64, na.rm=TRUE)

stack_farmers$hh.maize.agro1.q108j[stack_farmers$hh.maize.agro1.q108j=="n/a"] <- NA
stack_farmers$hh.maize.agro1.q108j <- as.numeric(as.character(stack_farmers$hh.maize.agro1.q108j))
sd(stack_farmers$hh.maize.agro1.q108j, na.rm=TRUE)
mean(stack_farmers$hh.maize.agro1.q108j, na.rm=TRUE)



possible.ns <- seq(from=100, to=1000, by=50)     # The sample sizes we'll be considering
powers <- rep(NA, length(possible.ns))           # Empty object to collect simulation estimates
alpha <- 0.05                                    # Standard significance level
#sims <- 500                                      # Number of simulations to conduct for each N
sims <- 500                                      # Number of simulations to conduct for each N


#### Outer loop to vary the number of subjects ####
for (j in 1:length(possible.ns)){
  N <- possible.ns[j]                              # Pick the jth value for N
  
  significant.experiments <- rep(NA, sims)         # Empty object to count significant experiments
  
  #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){
    Y0 <-  rnorm(n=N, mean=3.793162, sd=0.9739524)             # control potential outcome
#    Y0 <-  rnorm(n=N, mean=60, sd=20)
    tau <- 3.793162*0.06                                       # Hypothesize treatment effect of 0.5 x mean
#    tau <- 5   
    Y1 <- Y0 + tau                                 # treatment potential outcome
    Z.sim <- rbinom(n=N, size=1, prob=.5)          # Do a random assignment
    Y.sim <- Y1*Z.sim + Y0*(1-Z.sim)               # Reveal outcomes according to assignment
    fit.sim <- lm(Y.sim ~ Z.sim)                   # Do analysis (Simple regression)
    p.value <- summary(fit.sim)$coefficients[2,4]  # Extract p-values
    significant.experiments[i] <- (p.value <= alpha) # Determine significance according to p <= 0.05
  }
  
  powers[j] <- mean(significant.experiments)       # store average success rate (power) for each N
}
plot(possible.ns, powers, ylim=c(0,1))
cbind(possible.ns, powers)