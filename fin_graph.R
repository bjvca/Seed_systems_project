path <- getwd()
library(ggplot2)

base_farmer <- read.csv(paste(path,"baseline/data/farmer/public/baseline_farmers.csv",sep="/"))
mid_farmer <- read.csv(paste(path,"midline/data/farmer/public/midline.csv",sep="/"))
end_farmer <- read.csv(paste(path,"endline/data/farmer/public/endline.csv",sep="/"))


base_farmer$use_seed <- base_farmer$Check2.check.maize.q25a=="Yes"
mid_farmer$check.maize.q25a [mid_farmer$check.maize.q25a =="n/a"] <- NA
end_farmer$check.maize.q25a [end_farmer$check.maize.q25a =="n/a"] <- NA
mid_farmer$check.maize.q25a [mid_farmer$check.maize.q25a =="98"] <- NA
end_farmer$check.maize.q25a [end_farmer$check.maize.q25a =="98"] <- NA

mid_farmer$use_seed <- mid_farmer$check.maize.q25a=="Yes"
end_farmer$use_seed <- end_farmer$check.maize.q25a=="Yes"


#just merge treatment in baseline from midline
base_farmer <- merge(base_farmer, mid_farmer[c("farmer_ID","clearing")], by="farmer_ID")

base_farmer$round <- "baseline"
mid_farmer$round <- "midline"
end_farmer$round <- "endline"

summary(lm(use_seed~clearing,data=end_farmer))
summary(lm(use_seed~clearing,data=mid_farmer))

## out in long format
long <- rbind(base_farmer[c("use_seed","clearing","round")],mid_farmer[c("use_seed","clearing","round")],end_farmer[c("use_seed","clearing","round")])

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=TRUE,
                      conf.interval=.90, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

tgc <- summarySE(long, measurevar="use_seed", groupvars=c("round","clearing"))
# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.1) # move them .05 to the left and right

tgc$round <- factor(tgc$round , ordered = TRUE, levels = c("baseline", "midline","endline"))

ggplot(tgc, aes(x=round, y=use_seed, colour=clearing, group=clearing)) + 
  geom_errorbar(aes(ymin=use_seed-ci, ymax=use_seed+ci), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
  xlab("Survey round") +
  ylab("Seed Adoption") +
  scale_colour_hue(name="Supplement type",    # Legend label, use darker colors
                   breaks=c("OJ", "VC"),
                   labels=c("Orange juice", "Ascorbic acid"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("The effect of SeedAdvisor on seed adoption (farmer level)") +
  theme_bw() +
  theme(legend.justification=c(1,0),
        legend.position=c(1,0))               # Position legend in bottom right

### create a graph for ratings
base_ratings <- read.csv(paste(path,"baseline/data/agro_input/public/reviews_seed.csv",sep="/"))
mid_ratings <- read.csv(paste(path,"midline/data/agro_input/public/reviews_seed.csv",sep="/"))
end_ratings <- read.csv(paste(path,"endline/data//agro_input/public/reviews_seed.csv",sep="/"))

##step 1: create an indicator of which agro-input dealers are scored poor vs those that are scored good
##best done relative to catchement area mean

base_ratings$ind_poor <- base_ratings$general < base_ratings$general_av

ratings <- merge(base_ratings[c("shop_ID","general","ind_poor")],mid_ratings[c("shop_ID","general")], by="shop_ID")
names(ratings) <- c("shop_ID","baseline","ind_poor", "midline")
ratings <- merge(ratings,end_ratings[c("shop_ID","general","clearing")], by="shop_ID", all.y=TRUE)
names(ratings)[names(ratings) == 'general'] <- 'endline'


##put in long to be able to use summarySE function
ratings <- subset(ratings,clearing==TRUE)
ratings <- melt(ratings)
ratings <- subset(ratings, !is.na(value))

tgc <- summarySE(ratings, measurevar="value", groupvars=c("variable","ind_poor"))
tgc <- subset(tgc,!is.na(ind_poor))

# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.1) # move them .05 to the left and right

tgc$round <- factor(tgc$variable , ordered = TRUE, levels = c("baseline", "midline","endline"))

ggplot(tgc, aes(x=round, y=value, colour=ind_poor, group=ind_poor)) + 
  geom_errorbar(aes(ymin=value-ci, ymax=value+ci), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
  xlab("Survey round") +
  ylab("General Quality Score") +
  scale_colour_hue(name="Supplement type",    # Legend label, use darker colors
                   breaks=c("OJ", "VC"),
                   labels=c("Orange juice", "Ascorbic acid"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Heterogeneous response to Seed Advisor: poor (blue) vs good (red) dealers") +
  theme_bw() +
  theme(legend.justification=c(1,0),
        legend.position=c(1,0))               # Position legend in bottom right
