########

#including remoteness

mid_adoption_rem <-  array(NA,dim=c(4,15,5))
#loop here over outcomes c("mid_adoption_onfield","mid_Land_Races","mid_Bought_from_agro_input_shop")
outcomes <- c("mid_adoption_onfield","mid_Land_Races","mid_farmer_saved_seed", "mid_Bought_from_agro_input_shop","index_mid")
outcomes_base <- c("adoption_onfield","Land_Races","farmer_saved_seed","Bought_from_agro_input_shop","index_base" )
for (i in 1:length(outcomes)) {
  ols <- lm(as.formula(paste(paste(paste(paste(outcomes[i],"treatment*clearing*training",sep="~"),outcomes_base[i],sep="+"),"Check2.check.maize.q9",sep="+"),"Check2.check.maize.q9*treatment",sep="+")),data=all)
  
  vcov_cluster <- vcovCR(ols,cluster=all$shop_ID,type="CR0")
  coef_test(ols, vcov_cluster)
  mid_adoption_rem[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  mid_adoption_rem[2,1,i] <- coef_test(ols, vcov_cluster)$SE[2]
  mid_adoption_rem[3,1,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  mid_adoption_rem[4,1,i] <- nobs(ols)
  
  mid_adoption_rem[1,6,i] <- coef_test(ols, vcov_cluster)$beta[6]
  mid_adoption_rem[2,6,i] <- coef_test(ols, vcov_cluster)$SE[6]
  mid_adoption_rem[3,6,i] <- coef_test(ols, vcov_cluster)$p_Satt[6]
  mid_adoption_rem[4,6,i] <- nobs(ols)
  
  mid_adoption_rem[1,10,i] <- coef_test(ols, vcov_cluster)$beta[10]
  mid_adoption_rem[2,10,i] <- coef_test(ols, vcov_cluster)$SE[10]
  mid_adoption_rem[3,10,i] <- coef_test(ols, vcov_cluster)$p_Satt[10]
  mid_adoption_rem[4,10,i] <- nobs(ols)
  
  # ols <- lm(as.formula(paste(outcomes[i],paste("treatment*clearing*training",outcomes_base[i],sep="+"), sep="~")),data=all[all$adoption_onfield==1,])
  # vcov_cluster <- vcovCR(ols,cluster=all[all$adoption_onfield==1,]$shop_ID,type="CR0")
  # coef_test(ols, vcov_cluster)
  # mid_adoption_rem[1,2,i] <- coef_test(ols, vcov_cluster)$beta[2]
  # mid_adoption_rem[2,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  # mid_adoption_rem[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  # mid_adoption_rem[4,2,i] <- nobs(ols)
}

save(mid_adoption_rem,file=paste(path,"papers/learning_failures/code/output/mid_adoption_rem.Rdata",sep="/"))

########
