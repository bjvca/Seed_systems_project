set.seed(12345)

T1 <- sample(rep(c(1,0),212))
dta <- data.frame(T1)

dta$T2[dta$T1==1] <-sample(c(rep(1,140),rep(0,72)))
dta$T2[dta$T1==0] <-sample(c(rep(0,140),rep(1,72)))

dta$T3[dta$T1==1 & dta$T2==1] <-sample(c(rep(1,36),rep(0,104)))
dta$T3[dta$T1==1 & dta$T2==0] <-sample(c(rep(1,36),rep(0,36)))
dta$T3[dta$T1==0 & dta$T2==0] <-sample(c(rep(1,36),rep(0,104)))
dta$T3[dta$T1==0 & dta$T2==1] <-sample(c(rep(1,36),rep(0,36)))


dta$T4[dta$T1==1 & dta$T2==1 & dta$T3 == 0] <-sample(c(rep(1,18),rep(0,86)))
dta$T4[dta$T1==1 & dta$T2==0 & dta$T3 == 0] <-sample(c(rep(1,18),rep(0,18)))
dta$T4[dta$T1==0 & dta$T2==0 & dta$T3 == 0] <-sample(c(rep(1,18),rep(0,86)))
dta$T4[dta$T1==0 & dta$T2==1 & dta$T3 == 0] <-sample(c(rep(1,18),rep(0,18)))
dta$T4[dta$T1==1 & dta$T2==1 & dta$T3 == 1] <-sample(c(rep(1,18),rep(0,18)))
dta$T4[dta$T1==1 & dta$T2==0 & dta$T3 == 1] <-sample(c(rep(1,18),rep(0,18)))
dta$T4[dta$T1==0 & dta$T2==0 & dta$T3 == 1] <-sample(c(rep(1,18),rep(0,18)))
dta$T4[dta$T1==0 & dta$T2==1 & dta$T3 == 1] <-sample(c(rep(1,18),rep(0,18)))


dta$y <- rnorm(424, mean=10, sd=2) 

summary(lm(y~T1*T2*T3*T4,data=dta))
