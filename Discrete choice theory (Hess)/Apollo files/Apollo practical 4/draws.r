draws=runif(10000)

draws_N_0_1=qnorm(draws)

mean(draws_N_0_1)
sd(draws_N_0_1)

draws_N_neg2_1=-2+1*draws_N_0_1
mean(draws_N_neg2_1)
sd(draws_N_neg2_1)

LNdraws=exp(draws_N_neg2_1)
mean(LNdraws)
sd(LNdraws)

exp(-2+(1^2)/2)
exp(-2+(1^2)/2)*sqrt(exp(1^2)-1)
