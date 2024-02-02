rN1=rnorm(100000)
rN2=rnorm(100000)
rN3=rnorm(100000)
rN4=rnorm(100000)

rU1=runif(100000)
rU2=runif(100000)
rU3=runif(100000)
rU4=runif(100000)
rU5=runif(100000)
rU6=runif(100000)
rU7=runif(100000)
rU8=runif(100000)

### Normals
estimates=read.csv("output/MMNL_swiss_panel_all_normal_estimates.csv",row.names=1)
beta=estimates[,1]
names(beta)=row.names(estimates)
tt_Normal=(beta["b_tt_mu"]+beta["b_tt_sig"]*rN1)
tc_Normal=(beta["b_tc_mu"]+beta["b_tc_sig"]*rN2)
hw_Normal=(beta["b_hw_mu"]+beta["b_hw_sig"]*rN3)
ch_Normal=(beta["b_ch_mu"]+beta["b_ch_sig"]*rN4)

### Uniforms
estimates=read.csv("output/MMNL_swiss_panel_all_uniform_estimates.csv",row.names=1)
beta=estimates[,1]
names(beta)=row.names(estimates)
tt_Uniform=(beta["b_tt_a"]+beta["b_tt_b"]*rU1)
tc_Uniform=(beta["b_tc_a"]+beta["b_tc_b"]*rU2)
hw_Uniform=(beta["b_hw_a"]+beta["b_hw_b"]*rU3)
ch_Uniform=(beta["b_ch_a"]+beta["b_ch_b"]*rU4)

### Negative Lognormals
estimates=read.csv("output/MMNL_swiss_panel_all_negLN_estimates.csv",row.names=1)
beta=estimates[,1]
names(beta)=row.names(estimates)
tt_neg_Lognormal=-exp(beta["b_log_tt_mu"]+beta["b_log_tt_sig"]*rN1)
tc_neg_Lognormal=-exp(beta["b_log_tc_mu"]+beta["b_log_tc_sig"]*rN2)
hw_neg_Lognormal=-exp(beta["b_log_hw_mu"]+beta["b_log_hw_sig"]*rN3)
ch_neg_Lognormal=-exp(beta["b_log_ch_mu"]+beta["b_log_ch_sig"]*rN4)

### Triangular
estimates=read.csv("output/MMNL_swiss_panel_all_triangular_estimates.csv",row.names=1)
beta=estimates[,1]
names(beta)=row.names(estimates)
tt_Triangular=(beta["b_tt_a"]+beta["b_tt_b"]*(rU1+rU5))
tc_Triangular=(beta["b_tc_a"]+beta["b_tc_b"]*(rU2+rU6))
hw_Triangular=(beta["b_hw_a"]+beta["b_hw_b"]*(rU3+rU7))
ch_Triangular=(beta["b_ch_a"]+beta["b_ch_b"]*(rU4+rU8))

plot(ecdf(tt_Normal),col=1,main="Travel time coefficient",xlab="beta",ylim=c(0,1))
lines(ecdf(tt_neg_Lognormal),col=2)
lines(ecdf(tt_Triangular),col=3)
lines(ecdf(tt_Uniform),col=4)
abline(v=0,lty=2,col=2,lwd=2)
legend(0.05,0.8,c("Normal","neg_Lognormal","Triangular","Uniform"),col=c(1,2,3,4),lty=1,cex=1)


plot(ecdf(tc_Normal),col=1,main="Travel cost coefficient",xlab="beta",ylim=c(0,1))
lines(ecdf(tc_neg_Lognormal),col=2)
lines(ecdf(tc_Triangular),col=3)
lines(ecdf(tc_Uniform),col=4)
abline(v=0,lty=2,col=2,lwd=2)
legend(0.05,0.8,c("Normal","neg_Lognormal","Triangular","Uniform"),col=c(1,2,3,4),lty=1,cex=1)


plot(ecdf(hw_Normal),col=1,main="Headway coefficient",xlab="beta",ylim=c(0,1))
lines(ecdf(hw_neg_Lognormal),col=2)
lines(ecdf(hw_Triangular),col=3)
lines(ecdf(hw_Uniform),col=4)
abline(v=0,lty=2,col=2,lwd=2)
legend(0.05,0.8,c("Normal","neg_Lognormal","Triangular","Uniform"),col=c(1,2,3,4),lty=1,cex=1)

plot(ecdf(ch_Normal),col=1,main="Interchanges coefficient",xlab="beta",ylim=c(0,1))
lines(ecdf(ch_neg_Lognormal),col=2)
lines(ecdf(ch_Triangular),col=3)
lines(ecdf(ch_Uniform),col=4)
abline(v=0,lty=2,col=2,lwd=2)
legend(0.05,0.8,c("Normal","neg_Lognormal","Triangular","Uniform"),col=c(1,2,3,4),lty=1,cex=1)




