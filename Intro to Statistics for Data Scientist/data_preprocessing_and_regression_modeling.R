#dir = "/Users/SophieZheng/Google Drive/introduction to statistics for data scientists - MSBA 6120/project data suggestion/lending club"
#setwd(dir)

#before you run the code, please run 1. install.packages("sampling"); 2. library("sampling", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")

library(readxl)
library(sampling)

loan <- read_excel("LoanStats3b.xlsx",col_names = TRUE)
loan <- loan[!sapply(loan,function(var) (length(unique(var)) ==1) )]
attach(loan)
#loan = read.csv("LoanStats3d.csv",header = TRUE)

#...randomly sample the raw data. Note: our sample size is 10,000.
#...using Stratified sampling strategy
#...Proportionate allocation

#0. handling the missing values on column "issue_d", by deleting them, for the purpose to use strata function below
naomitted_loan = loan[!is.na(loan$issue_d),]
#nrow(naomitted_loan) =>  data size now becomes 52899

#1. get the proportion for each month in both 2012 and 2013; using 10001.5 in order to get total of proportionated size to be 10000
proportionated_strata_sample_size = as.numeric(round(c(table(naomitted_loan["issue_d"])/nrow(naomitted_loan["issue_d"])) * 10001.5))

#2. random sampling from each strata
#...Note: using strata function provided by sampling pakcage. 
set.seed(1)
strata = strata(naomitted_loan,stratanames=c("issue_d"),size=proportionated_strata_sample_size, method="srswor")
loan_10000 = getdata(naomitted_loan, strata)

detach(loan)
attach(loan_10000)

library(xlsx)
write.csv( loan_10000, file = "loan_10000.csv")

plot(annual_inc/1000, int_rate)

summary(int_rate)

#check if home_ownership has a relationship with int_rate
summary(aov(int_rate ~ home_ownership))
summary(aov(int_rate ~ purpose))

detach(loan_10000)
loan_10000$purposeF = factor(loan_10000$purpose)
loan_10000$home_ownershipF = factor(loan_10000$home_ownership)
loan_10000$pub_rec_bankruptciesF=factor(loan_10000$pub_rec_bankruptcies)
loan_10000$bc_open_to_buy[is.na(loan_10000$bc_open_to_buy)] <- mean(loan_10000$bc_open_to_buy, na.rm = TRUE)
attach(loan_10000)
loan_10000 = within(loan_10000, purposeF = relevel(purposeF, ref = "vacation"))
purposeF = relevel(purposeF, ref = "renewable_energy")
home_ownershipF = relevel(home_ownershipF, ref="NONE")

#test different regression models
m = lm(int_rate~loan_amnt+tot_hi_cred_lim+tot_cur_bal+home_ownership) #0.1716, R-square
m = lm(int_rate~loan_amnt+tot_hi_cred_lim+tot_cur_bal+home_ownershipF) #home_ownership is not significant
m = lm(int_rate~loan_amnt+tot_hi_cred_lim+tot_cur_bal+purposeF) #0.2147, R-square
m = lm(int_rate~loan_amnt+tot_hi_cred_lim+tot_cur_bal+purposeF+percent_bc_gt_75) #0.3038 R-square
m = lm(int_rate~loan_amnt+tot_hi_cred_lim+tot_cur_bal+purposeF+percent_bc_gt_75+installment) #0.3156 R-square
m = lm(int_rate~loan_amnt+tot_hi_cred_lim+tot_cur_bal+purposeF+percent_bc_gt_75+installment+revol_util) #0.3293 R-square
m = lm(int_rate~loan_amnt+tot_hi_cred_lim+tot_cur_bal+purposeF+percent_bc_gt_75+installment+revol_util+bc_open_to_buy) #0.3392

m = lm(int_rate~loan_amnt+tot_cur_bal+tot_hi_cred_lim+purposeF+pub_rec_bankruptcies+percent_bc_gt_75+installment+revol_util+bc_open_to_buy) #0.3409
irate=scale(int_rate)
m = lm(int_rate~loan_amnt+log(avg_cur_bal)+log(tot_hi_cred_lim)+purposeFF+percent_bc_gt_75+installment+revol_util+log(bc_open_to_buy+1)) #0.358 (without log in bc_open_tobuy: 0.3546)
m = lm(int_rate~loan_amnt+avg_cur_bal+tot_hi_cred_lim+purposeF+pub_rec_bankruptcies+percent_bc_gt_75+installment+revol_util+bc_open_to_buy)#0.3468 but larger S
a = num_actv_bc_tl+num_actv_rev_tl+num_bc_sats+num_bc_tl+num_il_tl+num_op_rev_tl+num_rev_accts+num_rev_tl_bal_gt_0+num_sats+num_tl_op_past_12m+num_tl_120dpd_2m+num_tl_30dpd+num_tl_90g_dpd_24m+num_accts_ever_120_pd
a[is.na(a)] = mean(a,na.rm=TRUE)
b=(a+total_acc)*(1-revol_util)
m = lm(int_rate~b)
b
summary(m)

m.stres = rstandard(m)
qqnorm(m.stres)
qqline(m.stres,col="red")
shapiro.test(m.stres)
plot(m$fitted.values, m.stres, pch=16)
summary(lm(m.stres~m$fitted.values))
hist(int_rate)
loan_1000$purposeFF = factor(loan_1000$purpose)
attach(loan_1000)
