vhlss <- c("vhlss02.Rda", "vhlss04.Rda", "vhlss06.Rda", "vhlss08.Rda", "vhlss10.Rda", "vhlss12.Rda")

for (i in vhlss) {
  load(i)
}

prov_vhlss <- c("prov02_vhlss.Rda", "prov04_vhlss.Rda", "prov06_vhlss.Rda", "prov08_vhlss.Rda", "prov10_vhlss.Rda", "prov12_vhlss.Rda")

for (i in prov_vhlss) {
  load(i)
}

##############################################
# VHLSS REGRESSIONS - PROBABILITY OF WORKING #
##############################################

png("work_vhlss.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss02),
        weights = ~wt75,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov| minority + urban,
        subset(vhlss04),
        weights = ~wt45,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss06),
        weights = ~wt45,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss08),
        weights = ~wt9,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss10),
        weights = ~wt9,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss12),
        weights = ~wt9,
        vcov = ~tinh)  
))
legend("bottomleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

png("work_vhlss_s.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss02, south == 1),
        weights = ~wt75,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss04, south == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss06, south == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss08, south == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss10, south == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss12, south == 1),
        weights = ~wt9,
        vcov = ~tinh)
))
legend("topleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

png("work_vhlss_n.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss02, south == 0),
        weights = ~wt75,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss04, south == 0),
        weights = ~wt45,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss06, south == 0),
        weights = ~wt45,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss08, south == 0),
        weights = ~wt9,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss10, south == 0),
        weights = ~wt9,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss12, south == 0),
        weights = ~wt9,
        vcov = ~tinh)
))
legend("topleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

png("work_vhlss_cas.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(killed_tot_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss02),
        weights = ~wt75,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(killed_tot_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov| minority + urban,
        subset(vhlss04),
        weights = ~wt45,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(killed_tot_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss06),
        weights = ~wt45,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(killed_tot_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss08),
        weights = ~wt9,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(killed_tot_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss10),
        weights = ~wt9,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(killed_tot_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss12),
        weights = ~wt9,
        vcov = ~tinh)  
))
legend("topleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

# By Married 

png("work_single_vhlss_s.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss02, south == 1 & single == 1),
        weights = ~wt75,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss04, south == 1 & single == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss06, south == 1 & single == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss08, south == 1 & single == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss10, south == 1 & single == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss12, south == 1 & single == 1),
        weights = ~wt9,
        vcov = ~tinh)
))
legend("topleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

png("work_married_vhlss_n.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss02, south == 0 & married == 1),
        weights = ~wt75,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss04, south == 0 & married == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss06, south == 0 & married == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss08, south == 0 & married == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss10, south == 0 & married == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss12, south == 0 & married == 1),
        weights = ~wt9,
        vcov = ~tinh)
))
legend("topleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

# Probability of being self employed

png("self_fe_vhlss_s.png")
iplot(list(
  feols(self ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban + industry,
        subset(vhlss02, south == 1 & work == 1),
        weights = ~wt75,
        vcov = ~tinh),
  feols(self ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban + industry,
        subset(vhlss04, south == 1 & work == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(self ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban + industry,
        subset(vhlss06, south == 1 & work == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(self ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban + industry,
        subset(vhlss08, south == 1 & work == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(self ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban + industry,
        subset(vhlss10, south == 1 & work == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(self ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban + industry,
        subset(vhlss12, south == 1 & work == 1),
        weights = ~wt9,
        vcov = ~tinh)
))
legend("bottomleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

png("self_fe_vhlss_n.png")
iplot(list(
  feols(self~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban + industry,
        subset(vhlss02, south == 0 & work == 1),
        weights = ~wt75,
        vcov = ~tinh),
  feols(self ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban + industry,
        subset(vhlss04, south == 0 & work == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(self ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban + industry,
        subset(vhlss06, south == 0 & work == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(self ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban + industry,
        subset(vhlss08, south == 0 & work == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(self ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban + industry,
        subset(vhlss10, south == 0 & work == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(self ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban + industry,
        subset(vhlss12, south == 0 & work == 1),
        weights = ~wt9,
        vcov = ~tinh)
))
legend("bottomleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

png("self_vhlss_cas.png")
iplot(list(
  feols(self ~ as.factor(female) + i(as.factor(female), log(killed_tot_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss02, work == 1),
        weights = ~wt75,
        vcov = ~tinh),
  feols(self ~ as.factor(female) + i(as.factor(female), log(killed_tot_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss04, work == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(self ~ as.factor(female) + i(as.factor(female), log(killed_tot_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss06, work == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(self ~ as.factor(female) + i(as.factor(female), log(killed_tot_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss08, work == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(self ~ as.factor(female) + i(as.factor(female), log(killed_tot_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss10, work == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(self ~ as.factor(female) + i(as.factor(female), log(killed_tot_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss12, work == 1),
        weights = ~wt9,
        vcov = ~tinh)
))
legend("bottomleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

# Probability by parental exposure 

iplot(list(
  feols(work ~ i(as.factor(female), log(tot_bmr_prov_ppn_mat)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + age_mat + as.factor(female),
        subset(vhlss14, south == 0 & age < 55),
        vcov = ~tinh),
  feols(work ~ i(as.factor(female), log(tot_bmr_prov_ppn_mat)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + age_mat + as.factor(female),
        subset(vhlss14, south == 1 & age < 55),
        vcov = ~tinh)
))
legend("topleft", col = 1:2, pch = 16, bty = "n", cex = 0.9, 
       legend = c("North", "South"))

iplot(list(
  feols(work ~ i(as.factor(female), log(tot_bmr_prov_ppn_pat)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + age_pat + as.factor(female),
        subset(vhlss14, south == 0 & age < 55),
        vcov = ~tinh),
  feols(work ~ i(as.factor(female), log(tot_bmr_prov_ppn_pat)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + age_pat + as.factor(female),
        subset(vhlss14, south == 1 & age < 55),
        vcov = ~tinh)
))


etable(list(
  feols(work ~ as.factor(widow_hh) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | urban + minority + married + tinh,
        subset(vhlss02, south == 1 & female == 1 & age < 46),
        vcov = ~tinh,
        weights = ~wt75),
  feols(work ~ as.factor(widow_hh) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | urban + minority + married + tinh,
        subset(vhlss04, south == 1 & female == 1 & age < 48),
        vcov = ~tinh,
        weights = ~wt45),
  feols(work ~ as.factor(widow_hh) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | urban + minority + married + tinh,
      subset(vhlss06, south == 1 & female == 1 & age < 50),
      vcov = ~tinh,
      weights = ~wt45),
  feols(work ~ as.factor(widow_hh) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | urban + minority + married + tinh,
      subset(vhlss08, south == 1 & female == 1 & age < 52),
      vcov = ~tinh,
      weights = ~wt9),
  feols(work ~ as.factor(widow_hh) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | urban + minority + married + tinh,
        subset(vhlss10, south == 1 & female == 1 & age < 54),
        vcov = ~tinh,
        weights = ~wt9),
  feols(work ~ as.factor(widow_hh) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | urban + minority + married + tinh,
        subset(vhlss12, south == 1 & female == 1 & age < 56),
        vcov = ~tinh,
        weights = ~wt9)))

etable(list(
  feols(work ~ as.factor(widow_hh) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | urban + minority + married,
        subset(vhlss02, south == 0 & age < 28 & female == 1),
        vcov = ~tinh,
        weights = ~wt75),
  feols(work ~ as.factor(widow_hh) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | urban + minority + married,
        subset(vhlss04, south == 0 & age < 30 & female == 1),
        vcov = ~tinh,
        weights = ~wt45),
  feols(work ~ as.factor(widow_hh) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | urban + minority + married,
        subset(vhlss06, south == 0 & age < 32 & female == 1),
        vcov = ~tinh,
        weights = ~wt45),
  feols(work ~ as.factor(widow_hh) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | urban + minority + married,
        subset(vhlss08, south == 0 & age < 34 & female == 1),
        vcov = ~tinh,
        weights = ~wt9),
  feols(work ~ as.factor(widow_hh) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | urban + minority + married,
        subset(vhlss10, south == 0 & age < 36 & female == 1),
        vcov = ~tinh,
        weights = ~wt9),
  feols(work ~ as.factor(widow_hh) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | urban + minority + married,
        subset(vhlss12, south == 0 & age < 38 & female == 1),
        vcov = ~tinh,
        weights = ~wt9)))
