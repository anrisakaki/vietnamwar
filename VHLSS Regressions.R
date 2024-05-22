vhlss <- c("vhlss02.Rda", "vhlss04.Rda", "vhlss06.Rda", "vhlss08.Rda", "vhlss10.Rda", "vhlss12.Rda")

for (i in vhlss) {
  load(i)
}

prov_vhlss <- c("prov02_vhlss.Rda", "prov04_vhlss.Rda", "prov06_vhlss.Rda", "prov08_vhlss.Rda", "prov10_vhlss.Rda", "prov12_vhlss.Rda")

for (i in prov_vhlss) {
  load(i)
}

##########################
# PROBABILITY OF WORKING #
##########################

png("work_vhlss_s.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_02) | minority + urban + marital,
        subset(vhlss02, south == 1),
        weights = ~wt75,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_04) | minority + urban + marital,
        subset(vhlss04, south == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_06) | minority + urban + marital,
        subset(vhlss06, south == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_08) | minority + urban + marital,
        subset(vhlss08, south == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_10) | minority + urban + marital,
        subset(vhlss10, south == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_12)| minority + urban + marital,
        subset(vhlss12, south == 1),
        weights = ~wt9,
        vcov = ~tinh)
))
legend("topleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

png("work_vhlss_n.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_02)| minority + urban + marital,
        subset(vhlss02, south == 0),
        weights = ~wt75,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_04) | minority + urban + marital,
        subset(vhlss04, south == 0),
        weights = ~wt45,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_06) | minority + urban + marital,
        subset(vhlss06, south == 0),
        weights = ~wt45,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_08) | minority + urban + marital,
        subset(vhlss08, south == 0),
        weights = ~wt9,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_10) | minority + urban + marital,
        subset(vhlss10, south == 0),
        weights = ~wt9,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_12) | minority + urban + marital,
        subset(vhlss12, south == 0),
        weights = ~wt9,
        vcov = ~tinh)
))
legend("topleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

png("work_vhlss_cas.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(killed_tot_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_02) | minority + urban + marital,
        subset(vhlss02),
        weights = ~wt75,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(killed_tot_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_04) | minority + urban + marital,
        subset(vhlss04),
        weights = ~wt45,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(killed_tot_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_06) | minority + urban + marital,
        subset(vhlss06),
        weights = ~wt45,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(killed_tot_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_08) | minority + urban + marital,
        subset(vhlss08),
        weights = ~wt9,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(killed_tot_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_10) | minority + urban + marital,
        subset(vhlss10),
        weights = ~wt9,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(killed_tot_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_12) | minority + urban + marital,
        subset(vhlss12),
        weights = ~wt9,
        vcov = ~tinh)  
))
legend("topleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

# By Widowhood 

png("work_widowed_vhlss_s.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss02, south == 1 & widowed == 1),
        weights = ~wt75,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss04, south == 1 & widowed == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss06, south == 1 & widowed == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss08, south == 1 & widowed == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss10, south == 1 & widowed == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss12, south == 1 & widowed == 1),
        weights = ~wt9,
        vcov = ~tinh)
))
legend("topleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

png("work_widowed_vhlss_n.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss02, south == 0 & widowed == 1),
        weights = ~wt75,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss04, south == 0 & widowed == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss06, south == 0 & widowed == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss08, south == 0 & widowed == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss10, south == 0 & widowed == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | minority + urban,
        subset(vhlss12, south == 0 & widowed == 1),
        weights = ~wt9,
        vcov = ~tinh)
))
legend("topleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

#############################################
# PROBABILITY OF WORKING BY EDUCATION LEVEL #
#############################################

#######################################
# PROBABILITY OF WORKING IN AGRI/MANU #
#######################################

# Agriculture 

png("selfagri_vhlss_s.png")
iplot(list(
  feols(selfagri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_02) | minority + urban + marital,
        subset(vhlss02, south == 1 & work == 1),
        weights = ~wt75,
        vcov = ~tinh),
  feols(selfagri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_04) | minority + urban + marital,
        subset(vhlss04, south == 1 & work == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(selfagri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_06) | minority + urban + marital,
        subset(vhlss06, south == 1 & work == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(selfagri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_08) | minority + urban + marital,
        subset(vhlss08, south == 1 & work == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(selfagri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_10) | minority + urban + marital,
        subset(vhlss10, south == 1 & work == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(selfagri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_12) | minority + urban + marital,
        subset(vhlss12, south == 1 & work == 1),
        weights = ~wt9,
        vcov = ~tinh)
))
legend("bottomleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

png("selfagri_vhlss_n.png")
iplot(list(
  feols(selfagri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_02) | minority + urban + marital,
        subset(vhlss02, south == 0 & work == 1),
        weights = ~wt75,
        vcov = ~tinh),
  feols(selfagri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_04) | minority + urban + marital,
        subset(vhlss04, south == 0 & work == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(selfagri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_06) | minority + urban + marital,
        subset(vhlss06, south == 0 & work == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(selfagri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_08) | minority + urban + marital,
        subset(vhlss08, south == 0 & work == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(selfagri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_10) | minority + urban + marital,
        subset(vhlss10, south == 0 & work == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(selfagri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_12) | minority + urban + marital,
        subset(vhlss12, south == 0 & work == 1),
        weights = ~wt9,
        vcov = ~tinh)
))
legend("bottomleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

# Manufacturing 

png("manu_vhlss_s.png")
iplot(list(
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_02) | minority + urban + marital,
        subset(vhlss02, south == 1 & work == 1),
        weights = ~wt75,
        vcov = ~tinh),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_04) | minority + urban + marital,
        subset(vhlss04, south == 1 & work == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_06) | minority + urban + marital,
        subset(vhlss06, south == 1 & work == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_08) | minority + urban + marital,
        subset(vhlss08, south == 1 & work == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_10) | minority + urban + marital,
        subset(vhlss10, south == 1 & work == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_12) | minority + urban + marital,
        subset(vhlss12, south == 1 & work == 1),
        weights = ~wt9,
        vcov = ~tinh)
))
legend("topleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

png("manu_vhlss_n.png")
iplot(list(
  feols(manu~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov  + log(popdensity_02) | minority + urban + industry + marital,
        subset(vhlss02, south == 0 & work == 1),
        weights = ~wt75,
        vcov = ~tinh),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov  + log(popdensity_04) | minority + urban + industry + marital,
        subset(vhlss04, south == 0 & work == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov  + log(popdensity_06) | minority + urban + industry + marital,
        subset(vhlss06, south == 0 & work == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov  + log(popdensity_08) | minority + urban + industry + marital,
        subset(vhlss08, south == 0 & work == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov  + log(popdensity_10) | minority + urban + industry + marital,
        subset(vhlss10, south == 0 & work == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov  + log(popdensity_12) | minority + urban + industry + marital,
        subset(vhlss12, south == 0 & work == 1),
        weights = ~wt9,
        vcov = ~tinh)
))
legend("topleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

# Self employment 

png("self_vhlss_s.png")
iplot(list(
  feols(self ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_02) | minority + urban + marital,
        subset(vhlss02, work == 1 & south == 1),
        weights = ~wt75,
        vcov = ~tinh),
  feols(self~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_04) | minority + urban + marital,
        subset(vhlss04, work == 1 & south == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(self ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_06) | minority + urban + marital,
        subset(vhlss06, work == 1 & south == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(self ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_08) | minority + urban + marital,
        subset(vhlss08, work == 1 & south == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(self ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_10) | minority + urban + marital,
        subset(vhlss10, work == 1 & south == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(self ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_12) | minority + urban + marital,
        subset(vhlss12, work == 1 & south == 1),
        weights = ~wt9,
        vcov = ~tinh)
))
legend("bottomleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

png("self_vhlss_n.png")
iplot(list(
  feols(self ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_02) | minority + urban + marital,
        subset(vhlss02, work == 1 & south == 0),
        weights = ~wt75,
        vcov = ~tinh),
  feols(self~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_04) | minority + urban + marital,
        subset(vhlss04, work == 1 & south == 0),
        weights = ~wt45,
        vcov = ~tinh),
  feols(self ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_06) | minority + urban + marital,
        subset(vhlss06, work == 1 & south == 0),
        weights = ~wt45,
        vcov = ~tinh),
  feols(self ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_08) | minority + urban + marital,
        subset(vhlss08, work == 1 & south == 0),
        weights = ~wt9,
        vcov = ~tinh),
  feols(self ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_10) | minority + urban + marital,
        subset(vhlss10, work == 1 & south == 0),
        weights = ~wt9,
        vcov = ~tinh),
  feols(self ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensity_12) | minority + urban + marital,
        subset(vhlss12, work == 1 & south == 0),
        weights = ~wt9,
        vcov = ~tinh)
))
legend("bottomleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()
