####################################################
# PROVINCE-LEVEL BMR ON THE PROBABILITY OF WORKING #
####################################################

# Per population 

png("work_vhlss_ppn_s.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ | minority + urban + marital,
        subset(vhlss02, south == 1),
        weights = ~wt75,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ | minority + urban + marital,
        subset(vhlss04, south == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ | minority + urban + marital,
        subset(vhlss06, south == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ | minority + urban + marital,
        subset(vhlss08, south == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ | minority + urban + marital,
        subset(vhlss10, south == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ | minority + urban + marital,
        subset(vhlss12, south == 1),
        weights = ~wt9,
        vcov = ~tinh)
))
legend("topleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

png("work_vhlss_ppn_n.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ | minority + urban + marital,
        subset(vhlss02, south == 0),
        weights = ~wt75,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ | minority + urban + marital,
        subset(vhlss04, south == 0),
        weights = ~wt45,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ | minority + urban + marital,
        subset(vhlss06, south == 0),
        weights = ~wt45,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ | minority + urban + marital,
        subset(vhlss08, south == 0),
        weights = ~wt9,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ | minority + urban + marital,
        subset(vhlss10, south == 0),
        weights = ~wt9,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ| minority + urban + marital,
        subset(vhlss12, south == 0),
        weights = ~wt9,
        vcov = ~tinh)
))
legend("topleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

# Aggregate 

png("work_vhlss_s.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ | minority + urban + marital,
        subset(vhlss02, south == 1),
        weights = ~wt75,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ | minority + urban + marital,
        subset(vhlss04, south == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ | minority + urban + marital,
        subset(vhlss06, south == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ | minority + urban + marital,
        subset(vhlss08, south == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ | minority + urban + marital,
        subset(vhlss10, south == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ | minority + urban + marital,
        subset(vhlss12, south == 1),
        weights = ~wt9,
        vcov = ~tinh)
))
legend("topleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

png("work_vhlss_n.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ | minority + urban + marital,
        subset(vhlss02, south == 0),
        weights = ~wt75,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ | minority + urban + marital,
        subset(vhlss04, south == 0),
        weights = ~wt45,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ | minority + urban + marital,
        subset(vhlss06, south == 0),
        weights = ~wt45,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ | minority + urban + marital,
        subset(vhlss08, south == 0),
        weights = ~wt9,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ | minority + urban + marital,
        subset(vhlss10, south == 0),
        weights = ~wt9,
        vcov = ~tinh),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ| minority + urban + marital,
        subset(vhlss12, south == 0),
        weights = ~wt9,
        vcov = ~tinh)
))
legend("topleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

#######################################
# PROBABILITY OF WORKING IN AGRI/MANU #
#######################################

# Agriculture 

## Per population 

png("agri_vhlss_ppn_s.png")
iplot(list(
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + log(popdensity_02) | minority + urban + marital,
        subset(vhlss02, south == 1 & work == 1),
        weights = ~wt75,
        vcov = ~tinh),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + log(popdensity_04) | minority + urban + marital,
        subset(vhlss04, south == 1 & work == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + log(popdensity_06) | minority + urban + marital,
        subset(vhlss06, south == 1 & work == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + log(popdensity_08) | minority + urban + marital,
        subset(vhlss08, south == 1 & work == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + log(popdensity_10) | minority + urban + marital,
        subset(vhlss10, south == 1 & work == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + log(popdensity_12) | minority + urban + marital,
        subset(vhlss12, south == 1 & work == 1),
        weights = ~wt9,
        vcov = ~tinh)
))
legend("bottomleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

png("agri_vhlss_ppn_n.png")
iplot(list(
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + log(popdensity_02) | minority + urban + marital,
        subset(vhlss02, south == 0 & work == 1),
        weights = ~wt75,
        vcov = ~tinh),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + log(popdensity_04) | minority + urban + marital,
        subset(vhlss04, south == 0 & work == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + log(popdensity_06) | minority + urban + marital,
        subset(vhlss06, south == 0 & work == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + log(popdensity_08) | minority + urban + marital,
        subset(vhlss08, south == 0 & work == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + log(popdensity_10) | minority + urban + marital,
        subset(vhlss10, south == 0 & work == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + log(popdensity_12) | minority + urban + marital,
        subset(vhlss12, south == 0 & work == 1),
        weights = ~wt9,
        vcov = ~tinh)
))
legend("bottomleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

## Aggregate 

png("agri_vhlss_s.png")
iplot(list(
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ + log(popdensity_02) | minority + urban + marital,
        subset(vhlss02, south == 1 & work == 1),
        weights = ~wt75,
        vcov = ~tinh),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ + log(popdensity_04) | minority + urban + marital,
        subset(vhlss04, south == 1 & work == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ + log(popdensity_06) | minority + urban + marital,
        subset(vhlss06, south == 1 & work == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ + log(popdensity_08) | minority + urban + marital,
        subset(vhlss08, south == 1 & work == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ + log(popdensity_10) | minority + urban + marital,
        subset(vhlss10, south == 1 & work == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ + log(popdensity_12) | minority + urban + marital,
        subset(vhlss12, south == 1 & work == 1),
        weights = ~wt9,
        vcov = ~tinh)
))
legend("bottomleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

png("agri_vhlss_n.png")
iplot(list(
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ + log(popdensity_02) | minority + urban + marital,
        subset(vhlss02, south == 0 & work == 1),
        weights = ~wt75,
        vcov = ~tinh),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ + log(popdensity_04) | minority + urban + marital,
        subset(vhlss04, south == 0 & work == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ + log(popdensity_06) | minority + urban + marital,
        subset(vhlss06, south == 0 & work == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ + log(popdensity_08) | minority + urban + marital,
        subset(vhlss08, south == 0 & work == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ + log(popdensity_10) | minority + urban + marital,
        subset(vhlss10, south == 0 & work == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ + log(popdensity_12) | minority + urban + marital,
        subset(vhlss12, south == 0 & work == 1),
        weights = ~wt9,
        vcov = ~tinh)
))
legend("bottomleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

# Manufacturing 

## Per population 

png("manu_vhlss_ppn_s.png")
iplot(list(
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + log(popdensity_02) | minority + urban + marital,
        subset(vhlss02, south == 1 & work == 1),
        weights = ~wt75,
        vcov = ~tinh),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + log(popdensity_04) | minority + urban + marital,
        subset(vhlss04, south == 1 & work == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + log(popdensity_06) | minority + urban + marital,
        subset(vhlss06, south == 1 & work == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + log(popdensity_08) | minority + urban + marital,
        subset(vhlss08, south == 1 & work == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + log(popdensity_10) | minority + urban + marital,
        subset(vhlss10, south == 1 & work == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ + log(popdensity_12) | minority + urban + marital,
        subset(vhlss12, south == 1 & work == 1),
        weights = ~wt9,
        vcov = ~tinh)
))
legend("topleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

png("manu_vhlss_ppn_n.png")
iplot(list(
  feols(manu~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ  + log(popdensity_02) | minority + urban + industry + marital,
        subset(vhlss02, south == 0 & work == 1),
        weights = ~wt75,
        vcov = ~tinh),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ  + log(popdensity_04) | minority + urban + industry + marital,
        subset(vhlss04, south == 0 & work == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ  + log(popdensity_06) | minority + urban + industry + marital,
        subset(vhlss06, south == 0 & work == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ  + log(popdensity_08) | minority + urban + industry + marital,
        subset(vhlss08, south == 0 & work == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ  + log(popdensity_10) | minority + urban + industry + marital,
        subset(vhlss10, south == 0 & work == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + age + age^2 + educ  + log(popdensity_12) | minority + urban + industry + marital,
        subset(vhlss12, south == 0 & work == 1),
        weights = ~wt9,
        vcov = ~tinh)
))
legend("topleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

## Aggregate 

png("manu_vhlss_s.png")
iplot(list(
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ + log(popdensity_02) | minority + urban + marital,
        subset(vhlss02, south == 1 & work == 1),
        weights = ~wt75,
        vcov = ~tinh),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ + log(popdensity_04) | minority + urban + marital,
        subset(vhlss04, south == 1 & work == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ + log(popdensity_06) | minority + urban + marital,
        subset(vhlss06, south == 1 & work == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ + log(popdensity_08) | minority + urban + marital,
        subset(vhlss08, south == 1 & work == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ + log(popdensity_10) | minority + urban + marital,
        subset(vhlss10, south == 1 & work == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ + log(popdensity_12) | minority + urban + marital,
        subset(vhlss12, south == 1 & work == 1),
        weights = ~wt9,
        vcov = ~tinh)
))
legend("topleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

png("manu_vhlss_n.png")
iplot(list(
  feols(manu~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ  + log(popdensity_02) | minority + urban + industry + marital,
        subset(vhlss02, south == 0 & work == 1),
        weights = ~wt75,
        vcov = ~tinh),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ  + log(popdensity_04) | minority + urban + industry + marital,
        subset(vhlss04, south == 0 & work == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ  + log(popdensity_06) | minority + urban + industry + marital,
        subset(vhlss06, south == 0 & work == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ  + log(popdensity_08) | minority + urban + industry + marital,
        subset(vhlss08, south == 0 & work == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ  + log(popdensity_10) | minority + urban + industry + marital,
        subset(vhlss10, south == 0 & work == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ  + log(popdensity_12) | minority + urban + industry + marital,
        subset(vhlss12, south == 0 & work == 1),
        weights = ~wt9,
        vcov = ~tinh)
))
legend("topleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

# Self employment 

png("selfemp_vhlss_s.png")
iplot(list(
  feols(selfemp ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ + log(popdensity_02) | minority + urban + marital,
        subset(vhlss02, work == 1 & south == 1),
        weights = ~wt75,
        vcov = ~tinh),
  feols(selfemp~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ + log(popdensity_04) | minority + urban + marital,
        subset(vhlss04, work == 1 & south == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(selfemp ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ + log(popdensity_06) | minority + urban + marital,
        subset(vhlss06, work == 1 & south == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(selfemp ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ + log(popdensity_08) | minority + urban + marital,
        subset(vhlss08, work == 1 & south == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(selfemp ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ + log(popdensity_10) | minority + urban + marital,
        subset(vhlss10, work == 1 & south == 1),
        weights = ~wt9,
        vcov = ~tinh),
  feols(selfemp ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ + log(popdensity_12) | minority + urban + marital,
        subset(vhlss12, work == 1 & south == 1),
        weights = ~wt9,
        vcov = ~tinh)
))
legend("bottomleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

png("selfemp_vhlss_n.png")
iplot(list(
  feols(selfemp ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ + log(popdensity_02) | minority + urban + marital,
        subset(vhlss02, work == 1 & south == 0),
        weights = ~wt75,
        vcov = ~tinh),
  feols(selfemp~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ + log(popdensity_04) | minority + urban + marital,
        subset(vhlss04, work == 1 & south == 0),
        weights = ~wt45,
        vcov = ~tinh),
  feols(selfemp ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ + log(popdensity_06) | minority + urban + marital,
        subset(vhlss06, work == 1 & south == 0),
        weights = ~wt45,
        vcov = ~tinh),
  feols(selfemp ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ + log(popdensity_08) | minority + urban + marital,
        subset(vhlss08, work == 1 & south == 0),
        weights = ~wt9,
        vcov = ~tinh),
  feols(selfemp ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ + log(popdensity_10) | minority + urban + marital,
        subset(vhlss10, work == 1 & south == 0),
        weights = ~wt9,
        vcov = ~tinh),
  feols(selfemp ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + age + age^2 + educ + log(popdensity_12) | minority + urban + marital,
        subset(vhlss12, work == 1 & south == 0),
        weights = ~wt9,
        vcov = ~tinh)
))
legend("bottomleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()
