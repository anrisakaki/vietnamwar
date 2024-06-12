####################################################
# PROVINCE-LEVEL BMR ON THE PROBABILITY OF WORKING #
####################################################

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

png("self_vhlss_s.png")
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

png("self_vhlss_n.png")
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


##############################
# PROBABILITY OF WORK BY AGE #
##############################

# 2002 

## Female 

work_agexbmr_02_s <- tidy(feols(work ~ i(as.factor(age), log(tot_bmr_prov_ppn)) + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | urban + age + minority,
                           subset(vhlss02, south == 1 & female == 1 & age < 65 & age > 15),
                           weights = ~wt75,
                           vcov = ~tinh + age)) %>% filter(grepl("log\\(tot_bmr_prov_ppn\\)", term))
work_agexbmr_02_n <- tidy(feols(work ~ i(as.factor(age), log(tot_bmr_prov_ppn)) + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | urban + age + minority,
                           subset(vhlss02, south == 0 & female == 1 & age < 65 & age > 15),
                           weights = ~wt75,
                           vcov = ~tinh + age)) %>% filter(grepl("log\\(tot_bmr_prov_ppn\\)", term))

selfagri_agexbmr_coef_02_s$age <- rep(seq(16, 64), each = nrow(selfagri_agexbmr_coef_02_s) / length(seq(16, 64)))
selfagri_agexbmr_coef_02_n$age <- rep(seq(16, 64), each = nrow(selfagri_agexbmr_coef_02_n) / length(seq(16, 64)))

selfagri_agexbmr_coef_02_n$group <- "North"
selfagri_agexbmr_coef_02_s$group <- "South"
selfagri_agexbmr_coef_02_ns <- bind_rows(selfagri_agexbmr_coef_02_n, selfagri_agexbmr_coef_02_s)
