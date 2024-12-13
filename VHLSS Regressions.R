vhlss <- c("vhlss02.Rda", "vhlss04.Rda", "vhlss06.Rda", "vhlss08.Rda", "vhlss10.Rda", "vhlss12.Rda")

for (i in vhlss) {
  load(i)
}

##########################
# PROBABILITY OF WORKING #
##########################

png("work_vhlss_dist_s.png")

etable(feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital + tinh + year,
            subset(vhlss, south == 1 & year < 2008),
            weights = ~wt,
            vcov = ~tinh+huyen))

iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss02, south == 1),
        weights = ~wt,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss04, south == 1),
        weights = ~wt,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss06, south == 1),
        weights = ~wt,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss08, south == 1),
        weights = ~wt,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss10, south == 1),
        weights = ~wt,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss12, south == 1),
        weights = ~wt,
        vcov = ~tinh+huyen)
))
legend("topleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

png("work_vhlss_dist_n.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss02, south == 0),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss04, south == 0),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss06, south == 0),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss08, south == 0),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss10, south == 0),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss12, south == 0),
        weights = ~wt9,
        vcov = ~tinh+huyen)
))
legend("topleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

png("work_vhlss_cs_dist_s.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital,
        subset(vhlss02, south == 1),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital,
        subset(vhlss04, south == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital,
        subset(vhlss06, south == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital,
        subset(vhlss08, south == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital,
        subset(vhlss10, south == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital,
        subset(vhlss12, south == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen)
))
legend("topleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

png("work_vhlss_cs_dist_n.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital,
        subset(vhlss02, south == 0),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital,
        subset(vhlss04, south == 0),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital,
        subset(vhlss06, south == 0),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital,
        subset(vhlss08, south == 0),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital,
        subset(vhlss10, south == 0),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital,
        subset(vhlss12, south == 0),
        weights = ~wt9,
        vcov = ~tinh+huyen)
))
legend("topleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()


png("work_vhlss_dist_cas.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(killed_tot)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss02, south == 1),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), log(killed_tot)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss04, south == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), log(killed_tot)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss06, south == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), log(killed_tot)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss08, south == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), log(killed_tot)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss10, south == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), log(killed_tot)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss12, south == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen)
))
legend("topleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

######################################
# PROBABILITY OF WORKING - BY SECTOR #
######################################

png("agri_vhlss_dist_s.png")
iplot(list(
  feols(agri ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss02, south == 1),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  feols(agri ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss04, south == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(agri ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss06, south == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(agri ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss08, south == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(agri ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss10, south == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(agri ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss12, south == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen)
))
legend("topleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

png("agri_vhlss_dist_n.png")
iplot(list(
  feols(agri ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss02, south == 0 & work == 1),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  feols(agri ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss04, south == 0 & work == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(agri ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss06, south == 0 & work == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(agri ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss08, south == 0 & work == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(agri ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss10, south == 0 & work == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(agri ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss12, south == 0 & work == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen)
))
legend("bottomleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

png("manu_vhlss_dist_s.png")
iplot(list(
  feols(manu ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss02, south == 1 & work == 1),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  feols(manu ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss04, south == 1 & work == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(manu ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss06, south == 1 & work == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(manu ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss08, south == 1 & work == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(manu ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss10, south == 1 & work == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(manu ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss12, south == 1 & work == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen)
))
legend("topleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

png("manu_vhlss_dist_n.png")
iplot(list(
  feols(manu ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss02, south == 0 & work == 1),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  feols(manu ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss04, south == 0 & work == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(manu ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss06, south == 0 & work == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(manu ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss08, south == 0 & work == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(manu ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss10, south == 0 & work == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(manu ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss12, south == 0 & work == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen)
))
legend("topleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()
