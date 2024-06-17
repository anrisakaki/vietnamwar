vhlss <- c("vhlss02.Rda", "vhlss04.Rda", "vhlss06.Rda", "vhlss08.Rda", "vhlss10.Rda", "vhlss12.Rda")

for (i in vhlss) {
  load(i)
}

##########################
# PROBABILITY OF WORKING #
##########################

png("work_vhlss_dist_s.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss02, south == 1),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss04, south == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss06, south == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss08, south == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss10, south == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss12, south == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen)
))
legend("topleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

png("work_vhlss_dist_n.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss02, south == 0),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss04, south == 0),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss06, south == 0),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss08, south == 0),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss10, south == 0),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
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

###############################################
# PROBABILITY OF BEING MANAGER OF HH BUSINESS #
###############################################

etable(list(
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus02, south == 1),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus04, south == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus06, south == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus02, south == 0),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus04, south == 0),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus06, south == 0),
        weights = ~wt45,
        vcov = ~tinh+huyen)
))

etable(list(
  feols(female  ~ log(tot_bmr_prov),
        subset(hhbus02, south == 1),
        vcov = ~tinh+huyen),
  feols(female  ~ log(tot_bmr_prov),
        subset(hhbus06, south == 1),
        vcov = ~tinh+huyen),
  feols(female  ~ log(tot_bmr_prov),
        subset(hhbus02, south == 0),
        vcov = ~tinh+huyen),
  feols(female  ~ log(tot_bmr_prov),
        subset(hhbus06, south == 0),
        vcov = ~tinh+huyen)
))

######################################
# PROBABILITY OF WORKING - BY SECTOR #
######################################

png("agri_vhlss_dist_s.png")
iplot(list(
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss02, south == 1),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss04, south == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss06, south == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss08, south == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss10, south == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss12, south == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen)
))
legend("topleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

png("agri_vhlss_dist_n.png")
iplot(list(
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss02, south == 0 & work == 1),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss04, south == 0 & work == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss06, south == 0 & work == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss08, south == 0 & work == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss10, south == 0 & work == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss12, south == 0 & work == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen)
))
legend("bottomleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

png("manu_vhlss_dist_s.png")
iplot(list(
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss02, south == 1 & work == 1),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss04, south == 1 & work == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss06, south == 1 & work == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss08, south == 1 & work == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss10, south == 1 & work == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss12, south == 1 & work == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen)
))
legend("topleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

png("manu_vhlss_dist_n.png")
iplot(list(
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss02, south == 0 & work == 1),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss04, south == 0 & work == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss06, south == 0 & work == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss08, south == 0 & work == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss10, south == 0 & work == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss12, south == 0 & work == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen)
))
legend("topleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

png("selfemp_vhlss_dist_s.png")
iplot(list(
  feols(selfemp ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss02, south == 1 & work == 1),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  feols(selfemp ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss04, south == 1 & work == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(selfemp ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss06, south == 1 & work == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(selfemp ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss08, south == 1 & work == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(selfemp ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss10, south == 1 & work == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(selfemp ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss12, south == 1 & work == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen)
))
legend("bottomleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

png("selfemp_vhlss_dist_n.png")
iplot(list(
  feols(selfemp ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss02, south == 0 & work == 1),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  feols(selfemp ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss04, south == 0 & work == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(selfemp ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss06, south == 0 & work == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(selfemp ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss08, south == 0 & work == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(selfemp ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss10, south == 0 & work == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(selfemp ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss12, south == 0 & work == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen)
))
legend("bottomleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

png("selfagri_vhlss_dist_s.png")
iplot(list(
  feols(selfagri ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss02, south == 1 & work == 1),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  feols(selfagri ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss04, south == 1 & work == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(selfagri ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss06, south == 1 & work == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(selfagri ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss08, south == 1 & work == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(selfagri ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss10, south == 1 & work == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(selfagri ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss12, south == 1 & work == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen)
))
legend("topleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

png("selfagri_vhlss_dist_n.png")
iplot(list(
  feols(selfagri ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss02, south == 0 & work == 1),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  feols(selfagri ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss04, south == 0 & work == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(selfagri ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss06, south == 0 & work == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(selfagri ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss08, south == 0 & work == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(selfagri ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss10, south == 0 & work == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(selfagri ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss12, south == 0 & work == 1),
        weights = ~wt9,
        vcov = ~tinh+huyen)
))
legend("topleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

###############################################
# PROBABILITY OF WORKING - BY PERIOD OF BIRTH #
###############################################

# District level

png("work_prewar_dist_vhlss_s.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss02, south == 1 & age > (2002-1975)),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss04, south == 1 & age > (2004-1975)),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss06, south == 1 & age > (2006-1975)),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss08, south == 1 & age > (2008-1975)),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss10, south == 1 & age > (2010-1975)),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss12, south == 1 & age > (2012-1975)),
        weights = ~wt9,
        vcov = ~tinh+huyen)
))
legend("topleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

png("work_postwar_dist_vhlss_s.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss02, south == 1 & age <= (2002-1975)),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss04, south == 1 & age <= (2004-1975)),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss06, south == 1 & age <= (2006-1975)),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss08, south == 1 & age <= (2008-1950)),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss10, south == 1 & age <= (2010-1975)),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss12, south == 1 & age <= (2012-1975)),
        weights = ~wt9,
        vcov = ~tinh+huyen)
))
legend("topleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

png("work_prewar_dist_vhlss_n.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss02, south == 0 & age > (2002-1975)),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss04, south == 0 & age > (2004-1975)),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss06, south == 0 & age > (2006-1975)),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss08, south == 0 & age > (2008-1975)),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss10, south == 0 & age > (2010-1975)),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss12, south == 0 & age > (2012-1975)),
        weights = ~wt9,
        vcov = ~tinh+huyen)
))
legend("bottomleft", col = 1:7, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()

png("work_postwar_dist_vhlss_n.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss02, south == 0 & age <= (2002-1975)),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss04, south == 0 & age <= (2004-1975)),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss06, south == 0 & age <= (2006-1975)),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss08, south == 0 & age <= (2008-1975)),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss10, south == 0 & age <= (2010-1975)),
        weights = ~wt9,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr)) + age + age^2 + educ | minority + urban + marital + tinh,
        subset(vhlss12, south == 0 & age <= (2012-1975)),
        weights = ~wt9,
        vcov = ~tinh+huyen)
))

legend("topleft", col = 1:6, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2001", "2003", "2005", "2007", "2009", "2011"))
dev.off()