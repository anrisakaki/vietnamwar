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
