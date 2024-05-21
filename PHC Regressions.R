phc <- c("phc89.Rda", "phc99.Rda", "phc09.Rda")

for (i in phc) {
  load(i)
}

dict = c("as.factor(female)" = "Female")

setFixest_coefplot(dict = dict, grid = FALSE, zero.par = list(lty = 2), main = " ")

################################
# Individual-level regressions #
################################

# Probability of working 

png("work_phc_n.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc89, south == 0 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc99, south == 0 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc09, south == 0 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn2009)))
legend("bottomleft", col = 1:3, pch = 16, bty = "n", cex = 0.9, 
       legend = c("1989", "1999", "2009"))
dev.off()

png("work_phc_s.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc89, south == 1 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc99, south == 1 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc09, south == 1 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn2009)))
legend("topleft", col = 1:3, pch = 16, bty = "n", cex = 0.9, 
       legend = c("1989", "1999", "2009"))
dev.off()

# Probability of working in agriculture 

png("manu_phc_n.png")
iplot(list(
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc89, south == 0 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc99, south == 0 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc09, south == 0 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn2009)))
legend("bottomleft", col = 1:3, pch = 16, bty = "n", cex = 0.9, 
       legend = c("1989", "1999", "2009"))
dev.off()

png("agri_phc_s.png")
iplot(list(
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc89, south == 1 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc99, south == 1 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc09, south == 1 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn2009)))
legend("bottomleft", col = 1:3, pch = 16, bty = "n", cex = 0.9, 
       legend = c("1989", "1999", "2009"))
dev.off()

png("agri_phc_n.png")
iplot(list(
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc89, work == 1 & south == 0 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc99, work == 1 & south == 0 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc09, work == 1 & south == 0 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn2009)))
legend("bottomleft", col = 1:3, pch = 16, bty = "n", cex = 0.9, 
       legend = c("1989", "1999", "2009"))
dev.off()

png("self_phc_s.png")
iplot(list(
  feols(self ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + married,
        subset(phc89, work == 1 & south == 1 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(self ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + married,
        subset(phc99, work == 1 & south == 1 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(self ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + married,
        subset(phc09, work == 1 & south == 1 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn2009)))
legend("topleft", col = 1:3, pch = 16, bty = "n", cex = 0.9, 
       legend = c("1989", "1999", "2009"))
dev.off()

png("self_phc_n.png")
iplot(list(
  feols(self ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + married,
        subset(phc89, work == 1 & south == 0 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(self ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + married,
        subset(phc99, work == 1 & south == 0 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(self ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + married,
        subset(phc09, work == 1 & south == 0 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn2009)))
legend("bottomleft", col = 1:3, pch = 16, bty = "n", cex = 0.9, 
       legend = c("1989", "1999", "2009"))
dev.off()

# Probability of working by marital status

png("work_widow_nochildren_phc_s.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc89, south == 1 & migration == 0 & widowed == 1 & age > 38 & nchild == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc99, south == 1 & migration == 0 & widowed == 1 & age > 48 & nchild == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc09, south == 1 & migration == 0  & widowed == 1 & age > 58 & nchild == 0),
        weights = ~perwt,
        vcov = ~geo1_vn2009)))
legend("bottomright", col = 1:3, pch = 16, bty = "n", cex = 0.9, 
       legend = c("1989", "1999", "2009"))
dev.off()

png("work_widow_children_phc_s.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc89, south == 1 & migration == 0 & widowed == 1 & age > 38 & nchild > 0),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc99, south == 1 & migration == 0 & widowed == 1 & age > 48 & nchild > 0),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc09, south == 1 & migration == 0  & widowed == 1 & age > 58 & nchild > 0),
        weights = ~perwt,
        vcov = ~geo1_vn2009)))
legend("bottomright", col = 1:3, pch = 16, bty = "n", cex = 0.9, 
       legend = c("1989", "1999", "2009"))
dev.off()

png("work_widow_nochildren_phc_n.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc89, south == 0 & migration == 0 & widowed == 1 & age > 38 & nchild == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc99, south == 0 & migration == 0 & widowed == 1 & age > 48 & nchild == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc09, south == 0 & migration == 0  & widowed == 1 & age > 58 & nchild == 0),
        weights = ~perwt,
        vcov = ~geo1_vn2009)))
legend("bottomright", col = 1:3, pch = 16, bty = "n", cex = 0.9, 
       legend = c("1989", "1999", "2009"))
dev.off()

png("work_widow_children_phc_n.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc89, south == 0 & migration == 0 & widowed == 1 & age > 38 & nchild > 0),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc99, south == 0 & migration == 0 & widowed == 1 & age > 48 & nchild > 0),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc09, south == 0 & migration == 0  & widowed == 1 & age > 58 & nchild > 0),
        weights = ~perwt,
        vcov = ~geo1_vn2009)))
legend("topright", col = 1:3, pch = 16, bty = "n", cex = 0.9, 
       legend = c("1989", "1999", "2009"))
dev.off()

png("work_married_phc_s.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc89, south == 1 & migration == 0 & married == 1 & age > 38),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc99, south == 1 & migration == 0 & married == 1 & age > 48),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc09, south == 1 & migration == 0  & married == 1 & age > 58),
        weights = ~perwt,
        vcov = ~geo1_vn2009)))
legend("topleft", col = 1:3, pch = 16, bty = "n", cex = 0.9, 
       legend = c("1989", "1999", "2009"))
dev.off()

png("work_married_phc_n.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc89, south == 0 & migration == 0 & married == 1 & age > 38),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc99, south == 0 & migration == 0 & married == 1 & age > 48),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc09, south == 0 & migration == 0  & married == 1 & age > 58),
        weights = ~perwt,
        vcov = ~geo1_vn2009)))
legend("topleft", col = 1:3, pch = 16, bty = "n", cex = 0.9, 
       legend = c("1989", "1999", "2009"))
dev.off()

png("work_single_phc_s.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc89, south == 1 & migration == 0 & single == 1 & age > 38),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc99, south == 1 & migration == 0 & single == 1 & age > 48),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc09, south == 1 & migration == 0  & single == 1 & age > 58),
        weights = ~perwt,
        vcov = ~geo1_vn2009)))
legend("bottomright", col = 1:3, pch = 16, bty = "n", cex = 0.9, 
       legend = c("1989", "1999", "2009"))
dev.off()

png("work_single_phc_n.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc89, south == 0 & migration == 0 & single == 1 & age > 38),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc99, south == 0 & migration == 0 & single == 1 & age > 48),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc09, south == 0 & migration == 0  & single == 1 & age > 58),
        weights = ~perwt,
        vcov = ~geo1_vn2009)))
legend("bottomright", col = 1:3, pch = 16, bty = "n", cex = 0.9, 
       legend = c("1989", "1999", "2009"))
dev.off()

################################################
# Probability of working, by living with widow #
################################################

etable(list(
  feols(work ~ as.factor(widow_hh) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc89, south == 1 & migration == 0 & relate == 2 & age < 30 & female == 1),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(widow_hh) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc99, south == 1 & migration == 0 & relate == 2 & age < 40 & female == 1),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(widow_hh) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc09, south == 1 & migration == 0 & relate == 2 & age < 50 & female == 1),
        weights = ~perwt,
        vcov = ~geo1_vn2009)), tex = T)

etable(list(
  feols(work ~ as.factor(widow_hh) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc89, south == 1 & migration == 0 & relate == 2 & age < 30 & female == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(widow_hh) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc99, south == 1 & migration == 0 & relate == 2 & age < 40 & female == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(widow_hh) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc09, south == 1 & migration == 0 & relate == 2 & age < 50 & female == 0),
        weights = ~perwt,
        vcov = ~geo1_vn2009)), tex = T)

etable(list(
  feols(work ~ as.factor(widow_hh) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc89, south == 0 & migration == 0 & relate == 2 & age < 30 & female == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(widow_hh) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc99, south == 0 & migration == 0 & relate == 2 & age < 40 & female == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(widow_hh) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc09, south == 0 & migration == 0 & relate == 2 & age < 50 & female == 0),
        weights = ~perwt,
        vcov = ~geo1_vn2009)), tex = T)

etable(list(
  feols(work ~ as.factor(widow_hh) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc89, south == 0 & migration == 0 & relate == 2 & age < 30 & female == 1),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(widow_hh) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc99, south == 0 & migration == 0 & relate == 2 & age < 40 & female == 1),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(widow_hh) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc09, south == 0 & migration == 0 & relate == 2 & age < 50 & female == 1),
        weights = ~perwt,
        vcov = ~geo1_vn2009)), tex = T)

png("work_widowxfemale_phc_s.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), widow_hh) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc89, south == 1 & migration == 0 & relate == 2 & age < 30),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(female) + i(as.factor(female), widow_hh) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc99, south == 1 & migration == 0 & relate == 2 & age < 40),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(female) + i(as.factor(female), widow_hh) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc09, south == 1 & migration == 0 & relate == 2 & age < 50),
        weights = ~perwt,
        vcov = ~geo1_vn2009)), xlab = "Daughter")
legend("bottomleft", col = 1:3, pch = 16, bty = "n", cex = 0.9, 
       legend = c("1989", "1999", "2009"))
dev.off()

png("work_widowxfemale_phc_n.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), widow_hh) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc89, south == 0 & migration == 0 & relate == 2 & age < 30),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(female) + i(as.factor(female), widow_hh) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc99, south == 0 & migration == 0 & relate == 2 & age < 40),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(female) + i(as.factor(female), widow_hh) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) + as.factor(urban) + as.factor(minority) | regnvn,
        subset(phc09, south == 0 & migration == 0 & relate == 2 & age < 50),
        weights = ~perwt,
        vcov = ~geo1_vn2009)), xlab = "Daughter")
legend("bottomleft", col = 1:3, pch = 16, bty = "n", cex = 0.9, 
       legend = c("1989", "1999", "2009"))
dev.off()