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

png("work_phc.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + married,
        subset(phc89, migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov  + log(popdensgeo1)| regnvn + urban + minority + married,
        subset(phc99, migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + married,
        subset(phc09, migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn2009)))
legend("bottomleft", col = 1:3, pch = 16, bty = "n", cex = 0.9, 
       legend = c("1989", "1999", "2009"))
dev.off()

png("work_phc_n.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + married,
        subset(phc89, south == 0 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + married,
        subset(phc99, south == 0 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + married,
        subset(phc09, south == 0 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn2009)))
legend("bottomleft", col = 1:3, pch = 16, bty = "n", cex = 0.9, 
       legend = c("1989", "1999", "2009"))
dev.off()

png("work_phc_s.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + married,
        subset(phc89, south == 1 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + married,
        subset(phc99, south == 1 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + married,
        subset(phc09, south == 1 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn2009)))
legend("topleft", col = 1:3, pch = 16, bty = "n", cex = 0.9, 
       legend = c("1989", "1999", "2009"))
dev.off()

# Probability of working in agriculture 

png("agri_phc.png")
iplot(list(
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | age + regnvn + urban + minority + married,
        subset(phc89, work == 1 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | age + regnvn + urban + minority + married,
        subset(phc99, work == 1 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | age + regnvn + urban + minority + married,
        subset(phc09, work == 1 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn2009)))
legend("bottomleft", col = 1:3, pch = 16, bty = "n", cex = 0.9, 
       legend = c("1989", "1999", "2009"))
dev.off()

png("agri_phc_n.png")
iplot(list(
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + married,
        subset(phc89, work == 1 & south == 0 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + married,
        subset(phc99, work == 1 & south == 0 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + married,
        subset(phc09, work == 1 & south == 0 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn2009)))
legend("bottomleft", col = 1:3, pch = 16, bty = "n", cex = 0.9, 
       legend = c("1989", "1999", "2009"))
dev.off()

png("agri_phc_s.png")
iplot(list(
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + married,
        subset(phc89, work == 1 & south == 1 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + married,
        subset(phc99, work == 1 & south == 1 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + married,
        subset(phc09, work == 1 & south == 1 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn2009)))
legend("bottomleft", col = 1:3, pch = 16, bty = "n", cex = 0.9, 
       legend = c("1989", "1999", "2009"))
dev.off()

# Probability of working in manufacturing 

png("manu_phc_n.png")
iplot(list(
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + married,
        subset(phc89, work == 1 & south == 0 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + married,
        subset(phc99, work == 1 & south == 0 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + married,
        subset(phc09, work == 1 & south == 0 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn2009)))
legend("bottomleft", col = 1:3, pch = 16, bty = "n", cex = 0.9, 
       legend = c("1989", "1999", "2009"))
dev.off()

png("manu_phc_s.png")
iplot(list(
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + married,
        subset(phc89, work == 1 & south == 1 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + married,
        subset(phc99, work == 1 & south == 1 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + married,
        subset(phc09, work == 1 & south == 1 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn2009)))
legend("bottomleft", col = 1:3, pch = 16, bty = "n", cex = 0.9, 
       legend = c("1989", "1999", "2009"))
dev.off()