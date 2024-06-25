phc <- c("phc89.Rda", "phc99.Rda", "phc09.Rda")

for (i in phc) {
  load(i)
}

dict = c("as.factor(female)" = "Female")

setFixest_coefplot(dict = dict, grid = FALSE, zero.par = list(lty = 2), main = " ")

###########################################
# Probability of working - district level #
###########################################
# Cross sectional 

work_agexbmr_cs_09_s <- tidy(feols(work ~ i(as.factor(age), log(tot_bmr)) + yrschool + nchild + log(popdensgeo2) + as.factor(urban) + as.factor(minority) + as.factor(married) | age,
                                subset(phc09, south == 1 & migration == 0 & female == 1 & age < 65 & age > 15),
                                weights = ~perwt,
                                vcov = ~geo2_vn2009 + age))%>% filter(grepl("log\\(tot_bmr\\)", term))
work_agexbmr_cs_09_n <- tidy(feols(work ~ i(as.factor(age), log(tot_bmr)) + yrschool + nchild + log(popdensgeo2) + as.factor(urban) + as.factor(minority) + as.factor(married) | age,
                                subset(phc09, south == 0 & migration == 0 & female == 1 & age < 65 & age > 15),
                                weights = ~perwt,
                                vcov = ~geo2_vn2009 + age))%>% filter(grepl("log\\(tot_bmr\\)", term))

work_agexbmr_cs_09_s$age <- rep(seq(17, 64), each = nrow(work_agexbmr_cs_09_s) / length(seq(17, 64)))
work_agexbmr_cs_09_n$age <- rep(seq(17, 64), each = nrow(work_agexbmr_cs_09_n) / length(seq(17, 64)))

work_agexbmr_cs_09_n$group <- "North"
work_agexbmr_cs_09_s$group <- "South"
work_agexbmr_cs_09_ns <- bind_rows(work_agexbmr_cs_09_n, work_agexbmr_cs_09_s)

# Province FE 

work_agexbmr_09_s <- tidy(feols(work ~ i(as.factor(age), log(tot_bmr)) + yrschool + nchild + log(popdensgeo2) + as.factor(urban) + as.factor(minority) + as.factor(married) | age + geo1_vn2009,
                           subset(phc09, south == 1 & migration == 0 & female == 1 & age < 65 & age > 15),
                           weights = ~perwt,
                           vcov = ~geo2_vn2009 + age))%>% filter(grepl("log\\(tot_bmr\\)", term))
work_agexbmr_09_n <- tidy(feols(work ~ i(as.factor(age), log(tot_bmr)) + yrschool + nchild + log(popdensgeo2) + as.factor(urban) + as.factor(minority) + as.factor(married) | age + geo1_vn2009,
                                subset(phc09, south == 0 & migration == 0 & female == 1 & age < 65 & age > 15),
                                weights = ~perwt,
                                vcov = ~geo2_vn2009 + age))%>% filter(grepl("log\\(tot_bmr\\)", term))

work_agexbmr_09_s$age <- rep(seq(17, 64), each = nrow(work_agexbmr_09_s) / length(seq(17, 64)))
work_agexbmr_09_n$age <- rep(seq(17, 64), each = nrow(work_agexbmr_09_n) / length(seq(17, 64)))

work_agexbmr_09_n$group <- "North"
work_agexbmr_09_s$group <- "South"
work_agexbmr_09_ns <- bind_rows(work_agexbmr_09_n, work_agexbmr_09_s)

# By casualties 

work_agexbmr_09_cas <- tidy(feols(work ~ i(as.factor(age), log(killed_tot)) + yrschool + nchild + log(popdensgeo2) + as.factor(urban) + as.factor(minority) + as.factor(married) | age + geo1_vn2009,
                                  subset(phc09, south == 1 & migration == 0 & female == 1 & age < 65 & age > 15),
                                  weights = ~perwt,
                                  vcov = ~geo2_vn2009 + age)) %>% 
  filter(grepl("log\\(killed_tot\\)", term))

work_agexbmr_09_cas$age <- rep(seq(17, 64), each = nrow(work_agexbmr_09_cas) / length(seq(17, 64)))

#############################################
# Probability of working by education level #
#############################################

# 1989

png("work_edattain_phc89_s.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc89, south == 1 & migration == 0 & edattain < 2),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc89, south == 1 & migration == 0 & edattain == 2),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc89, south == 1 & migration == 0 & edattain == 3),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc89, south == 1 & migration == 0 & edattain > 3 & edattain < 9),
        weights = ~perwt,
        vcov = ~geo1_vn1989)))
legend("topleft", col = 1:4, pch = 16, bty = "n", cex = 0.9, 
       legend = c("Less than primary", "Primary", "Secondary", "University"))
dev.off()

png("work_edattain_phc89_n.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc89, south == 0 & migration == 0 & edattain < 2),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc89, south == 0 & migration == 0 & edattain == 2),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc89, south == 0 & migration == 0 & edattain == 3),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc89, south == 0 & migration == 0 & edattain > 3 & edattain < 9),
        weights = ~perwt,
        vcov = ~geo1_vn1989)))
legend("bottomleft", col = 1:4, pch = 16, bty = "n", cex = 0.9, 
       legend = c("Less than primary", "Primary", "Secondary", "University"))
dev.off()

# 1999

png("work_edattain_phc99_s.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc99, south == 1 & migration == 0 & edattain < 2),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc99, south == 1 & migration == 0 & edattain == 2),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc99, south == 1 & migration == 0 & edattain == 3),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc99, south == 1 & migration == 0 & edattain > 3 & edattain < 9),
        weights = ~perwt,
        vcov = ~geo1_vn1999)))
legend("topleft", col = 1:4, pch = 16, bty = "n", cex = 0.9, 
       legend = c("Less than primary", "Primary", "Secondary", "University"))
dev.off()

png("work_edattain_phc99_n.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc99, south == 0 & migration == 0 & edattain < 2),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc99, south == 0 & migration == 0 & edattain == 2),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc99, south == 0 & migration == 0 & edattain == 3),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc99, south == 0 & migration == 0 & edattain > 3 & edattain < 9),
        weights = ~perwt,
        vcov = ~geo1_vn1999)))
legend("bottomleft", col = 1:4, pch = 16, bty = "n", cex = 0.9, 
       legend = c("Less than primary", "Primary", "Secondary", "University"))
dev.off()

# 2009

png("work_edattain_phc09_s.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc09, south == 1 & migration == 0 & edattain < 2),
        weights = ~perwt,
        vcov = ~geo1_vn2009),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc09, south == 1 & migration == 0 & edattain == 2),
        weights = ~perwt,
        vcov = ~geo1_vn2009),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc09, south == 1 & migration == 0 & edattain == 3),
        weights = ~perwt,
        vcov = ~geo1_vn2009),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc09, south == 1 & migration == 0 & edattain > 3 & edattain < 9),
        weights = ~perwt,
        vcov = ~geo1_vn2009)))
legend("topleft", col = 1:4, pch = 16, bty = "n", cex = 0.9, 
       legend = c("Less than primary", "Primary", "Secondary", "University"))
dev.off()

png("work_edattain_phc09_n.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc09, south == 0 & migration == 0 & edattain < 2),
        weights = ~perwt,
        vcov = ~geo1_vn2009),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc09, south == 0 & migration == 0 & edattain == 2),
        weights = ~perwt,
        vcov = ~geo1_vn2009),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc09, south == 0 & migration == 0 & edattain == 3),
        weights = ~perwt,
        vcov = ~geo1_vn2009),
  feols(work ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + nchild + age + age^2 + dist_nearest_base_prov + dist_nearest_hochi_prov + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc09, south == 0 & migration == 0 & edattain > 3 & edattain < 9),
        weights = ~perwt,
        vcov = ~geo1_vn2009)))
legend("bottomleft", col = 1:4, pch = 16, bty = "n", cex = 0.9, 
       legend = c("Less than primary", "Primary", "Secondary", "University"))
dev.off()

###############################################
# Probability of working in different sectors #
###############################################

# Agriculture 

## Per population 

png("agri_phc_ppn_n.png")
iplot(list(
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc89, work == 1 & south == 0 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc99, work == 1 & south == 0 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc09, work == 1 & south == 0 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn2009)))
legend("bottomleft", col = 1:3, pch = 16, bty = "n", cex = 0.9, 
       legend = c("1989", "1999", "2009"))
dev.off()

png("agri_phc_ppn_s.png")
iplot(list(
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc89, work == 1 & south == 1 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc99, work == 1 & south == 1 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc09, work == 1 & south == 1 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn2009)))
legend("bottomleft", col = 1:3, pch = 16, bty = "n", cex = 0.9, 
       legend = c("1989", "1999", "2009"))
dev.off()

## Aggregate 

png("agri_phc_n.png")
iplot(list(
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + yrschool + nchild + age + age^2 + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc89, work == 1 & south == 0 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + yrschool + nchild + age + age^2 + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc99, work == 1 & south == 0 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + yrschool + nchild + age + age^2 + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc09, work == 1 & south == 0 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn2009)))
legend("bottomleft", col = 1:3, pch = 16, bty = "n", cex = 0.9, 
       legend = c("1989", "1999", "2009"))
dev.off()

png("agri_phc_s.png")
iplot(list(
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + yrschool + nchild + age + age^2 + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc89, work == 1 & south == 1 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + yrschool + nchild + age + age^2 + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc99, work == 1 & south == 1 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(agri ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + yrschool + nchild + age + age^2 + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc09, work == 1 & south == 1 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn2009)))
legend("bottomleft", col = 1:3, pch = 16, bty = "n", cex = 0.9, 
       legend = c("1989", "1999", "2009"))
dev.off()

# Manufacturing 

## Per population 

png("manu_phc_ppn_n.png")
iplot(list(
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc89, south == 0 & migration == 0 & work == 1),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc99, south == 0 & migration == 0 & work == 1),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc09, south == 0 & migration == 0 & work == 1),
        weights = ~perwt,
        vcov = ~geo1_vn2009)))
legend("bottomleft", col = 1:3, pch = 16, bty = "n", cex = 0.9, 
       legend = c("1989", "1999", "2009"))
dev.off()

png("manu_phc_ppn_s.png")
iplot(list(
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc89, south == 1 & migration == 0 & work == 1),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc99, south == 1 & migration == 0 & work == 1),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov_ppn)) + yrschool + nchild + age + age^2 + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc09, south == 1 & migration == 0 & work == 1),
        weights = ~perwt,
        vcov = ~geo1_vn2009)))
legend("bottomleft", col = 1:3, pch = 16, bty = "n", cex = 0.9, 
       legend = c("1989", "1999", "2009"))
dev.off()

## Aggregate 

png("manu_phc_n.png")
iplot(list(
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + yrschool + nchild + age + age^2 + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc89, south == 0 & migration == 0 & work == 1),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + yrschool + nchild + age + age^2 + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc99, south == 0 & migration == 0 & work == 1),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + yrschool + nchild + age + age^2 + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc09, south == 0 & migration == 0 & work == 1),
        weights = ~perwt,
        vcov = ~geo1_vn2009)))
legend("bottomleft", col = 1:3, pch = 16, bty = "n", cex = 0.9, 
       legend = c("1989", "1999", "2009"))
dev.off()

png("manu_phc_s.png")
iplot(list(
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + yrschool + nchild + age + age^2 + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc89, south == 1 & migration == 0 & work == 1),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + yrschool + nchild + age + age^2 + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc99, south == 1 & migration == 0 & work == 1),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(manu ~ as.factor(female) + i(as.factor(female), log(tot_bmr_prov)) + yrschool + nchild + age + age^2 + log(popdensgeo1) | regnvn + urban + minority + marst,
        subset(phc09, south == 1 & migration == 0 & work == 1),
        weights = ~perwt,
        vcov = ~geo1_vn2009)))
legend("bottomleft", col = 1:3, pch = 16, bty = "n", cex = 0.9, 
       legend = c("1989", "1999", "2009"))
dev.off()
