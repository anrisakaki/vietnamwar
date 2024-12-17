phc <- c("phc89.Rda", "phc99.Rda", "phc09.Rda", "phc19.Rda")

for (i in phc) {
  load(i)
}

dict = c("as.factor(female)" = "Female")

setFixest_coefplot(dict = dict, grid = FALSE, zero.par = list(lty = 2), main = " ")

###########################################
# Probability of working - district level #
###########################################

# FE Model

etable(list(
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + as.factor(edattain) + nchild + age + age^2 + log(popdensgeo2) + as.factor(urban) + as.factor(minority) + as.factor(marst) + as.factor(migration) | geo1_vn2009,
        subset(phc09, south == 0 & age < 65 & age > 15),
        weights = ~perwt,
        vcov = ~geo2_vn2009 + age),
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + as.factor(edattain) + nchild + age + age^2 + log(popdensgeo2) + as.factor(urban) + as.factor(minority) + as.factor(marst) + as.factor(migration) | geo1_vn2009,
        subset(phc09, south == 1 & age < 65 & age > 15),
        weights = ~perwt,
        vcov = ~geo2_vn2009 + age),
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + as.factor(edattain) + nchild + age + age^2 + log(popdensgeo2) + as.factor(urban) + as.factor(minority) + as.factor(marst) + as.factor(migration) | geo1_vn2019,
        subset(phc19, south == 0 & age < 65 & age > 15),
        weights = ~perwt,
        vcov = ~geo2_vn2019 + age),
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + as.factor(edattain) + nchild + age + age^2 + log(popdensgeo2) + as.factor(urban) + as.factor(minority) + as.factor(marst) + as.factor(migration) | geo1_vn2019,
        subset(phc19, south == 1 & age < 65 & age > 15),
        weights = ~perwt,
        vcov = ~geo2_vn2019 + age)), tex = T)

# DiD Model

## 2009

work_agexbmr_09_s <- tidy(feols(work ~ i(as.factor(age), tot_bmr_std) + as.factor(edattain) + nchild + log(popdensgeo2) + as.factor(urban) + as.factor(minority) + as.factor(married) + as.factor(migration) | age + geo1_vn2009,
                           subset(phc09, south == 1 & female == 1 & age < 65 & age > 15),
                           weights = ~perwt,
                           vcov = ~geo2_vn2009 + age))%>% filter(grepl("tot_bmr_std", term))
work_agexbmr_09_n <- tidy(feols(work ~ i(as.factor(age), tot_bmr_std) + as.factor(edattain) + nchild + log(popdensgeo2) + as.factor(urban) + as.factor(minority) + as.factor(married) + as.factor(migration) | age + geo1_vn2009,
                                subset(phc09, south == 0 & female == 1 & age < 65 & age > 15),
                                weights = ~perwt,
                                vcov = ~geo2_vn2009 + age))%>% filter(grepl("tot_bmr_std", term))

work_agexbmr_09_s$age <- rep(seq(17, 64), each = nrow(work_agexbmr_09_s) / length(seq(17, 64)))
work_agexbmr_09_n$age <- rep(seq(17, 64), each = nrow(work_agexbmr_09_n) / length(seq(17, 64)))

work_agexbmr_09_n$group <- "North"
work_agexbmr_09_s$group <- "South"
work_agexbmr_09_ns <- bind_rows(work_agexbmr_09_n, work_agexbmr_09_s) %>% mutate(age_75 = 1975 - (2009 - age))

## 2019

work_agexbmr_19_s <- tidy(feols(work ~ i(as.factor(age), tot_bmr_std) + as.factor(edattain) + nchild + log(popdensgeo2) + as.factor(urban) + as.factor(minority) + as.factor(married) + as.factor(migration) | age + geo1_vn2019,
                                subset(phc19, south == 1 & female == 1 & age < 65 & age > 15),
                                weights = ~perwt,
                                vcov = ~geo2_vn2019 + age))%>% filter(grepl("tot_bmr_std", term))
work_agexbmr_19_n <- tidy(feols(work ~ i(as.factor(age), tot_bmr_std) + as.factor(edattain) + nchild + log(popdensgeo2) + as.factor(urban) + as.factor(minority) + as.factor(married) + as.factor(migration) | age + geo1_vn2019,
                                subset(phc19, south == 0 & female == 1 & age < 65 & age > 15),
                                weights = ~perwt,
                                vcov = ~geo2_vn2019 + age))%>% filter(grepl("tot_bmr_std", term))

work_agexbmr_19_s$age <- rep(seq(17, 64), each = nrow(work_agexbmr_09_s) / length(seq(17, 64)))
work_agexbmr_19_n$age <- rep(seq(17, 64), each = nrow(work_agexbmr_09_n) / length(seq(17, 64)))

work_agexbmr_19_n$group <- "North"
work_agexbmr_19_s$group <- "South"
work_agexbmr_19_ns <- bind_rows(work_agexbmr_19_n, work_agexbmr_19_s) %>% mutate(age_75 = 1975 - (2019 - age))

###########################################
# Probability of working - province level #
###########################################

png("work_phc_n.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_prov_std) + as.factor(edattain) + nchild + age + age^2 + log(popdensgeo1) + as.factor(urban) + as.factor(minority) + as.factor(marst) + as.factor(migration),
        subset(phc89, south == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_prov_std) + as.factor(edattain) + nchild + age + age^2 + log(popdensgeo1) + as.factor(urban) + as.factor(minority) + as.factor(marst) + as.factor(migration),
        subset(phc99, south == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_prov_std) + as.factor(edattain) + nchild + age + age^2 + log(popdensgeo1) + as.factor(urban) + as.factor(minority) + as.factor(marst) + as.factor(migration),
        subset(phc09, south == 0),
        weights = ~perwt,
        vcov = ~geo1_vn2009),
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_prov_std) + as.factor(edattain) + nchild + age + age^2 + log(popdensgeo1) + as.factor(urban) + as.factor(minority) + as.factor(marst) + as.factor(migration),
        subset(phc19, south == 0),
        weights = ~perwt,
        vcov = ~geo1_vn2019)))
legend("bottomleft", col = 1:4, pch = 16, bty = "n", cex = 0.9, 
       legend = c("1989", "1999", "2009", "2019"))
dev.off()

png("work_phc_s.png")
iplot(list(
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_prov_std) + as.factor(yrschool) + nchild + age + age^2 + log(popdensgeo1) + as.factor(urban) + as.factor(minority) + as.factor(marst) + as.factor(migration),
        subset(phc89, south == 1),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_prov_std) + as.factor(edattain) + nchild + age + age^2 + log(popdensgeo1) + as.factor(urban) + as.factor(minority) + as.factor(marst) + as.factor(migration),
        subset(phc99, south == 1),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_prov_std) + as.factor(edattain) + nchild + age + age^2 + log(popdensgeo1) + as.factor(urban) + as.factor(minority) + as.factor(marst) + as.factor(migration),
        subset(phc09, south == 1),
        weights = ~perwt,
        vcov = ~geo1_vn2009),
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_prov_std) + as.factor(edattain) + nchild + age + age^2 + log(popdensgeo1) + as.factor(urban) + as.factor(minority) + as.factor(marst) + as.factor(migration),
        subset(phc19, south == 1),
        weights = ~perwt,
        vcov = ~geo1_vn2019)))
legend("bottomleft", col = 1:4, pch = 16, bty = "n", cex = 0.9, 
       legend = c("1989", "1999", "2009", "2019"))
dev.off()