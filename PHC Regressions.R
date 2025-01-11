phc <- c("phc.Rda", "phc89.Rda", "phc99.Rda", "phc09.Rda", "phc19.Rda")

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

work_agexbmr_09_s <- tidy(feols(work ~ i(as.factor(age_75), tot_bmr_std) + as.factor(edattain) + nchild + log(popdensgeo2) + as.factor(urban) + as.factor(minority) + as.factor(married) + as.factor(migration) | age_75 + geo1_vn2009,
                           subset(phc09, south == 1 & female == 1 & age < 65 & age > 15),
                           weights = ~perwt,
                           vcov = ~geo2_vn2009 + age_75))%>% filter(grepl("tot_bmr_std", term))
work_agexbmr_09_n <- tidy(feols(work ~ i(as.factor(age_75), tot_bmr_std) + as.factor(edattain) + nchild + log(popdensgeo2) + as.factor(urban) + as.factor(minority) + as.factor(married) + as.factor(migration) | age_75 + geo1_vn2009,
                                subset(phc09, south == 0 & female == 1 & age < 65 & age > 15),
                                weights = ~perwt,
                                vcov = ~geo2_vn2009 + age_75))%>% filter(grepl("tot_bmr_std", term))

work_agexbmr_09_s$age_75 <- rep(seq(-18, 31), each = nrow(work_agexbmr_09_s) / length(seq(-18, 31)))
work_agexbmr_09_n$age_75 <- rep(seq(-18, 31), each = nrow(work_agexbmr_09_n) / length(seq(-18, 31)))

work_agexbmr_09_n$group <- "North"
work_agexbmr_09_s$group <- "South"
work_agexbmr_09_ns <- bind_rows(work_agexbmr_09_n, work_agexbmr_09_s) %>% filter(age_75 > -18)

## 2019

work_agexbmr_19_s <- tidy(feols(work ~ i(as.factor(age_75), tot_bmr_std) + as.factor(edattain) + nchild + log(popdensgeo2) + as.factor(urban) + as.factor(minority) + as.factor(married) + as.factor(migration) | age_75 + geo1_vn2019,
                                subset(phc19, south == 1 & female == 1 & age < 65 & age > 15),
                                weights = ~perwt,
                                vcov = ~geo2_vn2019 + age_75))%>% filter(grepl("tot_bmr_std", term))
work_agexbmr_19_n <- tidy(feols(work ~ i(as.factor(age_75), tot_bmr_std) + as.factor(edattain) + nchild + log(popdensgeo2) + as.factor(urban) + as.factor(minority) + as.factor(married) + as.factor(migration) | age_75 + geo1_vn2019,
                                subset(phc19, south == 0 & female == 1 & age < 65 & age > 15),
                                weights = ~perwt,
                                vcov = ~geo2_vn2019 + age_75))%>% filter(grepl("tot_bmr_std", term))

work_agexbmr_19_s$age_75 <- rep(seq(-27, 21), each = nrow(work_agexbmr_09_s) / length(seq(-27, 21)))
work_agexbmr_19_n$age_75 <- rep(seq(-27, 21), each = nrow(work_agexbmr_09_n) / length(seq(-27, 21)))

work_agexbmr_19_n$group <- "North"
work_agexbmr_19_s$group <- "South"
work_agexbmr_19_ns <- bind_rows(work_agexbmr_19_n, work_agexbmr_19_s) 

###########################################
# Probability of working - province level #
###########################################

etable(list(
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_prov_std) + as.factor(edattain) + nchild + age + age^2 + log(popdensgeo1) + as.factor(urban) + as.factor(minority) + as.factor(marst) + as.factor(migration),
        subset(phc89, south == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_prov_std) + as.factor(edattain) + nchild + age + age^2 + log(popdensgeo1) + as.factor(urban) + as.factor(minority) + as.factor(marst) + as.factor(migration),
        subset(phc89, south == 1),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_prov_std) + as.factor(edattain) + nchild + age + age^2 + log(popdensgeo1) + as.factor(urban) + as.factor(minority) + as.factor(marst) + as.factor(migration),
        subset(phc99, south == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_prov_std) + as.factor(edattain) + nchild + age + age^2 + log(popdensgeo1) + as.factor(urban) + as.factor(minority) + as.factor(marst) + as.factor(migration),
        subset(phc99, south == 1),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_prov_std) + as.factor(edattain) + nchild + age + age^2 + log(popdensgeo1) + as.factor(urban) + as.factor(minority) + as.factor(marst) + as.factor(migration),
        subset(phc09, south == 0),
        weights = ~perwt,
        vcov = ~geo1_vn2009),
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_prov_std) + as.factor(edattain) + nchild + age + age^2 + log(popdensgeo1) + as.factor(urban) + as.factor(minority) + as.factor(marst) + as.factor(migration),
        subset(phc09, south == 1),
        weights = ~perwt,
        vcov = ~geo1_vn2009),
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_prov_std) + as.factor(edattain) + nchild + age + age^2 + log(popdensgeo1) + as.factor(urban) + as.factor(minority) + as.factor(marst) + as.factor(migration),
        subset(phc19, south == 0),
        weights = ~perwt,
        vcov = ~geo1_vn2019),
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_prov_std) + as.factor(edattain) + nchild + age + age^2 + log(popdensgeo1) + as.factor(urban) + as.factor(minority) + as.factor(marst) + as.factor(migration),
        subset(phc19, south == 1),
        weights = ~perwt,
        vcov = ~geo1_vn2019)), tex = T)

##########################################################
# Economic consequence of being a widow - province level #
##########################################################

etable(list(
  feols(work ~ as.factor(widowed) + as.factor(edattain) + age + age^2 + log(popdensgeo2) + as.factor(urban) + as.factor(minority) + as.factor(migration) + nchild | geo1_vn2009,
        subset(phc09, south == 1 & female == 1 & age > (2009-(1975-15))),
        weights = ~perwt,
        vcov = ~geo2_vn2009),
  feols(work ~ as.factor(widowed) + as.factor(edattain) + age + age^2 + log(popdensgeo2) + as.factor(urban) + as.factor(minority) + as.factor(migration) + nchild | geo1_vn2019,
        subset(phc19, south == 1 & female == 1 & age > (2019-(1975-15))),
        weights = ~perwt,
        vcov = ~geo2_vn2019)), tex = T)

etable(list(
  feols(work ~ as.factor(widowed) + as.factor(edattain) + age + age^2 + log(popdensgeo2) + as.factor(urban) + as.factor(minority) + as.factor(migration) | geo1_vn2009,
        subset(phc09, south == 1 & female == 1 & age > (2009-(1975-15)) & nchild == 0),
        weights = ~perwt,
        vcov = ~geo2_vn2009),
  feols(work ~ as.factor(widowed) + as.factor(edattain) + age + age^2 + log(popdensgeo2) + as.factor(urban) + as.factor(minority) + as.factor(migration) + nchild | geo1_vn2009,
        subset(phc09, south == 1 & female == 1 & age > (2009-(1975-15)) & nchild > 0),
        weights = ~perwt,
        vcov = ~geo2_vn2009)), tex = T)

##########################################################
# Economic consequence of being a widow - province level #
##########################################################

etable(list(
  feols(work ~ as.factor(widowed) + as.factor(edattain) + age + age^2 + as.factor(urban) + as.factor(minority) + nchild + famsize | geo1_vn1989,
        subset(phc89, south == 1 & female == 1 & age > (1989-(1975-15)) & migration == 0 & evermarried == 1),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(widowed) + as.factor(edattain) + age + age^2 + as.factor(urban) + as.factor(minority) + nchild + famsize | geo1_vn1999,
        subset(phc99, south == 1 & female == 1 & age > (1999-(1975-15)) & migration == 0 & evermarried == 1),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(widowed) + as.factor(edattain) + age + age^2 + as.factor(urban) + as.factor(minority) + nchild + famsize | geo1_vn2009,
        subset(phc09, south == 1 & female == 1 & age > (2009-(1975-15)) & migration == 0 & evermarried == 1),
        weights = ~perwt,
        vcov = ~geo1_vn2009),
  feols(work ~ as.factor(widowed) + as.factor(edattain) + age + age^2 + as.factor(urban) + as.factor(minority) + nchild + famsize | geo1_vn2019,
        subset(phc19, south == 1 & female == 1 & age > (2019-(1975-15)) & migration == 0 & evermarried == 1),
        weights = ~perwt,
        vcov = ~geo1_vn2019)), tex = T)

etable(list(
  feols(work ~ as.factor(widowed) + as.factor(edattain) + age + age^2 + as.factor(urban) + as.factor(minority) + famsize | geo1_vn1989,
        subset(phc89, south == 1 & female == 1 & age > (1989-(1975-15)) & chborn == 0 & migration == 0 & evermarried == 1),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(widowed) + as.factor(edattain) + age + age^2 + as.factor(urban) + as.factor(minority) + famsize | geo1_vn1989,
        subset(phc89, south == 1 & female == 1 & age > (1989-(1975-15)) & chborn > 0 & chborn < 98 & migration == 0 & evermarried == 1),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(widowed) + as.factor(edattain) + age + age^2 + as.factor(urban) + as.factor(minority) + famsize | geo1_vn1999,
        subset(phc99, south == 1 & female == 1 & age > (1999-(1975-15)) & chborn == 0 & migration == 0 & evermarried == 1),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(widowed) + as.factor(edattain) + age + age^2 + as.factor(urban) + as.factor(minority) + famsize | geo1_vn1999,
        subset(phc99, south == 1 & female == 1 & age > (1999-(1975-15)) & chborn > 0 & chborn < 98 & migration == 0 & evermarried == 1),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(widowed) + as.factor(edattain) + age + age^2 + as.factor(urban) + as.factor(minority) + famsize| geo1_vn2009,
        subset(phc09, south == 1 & female == 1 & age > (2009-(1975-15)) & nchild == 0 & migration == 0 & evermarried == 1),
        weights = ~perwt,
        vcov = ~geo1_vn2009),
  feols(work ~ as.factor(widowed) + as.factor(edattain) + age + age^2 + as.factor(urban) + as.factor(minority) + famsize | geo1_vn2009,
        subset(phc09, south == 1 & female == 1 & age > (2009-(1975-15)) & nchild > 0 & migration == 0 & evermarried == 1),
        weights = ~perwt,
        vcov = ~geo1_vn2009),
  feols(work ~ as.factor(widowed) + as.factor(edattain) + age + age^2 + as.factor(urban) + as.factor(minority) + famsize | geo1_vn2019,
        subset(phc19, south == 1 & female == 1 & age > (2019-(1975-15)) & nchild == 0 & migration == 0 & evermarried == 1),
        weights = ~perwt,
        vcov = ~geo1_vn2019),
  feols(work ~ as.factor(widowed) + as.factor(edattain) + age + age^2 + as.factor(urban) + as.factor(minority) + famsize | geo1_vn2019,
        subset(phc19, south == 1 & female == 1 & age > (2019-(1975-15)) & nchild > 0 & migration == 0 & evermarried == 1),
        weights = ~perwt,
        vcov = ~geo1_vn2019)), tex = T)

#################################################
# Probability of working of daughters of widows #
#################################################

etable(feols(work ~ as.factor(widowed_mother) + age + age^2 + as.factor(urban) + as.factor(minority) + famsize + as.factor(edattain) | geo1_vn1989,
             subset(phc89, south == 1 & female == 1),
             weights = ~perwt,
             vcov = ~geo1_vn1989),
       feols(work ~ as.factor(widowed_mother) + age + age^2 + as.factor(urban) + as.factor(minority) + famsize + as.factor(edattain) | geo1_vn1999,
             subset(phc99, south == 1 & female == 1),
             weights = ~perwt,
             vcov = ~geo1_vn1999),
       feols(work ~ as.factor(widowed_mother) + age + age^2 + as.factor(urban) + as.factor(minority) + famsize + as.factor(edattain) | geo1_vn2009,
             subset(phc09, south == 1 & female == 1),
             weights = ~perwt,
             vcov = ~geo1_vn2009),
       feols(work ~ as.factor(widowed_mother) + age + age^2 + as.factor(urban) + as.factor(minority) + famsize + as.factor(edattain) | geo1_vn2019,
             subset(phc19, south == 1 & female == 1),
             weights = ~perwt,
             vcov = ~geo1_vn2019), tex = T)

etable(feols(work ~ as.factor(widowed_mother) + age + age^2 + as.factor(urban) + as.factor(minority) + famsize + as.factor(edattain) | geo1_vn1989,
             subset(phc89, south == 1 & female == 0),
             weights = ~perwt,
             vcov = ~geo1_vn1989),
       feols(work ~ as.factor(widowed_mother) + age + age^2 + as.factor(urban) + as.factor(minority) + famsize + as.factor(edattain) | geo1_vn1999,
             subset(phc99, south == 1 & female == 0),
             weights = ~perwt,
             vcov = ~geo1_vn1999),
       feols(work ~ as.factor(widowed_mother) + age + age^2 + as.factor(urban) + as.factor(minority) + famsize + as.factor(edattain) | geo1_vn2009,
             subset(phc09, south == 1 & female == 0),
             weights = ~perwt,
             vcov = ~geo1_vn2009),
       feols(work ~ as.factor(widowed_mother) + age + age^2 + as.factor(urban) + as.factor(minority) + famsize + as.factor(edattain) | geo1_vn2019,
             subset(phc19, south == 1 & female == 0),
             weights = ~perwt,
             vcov = ~geo1_vn2019), tex = T)

etable(feols(yrschool ~ as.factor(widowed_mother) + age + age^2 + as.factor(urban) + as.factor(minority) + as.factor(migration)| geo1_vn1989,
             subset(phc89, south == 1 & female == 1),
             weights = ~perwt,
             vcov = ~geo1_vn1989),
       feols(yrschool ~ as.factor(widowed_mother) + age + age^2 + as.factor(urban) + as.factor(minority) + as.factor(migration)| geo1_vn1999,
             subset(phc99, south == 1 & female == 1),
             weights = ~perwt,
             vcov = ~geo1_vn1999),
       feols(yrschool ~ as.factor(widowed_mother) + age + age^2 + as.factor(urban) + as.factor(minority) + as.factor(migration)| geo1_vn2009,
             subset(phc09, south == 1 & female == 1),
             weights = ~perwt,
             vcov = ~geo1_vn2009),
       feols(yrschool ~ as.factor(widowed_mother) + age + age^2 + as.factor(urban) + as.factor(minority) + as.factor(migration)| geo1_vn2019,
             subset(phc19, south == 1 & female == 1),
             weights = ~perwt,
             vcov = ~geo1_vn2019), tex = T)

etable(feols(yrschool ~ as.factor(widowed_mother) + age + age^2 + as.factor(urban) + as.factor(minority) + as.factor(migration) | geo1_vn1989,
             subset(phc89, south == 1 & female == 0),
             weights = ~perwt,
             vcov = ~geo1_vn1989),
       feols(yrschool ~ as.factor(widowed_mother) + age + age^2 + as.factor(urban) + as.factor(minority) + as.factor(migration)| geo1_vn1999,
             subset(phc99, south == 1 & female == 0),
             weights = ~perwt,
             vcov = ~geo1_vn1999),
       feols(yrschool ~ as.factor(widowed_mother) + age + age^2 + as.factor(urban) + as.factor(minority) + as.factor(migration)| geo1_vn2009,
             subset(phc09, south == 1 & female == 0),
             weights = ~perwt,
             vcov = ~geo1_vn2009),
       feols(yrschool ~ as.factor(widowed_mother) + age + age^2 + as.factor(urban) + as.factor(minority) + as.factor(migration) | geo1_vn2019,
             subset(phc19, south == 1 & female == 0),
             weights = ~perwt,
             vcov = ~geo1_vn2019), tex = T)

##########################################
# Age of first marriage - district level #
##########################################

png("agemarr_19_s.png")
iplot(feols(agemarr ~ i(age_75, tot_bmr_std) + as.factor(edattain) + as.factor(urban) + as.factor(minority) + as.factor(migration) + log(popdensgeo2) | age_75 + geo1_vn2019 + marryr,
            subset(phc19, south == 1 & female == 1 & age < 65 & age > 15 & agemarr > 0 & agemarr < 99 & age_75 > -24),
            weights = ~perwt,
            vcov = ~geo2_vn2019 + age_75), xlab = "Age in 1975", vline = 0)
dev.off()

etable(list(
  feols(spagegap ~ tot_bmr_std + as.factor(edattain) + log(popdensgeo2) + as.factor(urban) + as.factor(minority) + as.factor(migration) | geo1_vn2019,
        subset(phc19, south == 1 & age < 65 & age > 15 & female == 1),
        weights = ~perwt,
        vcov = ~geo2_vn2019),
  feols(speducgap ~ tot_bmr_std + log(popdensgeo2) + as.factor(urban) + as.factor(minority) + as.factor(migration) | geo1_vn2019,
        subset(phc19, south == 1 & age < 65 & age > 15 & female == 1),
        weights = ~perwt,
        vcov = ~geo2_vn2019)
))


######################################
# Substitution towards female labour #
######################################

png("manu_phc_FE_s.png")
iplot(list(
  feols(manu ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + as.factor(edattain) + nchild + age + age^2 + log(popdensgeo2) + as.factor(urban) + as.factor(minority) + as.factor(marst) + as.factor(migration) | geo1_vn2009,
        subset(phc09, south == 1),
        weights = ~perwt,
        vcov = ~geo2_vn2009),
  feols(manu ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + as.factor(edattain) + nchild + age + age^2 + log(popdensgeo2) + as.factor(urban) + as.factor(minority) + as.factor(marst) + as.factor(migration) | geo1_vn2019,
        subset(phc19, south == 1),
        weights = ~perwt,
        vcov = ~geo2_vn2019)))
legend("topleft", col = 1:2, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2009", "2019"))
dev.off()

png("construction_phc_FE_s.png")
iplot(list(
  feols(indgen == 50 ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + as.factor(edattain) + nchild + age + age^2 + log(popdensgeo2) + as.factor(urban) + as.factor(minority) + as.factor(marst) + as.factor(migration) | geo1_vn2009,
        subset(phc09, south == 1 & work == 1),
        weights = ~perwt,
        vcov = ~geo2_vn2009),
  feols(indgen == 50 ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + as.factor(edattain) + nchild + age + age^2 + log(popdensgeo2) + as.factor(urban) + as.factor(minority) + as.factor(marst) + as.factor(migration) | geo1_vn2019,
        subset(phc19, south == 1 & work == 1),
        weights = ~perwt,
        vcov = ~geo2_vn2019)))
legend("topleft", col = 1:2, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2009", "2019"))
dev.off()

png("services_phc_FE_s.png")
iplot(list(
  feols(indgen == 60 | indgen == 70 | indgen == 110 | indgen < 113 & indgen < 130  ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + as.factor(edattain) + nchild + age + age^2 + log(popdensgeo2) + as.factor(urban) + as.factor(minority) + as.factor(marst) + as.factor(migration) | geo1_vn2009,
        subset(phc09, south == 1 & work == 1),
        weights = ~perwt,
        vcov = ~geo2_vn2009),
  feols(indgen == 60 | indgen == 70 | indgen == 110 | indgen < 113 & indgen < 130 ~ as.factor(female) + i(as.factor(female), tot_bmr_std) + as.factor(edattain) + nchild + age + age^2 + log(popdensgeo2) + as.factor(urban) + as.factor(minority) + as.factor(marst) + as.factor(migration) | geo1_vn2019,
        subset(phc19, south == 1 & work == 1),
        weights = ~perwt,
        vcov = ~geo2_vn2019)))
legend("topleft", col = 1:2, pch = 16, bty = "n", cex = 0.9, 
       legend = c("2009", "2019"))
dev.off()
