###########################################################
# BMR VS SEX RATIO OF WORKERS, BY SECTOR - DISTRICT LEVEL #
###########################################################

ind_workerratio_01_n <- list(
  # Agriculture
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn02, south == 0 & nganh_kd2 == 1),
        vcov = ~tinh+huyen),
  # F&B 
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn02, south == 0 & nganh_kd2 == 15),
        vcov = ~tinh+huyen),
  # Textiles
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn02, south == 0 & nganh_kd2 == 17),
        vcov = ~tinh+huyen),
  # Apparel
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn02, south == 0 & nganh_kd2 == 18),
        vcov = ~tinh+huyen),
  # Leather
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn02, south == 0 & nganh_kd2 == 19),
        vcov = ~tinh+huyen),
  # Construction 
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn02, south == 0 & nganh_kd2 == 45),
        vcov = ~tinh+huyen),
  # Wholesale trade                               
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn02, south == 0 & nganh_kd2 == 51),
        vcov = ~tinh+huyen),
  # Retail trade
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn02, south == 0 & nganh_kd2 == 52),
        vcov = ~tinh+huyen),
  # Hotels and restaurants 
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn02, south == 0 & nganh_kd2 == 55),
        vcov = ~tinh+huyen),
  # Financial intermediation 
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn02, south == 0 & nganh_kd2 == 55),
        vcov = ~tinh+huyen),
  # Business support services 
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn02, south == 0 & nganh_kd2 == 74),
        vcov = ~tinh+huyen)
)

ind_workerratio_01_s <- list(
  # Agriculture
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn02, south == 1 & nganh_kd2 == 1),
        vcov = ~tinh+huyen),
  # F&B 
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn02, south == 1 & nganh_kd2 == 15),
        vcov = ~tinh+huyen),
  # Textiles
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn02, south == 1 & nganh_kd2 == 17),
        vcov = ~tinh+huyen),
  # Apparel
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn02, south == 1 & nganh_kd2 == 18),
        vcov = ~tinh+huyen),
  # Leather
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn02, south == 1 & nganh_kd2 == 19),
        vcov = ~tinh+huyen),
  # Construction 
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn02, south == 1 & nganh_kd2 == 45),
        vcov = ~tinh+huyen),
  # Wholesale trade                               
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn02, south == 1 & nganh_kd2 == 51),
        vcov = ~tinh+huyen),
  # Retail trade
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn02, south == 1 & nganh_kd2 == 52),
        vcov = ~tinh+huyen),
  # Hotels and restaurants 
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn02, south == 1 & nganh_kd2 == 55),
        vcov = ~tinh+huyen),
  # Financial intermediation 
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn02, south == 1 & nganh_kd2 == 55),
        vcov = ~tinh+huyen),
  # Business support services 
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn02, south == 1 & nganh_kd2 == 74),
        vcov = ~tinh+huyen)
)

sectors <- c("Agriculture", "F&B", "Textiles", "Apparel", "Leather", 
             "Construction", "Wholesale trade", "Retail trade", "Hotels and restaurants", "Financial intermediation", "Business support")

ind_workerratio_01_n <- lapply(ind_workerratio_01_n, tidy)
ind_workerratio_01_s <- lapply(ind_workerratio_01_s, tidy)
ind_workerratio_01_n <- do.call(rbind, ind_workerratio_01_n) %>% filter(term == "log(tot_bmr)")
ind_workerratio_01_s <- do.call(rbind, ind_workerratio_01_s) %>% filter(term == "log(tot_bmr)")

ind_workerratio_01_n$group <- "North"
ind_workerratio_01_s$group <- "South"
ind_workerratio_01_n$sector <- rep(sectors, each = 1) 
ind_workerratio_01_s$sector <- rep(sectors, each = 1) 

ind_workerratio_01_ns <- bind_rows(ind_workerratio_01_n, ind_workerratio_01_s)

###########################################################
# BMR VS SEX RATIO OF WORKERS, BY SECTOR - PROVINCE LEVEL #
###########################################################

# Agriculture 

agri_indfe_n <- (list(
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn02, south == 0 & nganh_kd2 == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn03, south == 0 & nganh_kd2 == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn04, south == 0 & nganh_kd2 == 1),
        vcov = ~tinh+huyen), 
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn05, south == 0 & nganh_kd2 == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn06, south == 0 & nganh_kd2 == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn07, south == 0 & nganh_kd2 == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn08, south == 0 & nganh_kd2 == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn09, south == 0 & nganh_kd2 == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn10, south == 0 & nganh_kd2 == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn11, south == 0 & nganh_kd2 == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn12, south == 0 & nganh_kd2 == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn13, south == 0 & nganh_kd2 == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn14, south == 0 & nganh_kd2 == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn15, south == 0 & nganh_kd2 == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn16, south == 0 & nganh_kd2 == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn17, south == 0 & nganh_kd2 == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn18, south == 0 & nganh_kd2 == 1),
        vcov = ~tinh+huyen)))

agri_indfe_s <- (list(
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn02, south == 1 & nganh_kd2 == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn03, south == 1 & nganh_kd2 == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn04, south == 1 & nganh_kd2 == 1),
        vcov = ~tinh+huyen), 
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn05, south == 1 & nganh_kd2 == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn06, south == 1 & nganh_kd2 == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn07, south == 1 & nganh_kd2 == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn08, south == 1 & nganh_kd2 == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn09, south == 1 & nganh_kd2 == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn10, south == 1 & nganh_kd2 == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn11, south == 1 & nganh_kd2 == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn12, south == 1 & nganh_kd2 == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn13, south == 1 & nganh_kd2 == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn14, south == 1 & nganh_kd2 == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn15, south == 1 & nganh_kd2 == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn16, south == 1 & nganh_kd2 == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn17, south == 1 & nganh_kd2 == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn18, south == 1 & nganh_kd2 == 1),
        vcov = ~tinh+huyen)))

agri_indfe_n <- lapply(agri_indfe_n, tidy)
agri_indfe_s <- lapply(agri_indfe_s, tidy)
agri_indfe_n <- do.call(rbind, agri_indfe_n) %>% filter(term == "log(tot_bmr)")
agri_indfe_s <- do.call(rbind, agri_indfe_s) %>% filter(term == "log(tot_bmr)")
agri_indfe_n$year <- rep(seq(2002, 2018), each = nrow(agri_indfe_n) / length(seq(2002, 2018)))
agri_indfe_s$year <- rep(seq(2002, 2018), each = nrow(agri_indfe_s) / length(seq(2002, 2018)))

agri_indfe_n$group <- "North"
agri_indfe_s$group <- "South"
agri_indfe_ns <- rbind(agri_indfe_n, agri_indfe_s)

# Manufacturing sector 

manu_prov_indfe_n <- (list(
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn02, south == 0 & manu == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn03, south == 0 & manu == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn04, south == 0 & manu == 1),
        vcov = ~tinh+huyen), 
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn05, south == 0 & manu == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn06, south == 0 & manu == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn07, south == 0 & manu == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn08, south == 0 & manu == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn09, south == 0 & manu == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn10, south == 0 & manu == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn11, south == 0 & manu == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn12, south == 0 & manu == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn13, south == 0 & manu == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn14, south == 0 & manu == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn15, south == 0 & manu == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn16, south == 0 & manu == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn17, south == 0 & manu == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn18, south == 0 & manu == 1),
        vcov = ~tinh+huyen)))

manu_prov_indfe_s <- (list(
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn02, south == 1 & manu == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn03, south == 1 & manu == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn04, south == 1 & manu == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn05, south == 1 & manu == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn06, south == 1 & manu == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn07, south == 1 & manu == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn08, south == 1 & manu == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn09, south == 1 & manu == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn10, south == 1 & manu == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn11, south == 1 & manu == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn12, south == 1 & manu == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn13, south == 1 & manu == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn14, south == 1 & manu == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn15, south == 1 & manu == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn16, south == 1 & manu == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn17, south == 1 & manu == 1),
        vcov = ~tinh+huyen),
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn ,
        subset(dn18, south == 1 & manu == 1),
        vcov = ~tinh+huyen)))

manu_prov_indfe_n <- lapply(manu_prov_indfe_n, tidy)
manu_prov_indfe_s <- lapply(manu_prov_indfe_s, tidy)
manu_prov_indfe_n <- do.call(rbind, manu_prov_indfe_n) %>% filter(term == "log(tot_bmr_prov)")
manu_prov_indfe_s <- do.call(rbind, manu_prov_indfe_s) %>% filter(term == "log(tot_bmr_prov)")
manu_prov_indfe_n$year <- rep(seq(2002, 2018), each = nrow(manu_prov_indfe_n) / length(seq(2002, 2018)))
manu_prov_indfe_s$year <- rep(seq(2002, 2018), each = nrow(manu_prov_indfe_s) / length(seq(2002, 2018)))

manu_prov_indfe_n$group <- "North"
manu_prov_indfe_s$group <- "South"
manu_prov_indfe_ns <- rbind(manu_prov_indfe_n, manu_prov_indfe_s)