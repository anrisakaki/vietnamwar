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
        vcov = ~tinh+huyen)
)

sectors <- c("Agriculture", "Food & Beverages", "Textiles", "Apparel", "Leather", 
             "Construction", "Wholesale trade", "Retail trade", "Hotels and restaurants")

ind_workerratio_01_n <- lapply(ind_workerratio_01_n, tidy)
ind_workerratio_01_s <- lapply(ind_workerratio_01_s, tidy)
ind_workerratio_01_n <- do.call(rbind, ind_workerratio_01_n) %>% filter(term == "log(tot_bmr)")
ind_workerratio_01_s <- do.call(rbind, ind_workerratio_01_s) %>% filter(term == "log(tot_bmr)")

ind_workerratio_01_n$group <- "North"
ind_workerratio_01_s$group <- "South"
ind_workerratio_01_n$sector <- rep(sectors, each = 1) 
ind_workerratio_01_s$sector <- rep(sectors, each = 1) 

ind_workerratio_01_ns <- bind_rows(ind_workerratio_01_n, ind_workerratio_01_s)

ind_workerratio_06_n <- list(
  # Agriculture
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn06, south == 0 & nganh_kd2 == 1),
        vcov = ~tinh+huyen),
  # F&B 
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn06, south == 0 & nganh_kd2 == 10 | south == 0 & nganh_kd2 == 11),
        vcov = ~tinh+huyen),
  # Textiles
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn06, south == 0 & nganh_kd2 == 13),
        vcov = ~tinh+huyen),
  # Apparel
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn06, south == 0 & nganh_kd2 == 14),
        vcov = ~tinh+huyen),
  # Leather
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn06, south == 0 & nganh_kd2 == 15),
        vcov = ~tinh+huyen),
  # Construction 
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn06, south == 0 & nganh_kd2 == 41 | south == 0 & nganh_kd2 == 42 | south == 0 & nganh_kd2 == 43),
        vcov = ~tinh+huyen),
  # Wholesale trade                               
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn06, south == 0 & nganh_kd2 == 46),
        vcov = ~tinh+huyen),
  # Retail trade
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn06, south == 0 & nganh_kd2 == 47),
        vcov = ~tinh+huyen),
  # Hotels and restaurants 
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn06, south == 0 & nganh_kd2 == 55 | south == 0 & nganh_kd2 == 56),
        vcov = ~tinh+huyen)
)

ind_workerratio_06_s <- list(
  # Agriculture
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn06, south == 1 & nganh_kd2 == 1),
        vcov = ~tinh+huyen),
  # F&B 
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn06, south == 1 & nganh_kd2 == 10 | south == 0 & nganh_kd2 == 11),
        vcov = ~tinh+huyen),
  # Textiles
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn06, south == 1 & nganh_kd2 == 13),
        vcov = ~tinh+huyen),
  # Apparel
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn06, south == 1 & nganh_kd2 == 14),
        vcov = ~tinh+huyen),
  # Leather
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn06, south == 1 & nganh_kd2 == 15),
        vcov = ~tinh+huyen),
  # Construction 
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn06, south == 1 & nganh_kd2 == 41 | south == 0 & nganh_kd2 == 42 | south == 0 & nganh_kd2 == 43),
        vcov = ~tinh+huyen),
  # Wholesale trade                               
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn06, south == 1 & nganh_kd2 == 46),
        vcov = ~tinh+huyen),
  # Retail trade
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn06, south == 1 & nganh_kd2 == 47),
        vcov = ~tinh+huyen),
  # Hotels and restaurants 
  feols(tot_workerratio ~ log(tot_bmr) | nganh_kd + lhdn + tinh,
        subset(dn06, south == 1 & nganh_kd2 == 55 | south == 0 & nganh_kd2 == 56),
        vcov = ~tinh+huyen)
)

ind_workerratio_06_n <- lapply(ind_workerratio_06_n, tidy)
ind_workerratio_06_s <- lapply(ind_workerratio_06_s, tidy)
ind_workerratio_06_n <- do.call(rbind, ind_workerratio_06_n) %>% filter(term == "log(tot_bmr)")
ind_workerratio_06_s <- do.call(rbind, ind_workerratio_06_s) %>% filter(term == "log(tot_bmr)")

ind_workerratio_06_n$group <- "North"
ind_workerratio_06_s$group <- "South"
ind_workerratio_06_n$sector <- rep(sectors, each = 1) 
ind_workerratio_06_s$sector <- rep(sectors, each = 1) 

ind_workerratio_06_ns <- bind_rows(ind_workerratio_06_n, ind_workerratio_06_s)

###########################################################
# BMR VS SEX RATIO OF WORKERS, BY SECTOR - PROVINCE LEVEL #
###########################################################

ind_workerratio_prov_06_n <- list(
  # Agriculture
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn06, south == 0 & nganh_kd2 == 1),
        vcov = ~tinh+huyen),
  # F&B 
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn06, south == 0 & nganh_kd2 == 10 | south == 0 & nganh_kd2 == 11),
        vcov = ~tinh+huyen),
  # Textiles
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn06, south == 0 & nganh_kd2 == 13),
        vcov = ~tinh+huyen),
  # Apparel
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn06, south == 0 & nganh_kd2 == 14),
        vcov = ~tinh+huyen),
  # Leather
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn06, south == 0 & nganh_kd2 == 15),
        vcov = ~tinh+huyen),
  # Construction 
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn06, south == 0 & nganh_kd2 == 41 | south == 0 & nganh_kd2 == 42 | south == 0 & nganh_kd2 == 43),
        vcov = ~tinh+huyen),
  # Wholesale trade                               
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn06, south == 0 & nganh_kd2 == 46),
        vcov = ~tinh+huyen),
  # Retail trade
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn06, south == 0 & nganh_kd2 == 47),
        vcov = ~tinh+huyen),
  # Hotels and restaurants 
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn06, south == 0 & nganh_kd2 == 55 | south == 0 & nganh_kd2 == 56),
        vcov = ~tinh+huyen)
)

ind_workerratio_prov_06_s <- list(
  # Agriculture
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn06, south == 1 & nganh_kd2 == 1),
        vcov = ~tinh+huyen),
  # F&B 
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn06, south == 1 & nganh_kd2 == 10 | south == 0 & nganh_kd2 == 11),
        vcov = ~tinh+huyen),
  # Textiles
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn06, south == 1 & nganh_kd2 == 13),
        vcov = ~tinh+huyen),
  # Apparel
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn06, south == 1 & nganh_kd2 == 14),
        vcov = ~tinh+huyen),
  # Leather
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn06, south == 1 & nganh_kd2 == 15),
        vcov = ~tinh+huyen),
  # Construction 
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn06, south == 1 & nganh_kd2 == 41 | south == 0 & nganh_kd2 == 42 | south == 0 & nganh_kd2 == 43),
        vcov = ~tinh+huyen),
  # Wholesale trade                               
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn06, south == 1 & nganh_kd2 == 46),
        vcov = ~tinh+huyen),
  # Retail trade
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn06, south == 1 & nganh_kd2 == 47),
        vcov = ~tinh+huyen),
  # Hotels and restaurants 
  feols(tot_workerratio ~ log(tot_bmr_prov) | nganh_kd + lhdn,
        subset(dn06, south == 1 & nganh_kd2 == 55 | south == 0 & nganh_kd2 == 56),
        vcov = ~tinh+huyen)
)

ind_workerratio_prov_06_n <- lapply(ind_workerratio_prov_06_n, tidy)
ind_workerratio_prov_06_s <- lapply(ind_workerratio_prov_06_s, tidy)
ind_workerratio_prov_06_n <- do.call(rbind, ind_workerratio_prov_06_n) %>% filter(term == "log(tot_bmr_prov)")
ind_workerratio_prov_06_s <- do.call(rbind, ind_workerratio_prov_06_s) %>% filter(term == "log(tot_bmr_prov)")

ind_workerratio_prov_06_n$group <- "North"
ind_workerratio_prov_06_s$group <- "South"
ind_workerratio_prov_06_n$sector <- rep(sectors, each = 1) 
ind_workerratio_prov_06_s$sector <- rep(sectors, each = 1) 

ind_workerratio_prov_06_ns <- bind_rows(ind_workerratio_prov_06_n, ind_workerratio_prov_06_s)
