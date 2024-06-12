load("province_bmr_sum.Rda")
load("district_bmr_sum.Rda")

bmr_sum_fn <- function(i){
  i %>% 
    group_by(tinh) %>% 
    summarise(tot_bmr_prov = sum(tot_bmr_prov),
              tot_bmr_lb_prov = sum(tot_bmr_lb_prov),
              killed_tot_prov = sum(killed_tot_prov),
              dist_nearest_base_prov = min(dist_nearest_base_prov),
              dist_nearest_hochi_prov = min(dist_nearest_hochi_prov),
              ppn60 = sum(ppn60)) %>% 
    select(tinh, everything()) %>% 
    mutate(tot_bmr_prov_ppn = tot_bmr_prov/ppn60,
           tot_bmr_lb_prov_ppn = tot_bmr_lb_prov/ppn60,
           killed_tot_prov_ppn = killed_tot_prov/ppn60)    
}

#############
# IPUMS PHC #
#############

bombs_province89 <- province_bmr_sum %>%
  ungroup() %>% 
  mutate(tinh = recode(name_1,
                              'Hậu Giang' = 43,
                              'Hà Nội' = 1,
                              'Hải Phòng' = 3,
                              'Hải Dương' = 21,
                              'Hưng Yên' = 21,
                              'Nam Định' = 23,
                              'Hà Nam' = 23,
                              'Ninh Bình' = 23,
                              'Thái Bình' = 22,
                              'Hà Giang' = 11,
                              'Cao Bằng' = 10,
                              'Lào Cai' = 14,
                              'Bắc Kạn' = 15,
                              'Lạng Sơn' = 12,
                              'Tuyên Quang' = 11,
                              'Yên Bái' = 14,
                              'Thái Nguyên' = 15,
                              'Phú Thọ' = 17,
                              'Vĩnh Phúc' = 17,
                              'Bắc Giang' = 18,
                              'Bắc Ninh' = 18,
                              'Quảng Ninh' = 19,
                              'Lai Châu' = 13,
                              'Hoà Bình' = 20,
                              'Thanh Hóa' = 24,
                              'Nghệ An'= 25,
                              'Hà Tĩnh' = 25,
                              'Quảng Bình' = 26,
                              'Quảng Trị' = 49,
                              'Thừa Thiên Huế' = 50,
                              'Đà Nẵng' = 27,
                              'Quảng Nam' = 27,
                              'Quảng Ngãi' = 47,
                              'Bình Định' = 28,
                              'Phú Yên' = 48,
                              'Khánh Hòa' = 29,
                              'Kon Tum' = 31,
                              'Gia Lai' = 31,
                              'Đắk Lắk' = 32,
                              'Đắk Nông' = 32,
                              'Hồ Chí Minh' = 2,
                              'Lâm Đồng' = 33,
                              'Ninh Thuận' = 30,
                              'Bình Phước' = 34,
                              'Tây Ninh' = 35,
                              'Bình Dương' = 34,
                              'Đồng Nai' = 36,
                              'Bình Thuận' = 30,
                              'Bà Rịa - Vũng Tàu' = 46,
                              'Long An' = 37,
                              'Đồng Tháp' = 38,
                              'An Giang' = 39,
                              'Tiền Giang' = 40,
                              'Vĩnh Long' = 42,
                              'Bến Tre' = 41,
                              'Kiên Giang' = 44,
                              'Cần Thơ' = 43,
                              'Trà Vinh' = 42,
                              'Sóc Trăng' = 43,
                              'Bạc Liêu' = 45,
                              'Cà Mau' = 45,
                              .default = NA_real_)) %>% 
  bmr_sum_fn() %>% 
  rename(geo1_vn1989 = tinh)

bombs_province99 <- province_bmr_sum %>%
  ungroup() %>% 
  mutate(tinh = recode(name_1,
                              'Hậu Giang' = 301,
                              'Hà Nội' = 101,
                              'Hải Phòng' = 103,
                              'Hải Dương' = 107,
                              'Hưng Yên' = 109,
                              'Nam Định' = 113,
                              'Hà Nam' = 111,
                              'Ninh Bình' = 117,
                              'Thái Bình' = 115,
                              'Hà Giang' = 201,
                              'Cao Bằng' = 203,
                              'Lào Cai' = 205,
                              'Bắc Kạn' = 207,
                              'Lạng Sơn' = 209,
                              'Tuyên Quang' = 211,
                              'Yên Bái' = 213,
                              'Thái Nguyên' = 215,
                              'Phú Thọ' = 217,
                              'Vĩnh Phúc' = 219,
                              'Bắc Giang' = 221,
                              'Bắc Ninh' = 223,
                              'Quảng Ninh' = 225,
                              'Lai Châu' = 301,
                              'Hoà Bình' = 305,
                              'Thanh Hóa' = 401,
                              'Nghệ An'= 403,
                              'Hà Tĩnh' = 405,
                              'Quảng Bình' = 407,
                              'Quảng Trị' = 409,
                              'Thừa Thiên Huế' = 411,
                              'Đà Nẵng' = 501,
                              'Quảng Nam' = 507,
                              'Quảng Ngãi' = 503,
                              'Bình Định' = 505,
                              'Phú Yên' = 509,
                              'Khánh Hòa' = 511,
                              'Kon Tum' = 601,
                              'Gia Lai' = 603,
                              'Đắk Lắk' = 605,
                              'Đắk Nông' = 605,
                              'Hồ Chí Minh' = 701,
                              'Lâm Đồng' = 703,
                              'Ninh Thuận' = 705,
                              'Bình Phước' = 707,
                              'Tây Ninh' = 709,
                              'Bình Dương' = 711,
                              'Đồng Nai' = 713,
                              'Bình Thuận' = 715,
                              'Bà Rịa - Vũng Tàu' = 717,
                              'Long An' = 801,
                              'Đồng Tháp' = 803,
                              'An Giang' = 805,
                              'Tiền Giang' = 807,
                              'Vĩnh Long' = 809,
                              'Bến Tre' = 811,
                              'Kiên Giang' = 813,
                              'Cần Thơ' = 815,
                              'Trà Vinh' = 817,
                              'Sóc Trăng' = 819,
                              'Bạc Liêu' = 821,
                              'Cà Mau' = 823,
                              .default = NA_real_)) %>% 
  bmr_sum_fn() %>% 
  rename(geo1_vn1999 = tinh)

bombs_province09 <- province_bmr_sum %>%
  mutate(tinh = recode(name_1,
                              'Đắk Nông' = 67,
                              'Hậu Giang' = 93,
                              'Hà Nội' = 1,
                              'Hải Phòng' = 31,
                              'Hải Dương' = 30,
                              'Hưng Yên' = 33,
                              'Nam Định' = 36,
                              'Hà Nam' = 35,
                              'Ninh Bình' = 37,
                              'Thái Bình' = 34,
                              'Hà Giang' = 2,
                              'Cao Bằng' = 4,
                              'Lào Cai' = 10,
                              'Bắc Kạn' = 6,
                              'Lạng Sơn' = 20,
                              'Tuyên Quang' = 8,
                              'Yên Bái' = 15,
                              'Thái Nguyên' = 19,
                              'Phú Thọ' = 25,
                              'Vĩnh Phúc' = 26,
                              'Bắc Giang' = 24,
                              'Bắc Ninh' = 27,
                              'Quảng Ninh' = 22,
                              'Lai Châu' = 12,
                              'Hoà Bình' = 17,
                              'Thanh Hóa' = 38,
                              'Nghệ An'= 40,
                              'Hà Tĩnh' = 42,
                              'Quảng Bình' = 44,
                              'Quảng Trị' = 45,
                              'Thừa Thiên Huế' = 46,
                              'Đà Nẵng' = 48,
                              'Quảng Nam' = 49,
                              'Quảng Ngãi' = 51,
                              'Bình Định' = 52,
                              'Phú Yên' = 54,
                              'Khánh Hòa' = 56,
                              'Kon Tum' = 62,
                              'Gia Lai' = 64,
                              'Đắk Lắk' = 66,
                              'Hồ Chí Minh' = 79,
                              'Lâm Đồng' = 68,
                              'Ninh Thuận' = 58,
                              'Bình Phước' = 70,
                              'Tây Ninh' = 72,
                              'Bình Dương' = 74,
                              'Đồng Nai' = 75,
                              'Bình Thuận' = 60,
                              'Bà Rịa - Vũng Tàu' = 77,
                              'Long An' = 80,
                              'Đồng Tháp' = 87,
                              'An Giang' = 89,
                              'Tiền Giang' = 82,
                              'Vĩnh Long' = 86,
                              'Bến Tre' = 83,
                              'Kiên Giang' = 91,
                              'Cần Thơ' = 92,
                              'Trà Vinh' = 84,
                              'Sóc Trăng' = 94,
                              'Bạc Liêu' = 95,
                              'Cà Mau' = 96,
                              .default = NA_real_)) %>% 
  bmr_sum_fn() %>% 
  rename(geo1_vn2009 = tinh)

save(bombs_province89, file = "bombs_province89.Rda")
save(bombs_province99, file = "bombs_province99.Rda")
save(bombs_province09, file = "bombs_province09.Rda")

#################
# VHLSS AND VES #
#################

province_bmr_sum02 <- province_bmr_sum %>% 
  mutate(tinh = ifelse(tinh == 816, 815, tinh),
         tinh = ifelse(tinh == 606, 605, tinh),
         tinh = ifelse(tinh == 302, 301, tinh)) %>% 
  bmr_sum_fn()

province_bmr_sum2 <- province_bmr_sum %>% 
  mutate(tinh = recode(name_1,
                       'An Giang' = 89,
                       'Bà Rịa - Vũng Tàu' = 77,
                       'Bắc Giang' = 24,
                       'Bắc Kạn' = 6,
                       'Bạc Liêu' = 95,
                       'Bắc Ninh' = 27,
                       'Bến Tre' = 83,
                       'Bình Định' = 52,
                       'Bình Dương' = 74,
                       'Bình Phước' = 70,
                       'Bình Thuận' = 60,
                       'Cà Mau' = 96,
                       'Cần Thơ' = 92,
                       'Hậu Giang' = 93,
                       'Cao Bằng' = 4,
                       'Đà Nẵng' = 48,
                       'Đắk Lắk' = 66,
                       'Đắk Nông' = 67,
                       'Điện Biên' = 11,
                       'Lai Châu' = 12,
                       'Đồng Nai' = 75,
                       'Đồng Tháp' = 87,
                       'Gia Lai' = 64,
                       'Hà Giang' = 2,
                       'Hà Nam' = 35,
                       'Hà Nội' = 1,
                       'Hà Tĩnh' = 42,
                       'Hải Dương' = 30,
                       'Hải Phòng' = 31,
                       'Hậu Giang' = 93,
                       'Hồ Chí Minh' = 79,
                       'Hoà Bình' = 17,
                       'Hưng Yên' = 33,
                       'Khánh Hòa' = 56,
                       'Kiên Giang' = 91,
                       'Kon Tum' = 62,
                       'Lâm Đồng' = 68,
                       'Lạng Sơn' = 20,
                       'Lào Cai' = 10,
                       'Long An' = 80,
                       'Nam Định' = 36,
                       'Nghệ An' = 40,
                       'Ninh Bình' = 37,
                       'Ninh Thuận' = 58,
                       'Phú Thọ' = 25,
                       'Phú Yên' = 54,
                       'Quảng Bình' = 44,
                       'Quảng Nam' = 49,
                       'Quảng Ngãi' = 51,
                       'Quảng Ninh' = 22,
                       'Quảng Trị' = 45,
                       'Sóc Trăng' = 94,
                       'Sơn La' = 14,
                       'Tây Ninh' = 72,
                       'Thái Bình' = 34,
                       'Thái Nguyên' = 19,
                       'Thanh Hóa' = 38,
                       'Thừa Thiên Huế' = 46,
                       'Tiền Giang' = 82,
                       'Trà Vinh' = 84,
                       'Tuyên Quang' = 8,
                       'Vĩnh Long' = 86,
                       'Vĩnh Phúc' = 26,
                       'Yên Bái' = 15,
                       .default = NA_real_)) %>% 
  mutate(tinh = ifelse(tinh == 14, 12, tinh),
         tinh = ifelse(tinh == 11, 12, tinh)) %>% 
  bmr_sum_fn()

save(province_bmr_sum02, file = "province_bmr_sum02.Rda")
save(province_bmr_sum, file = "province_bmr_sum.Rda")
save(province_bmr_sum2, file = "province_bmr_sum2.Rda")

###########################################
# CREATING CONSISTENT DISTRICT BOUNDARIES #
###########################################

dropward <- function(i){
  
  i %>% 
    select(-starts_with("ward")) %>% 
    distinct() %>% 
    mutate(provname = str_replace(provname, "^(Tỉnh|Thành phố)\\s+", ""))
}

district_bmr_fn <- function(i){
  
  i %>% 
    group_by(tinh, huyen) %>% 
    mutate(tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr),
           tot_bmr_lb = ifelse(is.na(tot_bmr_lb), 0, tot_bmr_lb),
           killed_tot = ifelse(is.na(killed_tot), 0, killed_tot)) %>% 
    summarise(tot_bmr = sum(tot_bmr),
              tot_bmr_lb = sum(tot_bmr_lb),
              killed_tot = sum(killed_tot)) %>% 
    filter(!is.na(tinh)) 
}

geoid_list <- lapply(geoid_list, dropward)

mccaig_dist <- mccaig_boundaries %>% select(provname2018, distname2018, prov2002, dist2002, prov2003, dist2003, prov2004, dist2004,
                                            prov2005, dist2005, prov2006, dist2006, prov2007, dist2007, prov2008, dist2008,
                                            prov2009, dist2009, prov2010, dist2010, prov2011, dist2011, prov2012, dist2012,
                                            prov2013, dist2013, prov2014, dist2014, prov2015, dist2015, prov2016, dist2016, 
                                            prov2017, dist2017, prov2018, dist2018) %>%
  distinct() %>% 
  rename(prov18 = prov2018,
         dist18 = dist2018)

mccaig_dist_vhlss <- mccaig_boundaries %>%
  mutate(
  ward2002vhlss = as.character(ward2002vhlss),  
  ward2004vhlss = as.character(ward2004vhlss),
  ward2006vhlss = as.character(ward2006vhlss),
  ward2008vhlss = as.character(ward2008vhlss),
  tinh02_vhlss = as.numeric(substr(ward2002vhlss, 1, 3)),  
  huyen02_vhlss = as.numeric(substr(ward2002vhlss, 4, 5)),
  tinh04_vhlss = as.numeric(substr(ward2004vhlss, 1, 3)),  
  huyen04_vhlss = as.numeric(substr(ward2004vhlss, 4, 5)),
  tinh06_vhlss = as.numeric(substr(ward2006vhlss, 1, 3)),  
  huyen06_vhlss = as.numeric(substr(ward2006vhlss, 4, 5)),
  tinh08_vhlss = as.numeric(substr(ward2008vhlss, 1, 3)),  
  huyen08_vhlss = as.numeric(substr(ward2008vhlss, 4, 5))) %>% 
  select(prov2018, dist2018, tinh02_vhlss, huyen02_vhlss, tinh04_vhlss, huyen04_vhlss,
         tinh06_vhlss, huyen06_vhlss, tinh08_vhlss, huyen08_vhlss) %>% 
  na.omit() %>% 
  distinct() %>% 
  rename(prov18 = prov2018,
         dist18 = dist2018)

mccaig_dist_phcs <- mccaig_boundaries %>%
  select(prov2018, dist2018, prov2009, ward2009census) %>% 
  distinct() %>% 
  rename(prov18 = prov2018,
         dist18 = dist2018)

district_bmr_sum <- list(geoid_list[[17]], district_bmr_sum) %>%
  reduce(full_join, by = c("provname", "distname")) %>% 
  distinct() %>% 
  select(prov18, dist18, everything()) %>% 
  select(-c("varname_1", "varname_2")) %>% 
  mutate(
    prov18 = if_else(provname == "Hồ Chí Minh" & distname == "Quận 2", 79, prov18),
    dist18 = if_else(provname == "Hồ Chí Minh" & distname == "Quận 2", 769, dist18),
    prov18 = if_else(provname == "Đồng Tháp" & distname == "Thị xã Hồng Ngự", 87, prov18),
    dist18 = if_else(provname == "Đồng Tháp" & distname == "Thị xã Hồng Ngự", 868, dist18),
    prov18 = if_else(provname == "Đắk Nông" & distname == "Thị xã Gia Nghĩa", 67, prov18),
    dist18 = if_else(provname == "Đắk Nông" & distname == "Thị xã Gia Nghĩa", 660, dist18),
    prov18 = if_else(provname == "Bà Rịa - Vũng Tàu" & distname == "Huyện Tân Thành", 77, prov18),
    dist18 = if_else(provname == "Bà Rịa - Vũng Tàu" & distname == "Huyện Tân Thành", 745, dist18),
    prov18 = if_else(provname == "Thanh Hóa" & distname == "Huyện Tĩnh Gia", 38, prov18),
    dist18 = if_else(provname == "Thanh Hóa" & distname == "Huyện Tĩnh Gia", 407, dist18),
    tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr),
    tot_bmr_lb = ifelse(is.na(tot_bmr_lb), 0, tot_bmr_lb)
  ) %>% 
  filter(!is.na(prov18))

district_bmr_sum02 <- district_bmr_sum %>% 
  full_join(mccaig_dist, by = c("prov18", "dist18")) %>% 
  rename(tinh = prov2002,
         huyen = dist2002) %>% 
  district_bmr_fn()

district_bmr_sum02_vhlss <- district_bmr_sum %>% 
  full_join(mccaig_dist_vhlss, by = c("prov18", "dist18")) %>% 
  rename(tinh = tinh02_vhlss,
         huyen = huyen02_vhlss) %>% 
  district_bmr_fn()

district_bmr_sum03 <- district_bmr_sum %>% 
  full_join(mccaig_dist, by = c("prov18", "dist18")) %>% 
  rename(tinh = prov2003,
         huyen = dist2003) %>% 
  district_bmr_fn()

district_bmr_sum04 <- district_bmr_sum %>% 
  full_join(mccaig_dist, by = c("prov18", "dist18")) %>% 
  rename(tinh = prov2004,
         huyen = dist2004) %>% 
  district_bmr_fn()

district_bmr_sum04_vhlss <- district_bmr_sum %>% 
  full_join(mccaig_dist_vhlss, by = c("prov18", "dist18")) %>% 
  rename(tinh = tinh04_vhlss,
         huyen = huyen04_vhlss) %>% 
  district_bmr_fn()

district_bmr_sum05 <- district_bmr_sum %>% 
  full_join(mccaig_dist, by = c("prov18", "dist18")) %>% 
  rename(tinh = prov2005,
         huyen = dist2005) %>% 
  district_bmr_fn()

district_bmr_sum06 <- district_bmr_sum %>% 
  full_join(mccaig_dist, by = c("prov18", "dist18")) %>% 
  rename(tinh = prov2006,
         huyen = dist2006) %>% 
  district_bmr_fn()

district_bmr_sum06_vhlss <- district_bmr_sum %>% 
  full_join(mccaig_dist_vhlss, by = c("prov18", "dist18")) %>% 
  rename(tinh = tinh06_vhlss,
         huyen = huyen06_vhlss) %>% 
  district_bmr_fn()

district_bmr_sum07 <- district_bmr_sum %>% 
  full_join(mccaig_dist, by = c("prov18", "dist18")) %>% 
  rename(tinh = prov2007,
         huyen = dist2007) %>% 
  district_bmr_fn()

district_bmr_sum08 <- district_bmr_sum %>% 
  full_join(mccaig_dist, by = c("prov18", "dist18")) %>% 
  rename(tinh = prov2008,
         huyen = dist2008) %>% 
  district_bmr_fn()

district_bmr_sum08_vhlss <- district_bmr_sum %>% 
  full_join(mccaig_dist_vhlss, by = c("prov18", "dist18")) %>% 
  rename(tinh = tinh08_vhlss,
         huyen = huyen08_vhlss) %>% 
  district_bmr_fn()

district_bmr_sum09 <- district_bmr_sum %>% 
  full_join(mccaig_dist, by = c("prov18", "dist18")) %>% 
  rename(tinh = prov2009,
         huyen = dist2009) %>% 
  district_bmr_fn()

district_bmr_sum10 <- district_bmr_sum %>% 
  full_join(mccaig_dist, by = c("prov18", "dist18")) %>% 
  rename(tinh = prov2010,
         huyen = dist2010) %>% 
  district_bmr_fn()

district_bmr_sum11 <- district_bmr_sum %>% 
  full_join(mccaig_dist, by = c("prov18", "dist18")) %>% 
  rename(tinh = prov2011,
         huyen = dist2011) %>% 
  district_bmr_fn()

district_bmr_sum12 <- district_bmr_sum %>% 
  full_join(mccaig_dist, by = c("prov18", "dist18")) %>% 
  rename(tinh = prov2012,
         huyen = dist2012) %>% 
  district_bmr_fn

district_bmr_sum13 <- district_bmr_sum %>% 
  full_join(mccaig_dist, by = c("prov18", "dist18")) %>% 
  rename(tinh = prov2013,
         huyen = dist2013) %>% 
  district_bmr_fn()

district_bmr_sum14 <- district_bmr_sum %>% 
  full_join(mccaig_dist, by = c("prov18", "dist18")) %>% 
  rename(tinh = prov2014,
         huyen = dist2014) %>% 
  district_bmr_fn()

district_bmr_sum15 <- district_bmr_sum %>% 
  full_join(mccaig_dist, by = c("prov18", "dist18")) %>% 
  rename(tinh = prov2015,
         huyen = dist2015) %>% 
  district_bmr_fn()

district_bmr_sum16 <- district_bmr_sum %>% 
  full_join(mccaig_dist, by = c("prov18", "dist18")) %>% 
  rename(tinh = prov2016,
         huyen = dist2016) %>% 
  district_bmr_fn()

district_bmr_sum17 <- district_bmr_sum %>% 
  full_join(mccaig_dist, by = c("prov18", "dist18")) %>% 
  rename(tinh = prov2017,
         huyen = dist2017) %>% 
  district_bmr_fn()

# PHC 

district_bmr_phc <- district_bmr_sum %>% 
  mutate(distname = str_replace(distname, "^(Huyện|Quận|Thị xã|Thị Xã|Thành phố|Thành Phố)\\s+", ""),
         distname = case_when(
           distname == '1' ~ 'Quận 1',
           distname == '10' ~ 'Quận 10',
           distname == '12' ~ 'Quận 12',
           distname == '11' ~ 'Quận 11',
           distname == '2' ~ 'Quận 2',
           distname == '3' ~ 'Quận 3',
           distname == '4' ~ 'Quận 4',
           distname == '5' ~ 'Quận 5',
           distname == '6' ~ 'Quận 6',
           distname == '7' ~ 'Quận 7',
           distname == '8' ~ 'Quận 8',
           distname == '9' ~ 'Quận 9',
           TRUE ~ distname))

district_bmr_sum_phc <- phc_dist %>%
  mutate(
    geo2_vn2009 = as.double(str_trim(str_extract(distname, "^\\d+"))),
    distname = str_trim(str_remove(distname, "^\\d+\\s*"))
  ) %>% 
  mutate(
    provname = case_when(
    provname == " Bình Định" ~ "Bình Định",
    provname == " Đắk Nông" ~ "Đắk Nông",
    provname == " Thừa Thiên Huế" ~ "Thừa Thiên Huế",
    provname == " Vĩnh Phúc" ~ "Vĩnh Phúc",
    provname == "Khánh Hoà" ~ "Khánh Hòa",
    provname == "Thanh Hoá" ~ "Thanh Hóa",
    TRUE ~ provname),
    distname = case_when(
      distname == "Mường ảng" ~ "Mường Ảng",
      distname == "Đồng Phù" & provname == "Bình Phước" ~ "Đồng Phú",
      distname == "Hoà Vang" & provname == "Đà Nẵng" ~ "Hòa Vang",
      distname == "Chư Pưh" & provname == "Gia Lai" ~ "Chư Păh",
      distname == "ứng Hòa" & provname == "Hà Nội" ~ "Ứng Hòa",
      distname == "ứng Hòa" & provname == "Hà Nội" ~ "Ứng Hòa",
      distname == "Hữũ Lung" & provname == "Lạng Sơn" ~ "Hữu Lũng",
      distname == "ý Yên" & provname == "Nam Định" ~ "Ý Yên",
      distname == "Bác ái" & provname == "Ninh Thuận" ~ "Bác Ái",
      distname == "Đông Hoà" & provname == "Phú Yên" ~ "Đông Hòa",
      TRUE ~ distname)) %>% 
  full_join(district_bmr_phc, by = c("distname", "provname"))

save(district_bmr_sum, file = "district_bmr_sum.Rda")