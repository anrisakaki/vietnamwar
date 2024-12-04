load("district_bmr_sum.Rda")

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
    ungroup() %>%
    mutate(mean_tot_bmr = mean(tot_bmr, na.rm = T),
           sd_tot_bmr = sd(tot_bmr, na.rm = T),
           tot_bmr_std = (tot_bmr - mean_tot_bmr)/sd_tot_bmr) %>% 
    select(-c(mean_tot_bmr, sd_tot_bmr)) %>% 
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
           TRUE ~ distname),
         mean_tot_bmr = mean(tot_bmr, na.rm = T),
         sd_tot_bmr = sd(tot_bmr, na.rm = T),
         tot_bmr_std = (tot_bmr - mean_tot_bmr)/sd_tot_bmr) %>% 
  select(-c(mean_tot_bmr, sd_tot_bmr))

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
  full_join(district_bmr_phc, by = c("distname", "provname")) %>% 
  distinct()

save(district_bmr_sum, file = "district_bmr_sum.Rda")