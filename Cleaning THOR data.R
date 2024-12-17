load("prov_casualties.Rda")
load("dist_casualties.Rda")

######################
# CLEANING THOR DATA #
######################

thor <- thor %>% 
  filter(!(SOURCERECORD == "SACCOACT" & MSNDATE >= 19710301)) %>%
  mutate(MSNDATE = ifelse(MSNDATE == "1970-02-29", "1970-03-01", MSNDATE),
         MFUNC_DESC_CLASS = ifelse(MFUNC_DESC == "COMBT CARGO AIR  DROP", "KINETIC", MFUNC_DESC_CLASS)) %>%
  mutate(MSNDATE = as.Date(MSNDATE),
         year = as.integer(format(MSNDATE, "%Y")),
         month = as.integer(format(MSNDATE, "%m")),
         day = as.integer(format(MSNDATE, "%d")),
         TGTTYPE = ifelse(TGTTYPE == "", NA, TGTTYPE),
         MFUNC_DESC = ifelse(MFUNC_DESC == "", NA, MFUNC_DESC),
         MFUNC_DESC_CLASS = ifelse(MFUNC_DESC == "COMBT CARGO AIR  DROP", "KINETIC", MFUNC_DESC_CLASS),
         MFUNC_DESC = ifelse(MFUNC_DESC == "COMBT CARGO AIR  DROP", "HEAVY BOMBARD", MFUNC_DESC),
         WEAPONSLOADEDWEIGHT = ifelse(WEAPONSLOADEDWEIGHT == 0, NA, WEAPONSLOADEDWEIGHT),
         WEAPONSLOADEDWEIGHT = ifelse(WEAPONSLOADEDWEIGHT == "-1", NA, WEAPONSLOADEDWEIGHT)) %>% 
  filter(TGTCOUNTRY == "NORTH VIETNAM" | TGTCOUNTRY == "SOUTH VIETNAM") %>% 
  filter(!is.na(TGTLATDD_DDD_WGS84),
         !is.na(TGTLONDDD_DDD_WGS84))

weapons_dict <- weapons_dict %>% select(-c(WEAPON_COUNT))

thor <- left_join(thor, weapons_dict, by = "WEAPONTYPE")

thor <- thor %>%
  filter(WEAPON_CLASS != "SUPPORT",
         WEAPON_CLASS != "GUN",
         MFUNC_DESC_CLASS == "KINETIC") %>% 
  mutate(
    civilian = case_when(
      TGTTYPE %in% c("CIV POPULATN CENTR", "VILLAGE", "HUTS", "ISLAND", "BUILDINGS") ~ 1,
      grepl("PERSONNEL", TGTTYPE, ignore.case = TRUE) ~ 1,
      TRUE ~ 0 
    ),    
    infrastructure = case_when(
      TGTTYPE %in% c("BRIDGE", "FERRY CROSSING", "PIER", "PORT FACILITY", "FACTORY INDUSTRIAL", "FACTORY,ANY",
                     "ELEC. PWR. FAC.", "PORT FACILITY", "SHIPYARD", "PUMPING STATION", "	THERMAL POWER PLANT") ~ 1,
      grepl("RAILROAD|BRIDGE|ROAD|REFINERY|TOWER", TGTTYPE, ignore.case = TRUE) ~ 1,
      TRUE ~ 0
  ),
  agriculture = case_when(
    TGTTYPE %in% c("AGRICULTURAL AREA") ~ 1,
    TRUE ~ 0 
  ),
  industry = case_when(
    TGTTYPE %in% c("CONSTRUCTION SITE", "FACTORY INDUSTRIAL", "FACTORY,ANY", "ELEC. PWR. FAC.") ~ 1,
    TRUE ~ 0
  ),
  WEIGHTDELIVERED = ifelse(is.na(WEAPONSLOADEDWEIGHT), NUMWEAPONSDELIVERED*WEAPONWEIGHT, WEAPONSLOADEDWEIGHT/10)) %>% 
  # Max ordnance weight of B52 was 70,000 lb 
  filter(WEIGHTDELIVERED <= 70000)

# Convert to a GeoDataFrame
gdf <- st_as_sf(thor, coords = c("TGTLONDDD_DDD_WGS84", "TGTLATDD_DDD_WGS84"), crs = 4326)

# Extract lat and lon separately
gdf$tgtlonddd_ddd_wgs84 <- st_coordinates(gdf)[, "X"]
gdf$tgtlatdd_ddd_wgs84 <- st_coordinates(gdf)[, "Y"]

gdf <- st_make_valid(gdf) 

####################################
# PROVINCE-LEVEL BOMBING INTENSITY #
####################################

province_bmr <- st_join(gdf, vnmap1) %>% rename_all(tolower)

province_bmr_sum_sf <- province_bmr %>% 
  ungroup() %>% 
  filter(!is.na(varname_1)) %>% 
  group_by(varname_1, name_1) %>% 
  summarise(tot_bmr = sum(numweaponsdelivered),
            tot_bmr_lb = sum(weightdelivered),
            tot_agri = sum(numweaponsdelivered[agriculture == 1]),
            tot_agri_lb = sum(weightdelivered[agriculture == 1]),
            tot_industry = sum(numweaponsdelivered[industry == 1]),
            tot_industry_lb = sum(weightdelivered[industry == 1])) %>% 
  ungroup() %>% 
  left_join(prov_casualties, by = "varname_1")

bases <- st_transform(bases, crs = 4326)
hcmtrail <- st_transform(hcmtrail, crs = 4326)

nearest_base <- st_nearest_feature(province_bmr_sum_sf, bases)
province_bmr_sum_sf$dist_nearest_base <-
  as.numeric(st_distance(province_bmr_sum_sf, bases[nearest_base, ], by_element = TRUE)) / 1000

nearest_trail <- st_nearest_feature(province_bmr_sum_sf, hcmtrail)
province_bmr_sum_sf$dist_nearest_hochi <-
  as.numeric(st_distance(province_bmr_sum_sf, hcmtrail[nearest_trail, ], by_element = TRUE)) / 1000

distance_info <- province_bmr_sum_sf %>% 
  select(varname_1, dist_nearest_base, dist_nearest_hochi)
distance_info <- sf::st_drop_geometry(distance_info)

province_bmr_sum <- province_bmr_sum_sf %>% 
  ungroup() %>% 
  select(-varname_1) %>%
  sf::st_drop_geometry() %>% 
  mutate(name_1 = recode(name_1,
                         'Điện Biên' = 'Lai Châu',
                         'Sơn La' = 'Lai Châu',
                         .default = name_1),
         tinh = recode(name_1,
                       'An Giang' = 805,
                       'Bà Rịa - Vũng Tàu' = 717,
                       'Bắc Giang' = 221,
                       'Bắc Kạn' = 207,
                       'Bạc Liêu' = 821,
                       'Bắc Ninh' = 106,
                       'Bến Tre' = 811,
                       'Bình Định' = 507,
                       'Bình Dương' = 711,
                       'Bình Phước' = 707,
                       'Bình Thuận' = 715,
                       'Cà Mau' = 823,
                       'Cần Thơ' = 815,
                       'Cao Bằng' = 203,
                       'Đà Nẵng' = 501,
                       'Đắk Lắk' = 605,
                       'Đắk Nông' = 606,
                       'Lai Châu' = 301,
                       'Đồng Nai' = 713,
                       'Đồng Tháp' = 803,
                       'Gia Lai' = 603,
                       'Hà Giang' = 201,
                       'Hà Nam' = 111,
                       'Hà Nội' = 101,
                       'Hà Tĩnh' = 405,
                       'Hải Dương' = 107,
                       'Hải Phòng' = 103,
                       'Hậu Giang' = 816,
                       'Hồ Chí Minh' = 701,
                       'Hoà Bình' = 305,
                       'Hưng Yên' = 109,
                       'Khánh Hòa' = 511,
                       'Kiên Giang' = 813,
                       'Kon Tum' = 601,
                       'Lâm Đồng' = 607,
                       'Lạng Sơn' = 209,
                       'Lào Cai' = 205,
                       'Long An' = 801,
                       'Nam Định' = 113,
                       'Nghệ An' = 403,
                       'Ninh Bình' = 117,
                       'Ninh Thuận' = 705,
                       'Phú Thọ' = 217,
                       'Phú Yên' = 509,
                       'Quảng Bình' = 407,
                       'Quảng Nam' = 503,
                       'Quảng Ngãi' = 505,
                       'Quảng Ninh' = 225,
                       'Quảng Trị' = 409,
                       'Sóc Trăng' = 819,
                       'Tây Ninh' = 709,
                       'Thái Bình' = 115,
                       'Thái Nguyên' = 215,
                       'Thanh Hóa' = 401,
                       'Thừa Thiên Huế' = 411,
                       'Tiền Giang' = 807,
                       'Trà Vinh' = 817,
                       'Tuyên Quang' = 211,
                       'Vĩnh Long' = 809,
                       'Vĩnh Phúc' = 104,
                       'Yên Bái' = 213,
                       .default = NA_real_)) %>% 
  group_by(tinh, name_1) %>% 
  summarise(tot_bmr_prov = sum(tot_bmr),
            tot_bmr_lb_prov = sum(tot_bmr_lb),
            killed_tot_prov = sum(killed_tot),
            dist_nearest_base_prov = min(dist_nearest_base),
            dist_nearest_hochi_prov = min(dist_nearest_hochi)) %>% 
  select(tinh, everything())

save(province_bmr_sum, file = "province_bmr_sum.Rda")

#################################
# Bombing intensity by district # 
#################################

geo2_vn <- st_make_valid(geo2_vn)
district_bmr_phc_sf <- st_join(geo2_vn, gdf)

district_bmr_phc <- district_bmr_phc_sf %>% 
  sf::st_drop_geometry() %>% 
  group_by(GEOLEVEL2) %>% 
  summarise(
    tot_bmr = sum(NUMWEAPONSDELIVERED, na.rm = T),
    tot_bmr_lb = sum(WEIGHTDELIVERED, na.rm = T)) %>% 
  sf::st_drop_geometry() %>% 
  mutate(mean_tot_bmr = mean(tot_bmr, na.rm = T),
         sd_tot_bmr = sd(tot_bmr, na.rm = T),
         tot_bmr_std = (tot_bmr - mean_tot_bmr)/sd_tot_bmr,
         GEOLEVEL2 = as.double(GEOLEVEL2)) %>% 
  filter(!is.na(GEOLEVEL2)) %>% 
  rename_all(tolower)

district_bmr_sum_phc_sf <- district_bmr_phc %>% 
  mutate(geolevel2 = as.character(geolevel2)) %>% 
  left_join(geo2_vn, by = c("geolevel2" = "GEOLEVEL2")) %>% 
  sf::st_as_sf()

######################
# SUMMARY OF TARGETS #
######################

targets_sum <- district_bmr %>% 
  sf::st_drop_geometry() %>% rename_all(tolower) %>%
  summarise(agri_s = sum(numweaponsdelivered[tgtcountry == "SOUTH VIETNAM" & agriculture == 1], na.rm = T),
            agri_n = sum(numweaponsdelivered[tgtcountry == "NORTH VIETNAM" & agriculture == 1], na.rm = T),
            infra_s = sum(numweaponsdelivered[tgtcountry == "SOUTH VIETNAM" & infrastructure == 1], na.rm = T),
            infra_n = sum(numweaponsdelivered[tgtcountry == "NORTH VIETNAM" & infrastructure == 1], na.rm = T),
            civilian_s = sum(numweaponsdelivered[tgtcountry == "SOUTH VIETNAM" & civilian == 1], na.rm = T),
            civilian_n = sum(numweaponsdelivered[tgtcountry == "NORTH VIETNAM" & civilian == 1], na.rm = T),
            industry_s = sum(numweaponsdelivered[tgtcountry == "SOUTH VIETNAM" & industry == 1], na.rm = T),
            industry_n = sum(numweaponsdelivered[tgtcountry == "NORTH VIETNAM" & industry == 1], na.rm = T))

south_missiontype <- as.data.frame(table(district_bmr$MFUNC_DESC[district_bmr$TGTCOUNTRY == "SOUTH VIETNAM"]))
south_missiontype$Share <- prop.table(south_missiontype$Freq)

north_missiontype <- as.data.frame(table(district_bmr$MFUNC_DESC[district_bmr$TGTCOUNTRY == "NORTH VIETNAM"]))
north_missiontype$Share <- prop.table(north_missiontype$Freq)

save(district_bmr_sum, file = "district_bmr_sum.Rda")
save(district_bmr_phc, file = "district_bmr_phc.Rda")
