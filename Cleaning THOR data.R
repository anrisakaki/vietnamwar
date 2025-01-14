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

province_centroids <- st_centroid(province_bmr_sum_sf)
province_centroids <- st_transform(province_centroids, crs = st_crs(hcmtrail))
nearest_trail <- st_nearest_feature(province_centroids, hcmtrail)

province_bmr_sum_sf$dist_nearest_hochi <- as.numeric(
  st_distance(province_centroids, hcmtrail[nearest_trail, ], by_element = TRUE)
) / 1000

province_bmr_sum <- province_bmr_sum_sf %>% 
  ungroup() %>% 
  select(-varname_1) %>%
  sf::st_drop_geometry() %>% 
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
                       'Cao Bằng' = 4,
                       'Đà Nẵng' = 48,
                       'Đắk Lắk' = 66,
                       'Đắk Nông' = 67,
                       'Điện Biên' = 11,
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
                       'Lai Châu' = 12,
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
  group_by(tinh, name_1) %>% 
  summarise(tot_bmr_prov = sum(tot_bmr),
            tot_bmr_lb_prov = sum(tot_bmr_lb),
            killed_tot_prov = sum(killed_tot),
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

district_centroids <- st_centroid(district_bmr_sum_phc_sf)
district_centroids <- st_transform(district_centroids, crs = st_crs(hcmtrail))
nearest_trail <- st_nearest_feature(district_centroids, hcmtrail)

district_bmr_phc$dist_nearest_hochi_dist <- as.numeric(
  st_distance(district_centroids, hcmtrail[nearest_trail, ], by_element = TRUE)
) / 1000

district_bmr_phc <- district_bmr_phc %>% 
  rename(geo2_vn = geolevel2)

save(district_bmr_phc, file = "district_bmr_phc.Rda")

######################
# SUMMARY OF TARGETS #
######################

targets_sum <- district_bmr_phc_sf %>% 
  sf::st_drop_geometry() %>% 
  rename_all(tolower) %>%
  summarise(agri_s = sum(numweaponsdelivered[tgtcountry == "SOUTH VIETNAM" & agriculture == 1], na.rm = T),
            agri_n = sum(numweaponsdelivered[tgtcountry == "NORTH VIETNAM" & agriculture == 1], na.rm = T),
            infra_s = sum(numweaponsdelivered[tgtcountry == "SOUTH VIETNAM" & infrastructure == 1], na.rm = T),
            infra_n = sum(numweaponsdelivered[tgtcountry == "NORTH VIETNAM" & infrastructure == 1], na.rm = T),
            civilian_s = sum(numweaponsdelivered[tgtcountry == "SOUTH VIETNAM" & civilian == 1], na.rm = T),
            civilian_n = sum(numweaponsdelivered[tgtcountry == "NORTH VIETNAM" & civilian == 1], na.rm = T),
            industry_s = sum(numweaponsdelivered[tgtcountry == "SOUTH VIETNAM" & industry == 1], na.rm = T),
            industry_n = sum(numweaponsdelivered[tgtcountry == "NORTH VIETNAM" & industry == 1], na.rm = T))
