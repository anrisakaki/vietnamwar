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
      TGTTYPE %in% c("AGRICULTURAL AREA", "CIV POPULATN CENTR", "VILLAGE", "HUTS", "ISLAND", "BUILDINGS") ~ 1,
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

south <- bombs_province_miguel %>% 
  select(provincename, south_corrected) %>% 
  mutate(varname_1 = recode(provincename,
                            'Ba Ria' = 'Ba Ria - Vung Tau',
                            'Da Nang (City)' = 'Da Nang',
                            'Ha Noi (City)' = 'Ha Noi',
                            'Hai Phong (City)' = 'Hai Phong',
                            'Ho Chi Minh (City)' = 'Ho Chi Minh',
                            'Thuathien-Hue' = 'Thua Thien Hue'))

# Spatial Join 

province_bmr <- st_join(gdf, vnmap1)

colnames(province_bmr) <- tolower(colnames(province_bmr))

province_names <- province_bmr %>% 
  sf::st_drop_geometry() %>% 
  select(name_1, varname_1) %>% 
  distinct() %>% 
  filter(!is.na(varname_1))

province_bmr_sum <- province_bmr %>% 
  ungroup() %>% 
  filter(!is.na(varname_1)) %>% 
  group_by(varname_1, name_1) %>% 
  summarise(tot_bmr = sum(numweaponsdelivered),
            tot_bmr_lb = sum(weightdelivered),
            tot_agri = sum(numweaponsdelivered[agriculture == 1]),
            tot_agri_lb = sum(weightdelivered[agriculture == 1]),
            tot_industry = sum(numweaponsdelivered[industry == 1]),
            tot_industry_lb = sum(weightdelivered[industry == 1]),
            ) %>% 
  ungroup() %>% 
  sf::st_drop_geometry() %>% 
  left_join(prov_casualties, by = "varname_1")

vnmap1 <- vnmap1 %>% rename(name_1 = NAME_1)

province_bmr_sf <- left_join(province_bmr_sum, vnmap1, by = "name_1") %>% st_as_sf()

bases <- st_transform(bases, crs = 4326)
hcmtrail <- st_transform(hcmtrail, crs = 4326)

nearest_base <- st_nearest_feature(province_bmr_sf, bases)
province_bmr_sf$dist_nearest_base <-
  as.numeric(st_distance(province_bmr_sf, bases[nearest_base, ], by_element = TRUE)) / 1000

nearest_trail <- st_nearest_feature(province_bmr_sf, hcmtrail)
province_bmr_sf$dist_nearest_hochi <-
  as.numeric(st_distance(province_bmr_sf, hcmtrail[nearest_trail, ], by_element = TRUE)) / 1000

distance_info <- province_bmr_sf %>% 
  select(varname_1, dist_nearest_base, dist_nearest_hochi)
distance_info <- sf::st_drop_geometry(distance_info)

prov_controls <- ppn60 %>%
  filter(!is.na(tinh)) %>% 
  select(tinh, ppn60)

province_bmr_sum <- province_bmr_sum %>% 
  left_join(distance_info, by = "varname_1") %>%
  filter(!is.na(tot_bmr)) %>% 
  ungroup() %>% 
  select(-varname_1) %>%
  mutate(name_1 = recode(name_1,
                         'Điện Biên' = 'Lai Châu',
                         'Sơn La' = 'Lai Châu',
                         .default = name_1)) %>% 
  mutate(tinh = recode(name_1,
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
  select(tinh, everything()) %>% 
  left_join(prov_controls, by = "tinh") %>% 
  mutate(tot_bmr_prov_ppn = tot_bmr_prov/ppn60,
         tot_bmr_lb_prov_ppn = tot_bmr_lb_prov/ppn60,
         killed_tot_prov_ppn = killed_tot_prov/ppn60)

save(province_bmr_sum, file = "province_bmr_sum.Rda")

# Target type by north/south 
targets_sum <- province_bmr %>% 
  sf::st_drop_geometry() %>% 
  summarise(agri_s = sum(numweaponsdelivered[tgtcountry == "SOUTH VIETNAM" & agriculture == 1], na.rm = T),
            agri_n = sum(numweaponsdelivered[tgtcountry == "NORTH VIETNAM" & agriculture == 1], na.rm = T),
            infra_s = sum(numweaponsdelivered[tgtcountry == "SOUTH VIETNAM" & infrastructure == 1], na.rm = T),
            infra_n = sum(numweaponsdelivered[tgtcountry == "NORTH VIETNAM" & infrastructure == 1], na.rm = T),
            civilian_s = sum(numweaponsdelivered[tgtcountry == "SOUTH VIETNAM" & civilian == 1], na.rm = T),
            civilian_n = sum(numweaponsdelivered[tgtcountry == "NORTH VIETNAM" & civilian == 1], na.rm = T),
            industry_s = sum(numweaponsdelivered[tgtcountry == "SOUTH VIETNAM" & industry == 1], na.rm = T),
            industry_n = sum(numweaponsdelivered[tgtcountry == "NORTH VIETNAM" & industry == 1], na.rm = T))


# Bombing intensity by district 

vnmap2 <- vnmap2 %>% 
  mutate(distname2018 = paste(TYPE_2, NAME_2, sep = " ")) %>% 
  mutate(distname2018 = case_when(
    distname2018 == 'Thành phố Thành Phố Bắc Kạn' ~ 'Thành Phố Bắc Kạn',
    distname2018 == 'Quận Quận 1' ~ 'Quận 1',
    distname2018 == 'Quận Quận 10' ~ 'Quận 10',
    distname2018 == 'Quận Quận 12' ~ 'Quận 12',
    distname2018 == 'Quận Quận 11' ~ 'Quận 11',
    distname2018 == 'Quận Quận 2' ~ 'Quận 2',
    distname2018 == 'Quận Quận 3' ~ 'Quận 3',
    distname2018 == 'Quận Quận 4' ~ 'Quận 4',
    distname2018 == 'Quận Quận 5' ~ 'Quận 5',
    distname2018 == 'Quận Quận 6' ~ 'Quận 6',
    distname2018 == 'Quận Quận 7' ~ 'Quận 7',
    distname2018 == 'Quận Quận 8' ~ 'Quận 8',
    distname2018 == 'Quận Quận 9' ~ 'Quận 9',
    distname2018 == "Huyện Tịnh Biên" & NAME_1 == "An Giang" ~ "Thị xã Tịnh Biên",
    distname2018 == "Thị xã Dĩ An" & NAME_1 == "Bình Dương" ~ "Thành phố Dĩ An",
    distname2018 == "Thị xã Thuận An" & NAME_1 == "Bình Dương" ~ "Thành phố Dĩ An",
    distname2018 == "Thị xã Tân Uyên" & NAME_1 == "Bình Dương" ~ "Huyện Bắc Tân Uyên",
    distname2018 == "Huyện Chơn Thành" & NAME_1 == "Bình Phước" ~ "Thị xã Chơn Thành",
    distname2018 == "Thị xã Đồng Xoài" & NAME_1 == "Bình Phước" ~ "Thành phố Đồng Xoài",
    distname2018 == "Huyện Hoài Nhơn" & NAME_1 == "Bình Định" ~ "Thị xã Hoài Nhơn",
    distname2018 == "Thành phố Qui Nhơn" & NAME_1 == "Bình Định" ~ "Thành phố Quy Nhơn",
    distname2018 == "Huyện Việt Yên" & NAME_1 == "Bắc Giang" ~ "Thị Xã Việt Yên",
    distname2018 == "Huyện Quế Võ" & NAME_1 == "Bắc Ninh" ~ "Thị xã Quế Võ",
    distname2018 == "Huyện Thuận Thành" & NAME_1 == "Bắc Ninh" ~ "Thị xã Thuận Thành",
    distname2018 == "Thị xã Từ Sơn" & NAME_1 == "Bắc Ninh" ~ "Thành phố Từ Sơn",
    distname2018 == "Huyện Duy Tiên" & NAME_1 == "Hà Nam" ~ "Thị xã Duy Tiên",
    distname2018 == "Thị xã Kỳ Anh (Thị xã)" & NAME_1 == "Hà Tĩnh" ~ "Thị xã Kỳ Anh",
    distname2018 == "Huyện Mỹ Hào" & NAME_1 == "Hưng Yên" ~ "Thị xã Mỹ Hào",
    distname2018 == "Thị xã Chí Linh" & NAME_1 == "Hải Dương" ~ "Thành phố Chí Linh",
    distname2018 == "Huyện Kinh Môn" & NAME_1 == "Hải Dương" ~ "Thị xã Kinh Môn",
    distname2018 == "Thị xã Long Mỹ (Thị xã)" & NAME_1 == "Hậu Giang" ~ "Thị xã Long Mỹ",
    distname2018 == "Thị xã Ngã Bảy" & NAME_1 == "Hậu Giang" ~ "Thành phố Ngã Bảy",
    distname2018 == "Thị xã Hà Tiên" & NAME_1 == "Kiên Giang" ~ "Thành phố Hà Tiên",
    distname2018 == "Huyện Phú Quốc" & NAME_1 == "Kiên Giang" ~ "Thành phố Phú Quốc",
    distname2018 == "Huyện Đông Hòa" & NAME_1 == "Phú Yên" ~ "Thị xã Đông Hòa",
    distname2018 == "Thành phố Thành Phố Đồng Hới" & NAME_1 == "Quảng Bình" ~ "Thành Phố Đồng Hới",
    distname2018 == "Huyện Đức Phổ" & NAME_1 == "Quảng Ngãi" ~ "Thị xã Đức Phổ",
    distname2018 == "Thị xã Phổ Yên" & NAME_1 == "Thái Nguyên" ~ "Thành phố Phổ Yên",
    distname2018 == "Thị xã Cai Lậy (Thị xã)" & NAME_1 == "Tiền Giang" ~ "Thị xã Cai Lậy",
    distname2018 == "Thị xã Duyên Hải (Thị xã)" & NAME_1 == "Trà Vinh" ~ "Thị xã Duyên Hải",
    distname2018 == "Huyện Hòa Thành" & NAME_1 == "Tây Ninh" ~ "Thị xã Hòa Thành",
    distname2018 == "Huyện Trảng Bàng" & NAME_1 == "Tây Ninh" ~ "Thị xã Trảng Bàng",
    distname2018 == "Huyện Vũng Liêm" & NAME_1 == "Vĩnh Long" ~ "Huyện  Vũng Liêm",
    distname2018 == "Thị xã Phúc Yên" & NAME_1 == "Vĩnh Phúc" ~ "Thành phố Phúc Yên",
    distname2018 == "Thị xã Thị Xã Buôn Hồ" & NAME_1 == "Đắk Lắk" ~ "Thị Xã Buôn Hồ",
    distname2018 == "Thị xã Gia Nghĩa" & NAME_1 == "Đắk Lắk" ~ "Thành phố Gia Nghĩa",
    distname2018 == "Thị xã Long Khánh" & NAME_1 == "Đồng Nai" ~ "Thành phố Long Khánh",
    distname2018 == "Thành phố Cao Lãnh (Thành phố)" & NAME_1 == "Đồng Tháp" ~ "Thành phố Cao Lãnh",
    distname2018 == "Thị xã Hồng Ngự (Thị xã)" & NAME_1 == "Đồng Tháp" ~ "Thị xã Hồng Ngự",
    TRUE ~ distname2018
  ),
  NAME_1 = ifelse(distname2018 == "Thành phố Gia Nghĩa" & NAME_1 == "Đắk Lắk", "Đắk Nông", NAME_1))

district_bmr <- st_join(gdf, vnmap2)
district_bmr <- st_join(district_bmr, vnmap1)

district_bmr_sf <- district_bmr %>% 
  group_by(NAME_1, VARNAME_1, VARNAME_2, distname2018) %>% 
  summarise(
    tot_bmr = sum(NUMWEAPONSDELIVERED, na.rm = T),
    tot_bmr_lb = sum(WEIGHTDELIVERED, na.rm = T)) %>% 
  sf::st_drop_geometry()

district_bmr_sf <- left_join(district_bmr_sf, vnmap2, by = c("NAME_1", "VARNAME_2", "distname2018")) %>% 
  st_as_sf()

nearest_base_dist <- st_nearest_feature(district_bmr_sf, bases)
district_bmr_sf$nearest_base <-
  as.numeric(st_distance(district_bmr_sf, bases[nearest_base_dist, ], by_element = TRUE)) / 1000

nearest_trail_dist <- st_nearest_feature(district_bmr_sf, hcmtrail)
district_bmr_sf$dist_nearest_hochi <-
  as.numeric(st_distance(district_bmr_sf, hcmtrail[nearest_trail_dist, ], by_element = TRUE)) / 1000

district_bmr_sum <- district_bmr_sf %>% 
  sf::st_drop_geometry() %>% 
  select(NAME_1, VARNAME_1, VARNAME_2, distname2018, tot_bmr, tot_bmr_lb, nearest_base, dist_nearest_hochi) %>% 
  rename_all(tolower) %>% 
  left_join(dist_casualties, by = c("varname_1", "distname2018")) %>% 
  rename(provname2018 = name_1) %>% 
  filter(!is.na(provname2018)) %>% 
  distinct()

save(district_bmr_sum, file = "district_bmr_sum.Rda")
