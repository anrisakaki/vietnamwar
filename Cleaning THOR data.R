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
  select(varname_1) %>% 
  distinct() %>% 
  filter(!is.na(varname_1))

south <- left_join(province_names, south, by = "varname_1") %>% 
  mutate(south_corrected = ifelse(varname_1 == "Dak Nong", 1, south_corrected),
         south_corrected = ifelse(varname_1 == "Hau Giang", 1, south_corrected),
         south_corrected = ifelse(varname_1 == "Dien Bien", 0, south_corrected)) %>% 
  rename(south = south_corrected)

province_bmr <- left_join(province_bmr, south, by = "varname_1")

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

provarea <- provarea %>% 
  rename(name_1 = Province) %>%
  select(name_1, Area) %>%
  mutate(name_1 = ifelse(name_1 == "Khánh Hoà", "Khánh Hòa", name_1),
         name_1 = ifelse(name_1 == "TP.Hồ Chí Minh", "Hồ Chí Minh", name_1),
         name_1 = ifelse(name_1 == "Thanh Hoá", "Thanh Hóa", name_1))

province_bmr_sum <- list(province_bmr_sum, provarea) %>% 
  reduce(full_join, by = "name_1")

vnmap1 <- vnmap1 %>% rename(name_1 = NAME_1)

province_bmr_sf <- left_join(province_bmr_sum, vnmap1, by = "name_1")
province_bmr_sf <- province_bmr_sf %>% st_as_sf()

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

province_bmr_sum <- left_join(province_bmr_sum, distance_info, by = "varname_1")

save(province_bmr_sum, file = "province_bmr_sum.Rda")

# Target type by north/south 

targets_sum <- province_bmr %>% 
  sf::st_drop_geometry() %>% 
  group_by(tgttype) %>% 
  summarise(tot_s = sum(numweaponsdelivered[south == 1], na.rm = T),
            tot_n = sum(numweaponsdelivered[south == 0], na.rm = T),
            ) %>% 
  filter(!is.na(tgttype))

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
    TRUE ~ distname2018
  ))

district_bmr <- st_join(gdf, vnmap2)

district_bmr_sum <- district_bmr %>% 
  group_by(NAME_1, distname2018) %>% 
  summarise(
    tot_bmr = sum(NUMWEAPONSDELIVERED, na.rm = T),
    civilian_bmr = sum(NUMWEAPONSDELIVERED[civilian == 1], na.rm = T),
    agri_bmr = sum(NUMWEAPONSDELIVERED[agriculture == 1], na.rm = T),
    industry_bmr = sum(NUMWEAPONSDELIVERED[industry == 1], na.rm = T)
  ) %>% 
  filter(!is.na(NAME_1)) %>% 
  sf::st_drop_geometry() %>% 
  rename(provname2018 = NAME_1) %>% 
  mutate()

district_bmr_sf <- district_bmr %>% 
  group_by(NAME_1, NAME_2, VARNAME_2) %>% 
  summarise(
    tot_bmr = sum(NUMWEAPONSDELIVERED, na.rm = T),
    civilian_bmr = sum(NUMWEAPONSDELIVERED[civilian == 1], na.rm = T),
    agri_bmr = sum(NUMWEAPONSDELIVERED[agriculture == 1], na.rm = T),
    industry_bmr = sum(NUMWEAPONSDELIVERED[industry == 1], na.rm = T)
  ) %>% 
  sf::st_drop_geometry()

district_bmr_sf <- left_join(district_bmr_sf, vnmap2, by = c("NAME_1", "NAME_2", "VARNAME_2")) %>% 
  st_as_sf()
