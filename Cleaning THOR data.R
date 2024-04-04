load("prov_casualties.Rda")

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

civilian_targets <- gdf %>% filter(civilian == 1) %>% sf_to_df()
infrastructure_targets <- gdf %>% filter(infrastructure == 1) %>% sf_to_df()

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

targets_s <- province_bmr %>% 
  sf::st_drop_geometry() %>% 
  filter(south == 1) %>% 
  group_by(tgttype) %>% 
  summarise(tot_s = sum(numweaponsdelivered)) %>% 
  filter(!is.na(tgttype))

targets_n <- province_bmr %>% 
  sf::st_drop_geometry() %>% 
  filter(south == 0) %>% 
  group_by(tgttype) %>% 
  summarise(tot_n = sum(numweaponsdelivered)) %>% 
  filter(!is.na(tgttype))

targets_ns <- full_join(targets_s, targets_n, by = "tgttype")

agri_targets_ns <- province_bmr %>% 
  filter(agriculture == 1) %>% 
  group_by(south) %>% 
  sf::st_drop_geometry() %>% 
  summarise(tot_agri = sum(numweaponsdelivered),
            tot_agri_lb = sum(weightdelivered)) %>% 
  filter(!is.na(south))

industry_targets_ns <- province_bmr %>% 
  filter(industry == 1) %>% 
  group_by(south) %>% 
  sf::st_drop_geometry() %>% 
  summarise(tot_industry = sum(numweaponsdelivered),
            tot_industry_lb = sum(weightdelivered)) %>% 
  filter(!is.na(south))

# Bombing intensity by district 

district_bmr <- st_join(gdf, vnmap2)

district_bmr_sum <- district_bmr %>% 
  group_by(NAME_1, VARNAME_2) %>% 
  summarise(
    tot_bmr = sum(NUMWEAPONSDELIVERED, na.rm = T),
    civilian_bmr = sum(NUMWEAPONSDELIVERED[civilian == 1], na.rm = T),
    agri_bmr = sum(NUMWEAPONSDELIVERED[agriculture == 1], na.rm = T),
    industry_bmr = sum(NUMWEAPONSDELIVERED[industry == 1], na.rm = T)
  )

# Bombing intensity based on old provincial boundaries 

vn_old <- st_transform(vn_old, crs = 4326)

thor_old <- st_join(gdf, vn_old)

thor_old_sum <- thor_old %>% 
  group_by(NAME) %>% 
  summarise(tot_bmr = sum(NUMWEAPONSDELIVERED),
            tot_bmr_lb = sum(WEIGHTDELIVERED)) %>% 
  sf::st_drop_geometry() %>% 
  filter(!is.na(NAME))

infra_old <- thor_old %>% 
  filter(infrastructure == 1) %>% 
  group_by(NAME) %>% 
  summarise(tot_infrastructure = sum(NUMWEAPONSDELIVERED),
            tot_infra_lb = sum(WEIGHTDELIVERED)) %>% 
  sf::st_drop_geometry() %>% 
  filter(!is.na(NAME))

civilian_old <- thor_old %>% 
  filter(civilian == 1) %>% 
  group_by(NAME) %>% 
  summarise(tot_civilian = sum(NUMWEAPONSDELIVERED),
            tot_civilian_lb = sum(WEIGHTDELIVERED)) %>% 
  sf::st_drop_geometry() %>% 
  filter(!is.na(NAME))

thor_old_sum <- list(thor_old_sum, infra_old, civilian_old) %>% 
  reduce(full_join, by = "NAME")

oldprov_sexratio <- merge(postwar_ppn, thor_old_sum, by = "NAME") %>% 
  mutate(sexratio = (M_1976/F_1976)*100)

save(oldprov_sexratio, file = "oldprov_sexratio.Rda")
