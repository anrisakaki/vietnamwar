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
  # Removing rocket and missIle launchers to prevent double counting 
  filter(!grepl("AGM|AIM|LAU|2.75", WEAPONTYPE),
         !grepl("PAMPHLET", WEAPONTYPE_DESC)) %>% 
  mutate(
    civilian = case_when(
      TGTTYPE %in% c("AGRICULTURAL AREA", "CIV POPULATN CENTR", "VILLAGE", "BUILDINGS", "HUTS") ~ 1,
      grepl("PERSONNEL", TGTTYPE, ignore.case = TRUE) ~ 1,
      TRUE ~ 0 
    ),
    air = case_when(
      grepl("AIR", MFUNC_DESC, ignore.case = TRUE) ~ 1,
      TRUE ~ 0
    ),
    civilian_ex_per = case_when(
      TGTTYPE %in% c("AGRICULTURAL AREA", "CIV POPULATN CENTR", "VILLAGE", "BUILDINGS", "HUTS") ~ 1,
      TRUE ~ 0 
    ),    
    industry = case_when(
      TGTTYPE %in% c("CONSTRUCTION SITE", "FACTORY INDUSTRIAL", "FACTORY,ANY", "ELEC. PWR. FAC.") ~ 1,
      TRUE ~ 0
    ),
    infrastructure = case_when(
      TGTTYPE %in% c("BRIDGE", "FERRY", "FERRY CROSSING", "PIER",
                     "TUNNEL", "TUNNELS") ~ 1,
      grepl("RAILROAD|BRIDGE|ROAD", TGTTYPE, ignore.case = TRUE) ~ 1,
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

# Spatial Join 

province_bombs <- st_join(gdf, vnmap1)

colnames(province_bombs) <- tolower(colnames(province_bombs))

province_bombs_sum <- province_bombs %>% 
  ungroup() %>% 
  filter(!is.na(varname_1)) %>% 
  group_by(varname_1, name_1) %>% 
  summarise(tot_bmr = sum(numweaponsdelivered),
            tot_bmr_lb = sum(weightdelivered)) %>% 
  ungroup()
province_bombs_sum <- sf::st_drop_geometry(province_bombs_sum)

province_civilian_ex_per_sum <- province_bombs %>% 
  ungroup() %>% 
  filter(civilian_ex_per == 1) %>% 
  group_by(varname_1, name_1) %>% 
  summarise(tot_civilian_ex_per = sum(numweaponsdelivered)) %>% 
  ungroup() %>% 
  filter(!is.na(varname_1))
province_dualuse_sum <- sf::st_drop_geometry(province_dualuse_sum)

province_civilian_sum <- province_bombs %>% 
  ungroup() %>% 
  filter(civilian == 1) %>% 
  group_by(varname_1) %>% 
  summarise(tot_civilian = sum(numweaponsdelivered)) %>% 
  ungroup() %>% 
  filter(!is.na(varname_1))
province_civilian_sum <- sf::st_drop_geometry(province_civilian_sum)

province_infra_sum <- province_bombs %>% 
  ungroup() %>% 
  filter(infrastructure == 1) %>% 
  group_by(varname_1) %>% 
  summarise(tot_infrastructure = sum(numweaponsdelivered)) %>% 
  ungroup() %>% 
  filter(!is.na(varname_1))
province_infra_sum <- sf::st_drop_geometry(province_infra_sum)

province_industry_sum <- province_bombs %>% 
  ungroup() %>% 
  filter(industry == 1) %>% 
  group_by(varname_1) %>% 
  summarise(tot_industry = sum(numweaponsdelivered)) %>% 
  ungroup() %>% 
  filter(!is.na(varname_1))
province_industry_sum <- sf::st_drop_geometry(province_industry_sum)

province_dualuse_sum <- list(province_dualuse_sum, province_civilian_sum, province_infra_sum, province_industry_sum) %>% 
  reduce(full_join, by = "varname_1") %>% 
  mutate(tot_industry = ifelse(is.na(tot_industry), 0, tot_industry),
         tot_infrastructure = ifelse(is.na(tot_infrastructure), 0, tot_infrastructure),
         tot_civilian = ifelse(is.na(tot_civilian), 0, tot_civilian))

provarea <- provarea %>% 
  rename(name_1 = Province) %>%
  select(name_1, Area) %>%
  mutate(name_1 = ifelse(name_1 == "Khánh Hoà", "Khánh Hòa", name_1),
         name_1 = ifelse(name_1 == "TP.Hồ Chí Minh", "Hồ Chí Minh", name_1),
         name_1 = ifelse(name_1 == "Thanh Hoá", "Thanh Hóa", name_1))

vnmap1 <- vnmap1 %>% rename(name_1 = NAME_1)

province_bombs_sum <- list(province_bombs_sum, provarea, vnmap1) %>% 
  reduce(full_join, by = "name_1") %>% 
  mutate(bmr_per = tot_bmr/Area)

province_bombs_sum <- left_join(province_bombs_sum, prov_casualties, by = "varname_1") %>% 
  mutate(deaths_per = killed_tot/Area)

province_bombs_sf <- province_bombs_sum %>% st_as_sf()

province_bombs_sum <- province_bombs_sum %>% select(varname_1, name_1, tot_bmr, Area, bmr_per, killed_tot, deaths_per, wounded_tot, missing_tot)
province_bombs_sum <- sf::st_drop_geometry(province_bombs_sum)

province_bombs_sum <- list(province_bombs_sum, province_dualuse_sum, province_mfunc_sum) %>% 
  reduce(full_join, by = c("varname_1", "name_1"))

colnames(province_bombs_sum) <- tolower(colnames(province_bombs_sum))

save(province_bombs_sum, file = "province_bombs_sum.rda")
