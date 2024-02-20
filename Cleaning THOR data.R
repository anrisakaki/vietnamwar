load("prov_casualties.Rda")

######################
# CLEANING THOR DATA #
######################

thor <- thor %>% 
  filter(!(SOURCERECORD == "SACCOACT" & MSNDATE >= 19710301)) %>% 
  mutate(MSNDATE = ifelse(MSNDATE == "1970-02-29", "1970-03-01", MSNDATE),
         MFUNC_DESC_CLASS = ifelse(MFUNC_DESC == "COMBT CARGO AIR  DROP", "KINETIC", MFUNC_DESC_CLASS)) %>%
  filter(!(SOURCERECORD == "SACCOACT" & MSNDATE >= 19710301)) %>%
  mutate(MSNDATE = as.Date(MSNDATE),
         year = as.integer(format(MSNDATE, "%Y")),
         month = as.integer(format(MSNDATE, "%m")),
         day = as.integer(format(MSNDATE, "%d")),
         TGTTYPE = ifelse(TGTTYPE == "", NA, TGTTYPE),
         MFUNC_DESC = ifelse(MFUNC_DESC == "", NA, MFUNC_DESC),
         MFUNC_DESC_CLASS = ifelse(MFUNC_DESC == "COMBT CARGO AIR  DROP", "KINETIC", MFUNC_DESC_CLASS),
         MFUNC_DESC = ifelse(MFUNC_DESC == "COMBT CARGO AIR  DROP", "HEAVY BOMBARD", MFUNC_DESC)) %>% 
  filter(TGTCOUNTRY == "NORTH VIETNAM" | TGTCOUNTRY == "SOUTH VIETNAM") %>% 
  filter(!is.na(TGTLATDD_DDD_WGS84),
         !is.na(TGTLONDDD_DDD_WGS84))

weapons_dict <- weapons_dict %>% 
  select(-c("WEAPON_ID", "WEAPON_COUNT"))

thor <- left_join(thor, weapons_dict, by = "WEAPONTYPE")

thor <- thor %>%
  filter(WEAPON_CLASS != "SUPPORT",
         WEAPON_CLASS != "GUN",
         MFUNC_DESC_CLASS == "KINETIC") %>% 
  mutate(
    dualuse = case_when(
      TGTTYPE %in% c("AGRICULTURAL AREA", "BRIDGE", "BUILDINGS", "CIV POPULATN CENTR", 
                     "CONSTRUCTION SITE", "FACTORY INDUSTRIAL", "FACTORY,ANY", "ELEC. PWR. FAC.", 
                     "FERRY", "FERRY CROSSING", "HUTS", "LIGHTS ON ROAD", "PIER", "PLANTS", 
                     "TUNNEL", "TUNNELS", "VILLAGE") ~ 1,
      grepl("RAILROAD|BRIDGE|ROAD", TGTTYPE, ignore.case = TRUE) ~ 1,
      TRUE ~ 0
    ),
    civilian = case_when(
      TGTTYPE %in% c("AGRICULTURAL AREA", "CIV POPULATN CENTR", "VILLAGE") ~ 1,
      TRUE ~ 0 
    ),
    industry = case_when(
      TGTTYPE %in% c("CONSTRUCTION SITE", "FACTORY INDUSTRIAL", "FACTORY,ANY", "ELEC. PWR. FAC.") ~ 1,
      TRUE ~ 0
    ),
    infrastructure = case_when(
      TGTTYPE %in% c("BRIDGE", "FERRY", "FERRY CROSSING", "PIER",
                     "TUNNEL", "TUNNELS", "VILLAGE") ~ 1,
      grepl("RAILROAD|BRIDGE|ROAD", TGTTYPE, ignore.case = TRUE) ~ 1,
      TRUE ~ 0
  ))

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
  summarise(tot_bmr = sum(numweaponsdelivered)) %>% 
  ungroup()
province_bombs_sum <- sf::st_drop_geometry(province_bombs_sum)

province_mfunc_sum <- province_bombs %>% 
  ungroup() %>% 
  mutate(mfunc_desc = ifelse(mfunc_desc == "DIR AIR SUPPT", "DIRECT AIR SUPPORT", mfunc_desc)) %>% 
  filter(!is.na(varname_1),
         mfunc_desc %in% c("STRIKE", "DIRECT AIR SUPPORT", "HEAVY BOMBARD", "CLOSE AIR SUPPORT", "AIR INTERDICTION") == T) %>% 
  group_by(varname_1, name_1, mfunc_desc) %>% 
  summarise(tot_bmr = sum(numweaponsdelivered)) %>% 
  ungroup()
province_mfunc_sum <- sf::st_drop_geometry(province_mfunc_sum)
province_mfunc_sum <- province_mfunc_sum %>% 
  pivot_wider(names_from = "mfunc_desc", values_from = "tot_bmr", values_fill = 0)

province_dualuse_sum <- province_bombs %>% 
  ungroup() %>% 
  filter(dualuse == 1) %>% 
  group_by(varname_1, name_1) %>% 
  summarise(tot_dualuse = sum(numweaponsdelivered)) %>% 
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
