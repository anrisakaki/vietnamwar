######################
# CLEANING THOR DATA #
######################

thor <- thor %>% 
  filter(TGTCOUNTRY == "NORTH VIETNAM" | TGTCOUNTRY == "SOUTH VIETNAM") %>%
  select(-c("THOR_DATA_VIET_ID", "SOURCEID", "SOURCERECORD", "VALID_AIRCRAFT_ROOT", "TIMEONTARGET", "WEAPONTYPECLASS", "AIRCRAFT_ROOT",
            "AIRFORCESQDN", "AIRFORCEGROUP", "CALLSIGN", "NUMOFACFT", "OPERATIONSUPPORTED", "UNIT", "TGTCLOUDCOVER", "TGTCONTROL",
            "ADDITIONALINFO", "GEOZONE", "ID", "RELEASEALTITUDE", "RELEASEFLTSPEED", "TIMEOFFTARGET", "TIMEONTARGET", "TGTID", "TGTORIGCOORDS", "TGTORIGCOORDSFORMAT")) %>%
  filter(!is.na(TGTLATDD_DDD_WGS84),
         !is.na(TGTLONDDD_DDD_WGS84))

weapons_dict <- weapons_dict %>% 
  select(-c("WEAPON_ID", "WEAPON_COUNT"))

thor <- left_join(thor, weapons_dict, by = "WEAPONTYPE")

thor <- thor %>%
  mutate(MSNDATE = as.Date(MSNDATE),
         year = as.integer(format(MSNDATE, "%Y")),
         month = as.integer(format(MSNDATE, "%m")),
         day = as.integer(format(MSNDATE, "%d"))) %>% 
  filter(WEAPON_CLASS != "SUPPORT",
         WEAPON_CLASS != "GUN",
         MFUNC_DESC == "AIR INTERDICTION" | MFUNC_DESC == "STRIKE")

# Export thor to ArcGIS 

thor_dist <- thor_district %>% 
  select(-c(OID_, Join_Count, TARGET_FID, Field1, NUMWEAPONSJETTISONED, NUMWEAPONSRETURNED, AREA, TOTPOP_CY)) %>%
  filter(!is.na(ID)) %>% 
  mutate(WEAPONSLOADEDWEIGHT = ifelse(WEAPONSLOADEDWEIGHT == -1, NA, WEAPONSLOADEDWEIGHT)) %>% 
  group_by(NAME) %>% 
  summarise(tot_bombs = sum(NUMWEAPONSDELIVERED),
            tot_bombs_weight= sum(WEAPONSLOADEDWEIGHT, na.rm = T))

dist_ <- district %>%
  select(distname2019, prov2002, dist2002) %>% 
  distinct() %>% 
  filter(!is.na(dist2002), 
         distname2019 != "") %>% 
  mutate(xa = sprintf("%02d", dist2002),
         district = paste0(as.character(prov2002), xa)) %>% 
  select(distname2019, district) %>% 
  rename(NAME = distname2019)

thor_dist <- left_join(thor_dist, dist_, by = "NAME") %>%
  filter(!is.na(district))

thor_prov <- thor_dist %>% 
  mutate(tinh = substr(district, 1, 3)) %>% 
  group_by(tinh) %>% 
  summarise(tot_bombs_prov = sum(tot_bombs))
