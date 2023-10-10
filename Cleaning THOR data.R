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
         MFUNC_DESC == "AIR INTERDICTION" | MFUNC_DESC == "STRIKE") %>%
  filter(NUMWEAPONSDELIVERED > 0)

# Export thor to ArcGIS 

thor_dist <- thor_dist %>% 
  select(-c(OID_, Join_Count, TARGET_FID, Field1, NUMWEAPONSJETTISONED, NUMWEAPONSRETURNED, AREA, TOTPOP_CY)) %>%
  filter(!is.na(ID))
