######################
# CLEANING THOR DATA #
######################

thor <- thor %>% 
  filter(TGTCOUNTRY == "NORTH VIETNAM" | TGTCOUNTRY == "SOUTH VIETNAM") %>%
  select(-c("THOR_DATA_VIET_ID", "SOURCEID", "SOURCERECORD", "VALID_AIRCRAFT_ROOT", "TIMEONTARGET", "WEAPONTYPECLASS", "AIRCRAFT_ROOT",
            "AIRFORCESQDN", "AIRFORCEGROUP", "CALLSIGN", "NUMOFACFT", "OPERATIONSUPPORTED", "UNIT", "TGTCLOUDCOVER", "TGTCONTROL",
            "ADDITIONALINFO", "GEOZONE", "ID", "RELEASEALTITUDE", "RELEASEFLTSPEED", "TIMEOFFTARGET", "TIMEONTARGET", "TGTID", "TGTORIGCOORDS", "TGTORIGCOORDSFORMAT")) %>%
  filter(!is.na(TGTLATDD_DDD_WGS84),
         !is.na(TGTLONDDD_DDD_WGS84)) %>%
  filter(MFUNC_DESC_CLASS == "KINETIC",
         MFUNC_DESC == "AIR INTERDICTION" | MFUNC_DESC == "STRIKE",
         NUMWEAPONSDELIVERED != 0,
         WEAPONSLOADEDWEIGHT != 0,
         WEAPONTYPEWEIGHT != 0,
         # Removing weapons that are not bombs, missiles or rockets 
         WEAPONTYPE != "105 HOWITZER AMMO",
         WEAPONTYPE != "105HE HOWITZER AMMO",
         WEAPONTYPE != "12.7 MM AMMO",
         WEAPONTYPE != "20 MM AMMO (HNDRDS)",
         WEAPONTYPE != "30 CAL",
         WEAPONTYPE != "30 DAY CAN",
         WEAPONTYPE != "40MM HE XM384",
         WEAPONTYPE != "40MM MISC METAL",
         WEAPONTYPE != "5.56MM",
         WEAPONTYPE != "5.59 MM AMMO (HNDRDS)",
         WEAPONTYPE != "50 CAL",
         WEAPONTYPE != "50 CALIBAR AMMO",
         WEAPONTYPE != "7.62 AMMO",
         WEAPONTYPE != "7.62MM",
         WEAPONTYPE != "ADU-253",
         WEAPONTYPE != "ADU-253 DISPENSER",
         WEAPONTYPE != "ADU-272",
         WEAPONTYPE != "ADU-273 DISPENSER",
         WEAPONTYPE != "ADU-285",
         WEAPONTYPE != "AERO-70 RKT LAUNCHER",
         WEAPONTYPE != "AIR DELIV SEISMIC MO",
         WEAPONTYPE != "ALQ-91",
         WEAPONTYPE != "BATTLEFIELD CASUALTYS",
         WEAPONTYPE != "CARGO (TONS)",
         WEAPONTYPE != "COMMIKE III SENSOR",
         WEAPONTYPE != "FLARES-MK24_MK6_LUUI8",
         WEAPONTYPE != "FLARES--MLU-32/899",
         WEAPONTYPE != "HERBICIDE",
         WEAPONTYPE != "HERBICIDE (GALLONS)",
         WEAPONTYPE != "HILL GENIE",
         WEAPONTYPE != "KMU342B (M117 LGB-750)",
         WEAPONTYPE != "LEAFLET BOMB",
         WEAPONTYPE != "LEAFLET MANUAL",
         WEAPONTYPE != "LUU2 FLARE",
         WEAPONTYPE != "M129E1 CHAFF DISP",
         WEAPONTYPE != "M129E2 CHAFF DISP",
         WEAPONTYPE != "M129EA1 LEAFLETS(1K)",
         WEAPONTYPE != "MC01 FAID",
         WEAPONTYPE != "MC02 DAR",
         WEAPONTYPE != "MK188",
         WEAPONTYPE != "MK32",
         WEAPONTYPE != "PASSENGERS",
         WEAPONTYPE != "PH FLASH M112",
         WEAPONTYPE != "PH FLASH M120",
         WEAPONTYPE != "PH FLASH M123",
         WEAPONTYPE != "PHOTO EQUIP",
         WEAPONTYPE != "RADIOS",
         WEAPONTYPE != "SA06 SENSOR",
         WEAPONTYPE != "SA08 SENSOR",
         WEAPONTYPE != "SAO1- CV1- 1 ADSID",
         WEAPONTYPE != "SAO2 HANDSID",
         WEAPONTYPE != "SAO3 HELOSID",
         WEAPONTYPE != "SAO4 FADSID",
         WEAPONTYPE != "SAP BM",
         WEAPONTYPE != "SB02- LSD- ACOUBUY",
         WEAPONTYPE != "SB03- SPIKE -CV3-",
         WEAPONTYPE != "SB04 DUMMY",
         WEAPONTYPE != "SC01 MAGID",
         WEAPONTYPE != "SC08 ACQUSID III SNSR",
         WEAPONTYPE != "SCO1 ADSID III SENSOR",
         WEAPONTYPE != "SD01 IRID",
         WEAPONTYPE != "SPEAKER HOURS (TENTHS)",
         WEAPONTYPE != "SUU24A",
         WEAPONTYPE != "SUU-40A or CS-1 BULK DISP",
         WEAPONTYPE != "TROOPS",
         WEAPONTYPE != "UNK",
         WEAPONTYPE != "UNKNOWN",
         WEAPONTYPE != "XK145",
         WEAPONTYPE != "XM384 40MM AMMO")

weapons_dict <- weapons_dict %>% 
  select(-c("WEAPON_ID", "WEAPON_COUNT"))

thor <- left_join(thor, weapons_dict, by = "WEAPONTYPE")

thor <- thor %>%
  mutate(MSNDATE = as.Date(MSNDATE),
         year = as.integer(format(MSNDATE, "%Y")),
         month = as.integer(format(MSNDATE, "%m")),
         day = as.integer(format(MSNDATE, "%d")))

thor_kinetic_missions_AI <- thor %>% 
  filter(MFUNC_DESC_CLASS == "KINETIC",
         MFUNC_DESC == "AIR INTERDICTION") %>% 
  # !grepl("PATROL | PREP", MFUNC_DESC)) %>% 
  select(TGTLONDDD_DDD_WGS84, TGTLATDD_DDD_WGS84, MISSIONID, MFUNC_DESC) %>%
  distinct()

thor_kinetic_missions_strike <- thor %>% 
  filter(MFUNC_DESC_CLASS == "KINETIC",
         MFUNC_DESC == "STRIKE") %>% 
  # !grepl("PATROL | PREP", MFUNC_DESC)) %>% 
  select(TGTLONDDD_DDD_WGS84, TGTLATDD_DDD_WGS84, MISSIONID, MFUNC_DESC) %>%
  distinct()

thor_kinetic_missions <- thor %>% 
  filter(MFUNC_DESC_CLASS == "KINETIC",
         MFUNC_DESC == "AIR INTERDICTION" | MFUNC_DESC == "STRIKE") %>% 
         # !grepl("PATROL | PREP", MFUNC_DESC)) %>% 
  select(TGTLONDDD_DDD_WGS84, TGTLATDD_DDD_WGS84, MISSIONID, MFUNC_DESC) %>%
  distinct()
