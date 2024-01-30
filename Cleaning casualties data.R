# Cleaning casualties data 
zones <- zones %>%
  mutate(ID = paste0(ID1, ID2)) %>% 
  select(Zone, ID) %>% 
  mutate(
    Zone = case_when(
      ID == 'YC' ~ '48Q',
      TRUE ~ Zone
    )
  )

cl_provcodes$MAJOR.PROVINCE.CODE <- as.character(cl_provcodes$MAJOR.PROVINCE.CODE) 

process_casualties <- function(df) {
  df <- df %>%
    mutate(ID = substr(UTM.MAP.COORDINATE, 1, 2),
           MAJOR.PROVINCE.CODE = as.character(MAJOR.PROVINCE.CODE)) %>%
    select(
      MAJOR.PROVINCE.CODE,
      NUMBER.DESTROYED.OF.KILLED,
      NUMBER.DAMAGED.OR.WOUNDED,
      NUMBER.CAPTURED.OR.MISSING,
      LOSS.NATIONALITY,
      UTM.MAP.COORDINATE,
      ID
    )
  
  df <- left_join(df, zones, by = "ID") %>% 
    mutate(MGRS = paste0(Zone, UTM.MAP.COORDINATE),
           MGRS = ifelse(is.na(Zone), NA, MGRS))
  
  df <- left_join(df, cl_provcodes, by = "MAJOR.PROVINCE.CODE")
  
  return(df)
  
  }

casualties <- lapply(casualties_list, process_casualties)
combined_casualties <- do.call(rbind, casualties[1:44])
  
process_casualties_sf <- function(df){
  
  df <- df %>%
    filter(!is.na(Zone),
           !grepl("UNKNOW", UTM.MAP.COORDINATE)) %>%
    mutate(MGRS = paste0(Zone, UTM.MAP.COORDINATE)) %>% 
    filter(MGRS != "48QYC190505",
           MGRS != "48PZC083997",
           MGRS != "48QYD320750",
           MGRS != "49PAT830990",
           MGRS != "49PAT860930") %>% 
    mutate(
      x = lapply(MGRS, mgrs_to_latlng, include_mgrs_ref = FALSE)
    ) %>% 
    unnest(x) %>% 
    st_as_sf(
      coords = c("lng", "lat"),
      crs = 4326
    )
  
  df <- st_join(df, vnmap1)
  
  return(df)
}

casualties_sf <- lapply(casualties, process_casualties_sf)
casualties_sf <- do.call(rbind, casualties_sf[1:44])

prov_codes_casualties <- casualties_sf %>%
  filter(!is.na(VARNAME_1)) %>% 
  select(MGRS, VARNAME_1) %>% 
  distinct()

combined_casualties <- left_join(combined_casualties, prov_codes_casualties, by = "MGRS")

prov_casualties <- combined_casualties %>% 
  mutate(
    LOSS.NATIONALITY = case_when(
      LOSS.NATIONALITY == 'RVN' ~ 'A',
      LOSS.NATIONALITY == 'Viet Cong' ~ 'V',
      TRUE ~ LOSS.NATIONALITY
    )
  ) %>%
  group_by(MAJOR.PROVINCE.CODE, LOSS.NATIONALITY) %>% 
  summarise(killed_tot = sum(NUMBER.DESTROYED.OF.KILLED),
            wounded_tot = sum(NUMBER.DAMAGED.OR.WOUNDED),
            missing_tot = sum(NUMBER.CAPTURED.OR.MISSING))

  
prov_casualties <- left_join(prov_casualties, cl_provcodes, by = "MAJOR.PROVINCE.CODE") %>% 
  mutate(Meaning = ifelse(is.na(Meaning), MAJOR.PROVINCE.CODE, Meaning)) %>% 
  mutate(
    varname_1 = case_when(
      MAJOR.PROVINCE.CODE == 'An Xuyen' ~ 'Ca Mau',
      MAJOR.PROVINCE.CODE == 'BA Xuyen' ~ 'Soc Trang',
      MAJOR.PROVINCE.CODE == 'Bien Hoa' ~ 'Dong Nai',
      MAJOR.PROVINCE.CODE == 'Binh Long' ~ 'Dong Nai',
      TRUE ~ MAJOR.PROVINCE.CODE
    )
  )

