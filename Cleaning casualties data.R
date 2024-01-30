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

process_casualties_sf <- function(df) {
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
  
  df <- left_join(df, zones, by = "ID")
  
  df <- df %>%
    filter(!is.na(Zone),
           !grepl("000000", UTM.MAP.COORDINATE)) %>%
    mutate(MGRS = paste0(Zone, UTM.MAP.COORDINATE)) %>% 
    filter(MGRS != "48QYC190505",
           MGRS != "49PBSUNKNOW") %>% 
    mutate(
      x = lapply(MGRS, mgrs_to_latlng, include_mgrs_ref = FALSE)
    ) %>% 
    unnest(x) %>% 
    st_as_sf(
      coords = c("lng", "lat"),
      crs = 4326
    )
  
  return(df)
}

casualties_sf <- lapply(casualties_list, process_casualties_sf)

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

