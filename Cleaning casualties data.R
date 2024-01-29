# Cleaning casualties data 
process_dataframe <- function(df) {
  df <- df %>%
    mutate(ID = substr(UTM.MAP.COORDINATE, 1, 2)) %>%
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
  
  return(df)
}

casualties <- lapply(casualties_list, process_dataframe)

casualties_sf_fn <- function(df) {
  df <- df %>%
    filter(!is.na(Zone),
           !grepl("000000", UTM.MAP.COORDINATE)) %>%
    mutate(MGRS = paste0(Zone, UTM.MAP.COORDINATE))
  
  return(df)
}

casualties_sf <- lapply(casualties, casualties_sf_fn)

casualties_sf[[1]] <- st_as_sf(casualties_sf[[1]], coords = "MGRS")

combined_casualties <- do.call(rbind, casualties_selected[1:44])

combined_casualties <- combined_casualties %>% 
  mutate(ID = substr(UTM.MAP.COORDINATE, 1, 2))

combined_casualties <- left_join(combined_casualties, zones, by = "ID") %>% 
  filter(!is.na(Zone),
         !grepl("000000", UTM.MAP.COORDINATE)) %>% 
  mutate(MGRS = paste0(Zone, UTM.MAP.COORDINATE))

combined_casualties_sf <- st_as_sf(combined_casualties, coords = "UTM.MAP.COORDINATE")

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

