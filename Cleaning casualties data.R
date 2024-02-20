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
      OPERATION.NAME,
      ID
    )
  
  df <- left_join(df, zones, by = "ID") %>% 
    mutate(MGRS = paste0(Zone, UTM.MAP.COORDINATE),
           MGRS = ifelse(is.na(Zone), NA, MGRS))
  
  df <- left_join(df, cl_provcodes, by = "MAJOR.PROVINCE.CODE")
  
  return(df)
  
  }

casualties <- lapply(casualties_list, process_casualties)
combined_casualties <- do.call(rbind, casualties[1:53])
  
process_casualties_sf <- function(df){
  
  df <- df %>%
    filter(!is.na(Zone),
           !grepl("UNKNOW", UTM.MAP.COORDINATE)) %>%
    filter(MGRS != "48QYC190505",
           MGRS != "48PZC083997",
           MGRS != "48QYD320750",
           MGRS != "49PAT830990",
           MGRS != "49PAT860930",
           MGRS != "49PBT046779",
           MGRS != "49PBT057734",
           MGRS != "49PBT020720",
           MGRS != "49PBT020720",
           MGRS != "49PBT050760",
           MGRS != "49PBT070780",
           MGRS != "49PBT578729",
           MGRS != "49PBT020720",
           MGRS != "49PBT040850",
           MGRS != "49PBT071721",
           MGRS != "48QYC950340",
           MGRS != "49PAT820760",
           MGRS != "49PAT840870",
           MGRS != "49PAT871761",
           MGRS != "49PAT899824",
           MGRS != "49PAT880850",
           MGRS != "49PAT900910",
           MGRS != "49PAT930830") %>% 
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
casualties_sf <- do.call(rbind, casualties_sf[1:53])

prov_codes_casualties <- casualties_sf %>%
  filter(!is.na(VARNAME_1)) %>% 
  select(MGRS, VARNAME_1) %>% 
  distinct()

combined_casualties <- left_join(combined_casualties, prov_codes_casualties, by = "MGRS")

# Can deduce Province by name of operation 
operation_prov <- combined_casualties %>% 
  select(OPERATION.NAME, VARNAME_1) %>% 
  filter(OPERATION.NAME != "",
         !is.na(VARNAME_1),
         OPERATION.NAME != "%NO NAME<") %>% 
  sf::st_drop_geometry(operation_prov) %>% 
  distinct() %>% 
  group_by(OPERATION.NAME) %>%
  filter(n() == 1,
         !grepl("INITIATED", OPERATION.NAME)) %>%
  ungroup() %>% 
  rename(prov = VARNAME_1)

combined_casualties <- left_join(combined_casualties, operation_prov, by = "OPERATION.NAME") %>% 
  mutate(prov = ifelse(is.na(prov), VARNAME_1, prov)) %>% 
  mutate(
    prov = case_when(
      Meaning == 'An Giang' & is.na(prov) ~ 'An Giang',
      Meaning == 'BA Xuyen' & is.na(prov) ~ 'Soc Trang',
      Meaning == 'Bac Lieu' & is.na(prov) ~ 'Bac Lieu',
      Meaning == 'Bien Hoa' & is.na(prov) ~ 'Dong Nai',
      Meaning == 'Binh Dinh' & is.na(prov) ~ 'Binh Dinh',
      Meaning == 'Binh Duong' & is.na(prov) ~ 'Binh Duong',
      Meaning == 'Binh Long' & is.na(prov) ~ 'Binh Phuoc',
      Meaning == 'Binh Thuan' & is.na(prov) ~ 'Binh Thuan',
      Meaning == 'Chau Doc' & is.na(prov) ~ 'An Giang',
      Meaning == 'Darlac' & is.na(prov) ~ 'Dak Lak',
      Meaning == 'Dinh Tuong' & is.na(prov) ~ 'Tien Giang',
      Meaning == 'Ghuong Thien' & is.na(prov) ~ 'Hau Giang',
      Meaning == 'Gia Dinh' & is.na(prov) ~ 'Ho Chi Minh',
      Meaning == 'Go Cong' & is.na(prov) ~ 'Tien Giang',
      Meaning == 'Hau Nghia' & is.na(prov) ~ 'Long An',
      Meaning == 'Khank Hoa' & is.na(prov) ~ 'Khanh Hoa',
      Meaning == 'Kien Giang' & is.na(prov) ~ 'Kien Giang',
      Meaning == 'Kien Hoa' & is.na(prov) ~ 'Ben Tre',
      Meaning == 'Kien Phong' & is.na(prov) ~ 'Dong Thap',
      Meaning == 'Kien Tuong' & is.na(prov) ~ 'Long An',
      Meaning == 'Kontum' & is.na(prov) ~ 'Kon Tum',
      Meaning == 'Lam Dong' & is.na(prov) ~ 'Lam Dong',
      Meaning == 'Long An' & is.na(prov) ~ 'Long An',
      Meaning == 'Long Khanh' & is.na(prov) ~ 'Dong Nai',
      Meaning == 'Ninh Thuan' & is.na(prov) ~ 'Ninh Thuan',
      Meaning == 'Phong Dinh' & is.na(prov) ~ 'Can Tho',
      Meaning == 'Phu Bon' & is.na(prov) ~ 'Gia Lai',
      Meaning == 'Phu Yen' & is.na(prov) ~ 'Phu Yen',
      Meaning == 'Phuoc Long' & is.na(prov) ~ 'Binh Phuoc',
      Meaning == 'Phuoc Tuy' & is.na(prov) ~ 'Ba Ria - Vung Tau',
      Meaning == 'Pleiku' & is.na(prov) ~ 'Dak Lak',
      Meaning == 'Quang Duc' & is.na(prov) ~ 'Dak Nong',
      Meaning == 'Quang Nam' & is.na(prov) ~ 'Quang Nam',
      Meaning == 'Quang Ngai' & is.na(prov) ~ 'Quang Ngai',
      Meaning == 'Quang Tin' & is.na(prov) ~ 'Quang Nam',
      Meaning == 'Quang Tri' & is.na(prov) ~ 'Quang Tri',
      Meaning == 'SA Dec' & is.na(prov) ~ 'Dong Thap',
      Meaning == 'Tay Ninh' & is.na(prov) ~ 'Tay Ninh',
      Meaning == 'Trung Phon' & is.na(prov) ~ 'An Giang',
      Meaning == 'Tuyen Duc' & is.na(prov) ~ 'Lam Dong',
      Meaning == 'Vinh Long' & is.na(prov) ~ 'Vinh Long',
      Meaning == 'Vinh Binh' & is.na(prov) ~ 'Tra Vinh',
      MAJOR.PROVINCE.CODE == 'Thua Thien' & is.na(prov) ~ 'Thua Thien Hue',
      MAJOR.PROVINCE.CODE == 39 & is.na(prov) ~ 'Ca Mau',
      MAJOR.PROVINCE.CODE == 'Binh Tuy' & is.na(prov) ~ 'Binh Thuan',
      TRUE ~ prov
    ))

prov_casualties <- combined_casualties %>% 
  mutate(
    Viet = ifelse(LOSS.NATIONALITY == "Viet Cong", NUMBER.DESTROYED.OF.KILLED, 0),
    LOSS.NATIONALITY = case_when(
      LOSS.NATIONALITY == 'RVN' ~ 'A',
      LOSS.NATIONALITY == 'Viet Cong' ~ 'V',
      TRUE ~ LOSS.NATIONALITY
    )
  ) %>%
  group_by(prov) %>% 
  summarise(viet_cong_deaths = sum(Viet),
            killed_tot = sum(NUMBER.DESTROYED.OF.KILLED),
            wounded_tot = sum(NUMBER.DAMAGED.OR.WOUNDED),
            missing_tot = sum(NUMBER.CAPTURED.OR.MISSING)) %>% 
  rename(VARNAME_1 = prov)

colnames(prov_casualties) <- tolower(colnames(prov_casualties))

save(prov_casualties, file = "prov_casualties.Rda")
