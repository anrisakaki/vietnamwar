# Cleaning casualties data 

casualties_selected <- lapply(casualties_list, select_variables)

select_variables <- function(df) {
  df_selected <- df %>%
    select(
      MAJOR.PROVINCE.CODE,
      NUMBER.DESTROYED.OF.KILLED,
      NUMBER.DAMAGED.OR.WOUNDED,
      NUMBER.CAPTURED.OR.MISSING,
      LOSS.NATIONALITY,
      UTM.MAP.COORDINATE
    )
  return(df_selected)
}

combined_casualties <- do.call(rbind, casualties_selected[1:44])

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

cl_provcodes <- cl_provcodes %>% rename(MAJOR.PROVINCE.CODE = Code) 
cl_provcodes$MAJOR.PROVINCE.CODE <- as.character(cl_provcodes$MAJOR.PROVINCE.CODE)
  
prov_casualties <- left_join(prov_casualties, cl_provcodes, by = "MAJOR.PROVINCE.CODE") %>% 
  mutate(Meaning = ifelse(is.na(Meaning), MAJOR.PROVINCE.CODE, Meaning)) %>% 
  mutate(
    varname_1 = case_when(
      MAJOR.PROVINCE.CODE == 'An Xuyen' ~ 'Ca Mau',
      MAJOR.PROVINCE.CODE == 'BA Xuyen' ~ 'Soc Trang',
      MAJOR.PROVINCE.CODE == 'BA Xuyen' ~ 'Soc Trang',
      TRUE ~ MAJOR.PROVINCE.CODE
    )
  )

