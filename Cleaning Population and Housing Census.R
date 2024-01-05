# Cleaning Population and Housing Census 

phc <- phc %>%
  mutate(married = ifelse(marst == 2, 1, 0),
         widowed = ifelse(marst == 4, 1, 0),
         minority = ifelse(ethnicvn > 1, 1, 0),
         literate = ifelse(lit == 2, 1, 0),
         work = ifelse(empstat == 1, 1, 0),
         disabled = ifelse(disabled == 1, 1, 0),
         female = ifelse(sex == 2, 1, 0),
         agri = ifelse(indgen == 10, 1, 0))

phc89 <- phc %>% 
  filter(year == 1989) %>% 
  select(year, serial, hhwt, geo1_vn, regnvn, pernum, perwt, nchild, age, female, marst, married, widowed,
         birthyr, minority, literate, work, edattain, yrschool, occ, indgen, agri, empsect, ind, geomig1_5)

phc99 <- phc %>% 
  filter(year == 1999) %>% 
  select(year, serial, hhwt, geo1_vn, regnvn, pernum, perwt, nchild, age, female, marst, married, widowed,
         birthyr, minority, literate, work, edattain, yrschool, occ, indgen, agri, empsect, ind, geomig1_5)

phc09 <- phc %>% 
  filter(year == 2009) %>% 
  select(year, serial, hhwt, geo1_vn, regnvn, pernum, perwt, nchild, age, female, marst, married, widowed,
         birthyr, minority, literate, work, edattain, yrschool, occ, indgen, agri, empsect, ind, geomig1_5)

# merge with bombing data 


