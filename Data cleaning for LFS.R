lfs_15 <- lfs_list[[1]] %>% 
  rename(age = C5,
         sex = C3,
         rel = C2,
         marital = C7,
         educ = C12,
         wage_work = C14,
         nonwage_work = C15,
         ISCO = C22,
         ISIC = C23,
         establishment = C24,
         brc = C26,
         emp_type = C28) %>% 
  filter(age > 14 & age < 66) %>% 
  mutate(year = 2015)

lfs_16 <- lfs_list[[2]] %>% 
  rename(age = c5,
         rel = c2,
         sex = c3,
         marital = c7,
         educ = c12,
         wage_work = c14,
         nonwage_work = c15,
         ISCO = c22,
         ISIC = c23,
         establishment = c24,
         brc = c26,
         emp_type = c28) %>% 
  filter(age > 14 & age < 66) %>% 
  mutate(year = 2016)

lfs_17 <- lfs_list[[3]] %>% 
  rename(age = C5,
         rel = C2,
         sex = C3,
         marital = C9,
         educ = C14,
         wage_work = C16,
         nonwage_work = C17,
         ISCO = C24,
         ISIC = C25,
         establishment = C26,
         brc = C28,
         emp_type = C30) %>% 
  filter(age > 14 & age < 66) %>% 
  mutate(year = 2017)

lfs_18 <- lfs_list[[4]] %>% 
  rename(age = C5,
         rel = C2,
         sex = C3,
         marital = C9,
         educ = C17,
         wage_work = C21,
         nonwage_work = C22,
         ISCO = C29C,
         ISIC = C30C,
         establishment = C31,
         brc = C33,
         emp_type = C35) %>% 
  filter(age > 14 & age < 66) %>% 
  mutate(year = 2018)

lfs_fn <- function(i){
  
  i %>% 
    mutate(work = ifelse(wage_work == 1 | nonwage_work == 1, 1, 0),
           nonagri_self = ifelse(nonwage_work == 1 & ))
}