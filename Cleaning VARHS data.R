#######################
# Cleaning VARHS data # 
#######################

# 2008 

varhs1_08 <- varhs1_08 %>% 
  mutate_all(~ ifelse(. == 98, NA, .)) %>% 
  rename(birth_year = p2q4_,
         illness = p2q5_) %>% 
  mutate(female = ifelse(p2q3_ == 2, 1, 0),
         illness = ifelse(illness == 1, 1, 0),
         mental_health = ifelse(!is.na(illness) & p2q6a_ == 1, 1, 0)) %>% 
  select(tinh_2008, quan_2008, xa_2008, ma_h0_2008, p2stt_, birth_year, female, illness, mental_health)

varhs5a_08 <- varhs5a_08 %>% 
  mutate_all(~ ifelse(. == 98, NA, .)) %>% 
  rename(p2stt_ = p28ma_,
         income = p28q10b_) %>% 
  select(tinh_2008, quan_2008, xa_2008, ma_h0_2008, p2stt_, income)

vars10_08 <- varhs10_08 %>% 
  mutate_all(~ ifelse(. == 98, NA, .)) %>% 
  rename(p2stt_ = stt) %>% 
  mutate(vet_union = ifelse(p47q1_ == 5, 1, 0)) %>% 
  select(tinh_2008, quan_2008, xa_2008, ma_h0_2008, p2stt_, vet_union)

vars_08 <- list(varhs1_08, varhs5a_08, vars10_08) %>% 
  reduce(full_join, by = c("tinh_2008", "quan_2008", "xa_2008", "ma_h0_2008", "p2stt_"))

# 2010 

varhs1_10 <- varhs1_10 %>% 
  mutate_all(~ ifelse(. == 98, NA, .)) %>% 
  rename(birth_year = p1q4_,
         illness = p1q5_,
         educ = p2q14_,
         p2stt_ = p1stt_) %>% 
  mutate(female = ifelse(p1q3_ == 2, 1, 0),
         illness = ifelse(illness == 1, 1, 0),
         vn_army = ifelse(p2q17_ == 1, 1, 0),
         mental_health = ifelse(!is.na(illness) & p1q6a_ == 1, 1, 0),
         child = ifelse(p1q2_ == 3, 1, 0)) %>% 
  select(tinh_2010, quan_2010, xa_2010, ma_h0_2010, p2stt_, birth_year, child, female, illness, mental_health, educ, vn_army)

varhs5_10 <- varhs5_10 %>% 
  mutate(work = ifelse(p26q1a_ == 1 | p26q1b_ == 1, 1, 0)) %>% 
  filter(p26q2_ == 98) %>% 
  rename(p2stt_ = p26ma_) %>% 
  select(tinh_2010, quan_2010, xa_2010, ma_h0_2010, p2stt_, work) %>%
  filter(work == 1)

varhs5a_10 <- varhs5a_10 %>% 
  mutate_all(~ ifelse(. == 98, NA, .)) %>% 
  rename(income = p27q10b_,
         p2stt_ = p27ma_) %>% 
  mutate(income = ifelse(p27q10a_ == 2, income*20, income),
         income = ifelse(p27q10a_ == 5, income/260, income)) %>% 
  select(tinh_2010, quan_2010, xa_2010, ma_h0_2010, p2stt_, income) 
  
varhs10_10 <- varhs10_10 %>% 
  mutate_all(~ ifelse(. == 98, NA, .)) %>% 
  rename(p2stt_ = p44q1_) %>% 
  mutate(vet_union = ifelse(p44q2_ == 5, 1, 0)) %>% 
  select(tinh_2010, quan_2010, xa_2010, ma_h0_2010, p2stt_, vet_union)

varhs_10 <- list(varhs1_10, varhs5_10, varhs5a_10, varhs10_10) %>% 
  reduce(full_join, by = c("tinh_2010", "quan_2010", "xa_2010", "ma_h0_2010", "p2stt_"))

varhs_10 <- varhs_10 %>% 
  mutate(vn_army = ifelse(is.na(vn_army), 1, 0)) %>% 
  group_by(tinh_2010, quan_2010, xa_2010, ma_h0_2010) %>% 
  mutate(hhid = cur_group_id()) %>% 
  group_by(tinh_2010, quan_2010, xa_2010, ma_h0_2010, p2stt_) %>% 
  mutate(ivid = cur_group_id()) %>% 
  group_by(hhid) %>% 
  mutate(hh_army = ifelse(any(vn_army == 1), 1, 0)) %>% 
  ungroup()
