ivid02 <- c("tinh","xa", "hoso", "matv", "tinh02", "huyen02", "xa02", "diaban02", "hoso02", "matv02", "qui", "phieu")
ivid04 <- c("tinh", "huyen", "diaban", "xa", "hoso", "matv", "ky")
ivid06 <- c("tinh", "huyen", "diaban", "xa", "hoso", "matv")

hhid02 <- c("tinh02", "huyen02", "xa02", "diaban02", "hoso02", "qui", "phieu")
hhid04 <- c("tinh", "huyen", "xa", "hoso", "diaban", "ky")
hhid06 <- c("tinh", "huyen", "xa", "hoso")

def02 <- inc_02 %>% select(tinh02, huyen02, xa02, diaban02, hoso02, qui, hhsize, urban, rcpi, mcpi, wt75)
def04 <- inc_04 %>% select(tinh, huyen, xa, hoso, urban, rcpi, mcpi, wt45)
def06 <- inc_04 %>% select(tinh, huyen, xa, hoso, urban, rcpi, mcpi, wt45)

########
# 2002 # 
########

vhlss02 <- list(m1_02, m2_02, m3_02) %>%
  map(~mutate(.x, matv02 = as.numeric(str_sub(as.character(matv02), -2)))) %>%
  reduce(full_join, by = ivid02) %>% 
  left_join(m5a_02, by = ivid02)

vhlss02 <- vhlss02 %>% 
  mutate(female = ifelse(m1c2 == 2, 1, 0),
         married = ifelse(m1c6 == 2, 1, 0),
         hhhead = ifelse(m1c3 == 1, 1, 0),
         fhead = ifelse(female == 1 & hhhead == 1, 1, 0),
         wagework = ifelse(m3c1a == 1, 1, 0),
         work = ifelse(m3c2 == 1, 1, 0),
         selfemp = ifelse(m3c8 == 0, 1, 0),
         selfagri = ifelse(m3c1b == 1, 1, 0),
         inc = m5ac6 + m5ac7e) %>% 
  rename(age = m1c5,
         educ = m2c1,
         hours = m3c3,
         days = m3c10,
         industry = m3c7) %>% 
  group_by(tinh02, huyen02, xa02, diaban02, hoso02, qui, phieu) %>% 
  mutate(hhid = cur_group_id()) %>% 
  select(tinh02, huyen02, xa02, diaban02, hoso02, matv02, qui, phieu, hhhead, fhead, female, age, educ, 
         work, wagework, selfemp, selfagri, industry, inc, hours, days, hhid) %>% 
  left_join(def02, by = c("tinh02", "huyen02", "xa02", "diaban02", "hoso02","qui"))

########
# 2004 # 
########

fself_emp <- list(m123a_04, m4a_04) %>% 
  reduce(full_join, by = ivid04) %>% 
  filter(m4ac1c == 1 & m1ac2 == 2) %>% 
  mutate(hhid04=tinh*10^9+huyen*10^7+xa*10^5+diaban*100+hoso) %>% 
  select(tinh, huyen, xa, diaban, hoso, hhid04, matv)

bus04 <- busid %>% 
  filter(!is.na(hhid04)) %>%
  select(hhid04, busi_num04, manager) %>% 
  rename(matv = manager) %>% 
  distinct()

# List of women being managers of their business 
f_bus <- merge(fself_emp, bus04, by = c("hhid04", "matv")) %>% 
  select(-c(hhid04, matv)) %>% 
  mutate(f_manager = 1) %>% 
  rename(m10ama = busi_num04) %>% 
  distinct()

fbus <- merge(fself_emp, bus04, by = c("hhid04", "matv")) %>% 
  select(-c(hhid04, busi_num04)) %>% 
  mutate(f_manager = 1)

hhbus04 <- m10a_04 %>% 
  filter() %>% 
  mutate(hh_lab = ifelse(m10c12 == 1, 1, 0),
         hh_lab = ifelse(is.na(hh_lab), 0, hh_lab)) %>% 
  mutate_all(~ifelse(. == -1, NA, .)) %>% 
  rename(bus_start = m10c7,
         num_lab = m10c6) %>% 
  select(tinh, huyen, xa, diaban, hoso, m10ama, ky, bus_start, hh_lab, num_lab) %>% 
  left_join(f_bus, by = c("tinh", "huyen", "xa", "diaban", "hoso", "m10ama")) %>% 
  full_join(ho1_04, by = c("tinh", "huyen", "xa", "diaban", "hoso", "ky")) %>% 
  left_join(def04, by = c("tinh", "huyen", "xa", "hoso")) %>% 
  rename(hhsize = tsnguoi, 
         inc_hh = tthu_4,
         agri_rev = m4b1t/rcpi/mcpi,
         agri_inc = m4b1tn/rcpi/mcpi,
         nonfarm_rev = tthu_11/rcpi/mcpi,
         nonfarm_exp = m4cc/rcpi/mcpi) %>% 
  mutate(f_manager = ifelse(is.na(f_manager) & nonfarm_rev > 0, 0, f_manager)) %>%   
  select(tinh, huyen, xa, diaban, hoso, ky, hhsize, bus_start, hh_lab, inc_hh, agri_rev, 
         agri_inc, nonfarm_rev, nonfarm_exp, f_manager, urban, wt45) %>%
  distinct() 

vhlss04 <- list(m123a_04, m4a_04) %>% 
  reduce(full_join, by = ivid04) %>% 
  mutate(female = ifelse(m1ac2 == 2, 1, 0),
         married = ifelse(m1ac6 == 2, 1, 0),
         hhhead = ifelse(m1ac3 == 1, 1, 0),
         fhead = ifelse(female == 1 & hhhead == 1, 1, 0),
         wagework = ifelse(m4ac1a == 1, 1, 0),
         work = ifelse(m4ac2 == 1, 1, 0),
         selfemp = ifelse(m4ac1c == 1, 1, 0),
         selfagri = ifelse(m4ac1b == 1, 1, 0),
         formal = ifelse(m4ac10b == 1, 1, 0)) %>% 
  rename(age = m1ac5,
         educ = m2c1,
         industry = m4ac5,
         days = m4ac7,
         hours = m4ac8,
         inc = m4ac11) %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, ky, hhhead, fhead, female, age, educ, 
         work, formal, wagework, selfemp, selfagri, industry, inc, hours, days) %>% 
  left_join(def04, by = c("tinh", "huyen", "xa", "hoso")) %>% 
  mutate(inc = inc/mcpi/rcpi) %>% 
  distinct() %>% 
  group_by(tinh, huyen, xa, hoso, ky) %>% 
  mutate(hhid = cur_group_id())  

########
# 2006 # 
########

vhlss06 <- list(m1a_06, m2a_06, m4a_06) %>% 
  map(~mutate(.x, diaban = as.numeric(diaban))) %>%
  reduce(full_join, by = ivid06) %>% 
  mutate(female = ifelse(m1ac2 == 2, 1, 0),
         married = ifelse(m1ac6 == 2, 1, 0),
         hhhead = ifelse(m1ac3 == 1, 1, 0),
         fhead = ifelse(female == 1 & hhhead == 1, 1, 0),
         wagework = ifelse(m4ac1a == 1, 1, 0),
         work = ifelse(m4ac2 == 1, 1, 0),
         selfemp = ifelse(m4ac1c == 1, 1, 0),
         selfagri = ifelse(m4ac1b == 1, 1, 0),
         formal = ifelse(m4ac10b == 1, 1, 0)) %>% 
  rename(age = m1ac5,
         educ = m2ac1,
         industry = m4ac5,
         days = m4ac7,
         hours = m4ac8,
         inc = m4ac11) %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, hhhead, fhead, female, age, educ, 
         work, formal, wagework, selfemp, selfagri, industry, inc, hours, days) %>% 
  left_join(def06, by = hhid06) %>% 
  mutate(inc = inc/mcpi/rcpi) %>% 
  distinct() %>% 
  group_by(tinh, huyen, xa, hoso) %>% 
  mutate(hhid = cur_group_id())  
