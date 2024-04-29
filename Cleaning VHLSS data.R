
ivid02 <- c("tinh","xa", "hoso", "matv", "tinh02", "huyen02", "xa02", "diaban02", "hoso02", "matv02", "qui", "phieu")
ivid04 <- c("tinh", "huyen", "diaban", "xa", "hoso", "matv", "ky")
ivid06 <- c("tinh", "huyen", "diaban", "xa", "hoso", "matv")

hhid02 <- c("tinh02", "huyen02", "xa02", "diaban02", "hoso02", "qui", "phieu")
hhid04 <- c("tinh", "huyen", "xa", "hoso", "diaban", "ky")
hhid06 <- c("tinh", "huyen", "xa", "hoso")
hhid10 <- c("tinh", "huyen", "xa", "diaban", "hoso")

def02 <- inc_02 %>% select(tinh02, huyen02, xa02, diaban02, hoso02, qui, hhsize, urban, rcpi, mcpi, wt75)
def04 <- inc_04 %>% select(tinh, huyen, xa, hoso, urban, rcpi, mcpi, wt45)
def06 <- inc_04 %>% select(tinh, huyen, xa, hoso, urban, rcpi, mcpi, wt45)

wt08 <- wt08 %>% select(tinh, huyen, xa, diaban, wt9)
wt10 <- wt10 %>% select(-quyen) %>% rename(urban = ttnt)
wt12 <- wt12 %>% select(tinh, huyen, xa, diaban, wt9)

vhlss_emp_fn <- function(i){
  i %>% 
    mutate(
      work = ifelse(is.na(work), 0, work),
      selfagri = ifelse(is.na(selfagri), 0, selfagri),
      selfemp = ifelse(is.na(selfemp), 0, selfemp),
      wagework = ifelse(is.na(wagework), 0, wagework),
      work = ifelse(work == 0 & age < 17 | work == 0 & age > 64, NA, work),
      selfagri = ifelse(selfagri == 0 & age < 17 | selfagri == 0 & age > 64, NA, selfagri),
      selfemp = ifelse(selfemp == 0 & age < 17 | selfemp == 0 & age > 64, NA, selfemp),
      wagework = ifelse(wagework == 0 & age < 17 | wagework == 0 & age > 64, NA, wagework)
      )
}

########
# 2002 # 
########

vhlss02 <- list(m1_02, m2_02, m3_02) %>%
  map(~mutate(.x, matv02 = as.numeric(str_sub(as.character(matv02), -2)))) %>%
  reduce(full_join, by = ivid02) %>% 
  left_join(m5a_02, by = ivid02) %>% 
  mutate(female = ifelse(m1c2 == 2, 1, 0),
         married = ifelse(m1c6 == 2, 1, 0),
         hhhead = ifelse(m1c3 == 1, 1, 0),
         fhead = ifelse(female == 1 & hhhead == 1, 1, 0),
         wagework = ifelse(m3c1a == 1, 1, 0),
         work = ifelse(m3c2 == 1, 1, 0),
         selfagri = ifelse(m3c1b == 1, 1, 0),
         selfagri = ifelse(selfagri == 0 & work == 0, NA, selfagri),
         selfemp = ifelse(m3c1c == 1, 1, 0),
         inc = m5ac6 + m5ac7e,
         wartime = ifelse(m1c5 > 42 & m1c5 < 62, 1, 0),
         tinh02 = ifelse(tinh02 == 105, 101, tinh02),
         tinh02 = ifelse(tinh02 == 303 | tinh02 == 302, 301, tinh02)) %>% 
  rename(age = m1c5,
         educ = m2c1,
         hours = m3c3,
         days = m3c10,
         industry = m3c7) %>% 
  vhlss_emp_fn () %>% 
  group_by(tinh02, huyen02, xa02, diaban02, hoso02, qui, phieu) %>% 
  mutate(hhid = cur_group_id()) %>% 
  select(tinh02, huyen02, xa02, diaban02, hoso02, matv02, qui, phieu, hhhead, fhead, female, age, wartime, educ, 
         work, wagework, selfemp, selfagri, industry, inc, hours, days, hhid) %>% 
  left_join(def02, by = c("tinh02", "huyen02", "xa02", "diaban02", "hoso02","qui")) %>% 
  rename(tinh = tinh02) %>% 
  left_join(province_bmr_sum, by = "tinh") %>% 
  mutate(south = ifelse(tinh > 407, 1, 0),
         agri = ifelse(industry < 5, 1, 0),
         agri = ifelse(is.na(agri), 0, agri),
         agri = ifelse(agri == 0 & age < 17 | agri == 0 & age > 64, NA, agri),
         manu = ifelse(industry > 14 & industry < 40, 1, 0),
         manu = ifelse(is.na(manu), 0, manu),
         manu = ifelse(manu == 0 & age < 17 | manu == 0 & age > 64, NA, manu),
         inc = ifelse(is.na(work), NA, inc),
         hours = ifelse(is.na(work), NA, hours),
         days = ifelse(is.na(work), NA, days))

########
# 2004 # 
########

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
         formal = ifelse(m4ac10b == 1, 1, 0),
         wartime = ifelse(m1ac5 > 44 & m1ac5 < 64, 1, 0),
         tinh = ifelse(tinh == 105, 101, tinh),
         tinh = ifelse(tinh == 303 | tinh == 302, 301, tinh)) %>% 
  rename(age = m1ac5,
         educ = m2c1,
         industry = m4ac5,
         days = m4ac7,
         hours = m4ac8,
         inc = m4ac11) %>% 
  vhlss_emp_fn () %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, ky, hhhead, fhead, female, age, married, wartime, educ, 
         work, formal, wagework, selfemp, selfagri, industry, inc, hours, days) %>% 
  left_join(def04, by = c("tinh", "huyen", "xa", "hoso")) %>% 
  mutate(inc = inc/mcpi/rcpi) %>% 
  distinct() %>% 
  group_by(tinh, huyen, xa, hoso, ky) %>% 
  mutate(hhid = cur_group_id(),
         south = ifelse(tinh > 407, 1, 0)) %>% 
  left_join(province_bmr_sum, by = "tinh") %>% 
  mutate(south = ifelse(tinh > 407, 1, 0),
         agri = ifelse(industry < 5, 1, 0),
         agri = ifelse(is.na(agri), 0, agri),
         agri = ifelse(agri == 0 & age < 17 | agri == 0 & age > 64, NA, agri),
         manu = ifelse(industry > 14 & industry < 40, 1, 0),
         manu = ifelse(is.na(manu), 0, manu),
         manu = ifelse(manu == 0 & age < 17 | manu == 0 & age > 64, NA, manu),
         inc = ifelse(is.na(work), NA, inc),
         hours = ifelse(is.na(work), NA, hours),
         days = ifelse(is.na(work), NA, days))

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
         formal = ifelse(m4ac10b == 1, 1, 0),
         wartime = ifelse(m1ac5 > 44 & m1ac5 < 64, 1, 0),
         work = ifelse(work == 0 & m1ac5 < 15 | work == 0 & m1ac5 > 64, NA, work),
         tinh = ifelse(tinh == 105, 101, tinh),
         tinh = ifelse(tinh == 303 | tinh == 302, 301, tinh)) %>% 
  rename(age = m1ac5,
         educ = m2ac1,
         industry = m4ac5,
         days = m4ac7,
         hours = m4ac8,
         inc = m4ac11) %>% 
  vhlss_emp_fn () %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, hhhead, fhead, female, age, wartime, educ, 
         work, formal, wagework, selfemp, selfagri, industry, inc, hours, days) %>% 
  left_join(def06, by = hhid06) %>% 
  mutate(inc = inc/mcpi/rcpi) %>% 
  distinct() %>% 
  group_by(tinh, huyen, xa, hoso) %>% 
  mutate(hhid = cur_group_id())  %>% 
  left_join(province_bmr_sum, by = "tinh") %>% 
  mutate(south = ifelse(tinh > 407, 1, 0),
         agri = ifelse(industry < 5, 1, 0),
         agri = ifelse(is.na(agri), 0, agri),
         agri = ifelse(agri == 0 & age < 17 | agri == 0 & age > 64, NA, agri),
         manu = ifelse(industry > 14 & industry < 40, 1, 0),
         manu = ifelse(is.na(manu), 0, manu),
         manu = ifelse(manu == 0 & age < 17 | manu == 0 & age > 64, NA, manu),
         inc = ifelse(is.na(work), NA, inc),
         hours = ifelse(is.na(work), NA, hours),
         days = ifelse(is.na(work), NA, days))

########
# 2008 #
########

vhlss08 <- list(m123a_08, m4a_08) %>% 
  reduce(full_join, by = ivid06) %>% 
  mutate(hh = tinh*10^10 + huyen*10^8 + xa*10^6 + diaban*10^3 + hoso,
         female = ifelse(m1ac2 == 2, 1, 0),
         married = ifelse(m1ac6 == 2, 1, 0),
         hhhead = ifelse(m1ac3 == 1, 1, 0),
         fhead = ifelse(female == 1 & hhhead == 1, 1, 0),
         wagework = ifelse(m4ac1a == 1, 1, 0),
         work = ifelse(m4ac2 == 1, 1, 0),
         selfemp = ifelse(m4ac1c == 1, 1, 0),
         selfagri = ifelse(m4ac1b == 1, 1, 0),
         work = ifelse(work == 0 & m1ac5 < 15 | work == 0 & m1ac5 > 64, NA, work),
         m2ac1 = as.numeric(m2ac1),
         tinh = ifelse(tinh == 105, 101, tinh),
         tinh = ifelse(tinh == 303 | tinh == 302, 301, tinh)) %>% 
  rename(age = m1ac5,
         educ = m2ac1,
         industry = m4ac5,
         days = m4ac7,
         hours = m4ac8,
         inc = m4ac11) %>% 
  vhlss_emp_fn () %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, hhhead, fhead, female, age, educ, 
         work, wagework, selfemp, selfagri, industry, inc, hours, days, hh) %>% 
  left_join(wt08, by = c("tinh", "huyen", "xa", "diaban")) %>% 
  mutate(south = ifelse(tinh > 407, 1, 0)) %>% 
  distinct() %>% 
  group_by(tinh, huyen, xa, diaban, hoso) %>% 
  mutate(hhid = cur_group_id())  %>% 
  left_join(province_bmr_sum, by = "tinh") %>% 
  mutate(south = ifelse(tinh > 407, 1, 0),
         agri = ifelse(industry < 5, 1, 0),
         agri = ifelse(is.na(agri), 0, agri),
         agri = ifelse(agri == 0 & age < 17 | agri == 0 & age > 64, NA, agri),
         manu = ifelse(industry > 9 & industry < 41, 1, 0),
         manu = ifelse(is.na(manu), 0, manu),
         manu = ifelse(manu == 0 & age < 17 | manu == 0 & age > 64, NA, manu),
         inc = ifelse(is.na(work), NA, inc),
         hours = ifelse(is.na(work), NA, hours),
         days = ifelse(is.na(work), NA, days))

########
# 2010 #
########

vhlss10 <- list(m1a_10, m2a_10, m4a1_10, m4a2_10, m4a3_10, m4a4_10) %>% 
  reduce(full_join, by = ivid06) %>% 
  mutate(female = ifelse(m1ac2 == 2, 1, 0),
         married = ifelse(m1ac6 == 2, 1, 0),
         hhhead = ifelse(m1ac3 == 1, 1, 0),
         fhead = ifelse(female == 1 & hhhead == 1, 1, 0),
         wagework = ifelse(m4ac1a == 1, 1, 0),
         work = ifelse(m4ac2 == 1, 1, 0),
         selfemp = ifelse(m4ac1c == 1, 1, 0),
         selfagri = ifelse(m4ac1b == 1, 1, 0),
         work = ifelse(work == 0 & m1ac5 < 15 | work == 0 & m1ac5 > 64, NA, work),
         tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh),
         m2ac1 = as.numeric(m2ac1)) %>% 
  rename(age = m1ac5,
         educ = m2ac1,
         industry = m4ac4,
         days = m4ac6,
         hours = m4ac7,
         inc = m4ac11) %>% 
  vhlss_emp_fn () %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, hhhead, fhead, female, age, educ, 
         work, wagework, selfemp, selfagri, industry, inc, hours, days) %>% 
  left_join(wt10, by = hhid10) %>% 
  distinct() %>% 
  group_by(tinh, huyen, xa, diaban, hoso) %>% 
  mutate(hhid = cur_group_id(),
         south = ifelse(tinh > 44, 1, 0))  %>% 
  left_join(province_bmr_sum2, by = "tinh") %>% 
  mutate(agri = ifelse(industry < 5, 1, 0),
         agri = ifelse(is.na(agri), 0, agri),
         agri = ifelse(agri == 0 & age < 17 | agri == 0 & age > 64, NA, agri),
         manu = ifelse(industry > 9 & industry < 41, 1, 0),
         manu = ifelse(is.na(manu), 0, manu),
         manu = ifelse(manu == 0 & age < 17 | manu == 0 & age > 64, NA, manu),
         inc = ifelse(is.na(work), NA, inc),
         hours = ifelse(is.na(work), NA, hours),
         days = ifelse(is.na(work), NA, days))

########
# 2012 #
########

vhlss12 <- list(m1a_12, m2a1_12) %>% 
  reduce(full_join, by = ivid06) %>% 
  mutate(female = ifelse(m1ac2 == 2, 1, 0),
         married = ifelse(m1ac6 == 2, 1, 0),
         hhhead = ifelse(m1ac3 == 1, 1, 0),
         fhead = ifelse(female == 1 & hhhead == 1, 1, 0),
         wagework = ifelse(m4ac1a == 1, 1, 0),
         work = ifelse(m4ac2 == 1, 1, 0),
         selfemp = ifelse(m4ac1c == 1, 1, 0),
         selfagri = ifelse(m4ac1b == 1, 1, 0),
         work = ifelse(work == 0 & m1ac5 < 15 | work == 0 & m1ac5 > 64, NA, work),
         tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh),
         m2ac1 = as.numeric(m2ac1)) %>% 
  rename(age = m1ac5,
         educ = m2ac1,
         industry = m4ac4,
         days = m4ac6,
         hours = m4ac7,
         inc = m4ac11) %>% 
  vhlss_emp_fn () %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, hhhead, fhead, female, age, educ, 
         work, wagework, selfemp, selfagri, industry, inc, hours, days) %>% 
  left_join(wt12, by = c("tinh", "huyen", "xa", "diaban")) %>% 
  distinct() %>% 
  group_by(tinh, huyen, xa, diaban, hoso) %>% 
  mutate(hhid = cur_group_id(),
         south = ifelse(tinh > 44, 1, 0))  %>% 
  left_join(province_bmr_sum2, by = "tinh") %>% 
  mutate(agri = ifelse(industry < 5, 1, 0),
         agri = ifelse(is.na(agri), 0, agri),
         agri = ifelse(agri == 0 & age < 17 | agri == 0 & age > 64, NA, agri),
         manu = ifelse(industry > 9 & industry < 41, 1, 0),
         manu = ifelse(is.na(manu), 0, manu),
         manu = ifelse(manu == 0 & age < 17 | manu == 0 & age > 64, NA, manu),
         inc = ifelse(is.na(work), NA, inc),
         hours = ifelse(is.na(work), NA, hours),
         days = ifelse(is.na(work), NA, days))

save(vhlss02, file = "vhlss02.Rda")
save(vhlss04, file = "vhlss04.Rda")
save(vhlss06, file = "vhlss06.Rda")
save(vhlss08, file = "vhlss08.Rda")
save(vhlss10, file = "vhlss10.Rda")
save(vhlss12, file = "vhlss12.Rda")
