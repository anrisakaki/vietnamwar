province_bmr_sum0002 <- province_bmr_sum02 %>% 
  rename(tinh = tinh02)

province_bmr_sum0419 <- province_bmr_sum %>% 
  mutate(tinh = recode(provname,
                       'An Giang' = 89,
                       'Bà Rịa - Vũng Tàu' = 77,
                       'Bắc Giang' = 24,
                       'Bắc Kạn' = 6,
                       'Bạc Liêu' = 95,
                       'Bắc Ninh' = 27,
                       'Bến Tre' = 83,
                       'Bình Định' = 52,
                       'Bình Dương' = 74,
                       'Bình Phước' = 70,
                       'Bình Thuận' = 60,
                       'Cà Mau' = 96,
                       'Cần Thơ' = 92,
                       'Hậu Giang' = 93,
                       'Cao Bằng' = 4,
                       'Đà Nẵng' = 48,
                       'Đắk Lắk' = 66,
                       'Đắk Nông' = 67,
                       'Điện Biên' = 11,
                       'Lai Châu' = 12,
                       'Đồng Nai' = 75,
                       'Đồng Tháp' = 87,
                       'Gia Lai' = 64,
                       'Hà Giang' = 2,
                       'Hà Nam' = 35,
                       'Hà Nội' = 1,
                       'Hà Tĩnh' = 42,
                       'Hải Dương' = 30,
                       'Hải Phòng' = 31,
                       'Hồ Chí Minh' = 79,
                       'Hoà Bình' = 17,
                       'Hưng Yên' = 33,
                       'Khánh Hòa' = 56,
                       'Kiên Giang' = 91,
                       'Kon Tum' = 62,
                       'Lâm Đồng' = 68,
                       'Lạng Sơn' = 20,
                       'Lào Cai' = 10,
                       'Long An' = 80,
                       'Nam Định' = 36,
                       'Nghệ An' = 40,
                       'Ninh Bình' = 37,
                       'Ninh Thuận' = 58,
                       'Phú Thọ' = 25,
                       'Phú Yên' = 54,
                       'Quảng Bình' = 44,
                       'Quảng Nam' = 49,
                       'Quảng Ngãi' = 51,
                       'Quảng Ninh' = 22,
                       'Quảng Trị' = 45,
                       'Sóc Trăng' = 94,
                       'Sơn La' = 14,
                       'Tây Ninh' = 72,
                       'Thái Bình' = 34,
                       'Thái Nguyên' = 19,
                       'Thanh Hóa' = 38,
                       'Thừa Thiên Huế' = 46,
                       'Tiền Giang' = 82,
                       'Trà Vinh' = 84,
                       'Tuyên Quang' = 8,
                       'Vĩnh Long' = 86,
                       'Vĩnh Phúc' = 26,
                       'Yên Bái' = 15,
                       .default = NA_real_))

############################
# Province-level summaries #
############################

dn0002_prov_fn <- function(i) {
  i %>%
    group_by(tinh) %>%
    mutate(tinh = ifelse(tinh == 105, 101, tinh),
           share_f = fworkers/nworkers,
           share_f = ifelse(fworkers < 1, 0, share_f)) %>% 
    filter(nworkers > 0) %>% 
    summarise(
      nworkers = sum(nworkers, na.rm = TRUE),
      fworkers = sum(fworkers, na.rm = TRUE),
      nworkers_eoy = sum(nworkers_eoy, na.rm = TRUE),
      fworkers_eoy = sum(fworkers_eoy, na.rm = TRUE),
      share_f = mean(share_f, na.rm = T)
    ) %>%
    mutate(
      workerratio = (nworkers - fworkers) / fworkers,
      workerratio_eoy = (nworkers_eoy - fworkers_eoy) / fworkers_eoy,
      south = ifelse(tinh > 407, 1, 0)
    ) %>% 
    left_join(province_bmr_sum0003, by = "tinh")
}

dn03_prov_fn <- function(i) {
  i %>%
    group_by(tinh) %>%
    mutate(tinh = ifelse(tinh == 105, 101, tinh),
           share_f = fworkers/nworkers,
           share_f = ifelse(fworkers < 1, 0, share_f)) %>% 
    summarise(
      nworkers = sum(nworkers, na.rm = TRUE),
      fworkers = sum(fworkers, na.rm = TRUE),
      nworkers_eoy = sum(nworkers_eoy, na.rm = TRUE),
      fworkers_eoy = sum(fworkers_eoy, na.rm = TRUE),
      share_f = mean(share_f, na.rm = T)
    ) %>%
    mutate(
      workerratio = (nworkers - fworkers) / fworkers,
      workerratio_eoy = (nworkers_eoy - fworkers_eoy) / fworkers_eoy
    ) %>% 
    left_join(province_bmr_sum, by = "tinh")
}

dn_prov_fn <- function(i) {
  i %>%
    mutate(across(tinh, as.double),
           tinh = ifelse(tinh == 28, 1, tinh)) %>% 
    rename(nworkers = ld11,
           fworkers = ld12,
           nworkers_eoy = ld13,
           fworkers_eoy = ld14) %>% 
    mutate(share_f = fworkers/nworkers,
           share_f = ifelse(fworkers < 1, 0, share_f)) %>% 
    group_by(tinh) %>%
    summarise(
      nworkers = sum(nworkers, na.rm = TRUE),
      fworkers = sum(fworkers, na.rm = TRUE),
      nworkers_eoy = sum(nworkers_eoy, na.rm = TRUE),
      fworkers_eoy = sum(fworkers_eoy, na.rm = TRUE),
      share_f = mean(share_f, na.rm = T)
    ) %>%
    mutate(
      workerratio = (nworkers - fworkers) / fworkers,
      workerratio_eoy = (nworkers_eoy - fworkers_eoy) / fworkers_eoy
    ) %>% 
    left_join(province_bmr_sum0419, by = "tinh")
}

dn1115_prov_fn <- function(i) {
  i %>%
    mutate(across(tinh, as.double),
           tinh = ifelse(tinh == 28, 1, tinh)) %>% 
    rename(ld13 = ld11,
           ld14 = ld12,
           ld11 = tsld,
           ld12 = tsldnu)
}

dn1619_prov_fn <- function(i) {
  i %>%
    mutate(across(tinh, as.double),
           tinh = ifelse(tinh == 28, 1, tinh)) %>% 
    rename(ld13 = ld11,
           ld14 = ld21,
           ld11 = tsld,
           ld12 = tsldnu)
}

dn00_prov <- ec_list[[1]] %>% 
  rename(nworkers = ld611,
         fworkers = ld612,
         nworkers_eoy = ld613,
         fworkers_eoy = ld614) %>% 
  dn0002_prov_fn() %>% 
  mutate(year = 2000)

dn01_prov <- ec_list[[2]] %>% 
  rename(nworkers = ldc11,
         fworkers = ldc12,
         nworkers_eoy = ldc41,
         fworkers_eoy = ldc42) %>% 
  dn0002_prov_fn() %>% 
  mutate(year = 2001)

dn02_prov <- ec_list[[3]] %>% 
  rename(nworkers = ld11,
         fworkers = ld12,
         nworkers_eoy = ld13,
         fworkers_eoy = ld14) %>% 
  dn0002_prov_fn() %>% 
  mutate(year = 2002)

dn03_prov <- ec_list[[4]] %>% 
  rename(nworkers = ld11,
         fworkers = ld12,
         nworkers_eoy = ld13,
         fworkers_eoy = ld14) %>% 
  dn03_prov_fn() %>% 
  mutate(year = 2003)
  
dn04_prov <- dn_prov_fn(ec_list[[5]]) %>% mutate(year = 2004)
dn05_prov <- dn_prov_fn(ec_list[[6]]) %>% mutate(year = 2005)
dn06_prov <- dn_prov_fn(ec_list[[7]]) %>% mutate(year = 2006)
dn07_prov <- dn_prov_fn(ec_list[[8]]) %>% mutate(year = 2007)
dn08_prov <- dn_prov_fn(ec_list[[9]]) %>% mutate(year = 2008)
dn09_prov <- dn_prov_fn(ec_list[[10]]) %>% mutate(year = 2009)
dn10_prov <- dn_prov_fn(ec_list[[11]]) %>% mutate(year = 2010)
dn11_prov <- dn1115_prov_fn(ec_list[[12]]) %>% dn_prov_fn() %>% mutate(year = 2011)
dn12_prov <- dn1115_prov_fn(ec_list[[13]]) %>% dn_prov_fn() %>% mutate(year = 2012)
dn13_prov <- dn1115_prov_fn(ec_list[[14]]) %>% dn_prov_fn() %>% mutate(year = 2013) %>% filter(!is.na(tot_bmr_prov))
dn14_prov <- dn1115_prov_fn(ec_list[[15]]) %>% dn_prov_fn() %>% mutate(year = 2014)
dn15_prov <- dn1115_prov_fn(ec_list[[16]]) %>% dn_prov_fn() %>% mutate(year = 2015)
dn16_prov <- dn1619_prov_fn(ec_list[[17]]) %>% dn_prov_fn() %>% mutate(year = 2016)
dn17_prov <- dn1619_prov_fn(ec_list[[18]]) %>% dn_prov_fn() %>% mutate(year = 2017)
dn18_prov <- dn1619_prov_fn(ec_list[[19]]) %>% dn_prov_fn() %>% mutate(year = 2018)

dn_prov <- bind_rows(dn04_prov, dn05_prov, dn06_prov,
                     dn07_prov, dn08_prov, dn09_prov, dn10_prov, dn11_prov, dn12_prov,
                     dn13_prov, dn14_prov, dn15_prov, dn16_prov, dn17_prov, dn18_prov)
save(dn_prov, file = "dn_prov.Rda")
