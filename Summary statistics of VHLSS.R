vhlss <- c("vhlss02.Rda", "vhlss04.Rda", "vhlss06.Rda", "vhlss08.Rda", "vhlss10.Rda", "vhlss12.Rda")

for (i in vhlss) {
  load(i)
}

prov_sum_fn <- function (i) {
  i %>% 
    mutate(
      sex_ratio = m_total / f_total,
      workerratio = m_workers / f_workers,
      selfemp_workerratio = mself_emp / fself_emp,
      flfp = f_workers / f_total,
      wagework_workerratio = mwagework/fwagework
    )
  }

prov02_vhlss <- vhlss02 %>% 
  filter(!is.na(female), age > 15 & age < 65) %>% 
  group_by(tinh) %>% 
  summarise(
    m_total = sum(wt75 * (female == 0), na.rm = T),
    f_total = sum(wt75 * (female == 1), na.rm = T),
    m_workers = sum(wt75 * (female == 0 & work == 1), na.rm = T),
    f_workers = sum(wt75 * (female == 1 & work == 1), na.rm = T),
    fself_emp = sum(wt75 * (female == 1 & selfemp == 1), na.rm = T),
    mself_emp = sum(wt75 * (female == 0 & selfemp == 1), na.rm = T),
    mwagework = sum(wt75 * (female == 0 & wagework == 1), na.rm = T),
    fwagework = sum(wt75 * (female == 1 & wagework == 1), na.rm = T)
  ) %>% 
  left_join(province_bmr_sum, by = "tinh") %>% 
  prov_sum_fn () %>% 
  mutate(south = ifelse(tinh > 407, 1, 0))

prov04_vhlss <- vhlss04 %>% 
  mutate(tinh = ifelse(tinh == 302, 301, tinh),
         tinh = ifelse(tinh == 606, 605, tinh),
         tinh = ifelse(tinh == 816, 815, tinh)) %>% 
  filter(!is.na(female), age > 15 & age < 65) %>% 
  group_by(tinh) %>% 
  summarise(
    m_total = sum(wt45 * (female == 0), na.rm = T),
    f_total = sum(wt45 * (female == 1), na.rm = T),
    m_workers = sum(wt45 * (female == 0 & work == 1), na.rm = T),
    f_workers = sum(wt45 * (female == 1 & work == 1), na.rm = T),
    fself_emp = sum(wt45 * (female == 1 & selfemp == 1), na.rm = T),
    mself_emp = sum(wt45 * (female == 0 & selfemp == 1), na.rm = T),
    mwagework = sum(wt45 * (female == 0 & wagework == 1), na.rm = T),
    fwagework = sum(wt45 * (female == 1 & wagework == 1), na.rm = T)
  ) %>% 
  prov_sum_fn () %>% 
  left_join(province_bmr_sum, by = "tinh") %>% 
  mutate(south = ifelse(tinh > 407, 1, 0))

prov06_vhlss <- vhlss06 %>% 
  mutate(tinh = ifelse(tinh == 302, 301, tinh),
         tinh = ifelse(tinh == 606, 605, tinh),
         tinh = ifelse(tinh == 816, 815, tinh)) %>% 
  filter(!is.na(female), age > 15 & age < 65) %>% 
  group_by(tinh) %>% 
  summarise(
    m_total = sum(wt45 * (female == 0), na.rm = T),
    f_total = sum(wt45 * (female == 1), na.rm = T),
    m_workers = sum(wt45 * (female == 0 & work == 1), na.rm = T),
    f_workers = sum(wt45 * (female == 1 & work == 1), na.rm = T),
    fself_emp = sum(wt45 * (female == 1 & selfemp == 1), na.rm = T),
    mself_emp = sum(wt45 * (female == 0 & selfemp == 1), na.rm = T),
    mwagework = sum(wt45 * (female == 0 & wagework == 1), na.rm = T),
    fwagework = sum(wt45 * (female == 1 & wagework == 1), na.rm = T)
  ) %>% 
  left_join(province_bmr_sum, by = "tinh") %>% 
  prov_sum_fn () %>% 
  mutate(south = ifelse(tinh > 407, 1, 0))

prov08_vhlss <- vhlss08 %>% 
  mutate(tinh = ifelse(tinh == 302, 301, tinh),
         tinh = ifelse(tinh == 606, 605, tinh),
         tinh = ifelse(tinh == 816, 815, tinh)) %>% 
  filter(!is.na(female), age > 15 & age < 65) %>% 
  group_by(tinh) %>% 
  summarise(
    m_total = sum(wt9 * (female == 0), na.rm = T),
    f_total = sum(wt9 * (female == 1), na.rm = T),
    m_workers = sum(wt9 * (female == 0 & work == 1), na.rm = T),
    f_workers = sum(wt9 * (female == 1 & work == 1), na.rm = T),
    fself_emp = sum(wt9 * (female == 1 & selfemp == 1), na.rm = T),
    mself_emp = sum(wt9 * (female == 0 & selfemp == 1), na.rm = T),
    mwagework = sum(wt9 * (female == 0 & wagework == 1), na.rm = T),
    fwagework = sum(wt9 * (female == 1 & wagework == 1), na.rm = T)
  ) %>% 
  left_join(province_bmr_sum, by = "tinh") %>% 
  prov_sum_fn() %>% 
  mutate(south = ifelse(tinh > 407, 1, 0))

prov10_vhlss <- vhlss10 %>% 
  mutate(tinh = ifelse(tinh == 67, 66, tinh),
         tinh = ifelse(tinh == 11, 12, tinh),
         tinh = ifelse(tinh == 93, 92, tinh)) %>% 
  filter(!is.na(female), age > 15 & age < 65) %>% 
  group_by(tinh) %>% 
  summarise(
    m_total = sum(wt9 * (female == 0), na.rm = T),
    f_total = sum(wt9 * (female == 1), na.rm = T),
    m_workers = sum(wt9 * (female == 0 & work == 1), na.rm = T),
    fself_emp = sum(wt9 * (female == 1 & selfemp == 1), na.rm = T),
    f_workers = sum(wt9 * (female == 1 & work == 1), na.rm = T),
    mself_emp = sum(wt9 * (female == 0 & selfemp == 1), na.rm = T),
    mwagework = sum(wt9 * (female == 0 & wagework == 1), na.rm = T),
    fwagework = sum(wt9 * (female == 1 & wagework == 1), na.rm = T)
  ) %>% 
  left_join(province_bmr_sum2, by = "tinh") %>% 
  prov_sum_fn() %>% 
  mutate(south = ifelse(tinh > 44, 1, 0))  

prov12_vhlss <- vhlss12 %>% 
  mutate(tinh = ifelse(tinh == 67, 66, tinh),
         tinh = ifelse(tinh == 11, 12, tinh),
         tinh = ifelse(tinh == 93, 92, tinh)) %>% 
  filter(!is.na(female), age > 15 & age < 65) %>% 
  group_by(tinh) %>% 
  summarise(
    m_total = sum(wt9 * (female == 0), na.rm = T),
    f_total = sum(wt9 * (female == 1), na.rm = T),
    m_workers = sum(wt9 * (female == 0 & work == 1), na.rm = T),
    fself_emp = sum(wt9 * (female == 1 & selfemp == 1), na.rm = T),
    f_workers = sum(wt9 * (female == 1 & work == 1), na.rm = T),
    mself_emp = sum(wt9 * (female == 0 & selfemp == 1), na.rm = T),
    mwagework = sum(wt9 * (female == 0 & wagework == 1), na.rm = T),
    fwagework = sum(wt9 * (female == 1 & wagework == 1), na.rm = T)
  ) %>% 
  left_join(province_bmr_sum2, by = "tinh") %>% 
  prov_sum_fn() %>% 
  mutate(south = ifelse(tinh > 44, 1, 0)) 

save(prov02_vhlss, file = "prov02_vhlss.Rda")
save(prov04_vhlss, file = "prov04_vhlss.Rda")
save(prov06_vhlss, file = "prov06_vhlss.Rda")
save(prov08_vhlss, file = "prov08_vhlss.Rda")
save(prov10_vhlss, file = "prov10_vhlss.Rda")
save(prov12_vhlss, file = "prov12_vhlss.Rda")