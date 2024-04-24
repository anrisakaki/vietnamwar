vhlss <- c("vhlss02.Rda", "vhlss04.Rda", "vhlss06.Rda", "vhlss08.Rda", "vhlss10.Rda", "vhlss12.Rda")

for (i in vhlss) {
  load(i)
}

####################################
# Calculating sex ratio of workers #
####################################

prov02_vhlss <- vhlss02 %>% 
  filter(!is.na(female), age > 15 & age < 65) %>% 
  group_by(tinh02) %>% 
  summarise(
    m_total = sum(wt75 * (female == 0), na.rm = T),
    f_total = sum(wt75 * (female == 1), na.rm = T),
    m_workers = sum(wt75 * (female == 0 & work == 1), na.rm = T),
    f_workers = sum(wt75 * (female == 1 & work == 1), na.rm = T),
    fself_emp = sum(wt75 * (female == 1 & selfemp == 1), na.rm = T),
    mself_emp = sum(wt75 * (female == 0 & selfemp == 1), na.rm = T),
    tot_bmr_prov = mean(tot_bmr_prov),
    tot_bmr_lb_prov = mean(tot_bmr_lb_prov),
    killed_tot_prov = mean(killed_tot_prov),
    dist_nearest_base_prov = mean(dist_nearest_base_prov),
    dist_nearest_hochi_prov = mean(dist_nearest_hochi_prov),
  ) %>% 
  mutate(
    sex_ratio = m_total / f_total,
    workerratio = m_workers / f_workers,
    selfemp_workerratio = mself_emp / fself_emp,
    flfp = f_workers / f_total,
    south = ifelse(tinh02 > 407, 1, 0)
  )

prov04_vhlss <- vhlss04 %>% 
  mutate(m_manager = ifelse(f_manager == 0, 1, 0)) %>% 
  filter(!is.na(female), age > 15 & age < 65) %>% 
  group_by(tinh) %>% 
  summarise(
    m_total = sum(wt45 * (female == 0), na.rm = T),
    f_total = sum(wt45 * (female == 1), na.rm = T),
    m_workers = sum(wt45 * (female == 0 & work == 1), na.rm = T),
    f_workers = sum(wt45 * (female == 1 & work == 1), na.rm = T),
    fself_emp = sum(wt45 * (female == 1 & selfemp == 1), na.rm = T),
    mself_emp = sum(wt45 * (female == 0 & selfemp == 1), na.rm = T),
    f_manager = sum(wt45 * (f_manager == 1), na.rm = T),
    m_manager = sum(wt45 * (m_manager == 1), na.rm = T),
    tot_bmr_prov = mean(tot_bmr_prov),
    tot_bmr_lb_prov = mean(tot_bmr_lb_prov),
    killed_tot_prov = mean(killed_tot_prov)
  ) %>% 
  mutate(
    sex_ratio = m_total / f_total,
    workerratio = m_workers / f_workers,
    selfemp_workerratio = mself_emp / fself_emp,
    manager_workerratio = m_manager/f_manager,
    flfp = f_workers / f_total,
    south = ifelse(tinh > 407, 1, 0)
  )

prov06_vhlss <- vhlss06 %>% 
  mutate(m_manager = ifelse(f_manager == 0, 1, 0)) %>% 
  filter(!is.na(female), age > 15 & age < 65) %>% 
  group_by(tinh) %>% 
  summarise(
    m_total = sum(wt45 * (female == 0), na.rm = T),
    f_total = sum(wt45 * (female == 1), na.rm = T),
    m_workers = sum(wt45 * (female == 0 & work == 1), na.rm = T),
    fself_emp = sum(wt45 * (female == 1 & selfemp == 1), na.rm = T),
    f_workers = sum(wt45 * (female == 1 & work == 1), na.rm = T),
    mself_emp = sum(wt45 * (female == 0 & selfemp == 1), na.rm = T),
    f_manager = sum(wt45 * (f_manager == 1), na.rm = T),
    m_manager = sum(wt45 * (m_manager == 1), na.rm = T),
    tot_bmr_prov = mean(tot_bmr_prov),
    tot_bmr_lb_prov = mean(tot_bmr_lb_prov),
    killed_tot_prov = mean(killed_tot_prov)
  ) %>% 
  mutate(
    sex_ratio = m_total / f_total,
    workerratio = m_workers / f_workers,
    selfemp_workerratio = mself_emp / fself_emp,
    manager_workerratio = m_manager/f_manager,
    flfp = f_workers / f_total,
    south = ifelse(tinh > 407, 1, 0)
  )

prov08_vhlss <- vhlss08 %>% 
  mutate(m_manager = ifelse(f_manager == 0, 1, 0)) %>% 
  filter(!is.na(female), age > 15 & age < 65) %>% 
  group_by(tinh) %>% 
  summarise(
    m_total = sum(wt9 * (female == 0), na.rm = T),
    f_total = sum(wt9 * (female == 1), na.rm = T),
    m_workers = sum(wt9 * (female == 0 & work == 1), na.rm = T),
    fself_emp = sum(wt9 * (female == 1 & selfemp == 1), na.rm = T),
    f_workers = sum(wt9 * (female == 1 & work == 1), na.rm = T),
    mself_emp = sum(wt9 * (female == 0 & selfemp == 1), na.rm = T),
    f_manager = sum(wt9 * (f_manager == 1), na.rm = T),
    m_manager = sum(wt9 * (m_manager == 1), na.rm = T),
    tot_bmr_prov = mean(tot_bmr_prov),
    tot_bmr_lb_prov = mean(tot_bmr_lb_prov),
    killed_tot_prov = mean(killed_tot_prov)
  ) %>% 
  mutate(
    sex_ratio = m_total / f_total,
    workerratio = m_workers / f_workers,
    selfemp_workerratio = mself_emp / fself_emp,
    manager_workerratio = m_manager/f_manager,
    flfp = f_workers / f_total,
    south = ifelse(tinh > 407, 1, 0)
  )

prov10_vhlss <- vhlss10 %>% 
  filter(!is.na(female), age > 15 & age < 65) %>% 
  group_by(tinh) %>% 
  summarise(
    m_total = sum(wt9 * (female == 0), na.rm = T),
    f_total = sum(wt9 * (female == 1), na.rm = T),
    m_workers = sum(wt9 * (female == 0 & work == 1), na.rm = T),
    fself_emp = sum(wt9 * (female == 1 & selfemp == 1), na.rm = T),
    f_workers = sum(wt9 * (female == 1 & work == 1), na.rm = T),
    mself_emp = sum(wt9 * (female == 0 & selfemp == 1), na.rm = T),
    tot_bmr_prov = mean(tot_bmr_prov),
    tot_bmr_lb_prov = mean(tot_bmr_lb_prov),
    killed_tot_prov = mean(killed_tot_prov)
  ) %>% 
  mutate(
    sex_ratio = m_total / f_total,
    workerratio = m_workers / f_workers,
    selfemp_workerratio = mself_emp / fself_emp,
    flfp = f_workers / f_total,
    south = ifelse(tinh > 44, 1, 0)
  )

prov12_vhlss <- vhlss12 %>% 
  filter(!is.na(female), age > 15 & age < 65) %>% 
  group_by(tinh) %>% 
  summarise(
    m_total = sum(wt9 * (female == 0), na.rm = T),
    f_total = sum(wt9 * (female == 1), na.rm = T),
    m_workers = sum(wt9 * (female == 0 & work == 1), na.rm = T),
    fself_emp = sum(wt9 * (female == 1 & selfemp == 1), na.rm = T),
    f_workers = sum(wt9 * (female == 1 & work == 1), na.rm = T),
    mself_emp = sum(wt9 * (female == 0 & selfemp == 1), na.rm = T),
    tot_bmr_prov = mean(tot_bmr_prov),
    tot_bmr_lb_prov = mean(tot_bmr_lb_prov),
    killed_tot_prov = mean(killed_tot_prov)
  ) %>% 
  mutate(
    sex_ratio = m_total / f_total,
    workerratio = m_workers / f_workers,
    selfemp_workerratio = mself_emp / fself_emp,
    flfp = f_workers / f_total,
    south = ifelse(tinh > 44, 1, 0)
  )

save(prov02_vhlss, file = "prov02_vhlss.Rda")
save(prov04_vhlss, file = "prov04_vhlss.Rda")
save(prov06_vhlss, file = "prov06_vhlss.Rda")
save(prov08_vhlss, file = "prov08_vhlss.Rda")
save(prov10_vhlss, file = "prov10_vhlss.Rda")
save(prov12_vhlss, file = "prov12_vhlss.Rda")