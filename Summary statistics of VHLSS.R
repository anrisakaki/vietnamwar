#####################################
# Calculating sex ratio of managers #
#####################################

# Province level ratio of workers and share of women working 

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
    tot_bmr_prov = mean(tot_bmr_prov)
  ) %>% 
  mutate(
    sex_ratio = m_total / f_total,
    workerratio = m_workers / f_workers,
    selfemp_workerratio = mself_emp / fself_emp,
    flfp = f_workers / f_total
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
    tot_bmr_prov = mean(tot_bmr_prov)
  ) %>% 
  mutate(
    sex_ratio = m_total / f_total,
    workerratio = m_workers / f_workers,
    selfemp_workerratio = mself_emp / fself_emp,
    manager_workerratio = m_manager/f_manager,
    flfp = f_workers / f_total
  )

prov06_vhlss <- vhlss06 %>% 
  filter(!is.na(female), age > 15 & age < 65) %>% 
  group_by(tinh) %>% 
  summarise(
    m_total = sum(wt45 * (female == 0), na.rm = T),
    f_total = sum(wt45 * (female == 1), na.rm = T),
    m_workers = sum(wt45 * (female == 0 & work == 1), na.rm = T),
    fself_emp = sum(wt45 * (female == 1 & selfemp == 1), na.rm = T),
    f_workers = sum(wt45 * (female == 1 & work == 1), na.rm = T),
    mself_emp = sum(wt45 * (female == 0 & selfemp == 1), na.rm = T),
    tot_bmr_prov = mean(tot_bmr_prov)
  ) %>% 
  mutate(
    sex_ratio = m_total / f_total,
    workerratio = m_workers / f_workers,
    selfemp_workerratio = mself_emp / fself_emp,
    flfp = f_workers / f_total
  )
