#####################################
# Calculating sex ratio of managers #
#####################################

# District level ratio of workers and share of women working 

dist02_vhlss <- vhlss02 %>% 
  filter(!is.na(female), age > 15 & age < 65) %>% 
  group_by(tinh02, huyen02) %>% 
  summarise(
    m_total = sum(wt75 * (female == 0), na.rm = T),
    f_total = sum(wt75 * (female == 1), na.rm = T),
    m_workers = sum(wt75 * (female == 0 & work == 1), na.rm = T),
    f_workers = sum(wt75 * (female == 1 & work == 1), na.rm = T),
    fself_emp = sum(wt75 * (female == 1 & selfemp == 1), na.rm = T),
    mself_emp = sum(wt75 * (female == 0 & selfemp == 1), na.rm = T)
  ) %>% 
  mutate(
    sex_ratio = m_total / f_total,
    workerratio = m_workers / f_workers,
    selfemp_workerratio = mself_emp / fself_emp,
    flfp = f_workers / f_total
  )
  

dist04_vhlss <- vhlss04 %>% 
  filter(!is.na(female), age > 15 & age < 65) %>% 
  group_by(tinh, huyen) %>% 
  summarise(
    m_total = sum(wt45 * (female == 0), na.rm = T),
    f_total = sum(wt45 * (female == 1), na.rm = T),
    m_workers = sum(wt45 * (female == 0 & work == 1), na.rm = T),
    f_workers = sum(wt45 * (female == 1 & work == 1), na.rm = T),
    fself_emp = sum(wt45 * (female == 1 & selfemp == 1), na.rm = T),
    mself_emp = sum(wt45 * (female == 0 & selfemp == 1), na.rm = T)
  ) %>% 
  mutate(
    sex_ratio = m_total / f_total,
    workerratio = m_workers / f_workers,
    selfemp_workerratio = mself_emp / fself_emp,
    flfp = f_workers / f_total
  )

dist06_vhlss <- vhlss06 %>% 
  filter(!is.na(female), age > 15 & age < 65) %>% 
  group_by(tinh, huyen) %>% 
  summarise(
    m_total = sum(wt45 * (female == 0), na.rm = T),
    f_total = sum(wt45 * (female == 1), na.rm = T),
    m_workers = sum(wt45 * (female == 0 & work == 1), na.rm = T),
    fself_emp = sum(wt45 * (female == 1 & selfemp == 1), na.rm = T),
    f_workers = sum(wt45 * (female == 1 & work == 1), na.rm = T),
    mself_emp = sum(wt45 * (female == 0 & selfemp == 1), na.rm = T)
  ) %>% 
  mutate(
    sex_ratio = m_total / f_total,
    workerratio = m_workers / f_workers,
    selfemp_workerratio = mself_emp / fself_emp,
    flfp = f_workers / f_total
  )

