#####################################
# Calculating sex ratio of managers #
#####################################

# District level ratio of workers and share of women working 

dist02 <- vhlss02 %>% 
  filter(age > 15 & age < 65) %>% 
  group_by(tinh02, huyen02) %>% 
  summarise(m_total = sum(female == 0, na.rm = TRUE, weights = wt75),
            f_total = sum(female == 1, na.rm = TRUE, weights = wt75),
            m_workers = sum(female == 0 & work == 1, na.rm = TRUE, weights = wt75),
            f_workers = sum(female == 1 & work == 1, na.rm = TRUE, weights = wt75),
            m_selfemp = sum(female == 0 & selfemp == 1, na.rm = TRUE, weights = wt75),
            f_selfemp = sum(female == 1 & selfemp == 1, na.rm = TRUE, weights = wt75)) %>% 
  mutate(flfp = f_workers/f_total,
         workerratio = m_workers/f_workers,
         selfemp_workerratio = m_selfemp/f_selfemp)

