birthcohort_sum <- varhs1_14 %>%
  filter(p2q10_!= 98) %>% 
  # mutate(female = ifelse(p1q3_ == 2, 1, 0)) %>%
  group_by(p1q4_) %>% 
  summarise(educ_mean = mean(p2q10_, na.rm = T)) %>% 
  mutate(educ_mean = round(educ_mean, 2)) %>% 
  filter(p1q4_ < 1988 & p1q4_ > 1945)

birthcohort_sum_LFS <- LFS_2015 %>%
  filter(C4N < 1989 & C4N > 1945) %>% 
  filter(!is.na(C12)) %>% 
  group_by(C4N) %>% 
  summarise(cohort_total = n())

cohort_education_tot <- LFS_2015 %>%
  filter(C4N < 1989 & C4N > 1945) %>% 
  filter(!is.na(C12)) %>% 
  group_by(C4N, C12) %>% 
  summarise(cohort_educ_total = n())

birthcohort_sum_LFS <- merge(cohort_education_tot, birthcohort_sum_LFS, by = "C4N") %>% 
  mutate(share = cohort_educ_total/cohort_total) %>% 
  select(C4N, C12, share) %>% 
  filter(C12 == 5 | C12 == 4 | C12 == 8)

ggplot(birthcohort_sum_LFS, aes(x = C4N, y = (share*100), colour = factor(C12))) +
  geom_line(size = 1.1) +
  labs(
    x = "Birth Cohort",
    y = ""
  ) +
  scale_color_manual(
    values = c("4" = "red", "5" = "blue", "8" = "green"),
    labels = c("Lower Secondary", "Upper Secondary", "University")
  ) +
  # Adding years of American bombing
  geom_rect(
    xmin = 1965, xmax = 1975, ymin = -Inf, ymax = Inf,  
    fill = "gray", alpha = 0.01  
  ) +
  annotate(
    "text", x = 1970, y = 18.5,  # Add text label
    label = "Period of intense American bombing", size = 4
  ) +  
  # End of French rule 
  geom_vline(xintercept = 1954, linetype = "dashed", color = "blue") +  
  geom_text(aes(x = 1954, y = max(20), label = "End of French rule"),
            vjust = -0.5, hjust = 1.3, color = "blue") +  
  # Start of VN War
  geom_vline(xintercept = 1955, linetype = "dashed", color = "red") +  
  geom_text(aes(x = 1955, y = max(20), label = "Start of Vietnam War"),
            vjust = -0.5, hjust = -0.2, color = "red") +  
  # Fall of Saigon 
  geom_vline(xintercept = 1975, linetype = "dashed", color = "red") +  
  geom_text(aes(x = 1975, y = max(20), label = "Fall of Saigon"),
            vjust = -0.5, hjust = 1.3, color = "red") +
  # Reunification of Vietnam 
  geom_vline(xintercept = 1976, linetype = "dashed", color = "dark green") +  
  geom_text(aes(x = 1976, y = max(20), label = "Reunification"),
            vjust = -0.5, hjust = -0.2, color = "dark green") +  
  # Doi Moi reforms 
  geom_vline(xintercept = 1980, linetype = "dashed", color = "dark green") +  
  geom_text(aes(x = 1980, y = max(20), label = "Doi Moi reforms"),
            vjust = -0.5, hjust = -0.2, color = "dark green") +  
  scale_x_continuous(breaks=seq(1946,1987,1)) +  
  theme_minimal() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank()) 

ggplot(birthcohort_sum, aes(x = p1q4_, y = educ_mean)) +
  geom_line(size = 1.1) +
  # End of French rule 
  geom_vline(xintercept = 1954, linetype = "dashed", color = "blue") +  
  geom_text(aes(x = 1954, y = max(10.2), label = "End of French rule"),
            vjust = -0.5, hjust = 1.3, color = "blue") +  
  # Start of VN War
  geom_vline(xintercept = 1955, linetype = "dashed", color = "red") +  
  geom_text(aes(x = 1955, y = max(10.2), label = "Start of Vietnam War"),
            vjust = -0.5, hjust = -0.2, color = "red") +  
  # Fall of Saigon 
  geom_vline(xintercept = 1975, linetype = "dashed", color = "red") +  
  geom_text(aes(x = 1975, y = max(10.2), label = "Fall of Saigon"),
            vjust = -0.5, hjust = 1.3, color = "red") +
  # Reunification of Vietnam 
  geom_vline(xintercept = 1976, linetype = "dashed", color = "dark green") +  
  geom_text(aes(x = 1976, y = max(10.2), label = "Reunification"),
            vjust = -0.5, hjust = -0.2, color = "dark green") +  
  # Doi Moi reforms 
  geom_vline(xintercept = 1980, linetype = "dashed", color = "dark green") +  
  geom_text(aes(x = 1980, y = max(10.2), label = "Doi Moi reforms"),
            vjust = -0.5, hjust = -0.2, color = "dark green") +  
  # Adding years of American bombing
  geom_rect(
    xmin = 1965, xmax = 1975, ymin = -Inf, ymax = Inf,  
    fill = "gray", alpha = 0.01  
  ) +
  annotate(
    "text", x = (1970), y = 9.5,  # Add text label
    label = "Period of intense American bombing", size = 4
  ) +  
  scale_x_continuous(breaks=seq(1946,1987,1)) +
  labs(x = "Birth cohort",
       y = "Avg. years in education") +
  theme_minimal() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank())
