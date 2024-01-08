# Calculating the sex ratio and LFP of men and women by age cohort in 1989 

prov_89_f <- phc89 %>% 
  filter(female == 1) %>% 
  group_by(geo1_vn) %>% 
  summarise(total_f = sum(perwt),
            log_tot_bomb = mean(log_tot_bomb),
            log_tot_bmr_per = mean(log_tot_bmr_per))

prov_89_m <- phc89 %>% 
  filter(female == 0) %>% 
  group_by(geo1_vn) %>% 
  summarise(total_m = sum(perwt))

sexratio_prov_89 <- merge(prov_89_f, prov_89_m, by = "geo1_vn") %>% 
  mutate(sexratio = total_m/total_f)

# Calculating the sex ratio and LFP of men and women by age cohort in 1999

agecohort_work <- phc %>% 
  group_by(year, age_cohort, female) %>%
  summarise(work = sum(work * perwt) / sum(perwt)) %>% 
  filter(!is.na(age_cohort),
         !is.na(age_cohort75))

agecohort75_work <- phc %>% 
  group_by(year, age_cohort75, female) %>%
  summarise(work = sum(work * perwt) / sum(perwt)) %>% 
  filter(!is.na(age_cohort75))

prov_ppn5776 <- prov_ppn5780 %>% 
  select(-c(Total_ppn, Male_ppn)) %>% 
  filter(!is.na(mshare),
         Year == 1957 | Year == 1976) %>% 
  pivot_wider(names_from = Year, values_from = mshare) %>% 
  rename(Y1957 = 2,
         Y1976 = 3) %>% 
  mutate(change = Y1957-Y1976) %>% 
  filter(!is.na(change))

prov_ppn5776 <- left_join(prov_ppn5776, prov7606, by = "Tinh76")
prov_ppn5776 <- inner_join(prov_ppn5776, bombs_prov, by = c("Tinh06" = "provname2002")) %>% select(tinh, change)

bombs_sum_prov <- left_join(bombs_sum_prov, prov_ppn5776, by = "tinh")

# Bombing intensity versus sex ratio 

ggplot(sexratio_prov_89, aes(x = log_tot_bmr_per, y = sexratio*100)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = T) +
  theme_minimal() +
  guides(fill = "none") +  
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        text = element_text(size=10)) + 
  labs(x = expression(log(Bombs~per~Km^2)),
       y = "Province-level sex ratio in 1989")
ggsave("bmr_sexratio89.jpeg", width = 7, height = 7)

# Birth cohort and labour force participation rate by age cohort 
ggplot(dplyr::filter(agecohort_work, female == 1 & year == 1989), aes(x = as.factor(age_cohort), y = work*100)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + 
  labs(x = "Age cohort",
       y = "FLFP rate in 1989") +
  # scale_x_continuous(breaks=seq(1925,1973,1)) +  
  theme_minimal() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank())  
ggsave("flfp89_agecohort.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(agecohort_work, female == 1 & year == 1999), aes(x = as.factor(age_cohort), y = work*100)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + 
  labs(x = "Age cohort",
       y = "FLFP rate in 1999") +
  # scale_x_continuous(breaks=seq(1925,1973,1)) +  
  theme_minimal() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank())  
ggsave("flfp99_agecohort.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(agecohort_work, female == 1 & year == 2009), aes(x = as.factor(age_cohort), y = work*100)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + 
  labs(x = "Age cohort",
       y = "FLFP rate in 2009") +
  # scale_x_continuous(breaks=seq(1925,1973,1)) +  
  theme_minimal() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank())  
ggsave("flfp09_agecohort.jpeg", width = 7, height = 7)

# Birth cohort and labour force participation rate by age cohort 
ggplot(dplyr::filter(agecohort75_work, female == 1 & year == 1989), aes(x = as.factor(age_cohort75), y = work*100)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + 
  labs(x = "Age cohort in 1975",
       y = "FLFP rate in 1989") +
  # scale_x_continuous(breaks=seq(1925,1973,1)) +  
  theme_minimal() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank())  
ggsave("flfp89_agecohort75.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(agecohort75_work, female == 1 & year == 1999), aes(x = as.factor(age_cohort75), y = work*100)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + 
  labs(x = "Age cohort in 1975",
       y = "FLFP rate in 1999") +
  # scale_x_continuous(breaks=seq(1925,1973,1)) +  
  theme_minimal() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank())  
ggsave("flfp99_agecohort75.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(agecohort75_work, female == 1 & year == 2009), aes(x = as.factor(age_cohort75), y = work*100)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + 
  labs(x = "Age cohort in 1975",
       y = "FLFP rate in 2009") +
  theme_minimal() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank()) 
ggsave("flfp09_agecohort75.jpeg", width = 7, height = 7)

# Birth cohort and education levels 

ggplot(birthcohort_sum_vhlss, aes(x = birth_year, y = educ_mean, colour = as.factor(female), group = as.factor(female))) +
  geom_line(size = 1.1) +
  # End of French rule 
  geom_vline(xintercept = 1954, linetype = "dashed", color = "blue") +  
  geom_text(aes(x = 1954, y = max(9.25), label = "End of French rule"),
            vjust = -0.5, hjust = 1.3, color = "blue") +  
  # Start of VN War
  geom_vline(xintercept = 1955, linetype = "dashed", color = "red") +  
  geom_text(aes(x = 1955, y = max(9.25), label = "Start of Vietnam War"),
            vjust = -0.5, hjust = -0.2, color = "red") +  
  # Fall of Saigon 
  geom_vline(xintercept = 1975, linetype = "dashed", color = "red") +  
  geom_text(aes(x = 1975, y = max(9.25), label = "Fall of Saigon"),
            vjust = -0.5, hjust = 1.3, color = "red") +
  # Reunification of Vietnam 
  geom_vline(xintercept = 1976, linetype = "dashed", color = "dark green") +  
  geom_text(aes(x = 1976, y = max(9.25), label = "Reunification"),
            vjust = -0.5, hjust = -0.2, color = "dark green") +  
  # Doi Moi reforms 
  geom_vline(xintercept = 1980, linetype = "dashed", color = "dark green") +  
  geom_text(aes(x = 1980, y = max(9.25), label = "Doi Moi reforms"),
            vjust = -0.5, hjust = -0.2, color = "dark green") +  
  # Adding years of American bombing
  geom_rect(
    xmin = 1965, xmax = 1975, ymin = -Inf, ymax = Inf,  
    fill = "gray", alpha = 0.01  
  ) +
  annotate(
    "text", x = (1970), y = 8.5,  # Add text label
    label = "Period of intense American bombing", size = 4
  ) +  
  scale_x_continuous(breaks=seq(1946,1987,1)) +
  labs(x = "Birth cohort",
       y = "") +
  theme_minimal() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank())
ggsave("vn_educ_birthcohort.jpeg", width = 17, height = 7)
