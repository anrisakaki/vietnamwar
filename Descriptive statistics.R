birthcohort_sum_vhlss <- vhlss06 %>%
  group_by(birth_year, female) %>%
  filter(!is.na(educ)) %>% 
  summarise(educ_mean = sum(educ * hhwt) / sum(hhwt),
            std_error = sd(educ) / sqrt(n())) %>% 
  mutate(educ_mean = round(educ_mean, 2)) %>% 
  filter(birth_year < 1988 & birth_year > 1945)

bombs_sum_prov <- vhlss06_bombs %>% 
  group_by(tinh, tot_bmr_prov, tot_bmr_per_prov) %>% 
  filter(!is.na(income),
         !is.na(educ)) %>% 
  filter(war_time == 1) %>% 
  summarise(income_mean_prov = sum(income * hhwt)/ sum(hhwt),
            educ_mean_prov = sum(educ * hhwt)/ sum(hhwt))

bombs_child <- vhlss06_bombs %>% 
  group_by(hhid) %>% 
  mutate(parent_war = ifelse(any(parent == 1 & war_time == 1), 1, 0)) %>% 
  filter(parent_war == 1 & child == 1 & birth_year > 1979 & birth_year < 1989) %>% 
  group_by(tinh) %>%
  summarise(educ_mean_child = sum(educ * hhwt)/ sum(hhwt))

bombs_sum <- hhinc06_bombs %>% 
  group_by(tinh) %>% 
  summarise(hh_inc = sum(tot_hhinc * wt45)/ sum(wt45))

bombs_sum_prov <- list(bombs_sum_prov, bombs_child, bombs_sum) %>% 
  reduce(full_join, by = "tinh")

# Bombing intensity and education/income 

ggplot(bombs_sum_prov, aes(x = log(tot_bmr_per_prov), y = log(hh_inc))) +
  geom_point() +  
  geom_smooth(method = "lm",
              se = F) +
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
       y = "Average Household Income")
ggsave("bombs_hhinc.png", width = 7, height = 7)

ggplot(bombs_sum_prov, aes(x = log(tot_bmr_per_prov), y = log(income_mean_prov))) +
  geom_point() +  
  geom_smooth(method = "lm",
              se = F) +
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
       y = "Average Household Income")
ggsave("bombs_inc.png", width = 7, height = 7)

ggplot(bombs_sum_prov, aes(x = log(tot_bmr_per_prov), y = educ_mean_prov)) +
  geom_point() +  
  geom_smooth(method = "lm",
              se = F) +
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
       y = "Average Education Attainment")

ggplot(bombs_sum_prov, aes(x = log(tot_bmr_per_prov), y = educ_mean_child)) +
  geom_point() +  
  geom_smooth(method = "lm",
              se = F) +
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
       y = "Average Education Attainment of Child")
ggsave("bombs_educ_child.png", width = 7, height = 7)

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
