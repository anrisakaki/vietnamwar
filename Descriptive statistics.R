# Bombing intensity versus 

ggplot(dplyr::filter(sexratio_prov79, Tinh79!= "Cao Bang"), aes(x = log_tot_bmr_per, y = sexratio*100)) +
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
       y = "Province-level sex ratio in 1979")
ggsave("bmr_sexratio89.jpeg", width = 7, height = 7)

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

ggplot(sexratio_prov_89, aes(x = log_tot_bmr_per, y = widowed_f*100)) +
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
       y = "Province-level share of widowed women in 1989")
ggsave("bmr_widowed_f89.jpeg", width = 7, height = 7)

ggplot(sexratio_prov_89, aes(x = log_tot_bmr_per, y = work_f*100)) +
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
       y = "Province-level FLFP in 1989")
ggsave("bmr_work_f89.jpeg", width = 7, height = 7)

# Sex ratio versus 

ggplot(sexratio_prov_89, aes(x = sexratio*100, y = work_f*100)) +
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
  labs(x = "Province-level sex ratio in 1989",
       y = "Province-level FLFP in 1989")
ggsave("sexratio_work_f89.jpeg", width = 7, height = 7)

# Birth cohort and labour force participation rate by age cohort
s
ggplot(dplyr::filter(agecohort_sum, female == 1 & year == 1989), aes(x = as.factor(age_cohort), y = work*100)) +
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

ggplot(dplyr::filter(agecohort_sum, female == 1 & year == 1999), aes(x = as.factor(age_cohort), y = work*100)) +
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

ggplot(dplyr::filter(agecohort_sum, female == 1 & year == 2009), aes(x = as.factor(age_cohort), y = work*100)) +
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
ggplot(dplyr::filter(agecohort75_sum, female == 1 & year == 1989), aes(x = as.factor(age_cohort75), y = work*100)) +
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

ggplot(dplyr::filter(agecohort75_sum, female == 1 & year == 1999), aes(x = as.factor(age_cohort75), y = work*100)) +
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

ggplot(dplyr::filter(agecohort75_sum, female == 1 & year == 2009), aes(x = as.factor(age_cohort75), y = work*100)) +
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
