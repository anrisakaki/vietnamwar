sex_ratios <- c("sexratio_prov_89.Rda", "sexratio_prov_99.Rda", "sexratio_prov_09.Rda")

for (i in sex_ratios) {
  load(i)
}

# Map of bombing intensity 

ggplot(province_bmr_sf) + 
  geom_sf(aes(fill = log(tot_bmr))) +
  scale_fill_gradient(name = "log(Total Bombs, \nMissiles and Rockets)", low = "green", high = "red", na.value = "white") + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        panel.background = element_blank()) +
  ggtitle("")
ggsave("province_bombs_sf.jpeg", width = 7, height = 7)

ggplot(province_bmr_sf) + 
  geom_sf(aes(fill = log(killed_tot))) +
  scale_fill_gradient(name = "log(Casualties)", low = "green", high = "red", na.value = "grey") + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        panel.background = element_blank()) +
  ggtitle("")
ggsave("province_casualties_sf.jpeg", width = 7, height = 7)

# Overall BMR and casualties 

ggplot(dplyr::filter(sexratio_prov_09, tot_casualties_per > 0), aes(x = log(tot_bomb), y = log(tot_killed))) +
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
  labs(x = "log(Total Bombs, Missiles and Rockets)",
       y = "log(Casualties)")
ggsave("bombs_casualties.jpeg", width = 7, height = 7)

# 1976

ggplot(oldprov_sexratio, aes(x = log(tot_bmr), y = sexratio)) +
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
  labs(x = "log(Bombs, Missiles and Rockets)",
       y = "Sex Ratio in 1976")
ggsave("bmr_sexratio76.jpeg", width = 7, height = 7)

# 1989 
bc_sexratio89_long <- bc_sexratio89 %>%
  pivot_longer(cols = c(sex_ratio_south, sex_ratio_north),
               names_to = "Region", values_to = "SexRatio")

ggplot(bc_sexratio89_long, aes(x = age, y = (SexRatio)*100, color = Region)) +
  geom_line() +
  geom_line(size = 1.1) +
  geom_vline(xintercept = c(30, 64), linetype = "dashed") +  
  geom_rect(
    xmin = 30, xmax = 64, ymin = -Inf, ymax = Inf,  
    fill = "gray", alpha = 0.01  
  ) +
  annotate(
    "text", x = (47), y = 103,  # Add text label
    label = "30-54", size = 3
  ) +  
  labs(x = "Age Cohort",
       y = "Sex Ratio",
       color = "Region") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", color = NA),  
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank()) +
  scale_color_discrete(labels = c("North", "South"))
ggsave("sexratio_birthcohort.jpeg", width = 17, height = 7)

## Sex ratio 

ggplot(sexratio_prov_89, aes(x = log(tot_bomb), y = sexratio*100)) +
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
  labs(x = "log(Bombs, Missiles and Rockets)",
       y = "Sex Ratio in 1989")
ggsave("bmr_sexratio89.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(sexratio_prov_89, south == 0), aes(x = log(tot_bomb), y = sexratio*100)) +
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
  labs(x = "log(Bombs, Missiles and Rockets)",
       y = "Sex Ratio in 1989 (North)")

ggplot(dplyr::filter(sexratio_prov_89, south == 1), aes(x = log(tot_bomb), y = sexratio*100)) +
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
  labs(x = "log(Bombs, Missiles and Rockets)",
       y = "Sex Ratio in 1989 (South)")

# Sex ratio of age cohort 30-64 

ggplot(sexratio_prov_89, aes(x = log(tot_bomb), y = sexratio3064*100)) +
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
  labs(x = "log(Bombs, Missiles and Rockets)",
       y = "30-64 Age Cohort Sex Ratio in 1989")
ggsave("bmr_sexratio3064_89.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(sexratio_prov_89, south == 0), aes(x = log(tot_bomb), y = sexratio3064*100)) +
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
  labs(x = "log(Bombs, Missiles and Rockets)",
       y = "30-64 Age Cohort Sex Ratio in 1989 (North)")
ggsave("bmr_sexratio3064_89_n.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(sexratio_prov_89, south == 1), aes(x = log(tot_bomb), y = sexratio3064*100)) +
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
  labs(x = "log(Bombs, Missiles and Rockets)",
       y = "30-64 Age Cohort Sex Ratio in 1989 (South)")
ggsave("bmr_sexratio3064_89_s.jpeg", width = 7, height = 7)

# Ratio of male to female workers  

ggplot(dplyr::filter(sexratio_prov_89, south == 0), aes(x = log(tot_bomb), y = workratio*100)) +
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
  labs(x = "log(Bombs, Missiles and Rockets)",
       y = "Ratio of Male to Female Workers in 1989 (North)")
ggsave("bmr_workerratio89_n.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(sexratio_prov_89, south == 1), aes(x = log(tot_bomb), y = workratio*100)) +
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
  labs(x = "log(Bombs, Missiles and Rockets)",
       y = "Ratio of Male to Female Workers in 1989 (South)")
ggsave("bmr_workerratio89_s.jpeg", width = 7, height = 7)

# Ratio of male to female workers aged 30-64 

ggplot(sexratio_prov_89, aes(x = log(tot_bomb), y = workratio3064*100)) +
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
  labs(x = "log(Bombs, Missiles and Rockets)",
       y = "Ratio of Male to Female Workers Aged 30-64 in 1989")
ggsave("bmr_workerratio_3064_89.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(sexratio_prov_89, south == 0), aes(x = log(tot_bomb), y = workratio3064*100)) +
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
  labs(x = "log(Bombs, Missiles and Rockets)",
       y = "Ratio of Male to Female Workers Aged 30-64 in 1989 (North)")
ggsave("bmr_workerratio_3064_89_n.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(sexratio_prov_89, south == 1), aes(x = log(tot_bomb), y = workratio3064*100)) +
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
  labs(x = "log(Bombs, Missiles and Rockets)",
       y = "Ratio of Male to Female Workers Aged 30-64 in 1989 (South)")
ggsave("bmr_workerratio_3064_89_s.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(sexratio_prov_89, south == 1), aes(x = log(tot_bomb), y = workratio3064*100)) +
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
  labs(x = "log(Bombs, Missiles and Rockets)",
       y = "Ratio of Male to Female Workers Aged 30-64 in 1989 (South)")
ggsave("bmr_workerratio_3064_89_s.jpeg", width = 7, height = 7)

# Share of widows 

ggplot(sexratio_prov_89, aes(x = log(tot_bomb), y = widowed_f*100)) +
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
  labs(x = "log(Bombs, Missiles and Rockets)",
       y = "Share of Widowed Women in 1989")
ggsave("bmr_widowed_f89.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(sexratio_prov_89, south == 0), aes(x = log(tot_bomb), y = widowed_f*100)) +
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
  labs(x = "log(Bombs, Missiles and Rockets)",
       y = "Share of Widowed Women in 1989 (North)")
ggsave("bmr_widowed_f89_n.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(sexratio_prov_89, south == 1), aes(x = log(tot_bomb), y = widowed_f*100)) +
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
  labs(x = "log(Bombs, Missiles and Rockets)",
       y = "Share of Widowed Women in 1989 (South)")
ggsave("bmr_widowed_f89_s.jpeg", width = 7, height = 7)

# Bombs vs industry male to female ratio 
ggplot(dplyr::filter(indgen_prov_sum, Industry == "Manufacturing"), aes(x = log(tot_bomb), y = workerratio*100)) +
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
  labs(x = "log(Bombs, Missiles and Rockets)",
       y = "Ratio of Male to Female Workers \nin Manufacturing in 1989")
ggsave("bmr_manu_89.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(indgen_prov_sum, Industry == "Manufacturing" & south == 0), aes(x = log(tot_bomb), y = workerratio*100)) +
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
  labs(x = "log(Bombs, Missiles and Rockets)",
       y = "Ratio of Male to Female Workers \nin Manufacturing in 1989 (North)")
ggsave("bmr_manu_89_n.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(indgen_prov_sum, Industry == "Manufacturing" & south == 1), aes(x = log(tot_bomb), y = workerratio*100)) +
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
  labs(x = "log(Bombs, Missiles and Rockets)",
       y = "Ratio of Male to Female Workers \nin Manufacturing in 1989 (South)")
ggsave("bmr_manu_89_s.jpeg", width = 7, height = 7)

# Widowhood vs work ratio 

ggplot(sexratio_prov_89, aes(x = widowed_f*100, y = workratio*100)) +
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
  labs(x = "Share of Widowed Women Agedin 1989",
       y = "Ratio of Male to Female Workersin 1989")
ggsave("widowed3064_workerratio3064_89.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(sexratio_prov_89, south == 0), aes(x = age3064_widowed_f*100, y = workratio3064*100)) +
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
  labs(x = "Share of Widowed Women Aged 30-64 in 1989",
       y = "Ratio of Male to Female Workers Aged 30-64 in 1989 (North)")
ggsave("widowed3064_workerratio3064_89_n.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(sexratio_prov_89, south == 1), aes(x = age3064_widowed_f*100, y = workratio3064*100)) +
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
  labs(x = "Share of Widowed Women Aged 30-64 in 1989",
       y = "Ratio of Male to Female Workers Aged 30-64 in 1989 (South)")
ggsave("widowed3064_workerratio3064_89_s.jpeg", width = 7, height = 7)

# Female and male labour force composition 
ggplot(dplyr::filter(indgen_sum, year == 1989 & indgen < 999), aes(x = (f_comp), y = reorder(Industry, f_comp))) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(f_comp, 2)), hjust = -0.2, size = 3) +
  labs(x = "Share of Female Labour Force in 1989 (%)",
       y = "Industry") +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        text = element_text(size=15)) + 
  scale_x_continuous(breaks = NULL) 
ggsave("fcomp_89.jpeg", width = 12, height = 12)

ggplot(dplyr::filter(indgen_sum, year == 1989 & indgen < 999), aes(x = (m_comp), y = reorder(Industry, m_comp))) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(m_comp, 2)), hjust = -0.2, size = 3) +
  labs(x = "Share of Male Labour Force in 1989 (%)",
       y = "Industry") +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        text = element_text(size=15)) + 
  scale_x_continuous(breaks = NULL) 
ggsave("mcomp_89.jpeg", width = 12, height = 12)

# Male to female ratio in each industry 

ggplot(dplyr::filter(indgen_sum, year == 1989 & indgen < 999), aes(x = (workerratio)*100, y = reorder(Industry, workerratio))) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(workerratio*100, 2)), hjust = -0.2, size = 3) +
  labs(x = "Ratio of Male to Female Workers in 1989",
       y = "Industry") +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        text = element_text(size=15)) + 
  scale_x_continuous(breaks = NULL) 
ggsave("indgen_sum_89.jpeg", width = 12, height = 12)

## By north/south 



# Male to female ratio in each occupation 

ggplot(dplyr::filter(occisco_sum, year == 1999 & occisco < 99), aes(x = (workerratio)*100, y = reorder(Occupation, workerratio))) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(workerratio*100, 2)), hjust = -0.2, size = 3) +
  labs(x = "Ratio of Male to Female Workers in 1999",
       y = "Occupation") +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        text = element_text(size=15)) + 
  scale_x_continuous(breaks = NULL) 
ggsave("occisco_sum_99.jpeg", width = 12, height = 12)

# Birth cohort and labour force participation rate by age cohort

ggplot(dplyr::filter(agecohort_sum,  year == 1989 &
                       !(age_cohort == "5-9" | age_cohort == "0-4" | age_cohort == "10-14" | age_cohort == "65+")),
       aes(x = as.factor(age_cohort), y = flfp, fill = group89)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + 
  labs(x = "Age cohort",
       y = "FLFP rate in 1989") +
  theme_minimal() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank()) +
  scale_fill_manual(values = c("#EE3B3B"), labels = c("Born before reunification"))
ggsave("flfp89_agecohort.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(agecohort_sum,  year == 1999 & 
                       !(age_cohort == "5-9" | age_cohort == "0-4" | age_cohort == "10-14" | age_cohort == "65+")),
       aes(x = as.factor(age_cohort), y = flfp, fill = group99)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + 
  labs(x = "Age cohort",
       y = "FLFP rate in 1999") +
  theme_minimal() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank()) +
  scale_fill_manual(values = c("#FF7256", "#EE3B3B"), labels = c("Born before reunification", "Born after reunification"))
ggsave("flfp99_agecohort.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(agecohort_sum,  year == 2009 & 
                       !(age_cohort == "5-9" | age_cohort == "0-4" | age_cohort == "10-14" | age_cohort == "65+")),
       aes(x = as.factor(age_cohort), y = flfp, fill = group09)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + 
  labs(x = "Age cohort",
       y = "FLFP rate in 2009") +
  theme_minimal() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank()) +
  scale_fill_manual(values = c("#FF7256", "#EE3B3B"), labels = c("Born before reunification", "Born after reunification"))
ggsave("flfp09_agecohort.jpeg", width = 7, height = 7)

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
