sex_ratios <- c("sexratio_prov_89.Rda", "sexratio_prov_99.Rda", "sexratio_prov_09.Rda")

for (i in sex_ratios) {
  load(i)
}

############################
# MAP OF BOMBING INTENSITY #
############################

ggplot(district_bmr_sum_phc_sf) + 
  geom_sf(aes(fill = log(tot_bmr))) +
  scale_fill_gradient(name = "log(Bombs, \nMissiles and Rockets)", low = "green", high = "red", na.value = "grey") + 
  geom_hline(yintercept = 17, color = "blue", linetype = "dashed") +
  annotate("text", x = Inf, y = 17.3, label = "17th Parallel", hjust = 1.1, color = "blue", size = 4) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        panel.background = element_blank()) +
  ggtitle("")
ggsave("district_bombs_sf.jpeg", width = 7, height = 7)

ggplot(district_bmr_sf) + 
  geom_sf(aes(fill = log(killed_tot))) +
  scale_fill_gradient(name = "log(Casualties)", low = "green", high = "red", na.value = "grey") + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        panel.background = element_blank()) +
  ggtitle("")
ggsave("district_casualties_sf.jpeg", width = 7, height = 7)

#################
# MISSION TYPES #
#################

south_missiontype$Region <- "South Vietnam"
north_missiontype$Region <- "North Vietnam"

missiontypes <- rbind(south_missiontype, north_missiontype)

ggplot(dplyr::filter(missiontypes, Var1 == "STRIKE" | Var1 == "CLOSE AIR SUPPORT"
                       | Var1 == "DIRECT AIR SUPPORT" | Var1 == "AIR INTERDICTION" | 
                       Var1 == "HEAVY BOMBARD" | Var1 == "ARMED RECCE"), aes(x = reorder(Var1, Share), y = Share*100, fill = Region)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() + 
  labs(
    x = "Mission Type",
    y = "Share of Total Missions (%)",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        text = element_text(size=10)) + 
  theme(axis.text.y = element_text(size = 10)) + 
  scale_y_continuous(breaks = seq(0, 100, by = 5))
ggsave("missiontypes.jpeg", width = 7, height = 7)

#################
# WIDOWS BY AGE #
#################

widowed75_89 <- widowed_89 %>% 
  mutate(age75 = age-14) %>% 
  mutate(region = ifelse(south == 1, "South", "North")) %>% 
  filter(age75 > 15 & age75 < 65)

ggplot(widowed_89, aes(x = share, y = as.factor(age75), fill = as.factor(region))) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() +
  labs(
    y = "Age in 1975",
    x = "Share of Women Widowed in 1989 (%)",
    fill = "region"
  ) +
  theme_minimal() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        text = element_text(size=15)) + 
  scale_x_continuous(breaks = seq(0, 80, by = 5))
ggsave("widows_byage.jpeg", width = 14, height = 14)

########################
# BMR VS MIGRANT SHARE #
########################

ggplot(dplyr::filter(sum89, south == 0), aes(x = log(tot_bmr_prov), y = (migrant_share)*100)) +
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
  labs(x = "log(BMR)",
       y = "Migrant Share (%)")

ggplot(dplyr::filter(sum89, south == 1), aes(x = log(tot_bmr_prov), y = (migrant_share)*100)) +
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
  labs(x = "log(BMR)",
       y = "Migrant Share (%)")

ggplot(dplyr::filter(sum99, south == 0), aes(x = log(tot_bmr_prov), y = (migrant_share)*100)) +
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
  labs(x = "log(BMR)",
       y = "Migrant Share (%)")

ggplot(dplyr::filter(sum99, south == 1), aes(x = log(tot_bmr_prov), y = (migrant_share)*100)) +
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
  labs(x = "log(BMR)",
       y = "Migrant Share (%)")

ggplot(dplyr::filter(sum09, south == 0), aes(x = log(tot_bmr_prov), y = (migrant_share)*100)) +
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
  labs(x = "log(BMR)",
       y = "Migrant Share (%)")

ggplot(dplyr::filter(sum09, south == 1), aes(x = log(tot_bmr_prov), y = (migrant_share)*100)) +
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
  labs(x = "log(BMR)",
       y = "Migrant Share (%)")

## District

ggplot(dplyr::filter(sum_dist09, south == 0), aes(x = log(tot_bmr), y = (migrant_share)*100)) +
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
  labs(x = "log(BMR)",
       y = "Migrant Share (%)")
ggsave("bmr_migrants_dist_09_n.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(sum_dist09, south == 1), aes(x = log(tot_bmr), y = (migrant_share)*100)) +
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
  labs(x = "log(BMR)",
       y = "Migrant Share (%)")
ggsave("bmr_migrants_dist_09_s.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(sum_dist19, south == 0), aes(x = log(tot_bmr), y = (migrant_share)*100)) +
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
  labs(x = "log(BMR)",
       y = "Migrant Share (%)")
ggsave("bmr_migrants_dist_19_n.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(sum_dist19, south == 1), aes(x = log(tot_bmr), y = (migrant_share)*100)) +
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
  labs(x = "log(BMR)",
       y = "Migrant Share (%)")
ggsave("bmr_migrants_dist_19_s.jpeg", width = 7, height = 7)

#####################
# BMR VS CASUALTIES #
#####################

ggplot(dplyr::filter(sum09, killed_tot_prov_ppn > 0), aes(x = log(tot_bmr_prov), y = log(killed_tot_prov_ppn))) +
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
  labs(x = "log(BMR)",
       y = "log(Casualties/Population)")
ggsave("bombs_casualties.jpeg", width = 7, height = 7)

##################################
# BIRTH COHORT SEX RATIO IN 1989 #
##################################

bc_sexratio89_long <- phc %>% 
  filter(year == 1989 & !is.na(age)) %>% 
  mutate(south = ifelse(geo1_vn1989 > 26 | geo1_vn1989 == 2, 1, 0))  %>% 
  group_by(age, south) %>% 
  summarise(n_men = sum(perwt[sex == 1]),
            n_women = sum(perwt[sex == 2])) %>% 
  mutate(SexRatio = (n_men/n_women)*100)

ggplot(bc_sexratio89_long, aes(x = 1975 - (1989 - age), y = (SexRatio), color = as.factor(south))) +
  geom_line(size = 1.1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.8) + 
  labs(
    x = "Age in 1975",
    y = "Sex Ratio",
    color = "south"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),  # Add axis lines
    legend.title = element_blank()
  ) +
  scale_x_continuous(
    breaks = seq(-10, 90, by = 2)
  ) +
  scale_y_continuous(
    breaks = seq(0, 120, by = 20)  # Y-axis in increments of 20
  ) +
  scale_color_discrete(labels = c("North", "South"))

ggsave("sexratio_birthcohort.jpeg", width = 17, height = 7)

####################
# BMR VS SEX RATIO #
####################

# 1989

ggplot(dplyr::filter(sum89, south == 0), aes(x = log(tot_bmr_prov), y = (tot_mlf/tot_flf)*100)) +
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
  labs(x = "log(BMR)",
       y = "Sex Ratio of Working Age Population")
ggsave("bmr_sexratio_89_n.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(sum89, south == 1), aes(x = log(tot_bmr_prov), y = (tot_mlf/tot_flf)*100)) +
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
  labs(x = "log(BMR)",
       y = "Sex Ratio of Working Age Population")
ggsave("bmr_sexratio_89_s.jpeg", width = 7, height = 7)

##########################
# BMR VS SHARE OF WIDOWS #
##########################

# 1989
ggplot(dplyr::filter(sum89, south == 0), aes(x = log(tot_bmr_prov), y = widow_share*100)) +
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
  labs(x = "log(BMR)",
       y = "Share of Widowed Women")
ggsave("bmr_widowed_f89_n.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(sum89, south == 1), aes(x = log(tot_bmr_prov), y = widow_share*100)) +
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
  labs(x = "log(BMR)",
       y = "Share of Widowed Women")
ggsave("bmr_widowed_f89_s.jpeg", width = 7, height = 7)

# 1999
ggplot(dplyr::filter(sum99, south == 0), aes(x = log(tot_bmr_prov), y = widow_share*100)) +
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
  labs(x = "log(BMR)",
       y = "Share of Widowed Women")
ggsave("bmr_widowed_f99_n.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(sum99, south == 1), aes(x = log(tot_bmr_prov), y = widow_share*100)) +
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
  labs(x = "log(BMR)",
       y = "Share of Widowed Women")
ggsave("bmr_widowed_f99_s.jpeg", width = 7, height = 7)

# 2009
ggplot(dplyr::filter(sum09, south == 0), aes(x = log(tot_bmr_prov), y = widow_share*100)) +
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
  labs(x = "log(BMR)",
       y = "Share of Widowed Women")
ggsave("bmr_widowed_f09_n.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(sum09, south == 1), aes(x = log(tot_bmr_prov), y = widow_share*100)) +
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
  labs(x = "log(BMR)",
       y = "Share of Widowed Women")
ggsave("bmr_widowed_f09_s.jpeg", width = 7, height = 7)

## district 

ggplot(dplyr::filter(sum_dist09, south == 0), aes(x = log(tot_bmr), y = widow_share*100)) +
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
  labs(x = "log(BMR)",
       y = "Share of Widowed Women")
ggsave("bmr_widowed_dist_f09_n.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(sum_dist09, south == 1), aes(x = log(tot_bmr), y = widow_share*100)) +
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
  labs(x = "log(BMR)",
       y = "Share of Widowed Women")
ggsave("bmr_widowed_dist_f09_s.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(sum_dist19, south == 0), aes(x = log(tot_bmr), y = widow_share*100)) +
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
  labs(x = "log(BMR)",
       y = "Share of Widowed Women")
ggsave("bmr_widowed_dist_f19_n.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(sum_dist19, south == 1), aes(x = log(tot_bmr), y = widow_share*100)) +
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
  labs(x = "log(BMR)",
       y = "Share of Widowed Women")
ggsave("bmr_widowed_dist_f19_s.jpeg", width = 7, height = 7)

###############
# BMR VS FLFP #
###############

# 1989
ggplot(dplyr::filter(sum89, south == 0), aes(x = log(tot_bmr_prov), y = flfp*100)) +
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
  labs(x = "log(BMR)",
       y = "FLFP")
ggsave("bmr_flfp_89_n.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(sum89, south == 1), aes(x = log(tot_bmr_prov), y = flfp*100)) +
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
  labs(x = "log(BMR)",
       y = "FLFP")
ggsave("bmr_flfp_89_s.jpeg", width = 7, height = 7)

# 1999
ggplot(dplyr::filter(sum99, south == 0), aes(x = log(tot_bmr_prov), y = flfp*100)) +
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
  labs(x = "log(BMR)",
       y = "FLFP")
ggsave("bmr_flfp_99_n.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(sum99, south == 1), aes(x = log(tot_bmr_prov), y = flfp*100)) +
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
  labs(x = "log(BMR)",
       y = "FLFP")
ggsave("bmr_flfp_99_s.jpeg", width = 7, height = 7)

# 2009
ggplot(dplyr::filter(sum09, south == 0), aes(x = log(tot_bmr_prov), y = flfp*100)) +
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
  labs(x = "log(BMR)",
       y = "FLFP")
ggsave("bmr_flfp_09_n.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(sum09, south == 1), aes(x = log(tot_bmr_prov), y = flfp*100)) +
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
  labs(x = "log(BMR)",
       y = "FLFP")
ggsave("bmr_flfp_09_s.jpeg", width = 7, height = 7)

## district 

ggplot(dplyr::filter(sum_dist09, south == 0), aes(x = log(tot_bmr), y = flfp*100)) +
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
  labs(x = "log(BMR)",
       y = "FLFP")
ggsave("bmr_flfp_dist_09_n.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(sum_dist09, south == 1), aes(x = log(tot_bmr), y = flfp*100)) +
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
  labs(x = "log(BMR)",
       y = "FLFP")
ggsave("bmr_flfp_dist_09_s.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(sum_dist19, south == 0), aes(x = log(tot_bmr), y = flfp*100)) +
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
  labs(x = "log(BMR)",
       y = "FLFP")
ggsave("bmr_flfp_dist_19_n.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(sum_dist19, south == 1), aes(x = log(tot_bmr), y = flfp*100)) +
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
  labs(x = "log(BMR)",
       y = "FLFP")
ggsave("bmr_flfp_dist_19_s.jpeg", width = 7, height = 7)

############################################
# MALE AND FEMALE LABOUR FORCE COMPOSITION #
############################################

# 1989

ggplot(dplyr::filter(indgen_s, year == 1989 & f_comp>1.1), aes(x = (f_comp), y = reorder(Industry, f_comp))) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(x = f_comp / 2, label = f_comp), size = 5, color = "black") +
  labs(x = "Share of Working Women in 1989 (%)",
       y = "") +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        text = element_text(size=20)) + 
  scale_x_continuous(breaks = NULL) 
ggsave("fcomp89_s.jpeg", width = 12, height = 12)

ggplot(dplyr::filter(indgen_n, year == 1989 & f_comp>1.4), aes(x = (f_comp), y = reorder(Industry, f_comp))) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(x = f_comp / 2, label = f_comp), size = 5, color = "black") +
  labs(x = "Share of Working Women in 1989 (%)",
       y = "") +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        text = element_text(size=20)) + 
  scale_x_continuous(breaks = NULL) 
ggsave("fcomp89_n.jpeg", width = 12, height = 12)

# 1999

ggplot(dplyr::filter(indgen_s, year == 1999 & f_comp>2), aes(x = (f_comp), y = reorder(Industry, f_comp))) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(x = f_comp / 2, label = f_comp), size = 5, color = "black") +
  labs(x = "Share of Working Women in 1999 (%)",
       y = "") +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        text = element_text(size=20)) + 
  scale_x_continuous(breaks = NULL) 
ggsave("fcomp99_s.jpeg", width = 12, height = 12)

ggplot(dplyr::filter(indgen_n, year == 1999 & f_comp>1), aes(x = (f_comp), y = reorder(Industry, f_comp))) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(x = f_comp / 2, label = f_comp), size = 5, color = "black") +
  labs(x = "Share of Working Women in 1999 (%)",
       y = "") +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        text = element_text(size=20)) + 
  scale_x_continuous(breaks = NULL) 
ggsave("fcomp99_n.jpeg", width = 12, height = 12)

# 2009

## Female

ggplot(dplyr::filter(indgen_s, year == 2009 & f_comp>4), aes(x = (f_comp), y = reorder(Industry, f_comp))) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(x = f_comp / 2, label = f_comp), size = 5, color = "black") +
  labs(x = "Share of Working Women in 2009 (%)",
       y = "") +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        text = element_text(size=20)) + 
  scale_x_continuous(breaks = NULL) 
ggsave("fcomp09_s.jpeg", width = 12, height = 12)

ggplot(dplyr::filter(indgen_n, year == 2009 & f_comp>2), aes(x = (f_comp), y = reorder(Industry, f_comp))) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(x = f_comp / 2, label = f_comp), size = 5, color = "black") +
  labs(x = "Share of Working Women in 2009 (%)",
       y = "") +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        text = element_text(size=20)) + 
  scale_x_continuous(breaks = NULL) 
ggsave("fcomp09_n.jpeg", width = 12, height = 12)

# 2019

ggplot(dplyr::filter(indgen_s, year == 2019 & f_comp>4), aes(x = (f_comp), y = reorder(Industry, f_comp))) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(x = f_comp / 2, label = f_comp), size = 5, color = "black") +
  labs(x = "Share of Working Women in 2019 (%)",
       y = "") +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        text = element_text(size=20)) + 
  scale_x_continuous(breaks = NULL) 
ggsave("fcomp19_s.jpeg", width = 12, height = 12)

ggplot(dplyr::filter(indgen_n, year == 2019 & f_comp>2), aes(x = (f_comp), y = reorder(Industry, f_comp))) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(x = f_comp / 2, label = f_comp), size = 5, color = "black") +
  labs(x = "Share of Working Women in 2009 (%)",
       y = "") +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        text = element_text(size=20)) + 
  scale_x_continuous(breaks = NULL) 
ggsave("fcomp19_n.jpeg", width = 12, height = 12)

## Male 
ggplot(dplyr::filter(indgen_s, year == 2019 & m_comp > 5), aes(x = (m_comp), y = reorder(Industry, m_comp))) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(x = m_comp / 2, label = m_comp), size = 5, color = "black") +
  labs(x = "Share of Men Women in 2019 (%)",
       y = "") +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        text = element_text(size=20)) + 
  scale_x_continuous(breaks = NULL) 
ggsave("mcomp19_s.jpeg", width = 12, height = 12)

ggplot(dplyr::filter(indgen_n, year == 2019 & m_comp > 3), aes(x = (m_comp), y = reorder(Industry, m_comp))) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(x = m_comp / 2, label = m_comp), size = 5, color = "black") +
  labs(x = "Share of Working Women in 2009 (%)",
       y = "") +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        text = element_text(size=20)) + 
  scale_x_continuous(breaks = NULL) 
ggsave("mcomp19_n.jpeg", width = 12, height = 12)

# Birth cohort and sex ratio 

ggplot(bc_sexratio89_long, aes(x = age, y = sex_ratio, colour = as.factor(group), group = as.factor(group))) +
  geom_line(size = 1.1) +
  # geom_vline(xintercept = 39, linetype = "dashed", color = "") + 
  geom_rect(
    xmin = 39, xmax = 68, ymin = -Inf, ymax = Inf,
    fill = "gray", alpha = 0.01
  ) +
  annotate(
    "text", x = (53.5), y = 1.1,  
    label = "Prime working age in 1975", size = 4
  ) + 
  # geom_vline(xintercept = 68, linetype = "dashed", color = "blue") +
  labs(x = "Age",
       y = "Sex Ratio") +
  theme_minimal() +
  scale_x_continuous(breaks=seq(0,100,5)) +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank())
ggsave("age_sexratio89.jpeg", width = 17, height = 7)

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
  geom_vline(xintercept = 1975, linetype = "dashed", color = "dark green") +  
  geom_text(aes(x = 1975, y = max(9.25), label = "Reunification"),
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
  c
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
