sex_ratios <- c("sexratio_prov_89.Rda", "sexratio_prov_99.Rda", "sexratio_prov_09.Rda")

for (i in sex_ratios) {
  load(i)
}

############################
# MAP OF BOMBING INTENSITY #
############################

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

ggplot(district_bmr_sf) + 
  geom_sf(aes(fill = log(tot_bmr))) +
  scale_fill_gradient(name = "log(Total Bombs, \nMissiles and Rockets)", low = "green", high = "red", na.value = "grey") + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        panel.background = element_blank()) +
  ggtitle("")
ggsave("district_bombs_sf.jpeg", width = 7, height = 7)

ggplot(district_bmr_sf) + 
  geom_sf(aes(fill = killed_tot)) +
  scale_fill_gradient(name = "log(Casualties)", low = "green", high = "red", na.value = "grey") + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        panel.background = element_blank()) +
  ggtitle("")
ggsave("district_casualties_sf.jpeg", width = 7, height = 7)

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
  labs(x = "log(BMR/Population)",
       y = "log(Casualties/Population)")
ggsave("bombs_casualties.jpeg", width = 7, height = 7)

##################################
# BIRTH COHORT SEX RATIO IN 1989 #
##################################

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

####################
# BMR VS SEX RATIO #
####################

# 1989

ggplot(dplyr::filter(sum89, south == 0), aes(x = log(tot_bmr_prov), y = sexratio*100)) +
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
  labs(x = "log(BMR/Population)",
       y = "Sex Ratio")
ggsave("bmr_sexratio_89_n.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(sum89, south == 1), aes(x = log(tot_bmr_prov), y = sexratio*100)) +
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
  labs(x = "log(BMR/Population)",
       y = "Sex Ratio")
ggsave("bmr_sexratio_89_s.jpeg", width = 7, height = 7)

# 1999

ggplot(dplyr::filter(sum99, south == 0), aes(x = log(tot_bmr_prov), y = sexratio*100)) +
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
  labs(x = "log(BMR/Population)",
       y = "Sex Ratio")
ggsave("bmr_sexratio_99_n.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(sum99, south == 1), aes(x = log(tot_bmr_prov), y = sexratio*100)) +
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
  labs(x = "log(BMR/Population)",
       y = "Sex Ratio")
ggsave("bmr_sexratio_99_s.jpeg", width = 7, height = 7)

# 2009

ggplot(dplyr::filter(sum09, south == 0), aes(x = log(tot_bmr_prov), y = sexratio*100)) +
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
  labs(x = "log(BMR/Population)",
       y = "Sex Ratio")
ggsave("bmr_sexratio_09_n.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(sum09, south == 1), aes(x = log(tot_bmr_prov), y = sexratio*100)) +
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
  labs(x = "log(BMR/Population)",
       y = "Sex Ratio")
ggsave("bmr_sexratio_09_s.jpeg", width = 7, height = 7)

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
  labs(x = "log(BMR/Population)",
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
  labs(x = "log(BMR/Population)",
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
  labs(x = "log(BMR/Population)",
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
  labs(x = "log(BMR/Population)",
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
  labs(x = "log(BMR/Population)",
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
  labs(x = "log(BMR/Population)",
       y = "Share of Widowed Women")
ggsave("bmr_widowed_f09_s.jpeg", width = 7, height = 7)

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
  labs(x = "log(BMR/Population)",
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
  labs(x = "log(BMR/Population)",
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
  labs(x = "log(BMR/Population)",
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
  labs(x = "log(BMR/Population)",
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
  labs(x = "log(BMR/Population)",
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
  labs(x = "log(BMR/Population)",
       y = "FLFP")
ggsave("bmr_flfp_09_s.jpeg", width = 7, height = 7)

###############
# BMR VS FLFP #
###############

# 1989
ggplot(dplyr::filter(sum89, south == 0), aes(x = sexratio*100, y = flfp*100)) +
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
  labs(x = "Sex Ratio",
       y = "FLFP")
ggsave("sexratio_flfp_89_n.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(sum89, south == 1), aes(x = sexratio*100, y = flfp*100)) +
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
  labs(x = "Sex Ratio",
       y = "FLFP")
ggsave("sexratio_flfp_89_s.jpeg", width = 7, height = 7)

# 1999
ggplot(dplyr::filter(sum99, south == 0), aes(x = sexratio*100, y = flfp*100)) +
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
  labs(x = "Sex Ratio",
       y = "FLFP")
ggsave("sexratio_flfp_99_n.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(sum99, south == 1), aes(x = sexratio*100, y = flfp*100)) +
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
  labs(x = "Sex Ratio",
       y = "FLFP")
ggsave("sexratio_flfp_99_s.jpeg", width = 7, height = 7)

# 2009

ggplot(dplyr::filter(sum09, south == 0), aes(x = sexratio*100, y = flfp*100)) +
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
  labs(x = "Sex Ratio",
       y = "FLFP")
ggsave("sexratio_flfp_09_n.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(sum09, south == 1), aes(x = sexratio*100, y = flfp*100)) +
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
  labs(x = "Sex Ratio",
       y = "FLFP")
ggsave("sexratio_flfp_09_s.jpeg", width = 7, height = 7)

########################################
# BMR VS SHARE OF FEMALE FOUNDED FIRMS #
########################################

ggplot(dplyr::filter(bmr_fdir_prov, south == 1), aes(x = log(tot_bmr), y = share_fdir)) +
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
       y = "Share of female-founded firms")

ggplot(dplyr::filter(bmr_fdir, south == 0), aes(x = log(tot_bmr), y = share_fdir)) +
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
       y = "Share of female-founded firms")

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

ggplot(dplyr::filter(indgen_s, year == 1989 & m_comp>3), aes(x = (m_comp), y = reorder(Industry, m_comp))) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(x = m_comp / 2, label = m_comp), size = 5, color = "black") +
  labs(x = "Share of Working Men in 1989 (%)",
       y = "") +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        text = element_text(size=20)) + 
  scale_x_continuous(breaks = NULL) 
ggsave("mcomp89_s.jpeg", width = 12, height = 12)

ggplot(dplyr::filter(indgen_n, year == 1989 & m_comp>2), aes(x = (m_comp), y = reorder(Industry, m_comp))) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(x = m_comp / 2, label = m_comp), size = 5, color = "black") +
  labs(x = "Share of Working Men in 1989 (%)",
       y = "") +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        text = element_text(size=20)) + 
  scale_x_continuous(breaks = NULL) 
ggsave("mcomp89_n.jpeg", width = 12, height = 12)

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

ggplot(dplyr::filter(indgen_s, year == 1999 & m_comp>2), aes(x = (m_comp), y = reorder(Industry, m_comp))) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(x = m_comp / 2, label = m_comp), size = 5, color = "black") +
  labs(x = "Share of Working Men in 1999 (%)",
       y = "") +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        text = element_text(size=20)) + 
  scale_x_continuous(breaks = NULL) 
ggsave("mcomp99_s.jpeg", width = 12, height = 12)

ggplot(dplyr::filter(indgen_n, year == 1999 & m_comp>3), aes(x = (m_comp), y = reorder(Industry, m_comp))) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(x = m_comp / 2, label = m_comp), size = 5, color = "black") +
  labs(x = "Share of Working Men in 1999 (%)",
       y = "") +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        text = element_text(size=20)) + 
  scale_x_continuous(breaks = NULL) 
ggsave("mcomp99_n.jpeg", width = 12, height = 12)

# 2002 

ggplot(dplyr::filter(ind02_s, f_comp>3.5 & !is.na(industry)), aes(x = (f_comp), y = reorder(Industry, f_comp))) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(x = f_comp / 2, label = f_comp), size = 5, color = "black") +
  labs(x = "Share of Working Women in 2001 (%)",
       y = "") +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        text = element_text(size=20)) + 
  scale_x_continuous(breaks = NULL) 
ggsave("fcomp02_s.jpeg", width = 12, height = 12)

ggplot(dplyr::filter(ind02_n, f_comp>1.6 & !is.na(industry)), aes(x = (f_comp), y = reorder(Industry, f_comp))) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(x = f_comp / 2, label = f_comp), size = 5, color = "black") +
  labs(x = "Share of Working Women in 2001 (%)",
       y = "") +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        text = element_text(size=20)) + 
  scale_x_continuous(breaks = NULL) 
ggsave("fcomp02_n.jpeg", width = 12, height = 12)

ggplot(dplyr::filter(ind02_s, m_comp>5 & !is.na(industry)), aes(x = (m_comp), y = reorder(Industry, m_comp))) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(x = m_comp / 2, label = m_comp), size = 5, color = "black") +
  labs(x = "Share of Working Men in 2001 (%)",
       y = "") +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        text = element_text(size=20)) + 
  scale_x_continuous(breaks = NULL) 
ggsave("mcomp02_s.jpeg", width = 12, height = 12)

ggplot(dplyr::filter(ind02_n, m_comp>3 & !is.na(industry)), aes(x = (m_comp), y = reorder(Industry, m_comp))) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(x = m_comp / 2, label = m_comp), size = 5, color = "black") +
  labs(x = "Share of Working Men in 2001 (%)",
       y = "") +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        text = element_text(size=20)) + 
  scale_x_continuous(breaks = NULL) 
ggsave("mcomp02_n.jpeg", width = 12, height = 12)

# 2009

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

ggplot(dplyr::filter(indgen_s, year == 2009 & m_comp>6), aes(x = (m_comp), y = reorder(Industry, m_comp))) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(x = m_comp / 2, label = m_comp), size = 5, color = "black") +
  labs(x = "Share of Working Men in 2009 (%)",
       y = "") +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        text = element_text(size=20)) + 
  scale_x_continuous(breaks = NULL) 
ggsave("mcomp09_s.jpeg", width = 12, height = 12)

ggplot(dplyr::filter(indgen_n, year == 2009 & m_comp>4), aes(x = (m_comp), y = reorder(Industry, m_comp))) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(x = m_comp / 2, label = m_comp), size = 5, color = "black") +
  labs(x = "Share of Working Men in 2009 (%)",
       y = "") +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        text = element_text(size=20)) + 
  scale_x_continuous(breaks = NULL) 
ggsave("mcomp09_n.jpeg", width = 12, height = 12)

# Birth cohort and labour force participation rate by age cohort

ggplot(dplyr::filter(agecohort_sum,  year == 1989 &
                       !(age_cohort == "5-9" | age_cohort == "0-4" | age_cohort == "10-14" | age_cohort == "65+")),
       aes(x = as.factor(age_cohort), y = flfp, fill = group89)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + 
  labs(x = "Age group",
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
  labs(x = "Age group",
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
  labs(x = "Age group",
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
