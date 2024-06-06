########################################
# BMR VS SHARE OF FEMALE FOUNDED FIRMS #
########################################

ggplot(dplyr::filter(bmr_fdir_prov, south == 0), aes(x = log(tot_bmr), y = share_fdir)) +
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
       y = "Sex Ratio")

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
       y = "Sex Ratio")

