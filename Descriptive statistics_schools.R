ggplot(schools, aes(x = (tot_bmr_per_prov), y = diff)) +
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
  labs(x = expression("Bombs/" * km^2),
       y = "Net no. of schools between 1965 and 1972") +
  geom_text(data = subset(schools, provname2002 == "Quảng Trị"), aes(label = provname2002), nudge_x = -10, nudge_y = 1, size = 3.5)
ggsave("schools_bombs.jpeg", width = 7, height = 7)
