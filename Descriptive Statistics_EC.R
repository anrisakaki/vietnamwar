# Map of number of workers across districts 

dn_dist_sf <- left_join(dn_dist, vnmap2, by = c("provname2018" = "NAME_1", "distname2018")) %>% st_as_sf()

ggplot(dn_dist_sf) + 
  geom_sf(aes(fill = nworkers)) +
  scale_fill_gradient(name = "Total Formal Workers", low = "green", high = "red", na.value = "white") + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        panel.background = element_blank()) +
  ggtitle("")

# Sex ratio of workers

## All country 

ggplot(dplyr::filter(dn_prov, year == 2018), aes(x = log(tot_bmr), y = log(workerratio*100))) +
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
       y = "log(Ratio of Male to Female Workers) in 2018")
ggsave("dn_prov_sexratio_18.jpeg", width = 7, height = 7)

# District 

ggplot(dplyr::filter(dn_dist, year == 2009), aes(x = log(tot_bmr), y = log(workerratio*100))) +
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
       y = "log(Ratio of Male to Female Workers) in 2009")
ggsave("dn_dist_sexratio_09.jpeg", width = 7, height = 7)

## South 

ggplot(dplyr::filter(dn_dist, year == 2009 & south == 1), aes(x = log(tot_bmr), y = log(workerratio*100))) +
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
       y = "log(Ratio of Male to Female Workers) in 2009 \n(South)")
ggsave("dn_dist_sexratio_09_s.jpeg", width = 7, height = 7)

ggplot(dplyr::filter(dn_prov, year == 2009 & south == 0), aes(x = log(tot_bmr), y = log(workerratio*100))) +
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
       y = "log(Ratio of Male to Female Workers) in 2000 \n(North)")
ggsave("dn_prov_sexratio_09_n.jpeg", width = 7, height = 7)

# Directors 

ggplot(dplyr::filter(dn_dist, south == 0), aes(x = log(tot_bmr), y = log(dirratio*100))) +
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
       y = "log(Ratio of Male to Female Directors) in 2016")
