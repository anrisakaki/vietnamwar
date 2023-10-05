thor_noweapons <- thor %>% 
  group_by(year, WEAPON_CLASS) %>% 
  summarise(tot_weapon_del = sum(NUMWEAPONSDELIVERED)) %>% 
  filter(!is.na(year)) 

thor_weapons_load <- thor %>% 
  filter(WEAPONSLOADEDWEIGHT != -1) %>% 
  group_by(year, WEAPON_CLASS) %>% 
  summarise(tot_weapon_load= sum(WEAPONSLOADEDWEIGHT)) %>% 
  filter(!is.na(year)) 

thor_sum <- list(thor_noweapons, thor_weapons_load) %>% 
  reduce(full_join, by = c("year", "WEAPON_CLASS"))

ggplot(dplyr::filter(thor_sum, WEAPON_CLASS == "BOMB" | WEAPON_CLASS == "ROCKET"), aes(x = year, y = tot_weapon_del, fill = as.factor(WEAPON_CLASS))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year",
       y = "Weapons Delivered") +
  scale_x_continuous(breaks = unique(thor_sum$year)) +
  theme_minimal() + 
  guides(fill = guide_legend(title = NULL)) +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank())  

ggplot(dplyr::filter(thor_sum, WEAPON_CLASS == "BOMB" | WEAPON_CLASS == "ROCKET" & year > 1969), aes(x = year, y = (tot_weapon_load/2204.62), fill = as.factor(WEAPON_CLASS))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year",
       y = "Total Weapon Load (Tonnes)") +
  scale_x_continuous(breaks = unique(thor_sum$year)) +
  theme_minimal() +
  guides(fill = guide_legend(title = NULL)) +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank())    
