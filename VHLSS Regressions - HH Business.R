################################################################
# PROBABILITY OF BEING MANAGER OF HH BUSINESS - DISTRICT LEVEL #
################################################################

etable(list(
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus02, south == 0),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus04, south == 0),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus06, south == 0),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus02, south == 1),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus04, south == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus06, south == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen)
), tex = T)

etable(list(
  feols(female  ~ log(tot_bmr) | industry + tinh,
        subset(hhbus02, south == 0),
        weights = ~wt75,
        vcov = ~tinh),
  feols(female  ~ log(tot_bmr) | industry + tinh,
        subset(hhbus04, south == 0),
        weights = ~wt45,
        vcov = ~tinh),
  feols(female  ~ log(tot_bmr) | industry + tinh,
        subset(hhbus06, south == 0),
        weights = ~wt45,
        vcov = ~tinh),
  feols(female  ~ log(tot_bmr) | industry + tinh,
        subset(hhbus02, south == 1),
        weights = ~wt75,
        vcov = ~tinh),
  feols(female  ~ log(tot_bmr) | industry + tinh,
        subset(hhbus04, south == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(female  ~ log(tot_bmr) | industry + tinh,
        subset(hhbus06, south == 1),
        weights = ~wt45,
        vcov = ~tinh)
), tex = T)

#############################################################################
# PROBABILITY OF BEING MANAGER OF HH BUSINESS - DISTRICT LEVEL - CASUALTIES #
#############################################################################

etable(list(
  feols(female  ~ log(killed_tot) | tinh,
        subset(hhbus02, south == 1),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  feols(female  ~ log(killed_tot) | tinh,
        subset(hhbus04, south == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(female  ~ log(killed_tot) | tinh,
        subset(hhbus06, south == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen)
), tex = T)

etable(list(
  feols(female  ~ log(killed_tot) | industry + tinh,
        subset(hhbus02, south == 1),
        weights = ~wt75,
        vcov = ~tinh),
  feols(female  ~ log(killed_tot) | industry + tinh,
        subset(hhbus04, south == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(female  ~ log(killed_tot) | industry + tinh,
        subset(hhbus06, south == 1),
        weights = ~wt45,
        vcov = ~tinh)
), tex = T)


################################################################
# PROBABILITY OF BEING MANAGER OF HH BUSINESS - PROVINCE LEVEL #
################################################################

etable(list(
  feols(female  ~ log(tot_bmr_prov),
        subset(hhbus02, south == 0),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  feols(female  ~ log(tot_bmr_prov),
        subset(hhbus04, south == 0),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(female  ~ log(tot_bmr_prov),
        subset(hhbus06, south == 0),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(female  ~ log(tot_bmr_prov),
        subset(hhbus02, south == 1),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  feols(female  ~ log(tot_bmr_prov),
        subset(hhbus04, south == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(female  ~ log(tot_bmr_prov),
        subset(hhbus06, south == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen)
), tex = T)

etable(list(
  feols(female  ~ log(tot_bmr_prov) | industry,
        subset(hhbus02, south == 0),
        weights = ~wt75,
        vcov = ~tinh),
  feols(female  ~ log(tot_bmr_prov) | industry,
        subset(hhbus04, south == 0),
        weights = ~wt45,
        vcov = ~tinh),
  feols(female  ~ log(tot_bmr_prov) | industry,
        subset(hhbus06, south == 0),
        weights = ~wt45,
        vcov = ~tinh),
  feols(female  ~ log(tot_bmr_prov) | industry,
        subset(hhbus02, south == 1),
        weights = ~wt75,
        vcov = ~tinh),
  feols(female  ~ log(tot_bmr_prov) | industry,
        subset(hhbus04, south == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(female  ~ log(tot_bmr_prov) | industry,
        subset(hhbus06, south == 1),
        weights = ~wt45,
        vcov = ~tinh)
), tex = T)

etable(list(
  feols(female  ~ log(tot_bmr_prov_ppn),
        subset(hhbus02, south == 0),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  feols(female  ~ log(tot_bmr_prov_ppn),
        subset(hhbus04, south == 0),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(female  ~ log(tot_bmr_prov_ppn),
        subset(hhbus06, south == 0),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(female  ~ log(tot_bmr_prov_ppn),
        subset(hhbus02, south == 1),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  feols(female  ~ log(tot_bmr_prov_ppn),
        subset(hhbus04, south == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(female  ~ log(tot_bmr_prov_ppn),
        subset(hhbus06, south == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen)
), tex = T)

etable(list(
  feols(female  ~ log(tot_bmr_prov_ppn) | industry,
        subset(hhbus02, south == 0),
        weights = ~wt75,
        vcov = ~tinh),
  feols(female  ~ log(tot_bmr_prov_ppn) | industry,
        subset(hhbus04, south == 0),
        weights = ~wt45,
        vcov = ~tinh),
  feols(female  ~ log(tot_bmr_prov_ppn) | industry,
        subset(hhbus06, south == 0),
        weights = ~wt45,
        vcov = ~tinh),
  feols(female  ~ log(tot_bmr_prov_ppn) | industry,
        subset(hhbus02, south == 1),
        weights = ~wt75,
        vcov = ~tinh),
  feols(female  ~ log(tot_bmr_prov_ppn) | industry,
        subset(hhbus04, south == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(female  ~ log(tot_bmr_prov_ppn) | industry,
        subset(hhbus06, south == 1),
        weights = ~wt45,
        vcov = ~tinh)
), tex = T)


###########################################################
# PROBABILITY OF BEING MANAGER OF HH BUSINESS - BY SECTOR #
###########################################################

# 2002 

fbus_sector_02_s <- list(
  # Furniture production
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus02, south == 1 & industry == 36),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  # Food & Beverages 
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus02, south == 1 & industry == 15),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  # Textiles  
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus02, south == 1 & industry == 17),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  # Apparel
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus02, south == 1 & industry == 18),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  # Wood
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus02, south == 1 & industry == 20),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  # Wholesale
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus02, south == 1 & industry == 51),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  # Retail sale
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus02, south == 1 & industry == 52),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  # Hotel and restaurant
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus02, south == 1 & industry == 55),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  # Transport
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus02, south == 1 & industry == 60),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  # Other services
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus02, south == 1 & industry == 93),
        weights = ~wt75,
        vcov = ~tinh+huyen))

fbus_sector_02_n <- list(
  # Furniture production
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus02, south == 0 & industry == 36),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  # Food & Beverages 
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus02, south == 0 & industry == 15),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  # Textiles and apparel 
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus02, south == 0 & industry == 17),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  # Apparel
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus02, south == 0 & industry == 18),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  # Wood
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus02, south == 0 & industry == 20),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  # Wholesale
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus02, south == 0 & industry == 51),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  # Retail sale
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus02, south == 0 & industry == 52),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  # Hotel and restaurant
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus02, south == 0 & industry == 55),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  # Transport
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus02, south == 0 & industry == 60),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  # Other services
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus02, south == 0 & industry == 93),
        weights = ~wt75,
        vcov = ~tinh+huyen))

# 2004 

fbus_sector_04_s <- list(
  # Furniture
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus04, south == 1 & industry == 36),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  # Food & Beverages 
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus04, south == 1 & industry == 15),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  # Textiles  
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus04, south == 1 & industry == 17),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  # Apparel
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus04, south == 1 & industry == 18),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  # Wood
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus04, south == 1 & industry == 20),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  # Vehicle sales 
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus04, south == 1 & industry == 50),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  # Retail sale
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus04, south == 1 & industry == 52),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  # Hotel and restaurant
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus04, south == 1 & industry == 55),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  # Transport
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus04, south == 1 & industry == 60),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  # Other services
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus04, south == 1 & industry == 93),
        weights = ~wt45,
        vcov = ~tinh+huyen))

fbus_sector_04_n <- list(
  # Furniture
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus04, south == 0 & industry == 36),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  # Food & Beverages 
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus04, south == 0 & industry == 15),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  # Textiles and apparel 
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus04, south == 0 & industry == 17),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  # Apparel
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus04, south == 0 & industry == 18),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  # Wood
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus04, south == 0 & industry == 20),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  # Wholesale
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus04, south == 0 & industry == 51),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  # Retail sale
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus04, south == 0 & industry == 52),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  # Hotel and restaurant
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus04, south == 0 & industry == 55),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  # Transport
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus04, south == 0 & industry == 60),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  # Other services
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus04, south == 0 & industry == 93),
        weights = ~wt45,
        vcov = ~tinh+huyen))

# 2006

fbus_sector_06_s <- list(
  # Furniture 
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus06, south == 1 & industry == 36),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  # Food & Beverages 
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus06, south == 1 & industry == 15),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  # Textiles  
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus06, south == 1 & industry == 17),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  # Apparel
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus06, south == 1 & industry == 18),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  # Wood
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus06, south == 1 & industry == 20),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  # Wholesale
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus06, south == 1 & industry == 51),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  # Retail sale
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus06, south == 1 & industry == 52),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  # Hotel and restaurant
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus06, south == 1 & industry == 55),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  # Transport
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus06, south == 1 & industry == 60),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  # Other services
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus06, south == 1 & industry == 93),
        weights = ~wt45,
        vcov = ~tinh+huyen))

fbus_sector_06_n <- list(
  # Furniture 
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus06, south == 0 & industry == 36),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  # Food & Beverages 
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus06, south == 0 & industry == 15),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  # Textiles and apparel 
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus06, south == 0 & industry == 17),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  # Apparel
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus06, south == 0 & industry == 18),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  # Wood
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus06, south == 0 & industry == 20),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  # Wholesale
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus06, south == 0 & industry == 51),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  # Retail sale
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus06, south == 0 & industry == 52),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  # Hotel and restaurant
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus06, south == 0 & industry == 55),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  # Transport
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus06, south == 0 & industry == 60),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  # Other services
  feols(female  ~ log(tot_bmr) | tinh,
        subset(hhbus06, south == 0 & industry == 93),
        weights = ~wt45,
        vcov = ~tinh+huyen))

sectors <- c("Furniture", "F&B", "Textiles", "Apparel", "Wood", "Wholesale trade", "Retail trade", "Hotel and restaurant", "Transport", "Other services")

fbus_sector_02_s <- lapply(fbus_sector_02_s, tidy)
fbus_sector_02_n <- lapply(fbus_sector_02_n, tidy)
fbus_sector_02_s <- do.call(rbind, fbus_sector_02_s) %>% filter(term == "log(tot_bmr)")
fbus_sector_02_n <- do.call(rbind, fbus_sector_02_n) %>% filter(term == "log(tot_bmr)")

fbus_sector_04_s <- lapply(fbus_sector_04_s, tidy)
fbus_sector_04_n <- lapply(fbus_sector_04_n, tidy)
fbus_sector_04_s <- do.call(rbind, fbus_sector_04_s) %>% filter(term == "log(tot_bmr)")
fbus_sector_04_n <- do.call(rbind, fbus_sector_04_n) %>% filter(term == "log(tot_bmr)")

fbus_sector_06_s <- lapply(fbus_sector_06_s, tidy)
fbus_sector_06_n <- lapply(fbus_sector_06_n, tidy)
fbus_sector_06_s <- do.call(rbind, fbus_sector_06_s) %>% filter(term == "log(tot_bmr)")
fbus_sector_06_n <- do.call(rbind, fbus_sector_06_n) %>% filter(term == "log(tot_bmr)")

fbus_sector_02_s$group <- "South"
fbus_sector_02_n$group <- "North"
fbus_sector_02_s$sector <- rep(sectors, each = 1)
fbus_sector_02_n$sector <- rep(sectors, each = 1)

fbus_sector_04_s$group <- "South"
fbus_sector_04_n$group <- "North"
fbus_sector_04_s$sector <- rep(sectors, each = 1)
fbus_sector_04_n$sector <- rep(sectors, each = 1)

fbus_sector_06_s$group <- "South"
fbus_sector_06_n$group <- "North"
fbus_sector_06_s$sector <- rep(sectors, each = 1)
fbus_sector_06_n$sector <- rep(sectors, each = 1)

#####################
# COEFFICIENT PLOTS #
#####################

fbus_sector_02_ns <- bind_rows(fbus_sector_02_n, fbus_sector_02_s)
fbus_sector_04_ns <- bind_rows(fbus_sector_04_n, fbus_sector_04_s)
fbus_sector_06_ns <- bind_rows(fbus_sector_06_n, fbus_sector_06_s)

fbus_sector_02_ns$sector <- factor(fbus_sector_02_ns$sector, levels = rev(sort(unique(fbus_sector_02_ns$sector))))
fbus_sector_04_ns$sector <- factor(fbus_sector_04_ns$sector, levels = rev(sort(unique(fbus_sector_04_ns$sector))))
fbus_sector_06_ns$sector <- factor(fbus_sector_06_ns$sector, levels = rev(sort(unique(fbus_sector_06_ns$sector))))

ggplot(fbus_sector_02_ns, aes(y = sector, x = estimate, color = group)) +  
  geom_point(position = position_dodge(width = 0.9), size = 3) +
  geom_errorbar(aes(xmin = estimate - std.error, xmax = estimate + std.error),  
                position = position_dodge(width = 0.9), width = 0.25) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +  
  labs(y = "Sector", x = "Estimate and 95% Interval") +  
  ggtitle("") +
  theme_minimal() +
  guides(fill = "none") +  
  theme(axis.line = element_line(color = 'black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 10)) + 
  theme(axis.text.y = element_text(angle = 0, hjust = 1))
ggsave("fbus_sector_02_ns.jpeg", width = 7, height = 7)

ggplot(fbus_sector_04_ns, aes(y = sector, x = estimate, color = group)) +  
  geom_point(position = position_dodge(width = 0.9), size = 3) +
  geom_errorbar(aes(xmin = estimate - std.error, xmax = estimate + std.error),  
                position = position_dodge(width = 0.9), width = 0.25) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +  
  labs(y = "Sector", x = "Estimate and 95% Interval") +  
  ggtitle("") +
  theme_minimal() +
  guides(fill = "none") +  
  theme(axis.line = element_line(color = 'black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 10)) + 
  theme(axis.text.y = element_text(angle = 0, hjust = 1))
ggsave("fbus_sector_04_ns.jpeg", width = 7, height = 7)

ggplot(fbus_sector_06_ns, aes(y = sector, x = estimate, color = group)) +  
  geom_point(position = position_dodge(width = 0.9), size = 3) +
  geom_errorbar(aes(xmin = estimate - std.error, xmax = estimate + std.error),  
                position = position_dodge(width = 0.9), width = 0.25) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +  
  labs(y = "Sector", x = "Estimate and 95% Interval") +  
  ggtitle("") +
  theme_minimal() +
  guides(fill = "none") +  
  theme(axis.line = element_line(color = 'black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 10)) + 
  theme(axis.text.y = element_text(angle = 0, hjust = 1))
ggsave("fbus_sector_06_ns.jpeg", width = 7, height = 7)
