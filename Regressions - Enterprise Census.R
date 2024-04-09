dn <- c("dn_prov.Rda", "dn_dist.Rda")

for (i in dn) {
  load(i)
}

bmr <- c("province_bmr_sum.Rda", "district_bmr_sum.Rda")

for (i in bmr) {
  load(i)
}

# Enterprise Census regressions 

## District level 

dn_dist_ols <- (list(
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2000),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2001),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2002),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2003),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2004),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2005),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2006),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2007),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2008),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2009),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2010),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2011),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2012),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2013),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2014),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2015),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2016),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2017),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2018),
        vcov = ~dist2018)))

dn_dist_ols_coef <- lapply(dn_dist_ols, tidy)
dn_dist_ols_coef_df <- do.call(rbind, dn_dist_ols_coef)
dn_dist_ols_coef_df$year <- rep(seq(2000, 2018), each = nrow(dn_dist_ols_coef_df) / length(seq(2000, 2018)))
dn_dist_ols_coef_df <- dn_dist_ols_coef_df %>% filter(term != "(Intercept)")

ggplot(dn_dist_ols_coef_df, aes(x = factor(year), y = estimate, ymin = estimate - std.error, ymax = estimate + std.error)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  labs(x = "Year", y = "Estimate and 95% Interval") +
  ggtitle("") +
  theme_minimal() +
  guides(fill = "none") +  
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        text = element_text(size=10)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("dn_dist_ols.jpeg", width = 7, height = 7)

### inc. province FE 

dn_dist_femodels <- (list(
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2000),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2001),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2002),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2003),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2004),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2005),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2006),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2007),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2008),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2009),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2010),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2011),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2012),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2013),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2014),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2015),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2016),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2017),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2018),
        vcov = ~dist2018)))

dn_dist_fecoef <- lapply(dn_dist_femodels, tidy)
dn_dist_fecoef_df <- do.call(rbind, dn_dist_fecoef)
dn_dist_fecoef_df$year <- rep(seq(2000, 2018), each = nrow(dn_dist_fecoef_df) / length(seq(2000, 2018)))

ggplot(dn_dist_fecoef_df, aes(x = factor(year), y = estimate, ymin = estimate - std.error, ymax = estimate + std.error)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  labs(x = "Year", y = "Estimate and 95% Interval") +
  ggtitle("") +
  theme_minimal() +
  guides(fill = "none") +  
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        text = element_text(size=10)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("dn_dist_fe.jpeg", width = 7, height = 7)

# By north/south

dn_dist_ols_s <- (list(
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2000 & south == 1),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2001 & south == 1),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2002 & south == 1),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2003 & south == 1),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2004 & south == 1),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2005 & south == 1),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2006 & south == 1),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2007 & south == 1),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2008 & south == 1),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2009 & south == 1),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2010 & south == 1),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2011 & south == 1),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2012 & south == 1),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2013 & south == 1),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2014 & south == 1),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2015 & south == 1),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2016 & south == 1),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2017 & south == 1),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2018 & south == 1),
        vcov = ~dist2018)))

dn_dist_ols_n <- (list(
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2000 & south == 0),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2001 & south == 0),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2002 & south == 0),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2003 & south == 0),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2004 & south == 0),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2005 & south == 0),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2006 & south == 0),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2007 & south == 0),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2008 & south == 0),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2009 & south == 0),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2010 & south == 0),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2011 & south == 0),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2012 & south == 0),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2013 & south == 0),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2014 & south == 0),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2015 & south == 0),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2016 & south == 0),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2017 & south == 0),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_dist, year == 2018 & south == 0),
        vcov = ~dist2018)))

dn_dist_ols_coef_n <- lapply(dn_dist_ols_n, tidy)
dn_dist_ols_coef_s <- lapply(dn_dist_ols_s, tidy)
dn_dist_ols_coef_n <- do.call(rbind, dn_dist_ols_coef_n)
dn_dist_ols_coef_s <- do.call(rbind, dn_dist_ols_coef_s)
dn_dist_ols_coef_s$year <- rep(seq(2000, 2018), each = nrow(dn_dist_ols_coef_s) / length(seq(2000, 2018)))
dn_dist_ols_coef_n$year <- rep(seq(2000, 2018), each = nrow(dn_dist_ols_coef_n) / length(seq(2000, 2018)))

dn_dist_ols_coef_n$group <- "North"
dn_dist_ols_coef_s$group <- "South"
dn_dist_ols_coef_ns <- rbind(dn_dist_ols_coef_n, dn_dist_ols_coef_s)
dn_dist_ols_coef_ns <- dn_dist_ols_coef_ns %>% filter(term != "(Intercept)")

ggplot(dn_dist_ols_coef_ns, aes(x = factor(year), y = estimate, ymin = estimate - std.error, ymax = estimate + std.error, color = group)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  labs(x = "Year", y = "Estimate and 95% Interval", color = "Region") +
  ggtitle("") +
  theme_minimal() +
  guides(fill = "none") +  
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        text = element_text(size=10)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("dn_dist_ols_coef_ns.jpeg", width = 7, height = 7)

## Inc. FE 

dn_dist_femodels_s <- (list(
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2000 & south == 1),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2001 & south == 1),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2002 & south == 1),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2003 & south == 1),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2004 & south == 1),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2005 & south == 1),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2006 & south == 1),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2007 & south == 1),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2008 & south == 1),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2009 & south == 1),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2010 & south == 1),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2011 & south == 1),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2012 & south == 1),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2013 & south == 1),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2014 & south == 1),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2015 & south == 1),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2016 & south == 1),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2017 & south == 1),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2018 & south == 1),
        vcov = ~dist2018)))

dn_dist_femodels_n <- (list(
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2000 & south == 0),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2001 & south == 0),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2002 & south == 0),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2003 & south == 0),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2004 & south == 0),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2005 & south == 0),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2006 & south == 0),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2007 & south == 0),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2008 & south == 0),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2009 & south == 0),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2010 & south == 0),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2011 & south == 0),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2012 & south == 0),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2013 & south == 0),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2014 & south == 0),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2015 & south == 0),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2016 & south == 0),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2017 & south == 0),
        vcov = ~dist2018),
  feols(workerratio ~ log(tot_bmr) | prov2018,
        subset(dn_dist, year == 2018 & south == 0),
        vcov = ~dist2018)))

dn_dist_fecoef_n <- lapply(dn_dist_femodels_n, tidy)
dn_dist_fecoef_s <- lapply(dn_dist_femodels_s, tidy)
dn_dist_fecoef_df_n <- do.call(rbind, dn_dist_fecoef_n)
dn_dist_fecoef_df_s <- do.call(rbind, dn_dist_fecoef_s)
dn_dist_fecoef_df_n$year <- rep(seq(2000, 2018), each = nrow(dn_dist_fecoef_df_n) / length(seq(2000, 2018)))
dn_dist_fecoef_df_s$year <- rep(seq(2000, 2018), each = nrow(dn_dist_fecoef_df_s) / length(seq(2000, 2018)))

dn_dist_fecoef_df_n$group <- "North"
dn_dist_fecoef_df_s$group <- "South"
dn_dist_fecoef_df_ns <- rbind(dn_dist_fecoef_df_n, dn_dist_fecoef_df_s)
dn_dist_fecoef_df_ns <- dn_dist_fecoef_df_ns %>% filter(term != "(Intercept)")

ggplot(dn_dist_fecoef_df_ns, aes(x = factor(year), y = estimate, ymin = estimate - std.error, ymax = estimate + std.error, color = group)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  labs(x = "Year", y = "Estimate and 95% Interval", color = "Region") +
  ggtitle("") +
  theme_minimal() +
  guides(fill = "none") +  
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        text = element_text(size=10)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("dn_dist_fecoef_ns.jpeg", width = 7, height = 7)

# Province level 

dn_prov_femodels <- (list(
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2000),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2001),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2002),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2003),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2004),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2005),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2006),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2007),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2008),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2009),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2010),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2011),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2012),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2013),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2014),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2015),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2016),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2017),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2018),
        vcov = ~prov2018)))

dn_prov_fecoef <- lapply(dn_prov_femodels, tidy)
dn_prov_fecoef <- do.call(rbind, dn_prov_fecoef)
dn_prov_fecoef$year <- rep(seq(2000, 2018), each = nrow(dn_prov_fecoef) / length(seq(2000, 2018)))

ggplot(dplyr::filter(dn_prov_fecoef, term == "log(tot_bmr)"), aes(x = factor(year), y = estimate, ymin = estimate - std.error, ymax = estimate + std.error)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  labs(x = "Year", y = "Estimate and 95% Interval") +
  ggtitle("") +
  theme_minimal() +
  guides(fill = "none") +  
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        text = element_text(size=10)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## South 

dn_prov_femodels_s <- (list(
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2000 & south == 1),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2001 & south == 1),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2002 & south == 1),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2003 & south == 1),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2004 & south == 1),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2005 & south == 1),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2006 & south == 1),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2007 & south == 1),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2008 & south == 1),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2009 & south == 1),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2010 & south == 1),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2011 & south == 1),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2012 & south == 1),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2013 & south == 1),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2014 & south == 1),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2015 & south == 1),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2016 & south == 1),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2017 & south == 1),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2018 & south == 1),
        vcov = ~prov2018)))

dn_prov_fecoef_s <- lapply(dn_prov_femodels_s, tidy)
dn_prov_fecoef_s <- do.call(rbind, dn_prov_fecoef_s)
dn_prov_fecoef_s$year <- rep(seq(2000, 2018), each = nrow(dn_prov_fecoef_s) / length(seq(2000, 2018)))

ggplot(dplyr::filter(dn_prov_fecoef_s, term == "log(tot_bmr)"), aes(x = factor(year), y = estimate, ymin = estimate - std.error, ymax = estimate + std.error)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  labs(x = "Year", y = "Estimate and 95% Interval") +
  ggtitle("") +
  theme_minimal() +
  guides(fill = "none") +  
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        text = element_text(size=10)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## North 

dn_prov_femodels_n <- (list(
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2000 & south == 0),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2001 & south == 0),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2002 & south == 0),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2003 & south == 0),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2004 & south == 0),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2005 & south == 0),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2006 & south == 0),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2007 & south == 0),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2008 & south == 0),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2009 & south == 0),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2010 & south == 0),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2011 & south == 0),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2012 & south == 0),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2013 & south == 0),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2014 & south == 0),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2015 & south == 0),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2016 & south == 0),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2017 & south == 0),
        vcov = ~prov2018),
  feols(workerratio ~ log(tot_bmr),
        subset(dn_prov, year == 2018 & south == 0),
        vcov = ~prov2018)))

dn_prov_fecoef_n <- lapply(dn_prov_femodels_n, tidy)
dn_prov_fecoef_n <- do.call(rbind, dn_prov_fecoef_n)
dn_prov_fecoef_n$year <- rep(seq(2000, 2018), each = nrow(dn_prov_fecoef_n) / length(seq(2000, 2018)))

ggplot(dplyr::filter(dn_prov_fecoef_n, term == "log(tot_bmr)"), aes(x = factor(year), y = estimate, ymin = estimate - std.error, ymax = estimate + std.error)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  labs(x = "Year", y = "Estimate and 95% Interval") +
  ggtitle("") +
  theme_minimal() +
  guides(fill = "none") +  
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        text = element_text(size=10)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
