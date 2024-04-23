##############################
# PROVINCE-LEVEL REGRESSIONS #
##############################

ggplot(vhlss_prov_coef_ns, aes(x = factor(year), y = estimate, ymin = estimate - std.error, ymax = estimate + std.error, color = group)) +
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
ggsave("vhlss_prov_coef_ns.jpeg", width = 7, height = 7)


