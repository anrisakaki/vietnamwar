##########################################################
# PROBABILITY OF WORKING, BY AGE COHORT - DISTRICT LEVEL #
##########################################################

ggplot(work_agexbmr_09_ns, aes(x = factor(age_75), y = (estimate), ymin = ((estimate - std.error)), ymax = ((estimate + std.error)), colour = group)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  labs(x = "Age in 1975", y = "Estimate and 95% Interval", color = "Region") +
  geom_vline(xintercept = 18, linetype = "dashed", color = "blue") +  
  ggtitle("") +
  theme_minimal() +
  guides(fill = "none") +  
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        text = element_text(size=15)) + 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))
ggsave("work_agexbmr_09_ns.jpeg", width = 14, height = 14)

ggplot(work_agexbmr_19_ns, aes(x = factor(age_75), y = (estimate), ymin = ((estimate - std.error)), ymax = ((estimate + std.error)), colour = group)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  labs(x = "Age in 1975", y = "Estimate and 95% Interval", color = "Region") +
  geom_vline(xintercept = 28, linetype = "dashed", color = "blue") +  
  ggtitle("") +
  theme_minimal() +
  guides(fill = "none") +  
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        text = element_text(size=15)) + 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))
ggsave("work_agexbmr_19_ns.jpeg", width = 14, height = 14)