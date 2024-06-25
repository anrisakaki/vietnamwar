##########################################################
# PROBABILITY OF WORKING, BY AGE COHORT - DISTRICT LEVEL #
##########################################################

ggplot(work_agexbmr_09_ns, aes(x = factor(age), y = (estimate), ymin = ((estimate - std.error)), ymax = ((estimate + std.error)), colour = group)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  labs(x = "Age in 2009", y = "Estimate and 95% Interval", color = "Region") +
  geom_vline(xintercept = 19, linetype = "dashed", color = "blue") +  
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

ggplot(work_agexbmr_cs_09_ns, aes(x = factor(age), y = (estimate), ymin = ((estimate - std.error)), ymax = ((estimate + std.error)), colour = group)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  labs(x = "Age in 2009", y = "Estimate and 95% Interval", color = "Region") +
  geom_vline(xintercept = 19, linetype = "dashed", color = "blue") +  
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
ggsave("work_agexbmr_cs_09_ns.jpeg", width = 14, height = 14)

ggplot(work_agexbmr_09_cas, aes(x = factor(age), y = (estimate), ymin = ((estimate - std.error)), ymax = ((estimate + std.error)))) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  labs(x = "Age in 2009", y = "Estimate and 95% Interval", color = "Region") +
  geom_vline(xintercept = 19, linetype = "dashed", color = "blue") +  
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
ggsave("work_agexbmr_09_cas.jpeg", width = 14, height = 14)

##########################################################
# Probability of working, by age cohort - province level #
##########################################################

ggplot(work_agexbmr_ppn_09_ns, aes(x = factor(age), y = (estimate), ymin = ((estimate - std.error)), ymax = ((estimate + std.error)), color = group)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  labs(x = "Age in 2009", y = "Estimate and 95% Interval", color = "Region") +
  geom_vline(xintercept = 19, linetype = "dashed", color = "blue") +  
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
ggsave("work_agexbmr_ppn_09_ns.jpeg", width = 14, height = 14)

ggplot(work_agexbmr_prov_09_ns, aes(x = factor(age), y = (estimate), ymin = ((estimate - std.error)), ymax = ((estimate + std.error)), color = group)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  labs(x = "Age in 2009", y = "Estimate and 95% Interval", color = "Region") +
  geom_vline(xintercept = 19, linetype = "dashed", color = "blue") +  
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
ggsave("work_agexbmr_prov_09_ns.jpeg", width = 14, height = 14)

########################################################
# Probability of working in agri / manu, by age cohort #
########################################################

ggplot(agri_agexbmr_coef_09_ns, aes(x = factor(age), y = (estimate), ymin = ((estimate - std.error)), ymax = ((estimate + std.error)), color = group)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  labs(x = "Age in 2009", y = "Estimate and 95% Interval", color = "Region") +
  ggtitle("") +
  geom_vline(xintercept = 18, linetype = "dashed", color = "blue") +  
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
ggsave("agri_agexbmr_09_ns.jpeg", width = 14, height = 14)

ggplot(manu_agexbmr_coef_09_ns, aes(x = factor(age), y = (estimate), ymin = ((estimate - std.error)), ymax = ((estimate + std.error)), color = group)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  labs(x = "Age in 2009", y = "Estimate and 95% Interval", color = "Region") +
  ggtitle("") +
  geom_vline(xintercept = 18, linetype = "dashed", color = "blue") +  
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
ggsave("manu_agexbmr_09_ns.jpeg", width = 14, height = 14)
