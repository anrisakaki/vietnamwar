ggplot(selfagri_agexbmr_coef_02_ns, aes(x = factor(age), y = (estimate), ymin = ((estimate - std.error)), ymax = ((estimate + std.error)), color = group)) +
  geom_pointrange() +
  geom_vline(xintercept = 10, linetype = "dashed", color = "blue") +  
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

ggplot(selfagri_agexbmr_coef_04_ns, aes(x = factor(age), y = (estimate), ymin = ((estimate - std.error)), ymax = ((estimate + std.error)), color = group)) +
  geom_pointrange() +
  geom_vline(xintercept = 12, linetype = "dashed", color = "blue") +  
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

ggplot(selfagri_agexbmr_coef_06_ns, aes(x = factor(age), y = (estimate), ymin = ((estimate - std.error)), ymax = ((estimate + std.error)), color = group)) +
  geom_pointrange() +
  geom_vline(xintercept = 14, linetype = "dashed", color = "blue") +  
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

###########################################
# PROBABILITY OF WORKING IN MANUFACTURING #
###########################################

# 2002 

ggplot(manu_agexbmr_coef_08_ns, aes(x = factor(age), y = (estimate), ymin = ((estimate - std.error)), ymax = ((estimate + std.error)), color = group)) +
  geom_pointrange() +
  geom_vline(xintercept = 14, linetype = "dashed", color = "blue") +  
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
