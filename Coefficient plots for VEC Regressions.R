##############
# FIRM-LEVEL #
##############

# By South 

ggplot(dn_indfe_coef_ns, aes(x = factor(year), y = estimate, ymin = estimate - std.error, ymax = estimate + std.error, color = group)) +
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
ggsave("dn_indfe_coef_ns.jpeg", width = 7, height = 7)

ggplot(dn_indfe_prov_coef_ns, aes(x = factor(year), y = estimate, ymin = estimate - std.error, ymax = estimate + std.error, color = group)) +
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
ggsave("dn_indfe_prov_coef_ns.jpeg", width = 7, height = 7)

## With casualties 

ggplot(dn_indfe_coef_cas, aes(x = factor(year), y = estimate, ymin = estimate - std.error, ymax = estimate + std.error)) +
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
ggsave("dn_indfe_coef_cas.jpeg", width = 7, height = 7)

ggplot(dn_indfe_prov_coef_cas, aes(x = factor(year), y = estimate, ymin = estimate - std.error, ymax = estimate + std.error)) +
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
ggsave("dn_indfe_prov_coef_cas.jpeg", width = 7, height = 7)

#############
# BY SECTOR #
#############
ind_workerratio_06_ns$sector <- factor(ind_workerratio_06_ns$sector, levels = rev(sort(unique(ind_workerratio_06_ns$sector))))
ind_workerratio_prov_06_ns$sector <- factor(ind_workerratio_prov_06_ns$sector, levels = rev(sort(unique(ind_workerratio_prov_06_ns$sector))))

ggplot(ind_workerratio_06_ns, aes(y = sector, x = estimate, color = group)) +  
  geom_point(position = position_dodge(width = 0.9), size = 3) +
  geom_errorbar(aes(xmin = estimate - std.error, xmax = estimate + std.error),  
                position = position_dodge(width = 0.9), width = 0.25) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +  
  labs(y = "", x = "Estimate and 95% Interval") +  
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

ggplot(ind_workerratio_prov_06_ns, aes(y = sector, x = estimate, color = group)) +  
  geom_point(position = position_dodge(width = 0.9), size = 3) +
  geom_errorbar(aes(xmin = estimate - std.error, xmax = estimate + std.error),  
                position = position_dodge(width = 0.9), width = 0.25) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +  
  labs(y = "", x = "Estimate and 95% Interval") +  
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
