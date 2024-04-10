# District-level 

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

(ggplot(dn_dist_olscoef_agri, aes(x = factor(year), y = estimate, ymin = estimate - std.error, ymax = estimate + std.error)) +
    geom_pointrange() +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
    labs(x = "Year", y = "Estimate and 95% Interval") +
    ggtitle("Agriculture") +
    theme_minimal() +
    guides(fill = "none") +  
    theme(axis.line = element_line(color='black'),
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          legend.title=element_blank(),
          text = element_text(size=10)) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) | 
    
    ggplot(dn_dist_olscoef_manu, aes(x = factor(year), y = estimate, ymin = estimate - std.error, ymax = estimate + std.error)) +
    geom_pointrange() +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
    labs(x = "Year", y = "Estimate and 95% Interval") +
    ggtitle("Manufacturing") +
    theme_minimal() +
    guides(fill = "none") +  
    theme(axis.line = element_line(color='black'),
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          legend.title=element_blank(),
          text = element_text(size=10)) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))) + 
  plot_layout(guides = 'collect') & theme(legend.position = 'bottom')
ggsave

(ggplot(dn_dist_fecoef_agri, aes(x = factor(year), y = estimate, ymin = estimate - std.error, ymax = estimate + std.error)) +
              geom_pointrange() +
              geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
              labs(x = "Year", y = "Estimate and 95% Interval") +
              ggtitle("Agriculture") +
              theme_minimal() +
              guides(fill = "none") +  
              theme(axis.line = element_line(color='black'),
                    plot.background = element_blank(),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_blank(),
                    legend.title=element_blank(),
                    text = element_text(size=10)) + 
              theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) | 
    ggplot(dn_dist_fecoef_manu, aes(x = factor(year), y = estimate, ymin = estimate - std.error, ymax = estimate + std.error)) +
              geom_pointrange() +
              geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
              labs(x = "Year", y = "Estimate and 95% Interval") +
              ggtitle("Manufacturing") +
              theme_minimal() +
              guides(fill = "none") +  
              theme(axis.line = element_line(color='black'),
                    plot.background = element_blank(),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_blank(),
                    legend.title=element_blank(),
                    text = element_text(size=10)) + 
              theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))) + 
  plot_layout(guides = 'collect') & theme(legend.position = 'bottom')

## By South 

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

## By industry 

ggplot(dn_dist_olscoef_agri_ns, aes(x = factor(year), y = estimate, ymin = estimate - std.error, ymax = estimate + std.error, color = group)) +
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

ggplot(dn_dist_olscoef_manu_ns, aes(x = factor(year), y = estimate, ymin = estimate - std.error, ymax = estimate + std.error, color = group)) +
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

# Province-level 

ggplot(dn_prov_ols_coef, aes(x = factor(year), y = estimate, ymin = estimate - std.error, ymax = estimate + std.error)) +
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
ggsave("dn_prov_ols_coef.jpeg", width = 7, height = 7)

ggplot(dn_prov_ols_coef_ns, aes(x = factor(year), y = estimate, ymin = estimate - std.error, ymax = estimate + std.error, color = group)) +
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
ggsave("dn_prov_ols_coef_ns.jpeg", width = 7, height = 7)

