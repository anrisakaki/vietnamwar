vhlss06_exposed <- vhlss06_bombs %>% 
  filter(exposed == 1)

etable(feols(educ ~ tot_bmr_per_prov + female + as.factor(ethnicity) + log_popdensity6061 + as.factor(south) | birth_year,
       subset(vhlss06_bombs, exposed == 1),
       weights = ~hhwt,
       vcov = ~tinh))

iplot(feols(
  educ ~ i(as.factor(exposed), tot_bmr_per_prov) + as.factor(ethnicity) + as.factor(female) | birth_year + tinh,
  subset(vhlss06_bombs, birth_year > 1959 & birth_year < 1980),
  weights = ~hhwt,
  vcov = ~tinh))


iplot(feols(educ ~ i(as.factor(female), tot_bmr_per_prov) + as.factor(ethnicity)| birth_year^tinh, 
            subset(vhlss06_bombs, birth_year > 1959 & birth_year < 1980),
            weights = ~hhwt,
            vcov = ~tinh), main = ""
)
