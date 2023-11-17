etable(feols(educ ~ i(as.factor(war_time), tot_bmr_per_prov) + female | birth_year, 
             vhlss06_bombs,
             weights = ~hhwt,
             vcov = ~tinh))

feols(educ ~ tot_bmr_per_prov + female | birth_year, 
      subset(vhlss06_bombs, war_time == 1),
      weights = ~hhwt,
      vcov = ~tinh)

feols(educ ~ i(as.factor(female), tot_bmr_per_prov) | birth_year, 
      subset(vhlss06_bombs, war_time == 1),
      weights = ~hhwt,
      vcov = ~tinh)
