# VHLSS REGRESSIONS # 

etable(list(
  feols(educ ~ tot_bombs | birth_year, subset(vhlss06_bombs, birth_year < 1975 & birth_year > 1955), weights = ~hhwt, vcov = ~district),
  feols(educ ~ log(tot_bombs) | birth_year, subset(vhlss06_bombs, birth_year < 1975 & birth_year > 1955), weights = ~hhwt, vcov = ~district)
))
