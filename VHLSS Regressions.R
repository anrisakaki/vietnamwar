iplot(feols(work ~ i(birth_year) + as.factor(ethnicity) + educ + as.factor(tinh),
            subset(vhlss06_bombs, birth_year > 1942 & birth_year < 1989 & female == 1),
            weights = ~hhwt), xlab = "Birth Year", main = "Effect on FLFP")

iplot(feols(work ~ i(birth_year) + as.factor(ethnicity) + educ + as.factor(tinh),
            subset(vhlss06_bombs, birth_year > 1942 & birth_year < 1989 & female == 0),
            weights = ~hhwt), xlab = "Birth Year", main = "Effect on MLFP")

iplot(feols(work ~ i(as.factor(birth_year),tot_bmr_per_prov) + as.factor(ethnicity) + educ | tinh,
            subset(vhlss06_bombs, female == 0 & birth_year > 1942 & birth_year < 1989),
            vcov = ~tinh,
            weights = ~hhwt))
