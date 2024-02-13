phc <- c("phc.Rda", "phc89.Rda", "phc99.Rda", "phc09.Rda")

for (i in phc) {
  load(i)
}

dict <- c("age_cohort" = "Age Cohort $\times$ log(Bombs/km^2)")
setFixest_coefplot(dict = dict, grid = F, zero.par = list( type="dotted", lty=2), main = "")

iplot(list(
  feols(work ~ i(age_cohort),
        subset(phc89, female == 1 & birthyr > 1925 & birthyr < 1974 & migration == 0),
        weights = ~perwt),
  feols(work ~ i(age_cohort),
        subset(phc99, female == 1 & birthyr > 1935 & birthyr < 1984 & migration == 0),
        weights = ~perwt),
  feols(work ~ i(age_cohort),
        subset(phc09, female == 1 & birthyr > 1945 & birthyr < 1994 & migration == 0),
        weights = ~perwt)))

# 1989

iplot(feols(work ~ i(age_cohort, log(tot_bomb_per)) + yrschool + nchild + married + as.factor(minority),
            subset(phc89, female == 1 & birthyr > 1925 & birthyr < 1974 & migration == 0),
            weights = ~perwt), main = " ")

iplot(feols(work ~ i(age_cohort75, log(tot_bomb_per) | geo1_vn1989),
            subset(phc89, female == 1 & birthyr > 1925 & birthyr < 1974 & migration == 0),
            weights = ~perwt), main = " ")

# 1999

iplot(feols(work ~ i(birthyr) + as.factor(minority) + yrschool + as.factor(geo1_vn1999) + nchild + married,
            subset(phc99, female == 1 & birthyr > 1935 & birthyr < 1984),
            weights = ~perwt), xlab = "Birth Year", main = "")

iplot(feols(work ~ i(birthyr) + as.factor(minority) + yrschool + as.factor(geo1_vn1999) + nchild + married,
            subset(phc99, female == 0 & birthyr > 1935 & birthyr < 1984),
            weights = ~perwt), xlab = "Birth Year", main = "")

iplot(feols(work ~ i(age75, log(tot_bomb_per)) + as.factor(minority) + yrschool  + nchild + married | geo1_vn1999,
            subset(phc99, female == 1 & birthyr > 1935 & birthyr < 1984),
            weights = ~perwt), xlab = "Age in 1975", main = "")

iplot(feols(work ~ i(age75, log(tot_bomb_per)) + as.factor(minority) + yrschool  + nchild + married | geo1_vn1999,
            subset(phc89, female == 0 & birthyr > 1935 & birthyr < 1984),
            weights = ~perwt), xlab = "Age in 1975", main = "")

# 2009

iplot(feols(work ~ i(birthyr) + as.factor(minority) + yrschool + as.factor(geo1_vn2009) + nchild + married,
            subset(phc09, female == 1 & birthyr > 1945 & birthyr < 1994),
            weights = ~perwt), xlab = "Birth Year", main = "")

iplot(feols(work ~ i(birthyr) + as.factor(minority) + yrschool + as.factor(geo1_vn) + nchild + married,
            subset(phc99, female == 0 & birthyr > 1945 & birthyr < 1994),
            weights = ~perwt), xlab = "Birth Year", main = "")

iplot(feols(work ~ i(age75, log_tot_bomb) + as.factor(minority) + yrschool  + nchild + married,
            subset(phc89, female == 1 & birthyr > 1945 & birthyr < 1994),
            weights = ~perwt), xlab = "Age in 1975", main = "")

iplot(feols(work ~ i(age75, log_tot_bomb) + as.factor(minority) + yrschool  + nchild + married,
            subset(phc89, female == 0 & birthyr > 1945 & birthyr < 1994),
            weights = ~perwt), xlab = "Age in 1975", main = "")
