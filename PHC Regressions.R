phc <- c("phc.Rda", "phc89.Rda", "phc99.Rda", "phc09.Rda")

for (i in phc) {
  load(i)
}

dict <- c("age_cohort" = "Age Cohort $\times$ log(Bombs/km^2)")
setFixest_coefplot(dict = dict, grid = F, zero.par = list( type="dotted", lty=2), main = "")

etable(list(
  feols(work ~ as.factor(female)/log(tot_civilian) + yrschool + nchild + married + as.factor(minority) + age | regnvn,
        subset(phc89, birthyr > 1925 & birthyr < 1974 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(female)/log(tot_civilian) + yrschool + nchild + married + as.factor(minority) + age | regnvn,
        subset(phc99, birthyr > 1935 & birthyr < 1984 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(female)/log(tot_civilian) + yrschool + nchild + married + as.factor(minority) + age | regnvn,
        subset(phc09, birthyr > 1945 & birthyr < 1994 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn2009)), tex = T)

etable(list(
  feols(work ~ as.factor(female)/log(tot_infrastructure) + yrschool + nchild + married + as.factor(minority) + age | regnvn,
        subset(phc89, birthyr > 1925 & birthyr < 1974 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(female)/log(tot_infrastructure) + yrschool + nchild + married + as.factor(minority) + age | regnvn,
        subset(phc99, birthyr > 1935 & birthyr < 1984 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(female)/log(tot_infrastructure) + yrschool + nchild + married + as.factor(minority) + age | regnvn,
        subset(phc09, birthyr > 1945 & birthyr < 1994 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn2009)), tex = T)

etable(list(
  feols(work ~ as.factor(female)/log(tot_infrastructure) + yrschool + nchild + married + as.factor(minority) + age | regnvn,
        subset(phc89, birthyr > 1925 & birthyr < 1974 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(female)/log(tot_infrastructure) + yrschool + nchild + married + as.factor(minority) + age | regnvn,
        subset(phc99, birthyr > 1935 & birthyr < 1984 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(female)/log(tot_infrastructure) + yrschool + nchild + married + as.factor(minority) + age | regnvn,
        subset(phc09, birthyr > 1945 & birthyr < 1994 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn2009)), tex = T)

etable(list(
  feols(work ~ as.factor(female)/log(tot_bomb) + yrschool + nchild + married + as.factor(minority) + age | regnvn,
        subset(phc89, birthyr > 1925 & birthyr < 1974 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(female)/log(tot_bomb) + yrschool + nchild + married + as.factor(minority) + age | regnvn,
        subset(phc99, birthyr > 1935 & birthyr < 1984 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(female)/log(tot_bomb) + yrschool + nchild + married + as.factor(minority) + age | regnvn,
        subset(phc09, birthyr > 1945 & birthyr < 1994 & migration == 0),
        weights = ~perwt,
        vcov = ~geo1_vn2009)), tex = T)


# 1989

png("flfp_bombs_89.png")
iplot(feols(work ~ i(age_cohort, tot_killed) + yrschool + nchild + married + as.factor(minority) | geo1_vn1989 + age_cohort,
            subset(phc89, female == 1 & birthyr > 1925 & birthyr < 1974 & migration == 0),
            weights = ~perwt,
            vcov = ~geo1_vn1989), main = " ", xlab = "")
dev.off()

# 1999
png("flfp_bombs_99.png")
iplot(feols(work ~ i(age_cohort, log(tot_bomb_per)) + yrschool + nchild + married + as.factor(minority) | geo1_vn1999,
            subset(phc99, female == 1 & birthyr > 1935 & birthyr < 1984 & migration == 0),
            weights = ~perwt,
            vcov = ~geo1_vn1999), main = " ")
dev.off()

# 2009

png("flfp_bombs_09.png")
iplot(feols(work ~ i(age_cohort, log(tot_bomb_per)) + yrschool + nchild + married + as.factor(minority) | geo1_vn2009,
            subset(phc09, female == 1 & birthyr > 1945 & birthyr < 1994 & migration == 0),
            weights = ~perwt,
            vcov = ~geo1_vn2009), main = " ")
dev.off()
