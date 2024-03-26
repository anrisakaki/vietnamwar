phc <- c("phc89.Rda", "phc99.Rda", "phc09.Rda")

for (i in phc) {
  load(i)
}

# Individual-level regressions 

etable(list(
  feols(work ~ as.factor(female)/log(tot_bomb) + yrschool + nchild + married + as.factor(minority) + age + age^2 | regnvn,
        phc89,
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(female)/log(tot_bomb) + yrschool + nchild + married + as.factor(minority) + age + age^2 | regnvn,
        phc99,
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(female)/log(tot_bomb) + yrschool + nchild + married + as.factor(minority) + age + age^2| regnvn,
        phc09,
        vcov = ~geo1_vn2009)
), tex = T)

etable(list(
  feols(work ~ as.factor(female)/log(tot_bomb) + yrschool + nchild + married + as.factor(minority) + age + age^2 | regnvn,
        subset(phc89, south == 0),
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(female)/log(tot_bomb) + yrschool + nchild + married + as.factor(minority) + age + age^2 | regnvn,
        subset(phc99, south == 0),
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(female)/log(tot_bomb) + yrschool + nchild + married + as.factor(minority) + age + age^2| regnvn,
        subset(phc09, south == 0),
        vcov = ~geo1_vn2009),
  feols(work ~ as.factor(female)/log(tot_bomb) + yrschool + nchild + married + as.factor(minority) + age + age^2 | regnvn,
        subset(phc89, south == 1),
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(female)/log(tot_bomb) + yrschool + nchild + married + as.factor(minority) + age + age^2 | regnvn,
        subset(phc99, south == 1),
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(female)/log(tot_bomb) + yrschool + nchild + married + as.factor(minority) + age + age^2| regnvn,
        subset(phc09, south == 1),
        vcov = ~geo1_vn2009)
), tex = T)

etable(list(
  feols(work ~ as.factor(female)/log(tot_killed) + yrschool + nchild + married + as.factor(minority) + age + age^2 | regnvn,
        phc89,
        vcov = ~geo1_vn1989),
  feols(work ~ as.factor(female)/log(tot_killed) + yrschool + nchild + married + as.factor(minority) + age + age^2 | regnvn,
        phc99,
        vcov = ~geo1_vn1999),
  feols(work ~ as.factor(female)/log(tot_killed) + yrschool + nchild + married + as.factor(minority) + age + age^2| regnvn,
        phc09,
        vcov = ~geo1_vn2009)
), tex = T)
