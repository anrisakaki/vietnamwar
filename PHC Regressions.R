phc <- c("phc89.Rda", "phc99.Rda", "phc09.Rda")

for (i in phc) {
  load(i)
}


# PHC Regressions 

## Individual-level regressions 

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

## Occupation 

occisco_prov_sum <- left_join(occisco_prov_sum, regvn, by = c("geo1_vn1999"))

etable(list(
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(occisco_prov_sum, Occupation == "Legislators, senior officials & managers"),
        vcov = ~geo1_vn1999),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(occisco_prov_sum, Occupation == "Professionals"),
        vcov = ~geo1_vn1999),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(occisco_prov_sum, Occupation == "Technicians & associate professionals"),
        vcov = ~geo1_vn1999),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(occisco_prov_sum, Occupation == "Clerks"),
        vcov = ~geo1_vn1999),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(occisco_prov_sum, Occupation == "Service workers"),
        vcov = ~geo1_vn1999),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(occisco_prov_sum, Occupation == "Skilled agricultural & fishery workers"),
        vcov = ~geo1_vn1999),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(occisco_prov_sum, Occupation == "Crafts & related trades workers"),
        vcov = ~geo1_vn1999),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(occisco_prov_sum, Occupation == "Plant & machine operators"),
        vcov = ~geo1_vn1999),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(occisco_prov_sum, Occupation == "Elementary occupations"),
        vcov = ~geo1_vn1999)
), tex = T)

etable(list(
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(occisco_prov_sum, Occupation == "Legislators, senior officials & managers" & south == 0),
        vcov = ~geo1_vn1999),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(occisco_prov_sum, Occupation == "Professionals" & south == 0),
        vcov = ~geo1_vn1999),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(occisco_prov_sum, Occupation == "Technicians & associate professionals" & south == 0),
        vcov = ~geo1_vn1999),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(occisco_prov_sum, Occupation == "Clerks" & south == 0),
        vcov = ~geo1_vn1999),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(occisco_prov_sum, Occupation == "Service workers" & south == 0),
        vcov = ~geo1_vn1999),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(occisco_prov_sum, Occupation == "Skilled agricultural & fishery workers" & south == 0),
        vcov = ~geo1_vn1999),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(occisco_prov_sum, Occupation == "Crafts & related trades workers" & south == 0),
        vcov = ~geo1_vn1999),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(occisco_prov_sum, Occupation == "Plant & machine operators" & south == 0),
        vcov = ~geo1_vn1999),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(occisco_prov_sum, Occupation == "Elementary occupations" & south == 0),
        vcov = ~geo1_vn1999)
), tex = T)

etable(list(
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(occisco_prov_sum, Occupation == "Legislators, senior officials & managers" & south == 1),
        vcov = ~geo1_vn1999),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(occisco_prov_sum, Occupation == "Professionals" & south == 1),
        vcov = ~geo1_vn1999),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(occisco_prov_sum, Occupation == "Technicians & associate professionals" & south == 1),
        vcov = ~geo1_vn1999),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(occisco_prov_sum, Occupation == "Clerks" & south == 1),
        vcov = ~geo1_vn1999),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(occisco_prov_sum, Occupation == "Service workers" & south == 1),
        vcov = ~geo1_vn1999),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(occisco_prov_sum, Occupation == "Skilled agricultural & fishery workers" & south == 1),
        vcov = ~geo1_vn1999),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(occisco_prov_sum, Occupation == "Crafts & related trades workers" & south == 1),
        vcov = ~geo1_vn1999),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(occisco_prov_sum, Occupation == "Plant & machine operators" & south == 1),
        vcov = ~geo1_vn1999),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(occisco_prov_sum, Occupation == "Elementary occupations" & south == 1),
        vcov = ~geo1_vn1999)
), tex = T)

etable(list(
  feols(workerratio ~ log(tot_killed) | regnvn,
        subset(occisco_prov_sum, Occupation == "Legislators, senior officials & managers"),
        vcov = ~geo1_vn1999),
  feols(workerratio ~ log(tot_killed) | regnvn,
        subset(occisco_prov_sum, Occupation == "Professionals"),
        vcov = ~geo1_vn1999),
  feols(workerratio ~ log(tot_killed) | regnvn,
        subset(occisco_prov_sum, Occupation == "Technicians & associate professionals"),
        vcov = ~geo1_vn1999),
  feols(workerratio ~ log(tot_killed) | regnvn,
        subset(occisco_prov_sum, Occupation == "Clerks"),
        vcov = ~geo1_vn1999),
  feols(workerratio ~ log(tot_killed) | regnvn,
        subset(occisco_prov_sum, Occupation == "Service workers"),
        vcov = ~geo1_vn1999),
  feols(workerratio ~ log(tot_killed) | regnvn,
        subset(occisco_prov_sum, Occupation == "Skilled agricultural & fishery workers"),
        vcov = ~geo1_vn1999),
  feols(workerratio ~ log(tot_killed) | regnvn,
        subset(occisco_prov_sum, Occupation == "Crafts & related trades workers"),
        vcov = ~geo1_vn1999),
  feols(workerratio ~ log(tot_killed) | regnvn,
        subset(occisco_prov_sum, Occupation == "Plant & machine operators"),
        vcov = ~geo1_vn1999),
  feols(workerratio ~ log(tot_killed) | regnvn,
        subset(occisco_prov_sum, Occupation == "Elementary occupations"),
        vcov = ~geo1_vn1999)
), tex = T)

## Industry 

indgen_prov_sum <- merge(indgen_prov_sum, regvn, by = "geo1_vn1989")

etable(list(
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(indgen_prov_sum, Industry == "Agriculture"),
        vcov = ~geo1_vn1989),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(indgen_prov_sum, Industry == "Manufacturing"),
        vcov = ~geo1_vn1989),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(indgen_prov_sum, Industry == "Construction"),
        vcov = ~geo1_vn1989),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(indgen_prov_sum, Industry == "Business services and real estate"),
        vcov = ~geo1_vn1989),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(indgen_prov_sum, Industry == "Education"),
        vcov = ~geo1_vn1989),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(indgen_prov_sum, Industry == "Wholesale and retail trade"),
        vcov = ~geo1_vn1989),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(indgen_prov_sum, Industry == "Hotels and restaurants"),
        vcov = ~geo1_vn1989),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(indgen_prov_sum, Industry == "Transportation"),
        vcov = ~geo1_vn1989),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(indgen_prov_sum, Industry == "Financial services and insurance"),
        vcov = ~geo1_vn1989)
), tex = T)

etable(list(
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(indgen_prov_sum, Industry == "Agriculture" & south == 0),
        vcov = ~geo1_vn1989),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(indgen_prov_sum, Industry == "Manufacturing" & south == 0),
        vcov = ~geo1_vn1989),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(indgen_prov_sum, Industry == "Construction" & south == 0),
        vcov = ~geo1_vn1989),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(indgen_prov_sum, Industry == "Business services and real estate" & south == 0),
        vcov = ~geo1_vn1989),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(indgen_prov_sum, Industry == "Education" & south == 0),
        vcov = ~geo1_vn1989),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(indgen_prov_sum, Industry == "Wholesale and retail trade" & south == 0),
        vcov = ~geo1_vn1989),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(indgen_prov_sum, Industry == "Hotels and restaurants" & south == 0),
        vcov = ~geo1_vn1989),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(indgen_prov_sum, Industry == "Transportation" & south == 0),
        vcov = ~geo1_vn1989),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(indgen_prov_sum, Industry == "Financial services and insurance" & south == 0),
        vcov = ~geo1_vn1989)
), tex = T)

etable(list(
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(indgen_prov_sum, Industry == "Agriculture" & south == 1),
        vcov = ~geo1_vn1989),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(indgen_prov_sum, Industry == "Manufacturing" & south == 1),
        vcov = ~geo1_vn1989),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(indgen_prov_sum, Industry == "Construction" & south == 1),
        vcov = ~geo1_vn1989),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(indgen_prov_sum, Industry == "Business services and real estate" & south == 1),
        vcov = ~geo1_vn1989),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(indgen_prov_sum, Industry == "Education" & south == 1),
        vcov = ~geo1_vn1989),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(indgen_prov_sum, Industry == "Wholesale and retail trade" & south == 1),
        vcov = ~geo1_vn1989),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(indgen_prov_sum, Industry == "Hotels and restaurants" & south == 1),
        vcov = ~geo1_vn1989),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(indgen_prov_sum, Industry == "Transportation" & south == 1),
        vcov = ~geo1_vn1989),
  feols(workerratio ~ log(tot_bomb) | regnvn,
        subset(indgen_prov_sum, Industry == "Financial services and insurance" & south == 1),
        vcov = ~geo1_vn1989)
), tex = T)

etable(list(
  feols(workerratio ~ log(tot_killed) | regnvn,
        subset(indgen_prov_sum, Industry == "Agriculture"),
        vcov = ~geo1_vn1989),
  feols(workerratio ~ log(tot_killed) | regnvn,
        subset(indgen_prov_sum, Industry == "Manufacturing"),
        vcov = ~geo1_vn1989),
  feols(workerratio ~ log(tot_killed) | regnvn,
        subset(indgen_prov_sum, Industry == "Construction"),
        vcov = ~geo1_vn1989),
  feols(workerratio ~ log(tot_killed) | regnvn,
        subset(indgen_prov_sum, Industry == "Business services and real estate"),
        vcov = ~geo1_vn1989),
  feols(workerratio ~ log(tot_killed) | regnvn,
        subset(indgen_prov_sum, Industry == "Education"),
        vcov = ~geo1_vn1989),
  feols(workerratio ~ log(tot_killed) | regnvn,
        subset(indgen_prov_sum, Industry == "Wholesale and retail trade"),
        vcov = ~geo1_vn1989),
  feols(workerratio ~ log(tot_killed) | regnvn,
        subset(indgen_prov_sum, Industry == "Hotels and restaurants"),
        vcov = ~geo1_vn1989),
  feols(workerratio ~ log(tot_killed) | regnvn,
        subset(indgen_prov_sum, Industry == "Transportation"),
        vcov = ~geo1_vn1989),
  feols(workerratio ~ log(tot_killed) | regnvn,
        subset(indgen_prov_sum, Industry == "Financial services and insurance"),
        vcov = ~geo1_vn1989)
), tex = T)
