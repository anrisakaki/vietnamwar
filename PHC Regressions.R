setFixest_coefplot(zero.par = list( type="dotted", lty=2))

# 1989

iplot(feols(work ~ i(birthyr) + as.factor(minority) + yrschool + nchild + married | geo1_vn,
            subset(phc89, female == 1 & birthyr > 1925 & birthyr < 1974 & migration == 0),
            weights = ~perwt), xlab = "Birth Year", main = "")

iplot(feols(work ~ i(birthyr) + as.factor(minority) + yrschool + as.factor(geo1_vn) + nchild + married,
            subset(phc89, female == 0 & birthyr > 1925 & birthyr < 1974),
            weights = ~perwt), xlab = "Birth Year", main = "")

iplot(feols(work ~ i(age75, log_tot_bomb) + as.factor(minority) + yrschool + nchild + married,
            subset(phc89, female == 1 & birthyr > 1925 & birthyr < 1974 & migration == 0),
            weights = ~perwt), xlab = "Age in 1975", main = "")

iplot(feols(work ~ i(age75, log_tot_bomb) + as.factor(minority) + yrschool  + nchild + married,
            subset(phc89, female == 0 & birthyr > 1925 & birthyr < 1974 & migration == 0),
            weights = ~perwt), xlab = "Age in 1975", main = "")

# 1999

iplot(feols(work ~ i(birthyr) + as.factor(minority) + yrschool + as.factor(geo1_vn) + nchild + married,
            subset(phc99, female == 1 & birthyr > 1935 & birthyr < 1984),
            weights = ~perwt), xlab = "Birth Year", main = "")

iplot(feols(work ~ i(birthyr) + as.factor(minority) + yrschool + as.factor(geo1_vn) + nchild + married,
            subset(phc99, female == 0 & birthyr > 1935 & birthyr < 1984),
            weights = ~perwt), xlab = "Birth Year", main = "")

iplot(feols(work ~ i(age75, log_tot_bomb) + as.factor(minority) + yrschool  + nchild + married,
            subset(phc89, female == 1 & birthyr > 1935 & birthyr < 1984),
            weights = ~perwt), xlab = "Age in 1975", main = "")

iplot(feols(work ~ i(age75, log_tot_bomb) + as.factor(minority) + yrschool  + nchild + married,
            subset(phc89, female == 0 & birthyr > 1935 & birthyr < 1984),
            weights = ~perwt), xlab = "Age in 1975", main = "")

# 2009

iplot(feols(work ~ i(birthyr) + as.factor(minority) + yrschool + as.factor(geo1_vn) + nchild + married,
            subset(phc99, female == 1 & birthyr > 1945 & birthyr < 1994),
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
