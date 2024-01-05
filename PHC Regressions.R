# 1989

iplot(feols(work ~ i(birthyr) + as.factor(minority) + yrschool + as.factor(geo1_vn) + nchild + married,
            subset(phc89, female == 1 & birthyr > 1925 & birthyr < 1974),
            weights = ~perwt), xlab = "Birth Year", main = "")

iplot(feols(work ~ i(birthyr) + as.factor(minority) + yrschool + as.factor(geo1_vn) + nchild + married,
            subset(phc89, female == 0 & birthyr > 1925 & birthyr < 1974),
            weights = ~perwt), xlab = "Birth Year", main = "")

# 1999

iplot(feols(work ~ i(birthyr) + as.factor(minority) + yrschool + as.factor(geo1_vn) + nchild + married,
            subset(phc99, female == 1 & birthyr > 1935 & birthyr < 1984),
            weights = ~perwt), xlab = "Birth Year", main = "")

iplot(feols(work ~ i(birthyr) + as.factor(minority) + yrschool + as.factor(geo1_vn) + nchild + married,
            subset(phc99, female == 0 & birthyr > 1935 & birthyr < 1984),
            weights = ~perwt), xlab = "Birth Year", main = "")
