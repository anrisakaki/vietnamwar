iplot(feols(v511 ~ i(v012, tot_bmr_prov_std) + as.factor(v131) + v133 | v012, dhs97))

etable(feols(v511 ~ tot_bmr_prov_std + as.factor(v131) + v133 + as.factor(v025) + as.factor(v130) | v012,
             subset(dhs97, v012 > (1997-(1975-15))),
             weights = ~v005))
