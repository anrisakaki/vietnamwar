iplot(feols(v511 ~ i(age_75, tot_bmr_prov_std) + as.factor(v131) + v133 + as.factor(v025) | age_75,
             dhs97,
             weights = ~v005,
             vcov = ~sprovin))

iplot(feols(v511 ~ i(age_75, tot_bmr_prov_std) + as.factor(v025) + v107 +as.factor(v131) | age_75,
            dhs02,
            weights = ~v005,
            vcov = ~sprovin))

etable(feols(spouse_age_diff ~ tot_bmr_prov_std + as.factor(v131) + v133 + as.factor(v025) + as.factor(v130) | v012,
             subset(dhs97, v012 > (1997-(1975-15))),
             weights = ~v005,
             vcov = ~sprovin))
