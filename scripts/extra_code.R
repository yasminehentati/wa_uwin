
simpglobal <- glmmTMB(simpson.di ~ 
                          city + 
                          scale(develop) + 
                          scale(med_income) + 
                          scale(Pct_Units_Lead) +
                          scale(PM2.5_Count) + 
                          scale(Average_PTSDF) + 
                          scale(Average_PNPL) + 
                          scale(Average_RSEI_Concentrations) + 
                          scale(Average_PWDIS) + 
                          scale(Annual_Tons_Km2_Diesel_NOx) + 
                          ( 1 | site), 
                        data = all_dat_veg,
                        family = "gaussian",
                        na.action = "na.fail")
# does not converge

# Grouped exposures/effects model
simpmod2 <- glmmTMB(simpson.di ~ city + scale(develop) + scale(EXPOSURE) + scale(EFFECT)  + 
                     scale(med_income) +  ( 1 | site), 
                   data = all_dat_veg, 
                   family = "gaussian",
                   na.action = "na.fail")
summary(simpmod2)

# separate city models 

simpmod3a <- glmmTMB(simpson.di ~ scale(develop) + scale(EXPOSURE) + scale(EFFECT)  + 
                         scale(med_income) + ( 1 | site), 
                    data = all_dat_sewa_veg, 
                    family = "gaussian")

simpmod3b <- glmmTMB(simpson.di ~ scale(develop) + scale(EXPOSURE) + scale(EFFECT)  + 
                      scale(med_income) + ( 1 | site), 
                    data = all_dat_tawa_veg, 
                    family = "gaussian")



#### Plot 

# Let's plot the top model 

simpmod2plot <- sjPlot::plot_model(simpmod2, title = "Simpsons diversity",, 
                                 #                axis.title = "",
                                 show.values = TRUE, show.p = TRUE) + 
  scale_color_sjplot("circus") + 
  scale_y_log10(limits = c(0.1, 10))
simpmod2plot

