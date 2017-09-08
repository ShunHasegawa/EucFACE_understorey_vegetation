
# prepare data frame ------------------------------------------------------

# merge soil nutrient data and ring means of C4:C3 ratio data
soil_data <- soil_data %>% 
  mutate(ring = as.character(ring),
         nitrogen = nitrate + ammonium)

c43r_soi <- graminoid_pfg_df %>% 
  group_by(year, co2, ring, pfg) %>% 
  summarise(value = sum(value)) %>%
  ungroup() %>% 
  spread(pfg, value) %>% 
  mutate(c43_r = c4 / c3) %>% 
  left_join(soil_data) %>% 
  mutate(s_c43_r  = scale(log(c43_r + .1))[, 1],    # Z-standardize for multiple regression
         s_n      = scale(log(nitrogen))[, 1],
         s_p      = scale(log(phosphate))[, 1])
  



# anlaysis ----------------------------------------------------------------


plot(s_c43_r ~ s_n, c43r_soi)
plot(s_c43_r ~ s_p, c43r_soi)
c43_soil_m1 <- lmer(s_c43_r ~ s_n + s_p + (1|ring) + (1|year), data = c43r_soi)


# model diagnosis
plot(c43_soil_m1)
qqPlot(resid(c43_soil_m1))


# get 95% and 90% CI for coefficients
summary(c43_soil_m1)
r.squaredGLMM(c43_soil_m1)
confint(c43_soil_m1, method = "boot", nsim = 999)
confint(c43_soil_m1, method = "boot", level = .9, nsim = 999)


# get relative importance of each predictor from a models set composed of models
# with all possible combination of each predictor
c43_soil_m1_all <- dredge(c43_soil_m1, REML = F)
c43_soil_m1_all
importance(c43_soil_m1_all)
  # ensure the total number of occurence of each predictor is equal


# partial regression plot
par(mfrow = c(1, 2))
l_ply(c("s_n", "s_p"), function(x) visreg(c43_soil_m1, xvar = x))
