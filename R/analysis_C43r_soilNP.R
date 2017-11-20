
# prepare data frame ------------------------------------------------------

# merge soil nutrient and environmental data with ring means of C4:C3 ratio data
soil_data <- soil_data %>% 
  mutate(ring = as.character(ring),
         year = as.character(year),
         nitrogen = nitrate + ammonium)

env_data <- env_data %>% 
  mutate(ring = as.character(ring),
         year = as.character(year))

ratio_c43 <- ratio_c43 %>% 
  mutate(ring = as.character(ring),
         year = as.character(year))

c43r_soil <- ratio_c43 %>% 
  rename(c43_r = value) %>% 
  left_join(soil_data, by = c("year", "ring")) %>% 
  left_join(env_data, by = c("year", "ring")) %>% 
  mutate(s_c43_r  = scale(log(c43_r))[, 1],    # Z-standardize for multiple regression
         s_n      = scale(log(nitrogen))[, 1],
         s_p      = scale(log(phosphate))[, 1],
         s_moist  = scale(Moist)[, 1],
         s_temp   = scale(Temp)[, 1],
         s_par    = scale(PAR)[, 1]) %>% 
  arrange(year, ring)




# anlaysis ----------------------------------------------------------------


plot(s_c43_r ~ s_n, c43r_soil)
plot(s_c43_r ~ s_p, c43r_soil)


# check multicollinearity
car::vif(lm(s_c43_r ~ s_n + s_p + s_moist + s_temp + s_par, data = c43r_soil))


# model
c43_soil_m1 <- lmer(s_c43_r ~ s_n + s_p + s_moist + s_temp + s_par + (1|ring) + (1|year), data = c43r_soil)


# model diagnosis
plot(c43_soil_m1)
qqPlot(resid(c43_soil_m1))


# get 95% and 90% CI for coefficients
summary(c43_soil_m1)
r.squaredGLMM(c43_soil_m1)
confint(c43_soil_m1, method = "boot", nsim = 999)
confint(c43_soil_m1, method = "boot", nsim = 999, level = .9)


# get relative importance of each predictor from a models set composed of models
# with all possible combination of each predictor
c43_soil_m1_all <- dredge(c43_soil_m1, REML = F)
c43_soil_m1_all
importance(c43_soil_m1_all)
  # ensure the total number of occurence of each predictor is equal


# partial regression plot
par(mfrow = c(1, 2))
l_ply(c("s_n", "s_p"), function(x) visreg(c43_soil_m1, xvar = x))
