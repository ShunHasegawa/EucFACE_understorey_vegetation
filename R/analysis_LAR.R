
# prepare data frame ------------------------------------------------------

# Compute total log annual rate of chnage (LAR) in C3 and C4 graminoids for
# dominant and subordiante spp, and merge with environmental variables

env_data <- env_data %>% 
  mutate(ring = as.character(ring), 
         plot = as.character(plot))


# compute loag annual rates of change for each group
lar_data <- graminoid_DS %>%
  mutate(type_pfg = paste(type, pfg, sep = "_")) %>% 
  select(-pfg, -type) %>% 
  arrange(variable, ring, plot, year) %>%
  mutate(value = value + 1) %>%                                              # some speceis were absent, so add 1
  group_by(co2, ring, plot, type_pfg, variable) %>%  
  mutate(lar = log(value / lag(value, 1))) %>%                               # comput LAR for each species
  filter(year != "Year0") %>%
  group_by(year, co2, ring, plot, type_pfg) %>% 
  summarise(total_lar = sum(lar)) %>%                                        # compute total LAR for each type_pfg
  ungroup() %>% 
  spread(key = type_pfg, value = total_lar) %>% 
  rename(lar_dc3 = D_c3, lar_sc3 = S_c3, lar_dc4 = D_c4, lar_sc4 = S_c4) %>% 
  left_join(env_data) %>% 
  mutate(s_lar_dc3  = scale(lar_dc3)[, 1],                                   # Z-standardize numeric variables
         s_lar_sc3  = scale(lar_sc3)[, 1],
         s_lar_dc4  = scale(lar_dc4)[, 1],
         s_lar_sc4  = scale(lar_sc4)[, 1],
         s_logmoist = scale(log(Moist))[, 1],
         s_temp     = scale(Temp)[, 1],
         s_logpar   = scale(log(PAR))[, 1])




# analysis  ------------------------------------------------------------

options(na.action = "na.fail") # chnage na.action setting to use MuMIn::dredge
xvars <- c("s_logpar", "s_logmoist", "s_temp", "co2")




# > dominant c3  -------------------------------------------------------------


# test interactions
lar_dc3_m1 <- lmer(s_lar_dc3 ~ co2 * (s_logmoist + s_temp + s_logpar) + 
                     (1|ring) + (1|ring:year) + (1|ring:plot), data = lar_data)
plot(lar_dc3_m1)
qqPlot(resid(lar_dc3_m1))
mcp.fnc(lar_dc3_m1)
oldf_dc3 <- romr.fnc(lar_dc3_m1, data = data.frame(lar_data))
dplyr::setdiff(oldf_dc3$data0, oldf_dc3$data)
# some outliers are suggested, so remove them
lar_dc3_m2 <- update(lar_dc3_m1, data = oldf_dc3$data)
plot(lar_dc3_m2)
qqPlot(resid(lar_dc3_m2))
lar_dc3_m1_full <- dredge(lar_dc3_m1, fixed = c("co2", "s_logmoist", "s_temp",  "s_logpar"))
lar_dc3_m2_full <- dredge(lar_dc3_m2, fixed = c("co2", "s_logmoist", "s_temp",  "s_logpar"))
lar_dc3_m1_full
lar_dc3_m2_full
# no interaction is indicated either way


# test main effects
lar_dc3_m3 <- lmer(s_lar_dc3 ~ co2 + s_logmoist+s_temp + s_logpar +
                     (1|ring) + (1|ring:year) + (1|ring:plot), data = lar_data)
lar_dc3_m4 <- lmer(s_lar_dc3 ~ co2 + s_logmoist+s_temp + s_logpar +
                     (1|ring) + (1|ring:year) + (1|ring:plot), data = oldf_dc3$data)


# model diagnosis
plot(lar_dc3_m3)
qqPlot(resid(lar_dc3_m3))
plot(lar_dc3_m4)
qqPlot(resid(lar_dc3_m4))

summary(lar_dc3_m3)
summary(lar_dc3_m4)
# no difference so present the first one without removing the outliers


# get 95% and 90% CI for coefficients
summary(lar_dc3_m3)
r.squaredGLMM(lar_dc3_m3)
confint(lar_dc3_m3, method = "boot", nsim = 999)
confint(lar_dc3_m3, method = "boot", level = .9, nsim = 999)


# get relative importance of each predictor from a models set composed of models
# with all possible combination of each predictor
lar_dc3_m3_all <- dredge(lar_dc3_m3, REML = F)
lar_dc3_m3_all
importance(lar_dc3_m3_all)
# ensure the total number of occurence of each predictor is equal


# partial regression plot
par(mfrow = c(2, 2))
l_ply(xvars, function(x) visreg(lar_dc3_m3, xvar = x))




# > subordinate c3  -------------------------------------------------------------


# test interactions
lar_sc3_m1 <- lmer(s_lar_sc3 ~ co2 * (s_logmoist + s_temp + s_logpar) + 
                     (1|ring) + (1|ring:year) + (1|ring:plot), data = lar_data)
plot(lar_sc3_m1)
qqPlot(resid(lar_sc3_m1))
lar_sc3_m1_full <- dredge(lar_sc3_m1, fixed = c("co2", "s_logmoist", "s_temp",  "s_logpar"))
lar_sc3_m1_full
# no interaction is indicated either way


# test main effects
lar_sc3_m2 <- lmer(s_lar_sc3 ~ co2 + s_logmoist+s_temp + s_logpar +
                     (1|ring) + (1|ring:year) + (1|ring:plot), data = lar_data)


# model diagnosis
plot(lar_sc3_m2)
qqPlot(resid(lar_sc3_m2))


# get 95% and 90% CI for coefficients
summary(lar_sc3_m2)
r.squaredGLMM(lar_sc3_m2)
confint(lar_sc3_m2, method = "boot", nsim = 999)
confint(lar_sc3_m2, method = "boot", level = .9, nsim = 999)


# get relative importance of each predictor from a models set composed of models
# with all possible combination of each predictor
lar_sc3_m2_all <- dredge(lar_sc3_m2, REML = F)
lar_sc3_m2_all
importance(lar_sc3_m2_all)
# ensure the total number of occurence of each predictor is equal


# partial regression plot
par(mfrow = c(2, 2))
l_ply(xvars, function(x) visreg(lar_sc3_m2, xvar = x))




# > dominant c4  -------------------------------------------------------------


# test interactions
lar_dc4_m1 <- lmer(s_lar_dc4 ~ co2 * (s_logmoist + s_temp + s_logpar) + 
                     (1|ring) + (1|ring:year) + (1|ring:plot), data = lar_data)
lar_dc4_m1_full <- dredge(lar_dc4_m1, fixed = c("co2", "s_logmoist", "s_temp",  "s_logpar"))
lar_dc4_m1_full
plot(lar_dc4_m1)
qqPlot(resid(lar_dc4_m1))
# no interaction is suggested


# test main effects
lar_dc4_m2 <- lmer(s_lar_dc4 ~ co2 + s_logmoist+s_temp + s_logpar +
                     (1|ring) + (1|ring:year) + (1|ring:plot), data = lar_data)


# model diagnosis
plot(lar_dc4_m2)
qqPlot(resid(lar_dc4_m2))


# get 95% and 90% CI for coefficients
summary(lar_dc4_m2)
r.squaredGLMM(lar_dc4_m2)
confint(lar_dc4_m2, method = "boot", nsim = 999)
confint(lar_dc4_m2, method = "boot", level = .9, nsim = 999)


# get relative importance of each predictor from a models set composed of models
# with all possible combination of each predictor
lar_dc4_m2_all <- dredge(lar_dc4_m2, REML = F)
lar_dc4_m2_all
importance(lar_dc4_m2_all)
# ensure the total number of occurence of each predictor is equal


# partial regression plot
par(mfrow = c(2, 2))
l_ply(xvars, function(x) visreg(lar_dc4_m2, xvar = x))




# > subordinate c4  -------------------------------------------------------------


# test interactions
lar_sc4_m1 <- lmer(s_lar_sc4 ~ co2 * (s_logmoist + s_temp + s_logpar) + 
                     (1|ring) + (1|ring:year) + (1|ring:plot), data = lar_data)
lar_sc4_m1_full <- dredge(lar_sc4_m1, fixed = c("co2", "s_logmoist", "s_temp",  "s_logpar"))
lar_sc4_m1_full
plot(lar_sc4_m1)
qqPlot(resid(lar_sc4_m1))
# no interaction is suggested

# test main effects
lar_sc4_m2 <- lmer(s_lar_sc4 ~ co2 + s_logmoist+s_temp + s_logpar +
                     (1|ring) + (1|ring:year) + (1|ring:plot), data = lar_data)


# model diagnosis
plot(lar_sc4_m2)
qqPlot(resid(lar_sc4_m2))


# get 95% and 90% CI for coefficients
summary(lar_sc4_m2)
r.squaredGLMM(lar_sc4_m2)
confint(lar_sc4_m2, method = "boot", nsim = 999)
confint(lar_sc4_m2, method = "boot", level = .9, nsim = 999)


# get relative importance of each predictor from a models set composed of models
# with all possible combination of each predictor
lar_sc4_m2_all <- dredge(lar_sc4_m2, REML = F)
lar_sc4_m2_all
importance(lar_sc4_m2_all)
# ensure the total number of occurence of each predictor is equal


# partial regression plot
par(mfrow = c(2, 2))
l_ply(xvars, function(x) visreg(lar_sc4_m2, xvar = x))

