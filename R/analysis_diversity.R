
# prepare data frames -----------------------------------------------------


# compute diversity indices
graminoid_diversity_ind <- site_data %>% 
  mutate(H = diversity(graminoid_data[, SppName_gram]),  # Shannon's index
         S = specnumber(graminoid_data[, SppName_gram]), # number of spp
         J = H/log(S)) %>%                               # Pielou's evenness
  group_by(year, ring, co2) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)), H, S, J) %>% 
  ungroup()


forb_diversity_ind <- site_data %>% 
  mutate(H = diversity(forb_data[ , SppName_forb]),
         S = specnumber(forb_data[ , SppName_forb]),
         J = H/log(S)) %>% 
  group_by(year, ring, co2) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)), H, S, J) %>% 
  ungroup()



# Move Year0 value to a new column to be used as covariate for the analysis
diversity_list <- list(graminoid = graminoid_diversity_ind, forb = forb_diversity_ind)


diversity_year0_list <- llply(diversity_list, function(x){
  
  DivDF_mlt <- gather(x, variable, value, H, S, J)
  
  DivDF_year0 <- DivDF_mlt %>% # Year0
    filter(year == "Year0") %>%
    select(ring, value, variable) %>%
    rename(value0 = value) %>% 
    left_join(filter(DivDF_mlt, year != "Year0"), by = c("ring", "variable")) %>% 
    filter(!is.na(value))
  
  return(DivDF_year0)
})

graminoid_diversity <- diversity_year0_list$graminoid
forb_diversity      <- diversity_year0_list$forb




# analysis ----------------------------------------------------------------


# Graminoid -------------------------------------------------------------

graminoid_h <- filter(graminoid_diversity, variable == "H")
graminoid_j <- filter(graminoid_diversity, variable == "J")
graminoid_s <- filter(graminoid_diversity, variable == "S")


# . H ---------------------------------------------------------------------


# model
gr_h_m1 <- lmer(value ~ co2 * year + value0 + (1|ring), data = graminoid_h)


# model diagnosis
plot(gr_h_m1)
qqPlot(residuals(gr_h_m1))


# F test
Anova(gr_h_m1, test.statistic = "F")


# Pairwise comparisons and 95% confidence intervals for covariate-adjusted means
gr_h_lsmean <- lsmeans::lsmeans(gr_h_m1, ~ co2 | year)
summary(pairs(gr_h_lsmean)[1:3], adjust = "none")
summary(gr_h_lsmean)


# CO2 response ratios (RR) on covariate-adjusted means
data.frame(summary(gr_h_lsmean)) %>% 
  group_by(co2) %>% 
  summarise(value = mean(lsmean)) %>%
  summarise(rr = value[co2 == "elev"] / value[co2 == "amb"] - 1)





# . J -----------------------------------------------------------------------

# model
gr_j_m1 <- lmer(value ~ co2 * year + value0 + (1|ring), data = graminoid_j)


# model diagnosis
plot(gr_j_m1)
qqPlot(residuals(gr_j_m1))


# F test
Anova(gr_j_m1, test.statistic = "F")


# 95% confidence intervals for covariate-adjusted means
gr_j_lsmean <- lsmeans::lsmeans(gr_j_m1, ~ co2 | year)
summary(gr_j_lsmean)


# CO2 response ratios (RR) on covariate-adjusted means
data.frame(summary(gr_j_lsmean)) %>% 
  group_by(co2) %>% 
  summarise(value = mean(lsmean)) %>%
  summarise(rr = value[co2 == "elev"] / value[co2 == "amb"] - 1)




# . S ---------------------------------------------------------------------


# model
gr_s_m1 <- lmer(sqrt(value) ~ co2 * year + sqrt(value0) + (1|ring), data = graminoid_s)


# model diagnosis
plot(gr_s_m1)
qqPlot(residuals(gr_s_m1))


# F test
Anova(gr_s_m1, test.statistic = "F")


# 95% confidence intervals for covariate-adjusted means
gr_s_lsmean <- lsmeans::lsmeans(gr_s_m1, ~ co2 | year, type = "response")
summary(gr_s_lsmean)


# CO2 response ratios (RR) on covariate-adjusted means
data.frame(summary(gr_s_lsmean)) %>% 
  group_by(co2) %>% 
  summarise(value = mean(response)) %>%
  summarise(rr = value[co2 == "elev"] / value[co2 == "amb"] - 1)




# Forb --------------------------------------------------------------------


forb_h <- filter(forb_diversity, variable == "H")
forb_j <- filter(forb_diversity, variable == "J")
forb_s <- filter(forb_diversity, variable == "S")


# . H ---------------------------------------------------------------------


# model
fo_h_m1 <- lmer(value ~ co2 * year + value0 + (1|ring), data = forb_h)


# model diagnosis
plot(fo_h_m1)
qqPlot(residuals(fo_h_m1))


# F test
Anova(fo_h_m1, test.statistic = "F")


# 95% confidence intervals for covariate-adjusted means
fo_h_lsmean <- lsmeans::lsmeans(fo_h_m1, ~ co2 | year)
summary(fo_h_lsmean)


# CO2 response ratios (RR) on covariate-adjusted means
data.frame(summary(fo_h_lsmean)) %>% 
  group_by(co2) %>% 
  summarise(value = mean(lsmean)) %>%
  summarise(rr = value[co2 == "elev"] / value[co2 == "amb"] - 1)


# . pre- & post-CO2 ------------------------------------------------------


# pre-CO2
fo_h_pre_m1  <- lm(H ~ co2, data = forb_diversity_ind, subset = year == "Year0")
plot(fo_h_pre_m1)
Anova(fo_h_pre_m1, test.statistic = "F")


# post-CO2
fo_h_post_m1 <- lmer(H ~ co2 * year + (1|ring), data = forb_diversity_ind, subset = year != "Year0")
plot(fo_h_post_m1)
qqPlot(residuals(fo_h_post_m1))
Anova(fo_h_post_m1, test.statistic = "F")




# . J -----------------------------------------------------------------------


# model
fo_j_m1 <- lmer(value ~ co2 * year + value0 + (1|ring), data = forb_j)


# model diagnosis
plot(fo_j_m1)
qqPlot(residuals((fo_j_m1)))


# F test
Anova(fo_j_m1, test.statistic = "F")


# 95% confidence intervals for covariate-adjusted means
fo_j_lsmean <- lsmeans::lsmeans(fo_j_m1, ~ co2 | year)
summary(fo_j_lsmean)


# CO2 response ratios (RR) on covariate-adjusted means
data.frame(summary(fo_j_lsmean)) %>% 
  group_by(co2) %>% 
  summarise(value = mean(lsmean)) %>%
  summarise(rr = value[co2 == "elev"] / value[co2 == "amb"] - 1)



# . pre- & post-CO2 ------------------------------------------------------


# pre-CO2
fo_j_pre_m1  <- lm(J ~ co2, data = forb_diversity_ind, subset = year == "Year0")
plot(fo_j_pre_m1)
Anova(fo_j_pre_m1, test.statistic = "F")


# post-CO2
fo_j_post_m1 <- lmer(J ~ co2 * year + (1|ring), data = forb_diversity_ind, subset = year != "Year0")
plot(fo_j_post_m1)
qqPlot(residuals(fo_j_post_m1))
Anova(fo_j_post_m1, test.statistic = "F")





# . S ---------------------------------------------------------------------


# model
fo_s_m1 <- lmer(value ~ co2 * year + value0 + (1|ring), data = forb_s)


# model diagnosis
plot(fo_s_m1)
qqPlot(residuals(fo_s_m1))


# F test
Anova(fo_s_m1, test.statistic = "F")


# 95% confidence intervals for covariate-adjusted means
fo_s_lsmean <- lsmeans::lsmeans(fo_s_m1, ~ co2 | year)
summary(fo_s_lsmean)


# CO2 response ratios (RR) on covariate-adjusted means
data.frame(summary(fo_s_lsmean)) %>% 
  group_by(co2) %>% 
  summarise(value = mean(lsmean)) %>%
  summarise(rr = value[co2 == "elev"] / value[co2 == "amb"] - 1)


# . pre- & post-CO2 ------------------------------------------------------

# pre-CO2
fo_s_pre_m1  <- lm(S ~ co2, data = forb_diversity_ind, subset = year == "Year0")
plot(fo_s_pre_m1)
Anova(fo_s_pre_m1, test.statistic = "F")


# post-CO2
fo_s_post_m1 <- lmer(S ~ co2 * year + (1|ring), data = forb_diversity_ind, subset = year != "Year0")
plot(fo_s_post_m1)
qqPlot(residuals(fo_s_post_m1))
Anova(fo_s_post_m1, test.statistic = "F")

