
# prepare data frame ------------------------------------------------------

# reshape graminoid_data to a long-format and combine with plant functional groups

graminoid_pfg_df <- graminoid_data %>% 
  gather(key = variable, value = value, one_of(SppName_gram)) %>% 
  left_join(sp_pfg) %>%
  mutate(ring = as.character(ring), 
         plot = as.character(plot))


# > classify into dominance types: Dominant/subordinate/transient -----------


# dominant or subordinate spp were defnied as those that occurred across all the
# study years (but no need to be in all rings)
ds_spp <- graminoid_pfg_df %>% 
  group_by(variable, year) %>% 
  summarise(value = sum(value)) %>% 
  group_by(variable) %>% 
  summarise(DS = !any(value == 0)) %>% 
  ungroup() %>% 
  filter(DS)


# relative abundance (species with the highest abundance was assigned 1)
relative_abund <- graminoid_pfg_df %>% 
  filter(variable %in% ds_spp$variable) %>% 
  group_by(variable, pfg) %>%
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  mutate(r_abund  = value / max(value),
         variable = factor(variable,
                           levels = variable[order(r_abund, decreasing = TRUE)])) %>% 
  arrange(-r_abund)



# plot in a descending order
ggplot(relative_abund, aes(x = as.numeric(variable), y = r_abund))+
  geom_point() +
  geom_path() +
  scale_x_continuous(breaks = c(seq(1, nrow(relative_abund), 1)),
                     labels =  as.character(relative_abund$variable[order(relative_abund$value, decreasing = TRUE)]))+
  scale_y_continuous(breaks = c(0, .1, seq(.25, 1, .25)), 
                     labels =  format(c(0, .1, seq(.25, 1, .25)), digits = 2))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = NULL, y = "Relative abundance")


# given this figure, set an arbitrary threshold to distinguish dominant and subordinate species at
# relative abundance of 0.1
relative_abund_ds <- relative_abund %>% 
  mutate(type      = ifelse(r_abund > .1, "D", "S"),
         Dominance = mapvalues(type, c("S", "D"), c("Subordinate", "dominant")))

ggplot(relative_abund_ds, aes(x = as.numeric(variable), y = r_abund, label = type))+
  geom_path() +
  geom_text() +
  geom_hline(yintercept = .1, linetype = "dashed") +
  scale_x_continuous(breaks = c(seq(1, nrow(relative_abund), 1)),
                     labels =  as.character(relative_abund$variable[order(relative_abund$value, decreasing = TRUE)]))+
  scale_y_continuous(breaks = c(0, .1, seq(.25, 1, .25)), 
                     labels =  format(c(0, .1, seq(.25, 1, .25)), digits = 2))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = NULL, y = "Relative abundance")




# > prepare df for analysis -------------------------------------------------

graminoid_DS <- graminoid_pfg_df %>% 
  filter(variable %in% ds_spp$variable) %>% 
  left_join(relative_abund_ds[, c("variable", "type")]) %>% 
  droplevels(.)

# df for sum for each group (dominant/subordinate C4/3)
graminoid_DS_sum <- graminoid_DS %>% 
  group_by(year, ring, co2, plot, pfg, type) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  mutate(variable = paste(type, pfg, sep = "_")) %>% 
  select(variable, year, ring, co2, plot, value) %>% 
  group_by(variable, year, ring, co2) %>% 
  summarise(value = mean(value)) %>% 
  ungroup()


# df for subordinate:dominant ratios
graminoid_DS_ratio <- graminoid_DS %>% 
  group_by(year, ring, co2, plot, type) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  spread(key = type, value = value) %>% 
  mutate(value = S / D, 
         variable = "ds_ratio") %>% 
  select(variable, year, ring, co2, plot, value) %>% 
  group_by(variable, year, ring, co2) %>% 
  summarise(value = mean(value)) %>% 
  ungroup()


# data frame for C4:C3 ratios (incl. transient spp)
graminoid_c43_ratio <- graminoid_pfg_df %>% 
  group_by(year, ring, co2, plot, pfg) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  spread(key = pfg, value) %>% 
  mutate(value    = c4 / c3, 
         variable = "c43_ratio") %>% 
  select(variable, year, co2, ring, plot, value) %>% 
  group_by(variable, year, co2, ring) %>% 
  summarise(value = mean(value)) %>% 
  ungroup()


# merge the data frames above
abund_data <- bind_rows(graminoid_DS_sum, graminoid_DS_ratio, graminoid_c43_ratio)


# move value in Year0 to a new column in order to use it as a covariate
abund_data_year0 <- abund_data %>%
  filter(year == "Year0") %>%
  select(ring, value, variable) %>%
  rename(value0 = value) %>% 
  left_join(filter(abund_data, year != "Year0"), by = c("ring", "variable")) 





# analysis ----------------------------------------------------------------


abund_dc3 <- filter(abund_data_year0, variable == "D_c3")
abund_dc4 <- filter(abund_data_year0, variable == "D_c4")
abund_sc3 <- filter(abund_data_year0, variable == "S_c3")
abund_sc4 <- filter(abund_data_year0, variable == "S_c4")
ratio_c43 <- filter(abund_data_year0, variable == "c43_ratio")
ratio_DS  <- filter(abund_data_year0, variable == "ds_ratio")



# > dominant c3 -----------------------------------------------------------


# model
plot(value ~ value0, data = abund_dc3)
dc3_m1 <- lmer(value ~ co2 * year + value0 + (1|ring), abund_dc3)


# model diagnosis
plot(dc3_m1)
qqPlot(resid(dc3_m1))


# F test
Anova(dc3_m1, test.statistic = "F")


# 95% confidence intervals for covariate-adjusted means
dc3_lsmean <- lsmeans::lsmeans(dc3_m1, ~ co2 | year)
dc3_95CI   <- data.frame(summary(dc3_lsmean))
dc3_95CI


# CO2 response ratios (RR) on covariate-adjusted means
dc3_95CI %>% 
  group_by(co2) %>% 
  summarise(value = mean(lsmean)) %>%
  summarise(rr = value[co2 == "elev"] / value[co2 == "amb"] - 1)


# > dominant c4 -----------------------------------------------------------

# model
plot(sqrt(value) ~ sqrt(value0), data = abund_dc4)
dc4_m1 <- lmer(sqrt(value) ~ co2 * year + sqrt(value0) + (1|ring), abund_dc4)


# model diagnosis
plot(dc4_m1)
qqPlot(resid(dc4_m1))


# F test
Anova(dc4_m1, test.statistic = "F")


# 95% confidence intervals for covariate-adjusted means
dc4_lsmean <- lsmeans::lsmeans(dc4_m1, ~ co2 | year, type = "response")
dc4_95CI   <- data.frame(summary(dc4_lsmean))
dc4_95CI


# CO2 response ratios (RR) on covariate-adjusted means
dc4_95CI %>% 
  group_by(co2) %>% 
  summarise(value = mean(response)) %>%
  summarise(rr = value[co2 == "elev"] / value[co2 == "amb"] - 1)



# > subordinate c3 --------------------------------------------------------

# model
plot(log(value + 1)  ~ log(value0 + 1), data = abund_sc3)
sc3_m1 <- lmer(log(value + 1) ~ co2 * year + log(value0 + 1) + (1|ring), abund_sc3)


# model diagnosis
plot(sc3_m1)
qqPlot(resid(sc3_m1))


# F test
Anova(sc3_m1, test.statistic = "F")


# 95% confidence intervals for covariate-adjusted means
sc3_lsmean <- lsmeans::lsmeans(sc3_m1, ~ co2 | year)
sc3_95CI   <- data.frame(summary(sc3_lsmean)) %>% 
  mutate_each(funs(exp(.) - 1), lsmean, lower.CL, upper.CL)
sc3_95CI


# CO2 response ratios (RR) on covariate-adjusted means
sc3_95CI %>% 
  group_by(co2) %>% 
  summarise(value = mean(lsmean)) %>%
  summarise(rr = value[co2 == "elev"] / value[co2 == "amb"] - 1)




# > subordinate c4 --------------------------------------------------------

# model
plot(sqrt(value) ~ sqrt(value0), abund_sc4)
sc4_m1 <- lmer(sqrt(value) ~ co2 * year + sqrt(value0) + (1|ring), abund_sc4)


# model diagnosis
qqPlot(resid(sc4_m1))
plot(sc4_m1)


# F test
Anova(sc4_m1, test.statistic = "F")


# pairwise comparison and 95% confidence intervals for covariate-adjusted means
sc4_lsmean <- lsmeans::lsmeans(sc4_m1, ~ co2 | year, type = "response")
summary(pairs(sc4_lsmean)[1:3], adjust = "none")

sc4_95CI   <- data.frame(summary(sc4_lsmean))
sc4_95CI


# CO2 response ratios (RR) on covariate-adjusted means
sc4_95CI %>% 
  group_by(co2) %>% 
  summarise(value = mean(response)) %>%
  summarise(rr = value[co2 == "elev"] / value[co2 == "amb"] - 1)




# > C4:C3 ratios ------------------------------------------------------------

plot(log(value) ~ log(value0), data = ratio_c43)


# model
c43r_m1 <- lmer(log(value) ~ co2 * year + log(value0) + (1|ring), data = ratio_c43)


# model diagnosis
plot(c43r_m1)
qqPlot(residuals(c43r_m1))


# F test
Anova(c43r_m1, test.statistic = "F")


# 95% confidence intervals for covariate-adjusted means
c43r_lsmean <- lsmeans::lsmeans(c43r_m1, ~ co2 | year, type = "response")
c43r_95CI   <- data.frame(summary(c43r_lsmean))
c43r_95CI


# CO2 response ratios (RR) on covariate-adjusted means
c43r_95CI %>% 
  group_by(co2) %>% 
  summarise(value = mean(response)) %>%
  summarise(rr = value[co2 == "elev"] / value[co2 == "amb"] - 1)




# > subordinate: dominant ratio -------------------------------------------

# model
plot(logit(value) ~ logit(value0), ratio_DS)
sd_m1 <- lmer(logit(value) ~ co2 * year + logit(value0) + (1|ring), data = ratio_DS)


# model diagnosis
plot(sd_m1)
qqPlot(resid(sd_m1))


# F test
Anova(sd_m1, test.statistic = "F")


# pairwise comparison and 95% confidence intervals for covariate-adjusted means
sdr_lsmean <- lsmeans::lsmeans(sd_m1, ~ co2 | year, type = "response")
summary(pairs(sdr_lsmean)[1:3], adjust = "none")

sdr_95CI   <- data.frame(summary(sdr_lsmean))
sdr_95CI


# CO2 response ratios (RR) on covariate-adjusted means
sdr_95CI %>% 
  group_by(co2) %>% 
  summarise(value = mean(response)) %>%
  summarise(rr = value[co2 == "elev"] / value[co2 == "amb"] - 1)

