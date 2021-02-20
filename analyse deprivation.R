library(tidyverse)
library(rstanarm)

options(mc.cores = parallel::detectCores())

deaths_msoa <- read_csv("data/deaths-msoa.csv")
deaths_la <- read_csv("data/deaths-la.csv")

m_la <- stan_lmer(DeathRate ~ Extent + (Extent | RGN19NM), data = deaths_la,
                  prior_intercept = normal(0, 5), prior = normal(0,2), prior_covariance = decov(regularization=2),
                  cores = 1, chains = 4)

saveRDS(m_la, "data/la-model.rds")

launch_shinystan(m_la, ppd = FALSE)

# ---- Obtain regional varying slopes ----
get_slopes <- function(m, param = "Extent") {
  # draws for overall mean
  mu_a_sims <- as.matrix(m, 
                         pars = param)
  # draws for region-level error
  u_sims <- as.matrix(m, 
                      regex_pars = paste0("b\\[", param, " RGN19NM\\:"))
  # draws for regional varying intercepts               
  a_sims <- as.numeric(mu_a_sims) + u_sims 
  
  # ---- Compute mean, SD, median, and 95% credible interval of varying intercepts ----
  # Posterior mean and SD of each alpha
  a_mean <- apply(X = a_sims,     # posterior mean
                  MARGIN = 2,
                  FUN = mean)
  a_sd <- apply(X = a_sims,       # posterior SD
                MARGIN = 2,
                FUN = sd)
  
  # Posterior median and 95% credible interval
  a_quant <- apply(X = a_sims, 
                   MARGIN = 2, 
                   FUN = quantile, 
                   probs = c(0.025, 0.50, 0.975))
  a_quant <- data.frame(t(a_quant))
  names(a_quant) <- c("Q2.5", "Q50", "Q97.5")
  
  # Combine summary statistics of posterior simulation draws
  a_df <- data.frame(a_mean, a_sd, a_quant)
  a_df
}

# ---- Plot predicted death rates ----
new_data <- expand_grid(
  Score = seq(min(deaths_la$Score), max(deaths_la$Score)),
  RGN19NM = unique(deaths_la$RGN19NM)
)

post <- posterior_predict(m_la, new_data)
pred <- posterior_epred(m_la, new_data)

quants <- apply(post, 2, quantile, probs = c(0.025, 0.5, 0.975))  # quantiles over mcmc samples
quants2 <- apply(pred, 2, quantile, probs = c(0.025, 0.5, 0.975))  # quantiles over mcmc samples
row.names(quants) <- c("sim.lwr", "sim.med", "sim.upr")
row.names(quants2) <- c("lwr", "DeathRate.pred", "upr")

new_data <- cbind(new_data, t(quants), t(quants2))

new_data %>% 
  ggplot(aes(x = Score, y = DeathRate.pred)) +
  geom_line(aes(colour = RGN19NM)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = RGN19NM), alpha = 0.2) +
  
  # geom_blank(data = ylims) +  # plot dummy data to give facets the same y limits by country
  
  facet_wrap(~RGN19NM) +
  theme_classic()

# ---- MSOA model ----
m_msoa <- stan_lmer(DeathRate ~ Extent + (Extent | RGN19NM), data = deaths_msoa,
                    prior_intercept = normal(0, 5), prior = normal(0,2), prior_covariance = decov(regularization=2),
                    cores = 1, chains = 4)

saveRDS(m_msoa, "data/msoa-model.rds")

launch_shinystan(m_msoa, ppd = FALSE)

# ---- Make tibble with slope estimates from both models ----
slopes_la <- get_slopes(m_la)
slopes_msoa <- get_slopes(m_msoa)

slopes <- bind_rows(
  slopes_la %>% rownames_to_column(var = "Parameter") %>% mutate(Model = "LA"),
  slopes_msoa %>% rownames_to_column(var = "Parameter") %>% mutate(Model = "MSOA")
) %>% 
  
  mutate(Region = Parameter %>% 
           str_remove("b\\[Extent RGN19NM:") %>% 
           str_remove("\\]") %>% 
           str_replace_all("_", " "))

# Save slopes
write_csv(slopes, "data/regression-slopes.csv")

# ---- Fit models with IMD score rather than extent ----
# Standardise scores
deaths_la$Score_z = as.numeric(scale(deaths_la$Score))
deaths_msoa$Score_z = as.numeric(scale(deaths_msoa$Score))

m_score_la <- stan_lmer(DeathRate ~ Score_z + (Score_z | RGN19NM), data = deaths_la,
                        prior_intercept = normal(0, 5), prior = normal(0,2), prior_covariance = decov(regularization=2),
                        cores = 1, chains = 4)

m_score_msoa <- stan_lmer(DeathRate ~ Score_z + (Score_z | RGN19NM), data = deaths_msoa,
                          prior_intercept = normal(0, 5), prior = normal(0,2), prior_covariance = decov(regularization=2),
                          cores = 1, chains = 4)

write_rds(m_score_la, "data/la-score-model.rds")
write_rds(m_score_msoa, "data/msoa-score-model.rds")

launch_shinystan(m_score_la, ppd = FALSE)
launch_shinystan(m_score_msoa, ppd = FALSE)

slopes_la <- get_slopes(m_score_la, param = "Score_z")
slopes_msoa <- get_slopes(m_score_msoa, param = "Score_z")

slopes <- bind_rows(
  slopes_la %>% rownames_to_column(var = "Parameter") %>% mutate(Model = "LA"),
  slopes_msoa %>% rownames_to_column(var = "Parameter") %>% mutate(Model = "MSOA")
) %>% 
  
  mutate(Region = Parameter %>% 
           str_remove("b\\[Score_z RGN19NM:") %>% 
           str_remove("\\]") %>% 
           str_replace_all("_", " "))

# Save slopes
write_csv(slopes, "data/regression-slopes-scores.csv")
