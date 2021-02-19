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

plot(m_la)
summary(m_la)

# Extract the posterior draws for all parameters
sims <- as.matrix(m_la)
dim(sims)
colnames(sims)

# ---- Obtain regional varying intercept a_j ----
# draws for overall mean
mu_a_sims <- as.matrix(m_la, 
                       pars = "Score")
# draws for region-level error
# u_sims <- as.matrix(m_la, 
#                     regex_pars = "b\\[\\(Intercept\\) RGN19NM\\:")
u_sims <- as.matrix(m_la, 
                    regex_pars = "b\\[Score RGN19NM\\:")
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
round(a_df, 2)

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
