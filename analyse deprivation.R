library(tidyverse)
library(rstanarm)

options(mc.cores = parallel::detectCores())

deaths_msoa <- read_csv("data/deaths-msoa.csv")
deaths_la <- read_csv("data/deaths-la.csv")

m_la <- stan_lmer(DeathRate ~ Score + (Score | RGN19NM), data = deaths_la,
                  prior_intercept = normal(0, 5), prior = normal(0,2), prior_covariance = decov(regularization=2),
                  cores = 1, chains = 1)

plot(m_la)
summary(m_la)

# Extract the posterior draws for all parameters
sims <- as.matrix(m_la)
dim(sims)
colnames(sims)

# ---- Obtain regional varying intercept a_j ----
# draws for overall mean
mu_a_sims <- as.matrix(m_la, 
                       pars = "(Intercept)")
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
