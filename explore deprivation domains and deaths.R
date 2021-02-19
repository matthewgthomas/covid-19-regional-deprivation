deaths_la <- read_csv("data/deaths-la.csv")

deaths_la %>% 
  ggplot(aes(y = DeathRate, x = Score, colour = RGN19NM, fill = RGN19NM)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~RGN19NM) +
  labs(y = "Death rate per 100,000 population", x = "IMD - average score (higher is more deprived)", colour = NULL, fill = NULL) +
  theme_classic() +
  theme(legend.position = "bottom")

deaths_la %>% 
  ggplot(aes(y = DeathRate, x = Health_Score, colour = RGN19NM, fill = RGN19NM)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~RGN19NM) +
  labs(y = "Death rate per 100,000 population", x = "IMD - average score (higher is more deprived)", colour = NULL, fill = NULL) +
  theme_classic() +
  theme(legend.position = "bottom")

deaths_la %>% 
  ggplot(aes(y = DeathRate, x = Barriers_Score, colour = RGN19NM, fill = RGN19NM)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~RGN19NM) +
  labs(y = "Death rate per 100,000 population", x = "IMD - average score (higher is more deprived)", colour = NULL, fill = NULL) +
  theme_classic() +
  theme(legend.position = "bottom")

deaths_la %>% 
  ggplot(aes(y = DeathRate, x = Barriers_Proportion, colour = RGN19NM, fill = RGN19NM)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~RGN19NM) +
  labs(y = "Death rate per 100,000 population", x = "IMD - average score (higher is more deprived)", colour = NULL, fill = NULL) +
  theme_classic() +
  theme(legend.position = "bottom")
