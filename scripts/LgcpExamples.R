library(tidyverse); library(gridExtra); library(ggthemes)

theme_set(theme_minimal(base_size = 12))

lgcp_sims = read_csv("~/Documents/ComposableStateSpaceModels/data/lgcpsims.csv", 
                     col_names = c("time", "observation", "hazard", "gamma", "state"))

### Parameter values
### m0 = 2.0, c0 = 1.0, theta = 0.1, alpha = 0.4, sigma = 0.5

toPlot = lgcp_sims %>%
  gather(key, value, -time) %>%
  filter(key %in% c("observation", "hazard", "state")) %>%
  mutate(key = factor(key, levels = c("observation", "hazard", "state")))

ggplot(toPlot, aes(time, value)) + 
  facet_wrap(~key, ncol = 1, scales = "free_y", strip.position = "right") +
  geom_point(data = toPlot %>% filter(key == "observation") %>% filter(value == 1)) +
  geom_line(data = toPlot %>% filter(key == "hazard")) +
  geom_line(data = toPlot %>% filter(key == "state"))

# ggsave("~/Documents/PhD/WriteUp/ComposableModelsPaper/StatsAndComp/Figures/Fig5.eps")

##################
# Filtering LGCP #
##################

lgcp_filtered = read_csv("~/Documents/ComposableStateSpaceModels/data/lgcpfiltered.csv", 
                        col_names = c("time", "observation", "pred_hazard", "hazard_lower", "hazard_upper",
                                      "pred_state", "state_lower", "state_upper"))

lgcp_filtered %>%
  select(-observation, -contains("state")) %>%
  inner_join(lgcp_sims %>% select(time, hazard), by = "time") %>%
  gather(key, value, -time, -hazard_lower, -hazard_upper) %>%
  ggplot(aes(x = time, y = value, linetype = key)) +
  geom_line() +
  geom_ribbon(aes(x = time, ymin = hazard_lower, ymax = hazard_upper), alpha = 0.5, colour = NA)

#########################
# Determine LGCP Params #
#########################

