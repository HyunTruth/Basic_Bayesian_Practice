library(dplyr)
#trying to analyze a baseball stats for hitting rate. Simplify to hit or no-hit, a binomial distribution
#set number of trials for simulation
num_trials <- 10e6
#run simulation, based on the prior known data, which is the most likely around .27, and range from .21 to .35
#it can be represented with a beta distribution with alpha = 81 and beta = 219
simulations <- data_frame(
  true_average = rbeta(num_trials, 81, 219),
  #run for number of hits per 300 at-bats, given the distribution
  hits = rbinom(num_trials, 300, true_average)
)
#find the simulations in which the number of hits = 100
hit_100 <- simulations %>%
  filter(hits == 100)
hit_100
#create histogram of average hit rate for each instances
plot <- hit_100 %>%
  ggplot(aes(true_average)) +
  geom_histogram(aes(y=..density..))

#distributions of 60 hits, 80 hits, and 100 hits
simulations %>%
  filter(hits %in% c(60, 80, 100)) %>%
  ggplot(aes(true_average, color = factor(hits))) +
  geom_density() +
  labs(x = "True average of players with H hits / 300 at-bats",
       color = "H")
