#working with a famous Lahman Baseball Dataset
library(dplyr)
library(tidyr)
library(Lahman)
# Filter out pitchers
career <- Batting %>%
  filter(AB > 0) %>%
  anti_join(Pitching, by = "playerID") %>%
  group_by(playerID) %>%
  summarize(H = sum(H), AB = sum(AB)) %>%
  mutate(average = H / AB)
# Include names along with the player IDs
career <- Master %>%
  tbl_df() %>%
  dplyr::select(playerID, nameFirst, nameLast) %>%
  unite(name, nameFirst, nameLast, sep = " ") %>%
  inner_join(career, by = "playerID") %>%
  dplyr::select(-playerID)

#Estimating prior from the existing data
library(stats4)
install.packages("VGAM")
career_filtered <- career %>%
  filter(AB > 500)
# log-likelihood function
ll <- function(alpha, beta) {
  x <- career_filtered$H
  total <- career_filtered$AB
  -sum(VGAM::dbetabinom.ab(x, total, alpha, beta, log = TRUE))
}
#maximum likelihood estimate
m <- mle(ll, start = list(alpha = 1, beta = 10), method = "L-BFGS-B",
         lower = c(0.0001, .1))
ab <- coef(m)
alpha0 <- ab[1]
beta0 <- ab[2]

#applying prior to each individual estimate
career_eb <- career %>%
  mutate(eb_estimate = (H + alpha0) / (AB + alpha0 + beta0))

#posterior distribution
career_eb <- career_eb %>%
  mutate(alpha1 = alpha0 + H,
         beta1 = beta0 + AB - H)

#credible intervals, 95% interval - two tailed.
career_eb <- career_eb %>%
  mutate(low = qbeta(.025, alpha1, beta1),
         high = qbeta(.975, alpha1, beta1))

#example of 95% credible intervals, error as whisker plotm using head
plot <- head(career_eb,5) %>%
  mutate(name = reorder(name, eb_estimate)) %>%
  ggplot(aes(eb_estimate, name)) +
  geom_point() +
  geom_errorbarh(aes(xmin = low, xmax = high)) +
  geom_vline(xintercept = alpha0 / (alpha0 + beta0), color = "red", lty = 2) +
  xlab("Estimated batting average (w/ 95% interval)") +
  ylab("Player")

#find the possibility where one does not hit over .300 (or Posterior Error Probability)
career_eb <- career_eb %>%
  mutate(PEP = pbeta(.3, alpha1, beta1))

#find top 100 players with lowest PEP (highest possibility to hit over .300)
top_players <- career_eb %>%
  arrange(PEP) %>%
  head(100)

#q-value : cumulative mean of all the posterior error probability. (or a q-value)
career_eb <- career_eb %>%
  arrange(PEP) %>%
  mutate(qvalue = cummean(PEP))

