#######################################################X
#----Analysis of Animal Movement Data in R Workshop----X
#----------- Module 09 -- Multiple animals ------------X
#----------------Last updated 2023-03-15---------------X
#-------------------Code Walkthrough-------------------X
#######################################################X

library(tidyverse)
library(amt)
library(broom)
library(patchwork)
library(NLMR)
library(here)
library(mvtnorm)
library(glmmTMB)
library(ResourceSelection) 
library(broom.mixed)
library(terra)

set.seed(1323)

source("fun/sim_ssf.R")

# HSF for multiple animals ----

# First, we will use the goats data set from the ResourceSelection package. Just
# as a side note, in this package a weighted distribution approach for HSF/RSF
# is implemented, which we did not consider here. If you are interested see
# here: https://www.jstor.org/stable/20069332
data(goats, package = "ResourceSelection")

head(goats)
table(goats$STATUS)

# We fit three models:
# 1. Ignore individuals
# 2. Random intercept 
# 3. Random intercept and random slope for all covariates

# Here we ignore individuals
m1 <- glmmTMB(STATUS ~ ELEVATION + SLOPE, 
              data = goats, family = binomial())

# This is the random intercept model. This is what was recommended before the
# Muff et al. 2020 paper and many researcher still do.
m2 <- glmmTMB(STATUS ~ ELEVATION + SLOPE + (1 | ID), 
              data = goats, family = binomial())

# This is a random slope and intercept model
m3 <- glmmTMB(STATUS ~ ELEVATION + SLOPE + 
                (SLOPE + ELEVATION | ID),
              data = goats, family = binomial())

# Lets compare the models
summary(m1)
summary(m2)
summary(m3)

# And create a figure that summarizes the result
bind_rows(
  tidy(m1, conf.int = TRUE) |> mutate(what = "glm"),
  tidy(m2, conf.int = TRUE) |> mutate(what = "glmm (intercept)"),
  tidy(m3, conf.int = TRUE) |> mutate(what = "glmm (intercept & slope)")
) |> filter(effect == "fixed") |> 
  mutate(term1 = str_remove_all(term, "[\\(\\)]"), 
         term1 = str_to_title(term1) |> factor() |> fct_inorder(), 
         what = factor(what) |> fct_inorder()) |> 
  ggplot(aes(what, estimate)) + 
           geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  facet_wrap(~ term1, scale = "free") + 
  geom_hline(yintercept = 0, col = "red", lty = 2) + 
  labs(y = "Estimate", x = "Model") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.title.x = element_blank())


# Simulations ----
# Next, we are going to use again simulations to show how to use random effects
# for HSF/RSF and SSFs:

# ... Landscape ----
# We first create a landscape, similar to how we created it for the iSSF module.
set.seed(1234301)

# Forest
dim <- 500
forest <- rast(NLMR::nlm_gaussianfield(dim, dim) < 0.5)
forest[] <- as.numeric(forest[])
plot(forest)

# and elevation
ele <- rast(NLMR::nlm_gaussianfield(dim, dim))
plot(ele)

covars <- c(forest, ele)
names(covars)
names(covars) <- c("forest", "elevation")

# ... Movement
# Now we can simulate movement for one animal. With a very simple movement model
# that consists of a uniform turn angle distribution and a exponential
# step-length distribution.
curve(dexp(x, rate = 1), from = 0, to = 20)

dat1 <- simulate_ssf(
  n_steps = 500, n_ch = 10, l = 0.5, xy0 = c(dim/2, dim/2), 
  resc = covars, coef = c(0.1, 0)
)

plot(covars)
plot(ele)
points(dat1)

# Let us extend the simulation for 10 animals from the same populations. We
# will draw the coefficients for each animal from a bivariate normal
# distribution.
coefs <- rmvnorm(n = 10, mean = c(0.01, -0.5), sigma = diag(c(0.1, 0.2)))

# For each animal we will simulate a trajectory of varying length between 50 and
# 1000 relocations.
n.pts <- runif(10, 50, 1000) |> round()

# Now we can simulate the 10 tracks using a `map()` call.
dat <- map(1:10, ~ {
  simulate_ssf(
    n_steps = n.pts[.x], n_ch = 10, l = 0.5, xy0 = c(dim/2, dim/2), 
    resc = covars, coef = coefs[.x, ]
    # We add a unique id for each animal
  ) |> mutate(id = .x)
})

# We want to put all data in one data data.frame, this can be achieved with
# `bind_rows()` from dplyr.
dat1 <- dat |> bind_rows()
dat1

# Finally, lets plot all animals together
dat1 |> ggplot(aes(x_, y_, col = factor(id))) + geom_point() + coord_equal()

# Here is where the analysis starts.

# HSF/RSF ----
# ... Prepare data ----
dat.hsf <- dat1 |> nest(data = -id) |> 
  mutate(random.points = map(data, random_points)) |> 
  select(-data) |> unnest(cols = random.points) |> 
  extract_covariates(covars) |> 
  mutate(weight = ifelse(case_, 1, 1e5))
dat.hsf

# ... Global model ----
m1 <- glm(case_ ~ forest + elevation, data = dat.hsf, weights = weight, family = binomial())

# ... Individual model ----
# First nest the data by id to run individual models
m2.dat <- dat.hsf |> nest(dat = -id) 

# We could either use a loop to fit a  model to each individual
res <- list()
for (i in 1:length(m2.dat$dat)) {
  res[[i]] <- glm(case_ ~ forest + elevation, data = m2.dat$dat[[i]], weights = weight, family = binomial())
}
res

# Or even better use map()
res <- map(
  m2.dat$dat, ~ 
    glm(case_ ~ forest + elevation, data = .x, weights = weight, family = binomial()))
res

# The `tidy()` function from the broom package, summarizes a model nicely into a tibble. 
res[[1]] |> tidy()

# And we can put everything together in on piped workflow
m2 <- m2.dat |> 
  mutate(mod = map(dat, ~ glm(case_ ~ forest + elevation, data = .x, 
                              weights = weight, family = binomial()) |> 
                     tidy(conf.int = TRUE))) |> 
  select(id, mod) |> unnest(cols = mod)

m2

m2 |> filter(term != "(Intercept)") |> 
  ggplot(aes(x = term, y = estimate)) + 
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), alpha = 0.4, 
                  position = position_jitter(width = 0.1)) +
  stat_summary(col = "red")

# ... Random intercept and slope ----
# Finally we can use a mixed model approach to with a random intercept and slope

# Random intercept only
m3 <- glmmTMB(case_ ~ forest + elevation + (1 | id), data = dat.hsf, 
              weights = weight, family = binomial())
summary(m3)

# A random slope model
m4 <- glmmTMB(case_ ~ forest + elevation + (0 + forest | id) + 
                (0 + elevation | id), 
              data = dat.hsf, weights = weight, family = binomial())
summary(m4)

# A second random slope model, where we allow the coefficients for forest and 
# elevation to covary. 
m5 <- glmmTMB(case_ ~ forest + elevation + 
                (forest + elevation | id), 
              data = dat.hsf, weights = weight, family = binomial())
summary(m5)


# True parameters under an SSF
# coefs <- rmvnorm(n = 10, mean = c(0.01, -0.5), sigma = diag(c(0.1, 0.2)))
# Forest: 0.01
# Elevation: -0.5

coef(m1)
fixef(m3)
fixef(m4)
fixef(m5)

diag(vcov(m1))[2:3] # Complete pooling
diag(vcov(m3)[[1]])[2:3] # Random intercept
diag(vcov(m4)[[1]])[2:3] # random Intercept and random slopes (SE are greatest and hence also the CI)
diag(vcov(m5)[[1]])[2:3] # random Intercept and random slopes (SE are greatest and hence also the CI)

bind_rows(
  tidy(m1, conf.int = TRUE) |> select( term, estimate, conf.low, conf.high) |> mutate(mod = "fixed"),
  tidy(m3, conf.int = TRUE) |> select( term, estimate, conf.low, conf.high) |> mutate(mod = "random intercept"), 
  tidy(m4, conf.int = TRUE) |> select( term, estimate, conf.low, conf.high) |> mutate(mod = "random slope 1"),
  tidy(m5, conf.int = TRUE) |> select( term, estimate, conf.low, conf.high) |> mutate(mod = "random slope 2")
) |> filter(term %in% c("forest", "elevation")) |> 
  ggplot(aes(mod, estimate)) + 
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) + 
  geom_hline(yintercept = 0, col = "red") +
  facet_wrap(~ term, scale = "free")

# iSSF ----
# ... Prepare data ----

dat.issf <- dat1 |> nest(data = -id) |> 
  mutate(random.steps = map(data, ~ steps(.x) |> random_steps())) |> 
  select(-data) |> unnest(cols = random.steps) |> 
  extract_covariates(covars) |> 
  mutate(log_sl_ = log(sl_), sin_ta_ = sin(ta_), 
         step_id1_ = paste(id, step_id_, sep = "."))


# ... Poisson trick ----
# Lets first use data for just one individual to investigate the Poisson trick.
dat.issf.1 <- filter(dat.issf, id == 1)

# Use clogit
m1 <- fit_issf(dat.issf.1, case_ ~ forest + elevation + sl_ + log_sl_ + strata(step_id1_))
summary(m1)

# Poisson formulation 
m2 <- glmmTMB(case_ ~ -1 + forest + elevation + sl_ + log_sl_ + 
                ## Make sure we use a unique step id here
                (1|step_id1_), 
              family = poisson(), data = dat.issf.1, doFit = FALSE)
m2$parameters$theta[1] <- log(1e3) # fixing the variance for stratum specific random intercepts
m2$mapArg <- list(theta = factor(NA))
m2 <- glmmTMB:::fitTMB(m2)

# Estimated coefficients are identical
coef(m1)
fixef(m2)

# So are the vars
diag(vcov(m1$model))
diag(vcov(m2)[[1]])

# ... Global model ----
# This model ignores that we have differen individuals.
m1 <- fit_issf(case_ ~ forest + elevation + sl_ + log_sl_ + strata(step_id1_), data = dat.issf)
summary(m1)

# ... Individual model ----
m2 <- dat.issf |> nest(dat = -id) |> 
  mutate(mod = map(dat, ~ {
    m <- fit_issf(case_ ~ forest + elevation + sl_ + log_sl_ + strata(step_id1_), data = .) 
    tidy(m$model, conf.int = TRUE)
    })) |> 
  select(id, mod) |> unnest(cols = mod)
m2
m2 |> 
  ggplot(aes(x = term, y = estimate)) + 
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), alpha = 0.4, 
                  position = position_jitter(width = 0.2)) +
  stat_summary(col = "red")

# ... Random intercept & random slope (no coefficients can not covary) ----
m3 <- glmmTMB(case_ ~ -1 + forest + elevation + sl_ + log_sl_ + (1 |step_id1_) +
                (0 + elevation | id) + (0 + forest | id) , 
               family = poisson(), data = dat.issf, doFit = FALSE)
m3$parameters$theta[1] <- log(1e3)
m3$mapArg <- list(theta = factor(c(NA, 1:2)))
m3 <- glmmTMB:::fitTMB(m3)
summary(m3)

# ... Random intercept & random slope (allow coefficients to covary) ----
m4 <- glmmTMB(case_ ~ -1 + forest + elevation + sl_ + log_sl_ + (1 |step_id1_) +
                (0 + elevation + forest | id) , 
               family = poisson(), data = dat.issf, doFit = FALSE)
m4$parameters$theta[1] <- log(1e3)
m4$mapArg <- list(theta = factor(c(NA, 1:3)))
m4 <- glmmTMB:::fitTMB(m4)
summary(m4)

# Comparing the models again
# As a reminder, here are the values that we used for simulating the data.
# coefs <- rmvnorm(n = 10, mean = c(0.01, -0.5), sigma = diag(c(0.1, 0.2)))
# Forest: 0.01
# Elevation: -0.5

# The estiamtes
coef(m1)[1:2]
fixef(m3)[[1]][1:2]
fixef(m4)[[1]][1:2]

# The standard errors
diag(vcov(m1$model)[1:2, 1:2])
diag(vcov(m3)[[1]])[1:2]
diag(vcov(m4)[[1]])[1:2]
# Compare Individual models and random-effects model

broom.mixed::tidy(m3, conf.int = TRUE, effects = "fixed")

bind_rows(
  tidy(m1$model, conf.int = TRUE) |> select( term, estimate, conf.low, conf.high) |> mutate(mod = "fixed"),
  tidy(m3, conf.int = TRUE, effect = "fixed") |> 
    select( term, estimate, conf.low, conf.high) |> mutate(mod = "random slope 1"), 
  tidy(m4, conf.int = TRUE, effects = "fixed") |>
    select( term, estimate, conf.low, conf.high) |> mutate(mod = "random slope 2")
) |> filter(term %in% c("forest", "elevation")) |> 
  ggplot(aes(mod, estimate)) + 
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) + 
  geom_hline(yintercept = 0, col = "red") +
  facet_wrap(~ term, scale = "free")
