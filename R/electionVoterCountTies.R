library(ggplot2)
library(tibble)
library(magrittr)
library(rstan)

# options(mc.cores = parallel::detectCores())
# rstan_options(auto_write = TRUE)

maxpop <- 35e4
# maxpop <- 60e3
minpop <- 50e3
n_districts <- 435

b <- 1.3

powerBuilder <- function(b) {
  C <- (b + 1) / (maxpop - minpop)^(b + 1)
  f <- function(x) (maxpop - x)^b * C
}

powerBuilderLog <- function(b) {
  logC = log(b + 1) - (b + 1) * log(maxpop - minpop)
  function(x) b * log(maxpop - x) + logC
}

drawSamples <- function(n_simulations, n_districts, b) {
  n_values_needed_from_district_sizes <- n_simulations * n_districts
  
  warmup <- 500
  nchain <- 4
  
  data_list <- list(
    b = b,
    maxpop = maxpop,
    minpop = minpop,
    voter_turnout_alpha=600,
    voter_turnout_beta=400,
    
    # too high! just for testing.
    party_a_vote_prop_alpha=5000,
    party_a_vote_prop_beta=5000
  )
  
  stan_model_code <- "
  data {
    real maxpop;              // Maximum population
    real minpop;              // Minimum population
    real b;                   // Constant b for power law distribution
    
    int voter_turnout_alpha;
    int voter_turnout_beta;
    
    // proportion of votes won by candidate A (exchangeable)
    int party_a_vote_prop_alpha;
    int party_a_vote_prop_beta;
  }
  
  transformed data {
    real logC;
    // C is ~1e-65, so causes underflow - use the log instead, which is -148.
    logC = log(b + 1) - (b + 1) * log(maxpop - minpop);
  }
  
  parameters {
    real<lower=minpop,upper=maxpop> district_pop;  // District population
    real<lower=0,upper=1> voter_turnout;           // Proportion of voters
    real<lower=0,upper=1> party_a_prop;
  }
  
  model {
    // Power law distribution for district population
    target += b * log(maxpop - district_pop) + logC;
    
    voter_turnout ~ beta(voter_turnout_alpha, voter_turnout_beta);
    party_a_prop ~ beta(party_a_vote_prop_alpha, party_a_vote_prop_beta);
  }
  
  generated quantities {
    int total_ties;
  
    {
      int voters;  // Number of voters in the district
      int party_a_votes;
      int is_tie;
      
      voters = max(1, binomial_rng(to_int(round(district_pop)), voter_turnout));
      party_a_votes = binomial_rng(voters, party_a_prop);
      is_tie = (2 * party_a_votes == voters) ? 1 : 0;
      total_ties = is_tie;
    }
  }"
  stan_model <- rstan::stan_model(model_code = stan_model_code)
  
  rstan::sampling(
    stan_model, data = data_list, chains = nchain,
    iter = ceiling(n_values_needed_from_district_sizes / nchain) + warmup, 
    warmup = warmup)
}

drawDistrictVoteCountSets <- function(n_simulations, n_districts, b) {
  fit <- drawSamples(n_simulations, n_districts, b)
  samples <- rstan::extract(fit)
  district_size_drawn_sets <- split(samples, 1:n_simulations)
  
  district_size_drawn_sets %>% 
    sapply(length) %>% 
    unique() %>% 
    length() %>% 
    assertthat::are_equal(1)
  
  district_size_drawn_sets
}

drawDistrictVoteCountSetsLargeN <- function(n_simulations, n_districts, b) {
  max_n_per_call <- 40000
  ratio <- floor(n_simulations / max_n_per_call)
  remainder <- n_simulations %% max_n_per_call
  
  nsim_per_call <- rep(max_n_per_call, ratio) %>% c(remainder)
  
  district_sets <- lapply(
    nsim_per_call, 
    function(nsim) drawDistrictVoteCountSets(nsim, n_districts, b))
  
   Reduce(c, district_sets)
}

valuesAllUnique <- function(vec) {
  length(vec) == length(unique(vec))
}

# complement of the probability of them all being different
uniform_district_size_answer <- 1 - prod((maxpop - n_districts):maxpop / maxpop)
uniform_district_size_answer

n_simulations <- 100000
sampling <- drawSamples(n_simulations, n_districts, b)
samples <- rstan::extract(sampling)

prop_election_tie <- sum(samples$total_ties) / length(samples$total_ties)

district_size_drawn_sets <- drawDistrictVoteCountSetsLargeN(n_simulations, n_districts, b)

nties <- district_size_drawn_sets %>%
  lapply(function(v) sort(v)) %>%
  sapply(purrr::negate(valuesAllUnique)) %>%
  sapply(any) %>% 
  sum()

power_district_size_answer <- nties / length(district_size_drawn_sets)
power_district_size_answer


beta_xs <- seq(0, 1, 0.001)
α <- 600
β <- 400
beta_density <- dbeta(beta_xs, α, β)
plot(beta_xs, beta_density, type='l')

xs <- seq(minpop, maxpop, 1000)
pfn <- powerBuilder(b)
pfnl <- powerBuilderLog(b)

step_logprobs <- log(pfn(minpop:(maxpop - 1)))
step_logprobs
sum(step_logprobs)

ggplot(tibble(xs, pfn(xs)), aes(x=xs)) +
  geom_line(aes(y=pfn(xs))) +
  theme_minimal()

ggplot(tibble(samples), aes(x=samples)) + geom_histogram(bins=1000) + theme_minimal()


# probability of tie
election_result_beta_params <- list(xs = beta_xs, α = 100, β = 100)
election_result_beta_params %$% plot(beta_xs, dbeta(xs, α, β), type = 'l')
election_result_probas <- election_result_beta_params %$% rbeta(n_districts, α, β)
votes_for_party_a_sets <- district_size_drawn_sets %>% 
  lapply(function(voters) round(voters * election_result_probas))
votes_if_tied_sets <- district_size_drawn_sets %>%
  lapply(function(voters) voters / 2)

countTies <- function(vote_counts, tie_counts) sum(vote_counts == tie_counts)

tieCounts <- mapply(countTies, votes_for_party_a_sets, votes_if_tied_sets)
sum(tieCounts) / (length(tieCounts) * n_districts)


# b)
values_to_use <- list(
  district_pop = runif(length(samples$district_pop), min = minpop, max = maxpop))
values_to_use$party_a_prop <- rbeta(length(values_to_use$district_pop), 50, 50)
values_to_use$voter_turnout <- rbeta(length(values_to_use$district_pop), 600, 400)
hist(values_to_use$party_a_prop)


voters <- values_to_use %$% {
  rbinom(length(district_pop_to_use),
  round(district_pop_to_use),
  voter_turnout)}

party_a_votes <- values_to_use %$% {
  rbinom(length(voters),
         voters,
         party_a_prop)}
party_b_votes <- voters - party_a_votes
votes_if_tied <- voters / 2
is_tie <- 2 * party_a_votes == voters

past_data <- list(total_elections = 20597, n_fewer_than_10_difference = 6,
                  n_fewer_than_100_difference = 49)
past_data %$% {n_fewer_than_10_difference / total_elections}
past_data %$% {n_fewer_than_100_difference / total_elections}

sum(abs(party_a_votes - party_b_votes) < 10) / length(votes_if_tied)
sum(abs(party_a_votes - party_b_votes) < 100) / length(votes_if_tied)

tie_prop <- sum(is_tie) / length(voters)
1 / tie_prop
