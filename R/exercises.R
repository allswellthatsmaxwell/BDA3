library(ggplot2)
library(tibble)
library(dplyr)
library(glue)



# ch1 1c
get_dist_df <- function(σ) {
  xs <- seq(-6, 6, length.out=1000)
  bind_rows(
    tibble(x = xs, y = dnorm(xs, 1, σ**2), μ = "μ = 1"),
    tibble(x = xs, y = dnorm(xs, 2, σ**2), μ = "μ = 2")) %>%
    mutate(σ = σ)
}

df <- Reduce(bind_rows, 
             lapply(seq(0.4, 1.6, 0.1),
                    get_dist_df))

df %>%
  ggplot(aes(x, y), color='gray', fill='gray') +
  geom_polygon() +
  facet_wrap(~σ, ncol=1, scales="free_y") +
  theme_minimal() +
  theme(strip.text = element_blank(), 
        axis.title = element_blank(),
        axis.text = element_blank())


# 1.4: point spreads
outcomes <- c(-7, -5, -3, -3, 1, 6, 7, 13, 15, 16, 20, 21)
# by hand:
# p(favorite wins | spread = 8) = 8/12 = 2/3 = 0.67
# p(favorite wins by at least 8 | spread = 8) = 5 / 12 = 0.42
# p(favorite wins by at least 8 | spread = 8 and favorite wins) = 5/8 = 0.625
spread <- 8
diffs <- outcomes - spread

s <- 13.86 # from the chapter; not this subset of data
m <- 0

curve(dnorm(x, mean = m, sd = s), 
      from = m - 4*s, 
      to = m + 4*s)
abline(v = 8, col = "maroon", lty = 2)

# by approximating N(μ, σ^2):
# p(favorite wins | spread = 8) = pnorm(8.5, m, s) = 0.73
# p(favorite wins by at least 8 | spread = 8) = pnorm(0.5, m, s) = 0.51
# p(favorite wins by at least 8 | spread = 8 and favorite wins) = 
# p(favorite wins by at least 8 | spread = 8) / p(favorite wins | spread = 8)
#   pnorm(0.5, m, s) / pnorm(8.5, m, s) = 0.70
# cumulative density that favorite wins:
pnorm(8.5, m, s)
# by at least 8: 
pnorm(0.5, m, s)
# so the answer is
pnorm(0.5, m, s) / pnorm(8.5, m, s)


