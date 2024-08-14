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

