set.seed(42)
mu <- 0.1
lambda <- 10.0 * mu # As Liow et al
k <- 20

crown_ages <- seq(from = 1.0, to = 50, by = 1.0)
n_crown_ages <- length(crown_ages)
n_trees_per_crown_age <- 20

df <- data.frame(
  crown_age = rep(crown_ages, each = n_trees_per_crown_age),
  gamma = rep(NA, n_crown_ages * n_trees_per_crown_age)
)

for (row in seq(1, nrow(df)))
{
  crown_age <- df$crown_age[row]
  p <- DDD::dd_sim(c(lambda, mu, k),crown_age)$tes
  df$gamma[row] <- ape::gammaStat(p)
}

ggplot2::ggplot(
  data = df,
  ggplot2::aes(x = crown_age, y = gamma)
) + ggplot2::geom_point() + ggplot2::geom_smooth()
