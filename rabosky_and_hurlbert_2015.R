set.seed(43)
crown_age <- 10
mu <- 0.1
lambda <- 10.0 * mu # As Liow et al
k <- 20

# Crown ages
ts <- seq(1,50,1.0)

n_trees_per_crown_age <- 20

df <- data.frame(
  crown_age = rep(ts, each = n_trees_per_crown_age),
  gamma = rep(NA, length(ts) * n_trees_per_crown_age)
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
