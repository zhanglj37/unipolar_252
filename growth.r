set.seed(123)

n <- 5000

theta0 <- rnorm(n)
x0 <- exp(theta0)

# strictly positive growth with Matthew effect
growth_true <- exp(
  0.3 * (theta0) + rnorm(n, 0, 0.3)
)

x1 <- x0 + growth_true

# bipolar representation
theta0_bi <- log(x0)
theta1_bi <- log(x1)
growth_bi <- theta1_bi - theta0_bi

cor_unipolar <- cor(x0, growth_true)
cor_bipolar  <- cor(theta0_bi, growth_bi)

cor_unipolar
cor_bipolar



