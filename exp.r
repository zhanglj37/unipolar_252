set.seed(123)

n <- 5000

# latent variable
theta <- rnorm(n, mean = 0, sd = 1)
# noise
epsilon <- rnorm(n)
# unipolar transform
x <- exp(theta)
# construct y so that cor(x, y) ≈ 0.5
y <- 0.5 * (x / sd(x)) + sqrt(1 - 0.5^2) * epsilon

# correlations
cor_theta_y <- cor(theta, y)
cor_x_y <- cor(x, y)

cor_theta_y
cor_x_y

par(mfrow = c(1, 2))

# theta vs y
plot(theta, y,
     pch = 16, col = rgb(0, 0, 0, 0.2),
     xlab = expression(theta),
     ylab = "y",
     main = paste("theta vs y\ncor =", round(cor_theta_y, 2)))

abline(lm(y ~ theta), col = "red", lwd = 2)


# exp(theta) vs y
plot(x, y,
     pch = 16, col = rgb(0, 0, 0, 0.2),
     xlab = expression(e^theta),
     ylab = "y",
     main = paste("exp(theta) vs y\ncor =", round(cor_x_y, 2)))

abline(lm(y ~ x), col = "red", lwd = 2)



