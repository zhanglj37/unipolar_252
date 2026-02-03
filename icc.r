# =========================
# Unipolar log-logistic ICC
# =========================
icc_unipolar <- function(lambda, d, c) {
  (lambda / d)^c / (1 + (lambda / d)^c)
}

# ability scale: e^theta
lambda <- exp(seq(-3, 4, length.out = 500))

# =========================
# Plot layout: two panels
# =========================
par(
  mfrow = c(1, 2),
  mar = c(4, 4, 3, 1)
)

# =========================
# Panel 1: Effect of d
# =========================
c_fix <- 1.5
d_vals <- c(0.5, 1, 2, 4)

# blue gradient: light -> dark
cols_d <- colorRampPalette(c("lightblue", "darkblue"))(length(d_vals))

plot(
  lambda,
  icc_unipolar(lambda, d_vals[1], c_fix),
  type = "l",
  lwd = 2,
  col = cols_d[1],
  ylim = c(0, 1),
  xlim = c(0, 6),
  xlab = expression(e^{theta}),
  ylab = "Probability",
  main = "Effect of difficulty (d)"
)

for (i in 2:length(d_vals)) {
  lines(
    lambda,
    icc_unipolar(lambda, d_vals[i], c_fix),
    lwd = 2,
    col = cols_d[i]
  )
}

legend(
  "bottomright",
  legend = paste("d =", d_vals),
  col = cols_d,
  lwd = 2,
  cex = 0.9
)

# =========================
# Panel 2: Effect of c
# =========================
d_fix <- 2
c_vals <- c(0.5, 1, 2, 4)

# red gradient: light -> dark
cols_c <- colorRampPalette(c("mistyrose", "darkred"))(length(c_vals))

plot(
  lambda,
  icc_unipolar(lambda, d_fix, c_vals[1]),
  type = "l",
  lwd = 2,
  col = cols_c[1],
  ylim = c(0, 1),
  xlim = c(0, 6),
  xlab = expression(e^{theta}),
  ylab = "Probability",
  main = "Effect of discrimination (c)"
)

for (i in 2:length(c_vals)) {
  lines(
    lambda,
    icc_unipolar(lambda, d_fix, c_vals[i]),
    lwd = 2,
    col = cols_c[i]
  )
}

legend(
  "bottomright",
  legend = paste("c =", c_vals),
  col = cols_c,
  lwd = 2,
  cex = 0.9
)



