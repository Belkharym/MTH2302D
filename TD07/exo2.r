MEMORY = read.csv("Memoire.csv", header = TRUE, dec = ".", sep = ";")

ANALYSIS = data.frame(
  alpha5 = NA,
  alpha95 = NA,
  STD_DEV = NA
)

QUANTILES = quantile(MEMORY$stress, probs = c(0.05, 0.95))
ANALYSIS[c("alpha5", "alpha95")] = QUANTILES
ANALYSIS$STD_DEV = sd(MEMORY$stress)

ANALYSIS

