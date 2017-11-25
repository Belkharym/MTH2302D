# CHARGER CSV

setwd("~/Git/Git_Projects/MTH2302D/TD07")
coton <- read.csv("Coton.csv", header=TRUE, sep=";", dec=",", col.names=c("PERCENT"))

# HISTOGRAMME

hist(coton$PERCENT)
hist(
  coton$PERCENT,
  col="lightpink",
  border="black",
  main="HISTOGRAMME",
  xlab="Pourcentages de coton",
  ylab="Fréquences"
)

# TABLEAU D'EFFECTIFS

TABLE = table(coton$PERCENT)

# BOÎTE À MOUSTACHES (box plot)

boxplot(coton$PERCENT)

boxplot(
  coton$PERCENT,
  col = c("lightblue"),
  horizontal = TRUE,
  notch = TRUE,
  main = "Pourcentages de coton",
  ylab = "Lot de tissus No1",
  las = 0
)

# GRAPHE PROBA NORMALE ET QUANTILE-QUANTILE

qqnorm(coton$PERCENT)
qqline(coton$PERCENT, col = "red", lwd = 2)

# AVG, VAR, STD DEV, VAR COEF, MED, Q1, Q3, EIQ

measures = data.frame(percent = c("PERCENT"),
                      moyenne = NA, var = NA, s = NA, Q1 = NA, médiane = NA,
                      Q3 = NA, CV = NA, EIQ = NA)

# AVG
measures$moyenne = mean(coton$PERCENT)

# STD DEV
measures$s = sd(coton$PERCENT)

# VARIANCE
measures$var = measures$s ^ 2

# VAR COEF
measures$CV = measures$s / measures$moyenne

# MED
measures$médiane = median(coton$PERCENT)

# Q1, Q3
QUANTILES = quantile(coton$PERCENT, probs = c(0.25, 0.75))
measures[c("Q1", "Q3")] = QUANTILES

# INTERQUARTILE INTERVAL

measures$EIQ = measures$Q3 - measures$Q1

# DISPLAY DATA

measures