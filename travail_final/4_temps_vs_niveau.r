#
# QUESTIONS POSÉES AU TRAVAIL 1:
# ------------------------------
# 4)"Y a-t-il une corrélation entre le temps de jeu et le niveau final atteint? Si oui, quelle est-elle?"

#########################################
# VALEURS DE CONFIGURATION DU PROGRAMME #
#########################################

TEMPS_MINUMUMS = 90 # temps minimal de jeu pour que la partie soit considérée valide
TRUST_LEVEL = 0.95
POLYNOMIAL_DEGREE = 1

#############
# FONCTIONS #
#############

# Fonction pour transformer le temps en numérique
timeInSeconds = function(timeAsString) {
  minutes = -1
  seconds = -1
  hundredths = -1
  buf = ""
  for (char in strsplit(timeAsString, "")[[1]]) {
    if (char == ":") {
      minutes = as.numeric(buf)
      buf = ""
    }
    else if (char == ".") {
      seconds = as.numeric(buf)
      buf = ""
    }
    else {
      buf = paste(buf, char, sep = "")
    }
  }

  if (minutes == -1) {
    # Pas de deux-points (ex: "02")
    minutes = as.numeric(buf)
    seconds = 0
    hundredths = 0
  }
  else if (seconds == -1) {
    # Pas de point (ex: "02:15")
    seconds = as.numeric(buf)
    hundredths = 0
  }
  else {
    hundredths = as.numeric(buf)
  }
  return (minutes * 60 + seconds + hundredths / 100)
}

#############
# PROGRAMME #
#############

# Charger les données
setwd("~/Gits/MTH2302D/"); # CHANGER POUR LE BON DOSSIER
data = read.csv(
  "donnees.csv",
  header = TRUE,
  col.names = c("observation", "joueur", "jour", "score", "niveau", "lignes", "temps", "fréquence_lignes", "tétriminos", "fréquence_tétriminos", "simples", "doubles", "triples", "tetris")
);
data["temps"] = apply(data, 1, FUN = function(row) { timeInSeconds(row["temps"]) }) # Transformer les temps en numériques
data = data[data$temps > TEMPS_MINUMUMS,] # Supprimer les observations dont le nombre de tétriminos est trop faible
data = data[order(data$temps),] # Trier par fréquence des tétriminos

# Vue initiale sur la question

plot(
  x = data[, "temps"],
  y = data[, "niveau"],
  main = "Temps vs. niveau atteint",
  xlab = "Temps (sec)",
  ylab = "Niveau atteint"
);

# Faire la régression

fit = lm(niveau ~ poly(temps, POLYNOMIAL_DEGREE), data = data)
summary(fit) # Résumé de la régression (R², p-value)

# Afficher la prédiction du modèle sur le graphe existant

minimum_time = min(data$temps)
maximum_time = max(data$temps)
x_to_evaluate_fit_at = seq(from = minimum_time, to = maximum_time, 0.1)

predictions =
  as.data.frame(predict(
    fit,
    newdata = data.frame(temps = x_to_evaluate_fit_at),
    interval='confidence',
    level=TRUST_LEVEL
  ))
predictions["temps"] = x_to_evaluate_fit_at

lines(x_to_evaluate_fit_at, predictions[, "lwr"], col='red',   lwd=1)
lines(x_to_evaluate_fit_at, predictions[, "fit"], col='green', lwd=3)
lines(x_to_evaluate_fit_at, predictions[, "upr"], col='red',   lwd=1)

# Déterminer si le temps de jeu et le niveau atteint sont corrélés et de quelle façon.

correlation_coefficent = cor(x = data$temps, y = data$niveau)
print(correlation_coefficent)

R_2 = correlation_coefficent ** 2
print(R_2)
