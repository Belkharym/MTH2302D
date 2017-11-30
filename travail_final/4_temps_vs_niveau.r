#
# QUESTIONS POSÉES AU TRAVAIL 1:
# ------------------------------
# 1) "Y a-t-il une corrélation entre la fréquence des tétriminos et le score obtenu par le joueur?
# Jusqu’à quel point? Autrement dit : les joueurs rapides font-ils plus de points?"
#
# 2) "À l’aide de la corrélation précédente, nous souhaiterions déterminer la fréquence optimale de
# tétriminos pour optimiser le score."

#########################################
# VALEURS DE CONFIGURATION DU PROGRAMME #
#########################################

TETRIMINOS_MINUMUMS = 100 # Nombre minimal de tétriminos pour que la partie soit considérée valide
TRUST_LEVEL = 0.95
POLYNOMIAL_DEGREE = 3

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
setwd("~/Git/Git_Projects/MTH2302D/"); # CHANGER POUR LE BON DOSSIER
data = read.csv(
  "donnees.csv",
  header = TRUE,
  col.names = c("observation", "joueur", "jour", "score", "niveau", "lignes", "temps", "fréquence_lignes", "tétriminos", "fréquence_tétriminos", "simples", "doubles", "triples", "tetris")
);
data["temps"] = apply(data, 1, FUN = function(row) { timeInSeconds(row["temps"]) }) # Transformer les temps en numériques
data = data[data$tétriminos > TETRIMINOS_MINUMUMS,] # Supprimer les observations dont le nombre de tétriminos est trop faible
data = data[order(data$fréquence_tétriminos),] # Trier par fréquence des tétriminos

# Vue initiale sur la question

plot(
  x = data[, "fréquence_tétriminos"],
  y = data[, "score"],
  main = "Fréquence des tétriminos vs. score",
  xlab = "Fréquence des tétriminos (tetriminos/min)",
  ylab = "Score atteint"
);

# Faire la régression

fit = lm(score ~ poly(fréquence_tétriminos, POLYNOMIAL_DEGREE), data = data)
summary(fit) # Résumé de la régression (R², p-value)

# Afficher la prédiction du modèle sur le graphe existant

minimum_frequency = min(data$fréquence_tétriminos)
maximum_frequency = max(data$fréquence_tétriminos)
x_to_evaluate_fit_at = seq(from = minimum_frequency, to = maximum_frequency, 0.1)

predictions =
  as.data.frame(predict(
    fit,
    newdata = data.frame(fréquence_tétriminos = x_to_evaluate_fit_at),
    interval='confidence',
    level=TRUST_LEVEL
  ))
predictions["fréquence_tétriminos"] = x_to_evaluate_fit_at

lines(x_to_evaluate_fit_at, predictions[, "lwr"], col='red',   lwd=1)
lines(x_to_evaluate_fit_at, predictions[, "fit"], col='green', lwd=3)
lines(x_to_evaluate_fit_at, predictions[, "upr"], col='red',   lwd=1)

# Déterminer la fréquence optimale selon le modèle (courbe min, courbe de prédiction, courbe max)

optimal_for_minimum_fit = predictions[which.max(predictions$lwr),]
optimal_for_minimum_fit = data.frame(fréquence_tétriminos = optimal_for_minimum_fit$fréquence_tétriminos, score_prédit = optimal_for_minimum_fit$lwr)
optimal_for_fit         = predictions[which.max(predictions$fit),]
optimal_for_fit         = data.frame(fréquence_tétriminos = optimal_for_fit$fréquence_tétriminos,         score_prédit = optimal_for_fit$fit        )
optimal_for_maximum_fit = predictions[which.max(predictions$upr),]
optimal_for_maximum_fit = data.frame(fréquence_tétriminos = optimal_for_maximum_fit$fréquence_tétriminos, score_prédit = optimal_for_maximum_fit$upr)
print(optimal_for_minimum_fit)
print(optimal_for_fit)
print(optimal_for_maximum_fit)

