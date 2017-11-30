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

#################

performAndPlotRegression = function(data, x_column_name, plot_color, main_title, xlab, ylab) {
  POLYNOMIAL_DEGREE = 1 # Régression linéaire
  equation = as.formula(paste("score ~ poly(", x_column_name, ", POLYNOMIAL_DEGREE)", sep = ""))
  fit = lm(equation, data = data)
  
  # Tracer les données
  plot(x = data[[x_column_name]], y = data$score, main = main_title, xlab = xlab, ylab = ylab, xlim = c(0.0, 1.0), ylim = c(0, 1000000))
  
  # Tracer la régression (ainsi que la régression min et max)
  x_to_evaluate_fit_at = data.frame(seq(from = 0, to = 1, 0.01))
  colnames(x_to_evaluate_fit_at) = c(x_column_name)
  
  predictions =
    as.data.frame(predict(
      fit,
      newdata = x_to_evaluate_fit_at,
      interval='confidence',
      level=TRUST_LEVEL
    ))
  predictions[x_column_name] = x_to_evaluate_fit_at
  lines(predictions[[x_column_name]], predictions[, "lwr"], col='red',   lwd=1)
  lines(predictions[[x_column_name]], predictions[, "fit"], col='green', lwd=3)
  lines(predictions[[x_column_name]], predictions[, "upr"], col='red',   lwd=1)
  
  return(fit)
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
)
data = data[data$tétriminos > TETRIMINOS_MINUMUMS,] # Supprimer les observations dont le nombre de tétriminos est trop faible
data = data[order(data$fréquence_tétriminos),] # Trier par score
data["temps"] = apply(data, 1, FUN = function(row) { timeInSeconds(row["temps"]) }) # Transformer les temps en numériques
data["proportion_simples"] = apply(data, 1, FUN = function(row) { return(1 * as.numeric(row["simples"]) / as.numeric(row["lignes"])) }) # Proportion de lignes qui ont été complétées par un simple
data["proportion_doubles"] = apply(data, 1, FUN = function(row) { return(2 * as.numeric(row["doubles"]) / as.numeric(row["lignes"])) }) # Proportion de lignes qui ont été complétées par un double
data["proportion_triples"] = apply(data, 1, FUN = function(row) { return(3 * as.numeric(row["triples"]) / as.numeric(row["lignes"])) }) # Proportion de lignes qui ont été complétées par un triple
data["proportion_tetris" ] = apply(data, 1, FUN = function(row) { return(4 * as.numeric(row["tetris" ]) / as.numeric(row["lignes"])) }) # Proportion de lignes qui ont été complétées par un tetris

# Effectuer la régression et l'afficher
options(scipen=10)

fit_simple = performAndPlotRegression(data = data, x_column_name = "proportion_simples", main_title = "Influence des \"simples\" sur le score", xlab = "Proportion de simples", ylab = "Score")
fit_double = performAndPlotRegression(data = data, x_column_name = "proportion_doubles", main_title = "Influence des \"doubles\" sur le score", xlab = "Proportion de doubles", ylab = "Score")
fit_triple = performAndPlotRegression(data = data, x_column_name = "proportion_triples", main_title = "Influence des \"triples\" sur le score", xlab = "Proportion de triples", ylab = "Score")
fit_tetris = performAndPlotRegression(data = data, x_column_name = "proportion_tetris" , main_title = "Influence des \"tetris\" sur le score" , xlab = "Proportion de tetris" , ylab = "Score")
