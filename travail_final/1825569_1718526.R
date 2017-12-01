#
# QUESTIONS POSÉES AU TRAVAIL 1:
# ------------------------------
# 1) "Y a-t-il une corrélation entre la fréquence des tétriminos et le score obtenu par le joueur?
# Jusqu’à quel point? Autrement dit : les joueurs rapides font-ils plus de points?"
#
# 2) "À l’aide de la corrélation précédente, nous souhaiterions déterminer la fréquence optimale de
# tétriminos pour optimiser le score."
#
# 3) "Quelle combinaison de lignes permet d’optimiser le score? Plus précisément, nous souhaitons
# comparer le rapport (nombre de lignes complétées par une combinaison / nombre de lignes
# total) au score afin de déterminer la combinaison à privilégier. Les tetris sont les complétions
# de lignes qui donnent le plus de points puisqu’elles demandent de compléter quatre lignes en un
# coup, mais elles demandent au joueur de prendre le risque de laisser sa pile monter et de perdre
# la partie."

#########################################
# VALEURS DE CONFIGURATION DU PROGRAMME #
#########################################

TETRIMINOS_MINUMUMS = 100 # Nombre minimal de tétriminos pour que la partie soit considérée valide
TEMPS_MINUMUMS = 90 # temps minimal de jeu pour que la partie soit considérée valide
TRUST_LEVEL = 0.95
POLYNOMIAL_DEGREE_1_2 = 3
POLYNOMIAL_DEGREE_4 = 1

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
setwd("~/Gits/MTH2302D/"); # CHANGER POUR LE BON DOSSIER
data = read.csv(
  "donnees.csv",
  header = TRUE,
  col.names = c("observation", "joueur", "jour", "score", "niveau", "lignes", "temps", "fréquence_lignes", "tétriminos", "fréquence_tétriminos", "simples", "doubles", "triples", "tetris")
);
data["temps"] = apply(data, 1, FUN = function(row) { timeInSeconds(row["temps"]) }) # Transformer les temps en numériques
data["proportion_simples"] = apply(data, 1, FUN = function(row) { return(1 * as.numeric(row["simples"]) / as.numeric(row["lignes"])) }) # Proportion de lignes qui ont été complétées par un simple
data["proportion_doubles"] = apply(data, 1, FUN = function(row) { return(2 * as.numeric(row["doubles"]) / as.numeric(row["lignes"])) }) # Proportion de lignes qui ont été complétées par un double
data["proportion_triples"] = apply(data, 1, FUN = function(row) { return(3 * as.numeric(row["triples"]) / as.numeric(row["lignes"])) }) # Proportion de lignes qui ont été complétées par un triple
data["proportion_tetris" ] = apply(data, 1, FUN = function(row) { return(4 * as.numeric(row["tetris" ]) / as.numeric(row["lignes"])) }) # Proportion de lignes qui ont été complétées par un tetris

data12 = data[data$tétriminos > TETRIMINOS_MINUMUMS,] # Supprimer les observations dont le nombre de tétriminos est trop faible
data12 = data12[order(data12$fréquence_tétriminos),] # Trier par fréquence des tétriminos

data3 = data[data$tétriminos > TETRIMINOS_MINUMUMS,] # Supprimer les observations dont le nombre de tétriminos est trop faible
data3 = data3[order(data12$fréquence_tétriminos),] # Trier par fréquence des tétriminos

data4 = data[data$temps > TEMPS_MINUMUMS,] # Supprimer les observations dont le nombre de tétriminos est trop faible
data4 = data4[order(data12$temps),] # Trier par fréquence des tétriminos

#######################
### Question 1 et 2 ###
#######################

# Vue initiale sur la question

plot(
  x = data12[, "fréquence_tétriminos"],
  y = data12[, "score"],
  main = "Fréquence des tétriminos vs. score",
  xlab = "Fréquence des tétriminos (tetriminos/min)",
  ylab = "Score atteint"
);

# Faire la régression

fit = lm(score ~ poly(fréquence_tétriminos, degree = POLYNOMIAL_DEGREE_1_2), data = data12)
summary(fit) # Résumé de la régression (R², p-value)

# Afficher la prédiction du modèle sur le graphe existant

minimum_frequency = min(data12$fréquence_tétriminos)
maximum_frequency = max(data12$fréquence_tétriminos)
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

##################
### Question 3 ###
##################

# Effectuer la régression et l'afficher
options(scipen=10)

fit_simple = performAndPlotRegression(data = data3, x_column_name = "proportion_simples", main_title = "Influence des \"simples\" sur le score", xlab = "Proportion de simples", ylab = "Score")
fit_double = performAndPlotRegression(data = data3, x_column_name = "proportion_doubles", main_title = "Influence des \"doubles\" sur le score", xlab = "Proportion de doubles", ylab = "Score")
fit_triple = performAndPlotRegression(data = data3, x_column_name = "proportion_triples", main_title = "Influence des \"triples\" sur le score", xlab = "Proportion de triples", ylab = "Score")
fit_tetris = performAndPlotRegression(data = data3, x_column_name = "proportion_tetris" , main_title = "Influence des \"tetris\" sur le score" , xlab = "Proportion de tetris" , ylab = "Score")

correlations = data.frame(
  correlation_simples = cor(x = data$proportion_simples, y = data$score),
  correlation_doubles = cor(x = data$proportion_doubles, y = data$score),
  correlation_triples = cor(x = data$proportion_triples, y = data$score),
  correlation_tetris  = cor(x = data$proportion_tetris , y = data$score)
)
print(correlations)

##################
### Question 4 ###
##################

# Vue initiale sur la question

plot(
  x = data4[, "temps"],
  y = data4[, "niveau"],
  main = "Temps vs. niveau atteint",
  xlab = "Temps (sec)",
  ylab = "Niveau atteint"
);

# Faire la régression

fit4 = lm(niveau ~ poly(temps, POLYNOMIAL_DEGREE_4), data = data4)
summary(fit4) # Résumé de la régression (R², p-value)

# Afficher la prédiction du modèle sur le graphe existant

minimum_time = min(data4$temps)
maximum_time = max(data4$temps)
x_to_evaluate_fit_at = seq(from = minimum_time, to = maximum_time, 0.1)

predictions =
  as.data.frame(predict(
    fit4,
    newdata = data.frame(temps = x_to_evaluate_fit_at),
    interval='confidence',
    level=TRUST_LEVEL
  ))
predictions["temps"] = x_to_evaluate_fit_at

lines(x_to_evaluate_fit_at, predictions[, "lwr"], col='red',   lwd=1)
lines(x_to_evaluate_fit_at, predictions[, "fit"], col='green', lwd=3)
lines(x_to_evaluate_fit_at, predictions[, "upr"], col='red',   lwd=1)

# Déterminer si le temps de jeu et le niveau atteint sont corrélés et de quelle façon.

correlation_coefficent = cor(x = data4$temps, y = data4$niveau)
print(correlation_coefficent)

R_2 = correlation_coefficent ** 2
print(R_2)
