
#
# QUESTION POSÉE AU TRAVAIL 1:
# "Y a-t-il une corrélation entre la fréquence des tétriminos et le score obtenu par le joueur?
# Jusqu’à quel point? Autrement dit : les joueurs rapides font-ils plus de points?"
#

# Charger les données
setwd("~/Git/Git_Projects/MTH2302D/");
data = read.csv(
  "donnees.csv",
  header = TRUE,
  col.names = c("observation", "joueur", "jour", "score", "niveau", "lignes", "temps", "fréquence_lignes", "tétriminos", "fréquence_tétriminos", "simples", "doubles", "triples", "tetris")
);

# Vue initiale sur la question

plot(
  x = data[, "fréquence_tétriminos"],
  y = data[, "score"],
  main = "Fréquence des tétriminos vs. Score",
  xlab = "Fréquence des tétriminos (tetriminos/min)",
  ylab = "Score atteint"
);

