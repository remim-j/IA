# Projet Intelligence Artificielle
# Quentin DA SILVA, William DEVOUARD, Rémi MARIE-JEANNE
# TELECOM Nancy 3A IAMD & IL
# Le 07/12/2017

# Chargement des données d'apprentissage et des données de test
data.train = read.csv("datatrain.csv")
data.test = read.csv("datatest.csv")

# Vérification sur les dimensions des 2 jeux de données
dim(data.train)
dim(data.test)

# Sélection des poids des arguments de la régression
weights = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

# Sélection des arguments
features = c("arg1", "arg2", "arg3", "arg4", "arg5", "arg6", "arg7", "arg8", "arg9", "arg10",
             "arg11", "arg12", "arg13", "arg14", "arg15", "arg16", "arg17", "arg18", "arg19", "arg20",
             "arg21", "arg22", "arg23", "arg24", "arg25", "arg26", "arg27", "arg28", "arg29", "arg30",
             "arg31", "arg32", "arg33", "arg34", "arg35", "arg36", "arg37", "arg38", "arg39", "arg40",
             "arg41", "arg42", "arg43", "arg44", "arg45", "arg46", "arg47", "arg48", "arg49", "arg50",
             "arg51", "arg52", "arg53", "arg54", "arg55", "arg56", "arg57", "arg58", "arg59", "arg60",
             "arg61", "arg62", "arg63", "arg64", "arg65", "arg66", "arg67", "arg68", "arg69", "arg70",
             "arg71", "arg72", "arg73", "arg74", "arg75", "arg76", "arg77", "arg78", "arg79", "arg80",
             "arg81", "arg82", "arg83", "arg84", "arg85", "arg86", "arg87", "arg88", "arg89", "arg90")

# Fonction permettant de concaténer les vecteurs de poids et d'arguments
concatWeightsFeatures <- function(wei, feat){
  
  res = c()
  
  for (i in 1:length(wei)) {
    
      #res = append(res, paste(paste(wei[i], " * "), feat[i]))
      res = append(res, feat[i])
  }
  
  return(res)
}

# Vecteur des arguments pondérés
weightedFeatures <- concatWeightsFeatures(weights, features)

# Nombre de chansons dans l'échantillon selectionné
test.size = 50000

# Nombre d'échantillons testés :
num.iterations = 1

# Erreur moyenne (en nombre d'années)
errMoy = 0

# Initialisation des objects pour le reporting des résultats
summary.results = {}
all.results = {}

for (i in 1:num.iterations) {
  
  # On sélectionne aléatoirement test.size chansons dans les données de test
  ind1 = sample(x = nrow(data.test), size = test.size, replace = FALSE)
  data.test.sub = data.test[ind1,]
  
  # Calcul du model linéaire à partir des données d'apprentissage
  arg1 <- data.test.sub[, 'arg1']
  arg2 <- data.test.sub[, 'arg2']
  arg3 <- data.test.sub[, 'arg3']
  arg4 <- data.test.sub[, 'arg4']
  arg5 <- data.test.sub[, 'arg5']
  arg6 <- data.test.sub[, 'arg6']
  arg7 <- data.test.sub[, 'arg7']
  arg8 <- data.test.sub[, 'arg8']
  arg9 <- data.test.sub[, 'arg9']
  arg10 <- data.test.sub[, 'arg10']
  arg11 <- data.test.sub[, 'arg11']
  arg12 <- data.test.sub[, 'arg12']
  y <- data.test.sub[, 'y']
  model = nls(y ~ a + b*I(arg1^z1) + c*arg2 + d*arg3 + e*arg4 + f*arg5 + g*arg6 + h*arg7 + i*arg8 + j*arg9 + k*arg10 + l*arg11 + m*arg12, start = list(a=1900, b=1, c=1, d=1, e=1, f=1, g=1, h=1, i=1, j=1, k=1, l=1, m=1, z1=0))
  
  # Les résultats du model linéaire
  summary(model)
  
  # Prédiction sur le jeu de test
  predicted = predict(model, data.test.sub)
  
  # Valeurs actuelles des années de sorties des chansons
  actual = data.test.sub[,'y']

  # Calcul de l'erreur moyenne
  mape = abs(actual-predicted)
          
  # Collecte des résultats
  summary.results = rbind(summary.results, mean(mape))
  all.results = rbind(all.results, cbind(i, actual, predicted, mape))
  
  # Affichage de l'erreur moyenne
  errMoy = mean(mape)
  cat("Erreur moyenne itération ", i, " : ", errMoy, "\n")
  
  plot(actual, col="blue")
  points(predicted,col="red")
}

rownames(summary.results) = c(1:num.iterations)
colnames(summary.results) = c("mean.MAPE")
rownames(all.results) = c(1:(test.size*num.iterations))
colnames(all.results) = c("i", "actual", "predicted", "mape")

# Arrondis des valeurs résultats et erreurs
summary.results[,"mean.MAPE"] = round(summary.results[,"mean.MAPE"], digits=3)
all.results[,"mape"] = round(all.results[,"mape"], digits=3)
all.results[,"predicted"] = round(all.results[,"predicted"], digits=0)

# Creation de fichiers CSV avec les résultats de la prédiction
file.remove("./errMoy.csv")
write.csv(summary.results, "./errMoy.csv", row.names=F)
file.remove("./results.csv")
write.csv(all.results, "./results.csv", row.names=F)