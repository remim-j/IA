# Projet Intelligence Artificielle
# Quentin DA SILVA, William DEVOUARD, R�mi MARIE-JEANNE
# TELECOM Nancy 3A IAMD & IL
# Le 07/12/2017

# Mod�le polynomial

# Chargement des donn�es d'apprentissage et des donn�es de test
data.train = read.csv("datatrain.csv")
data.test = read.csv("datatest.csv")

# V�rification sur les dimensions des 2 jeux de donn�es
dim(data.train)
dim(data.test)

# S�lection de l'attribut y
y = "y"

# S�lection des poids des arguments de la r�gression
weights = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
weights2 = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
             2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
             2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
weights3 = c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
             3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
             3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3)

# S�lection des arguments
features = c("arg1", "arg2", "arg3", "arg4", "arg5", "arg6", "arg7", "arg8", "arg9", "arg10",
             "arg11", "arg12", "arg13", "arg14", "arg15", "arg16", "arg17", "arg18", "arg19", "arg20",
             "arg21", "arg22", "arg23", "arg24", "arg25", "arg26", "arg27", "arg28", "arg29", "arg30",
             "arg31", "arg32", "arg33", "arg34", "arg35", "arg36", "arg37", "arg38", "arg39", "arg40",
             "arg41", "arg42", "arg43", "arg44", "arg45", "arg46", "arg47", "arg48", "arg49", "arg50",
             "arg51", "arg52", "arg53", "arg54", "arg55", "arg56", "arg57", "arg58", "arg59", "arg60",
             "arg61", "arg62", "arg63", "arg64", "arg65", "arg66", "arg67", "arg68", "arg69", "arg70",
             "arg71", "arg72", "arg73", "arg74", "arg75", "arg76", "arg77", "arg78", "arg79", "arg80",
             "arg81", "arg82", "arg83", "arg84", "arg85", "arg86", "arg87", "arg88", "arg89", "arg90")

# Fonction permettant de concat�ner les vecteurs de poids et d'arguments
concatWeightsFeatures <- function(wei, feat){
  
  res = c()
  
  for (i in 1:length(features)) {
    
    res = append(res, paste("poly(", feat[i], ",", wei[i], ")"))
  }
  
  return(res)
}

confusionMatrix <- function(actual, predicted) {
  
  vectZero <- rep(0, 100)
  xRes <- matrix(vectZero, nrow = 10, ncol = 10,
                 dimnames = list(c("Real2130","Real3140","Real4150", "Real5160","Real6170","Real7180", "Real8190","Real9100", "Real0110","Real1120"), 
                                 c("Pred2130","Pred3140","Pred4150", "Predi5160","Pred6170","Pred7180", "Pred8190","Pred9100", "Pred0110","Pred1120")))
  
  for (i in 1:test.size) {
    
    xi = 0;
    yi = 0;
    if (1921 <= actual[i] && actual[i] <= 1930) {
      
      xi = 1;
    }
    if (1931 <= actual[i] && actual[i] <= 1940) {
      
      xi = 2;
    }
    if (1941 <= actual[i] && actual[i] <= 1950) {
      
      xi = 3;
    }
    if (1951 <= actual[i] && actual[i] <= 1960) {
      
      xi = 4;
    }
    if (1961 <= actual[i] && actual[i] <= 1970) {
      
      xi = 5;
    }
    if (1971 <= actual[i] && actual[i] <= 1980) {
      
      xi = 6;
    }
    if (1981 <= actual[i] && actual[i] <= 1990) {
      
      xi = 7;
    }
    if (1991 <= actual[i] && actual[i] <= 2000) {
      
      xi = 8;
    }
    if (2001 <= actual[i] && actual[i] <= 2010) {
      
      xi = 9;
    }
    if (2011 <= actual[i] && actual[i] <= 2020) {
      
      xi = 10;
    }
    
    if (1921 <= pred2[i] && pred2[i] <= 1930) {
      
      yi = 1;
    }
    if (1931 <= pred2[i] && pred2[i] <= 1940) {
      
      yi = 2;
    }
    if (1941 <= pred2[i] && pred2[i] <= 1950) {
      
      yi = 3;
    }
    if (1951 <= pred2[i] && pred2[i] <= 1960) {
      
      yi = 4;
    }
    if (1961 <= pred2[i] && pred2[i] <= 1970) {
      
      yi = 5;
    }
    if (1971 <= pred2[i] && pred2[i] <= 1980) {
      
      yi = 6;
    }
    if (1981 <= pred2[i] && pred2[i] <= 1990) {
      
      yi = 7;
    }
    if (1991 <= pred2[i] && pred2[i] <= 2000) {
      
      yi = 8;
    }
    if (2001 <= pred2[i] && pred2[i] <= 2010) {
      
      yi = 9;
    }
    if (2011 <= pred2[i] && pred2[i] <= 2020) {
      
      yi = 10;
    }
    
    xRes[xi,yi] = xRes[xi,yi]+1;
  }
  
  return(xRes)
}

# Vecteur des arguments pond�r�s
weightedFeatures <- concatWeightsFeatures(weights2, features)

# Nombre de chansons dans l'�chantillon selectionn�
test.size = 30000

# Nombre d'�chantillons test�s
num.iterations = 1

# Erreur moyenne (en nombre d'ann�es)
errMoy = 0

# Initialisation des objects pour le reporting des r�sultats
summary.results = {}
all.results = {}

for (i in 1:num.iterations) {
  
  # On s�lectionne al�atoirement test.size chansons dans les donn�es de test
  ind1 = sample(x = nrow(data.train), size = test.size, replace = FALSE)
  data.train.sub = data.train[ind1,]
  
  #ind2 = sample(x = nrow(data.test), size = 200, replace = FALSE)
  data.test.sub = data.test#[ind2,]
  
  # Calcul du model lin�aire � partir des donn�es d'apprentissage
  model = lm(formula = paste(y, paste(weightedFeatures, collapse = " + "), sep = " ~ "), data = data.train.sub)
  
  # Les r�sultats du model lin�aire
  summary(model)
  
  # Affichage graphique
  opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
  plot(model, las = 1)
  par(opar)
  
  # Pr�diction sur le jeu de test
  train.predicted = predict(model, data.train.sub)
  test.predicted = predict(model, data.test.sub)
  
  # Valeurs actuelles des ann�es de sorties des chansons
  train.actual = data.train.sub[, y]
  test.actual = data.test.sub[, y]
  
  # Calcul de l'erreur moyenne
  train.mape = abs(train.actual - train.predicted)
  test.mape = abs(test.actual - test.predicted)
  
  # Affichage de l'erreur moyenne
  train.errMoy = mean(train.mape)
  test.errMoy = mean(test.mape)
  cat("Train : Erreur moyenne it�ration ", i, " : ", train.errMoy, "\n")
  cat("Test : Erreur moyenne it�ration ", i, " : ", test.errMoy, "\n")
  
  plot(test.actual, col = "blue")
  points(test.predicted, col = "red")
  
  # Collecte des r�sultats
  summary.results = rbind(summary.results, test.errMoy)
  all.results = rbind(all.results, cbind(i, test.actual, test.predicted, test.mape))

}

rownames(summary.results) = c(1:num.iterations)
colnames(summary.results) = c("mean.MAPE")
rownames(all.results) = c(1:51630)
colnames(all.results) = c("i", "actual", "predicted", "mape")

# Arrondis des valeurs r�sultats et erreurs
summary.results[, "mean.MAPE"] = round(summary.results[,"mean.MAPE"], digits = 3)
all.results[, "mape"] = round(all.results[, "mape"], digits = 3)
all.results[, "predicted"] = round(all.results[, "predicted"], digits = 0)

pred2 = all.results[, 'predicted']

confuMat = confusionMatrix(actual, pred2)

# Creation de fichiers CSV avec les r�sultats de la pr�diction
file.remove("./errMoy.csv")
write.csv(summary.results, "./errMoy.csv", row.names = F)
file.remove("./results.csv")
write.csv(all.results, "./results.csv", row.names = F)