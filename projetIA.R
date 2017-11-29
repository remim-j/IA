# load train and test data
data.train = read.csv("datatrain.csv")
data.test = read.csv("datatest.csv")

# overlap test set with train set so we have all features for both 
#data.test = data.train[which(data.train$Version.key %in% data.test$Versionkey),]

dim(data.train)

dim(data.test)

# set up feature list 
y = "y"
features = c("arg1", "arg2", "arg3", "arg4", "arg5", "arg6", "arg7", "arg8", "arg9", "arg10",
             "arg11", "arg12", "arg13", "arg14", "arg15", "arg16", "arg17", "arg18", "arg19", "arg20",
             "arg21", "arg22", "arg23", "arg24", "arg25", "arg26", "arg27", "arg28", "arg29", "arg30",
             "arg31", "arg32", "arg33", "arg34", "arg35", "arg36", "arg37", "arg38", "arg39", "arg40",
             "arg41", "arg42", "arg43", "arg44", "arg45", "arg46", "arg47", "arg48", "arg49", "arg50",
             "arg51", "arg52", "arg53", "arg54", "arg55", "arg56", "arg57", "arg58", "arg59", "arg60",
             "arg61", "arg62", "arg63", "arg64", "arg65", "arg66", "arg67", "arg68", "arg69", "arg70",
             "arg71", "arg72", "arg73", "arg74", "arg75", "arg76", "arg77", "arg78", "arg79", "arg80",
             "arg81", "arg82", "arg83", "arg84", "arg85", "arg86", "arg87", "arg88", "arg89", "arg90")

test.size = 80
num.iterations = 20

summary.results = {}
all.results = {}
for (i in 1:num.iterations) {
  
  # randomly sample 40 points from their test set and deduct from train set
  ind1 = sample(x = nrow(data.test), size = test.size, replace = FALSE)
  data.test.sub = data.test[ind1,]
  
  #ind2 = which(data.train$Version.key %in% data.test.sub$Version.key)
  #data.train.sub = data.train[-ind2,]
  data.train.sub = data.train
  
  # model train set using features
  model = lm(formula = paste(y, paste(features, collapse=" + "), sep=" ~ "), data=data.train.sub)
  
  # predict test set 
  predicted = predict(model, data.test.sub)
  actual = data.test.sub[,y]
  
  # calculate one mape for every single prediction you made (i.e. 40 mapes)
  mape = (abs((actual-predicted)/actual))*100
  
  # collect results for reporting
  summary.results = rbind(summary.results, mean(mape))
  all.results = rbind(all.results, cbind(i, actual, predicted, mape))
  
}

# set row and column names
rownames(summary.results) = c(1:20)
colnames(summary.results) = c("mean.MAPE")
rownames(all.results) = c(1:1600)
colnames(all.results) = c("i", "actual", "predicted", "mape")

# round to whole percentages
summary.results[,"mean.MAPE"] = round(summary.results[,"mean.MAPE"], digits=0)
all.results[,"mape"] = round(all.results[,"mape"], digits=0)

# write results to file
write.csv(summary.results, "./summary.results.csv", row.names=F)
write.csv(all.results, "./all.mape.results.csv", row.names=F)