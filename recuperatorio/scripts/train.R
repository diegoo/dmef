library(xgboost)
library(caret)

archivo.entrada <- "data/dataset.xgboost.tsv"
dataset <- read.table(archivo.entrada, header=TRUE, sep="\t", row.names="id_cliente")
dataset[is.na(dataset)] <- 0
semilla <- 102191

f.ganancia <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  suma = 0 ;
  for(i in length(labels)) {
    if(preds[2*i] > 0.03125) { suma <- suma + if(labels[i]==1) { 7750 } else { -250 }} ;
  }
  return(list(metric = "ganancia", value = suma))
}

vmax.depth = 14
vmin.child.weight = 19
vnround <- 650
ntree.limit <- 430
nfold <- 5

set.seed(semilla)
inTraining <- createDataPartition(dataset$binaria.clase, p = .70, list = FALSE)
dataset.training <- dataset[ inTraining,]
dataset.testing  <- dataset[-inTraining,]
dataset.training.sinclase <- dataset.training[, !(names(dataset.training) %in% c("binaria.clase"))]

modelo <- xgb.cv(
    data = xgb.DMatrix(data = data.matrix(dataset.training.sinclase), label = dataset.training$binaria.clase, missing = NA),
    stratified = TRUE, nfold = nfold,
    eta = 0.01, subsample = 1.0, colsample_bytree = 0.6, alpha = 0, lambda = 0.1, gamma = 0.01,
    min_child_weight = vmin.child.weight, max_depth = vmax.depth, nround = vnround,
    objective = 'multi:softprob', num_class = 2, nthread = 8,
    feval = f.ganancia, maximize = TRUE) ## , early.stop.round = 4

## xgb.save(modelo, paste("xgboost", vmax.depth, vmin.child.weight, vnround, "model", sep="."))

dataset.testing.sinclase <- dataset.testing[, !(names(dataset.testing) %in% c("binaria.clase"))]
am <- as.matrix(dataset.testing.sinclase)
testing.prediccion = predict(modelo, am, ntreelimit = ntree.limit)

id.cliente <- rownames(dataset.testing)
predicciones.archivo.salida <- paste("prediccion", vmax.depth, vmin.child.weight, vnround, "txt", sep=".")
total <- 0
for (i in 1:nrow(dataset.testing)) {
    if (testing.prediccion[i*2] > 0.03125) {
        acierto <- dataset.testing[i, c("binaria.clase")] == 1
        total <- total + if (acierto) { 7750 } else { -250 }
        cat(id.cliente[i], testing.prediccion[i*2], acierto, "\n", sep="\t", file=predicciones.archivo.salida, fill=FALSE, append=TRUE)
   }
}
cat(total / 0.30, '\n') ## 2769167
