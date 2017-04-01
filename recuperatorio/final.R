library(xgboost)
library(caret)

imputation.method <- -9999999999
semilla <- 102191
archivo.entrada <- "data/data.futuro.xgboost.tsv"
model.name <- "models/final.logistic.4.model"

## --------------------------------------------------------------------------------

get.ids <- function(prediction, ids, probability.threshold) {
    predicted.ids <- c()
    for (i in 1:length(ids)) {
        if (prediction[i] > probability.threshold) { ## *2
            predicted.ids <- append(predicted.ids, ids[i])
        }
    }
    return(predicted.ids);
}

dataset <- read.table(archivo.entrada, header=TRUE, sep="\t", row.names="id_cliente")
dataset[is.na(dataset)] <- imputation.method
dataset.m <- as.matrix(dataset)
ids <- rownames(dataset)

stopifnot(nrow(dataset) == 152832)
stopifnot(rownames(dataset)[1] == 1267605)
stopifnot(rownames(dataset)[length(rownames(dataset))] == 1000461)
stopifnot(length(rownames(dataset)) == nrow(dataset))

run <- function(ntree.limit, probability.threshold) {
    model <- xgb.load(model.name)
    set.seed(semilla)
    final.prediction <- predict(model, dataset.m, ntreelimit = ntree.limit)
    ## stopifnot(length(rownames(dataset)) == length(final.prediction) / 2)
    predicted.ids <- get.ids(final.prediction, ids, probability.threshold)

    archivo.salida <- "predictions.logistic.4.txt" ## paste("prediction", semilla, ntree.limit, probability.threshold, imputation.method, "logistic.5.txt", sep = ".")
    write(predicted.ids, file = archivo.salida, sep = '\n')
    cat("predicted.ids: ", length(predicted.ids), '\n')
}

## final.model (tree)
## > run(420, 0.0335)
## predicted.ids:  8087 

## good luck!
