library(xgboost)
library(caret)

semilla <- 102191
set.seed(semilla)
ntree.limit <- 480
imputation.method <- -9999999999
probability.threshold <- 0.03125
archivo.entrada <- "data/data.futuro.xgboost.tsv"
archivo.salida <- paste("prediction", semilla, ntree.limit, probability.threshold, ".txt")
model.name <- "models/xgboost.14.19.550.102191.-9999999999.FALSE.final.model"

## --------------------------------------------------------------------------------

get.ids <- function(prediction, ids, probability.threshold) {
    predicted.ids <- c()
    for (i in 1:length(ids)) {
        if (prediction[i*2] > probability.threshold) {
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

model <- xgb.load(model.name)
final.prediction <- predict(model, dataset.m, ntreelimit = ntree.limit)
stopifnot(length(rownames(dataset)) == length(final.prediction) / 2)
predicted.ids <- get.ids(final.prediction, ids, probability.threshold)
write(predicted.ids, file = archivo.salida, sep = '\n')

## good luck!
