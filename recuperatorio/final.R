library(xgboost)
library(caret)

imputation.method <- -9999999999
semilla <- 102191
archivo.entrada <- "data/data.futuro.xgboost.tsv"
model.name <- "models/final.model"

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

run <- function(ntree.limit, probability.threshold) {
    model <- xgb.load(model.name)
    set.seed(semilla)
    final.prediction <- predict(model, dataset.m, ntreelimit = ntree.limit)
    stopifnot(length(rownames(dataset)) == length(final.prediction) / 2)
    predicted.ids <- get.ids(final.prediction, ids, probability.threshold)

    archivo.salida <- paste("prediction", semilla, ntree.limit, probability.threshold, "txt", sep = ".")
    write(predicted.ids, file = archivo.salida, sep = '\n')
    cat("predicted.ids: ", length(predicted.ids), '\n')
}

## > run(420, 0.0335)
## predicted.ids:  8087 

## good luck!

## --------------------------------------------------------------------------------

## final.model:

## > head(ganancias[order(-ganancias$total),], 40)
##     semilla ntree.limit probability.threshold sample.total   total
## 33   102191         420                0.0335       870750 2902500
## 45   102191         430                0.0325       867750 2892500
## 67   102191         450                0.0285       865250 2884167
## 44   102191         430                0.0315       857750 2859167
## 32   102191         420                0.0325       856500 2855000
## 105  102191         480                0.0275       856000 2853333
## 21   102191         410                0.0345       854250 2847500
## 92   102191         470                0.0275       853750 2845833
## 18   102191         410                0.0315       853000 2843333
## 80   102191         460                0.0285       852500 2841667
## 68   102191         450                0.0295       851250 2837500
## 8    102191         400                0.0345       850000 2833333
## 71   102191         450                0.0325       849500 2831667
## 20   102191         410                0.0335       849000 2830000
## 6    102191         400                0.0325       848000 2826667
## 10   102191         400                0.0365       846000 2820000
## 56   102191         440                0.0305       845000 2816667
## 55   102191         440                0.0295       844500 2815000
## 22   102191         410                0.0355       843000 2810000
## 46   102191         430                0.0335       842750 2809167
## 79   102191         460                0.0275       842500 2808333
## 54   102191         440                0.0285       840750 2802500
## 31   102191         420                0.0315       840500 2801667
## 19   102191         410                0.0325       840250 2800833
## 30   102191         420                0.0305       840000 2800000
## 93   102191         470                0.0285       839500 2798333
## 131  102191         500                0.0275       839000 2796667
## 9    102191         400                0.0355       838750 2795833
## 43   102191         430                0.0305       838250 2794167
## 69   102191         450                0.0305       837500 2791667
## 57   102191         440                0.0315       837250 2790833
## 118  102191         490                0.0275       837000 2790000
## 7    102191         400                0.0335       836500 2788333
## 42   102191         430                0.0295       836500 2788333
## 59   102191         440                0.0335       835500 2785000
## 84   102191         460                0.0325       833750 2779167
## 119  102191         490                0.0285       833750 2779167
## 70   102191         450                0.0315       833000 2776667
## 106  102191         480                0.0285       832500 2775000
## 58   102191         440                0.0325       831750 2772500
