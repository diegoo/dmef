library(xgboost)
library(caret)

ganancia = function(probs, clases, prob) {
    suma = 0 ;
    largo = length(clases) ;
    for(i in 1:largo) {
        if(probs[2*i] > prob) { suma <- suma + if(clases[i]==1) { 7750 } else { -250 } };
    }
    return(suma)
}

archivo.entrada <- "data/dataset.xgboost.tsv"
archivo.salida  <- "resultados/ganancia.xgboost.1.tsv"
if(!file.exists(archivo.salida)) {
    cat("fecha","archivo","algoritmo","nulos","vmax.depth","vmin.child.weight","vnround","ganancia.promedio","tiempo.promedio","ganancias","\n",sep="\t",file=archivo.salida,fill=FALSE,append=FALSE)
}
dataset <- read.table(archivo.entrada, header=TRUE, sep="\t", row.names="id_cliente")
dataset[is.na(dataset)] <- 0
vmetodo.imputacion <- "ceros"
semilla <- c(102191, 200177, 410551, 552581, 892237)

for(vmax.depth in c(7,11,12)) { ## c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12)) {
    for(vmin.child.weight in c(4,5,15)) { ## c(1, 5, 10, 20, 50, 100)) {
        ganancias <- c()
        tiempos   <- c()
        vnround <- 650
        for(s in 1:length(semilla)) {
            ## s <- 1
            set.seed(semilla[s])
            inTraining <- createDataPartition(dataset$binaria.clase, p = .70, list = FALSE)
            dataset.training <- dataset[ inTraining,]
            dataset.testing  <- dataset[-inTraining,]
            t0 =  Sys.time()
            dataset.training.sinclase <- dataset.training[, !(names(dataset.training) %in% c("binaria.clase"))]
            data.train = xgb.DMatrix(data = data.matrix(dataset.training.sinclase), label = dataset.training$binaria.clase)
            modelo <- xgboost(
                data = data.train,
                eta = 0.01,
                subsample = 1.0,
                colsample_bytree = 0.6,
                min_child_weight = vmin.child.weight,
                max_depth = vmax.depth,
                alpha = 0, lambda = 0.1, gamma = 0.01,
                nround= vnround,
                eval_metric = "merror",
                num_class = 2,
                objective='multi:softprob',
                nthread=6)
            t1 = Sys.time()
            tiempos[s] <- as.numeric(t1 - t0, units = "secs")
            dataset.testing.sinclase <- dataset.testing[, !(names(dataset.testing) %in% c("binaria.clase")) ]
            am <- as.matrix(dataset.testing.sinclase)
            for(i in 1:50) {
                testing.prediccion = predict(modelo, am, ntreelimit= i*20)
                ganancias[i*5 + s] = ganancia(testing.prediccion, dataset.testing$binaria.clase, 0.03125) / 0.30
            }
        }
        for(i in 1:50) {
            cat(format(Sys.time(),"%Y%m%d%H%M%S"),archivo.entrada,"xgboost",vmetodo.imputacion,vmax.depth,vmin.child.weight,i*20,mean(c(ganancias[i*5+1],ganancias[i*5+2],ganancias[i*5+3],ganancias[i*5+4],ganancias[i*5+5])),mean(tiempos),ganancias[i*5+1],ganancias[i*5+2],ganancias[i*5+3],ganancias[i*5+4],ganancias[i*5+5],"\n",sep="\t",file=archivo.salida,fill=FALSE,append=TRUE)
        }
    }
}
