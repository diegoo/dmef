library(ranger)
library(caret)
library(randomForest)

ganancia = function(probs, clases, prob)
{
    suma = 0 ;
    largo = length(clases) ;
    for(i in 1:largo)
    {
        if(probs[ i, "SI"] > prob ){ suma <- suma + if(clases[i]=="SI") { 7750 } else { -250 } 
        } ;
    }
    return(suma)
}

umbral_ganancia_optimo = function(probs, clases)
{
    vgan_maxima = -9999999.0 ;
    vumbral = 0 ;
    for(i in 0:80) 
    {
        vgan = ganancia( probs, clases, 0.020 + i/1000)
        if(vgan > vgan_maxima)
        {
            vgan_maxima = vgan ;
            vumbral = 0.020 + i/1000 ;
        }
    }
    return(vumbral)
}

archivo_entrada <- "data/data.con.nuevas.variables.SI.NO.tsv"
archivo_salida <- "ganancia_ranger.txt"

if(!file.exists(archivo_salida))
{
    cat("fecha", "archivo", "algoritmo", "nulos", "tipo_umbral", "num.trees", "vmin.node.size", "umbral_promedio", "ganancia_promedio", "tiempo_promedio", "ganancias" , "umbrales", "\n", sep="\t", file=archivo_salida, fill=FALSE, append=FALSE)
}
lineas_archivo <- length(readLines(archivo_salida)) - 1
dataset <- read.table(archivo_entrada, header=TRUE, sep="\t", row.names="id_cliente")
dataset.imputed <- na.roughfix(dataset)
metodo_imputacion <- "na.roughfix"
semilla <- c(102191, 200177, 410551) ## , 552581, 892237)
linea <- 1

for( vnum.trees in c(5, 10, 20, 50, 100, 200, 500, 800, 1000, 1500, 2000, 5000))
{
    for( vmin.node.size in c(10000, 5000, 3000, 2000, 1000, 800, 700, 600, 500, 300, 200, 100, 50, 20))
    {
	ganancias <- c() 
	tiempos <- c()
	if(linea > lineas_archivo )
	{
            for(s in 1:length(semilla))
            {
                set.seed(semilla[s])
                inTraining <- createDataPartition(dataset.imputed$clase, p = .70, list = FALSE)
                dataset_training <- dataset.imputed[inTraining,]
                dataset_testing <- dataset.imputed[-inTraining,]
                t0 = Sys.time()
                modelo <- ranger(clase ~ ., data = dataset_training , num.trees=vnum.trees, min.node.size=vmin.node.size, probability=TRUE)	
                t1 = Sys.time()
                tiempos[s] <- as.numeric( t1 - t0, units = "secs")
                testing_prediccion = predict( modelo, dataset_testing)
                ganancias[s] = ganancia(testing_prediccion$predictions, dataset_testing$clase, 250/8000 ) / 0.30
            }
            cat(format(Sys.time(), "%Y%m%d %H%M%S"), archivo_entrada, "ranger", metodo_imputacion, "umbral_fijo", vnum.trees, vmin.node.size, 250/8000, mean(ganancias), mean(tiempos), ganancias, "\n", sep="\t", file=archivo_salida, fill=FALSE, append=TRUE)
	}
	linea <- linea + 1
	umbrales <- c()
	if(linea > lineas_archivo )
	{
            for(s in 1:length(semilla))
            {
                set.seed(semilla[s])
                inTraining <- createDataPartition(dataset.imputed$clase, p = .70, list = FALSE)
                dataset_train <- dataset.imputed[ inTraining,]
                dataset_testing <- dataset.imputed[-inTraining,]
                inValidation <- createDataPartition(dataset_train$clase, p = .70, list = FALSE)
                dataset_training <- dataset_train[ inValidation, ]
                dataset_validation <- dataset_train[ -inValidation, ]
                modelo <- ranger(clase ~ ., data = dataset_training , num.trees=vnum.trees, min.node.size=vmin.node.size, probability=TRUE)	
                validation_prediccion = predict( modelo, dataset_validation)
                umbrales[s] <- umbral_ganancia_optimo(validation_prediccion$predictions, dataset_validation$clase) 
                t0 = Sys.time()
                modelo <- ranger(clase ~ ., data = dataset_train , num.trees=vnum.trees, min.node.size=vmin.node.size, probability=TRUE)	
                t1 = Sys.time()
                tiempos[s] <- as.numeric(t1 - t0, units = "secs")
                testing_prediccion = predict( modelo, dataset_testing)
                ganancias[s] = ganancia(testing_prediccion$predictions, dataset_testing$clase, umbrales[s] ) / 0.30
            }
            cat(format(Sys.time(), "%Y%m%d %H%M%S"), archivo_entrada, "ranger", metodo_imputacion, "umbral_estimado", vnum.trees, vmin.node.size, mean(umbrales), mean(ganancias), mean(tiempos), ganancias, umbrales, "\n", sep="\t", file=archivo_salida, fill=FALSE, append=TRUE)
	}
	linea <- linea + 1
    }
}
