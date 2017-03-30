library(rpart)
library(caret)

ganancia = function(probs, clases) {
    suma = 0 ;
    for(i in 1:length(clases)) {
        if(probs[i, "SI"] > (250/8000)) { suma <- suma + if(clases[i]=="SI") { 7750 } else { -250 } };
    }
    return(suma)
}

cat("fecha", "archivo", "algoritmo", "cp" , "minsplit", "minbucket", "maxdepth", "ganancia_promedio", "tiempo_promedio", "ganancias", "\n", sep="\t", file="ganancia_rpart.tsv", fill=FALSE, append=FALSE)

data <- read.table("data/data.con.nuevas.variables.SI.NO.tsv", header=TRUE, sep="\t", row.names="id_cliente")
summary(data$clase)

vcp <- 0;
vminsplit <- 50;
vminbucket <- 8;

for(vmaxdepth in 12:16) {
    semilla <- c(102191, 200177, 410551) ## , 552581, 892237)
    ganancias <- c() 
    tiempos <- c()
    for(s in 1:length(semilla)) {
        set.seed(semilla[s])
        inTraining <- createDataPartition(data$clase, p = .70, list = FALSE)
        data_training <- data[inTraining,]
        data_testing  <- data[-inTraining,]
        t0 =  Sys.time()
        modelo <- rpart(clase ~ ., data = data_training, cp=vcp, minsplit=vminsplit, minbucket=vminbucket, maxdepth=vmaxdepth)  
        t1 = Sys.time()
        tiempos[s] <- as.numeric(t1 - t0, units = "secs")
        testing_prediccion = predict(modelo, data_testing, type = "prob")
        ganancias[s] = ganancia(testing_prediccion, data_testing$clase) / 0.30
    }
    cat(format(Sys.time(), "%Y%m%d %H%M%S"), "data/data.con.nuevas.variables.tsv", "rpart", vcp, vminsplit, vminbucket, vmaxdepth, mean(ganancias), mean(tiempos), ganancias, "\n", sep="\t", file="ganancia_rpart.tsv", fill=FALSE, append=TRUE)
}
