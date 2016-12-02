library(xgboost)
library(caret)

calcular.ganancia = function(probs, clases, prob) {
    suma = 0 ;
    vbaja1 = 0 ;
    vbaja2 = 0 ;
    vcontinua = 0 ;
    largo = length( clases ) ;
    for( i in 1:largo ) {
        if( probs[ 2*i ] > prob ) { 
            if( clases[i]=="BAJA+1" )    { vbaja1    <- vbaja1 + 1 }
            if( clases[i]=="BAJA+2" )    { vbaja2    <- vbaja2 + 1 ; cat("baja+2",probs[2*i],"\n")}
            if( clases[i]=="CONTINUA" )  { vcontinua <- vcontinua + 1 }
            suma <- suma + if( clases[i]=="BAJA+2" ) { 7750 } else { -250 } 
        } ;
    }
    return(c(suma, vbaja1, vbaja2, vcontinua))
}

dataset_filename <- "../data/producto_premium_201604.txt"
dataset <- read.table(dataset_filename, header=TRUE, sep="\t", row.names="numero_de_cliente") ## stringsAsFactor=FALSE, 
dataset$clase10 <- ifelse(dataset$clase == 'BAJA+2', 1, 0)

## agregar variables nuevas

cat("adding variables...\n")

nuevas.1 <- read.csv('abril/extra_abril_1.export', sep='\t', header=TRUE, stringsAsFactor=FALSE, na.strings="NULL")
nuevas.1$numero_de_cliente <- NULL
dataset <- cbind(dataset, nuevas.1)

nuevas.2 <- read.csv('abril/extra_abril_2.export', sep='\t', header=TRUE, stringsAsFactor=FALSE, na.strings="NULL")
nuevas.2$numero_de_cliente <- NULL

dataset$rel_Visa_Finiciomora    <- nuevas.2$rel_Visa_Finiciomora
dataset$rel_Master_Finiciomora  <- nuevas.2$rel_Master_Finiciomora
dataset$rel_Master_fechaalta    <- nuevas.2$rel_Master_fechaalta
dataset$rel_Visa_fechaalta      <- nuevas.2$rel_Visa_fechaalta

dataset$max_mtransferencias_emitidas        <- nuevas.2$max_mtransferencias_emitidas      
dataset$avg_ctarjeta_master_transacciones   <- nuevas.2$avg_ctarjeta_master_transacciones 
dataset$avg_ctarjeta_visa_transacciones     <- nuevas.2$avg_ctarjeta_visa_transacciones   
dataset$min_ccajeros_ajenos_transaccione    <- nuevas.2$min_ccajeros_ajenos_transaccione  
dataset$max_ccajeros_ajenos_transacciones   <- nuevas.2$max_ccajeros_ajenos_transacciones 
dataset$avg_ctarjeta_master_transacciones   <- nuevas.2$avg_ctarjeta_master_transacciones 
dataset$avg_ctarjeta_visa_transacciones     <- nuevas.2$avg_ctarjeta_visa_transacciones   
dataset$avg_Master_mpagospesos              <- nuevas.2$avg_Master_mpagospesos            
dataset$avg_Visa_mpagospesos                <- nuevas.2$avg_Visa_mpagospesos              
dataset$avg_Master_tconsumos                <- nuevas.2$avg_Master_tconsumos              
dataset$avg_Visa_tconsumos                  <- nuevas.2$avg_Visa_tconsumos                
dataset$min_Master_tconsumos                <- nuevas.2$min_Master_tconsumos              
dataset$min_Visa_tconsumos                  <- nuevas.2$min_Visa_tconsumos                
dataset$max_Master_tconsumos                <- nuevas.2$max_Master_tconsumos              
dataset$max_Visa_tconsumos                  <- nuevas.2$max_Visa_tconsumos                
dataset$avg_ccallcenter_transacciones       <- nuevas.2$avg_ccallcenter_transacciones

nuevas.2.tendencias <- read.csv('abril/extra_abril_2_tendencias.tsv', sep='\t', header=FALSE, stringsAsFactor=FALSE, na.strings="NULL")
colnames(nuevas.2.tendencias) <- paste(colnames(nuevas.2.tendencias), "nuevas2tendencias", sep = "_")
dataset <- cbind(dataset, nuevas.2.tendencias)
dataset$V73_nuevas2tendencias <- NULL
dataset$V74_nuevas2tendencias <- NULL
dataset$V75_nuevas2tendencias <- NULL
dataset$V76_nuevas2tendencias <- NULL

nuevas.3.tendencias <- read.csv('abril/extra_abril_3_tendencias.tsv', sep='\t', header=FALSE, stringsAsFactor=FALSE, na.strings="NULL")
colnames(nuevas.3.tendencias) <- paste(colnames(nuevas.3.tendencias), "nuevas3tendencias", sep = "_")
dataset <- cbind(dataset, nuevas.3.tendencias)

nuevas.4 <- read.csv('abril/extra_abril_4.export', sep='\t', header=TRUE, stringsAsFactor=FALSE, na.strings="NULL")
nuevas.4$numero_de_cliente <- NULL
dataset <- cbind(dataset, nuevas.4)

str(dataset, list.len=ncol(dataset))

## borrar fechas absolutas
dataset$Visa_Finiciomora <- NULL
dataset$Master_Finiciomora <- NULL
dataset$Visa_fechaalta <- NULL
dataset$Master_fechaalta <- NULL

## imputar nulos
## library(randomForest)
## dataset.imputed <- na.roughfix(dataset)
dataset.imputed <- dataset
dataset.imputed[is.na(dataset.imputed)] <- -999999.0

set.seed(200177)
inTraining <- createDataPartition(dataset.imputed$clase10, p = .70, list = FALSE)
dataset_training <- dataset.imputed[inTraining,]
dataset_testing  <- dataset.imputed[-inTraining,]

cat("splitting train/test...\n")
dataset_training.sinclase <- dataset_training[, !(names(dataset_training) %in% c("clase", "clase10"))]
dataset_testing.sinclase  <- dataset_testing[, !(names(dataset_testing) %in% c("clase", "clase10"))]

t0 = Sys.time()
cat("training...\n")
dtrain = xgb.DMatrix(data = data.matrix(dataset_training.sinclase), label = dataset_training$clase10)
modelo <- xgboost( 
    data = dtrain, 
    eta = 0.01, 
    subsample = 1.0, 
    colsample_bytree = 0.6, 
    min_child_weight = 5, 
    max_depth = 11,
    alpha = 0, lambda = 0.1, gamma = 0.01,
    nround= 650, 
    num_class = 2,
    objective="multi:softprob", eval_metric= "merror",
    nthread = 6)
cat("tiempo:", as.numeric(Sys.time() - t0, units = "secs"), "\n") ### 899.4762

## load model
## cat("loading...\n")
## modelo <- xgb.load('../modelo.bin')

## predict
cat("predicting...\n")
prediccion = predict(modelo, data.matrix(dataset_testing.sinclase))

## calculate gananacia on testing
## cat("baja+2 esperadas:", nrow(subset(dataset_testing, clase == 'BAJA+2')), "\n")
ganancia = calcular.ganancia(prediccion, dataset_testing$clase, 0.03125) ## 0.030
cat("ganancia | baja+1, baja+2, continua | ganancia / 0.30:", ganancia, ganancia[1] / 0.30, "\n")

## save model
## cat("saving...\n")
## xgb.save(modelo, 'modelo.bin')
