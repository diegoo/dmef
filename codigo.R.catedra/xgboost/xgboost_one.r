
# XGBOOST  que es MULTICORE  !
# source( "d:\\uba\\a2016\\codigoR\\xgboost_one.r" )

library(xgboost)
library(caret)



#definicion  funcion ganancia para nuestro problema

ganancia = function( probs, clases, prob )
{
  suma = 0 ;
  largo = length( clases ) ;

  for( i in 1:largo )
  {
    if( probs[ 2*i ]  > prob   ){ suma <- suma + if( clases[i]=="POS" ) { 7750 } else { -250 }  
     } ;
  }

  return( suma )
}



archivo_entrada <- "d:\\uba\\a2016\\abril_binario1.txt"
archivo_salida  <- "d:\\uba\\a2016\\salida_xgboost_one.txt"



#escribo los  titulos  del archivo salida
if( !file.exists( archivo_salida) )
{
	cat( "fecha", "archivo", "algoritmo", "nulos", "vmax_depth", "vmin_child_weight", "vnround",  "ganancia_promedio", "tiempo_promedio", "ganancias" ,  "\n", sep="\t", file=archivo_salida, fill=FALSE, append=FALSE )
}

lineas_archivo <-  length( readLines(archivo_salida) )  - 1



abril_dataset <- read.table( archivo_entrada, header=TRUE, sep="\t", row.names="numero_de_cliente")


#quito campos que son constantes
drops <- c("foto_mes","tpaquete1", "tpaquete3", "tpaquete5", "tpaquete8", "tautoservicio", "tcajas_consultas", "participa" )
abril_dataset <- abril_dataset[ , !(names(abril_dataset) %in% drops)]


#imputo los nulos, ya que xgboost  no acepta nulos
abril_dataset[ is.na(abril_dataset)] <- 0
vmetodo_imputacion <- "ceros"


semilla <- c( 102191, 200177, 410551, 552581, 892237 )

linea <- 1


for(  vmax_depth  in  c( 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ) )
{
for(  vmin_child_weight  in  c( 1, 5, 10, 20, 50, 100 ) )
{


	ganancias <- c() 
	tiempos   <- c()

	vnround <- 1000	


	
	if( linea > lineas_archivo  )
	{
		for( s in  1:5 )
		{
			# Genero training y testing con 70% , 30%
			set.seed( semilla[s] )
			abril_inTraining <- createDataPartition( abril_dataset$clase_binaria1, p = .70, list = FALSE)
			abril_dataset_training <- abril_dataset[ abril_inTraining,]
			abril_dataset_testing  <- abril_dataset[-abril_inTraining,]


			# generacion del modelo sobre los datos de training
			t0 =  Sys.time()

			abril_dataset_training_sinclase <-   abril_dataset_training[ , !(names(abril_dataset_training) %in% c("clase_binaria1") ) ] 
			abril_modelo   <- xgboost( 
				data = as.matrix( abril_dataset_training_sinclase ), 
				label = as.matrix( as.numeric(  (abril_dataset_training$clase_binaria1)=="POS" )  ), 
 				eta = 0.01, 
 				subsample = 0.7, 
 				colsample_bytree = 0.4, 
 				min_child_weight = vmin_child_weight, 
 				max_depth = vmax_depth,
 				alpha = 0, lambda = 0.1, gamma = 0.01,
 				nround= vnround, 
 				eval_metric = "merror",
				num_class = 2,
 				objective='multi:softprob' ,
				 #scale_pos_weight = 31
				)

			t1 = Sys.time()
			tiempos[s] <-  as.numeric(  t1 - t0, units = "secs")

			# calculo la ganancia normalizada  en testing

			abril_dataset_testing_sinclase <-   abril_dataset_testing[ , !(names(abril_dataset_testing) %in% c("clase_binaria1") ) ] 
			am <-  as.matrix( abril_dataset_testing_sinclase  )

			for( i in 1:50 )
			{
				abril_testing_prediccion  = predict(  abril_modelo,  am ,  ntreelimit= i*20 )
				ganancias[ i*5 + s] = ganancia( abril_testing_prediccion,  abril_dataset_testing$clase_binaria1,  0.03125  ) / 0.30
	
			}
	
		}

		for( i in 1:50 )
		{
			cat( format(Sys.time(), "%Y%m%d %H%M%S"), archivo_entrada, "xgboost", vmetodo_imputacion,  vmax_depth, vmin_child_weight, i*20, mean( c(ganancias[i*5+1], ganancias[i*5+2], ganancias[i*5+3], ganancias[i*5+4], ganancias[i*5+5] )), mean(tiempos), ganancias[i*5+1], ganancias[i*5+2], ganancias[i*5+3], ganancias[i*5+4], ganancias[i*5+5], "\n", sep="\t", file=archivo_salida, fill=FALSE, append=TRUE )
		}


	}
	linea <-  linea + 50
}
}



