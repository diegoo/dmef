
#Arbol con libreria  rpart
# source( "d:\\uba\\a2016\\codigoR\\rpart_binaria2_especial.r" )

library(rpart)
library(caret)




#definicion  funcion ganancia_puntual para nuestro problema
ganancia_puntual = function( probs, clases, prob_puntual )
{
  suma = 0 ;
  largo = length( clases ) ;

  for( i in 1:largo )
  {
    if( probs[ i, "POS"]  == prob_puntual   ){ suma <- suma + if( clases[i]=="BAJA+2" ) { 7750 } else { -250 }  
     } ;
  }

  return( suma )
}



#definicion  funcion ganancia_lista para una lista de probabilidades
ganancia_lista = function( probs, clases, prob_lista )
{
  suma = 0 ;
  largo = length( clases ) ;

  for( i in 1:largo )
  {
    if( probs[ i, "POS"] %in% prob_lista   ){ suma <- suma + if( clases[i]=="BAJA+2" ) { 7750 } else { -250 }  
     } ;
  }

  return( suma )
}


#funcion que devuelve las probabilidades de clase_binaria2  para las cuales la ganancia es positiva
hojas_positivas = function( probs, clases )	
{
 
  vunicos =  unique( probs[ , "POS"], incomparables=FALSE ) ;
  
  hojas_lista <- c() ;

  largo = length( vunicos ) ;


  for( i in 1:largo )
  {
     vgan =  ganancia_puntual( probs, clases, vunicos[i] )  
     if( vgan>0 )  hojas_lista = append( hojas_lista,  vunicos[i] ) ;
  }


  return(   hojas_lista  )
}



archivo_entrada <- "d:\\uba\\a2016\\abril_binario2.txt"
archivo_salida  <- "d:\\uba\\a2016\\salida_binaria2.txt"


#escribo los  titulos  del archivo salida
if( !file.exists( archivo_salida) )
{
	cat( "fecha", "archivo", "algoritmo", "peso", "cp" , "minsplit", "minbucket", "maxdepth", "ganancia_promedio", "tiempo_promedio", "ganancias" , "\n", sep="\t", file=archivo_salida, fill=FALSE, append=FALSE )
}

lineas_archivo <-  length( readLines(archivo_salida) )  - 1



#En este caso, el dataset trae el campo "clase"  y "clase_binaria2"  que junta  "BAJA+1" y "BAJA+2"  como positivos 
abril_dataset <- read.table( archivo_entrada, header=TRUE, sep="\t", row.names="numero_de_cliente")


semilla <- c( 102191, 200177, 410551, 552581, 892237 )

linea <- 1

for(  vcp  in  c( 0, 0.0005 ) )
{
for( vminsplit  in  c(  20, 50, 100, 200, 300, 400 )  )
{
for( vminbucket  in  c( trunc(vminsplit/4), trunc(vminsplit/3) )  )
{
for(  vmaxdepth  in  c(  5, 6, 7,8,9,10, 11, 12 ) )
{

	tiempos   <- c()

	ganancias <- c()

	if( linea > lineas_archivo )
	{

		for( s in  1:5 )
		{
			# Genero training y testing con 70% , 30%
			set.seed( semilla[s] )
			abril_inTraining <- createDataPartition( abril_dataset$clase_binaria2, p = .70, list = FALSE)
			abril_dataset_training <- abril_dataset[ abril_inTraining,]
			abril_dataset_testing  <- abril_dataset[-abril_inTraining,]

			# Quito la variable  "clase" (que es la original)  de training
			abril_dataset_training_sinclaseoriginal <- abril_dataset_training[ , !(names(abril_dataset_training) %in%  c("clase")  )]

			# generacion del modelo sobre los datos de training
			t0 =  Sys.time()
			abril_modelo  <- rpart( clase_binaria2 ~ .   ,   data = abril_dataset_training_sinclaseoriginal,  method="class", xval=0, maxsurrogate=1, surrogatestyle=1,   cp=vcp, minsplit=vminsplit, minbucket=vminbucket, maxdepth=vmaxdepth )  
			t1 = Sys.time()
			tiempos[s] <-  as.numeric(  t1 - t0, units = "secs")

		
			#determino las hojas con ganancia positiva en TRAINING
			abril_training_prediccion  = predict(  abril_modelo, abril_dataset_training , type = "prob")
			vhojas_positivas = hojas_positivas( abril_training_prediccion,  abril_dataset_training$clase )


			#calculo la ganancia en TESTING
			abril_testing_prediccion  = predict(  abril_modelo, abril_dataset_testing , type = "prob")
			ganancias[ s] = ganancia_lista( abril_testing_prediccion,  abril_dataset_testing$clase,  vhojas_positivas  ) / 0.30
			
		}


		cat( format(Sys.time(), "%Y%m%d %H%M%S"), archivo_entrada, "rpart", "sin_peso", vcp , vminsplit, vminbucket, vmaxdepth,  mean(ganancias), mean(tiempos), ganancias[1], ganancias[2], ganancias[3], ganancias[4], ganancias[5], "\n", sep="\t", file=archivo_salida, fill=FALSE, append=TRUE )

	
	}

	linea <- linea + 1




	#ahora calculo CON pesos

	tiempos <- c()


	if( linea > lineas_archivo )
	{


		for( s in  1:5 )
		{
			# Genero training y testing con 70% , 30%
			set.seed( semilla[s] )
			abril_inTraining <- createDataPartition( abril_dataset$clase_binaria2, p = .70, list = FALSE)
			abril_dataset_training <- abril_dataset[ abril_inTraining,]
			abril_dataset_testing  <- abril_dataset[-abril_inTraining,]

			# Quito la variable  "clase"  de training
			abril_dataset_training_sinclaseoriginal <- abril_dataset_training[ , !(names(abril_dataset_training) %in%  c("clase")  )]


			#Asigno pesos <7750, 250>  es equivalente a  <31, 1>  
			vweights <- ifelse( abril_dataset_training_sinclaseoriginal$clase_binaria2=='POS', 15, 1 )

			# generacion del modelo sobre los datos de training
			t0 =  Sys.time()
			abril_modelo  <- rpart( clase_binaria2 ~ .   ,   data = abril_dataset_training_sinclaseoriginal,  weights=vweights,  method="class", xval=0, maxsurrogate=1, surrogatestyle=1,   cp=vcp, minsplit=vminsplit, minbucket=vminbucket, maxdepth=vmaxdepth )  
			t1 = Sys.time()
			tiempos[s] <-  as.numeric(  t1 - t0, units = "secs")



			#determino las hojas con ganancia positiva en TRAINING con la clase origina
			abril_training_prediccion  = predict(  abril_modelo, abril_dataset_training , type = "prob")
			vhojas_positivas = hojas_positivas( abril_training_prediccion,  abril_dataset_training$clase )


			#calculo la ganancia en TESTING
			abril_testing_prediccion  = predict(  abril_modelo, abril_dataset_testing , type = "prob")
			ganancias[ s] = ganancia_lista( abril_testing_prediccion,  abril_dataset_testing$clase,  vhojas_positivas  ) / 0.30
	

		}

		cat( format(Sys.time(), "%Y%m%d %H%M%S"), archivo_entrada, "rpart", "con_peso", vcp , vminsplit, vminbucket, vmaxdepth,  mean(ganancias), mean(tiempos), ganancias[1], ganancias[2], ganancias[3], ganancias[4], ganancias[5], "\n", sep="\t", file=archivo_salida, fill=FALSE, append=TRUE )


	}

	linea <- linea + 1


}
}
}
}



