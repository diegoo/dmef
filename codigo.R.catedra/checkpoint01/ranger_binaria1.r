
#Arbol con libreria  rpart
# source( "d:\\uba\\a2016\\checkpoint01\\ranger_binaria1.r" )

library(ranger)
library(caret)

library(randomForest)  #solo se usa para imputar nulos




#definicion  funcion ganancia modificada para que devuelva ganancia, baja+1, baja+2, continua

ganancia = function( probs, clases, prob )
{
  suma = 0 ;
  vbaja1 = 0 ;
  vbaja2 = 0 ;
  vcontinua = 0 ;

  largo = length( clases ) ;

  for( i in 1:largo )
  {
    if( probs[ i, "POS"]  > prob   )
    { 
	if( clases[i]=="BAJA+1" )    { vbaja1    <- vbaja1 + 1 }
	if( clases[i]=="BAJA+2" )    { vbaja2    <- vbaja2 + 1 }
	if( clases[i]=="CONTINUA" )  { vcontinua <- vcontinua + 1 }

     	suma <- suma + if( clases[i]=="BAJA+2" ) { 7750 } else { -250 } 
	 
    } ;
  }

  return(  c( suma, vbaja1, vbaja2, vcontinua)   )
}



archivo_entrada     <- "d:\\uba\\a2016\\abril_binario1.txt"
archivo_checkpoint  <- "d:\\uba\\a2016\\checkpoint01\\201604_checkpoint_conclase.txt"


abril_dataset      <- read.table( archivo_entrada,    header=TRUE, sep="\t", row.names="numero_de_cliente")
checkpoint_dataset <- read.table( archivo_checkpoint, header=TRUE, sep="\t", row.names="numero_de_cliente")


#imputo nulos
abril_dataset <-  na.roughfix( abril_dataset )
checkpoint_dataset <-  na.roughfix( checkpoint_dataset )


#parametros para ranger
vnum.trees     = 500
vmin.node.size = 20
vmtry          = 20   #un poco mayor que raizcuadrada(169)


#definicion de pesos
vcase.weights  <- ifelse( abril_dataset$clase_binaria1=='POS', 31, 1 )

abril_modelo   <- ranger(clase_binaria1 ~ ., data = abril_dataset, mtry= vmtry, case.weights= vcase.weights, num.trees=vnum.trees,  min.node.size=vmin.node.size, probability=TRUE )	
			


#aplico el modelo al dataset
checkpoint_prediccion  = predict(  abril_modelo, checkpoint_dataset )

#como tengo el dataset de checkpoint con la clase, yo puedo calcular la ganancia
#el punto de corte 0.0568 sale del programa que busca el umbral
vgan = ganancia( checkpoint_prediccion$predictions,  checkpoint_dataset$clase,  0.0568 )
cat(    vgan, "\n" )


