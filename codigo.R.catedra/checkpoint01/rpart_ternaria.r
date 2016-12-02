
#Arbol con libreria  rpart
# source( "d:\\uba\\a2016\\checkpoint01\\rpart_ternaria.r" )

library(rpart)
library(caret)




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
    if( probs[ i, "BAJA+2"]  > prob   )
    { 
	if( clases[i]=="BAJA+1" )    { vbaja1    <- vbaja1 + 1 }
	if( clases[i]=="BAJA+2" )    { vbaja2    <- vbaja2 + 1 }
	if( clases[i]=="CONTINUA" )  { vcontinua <- vcontinua + 1 }

     	suma <- suma + if( clases[i]=="BAJA+2" ) { 7750 } else { -250 } 
	 
    } ;
  }

  return(  c( suma, vbaja1, vbaja2, vcontinua)   )
}





archivo_entrada     <- "d:\\uba\\a2016\\producto_premium_201604.txt"
archivo_checkpoint  <- "d:\\uba\\a2016\\checkpoint01\\201604_checkpoint_conclase.txt"


abril_dataset      <- read.table( archivo_entrada,    header=TRUE, sep="\t", row.names="numero_de_cliente")
checkpoint_dataset <- read.table( archivo_checkpoint, header=TRUE, sep="\t", row.names="numero_de_cliente")



#estos son los parametros ganadores con clase ternaria,  con_peso
vcp        =   0.000
vminsplit  = 200
vminbucket =  66
vmaxdepth  =   9



#entreno el modelo sobre TODOS los datos de abril
vweights <- ifelse( abril_dataset$clase=='BAJA+2', 31, 1 )

abril_modelo  <- rpart( clase ~ .   ,   data = abril_dataset,  weights=vweights, method="class", xval=0, maxsurrogate=1, surrogatestyle=1,   cp=vcp, minsplit=vminsplit, minbucket=vminbucket, maxdepth=vmaxdepth )  



#aplico el modelo al dataset
checkpoint_prediccion  = predict(  abril_modelo, checkpoint_dataset , type = "prob")

#como tengo el dataset de checkpoint con la clase, yo puedo calcular la ganancia
vgan = ganancia( checkpoint_prediccion,  checkpoint_dataset$clase,  0.50 )
cat( vgan )


#175250 35 33 287
