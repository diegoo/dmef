library(plyr)

## archivo_entrada <- "data/data.con.nuevas.variables.FUTURO.tsv"
## archivo_salida  <- "data/dataset.xgboost.futuro.tsv"
archivo_entrada <- "data/data.futuro.tsv"
archivo_salida  <- "data/data.futuro.xgboost.tsv"

dataset <- read.table(archivo_entrada, header=TRUE, sep="\t")

dataset$binaria.Master_tadelantosefectivo                  <- ifelse(dataset$Master_tadelantosefectivo=="Y", 1, 0)
dataset$binaria.Master_tconsumos                           <- ifelse(dataset$Master_tconsumos=="Y", 1, 0)
dataset$binaria.Visa_tadelantosefectivo                    <- ifelse(dataset$Visa_tadelantosefectivo=="Y", 1, 0)
dataset$binaria.Visa_tconsumos                             <- ifelse(dataset$Visa_tconsumos=="Y", 1, 0)
dataset$binaria.tautoservicio                              <- ifelse(dataset$tautoservicio=="Y", 1, 0)
dataset$binaria.tcajas_consultas                           <- ifelse(dataset$tcajas_consultas=="Y", 1, 0)
dataset$binaria.tcaja_ahorro                               <- ifelse(dataset$tcaja_ahorro=="Y", 1, 0)
dataset$binaria.tcaja_seguridad                            <- ifelse(dataset$tcaja_seguridad=="Y", 1, 0)
dataset$binaria.tcajas                                     <- ifelse(dataset$tcajas=="Y", 1, 0)
dataset$binaria.tcajas_depositos                           <- ifelse(dataset$tcajas_depositos=="Y", 1, 0)
dataset$binaria.tcajas_extracciones                        <- ifelse(dataset$tcajas_extracciones=="Y", 1, 0)
dataset$binaria.tcajas_otras                               <- ifelse(dataset$tcajas_otras=="Y", 1, 0)
dataset$binaria.tcallcenter                                <- ifelse(dataset$tcallcenter=="Y", 1, 0)
dataset$binaria.tcambio_monedas                            <- ifelse(dataset$tcambio_monedas=="Y", 1, 0)
dataset$binaria.tcuenta_corriente                          <- ifelse(dataset$tcuenta_corriente=="Y", 1, 0)
dataset$binaria.tcuenta_debitos_automaticos                <- ifelse(dataset$tcuenta_debitos_automaticos=="Y", 1, 0)
dataset$binaria.tcuentas                                   <- ifelse(dataset$tcuentas=="Y", 1, 0)
dataset$binaria.tfondos_comunes_inversion                  <- ifelse(dataset$tfondos_comunes_inversion=="Y", 1, 0)
dataset$binaria.thomebanking                               <- ifelse(dataset$thomebanking=="Y", 1, 0)
dataset$binaria.tmovimientos_ultimos90dias                 <- ifelse(dataset$tmovimientos_ultimos90dias=="Y", 1, 0)
dataset$binaria.tpagodeservicios                           <- ifelse(dataset$tpagodeservicios=="Y", 1, 0)
dataset$binaria.tpagomiscuentas                            <- ifelse(dataset$tpagomiscuentas=="Y", 1, 0)
dataset$binaria.tpaquete1                                  <- ifelse(dataset$tpaquete1=="Y", 1, 0)
dataset$binaria.tpaquete2                                  <- ifelse(dataset$tpaquete2=="Y", 1, 0)
dataset$binaria.tpaquete3                                  <- ifelse(dataset$tpaquete3=="Y", 1, 0)
dataset$binaria.tpaquete4                                  <- ifelse(dataset$tpaquete4=="Y", 1, 0)
dataset$binaria.tpaquete5                                  <- ifelse(dataset$tpaquete5=="Y", 1, 0)
dataset$binaria.tpaquete6                                  <- ifelse(dataset$tpaquete6=="Y", 1, 0)
dataset$binaria.tpaquete7                                  <- ifelse(dataset$tpaquete7=="Y", 1, 0)
dataset$binaria.tpaquete8                                  <- ifelse(dataset$tpaquete8=="Y", 1, 0)
dataset$binaria.tpaquete9                                  <- ifelse(dataset$tpaquete9=="Y", 1, 0)
dataset$binaria.tplan_sueldo                               <- ifelse(dataset$tplan_sueldo=="Y", 1, 0)
dataset$binaria.tplazo_fijo                                <- ifelse(dataset$tplazo_fijo=="Y", 1, 0)
dataset$binaria.tseguro_accidentes_personales              <- ifelse(dataset$tseguro_accidentes_personales=="Y", 1, 0)
dataset$binaria.tseguro_auto                               <- ifelse(dataset$tseguro_auto=="Y", 1, 0)
dataset$binaria.tseguro_vida_mercado_abierto               <- ifelse(dataset$tseguro_vida_mercado_abierto=="Y", 1, 0)
dataset$binaria.tseguro_vivienda                           <- ifelse(dataset$tseguro_vivienda=="Y", 1, 0)
dataset$binaria.ttarjeta_debito                            <- ifelse(dataset$ttarjeta_debito=="Y", 1, 0)
dataset$binaria.ttarjeta_master                            <- ifelse(dataset$ttarjeta_master=="Y", 1, 0)
dataset$binaria.ttarjeta_master_debitos_automaticos        <- ifelse(dataset$ttarjeta_master_debitos_automaticos=="Y", 1, 0)
dataset$binaria.ttarjeta_visa                              <- ifelse(dataset$ttarjeta_visa=="Y", 1, 0)
dataset$binaria.ttarjeta_visa_debitos_automaticos          <- ifelse(dataset$ttarjeta_visa_debitos_automaticos=="Y", 1, 0)
dataset$binaria.ttitulos                                   <- ifelse(dataset$ttitulos=="Y", 1, 0)
## dataset$binaria.nueva.1                                    <- as.numeric(revalue(dataset$nueva.1, c("10_10"=1,"10_11"=2,"10_12"=3,"10_19"=4,"19_10"=6,"19_19"=7,"12_10"=10,"10_NA"=100,"19_NA"=101,"NA_10"=200,"NA_12"=201,"NA_NA"=300,"NA_19"=400,"12_NA"=500)))
dataset$binaria.nueva.1                                    <- as.numeric(revalue(dataset$nueva.1, c("10_10"=1, "10_11"=2, "10_12"=3, "10_19"=4,
                                                                                                    "11_10"=10,"11_11"=11,"11_12"=12,"11_19"=13,
                                                                                                    "12_10"=30,"12_11"=31,"12_12"=32,"12_19"=33,
                                                                                                    "19_10"=40,"19_11"=41,"19_12"=42,"19_19"=43,
                                                                                                    "10_NA"=100,"11_NA"=200,"12_NA"=300,"19_NA"=400,
                                                                                                    "NA_10"=400,"NA_11"=500,"NA_12"=600,"NA_19"=700,
                                                                                                    "NA_NA"=1000)))

dataset$clase <- NULL
dataset$Master_tadelantosefectivo <- NULL
dataset$Master_tconsumos <- NULL
dataset$Visa_tadelantosefectivo <- NULL
dataset$Visa_tconsumos <- NULL
dataset$tautoservicio <- NULL
dataset$tcajas_consultas <- NULL
dataset$tcaja_ahorro <- NULL
dataset$tcaja_seguridad <- NULL
dataset$tcajas <- NULL
dataset$tcajas_depositos <- NULL
dataset$tcajas_extracciones <- NULL
dataset$tcajas_otras <- NULL
dataset$tcallcenter <- NULL
dataset$tcambio_monedas <- NULL
dataset$tcuenta_corriente <- NULL
dataset$tcuenta_debitos_automaticos <- NULL
dataset$tcuentas <- NULL
dataset$tfondos_comunes_inversion <- NULL
dataset$thomebanking <- NULL
dataset$tmovimientos_ultimos90dias <- NULL
dataset$tpagodeservicios <- NULL
dataset$tpagomiscuentas <- NULL
dataset$tpaquete1 <- NULL
dataset$tpaquete2 <- NULL
dataset$tpaquete3 <- NULL
dataset$tpaquete4 <- NULL
dataset$tpaquete5 <- NULL
dataset$tpaquete6 <- NULL
dataset$tpaquete7 <- NULL
dataset$tpaquete8 <- NULL
dataset$tpaquete9 <- NULL
dataset$tplan_sueldo <- NULL
dataset$tplazo_fijo <- NULL
dataset$tseguro_accidentes_personales <- NULL
dataset$tseguro_auto <- NULL
dataset$tseguro_vida_mercado_abierto <- NULL
dataset$tseguro_vivienda <- NULL
dataset$ttarjeta_debito <- NULL
dataset$ttarjeta_master <- NULL
dataset$ttarjeta_master_debitos_automaticos <- NULL
dataset$ttarjeta_visa <- NULL
dataset$ttarjeta_visa_debitos_automaticos <- NULL
dataset$ttitulos <- NULL
dataset$nueva.1 <- NULL
dataset$foto_mes <- NULL

dataset$Master_Fvencimiento <- NULL
dataset$Master_Finiciomora <- NULL
dataset$Master_fultimo_cierre <- NULL
dataset$Master_fechaalta <- NULL

dataset$Visa_Fvencimiento <- NULL
dataset$Visa_Finiciomora <- NULL
dataset$Visa_fultimo_cierre <- NULL
dataset$Visa_fechaalta <- NULL

write.table(dataset, file=archivo_salida, row.names=FALSE, quote=FALSE, sep="\t")
