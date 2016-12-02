USE dmef;

DROP TABLE IF EXISTS `extra_abril_5`;

CREATE TABLE `extra_abril_5` (
numero_de_cliente INT DEFAULT NULL,

 DECIMAL(7,5) DEFAULT NULL,

-- (mtarjeta_master_consumo + mtarjeta_visa_consumo, mautoservicio) / (Visa_msaldototal, Master_msaldototal)

* Master_Visa Fechainiciomora

DO IF (missing(Master_Finiciomora)).
COMPUTE MasVis_Finiciomora = Visa_Finiciomora.
ELSE.
COMPUTE  MasVis_Finiciomora =  Master_Finiciomora.
END IF.

DO IF (missing(Visa_Finiciomora)).
COMPUTE VisMas_Finiciomora = Master_Finiciomora.
ELSE.
COMPUTE  VisMas_Finiciomora = Visa_Finiciomora.
END IF.

COMPUTE MIN_VisyMas_Finiciomora = MIN(Master_Finiciomora,Visa_Finiciomora). 

COMPUTE MAX_VisyMas_Finiciomora = MAX(Master_Finiciomora,Visa_Finiciomora). 


* ceuntaestado max,min,avg,sum

* mlimitecompra max,min,avg,sum

*mfinanciación limite max,min,avg,sum

* mlimitecompra+mfinanciacionlimite por tarjeta

* mlimitecompra+mfinanciacionlimite  juntado ambas

*relacion monto cantidad transferencias emitidas

COMPUTE myc_transferencias_emitidas1=mtransferencias_emitidas * ctransferencias_emitidas. 
COMPUTE myc_transferencias_emitidas2=mtransferencias_emitidas / ctransferencias_emitidas. 

DO IF (missing(ctransferencias_emitidas) OR (ctransferencias_emitidas EQ 0)).
COMPUTE myc_transferencias_emitidas2 = 0.
ELSE.
COMPUTE myc_transferencias_emitidas2=mtransferencias_emitidas / ctransferencias_emitidas. 
END IF.

*relacion monto cantidad extraccionautoservico

COMPUTE myc_extraccion_autoservicio1=mextraccion_autoservicio * cextraccion_autoservicio. 
*COMPUTE myc_extraccion_autoservicio2=mextraccion_autoservicio / cextraccion_autoservicio. 

DO IF (missing(cextraccion_autoservicio) OR (cextraccion_autoservicio EQ 0)).
COMPUTE myc_extraccion_autoservicio2 = 0.
ELSE.
COMPUTE myc_extraccion_autoservicio2=mextraccion_autoservicio / cextraccion_autoservicio. 
END IF.

*Relacionmontocantidad cheques emitidos rechazados

COMPUTE myc_cheques_emitidos_rechazados1=mcheques_emitidos_rechazados * ccheques_emitidos_rechazados. 
*COMPUTE myc_cheques_emitidos_rechazados2=mcheques_emitidos_rechazados / ccheques_emitidos_rechazados. 
*
DO IF (missing(ccheques_emitidos_rechazados) OR (ccheques_emitidos_rechazados EQ 0)).
COMPUTE myc_cheques_emitidos_rechazados2 = 0.
ELSE.
COMPUTE myc_cheques_emitidos_rechazados2=mcheques_emitidos_rechazados / ccheques_emitidos_rechazados. 
END IF.

*Relacion monto y cantidad cheques depositados

COMPUTE myc_cheques_depositados1=mcheques_depositados * ccheques_depositados. 
*COMPUTE myc_cheques_depositados2=mcheques_depositados / ccheques_depositados. 
*
DO IF (missing(ccheques_depositados) OR (ccheques_depositados EQ 0)).
COMPUTE myc_cheques_depositados2 = 0.
ELSE.
COMPUTE myc_cheques_depositados2=mcheques_depositados / ccheques_depositados. 
END IF.

* Relación monto y cantidad extracciones cajeros automaticos

*COMPUTE myc_cajeros_propyajenos= (mcajeros_ajenos+mcajeros_propio) /  (ccajeros_ajenos_transacciones+ccajeros_propio_transacciones).
*
DO IF ( (missing(ccajeros_ajenos_transacciones) OR (ccajeros_ajenos_transacciones EQ 0)) AND (missing(ccajeros_propio_transacciones) OR (ccajeros_propio_transacciones EQ 0) ) ).
COMPUTE myc_cajeros_propyajenos = 0.
ELSE.
COMPUTE myc_cajeros_propyajenos= SUM(mcajeros_ajenos,mcajeros_propio)  / SUM(ccajeros_ajenos_transacciones,ccajeros_propio_transacciones).
END IF.

COMPUTE myc_cajeros_propyajenos2= SUM(mcajeros_ajenos, mcajeros_propio) * SUM(ccajeros_ajenos_transacciones, ccajeros_propio_transacciones).

* Suma de cantidad de Consultas

*COMPUTE consultas1=SUM(tcallcenter,tcajas_consultas,tcajas_otras, thomebanking). 

COMPUTE consultas2=SUM(ccallcenter_transacciones,cautoservicio_transacciones,ccajeros_propio_transacciones,ccajeros_ajenos_transacciones).

*Cantidad de prestamos
COMPUTE prestamos1=SUM(cprestamos_hipotecarios,cprestamos_personales). 

*Distintas relaciones sobre las transacciones de las tarjetas de crédito

COMPUTE tarjetas1=SUM(ctarjeta_debito_transacciones,ctarjeta_visa_transacciones,ctarjeta_master_transacciones). 

* Recodificacion

RECODE ttarjeta_debito ('Y'=1) ('N'=0) INTO recode_ttarjeta_debito.

RECODE ttarjeta_visa('Y'=1) ('N'=0) INTO recode_ttarjeta_visa.

RECODE ttarjeta_master ('Y'=1) ('N'=0) INTO recode_ttarjeta_master.

COMPUTE tarjetas2=(SUM( recode_ttarjeta_debito, recode_ttarjeta_visa, recode_ttarjeta_master)) * (SUM( recode_ttarjeta_debito, recode_ttarjeta_visa, recode_ttarjeta_master)).

DELETE VARIABLES recode_ttarjeta_debito recode_ttarjeta_visa recode_ttarjeta_master .


COMPUTE tarjetas3 = SUM(mtarjeta_master_consumo, mtarjeta_visa_consumo, mautoservicio).

COMPUTE tarjetas4 = SUM(mtarjeta_master_consumo, mtarjeta_visa_consumo).

DO IF (SUM(ctarjeta_master_transacciones,ctarjeta_visa_transacciones) EQ 0).
COMPUTE tarjetas5 = 0.
ELSE.
COMPUTE tarjetas5 = SUM(mtarjeta_master_consumo, mtarjeta_visa_consumo) / SUM(ctarjeta_master_transacciones,ctarjeta_visa_transacciones).
END IF.

COMPUTE tarjetas6 = SUM(mtarjeta_master_consumo, mtarjeta_visa_consumo) * SUM(ctarjeta_master_transacciones,ctarjeta_visa_transacciones).

*COMPUTE tarjetas7 = SUM(mtarjeta_master_consumo, mtarjeta_visa_consumo, mautoservicio) / SUM(ctarjeta_master_transacciones, ctarjeta_visa_transacciones, ctarjeta_debito_transacciones).
*
DO IF (SUM(ctarjeta_master_transacciones, ctarjeta_visa_transacciones, ctarjeta_debito_transacciones) EQ 0).
COMPUTE tarjetas7 = 0.
ELSE.
COMPUTE tarjetas7 = SUM(mtarjeta_master_consumo, mtarjeta_visa_consumo, mautoservicio) / SUM(ctarjeta_master_transacciones, ctarjeta_visa_transacciones, ctarjeta_debito_transacciones).
END IF.

COMPUTE tarjetas8 = SUM(mtarjeta_master_consumo, mtarjeta_visa_consumo, mautoservicio) * SUM(ctarjeta_master_transacciones, ctarjeta_visa_transacciones, ctarjeta_debito_transacciones).



* Visa y master marca_atraso

COMPUTE vis_mas_marca_atraso=SUM(Visa_marca_atraso,Master_marca_atraso). 

* Analisis saldos y pagos

COMPUTE salpag1=SUM(Visa_msaldototal,Master_msaldototal). 

COMPUTE salpag2=SUM(Visa_msaldopesos,Master_msaldopesos). 

COMPUTE salpag3=SUM(Visa_mconsumospesos,Master_mconsumospesos). 


COMPUTE salpag4=SUM(Visa_mpagado,Master_mpagado). 

COMPUTE salpag5=SUM(Visa_mpagospesos,Master_mpagospesos). 

COMPUTE salpag6=SUM(Visa_mpagominimo,Master_mpagominimo). 

COMPUTE salpag7=SUM(Visa_mpagospesos,Master_mpagospesos). 

*recodificacion de adelanto

RECODE Visa_tadelantosefectivo ('Y'=1) ('N'=0) INTO recode_Visa_tadelantosefectivo.
RECODE Master_tadelantosefectivo('Y'=1) ('N'=0) INTO recode_Master_tadelantosefectivo.
COMPUTE salpag8=SUM(recode_Visa_tadelantosefectivo,recode_Master_tadelantosefectivo) *SUM(recode_Visa_tadelantosefectivo,recode_Master_tadelantosefectivo).
DELETE VARIABLES recode_Visa_tadelantosefectivo recode_Master_tadelantosefectivo.

*Master visa fecha alta

COMPUTE MasVis_fechaalta=MAX(Visa_fechaalta,Master_fechaalta). 

*Analisis edad ultimos movimientos

COMPUTE UltMovsEdad1=marketing_activo_ultimos90dias* cliente_edad.

*COMPUTE UltMovsEdad2=marketing_activo_ultimos90dias/ cliente_edad.
*
DO IF (missing(cliente_edad) OR (cliente_edad EQ 0)).
COMPUTE UltMovsEdad2 = 0.
ELSE.
COMPUTE UltMovsEdad2=marketing_activo_ultimos90dias/ cliente_edad.
END IF.

*proporcional mlimitecompra

DO IF ( SUM(Visa_mlimitecompra,Master_mlimitecompra) EQ 0).
COMPUTE proporcion_saldo_compra = 0.
ELSE.
COMPUTE proporcion_saldo_compra = SUM(Visa_msaldototal,Master_msaldototal) / SUM(Visa_mlimitecompra,Master_mlimitecompra).
END IF.

DO IF ( SUM(Visa_mlimitecompra,Master_mlimitecompra) EQ 0).
COMPUTE proporcion_consumo_compra = 0.
ELSE.
COMPUTE proporcion_consumo_compra = SUM(mtarjeta_master_consumo, mtarjeta_visa_consumo, mautoservicio) / SUM(Visa_mlimitecompra,Master_mlimitecompra).
END IF.

DO IF ( SUM(Visa_msaldototal,Master_msaldototal) EQ 0).
COMPUTE proporcion_consumo_saldo = 0.
ELSE.
COMPUTE proporcion_consumo_saldo = SUM(mtarjeta_master_consumo, mtarjeta_visa_consumo, mautoservicio) / SUM(Visa_msaldototal,Master_msaldototal).
END IF.

* ccajeros_propios_descuentos  ctarjeta_debito_transacciones
COMPUTE MAX_ccajeros_ctarjeta=MAX(ccajeros_propios_descuentos ,ctarjeta_debito_transacciones). 
COMPUTE MIN_ccajeros_ctarjeta=MIN(ccajeros_propios_descuentos  ,ctarjeta_debito_transacciones). 
COMPUTE MEAN_ccajeros_ctarjeta=MEAN(ccajeros_propios_descuentos ,ctarjeta_debito_transacciones ).
COMPUTE SUM_ccajeros_ctarjeta=SUM(ccajeros_propios_descuentos ,ctarjeta_debito_transacciones). 

* Visa_madelantopesos  Master_madelantopesos
COMPUTE MAX_adelantos=MAX(Visa_madelantopesos ,Master_madelantopesos). 
COMPUTE MIN_adelantos=MIN(Visa_madelantopesos  ,Master_madelantopesos). 
COMPUTE MEAN_adelantos=MEAN(Visa_madelantopesos ,Master_madelantopesos ).
COMPUTE SUM_adelantos=SUM(Visa_madelantopesos ,Master_madelantopesos). 

* mcaja_ahorro_Paquete mcuentas_saldo mpasivos_margen
COMPUTE MAX_adelantos=MAX(mcaja_ahorro_Paquete ,mcuentas_saldo , mpasivos_margen). 
COMPUTE MIN_adelantos=MIN(mcaja_ahorro_Paquete ,mcuentas_saldo, mpasivos_margen). 
COMPUTE MEAN_adelantos=MEAN(mcaja_ahorro_Paquete ,mcuentas_saldo, mpasivos_margen ).
COMPUTE SUM_adelantos=SUM(mcaja_ahorro_Paquete ,mcuentas_saldo, mpasivos_margen). 

*marketing_activo_ultimos90dias mrentabilidad_annual
COMPUTE MAX_marketing_mrentabilidad=MAX(marketing_activo_ultimos90dias, mrentabilidad_annual). 
COMPUTE MIN_marketing_mrentabilidad=MIN(marketing_activo_ultimos90dias, mrentabilidad_annual). 
COMPUTE MEAN_marketing_mrentabilidad=MEAN(marketing_activo_ultimos90dias, mrentabilidad_annual ).
COMPUTE SUM_marketing_mrentabilidad=SUM(marketing_activo_ultimos90dias, mrentabilidad_annual). 

*mcomisiones_otras mcomisiones
COMPUTE MAX_mcomisiones=MAX(mcomisiones_otras , mcomisiones). 
COMPUTE MIN_mcomisiones=MIN(mcomisiones_otras, mcomisiones). 
COMPUTE MEAN_mcomisiones=MEAN(mcomisiones_otras, mcomisiones ).
COMPUTE SUM_mcomisiones=SUM(mcomisiones_otras, mcomisiones). 

*marketing_coss_selling 	ccomisiones_otras
COMPUTE MAX_marketing_ccomisiones=MAX(marketing_coss_selling , ccomisiones_otras). 
COMPUTE MIN_marketing_ccomisiones=MIN(marketing_coss_selling, ccomisiones_otras). 
COMPUTE MEAN_marketing_ccomisiones=MEAN(marketing_coss_selling, ccomisiones_otras ).
COMPUTE SUM_marketing_ccomisiones=SUM(marketing_coss_selling, ccomisiones_otras). 

**Visa_msaldototal mtarjeta_visa_consumo
COMPUTE MAX_Visa_msaldo_consumo=MAX(Visa_msaldototal , mtarjeta_visa_consumo). 
COMPUTE MIN_Visa_msaldo_consumo=MIN(Visa_msaldototal, mtarjeta_visa_consumo). 
COMPUTE MEAN_Visa_msaldo_consumo=MEAN(Visa_msaldototal, mtarjeta_visa_consumo ).
COMPUTE SUM_Visa_msaldo_consumo=SUM(Visa_msaldototal, mtarjeta_visa_consumo). 

**ctarjeta_visa_descuentos  ctarjeta_visa_transacciones
COMPUTE MAX_visa_descuentos_transacciones=MAX(ctarjeta_visa_descuentos , ctarjeta_visa_transacciones). 
COMPUTE MIN_visa_descuentos_transacciones=MIN(ctarjeta_visa_descuentos , ctarjeta_visa_transacciones). 
COMPUTE MEAN_visa_descuentos_transacciones=MEAN(ctarjeta_visa_descuentos , ctarjeta_visa_transacciones ).
COMPUTE SUM_visa_descuentos_transacciones=SUM(ctarjeta_visa_descuentos , ctarjeta_visa_transacciones). 

**Visa_msaldototal mtarjeta_visa_consumo  ctarjeta_visa_descuentos  ctarjeta_visa_transacciones
COMPUTE MAX_VISApopurri=MAX(Visa_msaldototal , mtarjeta_visa_consumo,ctarjeta_visa_descuentos,ctarjeta_visa_transacciones). 
COMPUTE MIN_VISApopurri=MIN(Visa_msaldototal , mtarjeta_visa_consumo,ctarjeta_visa_descuentos,ctarjeta_visa_transacciones). 
COMPUTE MEAN_VISApopurri=MEAN(Visa_msaldototal , mtarjeta_visa_consumo,ctarjeta_visa_descuentos,ctarjeta_visa_transacciones ).
COMPUTE SUM_VISApopurri=SUM(Visa_msaldototal , mtarjeta_visa_consumo,ctarjeta_visa_descuentos,ctarjeta_visa_transacciones). 

**Visa_mpagospesos  Master_mpagospesos
COMPUTE MAX_Visa_Master_mpagospesos=MAX(Visa_mpagospesos, Master_mpagospesos). 
COMPUTE MIN_Visa_Master_mpagospesos=MIN(Visa_mpagospesos, Master_mpagospesos). 
COMPUTE MEAN_Visa_Master_mpagospesos=MEAN(Visa_mpagospesos, Master_mpagospesos ).
COMPUTE SUM_Visa_Master_mpagospesos=SUM(Visa_mpagospesos, Master_mpagospesos). 

* MIN Master visa fecha alta
COMPUTE MIN_MasVis_fechaalta=MIN(Visa_fechaalta,Master_fechaalta). 

** MIN Master visa fecha alta
COMPUTE MEAN_MasVis_fechaalta=MEAN(Visa_fechaalta,Master_fechaalta). 

** COCIENTE  Master visa fecha alta
COMPUTE  COCIENTE_MasVis_fechaalta=Visa_fechaalta /Master_fechaalta. 

** SUM Master visa fecha alta
COMPUTE SUM_MasVis_fechaalta=SUM(Visa_fechaalta, Master_fechaalta). 

);

ALTER TABLE `extra_abril_5` ADD INDEX `numero_de_cliente_idx` (`numero_de_cliente`);
