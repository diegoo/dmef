USE dmef;

DROP TABLE IF EXISTS `extra_junio_2`;

CREATE TABLE `extra_junio_2` (
numero_de_cliente INT,

min_Master_madelantodolares DECIMAL(12,2) DEFAULT NULL,
min_Master_madelantopesos DECIMAL(12,2) DEFAULT NULL,
min_Master_marca_atraso INT DEFAULT NULL,
min_Master_mconsumosdolares DECIMAL(12,2) DEFAULT NULL,
min_Master_mconsumospesos DECIMAL(12,2) DEFAULT NULL,
min_Master_mconsumototal DECIMAL(12,2) DEFAULT NULL,
min_Master_mfinanciacion_limite DECIMAL(12,2) DEFAULT NULL,
min_Master_mlimitecompra DECIMAL(12,2) DEFAULT NULL,
min_Master_mpagado DECIMAL(12,2) DEFAULT NULL,
min_Master_mpagominimo DECIMAL(12,2) DEFAULT NULL,
min_Master_mpagosdolares DECIMAL(12,2) DEFAULT NULL,
min_Master_mpagospesos DECIMAL(12,2) DEFAULT NULL,
min_Master_msaldodolares DECIMAL(12,2) DEFAULT NULL,
min_Master_msaldopesos DECIMAL(12,2) DEFAULT NULL,
min_Master_msaldototal DECIMAL(12,2) DEFAULT NULL,
min_Master_tadelantosefectivo INT DEFAULT NULL,
min_Master_tconsumos INT DEFAULT NULL,

min_Visa_madelantodolares DECIMAL(12,2) DEFAULT NULL,
min_Visa_madelantopesos DECIMAL(12,2) DEFAULT NULL,
min_Visa_marca_atraso INT DEFAULT NULL,
min_Visa_mconsumosdolares DECIMAL(12,2) DEFAULT NULL,
min_Visa_mconsumospesos DECIMAL(12,2) DEFAULT NULL,
min_Visa_mconsumototal DECIMAL(12,2) DEFAULT NULL,
min_Visa_mfinanciacion_limite DECIMAL(12,2) DEFAULT NULL,
min_Visa_mlimitecompra DECIMAL(12,2) DEFAULT NULL,
min_Visa_mpagado DECIMAL(12,2) DEFAULT NULL,
min_Visa_mpagominimo DECIMAL(12,2) DEFAULT NULL,
min_Visa_mpagosdolares DECIMAL(12,2) DEFAULT NULL,
min_Visa_mpagospesos DECIMAL(12,2) DEFAULT NULL,
min_Visa_msaldodolares DECIMAL(12,2) DEFAULT NULL,
min_Visa_msaldopesos DECIMAL(12,2) DEFAULT NULL,
min_Visa_msaldototal DECIMAL(12,2) DEFAULT NULL,
min_Visa_tadelantosefectivo INT DEFAULT NULL,
min_Visa_tconsumos INT DEFAULT NULL,

max_Master_madelantodolares DECIMAL(12,2) DEFAULT NULL,
max_Master_madelantopesos DECIMAL(12,2) DEFAULT NULL,
max_Master_marca_atraso INT DEFAULT NULL,
max_Master_mconsumosdolares DECIMAL(12,2) DEFAULT NULL,
max_Master_mconsumospesos DECIMAL(12,2) DEFAULT NULL,
max_Master_mconsumototal DECIMAL(12,2) DEFAULT NULL,
max_Master_mfinanciacion_limite DECIMAL(12,2) DEFAULT NULL,
max_Master_mlimitecompra DECIMAL(12,2) DEFAULT NULL,
max_Master_mpagado DECIMAL(12,2) DEFAULT NULL,
max_Master_mpagominimo DECIMAL(12,2) DEFAULT NULL,
max_Master_mpagosdolares DECIMAL(12,2) DEFAULT NULL,
max_Master_mpagospesos DECIMAL(12,2) DEFAULT NULL,
max_Master_msaldodolares DECIMAL(12,2) DEFAULT NULL,
max_Master_msaldopesos DECIMAL(12,2) DEFAULT NULL,
max_Master_msaldototal DECIMAL(12,2) DEFAULT NULL,
max_Master_tadelantosefectivo INT DEFAULT NULL,
max_Master_tconsumos INT DEFAULT NULL,

max_Visa_madelantodolares DECIMAL(12,2) DEFAULT NULL,
max_Visa_madelantopesos DECIMAL(12,2) DEFAULT NULL,
max_Visa_marca_atraso INT DEFAULT NULL,
max_Visa_mconsumosdolares DECIMAL(12,2) DEFAULT NULL,
max_Visa_mconsumospesos DECIMAL(12,2) DEFAULT NULL,
max_Visa_mconsumototal DECIMAL(12,2) DEFAULT NULL,
max_Visa_mfinanciacion_limite DECIMAL(12,2) DEFAULT NULL,
max_Visa_mlimitecompra DECIMAL(12,2) DEFAULT NULL,
max_Visa_mpagado DECIMAL(12,2) DEFAULT NULL,
max_Visa_mpagominimo DECIMAL(12,2) DEFAULT NULL,
max_Visa_mpagosdolares DECIMAL(12,2) DEFAULT NULL,
max_Visa_mpagospesos DECIMAL(12,2) DEFAULT NULL,
max_Visa_msaldodolares DECIMAL(12,2) DEFAULT NULL,
max_Visa_msaldopesos DECIMAL(12,2) DEFAULT NULL,
max_Visa_msaldototal DECIMAL(12,2) DEFAULT NULL,
max_Visa_tadelantosefectivo INT DEFAULT NULL,
max_Visa_tconsumos INT DEFAULT NULL,

avg_Master_madelantodolares DECIMAL(12,2) DEFAULT NULL,
avg_Master_madelantopesos DECIMAL(12,2) DEFAULT NULL,
avg_Master_marca_atraso INT DEFAULT NULL,
avg_Master_mconsumosdolares DECIMAL(12,2) DEFAULT NULL,
avg_Master_mconsumospesos DECIMAL(12,2) DEFAULT NULL,
avg_Master_mconsumototal DECIMAL(12,2) DEFAULT NULL,
avg_Master_mfinanciacion_limite DECIMAL(12,2) DEFAULT NULL,
avg_Master_mlimitecompra DECIMAL(12,2) DEFAULT NULL,
avg_Master_mpagado DECIMAL(12,2) DEFAULT NULL,
avg_Master_mpagominimo DECIMAL(12,2) DEFAULT NULL,
avg_Master_mpagosdolares DECIMAL(12,2) DEFAULT NULL,
avg_Master_mpagospesos DECIMAL(12,2) DEFAULT NULL,
avg_Master_msaldodolares DECIMAL(12,2) DEFAULT NULL,
avg_Master_msaldopesos DECIMAL(12,2) DEFAULT NULL,
avg_Master_msaldototal DECIMAL(12,2) DEFAULT NULL,
avg_Master_tadelantosefectivo INT DEFAULT NULL,
avg_Master_tconsumos INT DEFAULT NULL,

avg_Visa_madelantodolares DECIMAL(12,2) DEFAULT NULL,
avg_Visa_madelantopesos DECIMAL(12,2) DEFAULT NULL,
avg_Visa_marca_atraso INT DEFAULT NULL,
avg_Visa_mconsumosdolares DECIMAL(12,2) DEFAULT NULL,
avg_Visa_mconsumospesos DECIMAL(12,2) DEFAULT NULL,
avg_Visa_mconsumototal DECIMAL(12,2) DEFAULT NULL,
avg_Visa_mfinanciacion_limite DECIMAL(12,2) DEFAULT NULL,
avg_Visa_mlimitecompra DECIMAL(12,2) DEFAULT NULL,
avg_Visa_mpagado DECIMAL(12,2) DEFAULT NULL,
avg_Visa_mpagominimo DECIMAL(12,2) DEFAULT NULL,
avg_Visa_mpagosdolares DECIMAL(12,2) DEFAULT NULL,
avg_Visa_mpagospesos DECIMAL(12,2) DEFAULT NULL,
avg_Visa_msaldodolares DECIMAL(12,2) DEFAULT NULL,
avg_Visa_msaldopesos DECIMAL(12,2) DEFAULT NULL,
avg_Visa_msaldototal DECIMAL(12,2) DEFAULT NULL,
avg_Visa_tadelantosefectivo INT DEFAULT NULL,
avg_Visa_tconsumos INT DEFAULT NULL,

min_cautoservicio_transacciones INT DEFAULT NULL,
min_ccajeros_ajenos_transacciones INT DEFAULT NULL,
min_ccajeros_propio_transacciones INT DEFAULT NULL,
min_ccajeros_propios_descuentos INT DEFAULT NULL,
min_ccallcenter_transacciones INT DEFAULT NULL,
min_ccambio_monedas_compra INT DEFAULT NULL,
min_ccambio_monedas_venta INT DEFAULT NULL,
min_ccheques_depositados INT DEFAULT NULL,
min_ccheques_depositados_rechazados INT DEFAULT NULL,
min_ccheques_emitidos INT DEFAULT NULL,
min_ccheques_emitidos_rechazados INT DEFAULT NULL,
min_ccomisiones_mantenimiento INT DEFAULT NULL,
min_ccomisiones_otras INT DEFAULT NULL,
min_ccuenta_descuentos INT DEFAULT NULL,
min_cextraccion_autoservicio INT DEFAULT NULL,
min_chomebanking_transacciones INT DEFAULT NULL,
min_cprestamos_hipotecarios INT DEFAULT NULL,
min_cprestamos_personales INT DEFAULT NULL,
min_cprestamos_prendarios INT DEFAULT NULL,
min_ctarjeta_debito_transacciones INT DEFAULT NULL,
min_ctarjeta_master_descuentos INT DEFAULT NULL,
min_ctarjeta_master_transacciones INT DEFAULT NULL,
min_ctarjeta_visa_descuentos INT DEFAULT NULL,
min_ctarjeta_visa_transacciones INT DEFAULT NULL,
min_ctransferencias_emitidas INT DEFAULT NULL,
min_ctransferencias_recibidas INT DEFAULT NULL,

max_cautoservicio_transacciones INT DEFAULT NULL,
max_ccajeros_ajenos_transacciones INT DEFAULT NULL,
max_ccajeros_propio_transacciones INT DEFAULT NULL,
max_ccajeros_propios_descuentos INT DEFAULT NULL,
max_ccallcenter_transacciones INT DEFAULT NULL,
max_ccambio_monedas_compra INT DEFAULT NULL,
max_ccambio_monedas_venta INT DEFAULT NULL,
max_ccheques_depositados INT DEFAULT NULL,
max_ccheques_depositados_rechazados INT DEFAULT NULL,
max_ccheques_emitidos INT DEFAULT NULL,
max_ccheques_emitidos_rechazados INT DEFAULT NULL,
max_ccomisiones_mantenimiento INT DEFAULT NULL,
max_ccomisiones_otras INT DEFAULT NULL,
max_ccuenta_descuentos INT DEFAULT NULL,
max_cextraccion_autoservicio INT DEFAULT NULL,
max_chomebanking_transacciones INT DEFAULT NULL,
max_cprestamos_hipotecarios INT DEFAULT NULL,
max_cprestamos_personales INT DEFAULT NULL,
max_cprestamos_prendarios INT DEFAULT NULL,
max_ctarjeta_debito_transacciones INT DEFAULT NULL,
max_ctarjeta_master_descuentos INT DEFAULT NULL,
max_ctarjeta_master_transacciones INT DEFAULT NULL,
max_ctarjeta_visa_descuentos INT DEFAULT NULL,
max_ctarjeta_visa_transacciones INT DEFAULT NULL,
max_ctransferencias_emitidas INT DEFAULT NULL,
max_ctransferencias_recibidas INT DEFAULT NULL,

avg_cautoservicio_transacciones INT DEFAULT NULL,
avg_ccajeros_ajenos_transacciones INT DEFAULT NULL,
avg_ccajeros_propio_transacciones INT DEFAULT NULL,
avg_ccajeros_propios_descuentos INT DEFAULT NULL,
avg_ccallcenter_transacciones INT DEFAULT NULL,
avg_ccambio_monedas_compra INT DEFAULT NULL,
avg_ccambio_monedas_venta INT DEFAULT NULL,
avg_ccheques_depositados INT DEFAULT NULL,
avg_ccheques_depositados_rechazados INT DEFAULT NULL,
avg_ccheques_emitidos INT DEFAULT NULL,
avg_ccheques_emitidos_rechazados INT DEFAULT NULL,
avg_ccomisiones_mantenimiento INT DEFAULT NULL,
avg_ccomisiones_otras INT DEFAULT NULL,
avg_ccuenta_descuentos INT DEFAULT NULL,
avg_cextraccion_autoservicio INT DEFAULT NULL,
avg_chomebanking_transacciones INT DEFAULT NULL,
avg_cprestamos_hipotecarios INT DEFAULT NULL,
avg_cprestamos_personales INT DEFAULT NULL,
avg_cprestamos_prendarios INT DEFAULT NULL,
avg_ctarjeta_debito_transacciones INT DEFAULT NULL,
avg_ctarjeta_master_descuentos INT DEFAULT NULL,
avg_ctarjeta_master_transacciones INT DEFAULT NULL,
avg_ctarjeta_visa_descuentos INT DEFAULT NULL,
avg_ctarjeta_visa_transacciones INT DEFAULT NULL,
avg_ctransferencias_emitidas INT DEFAULT NULL,
avg_ctransferencias_recibidas INT DEFAULT NULL,

min_mactivos_margen DECIMAL(12,2) DEFAULT NULL,
min_marketing_coss_selling DECIMAL(12,2) DEFAULT NULL,
min_mautoservicio DECIMAL(12,2) DEFAULT NULL,
min_mbonos_corporativos DECIMAL(12,2) DEFAULT NULL,
min_mcaja_ahorro_Nopaquete DECIMAL(12,2) DEFAULT NULL,
min_mcaja_ahorro_Paquete DECIMAL(12,2) DEFAULT NULL,
min_mcaja_ahorro_dolares DECIMAL(12,2) DEFAULT NULL,
min_mcajeros_ajenos DECIMAL(12,2) DEFAULT NULL,
min_mcajeros_propio DECIMAL(12,2) DEFAULT NULL,
min_mcajeros_propios_descuentos DECIMAL(12,2) DEFAULT NULL,
min_mcambio_monedas_compra DECIMAL(12,2) DEFAULT NULL,
min_mcambio_monedas_venta DECIMAL(12,2) DEFAULT NULL,
min_mcheques_depositados DECIMAL(12,2) DEFAULT NULL,
min_mcheques_depositados_rechazados DECIMAL(12,2) DEFAULT NULL,
min_mcheques_emitidos DECIMAL(12,2) DEFAULT NULL,
min_mcheques_emitidos_rechazados DECIMAL(12,2) DEFAULT NULL,
min_mcomisiones DECIMAL(12,2) DEFAULT NULL,
min_mcomisiones_mantenimiento DECIMAL(12,2) DEFAULT NULL,
min_mcomisiones_otras DECIMAL(12,2) DEFAULT NULL,
min_mcuenta_corriente_Nopaquete DECIMAL(12,2) DEFAULT NULL,
min_mcuenta_corriente_Paquete DECIMAL(12,2) DEFAULT NULL,
min_mcuenta_corriente_dolares DECIMAL(12,2) DEFAULT NULL,
min_mcuenta_debitos_automaticos DECIMAL(12,2) DEFAULT NULL,
min_mcuenta_descuentos DECIMAL(12,2) DEFAULT NULL,
min_mcuentas_saldo DECIMAL(12,2) DEFAULT NULL,
min_mdescubierto_preacordado DECIMAL(12,2) DEFAULT NULL,
min_mextraccion_autoservicio DECIMAL(12,2) DEFAULT NULL,
min_mfondos_comunes_inversion_dolares DECIMAL(12,2) DEFAULT NULL,
min_mfondos_comunes_inversion_pesos DECIMAL(12,2) DEFAULT NULL,
min_minversiones_otras DECIMAL(12,2) DEFAULT NULL,
min_mmonedas_extranjeras DECIMAL(12,2) DEFAULT NULL,
min_mpagodeservicios DECIMAL(12,2) DEFAULT NULL,
min_mpagomiscuentas DECIMAL(12,2) DEFAULT NULL,
min_mpasivos_margen DECIMAL(12,2) DEFAULT NULL,
min_mprestamos_hipotecarios DECIMAL(12,2) DEFAULT NULL,
min_mprestamos_personales DECIMAL(12,2) DEFAULT NULL,
min_mprestamos_prendarios DECIMAL(12,2) DEFAULT NULL,
min_mrentabilidad DECIMAL(12,2) DEFAULT NULL,
min_mrentabilidad_annual DECIMAL(12,2) DEFAULT NULL,
min_mtarjeta_master_consumo DECIMAL(12,2) DEFAULT NULL,
min_mtarjeta_master_descuentos DECIMAL(12,2) DEFAULT NULL,
min_mtarjeta_visa_consumo DECIMAL(12,2) DEFAULT NULL,
min_mtarjeta_visa_descuentos DECIMAL(12,2) DEFAULT NULL,
min_mtitulos DECIMAL(12,2) DEFAULT NULL,
min_mtransferencias_emitidas DECIMAL(12,2) DEFAULT NULL,
min_mtransferencias_recibidas DECIMAL(12,2) DEFAULT NULL,
min_mttarjeta_master_debitos_automaticos DECIMAL(12,2) DEFAULT NULL,
min_mttarjeta_visa_debitos_automaticos DECIMAL(12,2) DEFAULT NULL,

max_mactivos_margen DECIMAL(12,2) DEFAULT NULL,
max_marketing_coss_selling DECIMAL(12,2) DEFAULT NULL,
max_mautoservicio DECIMAL(12,2) DEFAULT NULL,
max_mbonos_corporativos DECIMAL(12,2) DEFAULT NULL,
max_mcaja_ahorro_Nopaquete DECIMAL(12,2) DEFAULT NULL,
max_mcaja_ahorro_Paquete DECIMAL(12,2) DEFAULT NULL,
max_mcaja_ahorro_dolares DECIMAL(12,2) DEFAULT NULL,
max_mcajeros_ajenos DECIMAL(12,2) DEFAULT NULL,
max_mcajeros_propio DECIMAL(12,2) DEFAULT NULL,
max_mcajeros_propios_descuentos DECIMAL(12,2) DEFAULT NULL,
max_mcambio_monedas_compra DECIMAL(12,2) DEFAULT NULL,
max_mcambio_monedas_venta DECIMAL(12,2) DEFAULT NULL,
max_mcheques_depositados DECIMAL(12,2) DEFAULT NULL,
max_mcheques_depositados_rechazados DECIMAL(12,2) DEFAULT NULL,
max_mcheques_emitidos DECIMAL(12,2) DEFAULT NULL,
max_mcheques_emitidos_rechazados DECIMAL(12,2) DEFAULT NULL,
max_mcomisiones DECIMAL(12,2) DEFAULT NULL,
max_mcomisiones_mantenimiento DECIMAL(12,2) DEFAULT NULL,
max_mcomisiones_otras DECIMAL(12,2) DEFAULT NULL,
max_mcuenta_corriente_Nopaquete DECIMAL(12,2) DEFAULT NULL,
max_mcuenta_corriente_Paquete DECIMAL(12,2) DEFAULT NULL,
max_mcuenta_corriente_dolares DECIMAL(12,2) DEFAULT NULL,
max_mcuenta_debitos_automaticos DECIMAL(12,2) DEFAULT NULL,
max_mcuenta_descuentos DECIMAL(12,2) DEFAULT NULL,
max_mcuentas_saldo DECIMAL(12,2) DEFAULT NULL,
max_mdescubierto_preacordado DECIMAL(12,2) DEFAULT NULL,
max_mextraccion_autoservicio DECIMAL(12,2) DEFAULT NULL,
max_mfondos_comunes_inversion_dolares DECIMAL(12,2) DEFAULT NULL,
max_mfondos_comunes_inversion_pesos DECIMAL(12,2) DEFAULT NULL,
max_minversiones_otras DECIMAL(12,2) DEFAULT NULL,
max_mmonedas_extranjeras DECIMAL(12,2) DEFAULT NULL,
max_mpagodeservicios DECIMAL(12,2) DEFAULT NULL,
max_mpagomiscuentas DECIMAL(12,2) DEFAULT NULL,
max_mpasivos_margen DECIMAL(12,2) DEFAULT NULL,
max_mprestamos_hipotecarios DECIMAL(12,2) DEFAULT NULL,
max_mprestamos_personales DECIMAL(12,2) DEFAULT NULL,
max_mprestamos_prendarios DECIMAL(12,2) DEFAULT NULL,
max_mrentabilidad DECIMAL(12,2) DEFAULT NULL,
max_mrentabilidad_annual DECIMAL(12,2) DEFAULT NULL,
max_mtarjeta_master_consumo DECIMAL(12,2) DEFAULT NULL,
max_mtarjeta_master_descuentos DECIMAL(12,2) DEFAULT NULL,
max_mtarjeta_visa_consumo DECIMAL(12,2) DEFAULT NULL,
max_mtarjeta_visa_descuentos DECIMAL(12,2) DEFAULT NULL,
max_mtitulos DECIMAL(12,2) DEFAULT NULL,
max_mtransferencias_emitidas DECIMAL(12,2) DEFAULT NULL,
max_mtransferencias_recibidas DECIMAL(12,2) DEFAULT NULL,
max_mttarjeta_master_debitos_automaticos DECIMAL(12,2) DEFAULT NULL,
max_mttarjeta_visa_debitos_automaticos DECIMAL(12,2) DEFAULT NULL,

avg_mactivos_margen DECIMAL(12,2) DEFAULT NULL,
avg_marketing_coss_selling DECIMAL(12,2) DEFAULT NULL,
avg_mautoservicio DECIMAL(12,2) DEFAULT NULL,
avg_mbonos_corporativos DECIMAL(12,2) DEFAULT NULL,
avg_mcaja_ahorro_Nopaquete DECIMAL(12,2) DEFAULT NULL,
avg_mcaja_ahorro_Paquete DECIMAL(12,2) DEFAULT NULL,
avg_mcaja_ahorro_dolares DECIMAL(12,2) DEFAULT NULL,
avg_mcajeros_ajenos DECIMAL(12,2) DEFAULT NULL,
avg_mcajeros_propio DECIMAL(12,2) DEFAULT NULL,
avg_mcajeros_propios_descuentos DECIMAL(12,2) DEFAULT NULL,
avg_mcambio_monedas_compra DECIMAL(12,2) DEFAULT NULL,
avg_mcambio_monedas_venta DECIMAL(12,2) DEFAULT NULL,
avg_mcheques_depositados DECIMAL(12,2) DEFAULT NULL,
avg_mcheques_depositados_rechazados DECIMAL(12,2) DEFAULT NULL,
avg_mcheques_emitidos DECIMAL(12,2) DEFAULT NULL,
avg_mcheques_emitidos_rechazados DECIMAL(12,2) DEFAULT NULL,
avg_mcomisiones DECIMAL(12,2) DEFAULT NULL,
avg_mcomisiones_mantenimiento DECIMAL(12,2) DEFAULT NULL,
avg_mcomisiones_otras DECIMAL(12,2) DEFAULT NULL,
avg_mcuenta_corriente_Nopaquete DECIMAL(12,2) DEFAULT NULL,
avg_mcuenta_corriente_Paquete DECIMAL(12,2) DEFAULT NULL,
avg_mcuenta_corriente_dolares DECIMAL(12,2) DEFAULT NULL,
avg_mcuenta_debitos_automaticos DECIMAL(12,2) DEFAULT NULL,
avg_mcuenta_descuentos DECIMAL(12,2) DEFAULT NULL,
avg_mcuentas_saldo DECIMAL(12,2) DEFAULT NULL,
avg_mdescubierto_preacordado DECIMAL(12,2) DEFAULT NULL,
avg_mextraccion_autoservicio DECIMAL(12,2) DEFAULT NULL,
avg_mfondos_comunes_inversion_dolares DECIMAL(12,2) DEFAULT NULL,
avg_mfondos_comunes_inversion_pesos DECIMAL(12,2) DEFAULT NULL,
avg_minversiones_otras DECIMAL(12,2) DEFAULT NULL,
avg_mmonedas_extranjeras DECIMAL(12,2) DEFAULT NULL,
avg_mpagodeservicios DECIMAL(12,2) DEFAULT NULL,
avg_mpagomiscuentas DECIMAL(12,2) DEFAULT NULL,
avg_mpasivos_margen DECIMAL(12,2) DEFAULT NULL,
avg_mprestamos_hipotecarios DECIMAL(12,2) DEFAULT NULL,
avg_mprestamos_personales DECIMAL(12,2) DEFAULT NULL,
avg_mprestamos_prendarios DECIMAL(12,2) DEFAULT NULL,
avg_mrentabilidad DECIMAL(12,2) DEFAULT NULL,
avg_mrentabilidad_annual DECIMAL(12,2) DEFAULT NULL,
avg_mtarjeta_master_consumo DECIMAL(12,2) DEFAULT NULL,
avg_mtarjeta_master_descuentos DECIMAL(12,2) DEFAULT NULL,
avg_mtarjeta_visa_consumo DECIMAL(12,2) DEFAULT NULL,
avg_mtarjeta_visa_descuentos DECIMAL(12,2) DEFAULT NULL,
avg_mtitulos DECIMAL(12,2) DEFAULT NULL,
avg_mtransferencias_emitidas DECIMAL(12,2) DEFAULT NULL,
avg_mtransferencias_recibidas DECIMAL(12,2) DEFAULT NULL,
avg_mttarjeta_master_debitos_automaticos DECIMAL(12,2) DEFAULT NULL,
avg_mttarjeta_visa_debitos_automaticos DECIMAL(12,2) DEFAULT NULL,

tendencia_Master_madelantodolares VARCHAR(100) DEFAULT NULL,
tendencia_Master_madelantopesos VARCHAR(100) DEFAULT NULL,
tendencia_Master_marca_atraso INT DEFAULT NULL,
tendencia_Master_mconsumosdolares VARCHAR(100) DEFAULT NULL,
tendencia_Master_mconsumospesos VARCHAR(100) DEFAULT NULL,
tendencia_Master_mconsumototal VARCHAR(100) DEFAULT NULL,
tendencia_Master_mfinanciacion_limite VARCHAR(100) DEFAULT NULL,
tendencia_Master_mlimitecompra VARCHAR(100) DEFAULT NULL,
tendencia_Master_mpagado VARCHAR(100) DEFAULT NULL,
tendencia_Master_mpagominimo VARCHAR(100) DEFAULT NULL,
tendencia_Master_mpagosdolares VARCHAR(100) DEFAULT NULL,
tendencia_Master_mpagospesos VARCHAR(100) DEFAULT NULL,
tendencia_Master_msaldodolares VARCHAR(100) DEFAULT NULL,
tendencia_Master_msaldopesos VARCHAR(100) DEFAULT NULL,
tendencia_Master_msaldototal VARCHAR(100) DEFAULT NULL,
tendencia_Master_tadelantosefectivo VARCHAR(100) DEFAULT NULL,
tendencia_Master_tconsumos VARCHAR(100) DEFAULT NULL,

tendencia_Visa_madelantodolares VARCHAR(100) DEFAULT NULL,
tendencia_Visa_madelantopesos VARCHAR(100) DEFAULT NULL,
tendencia_Visa_marca_atraso INT DEFAULT NULL,
tendencia_Visa_mconsumosdolares VARCHAR(100) DEFAULT NULL,
tendencia_Visa_mconsumospesos VARCHAR(100) DEFAULT NULL,
tendencia_Visa_mconsumototal VARCHAR(100) DEFAULT NULL,
tendencia_Visa_mfinanciacion_limite VARCHAR(100) DEFAULT NULL,
tendencia_Visa_mlimitecompra VARCHAR(100) DEFAULT NULL,
tendencia_Visa_mpagado VARCHAR(100) DEFAULT NULL,
tendencia_Visa_mpagominimo VARCHAR(100) DEFAULT NULL,
tendencia_Visa_mpagosdolares VARCHAR(100) DEFAULT NULL,
tendencia_Visa_mpagospesos VARCHAR(100) DEFAULT NULL,
tendencia_Visa_msaldodolares VARCHAR(100) DEFAULT NULL,
tendencia_Visa_msaldopesos VARCHAR(100) DEFAULT NULL,
tendencia_Visa_msaldototal VARCHAR(100) DEFAULT NULL,
tendencia_Visa_tadelantosefectivo VARCHAR(100) DEFAULT NULL,
tendencia_Visa_tconsumos VARCHAR(100) DEFAULT NULL,

tendencia_mactivos_margen VARCHAR(100) DEFAULT NULL,
tendencia_marketing_coss_selling VARCHAR(100) DEFAULT NULL,
tendencia_mautoservicio VARCHAR(100) DEFAULT NULL,
tendencia_mbonos_corporativos VARCHAR(100) DEFAULT NULL,
tendencia_mcaja_ahorro_Nopaquete VARCHAR(100) DEFAULT NULL,
tendencia_mcaja_ahorro_Paquete VARCHAR(100) DEFAULT NULL,
tendencia_mcaja_ahorro_dolares VARCHAR(100) DEFAULT NULL,
tendencia_mcajeros_ajenos VARCHAR(100) DEFAULT NULL,
tendencia_mcajeros_propio VARCHAR(100) DEFAULT NULL,
tendencia_mcajeros_propios_descuentos VARCHAR(100) DEFAULT NULL,
tendencia_mcambio_monedas_compra VARCHAR(100) DEFAULT NULL,
tendencia_mcambio_monedas_venta VARCHAR(100) DEFAULT NULL,
tendencia_mcheques_depositados VARCHAR(100) DEFAULT NULL,
tendencia_mcheques_depositados_rechazados VARCHAR(100) DEFAULT NULL,
tendencia_mcheques_emitidos VARCHAR(100) DEFAULT NULL,
tendencia_mcheques_emitidos_rechazados VARCHAR(100) DEFAULT NULL,
tendencia_mcomisiones VARCHAR(100) DEFAULT NULL,
tendencia_mcomisiones_mantenimiento VARCHAR(100) DEFAULT NULL,
tendencia_mcomisiones_otras VARCHAR(100) DEFAULT NULL,
tendencia_mcuenta_corriente_Nopaquete VARCHAR(100) DEFAULT NULL,
tendencia_mcuenta_corriente_Paquete VARCHAR(100) DEFAULT NULL,
tendencia_mcuenta_corriente_dolares VARCHAR(100) DEFAULT NULL,
tendencia_mcuenta_debitos_automaticos VARCHAR(100) DEFAULT NULL,
tendencia_mcuenta_descuentos VARCHAR(100) DEFAULT NULL,
tendencia_mcuentas_saldo VARCHAR(100) DEFAULT NULL,
tendencia_mdescubierto_preacordado VARCHAR(100) DEFAULT NULL,
tendencia_mextraccion_autoservicio VARCHAR(100) DEFAULT NULL,
tendencia_mfondos_comunes_inversion_dolares VARCHAR(100) DEFAULT NULL,
tendencia_mfondos_comunes_inversion_pesos VARCHAR(100) DEFAULT NULL,
tendencia_minversiones_otras VARCHAR(100) DEFAULT NULL,
tendencia_mmonedas_extranjeras VARCHAR(100) DEFAULT NULL,
tendencia_mpagodeservicios VARCHAR(100) DEFAULT NULL,
tendencia_mpagomiscuentas VARCHAR(100) DEFAULT NULL,
tendencia_mpasivos_margen VARCHAR(100) DEFAULT NULL,
tendencia_mprestamos_hipotecarios VARCHAR(100) DEFAULT NULL,
tendencia_mprestamos_personales VARCHAR(100) DEFAULT NULL,
tendencia_mprestamos_prendarios VARCHAR(100) DEFAULT NULL,
tendencia_mrentabilidad VARCHAR(100) DEFAULT NULL,
tendencia_mrentabilidad_annual VARCHAR(100) DEFAULT NULL,
tendencia_mtarjeta_master_consumo VARCHAR(100) DEFAULT NULL,
tendencia_mtarjeta_master_descuentos VARCHAR(100) DEFAULT NULL,
tendencia_mtarjeta_visa_consumo VARCHAR(100) DEFAULT NULL,
tendencia_mtarjeta_visa_descuentos VARCHAR(100) DEFAULT NULL,
tendencia_mtitulos VARCHAR(100) DEFAULT NULL,
tendencia_mtransferencias_emitidas VARCHAR(100) DEFAULT NULL,
tendencia_mtransferencias_recibidas VARCHAR(100) DEFAULT NULL,
tendencia_mttarjeta_master_debitos_automaticos VARCHAR(100) DEFAULT NULL,
tendencia_mttarjeta_visa_debitos_automaticos VARCHAR(100) DEFAULT NULL,

rel_Master_Finiciomora INT DEFAULT NULL,
rel_Visa_Finiciomora INT DEFAULT NULL,

rel_Master_fechaalta INT DEFAULT NULL,
rel_Visa_fechaalta INT DEFAULT NULL
);

ALTER TABLE `extra_junio_2` ADD INDEX `numero_de_cliente_idx` (`numero_de_cliente`);
