TRUNCATE extra_junio_3;

INSERT INTO extra_junio_3
(numero_de_cliente,

min_mplazo_fijo_dolares,
min_mplazo_fijo_pesos,
max_mplazo_fijo_dolares,
max_mplazo_fijo_pesos,
avg_mplazo_fijo_dolares,
avg_mplazo_fijo_pesos,
tendencia_mplazo_fijo_dolares,
tendencia_mplazo_fijo_pesos,

min_mplan_sueldo,
min_mplan_sueldo_manual,
min_cplan_sueldo_transaccion,
max_mplan_sueldo,
max_mplan_sueldo_manual,
max_cplan_sueldo_transaccion,
avg_mplan_sueldo,
avg_mplan_sueldo_manual,
avg_cplan_sueldo_transaccion,
tendencia_mplan_sueldo,
tendencia_mplan_sueldo_manual,
tendencia_cplan_sueldo_transaccion,

min_mtarjeta_master_consumo,
min_mtarjeta_master_descuentos,
max_mtarjeta_master_consumo,
max_mtarjeta_master_descuentos,
avg_mtarjeta_master_consumo,
avg_mtarjeta_master_descuentos,
tendencia_mtarjeta_master_consumo,
tendencia_mtarjeta_master_descuentos,

min_mtarjeta_visa_consumo,
min_mtarjeta_visa_descuentos,
max_mtarjeta_visa_consumo,
max_mtarjeta_visa_descuentos,
avg_mtarjeta_visa_consumo,
avg_mtarjeta_visa_descuentos,
tendencia_mtarjeta_visa_consumo,
tendencia_mtarjeta_visa_descuentos,

tendencia_Visa_cuenta_estado,
tendencia_Master_cuenta_estado
)

SELECT

numero_de_cliente,

IF(RIGHT(GROUP_CONCAT(tplazo_fijo),1) = '0', NULL, MIN(mplazo_fijo_dolares)), -- si termina en 0 la serie => NULL; si no, es porque tiene plazo fijo _ahora_
IF(RIGHT(GROUP_CONCAT(tplazo_fijo),1) = '0', NULL, MIN(mplazo_fijo_pesos)),
IF(RIGHT(GROUP_CONCAT(tplazo_fijo),1) = '0', NULL, MAX(mplazo_fijo_dolares)),
IF(RIGHT(GROUP_CONCAT(tplazo_fijo),1) = '0', NULL, MAX(mplazo_fijo_pesos)),
IF(RIGHT(GROUP_CONCAT(tplazo_fijo),1) = '0', NULL, AVG(mplazo_fijo_dolares)),
IF(RIGHT(GROUP_CONCAT(tplazo_fijo),1) = '0', NULL, AVG(mplazo_fijo_pesos)),
IF(SUM(tplazo_fijo) = 0, NULL, GROUP_CONCAT(mplazo_fijo_dolares)),
IF(SUM(tplazo_fijo) = 0, NULL, GROUP_CONCAT(mplazo_fijo_pesos)),

IF(RIGHT(GROUP_CONCAT(tplan_sueldo),1) = '0', NULL, MIN(mplan_sueldo)), -- si termina en 0 la serie => NULL; si no, es porque tiene plazo fijo _ahora_
IF(RIGHT(GROUP_CONCAT(tplan_sueldo),1) = '0', NULL, MIN(mplan_sueldo_manual)),
IF(RIGHT(GROUP_CONCAT(tplan_sueldo),1) = '0', NULL, MIN(cplan_sueldo_transaccion)),
IF(RIGHT(GROUP_CONCAT(tplan_sueldo),1) = '0', NULL, MAX(mplan_sueldo)),
IF(RIGHT(GROUP_CONCAT(tplan_sueldo),1) = '0', NULL, MAX(mplan_sueldo_manual)),
IF(RIGHT(GROUP_CONCAT(tplan_sueldo),1) = '0', NULL, MAX(cplan_sueldo_transaccion)),
IF(RIGHT(GROUP_CONCAT(tplan_sueldo),1) = '0', NULL, AVG(mplan_sueldo)),
IF(RIGHT(GROUP_CONCAT(tplan_sueldo),1) = '0', NULL, AVG(mplan_sueldo_manual)),
IF(RIGHT(GROUP_CONCAT(tplan_sueldo),1) = '0', NULL, AVG(cplan_sueldo_transaccion)),
IF(SUM(tplan_sueldo) = 0, NULL, GROUP_CONCAT(mplan_sueldo)),
IF(SUM(tplan_sueldo) = 0, NULL, GROUP_CONCAT(mplan_sueldo_manual)),
IF(SUM(tplan_sueldo) = 0, NULL, GROUP_CONCAT(cplan_sueldo_transaccion)),

IF(RIGHT(GROUP_CONCAT(ttarjeta_master),1) = '0', NULL, MIN(mtarjeta_master_consumo)),
IF(RIGHT(GROUP_CONCAT(ttarjeta_master),1) = '0', NULL, MIN(mtarjeta_master_descuentos)),
IF(RIGHT(GROUP_CONCAT(ttarjeta_master),1) = '0', NULL, MAX(mtarjeta_master_consumo)),
IF(RIGHT(GROUP_CONCAT(ttarjeta_master),1) = '0', NULL, MAX(mtarjeta_master_descuentos)),
IF(RIGHT(GROUP_CONCAT(ttarjeta_master),1) = '0', NULL, AVG(mtarjeta_master_consumo)),
IF(RIGHT(GROUP_CONCAT(ttarjeta_master),1) = '0', NULL, AVG(mtarjeta_master_descuentos)),
IF(SUM(ttarjeta_master) = 0, NULL, GROUP_CONCAT(mtarjeta_master_consumo)),
IF(SUM(ttarjeta_master) = 0, NULL, GROUP_CONCAT(mtarjeta_master_descuentos)),

IF(RIGHT(GROUP_CONCAT(ttarjeta_visa),1) = '0', NULL, MIN(mtarjeta_visa_consumo)),
IF(RIGHT(GROUP_CONCAT(ttarjeta_visa),1) = '0', NULL, MIN(mtarjeta_visa_descuentos)),
IF(RIGHT(GROUP_CONCAT(ttarjeta_visa),1) = '0', NULL, MAX(mtarjeta_visa_consumo)),
IF(RIGHT(GROUP_CONCAT(ttarjeta_visa),1) = '0', NULL, MAX(mtarjeta_visa_descuentos)),
IF(RIGHT(GROUP_CONCAT(ttarjeta_visa),1) = '0', NULL, AVG(mtarjeta_visa_consumo)),
IF(RIGHT(GROUP_CONCAT(ttarjeta_visa),1) = '0', NULL, AVG(mtarjeta_visa_descuentos)),
IF(SUM(ttarjeta_visa) = 0, NULL, GROUP_CONCAT(mtarjeta_visa_consumo)),
IF(SUM(ttarjeta_visa) = 0, NULL, GROUP_CONCAT(mtarjeta_visa_descuentos)),

GROUP_CONCAT(Visa_cuenta_estado),
GROUP_CONCAT(Master_cuenta_estado)

FROM hasta_junio
WHERE numero_de_cliente IN (SELECT numero_de_cliente FROM hasta_junio WHERE foto_mes = 201606)
GROUP BY numero_de_cliente
ORDER BY numero_de_cliente ASC;
