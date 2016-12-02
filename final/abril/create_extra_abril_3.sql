USE dmef;

DROP TABLE IF EXISTS `extra_abril_3`;

CREATE TABLE `extra_abril_3` (
numero_de_cliente INT,

min_mplazo_fijo_dolares DECIMAL(12,2) DEFAULT NULL,
min_mplazo_fijo_pesos DECIMAL(12,2) DEFAULT NULL,
max_mplazo_fijo_dolares DECIMAL(12,2) DEFAULT NULL,
max_mplazo_fijo_pesos DECIMAL(12,2) DEFAULT NULL,
avg_mplazo_fijo_dolares DECIMAL(12,2) DEFAULT NULL,
avg_mplazo_fijo_pesos DECIMAL(12,2) DEFAULT NULL,
tendencia_mplazo_fijo_dolares VARCHAR(300) DEFAULT NULL,
tendencia_mplazo_fijo_pesos VARCHAR(300) DEFAULT NULL,

min_mplan_sueldo DECIMAL(12,2) DEFAULT NULL,
min_mplan_sueldo_manual DECIMAL(12,2) DEFAULT NULL,
min_cplan_sueldo_transaccion DECIMAL(12,2) DEFAULT NULL,
max_mplan_sueldo DECIMAL(12,2) DEFAULT NULL,
max_mplan_sueldo_manual DECIMAL(12,2) DEFAULT NULL,
max_cplan_sueldo_transaccion DECIMAL(12,2) DEFAULT NULL,
avg_mplan_sueldo DECIMAL(12,2) DEFAULT NULL,
avg_mplan_sueldo_manual DECIMAL(12,2) DEFAULT NULL,
avg_cplan_sueldo_transaccion DECIMAL(12,2) DEFAULT NULL,
tendencia_mplan_sueldo VARCHAR(300) DEFAULT NULL,
tendencia_mplan_sueldo_manual VARCHAR(300) DEFAULT NULL,
tendencia_cplan_sueldo_transaccion VARCHAR(300) DEFAULT NULL,

min_mtarjeta_master_consumo DECIMAL(12,2) DEFAULT NULL,
min_mtarjeta_master_descuentos DECIMAL(12,2) DEFAULT NULL,
max_mtarjeta_master_consumo DECIMAL(12,2) DEFAULT NULL,
max_mtarjeta_master_descuentos DECIMAL(12,2) DEFAULT NULL,
avg_mtarjeta_master_consumo DECIMAL(12,2) DEFAULT NULL,
avg_mtarjeta_master_descuentos DECIMAL(12,2) DEFAULT NULL,
tendencia_mtarjeta_master_consumo VARCHAR(300) DEFAULT NULL,
tendencia_mtarjeta_master_descuentos VARCHAR(300) DEFAULT NULL,

min_mtarjeta_visa_consumo DECIMAL(12,2) DEFAULT NULL,
min_mtarjeta_visa_descuentos DECIMAL(12,2) DEFAULT NULL,
max_mtarjeta_visa_consumo DECIMAL(12,2) DEFAULT NULL,
max_mtarjeta_visa_descuentos DECIMAL(12,2) DEFAULT NULL,
avg_mtarjeta_visa_consumo DECIMAL(12,2) DEFAULT NULL,
avg_mtarjeta_visa_descuentos DECIMAL(12,2) DEFAULT NULL,
tendencia_mtarjeta_visa_consumo VARCHAR(300) DEFAULT NULL,
tendencia_mtarjeta_visa_descuentos VARCHAR(300) DEFAULT NULL,

tendencia_Visa_cuenta_estado VARCHAR(300) DEFAULT NULL,
tendencia_Master_cuenta_estado VARCHAR(300) DEFAULT NULL
);

ALTER TABLE `extra_abril_3` ADD INDEX `numero_de_cliente_idx` (`numero_de_cliente`);
