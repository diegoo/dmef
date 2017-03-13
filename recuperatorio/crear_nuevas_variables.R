data <- read.csv('data/recu_03.tsv', sep='\t')

data$nueva.1 <- paste(as.character(data$Master_cuenta_estado), as.character(data$Visa_cuenta_estado), sep = "_")
data$nueva.2 <- mean(c(data$ctarjeta_master_descuentos,data$ctarjeta_visa_descuentos))
data$nueva.3 <- mean(c(data$ctarjeta_master_transacciones,data$ctarjeta_visa_transacciones))
data$nueva.4 <- max(data$ctarjeta_master_descuentos,data$ctarjeta_visa_descuentos)
data$nueva.5 <- max(data$ctarjeta_master_transacciones,data$ctarjeta_visa_transacciones)
data$nueva.6 <- data$Master_madelantodolares / data$cliente_edad
data$nueva.7 <- data$Master_madelantopesos / data$cliente_edad
data$nueva.8 <- data$Master_marca_atraso / data$cliente_edad
data$nueva.9 <- data$Master_mconsumosdolares / data$cliente_edad
data$nueva.10 <- data$Master_mconsumospesos / data$cliente_edad
data$nueva.11 <- data$Master_mconsumototal / data$cliente_edad
data$nueva.12 <- data$Master_mfinanciacion_limite / data$cliente_edad
data$nueva.13 <- data$Master_mlimitecompra / data$cliente_edad
data$nueva.14 <- data$Master_mpagado / data$cliente_edad
data$nueva.15 <- data$Master_mpagominimo / data$cliente_edad
data$nueva.16 <- data$Master_mpagosdolares / data$cliente_edad
data$nueva.17 <- data$Master_mpagospesos / data$cliente_edad
data$nueva.18 <- data$Master_msaldodolares / data$cliente_edad
data$nueva.19 <- data$Master_msaldopesos / data$cliente_edad
data$nueva.20 <- data$Master_msaldototal / data$cliente_edad
data$nueva.21 <- data$Master_madelantodolares / data$cliente_antiguedad
data$nueva.22 <- data$Master_madelantopesos / data$cliente_antiguedad
data$nueva.23 <- data$Master_marca_atraso / data$cliente_antiguedad
data$nueva.24 <- data$Master_mconsumosdolares / data$cliente_antiguedad
data$nueva.25 <- data$Master_mconsumospesos / data$cliente_antiguedad
data$nueva.26 <- data$Master_mconsumototal / data$cliente_antiguedad
data$nueva.27 <- data$Master_mfinanciacion_limite / data$cliente_antiguedad
data$nueva.28 <- data$Master_mlimitecompra / data$cliente_antiguedad
data$nueva.29 <- data$Master_mpagado / data$cliente_antiguedad
data$nueva.30 <- data$Master_mpagominimo / data$cliente_antiguedad
data$nueva.31 <- data$Master_mpagosdolares / data$cliente_antiguedad
data$nueva.32 <- data$Master_mpagospesos / data$cliente_antiguedad
data$nueva.33 <- data$Master_msaldodolares / data$cliente_antiguedad
data$nueva.34 <- data$Master_msaldopesos / data$cliente_antiguedad
data$nueva.35 <- data$Master_msaldototal / data$cliente_antiguedad
data$nueva.36 <- data$Visa_madelantodolares / data$cliente_edad
data$nueva.37 <- data$Visa_madelantopesos / data$cliente_edad
data$nueva.38 <- data$Visa_marca_atraso / data$cliente_edad
data$nueva.39 <- data$Visa_mconsumosdolares / data$cliente_edad
data$nueva.40 <- data$Visa_mconsumospesos / data$cliente_edad
data$nueva.41 <- data$Visa_mconsumototal / data$cliente_edad
data$nueva.42 <- data$Visa_mfinanciacion_limite / data$cliente_edad
data$nueva.43 <- data$Visa_mlimitecompra / data$cliente_edad
data$nueva.44 <- data$Visa_mpagado / data$cliente_edad
data$nueva.45 <- data$Visa_mpagominimo / data$cliente_edad
data$nueva.46 <- data$Visa_mpagosdolares / data$cliente_edad
data$nueva.47 <- data$Visa_mpagospesos / data$cliente_edad
data$nueva.48 <- data$Visa_msaldodolares / data$cliente_edad
data$nueva.49 <- data$Visa_msaldopesos / data$cliente_edad
data$nueva.50 <- data$Visa_msaldototal / data$cliente_edad
data$nueva.51 <- data$Visa_madelantodolares / data$cliente_antiguedad
data$nueva.52 <- data$Visa_madelantopesos / data$cliente_antiguedad
data$nueva.53 <- data$Visa_marca_atraso / data$cliente_antiguedad
data$nueva.54 <- data$Visa_mconsumosdolares / data$cliente_antiguedad
data$nueva.55 <- data$Visa_mconsumospesos / data$cliente_antiguedad
data$nueva.56 <- data$Visa_mconsumototal / data$cliente_antiguedad
data$nueva.57 <- data$Visa_mfinanciacion_limite / data$cliente_antiguedad
data$nueva.58 <- data$Visa_mlimitecompra / data$cliente_antiguedad
data$nueva.59 <- data$Visa_mpagado / data$cliente_antiguedad
data$nueva.60 <- data$Visa_mpagominimo / data$cliente_antiguedad
data$nueva.61 <- data$Visa_mpagosdolares / data$cliente_antiguedad
data$nueva.62 <- data$Visa_mpagospesos / data$cliente_antiguedad
data$nueva.63 <- data$Visa_msaldodolares / data$cliente_antiguedad
data$nueva.64 <- data$Visa_msaldopesos / data$cliente_antiguedad
data$nueva.65 <- data$Visa_msaldototal / data$cliente_antiguedad
## data$nueva.66 <- max(data$Master_madelantodolares,data$Visa_madelantodolares)
## data$nueva.67 <- max(data$Master_madelantopesos,data$Visa_madelantopesos)
## data$nueva.68 <- max(data$Master_marca_atraso,data$Visa_marca_atraso)
## data$nueva.69 <- max(data$Master_mconsumosdolares,data$Visa_mconsumosdolares)
## data$nueva.70 <- max(data$Master_mconsumospesos,data$Visa_mconsumospesos)
## data$nueva.71 <- max(data$Master_mconsumototal,data$Visa_mconsumototal)
## data$nueva.72 <- max(data$Master_mfinanciacion_limite,data$Visa_mfinanciacion_limite)
## data$nueva.73 <- max(data$Master_mlimitecompra,data$Visa_mlimitecompra)
## data$nueva.74 <- max(data$Master_mpagado,data$Visa_mpagado)
## data$nueva.75 <- max(data$Master_mpagominimo,data$Visa_mpagominimo)
## data$nueva.76 <- max(data$Master_mpagosdolares,data$Visa_mpagosdolares)
## data$nueva.77 <- max(data$Master_mpagospesos,data$Visa_mpagospesos)
## data$nueva.78 <- max(data$Master_msaldodolares,data$Visa_msaldodolares)
## data$nueva.79 <- max(data$Master_msaldopesos,data$Visa_msaldopesos)
## data$nueva.80 <- max(data$Master_msaldototal,data$Visa_msaldototal)
## data$nueva.81 <- mean(c(data$Master_madelantodolares,data$Visa_madelantodolares))
## data$nueva.82 <- mean(c(data$Master_madelantopesos,data$Visa_madelantopesos))
## data$nueva.83 <- mean(c(data$Master_marca_atraso,data$Visa_marca_atraso))
## data$nueva.84 <- mean(c(data$Master_mconsumosdolares,data$Visa_mconsumosdolares))
## data$nueva.85 <- mean(c(data$Master_mconsumospesos,data$Visa_mconsumospesos))
## data$nueva.86 <- mean(c(data$Master_mconsumototal,data$Visa_mconsumototal))
## data$nueva.87 <- mean(c(data$Master_mfinanciacion_limite,data$Visa_mfinanciacion_limite))
## data$nueva.88 <- mean(c(data$Master_mlimitecompra,data$Visa_mlimitecompra))
## data$nueva.89 <- mean(c(data$Master_mpagado,data$Visa_mpagado))
## data$nueva.90 <- mean(c(data$Master_mpagominimo,data$Visa_mpagominimo))
## data$nueva.91 <- mean(c(data$Master_mpagosdolares,data$Visa_mpagosdolares))
## data$nueva.92 <- mean(c(data$Master_mpagospesos,data$Visa_mpagospesos))
## data$nueva.93 <- mean(c(data$Master_msaldodolares,data$Visa_msaldodolares))
## data$nueva.94 <- mean(c(data$Master_msaldopesos,data$Visa_msaldopesos))
## data$nueva.95 <- mean(c(data$Master_msaldototal,data$Visa_msaldototal))
data$nueva.96 <- data$mactivos_margen / data$cliente_edad
data$nueva.97 <- data$marketing_coss_selling / data$cliente_edad
data$nueva.98 <- data$mautoservicio / data$cliente_edad
data$nueva.99 <- data$mbonos_corporativos / data$cliente_edad
data$nueva.100 <- data$mbonos_gobierno / data$cliente_edad
data$nueva.101 <- data$mcaja_ahorro_Nopaquete / data$cliente_edad
data$nueva.102 <- data$mcaja_ahorro_Paquete / data$cliente_edad
data$nueva.103 <- data$mcaja_ahorro_dolares / data$cliente_edad
data$nueva.104 <- data$mcajeros_ajenos / data$cliente_edad
data$nueva.105 <- data$mcajeros_propio / data$cliente_edad
data$nueva.106 <- data$mcajeros_propios_descuentos / data$cliente_edad
data$nueva.107 <- data$mcambio_monedas_compra / data$cliente_edad
data$nueva.108 <- data$mcambio_monedas_venta / data$cliente_edad
data$nueva.109 <- data$mcheques_depositados / data$cliente_edad
data$nueva.110 <- data$mcheques_depositados_rechazados / data$cliente_edad
data$nueva.111 <- data$mcheques_emitidos / data$cliente_edad
data$nueva.112 <- data$mcheques_emitidos_rechazados / data$cliente_edad
data$nueva.113 <- data$mcomisiones / data$cliente_edad
data$nueva.114 <- data$mcomisiones_mantenimiento / data$cliente_edad
data$nueva.115 <- data$mcomisiones_otras / data$cliente_edad
data$nueva.116 <- data$mcuenta_corriente_Nopaquete / data$cliente_edad
data$nueva.117 <- data$mcuenta_corriente_Paquete / data$cliente_edad
data$nueva.118 <- data$mcuenta_corriente_dolares / data$cliente_edad
data$nueva.119 <- data$mcuenta_debitos_automaticos / data$cliente_edad
data$nueva.120 <- data$mcuenta_descuentos / data$cliente_edad
data$nueva.121 <- data$mcuentas_saldo / data$cliente_edad
data$nueva.122 <- data$mdescubierto_preacordado / data$cliente_edad
data$nueva.123 <- data$mextraccion_autoservicio / data$cliente_edad
data$nueva.124 <- data$mfondos_comunes_inversion_dolares / data$cliente_edad
data$nueva.125 <- data$mfondos_comunes_inversion_pesos / data$cliente_edad
data$nueva.126 <- data$minversiones_otras / data$cliente_edad
data$nueva.127 <- data$mmonedas_extranjeras / data$cliente_edad
data$nueva.128 <- data$mpagodeservicios / data$cliente_edad
data$nueva.129 <- data$mpagomiscuentas / data$cliente_edad
data$nueva.130 <- data$mpasivos_margen / data$cliente_edad
data$nueva.131 <- data$mplan_sueldo / data$cliente_edad
data$nueva.132 <- data$mplan_sueldo_manual / data$cliente_edad
data$nueva.133 <- data$mplazo_fijo_dolares / data$cliente_edad
data$nueva.134 <- data$mplazo_fijo_pesos / data$cliente_edad
data$nueva.135 <- data$mprestamos_hipotecarios / data$cliente_edad
data$nueva.136 <- data$mprestamos_personales / data$cliente_edad
data$nueva.137 <- data$mprestamos_prendarios / data$cliente_edad
data$nueva.138 <- data$mrentabilidad / data$cliente_edad
data$nueva.139 <- data$mrentabilidad_annual / data$cliente_edad
data$nueva.140 <- data$mtarjeta_master_consumo / data$cliente_edad
data$nueva.141 <- data$mtarjeta_master_descuentos / data$cliente_edad
data$nueva.142 <- data$mtarjeta_visa_consumo / data$cliente_edad
data$nueva.143 <- data$mtarjeta_visa_descuentos / data$cliente_edad
data$nueva.144 <- data$mtitulos / data$cliente_edad
data$nueva.145 <- data$mtransferencias_emitidas / data$cliente_edad
data$nueva.146 <- data$mtransferencias_recibidas / data$cliente_edad
data$nueva.147 <- data$mttarjeta_master_debitos_automaticos / data$cliente_edad
data$nueva.148 <- data$mttarjeta_visa_debitos_automaticos / data$cliente_edad
data$nueva.149 <- data$mactivos_margen / data$cliente_antiguedad
data$nueva.150 <- data$marketing_coss_selling / data$cliente_antiguedad
data$nueva.151 <- data$mautoservicio / data$cliente_antiguedad
data$nueva.152 <- data$mbonos_corporativos / data$cliente_antiguedad
data$nueva.153 <- data$mbonos_gobierno / data$cliente_antiguedad
data$nueva.154 <- data$mcaja_ahorro_Nopaquete / data$cliente_antiguedad
data$nueva.155 <- data$mcaja_ahorro_Paquete / data$cliente_antiguedad
data$nueva.156 <- data$mcaja_ahorro_dolares / data$cliente_antiguedad
data$nueva.157 <- data$mcajeros_ajenos / data$cliente_antiguedad
data$nueva.158 <- data$mcajeros_propio / data$cliente_antiguedad
data$nueva.159 <- data$mcajeros_propios_descuentos / data$cliente_antiguedad
data$nueva.160 <- data$mcambio_monedas_compra / data$cliente_antiguedad
data$nueva.161 <- data$mcambio_monedas_venta / data$cliente_antiguedad
data$nueva.162 <- data$mcheques_depositados / data$cliente_antiguedad
data$nueva.163 <- data$mcheques_depositados_rechazados / data$cliente_antiguedad
data$nueva.164 <- data$mcheques_emitidos / data$cliente_antiguedad
data$nueva.165 <- data$mcheques_emitidos_rechazados / data$cliente_antiguedad
data$nueva.166 <- data$mcomisiones / data$cliente_antiguedad
data$nueva.167 <- data$mcomisiones_mantenimiento / data$cliente_antiguedad
data$nueva.168 <- data$mcomisiones_otras / data$cliente_antiguedad
data$nueva.169 <- data$mcuenta_corriente_Nopaquete / data$cliente_antiguedad
data$nueva.170 <- data$mcuenta_corriente_Paquete / data$cliente_antiguedad
data$nueva.171 <- data$mcuenta_corriente_dolares / data$cliente_antiguedad
data$nueva.172 <- data$mcuenta_debitos_automaticos / data$cliente_antiguedad
data$nueva.173 <- data$mcuenta_descuentos / data$cliente_antiguedad
data$nueva.174 <- data$mcuentas_saldo / data$cliente_antiguedad
data$nueva.175 <- data$mdescubierto_preacordado / data$cliente_antiguedad
data$nueva.176 <- data$mextraccion_autoservicio / data$cliente_antiguedad
data$nueva.177 <- data$mfondos_comunes_inversion_dolares / data$cliente_antiguedad
data$nueva.178 <- data$mfondos_comunes_inversion_pesos / data$cliente_antiguedad
data$nueva.179 <- data$minversiones_otras / data$cliente_antiguedad
data$nueva.180 <- data$mmonedas_extranjeras / data$cliente_antiguedad
data$nueva.181 <- data$mpagodeservicios / data$cliente_antiguedad
data$nueva.182 <- data$mpagomiscuentas / data$cliente_antiguedad
data$nueva.183 <- data$mpasivos_margen / data$cliente_antiguedad
data$nueva.184 <- data$mplan_sueldo / data$cliente_antiguedad
data$nueva.185 <- data$mplan_sueldo_manual / data$cliente_antiguedad
data$nueva.186 <- data$mplazo_fijo_dolares / data$cliente_antiguedad
data$nueva.187 <- data$mplazo_fijo_pesos / data$cliente_antiguedad
data$nueva.188 <- data$mprestamos_hipotecarios / data$cliente_antiguedad
data$nueva.189 <- data$mprestamos_personales / data$cliente_antiguedad
data$nueva.190 <- data$mprestamos_prendarios / data$cliente_antiguedad
data$nueva.191 <- data$mrentabilidad / data$cliente_antiguedad
data$nueva.192 <- data$mrentabilidad_annual / data$cliente_antiguedad
data$nueva.193 <- data$mtarjeta_master_consumo / data$cliente_antiguedad
data$nueva.194 <- data$mtarjeta_master_descuentos / data$cliente_antiguedad
data$nueva.195 <- data$mtarjeta_visa_consumo / data$cliente_antiguedad
data$nueva.196 <- data$mtarjeta_visa_descuentos / data$cliente_antiguedad
data$nueva.197 <- data$mtitulos / data$cliente_antiguedad
data$nueva.198 <- data$mtransferencias_emitidas / data$cliente_antiguedad
data$nueva.199 <- data$mtransferencias_recibidas / data$cliente_antiguedad
data$nueva.200 <- data$mttarjeta_master_debitos_automaticos / data$cliente_antiguedad
data$nueva.201 <- data$mttarjeta_visa_debitos_automaticos / data$cliente_antiguedad
data$nueva.202 <- data$Visa_madelantodolares / data$Visa_mlimitecompra
data$nueva.203 <- data$Visa_madelantopesos / data$Visa_mlimitecompra
data$nueva.204 <- data$Visa_marca_atraso / data$Visa_mlimitecompra
data$nueva.205 <- data$Visa_mconsumosdolares / data$Visa_mlimitecompra
data$nueva.206 <- data$Visa_mconsumospesos / data$Visa_mlimitecompra
data$nueva.207 <- data$Visa_mconsumototal / data$Visa_mlimitecompra
data$nueva.208 <- data$Visa_mfinanciacion_limite / data$Visa_mlimitecompra
data$nueva.209 <- data$Visa_mpagado / data$Visa_mlimitecompra
data$nueva.210 <- data$Visa_mpagominimo / data$Visa_mlimitecompra
data$nueva.211 <- data$Visa_mpagosdolares / data$Visa_mlimitecompra
data$nueva.212 <- data$Visa_mpagospesos / data$Visa_mlimitecompra
data$nueva.213 <- data$Visa_msaldodolares / data$Visa_mlimitecompra
data$nueva.214 <- data$Visa_msaldopesos / data$Visa_mlimitecompra
data$nueva.215 <- data$Visa_msaldototal / data$Visa_mlimitecompra
data$nueva.216 <- data$Visa_madelantodolares / data$Visa_mlimitecompra
data$nueva.217 <- data$Visa_madelantopesos / data$Visa_mlimitecompra
data$nueva.218 <- data$Visa_marca_atraso / data$Visa_mlimitecompra
data$nueva.219 <- data$Visa_mconsumosdolares / data$Visa_mlimitecompra
data$nueva.220 <- data$Visa_mconsumospesos / data$Visa_mlimitecompra
data$nueva.221 <- data$Visa_mconsumototal / data$Visa_mlimitecompra
data$nueva.222 <- data$Visa_mfinanciacion_limite / data$Visa_mlimitecompra
data$nueva.223 <- data$Visa_mpagado / data$Visa_mlimitecompra
data$nueva.224 <- data$Visa_mpagominimo / data$Visa_mlimitecompra
data$nueva.225 <- data$Visa_mpagosdolares / data$Visa_mlimitecompra
data$nueva.226 <- data$Visa_mpagospesos / data$Visa_mlimitecompra
data$nueva.227 <- data$Visa_msaldodolares / data$Visa_mlimitecompra
data$nueva.228 <- data$Visa_msaldopesos / data$Visa_mlimitecompra
data$nueva.229 <- data$Visa_msaldototal / data$Visa_mlimitecompra
data$nueva.230 <- data$Visa_madelantodolares / data$Visa_mfinanciacion_limite
data$nueva.231 <- data$Visa_madelantopesos / data$Visa_mfinanciacion_limite
data$nueva.232 <- data$Visa_marca_atraso / data$Visa_mfinanciacion_limite
data$nueva.233 <- data$Visa_mconsumosdolares / data$Visa_mfinanciacion_limite
data$nueva.234 <- data$Visa_mconsumospesos / data$Visa_mfinanciacion_limite
data$nueva.235 <- data$Visa_mconsumototal / data$Visa_mfinanciacion_limite
data$nueva.236 <- data$Visa_mpagado / data$Visa_mfinanciacion_limite
data$nueva.237 <- data$Visa_mpagominimo / data$Visa_mfinanciacion_limite
data$nueva.238 <- data$Visa_mpagosdolares / data$Visa_mfinanciacion_limite
data$nueva.239 <- data$Visa_mpagospesos / data$Visa_mfinanciacion_limite
data$nueva.240 <- data$Visa_msaldodolares / data$Visa_mfinanciacion_limite
data$nueva.241 <- data$Visa_msaldopesos / data$Visa_mfinanciacion_limite
data$nueva.242 <- data$Visa_msaldototal / data$Visa_mfinanciacion_limite
data$nueva.243 <- data$Visa_madelantodolares / data$Visa_mfinanciacion_limite
data$nueva.244 <- data$Visa_madelantopesos / data$Visa_mfinanciacion_limite
data$nueva.245 <- data$Visa_marca_atraso / data$Visa_mfinanciacion_limite
data$nueva.246 <- data$Visa_mconsumosdolares / data$Visa_mfinanciacion_limite
data$nueva.247 <- data$Visa_mconsumospesos / data$Visa_mfinanciacion_limite
data$nueva.248 <- data$Visa_mconsumototal / data$Visa_mfinanciacion_limite
data$nueva.249 <- data$Visa_mpagado / data$Visa_mfinanciacion_limite
data$nueva.250 <- data$Visa_mpagominimo / data$Visa_mfinanciacion_limite
data$nueva.251 <- data$Visa_mpagosdolares / data$Visa_mfinanciacion_limite
data$nueva.252 <- data$Visa_mpagospesos / data$Visa_mfinanciacion_limite
data$nueva.253 <- data$Visa_msaldodolares / data$Visa_mfinanciacion_limite
data$nueva.254 <- data$Visa_msaldopesos / data$Visa_mfinanciacion_limite
data$nueva.255 <- data$Visa_msaldototal / data$Visa_mfinanciacion_limite
data$nueva.256 <- data$Master_madelantodolares / data$Master_mlimitecompra
data$nueva.257 <- data$Master_madelantopesos / data$Master_mlimitecompra
data$nueva.258 <- data$Master_marca_atraso / data$Master_mlimitecompra
data$nueva.259 <- data$Master_mconsumosdolares / data$Master_mlimitecompra
data$nueva.260 <- data$Master_mconsumospesos / data$Master_mlimitecompra
data$nueva.261 <- data$Master_mconsumototal / data$Master_mlimitecompra
data$nueva.262 <- data$Master_mfinanciacion_limite / data$Master_mlimitecompra
data$nueva.263 <- data$Master_mpagado / data$Master_mlimitecompra
data$nueva.264 <- data$Master_mpagominimo / data$Master_mlimitecompra
data$nueva.265 <- data$Master_mpagosdolares / data$Master_mlimitecompra
data$nueva.266 <- data$Master_mpagospesos / data$Master_mlimitecompra
data$nueva.267 <- data$Master_msaldodolares / data$Master_mlimitecompra
data$nueva.268 <- data$Master_msaldopesos / data$Master_mlimitecompra
data$nueva.269 <- data$Master_msaldototal / data$Master_mlimitecompra
data$nueva.270 <- data$Master_madelantodolares / data$Master_mlimitecompra
data$nueva.271 <- data$Master_madelantopesos / data$Master_mlimitecompra
data$nueva.272 <- data$Master_marca_atraso / data$Master_mlimitecompra
data$nueva.273 <- data$Master_mconsumosdolares / data$Master_mlimitecompra
data$nueva.274 <- data$Master_mconsumospesos / data$Master_mlimitecompra
data$nueva.275 <- data$Master_mconsumototal / data$Master_mlimitecompra
data$nueva.276 <- data$Master_mfinanciacion_limite / data$Master_mlimitecompra
data$nueva.277 <- data$Master_mpagado / data$Master_mlimitecompra
data$nueva.278 <- data$Master_mpagominimo / data$Master_mlimitecompra
data$nueva.279 <- data$Master_mpagosdolares / data$Master_mlimitecompra
data$nueva.280 <- data$Master_mpagospesos / data$Master_mlimitecompra
data$nueva.281 <- data$Master_msaldodolares / data$Master_mlimitecompra
data$nueva.282 <- data$Master_msaldopesos / data$Master_mlimitecompra
data$nueva.283 <- data$Master_msaldototal / data$Master_mlimitecompra
data$nueva.284 <- data$Master_madelantodolares / data$Master_mfinanciacion_limite
data$nueva.285 <- data$Master_madelantopesos / data$Master_mfinanciacion_limite
data$nueva.286 <- data$Master_marca_atraso / data$Master_mfinanciacion_limite
data$nueva.287 <- data$Master_mconsumosdolares / data$Master_mfinanciacion_limite
data$nueva.288 <- data$Master_mconsumospesos / data$Master_mfinanciacion_limite
data$nueva.289 <- data$Master_mconsumototal / data$Master_mfinanciacion_limite
data$nueva.290 <- data$Master_mfinanciacion_limite / data$Master_mfinanciacion_limite
data$nueva.291 <- data$Master_mfinanciacion_limite / data$Master_mfinanciacion_limite
data$nueva.292 <- data$Master_mpagado / data$Master_mfinanciacion_limite
data$nueva.293 <- data$Master_mpagominimo / data$Master_mfinanciacion_limite
data$nueva.294 <- data$Master_mpagosdolares / data$Master_mfinanciacion_limite
data$nueva.295 <- data$Master_mpagospesos / data$Master_mfinanciacion_limite
data$nueva.296 <- data$Master_msaldodolares / data$Master_mfinanciacion_limite
data$nueva.297 <- data$Master_msaldopesos / data$Master_mfinanciacion_limite
data$nueva.298 <- data$Master_msaldototal / data$Master_mfinanciacion_limite
data$nueva.299 <- data$Master_madelantodolares / data$Master_mfinanciacion_limite
data$nueva.300 <- data$Master_madelantopesos / data$Master_mfinanciacion_limite
data$nueva.301 <- data$Master_marca_atraso / data$Master_mfinanciacion_limite
data$nueva.302 <- data$Master_mconsumosdolares / data$Master_mfinanciacion_limite
data$nueva.303 <- data$Master_mconsumospesos / data$Master_mfinanciacion_limite
data$nueva.304 <- data$Master_mconsumototal / data$Master_mfinanciacion_limite
data$nueva.305 <- data$Master_mpagado / data$Master_mfinanciacion_limite
data$nueva.306 <- data$Master_mpagominimo / data$Master_mfinanciacion_limite
data$nueva.307 <- data$Master_mpagosdolares / data$Master_mfinanciacion_limite
data$nueva.308 <- data$Master_mpagospesos / data$Master_mfinanciacion_limite
data$nueva.309 <- data$Master_msaldodolares / data$Master_mfinanciacion_limite
data$nueva.310 <- data$Master_msaldopesos / data$Master_mfinanciacion_limite
data$nueva.311 <- data$Master_msaldototal / data$Master_mfinanciacion_limite

write.table(data, file="data.con.nuevas.variables.tsv", row.names=FALSE, quote=FALSE, sep="\t")
