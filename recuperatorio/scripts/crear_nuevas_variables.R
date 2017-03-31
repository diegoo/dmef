dataset <- read.csv('data/recu_03.tsv', sep='\t')

dataset$nueva.1 <- paste(as.character(dataset$Master_cuenta_estado), as.character(dataset$Visa_cuenta_estado), sep = "_")
dataset$nueva.2 <- mean(c(dataset$ctarjeta_master_descuentos,dataset$ctarjeta_visa_descuentos))
dataset$nueva.3 <- mean(c(dataset$ctarjeta_master_transacciones,dataset$ctarjeta_visa_transacciones))
dataset$nueva.4 <- max(dataset$ctarjeta_master_descuentos,dataset$ctarjeta_visa_descuentos)
dataset$nueva.5 <- max(dataset$ctarjeta_master_transacciones,dataset$ctarjeta_visa_transacciones)
dataset$nueva.6 <- dataset$Master_madelantodolares / dataset$cliente_edad
dataset$nueva.7 <- dataset$Master_madelantopesos / dataset$cliente_edad
dataset$nueva.8 <- dataset$Master_marca_atraso / dataset$cliente_edad
dataset$nueva.9 <- dataset$Master_mconsumosdolares / dataset$cliente_edad
dataset$nueva.10 <- dataset$Master_mconsumospesos / dataset$cliente_edad
dataset$nueva.11 <- dataset$Master_mconsumototal / dataset$cliente_edad
dataset$nueva.12 <- dataset$Master_mfinanciacion_limite / dataset$cliente_edad
dataset$nueva.13 <- dataset$Master_mlimitecompra / dataset$cliente_edad
dataset$nueva.14 <- dataset$Master_mpagado / dataset$cliente_edad
dataset$nueva.15 <- dataset$Master_mpagominimo / dataset$cliente_edad
dataset$nueva.16 <- dataset$Master_mpagosdolares / dataset$cliente_edad
dataset$nueva.17 <- dataset$Master_mpagospesos / dataset$cliente_edad
dataset$nueva.18 <- dataset$Master_msaldodolares / dataset$cliente_edad
dataset$nueva.19 <- dataset$Master_msaldopesos / dataset$cliente_edad
dataset$nueva.20 <- dataset$Master_msaldototal / dataset$cliente_edad
dataset$nueva.21 <- dataset$Master_madelantodolares / dataset$cliente_antiguedad
dataset$nueva.22 <- dataset$Master_madelantopesos / dataset$cliente_antiguedad
dataset$nueva.23 <- dataset$Master_marca_atraso / dataset$cliente_antiguedad
dataset$nueva.24 <- dataset$Master_mconsumosdolares / dataset$cliente_antiguedad
dataset$nueva.25 <- dataset$Master_mconsumospesos / dataset$cliente_antiguedad
dataset$nueva.26 <- dataset$Master_mconsumototal / dataset$cliente_antiguedad
dataset$nueva.27 <- dataset$Master_mfinanciacion_limite / dataset$cliente_antiguedad
dataset$nueva.28 <- dataset$Master_mlimitecompra / dataset$cliente_antiguedad
dataset$nueva.29 <- dataset$Master_mpagado / dataset$cliente_antiguedad
dataset$nueva.30 <- dataset$Master_mpagominimo / dataset$cliente_antiguedad
dataset$nueva.31 <- dataset$Master_mpagosdolares / dataset$cliente_antiguedad
dataset$nueva.32 <- dataset$Master_mpagospesos / dataset$cliente_antiguedad
dataset$nueva.33 <- dataset$Master_msaldodolares / dataset$cliente_antiguedad
dataset$nueva.34 <- dataset$Master_msaldopesos / dataset$cliente_antiguedad
dataset$nueva.35 <- dataset$Master_msaldototal / dataset$cliente_antiguedad
dataset$nueva.36 <- dataset$Visa_madelantodolares / dataset$cliente_edad
dataset$nueva.37 <- dataset$Visa_madelantopesos / dataset$cliente_edad
dataset$nueva.38 <- dataset$Visa_marca_atraso / dataset$cliente_edad
dataset$nueva.39 <- dataset$Visa_mconsumosdolares / dataset$cliente_edad
dataset$nueva.40 <- dataset$Visa_mconsumospesos / dataset$cliente_edad
dataset$nueva.41 <- dataset$Visa_mconsumototal / dataset$cliente_edad
dataset$nueva.42 <- dataset$Visa_mfinanciacion_limite / dataset$cliente_edad
dataset$nueva.43 <- dataset$Visa_mlimitecompra / dataset$cliente_edad
dataset$nueva.44 <- dataset$Visa_mpagado / dataset$cliente_edad
dataset$nueva.45 <- dataset$Visa_mpagominimo / dataset$cliente_edad
dataset$nueva.46 <- dataset$Visa_mpagosdolares / dataset$cliente_edad
dataset$nueva.47 <- dataset$Visa_mpagospesos / dataset$cliente_edad
dataset$nueva.48 <- dataset$Visa_msaldodolares / dataset$cliente_edad
dataset$nueva.49 <- dataset$Visa_msaldopesos / dataset$cliente_edad
dataset$nueva.50 <- dataset$Visa_msaldototal / dataset$cliente_edad
dataset$nueva.51 <- dataset$Visa_madelantodolares / dataset$cliente_antiguedad
dataset$nueva.52 <- dataset$Visa_madelantopesos / dataset$cliente_antiguedad
dataset$nueva.53 <- dataset$Visa_marca_atraso / dataset$cliente_antiguedad
dataset$nueva.54 <- dataset$Visa_mconsumosdolares / dataset$cliente_antiguedad
dataset$nueva.55 <- dataset$Visa_mconsumospesos / dataset$cliente_antiguedad
dataset$nueva.56 <- dataset$Visa_mconsumototal / dataset$cliente_antiguedad
dataset$nueva.57 <- dataset$Visa_mfinanciacion_limite / dataset$cliente_antiguedad
dataset$nueva.58 <- dataset$Visa_mlimitecompra / dataset$cliente_antiguedad
dataset$nueva.59 <- dataset$Visa_mpagado / dataset$cliente_antiguedad
dataset$nueva.60 <- dataset$Visa_mpagominimo / dataset$cliente_antiguedad
dataset$nueva.61 <- dataset$Visa_mpagosdolares / dataset$cliente_antiguedad
dataset$nueva.62 <- dataset$Visa_mpagospesos / dataset$cliente_antiguedad
dataset$nueva.63 <- dataset$Visa_msaldodolares / dataset$cliente_antiguedad
dataset$nueva.64 <- dataset$Visa_msaldopesos / dataset$cliente_antiguedad
dataset$nueva.65 <- dataset$Visa_msaldototal / dataset$cliente_antiguedad
## dataset$nueva.66 <- max(dataset$Master_madelantodolares,dataset$Visa_madelantodolares)
## dataset$nueva.67 <- max(dataset$Master_madelantopesos,dataset$Visa_madelantopesos)
## dataset$nueva.68 <- max(dataset$Master_marca_atraso,dataset$Visa_marca_atraso)
## dataset$nueva.69 <- max(dataset$Master_mconsumosdolares,dataset$Visa_mconsumosdolares)
## dataset$nueva.70 <- max(dataset$Master_mconsumospesos,dataset$Visa_mconsumospesos)
## dataset$nueva.71 <- max(dataset$Master_mconsumototal,dataset$Visa_mconsumototal)
## dataset$nueva.72 <- max(dataset$Master_mfinanciacion_limite,dataset$Visa_mfinanciacion_limite)
## dataset$nueva.73 <- max(dataset$Master_mlimitecompra,dataset$Visa_mlimitecompra)
## dataset$nueva.74 <- max(dataset$Master_mpagado,dataset$Visa_mpagado)
## dataset$nueva.75 <- max(dataset$Master_mpagominimo,dataset$Visa_mpagominimo)
## dataset$nueva.76 <- max(dataset$Master_mpagosdolares,dataset$Visa_mpagosdolares)
## dataset$nueva.77 <- max(dataset$Master_mpagospesos,dataset$Visa_mpagospesos)
## dataset$nueva.78 <- max(dataset$Master_msaldodolares,dataset$Visa_msaldodolares)
## dataset$nueva.79 <- max(dataset$Master_msaldopesos,dataset$Visa_msaldopesos)
## dataset$nueva.80 <- max(dataset$Master_msaldototal,dataset$Visa_msaldototal)
## dataset$nueva.81 <- mean(c(dataset$Master_madelantodolares,dataset$Visa_madelantodolares))
## dataset$nueva.82 <- mean(c(dataset$Master_madelantopesos,dataset$Visa_madelantopesos))
## dataset$nueva.83 <- mean(c(dataset$Master_marca_atraso,dataset$Visa_marca_atraso))
## dataset$nueva.84 <- mean(c(dataset$Master_mconsumosdolares,dataset$Visa_mconsumosdolares))
## dataset$nueva.85 <- mean(c(dataset$Master_mconsumospesos,dataset$Visa_mconsumospesos))
## dataset$nueva.86 <- mean(c(dataset$Master_mconsumototal,dataset$Visa_mconsumototal))
## dataset$nueva.87 <- mean(c(dataset$Master_mfinanciacion_limite,dataset$Visa_mfinanciacion_limite))
## dataset$nueva.88 <- mean(c(dataset$Master_mlimitecompra,dataset$Visa_mlimitecompra))
## dataset$nueva.89 <- mean(c(dataset$Master_mpagado,dataset$Visa_mpagado))
## dataset$nueva.90 <- mean(c(dataset$Master_mpagominimo,dataset$Visa_mpagominimo))
## dataset$nueva.91 <- mean(c(dataset$Master_mpagosdolares,dataset$Visa_mpagosdolares))
## dataset$nueva.92 <- mean(c(dataset$Master_mpagospesos,dataset$Visa_mpagospesos))
## dataset$nueva.93 <- mean(c(dataset$Master_msaldodolares,dataset$Visa_msaldodolares))
## dataset$nueva.94 <- mean(c(dataset$Master_msaldopesos,dataset$Visa_msaldopesos))
## dataset$nueva.95 <- mean(c(dataset$Master_msaldototal,dataset$Visa_msaldototal))
dataset$nueva.96 <- dataset$mactivos_margen / dataset$cliente_edad
dataset$nueva.97 <- dataset$marketing_coss_selling / dataset$cliente_edad
dataset$nueva.98 <- dataset$mautoservicio / dataset$cliente_edad
dataset$nueva.99 <- dataset$mbonos_corporativos / dataset$cliente_edad
dataset$nueva.100 <- dataset$mbonos_gobierno / dataset$cliente_edad
dataset$nueva.101 <- dataset$mcaja_ahorro_Nopaquete / dataset$cliente_edad
dataset$nueva.102 <- dataset$mcaja_ahorro_Paquete / dataset$cliente_edad
dataset$nueva.103 <- dataset$mcaja_ahorro_dolares / dataset$cliente_edad
dataset$nueva.104 <- dataset$mcajeros_ajenos / dataset$cliente_edad
dataset$nueva.105 <- dataset$mcajeros_propio / dataset$cliente_edad
dataset$nueva.106 <- dataset$mcajeros_propios_descuentos / dataset$cliente_edad
dataset$nueva.107 <- dataset$mcambio_monedas_compra / dataset$cliente_edad
dataset$nueva.108 <- dataset$mcambio_monedas_venta / dataset$cliente_edad
dataset$nueva.109 <- dataset$mcheques_depositados / dataset$cliente_edad
dataset$nueva.110 <- dataset$mcheques_depositados_rechazados / dataset$cliente_edad
dataset$nueva.111 <- dataset$mcheques_emitidos / dataset$cliente_edad
dataset$nueva.112 <- dataset$mcheques_emitidos_rechazados / dataset$cliente_edad
dataset$nueva.113 <- dataset$mcomisiones / dataset$cliente_edad
dataset$nueva.114 <- dataset$mcomisiones_mantenimiento / dataset$cliente_edad
dataset$nueva.115 <- dataset$mcomisiones_otras / dataset$cliente_edad
dataset$nueva.116 <- dataset$mcuenta_corriente_Nopaquete / dataset$cliente_edad
dataset$nueva.117 <- dataset$mcuenta_corriente_Paquete / dataset$cliente_edad
dataset$nueva.118 <- dataset$mcuenta_corriente_dolares / dataset$cliente_edad
dataset$nueva.119 <- dataset$mcuenta_debitos_automaticos / dataset$cliente_edad
dataset$nueva.120 <- dataset$mcuenta_descuentos / dataset$cliente_edad
dataset$nueva.121 <- dataset$mcuentas_saldo / dataset$cliente_edad
dataset$nueva.122 <- dataset$mdescubierto_preacordado / dataset$cliente_edad
dataset$nueva.123 <- dataset$mextraccion_autoservicio / dataset$cliente_edad
dataset$nueva.124 <- dataset$mfondos_comunes_inversion_dolares / dataset$cliente_edad
dataset$nueva.125 <- dataset$mfondos_comunes_inversion_pesos / dataset$cliente_edad
dataset$nueva.126 <- dataset$minversiones_otras / dataset$cliente_edad
dataset$nueva.127 <- dataset$mmonedas_extranjeras / dataset$cliente_edad
dataset$nueva.128 <- dataset$mpagodeservicios / dataset$cliente_edad
dataset$nueva.129 <- dataset$mpagomiscuentas / dataset$cliente_edad
dataset$nueva.130 <- dataset$mpasivos_margen / dataset$cliente_edad
dataset$nueva.131 <- dataset$mplan_sueldo / dataset$cliente_edad
dataset$nueva.132 <- dataset$mplan_sueldo_manual / dataset$cliente_edad
dataset$nueva.133 <- dataset$mplazo_fijo_dolares / dataset$cliente_edad
dataset$nueva.134 <- dataset$mplazo_fijo_pesos / dataset$cliente_edad
dataset$nueva.135 <- dataset$mprestamos_hipotecarios / dataset$cliente_edad
dataset$nueva.136 <- dataset$mprestamos_personales / dataset$cliente_edad
dataset$nueva.137 <- dataset$mprestamos_prendarios / dataset$cliente_edad
dataset$nueva.138 <- dataset$mrentabilidad / dataset$cliente_edad
dataset$nueva.139 <- dataset$mrentabilidad_annual / dataset$cliente_edad
dataset$nueva.140 <- dataset$mtarjeta_master_consumo / dataset$cliente_edad
dataset$nueva.141 <- dataset$mtarjeta_master_descuentos / dataset$cliente_edad
dataset$nueva.142 <- dataset$mtarjeta_visa_consumo / dataset$cliente_edad
dataset$nueva.143 <- dataset$mtarjeta_visa_descuentos / dataset$cliente_edad
dataset$nueva.144 <- dataset$mtitulos / dataset$cliente_edad
dataset$nueva.145 <- dataset$mtransferencias_emitidas / dataset$cliente_edad
dataset$nueva.146 <- dataset$mtransferencias_recibidas / dataset$cliente_edad
dataset$nueva.147 <- dataset$mttarjeta_master_debitos_automaticos / dataset$cliente_edad
dataset$nueva.148 <- dataset$mttarjeta_visa_debitos_automaticos / dataset$cliente_edad
dataset$nueva.149 <- dataset$mactivos_margen / dataset$cliente_antiguedad
dataset$nueva.150 <- dataset$marketing_coss_selling / dataset$cliente_antiguedad
dataset$nueva.151 <- dataset$mautoservicio / dataset$cliente_antiguedad
dataset$nueva.152 <- dataset$mbonos_corporativos / dataset$cliente_antiguedad
dataset$nueva.153 <- dataset$mbonos_gobierno / dataset$cliente_antiguedad
dataset$nueva.154 <- dataset$mcaja_ahorro_Nopaquete / dataset$cliente_antiguedad
dataset$nueva.155 <- dataset$mcaja_ahorro_Paquete / dataset$cliente_antiguedad
dataset$nueva.156 <- dataset$mcaja_ahorro_dolares / dataset$cliente_antiguedad
dataset$nueva.157 <- dataset$mcajeros_ajenos / dataset$cliente_antiguedad
dataset$nueva.158 <- dataset$mcajeros_propio / dataset$cliente_antiguedad
dataset$nueva.159 <- dataset$mcajeros_propios_descuentos / dataset$cliente_antiguedad
dataset$nueva.160 <- dataset$mcambio_monedas_compra / dataset$cliente_antiguedad
dataset$nueva.161 <- dataset$mcambio_monedas_venta / dataset$cliente_antiguedad
dataset$nueva.162 <- dataset$mcheques_depositados / dataset$cliente_antiguedad
dataset$nueva.163 <- dataset$mcheques_depositados_rechazados / dataset$cliente_antiguedad
dataset$nueva.164 <- dataset$mcheques_emitidos / dataset$cliente_antiguedad
dataset$nueva.165 <- dataset$mcheques_emitidos_rechazados / dataset$cliente_antiguedad
dataset$nueva.166 <- dataset$mcomisiones / dataset$cliente_antiguedad
dataset$nueva.167 <- dataset$mcomisiones_mantenimiento / dataset$cliente_antiguedad
dataset$nueva.168 <- dataset$mcomisiones_otras / dataset$cliente_antiguedad
dataset$nueva.169 <- dataset$mcuenta_corriente_Nopaquete / dataset$cliente_antiguedad
dataset$nueva.170 <- dataset$mcuenta_corriente_Paquete / dataset$cliente_antiguedad
dataset$nueva.171 <- dataset$mcuenta_corriente_dolares / dataset$cliente_antiguedad
dataset$nueva.172 <- dataset$mcuenta_debitos_automaticos / dataset$cliente_antiguedad
dataset$nueva.173 <- dataset$mcuenta_descuentos / dataset$cliente_antiguedad
dataset$nueva.174 <- dataset$mcuentas_saldo / dataset$cliente_antiguedad
dataset$nueva.175 <- dataset$mdescubierto_preacordado / dataset$cliente_antiguedad
dataset$nueva.176 <- dataset$mextraccion_autoservicio / dataset$cliente_antiguedad
dataset$nueva.177 <- dataset$mfondos_comunes_inversion_dolares / dataset$cliente_antiguedad
dataset$nueva.178 <- dataset$mfondos_comunes_inversion_pesos / dataset$cliente_antiguedad
dataset$nueva.179 <- dataset$minversiones_otras / dataset$cliente_antiguedad
dataset$nueva.180 <- dataset$mmonedas_extranjeras / dataset$cliente_antiguedad
dataset$nueva.181 <- dataset$mpagodeservicios / dataset$cliente_antiguedad
dataset$nueva.182 <- dataset$mpagomiscuentas / dataset$cliente_antiguedad
dataset$nueva.183 <- dataset$mpasivos_margen / dataset$cliente_antiguedad
dataset$nueva.184 <- dataset$mplan_sueldo / dataset$cliente_antiguedad
dataset$nueva.185 <- dataset$mplan_sueldo_manual / dataset$cliente_antiguedad
dataset$nueva.186 <- dataset$mplazo_fijo_dolares / dataset$cliente_antiguedad
dataset$nueva.187 <- dataset$mplazo_fijo_pesos / dataset$cliente_antiguedad
dataset$nueva.188 <- dataset$mprestamos_hipotecarios / dataset$cliente_antiguedad
dataset$nueva.189 <- dataset$mprestamos_personales / dataset$cliente_antiguedad
dataset$nueva.190 <- dataset$mprestamos_prendarios / dataset$cliente_antiguedad
dataset$nueva.191 <- dataset$mrentabilidad / dataset$cliente_antiguedad
dataset$nueva.192 <- dataset$mrentabilidad_annual / dataset$cliente_antiguedad
dataset$nueva.193 <- dataset$mtarjeta_master_consumo / dataset$cliente_antiguedad
dataset$nueva.194 <- dataset$mtarjeta_master_descuentos / dataset$cliente_antiguedad
dataset$nueva.195 <- dataset$mtarjeta_visa_consumo / dataset$cliente_antiguedad
dataset$nueva.196 <- dataset$mtarjeta_visa_descuentos / dataset$cliente_antiguedad
dataset$nueva.197 <- dataset$mtitulos / dataset$cliente_antiguedad
dataset$nueva.198 <- dataset$mtransferencias_emitidas / dataset$cliente_antiguedad
dataset$nueva.199 <- dataset$mtransferencias_recibidas / dataset$cliente_antiguedad
dataset$nueva.200 <- dataset$mttarjeta_master_debitos_automaticos / dataset$cliente_antiguedad
dataset$nueva.201 <- dataset$mttarjeta_visa_debitos_automaticos / dataset$cliente_antiguedad
dataset$nueva.202 <- dataset$Visa_madelantodolares / dataset$Visa_mlimitecompra
dataset$nueva.203 <- dataset$Visa_madelantopesos / dataset$Visa_mlimitecompra
dataset$nueva.204 <- dataset$Visa_marca_atraso / dataset$Visa_mlimitecompra
dataset$nueva.205 <- dataset$Visa_mconsumosdolares / dataset$Visa_mlimitecompra
dataset$nueva.206 <- dataset$Visa_mconsumospesos / dataset$Visa_mlimitecompra
dataset$nueva.207 <- dataset$Visa_mconsumototal / dataset$Visa_mlimitecompra
dataset$nueva.208 <- dataset$Visa_mfinanciacion_limite / dataset$Visa_mlimitecompra
dataset$nueva.209 <- dataset$Visa_mpagado / dataset$Visa_mlimitecompra
dataset$nueva.210 <- dataset$Visa_mpagominimo / dataset$Visa_mlimitecompra
dataset$nueva.211 <- dataset$Visa_mpagosdolares / dataset$Visa_mlimitecompra
dataset$nueva.212 <- dataset$Visa_mpagospesos / dataset$Visa_mlimitecompra
dataset$nueva.213 <- dataset$Visa_msaldodolares / dataset$Visa_mlimitecompra
dataset$nueva.214 <- dataset$Visa_msaldopesos / dataset$Visa_mlimitecompra
dataset$nueva.215 <- dataset$Visa_msaldototal / dataset$Visa_mlimitecompra
dataset$nueva.216 <- dataset$Visa_madelantodolares / dataset$Visa_mlimitecompra
dataset$nueva.217 <- dataset$Visa_madelantopesos / dataset$Visa_mlimitecompra
dataset$nueva.218 <- dataset$Visa_marca_atraso / dataset$Visa_mlimitecompra
dataset$nueva.219 <- dataset$Visa_mconsumosdolares / dataset$Visa_mlimitecompra
dataset$nueva.220 <- dataset$Visa_mconsumospesos / dataset$Visa_mlimitecompra
dataset$nueva.221 <- dataset$Visa_mconsumototal / dataset$Visa_mlimitecompra
dataset$nueva.222 <- dataset$Visa_mfinanciacion_limite / dataset$Visa_mlimitecompra
dataset$nueva.223 <- dataset$Visa_mpagado / dataset$Visa_mlimitecompra
dataset$nueva.224 <- dataset$Visa_mpagominimo / dataset$Visa_mlimitecompra
dataset$nueva.225 <- dataset$Visa_mpagosdolares / dataset$Visa_mlimitecompra
dataset$nueva.226 <- dataset$Visa_mpagospesos / dataset$Visa_mlimitecompra
dataset$nueva.227 <- dataset$Visa_msaldodolares / dataset$Visa_mlimitecompra
dataset$nueva.228 <- dataset$Visa_msaldopesos / dataset$Visa_mlimitecompra
dataset$nueva.229 <- dataset$Visa_msaldototal / dataset$Visa_mlimitecompra
dataset$nueva.230 <- dataset$Visa_madelantodolares / dataset$Visa_mfinanciacion_limite
dataset$nueva.231 <- dataset$Visa_madelantopesos / dataset$Visa_mfinanciacion_limite
dataset$nueva.232 <- dataset$Visa_marca_atraso / dataset$Visa_mfinanciacion_limite
dataset$nueva.233 <- dataset$Visa_mconsumosdolares / dataset$Visa_mfinanciacion_limite
dataset$nueva.234 <- dataset$Visa_mconsumospesos / dataset$Visa_mfinanciacion_limite
dataset$nueva.235 <- dataset$Visa_mconsumototal / dataset$Visa_mfinanciacion_limite
dataset$nueva.236 <- dataset$Visa_mpagado / dataset$Visa_mfinanciacion_limite
dataset$nueva.237 <- dataset$Visa_mpagominimo / dataset$Visa_mfinanciacion_limite
dataset$nueva.238 <- dataset$Visa_mpagosdolares / dataset$Visa_mfinanciacion_limite
dataset$nueva.239 <- dataset$Visa_mpagospesos / dataset$Visa_mfinanciacion_limite
dataset$nueva.240 <- dataset$Visa_msaldodolares / dataset$Visa_mfinanciacion_limite
dataset$nueva.241 <- dataset$Visa_msaldopesos / dataset$Visa_mfinanciacion_limite
dataset$nueva.242 <- dataset$Visa_msaldototal / dataset$Visa_mfinanciacion_limite
dataset$nueva.243 <- dataset$Visa_madelantodolares / dataset$Visa_mfinanciacion_limite
dataset$nueva.244 <- dataset$Visa_madelantopesos / dataset$Visa_mfinanciacion_limite
dataset$nueva.245 <- dataset$Visa_marca_atraso / dataset$Visa_mfinanciacion_limite
dataset$nueva.246 <- dataset$Visa_mconsumosdolares / dataset$Visa_mfinanciacion_limite
dataset$nueva.247 <- dataset$Visa_mconsumospesos / dataset$Visa_mfinanciacion_limite
dataset$nueva.248 <- dataset$Visa_mconsumototal / dataset$Visa_mfinanciacion_limite
dataset$nueva.249 <- dataset$Visa_mpagado / dataset$Visa_mfinanciacion_limite
dataset$nueva.250 <- dataset$Visa_mpagominimo / dataset$Visa_mfinanciacion_limite
dataset$nueva.251 <- dataset$Visa_mpagosdolares / dataset$Visa_mfinanciacion_limite
dataset$nueva.252 <- dataset$Visa_mpagospesos / dataset$Visa_mfinanciacion_limite
dataset$nueva.253 <- dataset$Visa_msaldodolares / dataset$Visa_mfinanciacion_limite
dataset$nueva.254 <- dataset$Visa_msaldopesos / dataset$Visa_mfinanciacion_limite
dataset$nueva.255 <- dataset$Visa_msaldototal / dataset$Visa_mfinanciacion_limite
dataset$nueva.256 <- dataset$Master_madelantodolares / dataset$Master_mlimitecompra
dataset$nueva.257 <- dataset$Master_madelantopesos / dataset$Master_mlimitecompra
dataset$nueva.258 <- dataset$Master_marca_atraso / dataset$Master_mlimitecompra
dataset$nueva.259 <- dataset$Master_mconsumosdolares / dataset$Master_mlimitecompra
dataset$nueva.260 <- dataset$Master_mconsumospesos / dataset$Master_mlimitecompra
dataset$nueva.261 <- dataset$Master_mconsumototal / dataset$Master_mlimitecompra
dataset$nueva.262 <- dataset$Master_mfinanciacion_limite / dataset$Master_mlimitecompra
dataset$nueva.263 <- dataset$Master_mpagado / dataset$Master_mlimitecompra
dataset$nueva.264 <- dataset$Master_mpagominimo / dataset$Master_mlimitecompra
dataset$nueva.265 <- dataset$Master_mpagosdolares / dataset$Master_mlimitecompra
dataset$nueva.266 <- dataset$Master_mpagospesos / dataset$Master_mlimitecompra
dataset$nueva.267 <- dataset$Master_msaldodolares / dataset$Master_mlimitecompra
dataset$nueva.268 <- dataset$Master_msaldopesos / dataset$Master_mlimitecompra
dataset$nueva.269 <- dataset$Master_msaldototal / dataset$Master_mlimitecompra
dataset$nueva.270 <- dataset$Master_madelantodolares / dataset$Master_mlimitecompra
dataset$nueva.271 <- dataset$Master_madelantopesos / dataset$Master_mlimitecompra
dataset$nueva.272 <- dataset$Master_marca_atraso / dataset$Master_mlimitecompra
dataset$nueva.273 <- dataset$Master_mconsumosdolares / dataset$Master_mlimitecompra
dataset$nueva.274 <- dataset$Master_mconsumospesos / dataset$Master_mlimitecompra
dataset$nueva.275 <- dataset$Master_mconsumototal / dataset$Master_mlimitecompra
dataset$nueva.276 <- dataset$Master_mfinanciacion_limite / dataset$Master_mlimitecompra
dataset$nueva.277 <- dataset$Master_mpagado / dataset$Master_mlimitecompra
dataset$nueva.278 <- dataset$Master_mpagominimo / dataset$Master_mlimitecompra
dataset$nueva.279 <- dataset$Master_mpagosdolares / dataset$Master_mlimitecompra
dataset$nueva.280 <- dataset$Master_mpagospesos / dataset$Master_mlimitecompra
dataset$nueva.281 <- dataset$Master_msaldodolares / dataset$Master_mlimitecompra
dataset$nueva.282 <- dataset$Master_msaldopesos / dataset$Master_mlimitecompra
dataset$nueva.283 <- dataset$Master_msaldototal / dataset$Master_mlimitecompra
dataset$nueva.284 <- dataset$Master_madelantodolares / dataset$Master_mfinanciacion_limite
dataset$nueva.285 <- dataset$Master_madelantopesos / dataset$Master_mfinanciacion_limite
dataset$nueva.286 <- dataset$Master_marca_atraso / dataset$Master_mfinanciacion_limite
dataset$nueva.287 <- dataset$Master_mconsumosdolares / dataset$Master_mfinanciacion_limite
dataset$nueva.288 <- dataset$Master_mconsumospesos / dataset$Master_mfinanciacion_limite
dataset$nueva.289 <- dataset$Master_mconsumototal / dataset$Master_mfinanciacion_limite
dataset$nueva.290 <- dataset$Master_mfinanciacion_limite / dataset$Master_mfinanciacion_limite
dataset$nueva.291 <- dataset$Master_mfinanciacion_limite / dataset$Master_mfinanciacion_limite
dataset$nueva.292 <- dataset$Master_mpagado / dataset$Master_mfinanciacion_limite
dataset$nueva.293 <- dataset$Master_mpagominimo / dataset$Master_mfinanciacion_limite
dataset$nueva.294 <- dataset$Master_mpagosdolares / dataset$Master_mfinanciacion_limite
dataset$nueva.295 <- dataset$Master_mpagospesos / dataset$Master_mfinanciacion_limite
dataset$nueva.296 <- dataset$Master_msaldodolares / dataset$Master_mfinanciacion_limite
dataset$nueva.297 <- dataset$Master_msaldopesos / dataset$Master_mfinanciacion_limite
dataset$nueva.298 <- dataset$Master_msaldototal / dataset$Master_mfinanciacion_limite
dataset$nueva.299 <- dataset$Master_madelantodolares / dataset$Master_mfinanciacion_limite
dataset$nueva.300 <- dataset$Master_madelantopesos / dataset$Master_mfinanciacion_limite
dataset$nueva.301 <- dataset$Master_marca_atraso / dataset$Master_mfinanciacion_limite
dataset$nueva.302 <- dataset$Master_mconsumosdolares / dataset$Master_mfinanciacion_limite
dataset$nueva.303 <- dataset$Master_mconsumospesos / dataset$Master_mfinanciacion_limite
dataset$nueva.304 <- dataset$Master_mconsumototal / dataset$Master_mfinanciacion_limite
dataset$nueva.305 <- dataset$Master_mpagado / dataset$Master_mfinanciacion_limite
dataset$nueva.306 <- dataset$Master_mpagominimo / dataset$Master_mfinanciacion_limite
dataset$nueva.307 <- dataset$Master_mpagosdolares / dataset$Master_mfinanciacion_limite
dataset$nueva.308 <- dataset$Master_mpagospesos / dataset$Master_mfinanciacion_limite
dataset$nueva.309 <- dataset$Master_msaldodolares / dataset$Master_mfinanciacion_limite
dataset$nueva.310 <- dataset$Master_msaldopesos / dataset$Master_mfinanciacion_limite
dataset$nueva.311 <- dataset$Master_msaldototal / dataset$Master_mfinanciacion_limite

## dias.desde.fotomes <- function(x) { if (is.na(x)) { return(-1000) } else { return(20150630-x) } }
dias.desde.fotomes <- function(x) { if (is.na(x)) { return(999999999) } else { return(as.numeric(as.Date("20150630", "%Y%m%d") - as.Date(as.character(x), "%Y%m%d"))) } }

dataset$nueva.f02.Master_Fvencimiento   <- mapply(dias.desde.fotomes, dataset$Master_Fvencimiento)
dataset$nueva.f03.Master_Finiciomora    <- mapply(dias.desde.fotomes, dataset$Master_Finiciomora)
dataset$nueva.f04.Master_fultimo_cierre <- mapply(dias.desde.fotomes, dataset$Master_fultimo_cierre)
dataset$nueva.f05.Master_fechaalta      <- mapply(dias.desde.fotomes, dataset$Master_fechaalta)

dataset$nueva.f07.Visa_Fvencimiento     <- mapply(dias.desde.fotomes, dataset$Visa_Fvencimiento)
dataset$nueva.f08.Visa_Finiciomora      <- mapply(dias.desde.fotomes, dataset$Visa_Finiciomora)
dataset$nueva.f09.Visa_fultimo_cierre   <- mapply(dias.desde.fotomes, dataset$Visa_fultimo_cierre)
dataset$nueva.f10.Visa_fechaalta        <- mapply(dias.desde.fotomes, dataset$Visa_fechaalta)

## write.table(dataset, file="data.con.nuevas.variables.tsv", row.names=FALSE, quote=FALSE, sep="\t")
write.table(dataset, file="data/data.final.tsv", row.names=FALSE, quote=FALSE, sep="\t")
