TRUNCATE extra_abril_1;

INSERT INTO extra_abril_1 (
numero_de_cliente,

cuenta_estados,

cliente_edad__cliente_antiguedad,
cliente_edad__Visa_mfinanciacion_limite,
cliente_edad__Master_mfinanciacion_limite,
cliente_edad__Visa_mlimitecompra,
cliente_edad__Master_mlimitecompra,

Master_madelantodolares__Master_mfinanciacion_limite,
Master_madelantopesos__Master_mfinanciacion_limite,
Master_marca_atraso__Master_mfinanciacion_limite,
Master_mconsumosdolares__Master_mfinanciacion_limite,
Master_mconsumospesos__Master_mfinanciacion_limite,
Master_mconsumototal__Master_mfinanciacion_limite,
Master_mlimitecompra__Master_mfinanciacion_limite,
Master_mpagado__Master_mfinanciacion_limite,
Master_mpagominimo__Master_mfinanciacion_limite,
Master_mpagosdolares__Master_mfinanciacion_limite,
Master_mpagospesos__Master_mfinanciacion_limite,
Master_msaldodolares__Master_mfinanciacion_limite,
Master_msaldopesos__Master_mfinanciacion_limite,
Master_msaldototal__Master_mfinanciacion_limite,
Master_tadelantosefectivo__Master_mfinanciacion_limite,
Master_tconsumos__Master_mfinanciacion_limite,

Master_madelantodolares__Master_mlimitecompra,
Master_madelantopesos__Master_mlimitecompra,
Master_marca_atraso__Master_mlimitecompra,
Master_mconsumosdolares__Master_mlimitecompra,
Master_mconsumospesos__Master_mlimitecompra,
Master_mconsumototal__Master_mlimitecompra,
Master_mfinanciacion_limite__Master_mlimitecompra,
Master_mpagado__Master_mlimitecompra,
Master_mpagominimo__Master_mlimitecompra,
Master_mpagosdolares__Master_mlimitecompra,
Master_mpagospesos__Master_mlimitecompra,
Master_msaldodolares__Master_mlimitecompra,
Master_msaldopesos__Master_mlimitecompra,
Master_msaldototal__Master_mlimitecompra,
Master_tadelantosefectivo__Master_mlimitecompra,
Master_tconsumos__Master_mlimitecompra,

Visa_madelantodolares__Visa_mfinanciacion_limite,
Visa_madelantopesos__Visa_mfinanciacion_limite,
Visa_marca_atraso__Visa_mfinanciacion_limite,
Visa_mconsumosdolares__Visa_mfinanciacion_limite,
Visa_mconsumospesos__Visa_mfinanciacion_limite,
Visa_mconsumototal__Visa_mfinanciacion_limite,
Visa_mlimitecompra__Visa_mfinanciacion_limite,
Visa_mpagado__Visa_mfinanciacion_limite,
Visa_mpagominimo__Visa_mfinanciacion_limite,
Visa_mpagosdolares__Visa_mfinanciacion_limite,
Visa_mpagospesos__Visa_mfinanciacion_limite,
Visa_msaldodolares__Visa_mfinanciacion_limite,
Visa_msaldopesos__Visa_mfinanciacion_limite,
Visa_msaldototal__Visa_mfinanciacion_limite,
Visa_tadelantosefectivo__Visa_mfinanciacion_limite,
Visa_tconsumos__Visa_mfinanciacion_limite,

Visa_madelantodolares__Visa_mlimitecompra,
Visa_madelantopesos__Visa_mlimitecompra,
Visa_marca_atraso__Visa_mlimitecompra,
Visa_mconsumosdolares__Visa_mlimitecompra,
Visa_mconsumospesos__Visa_mlimitecompra,
Visa_mconsumototal__Visa_mlimitecompra,
Visa_mfinanciacion_limite__Visa_mlimitecompra,
Visa_mpagado__Visa_mlimitecompra,
Visa_mpagominimo__Visa_mlimitecompra,
Visa_mpagosdolares__Visa_mlimitecompra,
Visa_mpagospesos__Visa_mlimitecompra,
Visa_msaldodolares__Visa_mlimitecompra,
Visa_msaldopesos__Visa_mlimitecompra,
Visa_msaldototal__Visa_mlimitecompra,
Visa_tadelantosefectivo__Visa_mlimitecompra,
Visa_tconsumos__Visa_mlimitecompra
)

SELECT

numero_de_cliente,

IF(Visa_cuenta_estado IS NULL OR Visa_cuenta_estado = ' ', -1000, Visa_cuenta_estado) + IF(Master_cuenta_estado IS NULL OR Master_cuenta_estado = ' ', -5000, Master_cuenta_estado),

CAST(cliente_edad / cliente_antiguedad AS DECIMAL(7,5)),
CAST(cliente_edad / Visa_mfinanciacion_limite AS DECIMAL(7,5)),
CAST(cliente_edad / Master_mfinanciacion_limite AS DECIMAL(7,5)),
CAST(cliente_edad / Visa_mlimitecompra AS DECIMAL(7,5)),
CAST(cliente_edad / Master_mlimitecompra AS DECIMAL(7,5)),

CAST(Master_madelantodolares / Master_mfinanciacion_limite AS DECIMAL(7,5)),
CAST(Master_madelantopesos / Master_mfinanciacion_limite AS DECIMAL(7,5)),
CAST(Master_marca_atraso / Master_mfinanciacion_limite AS DECIMAL(7,5)),
CAST(Master_mconsumosdolares / Master_mfinanciacion_limite AS DECIMAL(7,5)),
CAST(Master_mconsumospesos / Master_mfinanciacion_limite AS DECIMAL(7,5)),
CAST(Master_mconsumototal / Master_mfinanciacion_limite AS DECIMAL(7,5)),
CAST(Master_mlimitecompra / Master_mfinanciacion_limite AS DECIMAL(7,5)),
CAST(Master_mpagado / Master_mfinanciacion_limite AS DECIMAL(7,5)),
CAST(Master_mpagominimo / Master_mfinanciacion_limite AS DECIMAL(7,5)),
CAST(Master_mpagosdolares / Master_mfinanciacion_limite AS DECIMAL(7,5)),
CAST(Master_mpagospesos / Master_mfinanciacion_limite AS DECIMAL(7,5)),
CAST(Master_msaldodolares / Master_mfinanciacion_limite AS DECIMAL(7,5)),
CAST(Master_msaldopesos / Master_mfinanciacion_limite AS DECIMAL(7,5)),
CAST(Master_msaldototal / Master_mfinanciacion_limite AS DECIMAL(7,5)),
CAST(Master_tadelantosefectivo / Master_mfinanciacion_limite AS DECIMAL(7,5)),
CAST(Master_tconsumos / Master_mfinanciacion_limite AS DECIMAL(7,5)),

CAST(Master_madelantodolares / Master_mlimitecompra AS DECIMAL(7,5)),
CAST(Master_madelantopesos / Master_mlimitecompra AS DECIMAL(7,5)),
CAST(Master_marca_atraso / Master_mlimitecompra AS DECIMAL(7,5)),
CAST(Master_mconsumosdolares / Master_mlimitecompra AS DECIMAL(7,5)),
CAST(Master_mconsumospesos / Master_mlimitecompra AS DECIMAL(7,5)),
CAST(Master_mconsumototal / Master_mlimitecompra AS DECIMAL(7,5)),
CAST(Master_mfinanciacion_limite / Master_mlimitecompra AS DECIMAL(7,5)),
CAST(Master_mpagado / Master_mlimitecompra AS DECIMAL(7,5)),
CAST(Master_mpagominimo / Master_mlimitecompra AS DECIMAL(7,5)),
CAST(Master_mpagosdolares / Master_mlimitecompra AS DECIMAL(7,5)),
CAST(Master_mpagospesos / Master_mlimitecompra AS DECIMAL(7,5)),
CAST(Master_msaldodolares / Master_mlimitecompra AS DECIMAL(7,5)),
CAST(Master_msaldopesos / Master_mlimitecompra AS DECIMAL(7,5)),
CAST(Master_msaldototal / Master_mlimitecompra AS DECIMAL(7,5)),
CAST(Master_tadelantosefectivo / Master_mlimitecompra AS DECIMAL(7,5)),
CAST(Master_tconsumos / Master_mlimitecompra AS DECIMAL(7,5)),

CAST(Visa_madelantodolares / Visa_mfinanciacion_limite AS DECIMAL(7,5)),
CAST(Visa_madelantopesos / Visa_mfinanciacion_limite AS DECIMAL(7,5)),
CAST(Visa_marca_atraso / Visa_mfinanciacion_limite AS DECIMAL(7,5)),
CAST(Visa_mconsumosdolares / Visa_mfinanciacion_limite AS DECIMAL(7,5)),
CAST(Visa_mconsumospesos / Visa_mfinanciacion_limite AS DECIMAL(7,5)),
CAST(Visa_mconsumototal / Visa_mfinanciacion_limite AS DECIMAL(7,5)),
CAST(Visa_mlimitecompra / Visa_mfinanciacion_limite AS DECIMAL(7,5)),
CAST(Visa_mpagado / Visa_mfinanciacion_limite AS DECIMAL(7,5)),
CAST(Visa_mpagominimo / Visa_mfinanciacion_limite AS DECIMAL(7,5)),
CAST(Visa_mpagosdolares / Visa_mfinanciacion_limite AS DECIMAL(7,5)),
CAST(Visa_mpagospesos / Visa_mfinanciacion_limite AS DECIMAL(7,5)),
CAST(Visa_msaldodolares / Visa_mfinanciacion_limite AS DECIMAL(7,5)),
CAST(Visa_msaldopesos / Visa_mfinanciacion_limite AS DECIMAL(7,5)),
CAST(Visa_msaldototal / Visa_mfinanciacion_limite AS DECIMAL(7,5)),
CAST(Visa_tadelantosefectivo / Visa_mfinanciacion_limite AS DECIMAL(7,5)),
CAST(Visa_tconsumos / Visa_mfinanciacion_limite AS DECIMAL(7,5)),

CAST(Visa_madelantodolares / Visa_mlimitecompra AS DECIMAL(7,5)),
CAST(Visa_madelantopesos / Visa_mlimitecompra AS DECIMAL(7,5)),
CAST(Visa_marca_atraso / Visa_mlimitecompra AS DECIMAL(7,5)),
CAST(Visa_mconsumosdolares / Visa_mlimitecompra AS DECIMAL(7,5)),
CAST(Visa_mconsumospesos / Visa_mlimitecompra AS DECIMAL(7,5)),
CAST(Visa_mconsumototal / Visa_mlimitecompra AS DECIMAL(7,5)),
CAST(Visa_mfinanciacion_limite / Visa_mlimitecompra AS DECIMAL(7,5)),
CAST(Visa_mpagado / Visa_mlimitecompra AS DECIMAL(7,5)),
CAST(Visa_mpagominimo / Visa_mlimitecompra AS DECIMAL(7,5)),
CAST(Visa_mpagosdolares / Visa_mlimitecompra AS DECIMAL(7,5)),
CAST(Visa_mpagospesos / Visa_mlimitecompra AS DECIMAL(7,5)),
CAST(Visa_msaldodolares / Visa_mlimitecompra AS DECIMAL(7,5)),
CAST(Visa_msaldopesos / Visa_mlimitecompra AS DECIMAL(7,5)),
CAST(Visa_msaldototal / Visa_mlimitecompra AS DECIMAL(7,5)),
CAST(Visa_tadelantosefectivo / Visa_mlimitecompra AS DECIMAL(7,5)),
CAST(Visa_tconsumos / Visa_mlimitecompra AS DECIMAL(7,5))

FROM hasta_abril
WHERE foto_mes = 201604
ORDER BY numero_de_cliente ASC;
