# 8. Marcos y modelos de calidad

¿Con qué criterio decimos que una entrega es buena? En [7. Del hallazgo
a la
métrica](https://sebollin.github.io/lupa/articles/del-hallazgo-a-la-metrica.md)
medimos lo que `lupa` propuso a partir del perfil. Ahora el criterio lo
declaramos nosotros: qué aspectos de la calidad nos importan, con qué
métricas los medimos y cómo leemos los números.

> **En esta guía:**
>
> - declaramos un marco propio de dimensiones y factores con
>   [`marco_calidad()`](https://sebollin.github.io/lupa/reference/marco_calidad.md);
> - construimos métricas en tres niveles, y una métrica nueva con
>   [`metrica()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md);
> - reunimos las métricas en un modelo ligado al marco y vemos qué
>   cubre;
> - leemos el tablero con su `orientacion`, armamos un índice con pesos
>   declarados y evaluamos con perfiles de madurez;
> - conocemos los marcos incluidos: ISO/IEC 25012, CEA/CEPAL y AGESIC.

## Un marco es una taxonomía

`lupa` sigue el linaje de dimensiones, factores y métricas de Batini y
Scannapieco (2016). Una dimensión reúne factores: la completitud reúne
la densidad, por ejemplo. Un factor reúne métricas: `NoNulo` mide la
densidad. Esa jerarquía sirve para clasificar y para navegar el modelo,
no para calcular: el marco no define un “puntaje de exactitud” ni un
índice global, y
[`agregar()`](https://sebollin.github.io/lupa/reference/agregar.md) no
sube de una métrica a su factor ni a su dimensión. Promediar factores
sin una decisión explícita ocultaría prioridades y correlaciones entre
ellos.

Declaramos el marco de nuestra entrega, `datos_operativos`. Cada métrica
trae su dimensión y su factor, y un modelo ligado a un marco exige que
cada una caiga en uno de sus pares. Para las tres métricas del núcleo
que vamos a usar tomamos los pares del catálogo de AGESIC, que los
nombra; los otros dos factores son propios.

``` r

library(lupa)

catalogo <- catalogo_agesic()
del_nucleo <- unique(catalogo[
  catalogo$metrica_lupa %in% c("NoNulo", "EntidadDuplicada", "Formato"),
  c("dimension", "factor")
])
propios <- data.frame(
  dimension = c("Trazabilidad", "Frescura"),
  factor = c("Origen documentado", "Actualidad")
)
factores <- rbind(del_nucleo, propios)
factores$perfil_mide <- factores$dimension == "Unicidad"
instrucciones <- c(
  Completitud = "Medir NoNulo sobre las columnas obligatorias.",
  Unicidad = "Revisar las filas duplicadas que informa el perfil.",
  Exactitud = "Medir Formato con un validador de la columna.",
  Trazabilidad = "Declarar una métrica sobre el sistema de origen.",
  Frescura = "Declarar la vigencia con vigencia() y medir la desactualización."
)
factores$como_resolverlo <- unname(instrucciones[factores$dimension])

marco_entrega <- marco_calidad("Marco de la entrega operativa", factores)
as.data.frame(marco_entrega)[, c("dimension", "factor", "perfil_mide")]
#>      dimension                 factor perfil_mide
#> 1    Exactitud Correctitud sintáctica       FALSE
#> 2  Completitud               Densidad       FALSE
#> 3     Unicidad         No-duplicación        TRUE
#> 4 Trazabilidad     Origen documentado       FALSE
#> 5     Frescura             Actualidad       FALSE
```

En un data frame, `perfil_mide` dice para qué factores el perfil por sí
solo ya aporta evidencia suficiente; aquí, la no duplicación, porque
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
busca filas repetidas. `como_resolverlo` es la instrucción que se
muestra mientras el factor no esté medido. La forma abreviada, una lista
como `list(Completitud = "Densidad")`, deja `perfil_mide` en `FALSE`
para todos los factores: nunca presume que examinar equivale a medir.

Las métricas que trae `lupa` usan los nombres del marco de AGESIC
(`NoNulo` declara Completitud y Densidad); por eso tres de nuestros
factores los repiten. Para un factor con nombre propio, como “Origen
documentado”, escribimos una métrica que lo declare.

## Tres niveles de métrica

Una métrica existe en tres niveles. La genérica dice qué se mide; la
especializada, con qué criterio; la instanciada, dónde.
[`metricas_nucleo()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md)
reúne las genéricas que trae el paquete, y
[`propiedades_metrica()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md)
dice qué criterio se puede fijar en cada una:

``` r

nucleo <- metricas_nucleo()
propiedades_metrica(nucleo$NoNulo)
#>       propiedad configurada
#> 1 valores_nulos       FALSE
#> 2     aplicable       FALSE
#> 3      opcional       FALSE
```

[`especializar()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md)
fija esas propiedades y le da un nombre a la versión que resulta.
[`instanciar()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md)
la liga a una tabla, la entidad, y a una columna, el atributo:

``` r

no_nulo_sin_disfraz <- especializar(
  nucleo$NoNulo,
  nombre_especifico = "NoNuloSinDisfraz",
  valores_nulos = c("", "S/D")
)
completitud_canal <- instanciar(no_nulo_sin_disfraz, "datos_operativos", "canal")
completitud_canal$nombre
#> [1] "NoNuloSinDisfraz@datos_operativos.canal"
```

La especializada se puede ligar a otras columnas y a otras entregas sin
repetir el criterio. El nombre de la instancia tiene la forma
`Especializada@tabla.columna` y viaja en la columna
`metrica_instanciada` de cada medida. Las métricas que salen de una
propuesta se llaman como su fila (`NoNulo:canal`).

La entidad es el nombre de la tabla. Si a
[`medir()`](https://sebollin.github.io/lupa/reference/medir.md) le damos
un solo data frame, lo asocia a la entidad del modelo; si el modelo mide
varias tablas, recibe una lista con nombres, uno por entidad.

`Formato` acepta como criterio una expresión regular, un diccionario o
un validador: una función vectorizada que devuelve `TRUE` o `FALSE` para
cada valor. La usamos con el validador de correo para la columna
`contacto`. La especializada es a su vez una función: llamarla con la
tabla y la columna equivale a
[`instanciar()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md).

``` r

propiedades_metrica(nucleo$Formato)
#>           propiedad configurada
#> 1 expresion_regular       FALSE
#> 2       diccionario       FALSE
#> 3         validador       FALSE
correo_valido <- especializar(
  nucleo$Formato, "CorreoValido",
  validador = validadores_internacionales()$correo
)
formato_contacto <- correo_valido("datos_operativos", "contacto")
duplicadas <- instanciar(especializar(nucleo$EntidadDuplicada), "datos_operativos")
```

`EntidadDuplicada` no tiene propiedades que fijar y se liga sólo a la
tabla: mide cada fila.

## Validadores y packs territoriales

El núcleo incluye validadores internacionales: códigos ISO de países y
de monedas, correo, Luhn y módulo 97. Los de un país se agrupan en un
pack territorial. Uruguay es el pack de referencia, no una rama especial
del motor:

``` r

names(validadores_internacionales())
#> [1] "iso3166_alpha2" "iso3166_alpha3" "iso4217"        "correo"         "luhn"          
#> [6] "mod97"
validadores_uruguay()$cedula(c("1.234.567-2", "1.234.567-3"))
#> [1]  TRUE FALSE
```

Un proyecto de otro país arma su pack con
[`pack_validadores()`](https://sebollin.github.io/lupa/reference/pack_validadores.md)
y mantiene sus funciones en su propio paquete o guion, sin registrar
nombres ni modificar `lupa`. La ayuda de
[`pack_validadores()`](https://sebollin.github.io/lupa/reference/pack_validadores.md)
trae un ejemplo completo con un RUT chileno.

## Una métrica propia

Para “Origen documentado” no hay métrica en el núcleo: la declaramos con
[`metrica()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md).
El método recibe una lista con nombre de tablas y la instancia que se
está midiendo, y devuelve una fila por objeto medido con las columnas
`resultado`, `entidad`, `atributo`, `fila` y `objeto`. La granularidad
acepta el nombre del marco (`instanciaAtributo`) o su equivalente en
tablas (`celda`).

``` r

metodo_origen <- function(tablas, instancia) {
  x <- tablas[[instancia$entidad]][[instancia$atributos]]
  data.frame(
    resultado = !is.na(x) & nzchar(x),
    entidad = instancia$entidad,
    atributo = instancia$atributos,
    fila = seq_along(x),
    objeto = paste0(
      instancia$entidad, "$", instancia$atributos, "[", seq_along(x), "]"
    )
  )
}

OrigenDeclarado <- metrica(
  nombre = "OrigenDeclarado",
  semantica = "Indica si cada registro declara su sistema de origen.",
  granularidad = "celda",
  tipo_resultado = "booleano",
  dimension = "Trazabilidad",
  factor = "Origen documentado",
  metodo = metodo_origen
)
origen <- instanciar(especializar(OrigenDeclarado), "datos_operativos", "sistema")
```

[`metrica()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md)
devuelve la genérica, que también es una función. Por eso la forma corta
encadena dos llamadas:
`OrigenDeclarado()("datos_operativos", "sistema")` especializa con la
primera e instancia con la segunda.

## Un modelo ligado al marco

[`modelo()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md)
reúne las instancias. Con `marco =`, comprueba que cada una pertenezca a
uno de los pares del marco, y rechaza el modelo nombrando el par que
falta si alguna no pertenece. Medimos y cruzamos la medición con el
marco:

``` r

modelo_entrega <- modelo(
  list(completitud_canal, formato_contacto, duplicadas, origen),
  marco = marco_entrega
)
medicion <- medir(
  modelo_entrega, datos_operativos,
  id_medicion = "entrega-enero", fecha = "2026-01-15"
)
perfil <- perfilar(datos_operativos, fecha = "2026-01-15")
cobertura <- cobertura_analisis(perfil, medicion, modelo = marco_entrega)
cobertura[, c("dimension", "factor", "estado")]
#>      dimension                 factor       estado
#> 1    Exactitud Correctitud sintáctica       medida
#> 2  Completitud               Densidad       medida
#> 3     Unicidad         No-duplicación       medida
#> 4 Trazabilidad     Origen documentado       medida
#> 5     Frescura             Actualidad no_declarada
```

Cuatro de los cinco factores quedaron medidos. La actualidad sigue sin
declarar, y la tabla lo dice con la instrucción que escribimos en el
marco:

``` r

writeLines(cobertura$como_resolverlo[cobertura$factor == "Actualidad"])
#> Declarar la vigencia con vigencia() y medir la desactualización.
```

Los contratos temporales y de precisión se declaran con
[`vigencia()`](https://sebollin.github.io/lupa/reference/contratos_medicion.md)
y
[`escala()`](https://sebollin.github.io/lupa/reference/contratos_medicion.md):
el paquete no intenta aprenderlos de una sola entrega.

## El tablero y su orientación

[`tablero_calidad()`](https://sebollin.github.io/lupa/reference/tablero_calidad.md)
resume la medición en una fila por métrica y objeto, y conserva el marco
y su cobertura. Las métricas booleanas se resumen con `ratio`.

``` r

tablero <- tablero_calidad(medicion, marco = marco_entrega)
tablero[, c("dimension", "metrica", "objeto", "valor", "orientacion", "universo")]
#> 
#> ── Tablero de calidad ────────────────────────────────────────────────────────────────────
#>     dimension          metrica   objeto     valor orientacion universo
#>   Completitud           NoNulo    canal 0.8461538 conformidad   celdas
#>     Exactitud          Formato contacto 0.9230769 conformidad   celdas
#>      Unicidad EntidadDuplicada  (tabla) 0.1538462     defecto    filas
#>  Trazabilidad  OrigenDeclarado  sistema 1.0000000 conformidad   celdas
```

La columna `orientacion` dice cómo leer cada proporción: `conformidad`
significa que un valor mayor es mejor, `defecto` que un valor menor es
mejor y `no_aplica` que la métrica no es una proporción combinable. El
0.154 de `EntidadDuplicada` es la proporción de filas que participan en
un duplicado (cuanto más bajo, mejor), y el 0.846 de `NoNulo` es la
proporción de celdas con valor (cuanto más alto, mejor). Los números
están en la misma escala pero no se leen igual, y promediarlos sin mirar
`orientacion` da un número que no significa nada.

La columna `universo` dice la unidad de cada valor: celdas o filas. El
atributo `alcance` cuenta los factores del marco, los medidos y los que
no tienen métrica:

``` r

attr(tablero, "alcance")
#>   factores_marco factores_medidos sin_metrica_declarada no_aplican fuera_de_alcance
#> 1              5                4                     1          0                0
#>   medidos_fuera_del_marco
#> 1                       0
```

## Un índice sólo con pesos declarados

Sin `pesos` no hay número: `indice_calidad(tablero)` devuelve el mismo
tablero. Los pesos no pertenecen a `lupa`; los declaramos nosotros según
la decisión del proyecto.

``` r

class(indice_calidad(tablero))
#> [1] "tablero_calidad" "data.frame"

# Pesos elegidos para este ejemplo; no son valores predeterminados de lupa.
pesos <- c(Completitud = 0.4, Unicidad = 0.3, Exactitud = 0.2, Trazabilidad = 0.1)
indice <- indice_calidad(tablero, pesos = pesos)
indice$valor
#> [1] 0.8769231
indice$dimensiones[, c("dimension", "valor", "peso", "aporte")]
#>      dimension     valor peso    aporte
#> 1  Completitud 0.8461538  0.4 0.3384615
#> 2    Exactitud 0.9230769  0.2 0.1846154
#> 3     Unicidad 0.8461538  0.3 0.2538462
#> 4 Trazabilidad 1.0000000  0.1 0.1000000
```

Para las métricas de `defecto`, el índice usa `1 - valor`: la unicidad
entra como 0.846. Las de `no_aplica` quedan fuera y se declaran como
exclusiones. El índice tampoco promedia por su cuenta dos componentes de
una misma dimensión: si `NoNulo` midiera dos columnas, habría que
declarar `pesos_internos` para combinarlas. Y cuando los componentes
salen de unidades distintas, lo advierte:

``` r

writeLines(strwrap(indice$advertencia_universos, 88))
#> Los componentes salen de 2 universos distintos, así que el índice combina unidades que
#> no son comparables y sólo lo hace porque quien lo solicitó declaró los pesos. Por
#> universo: celdas: componente-0001, componente-0002, componente-0004; filas:
#> componente-0003.
```

La advertencia no resuelve la diferencia de unidades: el índice combina
celdas y filas sólo porque nosotros declaramos los pesos.

## Perfiles de madurez

Además de las reglas propias, `lupa` trae los tres perfiles de madurez
de AGESIC, `Basico`, `Intermedio` y `Avanzado`, con condiciones
estrictas `> 0.5`, `> 0.7` y `> 0.9`. Los cortes se aplican al resultado
agregado de cada métrica, así que primero agregamos cada una hasta su
objeto: las de celda hasta la columna, la de filas hasta la tabla.

``` r

agregadas <- rbind(
  agregar(medicion[medicion$metrica == "NoNulo", ], "atributo", "ratio"),
  agregar(medicion[medicion$metrica == "Formato", ], "atributo", "ratio"),
  agregar(medicion[medicion$metrica == "OrigenDeclarado", ], "atributo", "ratio"),
  agregar(medicion[medicion$metrica == "EntidadDuplicada", ], "entidad", "ratio")
)
madurez <- perfiles_madurez()
sapply(madurez, function(perfil) evaluar(agregadas, perfil)$perfiles$resultado)
#>     Basico Intermedio   Avanzado 
#>        1.0        1.0        0.5
```

Los perfiles incluidos leen la orientación: la duplicación se compara
como `1 - valor`. Cumplen las cuatro métricas con `> 0.5` y con `> 0.7`;
con `> 0.9`, sólo el correo y el origen. Sobre las medidas por celda,
que valen 0 o 1, los tres cortes serían la misma condición. Otra familia
de perfiles se construye con umbrales con nombre:

``` r

names(perfiles_madurez(umbrales = c(Inicial = 0.4, Consolidado = 0.85)))
#> [1] "Inicial"     "Consolidado"
```

## Los marcos incluidos

Además del marco propio, `lupa` trae tres marcos publicados. Ninguno es
una restricción del núcleo: el de AGESIC, que
[`cobertura_analisis()`](https://sebollin.github.io/lupa/reference/cobertura_analisis.md)
usa por omisión, es una instancia verificable más.

[`marco_iso25012()`](https://sebollin.github.io/lupa/reference/marco_calidad.md)
ofrece las 15 características de ISO/IEC 25012:2008. La norma distingue
características inherentes, dependientes del sistema y aplicables desde
ambas perspectivas. `lupa` usa esos tres grupos como dimensiones y las
características como factores; es una adaptación a la forma dimensión y
factor, no la afirmación de que la norma defina esa jerarquía. Sus
descripciones están redactadas para el paquete y no reproducen el texto
normativo.

``` r

iso <- marco_iso25012()
table(as.data.frame(iso)$dimension)
#> 
#>             Dependiente del sistema                           Inherente 
#>                                   3                                   5 
#> Inherente y dependiente del sistema 
#>                                   7
```

[`marco_cepal()`](https://sebollin.github.io/lupa/reference/marco_calidad.md)
representa los niveles y principios del marco nacional de aseguramiento
de la calidad de Naciones Unidas, adoptado y adaptado para América
Latina y el Caribe por la CEA/CEPAL.

``` r

cepal <- marco_cepal()
tabla_cepal <- as.data.frame(cepal)
c(
  niveles = length(unique(tabla_cepal$dimension)),
  principios = nrow(tabla_cepal),
  fuera_de_alcance = sum(tabla_cepal$disponibilidad == "fuera_de_alcance")
)
#>          niveles       principios fuera_de_alcance 
#>                4               19               13
```

Los 13 principios fuera de alcance no son una limitación transitoria:
miden el sistema estadístico, el entorno institucional o el proceso, no
el dato. “Asegurar la independencia profesional”, por ejemplo, no se
establece mirando una tabla.

[`marco_agesic()`](https://sebollin.github.io/lupa/reference/marco_calidad.md)
devuelve los 17 factores que
[`cobertura_analisis()`](https://sebollin.github.io/lupa/reference/cobertura_analisis.md)
usa por omisión. El catálogo que usamos al principio,
[`catalogo_agesic()`](https://sebollin.github.io/lupa/reference/catalogo_agesic.md),
mantiene aparte las 49 métricas del marco, con lo que el paquete hace
con cada una:

``` r

table(catalogo$estado)
#> 
#>     implementada   via_agregacion        pendiente fuera_de_alcance 
#>               29                9                1               10
ejemplos <- catalogo$metrica_agesic %in% c(
  "RatioNoNulos", "RatioCobertura", "IndiceFidelidadConReferencia"
)
catalogo[ejemplos, c("metrica_agesic", "estado", "motivo")]
#>                  metrica_agesic           estado               motivo
#> 14 IndiceFidelidadConReferencia fuera_de_alcance     decision_alcance
#> 27               RatioCobertura     implementada requiere_referencial
#> 30                 RatioNoNulos   via_agregacion           agregacion
writeLines(catalogo$observacion[ejemplos])
#> Métrica [TD:«Img»]; exige comparar imágenes contra una referencia.
#> Exige referencial() con completo = TRUE y alcance explícito.
#> Se obtiene con Ratio sobre NoNulo.
```

`estado` dice si la métrica está implementada, se obtiene por
agregación, está pendiente o queda fuera del alcance tabular, y `motivo`
explica la causa. `RatioNoNulos` es lo que hicimos en la guía anterior:
`NoNulo` agregado a la columna con `ratio`. `RatioCobertura` está
implementada, pero necesita un referencial. La fidelidad de imágenes
queda fuera. El catálogo es la respuesta verificable a “qué implementa
el paquete”: lo que queda fuera no se presenta como resuelto.

Un diccionario enumera los valores válidos. Un referencial, en cambio,
vincula claves con valores de una fuente externa, y sólo permite medir
cobertura cuando declara que está completo y con qué alcance. Lo
consumen las tres métricas de
[`metricas_referencial()`](https://sebollin.github.io/lupa/reference/metricas_referencial.md).

``` r

padron <- referencial(
  data.frame(codigo = c("01", "02"), nombre = c("Artigas", "Canelones")),
  clave = "codigo", valor = "nombre", completo = TRUE,
  alcance = "departamentos incluidos en el ejemplo"
)
padron
#> Referencial: referencial
#>   Filas: 2
#>   Clave: codigo
#>   Valores: nombre
#>   Completo: sí
#>   Alcance: departamentos incluidos en el ejemplo
```

## Lo que no se midió también es un resultado

La misma medición se lee distinto según el marco. Contamos los estados
de la cobertura de nuestra entrega frente a cada uno:

``` r

marcos <- list(
  propio = marco_entrega, AGESIC = marco_agesic(),
  ISO = marco_iso25012(), CEPAL = marco_cepal()
)
estados <- sapply(marcos, function(marco) {
  table(cobertura_analisis(perfil, medicion, modelo = marco)$estado)
})
estados
#>                  propio AGESIC ISO CEPAL
#> medida                4      3   0     0
#> no_declarada          1      9  15     6
#> no_aplica             0      4   0     0
#> fuera_de_alcance      0      1   0    13
```

Frente al marco propio, 4 factores medidos. Frente al de AGESIC, los 3
que nuestras métricas y el perfil alcanzan. Frente a ISO/IEC 25012 y a
CEA/CEPAL, ninguno: sus pares de dimensión y factor no coinciden con los
que declaran nuestras métricas, y la tabla muestra ese cero en lugar de
esconderlo. En el de CEA/CEPAL quedan 13 principios fuera de alcance y 6
sin declarar. La primera columna de cada cobertura nombra el marco, así
que una tabla exportada conserva el contexto.

## Referencias

[Batini C, Scannapieco M
(2016)](https://doi.org/10.1007/978-3-319-24106-7). *Data and
Information Quality: Dimensions, Principles and Techniques*. Springer.

[AGESIC
(2020)](https://www.gub.uy/agencia-gobierno-electronico-sociedad-informacion-conocimiento/).
*Marco de trabajo para la Gestión de la Calidad de Datos en Gobierno
Digital*, versión 1.6. Presidencia de la República, Uruguay.

[ISO/IEC (2008)](https://www.iso.org/standard/35736.html). *ISO/IEC
25012:2008 Software engineering — Software product Quality Requirements
and Evaluation (SQuaRE) — Data quality model*.

[Naciones Unidas
(2019)](https://unstats.un.org/unsd/methodology/dataquality/). *Manual
del marco nacional de aseguramiento de calidad en las estadísticas
oficiales*. Estudios en Métodos, serie M, N° 100, Nueva York.

[CEA/CEPAL (2022)](https://repositorio.cepal.org/handle/11362/47464).
*Guía para la implementación del marco de aseguramiento de la calidad
para procesos y productos estadísticos*. LC/CEA.11/19, Naciones Unidas,
Santiago.

## Resumen

- Un marco
  ([`marco_calidad()`](https://sebollin.github.io/lupa/reference/marco_calidad.md))
  es una taxonomía de dimensiones y factores: clasifica, no puntúa.
- Una métrica se especializa (con qué criterio) y se instancia (sobre
  qué tabla y columna);
  [`metrica()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md)
  declara una nueva con su propio método.
- `modelo(..., marco =)` liga las métricas al marco, y
  [`cobertura_analisis()`](https://sebollin.github.io/lupa/reference/cobertura_analisis.md)
  dice qué factores quedaron medidos y cómo cubrir los que faltan.
- El tablero publica la `orientacion` de cada valor; el índice existe
  sólo si declaramos los pesos.
- Los perfiles de madurez y las reglas se aplican a las medidas
  agregadas.

## Paso siguiente

En [9. Comparar entregas y seguir la
calidad](https://sebollin.github.io/lupa/articles/historico-y-deriva.md)
comparamos dos entregas de la misma tabla y seguimos la calidad medida a
lo largo del tiempo.
