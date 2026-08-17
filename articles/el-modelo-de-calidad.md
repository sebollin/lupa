# Un modelo de calidad declarable

`lupa` implementa un modelo de calidad de uso general. La arquitectura
sigue el linaje de dimensiones, factores y métricas descrito por Batini
y Scannapieco (2016), y permite que cada usuario declare la taxonomía
pertinente para su dominio. El marco de AGESIC viene incluido como una
instancia verificable, no como una restricción del núcleo.

``` r

library(lupa)
```

## Declarar dimensiones y factores

[`marco_calidad()`](https://sebollin.github.io/lupa/reference/marco_calidad.md)
recibe una tabla o una lista con nombres. El resultado es consultable y
puede validar que las métricas de un
[`modelo()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md)
pertenezcan a los factores declarados.

``` r

marco_propio <- marco_calidad("Marco de procedencia", list(
  Trazabilidad = c("Origen documentado", "Linaje reproducible"),
  Pertinencia = "Adecuación al uso"
))
marco_propio
#> 
#> ── Marco de procedencia ──
#> 
#> Dimensiones: 2
#> Factores: 3
#> Origen: usuario
as.data.frame(marco_propio)[, c("dimension", "factor")]
#>      dimension              factor
#> 1 Trazabilidad  Origen documentado
#> 2 Trazabilidad Linaje reproducible
#> 3  Pertinencia   Adecuación al uso
```

## Dimensión, factor y métrica son una taxonomía

Una dimensión reúne factores y un factor reúne métricas. Esa jerarquía
permite clasificar y navegar el modelo, pero el marco no define una
fórmula que produzca un “puntaje de Exactitud” ni un índice global.
Promediar factores sin un contrato adicional ocultaría prioridades y
correlaciones entre ellos.

Por eso `lupa` conserva `dimension` y `factor` en cada medida, pero
[`agregar()`](https://sebollin.github.io/lupa/reference/agregar.md) sólo
acepta las transiciones de granularidad declaradas.

``` r

granularidades()
#>    nivel           granularidad           relacional implementada
#> 1      1      instanciaAtributo                celda         TRUE
#> 2      2               atributo              columna         TRUE
#> 3      3      conjuntoAtributos conjunto de columnas         TRUE
#> 4      4       instanciaEntidad                tupla         TRUE
#> 5      5                entidad                tabla         TRUE
#> 6      6      conjuntoEntidades   conjunto de tablas         TRUE
#> 7      7              coleccion        base de datos        FALSE
#> 8      8    conjuntoColecciones                 <NA>        FALSE
#> 9      9           organizacion                 <NA>        FALSE
#> 10    10 conjuntoOrganizaciones                 <NA>        FALSE
transiciones_granularidad()
#>              origen                destino                fuente
#> 1 instanciaAtributo               atributo                 marco
#> 2 instanciaAtributo       instanciaEntidad extension_documentada
#> 3  instanciaEntidad                entidad                 marco
#> 4          atributo                entidad                 marco
#> 5           entidad      conjuntoEntidades                 marco
#> 6           entidad              coleccion                 marco
#> 7         coleccion    conjuntoColecciones                 marco
#> 8         coleccion           organizacion                 marco
#> 9      organizacion conjuntoOrganizaciones                 marco
```

Una celda puede agregarse hacia su columna o hacia su fila. Esas
direcciones son ortogonales, no peldaños de una escala lineal.

## Tres niveles de métrica

Una métrica genérica declara semántica, granularidad, tipo de resultado
y propiedades. Especializar fija propiedades reutilizables; instanciar
liga esa especialización a objetos concretos.

``` r

nucleo <- metricas_nucleo()
no_nulo <- nucleo$NoNulo
no_nulo_personas <- especializar(
  no_nulo, nombre_especifico = "NoNuloDocumento"
)
documento <- instanciar(no_nulo_personas, "personas", "documento")
documento$declaracion[c("nombre", "granularidad", "tipo_resultado")]
#> $nombre
#> [1] "NoNulo"
#> 
#> $granularidad
#> [1] "instanciaAtributo"
#> 
#> $tipo_resultado
#> [1] "booleano"
```

La separación permite reutilizar la misma definición sobre entregas y
columnas distintas.

### Una métrica propia de punta a punta

El gancho `metodo` recibe una lista con nombre de tablas y la instancia
que se está midiendo. Devuelve una fila por objeto medido con las
columnas `resultado`, `entidad`, `atributo`, `fila` y `objeto`. Este
ejemplo declara una métrica ajena a los catálogos incluidos y muestra
las dos llamadas de la fábrica: la primera especializa y la segunda
instancia.

``` r

metodo_origen <- function(tablas, instancia) {
  x <- tablas[[instancia$entidad]][[instancia$atributos]]
  data.frame(
    resultado = !is.na(x) & nzchar(x),
    entidad = instancia$entidad,
    atributo = instancia$atributos,
    fila = seq_along(x),
    objeto = paste0(instancia$entidad, "$", instancia$atributos,
                    "[", seq_along(x), "]")
  )
}

OrigenDeclarado <- metrica(
  nombre = "OrigenDeclarado",
  semantica = "Indica si cada registro declara su sistema de origen.",
  granularidad = "instanciaAtributo",
  tipo_resultado = "booleano",
  dimension = "Trazabilidad",
  factor = "Origen documentado",
  metodo = metodo_origen
)
origen <- OrigenDeclarado()(entidad = "entrega", atributos = "origen")
datos_origen <- data.frame(origen = c("sistema_a", "", "sistema_b"))
medir(modelo(origen), datos_origen)[, c("objeto_medible", "resultado")]
#>      objeto_medible resultado
#> 1 entrega$origen[1]         1
#> 2 entrega$origen[2]         0
#> 3 entrega$origen[3]         1
```

Cuando una métrica declara propiedades,
[`propiedades_metrica()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md)
permite consultarlas antes de llamar a
[`especializar()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md).

``` r

propiedades_metrica(nucleo$Formato)
#>           propiedad configurada
#> 1 expresion_regular       FALSE
#> 2       diccionario       FALSE
#> 3         validador       FALSE
```

### Validadores y packs territoriales

`Formato` acepta cualquier función vectorizada que devuelva un lógico
por valor. El núcleo incluye validadores internacionales de códigos ISO,
correo, Luhn y módulo 97. Uruguay es un pack territorial de referencia,
no una rama especial del motor.

``` r

internacionales <- validadores_internacionales()
uruguay <- validadores_uruguay()
internacionales$iso4217(c("UYU", "CLP", "ZZZ"))
#> [1]  TRUE  TRUE FALSE
uruguay$cedula(c("1.234.567-2", "1.234.567-3"))
#> [1]  TRUE FALSE

cedula_valida <- especializar(
  nucleo$Formato, "CedulaValida", validador = uruguay$cedula
)
medir(
  modelo(cedula_valida("personas", "documento")),
  data.frame(documento = c("1.234.567-2", "1.234.567-3"))
)[, c("objeto_medible", "resultado")]
#>          objeto_medible resultado
#> 1 personas$documento[1]         1
#> 2 personas$documento[2]         0
```

Un proyecto de otro país construye un pack con
[`pack_validadores()`](https://sebollin.github.io/lupa/reference/pack_validadores.md)
y mantiene su función en su propio paquete o script. No necesita
registrar nombres ni modificar `lupa`; la ayuda de
[`pack_validadores()`](https://sebollin.github.io/lupa/reference/pack_validadores.md)
incluye un ejemplo completo con un RUT chileno.

## Medir y agregar

``` r

datos <- data.frame(documento = c("1", "2", NA, "4"))
medidas <- medir(
  modelo(documento), datos,
  id_medicion = "enero",
  fecha = as.POSIXct("2026-01-31", tz = "UTC")
)
medidas[, c("objeto_medible", "resultado", "granularidad")]
#>          objeto_medible resultado      granularidad
#> 1 personas$documento[1]         1 instanciaAtributo
#> 2 personas$documento[2]         1 instanciaAtributo
#> 3 personas$documento[3]         0 instanciaAtributo
#> 4 personas$documento[4]         1 instanciaAtributo

agregar(medidas, "atributo", "ratio")[, c(
  "objeto_medible", "resultado", "agregacion"
)]
#>       objeto_medible resultado agregacion
#> 1 personas$documento      0.75      ratio
```

## Tablero, orientación e índice

[`tablero_calidad()`](https://sebollin.github.io/lupa/reference/tablero_calidad.md)
resume la medición y conserva su marco y su cobertura. La columna
`orientacion` dice cómo leer cada proporción: `conformidad` significa
que un valor mayor es mejor, `defecto` que un valor menor es mejor y
`no_aplica` que la métrica no es una proporción combinable. Un `0.0063`
de `EntidadDuplicada` expresa poco defecto; un `0.9992` de `NoNulo`
expresa mucha conformidad. Los números no se leen igual aunque estén en
la misma escala.

``` r

modelo_tablero <- modelo(list(
  instanciar(especializar(nucleo$NoNulo), "padron", "codigo"),
  instanciar(especializar(nucleo$EntidadDuplicada), "padron")
))
datos_tablero <- data.frame(codigo = c("A", "B", "B", NA))
medidas_tablero <- medir(
  modelo_tablero, datos_tablero, id_medicion = "tablero-ejemplo"
)
tablero <- tablero_calidad(medidas_tablero)
tablero[, c(
  "dimension", "factor", "metrica", "valor", "orientacion", "universo"
)]
#> 
#> ── Tablero de calidad ────────────────────────────────────────────────────────────────────
#>    dimension         factor          metrica valor orientacion universo
#>  Completitud       Densidad           NoNulo  0.75 conformidad   celdas
#>     Unicidad No-duplicación EntidadDuplicada  0.50     defecto    filas
attr(tablero, "alcance")
#>   factores_marco factores_medidos sin_metrica_declarada no_aplican fuera_de_alcance
#> 1             17                2                    10          0                5
```

Sin `pesos`, no hay número: `indice_calidad(tablero)` devuelve el
tablero. Los pesos no pertenecen a `lupa`; los declara el usuario según
la decisión del proyecto. El índice conserva además la cobertura que
acompaña al tablero.

``` r

indice_sin_pesos <- indice_calidad(tablero)
indice_sin_pesos
#> 
#> ── Tablero de calidad ────────────────────────────────────────────────────────────────────
#>       componente   dimension         factor          metrica  objeto valor orientacion
#>  componente-0001 Completitud       Densidad           NoNulo  codigo  0.75 conformidad
#>  componente-0002    Unicidad No-duplicación EntidadDuplicada (tabla)  0.50     defecto
#>  agregacion umbral universo
#>       ratio     NA   celdas
#>       ratio     NA    filas
#> 
#> ── Alcance del marco ──
#> 
#>  factores_marco factores_medidos sin_metrica_declarada no_aplican fuera_de_alcance
#>              17                2                    10          0                5

# Pesos elegidos para este ejemplo; no son valores predeterminados de lupa.
pesos_ejemplo <- c(Completitud = 0.6, Unicidad = 0.4)
indice <- indice_calidad(tablero, pesos = pesos_ejemplo)
indice
#> ── Índice de calidad declarado ───────────────────────────────────────────────────────────
#> Valor: 0.65
#> 
#> ── Cobertura del índice ──
#> 
#>  factores_marco factores_en_indice                                          factores
#>              17                  2 Completitud / Densidad; Unicidad / No-duplicación
#> ── Dimensiones, pesos y aportes ──
#>    dimension valor peso aporte                combinacion_interna
#>  Completitud  0.75  0.6   0.45 un componente; sin paso intermedio
#>     Unicidad  0.50  0.4   0.20 un componente; sin paso intermedio
#> ── Componentes de defecto invertidos ──
#>       componente dimension         factor          metrica  objeto valor orientacion
#>  componente-0002  Unicidad No-duplicación EntidadDuplicada (tabla)   0.5     defecto
#>  agregacion umbral universo transformacion valor_indice peso_interno
#>       ratio     NA    filas      1 - valor          0.5            1
#> ℹ Dentro de cada dimensión se usa un solo componente o los pesos_internos declarados; entre dimensiones se usan `pesos`.
#> ! Los componentes salen de universos distintos (por ejemplo, celdas, valores con formato reconocible y filas). El índice sólo los combina porque quien lo solicitó declaró los pesos.
indice$cobertura
#>   factores_marco factores_en_indice                                          factores
#> 1             17                  2 Completitud / Densidad; Unicidad / No-duplicación
```

Para las métricas con orientación `defecto`, el índice usa `1 - valor`;
las de `no_aplica` quedan fuera y se conservan como exclusiones. El
índice no promedia por su cuenta dos componentes de una misma dimensión:
exige que el usuario declare `pesos_internos` para esa combinación.

``` r

modelo_doble <- modelo(list(
  instanciar(
    especializar(nucleo$NoNulo), "padron", "codigo",
    nombre_instancia = "no-nulo-codigo"
  ),
  instanciar(
    especializar(nucleo$NoNulo), "padron", "nombre",
    nombre_instancia = "no-nulo-nombre"
  )
))
medidas_dobles <- medir(
  modelo_doble,
  data.frame(codigo = c("A", "B", NA), nombre = c("x", NA, "z")),
  id_medicion = "interno-ejemplo"
)
tablero_doble <- tablero_calidad(medidas_dobles)
tryCatch(
  indice_calidad(tablero_doble, pesos = c(Completitud = 1)),
  error = function(e) conditionMessage(e)
)
#> [1] "Las dimensiones con varios componentes requieren `pesos_internos`: Completitud."
```

Una advertencia acompaña al índice cuando sus componentes provienen de
universos distintos, como celdas y filas. La cobertura y la advertencia
no resuelven esa diferencia: el índice sólo los combina porque el
usuario declaró los pesos.

Las cuatro agregaciones implementadas son:

- `ratio`, para resultados booleanos;
- `ratio_umbral`, para resultados reales;
- `promedio`;
- `promedio_ponderado`, con pesos en `[0, 1]` que suman uno por destino.

La función valida el tipo de resultado y la transición. No agrega hacia
factor, dimensión o modelo. Los resultados no acotados que el marco usa
para `ErrorEstandar` y actualidad conservan su unidad y no entran en
estas cuatro agregaciones.

## Evaluar no es volver a medir

Una regla expresa una condición sobre resultados. La evaluación de una
regla es la proporción que la cumple; la evaluación de un perfil es la
media simple de sus reglas. Los perfiles incluidos de AGESIC usan
condiciones estrictas `> 0.5`, `> 0.7` y `> 0.9`; otra familia se
construye con umbrales con nombres.

``` r

medida_atributo <- agregar(medidas, "atributo", "ratio")
madurez <- perfiles_madurez()
evaluar(medida_atributo, madurez$Intermedio)$perfiles
#>   id_medicion      fecha     perfil n_reglas resultado
#> 1       enero 2026-01-31 Intermedio        1         1
names(perfiles_madurez(umbrales = c(Inicial = 0.4, Consolidado = 0.85)))
#> [1] "Inicial"     "Consolidado"
```

El `id_medicion` y la fecha permanecen en las medidas para comparar
corridas y monitorear deriva.

## Marcos incluidos

[`marco_iso25012()`](https://sebollin.github.io/lupa/reference/marco_calidad.md)
ofrece las quince características de ISO/IEC 25012:2008 como otra
taxonomía disponible. La norma distingue características inherentes,
dependientes del sistema y aplicables desde ambas perspectivas. `lupa`
usa esos tres grupos como dimensiones operativas y las características
como factores; es una adaptación para la interfaz dimensión–factor, no
la afirmación de que la norma defina esa jerarquía. Sus descripciones
están redactadas para el paquete y no reproducen el texto normativo.

``` r

iso <- marco_iso25012()
table(as.data.frame(iso)$dimension)
#> 
#>             Dependiente del sistema                           Inherente 
#>                                   3                                   5 
#> Inherente y dependiente del sistema 
#>                                   7
head(as.data.frame(iso)[, c("dimension", "factor", "descripcion")])
#>                             dimension        factor
#> 1                           Inherente     Exactitud
#> 2                           Inherente   Completitud
#> 3                           Inherente  Consistencia
#> 4                           Inherente  Credibilidad
#> 5                           Inherente    Actualidad
#> 6 Inherente y dependiente del sistema Accesibilidad
#>                                                                                      descripcion
#> 1 Considera si los datos representan correctamente los hechos o valores que pretenden describir.
#> 2         Considera si están presentes los valores y registros necesarios para el uso declarado.
#> 3                       Revisa que los datos no se contradigan entre sí ni con reglas acordadas.
#> 4                    Expresa la confianza respaldada por el origen y las evidencias disponibles.
#> 5                    Considera si los datos conservan vigencia para el momento y uso declarados.
#> 6              Considera si las personas o procesos autorizados pueden obtener y usar los datos.
```

### El marco y el catálogo de AGESIC

[`marco_agesic()`](https://sebollin.github.io/lupa/reference/marco_calidad.md)
devuelve los 17 factores usados por omisión en la cobertura.
[`catalogo_agesic()`](https://sebollin.github.io/lupa/reference/catalogo_agesic.md)
mantiene por separado las 49 entradas, incluidas las que se obtienen por
agregación o requieren insumos externos.

``` r

catalogo <- catalogo_agesic()
marco_agesic()
#> 
#> ── Marco de calidad de datos de AGESIC ──
#> 
#> Dimensiones: 5
#> Factores: 17
#> Origen: AGESIC 2020, versión 1.6
as.data.frame(table(catalogo$estado, catalogo$motivo))
#>                Var1                   Var2 Freq
#> 1      implementada     semantica_completa    6
#> 2    via_agregacion     semantica_completa    0
#> 3         pendiente     semantica_completa    0
#> 4  fuera_de_alcance     semantica_completa    0
#> 5      implementada      semantica_parcial    1
#> 6    via_agregacion      semantica_parcial    0
#> 7         pendiente      semantica_parcial    0
#> 8  fuera_de_alcance      semantica_parcial    0
#> 9      implementada             agregacion    0
#> 10   via_agregacion             agregacion    9
#> 11        pendiente             agregacion    0
#> 12 fuera_de_alcance             agregacion    0
#> 13     implementada   requiere_referencial    3
#> 14   via_agregacion   requiere_referencial    0
#> 15        pendiente   requiere_referencial    0
#> 16 fuera_de_alcance   requiere_referencial    0
#> 17     implementada requiere_configuracion   19
#> 18   via_agregacion requiere_configuracion    0
#> 19        pendiente requiere_configuracion    0
#> 20 fuera_de_alcance requiere_configuracion    0
#> 21     implementada        motor_pendiente    0
#> 22   via_agregacion        motor_pendiente    0
#> 23        pendiente        motor_pendiente    1
#> 24 fuera_de_alcance        motor_pendiente    0
#> 25     implementada       decision_alcance    0
#> 26   via_agregacion       decision_alcance    0
#> 27        pendiente       decision_alcance    0
#> 28 fuera_de_alcance       decision_alcance   10
catalogo[catalogo$estado == "via_agregacion", c(
  "metrica_agesic", "metrica_lupa", "implementacion"
)]
#>                     metrica_agesic                metrica_lupa
#> 3        RatioCorrectitudSemFuerte        CorrectitudSemFuerte
#> 4         RatioCorrectitudSemDébil         CorrectitudSemDebil
#> 21     RatioIntegridadIntraEntidad ReglaIntegridadIntraEntidad
#> 30                    RatioNoNulos                      NoNulo
#> 31          RatioDensidadPonderada           DensidadPonderada
#> 37          RatioAtributoDuplicado           AtributoDuplicado
#> 38 RatioConjuntoAtributosDuplicado  ConjuntoAtributosDuplicado
#> 39        RatioEntidadesDuplicadas            EntidadDuplicada
#> 41      RatioEntidadContradictoria       EntidadContradictoria
#>                                       implementacion
#> 3                    agregar(m, "atributo", "ratio")
#> 4                    agregar(m, "atributo", "ratio")
#> 21                    agregar(m, "entidad", "ratio")
#> 30                   agregar(m, "atributo", "ratio")
#> 31 agregar(m, "entidad", "ratio_umbral", umbral = u)
#> 37                   agregar(m, "atributo", "ratio")
#> 38                    agregar(m, "entidad", "ratio")
#> 39                    agregar(m, "entidad", "ratio")
#> 41                   agregar(m, "atributo", "ratio")
```

`estado` dice si la entrada está implementada, se obtiene por
agregación, está pendiente o queda fuera del alcance tabular. `motivo`
explica la causa: por ejemplo, distingue un motor pendiente de una
implementación disponible que necesita un referencial o una
configuración experta. La observación de cada fila explicita el contrato
concreto y las implementaciones parciales.

Un diccionario enumera valores sintácticamente válidos. Un referencial
vincula claves y valores externos, y sólo permite medir cobertura cuando
declara de qué universo es completo.

``` r

padron <- referencial(
  data.frame(codigo = c("01", "02"), nombre = c("Artigas", "Canelones")),
  clave = "codigo", valor = "nombre", completo = TRUE,
  alcance = "departamentos incluidos en el ejemplo"
)
names(metricas_referencial())
#> [1] "CorrectitudSemFuerte" "CorrectitudSemDebil"  "RatioCobertura"
padron
#> Referencial: referencial 
#>   Filas: 2 
#>   Clave: codigo 
#>   Valores: nombre 
#>   Completo: sí 
#>   Alcance: departamentos incluidos en el ejemplo
```

La tabla del catálogo es la respuesta verificable a “qué implementa el
paquete”; las entradas fuera de alcance no se presentan como resueltas.

## Lo que no se midió también es un resultado

Un perfil sin hallazgos no demuestra que todos los factores hayan sido
evaluados.
[`cobertura_analisis()`](https://sebollin.github.io/lupa/reference/cobertura_analisis.md)
separa lo medido de lo no declarado, lo que no aplica a los tipos
presentes y lo que permanece fuera de alcance.

``` r

perfil <- perfilar(datos_administrativos, analizar_dependencias = FALSE)
cobertura_analisis(perfil)
#>                                  marco    dimension                        factor
#> 1  Marco de calidad de datos de AGESIC    Exactitud         Correctitud semántica
#> 2  Marco de calidad de datos de AGESIC    Exactitud        Correctitud sintáctica
#> 3  Marco de calidad de datos de AGESIC    Exactitud                     Precisión
#> 4  Marco de calidad de datos de AGESIC    Exactitud Exactitud posicional absoluta
#> 5  Marco de calidad de datos de AGESIC    Exactitud Exactitud posicional relativa
#> 6  Marco de calidad de datos de AGESIC    Exactitud                     Fidelidad
#> 7  Marco de calidad de datos de AGESIC Consistencia      Integridad inter-entidad
#> 8  Marco de calidad de datos de AGESIC Consistencia      Integridad intra-entidad
#> 9  Marco de calidad de datos de AGESIC Consistencia         Integridad de dominio
#> 10 Marco de calidad de datos de AGESIC Consistencia       Consistencia topológica
#> 11 Marco de calidad de datos de AGESIC  Completitud                     Cobertura
#> 12 Marco de calidad de datos de AGESIC  Completitud                      Densidad
#> 13 Marco de calidad de datos de AGESIC  Completitud                      Comisión
#> 14 Marco de calidad de datos de AGESIC     Unicidad                No-duplicación
#> 15 Marco de calidad de datos de AGESIC     Unicidad              No-contradicción
#> 16 Marco de calidad de datos de AGESIC     Frescura                    Actualidad
#> 17 Marco de calidad de datos de AGESIC     Frescura                   Oportunidad
#>              estado
#> 1      no_declarada
#> 2      no_declarada
#> 3      no_declarada
#> 4         no_aplica
#> 5         no_aplica
#> 6  fuera_de_alcance
#> 7      no_declarada
#> 8      no_declarada
#> 9      no_declarada
#> 10        no_aplica
#> 11     no_declarada
#> 12           medida
#> 13        no_aplica
#> 14           medida
#> 15     no_declarada
#> 16     no_declarada
#> 17     no_declarada
#>                                                                             motivo
#> 1     El perfil describe evidencia, pero no recibió un requisito para este factor.
#> 2     El perfil describe evidencia, pero no recibió un requisito para este factor.
#> 3     El perfil describe evidencia, pero no recibió un requisito para este factor.
#> 4                                       No se identificaron columnas de geometría.
#> 5                                       No se identificaron columnas de geometría.
#> 6  Las métricas del factor requieren capacidades no implementadas en esta versión.
#> 7     El perfil describe evidencia, pero no recibió un requisito para este factor.
#> 8     El perfil describe evidencia, pero no recibió un requisito para este factor.
#> 9     El perfil describe evidencia, pero no recibió un requisito para este factor.
#> 10                                      No se identificaron columnas de geometría.
#> 11    El perfil describe evidencia, pero no recibió un requisito para este factor.
#> 12            El perfil contó ausentes reales y disfrazados en todas las columnas.
#> 13                                      No se identificaron columnas de geometría.
#> 14             El perfil examinó duplicación de valores, columnas y filas exactas.
#> 15    El perfil describe evidencia, pero no recibió un requisito para este factor.
#> 16    El perfil describe evidencia, pero no recibió un requisito para este factor.
#> 17    El perfil describe evidencia, pero no recibió un requisito para este factor.
#>                                                                 como_resolverlo
#> 1                      Crear referencial() e instanciar metricas_referencial().
#> 2                  Especializar Formato con expresión, diccionario o validador.
#> 3                    Declarar escala() o medir ErrorEstandar sobre el atributo.
#> 4  Requiere un backend o referencial especializado que no integra esta versión.
#> 5  Requiere un backend o referencial especializado que no integra esta versión.
#> 6  Requiere un backend o referencial especializado que no integra esta versión.
#> 7                Instanciar ReglaIntegridadInterEntidad con claves confirmadas.
#> 8               Confirmar una regla y especializar ReglaIntegridadIntraEntidad.
#> 9               Proveer un dominio a ValoresPosiblesPorExtension o Comprension.
#> 10 Requiere un backend o referencial especializado que no integra esta versión.
#> 11                Crear un referencial(completo = TRUE) y medir RatioCobertura.
#> 12                                             Usar NoNulo o DensidadPonderada.
#> 13 Requiere un backend o referencial especializado que no integra esta versión.
#> 14                     Usar las métricas de duplicación o el perfil automático.
#> 15 Requiere un backend o referencial especializado que no integra esta versión.
#> 16           Declarar vigencia() y medir DesactualizacionPorFecha o PorCambios.
#> 17                        Declarar vigencia() y medir una métrica Oportunidad*.
```

Los contratos temporales y de precisión se declaran con
[`vigencia()`](https://sebollin.github.io/lupa/reference/contratos_medicion.md)
y
[`escala()`](https://sebollin.github.io/lupa/reference/contratos_medicion.md):
el paquete no intenta aprenderlos de una sola entrega.

Para otro marco, la misma función informa exclusivamente sus dimensiones
y factores:

``` r

cobertura_analisis(perfil, modelo = marco_propio)
#>                  marco    dimension              factor       estado
#> 1 Marco de procedencia Trazabilidad  Origen documentado no_declarada
#> 2 Marco de procedencia Trazabilidad Linaje reproducible no_declarada
#> 3 Marco de procedencia  Pertinencia   Adecuación al uso no_declarada
#>                                                                         motivo
#> 1 El perfil describe evidencia, pero no recibió un requisito para este factor.
#> 2 El perfil describe evidencia, pero no recibió un requisito para este factor.
#> 3 El perfil describe evidencia, pero no recibió un requisito para este factor.
#>                                     como_resolverlo
#> 1 Declarar e instanciar una métrica de este factor.
#> 2 Declarar e instanciar una métrica de este factor.
#> 3 Declarar e instanciar una métrica de este factor.
```

La primera columna de cada salida identifica el marco activo, por lo que
una tabla exportada conserva el contexto de la evaluación.

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
