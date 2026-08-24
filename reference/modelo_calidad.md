# Construir métricas y modelos de calidad

`metrica()` declara una métrica genérica y devuelve una fábrica de
closures. `especializar()` fija sus propiedades de configuración; la
closure específica resultante puede instanciarse sobre distintas
columnas o tablas. `instanciar()` liga la métrica a objetos concretos y
materializa el método de medición. `modelo()` reúne métricas
instanciadas sin calcular un índice global.

## Usage

``` r
metrica(
  nombre,
  semantica,
  granularidad,
  tipo_resultado,
  propiedades = character(),
  dimension = NA_character_,
  factor = NA_character_,
  metodo = NULL,
  validar_propiedades = NULL,
  orientacion = if (tipo_resultado %in% c("booleano", "real")) "conformidad" else
    "no_aplica"
)

especializar(metrica, nombre_especifico = NULL, ...)

instanciar(
  metrica_especifica,
  entidad,
  atributos = character(),
  nombre_instancia = NULL,
  metodo = NULL,
  referencial = NULL
)

propiedades_metrica(x)

modelo(..., marco = NULL)

metricas_nucleo()
```

## Arguments

- nombre:

  Nombre estable y legible.

- semantica:

  Descripción de lo que mide la métrica.

- granularidad:

  Uno de los niveles de ontología devueltos por
  [`granularidades()`](https://sebollin.github.io/lupa/reference/granularidades.md)
  o su alias en la columna `relacional`. Los aliases se normalizan al
  nombre de ontología antes de guardarse en la métrica.

- tipo_resultado:

  `"booleano"`, `"real"` en `[0, 1]`, `"numero_real"`, `"entero"` no
  negativo o `"duracion"` no negativa. Los tres últimos conservan
  resultados no acotados del catálogo y no admiten las cuatro
  agregaciones normalizadas.

- propiedades:

  Nombres de las propiedades que fija `especializar()`.

- dimension, factor:

  Metadatos taxonómicos; no se usan para calcular puntuaciones.

- metodo:

  Método predeterminado opcional. Es una función de `tablas` e
  `instancia` que cumple el contrato descrito en **Contrato de
  `metodo`**.

- validar_propiedades:

  Función opcional que recibe la lista con nombre enviada a
  `especializar()` y debe devolver otra lista con nombre formada sólo
  por propiedades declaradas. Puede validar alternativas, completar
  valores predeterminados y normalizar la configuración. Si es `NULL`,
  todas las propiedades declaradas son obligatorias y no se admiten
  otras.

- orientacion:

  Sentido de lectura del resultado: `"conformidad"` indica que un valor
  mayor es mejor; `"defecto"`, que un valor menor es mejor; y
  `"no_aplica"`, que la métrica no es una proporción interpretable en
  esos términos. Es un vocabulario cerrado. Las métricas booleanas deben
  usar una de las dos primeras y los resultados no acotados deben usar
  la última.

- metrica:

  Objeto de clase `metrica_generica`.

- nombre_especifico:

  Nombre de la especialización. Si se omite, conserva el nombre
  genérico.

- ...:

  En `especializar()`, propiedades con nombre de las declaradas en
  `metrica(propiedades = )`; consúltelas con `propiedades_metrica()`. En
  `modelo()`, métricas instanciadas o una única lista que las contenga.

- metrica_especifica:

  Objeto de clase `metrica_especifica`.

- entidad:

  Nombres de las tablas ligadas, en el orden que espera el método.

- atributos:

  Nombres de las columnas ligadas, en el mismo orden.

- nombre_instancia:

  Nombre de la instancia. Si se omite, se deriva de la especialización y
  los objetos ligados.

- referencial:

  Objeto opcional creado por
  [`referencial()`](https://sebollin.github.io/lupa/reference/referencial.md).
  Se conserva en la instancia sin modificarlo: `instanciar()` no supone
  que toda métrica lo use. El `metodo` debe leerlo y validar el contrato
  que necesite; las métricas de
  [`metricas_referencial()`](https://sebollin.github.io/lupa/reference/metricas_referencial.md)
  hacen esa validación al medir.

- x:

  Métrica genérica, específica o instanciada.

- marco:

  Objeto opcional creado por
  [`marco_calidad()`](https://sebollin.github.io/lupa/reference/marco_calidad.md).
  Cuando se provee, todas las métricas instanciadas deben pertenecer a
  uno de sus pares dimensión-factor.

## Value

`metrica()` y `especializar()` devuelven closures S3; `instanciar()`
devuelve una `metrica_instanciada`; `modelo()` devuelve un
`modelo_calidad`; `metricas_nucleo()` devuelve una lista de métricas
genéricas. `propiedades_metrica()` devuelve un data frame con las
propiedades declaradas y si ya fueron configuradas.

## Details

`metricas_nucleo()` devuelve veintidós métricas automatizables una vez
declaradas sus propiedades;
[`escala()`](https://sebollin.github.io/lupa/reference/contratos_medicion.md)
y
[`vigencia()`](https://sebollin.github.io/lupa/reference/contratos_medicion.md)
hacen explícitos los insumos expertos que algunas necesitan.
[`metricas_referencial()`](https://sebollin.github.io/lupa/reference/metricas_referencial.md)
aporta por separado las tres métricas que consumen un padrón tabular.
Consulte
[`catalogo_agesic()`](https://sebollin.github.io/lupa/reference/catalogo_agesic.md)
para la correspondencia completa.

En `ReglaIntegridadInterEntidad`, `entidad` y `atributos` se ligan como
`c(referencia, dependiente)` y `c(clave_primaria, clave_foranea)`. Esta
implementación calcula cobertura PK/FK como resultado real y sólo cubre
una parte de la genérica del marco, que declara granularidad
`conjuntoEntidades`, resultado booleano y admite además una expresión
condicional.
[`catalogo_agesic()`](https://sebollin.github.io/lupa/reference/catalogo_agesic.md)
deja visible esa cobertura parcial. Pese a su nombre, `ErrorEstandar`
sigue literalmente la semántica de la tabla 16.5 del marco y devuelve la
desviación estándar muestral sin normalizar; exige al menos dos valores
numéricos válidos. Por eso declara `tipo_resultado = "numero_real"` y no
admite
[`agregar()`](https://sebollin.github.io/lupa/reference/agregar.md).

`Formato` acepta exactamente una de las propiedades `expresion_regular`,
`diccionario` o `validador`. Esta última permite conectar validadores
externos sin incorporarlos como dependencias. Tanto `Formato` como
`ValoresPosiblesPorExtension` omiten los valores `NA`: un ausente no
genera una medida en esas métricas y, por lo tanto, tampoco integra el
denominador de sus agregaciones. La completitud se mide por separado con
`NoNulo`; así un mismo ausente no se penaliza en dos factores. `NoNulo`
acepta el vector opcional `valores_nulos` para aplicar de forma
deliberada el diccionario de nulos que contempla el marco; sin
configurarlo, sólo considera los `NA` reales.
`ValoresPosiblesPorComprension` sigue la misma convención y acepta un
`predicado` o un rango definido por `minimo`, `maximo` e `inclusivo`.

Las métricas de duplicación marcan **todas** las apariciones que
participan en un grupo repetido, no sólo la segunda y siguientes.
`AtributoDuplicado` omite ausentes. `ConjuntoAtributosDuplicado` compara
las columnas ligadas y `EntidadDuplicada` compara la fila completa
cuando se instancia sin atributos. Si se ligan atributos de clave, sigue
la semántica del marco: marca filas con la misma clave cuyos demás
valores son iguales o ausentes en alguna de las dos. En las
comparaciones exactas, los `NA` forman parte de la combinación
comparada.

`EntidadContradictoria` compara los valores distintos de un atributo con
la misma normalización y medida de similitud que
[`detectar_duplicados_aproximados()`](https://sebollin.github.io/lupa/reference/detectar_duplicados_aproximados.md).
Marca las filas cuyo valor tiene un vecino por debajo de `umbral`, pero
no modifica datos ni propone una limpieza. Su alcance se conserva en el
atributo `alcance_metricas` del resultado de
[`medir()`](https://sebollin.github.io/lupa/reference/medir.md): informa
los valores distintos comparados, los pares evaluados y los que quedaron
bajo el umbral. `max_valores` permite acotar el vocabulario; cuando se
alcanza, el alcance declara el prefijo evaluado y los valores que
quedaron fuera.

`DesactualizacionPorFormato` devuelve `TRUE` cuando el valor **no**
cumple el formato vigente. Conforme a las tablas 16.29 y 16.30 del
marco, `OportunidadAtributoPorFecha` indica si la fecha es anterior o
igual a `fecha_limite`, y `OportunidadAtributoPorIntervalo` si pertenece
al intervalo cerrado `[inicio_vigencia, fin_vigencia]`; ambas son
booleanas.

`GradoOportunidadAtributoPorFecha` y
`GradoOportunidadAtributoPorIntervalo` son extensiones propias basadas
en el curso CPAP, no entradas adicionales del catálogo AGESIC. Conservan
la fórmula continua `max(0, min(1, 1 - (t1 - t2) / (t3 - t2)))` para
expresar cuánto margen de utilidad queda. Exigen que `t3` sea posterior
a `t2`; un intervalo de duración cero se rechaza porque no define el
cociente. Todas estas métricas omiten los valores `NA`: su ausencia
corresponde a completitud y no genera una segunda medida de
incumplimiento.

**Desviación documentada del marco:** en `DensidadPonderada`, un
atributo más crítico recibe un coeficiente mayor y, si falta, produce
una penalización mayor. El texto del marco indica acercar a cero el
coeficiente de mayor gravedad, lo que penalizaría menos el ausente
crítico y contradice el sentido de la ponderación. Los coeficientes
deben estar en `[0, 1]` y sumar 1.

`tipo_resultado` es el contrato canónico que consultan las agregaciones.
Las unidades no forman parte de este núcleo porque el marco presenta
ambas nociones de forma inconsistente y sólo el tipo permite validar las
fórmulas. El argumento referencial se conserva sin transformación dentro
de la instancia;
[`metricas_referencial()`](https://sebollin.github.io/lupa/reference/metricas_referencial.md)
declara y valida el contrato específico de correctitud semántica y
cobertura.

## Contrato de `metodo`

El método tiene la firma `function(tablas, instancia)`. `tablas` es una
lista con nombre de data frames, incluso cuando
[`medir()`](https://sebollin.github.io/lupa/reference/medir.md) recibió
una sola tabla. `instancia` expone `entidad`, `atributos`,
`configuracion`, `referencial` y `declaracion`; el método decide cómo
interpretar esos vínculos.

Debe devolver un data frame con exactamente una observación por objeto
medido y, como mínimo, estas columnas:

- `resultado`: valor medido. Debe respetar `tipo_resultado`: lógicos sin
  `NA` para `"booleano"`; números finitos en `[0, 1]` para `"real"`;
  números finitos para `"numero_real"`; enteros o duraciones no
  negativos para los tipos homónimos;

- `entidad`: nombre de la tabla a la que corresponde la medida;

- `atributo`: nombre de la columna o `NA_character_` cuando no
  corresponde;

- `fila`: posición de la fila o `NA_integer_` para resultados agregados;

- `objeto`: etiqueta legible y estable del objeto medido.

Las columnas adicionales se descartan. Un `metodo` pasado a
`instanciar()` reemplaza el predeterminado sólo para esa instancia. El
ejemplo ejecutable muestra la cadena genérica → específica → instanciada
completa.

## Contrato de propiedades

`propiedades` declara los nombres que puede recibir `especializar()`.
Sin `validar_propiedades`, todas son obligatorias. Con un validador
propio, éste recibe la lista `configuracion`, debe rechazar
combinaciones inválidas y puede devolver sólo el subconjunto activo o
añadir valores predeterminados, pero no propiedades ajenas a la
declaración. `propiedades_metrica()` permite consultar los nombres
aceptados sin inspeccionar atributos internos de las closures.

## References

[AGESIC
(2020)](https://www.gub.uy/agencia-gobierno-electronico-sociedad-informacion-conocimiento/).
*Marco de trabajo para la Gestión de la Calidad de Datos en Gobierno
Digital*, versión 1.6, Presidencia de la República, Uruguay.

Curso CPAP, material *Evaluación de Calidad*: fórmula continua de
oportunidad implementada bajo los nombres `GradoOportunidadAtributo*`.

## See also

[`referencial()`](https://sebollin.github.io/lupa/reference/referencial.md),
[`agregar()`](https://sebollin.github.io/lupa/reference/agregar.md),
[`medir()`](https://sebollin.github.io/lupa/reference/medir.md),
[`proponer_modelo()`](https://sebollin.github.io/lupa/reference/proponer_modelo.md)

## Examples

``` r
nucleo <- metricas_nucleo()
no_nulo <- especializar(
  nucleo$NoNulo, nombre_especifico = "NoNuloEdad"
)
instancia <- instanciar(no_nulo, entidad = "personas", atributos = "edad")
modelo_calidad <- modelo(instancia)
medir(modelo_calidad, data.frame(edad = c(20, NA, 35)))
#>                                     id_medida
#> 1 medicion-20260824T025021.208524-7514-000001
#> 2 medicion-20260824T025021.208524-7514-000002
#> 3 medicion-20260824T025021.208524-7514-000003
#>                            id_medicion               fecha metrica
#> 1 medicion-20260824T025021.208524-7514 2026-08-24 02:50:21  NoNulo
#> 2 medicion-20260824T025021.208524-7514 2026-08-24 02:50:21  NoNulo
#> 3 medicion-20260824T025021.208524-7514 2026-08-24 02:50:21  NoNulo
#>   metrica_especifica      metrica_instanciada   dimension   factor orientacion
#> 1         NoNuloEdad NoNuloEdad@personas.edad Completitud Densidad conformidad
#> 2         NoNuloEdad NoNuloEdad@personas.edad Completitud Densidad conformidad
#> 3         NoNuloEdad NoNuloEdad@personas.edad Completitud Densidad conformidad
#>        granularidad tipo_resultado  entidad atributo fila   objeto_medible
#> 1 instanciaAtributo       booleano personas     edad    1 personas$edad[1]
#> 2 instanciaAtributo       booleano personas     edad    2 personas$edad[2]
#> 3 instanciaAtributo       booleano personas     edad    3 personas$edad[3]
#>   resultado agregacion
#> 1         1       <NA>
#> 2         0       <NA>
#> 3         1       <NA>

# Las fábricas también se pueden encadenar: genérica() -> específica().
instancia_directa <- nucleo$NoNulo()(
  entidad = "personas", atributos = "edad"
)
propiedades_metrica(nucleo$Formato)
#>           propiedad configurada
#> 1 expresion_regular       FALSE
#> 2       diccionario       FALSE
#> 3         validador       FALSE

# Métrica propia: las dos llamadas encadenadas son especializar e instanciar.
metodo_origen <- function(tablas, instancia) {
  x <- tablas[[instancia$entidad]][[instancia$atributos]]
  filas <- seq_along(x)
  data.frame(
    resultado = !is.na(x) & nzchar(x),
    entidad = instancia$entidad,
    atributo = instancia$atributos,
    fila = filas,
    objeto = paste0(instancia$entidad, "$", instancia$atributos,
                    "[", filas, "]")
  )
}
OrigenDeclarado <- metrica(
  "OrigenDeclarado", "Indica si se declaró el origen del registro.",
  "instanciaAtributo", "booleano", orientacion = "conformidad",
  dimension = "Trazabilidad", factor = "Origen documentado",
  metodo = metodo_origen
)
origen <- OrigenDeclarado()(
  entidad = "entrega", atributos = "origen"
)
medir(
  modelo(origen),
  data.frame(origen = c("sistema_a", "", NA), stringsAsFactors = FALSE)
)
#>                                     id_medida
#> 1 medicion-20260824T025021.215584-7514-000001
#> 2 medicion-20260824T025021.215584-7514-000002
#> 3 medicion-20260824T025021.215584-7514-000003
#>                            id_medicion               fecha         metrica
#> 1 medicion-20260824T025021.215584-7514 2026-08-24 02:50:21 OrigenDeclarado
#> 2 medicion-20260824T025021.215584-7514 2026-08-24 02:50:21 OrigenDeclarado
#> 3 medicion-20260824T025021.215584-7514 2026-08-24 02:50:21 OrigenDeclarado
#>   metrica_especifica            metrica_instanciada    dimension
#> 1    OrigenDeclarado OrigenDeclarado@entrega.origen Trazabilidad
#> 2    OrigenDeclarado OrigenDeclarado@entrega.origen Trazabilidad
#> 3    OrigenDeclarado OrigenDeclarado@entrega.origen Trazabilidad
#>               factor orientacion      granularidad tipo_resultado entidad
#> 1 Origen documentado conformidad instanciaAtributo       booleano entrega
#> 2 Origen documentado conformidad instanciaAtributo       booleano entrega
#> 3 Origen documentado conformidad instanciaAtributo       booleano entrega
#>   atributo fila    objeto_medible resultado agregacion
#> 1   origen    1 entrega$origen[1]         1       <NA>
#> 2   origen    2 entrega$origen[2]         0       <NA>
#> 3   origen    3 entrega$origen[3]         0       <NA>

# Especialización oficial de teléfono fijo según el formato vigente del PNN.
telefono_pnn <- especializar(
  nucleo$DesactualizacionPorFormato,
  nombre_especifico = "TelefonoFijoPNN",
  expresion_regular = "^[0-9]{8}$"
)

# La métrica oficial es booleana y recibe la fecha límite Tf.
a_tiempo <- especializar(
  nucleo$OportunidadAtributoPorFecha,
  nombre_especifico = "EntregaATiempo",
  fecha_limite = as.Date("2026-06-30")
)
medir(
  modelo(instanciar(a_tiempo, "entregas", "fecha")),
  data.frame(fecha = as.Date(c("2026-06-29", "2026-07-01")))
)
#>                                     id_medida
#> 1 medicion-20260824T025021.220815-7514-000001
#> 2 medicion-20260824T025021.220815-7514-000002
#>                            id_medicion               fecha
#> 1 medicion-20260824T025021.220815-7514 2026-08-24 02:50:21
#> 2 medicion-20260824T025021.220815-7514 2026-08-24 02:50:21
#>                       metrica metrica_especifica           metrica_instanciada
#> 1 OportunidadAtributoPorFecha     EntregaATiempo EntregaATiempo@entregas.fecha
#> 2 OportunidadAtributoPorFecha     EntregaATiempo EntregaATiempo@entregas.fecha
#>   dimension      factor orientacion      granularidad tipo_resultado  entidad
#> 1  Frescura Oportunidad conformidad instanciaAtributo       booleano entregas
#> 2  Frescura Oportunidad conformidad instanciaAtributo       booleano entregas
#>   atributo fila    objeto_medible resultado agregacion
#> 1    fecha    1 entregas$fecha[1]         1       <NA>
#> 2    fecha    2 entregas$fecha[2]         0       <NA>

# La extensión continua conserva cuánto margen de utilidad queda.
grado <- especializar(
  nucleo$GradoOportunidadAtributoPorFecha,
  fecha_solicitud = as.Date("2026-06-01"),
  fecha_fin_utilidad = as.Date("2026-07-01")
)

# Formato(NumeroDocumento, DNIC) se obtiene conectando el validador incluido:
cedula_dnic <- especializar(
  nucleo$Formato, nombre_especifico = "NumeroDocumentoDNIC",
  validador = validar_ci_uy
)
```
