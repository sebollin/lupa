# Reglas y perfiles de evaluación

Una regla aplica una condición a los resultados de una o más métricas
instanciadas. Un perfil reúne reglas y su evaluación es la media
aritmética simple de las evaluaciones de esas reglas; no es un índice de
dimensión ni un índice global de calidad.

## Usage

``` r
regla_evaluacion(
  nombre,
  condicion,
  metricas = NULL,
  proporcion_minima = NULL,
  desenlace = NULL
)

perfil_evaluacion(nombre, ...)

perfiles_madurez(metricas = NULL, umbrales = NULL)
```

## Arguments

- nombre:

  Nombre de la regla o del perfil.

- condicion:

  Función que recibe el vector `resultado` de las medidas seleccionadas,
  en el orden de la tabla, y debe devolver un vector lógico sin ausentes
  de la misma longitud. Puede declarar un segundo argumento
  `orientacion` para recibir el metadato homónimo de cada medida; las
  funciones existentes de un argumento siguen siendo válidas. No
  modifica las medidas.

- metricas:

  Nombres de métricas instanciadas a las que se aplica la regla, es
  decir, valores de la columna `metrica_instanciada`. `NULL`, el valor
  predeterminado, aplica la condición a todas.

- proporcion_minima:

  `NULL`, para conservar una regla por medida, o un número entre `0` y
  `1` que declara la proporción mínima de medidas que deben cumplir
  `condicion`. En este segundo caso la regla es agregada: el umbral
  queda guardado en el objeto y
  [`evaluar()`](https://sebollin.github.io/lupa/reference/evaluar.md)
  publica la proporción, el veredicto y el universo de medidas que la
  produjo.

- desenlace:

  `NULL`, para limitar la regla a evaluar, o `"suprimir"` para declarar
  que las medidas que no cumplen `condicion` no deben publicarse. No
  existe un desenlace predeterminado.

- ...:

  Reglas creadas por `regla_evaluacion()` o una única lista que las
  contenga.

- umbrales:

  Vector numérico con nombres, estrictamente creciente y en `[0, 1]`.
  `NULL` conserva los tres perfiles incluidos de fábrica.

## Value

`regla_evaluacion()` devuelve una `regla_evaluacion`;
`perfil_evaluacion()` devuelve un `perfil_evaluacion`; y
`perfiles_madurez()` devuelve una lista de perfiles.

## Details

`perfiles_madurez()` crea por omisión los perfiles `Básico`,
`Intermedio` y `Avanzado` de AGESIC, con condiciones estrictas `> 0.5`,
`> 0.7` y `> 0.9`. El argumento `umbrales` permite construir otra
familia con nombres y cortes crecientes propios sobre las mismas
métricas instanciadas.

`regla_evaluacion()` almacena la función sin ejecutarla.
[`evaluar()`](https://sebollin.github.io/lupa/reference/evaluar.md)
selecciona las medidas mediante `metricas`, llama una vez a `condicion`
y rechaza resultados que no sean lógicos, que tengan otra longitud o que
contengan `NA`. Si `proporcion_minima` no es `NULL`, calcula sobre esos
mismos lógicos la proporción que cumple y la compara mediante `>=` con
el umbral declarado; no pondera medidas ni construye un puntaje global.
Las evaluaciones cuyas reglas no declaran `desenlace` conservan su
estructura anterior. Cuando una regla declara `desenlace = "suprimir"`,
[`evaluar()`](https://sebollin.github.io/lupa/reference/evaluar.md)
añade un plan trazable con una fila por medida incumplida y por regla;
no modifica la medición ni los datos que la originaron. La función
expresa un criterio de evaluación; no es un método de medición ni recibe
el data frame original. Si ningún nombre de `metricas` coincide, el
error enumera tanto los nombres solicitados como las métricas
instanciadas disponibles, que normalmente tienen la forma
`MetricaEspecifica@entidad.atributo`.

## See also

[`medir()`](https://sebollin.github.io/lupa/reference/medir.md),
[`evaluar()`](https://sebollin.github.io/lupa/reference/evaluar.md),
`perfiles_madurez()`

`regla_evaluacion()`,
[`comparar_evaluaciones()`](https://sebollin.github.io/lupa/reference/comparar_evaluaciones.md),
[`historico_calidad()`](https://sebollin.github.io/lupa/reference/historico_calidad.md)

[`evaluar()`](https://sebollin.github.io/lupa/reference/evaluar.md),
[`detectar_deriva_calidad()`](https://sebollin.github.io/lupa/reference/detectar_deriva_calidad.md)

## Examples

``` r
regla <- regla_evaluacion("Completitud suficiente", function(x) x > 0.9)
regla_70 <- regla_evaluacion(
  "Al menos 70 %", function(x) x > 0.9, proporcion_minima = 0.7
)
regla_publicacion <- regla_evaluacion(
  "Medida publicable", function(x) x > 0.9, desenlace = "suprimir"
)
perfil <- perfil_evaluacion("Operativo", regla)
madurez <- perfiles_madurez("NoNulo")
propios <- perfiles_madurez(
  "NoNulo", c(Exploratorio = 0.3, Operativo = 0.65, Consolidado = 0.85)
)
names(madurez)
#> [1] "Basico"     "Intermedio" "Avanzado"  
names(propios)
#> [1] "Exploratorio" "Operativo"    "Consolidado" 
perfil$nombre
#> [1] "Operativo"
```
