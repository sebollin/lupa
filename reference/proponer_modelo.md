# Proponer un modelo de calidad desde el profiling

Construye el puente entre el examen de datos y la medición. El resultado
no ejecuta métricas: es una tabla editable donde cada fila conserva el
hallazgo o diagnóstico que la originó, la justificación, la
configuración y una marca `incluir`.
[`modelo_desde_propuesta()`](https://sebollin.github.io/lupa/reference/modelo_desde_propuesta.md)
materializa únicamente las filas que el usuario deja activas.

## Usage

``` r
proponer_modelo(
  perfil,
  datos = NULL,
  relaciones = NULL,
  entidades_relacion = character(),
  max_valores_dominio = 20L,
  max_sugerencias = 100L
)
```

## Arguments

- perfil:

  Objeto creado por
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md).

- datos:

  Datos originales opcionales. Son necesarios para proponer dominios
  observados y materializar reglas de dependencia funcional.

- relaciones:

  Resultado opcional de
  [`detectar_relaciones()`](https://sebollin.github.io/lupa/reference/detectar_relaciones.md).

- entidades_relacion:

  Dos nombres de entidad correspondientes a las tablas usadas en
  `relaciones`.

- max_valores_dominio:

  Máximo de valores para proponer un dominio por extensión.

- max_sugerencias:

  Máximo de filas devueltas.

## Value

Data frame S3 de clase `propuesta_modelo`; las columnas de listas
contienen la configuración y los vínculos sin convertirlos en texto. Su
método [`print()`](https://rdrr.io/r/base/print.html) ya no falla por
codificación en ningún locale: la copia que se exhibe convierte lo que
trae codificación declarada, declara como UTF-8 los bytes válidos sin
declarar —R los escapa como `<U+00F1>` donde el locale no los
represente— y deja el resto tal cual mientras R pueda imprimirlo. Bajo
un locale UTF-8 la salida es exactamente la de R; bajo uno que no puede
representar un carácter difiere a propósito, publicando el punto de
código donde R escaparía los bytes. Sólo cuando R no puede imprimir se
reintenta con su propio octal, `\377`. Eso no convierte a
[`print()`](https://rdrr.io/r/base/print.html) en infalible: si una
clase suya tiene un método
[`format()`](https://rdrr.io/r/base/format.html) que da error, ese error
le llega sin alterar. Y un
[`format()`](https://rdrr.io/r/base/format.html) que consulte
[`Encoding()`](https://rdrr.io/r/base/Encoding.html) verá las marcas de
la copia, no las del objeto original.

## Details

Las reglas observadas que pueden sobreajustarse a una entrega —dominios
y patrones dominantes— se proponen inactivas. Los controles
estructurales directos, como `NoNulo`, duplicación exacta y dependencias
funcionales exactas, se activan. `max_sugerencias` recorta después de
ordenar por prioridad y el objeto declara el total en sus atributos. Si
una columna contiene faltantes disfrazados, `NoNulo` se propone pero
queda inactiva hasta normalizarlos o configurar la métrica para
reconocerlos. De otro modo mediría sólo los `NA` reales y sobrestimaría
la completitud.

## See also

[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
[`detectar_dependencias()`](https://sebollin.github.io/lupa/reference/detectar_dependencias.md),
[`modelo_desde_propuesta()`](https://sebollin.github.io/lupa/reference/modelo_desde_propuesta.md),
[`modelo()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md)

## Examples

``` r
datos <- data.frame(
  codigo = rep(1:3, each = 4),
  categoria = rep(c("A", "B", "C"), each = 4),
  valor = c(1:11, NA)
)
propuesta <- proponer_modelo(perfilar(datos), datos)
propuesta[, c("metrica", "origen", "incluir")]
#>                       metrica                                  origen incluir
#> 1                      NoNulo              perfil:n_faltantes_totales    TRUE
#> 2 ReglaIntegridadIntraEntidad dependencia_funcional:categoria->codigo    TRUE
#> 3 ReglaIntegridadIntraEntidad dependencia_funcional:codigo->categoria    TRUE
#> 4                     Formato               perfil:patron_dominante:A   FALSE
#> 5 ValoresPosiblesPorExtension                perfil:dominio_observado   FALSE
```
