# Construir y ampliar un histórico de calidad

Crea un data frame plano y versionado con corridas producidas por
[`medir()`](https://sebollin.github.io/lupa/reference/medir.md) o
[`evaluar()`](https://sebollin.github.io/lupa/reference/evaluar.md).
`acumular_historico()` agrega objetos al mismo esquema y es idempotente
cuando recibe otra vez registros idénticos, incluso si el RDS se guardó
o se vuelve a leer bajo otro locale.

## Usage

``` r
historico_calidad(..., detalle = c("resumen", "completo"))

acumular_historico(historico, ..., detalle = c("resumen", "completo"))
```

## Arguments

- ...:

  Objetos `medicion`, `evaluacion_calidad` o `historico_calidad`.
  También puede darse una única lista que los contenga.

- detalle:

  Para evaluaciones, `"resumen"` conserva los niveles de regla y perfil;
  `"completo"` conserva además cada evaluación de medida. Una `medicion`
  pasada explícitamente siempre se conserva completa.

- historico:

  Objeto creado por `historico_calidad()`.

## Value

Data frame S3 `historico_calidad`. La columna `version_esquema` y el
atributo del mismo nombre permiten migraciones futuras. `nivel`
corresponde a `medida`, `evaluacion_medida`, `evaluacion_regla` o
`evaluacion_perfil`; una métrica sin valores se conserva como
`metrica_no_evaluada` con su motivo, siempre que la medición tenga al
menos una medida. Una medida que una regla declaró
`desenlace = "suprimir"` no publica su valor **tampoco aquí**: su fila
deja `resultado` en `NA` y marca `objeto_medible` con
`[valor suprimido]`, en los dos niveles donde esa medida aparece
—`medida` y `evaluacion_medida`—, porque esta tabla está pensada para
exportarse. Con el detalle resumido, las medidas suprimidas entran
igual, enmascaradas en el nivel `evaluacion_medida`: así una medición
acumulada **después** —también sobre un histórico guardado o leído de un
CSV— queda tapada en las mismas medidas, sea cual sea el orden en que
llegan los objetos. Una medición **enteramente** vacía —ninguna métrica
pudo aplicarse— no se acumula: se rechaza citando el motivo que
[`medir()`](https://sebollin.github.io/lupa/reference/medir.md) declaró
en `cobertura_metricas`, porque no hay corrida que registrar. El
atributo `configuracion_evaluacion` conserva, en una tabla plana
separada, el modelo, su marco y sus tipos, la aplicabilidad, el perfil,
la identidad de tabla y **la fecha** de cada corrida. Acumular una
corrida con un `id_medicion` ya presente exige que todo eso coincida, la
fecha incluida: dos entregas distintas son dos corridas, y el error
nombra en qué difieren.

## Details

El detalle predeterminado evita repetir una fila por celda y regla
cuando el objetivo es monitorear la serie de evaluaciones. El objeto no
guarda modelos, closures, datos originales ni perfiles de profiling.
Esto mantiene la tabla exportable directamente con
[`write.csv()`](https://rdrr.io/r/utils/write.table.html) o una
herramienta de base de datos. Al volver a leerla, las fechas de texto se
leen en UTC, con su hora.

`[`, [`subset()`](https://rdrr.io/r/base/subset.html) y
`dplyr::filter()` conservan la configuración de las corridas que quedan;
[`rbind()`](https://rdrr.io/r/base/cbind.html) de dos históricos los
acumula, como `acumular_historico()`.

El esquema largo mapea las cuatro tablas de la sección 9.5 del marco
mediante `nivel`. Las columnas que no corresponden a un nivel quedan
como `NA`.

## References

[AGESIC
(2020)](https://www.gub.uy/agencia-gobierno-electronico-sociedad-informacion-conocimiento/).
*Marco de trabajo para la Gestión de la Calidad de Datos en Gobierno
Digital*, versión 1.6, sección 9.5, Presidencia de la República,
Uruguay.

## See also

[`medir()`](https://sebollin.github.io/lupa/reference/medir.md),
[`evaluar()`](https://sebollin.github.io/lupa/reference/evaluar.md),
[`detectar_deriva_calidad()`](https://sebollin.github.io/lupa/reference/detectar_deriva_calidad.md),
[`reportar()`](https://sebollin.github.io/lupa/reference/reportar.md)

`historico_calidad()`,
[`leer_historico()`](https://sebollin.github.io/lupa/reference/guardar_historico.md)

## Examples

``` r
nucleo <- metricas_nucleo()
instancia <- instanciar(especializar(nucleo$NoNulo), "personas", "edad")
medidas <- medir(
  modelo(instancia), data.frame(edad = c(20, NA)),
  id_medicion = "enero", fecha = as.POSIXct("2026-01-31", tz = "UTC")
)
evaluacion <- evaluar(
  medidas,
  perfil_evaluacion("Basico", regla_evaluacion("Presente", function(x) x > 0))
)
historico_calidad(medidas, evaluacion)
#>   version_esquema             nivel
#> 1               1            medida
#> 2               1            medida
#> 3               1  evaluacion_regla
#> 4               1 evaluacion_perfil
#>                                               id_registro
#> 1 =medida|=enero|~|~|=enero-NoNulo%40personas.edad-000001
#> 2 =medida|=enero|~|~|=enero-NoNulo%40personas.edad-000002
#> 3            =evaluacion_regla|=enero|=Basico|=Presente|~
#> 4                   =evaluacion_perfil|=enero|=Basico|~|~
#>                           id_medida id_medicion      fecha perfil    regla
#> 1 enero-NoNulo@personas.edad-000001       enero 2026-01-31   <NA>     <NA>
#> 2 enero-NoNulo@personas.edad-000002       enero 2026-01-31   <NA>     <NA>
#> 3                              <NA>       enero 2026-01-31 Basico Presente
#> 4                              <NA>       enero 2026-01-31 Basico     <NA>
#>   metrica metrica_especifica  metrica_instanciada   dimension   factor
#> 1  NoNulo             NoNulo NoNulo@personas.edad Completitud Densidad
#> 2  NoNulo             NoNulo NoNulo@personas.edad Completitud Densidad
#> 3    <NA>               <NA>                 <NA>        <NA>     <NA>
#> 4    <NA>               <NA>                 <NA>        <NA>     <NA>
#>        granularidad tipo_resultado  entidad atributo fila   objeto_medible
#> 1 instanciaAtributo       booleano personas     edad    1 personas$edad[1]
#> 2 instanciaAtributo       booleano personas     edad    2 personas$edad[2]
#> 3              <NA>           <NA>     <NA>     <NA>   NA             <NA>
#> 4              <NA>           <NA>     <NA>     <NA>   NA             <NA>
#>   n_elementos resultado agregacion
#> 1           1       1.0       <NA>
#> 2           1       0.0       <NA>
#> 3           2       0.5       <NA>
#> 4           1       0.5       <NA>
```
