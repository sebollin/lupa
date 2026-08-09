# Construir y ampliar un histórico de calidad

Crea un data frame plano y versionado con corridas producidas por
`medir()` o `evaluar()`. `acumular_historico()` agrega objetos al mismo
esquema y es idempotente cuando recibe otra vez registros idénticos.

## Uso

``` r
historico_calidad(..., detalle = c("resumen", "completo"))

acumular_historico(historico, ..., detalle = c("resumen", "completo"))
```

## Argumentos

  - ...:
    
    Objetos `medicion`, `evaluacion_calidad` o `historico_calidad`.
    También puede darse una única lista que los contenga.

  - detalle:
    
    Para evaluaciones, `"resumen"` conserva los niveles de regla y
    perfil; `"completo"` conserva además cada evaluación de medida. Una
    `medicion` pasada explícitamente siempre se conserva completa.

  - historico:
    
    Objeto creado por `historico_calidad()`.

## Valor

Data frame S3 `historico_calidad`. La columna `version_esquema` y el
atributo del mismo nombre permiten migraciones futuras. `nivel`
corresponde a `medida`, `evaluacion_medida`, `evaluacion_regla` o
`evaluacion_perfil`.

## Detalles

El detalle predeterminado evita repetir una fila por celda y regla
cuando el objetivo es monitorear la serie de evaluaciones. El objeto no
guarda modelos, closures, datos originales ni perfiles de profiling.
Esto mantiene la tabla exportable directamente con `write.csv()` o una
herramienta de base de datos.

El esquema largo mapea las cuatro tablas de la sección 9.5 del marco
mediante `nivel`. Las columnas que no corresponden a un nivel quedan
como `NA`.

## Referencias

[AGESIC
(2020)](https://www.gub.uy/agencia-gobierno-electronico-sociedad-informacion-conocimiento/).
*Marco de trabajo para la Gestión de la Calidad de Datos en Gobierno
Digital*, versión 1.6, sección 9.5, Presidencia de la República,
Uruguay.

## Ver también

`medir()`, `evaluar()`, `detectar_deriva_calidad()`, `reportar()`

`historico_calidad()`, `leer_historico()`

## Ejemplos

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
#>                                    id_registro    id_medida id_medicion
#> 1             =medida|=enero|~|~|=enero-000001 enero-000001       enero
#> 2             =medida|=enero|~|~|=enero-000002 enero-000002       enero
#> 3 =evaluacion_regla|=enero|=Basico|=Presente|~         <NA>       enero
#> 4        =evaluacion_perfil|=enero|=Basico|~|~         <NA>       enero
#>        fecha perfil    regla metrica metrica_especifica  metrica_instanciada
#> 1 2026-01-31   <NA>     <NA>  NoNulo             NoNulo NoNulo@personas.edad
#> 2 2026-01-31   <NA>     <NA>  NoNulo             NoNulo NoNulo@personas.edad
#> 3 2026-01-31 Basico Presente    <NA>               <NA>                 <NA>
#> 4 2026-01-31 Basico     <NA>    <NA>               <NA>                 <NA>
#>     dimension   factor      granularidad tipo_resultado  entidad atributo fila
#> 1 Completitud Densidad instanciaAtributo       booleano personas     edad    1
#> 2 Completitud Densidad instanciaAtributo       booleano personas     edad    2
#> 3        <NA>     <NA>              <NA>           <NA>     <NA>     <NA>   NA
#> 4        <NA>     <NA>              <NA>           <NA>     <NA>     <NA>   NA
#>     objeto_medible n_elementos resultado agregacion
#> 1 personas$edad[1]           1       1.0       <NA>
#> 2 personas$edad[2]           1       0.0       <NA>
#> 3             <NA>           2       0.5       <NA>
#> 4             <NA>           1       0.5       <NA>
```
