# Guardar y leer decisiones sobre hallazgos

Escribe la tabla de
[`decidir_hallazgos()`](https://sebollin.github.io/lupa/reference/decidir_hallazgos.md)
como un CSV en UTF-8, que se edita a mano y se versiona junto con el
proyecto, y la vuelve a leer. Una fila por hallazgo decidido; las
columnas son las de la tabla de decisiones. Para retirar una decisión,
se borra su fila.

## Usage

``` r
guardar_decisiones(
  decisiones,
  archivo,
  sobrescribir = FALSE,
  separador = c(",", ";")
)

leer_decisiones(archivo)
```

## Arguments

- decisiones:

  Tabla de
  [`decidir_hallazgos()`](https://sebollin.github.io/lupa/reference/decidir_hallazgos.md).

- archivo:

  Ruta del CSV.

- sobrescribir:

  Si es `FALSE` (por omisión), no reemplaza un archivo existente.

- separador:

  `","` (por omisión) o `";"`, que es el que abre bien una planilla con
  configuración regional en español. `leer_decisiones()` detecta cuál se
  usó.

## Value

`guardar_decisiones()` devuelve invisiblemente la ruta normalizada;
`leer_decisiones()`, una tabla `decisiones_hallazgos` validada.

## Details

El archivo se escribe en UTF-8 con marca de orden de bytes, que es lo
que una planilla necesita para abrirlo con sus acentos. Al leer se
acepta UTF-8 con o sin marca, con coma o punto y coma; si los bytes no
son UTF-8 válido se leen como Windows-1252 (lo que guarda una planilla
de Windows) y se avisa. La fecha se guarda como texto ISO 8601 con huso
y se lee en esa forma o como día/mes/año de una planilla (si el día y el
mes son ambiguos, se rechaza); un `n_afectados` con separador de miles
de planilla (`1.000`) se lee como mil, y lo que no se puede leer sin
adivinar es un error. Se valida todo: las decisiones válidas, un motivo,
quién y cuándo en cada fila, y ningún `id_hallazgo` repetido. Un archivo
que no se puede leer entero se rechaza con un error, nunca se devuelve
vacío en silencio; los finales de línea dentro de un motivo se conservan
tal cual.

## See also

[`decidir_hallazgos()`](https://sebollin.github.io/lupa/reference/decidir_hallazgos.md),
[`aplicar_decisiones()`](https://sebollin.github.io/lupa/reference/aplicar_decisiones.md)

## Examples

``` r
perfil <- perfilar(datos_administrativos, analizar_dependencias = FALSE)
decisiones <- decidir_hallazgos(
  perfil, "falso_positivo", motivo = "Revisado en la fuente.",
  quien = "equipo de datos", tipo_hallazgo = "faltantes"
)
archivo <- tempfile(fileext = ".csv")
guardar_decisiones(decisiones, archivo)
leer_decisiones(archivo)
#>                             id_hallazgo                 tabla grupo columna
#> 1 datos_administrativos||sexo|faltantes datos_administrativos  <NA>    sexo
#>   tipo_hallazgo       decision                 motivo           quien
#> 1     faltantes falso_positivo Revisado en la fuente. equipo de datos
#>                  fecha  severidad n_afectados
#> 1 2026-11-24T18:05:27Z sospechoso           2
#>                                             evidencia huella_valores
#> 1 0 ausentes reales y 2 disfrazados (0.154 del total)           <NA>
#>   huella_filas                                                    detalle
#> 1         <NA> reales=0;disfrazados=2;numericos=0;textuales=2;centinelas=
#>   huella_estructura
#> 1              <NA>
unlink(archivo)
```
