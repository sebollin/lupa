# 1. Empezar en diez minutos

Llega una tabla y, antes de usarla, queremos saber si podemos confiar en
ella: qué tiene mal, en qué filas está cada problema y qué no se pudo
revisar.

> **En esta guía:**
>
> - perfilamos una tabla con
>   [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
>   y leemos su resumen;
> - empezamos por los hallazgos de severidad `error`;
> - vamos de un hallazgo a las filas del sistema de origen con `clave`;
> - leemos en
>   [`cobertura()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
>   lo que no se pudo evaluar;
> - guardamos un informe y limpiamos una columna en una copia.

## Los datos

`datos_operativos` viene con el paquete: 13 registros sintéticos de un
sistema de eventos, con defectos sembrados a propósito. Antes de
perfilar, los miramos.

``` r

library(lupa)
datos_operativos
#>    id_registro codigo_usuario fecha_evento      canal  monto   zona   sistema
#> 1            1        USR-001   2024-01-31        web   1250  Norte principal
#> 2            2        USR-002   31/01/2024   telefono   1600 Centro principal
#> 3            3        USR0003   2024/02/15        Web   1425    Sur principal
#> 4            4        USR-004   16-03-2024 presencial    -99   Este principal
#> 5            5          MAL-5   2024.04.17        S/D      0  Oeste principal
#> 6            6        USR-006     20240518        web   1750  Norte principal
#> 7            7        USR-007   19/06/2024   telefono   1810 Centro principal
#> 8            8        USR-008   2024-07-20       web    1490    Sur principal
#> 9            9        USR-009   21/08/2024        APP   1660   Este principal
#> 10          10        USR-010   2024-09-22        web   1700  Oeste principal
#> 11          10        USR-010   23/10/2024        web    -50  Oeste principal
#> 12           1        USR-001   2024-01-31        web   1250  Norte principal
#> 13          11            S/D         NULL            999999 Centro principal
#>                      contacto id_copia id_evento
#> 1   usuario01@example.invalid        1    EVT001
#> 2   usuario02@example.invalid        2    EVT002
#> 3   usuario03@example.invalid        3    EVT003
#> 4   usuario04@example.invalid        4    EVT004
#> 5   usuario05@example.invalid        5    EVT005
#> 6   usuario06@example.invalid        6    EVT006
#> 7   usuario07@example.invalid        7    EVT007
#> 8   usuario08@example.invalid        8    EVT008
#> 9   usuario09@example.invalid        9    EVT009
#> 10  usuario10@example.invalid       10    EVT010
#> 11 usuario10b@example.invalid       10    EVT012
#> 12  usuario01@example.invalid        1    EVT001
#> 13               contacto-mal       11    EVT011
```

Esto es lo que trae cada columna (lo describe
[`?datos_operativos`](https://sebollin.github.io/lupa/reference/datos_operativos.md))
y el hallazgo que lo va a señalar:

| columna | qué trae a propósito | hallazgo que lo señala |
|----|----|----|
| `id_registro` | `1` y `10` repetidos; las dos filas con `10` no coinciden en lo demás | `posible_identificador`; la repetición, al declararla clave (guía 2) |
| `codigo_usuario` | `"S/D"` en lugar de un ausente; `USR0003` y `MAL-5` fuera del formato | `faltantes_disfrazados`; los formatos, ninguno (se ve más abajo por qué) |
| `fecha_evento` | cinco formatos de fecha y un `"NULL"` | `formatos_fecha_mixtos`, `faltantes_disfrazados` |
| `canal` | `"web"`, `"Web"` y `"web "`; `"S/D"` y una celda vacía | `mayusculas_inconsistentes`, `espacios_sobrantes`, `faltantes_disfrazados` |
| `monto` | `-99`, `0`, `-50` y `999999` | `faltantes_disfrazados` para el `-99`; el `0` y el `-50`, sólo si declaramos que no se admiten (`columnas_sin_ceros`, `columnas_no_negativas`); el `999999`, ninguno (se ve más abajo por qué) |
| `sistema` | el mismo valor en todas las filas | `constante` |
| `contacto` | correos sintéticos y uno mal formado | `dato_personal_posible`; el mal formado, ninguno (por la misma razón que los formatos de `codigo_usuario`) |
| `id_copia` | una copia de `id_registro` | `columnas_duplicadas` |
| `id_evento` | el identificador del evento; `EVT001` dos veces | `clave_no_unica`, al declararla clave |
| filas 1 y 12 | idénticas | `filas_duplicadas` |

`zona` no trae defectos.

## Perfilar

[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
lee la tabla y no la modifica. Le pasamos en `clave` la columna que
identifica cada evento en el sistema de origen: así cada hallazgo podrá
decir en qué eventos está.

``` r

perfil <- perfilar(datos_operativos, clave = "id_evento")
#> Warning: La clave declarada no es única: entre las 13 filas con la clave completa, 1 repite su
#> valor (2 filas participan en colisiones). La clave cumple la ausencia de nulos: no tiene
#> valores ausentes.
```

El aviso ya es un resultado: la clave que declaramos se repite. `lupa`
no la da por buena; la comprueba. Al imprimir el perfil vemos el
resumen:

``` r

perfil
#> 
#> ── Perfil de datos: datos_operativos ─────────────────────────────────────────────────────
#> ✖ 6 hallazgos con severidad error
#> ! 11 hallazgos sospechosos
#> ✔ 6 hallazgos informativos ok
#> ℹ 9 diagnósticos no evaluados
#> 
#> ── Resumen general ──
#> 
#> Filas: 13
#> Columnas: 10
#> Celdas: 130
#> Filas completas: 13
#> Filas duplicadas: 1
#> 
#> ── Resumen por columna ──
#> 
#>         columna tipo_inferido prop_faltantes_totales n_distintos n_outliers
#>     id_registro         doble             0.00000000          11         NA
#>  codigo_usuario         texto             0.07692308          11         NA
#>    fecha_evento         fecha             0.07692308          12         NA
#>           canal         texto             0.15384615           8         NA
#>           monto         doble             0.07692308          12         NA
#>            zona         texto             0.00000000           5         NA
#>         sistema         texto             0.00000000           1         NA
#>        contacto         texto             0.00000000          12         NA
#>        id_copia         doble             0.00000000          11         NA
#>       id_evento identificador             0.00000000          12         NA
#> ℹ Se muestran 5 de 118 campos; los demás están en `columnas()`.
```

Las cuatro primeras líneas cuentan los hallazgos por severidad y los
diagnósticos que no se pudieron evaluar. `error` quiere decir que un
control cruzó su criterio; `sospechoso`, que algo merece que lo
revisemos; `ok`, que es una lectura informativa, no un defecto. Debajo
vienen el resumen general y unas pocas medidas por columna. (La
impresión trae también la memoria que ocupa la tabla; acá no la
mostramos porque cambia según la instalación de R).

## Empezar por los errores

Los hallazgos son una tabla, y se filtran como cualquier `data.frame`.

``` r

h <- hallazgos(perfil)
h[h$severidad == "error", c("columna", "tipo_hallazgo", "n_afectados")]
#>           columna         tipo_hallazgo n_afectados
#> 3  codigo_usuario faltantes_disfrazados           1
#> 5    fecha_evento faltantes_disfrazados           1
#> 6    fecha_evento formatos_fecha_mixtos           5
#> 9           canal faltantes_disfrazados           2
#> 20           <NA>        clave_no_unica           2
#> 21           <NA>      filas_duplicadas           2
```

Son 6. Una `columna` en `NA` quiere decir que el hallazgo es de la tabla
entera: las filas 1 y 12 son idénticas (`filas_duplicadas`), y por eso
`EVT001` aparece dos veces en la clave (`clave_no_unica`).

## Del hallazgo a la fila

Tomamos uno: los ausentes disfrazados de `canal`. La evidencia dice qué
se vio, y la sugerencia, qué se podría hacer.

``` r

canal <- h[h$columna %in% "canal" & h$tipo_hallazgo == "faltantes_disfrazados", ]
writeLines(canal$evidencia)
#> <blanco> (1); S/D (1)
writeLines(strwrap(canal$sugerencia, 88))
#> Normalizar estas representaciones a NA conservando su significado si fuera necesario.
```

La trazabilidad dice en qué filas está, con los valores de la clave que
declaramos.

``` r

eventos <- canal$trazabilidad[[1]]$claves$id_evento
eventos
#> [1] "EVT005" "EVT011"
```

Con esos identificadores buscamos los eventos en el sistema de origen,
sin abrir la tabla entera. Acá podemos cotejarlos (con comillas, para
que se vea la celda vacía):

``` r

filas <- datos_operativos$id_evento %in% eventos
print(datos_operativos[filas, c("id_evento", "canal")], quote = TRUE)
#>    id_evento canal
#> 5   "EVT005" "S/D"
#> 13  "EVT011"    ""
```

Sin `clave`, la trazabilidad trae sólo los números de fila, que sirven
dentro de R pero no en el sistema de origen.

## Lo que no se pudo evaluar

Un control que no encontró nada no es lo mismo que un control que no
corrió. Lo segundo queda en
[`cobertura()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md),
con el motivo y cómo resolverlo.

``` r

no_evaluados <- cobertura(perfil)
no_evaluados[, c("diagnostico", "columna")]
#>              diagnostico      columna
#> 1               outliers  id_registro
#> 2               outliers fecha_evento
#> 3   resumen_cuantitativo fecha_evento
#> 4            patron_raro fecha_evento
#> 5               outliers        monto
#> 6               outliers     id_copia
#> 7 proximidad_vocabulario         zona
#> 8 proximidad_vocabulario     contacto
#> 9 proximidad_vocabulario    id_evento
```

El `999999` de `monto` no aparece entre los hallazgos, y no porque esté
bien: la búsqueda de valores extremos no corrió en esa columna.

``` r

motivo <- no_evaluados$motivo[
  no_evaluados$diagnostico == "outliers" & no_evaluados$columna == "monto"
]
writeLines(strwrap(motivo, 88))
#> No se evaluaron los límites de Tukey: hay 13 valores finitos y se requieren al menos
#> 20. Reunir más observaciones o declarar otro criterio antes de interpretar outliers.
```

Lo que no se pudo medir queda registrado; nunca cuenta como cero.

Los formatos raros de `codigo_usuario` (`USR0003` y `MAL-5`) tampoco
tienen hallazgo, por otra razón: ese control sí corrió, pero cada
formato ocupa una fila de 13, y por omisión un formato es raro cuando no
llega al 5 % de los valores (`umbral_patron_raro`). En una tabla más
larga lo señalaría.

## Un informe para compartir

[`reportar()`](https://sebollin.github.io/lupa/reference/reportar.md)
escribe un único archivo HTML, que se abre en cualquier navegador y no
necesita red ni programas aparte.

``` r

archivo <- file.path(tempdir(), "calidad-de-la-entrega.html")
reportar(perfil, archivo = archivo, titulo = "Calidad de la entrega")
```

El informe tiene estas secciones: Resumen general; Hallazgos por
severidad; Resumen por columna; Clasificación de posibles datos
personales; Cobertura del análisis; Cobertura de diagnósticos; Patrones
de formato; Formatos de fecha; Dependencias funcionales.
[`reportar()`](https://sebollin.github.io/lupa/reference/reportar.md)
devuelve la ruta del archivo: en una sesión interactiva,
`browseURL(archivo)` lo abre en el navegador. También recibe el
resultado completo de
[`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md),
con las mediciones y el plan de limpieza; eso lo vemos en [3. Explorar
columnas con
`analizar()`](https://sebollin.github.io/lupa/articles/explorar-columnas.md).

## Limpiar una columna

`lupa` no cambia los datos que perfila. Para limpiar, propone un plan
editable, y
[`aplicar()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)
ejecuta sobre una copia sólo las acciones que marcamos.

``` r

plan <- planificar_limpieza(perfil, datos_operativos)
plan[plan$columna %in% "canal", c("id_accion", "estrategia", "recomendada", "aplicar")]
#>      id_accion                    estrategia recomendada aplicar
#> 6  accion-0006 convertir_ausencias_textuales        TRUE    TRUE
#> 7  accion-0007             recortar_espacios        TRUE    TRUE
#> 8  accion-0008          convertir_minusculas       FALSE   FALSE
#> 9  accion-0009              convertir_titulo       FALSE   FALSE
#> 10 accion-0010          convertir_mayusculas       FALSE   FALSE
#> 11 accion-0011   convertir_segun_diccionario       FALSE   FALSE
```

Para `canal`, el plan recomienda y deja marcadas dos acciones: convertir
en `NA` los ausentes escritos como texto y recortar los espacios del
borde. Entre minúsculas, mayúsculas o un diccionario no elige: esa
decisión es nuestra. Cada acción trae su justificación, que dice también
qué conviene confirmar:

``` r

justificacion <- plan$justificacion[
  plan$columna %in% "canal" & plan$estrategia == "convertir_ausencias_textuales"
]
writeLines(strwrap(justificacion, 88))
#> Las representaciones textuales del catálogo son marcadores habituales de ausencia. El
#> cambio no es reversible: confirmar que ninguno de esos textos sea un valor legítimo de
#> la columna -`NA` es el código de Namibia, `NULL` puede ser un apellido-.
```

Para esta guía aplicamos sólo esas dos.

``` r

plan$aplicar <- plan$columna %in% "canal" & plan$recomendada
limpio <- aplicar(plan, datos_operativos)
limpio$registro[, c("columna", "estrategia", "n_cambiadas", "estado")]
#>   columna                    estrategia n_cambiadas    estado
#> 1   canal convertir_ausencias_textuales           2 ejecutada
#> 2   canal             recortar_espacios           1 ejecutada
```

Comparamos los valores antes y después en las filas que cambiaron:

``` r

antes <- datos_operativos$canal
despues <- limpio$datos$canal
cambiaron <- is.na(despues) | antes != despues
print(data.frame(id_evento = datos_operativos$id_evento, antes, despues)[cambiaron, ],
      quote = TRUE)
#>    id_evento  antes despues
#> 5   "EVT005"  "S/D"      NA
#> 8   "EVT008" "web "   "web"
#> 13  "EVT011"     ""      NA
```

La columna `antes` se leyó de `datos_operativos` después de aplicar el
plan: el original sigue como estaba. `"Web"` y `"APP"` quedaron igual,
porque no elegimos ninguna convención de mayúsculas. El plan completo,
con los grupos de acciones y su registro, está en [6. Limpiar con un
plan](https://sebollin.github.io/lupa/articles/limpiar-con-un-plan.md).

## Resumen

- [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  lee la tabla sin modificarla; imprimir el perfil da el recuento por
  severidad y lo que no se pudo evaluar.
- [`hallazgos()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
  es una tabla: empezamos por `severidad == "error"`.
- Con `clave`, la trazabilidad de cada hallazgo trae los identificadores
  para ir al sistema de origen.
- [`cobertura()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
  lista los controles que no corrieron: un hallazgo que falta no prueba
  que el dato esté bien.
- [`reportar()`](https://sebollin.github.io/lupa/reference/reportar.md)
  guarda un informe HTML;
  [`planificar_limpieza()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)
  y
  [`aplicar()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)
  limpian una copia con las acciones que elegimos.

## Paso siguiente

[2. Leer un
perfil](https://sebollin.github.io/lupa/articles/leer-un-perfil.md)
([`vignette("leer-un-perfil")`](https://sebollin.github.io/lupa/articles/leer-un-perfil.md))
explica las severidades, cada campo de un hallazgo, las claves y el
vocabulario del paquete.
