# Getting started in ten minutes

> This guide is the English version of [1. Empezar en diez
> minutos](https://sebollin.github.io/lupa/articles/lupa.md). `lupa`
> writes its messages and tables in Spanish, so the output below is in
> Spanish.

A table arrives and, before using it, we want to know whether we can
trust it. What is wrong with it, in which rows are the problems, and
what could not be checked?

> **In this guide:**
>
> - we profile a table with
>   [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
>   and read its summary;
> - we start with the findings of severity `error`;
> - we go from a finding to the rows of the source system with `clave`;
> - we read in
>   [`cobertura()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
>   what could not be evaluated;
> - we save a report and clean a column in a copy.

## The data

`datos_operativos` (operational data) comes with the package: 13
synthetic records from an event system, with defects planted on purpose.
Before profiling, we look at them.

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

This is what each column carries
([`?datos_operativos`](https://sebollin.github.io/lupa/reference/datos_operativos.md)
describes it), with the finding that flags it:

| column | what it carries on purpose | finding that flags it |
|----|----|----|
| `id_registro` | `1` and `10` repeated; the 2 rows with `10` differ in the rest | `posible_identificador` (possible identifier); the repetition, once declared as a key (guide 2) |
| `codigo_usuario` | `"S/D"` instead of a missing value; `USR0003` and `MAL-5` outside the format | `faltantes_disfrazados` (disguised missing values); the formats, none (see below why) |
| `fecha_evento` | 5 date formats and a `"NULL"` | `formatos_fecha_mixtos` (mixed date formats), `faltantes_disfrazados` |
| `canal` | `"web"`, `"Web"` and `"web "`; `"S/D"` and an empty cell | `mayusculas_inconsistentes` (inconsistent capitalization), `espacios_sobrantes` (extra spaces), `faltantes_disfrazados` |
| `monto` | `-99`, `0`, `-50` and `999999` | `faltantes_disfrazados` for the `-99`; the `0` and the `-50`, only if we declare that they are not allowed (`columnas_sin_ceros`, `columnas_no_negativas`: columns without zeros, non-negative columns); the `999999`, none (see below why) |
| `sistema` | the same value in every row | `constante` (constant) |
| `contacto` | synthetic emails and one malformed | `dato_personal_posible` (possible personal data); the malformed one, none (for the same reason as the formats of `codigo_usuario`) |
| `id_copia` | a copy of `id_registro` | `columnas_duplicadas` (duplicated columns) |
| `id_evento` | the identifier of the event; `EVT001` twice | `clave_no_unica` (non-unique key), once declared as a key |
| rows 1 and 12 | identical | `filas_duplicadas` (duplicated rows) |

`zona` carries no defects.

## Profiling

[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
(*profile*) reads the table without modifying it. In `clave` (*key*) we
pass the column that identifies each event in the source system. That
way, where a finding points to rows, it says which events they are.

``` r

perfil <- perfilar(datos_operativos, clave = "id_evento")
#> Warning: La clave declarada no es única: entre 13 filas con la clave completa, 1 repite su valor
#> (2 filas participan en colisiones). La clave cumple la ausencia de nulos: no tiene
#> valores ausentes.
```

The warning is the first result: the key we declared repeats. `lupa`
does not take it for granted; it checks it. Printing the profile shows
the summary:

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

The first 4 lines count the findings by severity and the diagnostics
that could not be evaluated:

- `error`: a check crossed its criterion;
- `sospechoso` (*suspicious*): something deserves a look;
- `ok`: an informative reading, not a defect.

Below come the general summary and a few measures per column. The
printout also shows the memory the table takes; we leave it out here
because it changes with the R installation.

## Starting with the errors

The findings that
[`hallazgos()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
(*findings*) returns are a table, and they are filtered like any
`data.frame`.

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

There are 6. A `columna` (*column*) in `NA` marks a finding about the
whole table. Rows 1 and 12 are identical (`filas_duplicadas`), and that
is why `EVT001` appears twice in the key (`clave_no_unica`).

## From the finding to the row

We take one: the disguised missing values of `canal` (*channel*). The
`evidencia` (evidence) says what was seen; the `sugerencia`
(suggestion), what could be done.

``` r

canal <- h[h$columna %in% "canal" & h$tipo_hallazgo == "faltantes_disfrazados", ]
writeLines(canal$evidencia)
#> <blanco> (1); S/D (1)
writeLines(strwrap(canal$sugerencia, 88))
#> Normalizar estas representaciones a NA conservando su significado si fuera necesario.
```

The `trazabilidad` (traceability) says in which rows it is, with the
values of the key we declared.

``` r

eventos <- canal$trazabilidad[[1]]$claves$id_evento
eventos
#> [1] "EVT005" "EVT011"
```

With those identifiers we look up the events in the source system,
without opening the whole table. Here we can check them (with quotes, so
that the empty cell shows):

``` r

filas <- datos_operativos$id_evento %in% eventos
print(datos_operativos[filas, c("id_evento", "canal")], quote = TRUE)
#>    id_evento canal
#> 5   "EVT005" "S/D"
#> 13  "EVT011"    ""
```

Without `clave`, the traceability carries only the row numbers, which
work inside R but not in the source system.

## ⚠️ What could not be evaluated

A check that found nothing is not the same as a check that did not run.
The second case stays in
[`cobertura()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
(*coverage*), with the reason and how to solve it.

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

The `999999` of `monto` (*amount*) is not among the findings, and not
because it is fine. The search for extreme values did not run on that
column:

``` r

motivo <- no_evaluados$motivo[
  no_evaluados$diagnostico == "outliers" & no_evaluados$columna == "monto"
]
writeLines(strwrap(motivo, 88))
#> No se evaluaron los límites de Tukey: hay 13 valores finitos y se requieren al menos
#> 20. Reunir más observaciones o declarar otro criterio antes de interpretar outliers.
```

What could not be measured stays on record; it never counts as zero.

The odd formats of `codigo_usuario` (`USR0003` and `MAL-5`) have no
finding either, for another reason: that check did run. Each format
takes 1 row out of 13. By default, a format is rare when it stays below
5% of the values (`umbral_patron_raro`, the rare-pattern threshold). In
a longer table it would be flagged.

## 📄 A report to share

[`reportar()`](https://sebollin.github.io/lupa/reference/reportar.md)
(*report*) writes a single HTML file. It opens in any browser and needs
no network and no other programs.

``` r

archivo <- file.path(tempdir(), "calidad-de-la-entrega.html")
reportar(perfil, archivo = archivo, titulo = "Calidad de la entrega")
```

The report has these sections: Resumen general; Hallazgos por severidad;
Resumen por columna; Clasificación de posibles datos personales;
Cobertura del análisis; Cobertura de diagnósticos; Patrones de formato;
Formatos de fecha; Dependencias funcionales.
[`reportar()`](https://sebollin.github.io/lupa/reference/reportar.md)
returns the path of the file; in an interactive session,
`browseURL(archivo)` opens it in the browser. It also accepts the full
result of
[`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md),
with the measurements and the cleaning plan: we see it in [3. Explorar
columnas con
`analizar()`](https://sebollin.github.io/lupa/articles/explorar-columnas.md)
(*Exploring columns with
[`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md)*,
in Spanish).

## Cleaning a column

`lupa` does not change the data it profiles. To clean, it proposes an
editable plan;
[`aplicar()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)
(*apply*) runs on a copy only the actions we mark.

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

For `canal`, the plan recommends and leaves marked 2 actions:

- turning into `NA` the missing values written as text;
- trimming the spaces at the edges.

Between lower case, upper case or a dictionary it does not choose: that
decision is ours. Each action carries its justification, which also says
what is worth confirming:

``` r

justificacion <- plan$justificacion[
  plan$columna %in% "canal" & plan$estrategia == "convertir_ausencias_textuales"
]
writeLines(strwrap(justificacion, 88))
#> Las representaciones textuales del catálogo son marcadores habituales de ausencia. El
#> cambio no es reversible: confirmar que ninguno de esos textos sea un valor legítimo de
#> la columna -`NA` es el código de Namibia, `NULL` puede ser un apellido-.
```

For this guide we apply only those 2 actions.

``` r

plan$aplicar <- plan$columna %in% "canal" & plan$recomendada
limpio <- aplicar(plan, datos_operativos)
limpio$registro[, c("columna", "estrategia", "n_cambiadas", "estado")]
#>   columna                    estrategia n_cambiadas    estado
#> 1   canal convertir_ausencias_textuales           2 ejecutada
#> 2   canal             recortar_espacios           1 ejecutada
```

We compare the values before and after in the rows that changed:

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

The `antes` (before) column was read from `datos_operativos` after
applying the plan: the original stays as it was. `"Web"` and `"APP"`
stayed the same, because we did not choose any capitalization
convention. The full plan, with its groups of actions and its log, is in
[6. Limpiar con un
plan](https://sebollin.github.io/lupa/articles/limpiar-con-un-plan.md)
(*Cleaning with a plan*, in Spanish).

## Summary

- [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  reads the table without modifying it; its printout counts the findings
  by severity and what could not be evaluated.
- [`hallazgos()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
  is a table: we start with `severidad == "error"`.
- With `clave`, the traceability of each finding carries the identifiers
  to go to the source system.
- [`cobertura()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
  lists the checks that did not run: a missing finding does not prove
  that the data is fine.
- [`reportar()`](https://sebollin.github.io/lupa/reference/reportar.md)
  saves an HTML report;
  [`planificar_limpieza()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)
  (*plan the cleaning*) and
  [`aplicar()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)
  clean a copy with the actions we choose.

## 🧭 Next step

[2. Leer un
perfil](https://sebollin.github.io/lupa/articles/leer-un-perfil.md)
(*Reading a profile*, in Spanish;
[`vignette("leer-un-perfil")`](https://sebollin.github.io/lupa/articles/leer-un-perfil.md))
explains the severities, each field of a finding, the keys and the
vocabulary of the package.
