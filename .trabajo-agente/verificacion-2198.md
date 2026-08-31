# Verificación de §2.198

Corrida contra el contenedor PostgreSQL 16.15 (`lupa-pg`) el 2026-08-31, con
`DBI` 1.3.0, `RPostgres` 1.4.10 y la misma credencial para crear las tablas,
leer `pg_class` y ejecutar `plan_perfilado_dbi()`. Se cargaron 10.000 filas en
cada tabla temporal; la primera recibió `ANALYZE` y la segunda no.

| caso | `pg_class.reltuples` | `attr(plan, "filas")` | fuente | magnitud | proyección moda | proyección mediana |
| --- | ---: | ---: | --- | --- | --- | --- |
| con `ANALYZE` | 10.000 | 10.000 | `pg_class.reltuples` | `baja` | disponible, 20.000 distintos estimados | disponible, 20.000 filas-mediana |
| sin `ANALYZE` | -1 | `NA` | `NA` | `desconocida` | no disponible | no disponible |

El primer caso se imprime como `~10.000 filas (estimacion de catalogo)` y sus
dos proyecciones se rotulan como estimaciones de catálogo, no como duraciones.
En el segundo, el motivo conserva `sin dato filas` y no supone cero.
