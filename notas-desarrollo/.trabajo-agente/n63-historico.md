# N63 — histórico independiente del locale

## Alcance

Se corrigieron tres puntos relacionados en los archivos autorizados:

- `R/historico.R`: el desempate de corridas con la misma fecha ordena primero
  por fecha y luego por `.orden_seguro(.clave_bytes(id_medicion))`; las claves
  compuestas de agrupación se construyen con `.clave_bytes()` y el `rbind`
  final no recibe los nombres internos de `split()`, que eran los que se
  convertían a nativo bajo `C`.
- `R/modelo-calidad.R`: la única huella decisoria que usaba `encodeString()`
  ahora usa `.clave_bytes()`. Los otros 14 usos de `encodeString()` del
  paquete, que son texto publicado, no se tocaron.
- La comparación de registros repetidos del histórico normaliza sus columnas
  de texto con `.clave_bytes()` sólo para decidir igualdad; no cambia el texto
  almacenado que se publica.

Los comentarios roxygen se actualizaron y `roxygen2::roxygenise(".")` regeneró
únicamente `man/detectar_deriva_calidad.Rd` y `man/historico_calidad.Rd`.
`NEWS.md` recibió un párrafo propio al final de la sección en curso.

## Guiones de reproducción

Todos los comandos se ejecutaron desde la raíz del paquete, con rutas relativas.

- Guardado: `notas-desarrollo/.trabajo-agente/repro-n63-guardar.R`
- Comparación: `notas-desarrollo/.trabajo-agente/repro-n63-comparar.R`
- Acumulación de control: `notas-desarrollo/.trabajo-agente/repro-n63-acumular.R`

El fixture usa `rawToChar(charToRaw(...))` para los textos no ASCII y fija la
fecha de ambas corridas en `2026-09-17 12:00:00 UTC`. El proceso que compara
silencia únicamente el aviso de importación de `readRDS()` sobre texto no
representable y captura por separado los avisos de
`detectar_deriva_calidad()`.

## Salidas enfrentadas

Antes del arreglo, sobre el mismo `baseline-u8.rds` y con el código original:

```text
es_UY.UTF-8: año_1 -> Zebra_2, delta=-0.25, deterioro, error, avisos=0
C:           Zebra_2 -> año_1, delta=0.25, mejora, ok, avisos=1
             unable to translate 'B<U+00E1>sico\034personas' to native encoding
```

Después, sobre el mismo `final-u8.rds`:

```text
es_UY.UTF-8: Zebra_2 -> año_1, delta=0.25, mejora, ok, avisos=0
C:           Zebra_2 -> año_1, delta=0.25, mejora, ok, avisos=0
```

La huella de configuración también cruza `saveRDS()` en ambas direcciones:

```text
C leyendo guardado en UTF-8:       ok=TRUE, filas=4, avisos=0
es_UY.UTF-8 leyendo guardado en C: ok=TRUE, filas=4, avisos=0
```

## Pruebas

Se agregó `tests/testthat/test-N63-historico-locale.R` y su helper. Cada caso
crea el RDS en un proceso `Rscript` y lo lee/compara en otro; no se construyen
ambos lados en la misma sesión.

```text
N63-historico-locale:       PASS 56
historico-deriva:           PASS 210
N61-defectos-publicados:    PASS 73
N62-defectos:               PASS 77
desempate-independiente...: PASS 6
locale:                     PASS 91, SKIP 1 (no hay locale turco UTF-8)
```

La prueba nueva cubre las dos direcciones de `acumular_historico()`, los casos
`u8→u8` y `C→C`, la comparación de los dos perfiles persistidos y ocho cruces
de `comparar_perfiles()`, todos con cero avisos en la operación comprobada.

No se ejecutaron `R CMD INSTALL` ni la suite completa, tal como pide el
encargo. El árbol tenía además cambios concurrentes en archivos fuera de esta
lista; se conservaron y no se incluyeron en este trabajo.
