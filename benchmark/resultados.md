# Resultado publicado

- Fecha de la corrida: 2026-08-17
- Commit de `lupa`: `137197c`
- Fuente: rama `master` de `BigDaMa/raha`, mediante las URL construidas por
  `verdad_raha.R`

## Instalación de `lupa` medida

| Paquete | Versión | Built |
| --- | --- | --- |
| `lupa` | `0.1.0` | `R 4.6.1; ; 2026-08-17 10:27:31 UTC; unix` |

## Raha: comparación celda a celda de dirty contra clean

| Dataset | Dimensión | Celdas diferentes | Filas afectadas | Tasa |
| --- | ---: | ---: | ---: | ---: |
| hospital | 1000 x 20 | 509 | 407 | 2,545 % |
| flights | 2376 x 7 | 4920 | 1904 | 29,582 % |
| beers | 2410 x 11 | 4362 | 2410 | 16,454 % |

## Cobertura por columna

| Dataset | Columnas con al menos una celda cambiada | Señaladas por `lupa` |
| --- | ---: | ---: |
| hospital | 17 de 20 | 19 |
| flights | 4 de 7 | 6 |
| beers | 5 de 11 | 9 |
| **Total** | **26** | **34** |

Las 26 columnas afectadas recibieron al menos un hallazgo. Las ocho columnas
adicionales también sostienen observaciones verdaderas, según la revisión
manual descrita en [`README.md`](README.md). Esto mide cobertura por columna,
no recall diagnóstico ni correspondencia entre celdas.

## Versión de los archivos obtenidos

Las huellas SHA-256 se calcularon sobre los bytes descargados. Permiten detectar
un cambio posterior en la rama `master` de origen.

| Dataset | Archivo | Bytes | Adler-32 | SHA-256 |
| --- | --- | ---: | --- | --- |
| hospital | dirty.csv | 303306 | `f970da05` | `dbc5575b915fe8b5e0ac6dc6172f38ba91e611fdb76d09a8f4a81cb7ea9925ac` |
| hospital | clean.csv | 303324 | `74435416` | `ea3ee44998455c0b491750c348509de176c758a3bbf58e4530c0a136bb248b4b` |
| flights | dirty.csv | 154776 | `d3a57255` | `1b5c1afa10aa0e7c20fd7e14d05c56772715b2771aa0f5fa67ed1709e1eecd46` |
| flights | clean.csv | 173159 | `ffe72679` | `0acfcfd8985b06fdd363965c9e8d9522c43e7589a93d79ae7dc311e1c37fdf3b` |
| beers | dirty.csv | 255295 | `b2bb7910` | `7110bf4931a9445a1675e544d6c996817c739136239f8a2b02e088c7ec0a1f68` |
| beers | clean.csv | 233019 | `71980a46` | `373227df59ad197e154dd5149125789e415019535c7223355e9486ee1b3b93de` |

El script calcula el tamaño y la huella Adler-32 exclusivamente con R base; la
tabla los conserva junto con el SHA-256 de auditoría de la corrida publicada.
