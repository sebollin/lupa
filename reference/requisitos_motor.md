# Consultar requisitos para conectarse a motores de bases

Devuelve el catálogo de paquetes R, bibliotecas del sistema, dialectos y
estado de prueba que `lupa` conoce. La presencia del paquete R se
comprueba en la máquina actual. La biblioteca del sistema no se declara
instalada sin una comprobación específica: cuando corresponde, la
columna lo deja como `no_comprobada` y conserva las rutas de resolución
posibles.

## Usage

``` r
requisitos_motor(motor = NULL)
```

## Arguments

- motor:

  `NULL` para devolver el catálogo completo, o el nombre de un motor,
  como `"oracle"`, `"postgresql"` o `"sql server"`.

## Value

Un `data.frame` de clase `requisitos_motor`, con una fila por variante
documentada. Incluye el estado comprobado del paquete R y un estado
explícito para la biblioteca del sistema: `no_comprobada` no significa
instalada.

## Details

El estado `probado` se copia de la tabla de motores de los README.
`esperado` significa que el dialecto está implementado pero no se
comprobó contra ese motor real en este repositorio. Los motores que no
figuran en esa tabla no se presentan como probados.
