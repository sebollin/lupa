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

El estado `probado` se toma de la verificación que corre
`benchmark/verificar_motor.R` en el repositorio del paquete; el artículo
"Motores y escala" del sitio publica, por motor, la versión del
servidor, el controlador, la fecha y el resultado de cada comprobación.
`esperado` significa que el dialecto está implementado pero no se
comprobó contra ese motor real en esa verificación. Los motores que no
cubre no se presentan como probados.

Ante una conexión abierta,
[`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md)
y
[`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md)
piden el paquete del **controlador con que se abrió**, que reconocen por
la clase de la conexión y el paquete que la define, no por el motor que
informa el servidor: un MySQL abierto con
[`RMariaDB::MariaDB()`](https://rmariadb.r-dbi.org/reference/dbConnect-MariaDBDriver-method.html)
usa RMariaDB y no pide ni carga RMySQL, cuya clase se llama igual y
rompería la conexión.

## Examples

``` r
# El catálogo completo: una fila por variante documentada.
todos <- requisitos_motor()
nrow(todos)
#> [1] 12

# Lo que hace falta para un motor en particular, antes de intentar conectarse.
requisitos_motor("postgresql")[, c("paquete_r", "estado_paquete_r")]
#>   paquete_r estado_paquete_r
#> 3 RPostgres        instalado

# `no_comprobada` en la biblioteca del sistema no significa instalada: el
# paquete no puede saberlo sin intentarlo, y lo dice en vez de suponerlo.
unique(requisitos_motor()$estado_biblioteca_sistema)
#> [1] "no_requerida"  "no_comprobada"
```
