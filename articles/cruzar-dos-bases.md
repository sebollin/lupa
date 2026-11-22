# 13. Cruzar dos bases

Un padrón vive en una base SQLite. Las visitas, en una base DuckDB. La
credencial que abre una no abre la otra, y ninguna consulta puede
nombrar las dos a la vez. ¿Cómo se sabe si cada visita encuentra a su
persona? ¿Y cómo se convierte esa pregunta en una medida que se repita
con cada entrega?

> **En esta guía:**
>
> - perfilar cada tabla en su motor con
>   [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md);
> - leer sólo las claves, por bloques, con
>   [`DBI::dbSendQuery()`](https://dbi.r-dbi.org/reference/dbSendQuery.html)
>   y [`DBI::dbFetch()`](https://dbi.r-dbi.org/reference/dbFetch.html);
> - comparar la cobertura en R con
>   [`detectar_relaciones()`](https://sebollin.github.io/lupa/reference/detectar_relaciones.md);
> - convertir la comparación en una métrica con
>   [`medir()`](https://sebollin.github.io/lupa/reference/medir.md) y
>   `ReglaIntegridadInterEntidad`;
> - decidir qué hacer cuando las claves no tienen el mismo tipo o la
>   misma forma.

## Dos bases, una pregunta

El ejemplo tiene 8 personas en el padrón y 20 visitas. Las visitas
guardan la persona como texto de 2 cifras, con cero a la izquierda. Dos
visitas citan a las personas 9 y 10, que no están en el padrón. Todo es
sintético.

``` r

library(lupa)

padron <- data.frame(
  id_persona = 1:8,
  sector = rep(c("Norte", "Sur"), each = 4),
  stringsAsFactors = FALSE
)
visitas <- data.frame(
  id_visita = 1:20,
  id_persona = sprintf("%02d", c(rep(1:8, 2), 1, 2, 9, 10)),
  motivo = rep(c("control", "consulta"), length.out = 20),
  stringsAsFactors = FALSE
)
head(visitas, 3)
#>   id_visita id_persona   motivo
#> 1         1         01  control
#> 2         2         02 consulta
#> 3         3         03  control
```

Las tablas se escriben en bases en memoria. Es sólo la preparación del
ejemplo: `lupa` no escribe en la base.

``` r

con_padron <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
DBI::dbWriteTable(con_padron, "padron", padron)
con_visitas <- suppressMessages(DBI::dbConnect(duckdb::duckdb()))
DBI::dbWriteTable(con_visitas, "visitas", visitas)
```

## Perfilar cada tabla en su motor

Cada tabla se perfila con la conexión que la ve.
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
devuelve los 2 bloques de [Perfilar una
base](https://sebollin.github.io/lupa/articles/perfilar-una-base.md): el
resumen que calcula el motor y el perfil de las filas que se leyeron.

``` r

perfil_padron <- perfilar_dbi(con_padron, "padron")
perfil_visitas <- perfilar_dbi(con_visitas, "visitas")
```

Para el cruce importa una sola cosa del perfil: cómo está guardada la
clave. El tipo declarado lo dice el motor; el inferido, las filas que se
leyeron.

``` r

clave_de <- function(perfil, columna) {
  fila <- perfil$perfil_muestra$columnas
  fila[
    fila$columna == columna,
    c("columna", "tipo_declarado", "tipo_inferido", "n", "n_distintos")
  ]
}
rbind(
  cbind(tabla = "padron", clave_de(perfil_padron, "id_persona")),
  cbind(tabla = "visitas", clave_de(perfil_visitas, "id_persona"))
)
#>     tabla    columna tipo_declarado tipo_inferido  n n_distintos
#> 1  padron id_persona         entero        entero  8           8
#> 2 visitas id_persona          texto        entero 20          10
```

El motor del padrón declara `id_persona` como entero. El de las visitas
la declara texto, aunque el perfil, al leerla, infiere entero. No es un
error del perfil: es la primera señal de que la clave no está guardada
igual de los dos lados. El perfil de las visitas lo anota como hallazgo:

``` r

hallazgos(perfil_visitas)[
  hallazgos(perfil_visitas)$columna == "id_persona",
  c("columna", "tipo_hallazgo", "severidad")
]
#>      columna           tipo_hallazgo  severidad
#> 2 id_persona tipo_declarado_distinto sospechoso
```

`tipo_declarado_distinto` no dice que haya un valor malo; dice que el
texto se parece a un número. En el padrón, la misma columna es entera.
Ahí empieza el problema del cruce.
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
no compara las dos tablas: cada llamada ve una sola conexión.

## Leer sólo las claves

Para comparar no hacen falta las filas enteras: alcanzan las claves. Y
conviene leerlas por bloques, para que el pico de memoria no dependa del
tamaño de la tabla sino del bloque. El paquete no exporta un lector de
claves; se arma con
[`DBI::dbSendQuery()`](https://dbi.r-dbi.org/reference/dbSendQuery.html)
y [`DBI::dbFetch()`](https://dbi.r-dbi.org/reference/dbFetch.html), las
mismas funciones con que
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
lee del motor.

``` r

leer_por_bloques <- function(conexion, consulta, bloque = 1000) {
  resultado <- DBI::dbSendQuery(conexion, consulta)
  on.exit(DBI::dbClearResult(resultado), add = TRUE)
  partes <- list()
  repeat {
    parte <- DBI::dbFetch(resultado, n = bloque)
    if (!nrow(parte)) break
    partes[[length(partes) + 1L]] <- parte
  }
  if (length(partes)) do.call(rbind, partes) else parte
}
```

Acá el bloque es de 10 filas para que se vea el recorrido; en una tabla
real, elegí el tamaño por la memoria disponible, no por la cantidad de
filas. Se lee el identificador de la visita junto con la clave: después
sirve para ubicar las filas huérfanas.

``` r

claves_padron <- leer_por_bloques(
  con_padron, "SELECT id_persona FROM padron ORDER BY id_persona",
  bloque = 10
)
claves_visitas_crudas <- leer_por_bloques(
  con_visitas, "SELECT id_visita, id_persona FROM visitas ORDER BY id_visita",
  bloque = 10
)
data.frame(
  lado = c("padron", "visitas"),
  clase = c(
    class(claves_padron$id_persona),
    class(claves_visitas_crudas$id_persona)
  ),
  ejemplo = c(
    claves_padron$id_persona[[1]],
    claves_visitas_crudas$id_persona[[1]]
  )
)
#>      lado     clase ejemplo
#> 1  padron   integer       1
#> 2 visitas character      01
```

El padrón llega con 8 claves enteras; las visitas, con 20 claves de
texto. La primera del padrón es `1`; la primera de las visitas, `"01"`.
A la vista son la misma persona; para la comparación, todavía no.

## Comparar en R

[`detectar_relaciones()`](https://sebollin.github.io/lupa/reference/detectar_relaciones.md)
compara 2 data frames en memoria. La primera tabla es la referencia; la
segunda, la que depende de ella. `columnas_candidatas` dice qué par
comparar: sin eso, prueba todas las combinaciones y puede encontrar
coincidencias que son sólo códigos parecidos.

``` r

cruce_crudo <- detectar_relaciones(
  claves_padron, claves_visitas_crudas,
  columnas_candidatas = list("id_persona", "id_persona")
)
cruce_crudo[, c(
  "cardinalidad", "n_valores_comunes", "cobertura_tabla2_en_tabla1",
  "n_filas_huerfanas_tabla2"
)]
#>        cardinalidad n_valores_comunes cobertura_tabla2_en_tabla1 n_filas_huerfanas_tabla2
#> 1 sin_coincidencias                 0                          0                       20
```

La cobertura de las visitas en el padrón es 0 %: ninguna de las 20
claves de visita coincide con las 8 del padrón. No es que falten todas
las personas: es que `"01"` y `1` no son el mismo valor para la
comparación.

## Arreglar la forma en el origen

El cero a la izquierda es relleno, no parte del código. Se corrige en la
consulta, antes de que el valor llegue a R: así la clave viaja con el
tipo que se va a comparar y la receta queda escrita en un solo lugar.
`BIGINT` es el nombre de DuckDB para el entero; cada motor tiene el
suyo.

``` r

claves_visitas <- leer_por_bloques(
  con_visitas,
  paste(
    "SELECT id_visita, CAST(id_persona AS BIGINT) AS id_persona",
    "FROM visitas ORDER BY id_visita"
  ),
  bloque = 10
)
cruce <- detectar_relaciones(
  claves_padron, claves_visitas,
  columnas_candidatas = list("id_persona", "id_persona")
)
cruce[, c(
  "cardinalidad", "n_valores_comunes", "cobertura_tabla2_en_tabla1",
  "n_filas_huerfanas_tabla2"
)]
#>   cardinalidad n_valores_comunes cobertura_tabla2_en_tabla1 n_filas_huerfanas_tabla2
#> 1          1:m                 8                        0.9                        2
```

Ahora la cobertura es 90 %: 18 de las 20 visitas encuentran su persona.
Quedan 2 visitas huérfanas, las de las personas 9 y 10, que no están en
el padrón.

``` r

huerfanas <- cruce$huerfanas_tabla2[[1]]
data.frame(
  total = huerfanas$total,
  mostradas = huerfanas$mostrados,
  estado = huerfanas$estado,
  alcance = huerfanas$alcance
)
#>   total mostradas     estado  alcance
#> 1     2         2 disponible completo
claves_visitas$id_visita[huerfanas$indices_fila]
#> [1] 19 20
```

La traza no publica los valores de las claves; publica las posiciones de
las filas. Con `id_visita` leído al lado, esas posiciones se traducen a
las visitas 19 y 20. En una tabla grande, el orden de lectura tiene que
ser estable para que la posición siga sirviendo: por eso la consulta
lleva `ORDER BY`.

## De la comparación a la métrica

Una cobertura suelta sirve para esta entrega. Para comparar entregas
hace falta una medida con nombre, fecha e identificador.
`ReglaIntegridadInterEntidad` mide la proporción de claves no ausentes
de la segunda entidad que existen en la primera.

``` r

nucleo <- metricas_nucleo()
regla <- instanciar(
  especializar(
    nucleo$ReglaIntegridadInterEntidad,
    nombre_especifico = "VisitaTienePersona"
  ),
  entidad = c("padron", "visitas"),
  atributos = c("id_persona", "id_persona")
)
medicion <- medir(
  modelo(regla),
  list(padron = claves_padron, visitas = claves_visitas),
  id_medicion = "cruce-2026-01",
  fecha = "2026-01-31"
)
medicion[, c("metrica_especifica", "objeto_medible", "resultado")]
#>   metrica_especifica                          objeto_medible resultado
#> 1 VisitaTienePersona visitas$id_persona -> padron$id_persona       0.9
```

La métrica devuelve una sola fila: la cobertura de la clave foránea, con
el nombre de la regla, la entidad dependiente y el objeto medido. El
`id_medicion` y la `fecha` son los que permiten comparar esta corrida
con la próxima. Un `NA` en la clave foránea no cuenta como huérfano: no
es un valor que se haya buscado, y por eso no entra en el denominador.

La métrica no normaliza nada. Si se le pasan las claves crudas, mide 0
%, que es el dato correcto para esa forma. La corrección se hace antes,
en la consulta, y se aplica a los dos lados.

## Cuando las claves no tienen la misma forma

El caso de las visitas era un cero a la izquierda. Hay otros dos que
aparecen seguido: mayúsculas y espacios de borde. La comparación en
memoria no los ignora.

``` r

codigos_a <- data.frame(codigo = c("A-01", "B-02", " C-03"))
codigos_b <- data.frame(codigo = c("a-01", "b-02", "c-03"))
comparar <- function(a, b) {
  detectar_relaciones(
    a, b, columnas_candidatas = list("codigo", "codigo")
  )$n_valores_comunes
}
c(
  crudo = comparar(codigos_a, codigos_b),
  normalizado = comparar(
    data.frame(codigo = toupper(trimws(codigos_a$codigo))),
    data.frame(codigo = toupper(trimws(codigos_b$codigo)))
  )
)
#>       crudo normalizado 
#>           0           3
```

Texto contra número no es un problema de clases, sino de escritura. Un
entero y un doble con el mismo valor entero coinciden, y un texto `"1"`
coincide con el número `1`; pero `"01"`, `"1.0"` o `" 1"` no. La
comparación usa la identidad exacta del valor: no recorta espacios ni
cambia mayúsculas. Con fechas pasa lo mismo: `"2026-01-31"` coincide con
un `Date`, pero `"31/01/2026"` no.

La decisión de cuál es la forma canónica es de los datos, no del
paquete. Si el cero a la izquierda es relleno, quitalo al leer; si es
parte del código, reponelo en los dos lados. Si las mayúsculas no
distinguen, normalizá las dos tablas con la misma función; si
distinguen, normalizarlas sería un error. Y si los espacios son
accidentales, recortalos en los dos lados. Lo que no podés hacer es
corregir un solo lado y llamar al resultado cobertura.

## Lo que el paquete no hace

No une dos conexiones en una sola consulta.
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md),
[`medir_dbi()`](https://sebollin.github.io/lupa/reference/medir_dbi.md),
[`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md)
y
[`relaciones_coleccion()`](https://sebollin.github.io/lupa/reference/relaciones_coleccion.md)
reciben una conexión o una colección;
[`detectar_relaciones()`](https://sebollin.github.io/lupa/reference/detectar_relaciones.md)
y [`medir()`](https://sebollin.github.io/lupa/reference/medir.md)
comparan data frames en R. La frontera es esa: cada motor perfila y
proyecta lo suyo, y R junta las claves.

Si las dos tablas estuvieran en la misma base,
[`medir_dbi()`](https://sebollin.github.io/lupa/reference/medir_dbi.md)
podría resolver la regla en el motor, con la vía
`sql_dedicado_referencial`. Con dos conexiones no hay una consulta que
las vea a las dos, y por eso la receta trae las claves. Esa lectura
acotada también cuida los datos: no viajan las filas completas, sólo los
identificadores que hacen falta para comparar.

Tampoco normaliza por vos: la comparación es exacta. Y no decide si una
relación es una clave foránea de verdad. La cobertura es evidencia; el
diccionario de datos y las restricciones del motor son los que
confirman. La cobertura se calcula sobre todas las claves que se le
pasan: no hay una muestra implícita que pueda alinearse con los datos.
Si se lee un subconjunto, la cobertura es de ese subconjunto y hay que
decirlo.

Las dos lecturas son de momentos distintos. Si las bases cambian entre
una y otra, la cobertura mezcla dos fotos: conviene leer las claves lo
más cerca posible y anotar la fecha de la corrida, como se hace con la
métrica.

## Resumen

- [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  describe cada tabla en su motor. El tipo declarado de la clave y el
  hallazgo `tipo_declarado_distinto` avisan si los dos lados no están
  escritos igual.
- Leer sólo las claves y por bloques acota el pico de memoria:
  `dbSendQuery()` + `dbFetch(n)` recorren la tabla sin traer las filas
  completas.
- [`detectar_relaciones()`](https://sebollin.github.io/lupa/reference/detectar_relaciones.md)
  mide la cobertura en las 2 direcciones y nombra las filas huérfanas;
  [`medir()`](https://sebollin.github.io/lupa/reference/medir.md) con
  `ReglaIntegridadInterEntidad` la convierte en una medida con fecha e
  identificador.
- La comparación es exacta: ceros a la izquierda, mayúsculas y espacios
  son valores distintos. Normalizar los dos lados con la misma regla, o
  corregir el tipo en la consulta, es parte de la receta.
- El paquete no cruza dos conexiones en una consulta: cada base se lee
  por separado y R es el punto de encuentro.

## 🧭 Paso siguiente

Para elegir el tamaño de los bloques y entender qué dialectos y costos
tiene cada motor, [Motores y
escala](https://sebollin.github.io/lupa/articles/motores-y-escala.md)
reúne el detalle.

``` r

DBI::dbDisconnect(con_padron)
DBI::dbDisconnect(con_visitas, shutdown = TRUE)
```
