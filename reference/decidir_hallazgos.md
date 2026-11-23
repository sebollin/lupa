# Decidir sobre hallazgos: aceptarlos o descartarlos con su motivo

Registra qué se resolvió de uno o varios hallazgos, con el motivo, quién
y cuándo, en una tabla que se guarda en un archivo editable
([`guardar_decisiones()`](https://sebollin.github.io/lupa/reference/guardar_decisiones.md))
y se aplica en la corrida siguiente
([`aplicar_decisiones()`](https://sebollin.github.io/lupa/reference/aplicar_decisiones.md)).

## Usage

``` r
decidir_hallazgos(
  x,
  decision = c("aceptado", "falso_positivo", "propiedad_legitima"),
  motivo,
  quien,
  id_hallazgo = NULL,
  tabla = NULL,
  columna = NULL,
  tipo_hallazgo = NULL,
  fecha = Sys.time(),
  decisiones = NULL,
  datos = NULL,
  nombre_tabla = NULL
)
```

## Arguments

- x:

  Resultado de
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
  [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md),
  [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md),
  [`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md)
  o
  [`perfilar_por()`](https://sebollin.github.io/lupa/reference/perfilar_por.md).

- decision:

  Una de `"aceptado"`, `"falso_positivo"` o `"propiedad_legitima"`.

- motivo:

  Texto con el motivo; no puede estar vacío.

- quien:

  Texto: quién decide.

- id_hallazgo:

  Ids de
  [`identificar_hallazgos()`](https://sebollin.github.io/lupa/reference/identificar_hallazgos.md),
  o una tabla con la columna `id_hallazgo`.

- tabla, columna, tipo_hallazgo:

  Filtros de texto (uno o varios valores).

- fecha:

  Fecha y hora de la decisión; por omisión, ahora.

- decisiones:

  Decisiones anteriores (de `decidir_hallazgos()` o
  [`leer_decisiones()`](https://sebollin.github.io/lupa/reference/guardar_decisiones.md))
  a las que se agregan las nuevas. Una decisión nueva sobre un hallazgo
  ya decidido reemplaza a la anterior.

- datos:

  La tabla que se perfiló, para la huella de los valores afectados. Un
  [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md)
  con `conservar_datos = TRUE` la trae.

- nombre_tabla:

  Nombre de la tabla que se perfiló. Obligatorio con el resultado de
  [`perfilar_por()`](https://sebollin.github.io/lupa/reference/perfilar_por.md),
  que no lo guarda; ver
  [`identificar_hallazgos()`](https://sebollin.github.io/lupa/reference/identificar_hallazgos.md).

## Value

Una tabla `decisiones_hallazgos`: una fila por hallazgo decidido, con
`id_hallazgo`, `tabla`, `grupo`, `columna`, `tipo_hallazgo`, `decision`,
`motivo`, `quien`, `fecha` y lo visto al decidir (`severidad`,
`n_afectados`, `evidencia`).

## Details

Hay tres decisiones:

- `"aceptado"`: el hallazgo es un defecto real que se reconoce y se
  acepta (no se corrige ahora, o se corrige por otra vía).

- `"falso_positivo"`: el paquete se equivocó; el dato está bien.

- `"propiedad_legitima"`: el hallazgo describe algo cierto del dato que
  no es un defecto (por ejemplo, una columna casi vacía porque así es el
  proceso).

Cada decisión guarda, además, lo que se vio al decidir: la severidad, la
cantidad de filas afectadas y una **huella del contenido afectado** que
no depende del orden de las filas. Es lo que permite, en la corrida
siguiente, saber si el hallazgo **cambió**: si cambia cualquiera de
ellas, la decisión no se aplica en silencio y el hallazgo vuelve como
`revisar`. La evidencia se guarda como referencia para quien revisa,
pero no se compara: su texto cambia con `OutDec` o con el orden de los
ejemplos.

La huella es de dos clases, y cada una necesita algo:

- de **valores**: los valores afectados. Hace falta la tabla (`datos`, o
  un
  [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md)
  con `conservar_datos = TRUE`); sin ella, la decisión guarda sólo las
  cuentas.

- de **filas**: las claves de las filas afectadas. Hace falta una clave
  (`clave` de
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  o la del catálogo): sin clave las filas sólo se nombran por su
  posición, y la posición depende del orden, así que no se hace.
  **Límite:** sin clave, un hallazgo de filas (`faltantes`,
  `filas_duplicadas`) cuyas filas cambian pero conserva la cifra no se
  detecta. Tampoco hay huella si el hallazgo localiza más filas que el
  límite de la trazabilidad.

Es un hash de las partes, de costo lineal en las filas afectadas.

En `faltantes` y `faltantes_disfrazados` se guarda además el
**detalle**: cuántas ausencias son declaradas y cuántas centinelas, y
cuáles son los centinelas numéricos (ordenados), tomados de lo que el
perfil publica por columna. Una celda que pasa de `NA` a `999`, o de
`999` a `-9`, es la misma cifra sobre las mismas filas con otro
contenido, y la decisión vuelve a `revisar`. **Límite:** los centinelas
de texto (`"N/A"`, `"NULL"`) el perfil los publica sólo como cuenta:
cambiar uno por otro con la misma cuenta se ve con `datos`, no sin
ellos. Los hallazgos que no tienen filas (`tipo_declarado_distinto`,
`columnas_duplicadas`, `nombres_columnas_problematicos`) se verifican
con una huella de la **estructura** —nombres y tipos de las columnas
involucradas—, que sale del perfil y no necesita datos.

El estado lo decide el **componente requerido del tipo de hallazgo**,
los que pueden cambiar sin que cambie la cuenta: en los hallazgos de
valores (`outliers`, dominios, patrones, `faltantes_disfrazados`, casi
duplicados...), los valores; en `faltantes`, las filas y el detalle; en
`filas_duplicadas` y otros de filas, las filas (o los valores de las
filas completas); en los estructurales, la estructura. Está escrito en
una tabla cerrada, y una prueba avisa si el paquete publica un tipo que
no está. Con todo lo requerido comparado y coincidente la decisión queda
verificada; si algún componente comparado difiere, es `revisar` (sólo
significa «cambió»); si algún requerido no se guardó al decidir, no se
pudo calcular ahora o no se puede calcular de ningún lado, es
`decidido_sin_verificar` y `verificacion` dice cuál. Un componente no
requerido que falta de un lado sólo se declara en `verificacion`. Así,
decidir con clave y aplicar con clave y `datos` verifica `faltantes`,
pero deja sin verificar a `outliers` (que necesita los valores al
decidir); y el cambio de un centinela de texto por otro con la misma
cuenta se ve sólo con `datos` en los dos lados, porque
`faltantes_disfrazados` requiere los valores. En el resultado de
[`perfilar_por()`](https://sebollin.github.io/lupa/reference/perfilar_por.md)
las filas son posiciones de la tabla: con `datos` la huella de valores
se calcula como en
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md) y
la estructura sale de los nombres y clases de sus columnas; sin `datos`,
sus hallazgos de valores quedan sin verificar. Los hallazgos que cambian
con la misma cuenta sin filas (`constante`, `casi_constante`,
`alta_cardinalidad`, `casi_clave`, `posible_identificador`) se comparan
por lo que el perfil publica de la columna: el valor constante o la
moda, su frecuencia y los distintos. Con la protección de datos
personales activa, los datos que conserva
[`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md)
están enmascarados y no se usan para la huella de valores. **Límites:**
en
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
sin `orden_muestra`, la muestra puede cambiar de filas entre corridas y
el detalle con ella; el motivo de una decisión se publica tal cual en
[`reportar()`](https://sebollin.github.io/lupa/reference/reportar.md),
también con `proteger_datos_personales`: no conviene escribir en él
valores de columnas personales.

Se elige a qué hallazgos se aplica con `id_hallazgo` (los de
[`identificar_hallazgos()`](https://sebollin.github.io/lupa/reference/identificar_hallazgos.md),
o la tabla misma filtrada) y/o con los filtros `tabla`, `columna` y
`tipo_hallazgo`, que se combinan con «y». Hace falta al menos un
criterio, y debe coincidir al menos un hallazgo.

## See also

[`aplicar_decisiones()`](https://sebollin.github.io/lupa/reference/aplicar_decisiones.md),
[`guardar_decisiones()`](https://sebollin.github.io/lupa/reference/guardar_decisiones.md),
[`identificar_hallazgos()`](https://sebollin.github.io/lupa/reference/identificar_hallazgos.md)

## Examples

``` r
perfil <- perfilar(datos_administrativos, analizar_dependencias = FALSE)
decisiones <- decidir_hallazgos(
  perfil, "propiedad_legitima",
  motivo = "La columna es opcional por diseño del formulario.",
  quien = "equipo de datos", tipo_hallazgo = "faltantes"
)
decisiones
#>                             id_hallazgo                 tabla grupo columna
#> 1 datos_administrativos||sexo|faltantes datos_administrativos  <NA>    sexo
#>   tipo_hallazgo           decision
#> 1     faltantes propiedad_legitima
#>                                              motivo           quien
#> 1 La columna es opcional por diseño del formulario. equipo de datos
#>                  fecha  severidad n_afectados
#> 1 2026-11-23T12:46:54Z sospechoso           2
#>                                             evidencia huella_valores
#> 1 0 ausentes reales y 2 disfrazados (0.154 del total)           <NA>
#>   huella_filas                                                    detalle
#> 1         <NA> reales=0;disfrazados=2;numericos=0;textuales=2;centinelas=
#>   huella_estructura
#> 1              <NA>
```
