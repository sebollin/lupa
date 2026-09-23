# Perfilar una tabla por grupos de filas

Aplica
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md) a
cada grupo de filas por separado y devuelve los hallazgos de todos los
grupos en una sola tabla. Es la respuesta al formato largo: una tabla
donde cada fila describe un atributo distinto no es una tabla, son
muchas apiladas, y perfilarla como si fuera una sola mezcla dominios que
no tienen nada que ver entre sí.

## Usage

``` r
perfilar_por(datos, por, clave = NULL, min_filas = 30L, ...)
```

## Arguments

- datos:

  Data frame a perfilar.

- por:

  Nombre de una columna atómica cuyos valores definen los grupos. Los
  ausentes forman un grupo propio, con la etiqueta `"(ausente)"`. Si la
  columna trae ese mismo texto como valor real, los dos caen en un solo
  grupo —no se pierde ninguna fila— y la colisión se declara en
  `cobertura_grupos`, con cuántas filas aporta cada una.

- clave:

  Nombres de columnas de identidad que se conservan en cada grupo aunque
  estén enteramente ausentes. Importa: sin la clave de entidad, el
  diagnóstico de filas duplicadas informa como duplicada cada repetición
  del valor del atributo.

  **La declaración viaja al perfilado de cada grupo**, igual que si se
  pasara `clave` a
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md):
  la ley de Benford no corre sobre esas columnas y la cobertura publica
  que la clave fue declarada, en vez de deducir que «parece un
  identificador». Si la clave nombra la columna de agrupación, ese
  nombre se recorta antes de reenviarlo, porque en la rebanada esa
  columna ya no está.

- min_filas:

  Grupos con menos filas que este número no se perfilan y se declaran en
  la cobertura. El valor por omisión evita conclusiones sobre grupos
  donde ningún diagnóstico tiene soporte.

- ...:

  Argumentos enviados a
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  para cada grupo. Los que acotan el trabajo —`muestra` entre ellos— se
  aplican **dentro de cada grupo**, no sobre la tabla entera: con
  `muestra = 100` y grupos de 500 filas, los diagnósticos que muestrean
  miran 100 de cada grupo. El alcance viaja en la evidencia de cada
  hallazgo, que declara sobre cuántos valores se midió; `n_filas_grupo`
  sigue siendo el tamaño del grupo, no el de la muestra.

## Value

Data frame de clase `hallazgos_por_grupo` con las columnas de
`hallazgos` de
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
precedidas por `grupo` y `n_filas_grupo`. El atributo `cobertura_grupos`
declara los grupos no perfilados, las columnas enteramente ausentes y
las declaraciones recortadas por grupo.

El atributo `etiquetas_personales` declara si la columna de agrupación
lleva datos personales. Las etiquetas de grupo **son** valores de esa
columna, así que la salida los publica —en los hallazgos y en las dos
tablas de cobertura— aunque
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
enmascare esa misma columna. No se enmascaran aquí porque la etiqueta es
el eje del resultado y sin ella los grupos no se distinguen; pero
tampoco ocurre en silencio: se avisa al ejecutar y queda declarado en el
objeto. Para que no se publiquen, agrupe por una columna seudonimizada.
El atributo queda vacío cuando la columna no lleva datos personales, y
también cuando `proteger_datos_personales` es `FALSE`, porque entonces
ya está declarado que se quieren los valores. Si el léxico no reconoce
el nombre de la columna, decláresela con `columnas_personales`: ese
argumento —como `columnas_opcionales`, `clave`, `columnas_sin_ceros`,
`columnas_no_negativas` y `aplicabilidad`— puede nombrar la columna de
agrupación, y se recorta de lo que se envía a
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
para cada grupo, donde esa columna ya no está. El recorte se publica por
grupo en `cobertura_grupos$columnas_descartadas`, junto con el nombre de
la declaración y el motivo. Para `aplicabilidad`, también se recorta una
regla cuando alguna variable de su fórmula —según
[`all.vars()`](https://rdrr.io/r/base/allnames.html)— es una columna
original ausente de la rebanada; las referencias que no son columnas
originales se conservan como referencias posibles al entorno de la
fórmula.

El atributo `cobertura_diagnosticos` declara, **por grupo**, los
diagnósticos que no se evaluaron y por qué. Cada grupo se perfila por
separado, así que cada uno declina los suyos: una columna puede tener
bastantes filas en un grupo y muy pocas en otro. Sin esa tabla, un grupo
sin hallazgos se lee como un grupo sano, cuando puede ser un grupo sobre
el que no se miró.

Las senales de numeracion densa y de identificacion se miden tambien
sobre columnas `integer64` con comparaciones exactas. Si un grupo vuelve
a conjeturar un centinela por la particion, el hallazgo se mueve a esta
cobertura y no se publica como defecto del grupo.

## Details

Dentro de cada grupo se descartan las columnas enteramente ausentes
antes de perfilar. En un modelo entidad-atributo-valor bien formado eso
deja viva exactamente la columna de valor que corresponde al atributo
del grupo, y es lo que evita informar como falta lo que es la forma del
dato. Las declaraciones que nombran columnas retiradas se recortan para
ese grupo y el nombre de cada recorte, con su motivo, se declara en
`cobertura_grupos$columnas_descartadas`. En `aplicabilidad` también se
recorta una regla si
[`all.vars()`](https://rdrr.io/r/base/allnames.html) de su fórmula
menciona una columna original que no está en la rebanada; una variable
que no es columna de la tabla se conserva porque puede venir
legítimamente del entorno de la fórmula.

La función no adivina cuál es la columna de agrupación: la declara quien
conoce el dato, igual que
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md) no
adivina claves ni jerarquías.

## See also

[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
[`cobertura_analisis()`](https://sebollin.github.io/lupa/reference/cobertura_analisis.md)

## Examples

``` r
largo <- data.frame(
  entidad = rep(1:40, each = 2),
  atributo = rep(c("pais", "edad"), 40),
  valor = c(rbind(sample(c("UY", "AR"), 40, TRUE), as.character(20:59)))
)
hallazgos <- perfilar_por(largo, "atributo", clave = "entidad", min_filas = 10)
head(hallazgos[, c("grupo", "columna", "tipo_hallazgo")])
#>   grupo columna         tipo_hallazgo
#> 1  pais entidad posible_identificador
#> 2  edad entidad posible_identificador
#> 3  edad   valor posible_identificador
```
