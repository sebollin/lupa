# Codificación y locales

El texto de una tabla llega como bytes y, a veces, con una marca de
codificación ([`Encoding()`](https://rdrr.io/r/base/Encoding.html)).
`lupa` no modifica esos datos: decide cómo mostrarlos, cómo compararlos
y, si se elige en un plan de limpieza, cómo repararlos en una copia.
Este artículo reúne esas reglas.

Los ejemplos construyen su texto a partir de bytes, con
[`rawToChar()`](https://rdrr.io/r/base/rawConversion.html), para no
depender de la codificación con que se guarde ni se lea este archivo.

## Cómo se muestra el texto

Las salidas impresas de `lupa` funcionan en cualquier locale, incluido
`LC_CTYPE = C`. Lo que se imprime es una copia del texto, preparada así:

- lo que trae una codificación declarada (`latin1`, `UTF-8`) se
  convierte;
- los bytes UTF-8 válidos que llegan sin marca se declaran UTF-8;
- el resto queda tal cual, mientras R pueda imprimirlo.

Bajo un locale UTF-8, la salida es la misma que la de R. La única
diferencia visible es para una clase propia cuyo
[`format()`](https://rdrr.io/r/base/format.html) consulte
[`Encoding()`](https://rdrr.io/r/base/Encoding.html): ese método ve la
marca de la copia.

Bajo un locale que no puede representar un carácter, la salida difiere a
propósito. Donde R escaparía los bytes, `lupa` publica el punto de
código (`<U+00F1>`). Como toda representación escapada, también las de
R, esa forma es ambigua con un texto que la contenga literalmente; lo
que cambia es cómo se muestra, no el dato. Sólo cuando R no puede
imprimir un valor, se reintenta escapando con su propio octal (`\377`),
y la tabla queda alineada también ahí.

Imprimir no es infalible por eso: si una clase propia tiene un
[`format()`](https://rdrr.io/r/base/format.html) que da error, ese error
llega sin alterar. Vale también para las columnas de listas de la
`propuesta_modelo` que devuelve
[`proponer_modelo()`](https://sebollin.github.io/lupa/reference/proponer_modelo.md).

## La frontera de entrada: bytes y marcas

Al entrar a
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md) y
a [`medir()`](https://sebollin.github.io/lupa/reference/medir.md), cada
valor de texto recibe una de tres lecturas:

``` r

# "Núñez" escrito en UTF-8 sin marca, en latin-1 con marca y en latin-1 sin marca.
utf8_sin_marca <- rawToChar(as.raw(c(0x4e, 0xc3, 0xba, 0xc3, 0xb1, 0x65, 0x7a)))
latin1_con_marca <- rawToChar(as.raw(c(0x4e, 0xfa, 0xf1, 0x65, 0x7a)))
Encoding(latin1_con_marca) <- "latin1"
latin1_sin_marca <- rawToChar(as.raw(c(0x4e, 0xfa, 0xf1, 0x65, 0x7a)))

apellidos <- data.frame(
  utf8_sin_marca = c(utf8_sin_marca, "Silva", "Sosa"),
  latin1_con_marca = c(latin1_con_marca, "Silva", "Sosa"),
  latin1_sin_marca = c(latin1_sin_marca, "Silva", "Sosa")
)
frontera <- perfilar(
  apellidos, analizar_dependencias = FALSE, proteger_datos_personales = FALSE
)
h <- hallazgos(frontera)
h[h$tipo_hallazgo == "codificacion_invalida", c("columna", "tipo_hallazgo", "severidad")]
#>            columna         tipo_hallazgo severidad
#> 1 latin1_sin_marca codificacion_invalida     error
envolver(h$sugerencia[h$tipo_hallazgo == "codificacion_invalida"])
#> Volver a leer la fuente declarando su codificación original; no reinterpretar ni
#> reemplazar los bytes sin ese dato.
Encoding(apellidos$utf8_sin_marca[[1]])
#> [1] "unknown"
```

- Los bytes UTF-8 válidos sin marca se declaran UTF-8 antes de ejecutar
  las métricas.
- Lo que declara su codificación se convierte.
- Lo que no es UTF-8 válido ni declara su codificación no se
  reinterpreta: se informa como `codificacion_invalida`, con la
  sugerencia de volver a leer la fuente declarando su codificación
  original.

La entrada no cambia:
[`Encoding()`](https://rdrr.io/r/base/Encoding.html) de la columna
original sigue diciendo lo que decía, y los nombres publicados en
`columnas$columna` conservan los bytes y la marca con que llegó la
tabla.

### Lo que se compara por bytes

- Las comparaciones textuales de `NoNulo`, `Formato`, los diccionarios y
  los dominios posibles usan una clave por bytes, así que una medición
  no cambia entre un locale UTF-8 y `C`. Vale también para las
  configuraciones aprendidas que se guardan con
  [`saveRDS()`](https://rdrr.io/r/base/readRDS.html).
- Las claves de deriva y de histórico comparan nombres y configuraciones
  por bytes, no por la marca de codificación ni por la intercalación del
  locale: la comparación sigue siendo estable si el perfil se guarda con
  [`saveRDS()`](https://rdrr.io/r/base/readRDS.html) y se relee bajo
  otro locale. La clave interna que agrupa hallazgos sigue la misma
  regla, y dos perfiles idénticos no emiten avisos bajo `LC_CTYPE = C`.
- [`normalizacion()`](https://sebollin.github.io/lupa/reference/normalizacion.md)
  aplica los pasos de ligaduras y de ancho completo por punto de código,
  después de declarar los bytes UTF-8 válidos: cumplen igual bajo un
  locale UTF-8 y bajo `C`.

### El texto que el paquete redacta

Las sugerencias que el perfil compone (por ejemplo, la línea de
`aplicabilidad` que propone `posible_ausencia_estructural`) marcan como
UTF-8 sólo la copia que entra al texto. El nombre de la tabla se
conserva tal como llegó, y el código publicado se puede copiar y pegar
igual bajo `LC_CTYPE = "C"`, también cuando el nombre lleva tildes.

## Números publicados como texto

Los números que pasan a un texto publicado (una evidencia, una
sugerencia, un motivo) usan notación fija con la precisión completa,
independiente de `scipen` y de `digits`, y conservan `OutDec`, la marca
decimal que elige la sesión:

``` r

set.seed(5)
montos <- data.frame(monto = c(rep(1234567.5, 40), round(runif(120, 1, 1e6), 1)))
evidencia_concentrada <- function() {
  h <- hallazgos(perfilar(montos, analizar_dependencias = FALSE))
  h$evidencia[h$tipo_hallazgo == "valor_concentrado"]
}
envolver(evidencia_concentrada())
#> Valor modal: 1234567.5; frecuencia de la moda: 40; frecuencia del segundo valor: 1;
#> cociente moda/segundo: 40.000; fraccion de la moda sobre validos: 0.250

previas <- options(OutDec = ",", scipen = -9, digits = 2)
envolver(evidencia_concentrada())
#> Valor modal: 1234567,5; frecuencia de la moda: 40; frecuencia del segundo valor: 1;
#> cociente moda/segundo: 40,000; fraccion de la moda sobre validos: 0,250
options(previas)
```

La clasificación de posibles datos personales usa la misma
representación fija: `scipen` y `digits` no pueden cambiar la decisión
ni borrar de `meta` los centinelas numéricos declarados.

## Codificación dañada: qué se detecta

El texto que se guardó en UTF-8, se leyó con otra codificación y se
volvió a guardar (mojibake) se informa como `codificacion_rota`, con
severidad `error`. La evidencia muestra cada valor con su reparación
propuesta y su estado, y
[`columnas()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
publica el estado de la columna en `estado_codificacion_reparacion`:
`no_parece_roto`, `reparado`, `reparado_parcialmente` o `no_se_pudo`.

``` r

# "José" y "Núñez" leídos como latin-1 y vueltos a guardar en UTF-8.
utf8 <- function(...) {
  x <- rawToChar(as.raw(c(...)))
  Encoding(x) <- "UTF-8"
  x
}
jose_roto <- utf8(0x4a, 0x6f, 0x73, 0xc3, 0x83, 0xc2, 0xa9)
nunez_roto <- utf8(0x4e, 0xc3, 0x83, 0xc2, 0xba, 0xc3, 0x83, 0xc2, 0xb1, 0x65, 0x7a)
nombres <- data.frame(
  id = 1:5,
  nombre = c(jose_roto, "Ana", nunez_roto, "Luis", "Eva")
)
roto <- perfilar(nombres, analizar_dependencias = FALSE, proteger_datos_personales = FALSE)
h <- hallazgos(roto)
h[h$tipo_hallazgo == "codificacion_rota", c("columna", "severidad", "n_afectados")]
#>   columna severidad n_afectados
#> 2  nombre     error           2
envolver(h$evidencia[h$tipo_hallazgo == "codificacion_rota"])
#> "JosÃ©" -> "José" [reparado]; "NÃºÃ±ez" -> "Núñez" [reparado]
columnas(roto)[, c("columna", "estado_codificacion_reparacion")]
#>   columna estado_codificacion_reparacion
#> 1      id                 no_parece_roto
#> 2  nombre                       reparado
```

Otros hallazgos de texto tienen su propio nombre y no son parte de esta
detección: `controles_invisibles`, `entidades_html` y
`normalizacion_unicode`.

## La estrategia `reparar_codificacion`

La reparación no es una función exportada: es una estrategia de
[`planificar_limpieza()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)
y
[`aplicar()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md).
El plan la propone para cada columna con `codificacion_rota`, y
[`aplicar()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)
la ejecuta sobre una copia de los datos.

``` r

plan <- planificar_limpieza(roto, nombres)
plan[, c("columna", "estrategia", "estado", "aplicar", "n_afectadas", "estado_reparacion")]
#>   columna           estrategia estado aplicar n_afectadas estado_reparacion
#> 1  nombre reparar_codificacion  lista    TRUE           2          reparado

resultado <- aplicar(plan, nombres)
data.frame(antes = nombres$nombre, despues = resultado$datos$nombre)
#>     antes despues
#> 1   JosÃ©    José
#> 2     Ana     Ana
#> 3 NÃºÃ±ez   Núñez
#> 4    Luis    Luis
#> 5     Eva     Eva
identical(nombres$nombre[[1]], jose_roto)
#> [1] TRUE
```

Lo que conviene saber antes de activarla:

- **Prueba varias codificaciones.** No se limita a latin-1: prueba las
  tablas congeladas de varias codificaciones y deja en
  `estado_reparacion` uno de `reparado`, `reparado_parcialmente` o
  `no_se_pudo`.
- **Una reparación parcial no se activa sola.** Si una celda sólo se
  repara en parte, la acción queda `lista` con `aplicar = FALSE`; se
  elige marcando `aplicar = TRUE`, y entonces se aplican también las
  parciales.
- **Una celda sin reparación no frena a las demás.** Una celda con el
  carácter de reemplazo U+FFFD, o que el motor no pudo reparar, queda
  como está, y la justificación de la acción la cuenta.
- **El plan y el hallazgo cuentan cosas distintas.** Una columna con
  tres valores de codificación rota, de los cuales uno es reparable,
  produce un hallazgo con `n_afectados = 3` y una acción con
  `n_afectadas = 1`. Las dos cifras son ciertas.
- **Un texto legítimo puede tener los bytes de un mojibake.** Un texto
  que cita literalmente la `Ã` seguida de `©` no se distingue de uno
  roto: se informa, la evidencia del plan lo muestra antes de aplicar y,
  si la acción se aplica, se reescribe como la letra que esos bytes
  codifican. Si la columna contiene texto así, la acción no se activa.
- **Unos mismos bytes pueden recibir dos diagnósticos.** `c2 80` es a la
  vez un carácter de control C1 y la huella de un `€` de Windows-1252
  leído como latin-1. Cuando el plan propone reparar la codificación y
  eliminar ese control, manda la reparación: corre primero y restituye
  el `€`, y la eliminación queda sin nada que hacer. En el caso raro de
  un control C1 genuino, la reparación lo convierte en `€`.

El recorrido completo de un plan, con la edición de las acciones, está
en [Limpiar con un
plan](https://sebollin.github.io/lupa/articles/limpiar-con-un-plan.md).

## Relación con ftfy

La reparación sigue en R el enfoque y los datos congelados de
[ftfy](https://github.com/rspeer/python-ftfy) 6.3.1, de [Robyn
Speer](https://github.com/rspeer). Usa 11 tablas de bytes (`latin-1`,
`sloppy-windows-1252`, `sloppy-windows-1251`, `sloppy-windows-1250`,
`sloppy-windows-1253`, `sloppy-windows-1254`, `sloppy-windows-1257`,
`iso-8859-2`, `macroman`, `cp437`, `koi8-r`), decodifica CESU-8 y el
caso Java `C0 80`, y no depende de `iconv` al cargar el paquete: las
tablas son literales congelados. La tabla KOI8-R y algunas reglas de
detección son extensiones propias de `lupa`, documentadas en el código
fuente.

El corpus de pruebas de ftfy viaja con las pruebas del paquete
(`tests/testthat/test-reparacion-texto.R`), y esta página lo recorre al
construirse:

``` r

table(suite, reproducido)
#>                 reproducido
#> suite            FALSE TRUE
#>   in-the-wild        1   69
#>   known-failures     1    9
#>   language-names     0   20
#>   negative           0   31
#>   synthetic          0   30
```

Se reproducen 159 de los 161 casos. Los 2 que no se reproducen son
exactamente los que el propio corpus marca como fallas esperadas
(`xfail`): una mezcla de Windows-1250 en un texto en inglés y una falla
conocida de la heurística con texto vietnamita. De los 31 casos
negativos, 30 esperan el texto sin cambios y quedan intactos; el
restante está marcado como mayormente negativo, y el corpus espera que
se reparen sólo sus controles C1.

La reparación no ofrece los pasos de estilo de `fix_text` de ftfy, como
deshacer HTML, curvar comillas, normalizar el ancho o normalizar
Unicode: cambiar datos legítimos en silencio no es reparar. Las
entidades HTML y las formas Unicode tienen sus propios hallazgos
(`entidades_html`, `normalizacion_unicode`), y las entidades, su propia
estrategia en el plan (`decodificar_entidades_html`).

Las partes traducidas de ftfy se distribuyen bajo Apache-2.0, y los
centinelas tomados de naniar, bajo MIT; los avisos completos están en
[`inst/COPYRIGHTS`](https://github.com/sebollin/lupa/blob/main/inst/COPYRIGHTS).
