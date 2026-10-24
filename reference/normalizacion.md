# Perfiles de normalizacion para comparar valores

La normalizacion afecta unicamente la representacion usada para
comparar; nunca modifica los datos de entrada. La descomposicion
canonica y el orden de sus marcas son siempre activos para que NFC y NFD
sean equivalentes en el subconjunto latino cubierto por lupa: las letras
latinas precompuestas, las marcas combinantes de U+0300 a U+0332
ordenadas por su clase combinante canónica, y los signos Kelvin y
Ångström, que Unicode declara iguales a la `K` y a la `Å`. Las tablas se
generan desde Unicode (`data-raw/normalizacion_unicode.R`). Los pasos
optativos de ligaduras y ancho completo tambien se aplican por punto de
codigo, despues de declarar como UTF-8 los bytes validos, por lo que no
dependen de `LC_CTYPE`. Bajar a minusculas tampoco depende del locale:
usa un mapa explicito para el latin acentuado, el griego, el cirilico,
el armenio y el ancho completo, y lo que queda fuera de ese mapa se
conserva. Para comparar, la sigma final griega se iguala a la sigma,
como en el plegado de caja de Unicode: una palabra griega en mayúsculas
y la misma en minúsculas, escrita con su sigma final, son el mismo
valor.

## Usage

``` r
normalizacion(
  minusculas = TRUE,
  espacios = TRUE,
  acentos = TRUE,
  comillas = TRUE,
  puntuacion = FALSE,
  ligaduras = FALSE,
  ancho = FALSE,
  proteger = c("ñ", "ü", intToUtf8(c(103L, 771L)))
)
```

## Arguments

- minusculas, espacios, acentos, comillas, puntuacion, ligaduras, ancho:

  Activan el paso correspondiente.

- proteger:

  Grafemas cuyas marcas deben conservarse al quitar acentos. Puede
  incluir una base seguida de una o más marcas combinantes, como la
  secuencia `g` seguida por una tilde combinante para la letra guaraní.
  Se declaran en minúscula y valen también en mayúscula: la eñe de
  `PEÑA` se conserva igual que la de `peña`.

## Value

Objeto de clase `normalizacion_lupa`: una lista con un elemento logico
por paso -`minusculas`, `espacios`, `acentos`, `comillas`, `puntuacion`,
`ligaduras`, `ancho`- y el vector `proteger` con los grafemas cuyas
marcas se conservan. Es una declaracion de como comparar: no toca los
datos, y la descomposicion canonica se aplica siempre.

## Details

El paso `espacios` lleva primero al espacio común la tabulación, el
salto de línea, el retorno de carro, el avance de página, la tabulación
vertical y los espacios Unicode, y borra los controles de transporte; al
final colapsa y recorta los blancos. Como va antes que `comillas`, una
comilla junto a una tabulación o un salto de línea se trata igual que
junto a un espacio.

El paso `comillas` quita una comilla de apertura cuando abre una palabra
—la precede un blanco o el inicio y la sigue una letra— y una de cierre
o un apóstrofo cuando la cierra —la precede una letra y la sigue un
blanco o el final—; los apóstrofos tipográficos que quedan se escriben
`'`. La comilla simple ASCII cierra pero no abre, porque también es el
apóstrofo de una elisión: `'Piedras'` se compara como `'piedras`,
mientras que la misma palabra entre comillas simples tipográficas se
compara como `piedras`.

## Examples

``` r
# El perfil por omisión: minúsculas, espacios, acentos y comillas.
normalizacion()
#> Perfil de normalizacion de lupa
#>   minusculas = TRUE
#>   espacios = TRUE
#>   acentos = TRUE
#>   comillas = TRUE
#>   puntuacion = FALSE
#>   ligaduras = FALSE
#>   ancho = FALSE
#>   proteger = ñ, ü, g̃

# Comparar sin quitar acentos, para que "papa" y "papá" no se fusionen. La
# eñe y la diéresis ya están protegidas en el perfil por omisión: "pena" y
# "peña" no se fusionan nunca.
perfil <- normalizacion(acentos = FALSE)
perfil$acentos
#> [1] FALSE

# La normalización sólo afecta la representación usada para COMPARAR; no
# modifica los datos ni los conteos. Estas tres formas siguen siendo tres
# valores distintos, y así se informan: lo que la normalización habilita es
# que se reconozcan como variantes de la misma cosa al buscar duplicados.
datos <- data.frame(ciudad = c("Montevideo", "MONTEVIDEO", "montevideo"))
salida <- perfilar(datos, normalizar = normalizacion())
salida$columnas$n_distintos  # 3, no 1
#> [1] 3
```
