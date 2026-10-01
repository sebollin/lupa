# Perfiles de normalizacion para comparar valores

La normalizacion afecta unicamente la representacion usada para
comparar; nunca modifica los datos de entrada. La descomposicion
canonica y el orden de sus marcas son siempre activos para que NFC y NFD
sean equivalentes en el subconjunto latino cubierto por lupa. Los pasos
optativos de ligaduras y ancho completo tambien se aplican por punto de
codigo, despues de declarar como UTF-8 los bytes validos, por lo que no
dependen de `LC_CTYPE`. Bajar a minusculas tampoco depende del locale:
usa un mapa explicito para el latin acentuado, el griego, el cirilico,
el armenio y el ancho completo, y lo que queda fuera de ese mapa se
conserva.

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
