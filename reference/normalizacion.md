# Perfiles de normalizacion para comparar valores

La normalizacion afecta unicamente la representacion usada para
comparar; nunca modifica los datos de entrada. La descomposicion
canonica y el orden de sus marcas son siempre activos para que NFC y NFD
sean equivalentes en el subconjunto latino cubierto por lupa.

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

## Value

Un objeto de clase normalizacion_lupa.

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

# Comparar sin quitar acentos, para que "canon" y "cañón" no se fusionen.
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
