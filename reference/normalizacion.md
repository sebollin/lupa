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
  incluir una base seguida de una o más marcas combinantes, como
  `"g\u0303"` para la letra guaraní.

## Value

Un objeto de clase normalizacion_lupa.
