# Detectar dependencias funcionales entre columnas

Busca pares ordenados `determinante -> dependiente`. El cumplimiento es
la proporción de filas que coincide con el valor modal del dependiente
para cada valor del determinante; por eso una dependencia aproximada
señala directamente las filas minoritarias que pueden ser errores de
carga.

## Usage

``` r
detectar_dependencias(
  datos,
  umbral = 0.995,
  muestra = 1e+05,
  max_columnas = 100L,
  umbral_casi_constante = 0.95,
  umbral_casi_clave = 0.8,
  incluir_claves = FALSE,
  min_observaciones = 10L,
  max_ejemplos = 5L
)
```

## Arguments

- datos:

  Tabla que se desea examinar.

- umbral:

  Cumplimiento mínimo en `[0, 1]`.

- muestra:

  Máximo de filas; `Inf` desactiva el muestreo.

- max_columnas:

  Máximo de columnas analizadas.

- umbral_casi_constante:

  Proporción modal a partir de la cual un determinante se descarta por
  casi constante.

- umbral_casi_clave:

  Tasa de valores distintos a partir de la cual un determinante se
  descarta por casi clave, salvo que `incluir_claves` sea verdadero.

- incluir_claves:

  Si se incluyen determinantes únicos, que satisfacen dependencias de
  forma trivial.

- min_observaciones:

  Mínimo de pares presentes para informar una dependencia.

- max_ejemplos:

  Máximo de contradicciones concretas en `evidencia`.

## Value

Data frame de clase `dependencias_funcionales`, ordenado por
cumplimiento y soporte. Los atributos `muestreado`, `filas_analizadas`,
`columnas_analizadas`, `columnas_omitidas`, `columnas_descartadas` y
`truncado` documentan el alcance efectivo. `columnas_descartadas` es un
data frame que explica por qué una columna no se usó como determinante.

## Details

Para evitar resultados vacíos o triviales, se omiten por defecto las
claves únicas, los determinantes cuya tasa de valores distintos alcanza
`umbral_casi_clave`, los determinantes cuya moda alcanza
`umbral_casi_constante` y los dependientes constantes. El valor
predeterminado de `umbral = 0.995` exige que como máximo 5 de cada 1.000
filas contradigan la relación. Los ausentes de cualquiera de las dos
columnas no integran el cálculo. El descarte ocurre antes de construir
agrupaciones. El valor predeterminado `umbral_casi_clave = 0.8` excluye
determinantes con menos de 1,25 filas por valor distinto en promedio:
aun si cumplen, suelen describir una casi-clave y no una regla
reutilizable.

El costo crece con el cuadrado de las columnas. `max_columnas` conserva
las primeras columnas analizables y `muestra` aplica una única muestra
sistemática a toda la tabla, de modo que las relaciones entre filas no
se rompen. Los atributos del resultado declaran ambos recortes.

## See also

[`detectar_claves()`](https://sebollin.github.io/lupa/reference/detectar_claves.md),
[`proponer_modelo()`](https://sebollin.github.io/lupa/reference/proponer_modelo.md),
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)

## Examples

``` r
datos <- data.frame(
  codigo = rep(1:3, each = 4),
  descripcion = rep(c("A", "B", "C"), each = 4),
  valor = seq_len(12)
)
detectar_dependencias(datos, min_observaciones = 4)
#>   determinante dependiente cumplimiento n_evaluados n_grupos n_violaciones
#> 1       codigo descripcion            1          12        3             0
#> 2  descripcion      codigo            1          12        3             0
#>   exacta evidencia
#> 1   TRUE          
#> 2   TRUE          
```
