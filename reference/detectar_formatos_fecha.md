# Detectar formatos de fecha

Reconoce formatos de fecha y fecha-hora sin escoger arbitrariamente
entre día/mes y mes/día. Cuando todos los valores con barra, guion o
punto son ambiguos, devuelve ambos formatos con estado `"candidato"`. El
atributo `formatos_mixtos` indica si hay evidencia de dos o más
representaciones en la columna. Se aceptan días y meses con uno o dos
dígitos. Los años de dos dígitos se detectan, pero siempre quedan como
candidatos y se señalan en la columna `anio_dos_digitos`: el siglo no se
interpreta en silencio. También reconoce meses escritos en español
rioplatense (`setiembre` y `set`) y en inglés, con la estructura
completa de una fecha o como mes y año. La tabla interna de nombres no
usa `LC_TIME`, por lo que el resultado es independiente del locale del
proceso; los nombres de mes dentro de una oración no se reconocen. Un
mes escrito desambigua día/mes, pero un año de dos dígitos sigue siendo
candidato. Los formatos escritos de mes y año, igual que las fechas
compactas, exigen un año entre 1800 y 2100. El formato compacto `%Y%m%d`
exige un año entre 1800 y 2100 para evitar que identificadores de ocho
dígitos se clasifiquen parcialmente como fechas.

## Usage

``` r
detectar_formatos_fecha(x, muestra = 1e+05)
```

## Arguments

- x:

  Vector de texto, fechas o fechas-hora.

- muestra:

  Máximo de valores que se analizan.

## Value

Data frame con formato, frecuencia, proporción, estado, granularidad
(`"dia"` o `"mes"`) y conteos de casos inequívocos y ambiguos. Los
atributos informan el muestreo, la cantidad de valores compatibles y la
presencia de formatos mixtos.

## See also

[`inferir_tipo()`](https://sebollin.github.io/lupa/reference/inferir_tipo.md),
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)

## Examples

``` r
detectar_formatos_fecha(c("2020-01-31", "31/01/2020"))
#>    formato n proporcion     estado n_inequivocos n_ambiguos anio_dos_digitos
#> 1 %Y-%m-%d 1        0.5 confirmado             1          0            FALSE
#> 2 %d/%m/%Y 1        0.5 confirmado             1          0            FALSE
#>   granularidad
#> 1          dia
#> 2          dia
detectar_formatos_fecha(c("01/02/2020", "02/03/2020"))
#>    formato n proporcion    estado n_inequivocos n_ambiguos anio_dos_digitos
#> 1 %d/%m/%Y 2          1 candidato             0          2            FALSE
#> 2 %m/%d/%Y 2          1 candidato             0          2            FALSE
#>   granularidad
#> 1          dia
#> 2          dia
```
