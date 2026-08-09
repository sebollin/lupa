# Declarar un conjunto de datos referencial

Un referencial representa conocimiento externo mediante una clave y, de
forma opcional, valores asociados a ella. Se diferencia de un
diccionario: el diccionario sólo enumera valores sintácticamente
válidos, mientras que el referencial permite comprobar que una entidad
existe y que sus atributos están asociados a la clave correcta.

## Uso

``` r
referencial(
  datos,
  clave,
  valor = character(),
  completo = FALSE,
  alcance = NULL,
  nombre = NULL
)
```

## Argumentos

  - datos:
    
    Tabla de referencia. Se conserva una copia ordinaria de R.

  - clave:
    
    Columnas que identifican unívocamente cada fila.

  - valor:
    
    Columnas cuyos valores se contrastan junto con la clave.

  - completo:
    
    Si el referencial declara contener todo el universo del alcance
    indicado. Es `FALSE` por omisión.

  - alcance:
    
    Descripción explícita de aquello de lo que el referencial se declara
    completo. Es obligatoria cuando `completo = TRUE`.

  - nombre:
    
    Nombre legible del referencial. Si se omite, usa el nombre del
    objeto de entrada o `"referencial"` cuando la tabla se construye en
    línea.

## Valor

Objeto de clase `referencial` con `datos`, `clave`, `valor`, `completo`,
`alcance` y `nombre`.

## Detalles

La declaración de completitud es explícita. `RatioCobertura` sólo tiene
sentido bajo una asunción de mundo cerrado y exige `completo = TRUE`;
una lista parcial puede usarse para correctitud, pero no como
denominador de cobertura.

`clave` no admite ausentes y debe identificar cada fila de `datos` de
forma única; puede contener varias columnas. `valor` es opcional, no
puede repetir columnas de `clave` y representa los atributos asociados
que se contrastan en correctitud semántica fuerte. `completo = FALSE` es
el valor predeterminado y permite omitir `alcance`. Al declarar
`completo = TRUE`, `alcance` pasa a ser obligatorio y debe nombrar el
universo que la tabla dice cubrir. El constructor copia la tabla y no
consulta fuentes externas.

## Ver también

`metricas_referencial()`, `instanciar()`, `detectar_relaciones()`

## Ejemplos

``` r
padron <- referencial(
  data.frame(codigo = c("01", "02"), departamento = c("Artigas", "Canelones")),
  clave = "codigo", valor = "departamento",
  completo = TRUE, alcance = "departamentos del Uruguay"
)
padron
#> Referencial: referencial 
#>   Filas: 2 
#>   Clave: codigo 
#>   Valores: departamento 
#>   Completo: sí 
#>   Alcance: departamentos del Uruguay 
```
