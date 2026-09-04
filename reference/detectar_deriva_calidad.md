# Detectar deriva en una serie de evaluaciones

Compara corridas consecutivas, ordenadas por fecha dentro de cada perfil
o regla, y marca cambios significativos en la escala `[0, 1]`.

## Usage

``` r
detectar_deriva_calidad(historico, nivel = c("perfil", "regla"), umbral = 0.05)
```

## Arguments

- historico:

  Objeto creado por
  [`historico_calidad()`](https://sebollin.github.io/lupa/reference/historico_calidad.md).

- nivel:

  `"perfil"` o `"regla"`.

- umbral:

  Cambio absoluto mínimo considerado significativo. El valor
  predeterminado de `0.05` representa cinco puntos porcentuales: evita
  tratar como deriva diferencias de redondeo, pero sigue siendo sensible
  a cambios operativamente visibles.

## Value

Data frame `deriva_calidad` con una fila por par de corridas
consecutivas. Una mejora significativa conserva severidad `ok`; un
deterioro de al menos un umbral es `sospechoso` y uno de al menos dos
umbrales es `error`. `identidad_tabla` separa series de tablas distintas
y `aspecto` marca el resultado o un cambio de configuración; este último
se informa como `error` pero no suprime la comparación.

## Examples

``` r
# El ejemplo de historico_calidad() muestra cómo construir las corridas.
```
