# Detectar deriva en una serie de evaluaciones

Compara corridas consecutivas, ordenadas por fecha dentro de cada perfil
o regla, y marca cambios significativos en la escala `[0, 1]`.

## Uso

``` r
detectar_deriva_calidad(historico, nivel = c("perfil", "regla"), umbral = 0.05)
```

## Argumentos

  - historico:
    
    Objeto creado por `historico_calidad()`.

  - nivel:
    
    `"perfil"` o `"regla"`.

  - umbral:
    
    Cambio absoluto mínimo considerado significativo. El valor
    predeterminado de `0.05` representa cinco puntos porcentuales: evita
    tratar como deriva diferencias de redondeo, pero sigue siendo
    sensible a cambios operativamente visibles.

## Valor

Data frame `deriva_calidad` con una fila por par de corridas
consecutivas. Una mejora significativa conserva severidad `ok`; un
deterioro de al menos un umbral es `sospechoso` y uno de al menos dos
umbrales es `error`.

## Ejemplos

``` r
# El ejemplo de historico_calidad() muestra cómo construir las corridas.
```
