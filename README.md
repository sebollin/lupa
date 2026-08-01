# lupa

`lupa` es un paquete de R para examinar la estructura de datos administrativos
antes de definir reglas o métricas de calidad.

El MVP descubre patrones de formato, infiere tipos implícitos, detecta formatos
de fecha mezclados y faltantes disfrazados, resume cada columna y devuelve
hallazgos accionables como un objeto de datos.

```r
library(lupa)

p <- perfilar(datos_administrativos)
p
summary(p)
p$hallazgos
```

## Convenciones

- `9` representa un dígito, `a` una minúscula y `A` una mayúscula.
- Los símbolos y espacios se conservan literalmente.
- Con `expandir = FALSE`, las repeticiones se colapsan: `9999` se representa
  como `9+`. Con `expandir = TRUE`, se conserva un token por carácter.
- Todas las proporciones están en la escala `[0, 1]`.
- Las severidades son el factor ordenado `ok < sospechoso < error`.

El paquete implementa exclusivamente la etapa de examen o *data profiling*.
No calcula métricas, agregaciones ni evaluaciones de calidad.

## Referencia conceptual

AGESIC (2020). *Marco de trabajo para la Gestión de la Calidad de Datos en
Gobierno Digital*, versión 1.6. Presidencia de la República, Uruguay, con la
Facultad de Ingeniería de la Universidad de la República.
