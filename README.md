# lupa

`lupa` es un paquete de R para examinar, medir y mejorar la calidad de datos
administrativos con resultados auditables.

El motor de profiling descubre patrones de formato, infiere tipos implícitos,
detecta formatos de fecha mezclados y faltantes disfrazados, resume cada
columna y devuelve hallazgos accionables como un objeto de datos. La capa de
calidad permite declarar, especializar, instanciar, medir y evaluar métricas con
granularidad explícita. La capa de remediación propone un plan editable y nunca
modifica datos como efecto del diagnóstico.

```r
library(lupa)

p <- perfilar(datos_administrativos)
p
summary(p)
p$hallazgos

plan <- planificar_limpieza(p)
plan[, c("grupo", "columna", "estrategia", "recomendada", "aplicar")]

# En una sesión interactiva, revisa sólo las decisiones pendientes o riesgosas.
plan <- guiar_limpieza(plan, datos_administrativos)

resultado <- aplicar(plan, datos_administrativos)
resultado$registro
```

## Convenciones

- `9` representa un dígito, `a` una minúscula y `A` una mayúscula.
- Los símbolos y espacios se conservan literalmente.
- Con `expandir = FALSE`, las repeticiones se colapsan: `9999` se representa
  como `9+`. Con `expandir = TRUE`, se conserva un token por carácter.
- Todas las proporciones están en la escala `[0, 1]`.
- Las severidades son el factor ordenado `ok < sospechoso < error`.
- Los valores `NA` no generan medidas de formato o dominio: la completitud se
  mide por separado con `NoNulo`.
- Sólo se activan de forma predeterminada acciones de limpieza que no requieren
  conocimiento del dominio. Las demás quedan desactivadas o bloqueadas.
- Las estrategias alternativas comparten un grupo y como máximo una puede
  quedar activa. No hacer nada queda registrado como una decisión, no como una
  transformación ficticia.
- Eliminar filas o columnas nunca se recomienda y exige el consentimiento
  adicional `permitir_eliminacion = TRUE`. Lo retirado se conserva en el
  resultado salvo que se solicite expresamente lo contrario.

El modelo conserva la jerarquía de dimensiones, factores y métricas como una
taxonomía. No calcula un índice global de calidad. Las agregaciones disponibles
son las cuatro definidas por el marco y validan el tipo de resultado declarado.

## Referencia conceptual

AGESIC (2020). *Marco de trabajo para la Gestión de la Calidad de Datos en
Gobierno Digital*, versión 1.6. Presidencia de la República, Uruguay, con la
Facultad de Ingeniería de la Universidad de la República.
