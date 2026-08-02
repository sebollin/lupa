# lupa

`lupa` examina, mide y ayuda a mejorar la calidad de datos administrativos sin
ocultar decisiones dentro de un reporte. Descubre patrones y problemas
estructurales, propone métricas y planes editables, conserva la trazabilidad de
las acciones y compara entregas a lo largo del tiempo.

El paquete sigue el *Marco de trabajo para la Gestión de la Calidad de Datos en
Gobierno Digital v1.6* de AGESIC. Sólo `cli` es una dependencia obligatoria; el
reporte HTML autocontenido se genera con R base y no requiere navegador, LaTeX
ni servicios externos.

## Instalación

Antes de la publicación, el paquete puede instalarse desde un archivo fuente
construido localmente:

```sh
R CMD build lupa
R CMD INSTALL lupa_0.1.0.tar.gz
```

Desde R también se puede instalar una ruta local:

```r
install.packages("ruta/al/archivo/lupa_0.1.0.tar.gz", repos = NULL)
```

## Recorrido mínimo

```r
library(lupa)
data(datos_administrativos)

# 1. Examinar. El resultado y sus hallazgos son objetos de datos.
perfil <- perfilar(datos_administrativos)
perfil
subset(perfil$hallazgos, severidad != "ok")
cobertura_analisis(perfil)

# 2. Proponer qué medir. La propuesta se revisa antes de materializarla.
propuesta <- proponer_modelo(perfil, datos_administrativos)
propuesta[, c("metrica", "origen", "justificacion", "incluir")]
modelo_calidad <- modelo_desde_propuesta(propuesta)

# 3. Medir y evaluar con una regla explícita.
medidas <- medir(modelo_calidad, datos_administrativos)
medida_entidad <- agregar(medidas, "entidad", "ratio")
regla <- regla_evaluacion(
  "Duplicación menor al 20 %",
  function(x) x < 0.2
)
evaluacion <- evaluar(
  medida_entidad,
  perfil_evaluacion("Control operativo", regla)
)

# 4. Revisar y aplicar un plan sobre una copia de los datos.
plan <- planificar_limpieza(perfil, datos_administrativos)
plan[, c("grupo", "estrategia", "recomendada", "aplicar")]
resultado <- aplicar(plan, datos_administrativos)
resultado$registro

# 5. Compartir un único archivo sin recursos externos.
archivo <- reportar(perfil, medidas, evaluacion, plan)
```

Las proporciones siempre usan la escala `[0, 1]`. Las severidades forman el
factor ordenado `ok < sospechoso < error`. El diagnóstico nunca modifica los
datos y ninguna eliminación se recomienda o ejecuta sin consentimiento
adicional.

## Guías

- [Empezar con lupa](vignettes/empezar-con-lupa.Rmd): recorrido completo sobre
  los datos sintéticos incluidos.
- [El marco de AGESIC](vignettes/el-marco-agesic.Rmd): métricas, granularidad,
  agregaciones y evaluación, incluido el motivo por el que no hay un índice
  global.
- [Limpiar con un plan](vignettes/limpiar-con-un-plan.Rmd): decisiones,
  alternativas excluyentes, salvaguardas y trazabilidad.
- [Histórico y deriva](vignettes/historico-y-deriva.Rmd): acumular evaluaciones
  y comparar entregas.

Después de instalar el paquete, las mismas guías se abren con:

```r
vignette("empezar-con-lupa", package = "lupa")
vignette("el-marco-agesic", package = "lupa")
vignette("limpiar-con-un-plan", package = "lupa")
vignette("historico-y-deriva", package = "lupa")
```

## Capacidades principales

- patrones de formato vectorizados, tipos implícitos, fechas mixtas y ausentes
  disfrazados;
- hallazgos accionables, claves, relaciones y dependencias funcionales;
- métricas genéricas, específicas e instanciadas con granularidad explícita;
- referenciales tabulares para correctitud semántica y cobertura;
- cuatro agregaciones tipadas y perfiles de evaluación sin índice global;
- propuesta editable del modelo y plan de limpieza auditable;
- histórico plano, deriva del modelo y comparación estructural de perfiles;
- reporte HTML autocontenido, con cobertura conceptual y protección
  predeterminada de valores personales concretos.

`catalogo_agesic()` expone como tabla el estado de las 49 entradas del catálogo,
incluidas las métricas obtenibles por agregación, las que requieren insumos
externos y las que permanecen fuera de alcance.

## Referencia conceptual

AGESIC (2020). *Marco de trabajo para la Gestión de la Calidad de Datos en
Gobierno Digital*, versión 1.6. Presidencia de la República, Uruguay, con la
Facultad de Ingeniería de la Universidad de la República.
