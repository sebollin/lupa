# lupa

<img src="man/figures/lupa.png" align="right" width="240" alt="logo de lupa" />

[![Licencia: GPL (>= 2)](https://img.shields.io/badge/license-GPL--2%20%7C%20GPL--3-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.html)
[![Ciclo de vida: madurando](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
[![README in English](https://img.shields.io/badge/README-English-1565c0.svg)](https://github.com/sebollin/lupa/blob/main/README.md)

<code>lupa</code> es un conjunto auditable de herramientas de R para perfilar
tablas, definir qué significa calidad para un uso concreto, medirla y mejorar
una copia de los datos sin ocultar los cambios. También busca duplicados
aproximados a escala, estima el trabajo antes de empezar y declara los límites
de lo que comparó.

## Inicio rápido

~~~r
library(lupa)
data(datos_operativos)

analisis <- analizar(datos_operativos)
analisis$perfil$hallazgos

archivo <- reportar(analisis, archivo = tempfile(fileext = ".html"))
unlink(archivo)
~~~

La API, las ayudas y las viñetas están en español. La [versión en inglés](https://github.com/sebollin/lupa/blob/main/README.md)
explica el mismo código. Esta tabla permite orientarse a quien lea ambos idiomas:

| API | Significado |
| --- | --- |
| <code>perfilar()</code> | perfilar |
| <code>analizar()</code> | analizar |
| <code>marco_calidad()</code> | marco de calidad |
| <code>planificar_limpieza()</code> | planificar una limpieza |
| <code>guiar_limpieza()</code> | guiar una limpieza |
| <code>aplicar()</code> | aplicar |
| <code>medir()</code> | medir |
| <code>evaluar()</code> | evaluar |
| <code>detectar_duplicados_aproximados()</code> | buscar duplicados aproximados |
| <code>reportar()</code> | crear un reporte |

<code>analizar()</code> es de sólo lectura: no convierte una observación en
requisito y nunca modifica la tabla.

## Instalar localmente

Hasta la primera publicación, construya e instale el paquete fuente local:

~~~sh
R CMD build lupa
R CMD INSTALL lupa_0.1.0.tar.gz
~~~

También puede instalar un tarball local desde R:

~~~r
install.packages("lupa_0.1.0.tar.gz", repos = NULL)
~~~

## Qué puede hacer con lupa

### Mirar una entrega por primera vez

<code>perfilar()</code> es la entrada pequeña y <code>analizar()</code> suma
distribuciones, asociaciones, diagnósticos temporales, escalas propuestas y
cobertura. También están <code>distribucion_valores()</code>,
<code>detectar_asociaciones()</code>, <code>analizar_tiempo()</code>,
<code>clasificar_variables()</code>, <code>inferir_tipo()</code>,
<code>descubrir_patrones()</code>, <code>detectar_formatos_fecha()</code> y
<code>sentinelas_naniar</code>.

~~~r
data(datos_operativos)
perfil <- perfilar(datos_operativos, analizar_dependencias = FALSE)
analisis <- analizar(datos_operativos)

list(
  valores = distribucion_valores(datos_operativos),
  asociaciones = detectar_asociaciones(datos_operativos, umbral = 0.3,
                                       max_pares = 10),
  tiempo = analizar_tiempo(datos_operativos),
  variables = clasificar_variables(datos_operativos),
  tipo = inferir_tipo(datos_operativos$cedula),
  patrones = descubrir_patrones(datos_operativos$contacto),
  fechas = detectar_formatos_fecha(datos_operativos$fecha_evento),
  sentinelas = perfilar(
    datos_operativos,
    sentinelas_numericos = sentinelas_naniar,
    analizar_dependencias = FALSE
  )$columnas
)
~~~

Todas las proporciones están en [0, 1]. La evidencia de datos personales se
protege por omisión, queda marcada en la salida y nunca se suprime en silencio.

### Encontrar la estructura que nadie declaró

<code>detectar_claves()</code>, <code>detectar_relaciones()</code> y
<code>detectar_dependencias()</code> encuentran estructura observada.
<code>granularidades()</code> y <code>transiciones_granularidad()</code> declaran
los niveles y transiciones de medición.

~~~r
data(datos_operativos)
claves <- detectar_claves(datos_operativos)
relaciones <- detectar_relaciones(datos_operativos, datos_operativos, muestra = 1000)
dependencias <- detectar_dependencias(datos_operativos, min_observaciones = 10)

list(claves = claves, relaciones = relaciones, dependencias = dependencias,
     niveles = granularidades(), transiciones = transiciones_granularidad())
~~~

Son observaciones, no prueba de una regla de negocio: pueden confirmarse en el
modelo o quedar como hallazgos.

### Definir qué es calidad

<code>marco_calidad()</code> recibe una taxonomía de dimensiones y factores.
<code>marco_agesic()</code>, <code>marco_iso25012()</code> y
<code>catalogo_agesic()</code> son instancias consultables. Las métricas se
construyen con <code>metrica()</code>, <code>especializar()</code>,
<code>instanciar()</code> y <code>modelo()</code>; las fábricas
<code>metricas_nucleo()</code> y <code>metricas_referencial()</code> se pueden
reutilizar. <code>proponer_modelo()</code>,
<code>modelo_desde_propuesta()</code>, <code>perfiles_madurez()</code> y
<code>cobertura_analisis()</code> mantienen la propuesta y su alcance visibles.

~~~r
data(datos_operativos)
marco_propio <- marco_calidad(
  "Marco operativo",
  list(Estructura = c("Ausencias observadas", "Duplicación exacta"))
)
marco_agesic()
marco_iso25012()
catalogo_agesic()

nucleo <- metricas_nucleo()
instancia <- instanciar(
  especializar(nucleo$NoNulo, nombre_especifico = "NoNuloDato"),
  "entrega", "dato"
)
modelo_calidad <- modelo(instancia)
perfil <- perfilar(datos_operativos, analizar_dependencias = FALSE)
propuesta <- proponer_modelo(perfil)
modelo_confirmado <- modelo_desde_propuesta(propuesta)
cobertura_analisis(perfil, modelo = marco_propio)
~~~

No hay puntaje global: promediar dimensiones escondería prioridades, unidades e
incertidumbre que el usuario no declaró.

### Medir y evaluar

<code>medir()</code> ejecuta un modelo; <code>agregar()</code> respeta las
transiciones declaradas; <code>regla_evaluacion()</code>,
<code>perfil_evaluacion()</code> y <code>evaluar()</code> expresan y aplican
condiciones. <code>escala()</code>, <code>referencial()</code> y
<code>vigencia()</code> declaran contratos adicionales.

~~~r
nucleo <- metricas_nucleo()
instancia <- instanciar(
  especializar(nucleo$NoNulo, nombre_especifico = "NoNuloDato"),
  "entrega", "dato"
)
medidas <- medir(
  modelo(instancia), data.frame(dato = c("A", NA, "C")),
  id_medicion = "entrega-001",
  fecha = as.POSIXct("2026-01-15", tz = "UTC")
)
medida_entidad <- agregar(medidas, "atributo", "ratio")
regla <- regla_evaluacion("Completitud mayor al 60 %", function(x) x > 0.6)
evaluacion <- evaluar(medida_entidad, perfil_evaluacion("Operativo", regla))
list(evaluacion = evaluacion, escala = escala(error = 0.1),
     vigencia = vigencia("fecha_actualizacion"))
~~~

### Limpiar sin romper nada

El plan de <code>planificar_limpieza()</code> es editable.
<code>guiar_limpieza()</code> acompaña decisiones en consola y
<code>aplicar()</code> ejecuta sólo lo elegido, sobre una copia, dejando un
registro. Las eliminaciones requieren permiso adicional.

~~~r
data(datos_administrativos)
perfil <- perfilar(datos_administrativos, analizar_dependencias = FALSE)
plan <- planificar_limpieza(perfil, datos_administrativos)
plan$aplicar[] <- FALSE
seleccion <- which(plan$recomendada)[1]
if (length(seleccion) == 1L && !is.na(seleccion)) {
  plan$aplicar[seleccion] <- TRUE
  plan$decision_grupo[seleccion] <- "elegida"
}
copia <- datos_administrativos
resultado <- aplicar(plan, datos_administrativos)
stopifnot(identical(datos_administrativos, copia))
resultado$registro[, c("estrategia", "n_cambiadas")]
~~~

La entrada no cambia y el resultado conserva la evidencia de cada acción.

### Buscar duplicados aproximados a escala

Hay teselas exactas y MinHash/LSH determinista. <code>estimar_costo()</code>
pronostica antes de empezar; <code>bloquear_por</code> declara la pérdida
estructural y <code>lotes = TRUE</code> conserva parciales sin pérdida cuando
cruza grupos. <code>nucleos</code> usa dos hilos de <code>stringdist</code> por
omisión: cambiarlo mueve el reloj, no la respuesta. Sin <code>stringdist</code>
la degradación es explícita.

~~~r
if (requireNamespace("stringdist", quietly = TRUE)) {
  datos <- data.frame(
    nombre = c("Ana Perez", "Ana Peres", "Luis Silva", "Luis Silva"),
    domicilio = c("Calle 1", "Calle 1", "Ruta 5", "Ruta 5"),
    anio = c(2022, 2022, 2021, 2021), stringsAsFactors = FALSE
  )
  estimacion <- estimar_costo(
    datos, columnas = c("nombre", "domicilio"), estrategia = "lsh",
    lsh_muestra_estimacion = 10, nucleos = 2
  )
  exacto <- detectar_duplicados_aproximados(
    datos, columnas = c("nombre", "domicilio"), estrategia = "teselas",
    max_resultados = 10, nucleos = 2
  )
  lsh <- detectar_duplicados_aproximados(
    datos, columnas = c("nombre", "domicilio"), estrategia = "lsh",
    max_resultados = 10, nucleos = 2
  )
  bloqueado <- detectar_duplicados_aproximados(
    datos, columnas = c("nombre", "domicilio"), estrategia = "teselas",
    bloquear_por = "anio", max_resultados = 10, nucleos = 2
  )
  directorio <- tempfile("lupa-lotes-")
  por_lotes <- detectar_duplicados_aproximados(
    datos, columnas = c("nombre", "domicilio"), estrategia = "teselas",
    lotes = TRUE, tamano_lote = 2, directorio_lotes = directorio,
    max_resultados = 10, nucleos = 2
  )
  unlink(directorio, recursive = TRUE)
  list(estimacion = estimacion, exacto = exacto$pares,
       lsh = lsh$pares, bloqueo = bloqueado$alcance,
       lotes = por_lotes$lotes)
}
~~~

Un par aproximado no afirma identidad ni sugiere eliminar o fusionar. El piso
de tiempo cubre sólo la comparación <code>stringdist</code>, no la corrida
completa; <code>lupa_tiempo_lsh</code> es una condición silenciosa fuera de una
sesión interactiva y <code>resultado$estimacion</code> contiene la medición no
determinista.

En un Intel Core i9-14900HX, 32 núcleos, Pop!_OS 22.04 LTS (Linux), R 4.6.1,
las medianas de tres procesos aislados para 100.000 filas fueron:

| hilos | mediana (s) | relativo a 2 |
| ---: | ---: | ---: |
| 2 | 133.28 | 1.00x |
| 4 | 97.44 | 0.73x |
| 8 | 76.19 | 0.57x |
| 16 | 70.31 | 0.53x |
| 31 | 71.69 | 0.54x |

Después de 16 hilos no hubo ganancia medida. Dos es el valor prudente para una
máquina compartida, y se puede subir. Los hilos no cambian el resultado.

| filas | candidatos LSH | recall del techo | hilos |
| ---: | ---: | ---: | ---: |
| 20.000 | 6.201.626 | 1.0000 | 2 |
| 100.000 | 140.097.499 | 1.0000 | 2 |
| 200.000 | 582.388.482 | 1.0000 | 2 |

Son referencias medidas, no promesas portables de tiempo: candidatos, estimación
y recall son deterministas; el reloj depende del procesador y de las cubetas.

### Seguir la calidad en el tiempo

<code>historico_calidad()</code>, <code>acumular_historico()</code>,
<code>guardar_historico()</code> y <code>leer_historico()</code> mantienen una
serie versionada. <code>detectar_deriva_calidad()</code>,
<code>comparar_perfiles()</code> y <code>comparar_evaluaciones()</code> separan
cambios de estructura y de evaluación.

~~~r
nucleo <- metricas_nucleo()
instancia <- instanciar(
  especializar(nucleo$NoNulo, nombre_especifico = "NoNuloDato"),
  "entrega", "dato"
)
modelo_calidad <- modelo(instancia)
enero <- agregar(
  medir(modelo_calidad, data.frame(dato = c("A", NA, "C", NA)),
        id_medicion = "enero", fecha = as.POSIXct("2026-01-31", tz = "UTC")),
  "atributo", "ratio"
)
febrero <- agregar(
  medir(modelo_calidad, data.frame(dato = c("A", "B", "C", NA)),
        id_medicion = "febrero", fecha = as.POSIXct("2026-02-28", tz = "UTC")),
  "atributo", "ratio"
)
regla <- regla_evaluacion("Completitud mayor al 60 %", function(x) x > 0.6)
perfil_e <- perfil_evaluacion("Operativo", regla)
enero_eval <- evaluar(enero, perfil_e)
febrero_eval <- evaluar(febrero, perfil_e)
historico <- historico_calidad(enero_eval, febrero_eval)
detectar_deriva_calidad(historico)
archivo <- tempfile(fileext = ".rds")
guardar_historico(historico, archivo)
recuperado <- leer_historico(archivo)
stopifnot(identical(historico, recuperado))
unlink(archivo)
~~~

### Compartir el resultado

<code>reportar()</code> crea un HTML autocontenido, escapado y sin red.
<code>guardar_analisis()</code> y <code>leer_analisis()</code> permiten retomar
el análisis; por omisión no guardan la tabla de entrada.

~~~r
data(datos_operativos)
analisis <- analizar(datos_operativos)
archivo_rds <- tempfile(fileext = ".rds")
guardar_analisis(analisis, archivo_rds)
analisis_recuperado <- leer_analisis(archivo_rds)
archivo_html <- reportar(
  analisis, archivo = tempfile(fileext = ".html"),
  titulo = "Calidad de datos operativos"
)
stopifnot(file.exists(archivo_html), file.exists(archivo_rds))
unlink(c(archivo_html, archivo_rds))
~~~

### Validadores y packs de extensión

<code>validadores_internacionales()</code> ofrece ISO 3166, ISO 4217, correo,
Luhn y módulo 97; <code>validadores_uruguay()</code> agrega identidad y RUT.
También están <code>validar_ci_uy()</code>, <code>validar_rut_uy()</code>,
<code>validar_luhn()</code>, <code>validar_mod97()</code>,
<code>validar_iso3166()</code>, <code>validar_iso4217()</code> y
<code>validar_correo()</code>.

~~~r
internacionales <- validadores_internacionales()
uruguay <- validadores_uruguay()
internacionales$iso4217(c("UYU", "CLP", "ZZZ"))
uruguay$cedula(c("1.234.567-2", "1.234.567-3"))
validar_iso3166(c("UY", "XX"))
validar_correo(c("persona@example.org", "no-es-correo"))
pack_ejemplo <- pack_validadores(
  "Ejemplo", list(codigo = function(x) !is.na(x) & x == "OK"), pais = "CL",
  descripcion = "Validador mantenido por el proyecto consumidor."
)
pack_ejemplo$codigo(c("OK", "otro"))
~~~

Un pack es una lista de funciones y no se registra globalmente. Se puede
conectar a <code>perfilar(validadores_personales = ...)</code> o a la métrica
<code>Formato</code>. AGESIC v1.6 es una referencia, no una restricción de país.

## El diseño en cuatro compromisos

* **No hay puntaje global.** Un número único escondería prioridades, unidades e
  incertidumbre.
* **Núcleo universal, catálogos enchufables.** <code>marco_calidad()</code>
  acepta una taxonomía propia; AGESIC v1.6 e ISO/IEC 25012 son instancias
  consultables; <code>pack_validadores()</code> suma otro país sin cambiar el
  núcleo.
* **Una dependencia obligatoria.** <code>cli</code> es la única dependencia en
  <code>Imports</code>; <code>stringdist</code> es opcional en
  <code>Suggests</code>.
* **No se afirma más de lo que se sabe.** Alcances parciales, pérdidas,
  <code>NA</code> cuando no se puede contar y pisos de tiempo quedan declarados;
  la máquina y los hilos acompañan cada medición.

## Guías, referencia y cita

~~~r
vignette("empezar-con-lupa", package = "lupa")
vignette("el-modelo-de-calidad", package = "lupa")
vignette("limpiar-con-un-plan", package = "lupa")
vignette("historico-y-deriva", package = "lupa")
vignette("escala-y-duplicados", package = "lupa")
citation("lupa")
~~~

El paquete se ubica junto a otras herramientas: <code>skimr</code> y
<code>DataExplorer</code> resumen y exploran; <code>pointblank</code>,
<code>validate</code> y <code>dataquieR</code> expresan o evalúan reglas;
<code>zoomerjoin</code>, <code>textreuse</code> y <code>reclin2</code> cubren
comparación textual o record linkage.
<code>calidad</code>, de INE Chile y mantenido por Klaus Lehmann y Ricardo
Pizarro, es un eje contiguo valioso: aplica criterios de CEPAL a la calidad de
estimaciones de encuestas (Estudios Estadísticos 101), mientras <code>lupa</code>
evalúa el dato tabular que produce una estimación.

El paquete aún no está en CRAN; por eso no se muestran insignias de CRAN ni de
R-CMD-check.
