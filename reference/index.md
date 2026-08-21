# Package index

## Examinar

Perfilar datos y descubrir su estructura.

- [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md)
  : Ejecutar el análisis descriptivo completo

- [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  : Perfilar un conjunto de datos

- [`perfilar_por()`](https://sebollin.github.io/lupa/reference/perfilar_por.md)
  : Perfilar una tabla por grupos de filas

- [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  : Perfilar una muestra leída mediante DBI

- [`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
  :

  Planificar el costo de
  [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  antes de pagarlo

- [`distribucion_valores()`](https://sebollin.github.io/lupa/reference/distribucion_valores.md)
  : Distribuciones de valores y cuantiles por columna

- [`detectar_asociaciones()`](https://sebollin.github.io/lupa/reference/detectar_asociaciones.md)
  : Detectar asociaciones entre columnas

- [`analizar_tiempo()`](https://sebollin.github.io/lupa/reference/analizar_tiempo.md)
  : Examinar regularidad y cobertura temporal

- [`clasificar_variables()`](https://sebollin.github.io/lupa/reference/clasificar_variables.md)
  : Proponer escalas y roles de las variables

- [`senal_redundante()`](https://sebollin.github.io/lupa/reference/senal_redundante.md)
  : Declarar una señal redundante entre columnas

- [`detectar_discordancias()`](https://sebollin.github.io/lupa/reference/detectar_discordancias.md)
  : Detectar filas donde señales redundantes se contradicen

- [`descubrir_patrones()`](https://sebollin.github.io/lupa/reference/descubrir_patrones.md)
  : Descubrir patrones de formato

- [`normalizacion()`](https://sebollin.github.io/lupa/reference/normalizacion.md)
  : Perfiles de normalizacion para comparar valores

- [`inferir_tipo()`](https://sebollin.github.io/lupa/reference/inferir_tipo.md)
  : Inferir el tipo implícito de un vector

- [`detectar_formatos_fecha()`](https://sebollin.github.io/lupa/reference/detectar_formatos_fecha.md)
  : Detectar formatos de fecha

- [`detectar_claves()`](https://sebollin.github.io/lupa/reference/detectar_claves.md)
  : Detectar claves candidatas

- [`detectar_dependencias()`](https://sebollin.github.io/lupa/reference/detectar_dependencias.md)
  : Detectar dependencias funcionales entre columnas

- [`detectar_relaciones()`](https://sebollin.github.io/lupa/reference/detectar_relaciones.md)
  : Detectar relaciones entre dos tablas

- [`detectar_duplicados_aproximados()`](https://sebollin.github.io/lupa/reference/detectar_duplicados_aproximados.md)
  : Detectar pares de filas con similitud aproximada

- [`estimar_costo()`](https://sebollin.github.io/lupa/reference/estimar_costo.md)
  : Estimar el costo de una comparación de duplicados

- [`cobertura_analisis()`](https://sebollin.github.io/lupa/reference/cobertura_analisis.md)
  : Informar la cobertura conceptual de un análisis

- [`as_tibble(`*`<perfil>`*`)`](https://sebollin.github.io/lupa/reference/as_tibble.perfil.md)
  : Convertir un perfil a tibble

- [`datos_administrativos`](https://sebollin.github.io/lupa/reference/datos_administrativos.md)
  : Datos administrativos sintéticos con problemas sembrados

- [`datos_operativos`](https://sebollin.github.io/lupa/reference/datos_operativos.md)
  : Datos operativos sintéticos y neutrales

## Proponer

Convertir diagnósticos en una propuesta editable de medición.

- [`proponer_modelo()`](https://sebollin.github.io/lupa/reference/proponer_modelo.md)
  : Proponer un modelo de calidad desde el profiling
- [`modelo_desde_propuesta()`](https://sebollin.github.io/lupa/reference/modelo_desde_propuesta.md)
  : Materializar una propuesta de modelo de calidad

## Medir

Declarar métricas, referenciales, granularidad y agregaciones.

- [`marco_calidad()`](https://sebollin.github.io/lupa/reference/marco_calidad.md)
  [`marco_agesic()`](https://sebollin.github.io/lupa/reference/marco_calidad.md)
  [`marco_iso25012()`](https://sebollin.github.io/lupa/reference/marco_calidad.md)
  [`marco_cepal()`](https://sebollin.github.io/lupa/reference/marco_calidad.md)
  : Declarar una taxonomía de calidad de datos
- [`metrica()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md)
  [`especializar()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md)
  [`instanciar()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md)
  [`propiedades_metrica()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md)
  [`modelo()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md)
  [`metricas_nucleo()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md)
  : Construir métricas y modelos de calidad
- [`referencial()`](https://sebollin.github.io/lupa/reference/referencial.md)
  : Declarar un conjunto de datos referencial
- [`vigencia()`](https://sebollin.github.io/lupa/reference/contratos_medicion.md)
  [`escala()`](https://sebollin.github.io/lupa/reference/contratos_medicion.md)
  : Declarar vigencia y escala de medición
- [`metricas_referencial()`](https://sebollin.github.io/lupa/reference/metricas_referencial.md)
  : Métricas que consumen un referencial tabular
- [`catalogo_agesic()`](https://sebollin.github.io/lupa/reference/catalogo_agesic.md)
  : Correspondencia con el catálogo de métricas de AGESIC
- [`granularidades()`](https://sebollin.github.io/lupa/reference/granularidades.md)
  [`transiciones_granularidad()`](https://sebollin.github.io/lupa/reference/granularidades.md)
  : Granularidades y transiciones de agregación
- [`medir()`](https://sebollin.github.io/lupa/reference/medir.md) :
  Medir un modelo de calidad
- [`agregar()`](https://sebollin.github.io/lupa/reference/agregar.md) :
  Agregar medidas entre granularidades
- [`tablero_calidad()`](https://sebollin.github.io/lupa/reference/tablero_calidad.md)
  : Construir un tablero de calidad
- [`indice_calidad()`](https://sebollin.github.io/lupa/reference/indice_calidad.md)
  : Calcular un índice de calidad declarado por el usuario
- [`estadisticos_estimacion()`](https://sebollin.github.io/lupa/reference/estadisticos_estimacion.md)
  : Catálogo de estadísticos de estimación reconocidos
- [`medicion_desde_estimaciones()`](https://sebollin.github.io/lupa/reference/medicion_desde_estimaciones.md)
  : Llevar estimaciones ya calculadas al contrato de medición

## Colecciones

Declarar una base de varias tablas y perfilarla como conjunto.

- [`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md)
  : Declarar la frontera de una colección
- [`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md)
  : Perfilar una colección declarada
- [`estimar_costo_coleccion()`](https://sebollin.github.io/lupa/reference/estimar_costo_coleccion.md)
  : Estimar el costo de buscar relaciones en una colección
- [`relaciones_coleccion()`](https://sebollin.github.io/lupa/reference/relaciones_coleccion.md)
  : Buscar claves foráneas candidatas entre pares declarados

## Validar

Validadores internacionales y packs territoriales extensibles.

- [`validar_url()`](https://sebollin.github.io/lupa/reference/validadores_formato.md)
  [`validar_iso3166()`](https://sebollin.github.io/lupa/reference/validadores_formato.md)
  [`validar_iso4217()`](https://sebollin.github.io/lupa/reference/validadores_formato.md)
  [`validar_correo()`](https://sebollin.github.io/lupa/reference/validadores_formato.md)
  [`validar_luhn()`](https://sebollin.github.io/lupa/reference/validadores_formato.md)
  [`validar_mod97()`](https://sebollin.github.io/lupa/reference/validadores_formato.md)
  : Validadores internacionales de sintaxis y dígitos de control
- [`validar_ci_uy()`](https://sebollin.github.io/lupa/reference/validadores_uy.md)
  [`validar_rut_uy()`](https://sebollin.github.io/lupa/reference/validadores_uy.md)
  : Validadores estructurales de Uruguay
- [`pack_validadores()`](https://sebollin.github.io/lupa/reference/pack_validadores.md)
  [`validadores_internacionales()`](https://sebollin.github.io/lupa/reference/pack_validadores.md)
  [`validadores_uruguay()`](https://sebollin.github.io/lupa/reference/pack_validadores.md)
  : Crear y consultar packs de validadores

## Evaluar

Aplicar reglas y perfiles de madurez a las medidas.

- [`regla_evaluacion()`](https://sebollin.github.io/lupa/reference/reglas_evaluacion.md)
  [`perfil_evaluacion()`](https://sebollin.github.io/lupa/reference/reglas_evaluacion.md)
  [`perfiles_madurez()`](https://sebollin.github.io/lupa/reference/reglas_evaluacion.md)
  : Reglas y perfiles de evaluación
- [`propiedades_regla()`](https://sebollin.github.io/lupa/reference/propiedades_regla.md)
  : Propiedades declaradas de una regla de evaluación
- [`evaluar()`](https://sebollin.github.io/lupa/reference/evaluar.md) :
  Evaluar medidas, reglas y perfiles
- [`comparar_evaluaciones()`](https://sebollin.github.io/lupa/reference/comparar_evaluaciones.md)
  : Comparar evaluaciones de perfil

## Remediar

Revisar y aplicar planes trazables sin mutación implícita.

- [`planificar_limpieza()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)
  [`aplicar()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)
  : Construir y aplicar un plan de limpieza auditable

- [`guiar_limpieza()`](https://sebollin.github.io/lupa/reference/guiar_limpieza.md)
  : Revisar decisiones de limpieza paso a paso

- [`sentinelas_naniar`](https://sebollin.github.io/lupa/reference/sentinelas_naniar.md)
  :

  Sentinelas numéricos publicados por
  [naniar](https://github.com/njtierney/naniar)

## Monitorear

Acumular corridas y detectar cambios entre entregas.

- [`historico_calidad()`](https://sebollin.github.io/lupa/reference/historico_calidad.md)
  [`acumular_historico()`](https://sebollin.github.io/lupa/reference/historico_calidad.md)
  : Construir y ampliar un histórico de calidad
- [`guardar_historico()`](https://sebollin.github.io/lupa/reference/guardar_historico.md)
  [`leer_historico()`](https://sebollin.github.io/lupa/reference/guardar_historico.md)
  : Guardar y recuperar un histórico de calidad
- [`detectar_deriva_calidad()`](https://sebollin.github.io/lupa/reference/detectar_deriva_calidad.md)
  : Detectar deriva en una serie de evaluaciones
- [`comparar_perfiles()`](https://sebollin.github.io/lupa/reference/comparar_perfiles.md)
  : Comparar dos perfiles y detectar deriva estructural

## Informar

Persistir un análisis y crear un reporte HTML autocontenido.

- [`guardar_analisis()`](https://sebollin.github.io/lupa/reference/persistir_analisis.md)
  [`leer_analisis()`](https://sebollin.github.io/lupa/reference/persistir_analisis.md)
  : Guardar y recuperar un análisis
- [`reportar()`](https://sebollin.github.io/lupa/reference/reportar.md)
  : Crear un reporte HTML autocontenido
