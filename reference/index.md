# Índice del paquete

## Examinar

Perfilar datos y descubrir su estructura.

<!-- end list -->

  - `analizar()` : Ejecutar el análisis descriptivo completo
  - `perfilar()` : Perfilar un conjunto de datos
  - `distribucion_valores()` : Distribuciones de valores y cuantiles por
    columna
  - `detectar_asociaciones()` : Detectar asociaciones entre columnas
  - `analizar_tiempo()` : Examinar regularidad y cobertura temporal
  - `clasificar_variables()` : Proponer escalas y roles de las variables
  - `descubrir_patrones()` : Descubrir patrones de formato
  - `inferir_tipo()` : Inferir el tipo implícito de un vector
  - `detectar_formatos_fecha()` : Detectar formatos de fecha
  - `detectar_claves()` : Detectar claves candidatas
  - `detectar_dependencias()` : Detectar dependencias funcionales entre
    columnas
  - `detectar_relaciones()` : Detectar relaciones entre dos tablas
  - `detectar_duplicados_aproximados()` : Detectar pares de filas con
    similitud aproximada
  - `estimar_costo()` : Estimar el costo de una comparación de
    duplicados
  - `cobertura_analisis()` : Informar la cobertura conceptual de un
    análisis
  - `as_tibble(<perfil>)` : Convertir un perfil a tibble
  - `datos_administrativos` : Datos administrativos sintéticos con
    problemas sembrados
  - `datos_operativos` : Datos operativos sintéticos y neutrales

## Proponer

Convertir diagnósticos en una propuesta editable de medición.

<!-- end list -->

  - `proponer_modelo()` : Proponer un modelo de calidad desde el
    profiling
  - `modelo_desde_propuesta()` : Materializar una propuesta de modelo de
    calidad

## Medir

Declarar métricas, referenciales, granularidad y agregaciones.

<!-- end list -->

  - `marco_calidad()` `marco_agesic()` `marco_iso25012()` : Declarar una
    taxonomía de calidad de datos
  - `metrica()` `especializar()` `instanciar()` `propiedades_metrica()`
    `modelo()` `metricas_nucleo()` : Construir métricas y modelos de
    calidad
  - `referencial()` : Declarar un conjunto de datos referencial
  - `vigencia()` `escala()` : Declarar vigencia y escala de medición
  - `metricas_referencial()` : Métricas que consumen un referencial
    tabular
  - `catalogo_agesic()` : Correspondencia con el catálogo de métricas de
    AGESIC
  - `granularidades()` `transiciones_granularidad()` : Granularidades y
    transiciones de agregación
  - `medir()` : Medir un modelo de calidad
  - `agregar()` : Agregar medidas entre granularidades

## Validar

Validadores internacionales y packs territoriales extensibles.

<!-- end list -->

  - `validar_iso3166()` `validar_iso4217()` `validar_correo()`
    `validar_luhn()` `validar_mod97()` : Validadores internacionales de
    sintaxis y dígitos de control
  - `validar_ci_uy()` `validar_rut_uy()` : Validadores estructurales de
    Uruguay
  - `pack_validadores()` `validadores_internacionales()`
    `validadores_uruguay()` : Crear y consultar packs de validadores

## Evaluar

Aplicar reglas y perfiles de madurez a las medidas.

<!-- end list -->

  - `regla_evaluacion()` `perfil_evaluacion()` `perfiles_madurez()` :
    Reglas y perfiles de evaluación
  - `evaluar()` : Evaluar medidas, reglas y perfiles
  - `comparar_evaluaciones()` : Comparar evaluaciones de perfil

## Remediar

Revisar y aplicar planes trazables sin mutación implícita.

<!-- end list -->

  - `planificar_limpieza()` `aplicar()` : Construir y aplicar un plan de
    limpieza auditable

  - `guiar_limpieza()` : Revisar decisiones de limpieza paso a paso

  - `sentinelas_naniar` :
    
    Sentinelas numéricos publicados por
    [naniar](https://github.com/njtierney/naniar)

## Monitorear

Acumular corridas y detectar cambios entre entregas.

<!-- end list -->

  - `historico_calidad()` `acumular_historico()` : Construir y ampliar
    un histórico de calidad
  - `guardar_historico()` `leer_historico()` : Guardar y recuperar un
    histórico de calidad
  - `detectar_deriva_calidad()` : Detectar deriva en una serie de
    evaluaciones
  - `comparar_perfiles()` : Comparar dos perfiles y detectar deriva
    estructural

## Informar

Persistir un análisis y crear un reporte HTML autocontenido.

<!-- end list -->

  - `guardar_analisis()` `leer_analisis()` : Guardar y recuperar un
    análisis
  - `reportar()` : Crear un reporte HTML autocontenido
