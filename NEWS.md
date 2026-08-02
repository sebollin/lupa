# lupa 0.1.0

## Examinar datos

- Perfila tablas administrativas con métricas generales y por columna,
  proporciones en `[0, 1]` y hallazgos filtrables.
- Descubre patrones de formato, tipos implícitos, formatos de fecha mixtos y
  ambiguos, años de dos dígitos, números regionales y problemas de codificación.
- Detecta claves candidatas, relaciones, cobertura referencial, columnas y
  filas duplicadas, y dependencias funcionales exactas o aproximadas.

## Medir y evaluar calidad

- Declara métricas genéricas, específicas e instanciadas con tipo de resultado
  y granularidad explícitos.
- Incluye catorce métricas automatizables, tres métricas tabulares basadas en
  referenciales y una correspondencia verificable con las 49 entradas del
  catálogo de AGESIC.
- Implementa las cuatro agregaciones del marco y la cadena de evaluación de
  medidas, reglas y perfiles de madurez. No calcula un índice global.
- Propone modelos editables a partir del perfil sin convertir observaciones de
  una sola entrega en requisitos silenciosos.

## Mejorar y monitorear

- Construye planes de limpieza editables con alternativas mutuamente
  excluyentes, justificación, modo guiado opcional y consentimiento adicional
  para eliminaciones.
- Aplica sólo acciones activas sobre una copia, conserva un registro y permite
  imputaciones confirmadas mediante dependencias funcionales exactas.
- Acumula evaluaciones en un histórico plano y versionado; detecta deriva del
  modelo y cambios estructurales entre perfiles.

## Informar

- Genera un único HTML autocontenido, en español, sin navegador, LaTeX ni
  recursos externos; todos los valores provenientes de los datos se escapan.
