# Medir un modelo de calidad

Ejecuta todas las métricas instanciadas de un `modelo_calidad`. Cada
fila es una medida reutilizable y conserva el identificador y la fecha
de la corrida.

## Usage

``` r
# S3 method for class 'medicion_resumen'
rbind(..., prioridad = NULL, deparse.level = 1)

medir(
  modelo,
  datos,
  id_medicion = NULL,
  fecha = Sys.time(),
  aplicabilidad = NULL,
  proteger_datos_personales = FALSE,
  columnas_personales = character(),
  validadores_personales = NULL,
  detalle = c("completo", "resumen"),
  orden_texto = c("idioma", "bytes")
)
```

## Arguments

- ...:

  En [`rbind()`](https://rdrr.io/r/base/cbind.html), los resúmenes que
  se unen; con `prioridad`, con nombre: el nombre de cada parte es su
  `fuente`.

- prioridad:

  En [`rbind()`](https://rdrr.io/r/base/cbind.html) sobre resúmenes, los
  nombres de todas las partes, de mayor a menor. Ver «Partes de una
  corrida».

- deparse.level:

  Se ignora; existe por el genérico
  [`rbind()`](https://rdrr.io/r/base/cbind.html).

- modelo:

  **Primer argumento.** Objeto operativo creado por
  [`modelo()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md);
  reúne las métricas instanciadas que se van a ejecutar. No es el objeto
  `perfil` que devuelve
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  ni el marco conceptual de
  [`marco_calidad()`](https://sebollin.github.io/lupa/reference/marco_calidad.md).

- datos:

  **Segundo argumento.** Data frame para una sola entidad o lista con
  nombre de data frames para varias entidades. Es la tabla que se mide,
  no un perfil ni un modelo. `medir()` mide los valores como llegaron:
  una columna `BIT` de MySQL leída con RMySQL llega en `0` aunque el bit
  esté encendido
  ([`medir_dbi()`](https://sebollin.github.io/lupa/reference/medir_dbi.md)
  no mide sus valores).

  Con una lista, tienen que estar las tablas de **todas** las entidades
  que el modelo mide; si falta alguna se rechaza antes de medir,
  nombrando todas las que faltan y cuáles se recibieron. Una tabla que
  el modelo no pide **se ignora**: no hace falta que la lista coincida
  exactamente con las entidades, sólo que no falte ninguna.

- id_medicion:

  Identificador de corrida. Si se omite, se genera uno. Cada medida
  lleva un `id_medida` que lo antepone a su métrica instanciada y la
  numera dentro de ella —`M-NoNulo@t.x-000001`—: una corrida armada por
  partes con el mismo `id_medicion` y unida con
  [`rbind()`](https://rdrr.io/r/base/cbind.html) no repite
  identificadores, y la misma métrica medida dos veces en la corrida sí,
  que es lo que
  [`evaluar()`](https://sebollin.github.io/lupa/reference/evaluar.md) y
  [`historico_calidad()`](https://sebollin.github.io/lupa/reference/historico_calidad.md)
  rechazan.

- fecha:

  Fecha y hora de la corrida. Se guarda en UTC; un texto —`"2026-03-01"`
  o `"2026-03-01 09:30:00"`— se lee en UTC, no en el huso de la sesión,
  así que la misma llamada registra el mismo instante en cualquier
  máquina. Un desplazamiento escrito en el texto
  —`"2026-03-01 09:30:00-03:00"`, `Z`, `+0100`— se respeta, y un texto
  que no se puede leer entero se rechaza.

- aplicabilidad:

  Lista con nombre por columna, donde cada elemento es una fórmula que
  dice en qué filas esa columna corresponde —por ejemplo
  `list(marca_auto = ~ tiene_auto == "Si")`—. Las filas fuera de ese
  universo salen de la medición en vez de contarse como ausencia. Una
  métrica de varias columnas se mide donde corresponden todas. Las
  medidas citan la fila de la tabla original —`fila` y
  `objeto_medible`—, no su posición en el universo.

  Es la misma declaración que recibe
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
  y hace falta porque una métrica de completitud sobre una columna
  condicionada mide lo que no corresponde: con universo de 300 filas
  sobre 1.000 y 30 vacías de verdad, sin declararlo da `0,270` y
  declarándolo `0,900`. El histórico y la deriva consumen mediciones,
  así que heredan el número que salga de acá.

  Sin declaración toda la tabla aplica y el resultado es el de siempre.

  Estas fórmulas —y la propiedad `aplicable` de una métrica como
  `NoNulo`— se evalúan con R tal como están escritas: `orden_texto` no
  las toca. Una comparación de texto ahí sigue el orden del locale de la
  sesión, una expresión regular sus clases de caracteres, un número
  comparado con un texto `scipen` y `OutDec`, y un instante comparado
  con un texto el huso. El sello registra ese entorno, y comparar dos
  corridas medidas en otro lo avisa.

- proteger_datos_personales:

  Si se enmascaran los candidatos de proximidad que corresponden a
  columnas personales. Por omisión `FALSE`. También gobierna lo que
  escribe un método propio: si alguna tabla recibida tiene columnas
  personales, su etiqueta `objeto` se reemplaza por la canónica del
  paquete (`tabla$columna[fila]`), y los valores de esas columnas se
  enmascaran en el motivo de una métrica `no_medible`. La protección
  alcanza a lo que `medir()` recibe: un valor que un método trae de otro
  lado —una tabla que no se le pasó a `medir()`— no se puede reconocer,
  y publicarlo es responsabilidad de quien escribe el método.

- columnas_personales:

  Columnas que traen datos personales, declaradas con la misma forma que
  acepta
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md):
  nombres de columna, o un vector con nombre donde el nombre es la
  columna y el valor es el tipo. Se protegen aunque el léxico por
  omisión no las reconozca —un nombre propio de la organización, un
  identificador interno—. Sólo tiene efecto con
  `proteger_datos_personales = TRUE`.

- validadores_personales:

  Pack o lista nombrada de funciones de validación, con la misma forma
  que acepta
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md).
  Sin esto, la medición usa el léxico por omisión.

- detalle:

  Nivel de detalle de la salida: `"completo"` (por omisión, una fila por
  objeto medido, clase `medicion`) o `"resumen"` (una fila por métrica
  instanciada y objeto de destino con `numerador`, `denominador`,
  `n_universo`, `n_no_evaluados`, `motivo_no_evaluados` y
  `unidad_conteo`, clase propia `medicion_resumen` sin herencia de
  `medicion`). El resumen produce el mismo
  [`tablero_calidad()`](https://sebollin.github.io/lupa/reference/tablero_calidad.md),
  [`indice_calidad()`](https://sebollin.github.io/lupa/reference/indice_calidad.md)
  y
  [`cobertura_analisis()`](https://sebollin.github.io/lupa/reference/cobertura_analisis.md)
  que la medición completa;
  [`evaluar()`](https://sebollin.github.io/lupa/reference/evaluar.md),
  [`agregar()`](https://sebollin.github.io/lupa/reference/agregar.md),
  [`historico_calidad()`](https://sebollin.github.io/lupa/reference/historico_calidad.md)
  y
  [`reportar()`](https://sebollin.github.io/lupa/reference/reportar.md)
  lo rechazan indicando qué hacer.

  **Las cuentas de cada cifra.** El resumen publica además `n_sin_valor`
  (celdas o filas del universo con alguna columna de valor vacía según
  [`is.na()`](https://rdrr.io/r/base/NA.html), `NaN` incluido),
  `n_filas_leidas` (filas de la tabla que vio la lectura que produjo la
  cifra, antes del recorte por aplicabilidad), `entidad_referida` y
  `n_filas_leidas_referida` (en una referencial, la tabla referida y sus
  filas en la misma lectura; con un referencial en R, su nombre —o
  `referencial <nombre>` si es también el de una entidad— y sus filas),
  y `estado_cuentas`, `motivo_cuentas` y `diferencia_cuentas`.
  `estado_cuentas` es `cierran`, `no_cierran` (la métrica dio más
  medidas que su universo; lo no evaluado no son los vacíos que la
  métrica declara no evaluar; o dos lecturas de la misma tabla en la
  corrida vieron otra tabla), `parcial` (la cifra se calculó sobre una
  parte declarada: un vocabulario truncado por `max_valores`) o
  `sin_cuentas`. Cada métrica del catálogo declara su universo: `NoNulo`
  evalúa también los vacíos —es lo que mide—; una de validez evalúa los
  valores presentes. Lo escribe quien produce la cifra y avisa cuando
  alguna no cierra;
  [`tablero_calidad()`](https://sebollin.github.io/lupa/reference/tablero_calidad.md)
  lo publica en su atributo `cuentas`,
  [`indice_calidad()`](https://sebollin.github.io/lupa/reference/indice_calidad.md)
  no combina esas cifras y
  [`semaforo_calidad()`](https://sebollin.github.io/lupa/reference/semaforo_calidad.md)
  no las pinta. `attr(x, "lecturas")` registra cada lectura de cada
  tabla, con su huella de datos en memoria; dos partes de una corrida
  sobre la misma tabla se reconocen por esa huella. Un referencial en R
  se registra también, en
  [`medir_dbi()`](https://sebollin.github.io/lupa/reference/medir_dbi.md)
  con `estrategia_no_sql = "bloques"` igual que en `medir()`, y lo
  identifican su nombre y la huella de sus datos, que van en la columna
  `tabla` de su lectura: dos referenciales distintos con el mismo nombre
  —el nombre por omisión de `referencial(<expresión>)` es siempre
  `referencial`— son dos referenciales y no uno que cambió, cada cifra
  cierra con la lectura del suyo, y eso no depende de si van en una
  llamada o en varias partes. En el motor, la columna `tabla` de las
  lecturas es la tabla que el motor resolvió —su catálogo, su esquema y
  su nombre como los guarda el catálogo, buscada en el orden del motor
  para un nombre sin esquema (en SQLite `temp`, `main` y las adjuntas;
  en DuckDB `temp` y después `search_path`) y comparando los nombres
  como el motor, cada parte citada—: dos rótulos de la misma tabla
  (`Sucursales` y `sucursales`, `MAIN` y `main`) se comparan como una.
  Si la base es un archivo, la tabla lleva la ruta del archivo en lugar
  del nombre con que se adjuntó: el mismo archivo con dos alias es una
  sola base. Una lectura del motor cuya tabla no se resolvió (`tabla`
  `NA`; en motores distintos de SQLite y DuckDB, todavía todas) no
  prueba nada: el control de sus cifras es `desconocido`.
  [`write.csv()`](https://rdrr.io/r/utils/write.table.html) exporta las
  columnas, no los atributos: lecturas, sello, cobertura y consolidación
  no viajan en un CSV.

  **Partes de una corrida.**
  [`rbind()`](https://rdrr.io/r/base/cbind.html) une resúmenes de una
  misma corrida —mismo `id_medicion` y misma `fecha`— que no repiten
  métrica y objeto, y vuelve a mirar las cuentas de la unión: dos partes
  que leyeron versiones distintas de una tabla quedan `no_cierran`.
  También si una parte la leyó en memoria y otra en el motor, o si las
  lecturas de una no traen la tabla que resolvió el motor: sin una
  identidad que comparar, se comparan por el nombre de la entidad, y con
  otro número de filas no cierran; con el mismo, cierran y el control de
  lecturas es `desconocido`. Una métrica con cifra en una parte y sin
  medir en otra se rechaza. Con `prioridad`
  (`rbind(motor = a, memoria = b, prioridad = c("motor", "memoria"))`)
  la misma celda en varias partes se resuelve por la parte de mayor
  prioridad, que queda en la columna `fuente`, y lo no elegido en
  `attr(x, "descartadas")` con si coincide con lo elegido. Sólo junta lo
  comparable: una corrida, el sello de cada parte con certificación
  `completa` y la misma huella y versión de lupa para cada métrica
  compartida, la misma aplicabilidad y el mismo marco, y **la misma
  instantánea de los datos** de cada tabla leída por más de una parte
  —la huella de los datos en memoria o el contador de modificaciones del
  motor—. La igualdad de filas no alcanza: si no hay cómo probar la
  instantánea, no se junta y se dice por qué. Una consolidación no se
  vuelve a unir. Las partes van enteras: una recortada a algunas
  columnas, con columnas que otra no trae o con filas vacías (las que
  agrega `x[cond, ]` cuando `cond` tiene `NA`) se rechaza con clase
  `lupa_resumen_incompleto`.

- orden_texto:

  Cómo se ordena el texto en las reglas que el paquete interpreta con un
  operador de orden: el rango (`minimo` y `maximo`) de
  `ValoresPosiblesPorComprension` y las comparaciones `<`, `<=`, `>` y
  `>=` de la fórmula de `ReglaIntegridadIntraEntidad`. Con `"idioma"`
  (por omisión) es el orden del locale de la sesión (`LC_COLLATE`), como
  siempre, y la misma regla puede dar otro número en otra máquina: en
  `es_UY.UTF-8`, `"A"` cae entre `"a"` y `"b"`; en `C`, no. Con
  `"bytes"` se compara por los bytes del UTF-8, que es el orden por
  punto de código y es el mismo en toda sesión: `"Z"` va antes que
  `"a"`, y `"ñ"` después de `"z"`. Sólo cambian las comparaciones donde
  R compara texto; un número o una fecha se comparan igual con los dos.
  Una función escrita por quien llama (`regla = function(d) ...`,
  `predicado`), `aplicable` y `aplicabilidad` se evalúan como están
  escritas, y `orden_texto` no las toca: aun con `"bytes"`, una
  comparación de texto ahí (`s < "b"`, `min(s)`, `sort(s)`) sigue el
  orden del locale de la sesión. El sello registra ese locale (ver
  «Sello de cada métrica»), y comparar dos corridas medidas con otro lo
  avisa.
  [`medir_dbi()`](https://sebollin.github.io/lupa/reference/medir_dbi.md)
  mide esas reglas sobre texto sólo con `"bytes"`.

## Value

Data frame S3 de clase `medicion`, con una fila por objeto medido. Los
booleanos se almacenan como `0` y `1` en la columna común `resultado`.
`orientacion` conserva si un valor alto expresa conformidad, si un valor
alto expresa defecto o si esa lectura no aplica. Algunas métricas que
trabajan con un vocabulario o un alcance parcial agregan un atributo
`alcance_metricas` con sus conteos y límites. Si una métrica no puede
medirse, no crea filas ni ceros: deja el motivo y cómo resolverlo en el
atributo `cobertura_metricas`, con un estado que distingue tres causas:
`sin_valores` cuando su universo no tiene valores, `contrato_incompleto`
cuando su contrato no trae un campo que necesita, y `no_medible` cuando
su método falló sobre estos datos —una columna sin dos valores para
`ErrorEstandar`, un atributo que la tabla no trae, una regla que
devuelve `NA`—, devolvió una salida que no cumple el contrato de
`metodo`, o no devolvió ninguna medida sobre un universo que sí tiene
valores. Cuando el método falló, el motivo conserva su mensaje, con los
valores de las columnas personales enmascarados si
`proteger_datos_personales = TRUE`, y `medir()` **avisa**: la falla no
se lleva la medición de las demás métricas, pero tampoco queda muda. Y
cuando **sí** pudo medirse pero sobre **menos** elementos de los que hay
en su universo aplicable —una métrica por celda no mide la celda vacía,
que no produce medida ni cuenta como incumplimiento—, el atributo
`alcance_medidas` publica cuántos midió de cuántos, con su unidad: sin
ese número, el agregado de tres celdas de cuatro no se distingue del de
cuatro. Viaja al tablero, se imprime con la medición y se publica en el
informe. También conserva `configuracion_modelo` y
`configuracion_aplicabilidad`, descripciones de la política usada para
que una deriva posterior pueda distinguir modelo de datos. Si el modelo
declara un marco, la medición conserva tambien `marco_calidad`; la
configuracion del modelo registra su nombre, sus pares dimension-factor
y el `tipo_resultado` de cada metrica.

Esa descripcion es **insensible al orden de la declaracion**: declarar
las mismas metricas, o los mismos pares del marco, en otro orden no
cambia el modelo, porque un modelo es un conjunto de instrumentos y un
marco un conjunto de pares. El orden de los `atributos` de una instancia
**sí** es parte del modelo, porque el metodo recibe las columnas en ese
orden. Y el `metodo` de medicion entra en la descripcion solo si lo
declaro quien llama: el metodo por omision de una metrica es del paquete
y su identidad ya viaja en el nombre de la metrica, de modo que un
cambio interno de lupa no se lee como un cambio del modelo del usuario.

Con `detalle = "resumen"`, devuelve un `data.frame` S3 de clase
`medicion_resumen` (sin heredar de `medicion`) con una fila por métrica
instanciada y objeto de destino. Las métricas no acotadas por celda
(`duracion`, `entero`, `numero_real` por celda) no se promedian y quedan
en `cobertura_metricas` con estado `no_resumible`.

**Sello de cada métrica.** El atributo `sello_metricas` tiene una fila
por métrica instanciada del modelo —también las que no se pudieron
medir— con `id_medicion`, `metrica_instanciada`, `metrica_especifica`,
`version_lupa` (la versión de lupa que midió), `huella_definicion` —el
md5 de una descripción canónica de su definición—, `certificacion`
—cuánto de la definición ve esa huella— y `motivo_huella`. Entran los
campos de la métrica que deciden qué se mide y cómo se lee (`nombre`,
`granularidad`, `tipo_resultado`, `orientacion`, `propiedades`,
`dimension` y `factor`; no la `semantica`, que es su descripción en
prosa), los parámetros con que se especializó, la entidad y los
atributos en su orden, el referencial (su clave, sus valores y sus
filas, no su nombre), el método cuando no es el del paquete, las reglas
de `aplicabilidad` de sus atributos y `orden_texto = "bytes"` si la
métrica ordena texto en esta medición —un rango o una comparación `<`,
`<=`, `>`, `>=` con algún operando de texto—. El método por omisión de
una métrica del paquete no entra: lo que haga en cada versión lo dice
`version_lupa`.

`certificacion` tiene tres niveles:

- `"completa"`: la definición es toda dato, y la huella la ve entera.
  Son las métricas del paquete con sus parámetros —valores, vectores,
  tablas, fechas, factores, con sus nombres y sus atributos—, la fórmula
  de `ReglaIntegridadIntraEntidad` (que no lee su entorno), el
  referencial, una fórmula de `aplicabilidad` o de `aplicable` que sólo
  compara columnas y valores con funciones que no leen nada más que sus
  argumentos (`==`, `%in%`, [`is.na()`](https://rdrr.io/r/base/NA.html),
  [`nchar()`](https://rdrr.io/r/base/nchar.html),
  [`grepl()`](https://rdrr.io/r/base/grep.html),
  [`ifelse()`](https://rdrr.io/r/base/ifelse.html) y otras así), y
  `orden_texto`. «No leen nada más» quiere decir: nada más que sus
  argumentos y el entorno de la sesión que el sello registra (abajo);
  por eso entran [`paste()`](https://rdrr.io/r/base/paste.html),
  [`gsub()`](https://rdrr.io/r/base/grep.html),
  [`sort()`](https://rdrr.io/r/base/sort.html),
  [`mean()`](https://rdrr.io/r/base/mean.html),
  [`weekdays()`](https://rdrr.io/r/base/weekday.POSIXt.html) y otras de
  base, y quedan afuera [`format()`](https://rdrr.io/r/base/format.html)
  (lee `digits`, que no se registra),
  [`Sys.time()`](https://rdrr.io/r/base/Sys.time.html),
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html) y lo que aplica
  otra función ([`sapply()`](https://rdrr.io/r/base/lapply.html)). Todo
  valor que entra —un parámetro, una columna del referencial, un literal
  de una fórmula— tiene que ser de una clase que el paquete lee como
  dato: sin clase, `factor`, `Date`, `POSIXct`, `POSIXlt`, `difftime`,
  `integer64`, un `data.frame` con columnas de esas clases, o un objeto
  del paquete (un referencial, una normalización). Si la huella no
  cambió, la definición no cambió.

- `"parcial"`: la definición incluye una función de quien llama —una
  regla escrita como función, un método declarado en
  [`instanciar()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md),
  el de una métrica escrita con
  [`metrica()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md)—,
  una función de un paquete que no es de R ni lupa, o una fórmula que
  llama a otra función. La huella ve su texto, sus atributos y el valor
  de cada variable libre que nombra y que no es de R ni de un paquete
  (`function(d) d$x > u` con `u = 1` y con `u = 2` tienen huellas
  distintas); de una función de un paquete, su versión y su cuerpo. Lo
  que no ve es lo que un código de R puede leer sin nombrarlo: un
  atributo pedido con `body("f")`, una función armada como texto, lo que
  [`local()`](https://rdrr.io/r/base/eval.html) o
  [`assign()`](https://rdrr.io/r/base/assign.html) cambian,
  [`...elt()`](https://rdrr.io/r/base/dots.html). También es parcial lo
  que la huella ve a medias: el argumento sin evaluar de una fábrica de
  funciones —`crear(umbral)`; entra su código, no su valor—, una cadena
  de más de ocho funciones que se llaman entre sí, o un valor de más de
  un millón de elementos (diez millones en el referencial), que se
  describe por su tipo, su largo y su clase sin leerlo. Y un valor de
  cualquier otra clase, en cualquier nivel —«valor con clase propia: su
  método puede cambiar lo que mide»—, o de una clase de dato o del
  paquete para la que quien llama escribió un método (un `$.referencial`
  o un `format.Date` en el entorno global, o uno registrado para una
  clase del paquete): la métrica lo lee con esos métodos, y la huella ve
  el valor, no el método. Si la huella cambió, la definición cambió; si
  no cambió, no se afirma que siga igual.

- `"no_certificable"`: la definición lee algo que la huella no puede
  describir, y `huella_definicion` es `"no_certificable"`. Un entorno,
  un puntero externo, una conexión, un objeto S4, un enlace activo,
  `..1` de una fábrica, o una lectura que no se ve en el texto:
  [`get()`](https://rdrr.io/r/base/get.html),
  [`mget()`](https://rdrr.io/r/base/get.html),
  [`dynGet()`](https://rdrr.io/r/base/get.html),
  [`exists()`](https://rdrr.io/r/base/exists.html),
  [`eval()`](https://rdrr.io/r/base/eval.html),
  [`parse()`](https://rdrr.io/r/base/parse.html),
  [`source()`](https://rdrr.io/r/base/source.html),
  [`getOption()`](https://rdrr.io/r/base/options.html),
  [`options()`](https://rdrr.io/r/base/options.html),
  [`Sys.getenv()`](https://rdrr.io/r/base/Sys.getenv.html), un entorno
  (`.GlobalEnv`,
  [`globalenv()`](https://rdrr.io/r/base/environment.html),
  [`environment()`](https://rdrr.io/r/base/environment.html),
  [`ls()`](https://rdrr.io/r/base/ls.html)), el marco de quien llama
  ([`sys.function()`](https://rdrr.io/r/base/sys.parent.html),
  [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html)), el
  despacho ([`UseMethod()`](https://rdrr.io/r/base/UseMethod.html)) o
  una de esas funciones con otro nombre o pasada a otra (`f <- get`,
  `sapply(x, "get")`).

**El entorno de la sesión.** Lo que puede cambiar lo medido sin cambiar
la definición va aparte de la huella, en
`attr(sello_metricas, "entorno")`: por corrida, `scipen` y `OutDec` (un
número comparado con un texto pasa por
[`as.character()`](https://rdrr.io/r/base/character.html)), `LC_COLLATE`
(el orden del texto), `LC_CTYPE` (las clases de caracteres), `LC_TIME`
(los nombres de meses y días), el huso (`TZ`) y la versión de R. Con
`scipen = 0` y `scipen = 100`, `NoNulo(valores_nulos = 100000)` sobre el
texto `"100000"` da otro resultado con la misma definición. Comparar dos
corridas con la misma huella y otro entorno avisa «misma definición,
otro entorno» con lo que cambió —no las declara `no_comparable`: la
definición es la misma—, y el histórico (`cobertura_sello`) y la deriva
(`cobertura_diagnosticos`) lo declaran. Un sello que no registra su
entorno, de una versión anterior, se lee como «entorno desconocido».

`motivo_huella` dice por qué una huella es parcial o no certificable.
Para una huella completa, escribir la regla como fórmula y pasar sus
valores como parámetros de
[`especializar()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md).
Una función de un paquete cargado con `pkgload` lo anota: su versión no
fija su código.

Sellar no ejecuta código de quien llama y nunca hace fallar ni demora
una medición: no evalúa una promesa, no dispara un enlace activo, no
despacha métodos —un objeto con clase se describe sin su clase, con la
clase como dato, así que un
[`length()`](https://rdrr.io/r/base/length.html), `[[` o
[`format()`](https://rdrr.io/r/base/format.html) propios no corren— y no
lee lo que pasa del tope —`1:1e9` en el entorno de una regla ocupa 680
bytes, y leerlo pediría 8 GB—. Si describir una métrica fallara igual,
esa métrica queda `"no_certificable"` con el error en `motivo_huella` y
la medición sigue. Límites: en el entorno global R no deja ver una
promesa sin evaluarla, y una creada ahí con
[`delayedAssign()`](https://rdrr.io/r/base/delayedAssign.html) se evalúa
al sellar; un vector ALTREP de una clase escrita en C no se distingue de
uno común sin [`.Internal()`](https://rdrr.io/r/base/Internal.html):
hasta un millón de elementos se copia una vez para describirlo, y esa
copia corre una vez el código de lectura de su clase; un método escrito
por quien llama para una clase implícita (`format.numeric`) no se busca,
ni uno de una clase de base registrado en la tabla de métodos de un
paquete; una variable que la regla modifica con `<<-` entra con su valor
del momento, y un contador que cambia en cada corrida cambia la huella
aunque la regla mida lo mismo.

La huella es la misma en otra sesión, en otro proceso y en otra máquina,
y no cambia con lo que no cambia el significado: el orden de los
argumentos de
[`especializar()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md),
el de los valores de una propiedad que se usa como conjunto (`valores`,
`diccionario`, `valores_nulos`) o de `proteger` en
[`normalizacion()`](https://sebollin.github.io/lupa/reference/normalizacion.md),
un paréntesis de más, un entero contra el doble del mismo valor (`0L` y
`0`; el cero con signo, `-0`, sí se distingue), el orden de los
atributos que nombra la fórmula de `ReglaIntegridadIntraEntidad`, o una
columna del referencial como factor en vez de texto. Con
`orden_texto = "idioma"` la huella no registra el locale de la sesión:
para seguir una regla sobre texto entre máquinas, `"bytes"`.

Comparar dos mediciones lee el sello:
[`rbind()`](https://rdrr.io/r/base/cbind.html),
[`historico_calidad()`](https://sebollin.github.io/lupa/reference/historico_calidad.md),
[`acumular_historico()`](https://sebollin.github.io/lupa/reference/historico_calidad.md)
y
[`comparar_evaluaciones()`](https://sebollin.github.io/lupa/reference/comparar_evaluaciones.md)
avisan cuando una métrica cambió de definición de una corrida a la
siguiente, y
[`detectar_deriva_calidad()`](https://sebollin.github.io/lupa/reference/detectar_deriva_calidad.md)
lo declara en su fila `definicion_metrica`. Cuando la huella no cambió
pero es parcial, no se avisa: el histórico lo declara en
`attr(historico, "cobertura_sello")` y la deriva en
`cobertura_diagnosticos` («huella parcial»).
[`rbind()`](https://rdrr.io/r/base/cbind.html) rechaza dos sellos
distintos de la misma métrica con el mismo `id_medicion`: son dos
entregas, no una corrida armada por partes. Una medición guardada por
una versión anterior no trae sello: se compara como antes y se avisa que
no lo trae.

## See also

[`medir_dbi()`](https://sebollin.github.io/lupa/reference/medir_dbi.md),
[`plan_medicion_dbi()`](https://sebollin.github.io/lupa/reference/plan_medicion_dbi.md),
[`tablero_calidad()`](https://sebollin.github.io/lupa/reference/tablero_calidad.md),
[`indice_calidad()`](https://sebollin.github.io/lupa/reference/indice_calidad.md),
[`evaluar()`](https://sebollin.github.io/lupa/reference/evaluar.md),
[`agregar()`](https://sebollin.github.io/lupa/reference/agregar.md)

## Examples

``` r
nucleo <- metricas_nucleo()
especifica <- especializar(nucleo$NoNulo, nombre_especifico = "NoNuloEdad")
instancia <- instanciar(especifica, "personas", "edad")
medir(modelo(instancia), data.frame(edad = c(20, NA, 35)))
#>                                                              id_medida
#> 1 medicion-20261128T061240.661511-8381-NoNuloEdad@personas.edad-000001
#> 2 medicion-20261128T061240.661511-8381-NoNuloEdad@personas.edad-000002
#> 3 medicion-20261128T061240.661511-8381-NoNuloEdad@personas.edad-000003
#>                            id_medicion               fecha metrica
#> 1 medicion-20261128T061240.661511-8381 2026-11-28 06:12:40  NoNulo
#> 2 medicion-20261128T061240.661511-8381 2026-11-28 06:12:40  NoNulo
#> 3 medicion-20261128T061240.661511-8381 2026-11-28 06:12:40  NoNulo
#>   metrica_especifica      metrica_instanciada   dimension   factor orientacion
#> 1         NoNuloEdad NoNuloEdad@personas.edad Completitud Densidad conformidad
#> 2         NoNuloEdad NoNuloEdad@personas.edad Completitud Densidad conformidad
#> 3         NoNuloEdad NoNuloEdad@personas.edad Completitud Densidad conformidad
#>        granularidad tipo_resultado  entidad atributo fila   objeto_medible
#> 1 instanciaAtributo       booleano personas     edad    1 personas$edad[1]
#> 2 instanciaAtributo       booleano personas     edad    2 personas$edad[2]
#> 3 instanciaAtributo       booleano personas     edad    3 personas$edad[3]
#>   resultado agregacion
#> 1         1       <NA>
#> 2         0       <NA>
#> 3         1       <NA>
```
