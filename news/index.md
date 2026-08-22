# Changelog

## lupa 0.1.0

### Una tabla con acentos rompia el perfil entero

Es el defecto mas serio de la tanda, y **lo introdujo el arreglo del
orden del vocabulario de ayer**. Sobre cualquier tabla leida con
[`read.csv()`](https://rdrr.io/r/utils/read.table.html) que tuviera
acentos:

    Error: Character encoding must be UTF-8, Latin-1 or bytes

El orden por bytes de R rechaza una cadena marcada `unknown` que
contenga bytes no ASCII, **aunque sean UTF-8 perfectamente validos**, y
asi llega cualquier CSV en espanol por el camino mas comun que hay:
`"Combustibles liquidos"`, `"Energia Electrica"`. El mismo error estaba
en el desempate de la moda.

- Ahora la codificacion se marca antes de ordenar. Lo que despues de eso
  siga sin ser valido pasa por `iconv(sub = "byte")`: no es bonito, pero
  es determinista y ordenable. Caer al orden del entorno habria devuelto
  la dependencia de la maquina que este orden existe para sacar.
- Los otros tres usos de orden por bytes del paquete ordenan enteros,
  que no tienen requisito de codificacion.
- **Ni las cuatro auditorias externas ni las 15.696 comprobaciones de la
  suite lo encontraron, porque todos los fixtures son ASCII.** Aparecio
  buscando otra cosa: el registro publico con el que se cierra una fila
  de la tabla de evidencia. La prueba nueva construye la cadena con
  [`rawToChar()`](https://rdrr.io/r/base/rawConversion.html), porque un
  literal en el fuente lo marca el parser de R y el caso no se ejercita.

### La tabla de evidencia dice ahora con que se reproduce cada fila

Llego a publicar tres numeros que nadie podia comprobar desde el
repositorio. El problema de fondo no eran los tres numeros sino que la
tabla no obligaba a que cada afirmacion tuviera un reproductor. Ahora
tiene una columna que lo dice.

- **Controles limpios**: decia 43 tablas y 25 senales. El generador esta
  en el repositorio, se redujo a 31 tablas y el ruido bajo a 8 -el
  paquete mejoro y el texto seguia diciendo lo viejo-. Los tres numeros
  quedan fijados en `test-ronda107.R`.
- **Defectos plantados**: se saca la fila. El numero es real, se midio
  en tres rondas, pero su banco no esta en el repositorio y
  reconstruirlo de memoria daria nueve defectos parecidos y no los
  mismos. Vuelve cuando exista su test.
- **Registro real de sanciones**: ahora hay
  `benchmark/medir_sanciones.R`, que baja el registro publico del
  catalogo nacional -2.556 filas- y contrasta cada hallazgo de severidad
  `error` contra una comprobacion escrita a mano en R base. Da **9 de
  9**, uno mas que cuando se midio. Ese archivo es ademas la regresion
  del defecto de codificacion de arriba: es el que lo destapo.

### Un token que es marca de formato ya no genera un falso duplicado

Mirando **que** reportaba el detector de vocabulario sobre una tabla
real de vuelos aparecieron dos familias mezcladas:

    [1:48 p.m. (27)  / 1:48 p.m.            Delayed (1)]   <- el estado del vuelo pegado
    [12:00 a.m. (5)  / 12:00 p.m. (42)]                    <- doce horas de diferencia

La primera es un hallazgo real. La segunda es un falso positivo:
`12:00 a.m.` y `12:00 p.m.` son **dos valores legitimos distintos**, y
no hay forma de saber mirando la columna cual fue tipeado mal. Marcarlos
a todos no es detectar, es sospechar en bloque de todos los valores de
una forma y acertar por casualidad los que estaban mal: la precision de
ese diagnostico era **0,259**, tres de cada cuatro marcados eran valores
correctos.

- El detector descarta un par cuando **todos los tokens que lo
  distinguen aparecen en buena parte de la columna**. `a.m.` y `p.m.`
  estan en casi todos los valores; `Delayed` esta en uno.
- Tres condiciones lo acotan, y las tres salieron de romper la suite con
  una version que no las tenia (44 pruebas caidas): el valor tiene que
  tener **mas de un token** —si no, el token que difiere es el valor
  entero y la regla borra el caso central del detector—, la cantidad de
  tokens tiene que coincidir —cuando cambia, como al pegar `Delayed`,
  hay que conservarlo— y el vocabulario tiene que tener al menos 20
  formas, porque “aparece en toda la columna” no significa nada sobre
  cinco.
- El descarte se declara en `n_pares_descartados_formato`.
- **El costo esta medido y publicado.** Sobre el banco de vuelos la
  precision sube de 0,524 a 0,658 y la cobertura baja de 0,281 a 0,238:
  se pierden 111 aciertos porque el banco inyecto erratas que son
  exactamente un cambio de meridiano. Esas caen debajo del techo
  estructural —un `p.m.` mal tipeado es indistinguible de uno correcto
  sin una referencia externa— y el lugar correcto para atraparlas es una
  regla entre columnas, no la proximidad de cadenas.
- Sobre el banco de hospitales **no cambia nada**: lo que distingue dos
  nombres es contenido y no una marca de formato. La regla actua solo
  donde la marca existe.

### Una columna en Latin-1 perdia sus acentos en silencio

Es el defecto mas grave que encontro esta tanda, y no es un caso de
borde: es un CSV viejo en espanol, que es la mayoria de lo que hay en
datos publicos de la region. Sobre una columna con cinco valores
distintos, el perfil informaba:

    n_distintos: 2        <- son 5
    n_faltantes: 0        <- dice que no falta nada
    cobertura:   0 filas  <- no declara nada

[`validUTF8()`](https://rdrr.io/r/base/validUTF8.html) mira los bytes, y
los de un texto marcado `latin1` no son UTF-8 validos, asi que `CAFE`,
`ANO` y `NUMERO` con tilde se volvian `NA` antes de llegar a cualquier
diagnostico. El invariante del paquete roto en su forma mas directa:
informar como medido lo que se descarto.

- **Lo que R sabe convertir ahora se convierte.**
  [`Encoding()`](https://rdrr.io/r/base/Encoding.html) dice `latin1`
  cuando R conoce la codificacion, y
  [`enc2utf8()`](https://rdrr.io/r/base/Encoding.html) convierte sin
  perder nada. El paquete estaba tirando informacion que podia recuperar
  con una llamada. Con eso, la misma columna informa `n_distintos = 5`.
- **Y lo que no se puede convertir, se declara.** Un texto cuya
  codificacion nadie declaro y cuyos bytes no son UTF-8 validos se sigue
  descartando —no hay forma de adivinar si `0xE9` era una `e` con tilde
  o basura— pero ahora `cobertura_diagnosticos` gana una fila
  `texto_no_descifrable` que dice cuantos valores quedaron afuera, y por
  que no cuentan ni como distintos ni como faltantes.

### La proteccion de datos personales dependia de por que puerta entraras

La media de una columna de cedulas salia **expuesta** por
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md) y
**tapada** por
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md):

    perfilar()      media = 5108024      detalle: [estadisticos de orden protegidos]
    perfilar_dbi()  media = NA           detalle: [estadisticos de orden y momentos protegidos]

El argumento estaba escrito del lado DBI desde antes —“la media de las
cedulas de una tabla chica reconstruye demasiado”— y el camino principal
no lo aplicaba. Ahora las dos tapan la media, y el texto distingue si se
taparon estadisticos de orden, momentos o los dos, para no declarar una
proteccion que no se aplico.

### El total del plan no era un techo, y ahora se publica como rango

`attr(plan, "supuesto")` decia que el total era un techo y declaraba
solo la direccion “menos”: una columna sin valores validos no emite sus
metricas. Nunca declaraba la direccion “mas”. Medido contra un motor que
rechaza lotes —el caso exacto que motivo la consolidacion—:

    plan (techo) = 22 consultas        real emitidas = 30

Si un lote falla, se emite la consulta del lote y ademas una por
columna. Quien decide la viabilidad de una corrida con ese numero se
quedaba corto justo en el escenario de degradacion. Ahora el plan
publica `total` —lo que cuesta si ningun lote se rechaza— y
`total_lotes_rechazados` —si se rechazaran todos—, y el costo real cae
entre los dos.

### Dos numeros publicados que no resistieron que otro los midiera

Los dos eran nuestros y de esta semana, y los dos son la misma forma de
error: **medir una cosa y publicarla como otra.**

- **“Una columna corriente de dos mil valores se compara entera”** es
  falso sin calificar el largo. El tope por trabajo muerde cuando
  `L^2 x n(n-1)/2` supera `2e10`, o sea a partir de **101 caracteres**
  para dos mil valores distintos. Y falso en el peor lugar: la columna
  de WKT de 900 caracteres que motivo el presupuesto cae del lado
  recortado, asi que la frase tranquilizadora no alcanzaba justo a los
  datos que hicieron falta el tope.
- **La tasa de 200 caracteres estaba inflada cuatro veces.** El banco
  dividia el tiempo por los pares que el *plan* contaria y no por los
  que de verdad se compararon: sobre 2000x200 el plan cuenta 1.999.000
  pares y se comparan 499.500, porque `max_trabajo` recorta a 1.000
  formas. La tasa real es unos **70.000** pares/seg, no “70.000 a
  270.000”.

### La tabla de evidencia del README, medida de nuevo

Publicaba **43 tablas de control y 25 senales**. El generador esta en el
repositorio, se redujo a 31 tablas, y el ruido bajo a 8 —el paquete
mejoro— pero el README siguio publicando los numeros viejos **porque
ninguna prueba los ataba**. Ahora dice 31 tablas, 0 errores y 8 senales,
y los tres estan fijados en `test-ronda107.R`: si cambian, la suite
falla y hay que actualizarlos a proposito.

Se saco la fila de los nueve defectos plantados. El numero es real —se
midio en tres rondas— pero el fixture no esta en el repositorio, y
reconstruirlo de memoria daria nueve defectos parecidos y no los mismos.
En la tabla que sostiene el argumento del paquete, una fila menos es
mejor que una fila que no se puede comprobar. Vuelve cuando exista su
test.

### Restos de correcciones anteriores

- Una trazabilidad sin filas se declaraba **disponible**: el condicional
  tenia las dos ramas iguales
  (`if (total) "disponible" else "disponible"`), resto de una
  correccion. Con cero indices el objeto prometia una localizacion que
  no existe, con `localizador = "ninguno"` al lado. La rama vacia va a
  `no_disponible`, que es el valor por omision de la propia funcion.
- `.reparar_mojibake_uno` estaba definida **dos veces con algoritmos
  distintos** y topes distintos (4 y 20 iteraciones). El orden
  alfabetico de carga decidia cual corria; la otra era codigo muerto que
  alguien podia “arreglar” creyendo que era la que se usa.
- `.bit64_disponible` y `.bit64_disponible_dbi` eran la misma funcion
  con dos nombres. Queda un solo punto de verdad.
- `.detectar_orden_columnas()` recibia un argumento `resultados` que su
  cuerpo no usaba, y el llamador lo construia para nada.
- `muestra = 1.5` se aceptaba en memoria y perfilaba **una** fila en
  silencio, mientras la via DBI daba error. Ahora las dos lo rechazan.
  La unica diferencia que queda es deliberada: `Inf` vale en memoria y
  no contra un motor.

### El veredicto ya no depende de como venga ordenado el archivo

Cuando el vocabulario de una columna de texto excede el presupuesto, hay
que elegir que formas comparar. Se elegian **las primeras en aparecer**,
y eso hacia que el resultado dependiera del orden de las filas.

Medido sobre la columna `nombre` de *Ejes de vias de circulacion* de
Montevideo —45.400 filas, 8.318 formas distintas, del catalogo nacional
de datos abiertos—, las **mismas filas** daban:

| orden de las filas                   | grupos de casi-duplicados |
|--------------------------------------|---------------------------|
| tal como viene el archivo            | **26**                    |
| desordenado (semillas 11, 202, 7777) | 71, 85, 70                |
| alfabetico                           | **148**                   |

De 26 a 148 segun como estuviera ordenado el archivo. Un perfilador que
hace eso mide la forma fisica de la tabla, no los datos, que es
exactamente lo que el paquete promete no hacer.

- Ahora las formas se **ordenan antes de recortar**. Los cinco ordenes
  de arriba dan **148** grupos: el resultado es el mismo venga como
  venga el archivo.
- Ordenar tiene ademas una razon de fondo: los casi-duplicados quedan
  **adyacentes** —`CAMINO CARRASCO` junto a `CAMINO AGRARIOS`—, asi que
  el corte cae entre familias en vez de partirlas. Una muestra al azar
  rompe pares: si de un grupo de dos sobrevive uno, el grupo desaparece.
  Por eso el azar rinde 70-85 y el orden rinde 148 **con el mismo
  presupuesto**.
- El orden es por bytes (`method = "radix"`) y no el del entorno: la
  intercalacion por omision cambia de una maquina a otra, y eso habria
  cambiado el defecto de lugar en vez de sacarlo.
- El mensaje de cobertura dice ahora que las formas comparadas son las
  primeras del alfabeto y que lo que queda afuera es su tramo final.
  Antes recomendaba “desordenar la tabla antes”, que era el mejor
  consejo posible mientras el defecto estuviera.

Lo destapo la tercera vuelta contra bases reales, que dejo esta
afirmacion sin verificar por no encontrar una columna que la ejercitara.
La columna existia.

### Un conteo del perfil ya no cambia de clase segun que tenga instalado el usuario

[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
devolvia `n_validos`, `n_faltantes`, `n_distintos`, `frecuencia_moda`,
`meta$filas` y `filas_totales_fuente` como **`integer64`** siempre que
`bit64` estuviera instalado, incluido un conteo de 20.

- Eso no agregaba precision: por debajo de 2^53 un `double` ya
  representa el entero exacto.

- Y agregaba tres problemas. La clase del mismo campo dependia de un
  `Suggests`.
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  devolvia `integer` para `n_distintos` y
  [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  devolvia `integer64`: dos puertas del mismo paquete en desacuerdo
  sobre el mismo campo.

- El tercero es el que decide. Un perfil guardado en una maquina con
  `bit64` y leido en una que no lo tiene mostraba:

        columna     n_validos   n_distintos
      1      id 9.881313e-323 9.881313e-323

  donde midio `20`. Sin error, sin aviso, y sumando como si fuera un
  numero. Informar como medido algo que no lo es, en el paquete cuyo
  argumento es justamente ese.

- Los conteos salen ahora `numeric`. `integer64` se conserva **solo
  donde compra exactitud**: un conteo por encima de 2^53 que el motor
  entrega como texto o como `integer64`. Para un conteo de filas eso
  significa una tabla de mas de nueve mil billones.

- De paso se corrige el error simetrico: `conteo_exacto` decia `FALSE`
  para un conteo entregado como texto por encima de 2^53, que es justo
  el caso donde si se guarda exacto. El paquete se declaraba menos
  preciso de lo que era.

### El R minimo declarado ahora es un R medido

`DESCRIPTION` declaraba `R (>= 3.6.0)`, y era una promesa que el paquete
**no cumple**. Pasa a `R (>= 4.1.0)`.

- Se venia afirmando que la suite **no puede** correr bajo R 3.6, porque
  `testthat` declara `R (>= 4.1.0)`. Eso es cierto contra CRAN de hoy y
  falso contra el snapshot de la epoca: ahi `testthat 3.1.7` instala sin
  problema. La suite corre, y da `[ FAIL 18 | PASS 15356 ]`. La
  generalizacion tapo dieciocho fallos.
- **Seis de los dieciocho salen de una sola causa.** Bajo R \< 4.0,
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html) trae
  `stringsAsFactors = TRUE`, y el paquete tiene 275 llamadas a
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html) que no lo
  declaran: ahi las columnas de texto nacen factor. No es cosmetico: la
  proteccion de datos personales escribe `"[valor protegido]"` en una
  columna factor, R lo rechaza por nivel invalido y queda `NA`. No hay
  fuga, pero la promesa sobre esa celda no se cumple, y falla en
  silencio. Bajo R 4.0.5 con los mismos paquetes esos seis desaparecen.
- **`4.1.0` y no `4.0.0`** porque es lo que pide `testthat` actual: en
  el piso declarado la suite corre con las herramientas de hoy, que es
  lo que hace verificable la promesa. Medido en contenedor antes de
  declararlo: `rocker/r-ver:4.1.3`, `checking tests ... OK`,
  `[ FAIL 0 | PASS 14613 ]`. Nada obligaba al numero viejo: `cli`, el
  unico Import, pide `R (>= 3.4)`.
- Los doce fallos restantes bajo versiones viejas de los `Suggests` no
  son del R: con `RSQLite 2.3.1` y `bit64 4.0.5` los conteos vuelven
  como `integer64` y viajan asi hasta el perfil, asi que un mismo campo
  cambia de clase segun que tenga instalado el usuario. Queda anotado
  como trabajo abierto, no como resuelto.

### Tres pruebas que exigian justo lo que decian no tener

Las tres tenian la misma forma, y solo se ven en un entorno donde el
paquete opcional de verdad no esta.

- La normalizacion Unicode juntaba en un bloque la mitad que **mide**
  —que necesita `stringi`— y la mitad que **declara su ausencia**,
  simulada con un mock. Sin `stringi`, la primera fallaba y se llevaba
  puesta a la segunda. Poner una guarda habria salteado las dos,
  incluida la que no la necesitaba: van separadas, con la guarda donde
  corresponde.
- Las dos pruebas de `integer64_sin_soporte` armaban el `data.frame` con
  la columna ya marcada como `integer64`, y eso obliga a
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) a
  buscar el metodo de `bit64`. La prueba de que falta `bit64` exigia que
  `bit64` estuviera. Ahora la clase se pone despues de armar la tabla,
  que ademas es el camino realista: la columna llega marcada dentro de
  una tabla que ya existe.
- [`DBI::Id()`](https://dbi.r-dbi.org/reference/Id.html) acepta
  argumentos sueltos recien desde 1.2. Con la version anterior el error
  que salta es el de DBI y la prueba mide otra cosa; van nombrados.

Nota sobre el instrumento: `_R_CHECK_DEPENDS_ONLY_=true` ocultaba
`bit64` para `skip_if_not_installed()` —saltaba ocho pruebas por eso— y
aun asi las dos de `integer64` pasaban ahi, mientras fallaban en el
contenedor. **El check con dependencias recortadas no es equivalente a
un entorno que no tiene el paquete.**

### El reintento que contra el driver real no se disparaba nunca

La version anterior agrego un reintento: si la lectura de la muestra
falla y hay columnas de tipo largo declaradas, se reintenta sin ellas y
se declara que quedaron afuera. Contra una base real no se disparo **ni
una vez**. En la tabla que motivo el arreglo —158 columnas, 90 de ellas
`varchar(max)`— el patron de tipos reconocio **0 de 90**, la muestra se
perdio entera y no aparecio el aviso de alcance: exactamente lo que el
arreglo prometia evitar.

- La causa no es un tipo que falte en la lista. El driver **no informa
  nombres**: `odbc` resuelve `dbColumnInfo()` con
  `nanodbc::result::column_datatype()`, que devuelve el **codigo
  numerico de ODBC**. Contra `{SQL Server}` las noventa columnas llegan
  como noventa veces `"-1"`. Comprobado en las dos puntas: la biblioteca
  compilada no expone `column_datatype_name` ni contiene un solo literal
  de nombre de tipo SQL, y el patron de nombres no matchea `"-1"`.
- El arreglo se habia probado contra un banco que hablaba en nombres,
  que es la unica forma de tipo que el patron sabe leer. El banco
  compartia con el patron justo la propiedad cuya ausencia era el fallo.
- **El reintento ya no infiere: pregunta.** Cuando la lectura falla, se
  aisla por descarte cuales columnas no se pueden leer —biseccion sobre
  el conjunto, podando los subconjuntos que si se leen— y se declaran
  esas. Es independiente del controlador: funciona sin reconocer ningun
  tipo.
- El patron queda como **atajo optimista**: si reconoce el tipo, ahorra
  el descarte. Se le agregaron los codigos ODBC (`-1`, `-4`, `-10`) y
  las variantes de nombre (`LONG VARCHAR`, `SQL_LONGVARCHAR`,
  `WLONGVARCHAR`) que tampoco veia. Pero la correccion ya no cuelga de
  el.
- Lo que se declara cambia segun como se supo. `omision_comprobada`
  distingue **medido** de **supuesto**: por descarte, cada columna fallo
  sola y el resto se leyo junto, y el aviso lo dice; por el atajo, sigue
  diciendo “no se comprobo que sean la causa”. Y `sondas_descarte`
  publica cuantas consultas costo averiguarlo.
- El descarte esta acotado por los dos lados: como mucho `2n` sondas
  sobre `n` columnas —que es lo que cuesta la biseccion en el peor
  caso—, tope absoluto de 512, y **nunca mas de la mitad del saldo de
  `max_consultas`**, para no recuperar la muestra a costa de quedarse
  sin presupuesto para el resto.
- Si el descarte no aisla nada, no se inventa una culpable: la muestra
  se declara no disponible y el motivo dice cuantos subconjuntos se
  sondearon y como termino —ninguna falla sola, o fallan todas, o el
  tope corto antes—.
- De paso: el reintento rearmaba el SQL a mano y perdia el muestreo del
  motor por el camino —volvia a una lectura de primeras filas mientras
  `metodo` seguia declarando el muestreo nativo—. Ahora la lectura
  original y el reintento salen de la misma receta, y `metodo`,
  `acotado_en` y `fraccion` se corrigen con lo que de verdad se emitio.

### Un plan que contaba la mitad del reloj

[`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
le ponia magnitud **`"baja"`** a una tabla de 3.912 filas que tardaba
**35 segundos**. Cada numero que informaba era cierto —64.592 lecturas
de fila, cero ordenaciones— y el juicio era falso: el trabajo no estaba
en el motor sino en R, comparando formas de una columna de geometria en
texto. Medir una mitad y llamarla el total es informar como completo
algo parcial.

- La magnitud se estima ahora en **dos mitades**. La del motor sigue en
  `filas_leidas` y `ordenaciones_completas`, resumida en
  `magnitud_motor`. La del cliente esta en `columnas_texto` y
  `pares_texto` —cuantos pares de formas podria comparar el detector de
  vocabulario sobre la muestra—, resumida en `magnitud_texto`.
  `magnitud` es **la mayor de las dos**.
- La unidad es el par de formas comparadas, que es una cuenta y no un
  indice: la muestra trae `m` filas, las formas distintas son a lo sumo
  `m`, y el detector nunca compara mas de `max_pares` por columna. El
  tope se lee de la firma del detector, no se copia, asi que no puede
  quedar estimando contra un numero viejo.
- Los umbrales (2e6 y 2e8 pares) estan anclados a la misma escala de
  segundos que los del motor, con la tasa medida: **de 660.000 a 960.000
  pares por segundo sobre valores de cuarenta caracteres**, contra los
  cinco millones de lecturas de fila por segundo de la referencia de
  PostgreSQL. La medicion esta en `benchmark/medir_costo_texto.R`,
  seccion 5, para que el umbral no sea un numero elegido a dedo.
- Lo que el plan **no** puede saber queda dicho, no escondido: el conteo
  de pares es exacto, pero cuanto cuesta cada uno depende del largo de
  los valores, que el plan no leyo. Sobre valores de doscientos
  caracteres la tasa cae a unos 70.000 pares por segundo, asi que con
  textos muy largos el tiempo real es varias veces el que sugiere la
  referencia. `supuesto_costo` lo declara en vez de prometer segundos.
- La impresion muestra las dos mitades, y cuando la que pesa es la de R
  nombra la palanca de ese lado —`max_trabajo_vocabulario`—, que las
  palancas del motor no tocan.

### Presupuestos que miden trabajo, no que cuentan unidades

Una tabla del catalogo de PostGIS —`spatial_ref_sys`, **3.912 filas y 5
columnas**— tardaba **243 segundos**. No era la geometria: eran cadenas
largas, WKT de proyecciones, y el detector de vocabulario se llevaba el
99,6 % del costo. Tenia dos topes, `max_valores = 5000` y
`max_pares = 2000000`, y **ninguno de los dos miraba cuanto costaba cada
unidad**: 800 valores son 319.600 pares, muy por debajo del tope, pero
cada comparacion era una Jaro-Winkler sobre 900 caracteres.

- La unidad del presupuesto es ahora la **comparacion de un caracter
  contra otro**, que es el bucle interno de la distancia: comparar dos
  valores de largos L1 y L2 cuesta del orden de `L1 x L2`. La suma sobre
  todos los pares de un prefijo sale **exacta y en tiempo lineal**, sin
  materializar la matriz.

- Contar pares por largo medio no alcanzaba. Medido, ese modelo compraba
  **5,3 millones de unidades por segundo con valores de 900 caracteres y
  44 millones con valores de 40**: ocho veces de diferencia es no tener
  modelo. Con el producto de largos la dispersion baja a 4,25 veces, y
  lo que queda es a favor de las columnas de valores cortos, que son el
  caso comun.

- `max_trabajo_vocabulario` vale `2e10` por omision, calibrado contra la
  medicion y no contra la intuicion:

  | valores | largo | sin tope | con tope  | comparado |
  |---------|-------|----------|-----------|-----------|
  | 400     | 900   | 15,1 s   | **4,3 s** | 55,5 %    |
  | 500     | 900   | 23,0 s   | **4,3 s** | 44,4 %    |
  | 800     | 900   | 61,3 s   | **4,6 s** | 27,8 %    |
  | 2000    | 80    | 5,0 s    | 5,1 s     | **100 %** |

  El ultimo renglon es el que importa tanto como el tercero: **una
  columna corriente de dos mil valores no se recorta**, siempre que sus
  valores midan menos de unos cien caracteres: el tope por trabajo
  muerde cuando `L^2 x n(n-1)/2` supera `2e10`, o sea a partir de 101
  caracteres para dos mil valores distintos. Tampoco 500x20 ni 1000x30.
  El riesgo del arreglo era romper el caso comun para arreglar el
  patologico.

- Una aclaracion que hay que hacer, porque la primera version de esta
  nota afirmaba de mas: **3000x20 si se recorta**, pero no por el
  presupuesto nuevo sino por `max_pares`, el tope viejo, que acota en
  2.000 formas sin mirar el largo. Sigue puesto porque acota la memoria
  de la matriz de pares. El recorte se declara con su motivo, asi que no
  se pierde en silencio, pero decir “no se recorta” era falso. Salio de
  que el banco apagaba `max_pares` para aislar el efecto del tope nuevo
  y despues se leyo esa medicion como si fuera lo que recibe un usuario.
  El banco separa ahora las dos cosas.

- Lo recortado **se declara**, con las dos cuentas separadas: cuantas
  formas normalizadas quedaron sin comparar y cuanto trabajo era, en el
  alcance del hallazgo y en `cobertura_diagnosticos`, junto con cual de
  los dos topes recorto. Si aprietan los dos, el motivo los nombra a los
  dos: el usuario tiene que poder elegir cual aflojar.

- El recorte toma las **primeras formas en aparecer**, no una muestra, y
  ahora lo dice. Con los mismos 300 valores y el mismo presupuesto,
  poniendo primero los largos entran 8 formas y poniendo primero los
  cortos entran 150: sobre una tabla ordenada lo que queda afuera es un
  tramo del orden. Decir cuantas quedaron sin comparar y callar cuales
  dejaba suponer un muestreo que no hubo.

- [`detectar_dependencias()`](https://sebollin.github.io/lupa/reference/detectar_dependencias.md)
  gana `max_trabajo`, en unidades **fila-par**, porque ahi el costo es
  del orden de `columnas^2 x filas` y `max_comparaciones` no lo veia:
  158 columnas son 24.806 pares, muy por debajo de las 200.000 del tope.
  Se combina con `max_comparaciones` y manda el mas restrictivo.
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  lo expone como `max_trabajo_dependencias`.

### Un plan que dice cuanto cuesta, no solo cuantas consultas son

[`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
contaba consultas, y contar consultas no responde la pregunta que trae
quien lo mira: si la corrida tarda segundos, minutos u horas. Catorce
consultas sobre dos millones de filas son mucho mas trabajo que
doscientas sobre mil.

- El plan estima ahora la **magnitud** en dos numeros que son cuentas de
  verdad y no un indice inventado: `filas_leidas` —cuantas filas habria
  que leer— y `ordenaciones_completas` —cuantas veces habria que ordenar
  la tabla entera—. De ahi sale `magnitud`: `"baja"`, `"media"`,
  `"alta"`, o `"desconocida"` si no se conoce el numero de filas. El
  peso de cada clase de consulta sale de su `alcance`, que ya venia
  declarado.

- Al imprimirlo, un trabajo alto viene con las **palancas concretas**
  para acotarlo —`modo = "muestreado"`, recortar `metricas`, bajar
  `muestra`, `max_consultas`—. Avisar que algo es grande sin decir que
  hacer no le sirve a nadie.

- Es una estimacion y lo dice en `supuesto_costo`: cuenta las filas que
  habria que leer **si ningun indice ayudara**, y cada ordenacion
  completa como `log2(filas)` pasadas. Los dos numeros publicados no
  dependen de ese supuesto, asi que quien no lo comparta puede rehacer
  la cuenta. La referencia esta medida: PostgreSQL 16 local, 2 millones
  de filas por 40 columnas en modo seguro, 14 consultas y 5,3 segundos.

- Sobre la forma que se midio contra PostgreSQL 16 —2 millones de filas
  por 40 columnas—, la clasificacion cae donde tiene que caer, y el
  ultimo renglon es la razon de ser de todo esto:

  | modo         | consultas | lecturas de fila | ordenaciones | magnitud |
  |--------------|-----------|------------------|--------------|----------|
  | `conteos`    | 8         | 6.001.000        | 0            | baja     |
  | `seguro`     | 14        | 26.001.000       | 0            | media    |
  | `exacto`     | 94        | 186.001.000      | 80           | alta     |
  | `muestreado` | 94        | 2.445.000        | 0            | **baja** |

  Las mismas **94 consultas** son «alta» en `exacto` y «baja» en
  `muestreado`: el conteo es identico y el trabajo difiere por setenta y
  seis veces. Contar consultas no podia distinguirlos. Y los conteos de
  `conteos` y `seguro` son los mismos 8 y 14 que se cronometraron en 2,4
  y 5,3 segundos.

- Si aparece una clase de consulta cuyo alcance no tiene peso declarado,
  la magnitud queda **`"desconocida"`** en vez de estimarse de menos en
  silencio.

- Sobre una tabla chica los parrafos de supuestos no se imprimen: tapan
  la respuesta en vez de matizarla. Siguen en los atributos, y la
  palabra «techo» viaja con el conteo en todos los casos.

### Una columna que el controlador no sabe traer ya no se lleva la muestra entera

Hay columnas que muchos controladores no pueden devolver en una lectura
corriente: `TEXT` y `NTEXT` en SQL Server, `CLOB` y `BLOB` en Oracle,
`bytea` en PostgreSQL. Pedirlas junto con el resto hace fallar la
consulta completa, y con ella se perdia toda la muestra. En una de las
tablas de la corrida real, 90 de 158 columnas eran de esos tipos.

- Cuando la lectura de la muestra falla y hay columnas declaradas con
  esos tipos, se **reintenta sin ellas**. La muestra vuelve con las
  columnas que si se pudieron leer, en vez de no volver.
- Lo que quedo afuera se declara: `resumen_tabla$cobertura` gana una
  fila `alcance_distinto` que nombra las columnas omitidas y **conserva
  el motivo textual del motor**, mas la via para incluirlas
  —convertirlas a texto acotado en una vista y perfilar la vista—.
- El aviso cuenta la **secuencia y no atribuye la causa**. El reintento
  salta ante cualquier fallo de lectura habiendo columnas de esos tipos
  declaradas; que ellas sean el motivo es lo probable, no lo comprobado,
  y un corte de red que se recupera en el segundo intento daria el mismo
  camino. Decir “el controlador las rechazo” seria informar como sabido
  algo que no se midio.
- El resumen por columna las cubre igual, porque esos agregados se
  calculan en el motor. Lo que falta es su perfil por fila, y eso es lo
  que dice la cobertura.
- **`meta` tambien se corrige, y esto lo encontro la refutacion.** El
  bloque de metadatos del muestreo se arma antes de leer, asi que
  quedaba congelado con la lectura que fallo: `columnas_leidas`
  declaraba haber leido justamente la columna que no se pudo leer, y
  `sql_muestra` publicaba la consulta original en vez de la que de
  verdad se emitio. La cobertura decia la verdad y `meta` decia otra
  cosa, que es informar como medido lo que no se midio, en el lugar
  donde se mira para saber que se hizo. Ahora `meta` trae las columnas
  que realmente se leyeron, el SQL del reintento, y ademas
  `columnas_omitidas` con su motivo.
- El reintento es **portable**: no emite conversiones propias de un
  motor, solo vuelve a pedir la consulta sin las columnas rechazadas. Un
  `CAST` distinto por dialecto habria sido otra superficie que mantener
  y probar contra cada motor.

### Un mensaje que se leia mal

El aviso de acciones destructivas de `plan_limpieza` mostraba
`p\u00e9rdida` en pantalla: la cadena tenia la barra invertida
duplicada, asi que el escape nunca se resolvia. Es un error que no ve
nadie —el paquete instala, la suite pasa y `R CMD check` no protesta,
porque la cadena es ASCII perfectamente valida— y solo se nota leyendo
el mensaje. Hay ahora un barrido que recorre los literales de cadena del
espacio de nombres y falla si alguno lleva un escape sin resolver. Era
el unico caso en el paquete.

La primera version del barrido miraba solo el cuerpo de cada funcion, y
la refutacion mostro que eso dejaba fuera dos sitios donde de verdad
viven mensajes: los **valores por omision de los argumentos** y los
**atributos**. Ahora los recorre. Lo que sigue sin ver es una constante
capturada por closure desde un ambito local, y eso queda dicho en el
propio test en vez de suponerse cubierto: en `lupa` no es un agujero,
porque las constantes del paquete son enlaces del espacio de nombres y
el barrido las recorre una por una.

### El costo a escala: de una consulta por columna a una por lote

La segunda corrida contra bases reales dejo una sola reserva seria, y
era esta: una tabla de decenas de millones de filas no terminaba de
perfilarse ni en `modo = "muestreado"`. La causa no era el muestreo sino
la cantidad de escaneos: el paquete emitia **una consulta por columna**
para cada bloque de metricas.

- Los agregados planos —conteos, minimo/maximo/media/ceros/negativos, y
  desvio— se piden ahora para **varias columnas en una sola consulta**,
  por lotes. La moda y la mediana siguen siendo una por columna, porque
  agrupan y ordenan.

- Medido contra PostgreSQL 16 con **2 millones de filas por 40
  columnas**:

  | modo      | antes                 | despues                 |
  |-----------|-----------------------|-------------------------|
  | `conteos` | 46 consultas, 5,4 s   | **8 consultas, 2,4 s**  |
  | `seguro`  | 128 consultas, 15,2 s | **14 consultas, 5,3 s** |

  Con las mismas 160 y 400 metricas calculadas.

- **Y los numeros no cambian**: sobre la misma tabla sembrada una sola
  vez, el perfil consolidado y el anterior coinciden en los dieciseis
  campos del resumen para seis tipos de columna, y en los noventa
  estados por metrica.

- **Si un lote falla, no se pierde el lote.** Se reintenta columna por
  columna, y lo que igual falle queda `no_disponible` con su motivo
  mientras las vecinas se calculan. Una consulta compartida es la forma
  perfecta de reintroducir el reflejo de todo-o-nada que el paquete
  corrigio en cinco lugares, asi que la degradacion se construyo desde
  el principio y tiene sus propios tests.

- `resumen_tabla$sql` conserva **una fila por columna y metrica** con
  todos sus campos, y agrega `lote` y `columnas_compartidas` para que se
  vea cual consulta fue compartida.
  [`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
  publica `tamano_lote`, y su total pasa a estar declarado como
  **techo** en `attr(plan, "supuesto")`: se contaba una mediana y un
  desvio por columna numerica, y una columna sin valores validos no los
  emite. Se afirmo durante varias rondas que el plan predecia exacto;
  era cierto sobre tablas con datos en todas las columnas y falso en
  cuanto aparece una vacia. La version anterior erraba por tres
  consultas en ese caso y esta por una, asi que no es una regresion: es
  una afirmacion que venia siendo mas fuerte que el codigo.

- En SQLite con tablas chicas el ahorro de tiempo es casi nulo: ahi
  domina el costo de R y no los escaneos. Queda dicho porque una
  medicion que no distingue las dos cosas invita a concluir de mas.

### Lo que rompio la refutacion sobre estos mismos cambios

- **Una conversion que pierde el valor ya no se publica como
  `calculado`.** SQLite responde el `MIN` de una columna declarada
  `DATE` como el texto `"2020-01-01"`;
  [`as.numeric()`](https://rdrr.io/r/base/numeric.html) lo convierte en
  `NA` y el estado quedaba `calculado`. Decir “se midio” y “no se midio”
  a la vez sobre el mismo campo. Ahora la metrica queda `no_disponible`
  con el valor original del motor en el motivo. Lo mismo para un
  `integer64` cuyo paso a doble lo cambiaria: el maximo publicado no
  estaria en la columna.
- **La guarda de exactitud de `integer64` tenia un agujero de un solo
  numero**: comparaba el doble ya convertido contra 2^53, y 2^53+1
  redondea justo a 2^53, asi que pasaba. Ahora se comprueba con la
  vuelta completa -a doble y de vuelta a entero-, que no depende de
  donde caiga el redondeo.
- **`meta$muestras_independientes` decia algo que la consolidacion
  volvio falso.** Las columnas de un mismo lote comparten consulta y por
  lo tanto comparten filas: sus metricas son comparables entre si, y las
  de lotes distintos no. El campo dice ahora las dos mitades.
- **El total de
  [`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
  pasa a estar declarado como techo.** Se cuenta una mediana y un desvio
  por columna numerica, y una columna sin valores validos no los emite.
  La version anterior a la consolidacion erraba por tres consultas en
  ese caso y esta por una: no es una regresion, es una afirmacion que
  venia siendo mas fuerte que el codigo.

### Cuatro correcciones de honestidad

Las cuatro salieron de mirar los datos crudos de la corrida real, y tres
de ellas de reproducir lo que el informe atribuia a otra causa.

- **La trazabilidad acepta `integer64`.** Un `bigint` llegaba a R como
  `integer64` y la trazabilidad lo rechazaba: el hallazgo se publicaba y
  la guarda tenia que avisar que no habia con que nombrar las filas. Se
  atribuyo a las geometrias, pero la lista incluia `outliers` sobre
  columnas que no tienen nada de espacial. **Afecta a cualquier
  `bigint`.** Por encima de 2^53 la traza no se entrega, porque la
  conversion deja de ser exacta y una fila mal senalada es peor que una
  sin senalar.
- **Cuando el motor dice que es permiso, el mensaje lo dice.**
  `dbExistsTable()` no distingue una tabla inexistente de una sin
  permiso, y el mensaje repetia esa duda incluso cuando el motor habia
  respondido `permiso denegado a la relacion`. En una corrida real
  fueron veintitres tablas descritas como inciertas con la respuesta en
  la mano. El texto del motor se conserva: el diagnostico no reemplaza
  la evidencia.
- **El hallazgo `faltantes` nombra la senal estructural.** Cuando
  `posible_ausencia_estructural` dispara sobre una columna, el
  `faltantes` de esa columna dice en su evidencia que existe esa lectura
  alternativa. **La severidad no se toca**, y eso se decidio con un caso
  en contra: en una tabla pivoteada la correlacion entre el mes y la
  columna del ano es real, y un mes sin dato puede ser un hueco genuino.
  Degradar ahi lo esconderia.
- **El objeto declara que las metricas muestreadas no comparten filas.**
  Estaba en la vineta, y un consumidor automatico lee el objeto. Aparece
  en `meta$muestras_independientes` solo en `muestreado` y `aproximado`;
  en los modos que miden sobre la tabla entera no hay nada que advertir.

### Leer un perfil sin conocer su forma, y saber que falta para cada motor

- [`hallazgos()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md),
  [`columnas()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md),
  [`cobertura()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md),
  [`n_filas()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
  y
  [`sql_perfil()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
  leen las cuatro formas de salida del paquete sin depender de como
  estan armadas. `perfil$general$filas` funcionaba sobre la salida en
  memoria y devolvia `NULL` sobre la salida DBI, donde el conteo vive en
  `resumen_tabla$meta$filas`; un `NULL` silencioso en un guion de
  medicion no avisa, y lo que sigue calcula sobre nada. **No inventan lo
  que no hay**: un perfil DBI sin muestra leida devuelve una tabla de
  hallazgos vacia con su aviso, y
  [`sql_perfil()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
  sobre un perfil en memoria devuelve `NULL` porque no se emitio SQL.
- [`requisitos_motor()`](https://sebollin.github.io/lupa/reference/requisitos_motor.md)
  contesta que hace falta para hablar con cada motor antes de chocarse:
  el paquete de R, la biblioteca del sistema con su nombre en Debian y
  en Fedora, **la alternativa sin permisos de administrador** cuando
  existe, el dialecto esperado y si esta probado contra motor real. Los
  errores de conexion se traducen: un
  `Can't open lib ... file not found` de ODBC pasa a decir que falta
  `unixodbc-dev` y cual es la salida sin `sudo`.
- Un controlador que no implementa `dbIsValid()` ya no se toma por
  conexion rota: se prueba `dbGetInfo()` antes de rendirse. El `ROracle`
  archivado es el caso, y por eso Oracle quedaba fuera de
  [`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md)
  aunque el SQL funcionara.

### Oracle contra motor real

- Verificado contra Oracle Free 23. Importa aparte porque **sus dos
  dialectos -`fetch_first` y `rownum`- nunca habian corrido contra un
  motor real**; los otros seis usan `limit` o `top`. Encontro cuatro
  cosas: la sonda del desvio necesitaba `FROM DUAL`; Oracle rechaza
  `TABLESAMPLE` y usa `SAMPLE (p)`, que se agrego como forma candidata
  con su sonda; `dbExistsTable()` del `ROracle` archivado devuelve falso
  para nombres calificados aunque el SQL funcione; y una columna `CLOB`
  no se puede agrupar ni ordenar, cosa que el paquete ya declaraba como
  no disponible sin haberlo previsto.
- Con esto son **siete motores probados contra motor real**, y los siete
  encontraron algo que ningun motor simulado habia encontrado.

### Lo que encontro una refutacion adversarial

Se puso un agente a romper las afirmaciones de esta tanda en vez de a
confirmarlas. Encontro ocho defectos, y los tres peores tenian la misma
forma: **la tabla con la que se verificaban los motores era comoda**.
Tenia 5.000 filas de tipos faciles, sin fecha nativa, sin enteros sin
signo y nunca mas chica que la muestra pedida. Es el mismo error que el
paquete ya persigue en los demas —el fixture que comparte la propiedad
cuya ausencia es el fallo— aplicado al propio verificador.

- **Una columna `DATE` se media como numero, con estado `calculado`.**
  El `dbFetch(n = 0)` de RMariaDB devuelve `numeric(0)` para una fecha:
  la clase se pierde junto con las filas, e
  [`is.numeric()`](https://rdrr.io/r/base/numeric.html) decia que si.
  Salian `minimo` en dias desde 1970 y `media` en YYYYMMDD -dos unidades
  distintas, las dos publicadas como la misma-. Ahora el tipo declarado
  por el motor manda sobre el prototipo.
- **Un `BIGINT UNSIGNED` cerca del tope daba `maximo` menor que
  `minimo`**, los dos `calculado`. Habia guarda de coherencia para “mas
  distintos que validos” y no para un rango imposible. Ahora tambien.
- **El muestreo extrapolaba dividiendo por las filas pedidas y no por
  las obtenidas.** Pedir mil filas de una tabla de diez daba
  `n_validos = 0` y `sin_valores` sobre una columna llena, con
  `fraccion = 1` al lado contradiciendolo. El tamano que se informa y
  que divide es el efectivo.
- **El objeto declara con que criterio se comparo.** Un cotejamiento que
  ignora la caja hace que el resumen SQL cuente dos valores distintos
  donde el perfil de muestra cuenta cuatro, sobre las mismas filas. Los
  dos numeros son ciertos en su propia comparacion; faltaba que el
  objeto dijera cual usa cada bloque.
- **La cobertura de una parte incompleta ya no se pierde al subir de
  nivel.** Un conjunto armado con una organizacion a la que le falto una
  coleccion decia cobertura 1. Ahora hereda `cobertura_de_partes` y
  marca `completo = FALSE`.
- **El renombre de partes rompia la composicion**:
  [`agregar()`](https://sebollin.github.io/lupa/reference/agregar.md)
  escribia el nombre del objeto y el nivel de arriba comparaba contra el
  declarado. Los dos identifican a la misma parte.
- Una parte con **peso cero** entraba a la cobertura sin aportar al
  numero, y ahora se declara.
- **`posible_ausencia_estructural` no disparaba con mas de 20 niveles**,
  o sea no veia `edad >= 65`, que es el caso mas frecuente de todos. De
  ahi salio una capacidad nueva: un corte numerico o de fecha tambien se
  ofrece como regla, con la formula escrita en el tipo correcto
  -`~ alta >= as.Date("2021-01-01")`, no `>= 18628`-.

`benchmark/verificar_motor.R` incorpora ahora los tipos y tamanos que
escondian esos defectos, para que la proxima tabla comoda no certifique
un motor que no lo esta.

### MariaDB, y las diez granularidades del marco

- Verificado contra MariaDB 11 real: los cinco modos sin ninguna metrica
  no disponible, los tres estadisticos coincidiendo con R, el plan
  exacto en los cinco. **Es el primer motor real que no encontro ningun
  defecto**, y tiene explicacion: habla el mismo protocolo que MySQL 8,
  que ya estaba verificado.
- [`organizacion()`](https://sebollin.github.io/lupa/reference/organizacion.md)
  declara que colecciones pertenecen a un organismo, y con eso
  [`agregar()`](https://sebollin.github.io/lupa/reference/agregar.md)
  mide las granularidades novena y decima del marco. Que bases
  pertenecen a que organismo **no esta en los datos**, asi que lo
  declara quien lo sabe; es el mismo mecanismo que ya usaba el conjunto
  de colecciones.
- **Los dos niveles institucionales son opcionales.** Un analisis de
  calidad no siempre tiene una organizacion detras -una entrega suelta,
  un archivo que alguien mando, una base sin dueno declarado- y nada
  obliga a pasar por ellos. Sin declaracion,
  [`agregar()`](https://sebollin.github.io/lupa/reference/agregar.md) se
  niega y explica como declararla, que es distinto de inventar una
  frontera que nadie nombro.
- La politica de pesos vale para los cuatro niveles con frontera:
  promediar organismos de tamano distinto sin declararlo es el mismo
  juicio inventado que el paquete se niega a hacer un piso mas abajo. Y
  el numero viaja con su cobertura: cuantas de las partes declaradas
  entraron efectivamente.

### DuckDB, y una sonda que mentia

- Verificado contra DuckDB 1.5 real: los cinco modos corren sin ninguna
  metrica no disponible, la media, la mediana y el desvio coinciden con
  los calculados en R sobre la tabla entera, y los nombres calificados
  con punto y las colecciones de dos esquemas funcionan.
- **Y encontro un defecto que ningun motor simulado podia encontrar.**
  DuckDB acepta `TABLESAMPLE SYSTEM (10) WHERE 1 = 0` y rechaza la misma
  clausula sin el filtro: con un filtro trivialmente falso el parser no
  llega a validar el metodo de muestreo. La sonda de capacidad usaba
  justo ese filtro para salir barata, asi que declaraba disponible una
  forma que el motor despues rechazaba. Ahora la sonda emite la forma
  real acotada por el limite del dialecto. **Una sonda que no ejercita
  la forma que despues se emite no prueba nada**, que es la misma
  leccion de la sonda del desvio, una ronda antes.
- El muestreo prefiere las formas de tamano predecible: primero la de
  **cantidad fija** —`TABLESAMPLE RESERVOIR (n ROWS)`—, despues la de
  **nivel de fila** —`TABLESAMPLE BERNOULLI (p)`—, y solo al final las
  de bloque. Medido contra PostgreSQL 16 pidiendo el 20 % de una tabla
  de 5.000 filas: `SYSTEM` devolvio 678, 904, 452 y 1.384 filas en
  cuatro corridas; `BERNOULLI`, 1.011, 1.017, 981 y 1.050. Un tamano que
  no se puede anticipar hace que dos metricas del mismo perfil dejen de
  ser comparables. Medido: `TABLESAMPLE (20 PERCENT)` en DuckDB es a
  nivel de bloque y devuelve `0` o `2048` filas sobre una tabla de
  5.000, asi que dos consultas del mismo perfil veian muestras de tamano
  distinto, y la guarda de coherencia declaraba no disponible una moda
  cuya frecuencia superaba unos validos que valian cero. Con la forma de
  cantidad fija, las cuatro metricas que caian vuelven a calcularse.
- El motor simulado que reproduce la trampa esta en la suite, asi que la
  regresion queda cubierta sin necesidad de DuckDB instalado.

### La senal que faltaba: nadie declara lo que no sabe que existe

- `posible_ausencia_estructural`, severidad `ok`. `aplicabilidad`
  resolvia el vacio por diseno y funcionaba, pero exigia que el usuario
  supiera que existe: quien perfilaba una tabla con columnas
  condicionadas sin declarar nada recibia el mismo informe enganoso que
  antes. Ahora, cuando el valor de una columna decide que filas tienen
  otra —`cumplimiento >= 0.99`—, o cuando dos o mas columnas se reparten
  las filas sin pisarse, el hallazgo lo dice con la evidencia medida y
  **la linea exacta que habria que escribir**. Sugiere; no decide, y no
  reescribe el universo por su cuenta. Las columnas ya declaradas quedan
  fuera del examen.
- Medido antes de encenderlo: sobre veinte conjuntos que vienen con R y
  sesenta tablas al azar con ausencia independiente produce **cero**
  senales, y dispara en el modelo entidad-atributo-valor, en el salto de
  patron de una encuesta y en las columnas excluyentes. Con 10 % de las
  filas fuera de la regla se calla, porque entonces la relacion existe y
  no es una regla. Cuesta 0,11 s sobre 200 columnas por 20.000 filas.
- `regla_silencia_ausencia`, tambien `ok`. Declarar opcional una columna
  con 80 % de ausentes dejaba el perfil limpio y la cobertura lo
  documentaba, pero quien no la leyera no se enteraba. El aviso existe
  para que eso sea una decision y no un efecto de la declaracion.
- `columnas_personales` declara que columnas traen datos personales, con
  tipo o sin el. Ningun lexico de nombres puede ser completo —una
  columna con documentos se puede llamar `cod_benef`— y esta es la
  salida correcta a ese limite: lo declara quien conoce el dato, gana
  sobre lo inferido, y no se vuelve a examinar.
- `dato_personal_protegido` dice si el valor **quedo** protegido, no si
  la clasificacion pensaba protegerlo. Con
  `proteger_datos_personales = FALSE` la moda se ve, y decir `TRUE` al
  lado de un valor visible era informar como hecho algo que no paso. La
  intencion sigue en `datos_personales$proteger`.
- Un correo ofuscado —`usuario at ejemplo punto com`— vuelve a ser un
  correo para la clasificacion, aunque
  [`validar_correo()`](https://sebollin.github.io/lupa/reference/validadores_formato.md)
  siga diciendo con razon que no es un correo valido. Son dos preguntas
  distintas: una mide la forma, la otra decide si hay dato personal. La
  frase `lunes at casa` no entra: el dominio tiene que traer su
  separador.
- Una matriz de dos dimensiones es una tabla y
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  la acepta. La conversion queda declarada en `meta$entrada_convertida`.

### La via DBI sobre tablas que no entran en memoria

- Modos `muestreado` y `aproximado`. El primero muestrea **en el motor**
  —`TABLESAMPLE` donde existe, orden pseudoaleatorio con limite donde
  no—; el segundo usa las funciones aproximadas nativas
  —`APPROX_COUNT_DISTINCT`, `PERCENTILE_CONT`, `approx_quantile`— con la
  misma mecanica de capacidad declarada y resuelta por sonda que ya
  usaba el dialecto.
- Toda metrica muestreada o aproximada viaja diciendolo: `estado`
  distingue `calculado`, `estimado` y `no_disponible`, y cada fila lleva
  `universo`, `tamano_muestra`, `fraccion`, `metodo` y `error_esperado`,
  que es `desconocido` cuando el motor no documenta una cota. **Nunca
  una cota inventada.**
- El conteo de distintos tiene estado propio, `observado_muestra`. La
  cardinalidad de una muestra no estima la del universo sin un estimador
  declarado, asi que se informa por lo que es, con el universo al lado.
- [`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
  predice **exactamente** las consultas de los cinco modos. Las sondas
  nuevas gastan un numero fijo aunque acierten en la primera forma, por
  la misma razon que la del desvio: un costo que dependa del motor hace
  que el plan deje de predecir.
- Los conteos conservan `integer64` cuando `bit64` esta instalado, asi
  que un conteo por encima de 2^53 deja de perder exactitud. Sin
  `bit64`, `meta$conteo_exacto` lo sigue declarando.

### Bases enteras, y el costo de compararlas

- [`detectar_relaciones()`](https://sebollin.github.io/lupa/reference/detectar_relaciones.md)
  y
  [`relaciones_coleccion()`](https://sebollin.github.io/lupa/reference/relaciones_coleccion.md)
  aceptan `columnas_candidatas`: declarar que columnas pueden participar
  es lo que hace manejable un costo que crece con el producto de anchos.
  En una prueba de 32 por 32 columnas, 1.024 combinaciones bajan a 9.
- **Dos clases de poda, y no se tratan igual.** Dos columnas de la misma
  familia con rangos numericos disjuntos no comparten ningun valor y eso
  se sabe sin comparar: la fila sale como siempre y la comparacion se
  ahorra. Esa poda esta siempre activa porque no cambia nada de lo
  informado. Las otras dos —familias distintas, cardinalidad imposible—
  si lo cambiarian: una columna de texto puede guardar `"2020-01-05"` y
  coincidir con una de fecha, y una cardinalidad imposible no dice que
  no haya coincidencias sino que no llegan al umbral. Van detras de
  `podar = TRUE`, y cuando se aplican **el par no desaparece**: sale con
  `cardinalidad = "sin_comparar"`, coberturas `NA` y su motivo. Un par
  que no se evaluo no es un par sin relacion.
- `tope_memoria_mb` acota las filas comparadas y declara los pares
  pendientes en vez de devolver menos sin decirlo.
- La granularidad `conjuntoColecciones` pasa a medirse, con la frontera
  declarada por el usuario y pesos explicitos: agregar entre colecciones
  sin pesos seria inventar un juicio, que es lo que
  [`indice_calidad()`](https://sebollin.github.io/lupa/reference/indice_calidad.md)
  se niega a hacer. `organizacion` y `conjuntoOrganizaciones` siguen sin
  implementar, y por la misma razon de siempre: no falta codigo, falta
  el objeto.
- La entrada `data.frame` de
  [`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md)
  valida `NA`, cadenas vacias y tipos igual que la entrada por vector de
  texto. Dos puertas del mismo paquete dejaron de comportarse distinto
  ante la misma entrada mala.

### Lo que el detector de vocabulario no puede ver, dicho

- `n_grupos_sin_variante_rara` cuenta los grupos de formas cercanas que
  el criterio de variante rara **nunca llego a formar**. Una variante
  mal escrita que ocupa la mitad de la columna no es una variante rara
  para el comparador y no se informaba; ahora el limite se declara
  aunque la deteccion no cambie.
- `variantes_equifrecuentes_vocabulario` es el diagnostico para ese
  caso: dos formas cercanas que se reparten la columna sin que ninguna
  sea dominante, que es la firma de dos operadores, una plantilla rota o
  una migracion parcial. **Queda apagado por omision, y la razon esta
  medida**: sobre la bateria de 31 tablas limpias produce un grupo
  sospechoso donde no hay defecto, y dispara en tablas de menos de
  veinte filas. Es aditivo: encenderlo no cambia ni pierde ninguna
  deteccion de `casi_duplicados_vocabulario`.
- La evidencia de `patron_raro` declara
  `desvio_unicamente_largo_corrida_numerica` cuando el unico desvio es
  la cantidad de digitos. No baja el ruido de trescientos correos
  correlativos —eso no tiene solucion sin dominio— pero convierte una
  lectura de dos segundos en una de cero.
- La razon de permutacion viaja como evidencia descriptiva del detector
  de orden. No filtra nada: el criterio quedo refutado con precision 0 %
  en cuatro tablas reales y no se usa para decidir.

### Costos declarados donde antes solo se tardaba

- `max_comparaciones_dependencias` acota la busqueda de dependencias
  funcionales, cuyo costo es del orden de `columnas^2 x filas` y empeora
  con determinantes casi unicos. Cuando el presupuesto se agota, lo
  comparado se informa y lo que quedo sin comparar se declara.
- La deteccion de fechas partidas dejo de materializar el producto
  cartesiano de los candidatos ano/mes/dia; el detector de vocabulario
  dejo de recorrer el vocabulario completo antes de aplicar su tope. Los
  dos declaran lo que no evaluaron.
- La confirmacion de un validador de documentos deja de recorrer la
  columna entera sin presupuesto. Cuando el tope se alcanza, el
  fundamento dice sobre cuantos valores se confirmo.
- `datos[0, 0]` sobre un objeto `sf` conserva la geometria, porque esa
  columna es pegajosa por diseno de ese paquete. Con las dependencias
  apagadas, el objeto vacio llegaba con una columna y el diagnostico
  declaraba un recorte que nadie pidio.
- Un par no comparado trae cobertura `NA`, y `datos[NA, ]` devuelve una
  fila entera de `NA` en vez de ninguna.
  [`relaciones_coleccion()`](https://sebollin.github.io/lupa/reference/relaciones_coleccion.md)
  filtra con [`which()`](https://rdrr.io/r/base/which.html).

### Infraestructura

- `inst/WORDLIST` completa la lista que faltaba:
  `spelling::spell_check_package()` vuelve cero. Las palabras son
  nombres propios, siglas, terminos tecnicos y fragmentos de
  identificadores del paquete.
- `CONTRIBUTING.md` corrige el orden de la verificacion previa.
  `test_dir()` y `test_file()` cargan el paquete con
  [`library(lupa)`](https://github.com/sebollin/lupa), que no expone las
  funciones internas y produce veinte errores falsos de «could not find
  function»; `test_check("lupa")` es lo que corre `R CMD check`, y
  necesita el paquete instalado.

### Cuatro motores reales

- El desvio se pide primero con la funcion nativa del motor
  —`STDDEV_SAMP` del estandar, `STDEV` en SQL Server— y solo cae al
  calculo de dos pasadas donde no existe ninguna de las dos. La forma
  anterior ponia la media como subconsulta escalar para no incrustarla
  como literal en el SQL guardado, y SQL Server rechaza una subconsulta
  dentro de un agregado: el arreglo de privacidad habia roto la
  compatibilidad, y solo un motor real podia mostrarlo.
- Verificado contra PostgreSQL 16, MySQL 8, SQL Server 2022 y SQLite: en
  los cuatro, ninguna metrica queda no disponible, y la media, la
  mediana y el desvio calculados por el motor coinciden con los
  calculados en R sobre la tabla entera. En SQL Server la sonda resuelve
  el dialecto `top` por su cuenta.

### Numeros que no pueden ser

- [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  resuelve un nombre calificado con punto igual que
  [`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md).
  `dbExistsTable()` no lo resuelve, asi que el mismo texto funcionaba en
  una funcion y fallaba en la otra diciendo que la tabla no existe. Un
  nombre literal con punto adentro sigue teniendo prioridad.

- El universo aplicable declarado sale tambien del analisis, no solo de
  los conteos. Con filas no aplicables que tienen valor, `n_distintos`
  las contaba mientras `n_validos` ya no, y `tasa_distintos` podia pasar
  de 1.

- La trazabilidad no nombra filas fuera del universo declarado. El
  conteo ya las excluia y nombrarlas igual producia la incoherencia que
  la guarda detecta.

- La via DBI valida la coherencia interna de lo que informa el motor:
  mas valores distintos que validos, o una frecuencia de moda mayor que
  las filas validas, son imposibles y se declaran no disponibles en vez
  de publicarse como calculados.

- El lexico de nombres de columna con datos personales cubre `persona`,
  `cliente`, `paciente`, `socio`, `beneficiario`, `titular`,
  `funcionario`, `usuario`, `solicitante`, `responsable`,
  `contribuyente`, `residencia`, `lugar_residencia` y `barrio`. Ningun
  lexico puede ser completo; estos son los frecuentes en registros
  administrativos.

### La via DBI deja de asumir un dialecto y de tirar lo que ya midio

- [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  resuelve el dialecto con una sonda de cero filas **antes** de emitir
  el bloque de agregados: `limit`, `top`, `fetch_first`, `rownum` y una
  via portable con `dbSendQuery()` + `dbFetch(n)`. Se puede declarar con
  `dialecto =` si la sonda no acierta.
- Las cuatro consultas obligatorias —campos, conteo, esquema y muestra—
  dejaron de ser fatales. Si la muestra falla, el objeto vuelve con
  `resumen_tabla` completo, `perfil_muestra = NULL` y una fila de
  cobertura con el motivo. Antes, un motor que no acepta `LIMIT`
  descartaba las 777 consultas ya pagadas.
- El esquema y la muestra enumeran columnas en vez de usar `SELECT *`, y
  si la lectura conjunta falla sondean columna por columna para
  descartar solo la que el motor rechaza.
- Los [`stop()`](https://rdrr.io/r/base/stop.html) de la via DBI tienen
  clase de condicion propia, asi que un fallo se puede atrapar y el
  resumen rescatar.
- Los alias se comillan y se comparan sin distinguir caja. Un motor que
  los pliega a mayusculas ya no produce metricas con estado `calculado`
  y valor vacio, que era peor que declararlas no disponibles.
- Argumentos nuevos para acotar el costo: `modo`, `metricas` y
  `max_consultas`, mas
  [`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md),
  que dice cuantas consultas va a costar el perfilado antes de
  emitirlas.
- `resumen_tabla` pasa por la proteccion de datos personales, que antes
  solo alcanzaba al perfil de la muestra: el bloque sin proteger era
  justamente el de alcance completo. El SQL guardado del desvio ya no
  incrusta la media observada. Nuevo `print.perfil_dbi`, que no imprime
  ningun valor de celda.

### El nivel coleccion deja de informar cero donde no midio

- Una tabla vacia ya no produce `prop_faltantes_maxima = -Inf` ni
  `n_columnas_sin_faltantes = 0`: son `NA`, con una fila de cobertura
  que declara que no hay nada que medir. Antes esa tabla se ordenaba
  como la de mejor calidad de la base.
- Componente nuevo `cobertura_metricas`: la declaracion de lo que el
  motor rechazo sube al nivel coleccion antes de descartar los perfiles,
  y existe tambien con `conservar_perfiles = FALSE`.
- [`estimar_costo_coleccion()`](https://sebollin.github.io/lupa/reference/estimar_costo_coleccion.md)
  usa la formula cerrada en vez de materializar los pares: 27,4 s y 233
  MB con 1700 tablas pasaron a 0,42 s. El resultado es identico. Acepta
  cero pares, deduplica y rechaza los autorreferenciales.
- [`relaciones_coleccion()`](https://sebollin.github.io/lupa/reference/relaciones_coleccion.md)
  cachea cada tabla en vez de releerla una vez por par.
- Los identificadores de mas de dos partes se rechazan nombrando la
  causa real. Antes se aceptaban y el fallo se le devolvia al usuario
  como un problema de permisos sobre una tabla que si podia leer.

### El perfilado espacial deja de ser inviable por tiempo

- Las columnas no atomicas ya no pasan por la maquinaria de texto.
  Convertirlas no producia sus valores sino su representacion como
  codigo, una vez por cada etapa que las tocaba: era el 85 % del costo
  de perfilar una capa espacial. Perfilar 62 poligonos de 200.000
  vertices paso de 323 s a 2,3 s.
- La transformacion de coordenadas se hace en una sola llamada y no una
  por geometria.
- Presupuesto de geometrias y de vertices, con el recorte declarado.
- WKT, WKB y hexadecimal se detectan, se convierten y se miden. Antes
  quedaban todas las metricas en `NA` con `cobertura_diagnosticos`
  vacia, y
  [`cobertura_analisis()`](https://sebollin.github.io/lupa/reference/cobertura_analisis.md)
  llegaba a afirmar que la geometria no aplicaba sobre datos que si eran
  geometricos.
- Un fallo parcial ya no descarta la columna entera: una geometria
  intransformable o un `NA` de la validez dejan de borrar el conteo y
  los indices de todas las demas.

### El vacio por diseno se declara y deja de contarse como defecto

- [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  acepta `aplicabilidad`, una lista de formulas por columna que declara
  en que filas la columna corresponde. Las filas fuera del universo
  salen de `n_faltantes` y de `prop_faltantes` en vez de contarse como
  ausencia. Antes, una tabla completa en las filas donde el dato
  corresponde podia informar completitud baja: el conteo era correcto y
  la lectura falsa.
- [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  acepta `columnas_opcionales` para el caso mas simple, donde la
  ausencia nunca es defecto y no hay una regla que escribir.
- La regla declarada, el universo resultante y las filas donde la regla
  no se pudo determinar quedan en `cobertura_diagnosticos`. Un universo
  recortado sin constancia seria el mismo defecto al reves.
- Las filas donde la regla no se puede evaluar no se cuentan como
  aplicables ni como no aplicables: van a
  `n_aplicabilidad_indeterminada`, porque no saber no es lo mismo que no
  corresponder.
- Nuevo hallazgo `valor_fuera_de_aplicabilidad`: un valor presente donde
  la regla dice que la columna no corresponde. Es el error simetrico y
  sin universo declarado no tenia forma de aparecer.
- La metrica `NoNulo` acepta `aplicable` con el mismo criterio, para que
  el universo declarado llegue al tablero y no solo al hallazgo.
- Nueva funcion
  [`perfilar_por()`](https://sebollin.github.io/lupa/reference/perfilar_por.md):
  perfila cada grupo de filas por separado y devuelve los hallazgos de
  todos los grupos en una tabla. Es la respuesta al formato largo, donde
  una sola columna mezcla dominios sin relacion. Las columnas
  enteramente ausentes dentro de cada grupo se descartan antes de
  perfilar, y el descarte se declara.
- Nueva vineta `vacio-por-diseno`, que documenta el supuesto tabular del
  paquete y las seis formas de tabla donde no vale.

### Privacidad: ante la duda se protege

- El clasificador de datos personales dejaba sin proteger una columna
  cuya forma era compatible con un documento de identidad cuando el
  validador no podia verificarla. Ese es justamente el caso de una base
  sucia, y los valores reales terminaban escritos en la evidencia de los
  hallazgos. Ahora se protege igual; la evidencia sigue declarando que
  la clasificacion es debil.

### Conteos que no se pueden contar

- `.moda_columna()` distinguia mal dos ausencias: la frecuencia cero de
  una columna sin valores validos y la imposibilidad de contar sobre una
  columna no atomica. La segunda ahora es `NA`.
- El hallazgo `constante` sobre una columna no atomica informa la
  frecuencia que se deduce de las filas validas y las nombra en la
  trazabilidad, en vez de informar cero afectados. El discriminador dejo
  de ser la etiqueta del tipo, que dejaba afuera a las columnas
  espaciales.

### Recortes declarados donde se los busca

- El recorte por `max_columnas_dependencias` se declara en
  `cobertura_diagnosticos`, como ya lo hacia el recorte hermano de la
  busqueda aritmetica. El tope aplicado se conserva como atributo, y el
  motivo aclara que la seleccion de columnas es por posicion.

### Patrones raros: ventana de operacion visible

- `patron_raro` declara en `cobertura_diagnosticos` cuando no puede
  ejecutarse porque el patron dominante no alcanza
  `umbral_patron_dominante`. La fila conserva la proporcion observada y
  explica como ajustar ese argumento.
- La evidencia de cada hallazgo `patron_raro` publica la proporcion del
  patron dominante y cuantas filas quedaron en patrones no dominantes
  excluidos por superar `umbral_patron_raro`. Ese conteo queda en la
  evidencia, no en la cobertura, porque no es una no medicion del
  diagnostico.

### Patrones raros y trazas accionables

- `patron_raro` conserva separado el tope de presentación y el alcance
  de la trazabilidad: `resumen_patrones` y la evidencia siguen mostrando
  como máximo seis patrones, mientras la traza usa sus nombres raros
  completos hasta un límite de 5.000. Si se alcanza ese límite,
  `cobertura_diagnosticos` y el alcance de la traza lo declaran.
- Las ausencias de una columna de lista se nombran, no sólo se cuentan.
  Una columna de listas —o un BLOB leído por
  [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)—
  informaba cuántos valores faltaban sin decir en qué filas, aunque
  [`is.na()`](https://rdrr.io/r/base/NA.html) los identifica elemento a
  elemento y es el mismo criterio con el que se contaron. Lo encontró la
  propia guarda de coherencia, que era exactamente para lo que se
  agregó.
- `casi_duplicados_vocabulario` entrega primero las filas de formas no
  dominantes y después las de formas dominantes. La unidad sigue siendo
  `valor_distinto`, el grupo sigue incluyendo la forma dominante y la
  evidencia informa cuántas filas mostradas pertenecen a cada tipo de
  forma.

### La traza de vocabulario y la guarda de coherencia cierran el circuito

- `casi_duplicados_vocabulario` conserva
  `unidad_conteo = "valor_distinto"` y ahora enumera las filas que
  contienen los valores de cada grupo seleccionado, incluida la forma
  dominante. La distancia sigue siendo una señal heurística, no una
  afirmación de identidad.
- [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  conserva cualquier hallazgo cuya traza no coincida y emite la
  advertencia de clase `lupa_trazabilidad_incoherente`. La guarda
  compara el total anterior al límite de presentación, funciona en ambas
  direcciones y adapta la comparación a la unidad declarada.

### Conteos y trazabilidad dejan de mezclar unidades

- `mayusculas_inconsistentes` y `normalizacion_unicode` declaran
  `unidad_conteo = "valor_distinto"` y cuentan en `n_evaluados` los
  valores distintos evaluados. `n_afectados` ya contaba esos valores; su
  traza sigue siendo por fila y enumera todas las filas que contienen
  los valores afectados, no sólo las defectuosas.
- `filas_duplicadas` cuenta ahora todas las filas participantes de los
  grupos, en línea con `EntidadDuplicada` y con
  `marcar_filas_duplicadas`. La evidencia conserva el número de
  excedentes para la acción que elimina repeticiones.
- Una constante de listas cuya frecuencia no puede contarse informa `NA`
  en `n_afectados` y deja el motivo en `cobertura_diagnosticos`; una
  matriz no analizada enumera todas sus filas en la trazabilidad.

### La trazabilidad deja de recalcular lo que el detector ya decidió

Un hallazgo dice cuántas unidades afecta y, cuando puede, cuáles. Ese
«cuáles» lo resolvía una rama de índices aparte que en varios casos
aplicaba un criterio **distinto** del detector que había producido el
hallazgo. Los dos no coincidían, y el desacuerdo no se veía porque nada
comparaba la evidencia contra los índices.

- `patron_raro` no nombraba ninguna fila cuando la columna tenía algún
  patrón de frecuencia intermedia. La guarda comparaba el total de
  patrones distintos contra el tamaño del resumen, que son cosas
  distintas: el resumen es el patrón dominante **más los patrones
  raros**, no un top-N, así que se disparaba en una situación
  perfectamente normal. Ahora
  [`descubrir_patrones()`](https://sebollin.github.io/lupa/reference/descubrir_patrones.md)
  expone `n_patrones_raros` y la guarda pregunta lo único que
  corresponde —si ese conjunto fue recortado por el tope de seis—. Sin
  recorte la enumeración es completa y `n_afectados` toma su valor real;
  con recorte se enumera igual y el alcance es `patrones_parciales`,
  declarado en la cobertura.
- `outliers`, `valores_no_finitos`, `ceros_no_permitidos` y
  `negativos_no_permitidos` condicionaban la enumeración a que la
  columna fuera numérica **en su tipo declarado**. Al leer un CSV como
  texto —el caso más común que hay— el perfilador infiere numérico,
  convierte y cuenta bien, pero la rama miraba un `character` y no
  devolvía nada: se informaban diez atípicos y no se nombraba ninguna
  fila. Ahora rastrean sobre la vista cuantitativa inferida, la misma
  que usó el detector.
- `codificacion_rota` reimplementaba la detección con una clase de
  caracteres más angosta que la del detector, de modo que un valor con
  el mojibake del carácter de reemplazo se contaba y no se nombraba —y
  ese vacío salía declarado con alcance `completo`, que es justo lo que
  este paquete no hace—. Ahora reutiliza la máscara del detector.
- `patron_raro` nombraba, además, filas que su propia evidencia acababa
  de descartar. En una secuencia entera densa, un patrón que difiere
  sólo por el largo —`9` frente a `9+`— no es un desvío: es el mismo
  número con menos dígitos. El detector lo filtraba al armar la
  evidencia; la rama de índices y `n_afectados` recorrían el resumen
  crudo. El conjunto filtrado se calcula ahora una sola vez y viaja con
  el resultado, de modo que no puede haber dos criterios.

**El principio que unifica los cuatro: la trazabilidad no recalcula lo
que el detector ya resolvió.** Cada vez que lo recalculaba, los dos
criterios se separaban en silencio.

**Y la prueba que faltaba.** La suite verificaba conteos, no
identidades: una prueba que comprueba `n_afectados == 10` pasa igual si
el paquete nombra diez filas equivocadas, ninguna, o seiscientas. Por
eso ninguno de estos desajustes se veía con toda la suite en verde.
Ahora hay fixtures que construyen tablas con índices corrompidos
**conocidos de antemano** y verifican aciertos, falsos positivos y
pérdidas.

### El piso de asimetría del vocabulario declara lo que deja afuera

- El comparador de vocabulario abría grupos por distancia con cualquier
  desbalance de frecuencias, y así señalaba `este` frente a `oeste` —dos
  puntos cardinales— como posibles variantes de un mismo valor. Medido
  sobre tablas limpias y sobre erratas sembradas, los falsos positivos
  quedan entre `1,0` y `1,5` de asimetría y las erratas reales desde
  `9,0`, así que ahora se exige una asimetría mínima de `2`,
  configurable con `min_asimetria_vocabulario`.
- **El piso no se aplica a los grupos formados por normalización.**
  `Montevideo`, `MONTEVIDEO` y `Montevideo` son tres grafías del mismo
  valor y con una aparición cada una su asimetría es `1,0`: ahí la
  equivalencia está comprobada y no es una conjetura sobre una errata.
- **Y lo que el piso deja afuera se declara.** En la banda de asimetría
  baja cae también una errata sistemática que afecte a una fracción
  grande de los registros, y por la forma es indistinguible de dos
  valores legítimamente parecidos. Elegir en silencio cuál se sacrifica
  sería justo lo que este paquete no hace: `cobertura_diagnosticos`
  informa cuántos grupos quedaron bajo el piso y cómo bajarlo.

### Colecciones: el séptimo nivel de granularidad deja de estar sólo declarado

- [`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md)
  declara qué tablas componen una base de datos, con su esquema, y
  [`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md)
  devuelve una fila por tabla con sus agregados exactos, más
  `cobertura_coleccion` con lo que no se pudo medir. La granularidad
  `coleccion` del marco estaba declarada y no se medía: lo que faltaba
  no era código sino el objeto.
- **La frontera se declara, nunca se descubre.** Recorrer el catálogo
  convertiría un error de permisos en un resultado, y una colección real
  pasa de mil tablas repartidas en decenas de esquemas.
- **El esquema es parte de la identidad de la tabla**, así que el mismo
  nombre en dos esquemas son dos tablas y no una repetida.
- **Lo que no se pudo leer se declara y nunca queda en cero**: una tabla
  sin permiso, un objeto declarado como vista, un motor que rechaza un
  agregado. En bases institucionales los permisos parciales son el caso
  normal.
- **Cada tabla declara su propio muestreo**, y no se promedian alcances
  distintos como si fueran uno.
- **No hay lectura instantánea.** Perfilar una colección son muchas
  consultas y la base puede cambiar entre ellas, así que cada fila trae
  el `momento` en que se midió y `meta$snapshot` declara que no lo hubo.
- El perfil pesado de cada tabla no se retiene salvo que se pida con
  `conservar_perfiles = TRUE`: con cientos de tablas no entraría en
  memoria.
- [`agregar()`](https://sebollin.github.io/lupa/reference/agregar.md)
  mide ahora esta granularidad, con tres condiciones. Exige la
  **frontera declarada**, porque sin saber sobre qué tablas se agrega el
  número no significa nada. Admite **sólo `promedio_ponderado`**: sin
  esa restricción bastaba pedir `promedio` para obtener un número entre
  tablas de universos distintos sin declarar nada, que es el juicio que
  el paquete se niega a inventar. Y **la cobertura viaja pegada al
  número**.
- Esa última condición es la que más importa, y salió de refutar el
  diseño. Un número sobre «la colección» calculado sólo con las tablas
  que se pudieron medir **informa como medido lo que no se midió**: el
  peso de la tabla ausente desaparece en vez de manifestar la falta de
  cobertura. Con quince tablas declaradas y seis sin permiso, el número
  describe nueve y se presenta como si describiera la colección. Ahora
  el resultado trae `tablas_declaradas`, `tablas_en_el_numero`,
  `tablas_sin_medir` con su motivo, la `cobertura` y la advertencia de
  que leerlo sin ella sería exactamente ese error.
- [`relaciones_coleccion()`](https://sebollin.github.io/lupa/reference/relaciones_coleccion.md)
  busca claves foráneas candidatas entre **los pares que se declaren**,
  y
  [`estimar_costo_coleccion()`](https://sebollin.github.io/lupa/reference/estimar_costo_coleccion.md)
  permite ver el costo antes. Los pares se declaran por la misma razón
  que la frontera: una clave foránea es **dirigida**, así que mil tablas
  dan casi un millón de direcciones, y el costo real no lo da el número
  de tablas sino el de comparaciones entre columnas. Cada par se compara
  sobre una muestra, y el objeto declara que **una relación candidata
  sobre una muestra no es una clave foránea comprobada**: es un indicio
  que hay que confirmar contra el diccionario de datos. Un par que no se
  pudo leer se declara en `cobertura_pares` en vez de desaparecer.
- [`granularidades()`](https://sebollin.github.io/lupa/reference/granularidades.md)
  declara el séptimo nivel como implementado: siete de diez. Los tres
  últimos siguen sin objeto, y no por falta de código: qué bases
  componen un conjunto y qué bases pertenecen a un organismo son
  decisiones de gobernanza que no están en ningún dato.

### Evaluar estimaciones que calculó otra herramienta

- [`medicion_desde_estimaciones()`](https://sebollin.github.io/lupa/reference/medicion_desde_estimaciones.md)
  recibe estimaciones ya calculadas —por `survey`, por
  [`calidad`](https://github.com/inesscc/calidad) del INE de Chile, o
  por cualquier otra fuente— y las lleva al contrato de
  [`medir()`](https://sebollin.github.io/lupa/reference/medir.md), para
  poder evaluarlas contra un marco declarado. **`lupa` no estima**: eso
  necesita diseño muestral, estimación de varianza y otra disciplina; lo
  que sabe hacer es evaluar contra un marco, y eso es lo que ofrece.
- Cada estadístico se convierte en **su propia medida canónica**, con su
  métrica, su tipo, su unidad y su orientación, porque los siete tienen
  dominios distintos: un coeficiente de variación de `0,30` y un tamaño
  de muestra de `0,30` no se leen igual.
  [`estadisticos_estimacion()`](https://sebollin.github.io/lupa/reference/estadisticos_estimacion.md)
  publica el catálogo.
- **La procedencia viaja en cada medida** y es obligatoria, para que
  nadie lea el resultado como si `lupa` lo hubiera calculado. Los
  estadísticos que la tabla no traiga no se rellenan con ceros: se
  declaran ausentes.

### Señales redundantes: la contradicción que ninguna columna muestra sola

- [`senal_redundante()`](https://sebollin.github.io/lupa/reference/senal_redundante.md)
  declara que varias columnas de una tabla codifican el mismo hecho, y
  [`detectar_discordancias()`](https://sebollin.github.io/lupa/reference/detectar_discordancias.md)
  informa las filas donde no concuerdan dentro de la ventana declarada.
  El caso típico son el año de la fecha, el año fiscal y el año del
  archivo: los tres pueden ser plausibles por separado y aun así
  contradecirse.
- **El grupo se declara, nunca se adivina.** Dos columnas de año pueden
  ser el de nacimiento y el de ingreso, y no tienen por qué coincidir;
  suponerlo sería inventar conocimiento del dominio.
- `transformacion` lleva columnas guardadas de formas distintas a una
  escala comparable —extraer el año de una fecha, por ejemplo—, y
  `ventana` es la tolerancia **en las unidades del valor comparado**,
  que no se adivina.
- Una fila con alguna columna ausente **no cuenta como desacuerdo**:
  sale del universo, y `n_evaluadas` lo declara. Si ninguna fila tiene
  todas las columnas presentes, `n_discordantes` queda en `NA` y la
  señal se declara no evaluada, en vez de informar cero discordancias.

### Los umbrales de una regla salen del closure y se pueden consultar

- [`regla_evaluacion()`](https://sebollin.github.io/lupa/reference/reglas_evaluacion.md)
  acepta `umbrales`, una lista con nombres que se le pasan a la
  condición al evaluarla. Antes el umbral quedaba encerrado en el
  *closure*: para mover un número había que escribir otra regla, y nadie
  podía consultar cuál era. Ahora la misma función evalúa distinto con
  dos umbrales —0,67 y 0,33 sobre los mismos valores— sin reconstruir la
  lógica.
- Una condición que no recibe un umbral declarado se rechaza enumerando
  los argumentos que sí acepta, en vez de ignorarlo en silencio. Una
  condición con `...` los recibe todos.
- **[`propiedades_regla()`](https://sebollin.github.io/lupa/reference/propiedades_regla.md)**
  muestra lo que una regla declara: métricas, nivel, proporción mínima,
  desenlace y umbrales. Es la contraparte de
  [`propiedades_metrica()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md),
  que describe métricas: un umbral pertenece a una regla y no cabía
  allí.

### Trazabilidad por clave declarada: del hallazgo que se lee al que se verifica

- [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  acepta `clave` con las columnas que identifican una fila. La
  trazabilidad de cada hallazgo trae además el valor de esas columnas
  para las filas señaladas, así que el caso se puede buscar en el
  sistema de origen sin abrir la tabla.
  [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md)
  lo traslada por `...` y por `argumentos_perfil`.
- La trazabilidad separa dos ejes que antes se confundían: `estado`
  —`disponible`, `truncada`, `no_disponible`, `no_aplica`— dice si se
  pudo localizar y hasta dónde, y **`localizador`** —`indice_fila`,
  `clave_declarada`, `ninguno`— dice con qué. Una trazabilidad puede ser
  al mismo tiempo por clave y truncada.
- Las claves viajan como data frame, una fila por índice mostrado y una
  columna por componente: concatenarlas perdería los tipos y haría
  ambigua una clave compuesta.
- **La clave que permite verificar es la que identifica a una persona.**
  Si alguna de sus columnas se clasifica como dato personal y la
  protección está activa, sus valores salen enmascarados igual que la
  evidencia, en todos los hallazgos —la clave viaja con la fila, no con
  la columna del hallazgo— y `claves_protegidas` declara cuáles se
  enmascararon.
- Una clave que nombra columnas inexistentes se rechaza enumerando las
  disponibles; una que no es única avisa y sigue, porque sirve igual
  para localizar aunque deje de ser una clave.

### La cobertura del vocabulario deja de contradecir al hallazgo

- `casi_duplicados_vocabulario` nombraba dos diagnósticos distintos:
  agrupar valores por su forma normalizada, que no depende de nada, y
  medir proximidad por distancia de edición, que necesita `stringdist`.
  Sin ese paquete el primero medía y el segundo se declaraba **bajo el
  mismo nombre y para la misma columna**, así que cruzar
  `cobertura_diagnosticos` con `hallazgos` por `(diagnostico, columna)`
  —el uso natural para un consumidor automático— devolvía una
  contradicción: el mismo diagnóstico declarado como no evaluado y
  reportado como medido.
- La cobertura pasa a llamarse **`proximidad_vocabulario`** en las tres
  razones que le corresponden: falta `stringdist`, el vocabulario excede
  el alcance de comparación, y el grupo candidato mayor abarca tanto que
  el diagnóstico no aplica. El hallazgo conserva su nombre. Quien filtre
  la cobertura por el nombre viejo tiene que actualizar el filtro.

### Dos cosas más que el objeto ahora declara

- `patron_raro` distingue en la evidencia las dos clases de desvío:
  `clase_desvio=largo_de_corrida` cuando el valor señalado sigue el
  mismo patrón con un número de otro largo —`persona9@` frente a
  `persona300@`— y `clase_desvio=estructural` cuando es otra forma
  —`SIN CODIGO` frente a `AB-12345`—. **La severidad no cambia**: los
  dos casos son indistinguibles por la forma y eso está medido. Lo que
  cambia es que quien lee el hallazgo lo resuelve de un vistazo en vez
  de comparar patrones a ojo.
- [`agregar()`](https://sebollin.github.io/lupa/reference/agregar.md)
  acepta el nombre relacional de la granularidad —`celda`, `columna`,
  `tupla`, `tabla`— igual que
  [`metrica()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md).
  Su propio mensaje de error ya los enumeraba, así que rechazarlos era
  una inconsistencia. El objeto sigue guardando el nombre canónico del
  marco.

### Spearman para relaciones monótonas que no son lineales

- [`detectar_asociaciones()`](https://sebollin.github.io/lupa/reference/detectar_asociaciones.md)
  acepta `metodo_numerico = "spearman"` y mide asociación monótona sobre
  los rangos, sin suponer linealidad. Sobre una relación cúbica con
  ruido, Pearson da 0,918 y Spearman 0,997. Pearson sigue siendo el
  valor por omisión, y el método elegido viaja en la columna `metodo`
  con su supuesto en `supuesto`, así que ninguna lectura depende de
  recordar cuál se pidió.
  [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md)
  lo traslada con `metodo_asociacion_numerica`.
- Los dos README explican ahora dónde viven la distribución de valores y
  las correlaciones —en
  [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md),
  no en
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)—
  y por qué esa separación es deliberada.

### Tres afirmaciones que el paquete hacía sin fundamento suficiente

- `alta_cardinalidad` se apoyaba sólo en la tasa de valores distintos, y
  con pocas filas esa tasa está dominada por el tamaño: una columna de
  dos valores en tres filas daba 0,67 y superaba el umbral, aunque una
  columna de dos valores no puede tener cardinalidad alta. Ahora el
  hallazgo exige además al menos diez valores distintos. Las columnas
  con cardinalidad alta real —treinta valores distintos en cuarenta
  filas— se siguen informando igual.
- `columnas_duplicadas` afirmaba que dos columnas tienen el mismo
  contenido en tablas **sin ninguna fila**, donde dos columnas vacías
  coinciden sin que eso sea evidencia. Ese caso pasó a
  `cobertura_diagnosticos` con su motivo. Cuando sí hay filas, la
  evidencia declara ahora sobre cuántas se comparó.
- `relacion_aritmetica_columnas` se salteaba en silencio cuando la tabla
  no llegaba al mínimo de filas comparables. Ahora se declara en
  `cobertura_diagnosticos`, y sólo cuando había combinaciones de
  columnas numéricas que evaluar.

### El vínculo entre una acción del plan y su hallazgo ya no depende de la prosa

- [`planificar_limpieza()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)
  recuperaba el par de columnas duplicadas comparando la cadena de
  evidencia completa del hallazgo. Al enriquecerse ese texto, dos pares
  distintos colapsaban en el mismo y el plan perdía una acción. El
  vínculo se hace ahora contra el primer tramo de la evidencia, que es
  el que identifica el par.

### El perfilado no toca los datos, y ahora está probado

- Ninguna función de análisis altera la tabla que recibe: ni sus
  valores, ni sus tipos, ni sus nombres, ni sus atributos. Una prueba de
  regresión lo verifica en
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
  [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md),
  [`detectar_claves()`](https://sebollin.github.io/lupa/reference/detectar_claves.md),
  [`detectar_dependencias()`](https://sebollin.github.io/lupa/reference/detectar_dependencias.md),
  [`distribucion_valores()`](https://sebollin.github.io/lupa/reference/distribucion_valores.md),
  [`detectar_asociaciones()`](https://sebollin.github.io/lupa/reference/detectar_asociaciones.md),
  [`detectar_duplicados_aproximados()`](https://sebollin.github.io/lupa/reference/detectar_duplicados_aproximados.md),
  [`planificar_limpieza()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)
  y
  [`guiar_limpieza()`](https://sebollin.github.io/lupa/reference/guiar_limpieza.md).
  El caso que importa es `data.table`, que R permite modificar por
  referencia: la prueba compara además la dirección de memoria del
  objeto.
  [`aplicar()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)
  devuelve una copia y deja intacta la original.

### Duplicados: el hallazgo no afirma una igualdad que produjo la normalización

- La normalización por omisión iguala mayúsculas, espacios, acentos y
  comillas, así que un par puede coincidir después de normalizar sin que
  los valores guardados sean iguales. `tipo_par` distingue ahora
  `exacto`, `exacto_normalizado` y `aproximado`; `igualo_normalizar`
  deja esa causa visible en cada fila. El hallazgo
  `duplicados_exactos_normalizados` evita afirmar que dos filas tienen
  los mismos valores y la trazabilidad lo busca entre los pares de ese
  tipo. `n_pares_exactos` cuenta sólo texto guardado igual y
  `n_pares_exactos_normalizados` completa la explicación junto con
  `n_pares_aproximados`.

### Casi-claves y precedencia de ausencias

- [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  informa una `casi_clave` cuando una columna tiene al menos 100 filas,
  supera 90 % de valores distintos y al menos la mitad de sus duplicados
  excedentes se concentra en un valor. Las fechas y fecha-hora se
  excluyen por su rol propuesto. La evidencia enumera las colisiones,
  sus frecuencias y los criterios aplicados. Los vectores `double` con
  algún valor finito fraccionario se excluyen, mientras que los formados
  por valores enteros se conservan para admitir identificadores
  importados desde archivos de texto. Los vectores `integer64` cuentan
  como enteros semánticos.
  [`detectar_claves()`](https://sebollin.github.io/lupa/reference/detectar_claves.md)
  las expone sin confundirlas con claves exactas, y
  [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md)
  las reitera en sus advertencias.
- `casi_duplicados_vocabulario` retira primero los valores ya detectados
  como `faltantes_disfrazados`. Un centinela de ausencia deja de
  presentarse como posible errata de otro valor; las variantes que no
  son centinelas conservan el diagnóstico.

### Tablero, indice declarado y medicion agregada

- [`tablero_calidad()`](https://sebollin.github.io/lupa/reference/tablero_calidad.md)
  resume una corrida por metrica y objeto, declara la agregacion
  aplicada en cada fila y conserva el alcance completo del marco.
- [`indice_calidad()`](https://sebollin.github.io/lupa/reference/indice_calidad.md)
  no calcula nada sin pesos del usuario. Con una declaracion completa
  conserva cobertura, pesos por dimension, combinaciones internas,
  inversiones de defectos, exclusiones `no_aplica` y la advertencia de
  que los componentes provienen de universos distintos.
- [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md)
  mide por omision la propuesta en estado `lista`, declara que no fue
  confirmada, agrega las medidas y conserva el tablero. El detalle fila
  a fila solo se retiene con `conservar_detalle_medicion = TRUE`; la
  medicion automatica se desactiva con `medir_propuesta = FALSE`.

### Secuencias enteras densas y vocabularios breves

- El perfil de columna publica si los enteros observados cubren
  densamente su rango, junto con densidad, posiciones y huecos. En esa
  condicion los centinelas numericos y los desvios que solo expresan el
  largo de una corrida de digitos no interpretan el contenido del
  identificador; los ausentes, duplicados y restantes diagnosticos
  siguen activos. Una secuencia densa y unica se presenta como
  `posible_identificador` y no recomienda convertir el texto numerico a
  una medida cuantitativa.
- `casi_duplicados_vocabulario` cubre una sustitucion en valores de
  hasta seis caracteres cuando la variante ocupa como maximo `0.05` de
  la columna y la forma dominante es al menos `10` veces mas frecuente y
  ocupa al menos `0.5` de la columna. El limite y los tres umbrales
  quedan en la evidencia y se pueden ajustar en
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md).

### Orientacion explicita de las metricas

- [`metrica()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md)
  declara si un resultado expresa `"conformidad"`, `"defecto"` o
  `"no_aplica"`. La orientacion viaja por
  [`medir()`](https://sebollin.github.io/lupa/reference/medir.md),
  [`agregar()`](https://sebollin.github.io/lupa/reference/agregar.md),
  [`reportar()`](https://sebollin.github.io/lupa/reference/reportar.md)
  y [`evaluar()`](https://sebollin.github.io/lupa/reference/evaluar.md)
  sin invertir los valores; una regla puede recibirla como segundo
  argumento. El historico conserva el esquema 1 y sigue leyendo archivos
  anteriores.
- `Formato` queda alineada con el factor `Correctitud sintactica` de
  [`marco_agesic()`](https://sebollin.github.io/lupa/reference/marco_calidad.md),
  y una prueba contrasta todos los pares dimension-factor del nucleo
  contra el marco.

### Marco CEA/CEPAL de aseguramiento de la calidad

- [`marco_cepal()`](https://sebollin.github.io/lupa/reference/marco_calidad.md)
  incorpora los cuatro niveles y diecinueve principios del marco
  nacional de aseguramiento de la calidad de las Naciones Unidas,
  adoptado y adaptado para América Latina y el Caribe por la CEA/CEPAL.
  Los principios 1 a 13 quedan declarados fuera del alcance de una
  tabla; los principios 14 a 19 quedan disponibles para documentar
  productos estadísticos, sin afirmar que el profiling genérico los
  mida.

### Severidad del vocabulario y escala de las relaciones

- `casi_duplicados_vocabulario` queda como señal `sospechoso` sólo
  cuando encuentra grupos; un resultado negativo queda como `ok` con
  cero afectados, y un diagnóstico que no aplica se registra en
  `cobertura_diagnosticos`.
- `relacion_orden_columnas` separa la escala de la relación fila a fila
  con un solapamiento intercuartil mínimo de `0.1`. Una brecha con IQR
  cero conserva una relación estable aunque los rangos no se solapen;
  ambos criterios y los pares descartados o recuperados quedan en el
  alcance.

### Perfil de una muestra DBI con universo explícito

- Se agrega
  [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  para separar los agregados SQL exactos sobre una tabla completa del
  perfil de 99 campos calculado sobre una muestra declarada. La salida
  registra el motor informado por DBI, cada consulta, los agregados no
  disponibles y la reproducibilidad efectiva del orden, sin escribir en
  la base.

### Desenlaces declarados por reglas

- [`regla_evaluacion()`](https://sebollin.github.io/lupa/reference/reglas_evaluacion.md)
  acepta `desenlace = "suprimir"` para que una regla declarada por el
  usuario produzca un plan sobre las medidas que no cumplen su
  condición. La evaluación conserva objeto, valor medido, motivo y regla
  sin modificar la medición ni los datos de origen. Sin esa declaración
  no crea desenlaces ni aplica umbrales de publicación.
- [`reportar()`](https://sebollin.github.io/lupa/reference/reportar.md)
  enmascara los valores alcanzados por ese plan tanto en la evaluación
  como en las mediciones incluidas en el mismo reporte. El enmascarado
  se hace sobre copias usadas para renderizar.

### Ley de Benford con aplicabilidad explícita

- [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  evalúa la ley de Benford solamente en columnas numéricas con
  suficiente evidencia inicial. Antes de comparar exige variación,
  ausencia de apariencia de identificador (incluidas secuencias
  correlativas), al menos 100 valores positivos, todos los valores
  finitos positivos y tres órdenes de magnitud. Las precondiciones y sus
  umbrales quedan en `meta$benford`; las que fallan se declaran en
  `cobertura_diagnosticos` y no producen hallazgos.
- Cuando aplica, el perfil conserva la distribución observada y esperada
  por primer dígito, el chi-cuadrado de Pearson y su valor p. Una
  desviación se presenta como señal descriptiva para revisar, nunca como
  acusación de fraude o manipulación.

### URLs, unidades y celdas multivaluadas

- [`validar_url()`](https://sebollin.github.io/lupa/reference/validadores_formato.md)
  valida de forma vectorizada URLs `http` y `https`, con esquema
  obligatorio por omisión, soporte para IDN y puertos, y rechazo
  deliberado de `javascript:`, `data:`, espacios y controles literales.
- [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  informa `unidades_mixtas` cuando una columna numérica escrita como
  texto combina sufijos de unidad, conservando sus frecuencias y sin
  convertir datos. Reconoce además monedas como prefijos o sufijos y
  emite `monedas_mixtas` con sus frecuencias, sin convertir ni suponer
  tasas de cambio. También informa `celdas_multivaluadas` sólo cuando
  las partes homogéneas pasan el control de patrones y tipo, incluidos
  identificadores numéricos con puntuación interna; nombres y
  direcciones con comas no se presentan como listas.

### Relaciones aritméticas entre columnas

- Reconoce una regularidad mediante un único soporte declarado
  (`umbral_aritmetica = 0.9`) dentro de la tolerancia y, una vez
  reconocida, informa todas sus discrepancias sin aplicar un segundo
  filtro por su cantidad absoluta: `max_violaciones_aritmetica` se
  elimina. El soporte, el universo mínimo y la tolerancia quedan en la
  evidencia y el alcance.
- [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  descubre identidades aditivas y proporcionalidades estables entre
  columnas numéricas y las presenta como evidencia observada, no como
  reglas del dominio. Cada hallazgo declara proporción de cumplimiento,
  universo de filas finitas, tolerancia numérica, constante proporcional
  y filas discrepantes.
- `umbral_aritmetica`, `min_filas_aritmetica`, `tolerancia_aritmetica` y
  `max_columnas_aritmetica` hacen visibles los supuestos y el costo del
  diagnóstico. Si el límite de columnas recorta combinaciones,
  `cobertura_diagnosticos` lo declara explícitamente.

### Capa de marcos

- [`regla_evaluacion()`](https://sebollin.github.io/lupa/reference/reglas_evaluacion.md)
  acepta `proporcion_minima` para declarar un veredicto sobre la
  proporción de medidas que cumple la condición. El objeto conserva el
  umbral; la evaluación muestra proporción, veredicto, componentes y
  universo, sin ponderar medidas ni crear un puntaje global. Las reglas
  por medida conservan su contrato y su estructura de salida.
- [`metrica()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md)
  acepta las etiquetas relacionales de
  [`granularidades()`](https://sebollin.github.io/lupa/reference/granularidades.md)
  —por ejemplo, `"columna"`— y guarda siempre su equivalente canónico de
  la ontología (`"atributo"`). Un valor inválido muestra ambos
  vocabularios.
- El error de una regla que no engancha ninguna medida enumera lo
  solicitado y las métricas instanciadas disponibles, incluidos sus
  nombres calificados.

### Perfilado de geometrías

- Las columnas `sfc` informan CRS, tipo de geometría, geometrías vacías
  e inválidas, coordenadas fuera del dominio declarado y caja
  envolvente. Una geometría sin CRS deja el conteo de dominio en `NA`:
  no se supone EPSG:4326. Las geometrías vacías se cuentan aparte y no
  integran el universo del chequeo de dominio.
- Los tipos mixtos se comparan por familia: las variantes simples y
  `MULTI` compatibles conviven sin hallazgo, mientras que familias
  distintas y `GEOMETRYCOLLECTION` se señalan. La validez declara
  `validez_criterio = "planar"`; sobre CRS geográficos un fallo planar
  es sospechoso y no afirma invalidez esférica.
- `n_dominio_evaluados` y `n_bbox_evaluados` hacen públicos los
  universos no vacíos de sus métricas; `bbox_alcance` declara que la
  caja usa las coordenadas crudas, incluidas las que estén fuera de
  dominio. `n_validez_evaluados` publica por separado el universo de
  GEOS, incluidas las geometrías vacías. `dimension_geometria` declara
  `XY`, `XYZ`, `XYM` o `XYZM`; Z y M quedan enumeradas en
  `dimensiones_no_evaluadas` y generan una fila de cobertura. Para `XYM`
  y `XYZM`, la validez topológica se calcula en XY después de `st_zm()`
  y `validez_preprocesamiento` declara ese paso.
- El control de dominio compara también las coordenadas transformadas
  con la `BBOX` del área de uso del WKT. Detecta, entre otros casos,
  grados donde el CRS espera metros; no detecta una zona UTM equivocada
  cuando las coordenadas interpretadas caen dentro del área de esa zona.
  Una caja mundial es un no-op evaluado y un WKT sin `BBOX` produce una
  fila de cobertura, sin asumir alcance global.
- Los nuevos hallazgos distinguen CRS ausente, geometrías inválidas o
  vacías, coordenadas imposibles y tipos geométricos mixtos. Si falta el
  paquete opcional `sf`, el perfil no inventa ceros ni hallazgos:
  registra una fila con `dependencia = "sf"` en
  `cobertura_diagnosticos`.

### Fechas con meses escritos

- [`detectar_formatos_fecha()`](https://sebollin.github.io/lupa/reference/detectar_formatos_fecha.md)
  reconoce fechas con meses escritos en español (incluye `setiembre` y
  `set`) y en inglés, además de los formatos numéricos existentes. La
  tabla de nombres es propia y no depende de `LC_TIME`, y sólo acepta la
  estructura completa de una fecha o de un mes con año: encontrar
  `marzo` dentro de una oración no convierte el texto en fecha. Los
  meses escritos desambiguan el día y el mes; los años de dos dígitos
  siguen siendo candidatos y no se les asigna un siglo en silencio.
- Los períodos expresados sólo como mes y año declaran
  `granularidad = "mes"` y no inventan el día 1 para calcular mínimos,
  medias o conversiones; esos resúmenes quedan en `NA` con estado
  `granularidad_incompleta`. Los años escritos en meses también se
  limitan al rango 1800–2100, como las fechas compactas.
- La detección de meses sólo ejecuta sus expresiones regulares sobre los
  valores candidatos y reutiliza ese resultado al calcular el resumen de
  la columna. Así el texto libre que menciona meses no paga el costo
  completo ni se vuelve a analizar.
- Ese resultado intermedio se mantiene sólo durante el perfilado y no
  queda adjunto al objeto público `formatos_fecha`. En columnas mixtas,
  los resúmenes de fecha se calculan sobre las fechas completas y
  declaran cuántas fechas de mes-año quedaron fuera; una columna
  compuesta sólo por períodos conserva el estado
  `granularidad_incompleta`.
- [`inferir_tipo()`](https://sebollin.github.io/lupa/reference/inferir_tipo.md)
  tampoco conserva el caché interno de detección de meses. El
  diagnóstico de variantes del vocabulario sigue siendo una señal
  heurística: Jaro–Winkler puede acercar nombres de calles o códigos con
  prefijos compartidos y sus grupos deben revisarse como sospechosos, no
  como identidades.
- El hallazgo de variantes del vocabulario sólo atribuye el límite de
  proporción cuando existe un grupo compatible que retener; si todas las
  cercanías fueron descartadas por secuencias numéricas incompatibles,
  lo informa con ese motivo.

### Variantes del vocabulario

- [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  agrega el hallazgo `casi_duplicados_vocabulario`: agrupa, por columna,
  variantes que la normalización funde o que quedan bajo el umbral de
  Jaro–Winkler y conserva la frecuencia de cada forma. La unidad es el
  valor distinto, no la fila; no se elige una forma canónica ni se
  modifica el dato. Las aristas de distancia forman estrellas alrededor
  de un valor de frecuencia estrictamente mayor y único; los empates no
  se fuerzan y no se cierra transitivamente una cadena de vecinos. Cada
  grupo declara su distancia mínima y máxima, y
  `max_proporcion_grupo_vocabulario` permite declarar que el diagnóstico
  no aplica cuando un componente abarca demasiado vocabulario; el filtro
  se activa desde 20 valores distintos o cuando el grupo mayor tiene al
  menos 10 variantes, y sólo suprime si la proporción también supera el
  umbral. Así no oculta grupos pequeños, pero tampoco entrega una
  columna entera como una sola familia. Cuando hay pares cercanos pero
  no una frecuencia central única, el alcance declara la falta de
  asimetría y apunta a
  [`detectar_duplicados_aproximados()`](https://sebollin.github.io/lupa/reference/detectar_duplicados_aproximados.md).
  Las aristas de distancia con secuencias numéricas distintas se
  descartan; los ceros de relleno y separadores de miles se consideran
  equivalentes, pero una errata dentro de un número puede quedar sin
  agrupar deliberadamente. El alcance informa los pares descartados por
  números y separa el tamaño potencial del componente del tamaño que
  queda compatible con esa regla. `casi_duplicados_vocabulario = FALSE`
  lo desactiva. El alcance declara los valores y pares comparados, los
  recortes y la ausencia de
  [`stringdist`](https://cran.r-project.org/package=stringdist); las
  fusiones exactas se siguen informando sin ese paquete. Los resultados
  del perfil pueden cambiar porque ahora se señalan estas variantes como
  evidencia para una revisión de vocabulario.

### Referenciales

- Las métricas de referenciales heredan el perfil de `normalizar`
  declarado en
  [`referencial()`](https://sebollin.github.io/lupa/reference/referencial.md)
  (o aceptan uno explícito), por lo que variantes de caja, acentos y
  espacios pueden pasar a reconocerse como presentes. Esto cambia los
  resultados de correctitud y cobertura de forma deliberada; las claves
  siguen evaluándose por identidad exacta.
- `CorrectitudSemFuerte` y `CorrectitudSemDebil` pueden agregar, sin
  cambiar el veredicto, el candidato más cercano y su distancia como
  evidencia. La proximidad usa Jaro–Winkler por omisión (`p = 0.1`,
  umbral `0.10`), sólo se calcula para fallos y declara sus límites o la
  ausencia de
  [`stringdist`](https://cran.r-project.org/package=stringdist). Se
  calcula sobre los valores fallidos distintos y se reparte a las filas
  repetidas; el alcance distingue filas fallidas, valores distintos y
  valores comparados.

### Perfil de normalización para comparar

- `normalizar` deja de ser sólo un interruptor lógico: `TRUE` conserva
  el caso común con minúsculas, espacios, acentos protegidos y comillas;
  `FALSE` desactiva esos pasos configurables; `"amplio"`,
  \[normalizacion()\] y una lista nombrada permiten elegirlos por
  columna. La representación normalizada sólo decide qué valores se
  comparan: nunca modifica los datos guardados.
- [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  conserva el perfil resuelto y los análisis de duplicados y claves lo
  heredan cuando reciben `normalizar = NULL`. Los resultados pueden
  cambiar porque el umbral se aplica sobre la cadena normalizada; el
  perfil informa, por vocabulario, cuántos valores fundió cada paso.
- La comparación aplica siempre descomposición y orden canónicos en el
  subconjunto latino cubierto; no reordena palabras ni aplica
  abreviaturas de vías. Las claves siguen descubriéndose por identidad
  exacta y agregan la unicidad normalizada como métrica informativa.
- El informe de fusiones compara el perfil completo con una versión que
  apaga cada paso por separado: sus cifras no son aditivas y el total
  normalizado se informa aparte. Ahora usa el vocabulario completo (las
  fusiones son una propiedad de pares que una muestra de valores puede
  ocultar) y la normalización se aplica de forma vectorizada; `n_usados`
  y el estado `exacto` dejan explícito el alcance real.
- El informe de fusiones no se calcula cuando `normalizar = FALSE`,
  porque no hay pasos configurables que evaluar. Cuando
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  ya lo calculó, `detectar_duplicados_aproximados(perfil = ...)` lo
  reutiliza en lugar de recorrer de nuevo el vocabulario.
- `proteger` acepta grafemas compuestos y el valor predeterminado
  conserva `g̃` además de `ñ` y `ü`, para no borrar letras guaraníes al
  comparar.

### Diagnósticos de texto invisible

- Amplía la detección a los espacios Unicode, marcas direccionales, BOM
  y otros invisibles de transporte. Los espacios Unicode se pueden
  colapsar a espacio ASCII sólo mediante una acción explícita y
  destructiva; ZWJ/ZWNJ se informan pero se conservan. La comparación
  normalizada usa estas mismas clases sin borrar caracteres
  semánticamente significativos.
- El hallazgo de separadores en campo, su acción y su conteo usan
  nombres específicos para cubrir tabulaciones, saltos, avances de
  página y tabulaciones verticales.
- [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  identifica controles C0/C1 e invisibles Unicode, entidades HTML
  reconocibles y separadores dentro de campos. La evidencia escapa esos
  caracteres (`<U+200B>`, `\\t`, `\\n`, `\\r`, `\\f`, `\\v`) y conserva
  los conteos por fila.
- Los controles invisibles que no son separadores se pueden eliminar y
  se recomiendan por defecto; decodificar entidades HTML y reemplazar
  separadores de línea quedan como acciones explícitas porque pueden
  cambiar contenido legítimo. Las tres dejan el número de valores
  cambiados en el registro.

### Reparación de texto y licencia

- La medida predeterminada de duplicados ahora aplica Jaro–Winkler con
  `p = 0.1` (el valor anterior era Jaro puro por `p = 0`) y el umbral
  pasa de `0.12` a `0.10`. Las dos decisiones pueden cambiar los pares
  informados al actualizar; el cambio es deliberado y queda declarado en
  la ayuda.

- Declara `cli (>= 3.0.0)`. El motor usa la interfaz de barras de
  progreso (`cli_progress_bar()` y sus compañeras), que existe recién
  desde esa versión; antes el requisito estaba supuesto y no escrito.

- Clasifica los duplicados exactos comparando los textos que realmente
  entran a la medida, después de normalizarlos, y no mediante igualdad
  exacta de un flotante. Esto hace el resultado independiente de la
  arquitectura y mantiene como `aproximado` un par de textos distintos
  aunque `soundex` devuelva distancia cero.

- Cierra el motor de reparación de texto: `decode_inconsistent_utf8`
  trabaja por subcadenas con el detector de [ftfy
  6.3.1](https://github.com/rspeer/python-ftfy), conserva los estados
  parciales con U+FFFD y agrega tres extensiones deliberadas de badness
  sobre ftfy 6.3.1: la regla de inicio del issue
  [\#222](https://github.com/rspeer/python-ftfy/issues/222), también
  discutida en el [PR](https://github.com/rspeer/python-ftfy/pull/232)
  [\#232](https://github.com/sebollin/lupa/issues/232); la regla de caja
  que detecta mojibake de KOI8-R del issue
  [\#231](https://github.com/rspeer/python-ftfy/issues/231); y la regla
  específica para `â` del issue
  [\#233](https://github.com/rspeer/python-ftfy/issues/233). La tabla de
  bytes KOI8-R es la cuarta extensión y la puerta literal `Ã` para
  formas portuguesas y francesas es la quinta.

- Incorpora un motor R puro para detectar y reparar mojibake en varias
  codificaciones, inspirado en el diseño y las tablas de [ftfy
  6.3.1](https://github.com/rspeer/python-ftfy) de [Robyn
  Speer](https://github.com/rspeer). Los resultados distinguen
  reparaciones completas, parciales y casos irrecuperables; los estados
  llegan al hallazgo, al plan y al registro.

- Completa el port de las reglas de detección y de los
  transcodificadores de [ftfy](https://github.com/rspeer/python-ftfy):
  las transformaciones de bytes se encadenan antes de decodificar, las
  pérdidas quedan como U+FFFD y estado `reparado_parcialmente`, y nunca
  se introduce un control invisible nuevo.

- Completa `restore_byte_a0` de [ftfy
  6.3.1](https://github.com/rspeer/python-ftfy): conserva la frontera de
  la palabra `à`, respeta las excepciones portuguesas y cubre las seis
  formas de bytes alterados, sin partir ni pegar palabras.

- Conserva los espacios no separables y agrega el decodificador R puro
  de variantes UTF-8 de [ftfy](https://github.com/rspeer/python-ftfy):
  combina pares CESU-8 y reconoce `C0 80`, e incorpora la tabla de bytes
  KOI8-R adicional, con los estados y pérdidas ya declarados.

- Declara como quinta extensión deliberada la puerta adicional para la
  secuencia literal `Ã`, que conserva las formas portuguesas y francesas
  observadas en padrones; el decodificador de variantes rechaza
  secuencias que producirían un NUL, en vez de omitir un carácter al
  materializar el texto.

- La licencia del paquete pasa de `GPL-2 | GPL-3` a `GPL-3`; las partes
  derivadas del diseño de [ftfy](https://github.com/rspeer/python-ftfy)
  se atribuyen en `LICENSE.note` bajo Apache-2.0.

- La estrategia de reparación de texto se registra como
  `reparar_codificacion`.

### Recursos de comparación

- Fija por omisión en dos los hilos que
  [`stringdist`](https://cran.r-project.org/package=stringdist) puede
  usar en las comparaciones aproximadas y declara el valor efectivo en
  `alcance`.
- El aviso interactivo del camino LSH identifica `nucleos` como la
  perilla que puede acortar la etapa de comparación, sin prometer una
  ganancia fija.
- La viñeta de escala documenta el rendimiento observado entre dos y
  treinta y un hilos y deja explícito que después de dieciséis no hubo
  una mejora medida.
- Documenta que el piso de tiempo de LSH cubre sólo la comparación de
  cadenas, no la firma, las cubetas ni el troceo; los resultados no
  dependen de la cantidad de hilos.
- Actualiza las mediciones de escala para anotar la configuración de
  hilos y evita presentar tiempos dependientes de la máquina como cifras
  exactas.

### Marcos declarables y alcance internacional

- Incorpora validadores vectorizados de ISO 3166, ISO 4217, correo, Luhn
  y módulo 97, junto con un pack uruguayo de cédula y RUT. Los packs
  territoriales se pueden extender sin registrar estado global ni
  modificar el núcleo.
- Separa clasificar de proteger datos personales: las formas numéricas
  poco discriminantes se informan sin suprimir estadísticos, mientras
  nombres semánticos, correos y documentos verificados conservan la
  protección.
- Documenta los contratos de todos los puntos de extensión y añade
  [`propiedades_metrica()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md)
  para consultar la configuración admitida sin inspeccionar closures.
- Incorpora
  [`marco_iso25012()`](https://sebollin.github.io/lupa/reference/marco_calidad.md)
  como adaptación opcional y explícita de las quince características de
  ISO/IEC 25012:2008.
- Identifica el marco activo en cada fila de
  [`cobertura_analisis()`](https://sebollin.github.io/lupa/reference/cobertura_analisis.md).
- Permite declarar taxonomías dimensión-factor con
  [`marco_calidad()`](https://sebollin.github.io/lupa/reference/marco_calidad.md),
  validar modelos contra ellas y calcular cobertura con AGESIC sólo como
  valor de fábrica mediante
  [`marco_agesic()`](https://sebollin.github.io/lupa/reference/marco_calidad.md).
- Permite construir familias de madurez con nombres y umbrales propios
  sin cambiar los tres perfiles incluidos.
- Hace que el vector de sentinelas numéricos sea una política completa:
  [`numeric()`](https://rdrr.io/r/base/numeric.html) los desactiva
  explícitamente.
- Reconoce coma y punto decimal, separadores de miles simétricos,
  símbolos monetarios y prefijos con forma de código ISO 4217.
- Clasifica RUT, DNI y otros documentos con la etiqueta neutral
  `documento_identidad`.
- Permite conectar packs personales territoriales al mismo clasificador
  de
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
  con tolerancia explícita de errores de digitación; los nombres
  semánticos (`telefono`, `fecha_nacimiento`, entre otros) conservan
  prioridad sobre formas numéricas genéricas.

### Examinar datos

- Detecta relaciones de orden sospechosas entre columnas numéricas o
  temporales comparables (por ejemplo, `inicio <= fin` y
  `monto_bruto <= monto_neto`). El hallazgo conserva los conteos y las
  filas fuera de orden, sugiere formalizar la regla con
  `ReglaIntegridadIntraEntidad` y declara en `meta$orden_columnas` las
  columnas y pares efectivamente comparados. Expone un filtro opcional
  de solapamiento intercuartil para tablas anchas; está apagado por
  omisión (umbral `0`) porque activarlo puede ocultar relaciones reales
  entre magnitudes de rangos distintos. Los pares descartados quedan
  contados en el alcance.

- Protege los estadísticos de orden y cuantiles de columnas personales,
  marca cada supresión en el objeto y conserva alertas de plausibilidad
  para fechas de nacimiento sin publicar sus extremos.

- Añade `datos_operativos`, un segundo conjunto sintético y neutral,
  reproducible desde `data-raw/`, con problemas de calidad sembrados.

- Añade
  [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md)
  como puerta de entrada al recorrido descriptivo, con cobertura
  conceptual y advertencias de alcance en el propio objeto.

- Incorpora distribuciones de valores acotadas, cuantiles, asociaciones
  de Pearson, V de Cramér y eta cuadrado, además de regularidad,
  duplicación, monotonicidad, cobertura, días de semana y huecos
  temporales.

- Propone escalas de medición y roles sin confirmar lo que sólo se
  infiere de los valores; conserva niveles declarados, observados y
  ausentes.

- Perfila tablas administrativas con métricas generales y por columna,
  proporciones en `[0, 1]` y hallazgos filtrables.

- Descubre patrones de formato, tipos implícitos, formatos de fecha
  mixtos y ambiguos, años de dos dígitos, números regionales y problemas
  de codificación.

- Detecta claves candidatas, relaciones, cobertura referencial, columnas
  y filas duplicadas, y dependencias funcionales exactas o aproximadas.

- Conserva la ambigüedad día/mes con barra, guion y punto; reconoce
  fracciones de segundo y offsets ISO 8601.

- Distingue NaN e infinitos, evita aproximar `integer64` fuera del rango
  exacto de `double` y cuenta valores distintos en columnas de listas y
  geometrías.

- Clasifica posibles datos personales sin juzgar su presencia y protege
  por defecto los valores concretos cuando la evidencia es
  discriminante.

- Normaliza factores a texto sólo en la operación, conserva `factor` en
  el perfil y devuelve texto al transformar columnas factor con
  [`aplicar()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md).

- Mantiene claves históricas estables en R 3.6 y fija explícitamente en
  UTC las fechas convertidas desde `Date`.

- Las claves históricas tratan el texto ilegible (UTF-8 inválido) como
  ausente: comparte con `NA` la marca `~`, en vez de intentar
  codificarlo como texto literal.

- Añade conteos explícitos de evaluados y afectados, con la unidad de
  conteo, a cada hallazgo; conserva NA cuando el alcance no permite
  conocerlos.

- Añade trazabilidad acotada por hallazgo mediante índices de fila, con
  estados explícitos para lo disponible, truncado, no aplicable y no
  disponible; el reporte resume el estado sin imprimir los índices.

### Medir y evaluar calidad

- Declara métricas genéricas, específicas e instanciadas con tipo de
  resultado y granularidad explícitos.
- Incluye veintiuna métricas automatizables, tres métricas tabulares
  basadas en referenciales y una correspondencia verificable con las 49
  entradas del catálogo de AGESIC.
- Separa en el catálogo la disponibilidad de cada métrica de la causa o
  el matiz de esa disponibilidad, y documenta las 49 correspondencias
  sin vacíos.
- Ajusta las métricas oficiales de oportunidad al resultado booleano del
  marco y conserva la fórmula continua del curso CPAP bajo nombres
  `GradoOportunidad*`.
- Incorpora contratos explícitos
  [`vigencia()`](https://sebollin.github.io/lupa/reference/contratos_medicion.md)
  y
  [`escala()`](https://sebollin.github.io/lupa/reference/contratos_medicion.md),
  y una tabla de cobertura que distingue lo medido, no declarado, no
  aplicable y fuera de alcance.
- Implementa las cuatro agregaciones del marco y la cadena de evaluación
  de medidas, reglas y perfiles de madurez. No calcula un índice global.
- Propone modelos editables a partir del perfil sin convertir
  observaciones de una sola entrega en requisitos silenciosos.
- Estima el costo antes de comparar, aplica un presupuesto de pares en
  los caminos exhaustivo y LSH, y publica el alcance de la estimación.
- Incorpora MinHash y LSH deterministas para generar candidatos a
  escala, con deduplicación por banda, garantía declarada y degradación
  explícita.
- Permite bloquear por una columna elegida por el usuario y estima los
  pares que el bloqueo puede dejar fuera, incluidos los ausentes como
  bloque propio.

### Mejorar y monitorear

- Construye planes de limpieza editables con alternativas mutuamente
  excluyentes, justificación, modo guiado opcional y consentimiento
  adicional para eliminaciones.
- Aplica sólo acciones activas sobre una copia, conserva un registro y
  permite imputaciones confirmadas mediante dependencias funcionales
  exactas.
- Acumula evaluaciones en un histórico plano y versionado; detecta
  deriva del modelo y cambios estructurales entre perfiles.
- Procesa comparaciones exhaustivas por lotes con parciales en un
  directorio declarado, cruza los lotes sin pérdida de pares y deja
  constancia de que no son reanudables.

### Informar

- Guarda y recupera análisis versionados sin datos de entrada por
  omisión y sin serializar entornos completos de reglas funcionales.
- Genera un único HTML autocontenido, en español, sin navegador, LaTeX
  ni recursos externos; los valores se escapan y la evidencia personal
  se enmascara por defecto.
