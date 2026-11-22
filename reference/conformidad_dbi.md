# Comprobar la conformidad entre R y un motor DBI

Ejecuta, regla por regla, una batería de valores límite inventados en
consultas `SELECT` y compara lo que cuenta el motor con lo que cuenta R
con la misma regla. No crea tablas ni lee filas del usuario: de cada
tabla del modelo lee sólo el esquema del catálogo, para escribir los
valores inventados con el tipo y la intercalación de cada columna real.
Si la tabla no existe, usa los tipos que se desprenden de los valores
declarados en la regla.

## Usage

``` r
conformidad_dbi(
  conexion,
  modelo,
  tablas = NULL,
  aplicabilidad = NULL,
  max_valores_in = 500L,
  dialecto = "auto"
)
```

## Arguments

- conexion:

  Conexión DBI activa que se quiere caracterizar, o una colección, igual
  que en
  [`medir_dbi()`](https://sebollin.github.io/lupa/reference/medir_dbi.md).

- modelo:

  Modelo creado por
  [`modelo()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md)
  con las reglas que se desean comprobar.

- tablas:

  Tabla física de cada entidad, igual que en
  [`medir_dbi()`](https://sebollin.github.io/lupa/reference/medir_dbi.md).
  Se usa sólo para leer el esquema de las columnas.

- aplicabilidad:

  Lista de fórmulas de aplicabilidad, igual que en
  [`medir_dbi()`](https://sebollin.github.io/lupa/reference/medir_dbi.md).

- max_valores_in:

  Tope de valores en listas `IN (...)`.

- dialecto:

  Dialecto SQL: `"auto"`, `"sqlite"`, `"duckdb"`, `"postgresql"`,
  `"mysql"`, `"mariadb"` o `"sqlserver"`.

## Value

Un `data.frame` de clase `conformidad_dbi`, con una fila por regla y
caso. La columna `conformidad` dice si el motor cuenta igual que R, no
si la regla se puede medir:

- `conforme`: con estos valores el motor y R dan el mismo numerador y el
  mismo denominador. No asegura que
  [`medir_dbi()`](https://sebollin.github.io/lupa/reference/medir_dbi.md)
  mida la regla: con los datos reales puede abstenerse por su esquema o
  por una guarda.

- `no_conforme`: el motor cuenta y da distinto que R. Es lo único que
  hace que `medir_dbi(conformidad =)` deje de contar la regla en el
  motor: la mide por bloques en R con `estrategia_no_sql = "bloques"`, o
  la declara `no_medible`. El límite es conservador: una diferencia que
  sólo aparece con un valor inventado que no es `NULL` (un entero cerca
  de 2^53, un texto con espacios) quita la regla del motor aunque la
  tabla real no tenga ese valor.

- `abstencion`:
  [`medir_dbi()`](https://sebollin.github.io/lupa/reference/medir_dbi.md)
  no publicaría cifra con estos valores, por su traductor o por una
  guarda; `motivo` dice cuál. No quita la regla.

- `no_evaluada`: la batería no pudo construir o contar el caso; la regla
  se mide como siempre, con un aviso.

- `no_verificado`: el motor no está verificado en la batería
  (PostgreSQL, MySQL, MariaDB y SQL Server); la regla se mide como
  siempre, con un aviso.

`condicion` es `"con_nulos"` en un caso que con los `NULL` inventados no
conformó o se abstuvo: el mismo caso sin `NULL` va en la fila
`*_sin_nulos`, y
[`medir_dbi()`](https://sebollin.github.io/lupa/reference/medir_dbi.md)
aplica una no conformidad con `NULL` sólo si la tabla real tiene `NULL`
en las columnas de la regla. Una constante de texto de la regla entra
también con otra caja y con espacios, en un caso sólo ASCII
(`limites_texto`) y en otro con acentos (`limites_texto_no_ascii`).
`esquema` dice de dónde salieron los tipos de las columnas: `"tabla"`
(el catálogo), `"supuesto"` (los valores de la regla) o `"parcial"`. Los
atributos `identidad`, `configuracion` y `reglas` son huellas de la
conexión, de la configuración y de la definición de cada regla, sin
datos personales;
[`medir_dbi()`](https://sebollin.github.io/lupa/reference/medir_dbi.md)
los usa para no aplicar una batería ajena. El atributo `intercalacion`
es la que el motor usa por omisión (`NA` con su motivo en
`intercalacion_motivo` si no se pudo leer).

## Details

La batería responde una sola pregunta: si el motor, cuando cuenta, da lo
mismo que R. No conformar es eso y nada más. Que
[`medir_dbi()`](https://sebollin.github.io/lupa/reference/medir_dbi.md)
se abstenga por una guarda propia con los valores inventados (texto no
ASCII en SQLite, una regla que da `NULL`, un cotejamiento que no es
binario) queda registrado como `abstencion`: con los datos reales decide
la misma guarda, como siempre.

## See also

[`medir_dbi()`](https://sebollin.github.io/lupa/reference/medir_dbi.md),
[`plan_medicion_dbi()`](https://sebollin.github.io/lupa/reference/plan_medicion_dbi.md)
