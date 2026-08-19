# Cómo contribuir a lupa

Gracias por el interés. Este documento dice qué esperar y qué se espera,
para que una contribución no se pierda por un malentendido de forma.

## Antes que nada: el invariante

`lupa` **nunca informa como medido lo que no midió.** Un diagnóstico que
no pudo correr —falta un paquete opcional, no hay filas suficientes, la
comparación se truncó— va a la tabla `cobertura_diagnosticos`, separada
de `hallazgos` y fuera de la escala de severidad, y su campo de alcance
queda en `NA`, nunca en cero. Un diagnóstico que sí corrió y no encontró
nada sale en `ok` con cero afectados, nunca como sospecha.

De ahí sale casi todo lo demás, así que **una contribución que rompa
esto no se puede aceptar aunque el código esté bien escrito**. Si una
propuesta obliga a elegir entre callar y afirmar, la respuesta es una
tercera: declarar.

Los otros que rigen:

- La severidad es un factor ordenado `ok < sospechoso < error`.
- **No hay puntaje global propio.**
  [`indice_calidad()`](https://sebollin.github.io/lupa/reference/indice_calidad.md)
  devuelve un número sólo si quien lo pide declara los pesos, y viaja
  con su cobertura, los pesos usados y qué componentes se invirtieron
  por orientación.
- **El perfilado no toca los datos.** Ninguna función de análisis altera
  la tabla que recibe, ni siquiera un `data.table`, que R permite
  modificar por referencia. La capa de remediación produce un plan
  editable y devuelve una copia.
- **No se agrega ni se modifica un diagnóstico que sobre los datos
  objetivo se equivocaría más veces de las que acertaría.** Si no se
  puede medir cuál de los dos casos es más frecuente, el cambio es una
  apuesta y no entra.

## Idioma

La **API y la documentación son en español**: nombres de funciones,
argumentos, valores, mensajes, páginas de ayuda y viñetas.
`DESCRIPTION`, `NEWS.md` y `cran-comments.md` van **en inglés**, porque
los lee CRAN. Hay un `README.md` en inglés y un `README.es.md` en
español, y **los dos se mantienen a la par**: un cambio en uno sin el
otro queda a medias.

Dentro del código R, **lo no-ASCII va escapado con `\uXXXX`**. En
comentarios y en documentación va UTF-8 normal.

## Reportar un defecto

Lo más útil es un ejemplo mínimo reproducible: los datos más chicos que
muestren el problema, la llamada exacta, lo que devolvió y lo que
esperabas. Si el defecto es una afirmación falsa sobre los datos —que es
la clase de defecto que más importa acá— alcanza con mostrar el hallazgo
y por qué no es cierto de esa tabla.

## Enviar un cambio

Cuatro cosas acompañan a cada cambio, y si alguna no corresponde
conviene decir por qué:

1.  **Una prueba que falle si el cambio se revierte.** No alcanza con
    que pase: una aserción que pasaría igual sin el comportamiento que
    dice probar no prueba nada. La forma barata de comprobarlo es romper
    el comportamiento a propósito sobre una copia y ver que la prueba se
    entera.
2.  **La documentación `roxygen`** de lo que se tocó, regenerada con
    `roxygen2::roxygenise()`.
3.  **Los dos README**, cuando el cambio es visible desde afuera.
4.  **La viñeta** que enseña esa capacidad.

Más `NEWS.md` cuando el cambio se nota al usar el paquete.

## Dependencias

**Sólo `cli` es obligatoria.** Todo lo demás va en `Suggests`, y cada
uso tiene que tener su rama de ausencia declarada: si el paquete
opcional no está, el diagnóstico va a `cobertura_diagnosticos` con su
motivo y su `como_resolverlo`, no falla ni se saltea en silencio.

El mínimo de R declarado es 3.6.0. Conviene tenerlo presente: funciones
posteriores a esa versión no se pueden usar sin más.

## Antes de abrir el pull request

``` sh
Rscript -e 'roxygen2::roxygenise(".")'
NOT_CRAN=true Rscript -e 'testthat::test_dir("tests/testthat")'
Rscript -e 'lintr::lint_dir("R")'
R CMD build . && R CMD check --as-cran lupa_*.tar.gz
```

`lintr` está configurado en `.lintr` con los linters que el código ya
cumple, así que **tiene que dar cero avisos**. Los que quedan afuera
están anotados ahí con su motivo; si te parece que alguno debería
entrar, es una conversación aparte del cambio que estés enviando.

## Una convención chica que importa

**Nombrar es enlazar.** En cualquier lugar del repositorio donde se
nombre otro paquete o a una persona, va el enlace: al repositorio del
paquete o al perfil de quien lo hizo. Vale para el README, la
documentación `roxygen`, las viñetas, `NEWS.md` y `LICENSE.note`.
También para las referencias bibliográficas: si hay DOI o el texto es de
acceso libre, va.
