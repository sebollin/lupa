# El recorrido de proteccion bajaba hoja por hoja y llamaba al reemplazo con un
# vector de largo 1. El reemplazo recorre cada valor protegido con un `gsub`, asi
# que cuesta lo mismo sobre una cadena que sobre diez mil: pagarlo por hoja hacia
# que publicar 4.754 hallazgos costara 42.794 llamadas. Medido, la llamada que
# tardaba 158 s tarda 11 s y hace 2 llamadas en lugar de 43.173.
#
# Este archivo fija las DOS mitades: que la salida no cambie -contra una
# implementacion de referencia escrita aca, hoja por hoja- y que el numero de
# llamadas NO crezca con la cantidad de hojas, que es la propiedad que la hace
# rapida. No se mide tiempo: un umbral de segundos medido en una maquina no
# transfiere a otra.

# La referencia: el recorrido hoja por hoja, tal como estaba antes del arreglo.
.o63_referencia <- function(x, valores) {
  if (!length(valores)) return(x)
  atributos <- attributes(x)
  estructurales <- c("names", "class", "row.names", "dim", "dimnames")
  for (atributo in setdiff(names(atributos), estructurales)) {
    attr(x, atributo) <- .o63_referencia(attr(x, atributo, exact = TRUE), valores)
  }
  if (inherits(x, "data.frame")) {
    for (j in seq_along(x)) {
      columna <- x[[j]]
      if (is.character(columna)) {
        x[[j]] <- lupa:::.reemplazar_valores_protegidos(columna, valores)
      } else if (is.factor(columna)) {
        levels(columna) <- lupa:::.reemplazar_valores_protegidos(
          levels(columna), valores
        )
        x[[j]] <- columna
      } else if (is.list(columna)) {
        x[[j]] <- lapply(columna, .o63_referencia, valores = valores)
      }
    }
    return(x)
  }
  if (is.list(x)) {
    x[] <- lapply(x, .o63_referencia, valores = valores)
    return(x)
  }
  lupa:::.reemplazar_valores_protegidos(x, valores)
}

.o63_datos <- function() {
  data.frame(
    nombre = c("Ana Perez", "Ana Perez", "Luis Diaz", "Maria Nunez",
               "Maria Nunez de Castro", NA),
    documento = c("41234567", "41234567", "51234567", "61234567", "71234567", ""),
    monto = c(10, 10, 20, 30, 40, NA),
    stringsAsFactors = FALSE
  )
}

.o63_valores <- function(datos) {
  lupa:::.valores_identificantes(
    unlist(datos[c("nombre", "documento")], use.names = FALSE)
  )
}

test_that("agrupar las hojas no cambia la salida", {
  datos <- .o63_datos()
  valores <- .o63_valores(datos)
  expect_gt(length(valores), 0L)

  perfil <- perfilar(datos, analizar_dependencias = FALSE,
                     proteger_datos_personales = FALSE)
  expect_identical(
    lupa:::.proteger_textos_salida(perfil, valores),
    .o63_referencia(perfil, valores)
  )

  plan <- planificar_limpieza(perfil, datos = datos)
  expect_identical(
    lupa:::.proteger_textos_salida(plan, valores),
    .o63_referencia(plan, valores)
  )

  # Una columna-lista con una entrada por fila y varias hojas en cada entrada:
  # es la forma de `hallazgos$trazabilidad`, que era el 99% de las llamadas.
  con_lista <- data.frame(id = c("uno", "dos"), stringsAsFactors = FALSE)
  con_lista$trazabilidad <- I(list(
    list(estado = "disponible", quien = "Ana Perez", indices = 1:2),
    list(estado = "truncado", quien = "Maria Nunez de Castro", indices = 3L)
  ))
  expect_identical(
    lupa:::.proteger_textos_salida(con_lista, valores),
    .o63_referencia(con_lista, valores)
  )

  # Atributos anidados: las dos pasadas tienen que seguir protegiendolos.
  con_atributos <- list(a = "Ana Perez", b = 1:3)
  attr(con_atributos, "nota") <- "la titular es Ana Perez"
  attr(con_atributos$b, "otra") <- c(x = "Maria Nunez")
  expect_identical(
    lupa:::.proteger_textos_salida(con_atributos, valores),
    .o63_referencia(con_atributos, valores)
  )
})

test_that("juntar las hojas no mezcla codificaciones", {
  # La trampa de la alternancia, una capa mas arriba: agrupar las hojas las
  # concatena, y si la concatenacion tradujera lo marcado `latin1` a UTF-8, el
  # reemplazo por lotes operaria sobre otros bytes que el reemplazo hoja por hoja
  # -y dejaria de enmascarar, que es como se vio en el intento por alternancia-.
  # Medido, `unlist()` conserva la codificacion de cada elemento; esta prueba lo
  # fija para que un cambio futuro no lo rompa en silencio.
  #
  # Los fixtures se construyen con `rawToChar` e `intToUtf8` para que el archivo
  # quede en ASCII.
  latin1 <- rawToChar(as.raw(c(0x4a, 0x6f, 0x73, 0xe9, 0x20, 0x50, 0x65,
                               0x72, 0x65, 0x7a)))
  Encoding(latin1) <- "latin1"
  utf8 <- intToUtf8(c(0x4d, 0x61, 0x72, 0xed, 0x61, 0x20, 0x4e, 0x75, 0xf1,
                      0x65, 0x7a))
  invalido <- rawToChar(as.raw(c(0x4a, 0x75, 0x61, 0x6e, 0x20, 0xff, 0x65,
                                 0x72, 0x65, 0x7a)))
  objeto <- list(a = latin1, b = utf8, c = invalido, d = "Ana Perez", e = 1:3)
  valores <- c(latin1, utf8, invalido, "Ana Perez")

  # Que el fixture sea el que se cree: tres codificaciones distintas.
  expect_setequal(Encoding(c(latin1, utf8, invalido)),
                  c("latin1", "UTF-8", "unknown"))

  expect_identical(
    lupa:::.proteger_textos_salida(objeto, valores),
    .o63_referencia(objeto, valores)
  )
  protegido <- lupa:::.proteger_textos_salida(objeto, valores)
  expect_true(all(unlist(protegido[c("a", "b", "c", "d")]) ==
                    "[valor protegido]"))
  expect_identical(protegido$e, 1:3)
})

test_that("las llamadas al reemplazo no crecen con la cantidad de hojas", {
  valores <- .o63_valores(.o63_datos())
  # Se cuenta con `local_mocked_bindings()`, que es lo que ya usa el resto de la
  # suite. Con `trace()` no alcanza: el `tracer` se evalua en el marco de la
  # funcion trazada, y desde ahi la cadena de entornos no pasa por el de la
  # prueba, asi que `cuenta` no se encuentra. Un mock es un cierre y si la captura.
  original <- lupa:::.reemplazar_valores_protegidos
  contar <- function(objeto) {
    cuenta <- 0L
    local_mocked_bindings(
      .reemplazar_valores_protegidos = function(x, valores) {
        cuenta <<- cuenta + 1L
        original(x, valores)
      },
      .package = "lupa"
    )
    invisible(lupa:::.proteger_textos_salida(objeto, valores))
    cuenta
  }
  armar <- function(n) {
    tabla <- data.frame(id = paste0("fila", seq_len(n)), stringsAsFactors = FALSE)
    tabla$trazabilidad <- I(lapply(seq_len(n), function(i) {
      list(estado = "disponible", quien = "Ana Perez", indices = i)
    }))
    tabla
  }
  chico <- contar(armar(10L))
  grande <- contar(armar(400L))
  # Cuarenta veces mas hojas, la misma cantidad de llamadas. Antes eran 3 por
  # entrada: 30 contra 1.200.
  expect_equal(chico, grande)
  expect_lte(grande, 4L)
  # Y que el contador MIDA: si el trace no funcionara, las dos cuentas serian 0 y
  # la igualdad de arriba pasaria sin haber medido nada.
  expect_gt(chico, 0L)
})

test_that("una salida grande de duplicados se protege con pocas llamadas", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(
    nombre = paste0("persona", seq_len(60L)),
    domicilio = paste0("calle", seq_len(60L)), stringsAsFactors = FALSE
  )
  datos$nombre[59:60] <- c("Juan Perez", "Juan Peres")
  datos$domicilio[59:60] <- "Calle Centro"
  cuenta <- 0L
  original <- lupa:::.reemplazar_valores_protegidos
  local_mocked_bindings(
    .reemplazar_valores_protegidos = function(x, valores) {
      cuenta <<- cuenta + 1L
      original(x, valores)
    },
    .package = "lupa"
  )
  resultado <- detectar_duplicados_aproximados(
    datos, muestra = 60L, max_pares = 20000L, max_resultados = Inf
  )
  expect_gt(nrow(resultado$hallazgos), 100L)
  # Con el recorrido hoja por hoja esto eran miles de llamadas; ahora es una por
  # objeto protegido y no depende de cuantos hallazgos haya.
  expect_lte(cuenta, 10L)
  expect_gt(cuenta, 0L)
})
