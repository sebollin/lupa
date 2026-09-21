# Las tres correcciones de la vuelta n88, todas sobre arreglos del mismo dia.
#
# La leccion comun: dos de las tres eran arreglos MIOS que cerraban el caso que
# nombraban y abrian otro al lado.

# --- H6: el parseo de dependencias del workflow de macOS --------------------
#
# El fragmento vive en `.github/workflows/macos.yaml`, que `.Rbuildignore`
# excluye: bajo `R CMD check` no esta. La prueba lo SALTEA ahi y corre donde el
# archivo se ve, que es donde se edita.
#
# Se EJECUTA el fragmento, no se busca como texto. Dos intentos anteriores
# fallaron y un `grep` habria aprobado a los dos:
#
#   sub("[[:space:]]*\\(.*", "")  partia por comas primero: `foo (>= 1.0, < 2.0)`
#       -DCF legal- daba "foo (>= 1.0" y "< 2.0)".
#   gsub("\\([^)]*\\)", "")       borraba parentesis primero, pero `[^)]*` no
#       cuenta anidamiento: arreglaba ese caso y ROMPIA dos que ya salian bien.

# Busca la RAIZ DE LAS FUENTES subiendo hasta encontrar el `DESCRIPTION` del
# paquete. Si no aparece, estamos contra el paquete instalado -bajo `R CMD
# check` el `.github/` no viaja- y la prueba se saltea con razon.
#
# Si SI aparece, el workflow tiene que estar: no encontrarlo es un fallo y no un
# salteo. La version anterior de esto usaba rutas relativas a mano y despues
# `test_path()`, y las dos SALTEABAN en silencio segun desde donde se corriera.
# El control rojo lo destapo: revirtiendo el arreglo del workflow la guarda
# seguia verde. Una guarda que saltea no se distingue de una que no mide.
.n88_raiz_fuentes <- function() {
  candidatos <- c(".", "..", "../..", "../../..", "../../../..")
  for (base in candidatos) {
    desc <- file.path(base, "DESCRIPTION")
    if (file.exists(desc)) {
      campos <- tryCatch(read.dcf(desc), error = function(e) NULL)
      if (!is.null(campos) && "Package" %in% colnames(campos) &&
          identical(unname(campos[1L, "Package"]), "lupa")) {
        return(base)
      }
    }
  }
  NULL
}

.n88_fragmento_workflow <- function() {
  raiz <- .n88_raiz_fuentes()
  if (is.null(raiz)) return(NULL)
  ruta <- file.path(raiz, ".github", "workflows", "macos.yaml")
  # Aca ya no se saltea: las fuentes estan a la vista.
  expect_true(
    file.exists(ruta),
    info = paste("no esta el workflow que esta prueba mide:", ruta)
  )
  lineas <- readLines(ruta, warn = FALSE)
  desde <- grep("^\\s*nombres_dependencias <- function\\(texto\\) \\{", lineas)
  expect_gt(length(desde), 0L)
  desde <- desde[[1L]]
  sangria <- sub("[^ ].*$", "", lineas[[desde]])
  cierres <- grep(paste0("^", sangria, "\\}\\s*$"), lineas)
  hasta <- cierres[cierres > desde]
  expect_gt(length(hasta), 0L)
  texto <- sub(paste0("^", sangria), "", lineas[desde:hasta[[1L]]])
  entorno <- new.env(parent = baseenv())
  eval(parse(text = paste(texto, collapse = "\n")), envir = entorno)
  get("nombres_dependencias", envir = entorno)
}

test_that("el parseo de dependencias sobrevive a los parentesis", {
  parsear <- .n88_fragmento_workflow()
  skip_if(is.null(parsear), "el workflow no esta a la vista")
  limpiar <- function(x) setdiff(x, c("R", "", NA))

  # El caso que el primer arreglo cerro.
  expect_identical(limpiar(parsear("foo (>= 1.0, < 2.0), bar")), c("foo", "bar"))
  # Los dos que el segundo arreglo ROMPIO, y que antes salian bien.
  expect_identical(
    limpiar(parsear("foo (>= 1.0 and < 2.0 (inclusive)), bar")), c("foo", "bar")
  )
  expect_identical(
    limpiar(parsear("foo (>= 1.0 (revisar), < 2.0), bar")), c("foo", "bar")
  )
  # Y los corrientes.
  expect_identical(limpiar(parsear("cli (>= 3.0.0), data.table")),
                   c("cli", "data.table"))
  expect_identical(limpiar(parsear("cli (>=\n3.4.0),\nrlang")), c("cli", "rlang"))
  expect_identical(limpiar(parsear("data.table, R.utils (>= 2.0)")),
                   c("data.table", "R.utils"))
  expect_identical(limpiar(parsear("R (>= 4.1.0), stats")), "stats")
  # Un parentesis SIN CERRAR no se arregla en silencio: aborta nombrando el
  # campo. Antes se tragaba lo que quedaba, y como los campos se pegaban con
  # comas antes de parsear, se llevaba el `Suggests` entero -medido: cinco
  # paquetes desaparecian-. Con `_R_CHECK_FORCE_SUGGESTS_: false` el check
  # pasaba igual y la fila publicada decia que macOS estaba en verde midiendo
  # mucho menos de lo que afirmaba. Una corrida verde que no midio lo que dice
  # es peor que una roja.
  expect_error(parsear("uno (>= 1.0, dos, tres"), "sin cerrar")
  expect_error(parsear("foo (>= 1.0, bar"), "sin cerrar")
  # Y sus dos espejos, que la primera version de esta guarda no cubria: un
  # nombre ENVUELTO en parentesis desaparecia de la lista de instalacion sin
  # error -`(foo), bar` daba solo `bar`- y un cierre suelto se tragaba. La
  # malformacion no tiene una sola forma, y la regla no depende de cual sea.
  expect_error(parsear("(foo), bar"), "sin nombre")
  # Y el caso que APAGABA la guarda: una coma legal dentro de un parentesis
  # desalineaba la cuenta contra la que se comparaba, asi que la comprobacion
  # se saltaba sola justo con la entrada complicada. `bar` desaparecia de la
  # lista de instalacion sin error y sin aviso.
  expect_error(parsear("foo (>= 1.0, < 2.0), (bar), baz"), "sin nombre")
  expect_error(parsear("(a), (b), c"), "sin nombre")
  expect_error(parsear("foo), bar"), "sin apertura")

  # Ninguna salida puede traer un parentesis: eso seria un nombre inventado.
  for (caso in c("foo (>= 1.0, < 2.0), bar", "foo (>= 1.0 (x), < 2.0), bar",
                 "foo (>= 1.0 and < 2.0 (y)), bar")) {
    expect_false(
      any(grepl("[()]", parsear(caso))),
      info = paste("fabrico un nombre con parentesis sobre:", caso)
    )
  }
})

# --- H5: la lista de compresiones depende de la version de R ---------------

test_that("`comprimir` admite lo que admite ESTA version de R", {
  analisis <- suppressWarnings(analizar(
    data.frame(x = c(1, 2, NA, 4)), fecha = as.POSIXct("2026-09-17", tz = "UTC")
  ))
  archivo <- tempfile(fileext = ".rds")
  on.exit(unlink(archivo), add = TRUE)

  # `saveRDS()` documenta "zstd" desde R 4.5, y el paquete declara R >= 4.1.0.
  # Con una lista fija pasaba una de dos, segun donde se corriera: o se rechaza
  # un metodo que R admite, o se admite uno que R no conoce y el error crudo
  # vuelve a llegar al usuario.
  if (getRversion() >= "4.5.0") {
    # Puede fallar por falta de soporte en el binario, y ese mensaje es de R y
    # dice que hacer. Lo que NO puede es venir de la validacion del paquete.
    resultado <- tryCatch(
      {
        guardar_analisis(analisis, archivo, sobrescribir = TRUE,
                         comprimir = "zstd")
        NULL
      },
      error = function(e) conditionMessage(e)
    )
    if (!is.null(resultado)) {
      expect_false(
        grepl("`comprimir`", resultado, fixed = TRUE),
        info = paste("la validacion rechazo un metodo que R documenta:", resultado)
      )
    }
  } else {
    expect_error(
      guardar_analisis(analisis, archivo, sobrescribir = TRUE,
                       comprimir = "zstd"),
      "`comprimir`"
    )
  }
})

# --- H7: el ejemplo que la documentacion publica ---------------------------

test_that("el ejemplo de la moda que la documentacion afirma sigue siendo cierto", {
  # `?comparar_perfiles` dice que un cambio de texto que no altera ninguna
  # propiedad medida no aparece, y da este caso. Una cifra publicada sin
  # reproductor envejece en silencio; este es su reproductor.
  hacer <- function(valor) suppressWarnings(perfilar(
    data.frame(x = c(rep(valor, 5L), "otro"), stringsAsFactors = FALSE),
    fecha = as.POSIXct("2026-09-17", tz = "UTC"),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  ))
  anterior <- hacer("Basico")
  actual <- hacer("Casico")
  expect_identical(anterior$columnas$moda[[1L]], "Basico")
  expect_identical(actual$columnas$moda[[1L]], "Casico")
  deriva <- suppressWarnings(as.data.frame(comparar_perfiles(anterior, actual)))
  expect_identical(nrow(deriva), 0L)
})
