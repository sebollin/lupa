# El lexico del propio paquete: las palabras y las marcas que escribe `lupa` en
# sus textos, sacadas de las cadenas de su codigo. Este archivo se ordena ultimo a
# proposito: cuando R lo carga al instalar, todas las funciones del paquete ya
# estan en el espacio de nombres, y el lexico se calcula una vez y queda guardado
# como constante. No hay una lista escrita a mano que se desactualice: sale del
# mismo codigo que escribe los textos.
#
# Para que sirve. La proteccion de datos personales busca cada valor protegido en
# toda la salida, y con columnas de nombre y de apellido separadas -lo habitual en
# un registro de personas- los valores protegidos son palabras sueltas. Varias son
# tambien palabras del paquete: `Blanco` tapaba toda evidencia que llevara la
# marca `<blanco>`, `Maximo` toda la que llevara la clave `grupo_maximo`, y
# `Patron` o `Constante` descripciones enteras. Medido en una refutacion, con
# nombres y apellidos reales; ninguna de esas celdas tenia un valor de nadie.
#
#   * `palabras`: las palabras de seis letras o mas, plegadas como las compara la
#     proteccion. En la prosa del paquete, fuera de lo citado entre comillas,
#     una de esas palabras no se tapa aunque coincida con un valor protegido de
#     una sola palabra: es el paquete el que la escribio.
#   * `marcas`: los rotulos entre angulos (`<blanco>`) y los identificadores con
#     guion bajo (`grupo_maximo`). No se comparan en ningun campo: son
#     estructura, como los nombres de columna.
.LEXICO_PAQUETE <- local({
  espacio <- topenv(environment())
  textos <- character()
  vacio <- function(x) is.symbol(x) && !nzchar(as.character(x))
  juntar <- function(e) {
    if (is.character(e)) {
      textos <<- c(textos, e)
      return(invisible(NULL))
    }
    if (is.function(e)) {
      cuerpo <- body(e)
      if (!is.null(cuerpo)) juntar(cuerpo)
      argumentos <- formals(e)
      for (i in seq_along(argumentos)) {
        if (!vacio(argumentos[[i]])) juntar(argumentos[[i]])
      }
      return(invisible(NULL))
    }
    if (is.call(e) || is.pairlist(e) || is.expression(e) || is.list(e)) {
      partes <- as.list(e)
      for (i in seq_along(partes)) {
        if (!vacio(partes[[i]])) juntar(partes[[i]])
      }
    }
    invisible(NULL)
  }
  for (nombre in sort(ls(espacio, all.names = TRUE))) {
    if (nombre %in% c(".PLIEGUE_COMPARAR_UNO", ".PLIEGUE_COMPARAR_VARIOS")) next
    objeto <- get(nombre, envir = espacio)
    if (is.function(objeto) || is.character(objeto)) juntar(objeto)
  }
  textos <- unique(textos[!is.na(textos) & nzchar(textos)])
  plegados <- .plegar_para_comparar(textos)
  palabras <- unlist(
    regmatches(plegados, gregexpr("\\p{L}+", plegados, perl = TRUE)),
    use.names = FALSE
  )
  palabras <- sort(unique(palabras[
    nchar(palabras) >= .MIN_LARGO_VALOR_IDENTIFICANTE
  ]))
  marcas <- unlist(regmatches(
    textos, gregexpr("<[a-z_]+>|[a-z0-9]+(_[a-z0-9]+)+", textos, perl = TRUE)
  ), use.names = FALSE)
  list(palabras = palabras, marcas = sort(unique(marcas)))
})
