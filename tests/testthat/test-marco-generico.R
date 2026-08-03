test_that("un marco ajeno a AGESIC recorre medición evaluación y cobertura", {
  marco <- marco_calidad("Marco de procedencia", list(
    Trazabilidad = c("Origen documentado", "Linaje reproducible"),
    Pertinencia = "Adecuación territorial"
  ))
  metodo <- function(tablas, instancia) {
    x <- tablas[[instancia$entidad]][[instancia$atributos]]
    data.frame(
      resultado = !is.na(x) & nzchar(x),
      entidad = instancia$entidad,
      atributo = instancia$atributos,
      fila = seq_along(x),
      objeto = paste0("entrega$origen[", seq_along(x), "]"),
      stringsAsFactors = FALSE
    )
  }
  origen_declarado <- metrica(
    "OrigenDeclarado", "Indica si cada registro declara su procedencia.",
    "instanciaAtributo", "booleano",
    dimension = "Trazabilidad", factor = "Origen documentado",
    metodo = metodo
  )
  instancia <- origen_declarado()(
    entidad = "entrega", atributos = "origen"
  )
  expect_error(
    origen_declarado(entidad = "entrega", atributos = "origen"),
    "especializarse primero"
  )
  modelo_propio <- modelo(instancia, marco = marco)
  datos <- data.frame(origen = c("sistema_a", "", "sistema_b"))
  medidas <- medir(modelo_propio, datos, id_medicion = "externa")
  atributo <- agregar(medidas, "atributo", "ratio")
  familia <- perfiles_madurez(
    atributo$metrica_instanciada,
    c(Inicial = 0.4, Consolidado = 0.8)
  )
  evaluaciones <- vapply(
    familia,
    function(x) evaluar(atributo, x)$perfiles$resultado,
    numeric(1L)
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  cobertura <- cobertura_analisis(perfil, medidas, modelo = marco)

  expect_s3_class(marco, "marco_calidad")
  expect_equal(nrow(as.data.frame(marco)), 3L)
  expect_s3_class(modelo_propio, "modelo_calidad")
  expect_equal(atributo$resultado, 2 / 3)
  expect_equal(evaluaciones, c(Inicial = 1, Consolidado = 0))
  expect_setequal(cobertura$dimension, c("Trazabilidad", "Pertinencia"))
  expect_equal(
    as.character(cobertura$estado[cobertura$factor == "Origen documentado"]),
    "medida"
  )
  expect_true(all(
    as.character(cobertura$estado[cobertura$factor != "Origen documentado"]) ==
      "no_declarada"
  ))
})

test_that("los marcos y familias rechazan declaraciones inconsistentes", {
  expect_error(marco_calidad("", list(A = "B")), "nombre")
  expect_error(marco_calidad("x", list()), "nombres")
  expect_error(
    marco_calidad("x", data.frame(dimension = "A")),
    "dimension.*factor"
  )
  expect_error(
    marco_calidad("x", data.frame(
      dimension = c("A", "A"), factor = c("B", "B")
    )),
    "único"
  )
  base <- data.frame(dimension = "A", factor = "B")
  expect_error(
    marco_calidad("x", transform(base, dimension = "")),
    "no pueden"
  )
  expect_error(
    marco_calidad("x", transform(base, como_resolverlo = "")),
    "como_resolverlo"
  )
  expect_error(
    marco_calidad("x", transform(base, perfil_mide = NA)),
    "perfil_mide"
  )
  expect_error(
    marco_calidad("x", transform(base, aplicabilidad = "ocasional")),
    "aplicabilidad"
  )
  expect_error(
    marco_calidad("x", transform(base, disponibilidad = "pendiente")),
    "disponibilidad"
  )
  expect_error(
    marco_calidad("x", transform(
      base, perfil_mide = TRUE, disponibilidad = "fuera_de_alcance"
    )),
    "fuera de alcance"
  )
  expect_s3_class(marco_agesic(), "marco_calidad")
  marco_minimo <- marco_calidad("Marco mínimo", base)
  salida <- capture.output(print(marco_minimo), type = "message")
  expect_true(any(grepl("Marco mínimo", salida, fixed = TRUE)))

  generica <- metrica(
    "M", "Mide algo", "instanciaAtributo", "booleano",
    dimension = "Otra", factor = "No declarado",
    metodo = function(tablas, instancia) data.frame(
      resultado = 1, entidad = instancia$entidad,
      atributo = instancia$atributos, fila = 1L, objeto = "t$x[1]"
    )
  )
  instancia <- generica()("t", "x")
  expect_error(generica(otra = 1), "no declarados")
  expect_error(
    modelo(instancia, marco = marco_calidad("x", list(A = "B"))),
    "no declara"
  )
  expect_error(modelo(instancia, marco = list()), "marco_calidad")
  expect_error(perfiles_madurez(umbrales = c(0.5, 0.8)), "nombres")
  expect_error(
    perfiles_madurez(umbrales = c(Alto = 0.8, Bajo = 0.5)),
    "crecientes"
  )
  expect_error(
    cobertura_analisis(
      perfilar(data.frame(x = 1), analizar_dependencias = FALSE),
      modelo = list()
    ),
    "marco_calidad"
  )
  expect_error(
    analizar(data.frame(x = 1), marco = list(), analizar_dependencias = FALSE),
    "marco_calidad"
  )
})

test_that("analizar respeta el marco asociado o declarado", {
  marco <- marco_calidad("Marco operativo", list(Operacion = "Presencia"))
  generica <- metrica(
    "Presente", "Comprueba presencia.", "instanciaAtributo", "booleano",
    dimension = "Operacion", factor = "Presencia",
    metodo = function(tablas, instancia) {
      x <- tablas[[instancia$entidad]][[instancia$atributos]]
      data.frame(
        resultado = !is.na(x), entidad = instancia$entidad,
        atributo = instancia$atributos, fila = seq_along(x),
        objeto = paste0("datos$x[", seq_along(x), "]")
      )
    }
  )
  confirmado <- modelo(generica()("datos", "x"), marco = marco)
  argumentos <- list(
    datos = data.frame(x = c(1, NA)), modelo_confirmado = confirmado,
    analizar_dependencias = FALSE, max_valores = 2,
    max_columnas_asociacion = 2, max_pares_asociacion = 1,
    max_columnas_temporales = 1
  )
  por_modelo <- do.call(analizar, argumentos)
  explicito <- do.call(analizar, c(argumentos, list(marco = marco)))

  expect_equal(por_modelo$meta$marco_calidad, "Marco operativo")
  expect_equal(explicito$meta$marco_calidad, "Marco operativo")
  expect_true(any(as.character(explicito$cobertura$estado) == "medida"))
})
