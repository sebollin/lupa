# Verifica una conexion DBI cualquiera contra las promesas de la tabla de
# motores de los README. No forma parte del paquete instalado.
#
# La tabla dice "probado contra el motor real" para algunos motores y "esperado"
# para otros, y esa distincion solo vale si la comprobacion se puede repetir.
# Este guion es la comprobacion. Se usa asi:
#
#   source("benchmark/verificar_motor.R")
#   con <- DBI::dbConnect(duckdb::duckdb())
#   verificar_motor(con, "DuckDB 1.5")
#
# Lo que comprueba, en este orden, porque cada uno depende del anterior:
#
#   1. que el dialecto se resuelva por sonda y no por el nombre del driver;
#   2. que ninguna metrica quede no disponible en los cinco modos;
#   3. que la media, la mediana y el desvio calculados por el motor coincidan
#      con los calculados en R sobre la tabla entera -- no alcanza con que
#      corra, tiene que dar bien;
#   4. que `plan_perfilado_dbi()` prediga exactamente las consultas emitidas
#      -- el contador es el del propio paquete, comprobado aparte contra una
#      conexion instrumentada que cuenta `dbSendQuery`;
#   5. que un nombre calificado con esquema funcione por texto y por `DBI::Id`;
#   6. que una coleccion de dos tablas se perfile.

.tabla_de_prueba_motor <- function(n = 5000L, semilla = 11L) {
  set.seed(semilla)
  datos <- data.frame(
    id = seq_len(n),
    monto = stats::runif(n, 0, 1e5),
    categoria = sample(letters[1:8], n, replace = TRUE),
    texto = paste0("registro ", seq_len(n)),
    fecha = as.Date("2020-01-01") + sample(0:900, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  datos$monto[sample(n, 200L)] <- NA_real_
  datos
}

# La tabla de arriba es comoda, y esa comodidad escondio tres defectos: una
# columna `DATE` medida como numero, un `BIGINT UNSIGNED` con maximo menor que
# el minimo, y la extrapolacion del muestreo dividiendo por las filas pedidas
# en vez de las obtenidas. Ninguno aparecia porque la tabla no tenia fecha
# nativa, ni enteros sin signo, ni menos filas que la muestra por omision.
#
# Estas comprobaciones existen para que la proxima tabla comoda no vuelva a
# certificar un motor que no lo esta. Cada una es un tipo o un tamano que un
# perfilador encuentra en una base real y que un fixture rara vez tiene.
.tipos_incomodos_motor <- function(conexion) {
  pruebas <- list(
    list(
      nombre = "fecha nativa",
      crear = "CREATE TABLE lupa_incomoda_fecha (f DATE)",
      poblar = "INSERT INTO lupa_incomoda_fecha VALUES ('2020-01-01')",
      tabla = "lupa_incomoda_fecha",
      revisar = function(perfil) {
        fila <- perfil$resumen_tabla$columnas
        # Una fecha no se promedia: o queda declarada sin aplicar, o el numero
        # sale en una unidad que nadie pidio.
        if (isTRUE(is.finite(fila$media[[1L]]))) {
          return("la media de una columna DATE salio como numero")
        }
        ""
      }
    ),
    list(
      nombre = "tabla mas chica que la muestra",
      crear = "CREATE TABLE lupa_incomoda_chica (monto DOUBLE PRECISION)",
      poblar = paste0(
        "INSERT INTO lupa_incomoda_chica VALUES ",
        paste0("(", 11:20, ")", collapse = ", ")
      ),
      tabla = "lupa_incomoda_chica",
      modo = "muestreado",
      revisar = function(perfil) {
        fila <- perfil$resumen_tabla$columnas
        validos <- as.numeric(fila$n_validos[[1L]])
        if (!isTRUE(is.finite(validos)) || validos != 10) {
          return(paste0("n_validos dio ", validos, " sobre una columna llena"))
        }
        if (!isTRUE(all.equal(fila$media[[1L]], 15.5))) {
          return(paste0("la media dio ", fila$media[[1L]], " y son 15.5"))
        }
        ""
      }
    )
  )
  cat("\ntipos y tamanos incomodos:\n")
  for (prueba in pruebas) {
    salida <- tryCatch({
      try(DBI::dbExecute(conexion, paste0("DROP TABLE ", prueba$tabla)),
          silent = TRUE)
      DBI::dbExecute(conexion, prueba$crear)
      DBI::dbExecute(conexion, prueba$poblar)
      perfil <- lupa::perfilar_dbi(
        conexion, prueba$tabla,
        modo = if (is.null(prueba$modo)) "exacto" else prueba$modo
      )
      queja <- prueba$revisar(perfil)
      if (nzchar(queja)) paste("!!", queja) else "ok"
    }, error = function(e) paste("el motor no admitio la prueba:",
                                 conditionMessage(e)))
    cat(sprintf("  %-32s %s\n", prueba$nombre, salida))
    try(DBI::dbExecute(conexion, paste0("DROP TABLE ", prueba$tabla)),
        silent = TRUE)
  }
  invisible(NULL)
}

verificar_motor <- function(conexion, nombre_motor, tabla = "lupa_verificacion",
                            esquema = "lupa_esquema") {
  stopifnot(inherits(conexion, "DBIConnection"))
  informacion <- tryCatch(DBI::dbGetInfo(conexion), error = function(e) list())
  es_oracle <- grepl(
    "oracle",
    paste(nombre_motor, class(conexion), unlist(informacion), collapse = " "),
    ignore.case = TRUE
  )
  datos <- .tabla_de_prueba_motor()
  DBI::dbWriteTable(conexion, tabla, datos, overwrite = TRUE)
  cat("=== ", nombre_motor, " ===\n", sep = "")

  modos <- c("exacto", "seguro", "conteos", "muestreado", "aproximado")
  filas <- lapply(modos, function(modo) {
    perfil <- lupa::perfilar_dbi(conexion, tabla, modo = modo)
    registros <- perfil$resumen_tabla$sql
    plan <- lupa::plan_perfilado_dbi(conexion, tabla, modo = modo)
    data.frame(
      modo = modo,
      dialecto = perfil$resumen_tabla$meta$dialecto$nombre,
      calculadas = sum(registros$estado == "calculado"),
      estimadas = sum(registros$estado == "estimado"),
      en_muestra = sum(registros$estado == "observado_muestra"),
      no_disponibles = sum(registros$estado == "no_disponible"),
      plan = as.numeric(attr(plan, "total")),
      emitidas = as.numeric(perfil$resumen_tabla$meta$consultas$emitidas),
      stringsAsFactors = FALSE
    )
  })
  resumen <- do.call(rbind, filas)
  resumen$plan_exacto <- resumen$plan == resumen$emitidas
  print(resumen, row.names = FALSE)
  if (any(resumen$no_disponibles > 0L)) {
    cat("\n!! hay metricas no disponibles: el motor rechazo algo\n")
  }
  if (!all(resumen$plan_exacto)) {
    cat("\n!! el plan no predijo el costo en algun modo\n")
  }

  columnas <- lupa::perfilar_dbi(conexion, tabla, modo = "exacto")$
    resumen_tabla$columnas
  del_motor <- function(metrica) columnas[[metrica]][columnas$columna == "monto"]
  referencia <- c(
    media = mean(datos$monto, na.rm = TRUE),
    mediana = stats::median(datos$monto, na.rm = TRUE),
    desvio = stats::sd(datos$monto, na.rm = TRUE)
  )
  cat("\nlos tres estadisticos contra R, sobre la tabla entera:\n")
  for (metrica in names(referencia)) {
    valor <- del_motor(metrica)
    cat(sprintf(
      "  %-8s motor %.6f   R %.6f   coincide %s\n", metrica, valor,
      referencia[[metrica]],
      isTRUE(all.equal(valor, referencia[[metrica]]))
    ))
  }

  cat("\nnombre calificado con esquema:\n")
  creado <- tryCatch({
    if (es_oracle) {
      # Oracle usa el usuario como esquema y no acepta CREATE SCHEMA IF NOT
      # EXISTS. La tabla fuente de dbWriteTable() queda con nombre comillado.
      esquema_usuario <- informacion$username
      esquema <- if (length(esquema_usuario) && !is.na(esquema_usuario)) {
        toupper(esquema_usuario)
      } else {
        toupper(esquema)
      }
      tabla_esquema <- toupper(tabla)
      tabla_calificada <- DBI::Id(schema = esquema, table = tabla_esquema)
      tabla_calificada_sql <- as.character(
        DBI::dbQuoteIdentifier(conexion, tabla_calificada)
      )
      try(
        DBI::dbExecute(conexion, paste0(
          "DROP TABLE ", tabla_calificada_sql, " PURGE"
        )),
        silent = TRUE
      )
      DBI::dbExecute(conexion, paste0(
        "CREATE TABLE ", tabla_calificada_sql, " AS SELECT * FROM ",
        as.character(DBI::dbQuoteIdentifier(conexion, tabla))
      ))
    } else {
      DBI::dbExecute(conexion, paste0("CREATE SCHEMA IF NOT EXISTS ", esquema))
      DBI::dbExecute(conexion, paste0(
        "CREATE TABLE ", esquema, ".", tabla, " AS SELECT * FROM ", tabla
      ))
    }
    TRUE
  }, error = function(e) {
    cat("  el motor no acepto crear el esquema:", conditionMessage(e), "\n")
    FALSE
  })
  if (creado) {
    referencia_texto <- if (es_oracle) {
      paste0(toupper(esquema), ".", toupper(tabla))
    } else {
      paste0(esquema, ".", tabla)
    }
    referencia_id <- DBI::Id(
      schema = if (es_oracle) toupper(esquema) else esquema,
      table = if (es_oracle) toupper(tabla) else tabla
    )
    for (referencia_tabla in list(referencia_texto, referencia_id)) {
      etiqueta <- if (is.character(referencia_tabla)) "por texto" else "por Id"
      salida <- tryCatch(
        as.character(lupa::perfilar_dbi(
          conexion, referencia_tabla, modo = "conteos"
        )$resumen_tabla$meta$filas),
        error = function(e) paste("ERROR:", conditionMessage(e))
      )
      cat("  ", etiqueta, ": ", salida, "\n", sep = "")
    }
    cat("\ncoleccion de dos tablas:\n")
    salida <- tryCatch({
      perfil <- lupa::perfilar_coleccion(
        lupa::coleccion(conexion, c(tabla, referencia_texto))
      )
      paste("ok,", nrow(perfil$resumen_coleccion), "tablas perfiladas")
    }, error = function(e) paste("ERROR:", conditionMessage(e)))
    cat("  ", salida, "\n", sep = "")
  }
  # Fuera del bloque de esquema: los tipos incomodos no dependen de que el
  # motor acepte crear esquemas, y son la parte que mas importa.
  .tipos_incomodos_motor(conexion)
  invisible(resumen)
}
