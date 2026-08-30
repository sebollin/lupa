# Un envoltorio propio permite simular un paquete ausente en los tests. La
# primera version redefinia `requireNamespace` dentro del paquete, y eso copia
# el cuerpo de la funcion de `base` -que llama a `.Internal()`- al espacio de
# nombres: `R CMD check` lo marca con WARNING, y con razon, porque `.Internal()`
# no es parte de la API. Un nombre propio hace lo mismo sin tocar `base`.
.hay_paquete <- function(nombre) {
  isTRUE(requireNamespace(nombre, quietly = TRUE))
}

.catalogo_requisitos_motor <- function() {
  data.frame(
    id_motor = c(
      "dbi", "sqlite", "postgresql", "mysql", "sql_server", "duckdb",
      "mariadb", "oracle_12c", "oracle_11", "bigquery", "odbc", "otro_dbi"
    ),
    motor = c(
      "DBI", "SQLite", "PostgreSQL", "MySQL", "SQL Server", "DuckDB",
      "MariaDB", "Oracle", "Oracle", "BigQuery", "ODBC", "Otro compatible con DBI"
    ),
    version = c(
      NA_character_, NA_character_, "16", "8", "2022", "1.5", "11", "Free 23 (12c+)",
      "11 y anteriores", NA_character_, NA_character_, NA_character_
    ),
    paquete_r = c(
      "DBI", "RSQLite", "RPostgres", "RMySQL", "odbc", "duckdb", "RMariaDB",
      "ROracle", "ROracle", "bigrquery", "odbc", NA_character_
    ),
    biblioteca_sistema = c(
      "Ninguna biblioteca externa obligatoria",
      "Ninguna biblioteca externa obligatoria; SQLite se incluye en RSQLite",
      "Cliente libpq",
      "Cliente MySQL compatible con libmysqlclient",
      "unixODBC y el controlador ODBC del motor",
      "Ninguna biblioteca externa obligatoria; DuckDB se incluye en duckdb",
      "MariaDB Connector/C",
      "Oracle Instant Client y libaio en Linux",
      "Oracle Instant Client y libaio en Linux",
      "libcurl para la compilacion si R no trae soporte; no hay cliente de base de datos",
      "unixODBC y el controlador ODBC del motor",
      "Depende del paquete R y del controlador elegido"
    ),
    paquete_debian_ubuntu = c(
      NA_character_, NA_character_, "libpq-dev", "default-libmysqlclient-dev",
      "unixodbc-dev y el paquete msodbcsql18 del proveedor", NA_character_,
      "libmariadb-dev", "libaio1; no hay un paquete de distribucion garantizado para Instant Client: usar el zip de Oracle",
      "libaio1; no hay un paquete de distribucion garantizado para Instant Client: usar el zip de Oracle",
      "libcurl4-openssl-dev si la instalacion lo solicita", "unixodbc-dev y el paquete del controlador",
      "El que indique el controlador"
    ),
    paquete_fedora_rhel = c(
      NA_character_, NA_character_, "libpq-devel", "mariadb-connector-c-devel",
      "unixODBC-devel y el paquete msodbcsql18 del proveedor", NA_character_,
      "mariadb-connector-c-devel", "libaio; no hay un paquete de distribucion garantizado para Instant Client: usar el zip de Oracle",
      "libaio; no hay un paquete de distribucion garantizado para Instant Client: usar el zip de Oracle",
      "libcurl-devel si la instalacion lo solicita", "unixODBC-devel y el paquete del controlador",
      "El que indique el controlador"
    ),
    alternativa_sin_administrador = c(
      "No aplica: DBI es una interfaz de R.",
      "No aplica: RSQLite incluye SQLite.",
      "Usar un cliente libpq ya instalado en un prefijo del usuario o compilarlo alli y se\u00f1alar su `pg_config` al instalar `RPostgres`.",
      "Compilar MySQL Connector/C o MariaDB Connector/C en un prefijo del usuario y se\u00f1alar sus rutas de cabeceras y bibliotecas al instalar `RMySQL`.",
      "Compilar FreeTDS en un prefijo del usuario y pasar la ruta directa del controlador en la cadena de conexion; no hace falta registrar un DSN.",
      "No aplica: duckdb incluye su biblioteca.",
      "Compilar MariaDB Connector/C en un prefijo del usuario y se\u00f1alarlo mediante `MARIADB_HOME` al instalar `RMariaDB`.",
      "Descargar el Instant Client como zip, descomprimirlo en una carpeta del usuario y declarar `OCI_LIB` y `OCI_INC` al compilar `ROracle`.",
      "Descargar el Instant Client como zip, descomprimirlo en una carpeta del usuario y declarar `OCI_LIB` y `OCI_INC` al compilar `ROracle`.",
      "Usar la biblioteca libcurl de la instalacion de R; si no alcanza, compilar libcurl en un prefijo del usuario.",
      "Compilar FreeTDS en un prefijo del usuario y pasar la ruta del controlador en la cadena de conexion; no hace falta registrarlo en el sistema.",
      "Instalar el paquete R y su cliente en un prefijo del usuario, usando las variables que documente ese controlador."
    ),
    dialecto = c(
      NA_character_, "limit", "limit", "limit", "top", "limit", "limit",
      "fetch_first", "rownum", "portable", "portable", "portable"
    ),
    # MariaDB 11 se verifico contra motor real despues de escribirse este
    # catalogo. Oracle figuraba como `probado` y no hay con que sostenerlo: el
    # unico informe de una corrida contra Oracle -2026-08-24- termina diciendo
    # que la medicion no se pudo ejecutar porque el entorno rechazo el acceso al
    # demonio de Docker, y que todos los puntos "permanecen no medidos". Sus dos
    # variantes quedan en `esperado`: el dialecto esta contemplado y la prueba
    # contra un servidor real esta pendiente.
    #
    # La regla que esto hace cumplir es la del proyecto: una fila vale por su log
    # y por su commit, no por el recuerdo de quien la escribio. `probado` sin log
    # no se distingue de `esperado`, y la diferencia entre esas dos palabras es
    # justamente lo unico que esta columna aporta.
    estado_prueba = c(
      "no_aplica", "probado", "probado", "probado", "probado", "probado",
      "probado", "esperado", "esperado", "no_documentado", "no_documentado",
      "reserva"
    ),
    fuente_estado = c(
      "Interfaz DBI; no es un motor de la tabla del README.",
      "README.md y README.es.md, tabla de motores.",
      "README.md y README.es.md, tabla de motores.",
      "README.md y README.es.md, tabla de motores.",
      "README.md y README.es.md, tabla de motores.",
      "README.md y README.es.md, tabla de motores.",
      "README.md y README.es.md: esperado, no comprobado contra el motor.",
      "README.md y README.es.md: esperado, no comprobado contra el motor.",
      "README.md y README.es.md: esperado, no comprobado contra el motor.",
      "No figura como fila de motor en los README.",
      "No figura como fila de motor en los README.",
      "README.md y README.es.md: reserva para cualquier compatible con DBI."
    ),
    sinonimos = c(
      "dbi,interfaz dbi", "sqlite", "postgres,postgresql,postgresql 16",
      "mysql,mysql 8", "sql server,sqlserver,sql server 2022,odbc sql server",
      "duckdb,duckdb 1.5", "mariadb,mariadb 11", "oracle,oracle 12c,oracle 12c+",
      "oracle,oracle 11,oracle 11 y anteriores", "bigquery,big query",
      "odbc", "otro dbi,compatible con dbi,cualquier otro dbi"
    ),
    stringsAsFactors = FALSE
  )
}

.normalizar_nombre_motor <- function(motor) {
  tolower(gsub("[^a-z0-9]+", "", trimws(as.character(motor))))
}

.fila_requisito_motor <- function(motor) {
  catalogo <- .catalogo_requisitos_motor()
  clave <- .normalizar_nombre_motor(motor)
  coincide <- vapply(strsplit(catalogo$sinonimos, ",", fixed = TRUE), function(x) {
    clave %in% vapply(x, .normalizar_nombre_motor, character(1L))
  }, logical(1L))
  catalogo[coincide, , drop = FALSE]
}

.detener_error_requisito <- function(clase, lineas) {
  herencia <- if (grepl("dbi", clase, fixed = TRUE)) "lupa_error_dbi" else character()
  cli::cli_abort(
    lineas,
    class = c(clase, "lupa_error_requisito", herencia, "error", "condition")
  )
}

.requerir_dbi_accionable <- function(accion = "usar una conexion DBI") {
  if (!.hay_paquete("DBI")) {
    .detener_error_requisito(
      "lupa_error_requisito_dbi",
      c(
        paste0(
          "No se puede ", accion, ": falta el paquete R `DBI`. ",
          "Para continuar, instalar el paquete opcional 'DBI' con ",
          "`install.packages(\"DBI\")`."
        ),
        "i" = "Instalarlo con `install.packages(\"DBI\")`."
      )
    )
  }
  invisible(TRUE)
}

.requerir_paquete_motor <- function(motor, accion = "usar el motor") {
  requisito <- .fila_requisito_motor(motor)
  if (!nrow(requisito)) {
    .detener_error_requisito(
      "lupa_error_argumento_requisitos",
      c(
        paste0("No se reconoce el motor `", motor, "`."),
        "i" = "Consultar `requisitos_motor()` para ver los motores disponibles."
      )
    )
  }
  paquetes <- unique(requisito$paquete_r[!is.na(requisito$paquete_r)])
  if (!length(paquetes)) return(invisible(requisito))
  for (paquete in paquetes) {
    if (.hay_paquete(paquete)) next
    fila <- requisito[requisito$paquete_r == paquete, , drop = FALSE][1L, , drop = FALSE]
    .detener_error_requisito(
      "lupa_error_requisito_motor",
      c(
        paste0(
          "No se puede ", accion, ": el motor ", fila$motor,
          " requiere el paquete R `", paquete, "`, que no esta instalado. ",
          "Instalarlo con `install.packages(\"", paquete, "\")`. ",
          "Si la compilacion falla, revisar `", fila$biblioteca_sistema,
          "`; sin administrador: ", fila$alternativa_sin_administrador
        ),
        "i" = paste0("Instalarlo con `install.packages(\"", paquete, "\")`."),
        "i" = paste0(
          "Si la compilacion falla, revisar `", fila$biblioteca_sistema,
          "`. En Debian/Ubuntu: ", fila$paquete_debian_ubuntu,
          "; en Fedora/RHEL: ", fila$paquete_fedora_rhel, "."
        ),
        "i" = paste0("Alternativa sin administrador: ", fila$alternativa_sin_administrador)
      )
    )
  }
  invisible(requisito)
}

.requisito_conexion_dbi <- function(conexion) {
  texto <- paste(class(conexion), collapse = " ")
  informacion <- tryCatch(DBI::dbGetInfo(conexion), error = function(e) NULL)
  if (length(informacion)) {
    texto <- paste(texto, paste(unlist(informacion), collapse = " "))
  }
  texto <- tolower(texto)
  id <- if (grepl("postgres|pqconnection", texto)) {
    "postgresql"
  } else if (grepl("mariadb", texto)) {
    "mariadb"
  } else if (grepl("mysql", texto)) {
    "mysql"
  } else if (grepl("sqlite", texto)) {
    "sqlite"
  } else if (grepl("duckdb", texto)) {
    "duckdb"
  } else if (grepl("oracle|oraconnection", texto)) {
    "oracle_12c"
  } else if (grepl("bigquery|bigqueryconnection", texto)) {
    "bigquery"
  } else if (grepl("sql server|sqlserver|mssql", texto)) {
    "sql_server"
  } else if (grepl("odbc|odbcconnection", texto)) {
    "odbc"
  } else {
    NA_character_
  }
  if (is.na(id)) return(NULL)
  catalogo <- .catalogo_requisitos_motor()
  catalogo[catalogo$id_motor == id, , drop = FALSE]
}

.validar_conexion_dbi <- function(conexion, accion = "usar una conexion DBI") {
  .requerir_dbi_accionable(accion)
  if (!inherits(conexion, "DBIConnection")) {
    .detener_error_requisito(
      "lupa_error_conexion_dbi",
      c(
        "La conexion no hereda de `DBIConnection`.",
        "i" = "Abrirla con un controlador DBI instalado y pasar esa conexion a `lupa`."
      )
    )
  }
  valida <- tryCatch(DBI::dbIsValid(conexion), error = function(e) e)
  if (inherits(valida, "condition")) {
    # Un controlador que no implementa `dbIsValid()` no es una conexion
    # invalida: es un controlador incompleto. El `ROracle` archivado es el caso,
    # y tratarlo como conexion rota dejaba a Oracle afuera de `coleccion()`
    # aunque el SQL funcionara. Antes de rendirse se prueba `dbGetInfo()`, que
    # responde si la conexion esta viva.
    respira <- isTRUE(tryCatch({
      DBI::dbGetInfo(conexion)
      TRUE
    }, error = function(e) FALSE))
    if (!respira) .detener_error_conexion_dbi(valida, accion = accion)
    valida <- TRUE
  }
  if (!isTRUE(valida)) {
    .detener_error_requisito(
      "lupa_error_conexion_dbi",
      c(
        paste0("No se puede ", accion, ": la conexion DBI no esta abierta y valida."),
        "i" = "Volver a abrirla con el controlador del motor."
      )
    )
  }
  requisito <- .requisito_conexion_dbi(conexion)
  if (!is.null(requisito) && nrow(requisito)) {
    .requerir_paquete_motor(requisito$id_motor[[1L]], accion = accion)
  }
  invisible(conexion)
}

.parece_falla_biblioteca_dbi <- function(texto) {
  patrones <- c(
    "cannot load shared object", "unable to load shared library",
    "no such file or directory.*\\.(so|dll|dylib)",
    "(library|driver|client).*(not found|no encontrado|cannot open)",
    "data source name.*not found", "driver.*not found", "im002", "im003",
    "oci.*(not found|cannot|missing)", "libpq.*(not found|cannot|missing)",
    "mariadb.*(not found|cannot|missing)", "mysql.*(not found|cannot|missing)"
  )
  any(vapply(patrones, grepl, logical(1L), x = texto, ignore.case = TRUE))
}

.detener_error_conexion_dbi <- function(error, motor = NULL,
                                        accion = "abrir una conexion DBI") {
  texto <- conditionMessage(error)
  requisito <- if (!is.null(motor)) .fila_requisito_motor(motor) else NULL
  if (is.null(requisito) || !nrow(requisito)) {
    requisito <- NULL
  } else {
    requisito <- requisito[!duplicated(requisito$paquete_r), , drop = FALSE]
  }
  if (.parece_falla_biblioteca_dbi(texto) && !is.null(requisito)) {
    fila <- requisito[1L, , drop = FALSE]
    .detener_error_requisito(
      "lupa_error_conexion_dbi",
      c(
        paste0(
          "No se pudo ", accion,
          ". El controlador informo un problema con una biblioteca externa. ",
          "No se comprobo que este instalada. Debian/Ubuntu: ",
          fila$paquete_debian_ubuntu, "; Fedora/RHEL: ", fila$paquete_fedora_rhel,
          ". Sin administrador: ", fila$alternativa_sin_administrador,
          " Mensaje original del controlador: ", texto
        ),
        "x" = paste0("Mensaje original del controlador: ", texto),
        "i" = paste0(
          "No se comprobo que la biblioteca este instalada. Para ", fila$motor,
          ": ", fila$biblioteca_sistema, "."
        ),
        "i" = paste0(
          "Debian/Ubuntu: ", fila$paquete_debian_ubuntu,
          "; Fedora/RHEL: ", fila$paquete_fedora_rhel, "."
        ),
        "i" = paste0("Sin administrador: ", fila$alternativa_sin_administrador)
      )
    )
  }
  .detener_error_requisito(
    "lupa_error_conexion_dbi",
    c(
      paste0("No se pudo ", accion, ". El controlador informo: ", texto),
      "i" = "Comprobar el servidor, las credenciales, el controlador y la ruta de sus bibliotecas."
    )
  )
}

#' @export
print.requisitos_motor <- function(x, ...) {
  # Recortar columnas conserva la clase, asi que el metodo puede recibir un
  # objeto al que le faltan los campos que iba a mostrar. Imprimir a medias
  # seria peor que decirlo: se cae a la vista de data frame, que si describe lo
  # que quedo.
  necesarias <- c(
    "motor", "version", "paquete_r", "estado_paquete_r",
    "estado_biblioteca_sistema", "dialecto"
  )
  if (!all(necesarias %in% names(x))) {
    return(print(as.data.frame(x), ...))
  }
  cli::cli_h1("Requisitos de motores")
  if (!nrow(x)) {
    cli::cli_text("No hay requisitos para el filtro indicado.")
    return(invisible(x))
  }
  for (i in seq_len(nrow(x))) {
    estado <- x$estado_paquete_r[[i]]
    sistema <- x$estado_biblioteca_sistema[[i]]
    paquete <- if (is.na(x$paquete_r[[i]])) "no aplica" else x$paquete_r[[i]]
    cli::cli_text(paste0(
      x$motor[[i]],
      if (!is.na(x$version[[i]])) paste0(" ", x$version[[i]]) else "",
      " | R `", paquete, "`: ", estado,
      " | biblioteca del sistema: ", sistema,
      " | dialecto: ", ifelse(is.na(x$dialecto[[i]]), "no aplica", x$dialecto[[i]])
    ))
  }
  invisible(x)
}

#' Consultar requisitos para conectarse a motores de bases
#'
#' Devuelve el catálogo de paquetes R, bibliotecas del sistema, dialectos y
#' estado de prueba que `lupa` conoce. La presencia del paquete R se comprueba
#' en la máquina actual. La biblioteca del sistema no se declara instalada sin
#' una comprobación específica: cuando corresponde, la columna lo deja como
#' `no_comprobada` y conserva las rutas de resolución posibles.
#'
#' El estado `probado` se copia de la tabla de motores de los README. `esperado`
#' significa que el dialecto está implementado pero no se comprobó contra ese
#' motor real en este repositorio. Los motores que no figuran en esa tabla no se
#' presentan como probados.
#'
#' @param motor `NULL` para devolver el catálogo completo, o el nombre de un
#'   motor, como `"oracle"`, `"postgresql"` o `"sql server"`.
#'
#' @return Un `data.frame` de clase `requisitos_motor`, con una fila por variante
#'   documentada. Incluye el estado comprobado del paquete R y un estado explícito
#'   para la biblioteca del sistema: `no_comprobada` no significa instalada.
#'
#' @export
#' @examples
#' # El catálogo completo: una fila por variante documentada.
#' todos <- requisitos_motor()
#' nrow(todos)
#'
#' # Lo que hace falta para un motor en particular, antes de intentar conectarse.
#' requisitos_motor("postgresql")[, c("paquete_r", "estado_paquete_r")]
#'
#' # `no_comprobada` en la biblioteca del sistema no significa instalada: el
#' # paquete no puede saberlo sin intentarlo, y lo dice en vez de suponerlo.
#' unique(requisitos_motor()$estado_biblioteca_sistema)
requisitos_motor <- function(motor = NULL) {
  if (!is.null(motor) &&
      (!is.character(motor) || length(motor) != 1L || is.na(motor) ||
       !nzchar(trimws(motor)))) {
    .detener_error_requisito(
      "lupa_error_argumento_requisitos",
      "`motor` debe ser NULL o el nombre de un motor en una cadena no vacia."
    )
  }
  catalogo <- .catalogo_requisitos_motor()
  if (!is.null(motor)) {
    seleccion <- .fila_requisito_motor(motor)
    if (!nrow(seleccion)) {
      .detener_error_requisito(
        "lupa_error_argumento_requisitos",
        c(
          paste0("No se reconoce el motor `", motor, "`."),
          "i" = "Consultar `requisitos_motor()` para ver los motores disponibles."
        )
      )
    }
    catalogo <- catalogo[catalogo$id_motor %in% seleccion$id_motor, , drop = FALSE]
  }
  catalogo$sinonimos <- NULL
  catalogo$paquete_r_instalado <- vapply(
    catalogo$paquete_r,
    function(paquete) {
      if (is.na(paquete)) return(NA)
      .hay_paquete(paquete)
    },
    logical(1L)
  )
  catalogo$estado_paquete_r <- ifelse(
    is.na(catalogo$paquete_r), "no_aplica",
    ifelse(catalogo$paquete_r_instalado, "instalado", "falta")
  )
  catalogo$estado_biblioteca_sistema <- ifelse(
    grepl("^Ninguna biblioteca", catalogo$biblioteca_sistema),
    "no_requerida", "no_comprobada"
  )
  catalogo$diagnostico_biblioteca_sistema <- ifelse(
    catalogo$estado_biblioteca_sistema == "no_requerida",
    "No requiere una biblioteca externa del sistema segun este catalogo.",
    "No se comprobo la biblioteca del sistema; si falla la compilacion del paquete R, revisar el requisito documentado."
  )
  class(catalogo) <- c("requisitos_motor", "data.frame")
  catalogo
}
