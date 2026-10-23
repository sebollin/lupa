library(testthat)
library(lupa)

# Por que hay un reporte de progreso ademas del del check.
#
# El 2026-09-26, en win-builder R-devel, el check murio asi:
#
#   * checking tests ... ERROR
#   Check process probably crashed or hung up for 20 minutes ... killed
#
# El reporte "check" no imprime NADA hasta el final, y esta suite tarda 29
# minutos en la cola de release de win-builder y 67 en la de oldrelease: pasa
# tramos enteros sin una linea. Un reporte de progreso deja una linea por archivo
# con su tiempo, que es lo que distingue "esta trabajando" de "se colgo" -para el
# vigilante de win-builder y para quien lee el log-.
#
# El reporte del CHECK se conserva: es el que escribe `testthat-problems.txt`.
# Que el check siga fallando ante una prueba roja NO se da por hecho: lo mide
# `notas-desarrollo/.trabajo-agente/probar-reporte.sh`, que corre el check sobre
# una copia con una prueba que debe fallar y otra vez sin ella.
#
# Que corre en CRAN. En la configuracion de la granja -Suggests instalados, sin
# NOT_CRAN- `checking tests` tardaba 17 minutos aca el 2026-10-23, unos 45 en
# win-builder. Desde entonces empiezan con `testthat::skip_on_cran()` las pruebas
# de regresion de las rondas de refutacion (`O*`, `N*`, `ronda*`) y los archivos
# que tardaban mas de 1 s en la medicion sin red, y las de la proteccion de datos,
# que es un complemento del paquete y no su cometido: 332 de 413. Siguen corriendo
# enteras en la suite local, en el CI y en R-hub, que usan NOT_CRAN=true. En CRAN
# quedan las pruebas generales de un segundo o menos y cuatro que se conservan por
# lo que cubren: el perfil, el plan y `aplicar()`, el informe HTML y los datos del
# paquete. En win-builder el check de la granja tardaba 562 a 619 s con 85
# archivos; la lista y los tiempos: `notas-desarrollo/.trabajo-agente/cran/`.
#
# Sin colores ni caracteres fuera de ASCII: esto se lee en un log de Windows.
options(cli.unicode = FALSE, crayon.enabled = FALSE,
        testthat.use_colours = FALSE)

test_check("lupa", reporter = testthat::MultiReporter$new(reporters = list(
  testthat::CheckReporter$new(),
  testthat::ProgressReporter$new(update_interval = Inf)
)))
