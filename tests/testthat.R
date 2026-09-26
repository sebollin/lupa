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
# Sin colores ni caracteres fuera de ASCII: esto se lee en un log de Windows.
options(cli.unicode = FALSE, crayon.enabled = FALSE,
        testthat.use_colours = FALSE)

test_check("lupa", reporter = testthat::MultiReporter$new(reporters = list(
  testthat::CheckReporter$new(),
  testthat::ProgressReporter$new(update_interval = Inf)
)))
