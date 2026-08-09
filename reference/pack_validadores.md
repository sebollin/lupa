# Crear y consultar packs de validadores

Un pack es una lista con nombres de funciones vectorizadas. No se
registra en estado global: puede definirse en otro paquete o en un
script y conectarse directamente con la propiedad `validador` de
`Formato` o con `validadores_personales` en `perfilar()`. Esta forma
permite agregar países o dominios sin modificar el núcleo de `lupa`; el
pack uruguayo predeterminado de `perfilar()` usa exactamente esta misma
puerta.

## Uso

``` r
pack_validadores(nombre, validadores, pais = NULL, descripcion = NULL)

validadores_internacionales()

validadores_uruguay()
```

## Argumentos

  - nombre:
    
    Nombre del pack.

  - validadores:
    
    Lista con nombres de funciones que aceptan un vector y devuelven un
    vector lógico de igual longitud.

  - pais:
    
    Código ISO 3166 alpha-2 opcional del país al que pertenece el pack.
    `NULL` representa un pack internacional o no territorial.

  - descripcion:
    
    Descripción breve opcional.

## Valor

`pack_validadores()` devuelve un objeto S3 `pack_validadores`.
`validadores_internacionales()` y `validadores_uruguay()` devuelven
packs preparados para usar.

## Ver también

`especializar()`, `validar_iso3166()`, `validar_ci_uy()`

## Ejemplos

``` r
internacionales <- validadores_internacionales()
internacionales$correo(c("persona@example.org", "incorrecto"))
#> [1]  TRUE FALSE

# Un pack de otro país se construye sin registrar ni cambiar el núcleo.
digito_rut_cl <- function(x) {
  uno <- function(valor) {
    z <- toupper(gsub("[.-]", "", valor))
    if (!grepl("^[0-9]{7,8}[0-9K]$", z)) return(FALSE)
    cuerpo <- as.integer(strsplit(substr(z, 1, nchar(z) - 1), "")[[1]])
    suma <- sum(rev(cuerpo) * rep(2:7, length.out = length(cuerpo)))
    esperado <- 11 - suma %% 11
    esperado <- if (esperado == 11) "0" else if (esperado == 10) "K" else
      as.character(esperado)
    identical(substr(z, nchar(z), nchar(z)), esperado)
  }
  ifelse(is.na(x), NA, vapply(as.character(x), uno, logical(1)))
}
chile <- pack_validadores(
  "Chile", list(rut = digito_rut_cl), pais = "CL",
  descripcion = "Validadores mantenidos por el proyecto consumidor."
)
chile$rut(c("12.345.678-5", "12.345.678-4"))
#> [1]  TRUE FALSE
```
