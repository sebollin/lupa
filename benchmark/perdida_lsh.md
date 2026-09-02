# Cuánto pierde el tamiz LSH

Medido con `benchmark/perdida_lsh.R`, reproducible. La primera corrida fue del
2026-08-18; la del 2026-09-02 sobre `5645c15` dio exactamente los mismos
conteos —son deterministas— y es la que queda en `datos/perdida_lsh_bandas.csv`
y `datos/perdida_lsh_umbral.csv`, de donde `graficar_figuras.R` dibuja la
figura del tamiz del README.

## Qué se mide, y por qué no está dentro del objeto

En el camino LSH hay **dos medidas seguidas**. El tamiz arma candidatos con
Jaccard de q-gramas y **no conoce `metodo`**; recién después se mide con el
método pedido. Los pares que el método final habría aceptado pero el tamiz no
propuso no se pierden en silencio: hasta ahora ni se contaban.

**Esto no puede ir en el `alcance` de una corrida normal.** La pérdida real sólo
se puede medir corriendo también el camino exhaustivo sobre la misma entrada;
publicarla en una corrida que no lo hizo sería informar como medida una pérdida
que esa corrida no midió. Por eso vive acá, como banco, con su corpus y sus
parámetros declarados.

## El diseño experimental

Comparar las dos salidas sin más mezclaría **seis causas de pérdida**: el tamiz,
las cubetas descartadas, el presupuesto de pares, el bloqueo, el muestreo y el
tope de resultados. Para aislar el tamiz, las otras cinco se neutralizan:
`muestra`, `max_pares`, `max_resultados` y `presupuesto_pares` en `Inf`, sin
`bloquear_por`. La diferencia que queda es atribuible al tamiz.

## Resultado, por número de bandas

Corpus sintético de nombres con número, 400 y 800 filas, tres semillas, método
`jw` con umbral `0,1`.

```
lsh_bandas   perdida media   perdida maxima
         8           0,758            0,803
        12           0,656            0,720
        20           0,521            0,614
```

Más bandas es un tamiz más permisivo: propone más candidatos y pierde menos, a
cambio de más comparaciones.

## Y el contraste que impide leer eso como una propiedad del método

800 filas, 12 bandas, variando cuán permisivo es el método final:

```
umbral   pares exhaustivo   pares LSH   perdidos   perdida
  0,02                  2           1          1     0,500
  0,05                337         166        171     0,507
  0,10             12.945       4.823      8.122     0,627
  0,20             31.609       9.273     22.336     0,707
```

**La pérdida no es una propiedad del tamiz: es la distancia entre lo que el
tamiz propone y lo que el método final acepta.** Con un umbral estricto los
pocos pares reales son muy parecidos y el tamiz los propone; con uno permisivo
el método acepta pares que Jaccard de q-gramas no acerca.

## Qué se puede concluir, y qué no

**Sí se puede concluir** que el camino LSH es un **tamiz de cribado y no un
sustituto del exhaustivo**, y que en este corpus deja afuera aproximadamente la
mitad o más de los pares que el método final habría aceptado. Quien necesite
exhaustividad tiene que usar el camino exhaustivo, con su costo.

**No se puede concluir** un número universal. La pérdida depende del corpus, del
método, del umbral y de `b`, `r` y `q`. Este banco mide un corpus sintético
diseñado para tener muchos vecinos cercanos; un padrón real con pocas erratas
aisladas se comporta distinto y hay que medirlo aparte.

**Lo que el objeto sigue diciendo, y está bien:** el `alcance` de una corrida
declara las garantías de Jaccard del esquema LSH y dice expresamente que **no
garantizan la medida final**. Eso era correcto y no cambia; lo que faltaba era
un número que dimensionara la advertencia.
