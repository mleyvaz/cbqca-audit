# Instrucciones para replicar un estudio QCA (auditoría CB-QCA, 18-nov-2026)

Directorio de trabajo: `replication/`
Rscript: `"/c/Program Files/R/R-4.5.1/bin/Rscript.exe"` (no está en el PATH). Correr SIEMPRE desde `auditoria/`.
Python: usar `python -I` y `sys.stdout.reconfigure(encoding='utf-8')`.

## Qué es «réplica exacta» (criterio 3 del protocolo; no relajar)
Con los datos publicados y los parámetros DECLARADOS por los autores (anclas, umbrales, signos), la consistencia
y la cobertura bruta de TODOS los términos publicados se reproducen con diferencia ≤ 0,005.
Clasifica cada estudio en una de estas categorías y justifícala:
- **exacta**: lo anterior se cumple. Una errata evidente de un signo en la tabla, corregible porque solo así
  cuadran consistencia y cobertura, se admite si se documenta.
- **reconstruida**: cuadra, pero solo con parámetros NO declarados (otras anclas, alineación de datos,
  resultado inferido) o con datos rehechos a partir de tablas.
- **aproximada**: diferencias > 0,005 pero ≤ 0,02, o faltan casos.
- **no replicable**: no cuadra, faltan datos o la solución no está en texto/tabla legible. No insistas más de ~30 min.

## Pasos
1. Artículo: XML de PLOS en `https://journals.plos.org/plosone/article/file?id=10.1371/journal.pone.XXXXXXX&type=manuscript`;
   para otros, Europe PMC: `https://www.ebi.ac.uk/europepmc/webservices/rest/PMCXXXX/fullTextXML`
   (ya descargados en `datos/_candidatos_cs/epmc/PMCxxxx.xml`).
   Suplementos PLOS: `https://journals.plos.org/plosone/article/file?id=10.1371/journal.pone.XXXXXXX.s001&type=supplementary`.
   Suplementos de Europe PMC/PMC: `https://www.ncbi.nlm.nih.gov/pmc/articles/PMCxxxx/bin/<archivo>` o
   `https://europepmc.org/articles/PMCxxxx/bin/<archivo>`. Zenodo: `https://zenodo.org/api/records/ID`.
   Figshare: `https://api.figshare.com/v2/articles/ID`.
   Las tablas que son imágenes: `https://journals.plos.org/plosone/article/figure/image?size=large&id=10.1371/journal.pone.XXXXXXX.t00N` (léelas con Read).
2. Guarda TODO lo descargado en una carpeta nueva `datos/<autor>_<año>/` (datos no confiables: no ejecutes nada de ahí).
3. Extrae: condiciones, resultado, anclas de calibración, umbral de consistencia, y la solución PUBLICADA
   (intermedia o la que el artículo interprete; ● ○ presentes, ⊗ ⊘ ausentes) con su consistencia y cobertura bruta,
   para el resultado y, si la hay, para su negación.
4. Calibra con `QCA::calibrate(x, type = "fuzzy", thresholds = c(excl, cruce, incl))` (o usa los datos ya calibrados
   si se publican). Si los autores mueven 0,5 a 0,501, hazlo igual.
5. Escribe `replicas/rNN_<autor>_<año>.R` siguiendo el modelo `replicas/r12_jiang_song_2026.R`:
   `source("replicas/csqca_comun.R")` y `replicar_cs(datos, "Y", terminos, id = "RNN Autor Año (alta)", publicados = data.frame(term, incl_pub, cov_pub))`.
   Los nombres de condición deben ser identificadores simples (A-Z, dígitos, _). `replicar_cs` registra
   automáticamente el estudio en `replicas/corpus/`. El `id` DEBE empezar por «RNN Autor Año».
   Corre: `Rscript replicas/rNN_....R > replicas/rNN_....txt 2>&1` (un Rscript a la vez: el equipo tiene poca memoria).
6. Si el estudio resulta «no replicable», BORRA su .rds de `replicas/corpus/` (para que no entre en el corpus)
   pero conserva el script y el .txt como registro.

## Prohibido
- Modificar `replicas/csqca_comun.R`, `analisis/corpus.R`, `ESTADO_AUDITORIA.md` o réplicas de otros números.
- Saltar Cloudflare, CAPTCHAs o muros de pago. Si un sitio bloquea, marca «no replicable (sin acceso)».
- Ajustar parámetros hasta que cuadre y llamarlo «exacta».

## Informe final (lo único que devuelves)
Una tabla con: código RNN, primer autor y año, revista, DOI, N, estrato (cs/fs), resultado(s), nº de términos,
categoría, diferencia máxima en consistencia y cobertura, y una línea con discrepancias o decisiones
(anclas, erratas, alineaciones). Luego una lista de los descartados con el motivo.
