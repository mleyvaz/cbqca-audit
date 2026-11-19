# Protocolo de la auditoría CB-QCA (versión 1, 16-nov-2026, ANTES de ver resultados)

## Pregunta
¿Qué parte de las soluciones QCA publicadas se sostiene como afirmación causal (probabilidades de causación de Pearl) y cuánta confusión no medida haría falta para que deje de sostenerse?

## Artículo y revista
- Título de trabajo: «How causal are published QCA solutions? Bounding the probabilities of causation behind configurational findings».
- Revista Q1 objetivo: Political Analysis (Krogslund et al. 2015 publicó ahí la crítica de sensibilidad). Alternativas Q1: Organizational Research Methods; Sociological Methods & Research (evitar mientras SMR-26-0314 esté en revisión, para no concentrar dos envíos del mismo autor).
- Método: paquete `cbqca` (Documents/cbqca), independiente de PI-QCA.

## Corpus
Inclusión:
1. Estudio publicado con revisión por pares que reporta al menos una solución de suficiencia.
2. Datos de replicación abiertos (repositorio, material suplementario o conjunto de datos de un paquete de R) con pertenencias calibradas, o datos brutos con anclas reportadas.
3. Réplica exacta: la solución publicada debe reproducirse con los datos y parámetros declarados. Si no se reproduce, el estudio se registra como «no replicado» y se excluye del análisis principal (se informa el número).
Estratos: (A) csQCA, análisis principal, sin dicotomización; (B) fsQCA, análisis secundario, con la consistencia difusa y la nítida lado a lado y el corte en 0,5 declarado como limitación.
Meta: ≥ 15 estudios csQCA y ≥ 25 fsQCA. Búsqueda: Codex y Gemini (web), paquetes de R (QCA, SetMethods, cna), Harvard Dataverse, OSF, Zenodo, archivos de replicación de revistas.

## Unidad de análisis
Cada término (camino) de cada solución publicada, y la solución completa.

## Medidas (fijadas antes de ver resultados)
- Primaria 1: proporción de términos publicados con cota inferior de PS < 0,5 bajo exogeneidad.
- Primaria 2: mediana del punto de quiebre de PNS (confusión que permite PNS = 0).
- Secundarias: cota inferior de PN; número de casos por término (n_in); ancho bootstrap (1000 réplicas) de la cota inferior de PS; relación entre consistencia publicada y PS mínima.
- Tipificación de la incertidumbre de cada término: muestreo (ancho bootstrap), identificación bajo exogeneidad (PS_up − PS_lo), confusión (punto de quiebre).

## Umbrales
tau = 0,5 para PS («más probable que no»); tau = 0 para PNS. Sensibilidad: tau = 0,3 y 0,7.

## Reglas de transparencia
- Todo el código y los datos derivados, públicos.
- Los resultados se reportan para todos los estudios incluidos, también si favorecen al QCA publicado.
- Ningún estudio se presenta como «erróneo»: la auditoría mide qué afirmación causal soporta cada solución, no si los autores se equivocaron.
- Antes de enviar: revisión adversarial (Codex veracidad, Gemini método) y coautor de la comunidad QCA si es posible.

## Iteraciones previstas
1. Lista de candidatos (Codex + Gemini + paquetes R) → depurar y verificar enlaces.
2. Réplica de cada solución publicada → tabla de réplica.
3. CB-QCA por término → tablas y figuras.
4. Redacción (flujo C) → revisión adversarial → corrección → envío.
