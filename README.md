# Complejidad Económica Provincial en Argentina (2021)

**Trabajo Práctico Integrador — Ciencia de Datos (Grupo 21)**

## Integrantes del grupo
* **Duca Facundo** — Registro: `912582`
* **Duarte Juan Pablo** — Registro: `914214`

## Objetivo e hipótesis de trabajo
El objetivo del trabajo es analizar las diferencias en el Índice de Complejidad Provincial (ICP) entre las 24 jurisdicciones subnacionales de Argentina en 2021 y evaluar en qué medida se asocian con la estructura sectorial de las canastas con Ventaja Comparativa Revelada (VCR) —en particular *Agro, Alimentos y Bebidas*, *Maquinaria* y *Químicos*— y con el nivel de ingreso relativo medido por el Índice de VAB per cápita.

Nuestra hipótesis sostiene que la complejidad económica provincial responde principalmente a la composición sectorial de la estructura productiva y exportadora antes que a un mero "efecto riqueza" agregado medido por el VAB per cápita.

## Base de datos utilizada
* **Fuentes principales:** Ministerio de Economía de la Nación (2021), *Complejidad Económica Provincial* (`https://datos.produccion.gob.ar/dataset/complejidad-economica-provincial`), dividida en la base de *Oportunidades productivas* (`base_da_1`) y la base de *Exportaciones actuales* (`base_da_2`).
* **Base auxiliar:** Índice de VAB per cápita provincial para 2021 (promedio nacional = 100), obtenido de Moskovits (2024, Tabla 2, elaborada sobre datos del Ministerio de Economía e INDEC), cruzada para las 24 jurisdicciones sin pérdida de observaciones.
* **Período:** Año 2021 (corte transversal).
* **Unidad de observación:** Par jurisdicción subnacional × partida arancelaria NCM a 6 dígitos (`cod_provincia` × `cod_ncm_6d`), consolidada a nivel de las 24 jurisdicciones para el análisis provincial.

## Descripción del análisis realizado
1. **Auditoría y limpieza:** Normalización de jurisdicciones y secciones económicas, control de unicidad de claves, diagnóstico de los 40 valores faltantes (concentrados en la sección `"Otros"` con `VCR = 0`, conservados sin imputación) y detección de valores atípicos bajo criterio $1{,}5 \times \text{IQR}$ (conservados por corresponder a fenómenos económicos genuinos y evaluados mediante análisis de sensibilidad pre/post-limpieza).
2. **Descomposición de Theil y Shift-Share:** Cuantificación del peso de la heterogeneidad entre sectores (~29 %) frente a la intrasectorial (~71 %) con pruebas de desplazamiento de origen (`base = 1` y `base = 2`), junto con la descomposición Shift-Share de la brecha provincial respecto al promedio nacional en las 21 jurisdicciones con VCR activo.
3. **Clustering provincial ($K\text{-means}$, $K = 4$):** Agrupamiento de canastas sectoriales estandarizadas (evaluado con WSS/codo, silueta y Gap) y asignación al centroide más cercano para las 3 jurisdicciones sin `VCR = 1` en `base_da_2`.
4. **Correlaciones y regresiones MCO con errores robustos HC3:** Evaluación bivariada de sectores clave, ICP y VAB per cápita, y estimación de regresiones lineales por Mínimos Cuadrados Ordinarios con errores estándar consistentes ante heterocedasticidad (`HC3`).

## Estructura del repositorio
```text
tp-ccd/
├── raw/                 # Datos crudos originales sin modificar
├── auxiliar/            # Base auxiliar de VAB per cápita (Moskovits, 2024, Tabla 2)
├── input/               # Bases limpias y consolidadas generadas por el script
├── output/
│   ├── tablas/          # Tablas de resultados y auditorías en CSV
│   └── graficos/        # Visualizaciones exportadas con ggsave()
├── script/              # Scripts de ejecución del proyecto
├── utils/               # Funciones auxiliares (si se separan en archivos individuales)
├── tp-ccd.Rproj         # Proyecto de RStudio
└── README.md            # Documentación del proyecto