# Extraccion de temperatura maxima, media y minima DIARIA a 2 m (AgERA5)
# Los NetCDF de este producto expresan la temperatura en kelvin (K).

if (!requireNamespace("terra", quietly = TRUE)) {
  stop("Instala el paquete: install.packages('terra')")
}

# ------------------------------------------------------------------
# 1. CONFIGURACION EDITABLE
# ------------------------------------------------------------------
fecha_inicio <- as.Date("2026-09-27")
fecha_fin <- as.Date("2026-10-27")

carpetas_nc <- c(
  "datos/agera5/temperatura/sep",
  "datos/agera5/temperatura/oct"
)
archivo_estaciones <- "config/estaciones.csv"
salida_larga <- "resultados/Temperaturas_AgERA5_largo.csv"
salida_comparativa <- "resultados/Temperaturas_AgERA5_comparativa.csv"

if (fecha_fin < fecha_inicio) stop("La fecha final es anterior a la inicial.")
if (!file.exists(archivo_estaciones)) {
  stop("No encuentro config/estaciones.csv. Abre RStudio en la raiz del repositorio.")
}

estaciones <- read.csv(archivo_estaciones, stringsAsFactors = FALSE)
if (!all(c("Estacion", "lon", "lat") %in% names(estaciones)) ||
    nrow(estaciones) < 1 || anyNA(estaciones) ||
    anyDuplicated(estaciones$Estacion) > 0 ||
    !is.numeric(estaciones$lon) || !is.numeric(estaciones$lat) ||
    any(abs(estaciones$lon) > 180) || any(abs(estaciones$lat) > 90)) {
  stop("Revisa config/estaciones.csv: Estacion, lon y lat deben ser validas.")
}
puntos <- terra::vect(estaciones, geom = c("lon", "lat"), crs = "EPSG:4326")

# ------------------------------------------------------------------
# 2. BUSCAR ARCHIVOS E IDENTIFICAR VARIABLES Y FECHAS
# ------------------------------------------------------------------
if (any(!dir.exists(carpetas_nc))) {
  stop("Faltan carpetas de AgERA5: ",
       paste(carpetas_nc[!dir.exists(carpetas_nc)], collapse = ", "))
}
archivos <- unlist(lapply(carpetas_nc, function(carpeta) {
  list.files(carpeta, pattern = "\\.nc$", full.names = TRUE,
             ignore.case = TRUE)
}), use.names = FALSE)
if (!length(archivos)) stop("No se encontraron archivos .nc de temperatura.")

leer_fecha <- function(nombre) {
  texto <- regmatches(nombre, regexpr("20[0-9]{6}", nombre))
  if (length(texto) != 1) stop("No encuentro fecha YYYYMMDD en ", nombre)
  fecha <- as.Date(texto, format = "%Y%m%d")
  if (is.na(fecha)) stop("Fecha invalida en ", nombre)
  fecha
}

leer_tipo <- function(nombre) {
  if (grepl("Max-24h", nombre, ignore.case = TRUE)) return("Max")
  if (grepl("Mean-24h", nombre, ignore.case = TRUE)) return("Mean")
  if (grepl("Min-24h", nombre, ignore.case = TRUE)) return("Min")
  stop("Temperatura no identificada como Max-24h, Mean-24h o Min-24h: ", nombre)
}

fechas_archivos <- as.Date(vapply(basename(archivos), function(n) {
  as.character(leer_fecha(n))
}, character(1)))
tipos <- vapply(basename(archivos), leer_tipo, character(1))

incluidos <- fechas_archivos >= fecha_inicio & fechas_archivos <= fecha_fin
archivos <- archivos[incluidos]
fechas_archivos <- fechas_archivos[incluidos]
tipos <- tipos[incluidos]

fechas_esperadas <- seq(fecha_inicio, fecha_fin, by = "day")
claves <- paste(as.character(fechas_archivos), tipos, sep = "|")
claves_esperadas <- as.vector(outer(as.character(fechas_esperadas),
                                    c("Min", "Mean", "Max"), paste, sep = "|"))
faltantes <- setdiff(claves_esperadas, claves)
if (length(faltantes)) {
  stop("Faltan combinaciones fecha-variable: ", paste(faltantes, collapse = ", "))
}
if (anyDuplicated(claves) > 0) {
  stop("Existen dos archivos de la misma variable para una fecha.")
}

# ------------------------------------------------------------------
# 3. EXTRAER VALORES Y CONVERTIR K A GRADOS CELSIUS
# ------------------------------------------------------------------
tabla_temperatura <- do.call(rbind, lapply(seq_along(archivos), function(i) {
  f <- archivos[i]
  r <- terra::rast(f)
  if (terra::nlyr(r) != 1) stop("Se esperaba una sola capa: ", basename(f))

  pts_r <- terra::project(puntos, terra::crs(r))
  celdas <- terra::cellFromXY(r, terra::crds(pts_r))
  if (anyNA(celdas)) {
    stop("Una o mas estaciones estan fuera de la cobertura de ", basename(f),
         ". Descarga NetCDF para las nuevas coordenadas.")
  }

  kelvin <- as.numeric(terra::extract(r, pts_r, ID = FALSE)[[1]])
  data.frame(
    Fecha = fechas_archivos[i],
    Estacion = estaciones$Estacion,
    Tipo = tipos[i],
    Temperatura_C = kelvin - 273.15,
    Archivo = basename(f),
    stringsAsFactors = FALSE
  )
}))

tabla_temperatura <- tabla_temperatura[
  order(tabla_temperatura$Fecha, tabla_temperatura$Estacion,
        tabla_temperatura$Tipo), ]
rownames(tabla_temperatura) <- NULL

# ------------------------------------------------------------------
# 4. TABLA COMPARATIVA Y VERIFICACIONES
# ------------------------------------------------------------------
tabla_comparativa <- reshape(
  tabla_temperatura[c("Fecha", "Estacion", "Tipo", "Temperatura_C")],
  idvar = c("Fecha", "Estacion"), timevar = "Tipo", direction = "wide"
)
names(tabla_comparativa) <- sub("^Temperatura_C\\.", "T_",
                                names(tabla_comparativa))

requeridas <- c("T_Min", "T_Mean", "T_Max")
if (!all(requeridas %in% names(tabla_comparativa))) {
  stop("No se pudieron construir las tres columnas de temperatura.")
}
tabla_comparativa <- tabla_comparativa[
  order(tabla_comparativa$Fecha, tabla_comparativa$Estacion), ]
rownames(tabla_comparativa) <- NULL

esperados <- length(fechas_esperadas) * nrow(estaciones)
if (nrow(tabla_temperatura) != esperados * 3 ||
    nrow(tabla_comparativa) != esperados ||
    anyDuplicated(tabla_temperatura[c("Fecha", "Estacion", "Tipo")]) > 0 ||
    anyNA(tabla_comparativa)) {
  stop("Hay registros duplicados, incompletos o valores NA.")
}

incoherentes <- with(tabla_comparativa,
                    T_Min > T_Mean | T_Mean > T_Max)
if (any(incoherentes)) {
  stop("Inconsistencias termicas: algunas temperaturas no cumplen Min <= Mean <= Max.")
}
tabla_comparativa$Amplitud_C <- tabla_comparativa$T_Max - tabla_comparativa$T_Min

# ------------------------------------------------------------------
# 5. EXPORTAR
# ------------------------------------------------------------------
dir.create(dirname(salida_larga), recursive = TRUE, showWarnings = FALSE)
write.csv(tabla_temperatura, salida_larga, row.names = FALSE,
          fileEncoding = "UTF-8")
write.csv(tabla_comparativa, salida_comparativa, row.names = FALSE,
          fileEncoding = "UTF-8")

cat("AgERA5 - temperatura diaria\n")
cat("Dias:", length(fechas_esperadas),
    "| Estaciones:", nrow(estaciones),
    "| Filas comparativas:", nrow(tabla_comparativa), "\n")
cat("CSV:", salida_larga, "\nCSV:", salida_comparativa, "\n")
print(head(tabla_comparativa))
