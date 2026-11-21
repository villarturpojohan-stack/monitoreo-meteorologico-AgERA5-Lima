# Extraccion de velocidad media DIARIA del viento a 10 m (AgERA5)
# Area del ejercicio: Lima, Peru. Rutas, fechas y estaciones editables.

if (!requireNamespace("terra", quietly = TRUE)) {
  stop("Instala el paquete: install.packages('terra')")
}

# ------------------------------------------------------------------
# 1. CONFIGURACION: editar para otras carpetas o fechas
# ------------------------------------------------------------------
fecha_inicio <- as.Date("2026-09-27")
fecha_fin <- as.Date("2026-10-27")

carpetas_nc <- c(
  "datos/agera5/viento/sep",
  "datos/agera5/viento/oct"
)
archivo_estaciones <- "config/estaciones.csv"
archivo_salida <- "resultados/Viento_AgERA5_diario.csv"

if (fecha_fin < fecha_inicio) stop("La fecha final es anterior a la inicial.")
if (!file.exists(archivo_estaciones)) {
  stop("No encuentro config/estaciones.csv. Abre RStudio en la raiz del repositorio.")
}

# Para cambiar ubicaciones basta editar config/estaciones.csv
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
# 2. BUSCAR ARCHIVOS Y LEER LA FECHA DEL NOMBRE
# ------------------------------------------------------------------
if (any(!dir.exists(carpetas_nc))) {
  stop("Faltan carpetas de AgERA5: ",
       paste(carpetas_nc[!dir.exists(carpetas_nc)], collapse = ", "))
}
archivos <- unlist(lapply(carpetas_nc, function(carpeta) {
  list.files(carpeta, pattern = "\\.nc$", full.names = TRUE,
             ignore.case = TRUE)
}), use.names = FALSE)
if (!length(archivos)) stop("No se encontraron archivos .nc de viento AgERA5.")

leer_fecha <- function(nombre) {
  texto <- regmatches(nombre, regexpr("20[0-9]{6}", nombre))
  if (length(texto) != 1) stop("No encuentro fecha YYYYMMDD en ", nombre)
  fecha <- as.Date(texto, format = "%Y%m%d")
  if (is.na(fecha)) stop("Fecha invalida en ", nombre)
  fecha
}

fechas_archivos <- as.Date(vapply(basename(archivos), function(n) {
  as.character(leer_fecha(n))
}, character(1)))
incluidos <- fechas_archivos >= fecha_inicio & fechas_archivos <= fecha_fin
archivos <- archivos[incluidos]
fechas_archivos <- fechas_archivos[incluidos]

fechas_esperadas <- seq(fecha_inicio, fecha_fin, by = "day")
faltantes <- as.Date(setdiff(as.character(fechas_esperadas),
                            as.character(fechas_archivos)))
if (length(faltantes)) {
  stop("Faltan archivos de viento para: ",
       paste(format(faltantes, "%Y-%m-%d"), collapse = ", "))
}
if (anyDuplicated(fechas_archivos) > 0) {
  stop("Existen dos archivos de viento para una misma fecha.")
}

# ------------------------------------------------------------------
# 3. EXTRAER VALORES EN CADA ESTACION
# ------------------------------------------------------------------
tabla_viento <- do.call(rbind, lapply(seq_along(archivos), function(i) {
  f <- archivos[i]
  r <- terra::rast(f)
  if (terra::nlyr(r) != 1) stop("Se esperaba una sola capa: ", basename(f))

  pts_r <- terra::project(puntos, terra::crs(r))
  celdas <- terra::cellFromXY(r, terra::crds(pts_r))
  if (anyNA(celdas)) {
    stop("Una o mas estaciones estan fuera de la cobertura de ", basename(f),
         ". Descarga NetCDF para las nuevas coordenadas.")
  }

  valores <- terra::extract(r, pts_r, ID = FALSE)[[1]]
  data.frame(
    Fecha = fechas_archivos[i],
    Estacion = estaciones$Estacion,
    Variable = "Velocidad media del viento a 10 m",
    Valor = as.numeric(valores),
    Unidad = "m/s",
    Archivo = basename(f),
    stringsAsFactors = FALSE
  )
}))

tabla_viento <- tabla_viento[order(tabla_viento$Fecha, tabla_viento$Estacion), ]
rownames(tabla_viento) <- NULL

# ------------------------------------------------------------------
# 4. COMPROBAR COMPLETITUD Y EXPORTAR
# ------------------------------------------------------------------
esperados <- length(fechas_esperadas) * nrow(estaciones)
duplicados <- duplicated(tabla_viento[c("Fecha", "Estacion")])
if (nrow(tabla_viento) != esperados || any(duplicados) ||
    anyNA(tabla_viento$Valor)) {
  stop("Datos incompletos, duplicados o con NA. Revisa los archivos de entrada.")
}

dir.create(dirname(archivo_salida), recursive = TRUE, showWarnings = FALSE)
write.csv(tabla_viento, archivo_salida, row.names = FALSE,
          fileEncoding = "UTF-8")

cat("AgERA5 - viento diario\n")
cat("Dias:", length(fechas_esperadas),
    "| Estaciones:", nrow(estaciones),
    "| Registros:", nrow(tabla_viento), "\n")
cat("CSV:", archivo_salida, "\n")
print(head(tabla_viento))
