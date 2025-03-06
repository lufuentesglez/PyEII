#' Carga de librerías necesarias
#'
#' @description
#' Esta función carga las librerías necesarias para el análisis estadístico.
#' Las librerías incluyen funciones para cálculos de asimetría, curtosis, ajuste de distribuciones,
#' intervalos de confianza, pruebas no paramétricas, bootstrap y estimación bayesiana.
#'
#' @details
#' Las librerías cargadas son:
#' \itemize{
#'   \item \code{e1071}: Para cálculos de asimetría y curtosis.
#'   \item \code{MASS}: Para ajuste de distribuciones (por ejemplo, \code{fitdistr}).
#'   \item \code{DescTools}: Para intervalos de confianza y pruebas estadísticas.
#'   \item \code{PropCIs}: Para intervalos de confianza de proporciones.
#'   \item \code{rcompanion}: Para pruebas no paramétricas.
#'   \item \code{EstimationTools}: Para estimaciones puntuales y por intervalos.
#'   \item \code{boot}: Para técnicas de bootstrap.
#'   \item \code{BayesFactor}: Para estimación bayesiana.
#' }
#'
#' @return
#' No devuelve ningún valor. Solo carga las librerías en el entorno de R.
#'
#'
#' @import e1071
#' @import MASS
#' @import DescTools
#' @import PropCIs
#' @import rcompanion
#' @import EstimationTools
#' @import boot
#' @import BayesFactor
#' @export
load_libraries <- function() {
  library(e1071)
  library(MASS)
  library(DescTools)
  library(PropCIs)
  library(rcompanion)
  library(EstimationTools)
  library(boot)
  library(BayesFactor)
}


#' Configuración inicial del entorno
#'
#' @description
#' Configura el entorno para garantizar reproducibilidad y definir parámetros comunes.
#'
#' @param seed Valor de la semilla para reproducibilidad (por defecto: 2023).
#' @param bootR Número de réplicas para métodos de bootstrap (por defecto: 1000).
#'
#' @details
#' Esta función fija la semilla del generador de números aleatorios (\code{set.seed}) y define
#' el número de réplicas para métodos de bootstrap.
#'
#' @return
#' No devuelve ningún valor. Solo configura el entorno.
#'
#' @export
setup_environment <- function(seed = 2023, bootR = 1000) {
  set.seed(seed)
  bootR <- bootR
}


#' Definición de datos
#'
#' @description
#' Define los datos de las variables a analizar.
#'
#' @details
#' Esta función crea una lista con los siguientes elementos:
#' \itemize{
#'   \item \code{Sample1_Data.Carbohydrate}: Un vector numérico que contiene valores de carbohidratos.
#'   \item \code{Sample2_Data.Carbohydrate}: Un vector numérico que contiene valores de carbohidratos no pareados.
#'   \item \code{Sample1_Data.Sugar.Total}: Un vector categórico que contiene categorías de azúcar (S1, S2, etc.).
#'   \item \code{Sample2_Data.Sugar.Total}: Un vector categórico que contiene categorías de azúcar no pareadas.
#'   \item \code{Data.Protein}: Un vector numérico que contiene valores de proteínas.
#'   \item \code{Data.Fat.Total.Lipid}: Un vector numérico que contiene valores de lípidos totales.
#'   \item \code{Data.Vitamins.Vitamin.B12}: Un vector numérico que contiene valores de vitamina B12.
#'   \item \code{Data.Major.Minerals.Magnesium}: Un vector numérico que contiene valores de magnesio.
#'   \item \code{Medias_Muestrales_30}: Un vector numérico que contiene las medias muestrales de 30 muestras de Data.Major.Minerals.Magnesium.
#'   \item \code{Proporciones_Muestrales_30}: Un vector numérico que contiene las proporciones muestrales de 30 muestras de Data.Sugar.Total == "S1".
#' }
#'
#' @return
#' Una lista con los siguientes elementos:
#' \itemize{
#'   \item \code{Sample1_Data.Carbohydrate}: Vector numérico.
#'   \item \code{Sample2_Data.Carbohydrate}: Vector numérico.
#'   \item \code{Sample1_Data.Sugar.Total}: Vector categórico.
#'   \item \code{Sample2_Data.Sugar.Total}: Vector categórico.
#'   \item \code{Data.Protein}: Vector numérico.
#'   \item \code{Data.Fat.Total.Lipid}: Vector numérico.
#'   \item \code{Data.Vitamins.Vitamin.B12}: Vector numérico.
#'   \item \code{Data.Major.Minerals.Magnesium}: Vector numérico.
#'   \item \code{Medias_Muestrales_30}: Vector numérico.
#'   \item \code{Proporciones_Muestrales_30}: Vector numérico.
#' }
#'
#' @export
define_data <- function() {
  # Datos de Sample1_Data.Carbohydrate
  Sample1_Data.Carbohydrate <- c(0, 0.5828941, 0.289542, 0.5439407, 0.6498322, 0.553541, 0.5905475, 0.7167677, 0.5992276, 0.7148335, 0.5817453, 0.4649249, 0.6743769, 0.6918767, 0.4309532, 0.6162988, 0.7012436, 0, 0.7230874, 0.6955362)
  
  # Datos de Sample2_Data.Carbohydrate
  Sample2_Data.Carbohydrate <- c(0, 0.6409975, 0, 0.7080199, 0.6485136, 0.7224053, 0, 0.5394527, 0.5844252, 0.6068672, 0, 0.5305104, 0.4786956, 0.4987135, 0.7186016, 0.5772132, 0, 0.4185277, 0.5630961, 0.6011605)
  
  # Datos de Sample1_Data.Sugar.Total
  Sample1_Data.Sugar.Total <- c("S1", "S2", "S1", "S2", "S3", "S1", "S1", "S1", "S1", "S1", "S1", "S1", "S4", "S3", "S2", "S1", "S2", "S1", "S1", "S5")
  
  # Datos de Sample2_Data.Sugar.Total
  Sample2_Data.Sugar.Total <- c("S1", "S1", "S1", "S1", "S4", "S3", "S1", "S2", "S1", "S1", "S1", "S1", "S2", "S1", "S1", "S2", "S1", "S2", "S1", "S1")
  
  # Datos de Data.Protein
  Data.Protein <- c(0.6492829, 0.3668104, 0.6144942, 0.5358623, 0.424903, 0.6292939, 0.5046221, 0.5447532, 0.3177311, 0.4338655, 0.4651436, 0.2180955, 0.4273604, 0.5287393, 0.3821949, 0.3632984, 0.5250996, 0.6268681, 0.5383786, 0.4114478)
  
  # Datos de Data.Fat.Total.Lipid
  Data.Fat.Total.Lipid <- c(2.652537, 1.671473, 2.085672, 1.998774, 2.265921, 2.487404, 2.674149, 2.823757, 2.585506, 3.301009, 1.82777, 0.5766134, 2.685123, 1.335001, 1.495149, 1.302913, 1.558145, 2.095561, 2.453588, 3.13201)
  
  # Datos de Data.Vitamins.Vitamin.B12
  Data.Vitamins.Vitamin.B12 <- c(1.238374, 0.1310283, 0.7839015, 0.1906204, 0.0861777, 0.9082586, 0.4510756, 0, 0, 0, 0.2311117, 0.05826891, 0.3364722, 0.2623643, 0.3074847, 0, 0.06765865, 0.3364722, 0, 0)
  
  # Datos de Data.Major.Minerals.Magnesium
  Data.Major.Minerals.Magnesium <- c(3.178054, 2.833213, 2.772589, 2.833213, 2.833213, 2.944439, 2.772589, 5.105945, 3.178054, 3.828641, 2.639057, 1.94591, 3.178054, 3.828641, 1.791759, 3.526361, 3.091042, 3.178054, 3.178054, 3.610918)
  
  # Medias muestrales de Data.Major.Minerals.Magnesium
  Medias_Muestrales_30 <- c(2.161451, 1.929523, 2.212274, 2.160833, 1.966287, 2.183474, 1.696679, 2.100056, 2.269257, 1.742109, 2.347255, 1.529998, 1.767131, 2.375847, 2.283483, 1.979091, 1.957344, 2.099081, 2.087817, 1.945333, 2.049644, 2.191166, 2.228053, 1.975063, 2.039961, 1.847781, 2.16524, 1.989566, 1.858383, 1.815181)
  
  # Proporciones muestrales de Data.Sugar.Total == "S1"
  Proporciones_Muestrales_30 <- c(0.6, 0.7, 0.75, 0.65, 0.7, 0.55, 0.65, 0.45, 0.65, 0.7, 0.65, 0.6, 0.75, 0.5, 0.6, 0.55, 0.6, 0.45, 0.6, 0.6, 0.6, 0.8, 0.55, 0.4, 0.75, 0.4, 0.9, 0.6, 0.5, 0.55)
  
  # Retorna una lista con todos los datos
  return(list(
    Sample1_Data.Carbohydrate = Sample1_Data.Carbohydrate,
    Sample2_Data.Carbohydrate = Sample2_Data.Carbohydrate,
    Sample1_Data.Sugar.Total = Sample1_Data.Sugar.Total,
    Sample2_Data.Sugar.Total = Sample2_Data.Sugar.Total,
    Data.Protein = Data.Protein,
    Data.Fat.Total.Lipid = Data.Fat.Total.Lipid,
    Data.Vitamins.Vitamin.B12 = Data.Vitamins.Vitamin.B12,
    Data.Major.Minerals.Magnesium = Data.Major.Minerals.Magnesium,
    Medias_Muestrales_30 = Medias_Muestrales_30,
    Proporciones_Muestrales_30 = Proporciones_Muestrales_30
  ))
}
