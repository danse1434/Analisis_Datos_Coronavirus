#-------------------------------------------------------------------------------#
#' Función de parámetros auxiliares para añadir a gráficos
#' @param ls lista de selección de países, tiene que tener un elemento llamado 
#' "orig" y otro elemento llamado "trad"
#' @param pal color de paleta a utilizar en el color
#' @param cond condiciones de 
#' 
#' @export lista de objetos *ggproto* con especificaciones de formato gráfico
#' @examples
#' ggplot() + aux_param(c1, "Dark2") 
#' 
aux_param <- function(ls, pal="Dark2", cond=1) {
  # Configuración 
  x <- list(breaks = ls$orig, labels = ls$trad, name='')
  
  # Configuración guides en todas las características
  y <- list(ncol = 2, default.unit="inch")

  # Utilización de paleta brewer o paleta viridis
  m <- if (cond==1|cond=='brewer') {
    eval( expr( scale_color_brewer(!!!x, palette = pal)  ))
  } else if (cond==2|cond=='viridis') {
    eval( expr(scale_color_viridis_d(!!!x, option = "viridis")))  
  } else {
    stop("Condición no válida")
  }
  
  n <- list(
    # Evaluación tardía
    eval( expr( scale_linetype_discrete(!!!x)            )),
    eval( expr( scale_shape_discrete(!!!x)               )),
    eval( expr( guides(colour   = guide_legend(!!!y),
                       linetype = guide_legend(!!!y),
                       shape    = guide_legend(!!!y))    )), 
    # Otras configuraciones
    coord_cartesian(xlim = c(0, 80), ylim = c(0, 6.0E4)),
    xlab("Días desde primer reporte"),
    ylab("Casos reportados"),
    theme_bw(), 
    theme(
      legend.position = c(0.30, 0.75),
      legend.title = element_blank(),
      legend.spacing.y = unit(1.0, 'mm'),
      legend.text = element_text(size = 8, margin = margin(t = 0.1)),
      legend.margin = margin(0.1, 0.1, 0.1, 0.1))
  )
  
  n <- append(n, m, after = 1)
  
  return(n)
}

#-------------------------------------------------------------------------------#
#' Función de tipo exponencial parametrizada en tiempos de vida media
#' @param x Archivo de datos que contiene datos de dd1 (días desde inicio 
#' de epidemia), y total_cases (casos totales)
#' @return objeto de tipo *nls* con resultados de regresión no lineal. 
#'
#' @examples
#' nls_function(data1)
#' datalist[['Chile']] %>% nls_function(.)
#' 
nls_function <- function(x) {
  out <- tryCatch({
    nls(Casos_Totales ~ exp(log(2) * dd1 / thalf),
        data = x,
        start = list(thalf = 5))
  },
  error = function(cond) {
    message(cond)
    return(NA)
  },
  warning = function(cond) {
    message(cond)
    return(NULL)
  })
  return(out)
}

#-------------------------------------------------------------------------------#
#' Función de adición de líneas guía de tiempo de duplicación
#' @param t tiempo de duplicación a graficar
#' @return lista con objeto stat_function que depende del eje x
#' @examples
#' ggplot() + ... + line_t(1)
#' 
line_t <- function(t) {
  list(stat_function(fun = function(x) {2 ^ (x / t)},
                     inherit.aes = F, lty = 'dotted', colour = 'gray1'))
}

#-------------------------------------------------------------------------------#
#' Función modificada - adición de líneas guía de tiempo de duplicación
#' @param t tiempo de duplicación a graficar
#' @return lista con objeto stat_function que depende del eje x, está 
#' multiplicado por 100 como corrección.
#' @examples
#' ggplot() + ... + line_t1(1)
#' 
line_t1 <- function(t) {
  list(stat_function(fun = function(x) {1E2*2 ^ ((x) / t)},
                     inherit.aes = F, lty = 'dotted', colour = 'gray1'))
}
