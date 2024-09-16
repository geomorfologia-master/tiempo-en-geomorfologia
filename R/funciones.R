estilo_kable <- function(df, titulo = '', cubre_anchura = F, formato = 'latex',
                         tamano_fuente = 8, no_na = T, tabla_grande = F,
                         anchuras = NULL) {
  df %>% kable(format = formato, escape = F, booktabs = T,
               digits = 2, caption = titulo, longtable = tabla_grande) %>%
    kable_styling(
      bootstrap_options = c("hover", "condensed"),
      latex_options = c("striped", "repeat_header", "HOLD_position"),
      full_width = cubre_anchura, position = "center",
      font_size = tamano_fuente) %>% 
    row_spec(0, bold = TRUE) %>% 
    {if(!is.null(anchuras)) column_spec(kable_input = ., column = anchuras[[1]], width = anchuras[[2]]) else .} %>% 
    {if(no_na) gsub(' NA |NA| NaN |NaN', '', .) else .}
}

estilo_kable_corto <- function(df, titulo = '', cubre_anchura = F, formato = 'latex') {
  df %>%
    kable(format = formato, escape = F, booktabs = T,
          digits = 2, caption = titulo) %>%
    kable_styling(bootstrap_options = c("hover", "condensed"),
                  full_width = cubre_anchura)
}

length_no_na <- function(x) {
  sum(!is.na(x))
}

media_mas_menos_error <- function(x) {
  if (length(x) > 1) {
    m <- mean(x, na.rm = TRUE)
    se <- sd(x, na.rm = TRUE) / sqrt(length(na.omit(x)))
    return(paste0(round(m, 2), " ± ", round(se, 2)))
  } else {
    return(NA)
  }
}

intervalo_confianza <- function(x, conf = 0.95) {
  if (length(x) > 1) {
    m <- mean(x, na.rm = TRUE)
    se <- sd(x, na.rm = TRUE) / sqrt(length(na.omit(x)))
    alpha <- 1 - conf
    error_margin <- qt(1 - alpha / 2, df = length(na.omit(x)) - 1) * se
    lower_bound <- m - error_margin
    upper_bound <- m + error_margin
    return(paste0("(", round(lower_bound, 2), ", ", round(upper_bound, 2), ")"))
  } else {
    return(NA)
  }
}

resumenes_est <- skim_with(
  base = sfl(
    Perdidos = n_missing,
    `Porcentaje completos` = complete_rate
  ),
  numeric = sfl(
    N = length_no_na,
    Mínimo = function(x) min(x, na.rm = T),
    Media = function(x) mean(x, na.rm = T), 
    Mediana = function(x) median(x, na.rm = T),
    Máximo = function(x) max(x, na.rm = T),
    Desviación = function(x) sd(x, na.rm = T),
    `Media±error` = media_mas_menos_error,
    `Intervalo de confianza` = intervalo_confianza,
    `P-valor SW` = function(x) shapiro.test(x)$p.value,
    Histograma = inline_hist),
  append = F
)

# Función para añadir resultados al tibble
anadir_resul_supue_anali_asoc <- function(analisis, item, valor, digitos = 6, formato = 'e', umbral_significancia = umbral_sign) {
  valor_format <- formatC(valor, format = formato, digits = digitos)
  sign <- ifelse(valor < umbral_significancia & grepl('p-valor', item, ignore.case = T), '*', '')
  resul_supue_anali_asoc <<- resul_supue_anali_asoc %>%
    add_row(Análisis = analisis, Ítem = item, Valor = valor_format, Sign = sign)
}

lm_eqn <- function(df, mi_formula){
  m <- lm(formula = mi_formula, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}
