# ==============================================================================
# Script: 02_eda_tables.R
# Objetivo: Cálculo de medidas descritivas e geração de tabelas para a 
# análise exploratória dos retornos mensais (em %)
# Disciplina: EST074 - Análise de Regressão (UFJF)
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(knitr)
  library(kableExtra)
})

#' Calcula medidas descritivas solicitadas para as ações e o índice S&P 500
#'
#' @param data data.frame contendo os retornos mensais (em %)
#' @param vars Vetor de nomes dos ativos a analisar
#' @return Um data.frame com as estatísticas descritivas ordenadas por Desvio Padrão
compute_descriptive_stats <- function(data, vars = c(
  "Sandisk", "Qualcomm", "Caterpillar", "McDonald's",
  "Exxon Mobil", "Microsoft", "Procter & Gamble", "Johnson & Johnson", "S&P 500"
)) {
  results <- lapply(vars, function(var_name) {
    x <- data[[var_name]]
    x <- x[!is.na(x)]
    
    q <- stats::quantile(x, probs = c(0.25, 0.5, 0.75))
    m <- mean(x)
    s <- sd(x)
    cv <- (s / abs(m)) * 100
    
    data.frame(
      Ativo = var_name,
      Minimo = min(x),
      Q1 = unname(q[1]),
      Mediana = unname(q[2]),
      Media = m,
      Q3 = unname(q[3]),
      Maximo = max(x),
      Desvio_Padrao = s,
      CV_Perc = cv,
      stringsAsFactors = FALSE
    )
  })
  
  df_summary <- do.call(rbind, results)
  rownames(df_summary) <- NULL
  return(df_summary)
}

#' Formata a tabela descritiva para saída profissional no relatório (LaTeX)
#'
#' @param desc_df data.frame retornado por compute_descriptive_stats
#' @param caption Título da tabela
#' @return Objeto kable formatado
format_descriptive_table <- function(desc_df, 
                                     caption = "Medidas descritivas dos retornos totais mensais em percentual (\\%) para as oito ações e o índice S\\&P 500 ($n = 36$ meses)") {
  display_df <- desc_df %>%
    dplyr::rename(
      `Ativo / Índice` = Ativo,
      `Mínimo` = Minimo,
      `1º Quartil` = Q1,
      `Mediana` = Mediana,
      `Média` = Media,
      `3º Quartil` = Q3,
      `Máximo` = Maximo,
      `Desvio Padrão` = Desvio_Padrao,
      `CV (%)` = CV_Perc
    )
  
  knitr::kable(
    display_df,
    format = "latex",
    booktabs = TRUE,
    row.names = FALSE,
    digits = 2,
    caption = caption,
    align = c("l", rep("r", ncol(display_df) - 1))
  ) %>%
    kableExtra::kable_styling(
      latex_options = c("striped", "hold_position"),
      font_size = 9
    )
}

#' Identifica observações atípicas (outliers) pelo critério 1,5 x IQR
#'
#' @param data data.frame original
#' @param vars Vetor de nomes de variáveis
#' @return data.frame com os outliers identificados
identify_outliers <- function(data, vars = c(
  "Sandisk", "Qualcomm", "Caterpillar", "McDonald's",
  "Exxon Mobil", "Microsoft", "Procter & Gamble", "Johnson & Johnson", "S&P 500"
)) {
  outliers_list <- list()
  
  for (v in vars) {
    x <- data[[v]]
    q1 <- stats::quantile(x, 0.25, na.rm = TRUE)
    q3 <- stats::quantile(x, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower_bound <- q1 - 1.5 * iqr
    upper_bound <- q3 + 1.5 * iqr
    
    idx <- which(x < lower_bound | x > upper_bound)
    if (length(idx) > 0) {
      for (i in idx) {
        tipo <- ifelse(x[i] < lower_bound, "Inferior", "Superior")
        limite <- ifelse(tipo == "Inferior", lower_bound, upper_bound)
        outliers_list[[length(outliers_list) + 1]] <- data.frame(
          Ativo = v,
          Mes = format(data$Month[i], "%m/%Y"),
          Valor = x[i],
          Tipo = tipo,
          Limite_IQR = unname(limite),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  df_outliers <- do.call(rbind, outliers_list)
  rownames(df_outliers) <- NULL
  return(df_outliers)
}

#' Formata tabela de outliers detectados para LaTeX
#'
#' @param df_outliers data.frame retornado por identify_outliers
#' @param caption Legenda da tabela
#' @return Objeto kable formatado
format_outliers_table <- function(df_outliers, 
                                  caption = "Retornos mensais atípicos identificados pelo critério de $1{,}5 \\times \\text{IQR}$") {
  display_df <- df_outliers %>%
    dplyr::rename(
      `Ativo / Índice` = Ativo,
      `Mês/Ano` = Mes,
      `Retorno Observado (%)` = Valor,
      `Tipo de Outlier` = Tipo,
      `Barreira IQR (%)` = Limite_IQR
    )
  
  knitr::kable(
    display_df,
    format = "latex",
    booktabs = TRUE,
    row.names = FALSE,
    digits = 2,
    caption = caption,
    align = c("l", "c", "r", "c", "r")
  ) %>%
    kableExtra::kable_styling(
      latex_options = c("striped", "hold_position"),
      font_size = 9
    )
}
