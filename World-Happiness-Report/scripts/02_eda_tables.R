# ==============================================================================
# Script: 02_eda_tables.R
# Objetivo: Cálculo de medidas descritivas e geração de tabelas para a 
# análise exploratória
# Disciplina: EST074 - Análise de Regressão (UFJF)
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(knitr)
  library(kableExtra)
})

#' Calcula medidas descritivas solicitadas para variáveis quantitativas
#'
#' @param data data.frame contendo as variáveis
#' @param vars Vetor de nomes de variáveis a analisar
#' @return Um data.frame com as estatísticas descritivas
compute_descriptive_stats <- function(data, vars = c("Score", "GDP", "Social", "Healthy", 
                                                    "Freedom", "Generosity", "Corruption")) {
  results <- lapply(vars, function(var_name) {
    x <- data[[var_name]]
    x <- x[!is.na(x)]
    
    q <- stats::quantile(x, probs = c(0.25, 0.5, 0.75))
    m <- mean(x)
    s <- sd(x)
    cv <- (s / m) * 100
    
    data.frame(
      Variavel = var_name,
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
  return(df_summary)
}

#' Formata a tabela descritiva para saída profissional no relatório (LaTeX)
#'
#' @param desc_df data.frame retornado por compute_descriptive_stats
#' @param caption Título da tabela
#' @return Objeto kable formatado
format_descriptive_table <- function(desc_df, 
                                     caption = "Medidas descritivas das variáveis do estudo (n = 146 países)") {
  # Renomeia colunas para exibição formal em português
  display_df <- desc_df %>%
    dplyr::rename(
      `Variável` = Variavel,
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
    digits = 3,
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
identify_outliers <- function(data, vars = c("Score", "GDP", "Social", "Healthy", 
                                            "Freedom", "Generosity", "Corruption")) {
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
          Variavel = v,
          Pais = data$Country[i],
          Valor = x[i],
          Tipo = tipo,
          Limite_IQR = limite,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  if (length(outliers_list) == 0) {
    return(data.frame(Mensagem = "Nenhum outlier identificado pelo critério 1,5 x IQR"))
  }
  
  df_outliers <- do.call(rbind, outliers_list)
  return(df_outliers)
}

#' Formata tabela de outliers detectados para LaTeX
#'
#' @param df_outliers data.frame retornado por identify_outliers
#' @param caption Legenda da tabela
#' @return Objeto kable formatado
format_outliers_table <- function(df_outliers, 
                                  caption = "Identificação de observações atípicas pelo critério 1,5 x IQR") {
  display_df <- df_outliers %>%
    dplyr::rename(
      `Variável` = Variavel,
      `País` = Pais,
      `Valor Observado` = Valor,
      `Tipo de Outlier` = Tipo,
      `Limite (1,5 x IQR)` = Limite_IQR
    )
  
  knitr::kable(
    display_df,
    format = "latex",
    booktabs = TRUE,
    row.names = FALSE,
    digits = 3,
    caption = caption,
    align = c("l", "l", "r", "c", "r")
  ) %>%
    kableExtra::kable_styling(
      latex_options = c("striped", "hold_position"),
      font_size = 9
    )
}
