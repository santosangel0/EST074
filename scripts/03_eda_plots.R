# ==============================================================================
# Script: 03_eda_plots.R
# Objetivo: Funções de visualização gráfica para análise exploratória
# Disciplina: EST074 - Análise de Regressão (UFJF)
# ==============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(patchwork)
})

# Tema padrão para gráficos acadêmicos
theme_academic <- function(base_size = 10) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 1, hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = base_size - 1, hjust = 0.5, color = "gray30"),
      axis.title = ggplot2::element_text(face = "bold", size = base_size),
      axis.text = ggplot2::element_text(size = base_size - 1, color = "black"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = "gray80", fill = NA, linewidth = 0.5),
      strip.background = ggplot2::element_rect(fill = "gray95", color = "gray80"),
      strip.text = ggplot2::element_text(face = "bold", size = base_size - 1)
    )
}

#' Gera histogramas com curva de densidade para variáveis quantitativas
#'
#' @param data data.frame com os dados
#' @param vars Vetor de variáveis numéricas
#' @return Objeto ggplot2
plot_histograms <- function(data, vars = c("Score", "GDP", "Social", "Healthy", 
                                          "Freedom", "Generosity", "Corruption")) {
  df_long <- data %>%
    dplyr::select(dplyr::all_of(vars)) %>%
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Variavel", values_to = "Valor")
  
  ggplot2::ggplot(df_long, ggplot2::aes(x = Valor)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), 
                            bins = 15, fill = "#2C3E50", color = "white", alpha = 0.75) +
    ggplot2::geom_density(color = "#E74C3C", linewidth = 0.8) +
    ggplot2::facet_wrap(~ Variavel, scales = "free", ncol = 3) +
    ggplot2::labs(x = "Valor", y = "Densidade") +
    theme_academic(base_size = 9)
}

#' Gera box-plots para identificação visual de mediana, dispersão e outliers
#'
#' @param data data.frame com os dados
#' @param vars Vetor de variáveis numéricas
#' @return Objeto ggplot2
plot_boxplots <- function(data, vars = c("Score", "GDP", "Social", "Healthy", 
                                        "Freedom", "Generosity", "Corruption")) {
  df_long <- data %>%
    dplyr::select(dplyr::all_of(vars)) %>%
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Variavel", values_to = "Valor")
  
  ggplot2::ggplot(df_long, ggplot2::aes(x = Variavel, y = Valor, fill = Variavel)) +
    ggplot2::geom_boxplot(outlier.color = "#E74C3C", outlier.size = 2, alpha = 0.7, show.legend = FALSE) +
    ggplot2::scale_fill_brewer(palette = "Blues") +
    ggplot2::facet_wrap(~ Variavel, scales = "free", ncol = 4) +
    ggplot2::labs(x = "Variável", y = "Valor") +
    theme_academic(base_size = 9) +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())
}

#' Gera diagramas de dispersão entre a variável resposta Score e as preditoras
#'
#' @param data data.frame com os dados
#' @param y_var Nome da variável resposta
#' @param x_vars Vetor de variáveis preditoras
#' @return Objeto ggplot2
plot_bivariate_relationships <- function(data, y_var = "Score", 
                                        x_vars = c("GDP", "Social", "Healthy", 
                                                   "Freedom", "Generosity", "Corruption")) {
  df_long <- data %>%
    dplyr::select(dplyr::all_of(c(y_var, x_vars))) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(x_vars), names_to = "Preditora", values_to = "Valor_X")
  
  ggplot2::ggplot(df_long, ggplot2::aes(x = Valor_X, y = .data[[y_var]])) +
    ggplot2::geom_point(color = "#34495E", alpha = 0.65, size = 1.6) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, color = "#2980B9", fill = "#BDC3C7", linewidth = 0.8) +
    ggplot2::facet_wrap(~ Preditora, scales = "free_x", ncol = 3) +
    ggplot2::labs(
      x = "Valor da Variável Explicativa",
      y = paste("Pontuação de Felicidade (", y_var, ")", sep = "")
    ) +
    theme_academic(base_size = 9)
}

#' Gera matriz de correlação de Pearson com anotações numéricas
#'
#' @param data data.frame com os dados
#' @param vars Vetor de variáveis para a correlação
#' @return Objeto ggplot2
plot_correlation_matrix <- function(data, vars = c("Score", "GDP", "Social", "Healthy", 
                                                  "Freedom", "Generosity", "Corruption")) {
  cor_mat <- stats::cor(data[vars], use = "complete.obs")
  cor_df <- as.data.frame(as.table(cor_mat))
  names(cor_df) <- c("Var1", "Var2", "Correlacao")
  
  ggplot2::ggplot(cor_df, ggplot2::aes(x = Var1, y = Var2, fill = Correlacao)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", Correlacao)), size = 3, color = "black") +
    ggplot2::scale_fill_gradient2(low = "#E74C3C", mid = "#FFFFFF", high = "#2980B9", midpoint = 0, limit = c(-1, 1)) +
    ggplot2::labs(x = "", y = "", fill = "r de Pearson") +
    theme_academic(base_size = 9) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}
