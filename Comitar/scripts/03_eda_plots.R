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
theme_academic <- function(base_size = 9) {
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

#' Gera histogramas com curva de densidade para os retornos mensais (em %)
#'
#' @param data data.frame com os dados
#' @param vars Vetor de variáveis numéricas
#' @return Objeto ggplot2
plot_histograms <- function(data, vars = c(
  "Sandisk", "Qualcomm", "Caterpillar", "McDonald's",
  "Exxon Mobil", "Microsoft", "Procter & Gamble", "Johnson & Johnson", "S&P 500"
)) {
  df_long <- data %>%
    dplyr::select(dplyr::all_of(vars)) %>%
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Ativo", values_to = "Retorno") %>%
    dplyr::mutate(Ativo = factor(Ativo, levels = vars))
  
  ggplot2::ggplot(df_long, ggplot2::aes(x = Retorno)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), 
                            bins = 10, fill = "#2C3E50", color = "white", alpha = 0.78) +
    ggplot2::geom_density(color = "#E74C3C", linewidth = 0.8) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.5) +
    ggplot2::facet_wrap(~ Ativo, scales = "free", ncol = 3) +
    ggplot2::labs(x = "Retorno Total Mensal (%)", y = "Densidade") +
    theme_academic(base_size = 9)
}

#' Gera box-plots comparativos dos retornos mensais ordenados por volatilidade
#'
#' @param data data.frame com os dados
#' @param vars Vetor de variáveis numéricas
#' @return Objeto ggplot2
plot_boxplots <- function(data, vars = c(
  "S&P 500", "Johnson & Johnson", "Procter & Gamble", "Microsoft",
  "Exxon Mobil", "McDonald's", "Caterpillar", "Qualcomm", "Sandisk"
)) {
  df_long <- data %>%
    dplyr::select(dplyr::all_of(vars)) %>%
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Ativo", values_to = "Retorno") %>%
    dplyr::mutate(
      Ativo = factor(Ativo, levels = vars),
      Tipo = ifelse(Ativo == "S&P 500", "Índice de Mercado", "Ação Individual")
    )
  
  ggplot2::ggplot(df_long, ggplot2::aes(x = Ativo, y = Retorno, fill = Tipo)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
    ggplot2::geom_boxplot(outlier.color = "#E74C3C", outlier.size = 1.8, alpha = 0.8) +
    ggplot2::scale_fill_manual(values = c("Índice de Mercado" = "#E67E22", "Ação Individual" = "#2980B9")) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = "Ativo / Índice",
      y = "Retorno Total Mensal (%)",
      fill = "Categoria"
    ) +
    theme_academic(base_size = 9) +
    ggplot2::theme(legend.position = "top")
}

#' Gera diagramas de dispersão entre cada ação (Y) e o S&P 500 (X) com reta de regressão
#'
#' @param data data.frame com os dados
#' @param stocks Vetor de nomes das 8 ações
#' @param market Nome da variável de mercado
#' @return Objeto ggplot2
plot_bivariate_relationships <- function(data, stocks = c(
  "Sandisk", "McDonald's", "Caterpillar", "Qualcomm",
  "Exxon Mobil", "Procter & Gamble", "Microsoft", "Johnson & Johnson"
), market = "S&P 500") {
  df_long <- data %>%
    dplyr::select(dplyr::all_of(c(market, stocks))) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(stocks), names_to = "Acao", values_to = "Retorno_Acao") %>%
    dplyr::mutate(Acao = factor(Acao, levels = stocks))
  
  ggplot2::ggplot(df_long, ggplot2::aes(x = .data[[market]], y = Retorno_Acao)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted", color = "gray60") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dotted", color = "gray60") +
    ggplot2::geom_point(color = "#34495E", alpha = 0.7, size = 1.6) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, color = "#2980B9", fill = "#BDC3C7", linewidth = 0.8) +
    ggplot2::facet_wrap(~ Acao, scales = "free_y", ncol = 4) +
    ggplot2::labs(
      x = "Retorno Total Mensal do Mercado - S&P 500 (%)",
      y = "Retorno Total Mensal da Ação (%)"
    ) +
    theme_academic(base_size = 9)
}
