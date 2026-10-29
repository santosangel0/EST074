# ==============================================================================
# Script: 04_model_analysis.R
# Objetivo: Ajuste das 8 regressões lineares simples (Modelo de Mercado / Beta),
# tabelas de coeficientes, qualidade de ajuste (R2) e visualização comparativa
# Disciplina: EST074 - Análise de Regressão (UFJF)
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(broom)
  library(knitr)
  library(kableExtra)
  library(ggplot2)
  library(patchwork)
})

#' Ajusta os modelos de regressão linear simples para cada ação em função do S&P 500
#'
#' @param data data.frame com os retornos (em %)
#' @param stocks Vetor com os nomes das 8 ações
#' @return Lista com os objetos lm e tabela consolidada de resultados
fit_beta_models <- function(data, stocks = c(
  "Sandisk", "McDonald's", "Caterpillar", "Qualcomm",
  "Exxon Mobil", "Procter & Gamble", "Microsoft", "Johnson & Johnson"
)) {
  models_list <- list()
  summary_rows <- list()
  
  for (s in stocks) {
    form <- stats::as.formula(paste0("`", s, "` ~ `S&P 500`"))
    m <- stats::lm(form, data = data)
    models_list[[s]] <- m
    
    sm <- summary(m)
    cf <- stats::coef(sm)
    ci <- stats::confint(m, level = 0.95)
    f_stat <- sm$fstatistic
    f_pval <- stats::pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
    
    summary_rows[[s]] <- data.frame(
      Acao = s,
      b0 = cf[1, 1],
      se_b0 = cf[1, 2],
      t_b0 = cf[1, 3],
      p_b0 = cf[1, 4],
      Beta = cf[2, 1],
      se_Beta = cf[2, 2],
      t_Beta = cf[2, 3],
      p_Beta = cf[2, 4],
      CI_low = ci[2, 1],
      CI_high = ci[2, 2],
      r = stats::cor(data[[s]], data[["S&P 500"]]),
      R2 = sm$r.squared,
      R2_adj = sm$adj.r.squared,
      R2_perc = sm$r.squared * 100,
      Nao_Explicado_perc = (1 - sm$r.squared) * 100,
      sigma = sm$sigma,
      F_stat = unname(f_stat[1]),
      p_F = unname(f_pval),
      stringsAsFactors = FALSE
    )
  }
  
  summary_df <- do.call(rbind, summary_rows) %>%
    dplyr::arrange(dplyr::desc(Beta))
  rownames(summary_df) <- NULL
  
  list(
    models = models_list,
    summary = summary_df
  )
}

#' Formata tabela com as estimativas de intercepto (b0) e inclinação Beta (b1)
#'
#' @param summary_df data.frame summary retornado por fit_beta_models
#' @param caption Título da tabela
#' @return Objeto kable formatado em LaTeX
format_beta_estimates_table <- function(summary_df,
                                        caption = "Estimativas dos parâmetros dos modelos de regressão linear simples ($\\widehat{R}_j = b_0 + b_1 R_{\\text{S\\&P 500}}$) ordenadas pelo coeficiente Beta ($b_1$)") {
  display_df <- summary_df %>%
    dplyr::mutate(
      Acao_tex = gsub("&", "\\\\&", Acao),
      b0_str = sprintf("%.3f (%.3f)", b0, se_b0),
      p_b0_str = sprintf("%.3f", p_b0),
      p_Beta_str = ifelse(p_Beta < 0.001, "< 0,001***",
                          ifelse(p_Beta < 0.01, sprintf("%.4f**", p_Beta),
                                 ifelse(p_Beta < 0.05, sprintf("%.4f*", p_Beta),
                                        sprintf("%.4f (ns)", p_Beta)))),
      IC_95 = sprintf("[%.3f; %.3f]", CI_low, CI_high),
      Perfil = ifelse(Beta > 1, "Agressiva ($\\beta > 1$)", "Defensiva ($\\beta < 1$)")
    ) %>%
    dplyr::select(
      `Ação` = Acao_tex,
      `$b_0$ (EP)` = b0_str,
      `Beta ($b_1$)` = Beta,
      `EP ($b_1$)` = se_Beta,
      `Estatística $t$` = t_Beta,
      `$p$-valor ($b_1$)` = p_Beta_str,
      `IC 95\\% ($b_1$)` = IC_95,
      `Classificação` = Perfil
    )
  
  knitr::kable(
    display_df,
    format = "latex",
    booktabs = TRUE,
    row.names = FALSE,
    escape = FALSE,
    digits = 3,
    caption = caption,
    align = c("l", "c", "r", "r", "r", "c", "c", "l")
  ) %>%
    kableExtra::kable_styling(
      latex_options = c("striped", "hold_position", "scale_down"),
      font_size = 9
    )
}

#' Formata tabela de qualidade de ajuste e decomposição da variância explicada pelo mercado
#'
#' @param summary_df data.frame summary retornado por fit_beta_models
#' @param caption Título da tabela
#' @return Objeto kable formatado em LaTeX
format_goodness_of_fit_table <- function(summary_df,
                                         caption = "Qualidade do ajuste e proporção da variabilidade do retorno explicada pelo mercado ($R^2$) vs. risco idiossincrático ($1 - R^2$)") {
  display_df <- summary_df %>%
    dplyr::arrange(dplyr::desc(R2)) %>%
    dplyr::mutate(
      Acao_tex = gsub("&", "\\\\&", Acao),
      R2_str = sprintf("%.2f\\%%", R2_perc),
      Idio_str = sprintf("%.2f\\%%", Nao_Explicado_perc),
      p_F_str = ifelse(p_F < 0.001, "< 0,001***",
                       ifelse(p_F < 0.01, sprintf("%.4f**", p_F),
                              ifelse(p_F < 0.05, sprintf("%.4f*", p_F),
                                     sprintf("%.4f (ns)", p_F))))
    ) %>%
    dplyr::select(
      `Ação` = Acao_tex,
      `$r$ de Pearson` = r,
      `$R^2$` = R2,
      `$R^2$ Ajustado` = R2_adj,
      `Explicado pelo Mercado ($R^2$)` = R2_str,
      `Risco Específico ($1 - R^2$)` = Idio_str,
      `$\\hat{\\sigma}_{\\text{res}}$ (\\%)` = sigma,
      `Estatística $F$` = F_stat
    )
  
  knitr::kable(
    display_df,
    format = "latex",
    booktabs = TRUE,
    row.names = FALSE,
    escape = FALSE,
    digits = 3,
    caption = caption,
    align = c("l", "r", "r", "r", "r", "r", "r", "r")
  ) %>%
    kableExtra::kable_styling(
      latex_options = c("striped", "hold_position", "scale_down"),
      font_size = 9
    )
}

#' Gera gráfico comparativo dos coeficientes Beta (IC 95%) e do percentual explicado pelo mercado (R2)
#'
#' @param summary_df data.frame summary retornado por fit_beta_models
#' @return Objeto patchwork combinando os dois gráficos
plot_beta_and_r2 <- function(summary_df) {
  df_plot <- summary_df %>%
    dplyr::mutate(
      Acao_ord_beta = factor(Acao, levels = rev(Acao)),
      Significativo = ifelse(p_Beta < 0.05, "Significativo (p < 0,05)", "Não significativo (p >= 0,05)")
    )
  
  p1 <- ggplot2::ggplot(df_plot, ggplot2::aes(x = Beta, y = Acao_ord_beta, color = Significativo)) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "#E74C3C", linewidth = 0.7) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dotted", color = "gray50", linewidth = 0.6) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = CI_low, xmax = CI_high), height = 0.25, linewidth = 0.7) +
    ggplot2::geom_point(size = 2.6) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", Beta)), vjust = -0.75, size = 2.6, show.legend = FALSE) +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(mult = c(0.08, 0.16))) +
    ggplot2::scale_color_manual(values = c("Significativo (p < 0,05)" = "#2980B9", "Não significativo (p >= 0,05)" = "#7F8C8D")) +
    ggplot2::labs(
      title = "(a) Coeficiente Beta e IC 95%",
      subtitle = "Linha tracejada: Beta do mercado = 1,0",
      x = "Estimativa de Beta (b1)",
      y = "Ação",
      color = ""
    ) +
    ggplot2::theme_minimal(base_size = 8.5) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 9),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 7.5, color = "gray30"),
      legend.position = "bottom"
    )
  
  df_r2 <- summary_df %>%
    dplyr::arrange(R2_perc) %>%
    dplyr::mutate(Acao_ord_r2 = factor(Acao, levels = Acao))
  
  p2 <- ggplot2::ggplot(df_r2, ggplot2::aes(x = R2_perc, y = Acao_ord_r2)) +
    ggplot2::geom_col(fill = "#2C3E50", alpha = 0.85, width = 0.62) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", R2_perc)), hjust = -0.15, size = 2.6, color = "black") +
    ggplot2::scale_x_continuous(limits = c(0, 42)) +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(mult = c(0.08, 0.12))) +
    ggplot2::labs(
      title = "(b) Variância Explicada (R²)",
      subtitle = "Percentual explicado pelo índice S&P 500",
      x = "Coeficiente de Determinação R² (%)",
      y = ""
    ) +
    ggplot2::theme_minimal(base_size = 8.5) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 9),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 7.5, color = "gray30")
    )
  
  p1 | p2
}
