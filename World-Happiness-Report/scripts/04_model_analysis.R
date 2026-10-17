# ==============================================================================
# Script: 04_model_analysis.R
# Objetivo: Ajuste, comparação, diagnóstico e formatação de tabelas de regressão linear
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

#' Ajusta o modelo completo e modelos reduzidos candidatos
#'
#' @param data data.frame com os dados
#' @return Lista contendo os modelos ajustados
fit_regression_models <- function(data) {
  # Modelo 1: Completo com todas as variáveis explicativas
  m_full <- stats::lm(
    Score ~ GDP + Social + Healthy + Freedom + Generosity + Corruption, 
    data = data
  )
  
  # Modelo 2: Sem Generosity (variável com maior p-valor no modelo completo: p = 0,0913)
  m_no_gen <- stats::lm(
    Score ~ GDP + Social + Healthy + Freedom + Corruption, 
    data = data
  )
  
  # Modelo 3: Reduzido sem Generosity e sem Corruption (ambas não significativas a 5% no modelo completo)
  m_red <- stats::lm(
    Score ~ GDP + Social + Healthy + Freedom, 
    data = data
  )
  
  list(
    Completo = m_full,
    Sem_Generosidade = m_no_gen,
    Reduzido = m_red
  )
}

#' Formata tabela de coeficientes com estimativas, erro padrão, t, p-valor e IC95%
#'
#' @param model Objeto lm
#' @param caption Título da tabela
#' @return Objeto kable formatado
format_coefficients_table <- function(model, caption = "Estimativas dos parâmetros do modelo de regressão linear") {
  tidied <- broom::tidy(model, conf.int = TRUE, conf.level = 0.95)
  
  # Nomes amigáveis para os termos
  name_map <- c(
    "(Intercept)" = "Intercepto ($\\beta_0$)",
    "GDP" = "PIB per capita ($\\beta_1$)",
    "Social" = "Apoio social ($\\beta_2$)",
    "Healthy" = "Expectativa de vida saudável ($\\beta_3$)",
    "Freedom" = "Liberdade ($\\beta_4$)",
    "Generosity" = "Generosidade ($\\beta_5$)",
    "Corruption" = "Percepção de corrupção ($\\beta_6$)"
  )
  
  display_df <- tidied %>%
    dplyr::mutate(
      Termo = ifelse(term %in% names(name_map), name_map[term], term),
      p_format = ifelse(p.value < 0.001, "< 0,001***", 
                        ifelse(p.value < 0.01, sprintf("%.4f**", p.value),
                               ifelse(p.value < 0.05, sprintf("%.4f*", p.value),
                                      sprintf("%.4f (ns)", p.value)))),
      IC_95 = sprintf("[%.3f; %.3f]", conf.low, conf.high)
    ) %>%
    dplyr::select(
      `Termo` = Termo,
      `Estimativa ($\\hat{\\beta}$)` = estimate,
      `Erro Padrão` = std.error,
      `Estatística $t$` = statistic,
      `$p$-valor` = p_format,
      `IC 95\\%` = IC_95
    )
  
  knitr::kable(
    display_df,
    format = "latex",
    booktabs = TRUE,
    row.names = FALSE,
    escape = FALSE,
    digits = 4,
    caption = caption,
    align = c("l", "r", "r", "r", "c", "c")
  ) %>%
    kableExtra::kable_styling(
      latex_options = c("striped", "hold_position"),
      font_size = 9
    )
}

#' Compara qualidade de ajuste e critérios de informação entre múltiplos modelos
#'
#' @param models_list Lista nomeada de objetos lm
#' @param caption Título da tabela
#' @return Objeto kable formatado
compare_models_table <- function(models_list, 
                                 caption = "Comparação da qualidade de ajuste entre os modelos avaliados") {
  name_labels <- c(
    "Completo" = "Completo (6 preditores)",
    "Sem_Generosidade" = "Sem Generosidade (5 preditores)",
    "Reduzido" = "Reduzido (4 preditores)"
  )
  
  comp_rows <- lapply(names(models_list), function(m_name) {
    m <- models_list[[m_name]]
    s <- summary(m)
    f_stat <- s$fstatistic
    f_pval <- stats::pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
    
    rotulo <- ifelse(m_name %in% names(name_labels), name_labels[m_name], gsub("_", " ", m_name))
    
    data.frame(
      Modelo = rotulo,
      Qtd_Preditores = length(stats::coef(m)) - 1,
      R2 = s$r.squared,
      R2_Ajustado = s$adj.r.squared,
      Erro_Padrao_Res = s$sigma,
      F_Stat = f_stat[1],
      p_valor_F = ifelse(f_pval < 0.001, "< 0,001", sprintf("%.4f", f_pval)),
      AIC = stats::AIC(m),
      BIC = stats::BIC(m),
      stringsAsFactors = FALSE
    )
  })
  
  comp_df <- do.call(rbind, comp_rows)
  rownames(comp_df) <- NULL
  
  display_df <- comp_df %>%
    dplyr::rename(
      `Modelo` = Modelo,
      `Preditoras` = Qtd_Preditores,
      `$R^2$` = R2,
      `$R^2$ Ajustado` = R2_Ajustado,
      `$\\hat{\\sigma}_{res}$` = Erro_Padrao_Res,
      `Estatística $F$` = F_Stat,
      `$p$-valor ($F$)` = p_valor_F,
      `AIC` = AIC,
      `BIC` = BIC
    )
  
  knitr::kable(
    display_df,
    format = "latex",
    booktabs = TRUE,
    row.names = FALSE,
    escape = FALSE,
    digits = 3,
    caption = caption,
    align = c("l", "c", "r", "r", "r", "r", "c", "r", "r")
  ) %>%
    kableExtra::kable_styling(
      latex_options = c("striped", "hold_position"),
      font_size = 9
    )
}

#' Realiza teste F parcial (ANOVA) para comparar modelos aninhados
#'
#' @param m_red Modelo reduzido
#' @param m_full Modelo completo
#' @param caption Título da tabela
#' @return Objeto kable formatado
format_anova_comparison <- function(m_red, m_full, 
                                    caption = "Teste $F$ parcial para comparação entre modelo reduzido e modelo completo") {
  an <- stats::anova(m_red, m_full)
  an_df <- as.data.frame(an)
  
  an_clean <- data.frame(
    Modelo = c("Reduzido (4 preditores)", "Completo (6 preditores)"),
    Graus_Liberdade_Res = an_df$Res.Df,
    SQ_Residuos = an_df$RSS,
    GL_Diferenca = c("-", as.character(an_df$Df[2])),
    SQ_Explicada_Extra = c("-", sprintf("%.4f", an_df$`Sum of Sq`[2])),
    Estatistica_F = c("-", sprintf("%.4f", an_df$F[2])),
    p_valor = c("-", ifelse(an_df$`Pr(>F)`[2] < 0.001, "< 0,001", sprintf("%.4f*", an_df$`Pr(>F)`[2]))),
    stringsAsFactors = FALSE
  )
  
  knitr::kable(
    an_clean,
    format = "latex",
    booktabs = TRUE,
    row.names = FALSE,
    escape = FALSE,
    caption = caption,
    col.names = c("Modelo", "$GL_{res}$", "$SQ_{res}$", "$\\Delta GL$", "$\\Delta SQ$", "Estatística $F$", "$p$-valor"),
    align = c("l", "r", "r", "c", "r", "r", "c")
  ) %>%
    kableExtra::kable_styling(
      latex_options = c("striped", "hold_position"),
      font_size = 9
    )
}

#' Constrói a equação do modelo em notação matemática LaTeX
#'
#' @param model Objeto lm
#' @param response_name Nome da variável resposta
#' @param digits Casas decimais
#' @return String com a equação em LaTeX
format_regression_equation <- function(model, response_name = "\\text{Score}", digits = 3) {
  cf <- stats::coef(model)
  names_cf <- names(cf)
  
  termo_int <- sprintf("%.*f", digits, cf[1])
  termos_pred <- sapply(2:length(cf), function(i) {
    sinal <- ifelse(cf[i] >= 0, "+", "-")
    val <- sprintf("%.*f", digits, abs(cf[i]))
    nome <- names_cf[i]
    paste(sinal, val, paste("\\cdot \\text{", nome, "}", sep = ""))
  })
  
  eq_str <- paste("\\widehat{", response_name, "} = ", termo_int, " ", 
                  paste(termos_pred, collapse = " "), sep = "")
  return(eq_str)
}

#' Gera gráficos de diagnóstico dos resíduos para validação das suposições
#'
#' @param model Objeto lm
#' @return Objeto patchwork combinando os 4 gráficos diagnósticos
plot_diagnostics <- function(model) {
  res_df <- data.frame(
    Fitted = stats::fitted(model),
    Residuals = stats::residuals(model),
    Std_Residuals = stats::rstandard(model),
    Leverage = stats::hatvalues(model),
    Theoretical_Q = stats::qqnorm(stats::rstandard(model), plot.it = FALSE)$x
  )
  
  # 1. Resíduos vs Valores Ajustados
  p1 <- ggplot2::ggplot(res_df, ggplot2::aes(x = Fitted, y = Residuals)) +
    ggplot2::geom_point(color = "#2C3E50", alpha = 0.7, size = 1.5) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "#E74C3C") +
    ggplot2::geom_smooth(method = "loess", se = FALSE, color = "#2980B9", linewidth = 0.7) +
    ggplot2::labs(title = "Resíduos vs Ajustados", x = "Valores Ajustados", y = "Resíduos") +
    ggplot2::theme_minimal(base_size = 8)
  
  # 2. Gráfico Q-Q Normal
  p2 <- ggplot2::ggplot(res_df, ggplot2::aes(x = Theoretical_Q, y = Std_Residuals)) +
    ggplot2::geom_point(color = "#2C3E50", alpha = 0.7, size = 1.5) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#E74C3C") +
    ggplot2::labs(title = "Q-Q Normal", x = "Quantis Teóricos", y = "Resíduos Padronizados") +
    ggplot2::theme_minimal(base_size = 8)
  
  # 3. Escala-Locação
  p3 <- ggplot2::ggplot(res_df, ggplot2::aes(x = Fitted, y = sqrt(abs(Std_Residuals)))) +
    ggplot2::geom_point(color = "#2C3E50", alpha = 0.7, size = 1.5) +
    ggplot2::geom_smooth(method = "loess", se = FALSE, color = "#2980B9", linewidth = 0.7) +
    ggplot2::labs(title = "Escala-Locação", x = "Valores Ajustados", y = expression(sqrt("|Resíduos Padronizados|"))) +
    ggplot2::theme_minimal(base_size = 8)
  
  # 4. Resíduos vs Alavancagem
  p4 <- ggplot2::ggplot(res_df, ggplot2::aes(x = Leverage, y = Std_Residuals)) +
    ggplot2::geom_point(color = "#2C3E50", alpha = 0.7, size = 1.5) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "#E74C3C") +
    ggplot2::geom_smooth(method = "loess", se = FALSE, color = "#2980B9", linewidth = 0.7) +
    ggplot2::labs(title = "Resíduos vs Alavancagem", x = "Alavancagem (Leverage)", y = "Resíduos Padronizados") +
    ggplot2::theme_minimal(base_size = 8)
  
  (p1 | p2) / (p3 | p4)
}
