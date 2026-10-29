# ==============================================================================
# Script: 01_data_loader.R
# Objetivo: Leitura, validação e preparação do conjunto de dados Beta.xlsx
# Disciplina: EST074 - Análise de Regressão (UFJF)
# ==============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
})

#' Carrega e valida os dados de retornos mensais das ações e do S&P 500
#'
#' @param file_path Caminho para o arquivo Excel de dados
#' @return Um data.frame / tibble com os dados tratados em percentual (%) e proporção
load_beta_data <- function(file_path = "Beta.xlsx") {
  if (!file.exists(file_path)) {
    stop(paste("Arquivo não encontrado no caminho especificado:", file_path))
  }
  
  df <- readxl::read_excel(file_path, sheet = "Data")
  
  required_cols <- c(
    "Month", "Microsoft", "Exxon Mobil", "Caterpillar", 
    "Johnson & Johnson", "McDonald's", "Sandisk", 
    "Qualcomm", "Procter & Gamble", "S&P 500"
  )
  
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(paste("As seguintes colunas obrigatórias estão ausentes:", 
               paste(missing_cols, collapse = ", ")))
  }
  
  # Converte retornos de proporção para percentual (%) para facilitar leitura financeira
  # Mantém a relação linear exata (b1 e R2 são invariantes à mudança de escala igual em X e Y)
  asset_cols <- setdiff(required_cols, "Month")
  
  df_clean <- df %>%
    dplyr::select(dplyr::all_of(required_cols)) %>%
    dplyr::mutate(
      Month = as.Date(Month),
      dplyr::across(dplyr::all_of(asset_cols), ~ as.numeric(.x) * 100)
    )
  
  return(df_clean)
}

#' Lista os nomes das 8 ações individuais e da variável de mercado
get_asset_names <- function() {
  list(
    stocks = c(
      "Microsoft", "Exxon Mobil", "Caterpillar", "Johnson & Johnson",
      "McDonald's", "Sandisk", "Qualcomm", "Procter & Gamble"
    ),
    market = "S&P 500",
    all = c(
      "Microsoft", "Exxon Mobil", "Caterpillar", "Johnson & Johnson",
      "McDonald's", "Sandisk", "Qualcomm", "Procter & Gamble", "S&P 500"
    )
  )
}
