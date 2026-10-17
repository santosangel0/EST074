# ==============================================================================
# Script: 01_data_loader.R
# Objetivo: Leitura, validação e preparação do conjunto de dados do 
# World Happiness Report
# Disciplina: EST074 - Análise de Regressão (UFJF)
# ==============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
})

#' Carrega e valida os dados do World Happiness Report
#'
#' @param file_path Caminho para o arquivo Excel de dados
#' @return Um data.frame / tibble com os dados tratados e validados
load_happiness_data <- function(file_path = "dados/Worldhappiness.xls") {
  if (!file.exists(file_path)) {
    stop(paste("Arquivo não encontrado no caminho especificado:", file_path))
  }
  
  # Leitura dos dados da aba 'Database'
  df <- readxl::read_excel(file_path, sheet = "Database")
  
  # Verificação das colunas requeridas para o modelo e identificação
  required_cols <- c("Country", "Score", "GDP", "Social", "Healthy", 
                     "Freedom", "Generosity", "Corruption")
  
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(paste("As seguintes colunas obrigatórias estão ausentes:", 
               paste(missing_cols, collapse = ", ")))
  }
  
  # Seleciona as colunas de interesse
  df_clean <- df %>%
    dplyr::select(dplyr::all_of(required_cols)) %>%
    dplyr::mutate(
      Country = as.character(Country),
      Score = as.numeric(Score),
      GDP = as.numeric(GDP),
      Social = as.numeric(Social),
      Healthy = as.numeric(Healthy),
      Freedom = as.numeric(Freedom),
      Generosity = as.numeric(Generosity),
      Corruption = as.numeric(Corruption)
    )
  
  # Checagem de dados faltantes (NA)
  total_na <- sum(is.na(df_clean))
  if (total_na > 0) {
    warning(paste("Atenção:", total_na, "valores faltantes detectados na base."))
  }
  
  return(df_clean)
}

#' Retorna mapa descritivo com os nomes e significados das variáveis
#'
#' @return Um data.frame com as variáveis, papéis no modelo e descrições
get_variable_descriptions <- function() {
  data.frame(
    Variavel = c("Score", "GDP", "Social", "Healthy", "Freedom", "Generosity", "Corruption"),
    Papel = c("Resposta (Y)", "Preditora (X1)", "Preditora (X2)", "Preditora (X3)", 
              "Preditora (X4)", "Preditora (X5)", "Preditora (X6)"),
    Descricao = c(
      "Índice médio de felicidade (escala de 0 a 10, Cantril ladder)",
      "PIB per capita (contribuição normalizada do produto per capita)",
      "Apoio social (suporte de familiares e amigos em momentos de necessidade)",
      "Expectativa de vida saudável ao nascer",
      "Liberdade percebida para tomar decisões de vida",
      "Generosidade (comportamento de doação e engajamento comunitário)",
      "Percepção de corrupção (menor corrupção percebida no governo/empresas)"
    ),
    stringsAsFactors = FALSE
  )
}
