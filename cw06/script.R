# Carregar os dados do arquivo Excel
# (Usando readxl para ler o .xlsx e convertendo para data.frame do R base)
dados <- as.data.frame(readxl::read_excel("EST074_waterbuffaloes.xlsx"))

# Exibir os nomes das variáveis
cat("Variáveis:\n")
print(names(dados))

# Exibir o tipo de cada dado (estrutura completa em R base)
cat("\nEstrutura dos dados (str):\n")
str(dados)

# Exibir a classe e o tipo interno de cada variável
cat("\nClasse de cada variável:\n")
print(sapply(dados, class))

cat("\nTipo (typeof) de cada variável:\n")
print(sapply(dados, typeof))

# Funções auxiliares para pairs(): histograma na diagonal e correlação no triângulo superior
panel.hist <- function(x, ...) {
  usr <- par("usr"); on.exit(par(usr = usr))
  par(usr = c(usr[1:2], 0, 1.5))
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y / max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "lightgray")
}

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr"); on.exit(par(usr = usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, use = "complete.obs")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, "r = ", txt)
  if (missing(cex.cor)) cex.cor <- 0.8 / strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * abs(r) + 0.5)
}

# Matriz de dispersão (todas as variáveis, incluindo Meeting), colorida por Meeting
dados_pairs <- dados
dados_pairs$Meeting <- as.factor(dados_pairs$Meeting)

pairs(
  dados_pairs,
  col = dados_pairs$Meeting,
  pch = 19,
  diag.panel = panel.hist,
  upper.panel = panel.cor,
  main = "Matriz de dispersão por tipo de reunião"
)

# Função em R base para teste de assimetria (z-test assintótico) com valor-p
teste_assimetria <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  g1 <- mean(((x - mean(x)) / sd(x))^3)
  z <- g1 / sqrt(6 / n)
  p_valor <- 2 * pnorm(-abs(z))
  c(assimetria = g1, z = z, p_valor = p_valor)
}

cat("\nTeste de assimetria de Value (antes do log):\n")
print(teste_assimetria(dados$Value))

# Teste de normalidade Shapiro-Wilk para o log de Value
log_value <- log(dados$Value)
cat("\nTeste de assimetria de log(Value) (com log):\n")
print(teste_assimetria(log_value))
cat("\nTeste de Shapiro-Wilk para log(Value):\n")
print(shapiro.test(log_value))

# QQ-plot para o log de Value
qqnorm(log_value, main = "QQ-Plot de log(Value)")
qqline(log_value, col = "red")

# Teste de Shapiro-Wilk discriminando por Meeting (sem log e com log)
cat("\nTeste de Shapiro-Wilk para Value por Meeting:\n")
print(tapply(dados$Value, dados$Meeting, shapiro.test))

cat("\nTeste de Shapiro-Wilk para log(Value) por Meeting:\n")
print(tapply(log_value, dados$Meeting, shapiro.test))

# Histogramas de Value discriminados por Meeting
par(mfrow = c(1, 2))
hist(dados$Value[dados$Meeting == "Ordinary"],
     main = "Value - Ordinary",
     xlab = "Value",
     col = "lightblue")
hist(dados$Value[dados$Meeting == "Special"],
     main = "Value - Special",
     xlab = "Value",
     col = "lightgreen")
par(mfrow = c(1, 1))

# Histogramas de log(Value) discriminados por Meeting
par(mfrow = c(1, 2))
hist(log_value[dados$Meeting == "Ordinary"],
     main = "log(Value) - Ordinary",
     xlab = "log(Value)",
     col = "lightblue")
hist(log_value[dados$Meeting == "Special"],
     main = "log(Value) - Special",
     xlab = "log(Value)",
     col = "lightgreen")
par(mfrow = c(1, 1))

# Teste t para as médias de log(Value) discriminando por Meeting
cat("\nTeste t para as médias de log(Value) por Meeting:\n")
print(t.test(log_value ~ dados$Meeting))

# Regressão linear (sem Total para evitar colinearidade com Members + Visitors)
modelo <- lm(Value ~ Meeting + Members + Visitors, data = dados)
tabela_betas <- as.data.frame(summary(modelo)$coefficients)
cat("\nTabela de coeficientes (betas) e significância:\n")
print(tabela_betas)

summary(dados)
hist(dados$Value)
hist(log(dados$Value))