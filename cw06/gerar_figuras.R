# Script para gerar as figuras vetoriais (PDF) do artigo CW06
dir.create("figuras", showWarnings = FALSE)

dados <- as.data.frame(readxl::read_excel("EST074_waterbuffaloes.xlsx"))
dados$Meeting <- factor(dados$Meeting, levels = c("Ordinary", "Special"))
log_value <- log(dados$Value)

# Cores sóbrias para artigo científico
cor_ord <- "#2B5B84"
cor_esp <- "#D95F02"
cores_grupo <- c(cor_ord, cor_esp)

# ------------------------------------------------------------------
# Figura 1: Gráfico de barras para a variável qualitativa Meeting
# ------------------------------------------------------------------
pdf("figuras/fig1_barras_meeting.pdf", width = 5.5, height = 3.8)
par(mar = c(4.5, 4.5, 3, 1.5))
tb <- table(dados$Meeting)
props <- round(100 * prop.table(tb), 1)
bp <- barplot(
  tb,
  ylim = c(0, max(tb) * 1.18),
  col = cores_grupo,
  border = NA,
  ylab = "Frequência absoluta (número de reuniões)",
  xlab = "Tipo de reunião (Meeting)",
  main = "Distribuição das reuniões por categoria"
)
text(bp, tb + 2.5, labels = paste0(tb, " (", props, "%)"), cex = 0.95, font = 2)
dev.off()

# ------------------------------------------------------------------
# Figura 2: Histogramas e Box-plots das variáveis quantitativas
# ------------------------------------------------------------------
pdf("figuras/fig2_hist_box_quant.pdf", width = 9.5, height = 5.2)
par(mfrow = c(2, 4), mar = c(4, 3.8, 2.5, 1))
vars <- c("Value", "Members", "Visitors", "Total")
titulos <- c("Value (US$)", "Members", "Visitors", "Total")

for (i in seq_along(vars)) {
  x <- dados[[vars[i]]]
  hist(x, main = paste("Histograma:", titulos[i]), xlab = titulos[i],
       ylab = "Frequência", col = "#7FA8C9", border = "white")
}
for (i in seq_along(vars)) {
  x <- dados[[vars[i]]]
  boxplot(x, main = paste("Box-plot:", titulos[i]), ylab = titulos[i],
          col = "#BDD3E6", border = "#2B5B84", pch = 19)
}
par(mfrow = c(1, 1))
dev.off()

# ------------------------------------------------------------------
# Figura 3: Value vs log(Value) discriminados por Meeting (histogramas e QQ-plot)
# ------------------------------------------------------------------
pdf("figuras/fig3_value_log_meeting.pdf", width = 9.5, height = 5.5)
par(mfrow = c(2, 3), mar = c(4.2, 4, 2.8, 1))

hist(dados$Value[dados$Meeting == "Ordinary"],
     main = "Value - Ordinary", xlab = "Value (US$)", ylab = "Frequência",
     col = "#7FA8C9", border = "white")
hist(dados$Value[dados$Meeting == "Special"],
     main = "Value - Special", xlab = "Value (US$)", ylab = "Frequência",
     col = "#F2A879", border = "white")
qqnorm(dados$Value, main = "Q-Q Plot: Value (original)",
       xlab = "Quantis teóricos", ylab = "Quantis amostrais", pch = 19, col = "#2B5B84")
qqline(dados$Value, col = "#C0392B", lwd = 2)

hist(log_value[dados$Meeting == "Ordinary"],
     main = "log(Value) - Ordinary", xlab = "log(Value)", ylab = "Frequência",
     col = "#7FA8C9", border = "white")
hist(log_value[dados$Meeting == "Special"],
     main = "log(Value) - Special", xlab = "log(Value)", ylab = "Frequência",
     col = "#F2A879", border = "white")
qqnorm(log_value, main = "Q-Q Plot: log(Value)",
       xlab = "Quantis teóricos", ylab = "Quantis amostrais", pch = 19, col = "#2B5B84")
qqline(log_value, col = "#C0392B", lwd = 2)

par(mfrow = c(1, 1))
dev.off()

# ------------------------------------------------------------------
# Figura 4: Matriz de dispersão (todas as variáveis)
# ------------------------------------------------------------------
panel.hist <- function(x, ...) {
  usr <- par("usr"); on.exit(par(usr = usr))
  par(usr = c(usr[1:2], 0, 1.5))
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y / max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "#BDD3E6", border = "white")
}

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr"); on.exit(par(usr = usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, use = "complete.obs")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, "r = ", txt)
  if (missing(cex.cor)) cex.cor <- 0.75 / strwidth(txt)
  text(0.5, 0.5, txt, cex = max(0.85, cex.cor * abs(r) + 0.45))
}

pdf("figuras/fig4_matriz_dispersao.pdf", width = 8, height = 6.8)
par(xpd = TRUE)
pairs(
  dados,
  col = cores_grupo[dados$Meeting],
  pch = 19,
  cex = 0.85,
  diag.panel = panel.hist,
  upper.panel = panel.cor,
  main = "Matriz de dispersão, histogramas e correlações (Azul: Ordinary | Laranja: Special)",
  oma = c(3, 3, 4.5, 3)
)
dev.off()

# ------------------------------------------------------------------
# Figura 5: Diagnóstico do modelo reduzido original (Value ~ Members + Visitors)
# ------------------------------------------------------------------
m_red <- lm(Value ~ Members + Visitors, data = dados)
pdf("figuras/fig5_diagnostico_original.pdf", width = 8.2, height = 6.4)
par(mfrow = c(2, 2), mar = c(4.2, 4.2, 2.8, 1.2))
plot(m_red, which = 1, pch = 19, col = "#2B5B84", caption = "", main = "Resíduos vs. Valores Ajustados")
plot(m_red, which = 2, pch = 19, col = "#2B5B84", caption = "", main = "Q-Q Plot dos Resíduos")
plot(m_red, which = 3, pch = 19, col = "#2B5B84", caption = "", main = "Escala-Localização")
plot(m_red, which = 4, col = "#2B5B84", lwd = 1.5, caption = "", main = "Distância de Cook")
par(mfrow = c(1, 1))
dev.off()

# ------------------------------------------------------------------
# Figura 6: Diagnóstico do modelo reduzido transformado (log(Value) ~ Members + Visitors)
# ------------------------------------------------------------------
m_log_red <- lm(log(Value) ~ Members + Visitors, data = dados)
pdf("figuras/fig6_diagnostico_log.pdf", width = 8.2, height = 6.4)
par(mfrow = c(2, 2), mar = c(4.2, 4.2, 2.8, 1.2))
plot(m_log_red, which = 1, pch = 19, col = "#2B5B84", caption = "", main = "Resíduos vs. Valores Ajustados")
plot(m_log_red, which = 2, pch = 19, col = "#2B5B84", caption = "", main = "Q-Q Plot dos Resíduos")
plot(m_log_red, which = 3, pch = 19, col = "#2B5B84", caption = "", main = "Escala-Localização")
plot(m_log_red, which = 4, col = "#2B5B84", lwd = 1.5, caption = "", main = "Distância de Cook")
par(mfrow = c(1, 1))
dev.off()

cat("Figuras geradas com sucesso em figuras/\n")
