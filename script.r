# install.packages(c(
#   "readxl", "TTR", "xts", "zoo", "imputeTS", "urca", "tseries", "FinTS",
#   "fracdiff", "vars", "igraph", "ggraph", "tidygraph", "ggplot2", "dplyr",
#   "tidyr", "scales", "sandwich", "lmtest", "patchwork", "ConnectednessApproach"
# ))

options(xts.warn_dplyr_breaks_lag = FALSE)

library(readxl)
library(TTR)
library(xts)
library(zoo)
library(imputeTS)
library(urca)
library(tseries)
library(FinTS)
library(fracdiff)
library(ConnectednessApproach)
library(vars)
library(igraph)
library(ggraph)
library(tidygraph)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(sandwich)
library(lmtest)
library(patchwork)

# ── Configuração do painel ───────────────────────────────────────────────
# Alterna entre os dois painéis
# Trocar PAINEL e rodar o script de novo — cada painel grava em sua própria
# subpasta (outputs/N7_principal/, outputs/N8_extensao/), nunca se sobrepõem.
PAINEL <- "N7_ate_0730"   # N7_principal ou N8_extensao ou N7_ate_0730

CONFIG_PAINEIS <- list(
  N7_principal = list(
    nomes       = c("ITUB4", "BBDC4", "BBAS3", "SANB11", "BPAC11", "BRSR6", "ABCB4"),
    data_inicio = "2017-10-01",
    data_fim    = "2026-09-30"
  ),
  N8_extensao = list(
    nomes       = c("ITUB4", "BBDC4", "BBAS3", "SANB11", "BPAC11", "BRSR6", "ABCB4", "BPAN4"),
    data_inicio = "2019-01-01",
    data_fim    = "2025-10-13"
  ),
  N7_ate_0730 = list(   # painel principal encerrado no último fechamento antes do fato relevante do Santander
    nomes       = c("ITUB4", "BBDC4", "BBAS3", "SANB11", "BPAC11", "BRSR6", "ABCB4"),
    data_inicio = "2017-10-01",
    data_fim    = "2026-07-30"
  )
)

stopifnot("PAINEL precisa ser 'N7_principal' ou 'N8_extensao'" = PAINEL %in% names(CONFIG_PAINEIS))
cfg <- CONFIG_PAINEIS[[PAINEL]]

CAMINHO_EXCEL <- "Bancos_Cotacoes_Comdinheiro.xlsx" # Cotacoes_ComDinheiro_8bancos_2014-2026.xlsx  ou Bancos_Cotacoes_Comdinheiro.xlsx
NOMES         <- cfg$nomes
DATA_INICIO   <- cfg$data_inicio
DATA_FIM      <- cfg$data_fim

OUTPUT_DIR <- file.path("outputs", PAINEL)
outfile    <- function(subpath) file.path(OUTPUT_DIR, subpath)

cat("\n=== Painel:", PAINEL, "| N =", length(NOMES), "bancos | período:",
    DATA_INICIO, "a", DATA_FIM, "| saída em", OUTPUT_DIR, "===\n")

dir.create(outfile("tabelas"),  recursive = TRUE, showWarnings = FALSE)
dir.create(outfile("graficos"), recursive = TRUE, showWarnings = FALSE)

N_JANELA_W    <- 200
H_HORIZONTE   <- 10
P_LAGS        <- 2
THRESHOLD_REL <- 0.05   # threshold relativo do grafo NET colapsado (Seção 16)

# Grade de kappa2: o ótimo (0,99) está na borda da grade. TRUE acrescenta 0,995
# (6 pares a mais; alguns minutos a mais). Mantido FALSE para reproduzir os números
# do texto; se virar TRUE, a Tabela da grade e o texto da Seção 4.3 precisam ser refeitos.
ESTENDER_GRADE_K2 <- FALSE


EVENTOS <- data.frame(
  data  = as.Date(c("2020-03-11", "2021-03-17", "2023-01-11", "2024-11-27", "2026-04-08")),
  label = c("Pandemia (OMS)", "Copom eleva a Selic", "Americanas", "Pacote fiscal", "Cessar-fogo EUA–Irã"),
  ylab  = c(82, 78, 82, 78, 86),     # alturas alternadas, no topo, acima da curva
  hj    = c(0, 0, 0, 0, 1),          # o último rótulo fica à esquerda da linha (borda direita)
  stringsAsFactors = FALSE
)

# Rótulos dos eventos na base do gráfico, escritos de baixo para cima: ali a curva do
# TCI nunca chega perto do texto (no topo, o rótulo "Aperto Selic" cobria a curva).
rotulos_eventos <- function(ymin) {
  geom_text(data = EVENTOS, aes(x = data, y = ymin, label = label),
            angle = 90, hjust = 0, vjust = -0.4, size = 2.8, color = "grey30",
            inherit.aes = FALSE)
}

# ==============================================================================
# ── Checagem de diretório de trabalho ───────────────────────────────────────
if (!file.exists(CAMINHO_EXCEL)) {
  stop(
    "Arquivo não encontrado: '", CAMINHO_EXCEL, "'\n",
    "  Working directory atual: ", getwd(), "\n",
    "  Rode setwd() para a pasta que contém o Excel e o script, ou ajuste CAMINHO_EXCEL."
  )
}

# ==============================================================================
# SEÇÃO 1 — LEITURA DOS DADOS (EXCEL)
# ==============================================================================

cat("\n[1/11] Lendo cotações do arquivo Excel (ComDinheiro)...\n")

dados_adj <- list()

for (nome in NOMES) {
  cat("  Lendo aba", nome, "...\n")
  
  raw <- read_excel(CAMINHO_EXCEL, sheet = nome)
  raw <- raw %>%
    mutate(Data = as.Date(Data)) %>%
    filter(Data >= as.Date(DATA_INICIO), Data <= as.Date(DATA_FIM)) %>%
    arrange(Data)
  
  if (nrow(raw) == 0) {
    stop("Nenhuma observação para ", nome, " no período ", DATA_INICIO, " a ", DATA_FIM, ".")
  }
  
  # Colunas *_Aj do ComDinheiro já vêm ajustadas por proventos e cisões.
  ohlc_adj <- xts(
    data.frame(
      Open       = raw$Abertura_Aj,
      High       = raw$Maximo_Aj,
      Low        = raw$Minimo_Aj,
      Close      = raw$Fechamento_Aj,
      Volume_BRL = raw$Volume_MM_RS * 1e6
    ),
    order.by = raw$Data
  )
  
  dados_adj[[nome]] <- ohlc_adj
}

cat("  OK —", length(dados_adj), "séries lidas.\n")

for (nome in NOMES) {
  d <- dados_adj[[nome]]
  cat(nome, "| obs:", nrow(d),
      "| de:", as.character(index(d)[1]),
      "| até:", as.character(index(d)[nrow(d)]),
      "| NAs:", sum(is.na(d)), "\n")
}


# ==============================================================================
# SEÇÃO 2 — SANITY CHECKS NO OHLC
# ==============================================================================

cat("\n[2/11] Verificando consistência OHLC...\n")

# EPS: tolerância numérica — sem ela, ruído de ponto flutuante (~1e-9) do
# cálculo do fator de ajuste do ComDinheiro é lido como violação real.
EPS <- 1e-6

for (nome in NOMES) {
  d <- dados_adj[[nome]]
  op <- d$Open; hi <- d$High; lo <- d$Low; cl <- d$Close
  
  violacao <- (hi < lo - EPS) | (hi < op - EPS) | (hi < cl - EPS) |
    (lo > op + EPS) | (lo > cl + EPS) |
    (!is.na(op) & op <= 0) | (!is.na(cl) & cl <= 0)
  violacao[is.na(violacao)] <- FALSE
  
  n_viol <- sum(violacao)
  if (n_viol > 0) {
    cat("  AVISO:", nome, "—", n_viol, "linha(s) com inconsistência OHLC. Convertidas para NA.\n")
    dados_adj[[nome]][violacao, c("Open","High","Low","Close")] <- NA
  }
}

cat("  OK - sanity checks concluídos.\n")


# ==============================================================================
# SEÇÃO 3 — FILTRO DE LIQUIDEZ
# ==============================================================================

cat("\n[3/11] Aplicando filtro de liquidez...\n")

# Filtro por volume financeiro (Volume_MM_RS). A planilha também tem a
# coluna Negocios (número de negócios do dia) como possível critério
# complementar de liquidez.
VOLUME_MIN_BRL <- 100000
contagem_liquidez <- list()



for (nome in NOMES) {
  d       <- dados_adj[[nome]]
  vol_brl <- as.numeric(d$Volume_BRL)
  filtro  <- !is.na(vol_brl) & (vol_brl < VOLUME_MIN_BRL)
  
  n_filtrados <- sum(filtro)
  contagem_liquidez[[nome]] <- n_filtrados
  if (n_filtrados > 0) {
    cat("  ", nome, ":", n_filtrados, "dia(s) com baixa liquidez → NA\n")
    dados_adj[[nome]][filtro, c("Open","High","Low","Close")] <- NA
  }
}

cat("  OK — filtro de liquidez aplicado.\n")


# ==============================================================================
# SEÇÃO 4 — IDENTIFICAÇÃO DE OUTLIERS
# ==============================================================================

cat("\n[4/11] Identificando outliers (|z| > 5)...\n")

THRESHOLD_Z <- 5
relatorio_outliers <- data.frame()

for (nome in NOMES) {
  cl  <- as.numeric(dados_adj[[nome]]$Close)
  dts <- index(dados_adj[[nome]])
  
  r    <- c(NA, diff(log(cl)))
  mu_r <- mean(r, na.rm = TRUE)
  sd_r <- sd(r, na.rm = TRUE)
  z    <- (r - mu_r) / sd_r
  
  idx_out <- which(abs(z) > THRESHOLD_Z)
  if (length(idx_out) > 0) {
    df <- data.frame(
      banco = nome, data = dts[idx_out],
      retorno = round(r[idx_out] * 100, 2), z_score = round(z[idx_out], 2)
    )
    relatorio_outliers <- rbind(relatorio_outliers, df)
  }
}

if (nrow(relatorio_outliers) > 0) {
  cat("\n  Outliers identificados (|z| > 5):\n")
  print(relatorio_outliers[order(relatorio_outliers$data), ])
  write.csv(relatorio_outliers, outfile("tabelas/outliers_para_revisao.csv"), row.names = FALSE)
  
  # Regra: >=2 bancos na mesma data = evento sistêmico
  # (mantém); 1 banco só = precisa de verificação externa. Apenas relatório —
  # não altera dados_adj.
  n_por_data <- table(relatorio_outliers$data)
  relatorio_outliers$n_bancos_na_data <- as.integer(n_por_data[as.character(relatorio_outliers$data)])
  relatorio_outliers$classificacao <- ifelse(
    relatorio_outliers$n_bancos_na_data >= 2,
    "Sistêmico (>=2 bancos na data) — manter sem checagem adicional",
    "ISOLADO (1 banco na data) — verificar em fonte externa"
  )
  
  isolados <- relatorio_outliers[relatorio_outliers$n_bancos_na_data == 1, ]
  isolados <- isolados[order(isolados$data), ]
  
  cat("\n  Resumo da classificação automática:\n")
  cat("   ", sum(relatorio_outliers$n_bancos_na_data >= 2), "linha(s) em datas sistêmicas (>=2 bancos)\n")
  cat("   ", nrow(isolados), "linha(s) ISOLADAS — precisam de verificação externa:\n")
  print(isolados[, c("banco", "data", "retorno", "z_score")], row.names = FALSE)
  
  write.csv(relatorio_outliers, outfile("tabelas/outliers_classificados.csv"), row.names = FALSE)
  cat("\n  Tabela classificada salva em", outfile("tabelas/outliers_classificados.csv"), "\n")
} else {
  cat("  Nenhum outlier com |z| > 5 encontrado.\n")
}

# ── Correlação BPAN4 x BPAC11 — diagnóstico do anúncio de incorporação ──────
if (all(c("BPAN4", "BPAC11") %in% NOMES)) {
  # O fato relevante do BTG é de 13/10/2025, à noite; 14/10 é o primeiro pregão posterior.
  ANUNCIO_INCORPORACAO <- as.Date("2025-10-14")
  
  # Lê direto do Excel, sem o corte de DATA_FIM: no painel N=8 extensão,
  # DATA_FIM já para um dia antes do anúncio (Seção 1, por desenho, para
  # não contaminar a estimação do VAR) — este diagnóstico precisa enxergar
  # além disso, até perto da deslistagem efetiva (jan/2026).
  # Fechamento_Hist (preço bruto, sem ajuste) é usado só para a razão de preços,
  # se a coluna existir na planilha.
  raw_pan <- read_excel(CAMINHO_EXCEL, sheet = "BPAN4")  %>%
    mutate(Data = as.Date(Data)) %>%
    select(Data, BPAN4 = Fechamento_Aj, any_of(c(BPAN4_hist = "Fechamento_Hist"))) %>% arrange(Data)
  raw_btg <- read_excel(CAMINHO_EXCEL, sheet = "BPAC11") %>%
    mutate(Data = as.Date(Data)) %>%
    select(Data, BPAC11 = Fechamento_Aj, any_of(c(BPAC11_hist = "Fechamento_Hist"))) %>% arrange(Data)
  
  precos <- raw_pan %>%
    inner_join(raw_btg, by = "Data") %>%
    filter(!is.na(BPAN4), !is.na(BPAC11), BPAN4 > 0, BPAC11 > 0) %>%
    arrange(Data)
  
  ret_df <- precos %>%
    mutate(ret_bpan4 = log(BPAN4 / lag(BPAN4)), ret_bpac11 = log(BPAC11 / lag(BPAC11))) %>%
    tidyr::drop_na(ret_bpan4, ret_bpac11)
  
  pre_tudo <- ret_df %>% filter(Data < ANUNCIO_INCORPORACAO)
  pos      <- ret_df %>% filter(Data >= ANUNCIO_INCORPORACAO)
  
  # Janela pré-evento do MESMO TAMANHO que a pós (não a amostra inteira) —
  # comparar anos de história contra meses pós-anúncio dilui qualquer
  # mudança de regime recente numa média de longuíssimo prazo.
  pre_janela <- tail(pre_tudo, nrow(pos))
  
  cat("\n  Correlação BPAN4 x BPAC11 (retornos diários):\n")
  cat("    Janela pré-evento, mesmo tamanho que a pós (", nrow(pre_janela), "dias):",
      round(cor(pre_janela$ret_bpan4, pre_janela$ret_bpac11), 3), "\n")
  cat("    Pós-evento, período inteiro (", nrow(pos), "dias):",
      round(cor(pos$ret_bpan4, pos$ret_bpac11), 3), "\n")
  n_ret <- nrow(ret_df)
  cat("    Correlação nos últimos N dias da amostra (direto sobre os retornos brutos):\n")
  for (k in c(10, 20, 30, 40, 50, 60)) {
    idx <- max(1, n_ret - k + 1):n_ret
    cor_k <- cor(ret_df$ret_bpan4[idx], ret_df$ret_bpac11[idx], use = "complete.obs")
    cat("      últimos", k, "dias:", round(cor_k, 4), "\n")
  }
  
  # ── Diagnóstico do dia do anúncio (14/10/2025) ──────────────────────────────
  # Este dia concentra o salto do Pan, sem contrapartida no BTG, e é ele que derruba
  # a correlação média pós-anúncio e a correlação móvel enquanto está dentro da janela.
  # A data em que ele SAI da janela (28/10 numa janela de 10 pregões; 26/11 numa de 30)
  # é artefato da janela, não evento — por isso este script NÃO procura "início da
  # convergência" por correlação móvel (versão anterior, que dava 28/10/2025).
  RELACAO_TROCA <- 0.2128   # unit do BTG por ação preferencial do Pan (fato relevante de 13/10/2025)
  JANELA_CURTA  <- 10
  JANELA_ROLL   <- 30
  
  ret_df$corr_curta <- zoo::rollapply(
    ret_df[, c("ret_bpan4", "ret_bpac11")], width = JANELA_CURTA,
    FUN = function(x) cor(x[, 1], x[, 2], use = "complete.obs"),
    by.column = FALSE, align = "right", fill = NA
  )
  ret_df$corr_roll <- zoo::rollapply(
    ret_df[, c("ret_bpan4", "ret_bpac11")], width = JANELA_ROLL,
    FUN = function(x) cor(x[, 1], x[, 2], use = "complete.obs"),
    by.column = FALSE, align = "right", fill = NA
  )
  
  i_anu <- which(ret_df$Data == ANUNCIO_INCORPORACAO)
  stopifnot("O pregão de 14/10/2025 não está na série de retornos" = length(i_anu) == 1)
  
  cat("\n  Dia do anúncio (", format(ANUNCIO_INCORPORACAO), "):\n", sep = "")
  cat("    Retorno logarítmico BPAN4:", round(100 * ret_df$ret_bpan4[i_anu], 2),
      "% | BPAC11:", round(100 * ret_df$ret_bpac11[i_anu], 2), "%\n")
  
  pos_sem_dia  <- pos %>% filter(Data != ANUNCIO_INCORPORACAO)
  janela_15_27 <- ret_df %>% filter(Data > ANUNCIO_INCORPORACAO, Data <= as.Date("2025-10-27"))
  cat("    Correlação pós-anúncio COM o dia 14/10:", round(cor(pos$ret_bpan4, pos$ret_bpac11), 3),
      "(n =", nrow(pos), ")\n")
  cat("    Correlação pós-anúncio SEM o dia 14/10:",
      round(cor(pos_sem_dia$ret_bpan4, pos_sem_dia$ret_bpac11), 3), "(n =", nrow(pos_sem_dia), ")\n")
  cat("    Correlação de 15/10 a 27/10:",
      round(cor(janela_15_27$ret_bpan4, janela_15_27$ret_bpac11), 3), "(n =", nrow(janela_15_27), ")\n")
  
  for (w in c(JANELA_CURTA, JANELA_ROLL)) {
    col_w <- if (w == JANELA_CURTA) "corr_curta" else "corr_roll"
    if (i_anu + w <= nrow(ret_df)) {
      cat("    O pregão de 14/10 deixa a janela de", w, "pregões em", format(ret_df$Data[i_anu + w]),
          "(correlação móvel de", round(ret_df[[col_w]][i_anu + w - 1], 2), "para",
          round(ret_df[[col_w]][i_anu + w], 2), ")\n")
    }
  }
  
  # Razão de preços BPAN4/BPAC11 (fechamento bruto) contra a relação de troca.
  if (all(c("BPAN4_hist", "BPAC11_hist") %in% names(precos))) {
    precos$razao <- precos$BPAN4_hist / precos$BPAC11_hist
    razao_vespera <- tail(precos$razao[precos$Data < ANUNCIO_INCORPORACAO], 1)
    razao_dia     <- precos$razao[precos$Data == ANUNCIO_INCORPORACAO]
    razao_pos     <- precos$razao[precos$Data >= ANUNCIO_INCORPORACAO]
    desvio_pos    <- (razao_pos / RELACAO_TROCA - 1) * 100
    cat("    Razão BPAN4/BPAC11: véspera", round(razao_vespera, 4), "| 14/10", round(razao_dia, 4),
        "(", round((razao_dia / RELACAO_TROCA - 1) * 100, 2), "% da relação de troca de", RELACAO_TROCA, ")\n")
    cat("    Desvio da razão em relação à relação de troca, de 14/10 em diante: média",
        round(mean(desvio_pos), 2), "% | dentro de ±2% em", sum(abs(desvio_pos) <= 2), "de", length(desvio_pos), "pregões\n")
  } else {
    cat("    Coluna Fechamento_Hist ausente na planilha: razão de preços não calculada.\n")
  }
  
  # Série de correlação móvel (30 dias) — só para o gráfico, não para os
  # números acima. Sem título na imagem: a legenda do LaTeX descreve a figura.
  p_corr_roll <- ret_df %>%
    filter(Data >= as.Date("2025-08-01")) %>%
    ggplot(aes(x = Data, y = corr_roll)) +
    geom_line(linewidth = 0.7, color = "#b2182b") +
    geom_vline(xintercept = as.numeric(ANUNCIO_INCORPORACAO), linetype = "dashed", color = "grey40") +
    geom_hline(yintercept = 0, linetype = "dotted", color = "grey60") +
    coord_cartesian(ylim = c(-1, 1)) +
    labs(x = NULL, y = "Correlação") +
    theme_minimal(base_size = 11) +
    theme(plot.background = element_rect(fill = "white", color = NA))
  
  ggsave(outfile("graficos/corr_bpan4_bpac11_rolling.png"), p_corr_roll, width = 10, height = 5, dpi = 300, bg = "white")
  cat("    Gráfico salvo em", outfile("graficos/corr_bpan4_bpac11_rolling.png"), "\n")
}

# ==============================================================================
# SEÇÃO 5 — ESTIMAÇÃO DA VOLATILIDADE: ROGERS-SATCHELL + OVERNIGHT
# ==============================================================================
# Estimador de 1 dia, sem sobreposição — evita a autocorrelação mecânica na
# defasagem 21 que uma janela deslizante atualizada diariamente induziria
# (observações consecutivas compartilhando 20 de 21 dias). Sem NAs de
# aquecimento de janela; independente de drift (ao contrário de
# Garman-Klass, mantido como comparação comentada abaixo).

cat("\n[5/11] Estimando volatilidade Rogers-Satchell + overnight...\n")

rogers_satchell_overnight <- function(ohlc_mat) {
  O <- as.numeric(ohlc_mat[, "Open"])
  H <- as.numeric(ohlc_mat[, "High"])
  L <- as.numeric(ohlc_mat[, "Low"])
  C <- as.numeric(ohlc_mat[, "Close"])
  C_lag <- c(NA_real_, C[-length(C)])          # Close do pregão anterior
  
  overnight2 <- (log(O / C_lag))^2             # termo overnight (1 dia, sem médias)
  rs         <- log(H / C) * log(H / O) + log(L / C) * log(L / O)   # Rogers-Satchell
  
  sig2 <- overnight2 + rs
  idx_regra <- which(!is.na(sig2) & sig2 < 1e-8)
  sig2[idx_regra] <- NA       # [INCLUSÃO — Diego, 24/09/2026] Substitui a linha anterior
  # (sig2 < 0 → NA). Por quê: num pregão que abre no fechamento anterior e na máxima e fecha na
  # mínima (ou abre na mínima e fecha na máxima), os três termos se anulam — log(O/C_lag) = 0,
  # log(H/O) = 0 e log(L/C) = 0 — e o estimador dá variância zero por construção, embora o papel
  # tenha percorrido toda a amplitude do dia (2% a 6% nos casos da amostra; registros conferidos
  # no COTAHIST da B3, não são erro de dado). RS é não negativo com OHLC consistente: o valor
  # negativo que a regra antiga capturava não vinha de microestrutura, e sim do ruído de ponto
  # flutuante do fator de ajuste, que deixa esses dias em ±1e-12. Com sinal negativo, o dia virava
  # NA; com sinal positivo, entrava no VAR como log-vol ≈ −11 (14 a 19 desvios da média), inflando
  # a variância própria e derrubando o TCI. O limiar trata os dois casos da mesma forma: dia sem
  # informação para o estimador de um dia → NA → imputação por Kalman na Seção 7. O limiar de
  # 1e-8 (desvio-padrão diário de 0,01%) está muito abaixo do 1º percentil de qualquer série;
  # 1e-6 dá resultados praticamente idênticos. Ver Feedback 4, Seção 3.1.
  
  valido <- !is.na(sig2)
  p_on   <- 100 * sum(overnight2[valido]) / sum(sig2[valido])   # parcela do overnight (%)
  
  out <- sqrt(sig2)                             # desvio-padrão diário, não anualizado
  attr(out, "datas_regra") <- index(ohlc_mat)[idx_regra]
  attr(out, "parcela_overnight") <- p_on
  out                             # desvio-padrão diário, não anualizado
}

vol_rs <- list()
contagem_regra <- list()
parcela_overnight <- list()


for (nome in NOMES) {
  d <- dados_adj[[nome]]
  ohlc_mat <- d[, c("Open","High","Low","Close")]
  
  n_nas <- sum(is.na(ohlc_mat))
  if (n_nas > 0) cat(" ", nome, ":", n_nas, "NAs no OHLC (mantidos até a Seção 7)\n")
  
  sd_diario <- rogers_satchell_overnight(ohlc_mat)
  parcela_overnight[[nome]] <- data.frame(
    Banco = nome,
    Parcela_overnight_pct = round(attr(sd_diario, "parcela_overnight"), 1)
  )
  
  # Pregões removidos pela regra sig2 < 1e-8 (só os que tinham OHLC válido)
  datas_regra <- attr(sd_diario, "datas_regra")
  contagem_regra[[nome]] <- data.frame(
    Banco                = nome,
    Pregoes              = nrow(ohlc_mat),
    Removidos_pela_regra = length(datas_regra),
    Pct                  = round(100 * length(datas_regra) / nrow(ohlc_mat), 2)
  )
  if (length(datas_regra) > 0)
    cat("   ", nome, "→ pregões de variância nula:", paste(format(datas_regra), collapse = ", "), "\n")
  
  vol_anual <- as.numeric(sd_diario) * sqrt(252)   # anualizado, convenção Diebold-Yilmaz (2012, 2014)
  vol_rs[[nome]] <- xts(vol_anual, order.by = index(d))
  
  n_nas_vol <- sum(is.na(vol_anual))
  if (n_nas_vol > 0) cat("   ", nome, "→", n_nas_vol, "NAs na volatilidade estimada\n")
}

parcela_overnight <- do.call(rbind, parcela_overnight); rownames(parcela_overnight) <- NULL
cat("\n  Parcela do termo overnight na soma de sigma2 (%):\n")
print(parcela_overnight, row.names = FALSE)
write.csv(parcela_overnight, outfile("tabelas/parcela_overnight.csv"), row.names = FALSE)

contagem_regra <- do.call(rbind, contagem_regra); rownames(contagem_regra) <- NULL
cat("\n  Pregões removidos pela regra sig2 < 1e-8 (por banco):\n")
print(contagem_regra, row.names = FALSE)
write.csv(contagem_regra, outfile("tabelas/dias_variancia_nula.csv"), row.names = FALSE)

cat("  OK — volatilidade Rogers-Satchell+overnight estimada para", length(vol_rs), "séries.\n")


# ==============================================================================
# SEÇÃO 6 — TRANSFORMAÇÃO LOGARÍTMICA
# ==============================================================================

cat("\n[6/11] Transformação logarítmica...\n")

PISO_NUMERICO <- 1e-8
log_vol <- list()

for (nome in NOMES) {
  vol <- as.numeric(vol_rs[[nome]])
  lv  <- log(pmax(vol, PISO_NUMERICO))
  log_vol[[nome]] <- xts(lv, order.by = index(vol_rs[[nome]]))
}

cat("  OK — log-volatilidade calculada para", length(log_vol), "séries.\n")


# ==============================================================================
# SEÇÃO 7 — ALINHAMENTO EM PAINEL T×8 E IMPUTAÇÃO POR FILTRO DE KALMAN
# ==============================================================================

cat("\n[7/11] Alinhando painel T×8 e imputando NAs (filtro de Kalman)...\n")



painel_raw <- do.call(merge, c(log_vol, all = TRUE))
colnames(painel_raw) <- NOMES

cat("\n  NAs por banco (antes de tirar os dias sem pregão):\n")
print(colSums(is.na(painel_raw)))
cat("\n  Distribuição de NAs por dia:\n")
print(table(rowSums(is.na(painel_raw))))

dias_sem_pregao <- rowSums(is.na(painel_raw)) == ncol(painel_raw)
cat("\n  Dias sem pregão removidos:", sum(dias_sem_pregao), "\n")
painel_raw <- painel_raw[!dias_sem_pregao, ]

# Contagem de imputados DEPOIS de tirar os dias sem pregão. O primeiro pregão da janela
# não tem fechamento anterior (NA em todos os bancos) e sai junto com os dias sem pregão,
# então não conta como imputado; antes, a contagem feita acima incluía esse dia.
n_imputados <- colSums(is.na(painel_raw))
cat("\n  NAs por banco a imputar (depois de tirar os dias sem pregão):\n")
print(n_imputados)

cat("  Imputando NAs com filtro de Kalman (auto.arima)...\n")
painel_imp <- apply(painel_raw, 2, function(col) {
  if (any(is.na(col))) na_kalman(col, model = "auto.arima", smooth = TRUE) else col
})
painel_imp <- xts(painel_imp, order.by = index(painel_raw))
colnames(painel_imp) <- NOMES

stopifnot("Ainda há NAs no painel após imputação!" = sum(is.na(painel_imp)) == 0)

tabela_pretratamento <- data.frame(
  Banco                     = NOMES,
  Dias_abaixo_piso_liquidez = as.integer(unlist(contagem_liquidez[NOMES])),
  Dias_variancia_nula       = contagem_regra$Removidos_pela_regra[match(NOMES, contagem_regra$Banco)],
  Total_imputado            = as.integer(n_imputados[NOMES])
)
cat("\n  Pré-tratamento por banco:\n")
print(tabela_pretratamento, row.names = FALSE)
write.csv(tabela_pretratamento, outfile("tabelas/pretratamento_por_banco.csv"), row.names = FALSE)

cat("  OK — painel final:", nrow(painel_imp), "observações ×", ncol(painel_imp), "bancos. Zero NAs.\n")

write.csv(data.frame(data = index(painel_imp), as.data.frame(painel_imp)),
          outfile("tabelas/painel_logvol.csv"), row.names = FALSE)


# ==============================================================================
# SEÇÃO 8 — TESTES DE ESTACIONARIEDADE E MEMÓRIA LONGA (PAINEL PÓS-KALMAN)
# ==============================================================================
# Roda sobre painel_imp, não sobre a série pré-imputação com na.omit().

cat("\n[8/11] Testes de estacionariedade e memória longa (painel pós-Kalman)...\n")

resultados_testes <- data.frame()

for (nome in NOMES) {
  lv <- as.numeric(painel_imp[, nome])
  
  adf_res     <- ur.df(lv, type = "drift", selectlags = "BIC")
  tau_stat    <- adf_res@teststat["statistic", "tau2"]
  tau_cv5     <- adf_res@cval["tau2", "5pct"]
  adf_rejeita <- tau_stat < tau_cv5
  
  kpss_res      <- tryCatch(kpss.test(lv, null = "Level"), error = function(e) NULL)
  kpss_pval     <- if (!is.null(kpss_res)) kpss_res$p.value else NA
  kpss_nrejeita <- if (!is.na(kpss_pval)) kpss_pval > 0.05 else NA
  
  ers_res <- tryCatch(ur.ers(lv, type = "DF-GLS", model = "constant", lag.max = 4),
                      error = function(e) NULL)
  ers_stat    <- if (!is.null(ers_res)) ers_res@teststat else NA
  ers_cv5     <- if (!is.null(ers_res)) ers_res@cval[1, "5pct"] else NA
  ers_rejeita <- if (!is.na(ers_stat) && !is.na(ers_cv5)) ers_stat < ers_cv5 else NA
  
  pp_res <- tryCatch(ur.pp(lv, type = "Z-tau", model = "constant", lags = "short"),
                     error = function(e) NULL)
  pp_stat    <- if (!is.null(pp_res)) pp_res@teststat else NA
  pp_cv5     <- if (!is.null(pp_res)) pp_res@cval[1, "5pct"] else NA
  pp_rejeita <- if (!is.na(pp_stat) && !is.na(pp_cv5)) pp_stat < pp_cv5 else NA
  
  # ur.za@cval = c(1%, 5%, 10%), vetor sem nomes
  za_res <- tryCatch(ur.za(lv, model = "both", lag = 4), error = function(e) NULL)
  za_stat    <- if (!is.null(za_res)) za_res@teststat else NA
  za_cv5     <- if (!is.null(za_res)) za_res@cval[2] else NA
  za_rejeita <- if (!is.na(za_stat) && !is.na(za_cv5)) za_stat < za_cv5 else NA
  
  lb_pval <- Box.test(lv, lag = 20, type = "Ljung-Box")$p.value
  arch_res  <- tryCatch(ArchTest(lv, lags = 10), error = function(e) NULL)
  arch_pval <- if (!is.null(arch_res)) arch_res$p.value else NA
  
  # Integração fracionária (Geweke-Porter-Hudak): a série pode não ser I(1)
  # nem I(0) — ADF e KPSS rejeitando simultaneamente é a assinatura clássica
  # de memória longa, não uma contradição a resolver. d in (0, 0.5):
  # estacionária com memória longa; d in [0.5, 1): não-estacionária com
  # memória longa. bandw.exp = 0.5 é o default (K = trunc(n^0.5)).
  gph_res <- tryCatch(fracdiff::fdGPH(lv, bandw.exp = 0.5), error = function(e) NULL)
  gph_d      <- if (!is.null(gph_res)) gph_res$d     else NA
  gph_sd_as  <- if (!is.null(gph_res)) gph_res$sd.as else NA
  
  resultados_testes <- rbind(resultados_testes, data.frame(
    Banco = nome,
    ADF_tau = round(tau_stat, 3), ADF_cv5pct = round(tau_cv5, 3), ADF_rejeita_H0 = adf_rejeita,
    KPSS_pval = round(kpss_pval, 3), KPSS_nao_rejeita = kpss_nrejeita,
    DFGLS_stat = round(ers_stat, 3), DFGLS_cv5pct = round(ers_cv5, 3), DFGLS_rejeita_H0 = ers_rejeita,
    PP_stat = round(pp_stat, 3), PP_cv5pct = round(pp_cv5, 3), PP_rejeita_H0 = pp_rejeita,
    ZA_stat = round(za_stat, 3), ZA_cv5pct = round(za_cv5, 3), ZA_rejeita_H0 = za_rejeita,
    LjungBox_p = round(lb_pval, 4), ARCH_LM_p = round(arch_pval, 4),
    GPH_d = round(gph_d, 3), GPH_SE_asint = round(gph_sd_as, 3),
    stringsAsFactors = FALSE
  ))
}

cat("\n  Resultados dos testes de estacionariedade e memória longa:\n")
print(resultados_testes)

falhas_adf   <- resultados_testes$Banco[resultados_testes$ADF_rejeita_H0 == FALSE]
falhas_kpss  <- resultados_testes$Banco[resultados_testes$KPSS_nao_rejeita == FALSE]
falhas_dfgls <- resultados_testes$Banco[resultados_testes$DFGLS_rejeita_H0 == FALSE]
falhas_pp    <- resultados_testes$Banco[resultados_testes$PP_rejeita_H0 == FALSE]
falhas_za    <- resultados_testes$Banco[resultados_testes$ZA_rejeita_H0 == FALSE]
if (length(falhas_adf) > 0)   warning("ADF NÃO rejeitou raiz unitária em: ", paste(falhas_adf, collapse=", "))
if (length(falhas_kpss) > 0)  warning("KPSS rejeitou estacionariedade em: ", paste(falhas_kpss, collapse=", "))
if (length(falhas_dfgls) > 0) warning("DF-GLS NÃO rejeitou raiz unitária em: ", paste(falhas_dfgls, collapse=", "))
if (length(falhas_pp) > 0)    warning("Phillips-Perron NÃO rejeitou raiz unitária em: ", paste(falhas_pp, collapse=", "))
if (length(falhas_za) > 0)    warning("Zivot-Andrews NÃO rejeitou raiz unitária em: ", paste(falhas_za, collapse=", "))

cat("\n  Nota: ADF rejeitando a raiz unitária e KPSS rejeitando a estacionariedade\n",
    "  ao mesmo tempo (comum aqui) é a assinatura de memória longa (0 < d < 1):\n",
    "  nem I(0) nem I(1). Ver coluna GPH_d.\n")

write.csv(resultados_testes, outfile("tabelas/testes_estacionariedade.csv"), row.names = FALSE)
cat("  Tabela salva em", outfile("tabelas/testes_estacionariedade.csv"), "\n")


# ==============================================================================
# SEÇÃO 9 — ESTATÍSTICAS DESCRITIVAS
# ==============================================================================

cat("\n[9/11] Calculando estatísticas descritivas...\n")

skewness_fn <- function(x) mean((x - mean(x))^3) / sd(x)^3
kurtosis_fn <- function(x) mean((x - mean(x))^4) / sd(x)^4 - 3

desc <- apply(painel_imp, 2, function(col) {
  c(Media = mean(col), DP = sd(col), Min = min(col), Mediana = median(col),
    Max = max(col), Assimetria = skewness_fn(col), Curtose = kurtosis_fn(col))
})

desc_df <- as.data.frame(t(round(desc, 4)))
cat("\n  Estatísticas descritivas (log-volatilidade):\n")
print(desc_df)

write.csv(desc_df, outfile("tabelas/estatisticas_descritivas.csv"))
cat("  Salvo em", outfile("tabelas/estatisticas_descritivas.csv"), "\n")


# ==============================================================================
# SEÇÃO 10 — VAR(2) + GFEVD + ÍNDICES DY — MODELO ESTÁTICO (FULL SAMPLE)
# ==============================================================================

cat("\n[10/11] Estimando VAR(2) + GFEVD (full sample)...\n")

Y <- zoo(as.matrix(painel_imp), order.by = index(painel_imp))

cat("  Selecionando número de defasagens (BIC)...\n")
var_select <- vars::VARselect(Y, lag.max = 6, type = "const")

bic_tabela <- data.frame(
  Lags = 1:6,
  AIC = round(var_select$criteria["AIC(n)", ], 4),
  BIC = round(var_select$criteria["SC(n)", ], 4),
  HQ  = round(var_select$criteria["HQ(n)", ], 4)
)
cat("\n  BIC por número de defasagens:\n")
print(bic_tabela)
write.csv(bic_tabela, outfile("tabelas/var_selecao_lags.csv"), row.names = FALSE)
cat("  BIC selecionado:", var_select$selection["SC(n)"], "defasagens\n")
cat("  Especificação adotada: p =", P_LAGS, "(justificada na metodologia)\n")

cat("\n  Verificando estabilidade do VAR...\n")
# vars::VAR/roots qualificados por namespace — ConnectednessApproach mascara VAR().
var_est <- vars::VAR(Y, p = P_LAGS, type = "const")
raizes  <- vars::roots(var_est, modulus = TRUE)
cat("  Módulo máximo das raízes:", round(max(raizes), 4))
if (max(raizes) < 1) {
  cat(" → VAR ESTÁVEL ✓\n")
} else {
  stop("VAR instável! Módulo máximo = ", round(max(raizes), 4), ". Revisar dados.")
}

cat("\n  Computando tabela de spillovers (GFEVD, H = ", H_HORIZONTE, ")...\n")

dca_full <- ConnectednessApproach(
  Y, nlag = P_LAGS, nfore = H_HORIZONTE, window = NULL, corrected = FALSE, model = "VAR"
)

tci_full     <- dca_full$TCI
to_full      <- dca_full$TO
from_full    <- dca_full$FROM
net_full     <- dca_full$NET
tabela_gfevd <- dca_full$TABLE

cat("\n  Total Connectedness Index (TCI):", round(tci_full, 2), "%\n")

tabela_dy <- data.frame(
  Banco = NOMES,
  TO   = round(as.numeric(to_full), 2),
  FROM = round(as.numeric(from_full), 2),
  NET  = round(as.numeric(net_full), 2)
)
tabela_dy <- tabela_dy[order(-tabela_dy$TO), ]
rownames(tabela_dy) <- NULL

cat("\n  Tabela DY (full sample):\n")
print(tabela_dy)

# ── Demonstração: o nível do TCI sobe com N ────────────────────────────────
if (PAINEL == "N7_principal") {
  set.seed(1)
  tci_vs_n <- do.call(rbind, lapply(c(2, 4, 6, 7), function(n) {
    tcis <- replicate(20, {
      sub <- sample(colnames(Y), n)
      as.numeric(ConnectednessApproach(Y[, sub], nlag = P_LAGS, nfore = H_HORIZONTE,
                                       window = NULL, corrected = FALSE, model = "VAR")$TCI)
    })
    data.frame(N = n, TCI_medio = round(mean(tcis), 1))
  }))
  cat("\n  TCI médio da amostra completa por número de bancos:\n")
  print(tci_vs_n, row.names = FALSE)
  write.csv(tci_vs_n, outfile("tabelas/tci_vs_N.csv"), row.names = FALSE)
}

write.csv(tabela_dy, outfile("tabelas/dy_fullsample.csv"), row.names = FALSE)
write.csv(tabela_gfevd, outfile("tabelas/gfevd_fullsample.csv"))
cat("  Tabelas salvas.\n")

# ==============================================================================
# SEÇÃO 11 (APÊNDICE) — JANELA ROLANTE (W = 200, H = 10)
# ==============================================================================
# A janela rolante data o pico de conectividade na borda direita da janela
# (quando ela está mais cheia de dados de crise), não na data do evento
# real. Mantida como comparação com a convenção de Diebold-Yilmaz (2012);
# a especificação dinâmica principal é o TVP-VAR (Seção 12).

cat("\n[11/11 — apêndice] Janela rolante (W =", N_JANELA_W, ", H =", H_HORIZONTE, ")...\n")

cat("Painel:", nrow(Y), "observações | de:", as.character(index(Y)[1]),
    "até:", as.character(index(Y)[nrow(Y)]), "\n")

dca_roll <- ConnectednessApproach(
  Y, nlag = P_LAGS, nfore = H_HORIZONTE, window = N_JANELA_W, corrected = FALSE, model = "VAR"
)

tci_rolling <- zoo(dca_roll$TCI[, 1], order.by = as.Date(rownames(dca_roll$TCI)))
datas_roll  <- index(tci_rolling)

to_rolling   <- zoo(dca_roll$TO,   order.by = datas_roll)
from_rolling <- zoo(dca_roll$FROM, order.by = datas_roll)
net_rolling  <- zoo(dca_roll$NET,  order.by = datas_roll)

colnames(to_rolling) <- colnames(from_rolling) <- colnames(net_rolling) <- NOMES

cat("OK —", length(datas_roll), "janelas | de:", as.character(datas_roll[1]),
    "até:", as.character(datas_roll[length(datas_roll)]), "\n")

write.csv(data.frame(data = datas_roll, TCI = as.numeric(tci_rolling)),
          outfile("tabelas/tci_rolling.csv"), row.names = FALSE)
write.csv(data.frame(data = datas_roll, as.data.frame(net_rolling)),
          outfile("tabelas/net_rolling.csv"), row.names = FALSE)

tci_df <- data.frame(data = datas_roll, TCI = as.numeric(tci_rolling))

# Gráfico auxiliar (não entra no texto): rótulos dos eventos na base, como na figura do TVP-VAR.
p_tci <- ggplot(tci_df, aes(x = data, y = TCI)) +
  geom_line(color = "#2166ac", linewidth = 0.7) +
  geom_vline(data = EVENTOS, aes(xintercept = as.numeric(data)),
             linetype = "dashed", color = "grey40", linewidth = 0.5) +
  rotulos_eventos(min(tci_df$TCI)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "Total Connectedness Index — Sistema Bancário Brasileiro (2019–2025)",
       subtitle = paste0("VAR(", P_LAGS, "), GFEVD H = ", H_HORIZONTE, ", janela W = ", N_JANELA_W, " dias úteis"),
       x = NULL, y = "TCI (%)") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA))

ggsave(outfile("graficos/tci_rolling.png"), p_tci, width = 12, height = 5, dpi = 300, bg = "white")
cat("  Gráfico TCI salvo.\n")

net_df <- data.frame(data = datas_roll, as.data.frame(net_rolling)) |>
  pivot_longer(-data, names_to = "Banco", values_to = "NET")

teto_net <- max(abs(net_df$NET), na.rm = TRUE)

p_net <- ggplot(net_df, aes(x = data, y = NET, color = Banco)) +
  geom_line(linewidth = 0.5, alpha = 0.85) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.4) +
  geom_vline(data = EVENTOS, aes(xintercept = as.numeric(data)),
             linetype = "dotted", color = "grey50", linewidth = 0.4, inherit.aes = FALSE) +
  geom_text(data = EVENTOS, aes(x = data, y = teto_net * 0.92, label = label),
            angle = 90, hjust = 1, vjust = -0.3, size = 2.2, color = "grey30", inherit.aes = FALSE) +
  facet_wrap(~Banco, ncol = 4) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-teto_net, teto_net)) +
  labs(title = "Spillover NET por instituição — janela rolante (W = 200)",
       subtitle = "Positivo = transmissor líquido; negativo = receptor líquido. Escala fixa entre painéis.",
       x = NULL, y = "NET (%)") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none", strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA))

ggsave(outfile("graficos/net_rolling.png"), p_net, width = 14, height = 8, dpi = 300, bg = "white")
cat("  Gráfico NET salvo.\n")

# ── Figura comparativa: TCI para W = 150, 200, 250 sobrepostos ─────────────
# Demonstra que o degrau/pico do TCI se desloca junto com o tamanho da
# janela — evidência de artefato de estimação, não mudança de regime.
# W=200 já foi calculado acima (dca_roll/tci_rolling); faltam W=150 e
# W=250, com o mesmo estimador, defasagens e horizonte.

cat("\n  Rodando janelas adicionais para a figura comparativa (W = 150, 250)...\n")

JANELAS_COMPARACAO <- c(150, 200, 250)

dca_roll_150 <- ConnectednessApproach(
  Y, nlag = P_LAGS, nfore = H_HORIZONTE, window = 150, corrected = FALSE, model = "VAR"
)
dca_roll_250 <- ConnectednessApproach(
  Y, nlag = P_LAGS, nfore = H_HORIZONTE, window = 250, corrected = FALSE, model = "VAR"
)

tci_150 <- zoo(dca_roll_150$TCI[, 1], order.by = as.Date(rownames(dca_roll_150$TCI)))
tci_250 <- zoo(dca_roll_250$TCI[, 1], order.by = as.Date(rownames(dca_roll_250$TCI)))

tci_comparacao <- rbind(
  data.frame(data = index(tci_150),   TCI = as.numeric(tci_150),   W = "W = 150"),
  data.frame(data = index(tci_rolling), TCI = as.numeric(tci_rolling), W = "W = 200"),
  data.frame(data = index(tci_250),   TCI = as.numeric(tci_250),   W = "W = 250")
)
tci_comparacao$W <- factor(tci_comparacao$W, levels = c("W = 150", "W = 200", "W = 250"))

write.csv(tci_comparacao, outfile("tabelas/tci_comparacao_janelas.csv"), row.names = FALSE)

# Data em que o colapso deixa cada janela: observação do colapso (fim do
# período, 23/03/2020) + W pregões. Índice na série diária do painel, não
# na série já defasada pela janela.
COLAPSO_REF <- as.Date("2020-03-23")
datas_painel <- index(painel_imp)
idx_colapso  <- which(datas_painel >= COLAPSO_REF)[1]

datas_saida <- sapply(JANELAS_COMPARACAO, function(w) {
  idx_saida <- idx_colapso + w
  if (idx_saida <= length(datas_painel)) as.character(datas_painel[idx_saida]) else NA
})

eventos_saida <- data.frame(
  data  = as.Date(datas_saida),
  W     = factor(paste0("W = ", JANELAS_COMPARACAO), levels = levels(tci_comparacao$W)),
  label = format(as.Date(datas_saida), "%d/%m/%Y")
)
cat("  Colapso (ref.", as.character(COLAPSO_REF), ") deixa cada janela em:\n")
print(eventos_saida[, c("W", "data")], row.names = FALSE)

cores_janela <- c("W = 150" = "#2166ac", "W = 200" = "#b2182b", "W = 250" = "#1a9850")

teto <- max(tci_comparacao$TCI, na.rm = TRUE)
eventos_saida$y_label <- teto * c(0.99, 0.90, 0.81)[match(eventos_saida$W, levels(eventos_saida$W))]

# Sem título nem subtítulo dentro da imagem: a legenda do LaTeX explica a faixa cinza e as
# linhas verticais.
p_comparacao <- ggplot(tci_comparacao, aes(x = data, y = TCI, color = W)) +
  annotate("rect", xmin = as.Date("2020-03-12"), xmax = as.Date("2020-03-23"),
           ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
  geom_line(linewidth = 0.6) +
  geom_vline(data = eventos_saida, aes(xintercept = as.numeric(data), color = W),
             linetype = "dashed", linewidth = 0.5, show.legend = FALSE) +
  geom_label(data = eventos_saida, aes(x = data, y = y_label, label = label, color = W),
             size = 2.6, hjust = 0, fontface = "bold", label.size = 0,
             fill = "white", show.legend = FALSE) +
  scale_color_manual(values = cores_janela, name = NULL) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0.05, 0.1))) +
  labs(x = NULL, y = "TCI (%)") +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    plot.margin = margin(t = 12, r = 16, b = 10, l = 10),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave(outfile("graficos/tci_comparacao_janelas.png"), p_comparacao, width = 12, height = 7, dpi = 300, bg = "white")
cat("  Gráfico comparativo (W = 150/200/250) salvo.\n")


# ==============================================================================
# SEÇÃO 12 — TVP-VAR COM FATORES DE ESQUECIMENTO (ESPECIFICAÇÃO PRINCIPAL)
# ==============================================================================
# Substitui a janela rolante como especificação dinâmica principal. Em vez
# de um corte abrupto após W dias, os coeficientes do VAR (beta_t) e a
# covariância dos choques (Sigma_t) evoluem por fatores de esquecimento
# kappa1 e kappa2, sem descontinuidade e sem perder as primeiras
# observações da amostra. kappa1 governa a velocidade de deriva dos
# coeficientes (Eq. 3); kappa2, a velocidade de adaptação da covariância
# (Eq. 4).
#
# O pacote ConnectednessApproach estima o TVP-VAR final dado (kappa1,
# kappa2) — isso é só um argumento de função. O que NÃO existe pronto é a
# SELEÇÃO de kappa1/kappa2: implementada abaixo o filtro de Kalman com
# fatores de esquecimento (Eq. 1-6) para rodar a grade e escolher o par por
# verossimilhança preditiva fora da amostra.
#
# AVISO DE DESEMPENHO: a grade tem 6 x 3 = 18 combinações, cada uma rodando
# o filtro sobre a amostra inteira (~1700 observações, estado de dimensão
# N*(N*p+1) = 136). Em R puro isso pode levar alguns minutos.

cat("\n[12] TVP-VAR — seleção de fatores de esquecimento por verossimilhança preditiva...\n")

# ── Passo 1: matriz Y e amostra de treino ───────────────────────────────────
Y_mat <- as.matrix(painel_imp)
N     <- ncol(Y_mat)
Tt    <- nrow(Y_mat)
T0    <- 200   # observações de treino

var_treino <- vars::VAR(Y_mat[1:T0, ], p = P_LAGS, type = "const")

# beta0: coeficientes empilhados equação a equação, na MESMA ordem de
# z_t = (y_{t-1}, ..., y_{t-p}, 1) — que é a ordem que vars::VAR usa
# internamente (lag 1 de todas as variáveis, lag 2 de todas, ..., const).
beta0  <- as.numeric(unlist(lapply(var_treino$varresult, coef)))
P0     <- vcov(var_treino)                      # covariância dos coeficientes (OLS, treino)
Sigma0 <- stats::cov(residuals(var_treino))      # covariância dos resíduos (treino)

k_dim <- N * (N * P_LAGS + 1)
stopifnot(
  "Dimensão de beta0 não bate com N*(N*p+1)" = length(beta0) == k_dim,
  "Dimensão de P0 não bate com N*(N*p+1)"    = all(dim(P0) == k_dim)
)
cat("  Treino: T0 =", T0, "obs | dimensão do estado k =", k_dim, "\n")

# ── Passo 2: filtro de Kalman com fatores de esquecimento ──────────────────
construir_zt <- function(Y_mat, t, p) {
  N <- ncol(Y_mat)
  z <- numeric(N * p + 1)
  for (l in 1:p) z[((l - 1) * N + 1):(l * N)] <- Y_mat[t - l, ]
  z[N * p + 1] <- 1
  z
}

tvpvar_loglik <- function(Y_mat, p, kappa1, kappa2, t0, beta0, P0, Sigma0) {
  N  <- ncol(Y_mat)
  Tt <- nrow(Y_mat)
  
  beta  <- beta0
  P     <- P0
  Sigma <- Sigma0
  loglik <- 0
  
  for (t in (p + 1):Tt) {
    zt <- construir_zt(Y_mat, t, p)
    Zt <- diag(N) %x% matrix(zt, nrow = 1)        # N x k, Eq. (1)
    
    P_pred <- P / kappa1                          # Eq. (3)
    Ft     <- Zt %*% P_pred %*% t(Zt) + Sigma      # Eq. (5), usa Sigma_{t-1}
    et     <- Y_mat[t, ] - as.numeric(Zt %*% beta)
    
    Ft_inv <- solve(Ft)
    
    if (t > t0) {
      logdetF <- as.numeric(determinant(Ft, logarithm = TRUE)$modulus)
      loglik  <- loglik - 0.5 * (N * log(2 * pi) + logdetF + as.numeric(t(et) %*% Ft_inv %*% et))  # Eq. (6)
    }
    
    Kt   <- P_pred %*% t(Zt) %*% Ft_inv
    beta <- beta + as.numeric(Kt %*% et)
    P    <- P_pred - Kt %*% Zt %*% P_pred
    
    eps_hat <- Y_mat[t, ] - as.numeric(Zt %*% beta)
    Sigma   <- kappa2 * Sigma + (1 - kappa2) * (eps_hat %*% t(eps_hat))   # Eq. (4)
  }
  
  loglik
}

# ── Passo 3: grade e verossimilhança preditiva ──────────────────────────────
grade_k1 <- c(0.94, 0.96, 0.98, 0.99, 0.995, 1.00)
grade_k2 <- c(0.94, 0.96, 0.99, if (ESTENDER_GRADE_K2) 0.995)   # 0,995 só se ESTENDER_GRADE_K2 = TRUE

grade <- expand.grid(kappa1 = grade_k1, kappa2 = grade_k2)
grade$loglik <- NA_real_

for (i in seq_len(nrow(grade))) {
  cat("  (", i, "/", nrow(grade), ") kappa1 =", grade$kappa1[i], ", kappa2 =", grade$kappa2[i], "... ")
  ll <- tvpvar_loglik(Y_mat, P_LAGS, grade$kappa1[i], grade$kappa2[i], T0, beta0, P0, Sigma0)
  grade$loglik[i] <- ll
  cat("logL =", round(ll, 1), "\n")
}

# ── Passo 4: pesos posteriores ───────────────────────────────────────────────
grade$loglik_rel <- grade$loglik - max(grade$loglik)   # subtrai o máximo antes de exponenciar
grade$peso_post  <- exp(grade$loglik_rel) / sum(exp(grade$loglik_rel))

grade_tabela <- grade[order(-grade$peso_post), ]
cat("\n  Grade completa (ordenada por peso posterior):\n")
print(grade_tabela, row.names = FALSE)

write.csv(grade_tabela, outfile("tabelas/tvpvar_grade_kappa.csv"), row.names = FALSE)

# kappa1 = 1 (coeficientes constantes) é caso de fronteira que nosso filtro
# de Kalman lida bem na busca em grade, mas o ConnectednessApproach exige
# kappa1 ESTRITAMENTE entre 0 e 1 para estimar o TVP-VAR final — excluído
# antes de escolher o vencedor.
if (grade_tabela$kappa1[1] >= 1 && any(grade_tabela$kappa1 < 1)) {
  warning("kappa1 = 1 teve o maior peso posterior, mas não pode ser usado no ",
          "ConnectednessApproach (exige kappa1 < 1). Selecionando o melhor par ",
          "com kappa1 < 1 em seu lugar.")
}
melhor <- grade_tabela[grade_tabela$kappa1 < 1, ][1, ]
KAPPA1 <- melhor$kappa1
KAPPA2 <- melhor$kappa2
cat("\n  Par selecionado: kappa1 =", KAPPA1, ", kappa2 =", KAPPA2,
    "(peso posterior =", round(melhor$peso_post, 4), ")\n")
if (KAPPA2 == max(grade_k2)) {
  cat("  AVISO: kappa2 selecionado está no limite superior da grade (", max(grade_k2), ").\n", sep = "")
}

# ── Passo 5: estimação final via ConnectednessApproach ──────────────────────
cat("\n  Estimando TVP-VAR final (kappa1 =", KAPPA1, ", kappa2 =", KAPPA2, ")...\n")

dca_tvp <- ConnectednessApproach(
  Y, nlag = P_LAGS, nfore = H_HORIZONTE, model = "TVP-VAR", connectedness = "Time",
  VAR_config = list(TVPVAR = list(kappa1 = KAPPA1, kappa2 = KAPPA2, prior = "BayesPrior", gamma = 0.01))
)

tci_tvp <- zoo(dca_tvp$TCI[, 1], order.by = as.Date(rownames(dca_tvp$TCI)))
net_tvp <- zoo(dca_tvp$NET, order.by = index(tci_tvp))
colnames(net_tvp) <- NOMES

cat("  OK —", length(tci_tvp), "observações de TCI dinâmico (TVP-VAR).\n")

write.csv(data.frame(data = index(tci_tvp), TCI = as.numeric(tci_tvp)),
          outfile("tabelas/tci_tvpvar.csv"), row.names = FALSE)
write.csv(data.frame(data = index(net_tvp), as.data.frame(net_tvp)),
          outfile("tabelas/net_tvpvar.csv"), row.names = FALSE)

# Figura do texto (Figura do TCI do TVP-VAR): sem título na imagem; eventos da Tabela de
# cronologia, com rótulos na base para não cobrir a curva.
p_tci_tvp <- ggplot(data.frame(data = index(tci_tvp), TCI = as.numeric(tci_tvp)),
                    aes(x = data, y = TCI)) +
  geom_line(color = "#b2182b", linewidth = 0.7) +
  geom_vline(data = EVENTOS, aes(xintercept = as.numeric(data)),
             linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_label(data = EVENTOS, aes(x = data, y = ylab, label = label, hjust = hj),
             inherit.aes = FALSE, size = 3.6, fill = "white", color = "grey20",
             label.size = 0, label.padding = unit(0.15, "lines")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  coord_cartesian(ylim = c(min(as.numeric(tci_tvp)) - 2, 88)) +
  labs(x = NULL, y = "TCI (%)") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA))

ggsave(outfile("graficos/tci_tvpvar.png"), p_tci_tvp, width = 8, height = 4.4, dpi = 300, bg = "white")
cat("  Gráfico TCI (TVP-VAR) salvo.\n")


# ==============================================================================
# SEÇÃO 13 — REDES: MATRIZ DE ADJACÊNCIA (FULL SAMPLE)
# ==============================================================================

cat("\n[13] Construindo matriz de adjacência (grafo completo, sem threshold)...\n")

cat("  Nomes disponíveis em dca_full:\n")
print(names(dca_full))
cat("\n  Dimensões de dca_full$CT:", paste(dim(dca_full$CT), collapse = " x "), "\n")
print(dimnames(dca_full$CT))

CT_arr <- dca_full$CT
nd <- length(dim(CT_arr))

if (nd == 2) {
  theta <- CT_arr
} else if (nd == 3) {
  theta <- CT_arr[ , , 1]
} else if (nd == 4) {
  theta <- CT_arr[ , , 1, dim(CT_arr)[4]]
} else {
  stop("Formato inesperado de dca_full$CT (", nd, " dimensões).")
}

stopifnot("theta não é NOMES x NOMES" = all(dim(theta) == length(NOMES)))
dimnames(theta) <- list(NOMES, NOMES)

cat("\n  Matriz Θ̃ extraída (", nrow(theta), "x", ncol(theta), "):\n")
print(round(theta, 3))
cat("\n  Soma de cada linha (deve ser ≈ 1 — Eq. 6.8):\n")
print(round(rowSums(theta), 4))

theta_offdiag <- theta
diag(theta_offdiag) <- 0

# Teste de sanidade: TCI manual (Eq. 6.9) vs. dca_full$TCI — confirma que a
# fatia extraída do array está correta.
tci_manual <- mean(theta_offdiag) * length(NOMES) * 100

cat("\n  Verificação — TCI manual vs. TCI do pacote:\n")
cat("    Manual:", round(tci_manual, 4), "%  |  Pacote:", round(dca_full$TCI, 4), "%\n")

if (abs(tci_manual - as.numeric(dca_full$TCI)) > 0.05) {
  warning("TCI manual e do pacote NÃO batem — revise a indexação de dca_full$CT.")
} else {
  cat("OK.\n")
}

# ==============================================================================
# SEÇÃO 14 — REDES: ORIENTAÇÃO DAS ARESTAS E OBJETO IGRAPH (FULL SAMPLE)
# ==============================================================================

cat("\n[14] Construindo grafo dirigido (g_full) com orientação corrigida...\n")

# theta_ij = fluxo j -> i (texto), mas igraph lê mat[i,j] como aresta
# saindo de i. Transpor corrige a direção sem gerar erro de execução caso
# esquecido — por isso o teste de sanidade abaixo é obrigatório.
adj_full <- t(theta_offdiag)

g_full <- graph_from_adjacency_matrix(adj_full, mode = "directed", weighted = TRUE, diag = FALSE)

cat("  Grafo g_full:", vcount(g_full), "nós,", ecount(g_full), "arestas",
    "(esperado:", length(NOMES), "nós,", length(NOMES) * (length(NOMES) - 1), "arestas)\n")

# strength(out) = TOi (Eq. 6.11), strength(in) = FROMi (Eq. 6.10) — sem
# dividir por N. Sinal de NETi tem que bater com dca_full$NET.
to_grafo   <- igraph::strength(g_full, mode = "out") * 100
from_grafo <- igraph::strength(g_full, mode = "in")  * 100
net_grafo  <- to_grafo - from_grafo

net_pacote <- as.numeric(dca_full$NET)
names(net_pacote) <- NOMES

comparacao_net <- data.frame(
  Banco = NOMES,
  NET_pacote = round(net_pacote[NOMES], 2),
  NET_grafo  = round(net_grafo[NOMES], 2),
  Mesmo_sinal = sign(net_pacote[NOMES]) == sign(net_grafo[NOMES])
)
cat("\n  Verificação de direção — NET do pacote vs. NET do grafo:\n")
print(comparacao_net, row.names = FALSE)

if (!all(comparacao_net$Mesmo_sinal)) {
  warning("Sinal de NET do grafo não bate com dca_full$NET — confira a transposição.")
} else {
  cat("\n  OK — sinais batem em todos os bancos.\n")
}

V(g_full)$name <- NOMES


# ==============================================================================
# SEÇÃO 15 — REDES: MÉTRICAS DE CENTRALIDADE (FULL SAMPLE)
# ==============================================================================

cat("\n[15] Calculando métricas de centralidade (grafo completo)...\n")

# eigen_centrality(directed=TRUE) já é a versão "in" por construção (nó
# importante se é apontado por nós importantes) — sem ajuste manual. Ou
# seja: mede o quanto o banco RECEBE de bancos importantes, não transmite.
eigen_full <- igraph::eigen_centrality(g_full, directed = TRUE)$vector

# PageRank segue a direção j -> i por padrão, mesma lógica desejada.
pagerank_full <- igraph::page_rank(g_full, directed = TRUE, weights = E(g_full)$weight)$vector

metricas_centralidade <- data.frame(
  Banco = V(g_full)$name,
  Eigenvector_in = round(eigen_full[V(g_full)$name], 4),
  PageRank       = round(pagerank_full[V(g_full)$name], 4)
)
metricas_centralidade <- metricas_centralidade[order(-metricas_centralidade$PageRank), ]
rownames(metricas_centralidade) <- NULL

cat("\n  Métricas de centralidade (full sample, ordenado por PageRank):\n")
print(metricas_centralidade)

write.csv(metricas_centralidade, outfile("tabelas/centralidade_fullsample.csv"), row.names = FALSE)
cat("  Salvo em", outfile("tabelas/centralidade_fullsample.csv"), "\n")

# ── Diagnóstico: as métricas recursivas ordenam os bancos como FROM e TO? ───
# A Metodologia afirma que o PageRank e o autovetor ordenam como o FROM no grafo original e
# como o TO no grafo invertido (correlação de postos de Spearman igual a 1 nos quatro casos).
# Este bloco calcula os quatro números que sustentam a frase; se algum for menor que 1,
# a frase do texto precisa ser suavizada.
g_inv <- graph_from_adjacency_matrix(t(adj_full), mode = "directed", weighted = TRUE, diag = FALSE)
V(g_inv)$name <- NOMES
eigen_inv    <- igraph::eigen_centrality(g_inv, directed = TRUE)$vector
pagerank_inv <- igraph::page_rank(g_inv, directed = TRUE, weights = E(g_inv)$weight)$vector

spearman_centralidade <- data.frame(
  Comparacao = c("PageRank (grafo original) x FROM", "Autovetor (grafo original) x FROM",
                 "PageRank (grafo invertido) x TO",  "Autovetor (grafo invertido) x TO"),
  Spearman = round(c(
    cor(pagerank_full[NOMES], from_grafo[NOMES], method = "spearman"),
    cor(eigen_full[NOMES],    from_grafo[NOMES], method = "spearman"),
    cor(pagerank_inv[NOMES],  to_grafo[NOMES],   method = "spearman"),
    cor(eigen_inv[NOMES],     to_grafo[NOMES],   method = "spearman")
  ), 4)
)
cat("\n  Correlação de postos (Spearman) entre centralidade recursiva e FROM/TO:\n")
print(spearman_centralidade, row.names = FALSE)
if (any(spearman_centralidade$Spearman < 1)) {
  warning("Algum Spearman é menor que 1: a frase da Metodologia ('igual a 1 nos quatro casos') precisa ser ajustada.")
}
write.csv(spearman_centralidade, outfile("tabelas/spearman_centralidade.csv"), row.names = FALSE)

# Consolida TO/FROM/NET (Seção 10) com as métricas de centralidade numa
# tabela só.
tabela_dy_rede <- merge(tabela_dy, metricas_centralidade, by = "Banco")
tabela_dy_rede <- tabela_dy_rede[order(-tabela_dy_rede$TO), ]
rownames(tabela_dy_rede) <- NULL

cat("\n  Tabela consolidada (DY + centralidade):\n")
print(tabela_dy_rede)

write.csv(tabela_dy_rede, outfile("tabelas/dy_centralidade_fullsample.csv"), row.names = FALSE)
cat("  Salvo em", outfile("tabelas/dy_centralidade_fullsample.csv"), "\n")

# ── Confronto com a segmentação prudencial do BCB ───────────────────────────
# O ranking comparado ao S1 é de TRANSMISSÃO (TOi, Eq. 6.11): um banco
# sistemicamente importante é aquele cujo estresse se propaga ao sistema.
# PageRank/eigenvector_in medem receptividade (direção de entrada) e ficam na
# tabela apenas como métricas complementares.
# Base normativa: Res. CMN 4.553/2017 (segmentação S1-S5) e Res. CMN 4.958/2021,
# art. 8º, §2º (ACPSistêmico se aplica ao S1).
# S1: Banco do Brasil, Bradesco, BTG Pactual, Caixa, Itaú, Santander.
# Só o enquadramento no S1 entra no texto; os demais segmentos (S2, S3) não foram conferidos
# contra a lista oficial do BCB, por isso os bancos fora do S1 aparecem sem rótulo de segmento.
segmento_bcb <- c(
  ITUB4 = "S1", BBDC4 = "S1", BBAS3 = "S1", SANB11 = "S1", BPAC11 = "S1",
  BRSR6 = "Fora do S1", ABCB4 = "Fora do S1", BPAN4 = "Fora do S1"
)

comparacao_dsib <- tabela_dy_rede   # TO, FROM, NET, Eigenvector_in, PageRank
comparacao_dsib$Segmento_BCB <- segmento_bcb[comparacao_dsib$Banco]
comparacao_dsib$S1_BCB       <- comparacao_dsib$Segmento_BCB == "S1"
comparacao_dsib <- comparacao_dsib[order(-comparacao_dsib$TO), ]
comparacao_dsib$Rank_rede_TO <- seq_len(nrow(comparacao_dsib))
rownames(comparacao_dsib) <- NULL

cat("\n  Confronto: ranking de transmissão (TOi) vs. enquadramento no S1 do BCB:\n")
print(comparacao_dsib)

write.csv(comparacao_dsib, outfile("tabelas/comparacao_dsib.csv"), row.names = FALSE)
cat("  Tabela salva em", outfile("tabelas/comparacao_dsib.csv"), "\n")

# Classificação manual (Seção 6.1.1) — usada aqui e no preenchimento dos
# nós da Seção 16.
tipo_controle <- c(
  ITUB4 = "Privado nacional", BBDC4 = "Privado nacional",
  BBAS3 = "Público federal", SANB11 = "Privado estrangeiro",
  BPAC11 = "Privado nacional (BTG)", BRSR6 = "Público estadual",
  ABCB4 = "Privado estrangeiro", BPAN4 = "Privado nacional (BTG)"
)

# Para as figuras de rede: no painel N = 7 não há Pan, então o BTG não precisa de categoria
# própria; a categoria "(BTG)" só existe no painel que inclui o par ligado por controle.
tipo_plot <- tipo_controle
if (!"BPAN4" %in% NOMES) tipo_plot["BPAC11"] <- "Privado nacional"

# Mesmas cores da paleta Set2, mas atribuídas por nome: não mudam entre painéis nem entre
# a figura da amostra completa e a dos subperíodos.
CORES_CONTROLE <- c(
  "Privado nacional"       = "#66C2A5",
  "Privado nacional (BTG)" = "#FC8D62",
  "Privado estrangeiro"    = "#8DA0CB",
  "Público federal"        = "#E78AC3",
  "Público estadual"       = "#A6D854"
)

# Sufixo dos arquivos de figura de rede (n7 ou n8), igual ao esperado pelo LaTeX.
SUF <- if (length(NOMES) == 7) "n7" else "n8"

# Porte: grande (Itaú, Bradesco, BB, Santander, BTG) vs. médio (Banrisul,
# ABC Brasil, Banco Pan).
porte <- c(
  ITUB4 = "Grande", BBDC4 = "Grande", BBAS3 = "Grande", SANB11 = "Grande",
  BPAC11 = "Grande", BRSR6 = "Médio", ABCB4 = "Médio", BPAN4 = "Médio"
)

# ── Regressão diádica 

cat("\n  Montando painel diádico (pares i != j)...\n")

estatal  <- c("BBAS3", "BRSR6")
natureza <- function(b) ifelse(b %in% estatal, "Estatal", "Privado")

pares_ij <- expand.grid(receptor = NOMES, emissor = NOMES, stringsAsFactors = FALSE)
pares_ij <- pares_ij[pares_ij$receptor != pares_ij$emissor, ]

pares_ij$theta_pct      <- mapply(function(i, j) theta_offdiag[i, j] * 100,
                                  pares_ij$receptor, pares_ij$emissor)
pares_ij$mesmo_controle <- as.integer(natureza(pares_ij$receptor) == natureza(pares_ij$emissor))
pares_ij$ambos_grande   <- as.integer(porte[pares_ij$receptor] == "Grande" & porte[pares_ij$emissor] == "Grande")
pares_ij$par_id         <- apply(cbind(pares_ij$receptor, pares_ij$emissor), 1, function(x) paste(sort(x), collapse = "-"))
pares_ij$vinculo        <- as.integer(pares_ij$par_id == "BPAC11-BPAN4")   # sempre 0 no N=7

cat("  ", nrow(pares_ij), "observações (pares dirigidos),", length(unique(pares_ij$par_id)), "pares não-dirigidos (clusters)\n")

termos <- c("mesmo_controle", "ambos_grande")
if ("BPAN4" %in% NOMES) termos <- c(termos, "vinculo")   # só no painel de extensão

lm_diadica <- lm(
  as.formula(paste("theta_pct ~ factor(emissor) + factor(receptor) +", paste(termos, collapse = " + "))),
  data = pares_ij
)

# Erros-padrão clusterizados por par não-dirigido: theta_ij e theta_ji
# nascem da mesma relação bilateral, resíduos não são independentes.
vcov_cluster  <- sandwich::vcovCL(lm_diadica, cluster = pares_ij$par_id)
teste_diadica <- lmtest::coeftest(lm_diadica, vcov = vcov_cluster)

rotulos <- c(mesmo_controle = "Mesma natureza de controle (estatal/privado)",
             ambos_grande   = "Ambos de grande porte",
             vinculo        = "Vínculo societário (BTG e Pan)")

resultado_diadica <- data.frame(
  Indicadora  = unname(rotulos[termos]),
  Coeficiente = round(teste_diadica[termos, "Estimate"], 3),
  Erro_padrao = round(teste_diadica[termos, "Std. Error"], 3),
  t           = round(teste_diadica[termos, "t value"], 2),
  p           = round(teste_diadica[termos, "Pr(>|t|)"], 4)
)
rownames(resultado_diadica) <- NULL

cat("\n  Regressão diádica — coeficientes de interesse (p.p. de variância explicada):\n")
print(resultado_diadica, row.names = FALSE)

write.csv(resultado_diadica, outfile("tabelas/regressao_diadica_fullsample.csv"), row.names = FALSE)
write.csv(pares_ij, outfile("tabelas/painel_diadico.csv"), row.names = FALSE)
cat("  Tabelas salvas.\n")

# ── Indicadoras por par específico (substitui mesmo_controle) ──────────────
# A dummy agregada mesmo_controle mistura pares fortes (vínculo societário)
# com pares que só compartilham uma etiqueta nominal. Substitui por uma
# indicadora por par que compartilha categoria — mesma lógica da Tabela 3
# (segunda metade), mas ainda em corte único (full sample estático): cada
# indicador tem só 2 observações (i->j e j->i daquele par) depois dos
# efeitos fixos, então a precisão aqui é baixa — a versão robusta vem da
# extensão temporal (cortes mensais), próximo passo.
par_especifico <- function(a, b, banco1, banco2) {
  as.integer((a == banco1 & b == banco2) | (a == banco2 & b == banco1))
}

# Só entra na regressão o par cujos dois bancos existem no painel ativo —
# par_btg_pan, por exemplo, não existe no painel N=7 (sem BPAN4).
PARES_ESPECIFICOS <- list(
  par_itub_bbdc = list(bancos = c("ITUB4", "BBDC4"),  label = "Bradesco e Itaú (privados nacionais)"),
  par_btg_pan   = list(bancos = c("BPAC11", "BPAN4"), label = "BTG e Pan (vínculo societário)"),
  par_bb_brsr   = list(bancos = c("BBAS3", "BRSR6"),  label = "Banco do Brasil e Banrisul (estatais)"),
  par_abc_san   = list(bancos = c("ABCB4", "SANB11"), label = "ABC Brasil e Santander (\"estrangeiros\")")
)
pares_disponiveis <- Filter(function(p) all(p$bancos %in% NOMES), PARES_ESPECIFICOS)

if (length(pares_disponiveis) < length(PARES_ESPECIFICOS)) {
  faltando <- setdiff(names(PARES_ESPECIFICOS), names(pares_disponiveis))
  cat("  Pares fora do painel atual (banco ausente em NOMES):", paste(faltando, collapse = ", "), "\n")
}

for (var_nome in names(pares_disponiveis)) {
  bancos <- pares_disponiveis[[var_nome]]$bancos
  pares_ij[[var_nome]] <- par_especifico(pares_ij$receptor, pares_ij$emissor, bancos[1], bancos[2])
}

vars_par    <- names(pares_disponiveis)
formula_par <- as.formula(paste(
  "theta_pct ~ factor(emissor) + factor(receptor) +",
  paste(vars_par, collapse = " + "), "+ ambos_grande"
))
lm_diadica_par <- lm(formula_par, data = pares_ij)

vcov_cluster_par  <- sandwich::vcovCL(lm_diadica_par, cluster = pares_ij$par_id)
teste_diadica_par <- lmtest::coeftest(lm_diadica_par, vcov = vcov_cluster_par)

vars_todos   <- c(vars_par, "ambos_grande")
labels_todos <- c(sapply(pares_disponiveis, function(p) p$label), "Ambos de grande porte")

resultado_diadica_par <- data.frame(
  Indicadora  = labels_todos,
  Coeficiente = round(teste_diadica_par[vars_todos, "Estimate"], 3),
  Erro_padrao = round(teste_diadica_par[vars_todos, "Std. Error"], 3),
  t           = round(teste_diadica_par[vars_todos, "t value"], 2),
  p           = round(teste_diadica_par[vars_todos, "Pr(>|t|)"], 4)
)

cat("\n  Regressão diádica — indicadoras por par específico (corte único, preliminar):\n")
print(resultado_diadica_par, row.names = FALSE)

write.csv(resultado_diadica_par, outfile("tabelas/regressao_diadica_par_especifico.csv"), row.names = FALSE)
cat("  Tabela salva em", outfile("tabelas/regressao_diadica_par_especifico.csv"), "\n")


# ── Regressão diádica: extensão temporal (cortes mensais) ──────────────────
# Em vez de uma matriz theta única (full sample), o TVP-VAR (Seção 12) já
# produz uma matriz theta_t por dia. Reestima a regressão diádica por par
# específico em cada corte mensal e resume a série resultante de
# coeficientes — a robustez vem de o sinal se repetir corte a corte, não do
# p-valor de uma estimativa isolada.

cat("\n  Extensão temporal da regressão diádica — extraindo cortes mensais do TVP-VAR...\n")

# Inspecionar antes de indexar — mesmo cuidado da Seção 13 com dca_full$CT.
cat("  Dimensões de dca_tvp$CT:", paste(dim(dca_tvp$CT), collapse = " x "), "\n")

CT_tvp   <- dca_tvp$CT
nd_tvp   <- length(dim(CT_tvp))
datas_tvp <- index(tci_tvp)

stopifnot(
  "Terceira dimensão de dca_tvp$CT não bate com o número de datas do TCI-TVP" =
    dim(CT_tvp)[3] == length(datas_tvp)
)

# Um corte por mês: último dia disponível de cada mês (evita usar dias
# consecutivos quase idênticos, que não trazem informação nova).
meses           <- format(datas_tvp, "%Y-%m")
idx_ultimo_dia  <- !duplicated(meses, fromLast = TRUE)
idx_corte       <- which(idx_ultimo_dia)
datas_corte     <- datas_tvp[idx_corte]

cat("  ", length(datas_corte), "cortes mensais, de", as.character(min(datas_corte)),
    "a", as.character(max(datas_corte)), "\n")

coefs_temporais <- data.frame()
formula_par_t <- as.formula(paste(
  "theta_pct ~ factor(emissor) + factor(receptor) +",
  paste(vars_par, collapse = " + "), "+ ambos_grande"
))

for (k in seq_along(idx_corte)) {
  t_idx <- idx_corte[k]
  dt    <- datas_corte[k]
  
  if (nd_tvp == 3) {
    theta_t <- CT_tvp[, , t_idx]
  } else if (nd_tvp == 4) {
    theta_t <- CT_tvp[, , t_idx, dim(CT_tvp)[4]]
  } else {
    stop("Formato inesperado de dca_tvp$CT (", nd_tvp, " dimensões).")
  }
  dimnames(theta_t) <- list(NOMES, NOMES)
  diag(theta_t) <- 0
  
  pares_t <- expand.grid(receptor = NOMES, emissor = NOMES, stringsAsFactors = FALSE)
  pares_t <- pares_t[pares_t$receptor != pares_t$emissor, ]
  pares_t$theta_pct    <- mapply(function(i, j) theta_t[i, j] * 100, pares_t$receptor, pares_t$emissor)
  pares_t$ambos_grande <- as.integer(porte[pares_t$receptor] == "Grande" & porte[pares_t$emissor] == "Grande")
  for (var_nome in vars_par) {
    bancos <- pares_disponiveis[[var_nome]]$bancos
    pares_t[[var_nome]] <- par_especifico(pares_t$receptor, pares_t$emissor, bancos[1], bancos[2])
  }
  
  lm_t <- tryCatch(lm(formula_par_t, data = pares_t), error = function(e) NULL)
  if (is.null(lm_t)) next
  
  cf <- coef(lm_t)
  linha <- data.frame(data = dt)
  for (var_nome in vars_par) linha[[var_nome]] <- unname(cf[var_nome])
  linha$ambos_grande <- unname(cf["ambos_grande"])
  coefs_temporais <- rbind(coefs_temporais, linha)
}

cat("  ", nrow(coefs_temporais), "cortes estimados com sucesso.\n")

# Resumo: média da série de coeficientes, % de cortes com sinal positivo, e
# teste t da média contra zero. O teste t trata os cortes mensais como independentes e eles são
# autocorrelacionados (o TVP-VAR é suave), então o t superestima a evidência e serve só de
# referência; o que sustenta o argumento é a proporção de cortes com o mesmo sinal.
resumir_serie <- function(x) {
  x  <- x[!is.na(x)]
  tt <- t.test(x, mu = 0)
  data.frame(
    Media         = round(mean(x), 3),
    Pct_positivo  = paste0(sum(x > 0), "/", length(x)),
    t             = round(unname(tt$statistic), 2),
    p             = round(tt$p.value, 4)
  )
}

resumo_temporal <- do.call(rbind, c(
  lapply(vars_par, function(v) cbind(Indicadora = pares_disponiveis[[v]]$label, resumir_serie(coefs_temporais[[v]]))),
  list(cbind(Indicadora = "Ambos de grande porte", resumir_serie(coefs_temporais$ambos_grande)))
))

cat("\n  Regressão diádica — extensão temporal (", nrow(coefs_temporais), "cortes mensais):\n")
print(resumo_temporal, row.names = FALSE)

write.csv(coefs_temporais, outfile("tabelas/regressao_diadica_temporal_serie.csv"), row.names = FALSE)
write.csv(resumo_temporal, outfile("tabelas/regressao_diadica_temporal_resumo.csv"), row.names = FALSE)
cat("  Tabelas salvas.\n")

# ── Gráfico: coeficientes diádicos ao longo do tempo ────────────────────────
niveis_par  <- c(vars_par, "ambos_grande")
labels_curtos <- c(
  par_itub_bbdc = "Bradesco e Itaú", par_btg_pan = "BTG e Pan",
  par_bb_brsr = "BB e Banrisul", par_abc_san = "ABC Brasil e Santander",
  ambos_grande = "Ambos grande porte"
)

coefs_long <- coefs_temporais %>%
  pivot_longer(-data, names_to = "par", values_to = "coeficiente") %>%
  mutate(par = factor(par, levels = niveis_par, labels = labels_curtos[niveis_par]))

p_coefs_temporais <- ggplot(coefs_long, aes(x = data, y = coeficiente, color = par)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.4) +
  geom_line(linewidth = 0.6) +
  facet_wrap(~par, ncol = 1, scales = "free_y") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Coeficientes da regressão diádica por corte mensal (TVP-VAR)",
       subtitle = "Cada painel: coeficiente daquele par/indicadora, um valor por mês, com efeitos fixos de emissor/receptor",
       x = NULL, y = "Coeficiente (p.p.)") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none", strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA))

ggsave(outfile("graficos/coefs_diadicos_temporais.png"), p_coefs_temporais, width = 10, height = 12, dpi = 300, bg = "white")
cat("  Gráfico salvo em", outfile("graficos/coefs_diadicos_temporais.png"), "\n")

if ("BTG e Pan" %in% levels(coefs_long$par)) {
  serie_btgpan <- coefs_long %>% filter(par == "BTG e Pan")
  
  # Regimes da Tabela 13 (as médias saem da própria série, então batem com a tabela)
  regimes <- data.frame(
    ini = as.Date(c("2019-01-01", "2021-05-01", "2023-01-01")),
    fim = as.Date(c("2021-04-30", "2022-12-31", "2025-10-31"))
  )
  regimes$media <- mapply(function(a, b) mean(serie_btgpan$coeficiente[serie_btgpan$data >= a & serie_btgpan$data <= b]),
                          regimes$ini, regimes$fim)
  regimes$rotulo <- format(round(regimes$media, 2), nsmall = 2, decimal.mark = ",")
  regimes$meio   <- regimes$ini + (regimes$fim - regimes$ini) / 2
  
  p_coef_btgpan <- ggplot(serie_btgpan, aes(x = data, y = coeficiente)) +
    annotate("rect", xmin = min(serie_btgpan$data), xmax = as.Date("2019-09-30"),
             ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.35) +
    annotate("text", x = min(serie_btgpan$data), y = Inf, label = "Treino do filtro",
             hjust = 0, vjust = 1.6, size = 3.4, color = "grey25") +
    geom_vline(xintercept = as.numeric(as.Date("2021-05-01")), linetype = "dashed", color = "grey30") +
    geom_vline(xintercept = as.numeric(as.Date("2023-01-01")), linetype = "dotted", color = "grey50") +
    annotate("text", x = as.Date("2021-05-01"), y = Inf, label = " Controle integral do BTG (mai/2021)",
             hjust = 0, vjust = 1.6, size = 3.4, color = "grey25") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.4) +
    geom_segment(data = regimes, aes(x = ini, xend = fim, y = media, yend = media),
                 inherit.aes = FALSE, color = "#2166ac", linewidth = 1) +
    geom_line(linewidth = 0.8, color = "#b2182b") +
    geom_label(data = regimes, aes(x = meio, y = media, label = rotulo), inherit.aes = FALSE,
               size = 3.4, color = "#2166ac", fill = "white", label.size = 0, vjust = -0.3) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(x = NULL, y = "Coeficiente (p.p.)") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = NA))
  
  ggsave(outfile("graficos/coef_btgpan_temporal.png"), p_coef_btgpan, width = 8, height = 4.4, dpi = 300, bg = "white")
  cat("  Gráfico salvo em", outfile("graficos/coef_btgpan_temporal.png"), "\n")
}

# ── Médias anuais do coeficiente BTG-Pan ────────────────────────────────────
# Só existe no painel que inclui o BPAN4 (N=8 extensão) — no N=7 principal
# esse par não está presente, então o bloco é pulado.
if ("par_btg_pan" %in% names(coefs_temporais)) {
  coefs_temporais$ano <- format(coefs_temporais$data, "%Y")
  media_anual_btgpan <- aggregate(par_btg_pan ~ ano, data = coefs_temporais, FUN = mean)
  media_anual_btgpan$par_btg_pan <- round(media_anual_btgpan$par_btg_pan, 2)
  colnames(media_anual_btgpan) <- c("ano", "btg_pan")
  
  cat("\n  Coeficiente BTG-Pan — média por ano:\n")
  print(media_anual_btgpan, row.names = FALSE)
  
  write.csv(media_anual_btgpan, outfile("tabelas/btgpan_coeficiente_anual.csv"), row.names = FALSE)
  cat("  Tabela salva em", outfile("tabelas/btgpan_coeficiente_anual.csv"), "\n")
} else {
  cat("\n  Par BTG-Pan não presente neste painel (sem BPAN4) — tabela anual não gerada.\n")
}


# ==============================================================================
# SEÇÃO 16 — REDES: GRAFO DE VISUALIZAÇÃO (NET COLAPSADO, THRESHOLD RELATIVO)
# ==============================================================================

cat("\n[16] Construindo grafo de visualização (NET colapsado, threshold relativo",
    THRESHOLD_REL * 100, "% do par mais forte)...\n")

# NETij = theta_ji - theta_ij (Eq. 6.13): positivo = i -> j.
npdc <- adj_full - theta_offdiag
stopifnot("npdc deveria ser antissimétrica" = all(abs(npdc + t(npdc)) < 1e-12))

# NETij (diferença) é estruturalmente menor que theta_ij bruto e se dilui
# entre até 7 contrapartes — um threshold absoluto pensado para o grafo
# bruto (Opção A) poda quase tudo e isola nós. Por isso usamos corte
# relativo ao par mais forte observado.
pares <- combn(NOMES, 2, simplify = FALSE)
dist_netij <- data.frame(
  par   = sapply(pares, function(p) paste(p, collapse = "-")),
  valor = sapply(pares, function(p) abs(npdc[p[1], p[2]]) * 100)
)
dist_netij <- dist_netij[order(-dist_netij$valor), ]
cat("\n  Distribuição de |NETij| entre os", length(pares), "pares (p.p.):\n")
print(dist_netij, row.names = FALSE)
cat("\n  Máximo:", round(max(dist_netij$valor), 3),
    "| Mediana:", round(median(dist_netij$valor), 3),
    "| Mínimo:", round(min(dist_netij$valor), 3), "\n")

corte_net <- THRESHOLD_REL * max(dist_netij$valor) / 100

edges_net <- do.call(rbind, lapply(pares, function(par) {
  i <- par[1]; j <- par[2]
  valor <- npdc[i, j]
  if (abs(valor) < corte_net) return(NULL)
  if (valor > 0) data.frame(from = i, to = j, weight = abs(valor) * 100)
  else           data.frame(from = j, to = i, weight = abs(valor) * 100)
}))

cat("  Pares retidos após threshold:", nrow(edges_net), "de", length(pares), "possíveis\n")
print(edges_net[order(-edges_net$weight), ], row.names = FALSE)

g_net <- graph_from_data_frame(edges_net, directed = TRUE, vertices = data.frame(name = NOMES))

V(g_net)$TO   <- to_grafo[V(g_net)$name]
V(g_net)$NET  <- net_grafo[V(g_net)$name]
V(g_net)$Tipo <- tipo_plot[V(g_net)$name]

# Layout FR calculado uma vez — reaproveitar fixo nos painéis de subperíodo futuros.
set.seed(42)
layout_fr <- create_layout(g_net, layout = "fr")

# Sem título nem subtítulo dentro da imagem: a legenda do LaTeX descreve a figura.
LARG_IN <- 6.3   # largura útil do texto na ABNT (A4, margens 3/2 cm)

p_rede <- ggraph(layout_fr) +
  geom_edge_link(
    aes(width = weight, alpha = weight),
    arrow = arrow(length = unit(2.5, "mm"), type = "closed"),
    end_cap = circle(4, "mm"), color = "grey40"
  ) +
  scale_edge_width(range = c(0.3, 1.8), name = "|NET| par a par (p.p.)") +
  scale_edge_alpha(range = c(0.4, 0.9), guide = "none") +
  geom_node_point(aes(size = sqrt(TO), fill = Tipo), shape = 21, color = "black", stroke = 0.5) +
  scale_size_continuous(range = c(4, 11), name = "TO (raiz quadrada, p.p.)") +
  scale_fill_manual(values = CORES_CONTROLE, name = "Tipo de controle", drop = TRUE) +
  geom_node_text(aes(label = name), vjust = -1.5, size = 3.5, fontface = "bold") +  # 3,5 mm ≈ 10 pt
  scale_x_continuous(expand = expansion(mult = 0.15)) +
  scale_y_continuous(expand = expansion(mult = c(0.10, 0.18))) +
  coord_cartesian(clip = "off") +
  guides(
    fill = guide_legend(order = 1, nrow = 2, override.aes = list(size = 5)),
    size = guide_legend(order = 2, nrow = 1),
    edge_width = guide_legend(order = 3, nrow = 1)
  ) +
  theme_void(base_size = 10) +
  theme(
    legend.position   = "bottom",
    legend.box        = "vertical",
    legend.title      = element_text(size = 10, face = "bold"),
    legend.text       = element_text(size = 9),
    legend.key.size   = unit(5, "mm"),
    legend.spacing.y  = unit(1, "mm"),
    plot.margin       = margin(4, 8, 4, 8),
    plot.background   = element_rect(fill = "white", color = NA),
    panel.background  = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA)
  )

arq_rede <- outfile(sprintf("graficos/rede_%s_amostra_completa.png", SUF))
ggsave(arq_rede, p_rede, width = LARG_IN, height = 6.2, dpi = 300, bg = "white")
cat("  Grafo salvo em", arq_rede, "\n")


# ==============================================================================
# SEÇÃO 17 — REDES: PAINEL DE SUBPERÍODOS (LAYOUT FIXO)
# ==============================================================================
# Reestima a rede NET colapsada (mesma lógica da Seção 16) em 4 subperíodos,
# usando o layout Fruchterman-Reingold já calculado no full sample
# (layout_fr) — fixo entre os painéis, para que diferenças visuais reflitam
# reconfiguração da topologia, não reposicionamento dos nós.

cat("\n[17] Construindo painel de subperíodos (layout fixo)...\n")

SUBPERIODOS <- list(
  "Pré-COVID"    = c("2019-08-01", "2020-02-29"),
  "COVID-19"     = c("2020-03-01", "2020-08-31"),
  "Aperto Selic" = c("2021-03-17", "2021-09-17"),
  "Americanas"   = c("2023-01-12", "2023-07-12")
)

coords_fixas <- data.frame(name = layout_fr$name, x = layout_fr$x, y = layout_fr$y)
stopifnot("Coordenadas fixas não cobrem todos os bancos" = all(NOMES %in% coords_fixas$name))

# 1ª passada: estima cada subperíodo e guarda TO, NET e a matriz de NET par a par.
estimar_subperiodo <- function(data_ini, data_fim, titulo) {
  Y_sub <- Y[index(Y) >= as.Date(data_ini) & index(Y) <= as.Date(data_fim), ]
  cat("    [", titulo, "]", nrow(Y_sub), "pregões\n")
  dca_sub <- tryCatch(
    ConnectednessApproach(Y_sub, nlag = P_LAGS, nfore = H_HORIZONTE, window = NULL,
                          corrected = FALSE, model = "VAR"),
    error = function(e) NULL
  )
  if (is.null(dca_sub)) {
    cat("    [", titulo, "] falhou ao estimar — pulando.\n")
    return(NULL)
  }
  CT_sub <- dca_sub$CT
  nd_sub <- length(dim(CT_sub))
  theta_sub <- if (nd_sub == 2) CT_sub else if (nd_sub == 3) CT_sub[, , 1] else CT_sub[, , 1, dim(CT_sub)[4]]
  dimnames(theta_sub) <- list(NOMES, NOMES)
  diag(theta_sub) <- 0
  adj_sub  <- t(theta_sub)
  npdc_sub <- adj_sub - theta_sub
  g_dir_sub <- graph_from_adjacency_matrix(adj_sub, mode = "directed", weighted = TRUE, diag = FALSE)
  to_sub  <- igraph::strength(g_dir_sub, mode = "out") * 100
  net_sub <- to_sub - igraph::strength(g_dir_sub, mode = "in") * 100
  pares_sub <- combn(NOMES, 2, simplify = FALSE)
  dist_sub  <- sapply(pares_sub, function(p) abs(npdc_sub[p[1], p[2]]) * 100)
  list(titulo = titulo, data_ini = data_ini, data_fim = data_fim, npdc = npdc_sub,
       to = to_sub, net = net_sub, pares = pares_sub, max_net = max(dist_sub))
}

est_sub <- mapply(function(nome, datas) estimar_subperiodo(datas[1], datas[2], nome),
                  names(SUBPERIODOS), SUBPERIODOS, SIMPLIFY = FALSE)
est_sub <- Filter(Negate(is.null), est_sub)

# Escalas COMUNS aos quatro painéis, em valor absoluto.
LIM_NET  <- max(sapply(est_sub, function(e) e$max_net))
LIM_SIZE <- max(sapply(est_sub, function(e) sqrt(max(e$to))))
cat("  Escalas comuns: |NET| máximo =", round(LIM_NET, 2), "p.p. | raiz de TO máxima =", round(LIM_SIZE, 2), "\n")

# 2ª passada: desenha. O limiar de visualização (5% do par mais forte) continua sendo de cada painel.
desenhar_subperiodo <- function(e) {
  corte_sub <- THRESHOLD_REL * e$max_net / 100
  edges_sub <- do.call(rbind, lapply(e$pares, function(par) {
    i <- par[1]; j <- par[2]
    valor <- e$npdc[i, j]
    if (abs(valor) < corte_sub) return(NULL)
    if (valor > 0) data.frame(from = i, to = j, weight = abs(valor) * 100)
    else           data.frame(from = j, to = i, weight = abs(valor) * 100)
  }))
  cat("    [", e$titulo, "] pares retidos:", nrow(edges_sub), "de", length(e$pares), "\n")
  g_sub <- graph_from_data_frame(edges_sub, directed = TRUE, vertices = data.frame(name = NOMES))
  V(g_sub)$TO   <- e$to[V(g_sub)$name]
  V(g_sub)$Tipo <- tipo_plot[V(g_sub)$name]
  x_fixo <- coords_fixas$x[match(V(g_sub)$name, coords_fixas$name)]
  y_fixo <- coords_fixas$y[match(V(g_sub)$name, coords_fixas$name)]
  layout_sub <- create_layout(g_sub, layout = "manual", x = x_fixo, y = y_fixo)
  ggraph(layout_sub) +
    geom_edge_link(aes(width = weight), arrow = arrow(length = unit(2, "mm"), type = "closed"),
                   end_cap = circle(4, "mm"), color = "grey40", alpha = 0.7) +
    scale_edge_width(range = c(0.2, 1.4), limits = c(0, LIM_NET), name = "|NET| par a par (p.p.)") +
    geom_node_point(aes(size = sqrt(pmax(TO, 0)), fill = Tipo), shape = 21, color = "black", stroke = 0.5) +
    scale_size_continuous(range = c(3, 8), limits = c(0, LIM_SIZE), name = "TO (raiz quadrada, p.p.)") +
    scale_fill_manual(values = CORES_CONTROLE, name = "Tipo de controle", drop = TRUE) +
    labs(title = e$titulo, subtitle = paste0(e$data_ini, " a ", e$data_fim)) +
    geom_node_text(aes(label = name), size = 3, vjust = -1.6) +
    scale_x_continuous(expand = expansion(mult = 0.18)) +
    scale_y_continuous(expand = expansion(mult = c(0.12, 0.22))) +
    coord_cartesian(clip = "off") +
    labs(title = e$titulo, subtitle = paste0(e$data_ini, " a ", e$data_fim)) +
    theme_void(base_size = 9) +
    theme_void(base_size = 9) +
    theme(plot.title    = element_text(face = "bold", size = 10, hjust = 0.5),
          plot.subtitle = element_text(size = 8, color = "grey30", hjust = 0.5),
          plot.background = element_rect(fill = "white", color = NA),
          plot.margin = margin(4, 10, 4, 10))
}

paineis_sub <- lapply(est_sub, desenhar_subperiodo)

# As três legendas (cor, tamanho e espessura) são idênticas nos painéis e coletadas uma única vez.
p_subperiodos <- wrap_plots(paineis_sub, ncol = 2, guides = "collect") &
  guides(fill = guide_legend(nrow = 1, override.aes = list(size = 5))) &
  theme(legend.position = "bottom", legend.box = "vertical",
        legend.title = element_text(size = 9, face = "bold"),
        legend.text  = element_text(size = 8.5),
        legend.key.size = unit(4.5, "mm"),
        plot.background = element_rect(fill = "white", color = NA))

arq_sub <- outfile(sprintf("graficos/rede_%s_subperiodos.png", SUF))
ggsave(arq_sub, p_subperiodos, width = LARG_IN, height = 8.6, dpi = 300, bg = "white")

cat("  Painel de subperíodos salvo em", arq_sub, "(", length(paineis_sub), "de", length(SUBPERIODOS), "subperíodos estimados)\n")


# ==============================================================================
# SEÇÃO 18 (APÊNDICE) — ROBUSTEZ
# ==============================================================================
# Sensibilidade do TCI e do ranking de transmissor/receptor líquido a cinco
# dimensões: número de defasagens (p), horizonte da GFEVD (H), tamanho da
# janela rolante (W), fatores de esquecimento (kappa1/kappa2) e sub-amostras.
# Em cada bloco, reestima e reporta TCI + o banco com maior NET (transmissor
# líquido) e menor NET (receptor líquido) — a pergunta é se essa identidade
# se mantém estável entre as variações, não só se o TCI muda de nível.

cat("\n[18 — apêndice] Robustez...\n")

# ── A. Sensibilidade ao número de defasagens (p) ────────────────────────────
cat("\n  A) Variando p (defasagens do VAR, full sample estático)...\n")

robustez_p <- data.frame()
for (p_alt in 1:4) {
  dca_p <- ConnectednessApproach(Y, nlag = p_alt, nfore = H_HORIZONTE, window = NULL,
                                 corrected = FALSE, model = "VAR")
  net_p <- as.numeric(dca_p$NET)
  robustez_p <- rbind(robustez_p, data.frame(
    p = p_alt, TCI = round(dca_p$TCI, 2),
    Maior_transmissor = NOMES[which.max(net_p)], Maior_receptor = NOMES[which.min(net_p)]
  ))
}
cat("  Resultado:\n"); print(robustez_p, row.names = FALSE)
write.csv(robustez_p, outfile("tabelas/robustez_lags.csv"), row.names = FALSE)

# ── B. Sensibilidade ao horizonte da GFEVD (H) ──────────────────────────────
cat("\n  B) Variando H (horizonte da GFEVD, full sample estático)...\n")

robustez_h <- data.frame()
for (h_alt in c(5, 10, 15, 20)) {
  dca_h <- ConnectednessApproach(Y, nlag = P_LAGS, nfore = h_alt, window = NULL,
                                 corrected = FALSE, model = "VAR")
  net_h <- as.numeric(dca_h$NET)
  robustez_h <- rbind(robustez_h, data.frame(
    H = h_alt, TCI = round(dca_h$TCI, 2),
    Maior_transmissor = NOMES[which.max(net_h)], Maior_receptor = NOMES[which.min(net_h)]
  ))
}
cat("  Resultado:\n"); print(robustez_h, row.names = FALSE)
write.csv(robustez_h, outfile("tabelas/robustez_horizonte.csv"), row.names = FALSE)

# ── C. Sensibilidade ao tamanho da janela rolante (W) ───────────────────────
# Reaproveita as três séries já calculadas na Seção 11 (tci_comparacao).
cat("\n  C) Variando W (janela rolante) — reaproveitando Seção 11...\n")

robustez_w <- aggregate(TCI ~ W, data = tci_comparacao,
                        FUN = function(x) c(media = mean(x, na.rm = TRUE), dp = sd(x, na.rm = TRUE)))
robustez_w <- do.call(data.frame, robustez_w)
colnames(robustez_w) <- c("W", "TCI_medio", "TCI_dp")
robustez_w$TCI_medio <- round(robustez_w$TCI_medio, 2)
robustez_w$TCI_dp    <- round(robustez_w$TCI_dp, 2)

# Correlação entre pares de séries (alinhadas por data em comum)
wide_w <- tci_comparacao %>% pivot_wider(names_from = W, values_from = TCI) %>% na.omit()
cor_w <- cor(wide_w[, c("W = 150", "W = 200", "W = 250")])

cat("  TCI médio/DP por janela:\n"); print(robustez_w, row.names = FALSE)
cat("  Correlação entre séries (datas em comum):\n"); print(round(cor_w, 3))
write.csv(robustez_w, outfile("tabelas/robustez_janela.csv"), row.names = FALSE)

# ── D. Sensibilidade aos fatores de esquecimento (kappa1, kappa2) ──────────
# Avalia o par selecionado e seus vizinhos imediatos na grade (um passo em
# kappa1 ou em kappa2, com o outro fixo e kappa1 < 1). Critério de proximidade,
# não de ordenação por peso posterior: fora dos dois primeiros pares os pesos
# são numericamente nulos. O par selecionado reaproveita dca_tvp.
# AVISO: cada vizinho roda o TVP-VAR completo de novo (alguns minutos).
cat("\n  D) Variando (kappa1, kappa2) — par selecionado e vizinhos na grade...\n")

vizinhos_grade <- function(k1, k2, g1 = grade_k1, g2 = grade_k2) {
  i1 <- which(g1 == k1); i2 <- which(g2 == k2)
  idx1 <- c(i1 - 1, i1 + 1); idx1 <- idx1[idx1 >= 1 & idx1 <= length(g1)]
  idx2 <- c(i2 - 1, i2 + 1); idx2 <- idx2[idx2 >= 1 & idx2 <= length(g2)]
  cand <- rbind(data.frame(kappa1 = g1[idx1], kappa2 = k2),
                data.frame(kappa1 = k1,       kappa2 = g2[idx2]))
  cand[cand$kappa1 < 1, ]
}

pares_robustez <- rbind(
  data.frame(kappa1 = KAPPA1, kappa2 = KAPPA2, Tipo = "Selecionado"),
  transform(vizinhos_grade(KAPPA1, KAPPA2), Tipo = "Vizinho")
)
pares_robustez$peso_posterior <- grade_tabela$peso_post[
  match(paste(pares_robustez$kappa1, pares_robustez$kappa2),
        paste(grade_tabela$kappa1, grade_tabela$kappa2))
]

robustez_kappa <- data.frame()
for (i in seq_len(nrow(pares_robustez))) {
  k1 <- pares_robustez$kappa1[i]; k2 <- pares_robustez$kappa2[i]
  
  if (pares_robustez$Tipo[i] == "Selecionado") {
    dca_k <- dca_tvp   # já calculado na Seção 12
  } else {
    cat("    Estimando TVP-VAR para kappa1 =", k1, ", kappa2 =", k2, "...\n")
    dca_k <- ConnectednessApproach(
      Y, nlag = P_LAGS, nfore = H_HORIZONTE, model = "TVP-VAR", connectedness = "Time",
      VAR_config = list(TVPVAR = list(kappa1 = k1, kappa2 = k2, prior = "BayesPrior", gamma = 0.01))
    )
  }
  
  tci_k       <- as.numeric(dca_k$TCI[, 1])
  net_medio_k <- colMeans(dca_k$NET)
  
  robustez_kappa <- rbind(robustez_kappa, data.frame(
    kappa1 = k1, kappa2 = k2, Tipo = pares_robustez$Tipo[i],
    peso_posterior = round(pares_robustez$peso_posterior[i], 4),
    TCI_medio = round(mean(tci_k, na.rm = TRUE), 2),
    Maior_transmissor = NOMES[which.max(net_medio_k)],
    Maior_receptor    = NOMES[which.min(net_medio_k)]
  ))
}
cat("  Resultado:\n"); print(robustez_kappa, row.names = FALSE)
write.csv(robustez_kappa, outfile("tabelas/robustez_kappa.csv"), row.names = FALSE)

# ── E. Sub-amostras (primeira vs. segunda metade do período) ───────────────
cat("\n  E) Sub-amostras — primeira vs. segunda metade do período...\n")

n_total <- nrow(Y)
meio    <- floor(n_total / 2)
Y_primeira <- Y[1:meio, ]
Y_segunda  <- Y[(meio + 1):n_total, ]

robustez_sub <- data.frame()
for (nome_sub in c("Primeira metade", "Segunda metade")) {
  Y_sub <- if (nome_sub == "Primeira metade") Y_primeira else Y_segunda
  dca_sub <- ConnectednessApproach(Y_sub, nlag = P_LAGS, nfore = H_HORIZONTE, window = NULL,
                                   corrected = FALSE, model = "VAR")
  net_sub <- as.numeric(dca_sub$NET)
  robustez_sub <- rbind(robustez_sub, data.frame(
    Subamostra = nome_sub,
    Periodo = paste(as.character(index(Y_sub)[1]), "a", as.character(index(Y_sub)[nrow(Y_sub)])),
    TCI = round(dca_sub$TCI, 2),
    Maior_transmissor = NOMES[which.max(net_sub)], Maior_receptor = NOMES[which.min(net_sub)]
  ))
}
cat("  Resultado:\n"); print(robustez_sub, row.names = FALSE)
write.csv(robustez_sub, outfile("tabelas/robustez_subamostras.csv"), row.names = FALSE)

# ── Checagem final: a identidade do maior transmissor/receptor se mantém? ──
transmissores <- c(robustez_p$Maior_transmissor, robustez_h$Maior_transmissor,
                   robustez_kappa$Maior_transmissor, robustez_sub$Maior_transmissor)
receptores    <- c(robustez_p$Maior_receptor, robustez_h$Maior_receptor,
                   robustez_kappa$Maior_receptor, robustez_sub$Maior_receptor)

cat("\n  Estabilidade do maior transmissor líquido em", length(transmissores), "especificações:\n")
print(table(transmissores))
cat("\n  Estabilidade do maior receptor líquido em", length(receptores), "especificações:\n")
print(table(receptores))

# ==============================================================================
# REPRODUTIBILIDADE — sessionInfo()
# ==============================================================================
# Versões de pacote afetam resultados de auto.arima() (Seção 7) e do layout
# Fruchterman-Reingold (Seção 16) — salvar para referência.
writeLines(capture.output(sessionInfo()), outfile("sessionInfo.txt"))
cat("\n  sessionInfo() salva em", outfile("sessionInfo.txt"), "\n")