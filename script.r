# install.packages(c(
#   "quantmod",
#   "TTR",
#   "xts",
#   "zoo",
#   "imputeTS",
#   "urca",
#   "tseries",
#   "FinTS",
#   "vars",
#   "igraph",
#   "ggraph",
#   "tidygraph",
#   "ggplot2",
#   "dplyr",
#   "tidyr",
#   "scales",
#   "patchwork",
#   "ConnectednessApproach"
# ))

options(xts.warn_dplyr_breaks_lag = FALSE)

library(quantmod)
library(TTR)
library(xts)
library(zoo)
library(imputeTS)
library(urca)
library(tseries)
library(FinTS)
library(ConnectednessApproach)
library(vars)
library(igraph)
library(ggraph)
library(tidygraph)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# ── Diretório de outputs ──────────────────────────────────────────────────────
dir.create("outputs/tabelas",  recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/graficos", recursive = TRUE, showWarnings = FALSE)

# ── Constantes globais ────────────────────────────────────────────────────────
TICKERS <- c("ITUB4.SA", "BBDC4.SA", "BBAS3.SA", "SANB11.SA",
             "BPAC11.SA", "BRSR6.SA", "ABCB4.SA")

NOMES   <- c("ITUB4", "BBDC4", "BBAS3", "SANB11",
             "BPAC11", "BRSR6", "ABCB4")

DATA_INICIO <- "2014-01-01"
DATA_FIM    <- "2025-12-31"

N_JANELA_YZ <- 21     # janela Yang-Zhang 
N_JANELA_W  <- 200    # janela rolante do VAR 
H_HORIZONTE <- 10     # horizonte de previsão da GFEVD
P_LAGS      <- 2      # defasagens do VAR (especificação principal)
THRESHOLD   <- 0.05   # threshold de esparsificação do grafo (5%)

# Eventos de drift relevantes para anotação nos gráficos
EVENTOS <- data.frame(
  data  = as.Date(c("2017-05-18", "2020-03-11", "2022-01-01",
                    "2023-01-12", "2024-10-01")),
  label = c("Joesley Day", "COVID-19", "Aperto Selic",
            "Americanas", "Crise Fiscal 2024"),
  stringsAsFactors = FALSE
)

# ==============================================================================
# SEÇÃO 1 — COLETA DE DADOS
# ==============================================================================

cat("\n[1/13] Coletando dados do Yahoo Finance...\n")

dados_brutos <- list()

for (ticker in TICKERS) {
  nome <- sub("\\.SA$", "", ticker)
  cat("  Baixando", ticker, "...\n")

  tryCatch({
    dados_brutos[[nome]] <- getSymbols(
      ticker,
      src        = "yahoo",
      from       = DATA_INICIO,
      to         = DATA_FIM,
      auto.assign = FALSE,
      return.class = "xts"
    )
    Sys.sleep(0.5)
  }, error = function(e) {
    warning("Falha ao baixar ", ticker, ": ", e$message)
  })
}

# Verificação: todos os tickers foram baixados?
baixados <- names(dados_brutos)
faltando  <- setdiff(NOMES, baixados)
if (length(faltando) > 0) {
  stop("Tickers não baixados: ", paste(faltando, collapse = ", "),
       "\nVerifique a conexão ou os sufixos no Yahoo Finance.")
}

cat("  OK — ", length(dados_brutos), "séries coletadas.\n")

# Resumo de cada série: datas, número de observações, NAs
for (nome in NOMES) {
  d <- dados_brutos[[nome]]
  cat(nome, "| obs:", nrow(d), 
      "| de:", as.character(index(d)[1]),
      "| até:", as.character(index(d)[nrow(d)]),
      "| NAs:", sum(is.na(d)), "\n")
}

# ==============================================================================
# SEÇÃO 2 — AJUSTE POR PROVENTOS
# ==============================================================================

d <- dados_brutos[["ABCB4"]]
janela <- d["2015-12-28/2016-01-08"]
data.frame(
  data     = index(janela),
  Close    = as.numeric(Cl(janela)),
  Adjusted = as.numeric(Ad(janela)),
  phi      = as.numeric(Ad(janela)) / as.numeric(Cl(janela))
)


cat("\n[2/13] Ajustando OHLC por proventos...\n")

dados_adj <- list()

for (nome in NOMES) {
  raw <- dados_brutos[[nome]]

  # Extrair campos pelo índice de coluna (mais robusto que nome)
  op  <- as.numeric(Op(raw))
  hi  <- as.numeric(Hi(raw))
  lo  <- as.numeric(Lo(raw))
  cl  <- as.numeric(Cl(raw))
  vol <- as.numeric(Vo(raw))
  adj <- as.numeric(Ad(raw))
  dts <- index(raw)

  # Fator de ajuste proporcional
  phi <- adj / cl
  phi[is.na(phi) | is.infinite(phi)] <- 1  # dias com Close = 0 (não deve ocorrer)

  ohlc_adj <- xts(
    data.frame(
      Open   = op  * phi,
      High   = hi  * phi,
      Low    = lo  * phi,
      Close  = adj,         # AdjClose é o Close ajustado por definição
      Volume = vol
    ),
    order.by = dts
  )

  dados_adj[[nome]] <- ohlc_adj
}

cat("  OK — ajuste aplicado a", length(dados_adj), "séries.\n")



# ==============================================================================
# SEÇÃO 3 — SANITY CHECKS NO OHLC
# ==============================================================================

cat("\n[3/13] Verificando consistência OHLC...\n")

for (nome in NOMES) {
  d <- dados_adj[[nome]]

  op <- d$Open; hi <- d$High; lo <- d$Low; cl <- d$Close

  violacao <- (hi < lo)  | (hi < op)  | (hi < cl) |
    (lo > op)  | (lo > cl)  |
    (!is.na(op) & op <= 0)  | (!is.na(cl) & cl <= 0)
  violacao[is.na(violacao)] <- FALSE

  n_viol <- sum(violacao)
  if (n_viol > 0) {
    cat("  AVISO:", nome, "—", n_viol,
        "linha(s) com inconsistência OHLC. Convertidas para NA.\n")
    dados_adj[[nome]][violacao, ] <- NA
  }
}

cat("  OK — sanity checks concluídos.\n")


# ==============================================================================
# SEÇÃO 4 — FILTRO DE LIQUIDEZ
# ==============================================================================

cat("\n[4/13] Aplicando filtro de liquidez...\n")

VOLUME_MIN_QTD <- 1000       # ações
VOLUME_MIN_BRL <- 100000     # R$

for (nome in NOMES) {
  d   <- dados_adj[[nome]]
  vol <- as.numeric(d$Volume)
  cl  <- as.numeric(d$Close)

  vol_brl <- vol * cl

  filtro_qtd <- !is.na(vol) & (vol < VOLUME_MIN_QTD)
  filtro_brl <- !is.na(vol_brl) & (vol_brl < VOLUME_MIN_BRL)
  filtro     <- filtro_qtd | filtro_brl

  n_filtrados <- sum(filtro)
  if (n_filtrados > 0) {
    cat("  ", nome, ":", n_filtrados, "dia(s) com baixa liquidez → NA\n")
    dados_adj[[nome]][filtro, c("Open","High","Low","Close")] <- NA
  }
}

cat("  OK — filtro de liquidez aplicado.\n")


# ==============================================================================
# SEÇÃO 5 — IDENTIFICAÇÃO DE OUTLIERS
# ==============================================================================

cat("\n[5/13] Identificando outliers (|z| > 5)...\n")

THRESHOLD_Z <- 5

relatorio_outliers <- data.frame()

for (nome in NOMES) {
  cl  <- as.numeric(dados_adj[[nome]]$Close)
  dts <- index(dados_adj[[nome]])

  # Retorno close-to-close
  r <- c(NA, diff(log(cl)))

  # Z-score robusto (ignora NAs)
  mu_r  <- mean(r, na.rm = TRUE)
  sd_r  <- sd(r, na.rm = TRUE)
  z     <- (r - mu_r) / sd_r

  idx_out <- which(abs(z) > THRESHOLD_Z)

  if (length(idx_out) > 0) {
    df <- data.frame(
      banco   = nome,
      data    = dts[idx_out],
      retorno = round(r[idx_out] * 100, 2),
      z_score = round(z[idx_out], 2)
    )
    relatorio_outliers <- rbind(relatorio_outliers, df)
  }
}

if (nrow(relatorio_outliers) > 0) {
  cat("\n  Outliers identificados (|z| > 5):\n")
  print(relatorio_outliers[order(relatorio_outliers$data), ])
  write.csv(relatorio_outliers,
            "outputs/tabelas/outliers_para_revisao.csv",
            row.names = FALSE)
  cat("\n  Relatório salvo em outputs/tabelas/outliers_para_revisao.csv\n")
  cat("  AÇÃO NECESSÁRIA: revise manualmente antes de prosseguir.\n")
  cat("  Datas conhecidas como eventos reais (MANTER):\n")
  cat("    2017-05-18 (Joesley Day)\n")
  cat("    2020-03-12 e 2020-03-16 (COVID circuit breakers)\n")
  cat("    2023-01-12 (Americanas — apenas ITUB4, BBDC4)\n")
} else {
  cat("  Nenhum outlier com |z| > 5 encontrado.\n")
}


# ==============================================================================
# SEÇÃO 6 — ESTIMAÇÃO DA VOLATILIDADE DE YANG-ZHANG
# ==============================================================================

dados_adj[["BPAC11"]] <- dados_adj[["BPAC11"]]["2017-08-01/"]

cat("\n[6/13] Estimando volatilidade Yang-Zhang (n = 21 dias úteis)...\n")

vol_yz <- list()

for (nome in NOMES) {
  d <- dados_adj[[nome]]
  
  ohlc_mat <- d[, c("Open","High","Low","Close")]
  
  # Verificar NAs antes
  n_nas <- sum(is.na(ohlc_mat))
  if (n_nas > 0) cat(" ", nome, ":", n_nas, "NAs → interpolando...\n")
  
  # Interpolação linear coluna por coluna
  ohlc_imp <- ohlc_mat
  for (col in c("Open","High","Low","Close")) {
    serie <- as.numeric(ohlc_mat[, col])
    if (any(is.na(serie))) {
      ohlc_imp[, col] <- na.approx(serie, na.rm = FALSE)
    }
  }
  
  # Se ainda houver NAs nas pontas (na.approx não extrapola)
  # substitui pelo valor mais próximo disponível
  for (col in c("Open","High","Low","Close")) {
    ohlc_imp[, col] <- zoo::na.locf(ohlc_imp[, col], fromLast = FALSE)
    ohlc_imp[, col] <- zoo::na.locf(ohlc_imp[, col], fromLast = TRUE)
  }
  
  vyz <- TTR::volatility(
    OHLC = ohlc_imp,
    n    = N_JANELA_YZ,
    calc = "yang.zhang",
    N    = 252
  )
  
  vyz_mensal <- vyz / sqrt(252 / N_JANELA_YZ)
  vol_yz[[nome]] <- xts(vyz_mensal, order.by = index(d))
}

cat("  OK — volatilidade Yang-Zhang estimada para", length(vol_yz), "séries.\n")


# ==============================================================================
# SEÇÃO 7 — TRANSFORMAÇÃO LOGARÍTMICA E TESTES DE ESTACIONARIEDADE
# ==============================================================================

cat("\n[7/13] Transformação log e testes de estacionariedade...\n")

PISO_NUMERICO <- 1e-8

log_vol <- list()
resultados_testes <- data.frame()

for (nome in NOMES) {
  vyz <- as.numeric(vol_yz[[nome]])
  
  # Piso e log
  vyz_piso <- pmax(vyz, PISO_NUMERICO)
  lv       <- log(vyz_piso)
  
  # Armazenar como xts
  log_vol[[nome]] <- xts(lv, order.by = index(vol_yz[[nome]]))
  
  # Remover NAs para os testes
  lv_limpo <- na.omit(lv)
  
  # ── Teste ADF ──────────────────────────────────────────────────────────────
  adf_res     <- ur.df(lv_limpo, type = "drift", selectlags = "BIC")
  tau_stat    <- adf_res@teststat["statistic", "tau2"]
  tau_cv5     <- adf_res@cval["tau2", "5pct"]
  adf_rejeita <- tau_stat < tau_cv5
  
  # ── Teste KPSS ─────────────────────────────────────────────────────────────
  kpss_res      <- tryCatch(kpss.test(lv_limpo, null = "Level"), error = function(e) NULL)
  kpss_pval     <- if (!is.null(kpss_res)) kpss_res$p.value else NA
  kpss_nrejeita <- if (!is.na(kpss_pval)) kpss_pval > 0.05 else NA
  
  # ── Ljung-Box ──────────────────────────────────────────────────────────────
  lb_pval <- Box.test(lv_limpo, lag = 20, type = "Ljung-Box")$p.value
  
  # ── ARCH-LM ────────────────────────────────────────────────────────────────
  arch_res  <- tryCatch(ArchTest(lv_limpo, lags = 10), error = function(e) NULL)
  arch_pval <- if (!is.null(arch_res)) arch_res$p.value else NA
  
  resultados_testes <- rbind(resultados_testes, data.frame(
    Banco            = nome,
    ADF_tau          = round(tau_stat, 3),
    ADF_cv5pct       = round(tau_cv5, 3),
    ADF_rejeita_H0   = adf_rejeita,
    KPSS_pval        = round(kpss_pval, 3),
    KPSS_nao_rejeita = kpss_nrejeita,
    LjungBox_p       = round(lb_pval, 4),
    ARCH_LM_p        = round(arch_pval, 4),
    stringsAsFactors = FALSE
  ))
}

cat("\n  Resultados dos testes de estacionariedade:\n")
print(resultados_testes)

# Verificação automática: alguma série falhou?
falhas_adf  <- resultados_testes$Banco[resultados_testes$ADF_rejeita_H0 == FALSE]
falhas_kpss <- resultados_testes$Banco[resultados_testes$KPSS_nao_rejeita == FALSE]
if (length(falhas_adf) > 0)
  warning("ADF NÃO rejeitou raiz unitária em: ", paste(falhas_adf, collapse=", "))
if (length(falhas_kpss) > 0)
  warning("KPSS rejeitou estacionariedade em: ", paste(falhas_kpss, collapse=", "))

write.csv(resultados_testes,
          "outputs/tabelas/testes_estacionariedade.csv",
          row.names = FALSE)
cat("  Tabela salva em outputs/tabelas/testes_estacionariedade.csv\n")


# ==============================================================================
# SEÇÃO 8 — ALINHAMENTO EM PAINEL T×8 E IMPUTAÇÃO POR FILTRO DE KALMAN
# ==============================================================================

cat("\n[8/13] Alinhando painel T×8 e imputando NAs (filtro de Kalman)...\n")

# ── Passo 1: merge por data ───────────────────────────────────────────────────
painel_raw <- do.call(merge, c(log_vol, all = TRUE))
colnames(painel_raw) <- NOMES

# ── Passo 2: diagnóstico ─────────────────────────────────────────────────────
nas_por_banco <- colSums(is.na(painel_raw))
nas_por_dia   <- rowSums(is.na(painel_raw))

cat("\n  NAs por banco (antes da imputação):\n")
print(nas_por_banco)
cat("\n  Distribuição de NAs por dia:\n")
print(table(nas_por_dia))

# ── Passo 3: remover datas sem pregão (todos NA) ──────────────────────────────
dias_sem_pregao <- rowSums(is.na(painel_raw)) == ncol(painel_raw)
cat("\n  Dias sem pregão removidos:", sum(dias_sem_pregao), "\n")
painel_raw <- painel_raw[!dias_sem_pregao, ]

# ── Passo 4: imputação por filtro de Kalman ───────────────────────────────────
cat("  Imputando NAs com filtro de Kalman (auto.arima)...\n")
painel_imp <- apply(painel_raw, 2, function(col) {
  if (any(is.na(col))) {
    na_kalman(col, model = "auto.arima", smooth = TRUE)
  } else {
    col
  }
})
painel_imp <- xts(painel_imp, order.by = index(painel_raw))
colnames(painel_imp) <- NOMES

# ── Passo 5: verificação final ────────────────────────────────────────────────
stopifnot(
  "Ainda há NAs no painel após imputação!" = sum(is.na(painel_imp)) == 0
)

cat("  OK — painel final:", nrow(painel_imp), "observações ×",
    ncol(painel_imp), "bancos. Zero NAs.\n")

# Salvar painel para referência
write.csv(
  data.frame(data = index(painel_imp), as.data.frame(painel_imp)),
  "outputs/tabelas/painel_logvol.csv",
  row.names = FALSE
)


# ==============================================================================
# SEÇÃO 9 — ESTATÍSTICAS DESCRITIVAS
# ==============================================================================

cat("\n[9/13] Calculando estatísticas descritivas...\n")

# Funções auxiliares (base R)
skewness_fn  <- function(x) mean((x - mean(x))^3) / sd(x)^3
kurtosis_fn  <- function(x) mean((x - mean(x))^4) / sd(x)^4 - 3  # excesso

desc <- apply(painel_imp, 2, function(col) {
  c(
    Media    = mean(col),
    DP       = sd(col),
    Min      = min(col),
    Mediana  = median(col),
    Max      = max(col),
    Assimetria = skewness_fn(col),
    Curtose  = kurtosis_fn(col)
  )
})

desc_df <- as.data.frame(t(round(desc, 4)))
cat("\n  Estatísticas descritivas (log-volatilidade):\n")
print(desc_df)

write.csv(desc_df, "outputs/tabelas/estatisticas_descritivas.csv")
cat("  Salvo em outputs/tabelas/estatisticas_descritivas.csv\n")


# ==============================================================================
# SEÇÃO 10 — VAR(2) + GFEVD + ÍNDICES DY — MODELO ESTÁTICO (FULL SAMPLE)
# ==============================================================================

cat("\n[10/13] Estimando VAR(2) + GFEVD (full sample)...\n")

Y <- zoo(as.matrix(painel_imp), order.by = index(painel_imp))  # ConnectednessApproach opera com matrix

# ── (a) Seleção de defasagens ─────────────────────────────────────────────────
cat("  Selecionando número de defasagens (BIC)...\n")
var_select <- VARselect(Y, lag.max = 6, type = "const")

bic_tabela <- data.frame(
  Lags = 1:6,
  AIC  = round(var_select$criteria["AIC(n)", ], 4),
  BIC  = round(var_select$criteria["SC(n)", ], 4),
  HQ   = round(var_select$criteria["HQ(n)", ], 4)
)
cat("\n  BIC por número de defasagens:\n")
print(bic_tabela)
write.csv(bic_tabela, "outputs/tabelas/var_selecao_lags.csv", row.names = FALSE)
cat("  BIC selecionado:", var_select$selection["SC(n)"], "defasagens\n")
cat("  Especificação adotada: p =", P_LAGS, "(justificada na metodologia)\n")

# ── (b) Estabilidade do VAR ────────────────────────────────────────────────────
cat("\n  Verificando estabilidade do VAR...\n")
var_est   <- VAR(Y, p = P_LAGS, type = "const")
raizes    <- roots(var_est, modulus = TRUE)
cat("  Módulo máximo das raízes:", round(max(raizes), 4))
if (max(raizes) < 1) {
  cat(" → VAR ESTÁVEL ✓\n")
} else {
  stop("VAR instável! Módulo máximo = ", round(max(raizes), 4),
       ". Revisar dados antes de prosseguir.")
}

# ── (c) Tabela de spillovers DY (full sample) ─────────────────────────────────
cat("\n  Computando tabela de spillovers (GFEVD, H = ", H_HORIZONTE, ")...\n")

dca_full <- ConnectednessApproach(
  Y,
  nlag     = P_LAGS,
  nfore    = H_HORIZONTE,
  window   = NULL,
  corrected = FALSE,
  model    = "VAR"
)

# Extrair objetos principais
tci_full   <- dca_full$TCI
to_full    <- dca_full$TO
from_full  <- dca_full$FROM
net_full   <- dca_full$NET
tabela_gfevd <- dca_full$TABLE

cat("\n  Total Connectedness Index (TCI):", round(tci_full, 2), "%\n")

# Tabela resumo TO / FROM / NET
tabela_dy <- data.frame(
  Banco = NOMES,
  TO    = round(as.numeric(to_full),   2),
  FROM  = round(as.numeric(from_full), 2),
  NET   = round(as.numeric(net_full),  2)
)
tabela_dy <- tabela_dy[order(-tabela_dy$TO), ]
rownames(tabela_dy) <- NULL

cat("\n  Tabela DY (full sample):\n")
print(tabela_dy)

write.csv(tabela_dy,    "outputs/tabelas/dy_fullsample.csv",   row.names = FALSE)
write.csv(tabela_gfevd, "outputs/tabelas/gfevd_fullsample.csv")
cat("  Tabelas salvas.\n")


# ==============================================================================
# SEÇÃO 11 — ANÁLISE DINÂMICA: JANELA ROLANTE (W = 200, H = 10)
# ==============================================================================

cat("\n[11/13] Análise dinâmica — janela rolante (W =", N_JANELA_W,
    ", H =", H_HORIZONTE, ")...\n")
cat("  (Isso pode levar alguns minutos)\n")

# Truncar painel para quando todos os bancos têm dados
# BPAC11 começa em agosto de 2017, então o painel começa lá
data_inicio_comum <- as.Date("2017-08-01")
Y <- Y[index(Y) >= data_inicio_comum, ]

cat("Painel truncado:", nrow(Y), "observações | de:",
    as.character(index(Y)[1]), "até:", 
    as.character(index(Y)[nrow(Y)]), "\n")

dca_roll <- ConnectednessApproach(
  Y,
  nlag     = P_LAGS,
  nfore    = H_HORIZONTE,
  window   = N_JANELA_W,
  corrected = FALSE,
  model    = "VAR"
)

# Extrair séries temporais dos índices
tci_rolling <- zoo(dca_roll$TCI[, 1], order.by = as.Date(rownames(dca_roll$TCI)))
datas_roll  <- index(tci_rolling)

to_rolling   <- zoo(dca_roll$TO,   order.by = datas_roll)
from_rolling <- zoo(dca_roll$FROM, order.by = datas_roll)
net_rolling  <- zoo(dca_roll$NET,  order.by = datas_roll)

colnames(to_rolling)   <- NOMES
colnames(from_rolling) <- NOMES
colnames(net_rolling)  <- NOMES

cat("OK —", length(datas_roll), "janelas | de:",
    as.character(datas_roll[1]), "até:",
    as.character(datas_roll[length(datas_roll)]), "\n")
# Salvar
write.csv(
  data.frame(data = datas_roll, TCI = as.numeric(tci_rolling)),
  "outputs/tabelas/tci_rolling.csv", row.names = FALSE
)
write.csv(
  data.frame(data = datas_roll, as.data.frame(net_rolling)),
  "outputs/tabelas/net_rolling.csv", row.names = FALSE
)

cat("  OK —", length(datas_roll), "janelas estimadas.\n")

# ── Gráfico 1: TCI rolling ────────────────────────────────────────────────────
tci_df <- data.frame(
  data = datas_roll,
  TCI  = as.numeric(tci_rolling)
)

p_tci <- ggplot(tci_df, aes(x = data, y = TCI)) +
  geom_line(color = "#2166ac", linewidth = 0.7) +
  geom_vline(data = EVENTOS, aes(xintercept = as.numeric(data)),
             linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_text(data = EVENTOS,
            aes(x = data, y = max(tci_df$TCI) * 0.97, label = label),
            angle = 90, hjust = 1, vjust = -0.3,
            size = 2.8, color = "grey30") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Total Connectedness Index — Sistema Bancário Brasileiro (2014–2025)",
    subtitle = paste0("VAR(", P_LAGS, "), GFEVD H = ", H_HORIZONTE,
                      ", janela W = ", N_JANELA_W, " dias úteis"),
    x = NULL, y = "TCI (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 12),
    axis.text.x   = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

ggsave("outputs/graficos/tci_rolling.png", p_tci,
       width = 12, height = 5, dpi = 300)
cat("  Gráfico TCI salvo.\n")

# ── Gráfico 2: NET rolling por banco ─────────────────────────────────────────
net_df <- data.frame(data = datas_roll, as.data.frame(net_rolling)) |>
  pivot_longer(-data, names_to = "Banco", values_to = "NET")

p_net <- ggplot(net_df, aes(x = data, y = NET, color = Banco)) +
  geom_line(linewidth = 0.5, alpha = 0.85) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.4) +
  geom_vline(data = EVENTOS, aes(xintercept = as.numeric(data)),
             linetype = "dotted", color = "grey50", linewidth = 0.4,
             inherit.aes = FALSE) +
  facet_wrap(~Banco, ncol = 4, scales = "free_y") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Spillover NET por instituição — janela rolante (W = 200)",
    subtitle = "Positivo = transmissor líquido; negativo = receptor líquido",
    x = NULL, y = "NET (%)"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position   = "none",
    strip.text        = element_text(face = "bold"),
    axis.text.x       = element_text(angle = 45, hjust = 1, size = 7),
    panel.grid.minor  = element_blank()
  )

ggsave("outputs/graficos/net_rolling.png", p_net,
       width = 14, height = 8, dpi = 300)
cat("  Gráfico NET salvo.\n")