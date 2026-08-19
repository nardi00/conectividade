# install.packages(c(
#   "readxl", "TTR", "xts", "zoo", "imputeTS", "urca", "tseries", "FinTS",
#   "vars", "igraph", "ggraph", "tidygraph", "ggplot2", "dplyr", "tidyr",
#   "scales", "patchwork", "ConnectednessApproach"
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
library(ConnectednessApproach)
library(vars)
library(igraph)
library(ggraph)
library(tidygraph)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

dir.create("outputs/tabelas",  recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/graficos", recursive = TRUE, showWarnings = FALSE)

# Constantes globais
CAMINHO_EXCEL <- "Cotacoes_ComDinheiro_8bancos_2014-2026.xlsx"

NOMES <- c("ITUB4", "BBDC4", "BBAS3", "SANB11",
           "BPAC11", "BRSR6", "ABCB4", "BPAN4")

DATA_INICIO <- "2019-01-01"
DATA_FIM    <- "2025-12-31"

N_JANELA_YZ   <- 21
N_JANELA_W    <- 200
H_HORIZONTE   <- 10
P_LAGS        <- 2
THRESHOLD     <- 0.05   
THRESHOLD_REL <- 0.05   

EVENTOS <- data.frame(
  data  = as.Date(c("2020-03-11", "2022-01-01", "2023-01-12", "2024-10-01")),
  label = c("COVID-19", "Aperto Selic", "Americanas", "Crise Fiscal 2024"),
  stringsAsFactors = FALSE
)

# ==============================================================================
# LEITURA DOS DADOS (EXCEL)
# ==============================================================================

cat("\n[1/10] Lendo cotações do arquivo Excel (ComDinheiro)...\n")

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

cat("OK -", length(dados_adj), "séries lidas.\n")

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

cat("\n[2/10] Verificando consistência OHLC...\n")

# tolerância numérica: sem ela, ruído de ponto flutuante (~1e-9) do
# cálculo do fator de ajuste do ComDinheiro é lido como violação.
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

cat("OK - sanity checks concluídos.\n")


# ==============================================================================
# SEÇÃO 3 — FILTRO DE LIQUIDEZ
# ==============================================================================

cat("\n[3/10] Aplicando filtro de liquidez...\n")

# Sem volume em quantidade de ações no ComDinheiro, só piso de volume financeiro.
VOLUME_MIN_BRL <- 100000

for (nome in NOMES) {
  d       <- dados_adj[[nome]]
  vol_brl <- as.numeric(d$Volume_BRL)
  filtro  <- !is.na(vol_brl) & (vol_brl < VOLUME_MIN_BRL)
  
  n_filtrados <- sum(filtro)
  if (n_filtrados > 0) {
    cat("  ", nome, ":", n_filtrados, "dia(s) com baixa liquidez → NA\n")
    dados_adj[[nome]][filtro, c("Open","High","Low","Close")] <- NA
  }
}

cat("OK - filtro de liquidez aplicado.\n")


# ==============================================================================
# SEÇÃO 4 — IDENTIFICAÇÃO DE OUTLIERS
# ==============================================================================

cat("\n[4/10] Identificando outliers (|z| > 5)...\n")

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
  cat("\nOutliers identificados (|z| > 5):\n")
  print(relatorio_outliers[order(relatorio_outliers$data), ])
  
  n_por_data <- table(relatorio_outliers$data)
  relatorio_outliers$n_bancos_na_data <- as.integer(n_por_data[as.character(relatorio_outliers$data)])
  relatorio_outliers$classificacao <- ifelse(
    relatorio_outliers$n_bancos_na_data >= 2,
    "Sistêmico (>=2 bancos na data)",
    "ISOLADO (1 banco na data)"
  )
  
  isolados <- relatorio_outliers[relatorio_outliers$n_bancos_na_data == 1, ]
  isolados <- isolados[order(isolados$data), ]
  
  cat("\nResumo da classificação automática:\n")
  cat("   ", sum(relatorio_outliers$n_bancos_na_data >= 2), "linha(s) em datas sistêmicas (>=2 bancos)\n")
  cat("   ", nrow(isolados), "linha(s) ISOLADAS - precisam de verificação externa:\n")
  print(isolados[, c("banco", "data", "retorno", "z_score")], row.names = FALSE)
  
  write.csv(relatorio_outliers, "outputs/tabelas/outliers_classificados.csv", row.names = FALSE)
  cat("\nTabela classificada salva em outputs/tabelas/outliers_classificados.csv\n")
} else {
  cat("Nenhum outlier com |z| > 5 encontrado.\n")
}


# ==============================================================================
# SEÇÃO 5 — ESTIMAÇÃO DA VOLATILIDADE DE YANG-ZHANG
# ==============================================================================

cat("\n[5/10] Estimando volatilidade Yang-Zhang (n = 21 dias úteis)...\n")

# Implementação própria (Eq. 6.1-6.5), tolerante a NA: TTR::volatility() quebra 
# com NA fora do início da série ("Series contains non-leading NAs").
yang_zhang_na <- function(ohlc_mat, n = 21) {
  O <- as.numeric(ohlc_mat[, "Open"])
  H <- as.numeric(ohlc_mat[, "High"])
  L <- as.numeric(ohlc_mat[, "Low"])
  C <- as.numeric(ohlc_mat[, "Close"])
  C_lag <- c(NA_real_, C[-length(C)])
  
  Nt  <- length(C)
  out <- rep(NA_real_, Nt)
  k   <- 0.34 / (1.34 + (n + 1) / (n - 1))   # Eq. 6.5
  
  for (i in n:Nt) {
    idx <- (i - n + 1):i
    o <- O[idx]; h <- H[idx]; l <- L[idx]; c <- C[idx]; cl <- C_lag[idx]
    if (anyNA(c(o, h, l, c, cl))) next
    
    ro <- log(o / cl)                                            # Eq. 6.2
    rc <- log(c / o)                                              # Eq. 6.3
    rs <- log(h / c) * log(h / o) + log(l / c) * log(l / o)       # Eq. 6.4
    
    sig2_o  <- sum((ro - mean(ro))^2) / (n - 1)
    sig2_c  <- sum((rc - mean(rc))^2) / (n - 1)
    sig2_rs <- sum(rs) / (n - 1)
    sig2_yz <- sig2_o + k * sig2_c + (1 - k) * sig2_rs             # Eq. 6.1
    out[i]  <- sqrt(sig2_yz)
  }
  out
}

vol_yz <- list()

for (nome in NOMES) {
  d <- dados_adj[[nome]]
  ohlc_mat <- d[, c("Open","High","Low","Close")]
  
  n_nas <- sum(is.na(ohlc_mat))
  if (n_nas > 0) cat(" ", nome, ":", n_nas, "NAs no OHLC\n")
  
  ohlc_imp <- ohlc_mat

  
  sd_diario  <- yang_zhang_na(ohlc_imp, n = N_JANELA_YZ)
  vyz        <- sd_diario * sqrt(252)
  vyz_mensal <- vyz / sqrt(252 / N_JANELA_YZ)
  vol_yz[[nome]] <- xts(vyz_mensal, order.by = index(d))
  
  n_nas_vol <- sum(is.na(vyz_mensal))
  if (n_nas_vol > 0) cat("   ", nome, "→", n_nas_vol, "NAs na volatilidade estimada\n")
}

cat("OK - volatilidade Yang-Zhang estimada para", length(vol_yz), "séries.\n")


# ==============================================================================
# SEÇÃO 6 — TRANSFORMAÇÃO LOGARÍTMICA E TESTES DE ESTACIONARIEDADE
# ==============================================================================

cat("\n[6/10] Transformação log e testes de estacionariedade...\n")

PISO_NUMERICO <- 1e-8
log_vol <- list()
resultados_testes <- data.frame()

for (nome in NOMES) {
  vyz <- as.numeric(vol_yz[[nome]])
  lv  <- log(pmax(vyz, PISO_NUMERICO))
  log_vol[[nome]] <- xts(lv, order.by = index(vol_yz[[nome]]))
  lv_limpo <- na.omit(lv)
  
  adf_res     <- ur.df(lv_limpo, type = "drift", selectlags = "BIC")
  tau_stat    <- adf_res@teststat["statistic", "tau2"]
  tau_cv5     <- adf_res@cval["tau2", "5pct"]
  adf_rejeita <- tau_stat < tau_cv5
  
  kpss_res      <- tryCatch(kpss.test(lv_limpo, null = "Level"), error = function(e) NULL)
  kpss_pval     <- if (!is.null(kpss_res)) kpss_res$p.value else NA
  kpss_nrejeita <- if (!is.na(kpss_pval)) kpss_pval > 0.05 else NA
  
  ers_res <- tryCatch(ur.ers(lv_limpo, type = "DF-GLS", model = "constant", lag.max = 4),
                      error = function(e) NULL)
  ers_stat    <- if (!is.null(ers_res)) ers_res@teststat else NA
  ers_cv5     <- if (!is.null(ers_res)) ers_res@cval[1, "5pct"] else NA
  ers_rejeita <- if (!is.na(ers_stat) && !is.na(ers_cv5)) ers_stat < ers_cv5 else NA
  
  pp_res <- tryCatch(ur.pp(lv_limpo, type = "Z-tau", model = "constant", lags = "short"),
                     error = function(e) NULL)
  pp_stat    <- if (!is.null(pp_res)) pp_res@teststat else NA
  pp_cv5     <- if (!is.null(pp_res)) pp_res@cval[1, "5pct"] else NA
  pp_rejeita <- if (!is.na(pp_stat) && !is.na(pp_cv5)) pp_stat < pp_cv5 else NA
  
  # ur.za@cval = c(1%, 5%, 10%), vetor sem nomes
  za_res <- tryCatch(ur.za(lv_limpo, model = "both", lag = 4), error = function(e) NULL)
  za_stat    <- if (!is.null(za_res)) za_res@teststat else NA
  za_cv5     <- if (!is.null(za_res)) za_res@cval[2] else NA
  za_rejeita <- if (!is.na(za_stat) && !is.na(za_cv5)) za_stat < za_cv5 else NA
  
  lb_pval <- Box.test(lv_limpo, lag = 20, type = "Ljung-Box")$p.value
  arch_res  <- tryCatch(ArchTest(lv_limpo, lags = 10), error = function(e) NULL)
  arch_pval <- if (!is.null(arch_res)) arch_res$p.value else NA
  
  resultados_testes <- rbind(resultados_testes, data.frame(
    Banco = nome,
    ADF_tau = round(tau_stat, 3), ADF_cv5pct = round(tau_cv5, 3), ADF_rejeita_H0 = adf_rejeita,
    KPSS_pval = round(kpss_pval, 3), KPSS_nao_rejeita = kpss_nrejeita,
    DFGLS_stat = round(ers_stat, 3), DFGLS_cv5pct = round(ers_cv5, 3), DFGLS_rejeita_H0 = ers_rejeita,
    PP_stat = round(pp_stat, 3), PP_cv5pct = round(pp_cv5, 3), PP_rejeita_H0 = pp_rejeita,
    ZA_stat = round(za_stat, 3), ZA_cv5pct = round(za_cv5, 3), ZA_rejeita_H0 = za_rejeita,
    LjungBox_p = round(lb_pval, 4), ARCH_LM_p = round(arch_pval, 4),
    stringsAsFactors = FALSE
  ))
}

cat("\nResultados dos testes de estacionariedade:\n")
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

write.csv(resultados_testes, "outputs/tabelas/testes_estacionariedade.csv", row.names = FALSE)
cat("Tabela salva em outputs/tabelas/testes_estacionariedade.csv\n")


# ==============================================================================
# SEÇÃO 7 - ALINHAMENTO EM PAINEL T×8 E IMPUTAÇÃO POR FILTRO DE KALMAN
# ==============================================================================

cat("\n[7/10] Alinhando painel T×8 e imputando NAs (filtro de Kalman)...\n")

painel_raw <- do.call(merge, c(log_vol, all = TRUE))
colnames(painel_raw) <- NOMES

cat("\n  NAs por banco (antes da imputação):\n")
print(colSums(is.na(painel_raw)))
cat("\n  Distribuição de NAs por dia:\n")
print(table(rowSums(is.na(painel_raw))))

dias_sem_pregao <- rowSums(is.na(painel_raw)) == ncol(painel_raw)
cat("\n  Dias sem pregão removidos:", sum(dias_sem_pregao), "\n")
painel_raw <- painel_raw[!dias_sem_pregao, ]

cat(" Imputando NAs com filtro de Kalman (auto.arima)...\n")
painel_imp <- apply(painel_raw, 2, function(col) {
  if (any(is.na(col))) na_kalman(col, model = "auto.arima", smooth = TRUE) else col
})
painel_imp <- xts(painel_imp, order.by = index(painel_raw))
colnames(painel_imp) <- NOMES

stopifnot("Ainda há NAs no painel após imputação!" = sum(is.na(painel_imp)) == 0)

cat("  OK - painel final:", nrow(painel_imp), "observações ×", ncol(painel_imp), "bancos. Zero NAs.\n")

write.csv(data.frame(data = index(painel_imp), as.data.frame(painel_imp)),
          "outputs/tabelas/painel_logvol.csv", row.names = FALSE)


# ==============================================================================
# SEÇÃO 8 — ESTATÍSTICAS DESCRITIVAS
# ==============================================================================

cat("\n[8/10] Calculando estatísticas descritivas...\n")

skewness_fn <- function(x) mean((x - mean(x))^3) / sd(x)^3
kurtosis_fn <- function(x) mean((x - mean(x))^4) / sd(x)^4 - 3

desc <- apply(painel_imp, 2, function(col) {
  c(Media = mean(col), DP = sd(col), Min = min(col), Mediana = median(col),
    Max = max(col), Assimetria = skewness_fn(col), Curtose = kurtosis_fn(col))
})

desc_df <- as.data.frame(t(round(desc, 4)))
cat("\n  Estatísticas descritivas (log-volatilidade):\n")
print(desc_df)

write.csv(desc_df, "outputs/tabelas/estatisticas_descritivas.csv")
cat("  Salvo em outputs/tabelas/estatisticas_descritivas.csv\n")


# ==============================================================================
# SEÇÃO 9 — VAR(2) + GFEVD + ÍNDICES DY — MODELO ESTÁTICO (FULL SAMPLE)
# ==============================================================================

cat("\n[9/10] Estimando VAR(2) + GFEVD (full sample)...\n")

Y <- zoo(as.matrix(painel_imp), order.by = index(painel_imp))

cat("  Selecionando número de defasagens (BIC)...\n")
var_select <- vars::VARselect(Y, lag.max = 6, type = "const")

bic_tabela <- data.frame(
  Lags = 1:6,
  AIC = round(var_select$criteria["AIC(n)", ], 4),
  BIC = round(var_select$criteria["SC(n)", ], 4),
  HQ  = round(var_select$criteria["HQ(n)", ], 4)
)
cat("\n BIC por número de defasagens:\n")
print(bic_tabela)
write.csv(bic_tabela, "outputs/tabelas/var_selecao_lags.csv", row.names = FALSE)
cat("  BIC selecionado:", var_select$selection["SC(n)"], "defasagens\n")
cat("  Especificação adotada: p =", P_LAGS, "(justificada na metodologia)\n")

cat("\n Verificando estabilidade do VAR...\n")

var_est <- vars::VAR(Y, p = P_LAGS, type = "const")
raizes  <- vars::roots(var_est, modulus = TRUE)
cat("  Módulo máximo das raízes:", round(max(raizes), 4))
if (max(raizes) < 1) {
  cat(" → VAR ESTÁVEL ✓\n")
} else {
  stop("VAR instável! Módulo máximo = ", round(max(raizes), 4), ". Revisar dados.")
}

cat("\n Computando tabela de spillovers (GFEVD, H = ", H_HORIZONTE, ")...\n")

dca_full <- ConnectednessApproach(
  Y, nlag = P_LAGS, nfore = H_HORIZONTE, window = NULL, corrected = FALSE, model = "VAR"
)

tci_full     <- dca_full$TCI
to_full      <- dca_full$TO
from_full    <- dca_full$FROM
net_full     <- dca_full$NET
tabela_gfevd <- dca_full$TABLE

cat("\n Total Connectedness Index (TCI):", round(tci_full, 2), "%\n")

tabela_dy <- data.frame(
  Banco = NOMES,
  TO   = round(as.numeric(to_full), 2),
  FROM = round(as.numeric(from_full), 2),
  NET  = round(as.numeric(net_full), 2)
)
tabela_dy <- tabela_dy[order(-tabela_dy$TO), ]
rownames(tabela_dy) <- NULL

cat("\n Tabela DY (full sample):\n")
print(tabela_dy)

write.csv(tabela_dy, "outputs/tabelas/dy_fullsample.csv", row.names = FALSE)
write.csv(tabela_gfevd, "outputs/tabelas/gfevd_fullsample.csv")
cat("Tabelas salvas.\n")


# ==============================================================================
# SEÇÃO 10 — ANÁLISE DINÂMICA: JANELA ROLANTE (W = 200, H = 10)
# ==============================================================================

cat("\n[10/10] Análise dinâmica - janela rolante (W =", N_JANELA_W, ", H =", H_HORIZONTE, ")...\n")

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

cat("OK -", length(datas_roll), "janelas | de:", as.character(datas_roll[1]),
    "até:", as.character(datas_roll[length(datas_roll)]), "\n")

write.csv(data.frame(data = datas_roll, TCI = as.numeric(tci_rolling)),
          "outputs/tabelas/tci_rolling.csv", row.names = FALSE)
write.csv(data.frame(data = datas_roll, as.data.frame(net_rolling)),
          "outputs/tabelas/net_rolling.csv", row.names = FALSE)

tci_df <- data.frame(data = datas_roll, TCI = as.numeric(tci_rolling))

p_tci <- ggplot(tci_df, aes(x = data, y = TCI)) +
  geom_line(color = "#2166ac", linewidth = 0.7) +
  geom_vline(data = EVENTOS, aes(xintercept = as.numeric(data)),
             linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_text(data = EVENTOS, aes(x = data, y = max(tci_df$TCI) * 0.97, label = label),
            angle = 90, hjust = 1, vjust = -0.3, size = 2.8, color = "grey30") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "Total Connectedness Index - Sistema Bancário Brasileiro (2019 - 2025)",
       subtitle = paste0("VAR(", P_LAGS, "), GFEVD H = ", H_HORIZONTE, ", janela W = ", N_JANELA_W, " dias úteis"),
       x = NULL, y = "TCI (%)") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank())

ggsave("outputs/graficos/tci_rolling.png", p_tci, width = 12, height = 5, dpi = 300)
cat("Gráfico TCI salvo.\n")

net_df <- data.frame(data = datas_roll, as.data.frame(net_rolling)) |>
  pivot_longer(-data, names_to = "Banco", values_to = "NET")

p_net <- ggplot(net_df, aes(x = data, y = NET, color = Banco)) +
  geom_line(linewidth = 0.5, alpha = 0.85) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.4) +
  geom_vline(data = EVENTOS, aes(xintercept = as.numeric(data)),
             linetype = "dotted", color = "grey50", linewidth = 0.4, inherit.aes = FALSE) +
  facet_wrap(~Banco, ncol = 4, scales = "free_y") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "Spillover NET por instituição - janela rolante (W = 200)",
       subtitle = "Positivo = transmissor líquido; negativo = receptor líquido",
       x = NULL, y = "NET (%)") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none", strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        panel.grid.minor = element_blank())

ggsave("outputs/graficos/net_rolling.png", p_net, width = 14, height = 8, dpi = 300)
cat("  Gráfico NET salvo.\n")


# ==============================================================================
# SEÇÃO 11 - REDES: MATRIZ DE ADJACÊNCIA (FULL SAMPLE)
# ==============================================================================

cat("\n[11] Construindo matriz de adjacência (grafo completo, sem threshold)...\n")

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

# TCI manual vs. dca_full$TCI para confirmar que a
# fatia extraída do array é a correta
tci_manual <- mean(theta_offdiag) * length(NOMES) * 100

cat("\n  Verificação - TCI manual vs. TCI do pacote:\n")
cat("    Manual:", round(tci_manual, 4), "%  |  Pacote:", round(dca_full$TCI, 4), "%\n")

if (abs(tci_manual - as.numeric(dca_full$TCI)) > 0.05) {
  warning("TCI manual e do pacote NÃO batem")
} else {
  cat("OK\n")
}


# ==============================================================================
# SEÇÃO 12 — REDES: ORIENTAÇÃO DAS ARESTAS E OBJETO IGRAPH (FULL SAMPLE)
# ==============================================================================

cat("\n[12] Construindo grafo dirigido (g_full) com orientação corrigida...\n")

# theta_ij = fluxo j -> i, mas igraph lê mat[i,j] como aresta
# saindo de i. Transpor corrige a direção sem gerar erro de execução caso
# esquecido
adj_full <- t(theta_offdiag)

g_full <- graph_from_adjacency_matrix(adj_full, mode = "directed", weighted = TRUE, diag = FALSE)

cat("  Grafo g_full:", vcount(g_full), "nós,", ecount(g_full), "arestas",
    "(esperado:", length(NOMES), "nós,", length(NOMES) * (length(NOMES) - 1), "arestas)\n")

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
cat("\n  Verificação de direção - NET do pacote vs. NET do grafo:\n")
print(comparacao_net, row.names = FALSE)

if (!all(comparacao_net$Mesmo_sinal)) {
  warning("Sinal de NET do grafo não bate com dca_full$NET")
} else {
  cat("\n  OK, sinais batem em todos os bancos.\n")
}

V(g_full)$name <- NOMES


# ==============================================================================
# SEÇÃO 13 — REDES: MÉTRICAS DE CENTRALIDADE (FULL SAMPLE)
# ==============================================================================

cat("\n[13] Calculando métricas de centralidade (grafo completo)...\n")

betweenness_full <- igraph::betweenness(g_full, directed = TRUE, weights = 1 / E(g_full)$weight)

eigen_full <- igraph::eigen_centrality(g_full, directed = TRUE)$vector

pagerank_full <- igraph::page_rank(g_full, directed = TRUE, weights = E(g_full)$weight)$vector

metricas_centralidade <- data.frame(
  Banco = V(g_full)$name,
  Betweenness    = round(betweenness_full[V(g_full)$name], 4),
  Eigenvector_in = round(eigen_full[V(g_full)$name], 4),
  PageRank       = round(pagerank_full[V(g_full)$name], 4)
)
metricas_centralidade <- metricas_centralidade[order(-metricas_centralidade$PageRank), ]
rownames(metricas_centralidade) <- NULL

cat("\n  Métricas de centralidade (full sample, ordenado por PageRank):\n")
print(metricas_centralidade)

write.csv(metricas_centralidade, "outputs/tabelas/centralidade_fullsample.csv", row.names = FALSE)
cat("  Salvo em outputs/tabelas/centralidade_fullsample.csv\n")

# Classificação para assortatividade por tipo de controle.
tipo_controle <- c(
  ITUB4 = "Privado nacional", BBDC4 = "Privado nacional",
  BBAS3 = "Público federal", SANB11 = "Privado estrangeiro",
  BPAC11 = "Privado nacional (BTG)", BRSR6 = "Público estadual",
  ABCB4 = "Privado estrangeiro", BPAN4 = "Privado nacional (BTG)"
)
tipo_controle_ord <- tipo_controle[V(g_full)$name]

assortatividade_controle <- igraph::assortativity_nominal(
  g_full, types = as.integer(factor(tipo_controle_ord)), directed = TRUE
)

cat("\n  Assortatividade por tipo de controle institucional:", round(assortatividade_controle, 4), "\n")
cat("  (positivo = mesmo tipo se conecta mais entre si; negativo = predomina entre tipos diferentes)\n")
cat("\n  Classificação usada:\n")
print(data.frame(Banco = names(tipo_controle_ord), Tipo_controle = tipo_controle_ord), row.names = FALSE)


# ==============================================================================
# SEÇÃO 14 — REDES: GRAFO DE VISUALIZAÇÃO (NET COLAPSADO, THRESHOLD RELATIVO)
# ==============================================================================

cat("\n[14] Construindo grafo de visualização (NET colapsado, threshold relativo",
    THRESHOLD_REL * 100, "% do par mais forte)...\n")

npdc <- adj_full - theta_offdiag
stopifnot("npdc deveria ser antissimétrica" = all(abs(npdc + t(npdc)) < 1e-12))


pares <- combn(NOMES, 2, simplify = FALSE)
dist_netij <- data.frame(
  par   = sapply(pares, function(p) paste(p, collapse = "-")),
  valor = sapply(pares, function(p) abs(npdc[p[1], p[2]]) * 100)
)
dist_netij <- dist_netij[order(-dist_netij$valor), ]
cat("\n  Distribuição de |NETij| entre os 28 pares (p.p.):\n")
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
V(g_net)$Tipo <- tipo_controle[V(g_net)$name]

set.seed(42)
layout_fr <- create_layout(g_net, layout = "fr")

p_rede <- ggraph(layout_fr) +
  geom_edge_link(
    aes(width = weight, alpha = weight),
    arrow = arrow(length = unit(3, "mm"), type = "closed"),
    end_cap = circle(6, "mm"), color = "grey40"
  ) +
  scale_edge_width(range = c(0.3, 2.5), guide = "none") +
  scale_edge_alpha(range = c(0.4, 0.9), guide = "none") +
  geom_node_point(aes(size = sqrt(TO), fill = NET), shape = 21, color = "black", stroke = 0.6) +
  scale_size_continuous(range = c(6, 16), guide = "none") +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b", midpoint = 0, name = "NET (%)") +
  geom_node_text(aes(label = name), vjust = -1.6, size = 4, fontface = "bold") +
  labs(
    title = "Rede de conectividade - Sistema Bancário Brasileiro (full sample, 2019-2025)",
    subtitle = paste0("Arestas: NET par-a-par (Eq. 6.13), threshold relativo ", THRESHOLD_REL * 100,
                      "% do par mais forte | Tamanho do nó ~ TOi | Layout: Fruchterman-Reingold")
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "grey30"),
    legend.position = "right",
    plot.background   = element_rect(fill = "white", color = NA),
    panel.background  = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA)
  )

ggsave("outputs/graficos/rede_full_sample.png", p_rede, width = 10, height = 8, dpi = 300, bg = "white")
cat("  Grafo salvo em outputs/graficos/rede_full_sample.png\n")