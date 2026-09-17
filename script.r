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

dir.create("outputs/tabelas",  recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/graficos", recursive = TRUE, showWarnings = FALSE)

# ── Constantes globais ────────────────────────────────────────────────────
CAMINHO_EXCEL <- "Cotacoes_ComDinheiro_8bancos_2014-2026.xlsx"

NOMES <- c("ITUB4", "BBDC4", "BBAS3", "SANB11",
           "BPAC11", "BRSR6", "ABCB4", "BPAN4")

DATA_INICIO <- "2019-01-01"
DATA_FIM    <- "2025-12-31"

N_JANELA_W    <- 200
H_HORIZONTE   <- 10
P_LAGS        <- 2
THRESHOLD_REL <- 0.05   # threshold relativo do grafo NET colapsado (Seção 14)

EVENTOS <- data.frame(
  data  = as.Date(c("2020-03-11", "2021-03-17", "2023-01-12", "2024-10-01")),
  label = c("COVID-19", "Aperto Selic", "Americanas", "Crise Fiscal 2024"),
  stringsAsFactors = FALSE
)

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
# complementar de liquidez, não incorporado aqui ainda.
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
  write.csv(relatorio_outliers, "outputs/tabelas/outliers_para_revisao.csv", row.names = FALSE)
  
  # Regra da Seção 6.1.3, item 4: >=2 bancos na mesma data = evento sistêmico
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
  
  write.csv(relatorio_outliers, "outputs/tabelas/outliers_classificados.csv", row.names = FALSE)
  cat("\n  Tabela classificada salva em outputs/tabelas/outliers_classificados.csv\n")
} else {
  cat("  Nenhum outlier com |z| > 5 encontrado.\n")
}

# ── Correlação BPAN4 x BPAC11 — pré/pós anúncio de incorporação ────────────
if (all(c("BPAN4", "BPAC11") %in% NOMES)) {
  ANUNCIO_INCORPORACAO <- as.Date("2025-10-14")
  
  ret_bpan4  <- diff(log(as.numeric(dados_adj[["BPAN4"]]$Close)))
  ret_bpac11 <- diff(log(as.numeric(dados_adj[["BPAC11"]]$Close)))
  ret_df <- na.omit(data.frame(
    data = index(dados_adj[["BPAN4"]])[-1], bpan4 = ret_bpan4, bpac11 = ret_bpac11
  ))
  
  pre_tudo <- ret_df[ret_df$data <  ANUNCIO_INCORPORACAO, ]
  pos      <- ret_df[ret_df$data >= ANUNCIO_INCORPORACAO, ]
  
  # Janela pré-evento do MESMO TAMANHO que a pós (não a amostra inteira) —
  # comparar 7 anos de história contra 2-3 meses pós-anúncio dilui qualquer
  # mudança de regime recente numa média de longuíssimo prazo. Para testar
  # se a correlação mudou AO REDOR do evento, as duas janelas precisam ter
  # tamanho comparável e estar próximas da mesma data.
  n_pos <- nrow(pos)
  pre_janela <- tail(pre_tudo, n_pos)
  
  cor_pre_tudo   <- cor(pre_tudo$bpan4, pre_tudo$bpac11)
  cor_pre_janela <- if (nrow(pre_janela) >= 2) cor(pre_janela$bpan4, pre_janela$bpac11) else NA
  cor_pos        <- if (n_pos >= 2) cor(pos$bpan4, pos$bpac11) else NA
  
  cat("\n  Correlação BPAN4 x BPAC11 (retornos diários):\n")
  cat("    Amostra inteira até", as.character(ANUNCIO_INCORPORACAO), "(contexto, não comparável):",
      round(cor_pre_tudo, 3), "(n =", nrow(pre_tudo), ")\n")
  cat("    Janela pré-evento, mesmo tamanho que a pós (", nrow(pre_janela), "dias):",
      round(cor_pre_janela, 3), "\n")
  cat("    A partir de", as.character(ANUNCIO_INCORPORACAO), ":", round(cor_pos, 3), "(n =", n_pos, ")\n")
  
  # Checagem à parte: a deslistagem efetiva só ocorreu no fim de janeiro de
  # 2026 — bem depois do DATA_FIM global (31/12/2025). Se a convergência de
  # preço só se completa perto da execução da troca de ações, a janela
  # pós-evento usada acima (out-dez/2025) pode estar cedo demais para
  # capturá-la. Lê as duas abas de novo, sem o corte de DATA_FIM, só para
  # este diagnóstico — não altera dados_adj nem o painel principal.
  raw_pan_ext <- read_excel(CAMINHO_EXCEL, sheet = "BPAN4")  %>% mutate(Data = as.Date(Data)) %>% arrange(Data)
  raw_btg_ext <- read_excel(CAMINHO_EXCEL, sheet = "BPAC11") %>% mutate(Data = as.Date(Data)) %>% arrange(Data)
  
  ret_ext <- na.omit(data.frame(
    data  = raw_pan_ext$Data[-1],
    bpan4 = diff(log(raw_pan_ext$Fechamento_Aj)),
    bpac11 = diff(log(raw_btg_ext$Fechamento_Aj[match(raw_pan_ext$Data, raw_btg_ext$Data)]))
  ))
  
  pos_ext <- ret_ext[ret_ext$data >= ANUNCIO_INCORPORACAO, ]
  cor_pos_ext <- if (nrow(pos_ext) >= 2) cor(pos_ext$bpan4, pos_ext$bpac11) else NA
  
  cat("\n    [Diagnóstico, fora do DATA_FIM global] Pós-anúncio até a última data disponível (",
      as.character(max(pos_ext$data)), "):", round(cor_pos_ext, 3), "(n =", nrow(pos_ext), ")\n")
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
  sig2[!is.na(sig2) & sig2 < 0] <- NA          # RS pode sair negativo por ruído de
  # microestrutura em dias de range muito
  # estreito — tratado como NA, não como zero
  sqrt(sig2)                                    # desvio-padrão diário, não anualizado
}

# Apêndice (não usado na especificação principal): Garman-Klass, convenção
# Diebold-Yilmaz.
# garman_klass <- function(ohlc_mat) {
#   O <- as.numeric(ohlc_mat[, "Open"]); H <- as.numeric(ohlc_mat[, "High"])
#   L <- as.numeric(ohlc_mat[, "Low"]);  C <- as.numeric(ohlc_mat[, "Close"])
#   sig2 <- 0.5 * (log(H / L))^2 - (2 * log(2) - 1) * (log(C / O))^2
#   sqrt(pmax(sig2, 0))
# }

vol_rs <- list()

for (nome in NOMES) {
  d <- dados_adj[[nome]]
  ohlc_mat <- d[, c("Open","High","Low","Close")]
  
  n_nas <- sum(is.na(ohlc_mat))
  if (n_nas > 0) cat(" ", nome, ":", n_nas, "NAs no OHLC (mantidos até a Seção 7)\n")
  
  sd_diario <- rogers_satchell_overnight(ohlc_mat)
  vol_anual <- sd_diario * sqrt(252)   # anualizado, convenção Diebold-Yilmaz (2012, 2014)
  vol_rs[[nome]] <- xts(vol_anual, order.by = index(d))
  
  n_nas_vol <- sum(is.na(vol_anual))
  if (n_nas_vol > 0) cat("   ", nome, "→", n_nas_vol, "NAs na volatilidade estimada\n")
}

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

cat("\n  NAs por banco (antes da imputação):\n")
print(colSums(is.na(painel_raw)))
cat("\n  Distribuição de NAs por dia:\n")
print(table(rowSums(is.na(painel_raw))))

dias_sem_pregao <- rowSums(is.na(painel_raw)) == ncol(painel_raw)
cat("\n  Dias sem pregão removidos:", sum(dias_sem_pregao), "\n")
painel_raw <- painel_raw[!dias_sem_pregao, ]

cat("  Imputando NAs com filtro de Kalman (auto.arima)...\n")
painel_imp <- apply(painel_raw, 2, function(col) {
  if (any(is.na(col))) na_kalman(col, model = "auto.arima", smooth = TRUE) else col
})
painel_imp <- xts(painel_imp, order.by = index(painel_raw))
colnames(painel_imp) <- NOMES

stopifnot("Ainda há NAs no painel após imputação!" = sum(is.na(painel_imp)) == 0)

cat("  OK — painel final:", nrow(painel_imp), "observações ×", ncol(painel_imp), "bancos. Zero NAs.\n")

write.csv(data.frame(data = index(painel_imp), as.data.frame(painel_imp)),
          "outputs/tabelas/painel_logvol.csv", row.names = FALSE)


# ==============================================================================
# SEÇÃO 8 — TESTES DE ESTACIONARIEDADE E MEMÓRIA LONGA (PAINEL PÓS-KALMAN)
# ==============================================================================
# Roda sobre painel_imp (contínuo, sem NA por construção — Seção 7), não
# sobre a série pré-imputação com na.omit(), que emendaria segmentos não
# contíguos (colaria o dia 50 direto no dia 73 se o meio virasse NA).

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

cat("\n  Nota: ADF rejeitando e KPSS não rejeitando simultaneamente (comum aqui)\n",
    "  não é contradição — é a assinatura de memória longa (0 < d < 1), não de\n",
    "  raiz unitária. Ver coluna GPH_d.\n")

write.csv(resultados_testes, "outputs/tabelas/testes_estacionariedade.csv", row.names = FALSE)
cat("  Tabela salva em outputs/tabelas/testes_estacionariedade.csv\n")


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

write.csv(desc_df, "outputs/tabelas/estatisticas_descritivas.csv")
cat("  Salvo em outputs/tabelas/estatisticas_descritivas.csv\n")


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
write.csv(bic_tabela, "outputs/tabelas/var_selecao_lags.csv", row.names = FALSE)
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

write.csv(tabela_dy, "outputs/tabelas/dy_fullsample.csv", row.names = FALSE)
write.csv(tabela_gfevd, "outputs/tabelas/gfevd_fullsample.csv")
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
  labs(title = "Total Connectedness Index — Sistema Bancário Brasileiro (2019–2025)",
       subtitle = paste0("VAR(", P_LAGS, "), GFEVD H = ", H_HORIZONTE, ", janela W = ", N_JANELA_W, " dias úteis"),
       x = NULL, y = "TCI (%)") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank())

ggsave("outputs/graficos/tci_rolling.png", p_tci, width = 12, height = 5, dpi = 300)
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
        panel.grid.minor = element_blank())

ggsave("outputs/graficos/net_rolling.png", p_net, width = 14, height = 8, dpi = 300)
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

write.csv(tci_comparacao, "outputs/tabelas/tci_comparacao_janelas.csv", row.names = FALSE)

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

# As três datas de saída ficam próximas (só ~50 pregões entre elas) — se os
# rótulos ficarem todos na mesma altura, colidem entre si e com a curva.
# Escalona a altura em três níveis (um por janela) para não sobrepor.
teto <- max(tci_comparacao$TCI, na.rm = TRUE)
eventos_saida$y_label <- teto * c(0.99, 0.90, 0.81)[match(eventos_saida$W, levels(eventos_saida$W))]

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
  labs(
    title = "Índice de conectividade total por janela rolante (W = 150, 200, 250)",
    subtitle = "Faixa cinza: colapso de mar/2020. Linhas verticais: data em que o colapso deixa cada janela — o degrau acompanha W, não o evento.",
    x = NULL, y = "TCI (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9, color = "grey30"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    plot.margin = margin(t = 12, r = 16, b = 10, l = 10)
  )

ggsave("outputs/graficos/tci_comparacao_janelas.png", p_comparacao, width = 12, height = 7, dpi = 300, bg = "white")
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
# SELEÇÃO de kappa1/kappa2: implementamos abaixo o filtro de Kalman com
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
grade_k2 <- c(0.94, 0.96, 0.99)

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

write.csv(grade_tabela, "outputs/tabelas/tvpvar_grade_kappa.csv", row.names = FALSE)

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
          "outputs/tabelas/tci_tvpvar.csv", row.names = FALSE)
write.csv(data.frame(data = index(net_tvp), as.data.frame(net_tvp)),
          "outputs/tabelas/net_tvpvar.csv", row.names = FALSE)

p_tci_tvp <- ggplot(data.frame(data = index(tci_tvp), TCI = as.numeric(tci_tvp)),
                    aes(x = data, y = TCI)) +
  geom_line(color = "#b2182b", linewidth = 0.7) +
  geom_vline(data = EVENTOS, aes(xintercept = as.numeric(data)),
             linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_text(data = EVENTOS, aes(x = data, y = max(as.numeric(tci_tvp)) * 0.97, label = label),
            angle = 90, hjust = 1, vjust = -0.3, size = 2.8, color = "grey30") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "Total Connectedness Index — TVP-VAR (especificação principal)",
       subtitle = paste0("kappa1 = ", KAPPA1, ", kappa2 = ", KAPPA2,
                         " | GFEVD H = ", H_HORIZONTE),
       x = NULL, y = "TCI (%)") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank())

ggsave("outputs/graficos/tci_tvpvar.png", p_tci_tvp, width = 12, height = 5, dpi = 300)
cat("  Gráfico TCI (TVP-VAR) salvo.\n")


# ==============================================================================
# SEÇÃO 13 — REDES: MATRIZ DE ADJACÊNCIA (FULL SAMPLE)
# ==============================================================================

cat("\n[13] Construindo matriz de adjacência (grafo completo, sem threshold)...\n")

# dca_full$CT ("Connectedness Table"), não $TABLE. Formato 3D/4D não
# documentado para connectedness="Time" — inspecionar antes de indexar.
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
  cat("    OK — bateu.\n")
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

# Betweenness não incluída: não é interpretável em grafo completo — com
# as 56 arestas dirigidas todas presentes, os valores saem como ruído de
# baixa magnitude, sem leitura econômica. Métricas baseadas em força/peso.

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

write.csv(metricas_centralidade, "outputs/tabelas/centralidade_fullsample.csv", row.names = FALSE)
cat("  Salvo em outputs/tabelas/centralidade_fullsample.csv\n")

# Consolida TO/FROM/NET (Seção 10) com as métricas de centralidade numa
# tabela só.
tabela_dy_rede <- merge(tabela_dy, metricas_centralidade, by = "Banco")
tabela_dy_rede <- tabela_dy_rede[order(-tabela_dy_rede$TO), ]
rownames(tabela_dy_rede) <- NULL

cat("\n  Tabela consolidada (DY + centralidade):\n")
print(tabela_dy_rede)

write.csv(tabela_dy_rede, "outputs/tabelas/dy_centralidade_fullsample.csv", row.names = FALSE)
cat("  Salvo em outputs/tabelas/dy_centralidade_fullsample.csv\n")

# Classificação manual (Seção 6.1.1) — usada aqui e no preenchimento dos
# nós da Seção 16.
tipo_controle <- c(
  ITUB4 = "Privado nacional", BBDC4 = "Privado nacional",
  BBAS3 = "Público federal", SANB11 = "Privado estrangeiro",
  BPAC11 = "Privado nacional (BTG)", BRSR6 = "Público estadual",
  ABCB4 = "Privado estrangeiro", BPAN4 = "Privado nacional (BTG)"
)

# Porte: grande (Itaú, Bradesco, BB, Santander, BTG) vs. médio (Banrisul,
# ABC Brasil, Banco Pan).
porte <- c(
  ITUB4 = "Grande", BBDC4 = "Grande", BBAS3 = "Grande", SANB11 = "Grande",
  BPAC11 = "Grande", BRSR6 = "Médio", ABCB4 = "Médio", BPAN4 = "Médio"
)

# ── Regressão diádica (substitui a assortatividade) ────────────────────────
# Unidade de observação: o par ordenado (i,j), i != j — 56 linhas (8x7).
# Variável dependente: theta_ij x 100 (Eq. 9). alpha_j e gamma_i são
# efeitos fixos de emissor e receptor — absorvem a propensão geral de cada
# banco a transmitir/receber de QUALQUER contraparte. O que sobra em
# mesmo_controle/ambos_grande é o que aquele PAR específico tem de
# particular, descontada essa propensão geral.
cat("\n  Montando painel diádico (pares i != j)...\n")

pares_ij <- expand.grid(receptor = NOMES, emissor = NOMES, stringsAsFactors = FALSE)
pares_ij <- pares_ij[pares_ij$receptor != pares_ij$emissor, ]

pares_ij$theta_pct       <- mapply(function(i, j) theta_offdiag[i, j] * 100,
                                   pares_ij$receptor, pares_ij$emissor)
pares_ij$mesmo_controle  <- as.integer(tipo_controle[pares_ij$receptor] == tipo_controle[pares_ij$emissor])
pares_ij$ambos_grande    <- as.integer(porte[pares_ij$receptor] == "Grande" & porte[pares_ij$emissor] == "Grande")
pares_ij$par_id          <- apply(cbind(pares_ij$receptor, pares_ij$emissor), 1, function(x) paste(sort(x), collapse = "-"))

cat("  ", nrow(pares_ij), "observações (pares dirigidos),", length(unique(pares_ij$par_id)), "pares não-dirigidos (clusters)\n")

lm_diadica <- lm(theta_pct ~ factor(emissor) + factor(receptor) + mesmo_controle + ambos_grande,
                 data = pares_ij)

# Erros-padrão clusterizados por par não-dirigido: theta_ij e theta_ji
# nascem da mesma relação bilateral, resíduos não são independentes.
vcov_cluster <- sandwich::vcovCL(lm_diadica, cluster = pares_ij$par_id)
teste_diadica <- lmtest::coeftest(lm_diadica, vcov = vcov_cluster)

resultado_diadica <- data.frame(
  Indicadora  = c("Mesmo tipo de controle", "Ambos de grande porte"),
  Coeficiente = round(teste_diadica[c("mesmo_controle", "ambos_grande"), "Estimate"], 3),
  Erro_padrao = round(teste_diadica[c("mesmo_controle", "ambos_grande"), "Std. Error"], 3),
  t           = round(teste_diadica[c("mesmo_controle", "ambos_grande"), "t value"], 2),
  p           = round(teste_diadica[c("mesmo_controle", "ambos_grande"), "Pr(>|t|)"], 4)
)

cat("\n  Regressão diádica — coeficientes de interesse (p.p. de variância explicada):\n")
print(resultado_diadica, row.names = FALSE)

write.csv(resultado_diadica, "outputs/tabelas/regressao_diadica_fullsample.csv", row.names = FALSE)
write.csv(pares_ij, "outputs/tabelas/painel_diadico.csv", row.names = FALSE)
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
pares_ij$par_itub_bbdc <- par_especifico(pares_ij$receptor, pares_ij$emissor, "ITUB4", "BBDC4")
pares_ij$par_btg_pan   <- par_especifico(pares_ij$receptor, pares_ij$emissor, "BPAC11", "BPAN4")
pares_ij$par_bb_brsr   <- par_especifico(pares_ij$receptor, pares_ij$emissor, "BBAS3", "BRSR6")
pares_ij$par_abc_san   <- par_especifico(pares_ij$receptor, pares_ij$emissor, "ABCB4", "SANB11")

lm_diadica_par <- lm(
  theta_pct ~ factor(emissor) + factor(receptor) +
    par_itub_bbdc + par_btg_pan + par_bb_brsr + par_abc_san + ambos_grande,
  data = pares_ij
)

vcov_cluster_par  <- sandwich::vcovCL(lm_diadica_par, cluster = pares_ij$par_id)
teste_diadica_par <- lmtest::coeftest(lm_diadica_par, vcov = vcov_cluster_par)

vars_par <- c("par_itub_bbdc", "par_btg_pan", "par_bb_brsr", "par_abc_san", "ambos_grande")
resultado_diadica_par <- data.frame(
  Indicadora  = c("Bradesco e Itaú (privados nacionais)", "BTG e Pan (vínculo societário)",
                  "Banco do Brasil e Banrisul (estatais)", "ABC Brasil e Santander (\"estrangeiros\")",
                  "Ambos de grande porte"),
  Coeficiente = round(teste_diadica_par[vars_par, "Estimate"], 3),
  Erro_padrao = round(teste_diadica_par[vars_par, "Std. Error"], 3),
  t           = round(teste_diadica_par[vars_par, "t value"], 2),
  p           = round(teste_diadica_par[vars_par, "Pr(>|t|)"], 4)
)

cat("\n  Regressão diádica — indicadoras por par específico (corte único, preliminar):\n")
print(resultado_diadica_par, row.names = FALSE)

write.csv(resultado_diadica_par, "outputs/tabelas/regressao_diadica_par_especifico.csv", row.names = FALSE)
cat("  Tabela salva em outputs/tabelas/regressao_diadica_par_especifico.csv\n")


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
  pares_t$theta_pct      <- mapply(function(i, j) theta_t[i, j] * 100, pares_t$receptor, pares_t$emissor)
  pares_t$par_itub_bbdc  <- par_especifico(pares_t$receptor, pares_t$emissor, "ITUB4", "BBDC4")
  pares_t$par_btg_pan    <- par_especifico(pares_t$receptor, pares_t$emissor, "BPAC11", "BPAN4")
  pares_t$par_bb_brsr    <- par_especifico(pares_t$receptor, pares_t$emissor, "BBAS3", "BRSR6")
  pares_t$par_abc_san    <- par_especifico(pares_t$receptor, pares_t$emissor, "ABCB4", "SANB11")
  pares_t$ambos_grande   <- as.integer(porte[pares_t$receptor] == "Grande" & porte[pares_t$emissor] == "Grande")
  
  lm_t <- tryCatch(
    lm(theta_pct ~ factor(emissor) + factor(receptor) +
         par_itub_bbdc + par_btg_pan + par_bb_brsr + par_abc_san + ambos_grande,
       data = pares_t),
    error = function(e) NULL
  )
  if (is.null(lm_t)) next
  
  cf <- coef(lm_t)
  coefs_temporais <- rbind(coefs_temporais, data.frame(
    data         = dt,
    itub_bbdc    = unname(cf["par_itub_bbdc"]),
    btg_pan      = unname(cf["par_btg_pan"]),
    bb_brsr      = unname(cf["par_bb_brsr"]),
    abc_san      = unname(cf["par_abc_san"]),
    ambos_grande = unname(cf["ambos_grande"])
  ))
}

cat("  ", nrow(coefs_temporais), "cortes estimados com sucesso.\n")

# Resumo: média da série de coeficientes, % de cortes com sinal positivo, e
# teste t da média contra zero — é essa média testada, não um corte isolado,
# que sustenta o argumento.
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

resumo_temporal <- rbind(
  cbind(Indicadora = "Bradesco e Itaú (privados nacionais)",    resumir_serie(coefs_temporais$itub_bbdc)),
  cbind(Indicadora = "BTG e Pan (vínculo societário)",          resumir_serie(coefs_temporais$btg_pan)),
  cbind(Indicadora = "Banco do Brasil e Banrisul (estatais)",   resumir_serie(coefs_temporais$bb_brsr)),
  cbind(Indicadora = "ABC Brasil e Santander (\"estrangeiros\")", resumir_serie(coefs_temporais$abc_san)),
  cbind(Indicadora = "Ambos de grande porte",                   resumir_serie(coefs_temporais$ambos_grande))
)

cat("\n  Regressão diádica — extensão temporal (", nrow(coefs_temporais), "cortes mensais):\n")
print(resumo_temporal, row.names = FALSE)

write.csv(coefs_temporais, "outputs/tabelas/regressao_diadica_temporal_serie.csv", row.names = FALSE)
write.csv(resumo_temporal, "outputs/tabelas/regressao_diadica_temporal_resumo.csv", row.names = FALSE)
cat("  Tabelas salvas.\n")

# ── Gráfico: coeficientes diádicos ao longo do tempo ────────────────────────
coefs_long <- coefs_temporais %>%
  pivot_longer(-data, names_to = "par", values_to = "coeficiente") %>%
  mutate(par = factor(par,
                      levels = c("itub_bbdc", "btg_pan", "bb_brsr", "abc_san", "ambos_grande"),
                      labels = c("Bradesco e Itaú", "BTG e Pan", "BB e Banrisul", "ABC Brasil e Santander", "Ambos grande porte")
  ))

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

ggsave("outputs/graficos/coefs_diadicos_temporais.png", p_coefs_temporais, width = 10, height = 12, dpi = 300, bg = "white")
cat("  Gráfico salvo em outputs/graficos/coefs_diadicos_temporais.png\n")

# ── Médias anuais do coeficiente BTG-Pan ────────────────────────────────────
coefs_temporais$ano <- format(coefs_temporais$data, "%Y")
media_anual_btgpan <- aggregate(btg_pan ~ ano, data = coefs_temporais, FUN = mean)
media_anual_btgpan$btg_pan <- round(media_anual_btgpan$btg_pan, 2)

cat("\n  Coeficiente BTG-Pan — média por ano:\n")
print(media_anual_btgpan, row.names = FALSE)

write.csv(media_anual_btgpan, "outputs/tabelas/btgpan_coeficiente_anual.csv", row.names = FALSE)
cat("  Tabela salva em outputs/tabelas/btgpan_coeficiente_anual.csv\n")


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

# Layout FR calculado uma vez — reaproveitar fixo nos painéis de subperíodo futuros.
set.seed(42)
layout_fr <- create_layout(g_net, layout = "fr")

p_rede <- ggraph(layout_fr) +
  geom_edge_link(
    aes(width = weight, alpha = weight),
    arrow = arrow(length = unit(3, "mm"), type = "closed"),
    end_cap = circle(6, "mm"), color = "grey40"
  ) +
  scale_edge_width(range = c(0.3, 2.5), name = "Peso do fluxo (p.p.)") +
  scale_edge_alpha(range = c(0.4, 0.9), guide = "none") +   # redundante com a espessura, sem legenda própria
  geom_node_point(aes(size = sqrt(TO), fill = Tipo), shape = 21, color = "black", stroke = 0.6) +
  scale_size_continuous(range = c(6, 16), name = "TOi (√, p.p.)") +
  scale_fill_brewer(palette = "Set2", name = "Tipo de controle") +
  geom_node_text(aes(label = name), vjust = -1.6, size = 4, fontface = "bold") +
  labs(
    title = "Rede de conectividade — Sistema Bancário Brasileiro (full sample, 2019-2025)",
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

ggsave("outputs/graficos/rede_full_sample.png", p_rede, width = 11, height = 8, dpi = 300, bg = "white")
cat("  Grafo salvo em outputs/graficos/rede_full_sample.png\n")


# ==============================================================================
# SEÇÃO 17 (APÊNDICE) — ROBUSTEZ
# ==============================================================================
# Sensibilidade do TCI e do ranking de transmissor/receptor líquido a cinco
# dimensões: número de defasagens (p), horizonte da GFEVD (H), tamanho da
# janela rolante (W), fatores de esquecimento (kappa1/kappa2) e sub-amostras.
# Em cada bloco, reestima e reporta TCI + o banco com maior NET (transmissor
# líquido) e menor NET (receptor líquido) — a pergunta é se essa identidade
# se mantém estável entre as variações, não só se o TCI muda de nível.

cat("\n[17 — apêndice] Robustez...\n")

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
write.csv(robustez_p, "outputs/tabelas/robustez_lags.csv", row.names = FALSE)

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
write.csv(robustez_h, "outputs/tabelas/robustez_horizonte.csv", row.names = FALSE)

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
write.csv(robustez_w, "outputs/tabelas/robustez_janela.csv", row.names = FALSE)

# ── D. Sensibilidade aos fatores de esquecimento (kappa1, kappa2) ──────────
# Reestima só os 2 próximos pares mais bem colocados na grade (Seção 12) —
# o vencedor (KAPPA1/KAPPA2) já foi calculado como dca_tvp e é reaproveitado.
# AVISO: cada par não reaproveitado roda o TVP-VAR completo de novo — pode
# levar alguns minutos por par.
cat("\n  D) Variando (kappa1, kappa2) — top 3 da grade de verossimilhança...\n")

# kappa1 = 1 (coeficientes constantes) é um caso de fronteira que nosso
# filtro de Kalman (usado só para a busca em grade) lida bem, mas o
# ConnectednessApproach exige kappa1 ESTRITAMENTE entre 0 e 1 para estimar
# o TVP-VAR de fato — excluído aqui antes de escolher o top 3 a reestimar.
top3_kappa <- head(grade_tabela[grade_tabela$kappa1 < 1, ], 3)
robustez_kappa <- data.frame()

for (i in seq_len(nrow(top3_kappa))) {
  k1 <- top3_kappa$kappa1[i]; k2 <- top3_kappa$kappa2[i]
  
  if (isTRUE(all.equal(k1, KAPPA1)) && isTRUE(all.equal(k2, KAPPA2))) {
    dca_k <- dca_tvp   # já calculado na Seção 12
  } else {
    cat("    Estimando TVP-VAR para kappa1 =", k1, ", kappa2 =", k2, "...\n")
    dca_k <- ConnectednessApproach(
      Y, nlag = P_LAGS, nfore = H_HORIZONTE, model = "TVP-VAR", connectedness = "Time",
      VAR_config = list(TVPVAR = list(kappa1 = k1, kappa2 = k2, prior = "BayesPrior", gamma = 0.01))
    )
  }
  
  tci_k <- as.numeric(dca_k$TCI[, 1])
  net_medio_k <- colMeans(dca_k$NET)
  
  robustez_kappa <- rbind(robustez_kappa, data.frame(
    kappa1 = k1, kappa2 = k2, peso_posterior = round(top3_kappa$peso_post[i], 4),
    TCI_medio = round(mean(tci_k, na.rm = TRUE), 2),
    Maior_transmissor = NOMES[which.max(net_medio_k)], Maior_receptor = NOMES[which.min(net_medio_k)]
  ))
}
cat("  Resultado:\n"); print(robustez_kappa, row.names = FALSE)
write.csv(robustez_kappa, "outputs/tabelas/robustez_kappa.csv", row.names = FALSE)

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
write.csv(robustez_sub, "outputs/tabelas/robustez_subamostras.csv", row.names = FALSE)

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
writeLines(capture.output(sessionInfo()), "outputs/sessionInfo.txt")
cat("\n  sessionInfo() salva em outputs/sessionInfo.txt\n")