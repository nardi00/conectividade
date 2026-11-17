# ==============================================================================
# APÊNDICE: GARMAN-KLASS COM TERMO OVERNIGHT x ROGERS-SATCHELL COM TERMO OVERNIGHT
# ==============================================================================
# O Garman-Klass só zera quando H = L. Por isso ele serve de verificação de que a correção
# dos pregões de variância nula (regra sig2 < 1e-8 do Rogers-Satchell) não é artefato da
# regra de exclusão. Este script roda os DOIS painéis com os DOIS estimadores e grava:
#   - CSV de comparação (TCI; TO/NET/posto por banco; coeficientes da Tabela 10);
#   - fragmentos LaTeX (tabulares) para o apêndice.
# Só usa a parte ESTÁTICA do pipeline (sem TVP-VAR), então é rápido.
#
# VERIFICAÇÃO OBRIGATÓRIA: a linha "RS" de cada painel precisa reproduzir o texto principal
# (TCI 52,86 e 55,71; TO/NET da Tabela 9; coeficientes da Tabela 10). Se não reproduzir, há
# diferença de configuração em relação ao script principal e o apêndice não pode ser usado.
# ==============================================================================

options(xts.warn_dplyr_breaks_lag = FALSE)

library(readxl)
library(xts)
library(zoo)
library(imputeTS)
library(ConnectednessApproach)
library(sandwich)
library(lmtest)
library(dplyr)

CAMINHO_EXCEL <- "Bancos_Cotacoes_Comdinheiro.xlsx"   # o mesmo do script principal
OUT <- file.path("outputs", "Apendice_GK")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

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
  )
)

P_LAGS         <- 2
H_HORIZONTE    <- 10
VOLUME_MIN_BRL <- 100000
EPS            <- 1e-6
PISO_NUMERICO  <- 1e-8

porte <- c(ITUB4 = "Grande", BBDC4 = "Grande", BBAS3 = "Grande", SANB11 = "Grande",
           BPAC11 = "Grande", BRSR6 = "Médio", ABCB4 = "Médio", BPAN4 = "Médio")
S1 <- c("ITUB4", "BBDC4", "BBAS3", "SANB11", "BPAC11")

# Mesma ordem da Tabela 10 do texto
PARES_ESPECIFICOS <- list(
  par_itub_bbdc = list(bancos = c("ITUB4", "BBDC4"),  label = "Itaú e Bradesco"),
  par_bb_brsr   = list(bancos = c("BBAS3", "BRSR6"),  label = "Banco do Brasil e Banrisul"),
  par_abc_san   = list(bancos = c("ABCB4", "SANB11"), label = "ABC Brasil e Santander"),
  par_btg_pan   = list(bancos = c("BPAC11", "BPAN4"), label = "BTG e Pan")
)

# ── Leitura e filtros (mesmos das Seções 1 a 3 do script principal) ────────────
ler_ohlc <- function(nome, ini, fim) {
  raw <- read_excel(CAMINHO_EXCEL, sheet = nome) %>%
    mutate(Data = as.Date(Data)) %>%
    filter(Data >= as.Date(ini), Data <= as.Date(fim)) %>%
    arrange(Data)
  m <- xts(
    data.frame(Open = raw$Abertura_Aj, High = raw$Maximo_Aj, Low = raw$Minimo_Aj,
               Close = raw$Fechamento_Aj, Volume_BRL = raw$Volume_MM_RS * 1e6),
    order.by = raw$Data
  )
  op <- as.numeric(m$Open); hi <- as.numeric(m$High); lo <- as.numeric(m$Low); cl <- as.numeric(m$Close)
  viol <- (hi < lo - EPS) | (hi < op - EPS) | (hi < cl - EPS) |
    (lo > op + EPS) | (lo > cl + EPS) | (!is.na(op) & op <= 0) | (!is.na(cl) & cl <= 0)
  viol[is.na(viol)] <- FALSE
  m[viol, c("Open", "High", "Low", "Close")] <- NA
  vol <- as.numeric(m$Volume_BRL)
  liq <- !is.na(vol) & (vol < VOLUME_MIN_BRL)
  m[liq, c("Open", "High", "Low", "Close")] <- NA
  m
}

# ── Estimadores: os dois com o MESMO termo overnight ────────────────────────────
sigma2_estimador <- function(ohlc, estimador) {
  O <- as.numeric(ohlc[, "Open"]); H <- as.numeric(ohlc[, "High"])
  L <- as.numeric(ohlc[, "Low"]);  C <- as.numeric(ohlc[, "Close"])
  C_lag <- c(NA_real_, C[-length(C)])
  overnight2 <- (log(O / C_lag))^2
  base <- if (estimador == "RS") {
    log(H / C) * log(H / O) + log(L / C) * log(L / O)                  # Rogers-Satchell
  } else {
    0.5 * (log(H / L))^2 - (2 * log(2) - 1) * (log(C / O))^2           # Garman-Klass
  }
  sig2 <- overnight2 + base
  nulo <- !is.na(sig2) & sig2 < 1e-8        # mesma regra do script principal
  sig2[nulo] <- NA
  list(sig2 = sig2, n_nulos = sum(nulo))
}

montar_painel <- function(cfg, estimador) {
  lv <- list(); nulos <- integer(0)
  for (nome in cfg$nomes) {
    ohlc <- ler_ohlc(nome, cfg$data_inicio, cfg$data_fim)
    r <- sigma2_estimador(ohlc, estimador)
    nulos[nome] <- r$n_nulos
    vol_anual <- sqrt(r$sig2) * sqrt(252)
    lv[[nome]] <- xts(log(pmax(vol_anual, PISO_NUMERICO)), order.by = index(ohlc))
  }
  P <- do.call(merge, c(lv, all = TRUE))
  colnames(P) <- cfg$nomes
  P <- P[rowSums(is.na(P)) < ncol(P), ]                       # tira dias sem pregão
  n_imputados <- colSums(is.na(P))
  Pimp <- apply(P, 2, function(col) {
    if (any(is.na(col))) na_kalman(col, model = "auto.arima", smooth = TRUE) else col
  })
  Pimp <- xts(Pimp, order.by = index(P))
  colnames(Pimp) <- cfg$nomes
  stopifnot("Ainda há NAs após a imputação" = sum(is.na(Pimp)) == 0)
  list(painel = Pimp, nulos = nulos, n_imputados = n_imputados)
}

# ── VAR(2) + GFEVD (estático) ────────────────────────────────────────────────────
estimar_estatico <- function(P) {
  Y <- zoo(as.matrix(P), order.by = index(P))
  dca <- ConnectednessApproach(Y, nlag = P_LAGS, nfore = H_HORIZONTE, window = NULL,
                               corrected = FALSE, model = "VAR")
  CT <- dca$CT; nd <- length(dim(CT))
  theta <- if (nd == 2) CT else if (nd == 3) CT[, , 1] else CT[, , 1, dim(CT)[4]]
  dimnames(theta) <- list(colnames(P), colnames(P))
  theta_off <- theta; diag(theta_off) <- 0
  list(tci = as.numeric(dca$TCI), to = as.numeric(dca$TO), from = as.numeric(dca$FROM),
       net = as.numeric(dca$NET), theta = theta_off)
}

# ── Regressão diádica por par específico (corte único; igual à Tabela 10) ───────
diadica_por_par <- function(theta_off, nomes) {
  pares_ij <- expand.grid(receptor = nomes, emissor = nomes, stringsAsFactors = FALSE)
  pares_ij <- pares_ij[pares_ij$receptor != pares_ij$emissor, ]
  pares_ij$theta_pct    <- mapply(function(i, j) theta_off[i, j] * 100, pares_ij$receptor, pares_ij$emissor)
  pares_ij$ambos_grande <- as.integer(porte[pares_ij$receptor] == "Grande" & porte[pares_ij$emissor] == "Grande")
  pares_ij$par_id       <- apply(cbind(pares_ij$receptor, pares_ij$emissor), 1, function(x) paste(sort(x), collapse = "-"))
  disponiveis <- Filter(function(p) all(p$bancos %in% nomes), PARES_ESPECIFICOS)
  for (v in names(disponiveis)) {
    b <- disponiveis[[v]]$bancos
    pares_ij[[v]] <- as.integer((pares_ij$receptor == b[1] & pares_ij$emissor == b[2]) |
                                  (pares_ij$receptor == b[2] & pares_ij$emissor == b[1]))
  }
  vars_par <- names(disponiveis)
  f <- as.formula(paste("theta_pct ~ factor(emissor) + factor(receptor) +",
                        paste(vars_par, collapse = " + "), "+ ambos_grande"))
  m  <- lm(f, data = pares_ij)
  ct <- lmtest::coeftest(m, vcov = sandwich::vcovCL(m, cluster = pares_ij$par_id))
  vars_todos <- c(vars_par, "ambos_grande")
  data.frame(
    var    = vars_todos,
    rotulo = c(vapply(disponiveis, function(p) p$label, character(1)), "Ambos de grande porte"),
    coef   = ct[vars_todos, "Estimate"],
    ep     = ct[vars_todos, "Std. Error"],
    p      = ct[vars_todos, "Pr(>|t|)"],
    row.names = NULL, stringsAsFactors = FALSE
  )
}

# ── Rodada: 2 painéis x 2 estimadores ────────────────────────────────────────────
res <- list()
for (pn in names(CONFIG_PAINEIS)) {
  cfg <- CONFIG_PAINEIS[[pn]]
  res[[pn]] <- list()
  for (est in c("RS", "GK")) {
    cat("\n==", pn, "|", est, "==\n")
    pp <- montar_painel(cfg, est)
    ee <- estimar_estatico(pp$painel)
    di <- diadica_por_par(ee$theta, cfg$nomes)
    dy <- data.frame(Banco = cfg$nomes, TO = ee$to, FROM = ee$from, NET = ee$net)
    dy$Posto <- rank(-dy$TO, ties.method = "first")
    res[[pn]][[est]] <- list(obs = nrow(pp$painel), nulos = sum(pp$nulos), tci = ee$tci, dy = dy, di = di)
    cat("  Observações:", nrow(pp$painel), "| pregões de variância nula:", sum(pp$nulos),
        "| TCI:", round(ee$tci, 2), "\n")
    print(dy[order(-dy$TO), ], row.names = FALSE)
    print(di, row.names = FALSE)
    top5 <- dy$Banco[order(-dy$TO)][1:5]
    cat("  Cinco primeiros por TO:", paste(top5, collapse = ", "), "| todos do S1:", all(top5 %in% S1), "\n")
    cat("  Maior transmissor líquido:", dy$Banco[which.max(dy$NET)],
        "| maior receptor líquido:", dy$Banco[which.min(dy$NET)], "\n")
  }
}

# ── Formatação (decimal com vírgula; negativos em modo matemático) ───────────────
fmt_num <- function(x, d = 2) {
  s <- formatC(abs(x), format = "f", digits = d)
  if (x < 0) paste0("$-", sub("\\.", "{,}", s), "$") else sub("\\.", ",", s)
}
fmt_p <- function(p) {
  if (p < 0.001) "$<0{,}001$" else sub("\\.", ",", formatC(p, format = "f", digits = 3))
}
cel_coef <- function(r) paste0(fmt_num(r$coef, 2), " (", fmt_num(r$ep, 2), ")")

# ── CSVs ─────────────────────────────────────────────────────────────────────────
tci_tab <- do.call(rbind, lapply(names(res), function(pn) do.call(rbind, lapply(names(res[[pn]]), function(est) {
  r <- res[[pn]][[est]]
  data.frame(painel = pn, estimador = est, obs = r$obs, dias_variancia_nula = r$nulos, TCI = round(r$tci, 2))
}))))
write.csv(tci_tab, file.path(OUT, "comparacao_tci.csv"), row.names = FALSE)

for (pn in names(res)) {
  a <- res[[pn]]$RS$dy; b <- res[[pn]]$GK$dy
  comp <- data.frame(Banco = a$Banco, TO_RS = round(a$TO, 2), NET_RS = round(a$NET, 2), Posto_RS = a$Posto,
                     TO_GK = round(b$TO, 2), NET_GK = round(b$NET, 2), Posto_GK = b$Posto)
  write.csv(comp[order(-comp$TO_RS), ], file.path(OUT, paste0("comparacao_dy_", pn, ".csv")), row.names = FALSE)
  d <- merge(res[[pn]]$RS$di, res[[pn]]$GK$di, by = c("var", "rotulo"), suffixes = c("_RS", "_GK"), sort = FALSE)
  write.csv(d, file.path(OUT, paste0("comparacao_diadica_", pn, ".csv")), row.names = FALSE)
}

# ── Fragmentos LaTeX ─────────────────────────────────────────────────────────────
# (A) TCI
linhas <- c("\\begin{tabular}{llcc}", "\\toprule",
            "Painel & Estimador & Pregões de variância nula & TCI (\\%) \\\\", "\\midrule")
for (pn in names(res)) {
  rotulo_painel <- if (pn == "N7_principal") "N = 7" else "N = 8"
  for (est in c("RS", "GK")) {
    r <- res[[pn]][[est]]
    nome_est <- if (est == "RS") "Rogers-Satchell" else "Garman-Klass"
    linhas <- c(linhas, sprintf("%s & %s & %d & %s \\\\", rotulo_painel, nome_est, r$nulos, fmt_num(r$tci, 2)))
  }
  if (pn != tail(names(res), 1)) linhas <- c(linhas, "\\midrule")
}
linhas <- c(linhas, "\\bottomrule", "\\end{tabular}")
writeLines(linhas, file.path(OUT, "tab_gk_tci.tex"))

# (B) Ordenação por TO, NET e posto, nos dois estimadores
linhas <- c("\\begin{tabular}{lrrcrrcc}", "\\toprule",
            " & \\multicolumn{3}{c}{Rogers-Satchell} & \\multicolumn{3}{c}{Garman-Klass} & \\\\",
            "\\cmidrule(lr){2-4}\\cmidrule(lr){5-7}",
            "Instituição & TO & NET & Posto & TO & NET & Posto & No S1 \\\\", "\\midrule")
for (pn in names(res)) {
  rotulo_painel <- if (pn == "N7_principal") "Painel principal (N = 7)" else "Painel de extensão (N = 8)"
  linhas <- c(linhas, sprintf("\\multicolumn{8}{l}{\\textit{%s}} \\\\", rotulo_painel))
  a <- res[[pn]]$RS$dy; b <- res[[pn]]$GK$dy
  b <- b[match(a$Banco, b$Banco), ]
  o <- order(-a$TO)
  for (k in o) {
    linhas <- c(linhas, sprintf("%s & %s & %s & %d & %s & %s & %d & %s \\\\",
                                a$Banco[k], fmt_num(a$TO[k], 2), fmt_num(a$NET[k], 2), a$Posto[k],
                                fmt_num(b$TO[k], 2), fmt_num(b$NET[k], 2), b$Posto[k],
                                if (a$Banco[k] %in% S1) "sim" else "não"))
  }
  if (pn != tail(names(res), 1)) linhas <- c(linhas, "\\midrule")
}
linhas <- c(linhas, "\\bottomrule", "\\end{tabular}")
writeLines(linhas, file.path(OUT, "tab_gk_dy.tex"))

# (C) Coeficientes da Tabela 10 com o Garman-Klass (mesmo layout da Tabela 10 do texto)
d7 <- res$N7_principal$GK$di; d8 <- res$N8_extensao$GK$di
ordem <- c("par_itub_bbdc", "par_bb_brsr", "par_abc_san", "par_btg_pan", "ambos_grande")
rotulos_tab <- c(par_itub_bbdc = "Itaú e Bradesco", par_bb_brsr = "Banco do Brasil e Banrisul",
                 par_abc_san = "ABC Brasil e Santander", par_btg_pan = "BTG e Pan",
                 ambos_grande = "Ambos de grande porte")
linhas <- c("\\begin{tabular}{lcccc}", "\\toprule",
            " & \\multicolumn{2}{c}{N = 7} & \\multicolumn{2}{c}{N = 8} \\\\",
            "Indicadora & Coef. (EP) & $p$ & Coef. (EP) & $p$ \\\\", "\\midrule")
for (v in ordem) {
  r7 <- d7[d7$var == v, ]; r8 <- d8[d8$var == v, ]
  c7 <- if (nrow(r7)) c(cel_coef(r7), fmt_p(r7$p)) else c("--", "--")
  c8 <- if (nrow(r8)) c(cel_coef(r8), fmt_p(r8$p)) else c("--", "--")
  linhas <- c(linhas, sprintf("%s & %s & %s & %s & %s \\\\", rotulos_tab[[v]], c7[1], c7[2], c8[1], c8[2]))
}
linhas <- c(linhas, "\\bottomrule", "\\end{tabular}")
writeLines(linhas, file.path(OUT, "tab_gk_diadica.tex"))

cat("\nArquivos gravados em", OUT, "\n")
cat("Confira: a linha RS de cada painel deve reproduzir TCI 52,86 / 55,71 e a Tabela 10 do texto.\n")
writeLines(capture.output(sessionInfo()), file.path(OUT, "sessionInfo.txt"))