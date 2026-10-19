# Consolida os arquivos COMDINHEIRO_alunos_bolivar<TICKER>.xlsx
# em um único Excel, com uma aba por ticker e "nd" convertido em vazio (NA)

library(readxl)
library(writexl)

pasta_entrada <- "ComDinheiro"   # pasta onde estão os arquivos do Comdinheiro
arquivo_saida <- "Bancos_Cotacoes_Comdinheiro.xlsx"

# nome original -> nome novo
de_para <- c(
  "Data"              = "Data",
  "Abertura Ajustado" = "Abertura_Aj",
  "Máx Ajustado"      = "Maximo_Aj",
  "Mín Ajustado"      = "Minimo_Aj",
  "Fech Ajustado"     = "Fechamento_Aj",
  "Fech Histórico"    = "Fechamento_Hist",
  "Variação(%)"       = "Variacao_pct",
  "Vol (MM R$)"       = "Volume_MM_RS",
  "Negócios"          = "Negocios",
  "Fator"             = "Fator",
  "Tipo"              = "Tipo"
)
cols_num <- c("Abertura_Aj", "Maximo_Aj", "Minimo_Aj", "Fechamento_Aj",
              "Fechamento_Hist", "Variacao_pct", "Volume_MM_RS",
              "Negocios", "Fator")

arquivos <- list.files(pasta_entrada,
                       pattern = "\\.xlsx$",
                       full.names = TRUE)

abas <- list()
for (arq in arquivos) {
  ticker <- tools::file_path_sans_ext(basename(arq))
  
  # lê tudo como texto para tratar o "nd" de forma controlada
  df <- as.data.frame(read_excel(arq, col_types = "text"))
  
  # seleciona, ordena e renomeia as colunas (descarta "Médio Ajustado")
  df <- df[, names(de_para)]
  names(df) <- unname(de_para)
  
  # "nd" -> NA em todas as colunas
  df[df == "nd"] <- NA
  
  # remove linhas sem nenhum dado (só a data preenchida)
  sem_dados <- rowSums(!is.na(df[, names(df) != "Data"])) == 0
  df <- df[!sem_dados, ]
  
  # tipos corretos
  df$Data <- as.Date(as.numeric(df$Data), origin = "1899-12-30")  # data serial do Excel
  df[cols_num] <- lapply(df[cols_num], as.numeric)
  
  abas[[ticker]] <- as.data.frame(df)
  cat(ticker, ":", nrow(df), "linhas\n")
  cat(ticker, ":", nrow(df), "linhas (", sum(sem_dados), "removidas )\n")
}

write_xlsx(abas, arquivo_saida)
cat("Arquivo gerado:", arquivo_saida, "\n")