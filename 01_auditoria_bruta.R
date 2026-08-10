# ==============================================================================
# ETAPA 01: INGESTÃO E AUDITORIA DAS BASES BRUTAS (OTIMIZADA PARA GRANDES ARQUIVOS)
# Agenda: Artigo 3 - Economia Política da Capacidade Estatal e CGVs
# ==============================================================================

cat("\n===================================================\n")
cat("--> Executando Etapa 01: Ingestão e Inspeção (N = 30)\n")
cat("===================================================\n\n")

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(data.table) # Essencial para ler CSVs gigantes do TiVA sem estourar memória RAM
})

# 1. Configuração de Diretórios e Log
pasta_logs <- "outputs/logs"
dir.create(pasta_logs, recursive = TRUE, showWarnings = FALSE)
dir.create("dados_brutos", recursive = TRUE, showWarnings = FALSE)

arquivo_log <- file.path(pasta_logs, "etapa01_ingestao.log")
log_con <- file(arquivo_log, open = "wt")

# Amostra de Análise
paises_latam      <- c("BRA", "MEX", "CHL", "ARG", "COL", "PER", "CRI", "URY")
paises_asia       <- c("CHN", "KOR", "VNM", "MYS", "THA", "IDN", "PHL", "IND", "BRN", "TWN")
paises_leste_euro <- c("POL", "HUN", "TUR", "ROU", "BGR", "HRV")
paises_africa_me  <- c("ZAF", "MAR", "EGY", "TUN", "SAU", "KAZ")

amostra_30 <- c(paises_latam, paises_asia, paises_leste_euro, paises_africa_me)
ano_inicio <- 1995; ano_fim <- 2022; t_anos <- ano_fim - ano_inicio + 1

# 2. Funções Auxiliares de Inspeção e Leitura de Alta Performance
inspecionar <- function(df, nome_fonte) {
  linhas <- nrow(df)
  colunas <- ncol(df)
  
  msg <- sprintf("Fonte: %-25s | Dimensões: %8d linhas x %3d colunas", nome_fonte, linhas, colunas)
  
  if (linhas == 0) {
    status_msg <- paste("❌ ERRO CRÍTICO:", msg, "[ BASE VAZIA! CHECK OS ARQUIVOS ]")
    cat(status_msg, "\n")
    writeLines(status_msg, log_con)
  } else {
    status_msg <- paste("✅ SUCESSO:     ", msg)
    cat(status_msg, "\n")
    writeLines(status_msg, log_con)
  }
  invisible(df)
}

ler_arquivo_flexivel <- function(pasta, padrao_regex, sheet = NULL) {
  if (!dir.exists(pasta)) return(tibble())
  
  arqs <- list.files(pasta, pattern = padrao_regex, full.names = TRUE, ignore.case = TRUE, recursive = TRUE)
  if (length(arqs) == 0) return(tibble())
  
  arq_alvo <- arqs[1]
  ext <- tolower(tools::file_ext(arq_alvo))
  
  tryCatch({
    if (ext %in% c("xlsx", "xls")) {
      abas <- excel_sheets(arq_alvo)
      aba_alvo <- if (!is.null(sheet) && sheet %in% abas) sheet else abas[1]
      return(read_excel(arq_alvo, sheet = aba_alvo))
    } else if (ext %in% c("csv", "txt")) {
      # Uso do data.table::fread para evitar o erro 'std::bad_alloc'
      df_dt <- data.table::fread(
        file = arq_alvo, 
        showProgress = FALSE, 
        encoding = "UTF-8",
        fill = TRUE,
        logical01 = FALSE
      )
      return(as_tibble(df_dt))
    } else {
      return(tibble())
    }
  }, error = function(e) {
    # Fallback para read_csv caso fread falhe por algum motivo raro
    tryCatch({
      return(readr::read_csv(arq_alvo, show_col_types = FALSE, progress = FALSE))
    }, error = function(e2) {
      cat("⚠️ Erro irrecuperável ao ler", arq_alvo, ":", e2$message, "\n")
      return(tibble())
    })
  })
}

# 3. Ingestão de Dados
cat("--> Lendo Indicadores Socioeconômicos e Institucionais...\n")
wdi_raw    <- ler_arquivo_flexivel("dados_brutos/wdi", "wdi.*csv$") %>% inspecionar("WDI")
wgi_raw    <- ler_arquivo_flexivel("dados_brutos/wgi", "wgi.*(xlsx|xls|csv)$") %>% inspecionar("WGI")
vdem_raw   <- ler_arquivo_flexivel("dados_brutos/vdem", "v-dem.*csv$") %>% inspecionar("V-Dem")
atlas_raw  <- ler_arquivo_flexivel("dados_brutos/atlas_eci", "(eci|growthrankings).*csv$") %>% inspecionar("Atlas ECI")
pwt_raw    <- ler_arquivo_flexivel("dados_brutos/pwt", "pwt.*(xlsx|xls|csv)$", sheet = "Data") %>% inspecionar("PWT")
bti_raw    <- ler_arquivo_flexivel("dados_brutos/BTI", ".*bti.*(xlsx|xls|csv)$") %>% inspecionar("BTI Longitudinal")
unesco_raw <- ler_arquivo_flexivel("dados_brutos/unesco_UIS", ".*(uis|expgdp).*csv$") %>% inspecionar("UNESCO P&D")
qog_raw    <- ler_arquivo_flexivel("dados_brutos/QOG", "qog.*csv$") %>% inspecionar("Quality of Government")

cat("\n--> Lendo Bases de Cadeias Globais de Valor (CGVs)...\n")
tiva_dvash     <- ler_arquivo_flexivel("dados_brutos/tiva", ".*(dvapsh|dvash).*csv$") %>% inspecionar("TiVA EXGR_DVAPSH")
tiva_fvash    <- ler_arquivo_flexivel("dados_brutos/tiva", ".*(fvash).*csv$") %>% inspecionar("TiVA EXGR_FVASH")
tiva_intdvapsh <- ler_arquivo_flexivel("dados_brutos/tiva", ".*(intdvapsh).*csv$") %>% inspecionar("TiVA EXGR_INTDVAPSH")

wbgvc_countries <- ler_arquivo_flexivel("dados_brutos/wb_gvc", ".*gvc-countries.*csv$") %>% inspecionar("WB GVC Countries")
wbgvc_world     <- ler_arquivo_flexivel("dados_brutos/wb_gvc", ".*gvc_output_WORLD.*csv$") %>% inspecionar("WB GVC World")
wbgvc_wits      <- ler_arquivo_flexivel("dados_brutos/wb_gvc", ".*gvc_output_WITS.*csv$") %>% inspecionar("WB GVC WITS")

# 4. Checagem de Erros de Ingestão
bases_cgv <- list(DVAPSH = tiva_dvash, FVASH = tiva_fvash, INTDVAPSH = tiva_intdvapsh)
vazias_cgv <- names(bases_cgv)[sapply(bases_cgv, function(x) nrow(x) == 0)]

if (length(vazias_cgv) > 0) {
  cat("\n===================================================\n")
  cat("⚠️ ATENÇÃO: As seguintes bases essenciais de CGV ainda estão VAZIAS:\n")
  cat("   ", paste(vazias_cgv, collapse = ", "), "\n")
  cat("===================================================\n")
} else {
  cat("\n✅ Todas as bases de CGV foram carregadas na memória com sucesso!\n")
}

# 5. Consolidação e Salvamento
fontes_inspecionadas <- list(
  meta = list(
    amostra         = amostra_30, 
    ano_inicio      = ano_inicio, 
    ano_fim         = ano_fim, 
    t_anos          = t_anos,
    wdi_indicators  = c(
      "gdp_pc_ppp"      = "NY.GDP.PCAP.PP.KD",
      "terms_of_trade" = "TT.PRI.MRCH.XD.WD"
    )
  ),
  outliers_primarios = c("ZAF", "BRN", "SAU", "KAZ"),
  dados_brutos = list(
    wdi             = wdi_raw, 
    wgi             = wgi_raw, 
    vdem            = vdem_raw, 
    atlas           = atlas_raw, 
    pwt             = pwt_raw,
    tiva_dvash      = tiva_dvash, 
    tiva_fvash      = tiva_fvash, 
    tiva_intdvapsh  = tiva_intdvapsh,
    wbgvc_countries = wbgvc_countries, 
    wbgvc_world     = wbgvc_world, 
    wbgvc_wits      = wbgvc_wits,
    unesco          = unesco_raw, 
    qog             = qog_raw, 
    bti             = bti_raw
  )
)

write_rds(fontes_inspecionadas, "dados_brutos/fontes_inspecionadas.rds")
close(log_con)

cat("\n===================================================\n")
cat("✅ Etapa 01 concluída! 'fontes_inspecionadas.rds' gerado com sucesso.\n")
cat("📄 Log salvo em: 'outputs/logs/etapa01_ingestao.log'\n")
cat("===================================================\n\n")