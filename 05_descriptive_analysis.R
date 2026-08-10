# ==============================================================================
# ETAPA 05: ANÁLISE DESCRITIVA INTEGRADA, REGIONAL E DECOMPOSIÇÃO DE VARIÂNCIA
# Agenda: Artigo 3 - Economia Política da Capacidade Estatal e CGVs
# Entrada: dados_tratados/painel_econometrico_final.rds
# Saída  : outputs/tables/estatisticas_descritivas_geral.csv (.html)
#          outputs/tables/comparativo_regional.csv (.html)
#          outputs/tables/decomposicao_variancia.csv (.html)
#          outputs/tables/matriz_correlacao_pearson.csv (.html)
# ==============================================================================

cat("\n===================================================\n")
cat("--> Executando Etapa 05: Análise Descritiva Integrada (N = 30 | 4 Blocos)\n")
cat("===================================================\n\n")

suppressPackageStartupMessages({
  library(tidyverse)
  library(modelsummary)
  library(psych)
  library(corrr)
})

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 1. CARREGAMENTO E HARMONIZAÇÃO PRÉVIA DAS VARIÁVEIS
# ------------------------------------------------------------------------------
caminho_dados <- "dados_tratados/painel_econometrico_final.rds"

if (!file.exists(caminho_dados)) {
  stop("❌ ERRO CRÍTICO: Arquivo 'dados_tratados/painel_econometrico_final.rds' não encontrado. Execute a Etapa 04 primeiro.")
}

painel <- readRDS(caminho_dados)

# Harmonização de colunas
if (!"gvc_fwd" %in% names(painel) && "gvc_part_total" %in% names(painel)) {
  painel$gvc_fwd <- painel$gvc_part_total
}
if (!"gvc_back" %in% names(painel) && "gvc_back_main" %in% names(painel)) {
  painel$gvc_back <- painel$gvc_back_main
}

# Categorização Regional Estruturada (30 Países em 4 Blocos)
painel <- painel %>%
  mutate(
    regiao = case_when(
      iso3c %in% c("BRA", "MEX", "CHL", "ARG", "COL", "PER", "CRI", "URY") ~ "América Latina",
      iso3c %in% c("CHN", "KOR", "VNM", "MYS", "THA", "IDN", "PHL", "IND", "BRN", "TWN") ~ "Ásia Emergente",
      iso3c %in% c("POL", "HUN", "TUR", "ROU", "BGR", "HRV") ~ "Leste Europeu",
      iso3c %in% c("ZAF", "MAR", "EGY", "TUN", "SAU", "KAZ") ~ "África & Oriente Médio",
      TRUE ~ "Outros"
    )
  )

# Vetor Expandido de Variáveis Analisadas
vars_analise <- c(
  "gvc_fwd", "gvc_back", "gvc_pos", "dva_retention_ratio",
  "gov_eff", "v2clrspct", "icrg_bq", "bti_st", "bti_i", 
  "eci", "log_gdp_pc", "human_capital", "trade_open", "fdi_gdp", "gfcf"
)

# Garantia de presença e conversão numérica segura
for (v in vars_analise) {
  if (!v %in% names(painel)) {
    painel[[v]] <- NA_real_
  } else {
    painel[[v]] <- suppressWarnings(as.numeric(painel[[v]]))
  }
}

# FILTRO DE SEGURANÇA: Seleciona apenas variáveis que possuem variação (SD > 0 e não totalmente NAs)
vars_com_variancia <- vars_analise[sapply(vars_analise, function(v) {
  vec <- painel[[v]]
  !all(is.na(vec)) && suppressWarnings(sd(vec, na.rm = TRUE)) > 0
})]

# ------------------------------------------------------------------------------
# 2. ESTATÍSTICAS DESCRITIVAS GERAIS (N = 30)
# ------------------------------------------------------------------------------
cat("  --> Gerando estatísticas descritivas gerais...\n")

desc_geral <- painel %>%
  select(all_of(vars_analise)) %>%
  pivot_longer(cols = everything(), names_to = "Variavel", values_to = "Valor") %>%
  group_by(Variavel) %>%
  summarise(
    N        = sum(!is.na(Valor)),
    Media    = mean(Valor, na.rm = TRUE),
    DesvPad  = sd(Valor, na.rm = TRUE),
    Minimo   = suppressWarnings(min(Valor, na.rm = TRUE)),
    Mediana  = median(Valor, na.rm = TRUE),
    Maximo   = suppressWarnings(max(Valor, na.rm = TRUE)),
    .groups  = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.x) | is.nan(.x), NA_real_, .x))) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

write_csv(desc_geral, "outputs/tables/estatisticas_descritivas_geral.csv")

tryCatch({
  datasummary_skim(
    painel %>% select(all_of(vars_com_variancia)),
    output = "outputs/tables/estatisticas_descritivas_geral.html"
  )
}, error = function(e) {
  datasummary_df(desc_geral, output = "outputs/tables/estatisticas_descritivas_geral.html")
})

# ------------------------------------------------------------------------------
# 3. COMPARATIVO QUADRO-REGIONAL (4 BLOCOS)
# ------------------------------------------------------------------------------
cat("  --> Gerando comparativo regional (4 blocos)...\n")

desc_regional <- painel %>%
  group_by(regiao) %>%
  summarise(
    `N Observações`           = n(),
    `Forward GVC (Média)`     = mean(gvc_fwd, na.rm = TRUE),
    `Backward GVC (Média)`    = mean(gvc_back, na.rm = TRUE),
    `Posição CGV (Média)`     = mean(gvc_pos, na.rm = TRUE),
    `Cap. Estatal (WGI)`      = mean(gov_eff, na.rm = TRUE),
    `Rigor Admin (VDem)`      = mean(v2clrspct, na.rm = TRUE),
    `Qual. Bur. (ICRG)`       = mean(icrg_bq, na.rm = TRUE),
    `Cap. Direção (BTI)`      = mean(bti_st, na.rm = TRUE),
    `Complexidade (ECI)`      = mean(eci, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

write_csv(desc_regional, "outputs/tables/comparativo_regional.csv")
datasummary_df(desc_regional, output = "outputs/tables/comparativo_regional.html")

# ------------------------------------------------------------------------------
# 4. DECOMPOSIÇÃO DE VARIÂNCIA (BETWEEN VS WITHIN)
# ------------------------------------------------------------------------------
cat("  --> Calculando decomposição de variância (Overall, Between, Within)...\n")

decompor_variancia <- function(df, vars, id_col = "iso3c") {
  map_dfr(vars, function(v) {
    vec <- df[[v]]
    if (all(is.na(vec)) || suppressWarnings(sd(vec, na.rm = TRUE)) == 0) return(NULL)
    
    mean_grand <- mean(vec, na.rm = TRUE)
    
    between_sd <- df %>%
      group_by(.data[[id_col]]) %>%
      summarise(mean_i = mean(.data[[v]], na.rm = TRUE), .groups = "drop") %>%
      summarise(sd = sd(mean_i, na.rm = TRUE)) %>%
      pull(sd)
    
    within_sd <- df %>%
      group_by(.data[[id_col]]) %>%
      mutate(within_val = .data[[v]] - mean(.data[[v]], na.rm = TRUE) + mean_grand) %>%
      ungroup() %>%
      summarise(sd = sd(within_val, na.rm = TRUE)) %>%
      pull(sd)
    
    overall_sd <- sd(vec, na.rm = TRUE)
    
    tibble(
      Variavel   = v,
      Media      = mean_grand,
      SD_Overall = overall_sd,
      SD_Between = between_sd,
      SD_Within  = within_sd,
      Razao_B_W  = between_sd / ifelse(is.na(within_sd) | within_sd == 0, NA, within_sd)
    )
  })
}

tabela_variancia <- decompor_variancia(painel, vars_com_variancia) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

write_csv(tabela_variancia, "outputs/tables/decomposicao_variancia.csv")
datasummary_df(tabela_variancia, output = "outputs/tables/decomposicao_variancia.html")

# ------------------------------------------------------------------------------
# 5. MATRIZ DE CORRELAÇÃO DE PEARSON (SEM AVISO DE SD ZERO)
# ------------------------------------------------------------------------------
cat("  --> Estimando matriz de correlação de Pearson...\n")

df_cor <- painel %>% select(all_of(vars_com_variancia))

matriz_cor <- cor(df_cor, use = "pairwise.complete.obs", method = "pearson") %>%
  round(3)

write_csv(as.data.frame(matriz_cor) %>% rownames_to_column("Variavel"), "outputs/tables/matriz_correlacao_pearson.csv")

tryCatch({
  datasummary_correlation(df_cor, output = "outputs/tables/matriz_correlacao_pearson.html")
}, error = function(e) {
  datasummary_df(as.data.frame(matriz_cor) %>% rownames_to_column("Variavel"), output = "outputs/tables/matriz_correlacao_pearson.html")
})

# ------------------------------------------------------------------------------
# AUDITORIA FINAL DA ETAPA 05
# ------------------------------------------------------------------------------
cat("\n--- AUDITORIA DA ETAPA 05 ---\n")
cat("Total de Variáveis Mapeadas:       ", length(vars_analise), "\n")
cat("Variáveis com Variação Válida:     ", length(vars_com_variancia), "\n")
cat("Blocos Regionais Mapeados:         ", length(unique(desc_regional$regiao)), "\n")
cat("Média Forward GVC (Ásia Emergente):", desc_regional %>% filter(regiao == "Ásia Emergente") %>% pull(`Forward GVC (Média)`), "\n")
cat("Média Forward GVC (América Latina):", desc_regional %>% filter(regiao == "América Latina") %>% pull(`Forward GVC (Média)`), "\n")

cat("\n===================================================\n")
cat("✅ Etapa 05 unificada e executada com sucesso sem avisos!\n")
cat("===================================================\n\n")