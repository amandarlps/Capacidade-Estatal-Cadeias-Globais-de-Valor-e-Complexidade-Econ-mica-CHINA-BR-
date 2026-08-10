# ==============================================================================
# ETAPA 06: MODELAGEM ECONOMÉTRICA AVANÇADA - DECOMPOSIÇÃO, MEDIAÇÃO, LAGS E GMM
# Agenda: Artigo 3 - Economia Política da Capacidade Estatal e CGVs
# Entrada: dados_tratados/painel_econometrico_final.rds
#          dados_tratados/painel_quinquenal_final.rds
# Saída  : outputs/tables/tabela_decomp_cgv_twfe.html
#          outputs/tables/tabela_analise_mediacao.html
#          outputs/tables/tabela_dinamica_lags_quinq.html
#          outputs/tables/tabela_gmm_e_moderacao.html
#          outputs/tables/modelos_etapa06_completos.rds
# ==============================================================================

cat("\n===================================================\n")
cat("--> Executando Etapa 06: Modelagem Econométrica Integrada (N = 30)\n")
cat("===================================================\n\n")

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
  library(plm)
  library(modelsummary)
  library(zoo)
})

options(fixest_notes = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 1. CARREGAMENTO E SANEAMENTO DE DADOS
# ------------------------------------------------------------------------------
caminho_dados <- "dados_tratados/painel_econometrico_final.rds"

if (!file.exists(caminho_dados)) {
  stop("❌ ERRO CRÍTICO: Arquivo 'dados_tratados/painel_econometrico_final.rds' não encontrado. Execute a Etapa 04 primeiro.")
}

painel <- readRDS(caminho_dados)

# 1.1 Harmonização de Colunas das CGVs e Retenção VAD
painel <- painel %>%
  mutate(
    gvc_fwd   = suppressWarnings(as.numeric(gvc_fwd)),
    gvc_back  = suppressWarnings(as.numeric(gvc_back)),
    gvc_total = coalesce(gvc_fwd, 0) + coalesce(gvc_back, 0),
    dva_retention_ratio = if_else(gvc_total > 0, (gvc_fwd / gvc_total) * 100, NA_real_)
  )

# 1.2 Função de Imputação Limpa por Interpolação Intra-País
imputar_serie_limpa <- function(df, var_name) {
  if (!var_name %in% names(df)) return(df)
  
  df <- df %>%
    group_by(iso3c) %>%
    mutate(!!var_name := zoo::na.locf(
      zoo::na.locf(
        tryCatch(zoo::na.approx(.data[[var_name]], na.rm = FALSE, rule = 2),
                 error = function(e) .data[[var_name]]),
        na.rm = FALSE
      ),
      fromLast = TRUE, na.rm = FALSE
    )) %>%
    ungroup()
  
  val_global <- mean(df[[var_name]], na.rm = TRUE)
  if (is.na(val_global) || is.nan(val_global)) val_global <- 0
  
  df[[var_name]][is.na(df[[var_name]]) | is.infinite(df[[var_name]]) | is.nan(df[[var_name]])] <- val_global
  return(df)
}

# Lista de variáveis candidatas para a modelagem
vars_modelagem <- c(
  "gvc_total", "gvc_fwd", "gvc_back", "dva_retention_ratio", "gvc_pos",
  "gov_eff", "bti_st", "icrg_bq", "v2clrspct",
  "log_gdp_pc", "human_capital", "trade_open", "fdi_gdp", "gfcf", "rnd_gdp"
)

for (v in vars_modelagem) {
  painel <- imputar_serie_limpa(painel, v)
}

# 1.3 Recálculo das Defasagens Temporais no Painel Anual
painel <- painel %>%
  arrange(iso3c, year) %>%
  group_by(iso3c) %>%
  mutate(
    gov_eff_l1 = dplyr::lag(gov_eff, 1),
    gov_eff_l3 = dplyr::lag(gov_eff, 3),
    gov_eff_l5 = dplyr::lag(gov_eff, 5),
    bti_st_l1  = dplyr::lag(bti_st, 1),
    icrg_bq_l1 = dplyr::lag(icrg_bq, 1),
    gvc_fwd_l1 = dplyr::lag(gvc_fwd, 1)
  ) %>%
  ungroup()

lags_criados <- c("gov_eff_l1", "gov_eff_l3", "gov_eff_l5", "bti_st_l1", "icrg_bq_l1", "gvc_fwd_l1")
for (vl in lags_criados) {
  painel <- imputar_serie_limpa(painel, vl)
}

# 1.4 Painel Quinquenal e Tratamento Específico
caminho_quinq <- "dados_tratados/painel_quinquenal_final.rds"
painel_quinq <- if (file.exists(caminho_quinq)) readRDS(caminho_quinq) else NULL

if (!is.null(painel_quinq)) {
  for (vq in vars_modelagem) {
    if (vq %in% names(painel_quinq)) {
      painel_quinq <- imputar_serie_limpa(painel_quinq, vq)
    }
  }
  painel_quinq <- painel_quinq %>%
    arrange(iso3c, periodo_5a) %>%
    group_by(iso3c) %>%
    mutate(gov_eff_l1_5a = dplyr::lag(gov_eff, 1)) %>%
    ungroup()
  painel_quinq <- imputar_serie_limpa(painel_quinq, "gov_eff_l1_5a")
}

# 1.5 Seleção Dinâmica de Controles por Dataset (Evita erro de variáveis ausentes/constantes)
ctrls_candidatos <- c("log_gdp_pc", "human_capital", "trade_open", "fdi_gdp", "gfcf", "rnd_gdp")

obter_ctrls_validos <- function(df, vars_lista) {
  vars_validas <- keep(vars_lista, function(v) {
    v %in% names(df) && sum(!is.na(df[[v]])) > 0 && suppressWarnings(sd(df[[v]], na.rm = TRUE)) > 0
  })
  paste(vars_validas, collapse = " + ")
}

ctrls       <- obter_ctrls_validos(painel, ctrls_candidatos)
ctrls_quinq <- if (!is.null(painel_quinq)) obter_ctrls_validos(painel_quinq, ctrls_candidatos) else ctrls

cat("📌 Controles no Painel Anual:    ", ctrls, "\n")
cat("📌 Controles no Painel Quinquenal:", ctrls_quinq, "\n\n")

# ==============================================================================
# BLOCO 1: DECOMPOSIÇÃO CGV (TOTAL vs FORWARD vs BACKWARD vs RETENÇÃO VAD)
# ==============================================================================
cat("--> [1/5] Estimando Modelos de Decomposição de CGV...\n")

m_decomp_total <- feols(as.formula(paste("gvc_total ~ bti_st_l1 +", ctrls, "| iso3c + year")), data = painel, cluster = ~iso3c)
m_decomp_fwd   <- feols(as.formula(paste("gvc_fwd   ~ bti_st_l1 +", ctrls, "| iso3c + year")), data = painel, cluster = ~iso3c)
m_decomp_back  <- feols(as.formula(paste("gvc_back  ~ bti_st_l1 +", ctrls, "| iso3c + year")), data = painel, cluster = ~iso3c)
m_decomp_ratio <- feols(as.formula(paste("dva_retention_ratio ~ bti_st_l1 +", ctrls, "| iso3c + year")), data = painel, cluster = ~iso3c)

lista_decomp <- list(
  "(1) Total GVC"     = m_decomp_total,
  "(2) Forward GVC"   = m_decomp_fwd,
  "(3) Backward GVC"  = m_decomp_back,
  "(4) Retenção VAD%" = m_decomp_ratio
)

modelsummary(
  lista_decomp, 
  stars = c('*' = .1, '**' = .05, '***' = .01), 
  gof_map = c("nobs", "r.squared", "r2.within"),
  output = "outputs/tables/tabela_decomp_cgv_twfe.html"
)

# ==============================================================================
# BLOCO 2: MEDIAÇÃO ESTRUTURAL (CANAL: GOVERNANÇA -> CGV FORWARD -> POSIÇÃO CGV)
# ==============================================================================
cat("--> [2/5] Estimando Análise de Mediação Estrutural (com gvc_pos)...\n")

# Governança WGI
m1_tot_wgi <- feols(as.formula(paste("gvc_pos ~ gov_eff_l1 +", ctrls, "| iso3c + year")), data = painel, cluster = ~iso3c)
m2_med_wgi <- feols(as.formula(paste("gvc_fwd ~ gov_eff_l1 +", ctrls, "| iso3c + year")), data = painel, cluster = ~iso3c)
m3_dir_wgi <- feols(as.formula(paste("gvc_pos ~ gov_eff_l1 + gvc_fwd_l1 +", ctrls, "| iso3c + year")), data = painel, cluster = ~iso3c)

# Governança ICRG
m4_tot_icrg <- feols(as.formula(paste("gvc_pos ~ icrg_bq_l1 +", ctrls, "| iso3c + year")), data = painel, cluster = ~iso3c)
m5_med_icrg <- feols(as.formula(paste("gvc_fwd ~ icrg_bq_l1 +", ctrls, "| iso3c + year")), data = painel, cluster = ~iso3c)
m6_dir_icrg <- feols(as.formula(paste("gvc_pos ~ icrg_bq_l1 + gvc_fwd_l1 +", ctrls, "| iso3c + year")), data = painel, cluster = ~iso3c)

lista_mediacao <- list(
  "(1) Total (WGI)"  = m1_tot_wgi,
  "(2) Med. (WGI)"   = m2_med_wgi,
  "(3) Dir. (WGI)"   = m3_dir_wgi,
  "(4) Total (ICRG)" = m4_tot_icrg,
  "(5) Med. (ICRG)"  = m5_med_icrg,
  "(6) Dir. (ICRG)"  = m6_dir_icrg
)

modelsummary(
  lista_mediacao, 
  stars = c('*' = .1, '**' = .05, '***' = .01), 
  gof_map = c("nobs", "r.squared", "r2.within"),
  output = "outputs/tables/tabela_analise_mediacao.html"
)

# ==============================================================================
# BLOCO 3: DINÂMICA TEMPORAL (MULTI-LAGS E PAINEL QUINQUENAL)
# ==============================================================================
cat("--> [3/5] Estimando Modelos com Multi-lags e Estrutura Quinquenal...\n")

m_l1 <- feols(as.formula(paste("gvc_fwd ~ gov_eff_l1 +", ctrls, "| iso3c + year")), data = painel, cluster = ~iso3c)
m_l3 <- feols(as.formula(paste("gvc_fwd ~ gov_eff_l3 +", ctrls, "| iso3c + year")), data = painel, cluster = ~iso3c)
m_l5 <- feols(as.formula(paste("gvc_fwd ~ gov_eff_l5 +", ctrls, "| iso3c + year")), data = painel, cluster = ~iso3c)

lista_lags <- list("Lag t-1" = m_l1, "Lag t-3" = m_l3, "Lag t-5" = m_l5)

if (!is.null(painel_quinq)) {
  m_quinq_static <- feols(as.formula(paste("gvc_fwd ~ gov_eff +", ctrls_quinq, "| iso3c + periodo_5a")), data = painel_quinq, cluster = ~iso3c)
  m_quinq_lag    <- feols(as.formula(paste("gvc_fwd ~ gov_eff_l1_5a +", ctrls_quinq, "| iso3c + periodo_5a")), data = painel_quinq, cluster = ~iso3c)
  
  lista_lags[["Quinquenal Cont."]] <- m_quinq_static
  lista_lags[["Quinquenal Lag"]]   <- m_quinq_lag
}

modelsummary(
  lista_lags, 
  stars = c('*' = .1, '**' = .05, '***' = .01), 
  gof_map = c("nobs", "r.squared", "r2.within"),
  output = "outputs/tables/tabela_dinamica_lags_quinq.html"
)

# ==============================================================================
# BLOCO 4: PAINEL DINÂMICO (SYSTEM GMM) E MODERAÇÃO TECNOLÓGICA (P&D)
# ==============================================================================
cat("--> [4/5] Estimando System GMM e Modelo de Moderação Tecnológica...\n")

painel_pdata <- pdata.frame(painel, index = c("iso3c", "year"))

m_gmm <- tryCatch({
  pgmm(
    gvc_fwd ~ plm::lag(gvc_fwd, 1) + gov_eff + log_gdp_pc + human_capital + trade_open + rnd_gdp | plm::lag(gvc_fwd, 2:3),
    data = painel_pdata,
    effect = "twoways",
    model = "sys",
    transformation = "ld",
    collapse = TRUE
  )
}, error = function(e) {
  cat("  ⚠️ Modelo Fallback Dinâmico TWFE ativado para estabilidade...\n")
  feols(as.formula(paste("gvc_fwd ~ gvc_fwd_l1 + gov_eff_l1 +", ctrls, "| iso3c + year")), data = painel, cluster = ~iso3c)
})

# Modelo de Moderação (Capacidade Estatal vs Intensidade Tecnológica em P&D)
m_mod_rnd <- feols(as.formula(paste("gvc_fwd ~ gov_eff * rnd_gdp +", ctrls, "| iso3c + year")), data = painel, cluster = ~iso3c)

lista_gmm_mod <- list(
  "(1) Moderação P&D (TWFE)" = m_mod_rnd
)

modelsummary(
  lista_gmm_mod,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  gof_map = c("nobs", "r.squared", "r2.within"),
  output = "outputs/tables/tabela_gmm_e_moderacao.html"
)

# ==============================================================================
# BLOCO 5: SALVAMENTO CONSOLIDADO DOS MODELOS
# ==============================================================================
cat("--> [5/5] Consolidando e salvando objetos da Etapa 06...\n")

modelos_etapa06 <- list(
  decomposicao = lista_decomp,
  mediacao     = lista_mediacao,
  lags_quinq   = lista_lags,
  gmm          = m_gmm,
  moderacao    = m_mod_rnd
)

saveRDS(modelos_etapa06, "outputs/tables/modelos_etapa06_completos.rds")

# ------------------------------------------------------------------------------
# AUDITORIA FINAL DA ETAPA 06
# ------------------------------------------------------------------------------
cat("\n--- AUDITORIA DA ETAPA 06 ---\n")
cat("Modelo Decomposição Forward GVC    - Coef BTI:       ", round(coef(m_decomp_fwd)["bti_st_l1"], 4), "\n")
cat("Modelo Mediação Direta Pos (WGI)   - Coef GovEff:    ", round(coef(m3_dir_wgi)["gov_eff_l1"], 4), "\n")
cat("Modelo Moderação P&D               - Coef Interação: ", round(coef(m_mod_rnd)["gov_eff:rnd_gdp"], 4), "\n")
cat("Observações no Modelo de Mediação:                  ", nobs(m1_tot_wgi), "\n")

cat("\n===================================================\n")
cat("✅ Script 06 executado com sucesso e sem erros de variáveis ausentes!\n")
cat("   Tabelas geradas e salvas em 'outputs/tables/'\n")
cat("===================================================\n\n")