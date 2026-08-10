# ==============================================================================
# ETAPA 07 (INTEGRADA E COMPLETA): CHECAGENS DE ROBUSTEZ, DRISCOLL-KRAAY E PLACEBOS
# Entrada: dados_tratados/painel_econometrico_final.rds
# Saída  : outputs/tables/tabela_robustez_dk_proxies.html
#          outputs/tables/tabela_robustez_outliers_placebos.html
#          outputs/tables/modelos_robustez_dk_inter.rds
#          outputs/tables/modelos_robustez_placebos.rds
# ==============================================================================

cat("\n===================================================\n")
cat("--> Executando Etapa 07: Robustez, Driscoll-Kraay e Interações (N = 30)\n")
cat("===================================================\n\n")

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
  library(modelsummary)
  library(zoo)
})

options(fixest_notes = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 1. INGESTÃO E PREPARAÇÃO SEGURA DE DADOS
# ------------------------------------------------------------------------------
caminho_dados <- "dados_tratados/painel_econometrico_final.rds"

if (!file.exists(caminho_dados)) {
  stop("❌ ERRO CRÍTICO: Arquivo 'dados_tratados/painel_econometrico_final.rds' não encontrado. Execute as etapas anteriores primeiro.")
}

painel <- readRDS(caminho_dados)

# 1.1 Harmonização de Colunas e Construção de Dummies
if (!"gvc_fwd" %in% names(painel) && "gvc_part_total" %in% names(painel)) {
  painel$gvc_fwd <- painel$gvc_part_total
}

if (!"outlier_primario" %in% names(painel)) {
  painel$outlier_primario <- if_else(painel$iso3c %in% c("ZAF", "BRN", "SAU", "KAZ"), 1, 0)
}

painel$latam <- if_else(painel$iso3c %in% c("BRA", "MEX", "CHL", "ARG", "COL", "PER", "CRI", "URY"), 1, 0)

# 1.2 Imputação Determinística Limpa (Sem ruído aleatório)
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

vars_checar <- c(
  "gvc_fwd", "gov_eff", "v2clrspct", "icrg_bq", "bti_st",
  "log_gdp_pc", "human_capital", "trade_open", "fdi_gdp", "gfcf", "rnd_gdp"
)

for (v in vars_checar) {
  painel <- imputar_serie_limpa(painel, v)
}

# 1.3 Seleção Dinâmica de Controles Válidos
ctrls_candidatos <- c("log_gdp_pc", "human_capital", "trade_open", "fdi_gdp", "gfcf", "rnd_gdp")

obter_ctrls_validos <- function(df, vars_lista) {
  vars_validas <- keep(vars_lista, function(v) {
    v %in% names(df) && sum(!is.na(df[[v]])) > 0 && suppressWarnings(sd(df[[v]], na.rm = TRUE)) > 0
  })
  paste(vars_validas, collapse = " + ")
}

ctrls <- obter_ctrls_validos(painel, ctrls_candidatos)
cat("📌 Controles no Painel de Robustez:", ctrls, "\n\n")

# Dicionário de Rótulos Acadêmicos
renomear_vars <- c(
  "gov_eff"          = "Capacidade Estatal (WGI)",
  "v2clrspct"        = "Rigor Admin. (V-Dem)",
  "icrg_bq"          = "Qual. Burocrática (ICRG)",
  "bti_st"           = "Cap. Direção (BTI)",
  "v2clrspct:latam"  = "V-Dem x América Latina",
  "icrg_bq:latam"     = "ICRG x América Latina",
  "gov_eff_lead2"    = "Cap. Estatal (Lead t+2)",
  "gov_eff_lead3"    = "Cap. Estatal (Lead t+3)",
  "log_gdp_pc"       = "ln(PIB per capita)",
  "human_capital"    = "Capital Humano",
  "trade_open"       = "Abertura Comercial",
  "fdi_gdp"          = "FDI (% PIB)",
  "gfcf"             = "FBCF (% PIB)",
  "rnd_gdp"          = "Gastos em P&D (% PIB)"
)

# ==============================================================================
# BLOCO 1: PROXIES ALTERNATIVAS COM ERROS-PADRÃO DRISCOLL-KRAAY (DK)
# ==============================================================================
cat("--> [1/3] Estimando Proxies com Erros-Padrão Driscoll-Kraay...\n")

f_wgi  <- as.formula(paste("gvc_fwd ~ gov_eff +", ctrls, "| iso3c + year"))
f_vdem <- as.formula(paste("gvc_fwd ~ v2clrspct +", ctrls, "| iso3c + year"))
f_icrg <- as.formula(paste("gvc_fwd ~ icrg_bq +", ctrls, "| iso3c + year"))
f_bti  <- as.formula(paste("gvc_fwd ~ bti_st +", ctrls, "| iso3c + year"))

mod_wgi_dk  <- feols(f_wgi,  data = painel, panel.id = ~iso3c + year, vcov = "DK")
mod_vdem_dk <- feols(f_vdem, data = painel, panel.id = ~iso3c + year, vcov = "DK")
mod_icrg_dk <- feols(f_icrg, data = painel, panel.id = ~iso3c + year, vcov = "DK")
mod_bti_dk  <- feols(f_bti,  data = painel, panel.id = ~iso3c + year, vcov = "DK")

# ==============================================================================
# BLOCO 2: INTERAÇÕES REGIONAIS SOB ESTIMAÇÃO TWFE COMPLETA
# ==============================================================================
cat("--> [2/3] Estimando Interações Regionais (LATAM) em Painel TWFE...\n")

# Dummies invariantes no tempo (latam) são absorvidas por iso3c; a interação é identificada
f_vdem_inter <- as.formula(paste("gvc_fwd ~ v2clrspct + v2clrspct:latam +", ctrls, "| iso3c + year"))
f_icrg_inter <- as.formula(paste("gvc_fwd ~ icrg_bq + icrg_bq:latam +", ctrls, "| iso3c + year"))

mod_vdem_inter <- feols(f_vdem_inter, data = painel, cluster = ~iso3c)
mod_icrg_inter <- feols(f_icrg_inter, data = painel, cluster = ~iso3c)

lista_dk_inter <- list(
  "(1) WGI (DK)"      = mod_wgi_dk,
  "(2) V-Dem (DK)"    = mod_vdem_dk,
  "(3) ICRG (DK)"     = mod_icrg_dk,
  "(4) BTI (DK)"      = mod_bti_dk,
  "(5) VDem x LATAM"  = mod_vdem_inter,
  "(6) ICRG x LATAM"  = mod_icrg_inter
)

modelsummary(
  lista_dk_inter, 
  coef_map = renomear_vars,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  gof_map = c("nobs", "r.squared", "r2.within"),
  output = "outputs/tables/tabela_robustez_dk_proxies.html"
)

# ==============================================================================
# BLOCO 3: SENSIBILIDADE A OUTLIERS E TESTES PLACEBO TEMPORAIS
# ==============================================================================
cat("--> [3/3] Estimando Amostra sem Outliers e Placebos Temporais...\n")

# 3.1 Modelagem Sem Outliers Primários (ZAF, BRN, SAU, KAZ)
painel_no_out <- painel %>% filter(outlier_primario == 0)
mod_no_outliers <- feols(f_wgi, data = painel_no_out, panel.id = ~iso3c + year, vcov = "DK")

# 3.2 Testes Placebo Temporais (Leads t+2, t+3)
painel_placebo <- painel %>%
  arrange(iso3c, year) %>%
  group_by(iso3c) %>%
  mutate(
    gov_eff_lead2 = dplyr::lead(gov_eff, 2),
    gov_eff_lead3 = dplyr::lead(gov_eff, 3)
  ) %>%
  ungroup()

f_placebo_f2 <- as.formula(paste("gvc_fwd ~ gov_eff_lead2 +", ctrls, "| iso3c + year"))
f_placebo_f3 <- as.formula(paste("gvc_fwd ~ gov_eff_lead3 +", ctrls, "| iso3c + year"))

mod_placebo_f2 <- feols(f_placebo_f2, data = painel_placebo, cluster = ~iso3c)
mod_placebo_f3 <- feols(f_placebo_f3, data = painel_placebo, cluster = ~iso3c)

lista_sensibilidade <- list(
  "Sem Outliers (DK)"  = mod_no_outliers,
  "Placebo (Lead t+2)" = mod_placebo_f2,
  "Placebo (Lead t+3)" = mod_placebo_f3
)

modelsummary(
  lista_sensibilidade,
  coef_map = renomear_vars,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  gof_map = c("nobs", "r.squared", "r2.within"),
  output = "outputs/tables/tabela_robustez_outliers_placebos.html"
)

# Consolidação dos Objetos RDS
saveRDS(lista_dk_inter, "outputs/tables/modelos_robustez_dk_inter.rds")
saveRDS(lista_sensibilidade, "outputs/tables/modelos_robustez_placebos.rds")

cat("\n===================================================\n")
cat("✅ Script 07 revisado e executado com rigor acadêmico!\n")
cat("   Tabelas salvas em 'outputs/tables/'\n")
cat("===================================================\n\n")