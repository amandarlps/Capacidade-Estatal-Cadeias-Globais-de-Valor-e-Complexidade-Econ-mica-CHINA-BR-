# ==============================================================================
# SCRIPT 04 REVISADO: ENGENHARIA DE RECURSOS, LAGS EXPANDIDOS E PAINEL QUINQUENAL
# Agenda: Artigo 3 - Economia Política da Capacidade Estatal e CGVs
# Entrada: dados_tratados/planilha_mestre.rds
# Saída  : dados_tratados/painel_econometrico_final.rds (.csv),
#          dados_tratados/painel_quinquenal_final.rds (.csv)
# ==============================================================================

cat("\n===================================================\n")
cat("--> Executando Etapa 04: Engenharia de Recursos e Painel Quinquenal\n")
cat("===================================================\n\n")

suppressPackageStartupMessages({
  library(tidyverse)
  library(zoo)
})

# ------------------------------------------------------------------------------
# 1. CARREGAMENTO E CHECAGEM DE VARIÁVEIS
# ------------------------------------------------------------------------------
caminho_dados <- "dados_tratados/planilha_mestre.rds"

if (!file.exists(caminho_dados)) {
  stop("❌ ERRO CRÍTICO: Arquivo 'dados_tratados/planilha_mestre.rds' não encontrado. Execute os scripts anteriores primeiro.")
}

painel <- readRDS(caminho_dados)

# Trava de Segurança: Inicialização de variáveis ausentes
vars_necessarias <- c(
  "bti_st", "bti_i", "bti_p", "bti_l", "gdp_pc", "rgdpe_pc", "gvc_part_total", "gvc_back_main",
  "gov_eff", "eci", "v2clrspct", "icrg_bq", "human_capital", "trade_open", 
  "terms_of_trade", "fdi_gdp", "gfcf", "nat_resources"
)

for (v in vars_necessarias) {
  if (!v %in% names(painel)) {
    painel[[v]] <- NA_real_
  }
}

mapa_paises <- c(
  "ARG" = "Argentina", "BGR" = "Bulgária", "BRA" = "Brasil", "BRN" = "Brunei",
  "CHL" = "Chile", "CHN" = "China", "COL" = "Colômbia", "CRI" = "Costa Rica",
  "EGY" = "Egito", "HRV" = "Croácia", "HUN" = "Hungria", "IDN" = "Indonésia",
  "IND" = "Índia", "KAZ" = "Cazaquistão", "KOR" = "Coreia do Sul", "MAR" = "Marrocos",
  "MEX" = "México", "MYS" = "Malásia", "PER" = "Peru", "PHL" = "Filipinas",
  "POL" = "Polônia", "ROU" = "Romênia", "SAU" = "Arábia Saudita", "THA" = "Tailândia",
  "TUN" = "Tunisia", "TUR" = "Turquia", "TWN" = "Taiwan", "URY" = "Uruguai",
  "VNM" = "Vietnã", "ZAF" = "África do Sul"
)

if ("in_main_window" %in% names(painel)) {
  painel <- painel %>% filter(in_main_window)
}

# ------------------------------------------------------------------------------
# 2. ENGENHARIA DE RECURSOS E DEFLAGS NO PAINEL ANUAL
# ------------------------------------------------------------------------------
painel_final <- painel %>%
  mutate(country_name = coalesce(mapa_paises[iso3c], iso3c)) %>%
  arrange(iso3c, year) %>%
  group_by(iso3c) %>%
  fill(any_of(c("bti_st", "bti_i", "bti_p", "bti_l")), .direction = "downup") %>%
  mutate(
    # GDP per capita e Logaritmo
    gdp_pc_clean        = ifelse(is.na(gdp_pc) | gdp_pc <= 0, rgdpe_pc, gdp_pc),
    log_gdp_pc          = log(pmax(suppressWarnings(as.numeric(gdp_pc_clean)), 1, na.rm = TRUE)),
    
    # Decomposição e Posição em CGVs
    gvc_fwd             = suppressWarnings(as.numeric(gvc_part_total)),
    gvc_back            = suppressWarnings(as.numeric(gvc_back_main)),
    gvc_total           = coalesce(gvc_fwd, 0) + coalesce(gvc_back, 0),
    dva_retention_ratio = if_else(gvc_total > 0, (gvc_fwd / gvc_total) * 100, NA_real_),
    gvc_pos             = log(1 + (pmax(coalesce(gvc_fwd, 0), 0) / 100)) - log(1 + (pmax(coalesce(gvc_back, 0), 0) / 100)),
    
    # Interações
    inter_gov_eci       = suppressWarnings(as.numeric(gov_eff)) * suppressWarnings(as.numeric(eci)),
    inter_gov_gdp       = suppressWarnings(as.numeric(gov_eff)) * log_gdp_pc,
    
    # Flag de Outliers Primários
    outlier_primario    = if_else(iso3c %in% c("ZAF", "BRN", "SAU", "KAZ"), 1, 0),
    
    # DEFASAGENS TEMPORAIS AMPLIADAS (t-1, t-3, t-5)
    gov_eff_l1          = dplyr::lag(gov_eff, 1),
    gov_eff_l3          = dplyr::lag(gov_eff, 3),
    gov_eff_l5          = dplyr::lag(gov_eff, 5),
    
    bti_st_l1           = dplyr::lag(bti_st, 1),
    bti_st_l3           = dplyr::lag(bti_st, 3),
    bti_st_l5           = dplyr::lag(bti_st, 5),
    
    v2clrspct_l1        = dplyr::lag(v2clrspct, 1),
    v2clrspct_l3        = dplyr::lag(v2clrspct, 3),
    v2clrspct_l5        = dplyr::lag(v2clrspct, 5),
    
    gvc_fwd_l1          = dplyr::lag(gvc_fwd, 1),
    gvc_back_l1         = dplyr::lag(gvc_back, 1),
    eci_l1              = dplyr::lag(eci, 1)
  ) %>%
  ungroup()

# Função auxiliar para sanitizar NaNs/Infs pós-transformação
sanitizar_numericos <- function(df) {
  df %>% mutate(across(where(is.numeric), ~ ifelse(is.nan(.x) | is.infinite(.x), NA_real_, .x)))
}

painel_final <- sanitizar_numericos(painel_final)

# ------------------------------------------------------------------------------
# 3. CONVERSÃO PARA PAINEL EM MÉDIAS QUINQUENAIS (BLOCOS DE 5 ANOS)
# ------------------------------------------------------------------------------
vars_para_agregar <- c(
  "gvc_fwd", "gvc_back", "gvc_total", "gvc_pos", "dva_retention_ratio",
  "gov_eff", "bti_st", "v2clrspct", "icrg_bq", "eci", "log_gdp_pc",
  "human_capital", "trade_open", "terms_of_trade", "fdi_gdp", "gfcf", "nat_resources"
)

painel_quinquenal <- painel_final %>%
  mutate(periodo_5a = floor((year - 1996) / 5) * 5 + 1996) %>%
  group_by(iso3c, country_name, periodo_5a) %>%
  summarise(
    across(
      any_of(vars_para_agregar),
      ~ mean(suppressWarnings(as.numeric(.x)), na.rm = TRUE)
    ),
    outlier_primario = max(outlier_primario, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  sanitizar_numericos() %>%
  arrange(iso3c, periodo_5a) %>%
  group_by(iso3c) %>%
  mutate(
    gov_eff_l1_5a = dplyr::lag(gov_eff, 1),
    bti_st_l1_5a  = dplyr::lag(bti_st, 1)
  ) %>%
  ungroup()

# ------------------------------------------------------------------------------
# 4. SALVAMENTO DOS OBJETOS FINAIS
# ------------------------------------------------------------------------------
dir.create("dados_tratados", recursive = TRUE, showWarnings = FALSE)

write_rds(painel_final, "dados_tratados/painel_econometrico_final.rds")
write_csv(painel_final, "dados_tratados/painel_econometrico_final.csv")

write_rds(painel_quinquenal, "dados_tratados/painel_quinquenal_final.rds")
write_csv(painel_quinquenal, "dados_tratados/painel_quinquenal_final.csv")

# ------------------------------------------------------------------------------
# AUDITORIA FINAL DA ETAPA 04
# ------------------------------------------------------------------------------
cat("\n--- AUDITORIA DOS PAINÉIS GERADOS ---\n")
cat("Painel Anual      - Observações:", nrow(painel_final), " | Países:", length(unique(painel_final$iso3c)), "\n")
cat("Painel Quinquenal - Observações:", nrow(painel_quinquenal), " | Bloco de Anos:", length(unique(painel_quinquenal$periodo_5a)), "\n")
cat("Média gvc_pos (Anual):          ", round(mean(painel_final$gvc_pos, na.rm = TRUE), 4), "\n")
cat("Média gov_eff_l1 (Defasado t-1):", round(mean(painel_final$gov_eff_l1, na.rm = TRUE), 4), "\n")

cat("\n===================================================\n")
cat("✅ Etapa 04 concluída com sucesso! Painel Anual e Quinquenal gravados.\n")
cat("===================================================\n\n")