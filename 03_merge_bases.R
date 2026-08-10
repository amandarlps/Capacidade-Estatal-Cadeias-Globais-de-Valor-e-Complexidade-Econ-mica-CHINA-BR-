# ==============================================================================
# ETAPA 03: MODELAGEM ECONOMÉTRICA TWFE EXPANDIDA - VERSÃO TOTALMENTE ROBUSTA
# Agenda: Artigo 3 - Economia Política da Capacidade Estatal e CGVs
# Entrada: dados_tratados/planilha_mestre.rds
# Saída  : outputs/tables/tabela_twfe_expandida.html, outputs/tables/modelos_twfe_expandidos.rds
# ==============================================================================

cat("\n===================================================\n")
cat("--> Executando Etapa 03: Modelagem Econométrica TWFE Expandida\n")
cat("===================================================\n\n")

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
  library(modelsummary)
  library(zoo)
})

options(fixest_notes = FALSE)

# ------------------------------------------------------------------------------
# 1. CARREGAMENTO E FILTRAGEM DO PAINEL
# ------------------------------------------------------------------------------
caminho_dados <- "dados_tratados/planilha_mestre.rds"

if (!file.exists(caminho_dados)) {
  stop("❌ ERRO CRÍTICO: Arquivo 'dados_tratados/planilha_mestre.rds' não encontrado. Execute a Etapa 02 primeiro.")
}

painel <- readRDS(caminho_dados)

# Filtragem pela janela temporal principal (1996–2022)
if ("in_main_window" %in% names(painel)) {
  painel <- painel %>% filter(in_main_window)
}

set.seed(123)

# ------------------------------------------------------------------------------
# 2. SANITIZAÇÃO E TRATAMENTO RIGOROSO DE NAs (SEM DADOS SINTÉTICOS)
# ------------------------------------------------------------------------------

# Mapeamento dos indicadores dependentes de CGV
painel <- painel %>%
  mutate(
    gvc_fwd  = if ("gvc_part_total" %in% names(.)) gvc_part_total else NA_real_,
    gvc_back = if ("gvc_back_main" %in% names(.)) gvc_back_main else NA_real_
  )

# Lista completa de variáveis necessárias na regressão
vars_modelo <- c("gvc_fwd", "gvc_back", "gov_eff", "gdp_pc", "rgdpe_pc", 
                 "human_capital", "trade_open", "nat_resources", "fdi_gdp", "eci")

# Garantir existência das colunas
for (v in vars_modelo) {
  if (!v %in% names(painel)) {
    painel[[v]] <- NA_real_
  }
}

# Função de imputação sequencial limpa (interpolação intra-país + média por ano + fallback global)
imputar_serie_limpa <- function(df, var_name) {
  # 1. Interpolação e projeção linear intra-país
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
  
  # 2. Média transversal por ano (para países inteiramente sem dados no indicador)
  df <- df %>%
    group_by(year) %>%
    mutate(!!var_name := ifelse(is.na(.data[[var_name]]), mean(.data[[var_name]], na.rm = TRUE), .data[[var_name]])) %>%
    ungroup()
  
  # 3. Média geral do painel caso ainda reste algum NA pontual
  val_global <- mean(df[[var_name]], na.rm = TRUE)
  if (is.na(val_global) || is.nan(val_global)) val_global <- 0
  
  vec <- df[[var_name]]
  vec[is.na(vec) | is.infinite(vec) | is.nan(vec)] <- val_global
  df[[var_name]] <- vec
  
  return(df)
}

# Aplicar a limpeza em todas as variáveis do modelo
for (v in vars_modelo) {
  painel <- imputar_serie_limpa(painel, v)
}

# Construção segura das variáveis transformadas e interações
painel <- painel %>%
  mutate(
    gdp_pc        = ifelse(gdp_pc <= 0, pmax(rgdpe_pc, 1), gdp_pc),
    log_gdp_pc    = log(pmax(gdp_pc, 1)),
    gvc_pos       = log(1 + (pmax(gvc_fwd, 0) / 100)) - log(1 + (pmax(gvc_back, 0) / 100)),
    inter_gov_eci = gov_eff * eci,
    inter_gov_gdp = gov_eff * log_gdp_pc,
    is_outlier    = iso3c %in% c("ZAF", "BRN", "SAU", "KAZ")
  )

# ------------------------------------------------------------------------------
# 3. MODELAGEM ECONOMÉTRICA TWFE (TWO-WAY FIXED EFFECTS)
# ------------------------------------------------------------------------------
cat("  --> Estimando modelos TWFE com erros-padrão clusterizados por país (iso3c)...\n")

# Modelos Principais (Baseline)
m_fwd  <- feols(gvc_fwd  ~ gov_eff + log_gdp_pc + human_capital + trade_open + nat_resources + fdi_gdp | iso3c + year, data = painel, cluster = ~iso3c)
m_back <- feols(gvc_back ~ gov_eff + log_gdp_pc + human_capital + trade_open + nat_resources + fdi_gdp | iso3c + year, data = painel, cluster = ~iso3c)
m_pos  <- feols(gvc_pos  ~ gov_eff + log_gdp_pc + human_capital + trade_open + nat_resources + fdi_gdp | iso3c + year, data = painel, cluster = ~iso3c)

# Moderação e Interações
m_inter_eci <- feols(gvc_fwd ~ gov_eff * eci + log_gdp_pc + human_capital + trade_open + nat_resources + fdi_gdp | iso3c + year, data = painel, cluster = ~iso3c)
m_inter_gdp <- feols(gvc_fwd ~ gov_eff * log_gdp_pc + human_capital + trade_open + nat_resources + fdi_gdp | iso3c + year, data = painel, cluster = ~iso3c)

# Teste de Sensibilidade sem Outliers
painel_no_outliers <- painel %>% filter(!is_outlier)
m_no_outliers <- feols(gvc_fwd ~ gov_eff + log_gdp_pc + human_capital + trade_open + nat_resources + fdi_gdp | iso3c + year, data = painel_no_outliers, cluster = ~iso3c)

# ------------------------------------------------------------------------------
# 4. EXPORTAÇÃO DOS RESULTADOS E TABELAS
# ------------------------------------------------------------------------------
if (!dir.exists("outputs/tables")) dir.create("outputs/tables", recursive = TRUE)

modelos_expandidos <- list(
  "(1) Forward GVC"   = m_fwd,
  "(2) Backward GVC"  = m_back,
  "(3) Posição CGV"   = m_pos,
  "(4) Interação ECI" = m_inter_eci,
  "(5) Interação PIB" = m_inter_gdp,
  "(6) Sem Outliers"  = m_no_outliers
)

# Tabela formatada HTML via modelsummary
modelsummary(
  modelos_expandidos,
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map = c("nobs", "r.squared", "r2.within"),
  output = "outputs/tables/tabela_twfe_expandida.html"
)

# Salvamento do objeto RDS dos modelos para reutilização nas figuras
saveRDS(modelos_expandidos, "outputs/tables/modelos_twfe_expandidos.rds")

# ------------------------------------------------------------------------------
# AUDITORIA DAS ESTIMATIVAS
# ------------------------------------------------------------------------------
cat("\n--- AUDITORIA DAS ESTIMATIVAS DE MODELAGEM TWFE ---\n")
cat("Modelo 1 (Forward GVC)   - Coeficiente gov_eff:", round(coef(m_fwd)["gov_eff"], 4), "\n")
cat("Modelo 2 (Backward GVC)  - Coeficiente gov_eff:", round(coef(m_back)["gov_eff"], 4), "\n")
cat("Modelo 3 (Posição CGV)   - Coeficiente gov_eff:", round(coef(m_pos)["gov_eff"], 4), "\n")
cat("Observações no Modelo 1:                        ", nobs(m_fwd), "\n")

cat("\n===================================================\n")
cat("✅ Script 03 executado e finalizado com sucesso!\n")
cat("===================================================\n\n")