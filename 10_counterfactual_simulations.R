# ==============================================================================
# ETAPA 10: SIMULAÇÃO CONTRAFACTUAL REGIONAL (LATAM VS FRONTEIRA ASIÁTICA)
# Premissa: Ganho em Retenção de VAD em CGV ao Alinhar Instituições à Fronteira
#           do Leste Asiático (Fecho do Hiato até o 75º Percentil)
# Entrada : dados_tratados/painel_econometrico_final.rds
#           outputs/tables/tabela_modelos_globais.csv (se disponível)
# Saída   : outputs/tables/tabela_simulacao_contrafactual.csv
#           outputs/figures/figura_contrafactual_latam_corrigida.png
#           outputs/figures/figura_contrafactual_latam_bti_principal.png
#           outputs/figures/figura_contrafactual_latam_wgi_baseline.png
#
# Ajustes de Auditoria:
# 1. Remoção completa dos parâmetros hard-coded (fallbacks de 0.15, 0.12, 0.10).
# 2. Importação direta dos coeficientes validados nas Etapas 08/09.
# 3. Alinhamento conceitual e documental rigoroso: Simulação do Fecho do Hiato 
#    Institucional (P75 do Leste Asiático) em substituição à nomenclatura de choque +1 SD.
# ==============================================================================

cat("\n===================================================\n")
cat("--> Executando Etapa 10: Simulação Contrafactual Regional Ajustada\n")
cat("===================================================\n\n")

# ------------------------------------------------------------------------------
# 0. SETUP DE PACOTES, DIRETÓRIOS E TEMA VISUAL
# ------------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, fixest, scales, ggrepel, zoo)

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

tema_artigo <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = 12.5, color = "#1a1a1a", margin = margin(b = 4)),
      plot.subtitle = element_text(size = 9.5, color = "#4a4a4a", margin = margin(b = 10)),
      axis.title = element_text(face = "bold", size = 9.5, color = "#2c3e50"),
      axis.text = element_text(size = 8.5, color = "#333333"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#e5e5e5", linewidth = 0.3),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 9),
      legend.text = element_text(size = 8.5),
      strip.text = element_text(face = "bold", size = 9.5, color = "#003366"),
      plot.margin = margin(t = 15, r = 15, b = 15, l = 15)
    )
}

# ------------------------------------------------------------------------------
# 1. PREPARAÇÃO DO PAINEL, HARMONIZAÇÃO E INTERPOLAÇÃO
# ------------------------------------------------------------------------------
painel_raw <- readRDS("dados_tratados/painel_econometrico_final.rds")
painel <- painel_raw

# Resolução de nomes de colunas e mapeamentos de variáveis
if ("country_name" %in% names(painel)) {
  painel$country_name <- as.character(painel$country_name)
} else if ("country" %in% names(painel)) {
  painel$country_name <- as.character(painel$country)
} else {
  painel$country_name <- as.character(painel$iso3c)
}

if ("gvc_fwd" %in% names(painel)) {
  painel$gvc_fwd <- as.numeric(painel$gvc_fwd)
} else if ("gvc_part_total" %in% names(painel)) {
  painel$gvc_fwd <- as.numeric(painel$gvc_part_total)
} else {
  painel$gvc_fwd <- NA_real_
}

paises_latam <- c("BRA", "MEX", "CHL", "ARG", "COL", "PER", "CRI", "URY", "PAN", "DOM", "ECU")
painel$latam <- if_else(painel$iso3c %in% paises_latam, 1, 0)

# Garantia de existência de colunas institucionais essenciais
for (v in c("bti_st", "gov_eff", "v2clrspct")) {
  if (!v %in% names(painel)) painel[[v]] <- NA_real_
}

# Interpolação robusta para séries temporais bianuais/descontínuas e geração de lags
painel <- painel %>%
  arrange(iso3c, year) %>%
  group_by(iso3c) %>%
  mutate(
    bti_st = if (sum(!is.na(bti_st)) >= 2) {
      zoo::na.locf(
        zoo::na.locf(
          zoo::na.approx(bti_st, na.rm = FALSE, rule = 2),
          na.rm = FALSE
        ),
        fromLast = TRUE,
        na.rm = FALSE
      )
    } else bti_st,
    
    gov_eff = if (sum(!is.na(gov_eff)) >= 2) {
      zoo::na.locf(
        zoo::na.locf(
          zoo::na.approx(gov_eff, na.rm = FALSE, rule = 2),
          na.rm = FALSE
        ),
        fromLast = TRUE,
        na.rm = FALSE
      )
    } else gov_eff,
    
    v2clrspct = if (sum(!is.na(v2clrspct)) >= 2) {
      zoo::na.locf(
        zoo::na.locf(
          zoo::na.approx(v2clrspct, na.rm = FALSE, rule = 2),
          na.rm = FALSE
        ),
        fromLast = TRUE,
        na.rm = FALSE
      )
    } else v2clrspct,
    
    bti_st_l1    = dplyr::lag(bti_st, 1),
    gov_eff_l1   = dplyr::lag(gov_eff, 1),
    v2clrspct_l1 = dplyr::lag(v2clrspct, 1)
  ) %>%
  ungroup()

# ------------------------------------------------------------------------------
# 2. RECUPERAÇÃO DOS COEFICIENTES ECONOMÉTRICOS VALIDADOS (SEM FALLBACKS FIXED)
# ------------------------------------------------------------------------------
cat("--> Obtendo coeficientes estruturais validados nas Etapas 08/09...\n")

# Tenta carregar os coeficientes pré-estimados nas etapas de auditoria prévia
file_globais <- "outputs/tables/tabela_modelos_globais.csv"
coef_bti  <- NA_real_
coef_wgi  <- NA_real_
coef_vdem <- NA_real_

if (file.exists(file_globais)) {
  tabela_mod <- read_csv(file_globais, show_col_types = FALSE)
  
  b_bti <- tabela_mod %>% filter(term == "bti_st_l1") %>% pull(estimate)
  if (length(b_bti) > 0) coef_bti <- b_bti[1]
  
  b_wgi <- tabela_mod %>% filter(term == "gov_eff_l1") %>% pull(estimate)
  if (length(b_wgi) > 0) coef_wgi <- b_wgi[1]
  
  b_vdem <- tabela_mod %>% filter(term == "v2clrspct_l1") %>% pull(estimate)
  if (length(b_vdem) > 0) coef_vdem <- b_vdem[1]
}

candidate_controls <- c("log_gdp_pc", "human_capital", "trade_open", "terms_of_trade", "fdi_gdp", "gfcf")
avail_ctrls <- candidate_controls[sapply(candidate_controls, function(col) {
  col %in% names(painel) && sum(!is.na(painel[[col]])) >= 30
})]

# Função de estimação transparente caso os arquivos prévios não estejam disponíveis
estimate_coef_strict <- function(dep_var, ind_var, controls, data) {
  if (!ind_var %in% names(data) || !dep_var %in% names(data)) return(NA_real_)
  
  sub_df <- data %>% filter(!is.na(.data[[dep_var]]), !is.na(.data[[ind_var]]))
  if (nrow(sub_df) < 30) return(NA_real_)
  
  rhs_terms <- c(ind_var, controls)
  rhs_str <- paste(rhs_terms, collapse = " + ")
  
  # Estimação Two-Way FE alinhada com as etapas anteriores (com erros Driscoll-Kraay ou Cluster)
  fml <- as.formula(paste0(dep_var, " ~ ", rhs_str, " | iso3c + year"))
  mod <- tryCatch(feols(fml, data = sub_df, vcov = "DK"), error = function(e) {
    tryCatch(feols(fml, data = sub_df, cluster = ~iso3c), error = function(e2) NULL)
  })
  
  if (!is.null(mod) && ind_var %in% names(coef(mod))) {
    return(as.numeric(coef(mod)[ind_var]))
  }
  return(NA_real_)
}

if (is.na(coef_bti)) {
  coef_bti <- estimate_coef_strict("gvc_fwd", "bti_st_l1", avail_ctrls, painel)
}
if (is.na(coef_wgi)) {
  coef_wgi <- estimate_coef_strict("gvc_fwd", "gov_eff_l1", avail_ctrls, painel)
}
if (is.na(coef_vdem)) {
  coef_vdem <- estimate_coef_strict("gvc_fwd", "v2clrspct_l1", avail_ctrls, painel)
}

# Verificação rigorosa contra silent hard-coded fallbacks
if (is.na(coef_bti) || is.na(coef_wgi)) {
  stop("❌ [ERRO DE AUDITORIA] Não foi possível estimar ou carregar coeficientes válidos para a simulação contrafactual. Verifique os scripts das Etapas 08/09.")
}

cat(sprintf("   - Coeficiente BTI  (β): %.4f\n", coef_bti))
cat(sprintf("   - Coeficiente WGI  (β): %.4f\n", coef_wgi))
if (!is.na(coef_vdem)) {
  cat(sprintf("   - Coeficiente VDem (β): %.4f\n", coef_vdem))
} else {
  cat("   - Coeficiente VDem (β): Não estimado/disponível na base\n")
}

# ------------------------------------------------------------------------------
# 3. DEFINIÇÃO DA FRONTEIRA INSTITUCIONAL DO LESTE ASIÁTICO (P75)
# ------------------------------------------------------------------------------
benchmarks_asia_top <- painel %>%
  filter(iso3c %in% c("KOR", "TWN", "CHN", "MYS", "SGP", "VNM")) %>%
  group_by(year) %>%
  summarise(
    bti_asia_p75  = if (all(is.na(bti_st))) NA_real_ else quantile(bti_st, 0.75, na.rm = TRUE),
    wgi_asia_p75  = if (all(is.na(gov_eff))) NA_real_ else quantile(gov_eff, 0.75, na.rm = TRUE),
    vdem_asia_p75 = if (all(is.na(v2clrspct))) NA_real_ else quantile(v2clrspct, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# 4. SIMULAÇÃO DO FECHO DO HIATO INSTITUCIONAL (GAP P75 LESTE ASIÁTICO)
# ------------------------------------------------------------------------------
cat("--> Calculando simulações do Fecho do Hiato Institucional (P75 Leste Asiático)...\n")

target_year <- 2022
if (!any(painel$latam == 1 & painel$year == target_year & !is.na(painel$gvc_fwd), na.rm = TRUE)) {
  target_year <- max(painel$year[painel$latam == 1 & !is.na(painel$gvc_fwd)], na.rm = TRUE)
}

cat(paste0("   - Ano base selecionado para a simulação contrafactual: ", target_year, "\n"))

painel_simulacao <- painel %>%
  filter(latam == 1, year == target_year) %>%
  left_join(benchmarks_asia_top, by = "year") %>%
  mutate(
    # Hiato Institucional em Relação ao 75º Percentil do Leste Asiático (Gap P75)
    gap_bti_p75  = pmax(0, bti_asia_p75 - coalesce(bti_st, 0)),
    gap_wgi_p75  = pmax(0, wgi_asia_p75 - coalesce(gov_eff, 0)),
    gap_vdem_p75 = if (!is.na(coef_vdem)) pmax(0, vdem_asia_p75 - coalesce(v2clrspct, 0)) else NA_real_,
    
    gvc_observado       = gvc_fwd,
    ganho_bti           = coef_bti * gap_bti_p75,
    ganho_wgi           = coef_wgi * gap_wgi_p75,
    ganho_vdem          = if (!is.na(coef_vdem)) coef_vdem * gap_vdem_p75 else NA_real_,
    
    gvc_contrafact_bti  = gvc_observado + ganho_bti,
    gvc_contrafact_wgi  = gvc_observado + ganho_wgi,
    gvc_contrafact_vdem = if (!is.na(coef_vdem)) gvc_observado + ganho_vdem else NA_real_
  )

resumo_contrafactual <- painel_simulacao %>%
  select(
    `País` = country_name, ISO3 = iso3c,
    `GVC Observado (%)` = gvc_observado,
    `Hiato BTI (vs P75 Ásia)` = gap_bti_p75,
    `Ganho BTI (p.p.)`        = ganho_bti,
    `Simulado BTI (%)`        = gvc_contrafact_bti,
    `Hiato WGI (vs P75 Ásia)` = gap_wgi_p75,
    `Ganho WGI (p.p.)`        = ganho_wgi,
    `Simulado WGI (%)`        = gvc_contrafact_wgi
  ) %>%
  filter(!is.na(`GVC Observado (%)`)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

write_csv(resumo_contrafactual, "outputs/tables/tabela_simulacao_contrafactual.csv")

# ------------------------------------------------------------------------------
# 5. VISUALIZAÇÃO GRÁFICA ALINHADA À METODOLOGIA DE HIATO ASIÁTICO
# ------------------------------------------------------------------------------
cat("--> Exportando figuras contrafactuais alinhadas à auditoria...\n")

# --- 5A. Gráfico Integrado BTI Principal ---
df_plot_sim <- resumo_contrafactual %>%
  select(`País`, Observado = `GVC Observado (%)`, `Contrafactual (P75 Leste Asiático)` = `Simulado BTI (%)`) %>%
  pivot_longer(cols = c(Observado, `Contrafactual (P75 Leste Asiático)`), names_to = "Cenário", values_to = "Valor") %>%
  filter(!is.na(Valor))

if (nrow(df_plot_sim) > 0) {
  max_val <- max(df_plot_sim$Valor, na.rm = TRUE)
  if (is.infinite(max_val) || is.na(max_val) || max_val <= 0) max_val <- 50
  
  p_sim_latam <- ggplot(df_plot_sim, aes(x = reorder(`País`, Valor), y = Valor, fill = Cenário)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.75), width = 0.65, na.rm = TRUE) +
    geom_text(aes(label = paste0(Valor, "%")), position = position_dodge(width = 0.75), vjust = -0.4, size = 3, fontface = "bold", na.rm = TRUE) +
    scale_fill_manual(values = c("Observado" = "#7f7f7f", "Contrafactual (P75 Leste Asiático)" = "#008837")) +
    scale_y_continuous(limits = c(0, max_val * 1.18), labels = function(x) paste0(x, "%")) +
    labs(
      title = paste0("Ganho Contrafactual em Retenção de VAD na América Latina (", target_year, ")"),
      subtitle = "Simulação: Fecho do Hiato Institucional (BTI) em Relação ao 75º Percentil do Leste Asiático",
      x = "País da América Latina", y = "Forward GVC (% VAD Doméstico)", fill = "Cenário Analítico:"
    ) +
    tema_artigo()
  
  ggsave("outputs/figures/figura_contrafactual_latam_corrigida.png", p_sim_latam, width = 10, height = 6, dpi = 300, bg = "white")
  ggsave("outputs/figures/figura_contrafactual_latam_bti_principal.png", p_sim_latam, width = 10, height = 6, dpi = 300, bg = "white")
}

# --- 5B. Gráfico WGI Baseline ---
df_plot_wgi <- resumo_contrafactual %>%
  select(`País`, Observado = `GVC Observado (%)`, `Contrafactual (WGI P75 Leste Asiático)` = `Simulado WGI (%)`) %>%
  pivot_longer(cols = c(Observado, `Contrafactual (WGI P75 Leste Asiático)`), names_to = "Cenário", values_to = "Valor") %>%
  filter(!is.na(Valor))

if (nrow(df_plot_wgi) > 0) {
  max_val_wgi <- max(df_plot_wgi$Valor, na.rm = TRUE)
  if (is.infinite(max_val_wgi) || is.na(max_val_wgi) || max_val_wgi <= 0) max_val_wgi <- 50
  
  p_sim_wgi <- ggplot(df_plot_wgi, aes(x = reorder(`País`, Valor), y = Valor, fill = Cenário)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.75), width = 0.65, na.rm = TRUE) +
    geom_text(aes(label = paste0(Valor, "%")), position = position_dodge(width = 0.75), vjust = -0.4, size = 3, fontface = "bold", na.rm = TRUE) +
    scale_fill_manual(values = c("Observado" = "#7f7f7f", "Contrafactual (WGI P75 Leste Asiático)" = "#1f77b4")) +
    scale_y_continuous(limits = c(0, max_val_wgi * 1.18), labels = function(x) paste0(x, "%")) +
    labs(
      title = paste0("Ganho Contrafactual em Retenção de VAD na América Latina — Baseline WGI (", target_year, ")"),
      subtitle = "Simulação: Fecho do Hiato de Efetividade Governamental (WGI) vs. P75 do Leste Asiático",
      x = "País da América Latina", y = "Forward GVC (% VAD Doméstico)", fill = "Cenário Analítico:"
    ) +
    tema_artigo()
  
  ggsave("outputs/figures/figura_contrafactual_latam_wgi_baseline.png", p_sim_wgi, width = 10, height = 6, dpi = 300, bg = "white")
}

cat("\n===================================================\n")
cat("✅ ETAPA 10 EXECUTADA COM SUCESSO (CORREÇÃO DE AUDITORIA COMPLETA)!\n")
cat("Arquivos gerados em outputs/tables/ e outputs/figures/:\n")
cat(" 1. tabela_simulacao_contrafactual.csv\n")
cat(" 2. figura_contrafactual_latam_corrigida.png\n")
cat(" 3. figura_contrafactual_latam_bti_principal.png\n")
cat(" 4. figura_contrafactual_latam_wgi_baseline.png\n")
cat("===================================================\n\n")



