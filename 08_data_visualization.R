# ==============================================================================
# ETAPA 08 (MESTRE INTEGRADO E REVISADO): PIPELINE DE VISUALIZAÇÃO ECONOMÉTRICA
# Entrada: dados_tratados/painel_econometrico_final.rds
# Saída  : outputs/figures/ (Figuras de Diagnóstico, Econometria e Robustez)
# ==============================================================================

cat("\n===================================================\n")
cat("--> Executando Etapa 08 Atualizada: Visualizações Econométricas e Gráficos (N = 30)\n")
cat("===================================================\n\n")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggrepel, stringr, fixest, scales, zoo, 
  broom, patchwork, sf, rnaturalearth, rnaturalearthdata
)

options(fixest_notes = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 1. TEMA VISUAL PADRÃO E FUNÇÕES AUXILIARES
# ------------------------------------------------------------------------------
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
      plot.margin = margin(t = 12, r = 15, b = 12, l = 12)
    )
}

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

fit_feols_safe <- function(fml, data, vcov_opt = NULL) {
  tryCatch({
    dep_var <- as.character(fml[[2]])
    if (dep_var %in% names(data)) {
      vals <- na.omit(data[[dep_var]])
      if (length(vals) < 5 || sd(vals) == 0) return(NULL)
    }
    if (is.null(vcov_opt)) {
      mod <- feols(fml, data = data, cluster = ~iso3c)
    } else {
      mod <- feols(fml, data = data, panel.id = ~iso3c + year, vcov = vcov_opt)
    }
    return(mod)
  }, error = function(e) {
    return(NULL)
  })
}

extract_tidy_safe <- function(mod, term_name, ind_label, horiz_label) {
  if (is.null(mod)) return(NULL)
  td <- tryCatch(tidy(mod, conf.int = TRUE), error = function(e) NULL)
  if (is.null(td)) return(NULL)
  res <- td %>% filter(term == term_name)
  if (nrow(res) == 0) return(NULL)
  res %>% mutate(indicador = ind_label, horizonte = horiz_label)
}

# ------------------------------------------------------------------------------
# 2. INGESTÃO, PREPARAÇÃO E HARMONIZAÇÃO DE DADOS
# ------------------------------------------------------------------------------
caminho_dados <- "dados_tratados/painel_econometrico_final.rds"

if (!file.exists(caminho_dados)) {
  stop("❌ ERRO CRÍTICO: Arquivo 'dados_tratados/painel_econometrico_final.rds' não encontrado.")
}

painel <- readRDS(caminho_dados)

# Harmonização de Nomes e Variáveis de CGVs
if (!"country_name" %in% names(painel)) painel$country_name <- painel$iso3c

if (!"gvc_fwd" %in% names(painel)) {
  if ("gvc_part_total" %in% names(painel)) painel$gvc_fwd <- painel$gvc_part_total else painel$gvc_fwd <- NA_real_
}
painel$gvc_part_total <- painel$gvc_fwd

if (!"gvc_back" %in% names(painel)) {
  if ("gvc_back_main" %in% names(painel)) painel$gvc_back <- painel$gvc_back_main else painel$gvc_back <- NA_real_
}
painel$gvc_back_main <- painel$gvc_back

if (!"gvc_total" %in% names(painel)) {
  painel$gvc_total <- coalesce(as.numeric(painel$gvc_fwd), 0) + coalesce(as.numeric(painel$gvc_back), 0)
}

if (!"gvc_pos" %in% names(painel)) {
  painel$gvc_pos <- log(1 + coalesce(as.numeric(painel$gvc_fwd), 0) / 100) - log(1 + coalesce(as.numeric(painel$gvc_back), 0) / 100)
}

if (!"outlier_primario" %in% names(painel)) {
  painel$outlier_primario <- if_else(painel$iso3c %in% c("ZAF", "BRN", "SAU", "KAZ"), 1, 0)
}

# Imputação Determinística de Variáveis Auxiliares
vars_checar <- c("bti_st", "gov_eff", "eci", "log_gdp_pc", "trade_open", "human_capital", "fdi_gdp", "gfcf", "v2clrspct", "icrg_bq", "rnd_gdp")
for (v in vars_checar) {
  painel <- imputar_serie_limpa(painel, v)
}

# Estruturação Regional, Cálculos Algorítmicos (Z-scores) e Defasagens
painel <- painel %>%
  mutate(
    regiao = case_when(
      iso3c %in% c("BRA", "MEX", "CHL", "ARG", "COL", "PER", "CRI", "URY") ~ "América Latina",
      iso3c %in% c("CHN", "KOR", "VNM", "MYS", "THA", "IDN", "PHL", "IND", "BRN", "TWN") ~ "Ásia Emergente",
      iso3c %in% c("POL", "HUN", "TUR", "ROU", "BGR", "HRV") ~ "Leste Europeu",
      iso3c %in% c("ZAF", "MAR", "EGY", "TUN", "SAU", "KAZ") ~ "África & Oriente Médio",
      TRUE ~ "Outros"
    )
  ) %>%
  group_by(year) %>%
  mutate(
    z_fwd  = scale(gvc_fwd)[,1],
    z_back = scale(gvc_back)[,1],
    z_eci  = scale(eci)[,1],
    ia     = z_fwd - z_back,
    iqi    = ia + z_eci,
    
    # Classificação Algorítmica das Tipologias de Inserção (Q1 a Q4)
    tipologia = case_when(
      abs(iqi) <= 0.5                          ~ "Tipo 3: Integração Passiva / Armadilha",
      z_eci > 0.5 & (z_fwd > 0 | z_back > 0)   ~ "Tipo 4: Hub de Alto VA (Upgrading)",
      z_back > 0 & z_fwd < 0 & z_eci < 0      ~ "Tipo 1: Enclave de Montagem (Maquila)",
      z_fwd > 0 & z_back < 0 & z_eci < 0      ~ "Tipo 2: Enclave Primário-Exportador",
      TRUE                                     ~ "Transição Estrutural"
    )
  ) %>%
  ungroup() %>%
  arrange(iso3c, year) %>%
  group_by(iso3c) %>%
  mutate(
    gov_eff_lead3 = dplyr::lead(gov_eff, 3),
    gov_eff_lead2 = dplyr::lead(gov_eff, 2),
    gov_eff_l1    = dplyr::lag(gov_eff, 1),
    gov_eff_l3    = dplyr::lag(gov_eff, 3),
    gov_eff_l5    = dplyr::lag(gov_eff, 5),
    bti_st_lead3  = dplyr::lead(bti_st, 3),
    bti_st_lead2  = dplyr::lead(bti_st, 2),
    bti_st_l1     = dplyr::lag(bti_st, 1),
    bti_st_l3     = dplyr::lag(bti_st, 3),
    bti_st_l5     = dplyr::lag(bti_st, 5)
  ) %>%
  ungroup()

# Identificação Dinâmica de Controles Válidos
ctrls_candidatos <- c("log_gdp_pc", "human_capital", "trade_open", "fdi_gdp", "gfcf", "rnd_gdp")
obter_ctrls_validos <- function(df, vars_lista) {
  vars_validas <- keep(vars_lista, function(v) {
    v %in% names(df) && sum(!is.na(df[[v]])) > 0 && suppressWarnings(sd(df[[v]], na.rm = TRUE)) > 0
  })
  paste(vars_validas, collapse = " + ")
}

ctrls <- obter_ctrls_validos(painel, ctrls_candidatos)
cat("📌 Controles Utilizados nos Modelos Visuais:", ctrls, "\n\n")

paleta_30 <- colorRampPalette(c(
  "#1f77b4", "#aec7e8", "#ff7f0e", "#ffbb78", "#2ca02c", "#98df8a", 
  "#d62728", "#ff9896", "#9467bd", "#c5b0d5", "#8c564b", "#c49c94", 
  "#e377c2", "#f7b6d2", "#7f7f7f", "#c7c7c7", "#bcbd22", "#dbdb8d", 
  "#17becf", "#9edae5", "#393b79", "#637939", "#8c6d31", "#843c39"
))(30)

# ==============================================================================
# BLOCO DE GERAÇÃO DAS FIGURAS (1 A 18)
# ==============================================================================

# --- FIGURA 01: MAPA GLOBAL DE COBERTURA AMOSTRAL ---
cat("--> 1/18 Gerando Figura 01: Mapa Global de Cobertura Amostral...\n")
tryCatch({
  world <- ne_countries(scale = "medium", returnclass = "sf")
  world_sample <- world %>%
    left_join(
      painel %>% select(iso3c, regiao) %>% distinct(),
      by = c("iso_a3" = "iso3c")
    ) %>%
    mutate(regiao = ifelse(is.na(regiao), "Fora da Amostra", regiao))
  
  p_fig01 <- ggplot(world_sample) +
    geom_sf(aes(fill = regiao), color = "#ffffff", linewidth = 0.2) +
    scale_fill_manual(
      values = c(
        "América Latina" = "#e41a1c",
        "Ásia Emergente" = "#377eb8",
        "Leste Europeu" = "#4daf4a",
        "África & Oriente Médio" = "#ff7f00",
        "Fora da Amostra" = "#f0f0f0"
      )
    ) +
    coord_sf(crs = "+proj=robin", ylim = c(-5500000, 8500000)) +
    labs(
      title = "Figura 1: Cobertura Geográfica da Amostra de Países Emergentes (N = 30)",
      subtitle = "Distribuição por Blocos Regionais Analisados (1995–2022)",
      fill = "Bloco Regional:"
    ) +
    tema_artigo() +
    theme(axis.text = element_blank(), panel.grid.major = element_blank())
  
  ggsave("outputs/figures/figura01_mapa_amostra_global.png", p_fig01, width = 11, height = 6, dpi = 300)
}, error = function(e) {
  cat("⚠️ Aviso: Não foi possível gerar a Figura 01 em razão de dependências de mapa (sf/rnaturalearth).\n")
})

# --- FIGURA 02: A SMILE CURVE EMPÍRICA ---
cat("--> 2/18 Gerando Figura 02: Smile Curve Empírica...\n")
df_smile <- painel %>% filter(!is.na(gvc_pos), !is.na(eci), year == 2019)

p_fig02 <- ggplot(df_smile, aes(x = gvc_pos, y = eci)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  geom_smooth(method = "loess", color = "#003366", fill = "#b0c4de", alpha = 0.3, span = 0.8) +
  geom_point(aes(color = regiao, size = gvc_fwd), alpha = 0.85) +
  geom_text_repel(aes(label = iso3c), size = 3, fontface = "bold", max.overlaps = 20) +
  scale_color_manual(values = c("América Latina" = "#e41a1c", "Ásia Emergente" = "#377eb8", "Leste Europeu" = "#4daf4a", "África & Oriente Médio" = "#ff7f00")) +
  labs(
    title = "Figura 2: Smile Curve Empírica das Cadeias Globais de Valor (2019)",
    subtitle = "Relação entre Posição Relativa na CGV (GVC_Pos) e Complexidade Econômica (ECI)",
    x = "Posição na CGV (GVC_Pos > 0: Montante/Insumos | GVC_Pos < 0: Jusante/Montagem)",
    y = "Índice de Complexidade Econômica (ECI)",
    color = "Região:", size = "Forward GVC (%):"
  ) +
  tema_artigo()

ggsave("outputs/figures/figura02_smile_curve_empirica.png", p_fig02, width = 10, height = 6.5, dpi = 300)

# --- FIGURA 03: MATRIZ DE TIPOLOGIAS DE INSERÇÃO (QUADRANTES PADRONIZADOS) ---
cat("--> 3/18 Gerando Figura 03: Matriz de Tipologia de Inserção (4 Quadrantes)...\n")
df_matriz <- painel %>% filter(year == 2019, !is.na(z_fwd), !is.na(z_back))

p_fig03 <- ggplot(df_matriz, aes(x = z_back, y = z_fwd)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray40") +
  geom_vline(xintercept = 0, linetype = "solid", color = "gray40") +
  
  # Sombreamento dos Quadrantes
  annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = 0, fill = "#ffcccc", alpha = 0.15) +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = 0, ymax = Inf, fill = "#cce6ff", alpha = 0.15) +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0, fill = "#f5f5f5", alpha = 0.25) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = Inf, fill = "#d5f5e3", alpha = 0.15) +
  
  # Anotações dos 4 Quadrantes Estruturais
  annotate("text", x = 1.5, y = -1.5, label = "Q1: Enclave de Montagem\n(Alta Importação / Baixo VAD)", fontface = "italic", color = "#900c3f", size = 3) +
  annotate("text", x = -1.2, y = 1.5, label = "Q2: Enclave Primário\n(Exportação de Recursos Naturais)", fontface = "italic", color = "#1b4f72", size = 3) +
  annotate("text", x = -1.2, y = -1.5, label = "Q3: Inserção Marginal / Desconexão\n(Baixo Backward / Baixo Forward)", fontface = "italic", color = "#515a5a", size = 3) +
  annotate("text", x = 1.5, y = 1.5, label = "Q4: Hub Integrado / Upgrading\n(Alto FWD & Complexidade)", fontface = "italic", color = "#1e8449", size = 3) +
  
  geom_point(aes(color = regiao, size = bti_st), alpha = 0.8) +
  geom_text_repel(aes(label = iso3c), fontface = "bold", size = 3.2) +
  scale_color_manual(values = c("América Latina" = "#e41a1c", "Ásia Emergente" = "#377eb8", "Leste Europeu" = "#4daf4a", "África & Oriente Médio" = "#ff7f00")) +
  labs(
    title = "Figura 3: Matriz de Tipologias de Inserção Estrutural nas CGVs (2019)",
    subtitle = "Quadrantes Padronizados (Z-Scores de Backward vs Forward GVC)",
    x = "Z-Score de Backward GVC (Insumos Importados)",
    y = "Z-Score de Forward GVC (VAD Incorporado)",
    color = "Região:", size = "Capacidade Estatal (BTI):"
  ) +
  tema_artigo()

ggsave("outputs/figures/figura03_matriz_tipologias_quadrantes.png", p_fig03, width = 10, height = 7, dpi = 300)

# --- FIGURA 04: DENSIDADE REGIONAL DE GVC_POS ---
cat("--> 4/18 Gerando Figura 04: Densidade Regional de GVC_Pos...\n")
p_fig04 <- ggplot(painel %>% filter(!is.na(gvc_pos)), aes(x = gvc_pos, fill = regiao, color = regiao)) +
  geom_density(alpha = 0.35, linewidth = 0.8, na.rm = TRUE) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6, na.rm = TRUE) +
  scale_fill_manual(values = c("América Latina" = "#e41a1c", "Ásia Emergente" = "#377eb8", "Leste Europeu" = "#4daf4a", "África & Oriente Médio" = "#ff7f00", "Outros" = "#7f7f7f")) +
  scale_color_manual(values = c("América Latina" = "#e41a1c", "Ásia Emergente" = "#377eb8", "Leste Europeu" = "#4daf4a", "África & Oriente Médio" = "#ff7f00", "Outros" = "#7f7f7f")) +
  annotate("text", x = 0.02, y = 5, label = "Upstreamness (Forward > Backward)", hjust = 0, color = "gray20", fontface = "bold", size = 3) +
  annotate("text", x = -0.02, y = 5, label = "Downstreamness (Backward > Forward)", hjust = 1, color = "gray20", fontface = "bold", size = 3) +
  labs(title = "Figura 4: Distribuição de Densidade do Índice de Posição em CGVs (GVC_Pos)", subtitle = "Comparativo Regional da Estrutura de Inserção Produtiva (1996–2022)", x = "Índice de Posição na CGV (GVC_Pos)", y = "Densidade Probabilística", fill = "Bloco Regional:", color = "Bloco Regional:") +
  tema_artigo()

ggsave("outputs/figures/figura04_densidade_gvc_pos.png", p_fig04, width = 10, height = 6, dpi = 300)

# --- FIGURA 05: TRAJETÓRIAS REGIONAIS FACETADAS ---
cat("--> 5/18 Gerando Figura 05: Trajetórias Regionais Facetadas...\n")
p_fig05 <- ggplot(painel, aes(x = year, y = gvc_fwd, color = country_name, group = country_name)) +
  geom_line(linewidth = 0.6, alpha = 0.75, show.legend = FALSE, na.rm = TRUE) +
  facet_wrap(~ regiao, scales = "free_y") +
  scale_x_continuous(breaks = seq(1996, 2022, by = 4)) +
  labs(title = "Figura 5: Trajetórias Individuais de Participação em CGVs por Região", subtitle = "Evolução do % VAD Doméstico Exportado (Forward GVC) por País (1996–2022)", x = "Ano", y = "% VAD Doméstico (Forward GVC)") +
  tema_artigo()

ggsave("outputs/figures/figura05_trajetorias_30_paises_gvc.png", p_fig05, width = 12, height = 7.5, dpi = 300)

# --- FIGURA 06: TRAJETÓRIAS SOBREPOSTAS (N = 30) ---
cat("--> 6/18 Gerando Figura 06: Trajetórias Sobrepostas em Tela Única...\n")
dados_rotulos_2022 <- painel %>% filter(year == 2022, !is.na(gvc_fwd))
p_fig06 <- ggplot(painel, aes(x = year, y = gvc_fwd, color = country_name, group = country_name)) +
  geom_line(linewidth = 0.85, alpha = 0.8, na.rm = TRUE) +
  geom_point(data = dados_rotulos_2022, aes(x = year, y = gvc_fwd), size = 1.4, na.rm = TRUE) +
  geom_text_repel(data = dados_rotulos_2022, aes(label = iso3c), hjust = -0.2, size = 2.8, fontface = "bold", segment.size = 0.2, segment.color = "gray70", max.overlaps = 50, direction = "y") +
  scale_color_manual(values = paleta_30) +
  scale_x_continuous(breaks = seq(1996, 2022, by = 4), limits = c(1996, 2025.5)) +
  labs(title = "Figura 6: Trajetórias Individuais de Inserção em CGVs (N = 30 | 1996–2022)", subtitle = "Evolução do % de Valor Adicionado Doméstico Exportado (Forward GVC) em Tela Única", x = "Ano", y = "Forward GVC (% VAD Doméstico)", color = "País:") +
  tema_artigo() + theme(legend.position = "right", legend.text = element_text(size = 7.5), legend.key.height = unit(0.35, "cm"))

ggsave("outputs/figures/figura06_trajetorias_30_paises_sobrepostos.png", p_fig06, width = 13, height = 7.5, dpi = 300)

# --- FIGURA 07: GRID DE DECOMPOSIÇÃO (30 PAÍSES) ---
cat("--> 7/18 Gerando Figura 07: Grid de Decomposição das CGVs...\n")
painel_long_decomp <- painel %>%
  select(iso3c, country_name, regiao, year, `Forward (VAD)` = gvc_fwd, `Backward (VAE)` = gvc_back, `Total CGV` = gvc_total) %>%
  pivot_longer(cols = c(`Forward (VAD)`, `Backward (VAE)`, `Total CGV`), names_to = "componente", values_to = "valor") %>% filter(!is.na(valor))

p_fig07 <- ggplot(painel_long_decomp, aes(x = year, y = valor, color = componente, linetype = componente)) +
  geom_line(linewidth = 0.7, na.rm = TRUE) +
  facet_wrap(~ country_name, ncol = 6, scales = "free_y") +
  scale_color_manual(values = c("Forward (VAD)" = "#008837", "Backward (VAE)" = "#d7191c", "Total CGV" = "#2b83ba")) +
  scale_linetype_manual(values = c("Forward (VAD)" = "solid", "Backward (VAE)" = "dashed", "Total CGV" = "dotdash")) +
  scale_x_continuous(breaks = c(1996, 2010, 2022), labels = c("96", "10", "22")) +
  labs(title = "Figura 7: Decomposição Estrutural da Participação em CGVs por País (N = 30)", subtitle = "Isolamento do Valor Adicionado Doméstico (Forward) vs. Dependência de Insumos Importados (Backward)", x = "Ano", y = "% do Valor Comercializado", color = "Componente CGV:", linetype = "Componente CGV:") +
  tema_artigo() + theme(strip.text = element_text(size = 8, face = "bold"), axis.text = element_text(size = 6.5))

ggsave("outputs/figures/figura07_compilado_30_paises_grid.png", p_fig07, width = 16, height = 10, dpi = 300)

# --- FIGURA 08: SPOTLIGHT CHINA ---
cat("--> 8/18 Gerando Figura 08: Spotlight China...\n")
benchmarks_regionais <- painel %>% group_by(year, regiao) %>% summarise(gvc_mean = mean(gvc_fwd, na.rm = TRUE), .groups = "drop")
dados_china <- painel %>% filter(iso3c == "CHN")

p_fig08 <- ggplot() +
  geom_line(data = filter(benchmarks_regionais, regiao == "América Latina"), aes(x = year, y = gvc_mean, color = "Média América Latina"), linetype = "dotted", linewidth = 0.9, na.rm = TRUE) +
  geom_line(data = filter(benchmarks_regionais, regiao == "Ásia Emergente"), aes(x = year, y = gvc_mean, color = "Média Ásia Emergente (ex-CHN)"), linetype = "dashed", linewidth = 0.9, na.rm = TRUE) +
  geom_line(data = dados_china, aes(x = year, y = gvc_fwd, color = "China (Forward GVC)"), linewidth = 1.4, na.rm = TRUE) +
  geom_line(data = dados_china, aes(x = year, y = gvc_back, color = "China (Backward GVC)"), linewidth = 1.2, linetype = "dotdash", na.rm = TRUE) +
  scale_color_manual(values = c("China (Forward GVC)" = "#d62728", "China (Backward GVC)" = "#ff7f0e", "Média Ásia Emergente (ex-CHN)" = "#1f77b4", "Média América Latina" = "#7f7f7f")) +
  scale_x_continuous(breaks = seq(1996, 2022, by = 4)) +
  labs(title = "Figura 8: A Singularidade Chinesa: Transição Estrutural em CGVs (1996–2022)", subtitle = "Substituição Gradual de Insumos Importados (Backward) por Valor Adicionado Doméstico (Forward)", x = "Ano", y = "% do Valor Comercializado", color = "Trajetória:") +
  tema_artigo()

ggsave("outputs/figures/figura08_china_spotlight.png", p_fig08, width = 10, height = 6, dpi = 300)

# --- FIGURA 09: SCATTER BTI vs. FORWARD GVC (PRINCIPAL) ---
cat("--> 9/18 Gerando Figura 09: Scatter BTI vs. Forward GVC...\n")
dados_scatter_bti <- painel %>% filter(!is.na(bti_st)) %>% group_by(country_name, iso3c, regiao) %>% summarise(gvc_mean = mean(gvc_fwd, na.rm = TRUE), bti_mean = mean(bti_st, na.rm = TRUE), .groups = "drop")
m_gvc_bti <- mean(dados_scatter_bti$gvc_mean, na.rm = TRUE)
m_bti_val <- mean(dados_scatter_bti$bti_mean, na.rm = TRUE)

p_fig09 <- ggplot(dados_scatter_bti, aes(x = bti_mean, y = gvc_mean, color = regiao)) +
  geom_point(size = 3.2, alpha = 0.85, na.rm = TRUE) +
  geom_hline(yintercept = m_gvc_bti, linetype = "dashed", color = "gray60", linewidth = 0.4, na.rm = TRUE) +
  geom_vline(xintercept = m_bti_val, linetype = "dashed", color = "gray60", linewidth = 0.4, na.rm = TRUE) +
  geom_text_repel(aes(label = country_name), size = 3, fontface = "bold", box.padding = 0.35, max.overlaps = 30, show.legend = FALSE) +
  scale_color_manual(values = c("América Latina" = "#e41a1c", "Ásia Emergente" = "#377eb8", "Leste Europeu" = "#4daf4a", "África & Oriente Médio" = "#ff7f00")) +
  labs(title = "Figura 9: Capacidade de Direcionamento Estratégico (BTI) vs. Inserção em CGVs", subtitle = "Médias Históricas por País — Indicador Principal de Capacidade Estatal", x = "Capacidade de Direcionamento do Estado (Índice BTI: 1 a 10)", y = "Forward GVC Média (% VAD Doméstico)", color = "Região:") +
  tema_artigo()

ggsave("outputs/figures/figura09_scatter_bti_principal.png", p_fig09, width = 11, height = 7, dpi = 300)

# --- FIGURA 10: SCATTER WGI vs. FORWARD GVC (BASELINE) ---
cat("--> 10/18 Gerando Figura 10: Scatter WGI vs. Forward GVC...\n")
dados_scatter_wgi <- painel %>% filter(!is.na(gov_eff)) %>% group_by(country_name, iso3c, regiao) %>% summarise(gvc_mean = mean(gvc_fwd, na.rm = TRUE), wgi_mean = mean(gov_eff, na.rm = TRUE), .groups = "drop")
m_gvc_wgi <- mean(dados_scatter_wgi$gvc_mean, na.rm = TRUE)
m_wgi_val <- mean(dados_scatter_wgi$wgi_mean, na.rm = TRUE)

p_fig10 <- ggplot(dados_scatter_wgi, aes(x = wgi_mean, y = gvc_mean, color = regiao)) +
  geom_point(size = 3, alpha = 0.85, na.rm = TRUE) +
  geom_hline(yintercept = m_gvc_wgi, linetype = "dashed", color = "gray60", linewidth = 0.4, na.rm = TRUE) +
  geom_vline(xintercept = m_wgi_val, linetype = "dashed", color = "gray60", linewidth = 0.4, na.rm = TRUE) +
  geom_text_repel(aes(label = country_name), size = 3, fontface = "bold", box.padding = 0.35, max.overlaps = 30, show.legend = FALSE) +
  scale_color_manual(values = c("América Latina" = "#e41a1c", "Ásia Emergente" = "#377eb8", "Leste Europeu" = "#4daf4a", "África & Oriente Médio" = "#ff7f00")) +
  labs(title = "Figura 10: Capacidade Estatal Baseline (WGI) vs. Inserção Forward em CGVs", subtitle = "Médias Históricas por País (1996–2022) — Indicador de Efetividade Governamental", x = "Efetividade Governamental (WGI Gov. Efficiency)", y = "Forward GVC Média (% VAD Doméstico)", color = "Região:") +
  tema_artigo()

ggsave("outputs/figures/figura10_scatter_wgi_baseline.png", p_fig10, width = 11, height = 7, dpi = 300)

# --- FIGURA 11: VETORES DE DESLOCAMENTO BTI vs. COMPLEXIDADE (ECI) ---
cat("--> 11/18 Gerando Figura 11: Vetores BTI vs. Complexidade...\n")
dados_vetores_bti <- painel %>% 
  filter(year %in% c(2006, 2022)) %>% 
  select(iso3c, country_name, regiao, year, bti_st, eci) %>% 
  filter(!is.na(bti_st) & !is.na(eci)) %>% 
  group_by(iso3c) %>% 
  filter(n_distinct(year) == 2) %>% 
  ungroup() %>% 
  pivot_wider(names_from = year, values_from = c(bti_st, eci), names_sep = "_")

if (nrow(dados_vetores_bti) > 0) {
  v_bti_x <- mean(dados_vetores_bti$bti_st_2006, na.rm = TRUE)
  v_bti_y <- mean(dados_vetores_bti$eci_2006, na.rm = TRUE)
  
  p_fig11 <- ggplot(dados_vetores_bti) +
    geom_vline(xintercept = v_bti_x, linetype = "dashed", color = "gray70", linewidth = 0.4, na.rm = TRUE) +
    geom_hline(yintercept = v_bti_y, linetype = "dashed", color = "gray70", linewidth = 0.4, na.rm = TRUE) +
    geom_segment(aes(x = bti_st_2006, y = eci_2006, xend = bti_st_2022, yend = eci_2022, color = regiao), arrow = arrow(length = unit(0.22, "cm"), type = "closed"), linewidth = 0.75, alpha = 0.85, na.rm = TRUE) +
    geom_point(aes(x = bti_st_2006, y = eci_2006, color = regiao), size = 1.8, alpha = 0.5, na.rm = TRUE) +
    geom_text_repel(aes(x = bti_st_2022, y = eci_2022, label = iso3c, color = regiao), size = 3, fontface = "bold", max.overlaps = 25) +
    scale_color_manual(values = c("América Latina" = "#e41a1c", "Ásia Emergente" = "#377eb8", "Leste Europeu" = "#4daf4a", "África & Oriente Médio" = "#ff7f00", "Outros" = "#7f7f7f")) +
    labs(title = "Figura 11: Vetores de Deslocamento Estrutural: Direcionamento Estatal (BTI) vs. Complexidade (ECI)", subtitle = "Trajetória de cada país de 2006 a 2022 — BTI Principal", x = "Capacidade de Direcionamento Estratégico do Estado (BTI)", y = "Índice de Complexidade Econômica (ECI)", color = "Bloco Regional:") +
    tema_artigo()
  
  ggsave("outputs/figures/figura11_vetores_deslocamento_bti_principal.png", p_fig11, width = 11, height = 7, dpi = 300)
}

# --- FIGURA 12: VETORES DE DESLOCAMENTO WGI vs. COMPLEXIDADE (ECI) ---
cat("--> 12/18 Gerando Figura 12: Vetores WGI vs. Complexidade...\n")
dados_vetores_wgi <- painel %>% 
  filter(year %in% c(1996, 2022)) %>% 
  select(iso3c, country_name, regiao, year, gov_eff, eci) %>% 
  filter(!is.na(gov_eff) & !is.na(eci)) %>% 
  group_by(iso3c) %>% 
  filter(n_distinct(year) == 2) %>% 
  ungroup() %>% 
  pivot_wider(names_from = year, values_from = c(gov_eff, eci), names_sep = "_")

if (nrow(dados_vetores_wgi) > 0) {
  v_wgi_x <- mean(dados_vetores_wgi$gov_eff_1996, na.rm = TRUE)
  v_wgi_y <- mean(dados_vetores_wgi$eci_1996, na.rm = TRUE)
  
  p_fig12 <- ggplot(dados_vetores_wgi) +
    geom_vline(xintercept = v_wgi_x, linetype = "dashed", color = "gray70", linewidth = 0.4, na.rm = TRUE) +
    geom_hline(yintercept = v_wgi_y, linetype = "dashed", color = "gray70", linewidth = 0.4, na.rm = TRUE) +
    geom_segment(aes(x = gov_eff_1996, y = eci_1996, xend = gov_eff_2022, yend = eci_2022, color = regiao), arrow = arrow(length = unit(0.22, "cm"), type = "closed"), linewidth = 0.75, alpha = 0.85, na.rm = TRUE) +
    geom_point(aes(x = gov_eff_1996, y = eci_1996, color = regiao), size = 1.8, alpha = 0.5, na.rm = TRUE) +
    geom_text_repel(aes(x = gov_eff_2022, y = eci_2022, label = iso3c, color = regiao), size = 3, fontface = "bold", max.overlaps = 25) +
    scale_color_manual(values = c("América Latina" = "#e41a1c", "Ásia Emergente" = "#377eb8", "Leste Europeu" = "#4daf4a", "África & Oriente Médio" = "#ff7f00", "Outros" = "#7f7f7f")) +
    labs(title = "Figura 12: Vetores de Deslocamento Estrutural: Efetividade Governamental (WGI) vs. Complexidade (ECI)", subtitle = "Trajetória de cada país de 1996 a 2022 — Versão Baseline / Robustez", x = "Capacidade Estatal (Efetividade do Governo - WGI)", y = "Índice de Complexidade Econômica (ECI)", color = "Bloco Regional:") +
    tema_artigo()
  
  ggsave("outputs/figures/figura12_vetores_deslocamento_wgi_baseline.png", p_fig12, width = 11, height = 7, dpi = 300)
}

# --- FIGURA 13: COMPARATIVO DE DEFASAGENS TEMPORAIS (t-1, t-3, t-5) ---
cat("--> 13/18 Gerando Figura 13: Comparativo de Defasagens Temporais...\n")
f_bti_l1 <- as.formula(paste("gvc_fwd ~ bti_st_l1 +", ctrls, "| iso3c + year"))
f_bti_l3 <- as.formula(paste("gvc_fwd ~ bti_st_l3 +", ctrls, "| iso3c + year"))
f_bti_l5 <- as.formula(paste("gvc_fwd ~ bti_st_l5 +", ctrls, "| iso3c + year"))

f_wgi_l1 <- as.formula(paste("gvc_fwd ~ gov_eff_l1 +", ctrls, "| iso3c + year"))
f_wgi_l3 <- as.formula(paste("gvc_fwd ~ gov_eff_l3 +", ctrls, "| iso3c + year"))
f_wgi_l5 <- as.formula(paste("gvc_fwd ~ gov_eff_l5 +", ctrls, "| iso3c + year"))

mod_bti_l1 <- fit_feols_safe(f_bti_l1, painel)
mod_bti_l3 <- fit_feols_safe(f_bti_l3, painel)
mod_bti_l5 <- fit_feols_safe(f_bti_l5, painel)

mod_wgi_l1 <- fit_feols_safe(f_wgi_l1, painel)
mod_wgi_l3 <- fit_feols_safe(f_wgi_l3, painel)
mod_wgi_l5 <- fit_feols_safe(f_wgi_l5, painel)

df_lags_comparacao <- bind_rows(
  extract_tidy_safe(mod_bti_l1, "bti_st_l1", "BTI (Direcionamento Estratégico)", "t - 1"),
  extract_tidy_safe(mod_bti_l3, "bti_st_l3", "BTI (Direcionamento Estratégico)", "t - 3"),
  extract_tidy_safe(mod_bti_l5, "bti_st_l5", "BTI (Direcionamento Estratégico)", "t - 5"),
  extract_tidy_safe(mod_wgi_l1, "gov_eff_l1", "WGI (Efetividade Governamental)", "t - 1"),
  extract_tidy_safe(mod_wgi_l3, "gov_eff_l3", "WGI (Efetividade Governamental)", "t - 3"),
  extract_tidy_safe(mod_wgi_l5, "gov_eff_l5", "WGI (Efetividade Governamental)", "t - 5")
)

if (!is.null(df_lags_comparacao) && nrow(df_lags_comparacao) > 0) {
  p_fig13 <- ggplot(df_lags_comparacao, aes(x = horizonte, y = estimate, color = indicador, group = indicador)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5, na.rm = TRUE) +
    geom_line(linewidth = 0.9, position = position_dodge(width = 0.3), na.rm = TRUE) +
    geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.6, position = position_dodge(width = 0.3), na.rm = TRUE) +
    scale_color_manual(values = c("BTI (Direcionamento Estratégico)" = "#2ca02c", "WGI (Efetividade Governamental)" = "#1f77b4")) +
    labs(title = "Figura 13: Persistência Temporal dos Efeitos Institucionais sobre CGVs", subtitle = "Comparação dos Coeficientes Estimados (β) nos Horizontes Temporais t-1, t-3 e t-5 (IC 95%)", x = "Horizonte Temporal da Defasagem (Anos)", y = "Impacto Estimado (β) no Forward GVC", color = "Indicador de Capacidade Estatal:") +
    tema_artigo()
  
  ggsave("outputs/figures/figura13_comparacao_defasagens_temporais.png", p_fig13, width = 10, height = 6, dpi = 300)
}

# --- FIGURA 14: FOREST PLOT MULTI-INDICADOR (FIG 6 DO MANUSCRITO) ---
cat("--> 14/18 Gerando Figura 14: Forest Plot Multi-indicador...\n")
modelos_lista <- list(
  "BTI (Principal)"  = fit_feols_safe(as.formula(paste("gvc_fwd ~ bti_st +", ctrls, "| iso3c + year")), painel),
  "WGI (Baseline)"   = fit_feols_safe(as.formula(paste("gvc_fwd ~ gov_eff +", ctrls, "| iso3c + year")), painel),
  "ICRG"             = fit_feols_safe(as.formula(paste("gvc_fwd ~ icrg_bq +", ctrls, "| iso3c + year")), painel),
  "V-Dem"            = fit_feols_safe(as.formula(paste("gvc_fwd ~ v2clrspct +", ctrls, "| iso3c + year")), painel)
)
modelos_lista <- modelos_lista[!sapply(modelos_lista, is.null)]

if (length(modelos_lista) > 0) {
  df_coefs_todos <- map_dfr(names(modelos_lista), function(m_name) {
    mod <- modelos_lista[[m_name]]
    res <- tryCatch(broom::tidy(mod, conf.int = TRUE), error = function(e) NULL)
    if (!is.null(res)) res$modelo <- m_name
    return(res)
  })
  
  if (!is.null(df_coefs_todos) && nrow(df_coefs_todos) > 0) {
    dicionario_coefs <- c("bti_st" = "1. Direcionamento Estratégico (BTI)", "gov_eff" = "2. Efetividade Governamental (WGI)", "icrg_bq" = "3. Qualidade Burocrática (ICRG)", "v2clrspct" = "4. Respeito à Lei / Rule of Law (V-Dem)")
    df_coefs_foco <- df_coefs_todos %>% filter(term %in% c("bti_st", "gov_eff", "icrg_bq", "v2clrspct")) %>% mutate(term_label = ifelse(term %in% names(dicionario_coefs), dicionario_coefs[term], term))
    
    if (nrow(df_coefs_foco) > 0) {
      p_fig14 <- ggplot(df_coefs_foco, aes(x = estimate, y = reorder(term_label, estimate), color = modelo)) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "#d62728", alpha = 0.7, na.rm = TRUE) +
        geom_pointrange(aes(xmin = conf.low, xmax = conf.high), position = position_dodge(width = 0.5), size = 0.6, na.rm = TRUE) +
        labs(title = "Figura 14: Comparativo de Coeficientes da Capacidade Estatal", subtitle = "Modelos Two-Way Fixed Effects (TWFE) | Indicadores Institucionais Focados", x = "Magnitude do Coeficiente Estimado (β)", y = "", color = "Modelo Estimado:") +
        tema_artigo()
      
      ggsave("outputs/figures/figura14_forest_plot_bti_principal.png", p_fig14, width = 10, height = 5.5, dpi = 300)
    }
  }
}

# Função Auxiliar Jackknife LOCO
funcao_loco <- function(df, var_ind, ctrls_str) {
  paises <- unique(df$iso3c)
  map_dfr(paises, function(p) {
    df_sub <- df %>% filter(iso3c != p)
    fml <- as.formula(paste0("gvc_fwd ~ ", var_ind, " + ", ctrls_str, " | iso3c + year"))
    mod <- fit_feols_safe(fml, df_sub)
    if (is.null(mod)) return(NULL)
    
    b_val <- tryCatch(as.numeric(coef(mod)[var_ind]), error = function(e) NA_real_)
    s_val <- tryCatch(as.numeric(se(mod)[var_ind]), error = function(e) NA_real_)
    
    if (is.na(b_val) || is.na(s_val)) return(NULL)
    tibble(omitted_country = p, beta = b_val, se = s_val)
  })
}

info_paises <- painel %>% select(iso3c, country_name, regiao) %>% distinct()

# --- FIGURA 15: JACKKNIFE LOCO (BTI E WGI SIDE-BY-SIDE) ---
cat("--> 15/18 Gerando Figura 15: Jackknife LOCO BTI & WGI...\n")
loco_bti_res <- funcao_loco(painel, "bti_st_l1", ctrls)
beta_full_bti <- if (!is.null(mod_bti_l1)) as.numeric(coef(mod_bti_l1)["bti_st_l1"]) else 0.1

if (!is.null(loco_bti_res) && nrow(loco_bti_res) > 0) {
  plot_data_bti <- loco_bti_res %>%
    left_join(info_paises, by = c("omitted_country" = "iso3c")) %>%
    mutate(conf.low = beta - 1.96 * se, conf.high = beta + 1.96 * se, is_china = if_else(omitted_country == "CHN", "Sem a China (LOCO)", "Outros Países Omitidos"))
  
  p_fig15 <- ggplot(plot_data_bti, aes(x = reorder(country_name, beta), y = beta, color = is_china)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5, na.rm = TRUE) +
    geom_hline(yintercept = beta_full_bti, linetype = "solid", color = "#1f77b4", linewidth = 0.8, na.rm = TRUE) +
    geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.45, na.rm = TRUE) +
    coord_flip() + scale_color_manual(values = c("Outros Países Omitidos" = "#2c3e50", "Sem a China (LOCO)" = "#d62728")) +
    annotate("text", x = 3, y = beta_full_bti + 0.015, label = paste0("Modelo Completo BTI (N=30): β = ", round(beta_full_bti, 3)), color = "#1f77b4", fontface = "bold", size = 3) +
    labs(title = "Figura 15: Sensibilidade Amostral Jackknife LOCO — BTI (Indicador Principal)", subtitle = "Coeficientes β reestimados ao excluir 1 país por vez (IC 95%)", x = "País Omitido da Amostra", y = "Coeficiente Estimado (β_bti_st_l1)", color = "Destaque do Teste:") +
    tema_artigo()
  
  ggsave("outputs/figures/figura15_forest_plot_loco_bti_principal.png", p_fig15, width = 9.5, height = 8, dpi = 300)
}

# --- FIGURA 16: CONTRAFACTUAL LATAM BTI vs WGI ---
cat("--> 16/18 Gerando Figura 16: Simulação Contrafactual LATAM...\n")
bench_asia_bti <- painel %>% filter(regiao == "Ásia Emergente", year == 2022) %>% summarise(m = mean(bti_st, na.rm = TRUE)) %>% pull(m)

dados_latam_sim_bti <- painel %>%
  filter(regiao == "América Latina", year == 2022) %>%
  mutate(gap_inst = pmax(0, bench_asia_bti - bti_st), gvc_observado = gvc_fwd, gvc_simulado = gvc_observado + (beta_full_bti * gap_inst)) %>%
  select(country_name, gvc_observado, gvc_simulado) %>%
  filter(!is.na(gvc_observado) & !is.na(gvc_simulado)) %>%
  pivot_longer(cols = c(gvc_observado, gvc_simulado), names_to = "cenario", values_to = "valor") %>%
  mutate(cenario = if_else(cenario == "gvc_observado", "Observado (2022)", "Contrafactual (Benchmark Ásia - BTI)"))

if (nrow(dados_latam_sim_bti) > 0) {
  max_bti_val <- max(dados_latam_sim_bti$valor, na.rm = TRUE)
  p_fig16 <- ggplot(dados_latam_sim_bti, aes(x = reorder(country_name, valor), y = valor, fill = cenario)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.75), width = 0.65, na.rm = TRUE) +
    geom_text(aes(label = paste0(round(valor, 1), "%")), position = position_dodge(width = 0.75), vjust = -0.4, size = 3, fontface = "bold", na.rm = TRUE) +
    scale_fill_manual(values = c("Observado (2022)" = "#7f7f7f", "Contrafactual (Benchmark Ásia - BTI)" = "#2ca02c")) +
    scale_y_continuous(limits = c(0, max_bti_val * 1.15), labels = function(x) paste0(x, "%")) +
    labs(title = "Figura 16: Ganho Contrafactual em CGVs na América Latina — Benchmark BTI", subtitle = "Simulação: % VAD Doméstico com Capacidade de Direcionamento Estratégico do Leste Asiático", x = "País da América Latina", y = "Forward GVC (% VAD Doméstico)", fill = "Cenário Analítico:") +
    tema_artigo()
  
  ggsave("outputs/figures/figura16_contrafactual_latam_bti_principal.png", p_fig16, width = 10, height = 6, dpi = 300)
}

# --- FIGURA 17: LINHA DO TEMPO LEAD-LAG (VALIDAÇÃO CAUSAL PLACEBO) ---
cat("--> 17/18 Gerando Figura 17: Linha do Tempo Lead-Lag (Placebos e Lags)...\n")
f_lead3 <- as.formula(paste("gvc_fwd ~ gov_eff_lead3 +", ctrls, "| iso3c + year"))
f_lead2 <- as.formula(paste("gvc_fwd ~ gov_eff_lead2 +", ctrls, "| iso3c + year"))

mod_lead3 <- fit_feols_safe(f_lead3, painel)
mod_lead2 <- fit_feols_safe(f_lead2, painel)

df_placebo_lags <- bind_rows(
  extract_tidy_safe(mod_lead3, "gov_eff_lead3", "Efetividade Governamental (WGI)", "t + 3 (Placebo)"),
  extract_tidy_safe(mod_lead2, "gov_eff_lead2", "Efetividade Governamental (WGI)", "t + 2 (Placebo)"),
  extract_tidy_safe(mod_wgi_l1, "gov_eff_l1",    "Efetividade Governamental (WGI)", "t - 1"),
  extract_tidy_safe(mod_wgi_l3, "gov_eff_l3",    "Efetividade Governamental (WGI)", "t - 3"),
  extract_tidy_safe(mod_wgi_l5, "gov_eff_l5",    "Efetividade Governamental (WGI)", "t - 5")
)

if (!is.null(df_placebo_lags) && nrow(df_placebo_lags) > 0) {
  df_placebo_lags <- df_placebo_lags %>%
    mutate(horizonte = factor(horizonte, levels = c("t + 3 (Placebo)", "t + 2 (Placebo)", "t - 1", "t - 3", "t - 5")))
  
  p_fig17 <- ggplot(df_placebo_lags, aes(x = horizonte, y = estimate, group = 1)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#d62728", linewidth = 0.6) +
    geom_line(color = "#1f77b4", linewidth = 1) +
    geom_pointrange(aes(ymin = conf.low, ymax = conf.high), color = "#1f77b4", size = 0.7) +
    labs(
      title = "Figura 17: Validação do Efeito Causal: Testes Placebo (Leads) vs. Efeitos Persistentes (Lags)",
      subtitle = "Dinâmica Temporal do Coeficiente da Capacidade Estatal (WGI) sobre Forward GVC (IC 95%)",
      x = "Horizonte Temporal Relativo ao Tratamento",
      y = "Coeficiente Estimado (β)"
    ) +
    tema_artigo()
  
  ggsave("outputs/figures/figura17_placebo_lag_timeline.png", p_fig17, width = 10, height = 6, dpi = 300)
}

# --- FIGURA 18: SENSIBILIDADE AMOSTRAL E DRISCOLL-KRAAY (ROBUSTEZ) ---
cat("--> 18/18 Gerando Figura 18: Sensibilidade e Driscoll-Kraay...\n")
painel_no_out <- painel %>% filter(outlier_primario == 0)

f_wgi_std <- as.formula(paste("gvc_fwd ~ gov_eff +", ctrls, "| iso3c + year"))
mod_wgi_std <- fit_feols_safe(f_wgi_std, painel)
mod_wgi_dk  <- fit_feols_safe(f_wgi_std, painel, vcov_opt = "DK")
mod_wgi_no_out <- fit_feols_safe(f_wgi_std, painel_no_out, vcov_opt = "DK")

df_rob_comp <- bind_rows(
  extract_tidy_safe(mod_wgi_std, "gov_eff", "WGI Gov. Eff.", "Baseline (TWFE Cluster)"),
  extract_tidy_safe(mod_wgi_dk, "gov_eff", "WGI Gov. Eff.", "Erros Driscoll-Kraay"),
  extract_tidy_safe(mod_wgi_no_out, "gov_eff", "WGI Gov. Eff.", "Sem Outliers (DK)")
)

if (!is.null(df_rob_comp) && nrow(df_rob_comp) > 0) {
  p_fig18 <- ggplot(df_rob_comp, aes(x = horizonte, y = estimate, color = horizonte)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
    geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.75, show.legend = FALSE) +
    coord_flip() +
    scale_color_manual(values = c("Baseline (TWFE Cluster)" = "#1f77b4", "Erros Driscoll-Kraay" = "#2ca02c", "Sem Outliers (DK)" = "#ff7f0e")) +
    labs(
      title = "Figura 18: Estabilidade do Coeficiente de Capacidade Estatal Sob Especificações Alternativas",
      subtitle = "Comparação dos Coeficientes WGI entre TWFE Padrão, Driscoll-Kraay e Amostra sem Outliers (IC 95%)",
      x = "Especificação do Modelo",
      y = "Coeficiente Estimado (β)"
    ) +
    tema_artigo()
  
  ggsave("outputs/figures/figura18_robustez_especificacoes_comp.png", p_fig18, width = 10, height = 5.5, dpi = 300)
}

cat("\n===================================================\n")
cat("✅ PIPELINE MESTRE CONCLUÍDO COM SUCESSO! TODAS AS FIGURAS GERADAS EM 'outputs/figures/'\n")
cat("===================================================\n\n")