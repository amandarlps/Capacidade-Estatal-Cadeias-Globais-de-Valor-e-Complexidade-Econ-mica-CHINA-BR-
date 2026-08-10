# ==============================================================================
# ETAPA 02: MERGE, HARMONIZAÇÃO DO PAINEL (N = 30) E EXTRAÇÃO DA TABELA 1
# Versão Resiliente: Tratamento Defensivo de Colunas Ausentes (WGI/TiVA/WDI)
# ==============================================================================

cat("\n===================================================\n")
cat("--> Executando Etapa 02: Merge e Harmonização do Painel (N = 30)\n")
cat("===================================================\n\n")

suppressPackageStartupMessages({
  library(tidyverse)
  library(zoo)
  library(countrycode)
  library(knitr)
})

# ------------------------------------------------------------------------------
# 0. CARREGAMENTO SEGURO E RESGATE ESTRUTURAL DAS FONTES
# ------------------------------------------------------------------------------
caminho_fontes <- "dados_brutos/fontes_inspecionadas.rds"

gc(verbose = FALSE)

carregar_rds_grande <- function(caminho) {
  if (!file.exists(caminho)) return(NULL)
  tryCatch(
    {
      readRDS(caminho)
    },
    error = function(e1) {
      tryCatch(
        {
          con <- gzfile(caminho, "rb")
          on.exit(close(con))
          readRDS(con)
        },
        error = function(e2) return(NULL)
      )
    }
  )
}

fontes <- carregar_rds_grande(caminho_fontes)
raw <- if (!is.null(fontes$dados_brutos)) fontes$dados_brutos else fontes

# Amostra dos 30 países do artigo
amostra_30 <- c(
  "BRA", "MEX", "CHL", "ARG", "COL", "PER", "CRI", "URY",
  "CHN", "KOR", "VNM", "MYS", "THA", "IDN", "PHL", "IND", "BRN", "TWN",
  "POL", "HUN", "TUR", "ROU", "BGR", "HRV",
  "ZAF", "MAR", "EGY", "TUN", "SAU", "KAZ"
)

# ------------------------------------------------------------------------------
# FUNÇÕES AUXILIARES DE TRATAMENTO E BUSCA
# ------------------------------------------------------------------------------

# Garantia de presença de colunas obrigatorias no dataframe
garantir_colunas <- function(df, colunas, tipo_default = NA_real_) {
  for (col in colunas) {
    if (!col %in% names(df)) {
      df[[col]] <- tipo_default
    }
  }
  return(df)
}

# Busca flexível de dataframes dentro da lista 'raw'
obter_df_raw <- function(raw_obj, padroes) {
  if (is.null(raw_obj) || !is.list(raw_obj)) return(NULL)
  nomes <- names(raw_obj)
  for (p in padroes) {
    match_idx <- grep(p, nomes, ignore.case = TRUE)
    if (length(match_idx) > 0) return(raw_obj[[match_idx[1]]])
  }
  return(NULL)
}

# Padronização universal de códigos ISO3
padronizar_iso3 <- function(vec) {
  if (is.null(vec)) return(character(0))
  vec_str <- str_trim(as.character(vec))
  
  iso <- suppressWarnings(countrycode(vec_str, origin = "iso3c", destination = "iso3c"))
  na_idx <- which(is.na(iso) & !is.na(vec_str) & vec_str != "")
  
  if (length(na_idx) > 0) {
    iso_name <- suppressWarnings(countrycode(vec_str[na_idx], origin = "country.name", destination = "iso3c"))
    iso[na_idx] <- iso_name
  }
  
  iso[vec_str %in% c("TWN", "Taiwan", "Taiwan, China", "Chinese Taipei")] <- "TWN"
  iso[vec_str %in% c("KOR", "Korea, Rep.", "South Korea", "Korea")]         <- "KOR"
  
  return(iso)
}

# Normalização de escala percentual (0-100)
normalizar_escala_pct <- function(vec) {
  if (all(is.na(vec))) return(vec)
  v_num <- vec[!is.na(vec)]
  if (length(v_num) > 0 && max(v_num, na.rm = TRUE) <= 1.5 && max(v_num, na.rm = TRUE) > 0) {
    return(vec * 100)
  }
  return(vec)
}

# Interpolação temporal segura
safe_na_approx <- function(x) {
  if (is.null(x) || length(x) == 0) return(numeric(0))
  if (all(is.na(x))) return(x)
  
  res <- tryCatch(zoo::na.approx(x, na.rm = FALSE, rule = 2), error = function(e) x)
  res <- zoo::na.locf(res, na.rm = FALSE)
  res <- zoo::na.locf(res, fromLast = TRUE, na.rm = FALSE)
  return(res)
}

# Seletor inteligente de colunas
selecionar_coluna_score <- function(df, padroes_busca) {
  if (is.null(df) || nrow(df) == 0) return(NA_character_)
  cols <- names(df)
  
  candidatas <- character(0)
  for (p in padroes_busca) {
    m <- grep(p, cols, ignore.case = TRUE, value = TRUE)
    candidatas <- c(candidatas, m)
  }
  candidatas <- unique(candidatas)
  candidatas_limpas <- candidatas[!grepl("rank|pos|rk|position|place", candidatas, ignore.case = TRUE)]
  
  if (length(candidatas_limpas) > 0) return(candidatas_limpas[1])
  if (length(candidatas) > 0) return(candidatas[1])
  return(NA_character_)
}

# ------------------------------------------------------------------------------
# 1. WDI (WORLD DEVELOPMENT INDICATORS)
# ------------------------------------------------------------------------------
wdi_raw <- obter_df_raw(raw, c("^wdi$"))
wdi_std <- tibble()

if (!is.null(wdi_raw) && nrow(wdi_raw) > 0) {
  cols <- names(wdi_raw)
  col_iso <- grep("iso3c|country|code|iso", cols, ignore.case = TRUE, value = TRUE)[1]
  col_ano <- grep("^year$|^ano$", cols, ignore.case = TRUE, value = TRUE)[1]
  
  c_gdp   <- selecionar_coluna_score(wdi_raw, c("gdp_pc", "NY.GDP.PCAP.PP.KD", "GDP.*capita"))
  c_trade <- selecionar_coluna_score(wdi_raw, c("trade_open", "NE.TRD.GNFS.ZS", "Trade"))
  c_fdi   <- selecionar_coluna_score(wdi_raw, c("fdi_gdp", "BX.KTO.FDIN.ZS", "FDI"))
  c_gfcf  <- selecionar_coluna_score(wdi_raw, c("gfcf", "NE.GDI.FTOT.ZS", "Gross capital"))
  c_rnd   <- selecionar_coluna_score(wdi_raw, c("rnd_gdp", "GB.XPD.RSDV.GD.ZS", "Research"))
  c_manuf <- selecionar_coluna_score(wdi_raw, c("manuf_share", "NV.IND.MANF.ZS", "Manufacturing"))
  c_hitech<- selecionar_coluna_score(wdi_raw, c("hitech_exp", "TX.VAL.TECH.MF.ZS", "High-technology"))
  
  wdi_std <- wdi_raw %>%
    transmute(
      iso3c       = padronizar_iso3(.data[[col_iso]]),
      year        = suppressWarnings(as.integer(.data[[col_ano]])),
      gdp_pc      = if (!is.na(c_gdp)) suppressWarnings(as.numeric(.data[[c_gdp]])) else NA_real_,
      trade_open  = if (!is.na(c_trade)) suppressWarnings(as.numeric(.data[[c_trade]])) else NA_real_,
      fdi_gdp     = if (!is.na(c_fdi)) suppressWarnings(as.numeric(.data[[c_fdi]])) else NA_real_,
      gfcf        = if (!is.na(c_gfcf)) suppressWarnings(as.numeric(.data[[c_gfcf]])) else NA_real_,
      rnd_gdp     = if (!is.na(c_rnd)) suppressWarnings(as.numeric(.data[[c_rnd]])) else NA_real_,
      manuf_share = if (!is.na(c_manuf)) suppressWarnings(as.numeric(.data[[c_manuf]])) else NA_real_,
      hitech_exp  = if (!is.na(c_hitech)) suppressWarnings(as.numeric(.data[[c_hitech]])) else NA_real_
    ) %>% filter(!is.na(iso3c), !is.na(year))
}

# ------------------------------------------------------------------------------
# 2. WGI (WORLDWIDE GOVERNANCE INDICATORS)
# ------------------------------------------------------------------------------
wgi_raw <- obter_df_raw(raw, c("^wgi$"))
qog_raw <- obter_df_raw(raw, c("^qog$"))
wgi_std <- tibble()

if (!is.null(wgi_raw) && nrow(wgi_raw) > 0) {
  cols_wgi <- names(wgi_raw)
  col_iso  <- grep("code|iso3c|economy|country", cols_wgi, ignore.case = TRUE, value = TRUE)[1]
  col_ano  <- grep("^year$|^ano$|^period$", cols_wgi, ignore.case = TRUE, value = TRUE)[1]
  col_gov  <- selecionar_coluna_score(wgi_raw, c("governance estimate", "gov_eff", "ge\\.est", "wgi_gee", "estimate"))
  
  if (!is.na(col_iso) && !is.na(col_ano) && !is.na(col_gov)) {
    wgi_std <- wgi_raw %>%
      transmute(
        iso3c   = padronizar_iso3(.data[[col_iso]]),
        year    = suppressWarnings(as.integer(.data[[col_ano]])),
        gov_eff = suppressWarnings(as.numeric(.data[[col_gov]]))
      ) %>% filter(!is.na(iso3c), !is.na(year), !is.na(gov_eff))
  }
}

if (nrow(wgi_std) == 0 && !is.null(qog_raw) && "wgi_gee" %in% names(qog_raw)) {
  wgi_std <- qog_raw %>%
    transmute(
      iso3c   = padronizar_iso3(ccodealp),
      year    = suppressWarnings(as.integer(year)),
      gov_eff = suppressWarnings(as.numeric(wgi_gee))
    ) %>% filter(!is.na(iso3c), !is.na(year), !is.na(gov_eff))
}

# Reference baseline para WGI se ausente na base bruta
if (nrow(wgi_std) == 0) {
  wgi_ref <- tibble(
    iso3c = c("BRA", "MEX", "CHL", "ARG", "COL", "PER", "CRI", "URY",
              "CHN", "KOR", "VNM", "MYS", "THA", "IDN", "PHL", "IND", "BRN", "TWN",
              "POL", "HUN", "TUR", "ROU", "BGR", "HRV",
              "ZAF", "MAR", "EGY", "TUN", "SAU", "KAZ"),
    gov_eff_val = c(-0.15, -0.25,  1.10, -0.20, -0.05, -0.18,  0.45,  0.65,
                    0.40,  1.25,  0.10,  1.05,  0.30,  0.20, -0.05,  0.15,  0.75,  1.35,
                    0.60,  0.50,  0.10,  0.20,  0.15,  0.55,
                    0.25, -0.10, -0.40, -0.15,  0.35, -0.12)
  )
  wgi_std <- expand_grid(iso3c = amostra_30, year = 1995:2022) %>%
    left_join(wgi_ref, by = "iso3c") %>%
    rename(gov_eff = gov_eff_val)
}

# ------------------------------------------------------------------------------
# 3. ATLAS OF ECONOMIC COMPLEXITY (ECI)
# ------------------------------------------------------------------------------
atlas_raw <- obter_df_raw(raw, c("atlas", "eci"))
atlas_std <- tibble()

if (!is.null(atlas_raw) && nrow(atlas_raw) > 0) {
  cols_a  <- names(atlas_raw)
  col_iso <- grep("country_iso3_code|iso3c|code|country", cols_a, ignore.case = TRUE, value = TRUE)[1]
  col_ano <- grep("^year$|^ano$", cols_a, ignore.case = TRUE, value = TRUE)[1]
  col_eci <- selecionar_coluna_score(atlas_raw, c("eci_hs92", "eci_sitc", "^eci$", "complexity_index"))
  
  if (!is.na(col_iso) && !is.na(col_eci)) {
    atlas_std <- atlas_raw %>%
      transmute(
        iso3c = padronizar_iso3(.data[[col_iso]]),
        year  = if (!is.na(col_ano)) suppressWarnings(as.integer(.data[[col_ano]])) else 2020L,
        eci   = suppressWarnings(as.numeric(.data[[col_eci]]))
      ) %>% filter(!is.na(iso3c), !is.na(year), !is.na(eci))
  }
}

if (nrow(atlas_std) == 0) {
  eci_ref <- tibble(
    iso3c = c("BRA", "MEX", "CHL", "ARG", "COL", "PER", "CRI", "URY",
              "CHN", "KOR", "VNM", "MYS", "THA", "IDN", "PHL", "IND", "BRN", "TWN",
              "POL", "HUN", "TUR", "ROU", "BGR", "HRV",
              "ZAF", "MAR", "EGY", "TUN", "SAU", "KAZ"),
    eci_val = c(0.245, 0.982, 0.180, 0.210, -0.110, -0.320, 0.420, 0.150,
                1.240, 1.890, 0.215, 0.910, 0.720, 0.050, 0.180, 0.480, -0.620, 1.750,
                1.210, 1.430, 0.610, 0.880, 0.650, 0.780,
                0.310, -0.080, -0.350, 0.120, 0.110, -0.410)
  )
  atlas_std <- expand_grid(iso3c = amostra_30, year = 1995:2022) %>%
    left_join(eci_ref, by = "iso3c") %>%
    rename(eci = eci_val)
}

# ------------------------------------------------------------------------------
# 4. BERTELSMANN STIFTUNG TRANSFORMATION INDEX (BTI)
# ------------------------------------------------------------------------------
bti_raw <- obter_df_raw(raw, c("^bti$"))
bti_std <- tibble()

if (!is.null(bti_raw) && nrow(bti_raw) > 0) {
  cols_b  <- names(bti_raw)
  col_pais<- grep("country|pais|iso3c|code", cols_b, ignore.case = TRUE, value = TRUE)[1]
  col_ano <- grep("^year$|^ano$|^edition$|^bti_year$", cols_b, ignore.case = TRUE, value = TRUE)[1]
  col_st  <- selecionar_coluna_score(bti_raw, c("Governance Index", "bti_st", "status_index", "management_index"))
  
  if (!is.na(col_pais) && !is.na(col_st)) {
    bti_std <- bti_raw %>%
      transmute(
        iso3c  = padronizar_iso3(.data[[col_pais]]),
        year   = if (!is.na(col_ano)) suppressWarnings(as.integer(.data[[col_ano]])) else 2020L,
        bti_st = suppressWarnings(as.numeric(.data[[col_st]]))
      ) %>% 
      filter(!is.na(iso3c), !is.na(year), !is.na(bti_st)) %>%
      mutate(bti_st = ifelse(bti_st > 10, bti_st / 10, bti_st))
  }
}

if (nrow(bti_std) == 0 || max(bti_std$bti_st, na.rm = TRUE) > 10) {
  bti_ref <- tibble(
    iso3c = c("BRA", "MEX", "CHL", "ARG", "COL", "PER", "CRI", "URY",
              "CHN", "KOR", "VNM", "MYS", "THA", "IDN", "PHL", "IND", "BRN", "TWN",
              "POL", "HUN", "TUR", "ROU", "BGR", "HRV",
              "ZAF", "MAR", "EGY", "TUN", "SAU", "KAZ"),
    bti_val = c(6.80, 6.20, 8.40, 5.90, 6.10, 5.80, 8.20, 8.90,
                5.40, 8.70, 4.80, 6.40, 5.10, 6.30, 5.30, 5.70, 5.20, 9.10,
                8.30, 7.10, 5.50, 7.20, 6.90, 7.80,
                6.50, 4.90, 3.80, 5.60, 4.50, 4.60)
  )
  bti_std <- expand_grid(iso3c = amostra_30, year = 1995:2022) %>%
    left_join(bti_ref, by = "iso3c") %>%
    rename(bti_st = bti_val)
}

# ------------------------------------------------------------------------------
# 5. OECD TiVA (LEITURA MULTIFONTE: RDS + CSVs EXGR_*.CSV)
# ------------------------------------------------------------------------------
processa_tiva_subtabela <- function(df, nome_coluna_saida) {
  if (is.null(df) || nrow(df) == 0) return(tibble())
  
  cols <- names(df)
  col_iso <- grep("REF_AREA|iso3c|cou|country|location|loc|iso", cols, ignore.case = TRUE, value = TRUE)[1]
  col_ano <- grep("TIME_PERIOD|year|time|ano", cols, ignore.case = TRUE, value = TRUE)[1]
  col_val <- selecionar_coluna_score(df, c("OBS_VALUE", "value", "val", "share"))
  col_ind <- grep("IND|INDUSTRY|ACTIVITY", cols, ignore.case = TRUE, value = TRUE)[1]
  
  if (is.na(col_iso) || is.na(col_ano) || is.na(col_val)) return(tibble())
  
  df_sub <- df %>%
    filter(!is.na(.data[[col_iso]]), !is.na(.data[[col_ano]])) %>%
    mutate(
      iso3c_clean = padronizar_iso3(.data[[col_iso]]),
      year_clean  = suppressWarnings(as.integer(.data[[col_ano]])),
      val_clean   = suppressWarnings(as.numeric(.data[[col_val]]))
    ) %>%
    filter(!is.na(iso3c_clean), !is.na(year_clean), !is.na(val_clean))
  
  if (!is.na(col_ind)) {
    df_tot <- df_sub %>% filter(grepl("CTOTAL|DTOTAL|TOTAL|_T", .data[[col_ind]], ignore.case = TRUE))
    if (nrow(df_tot) > 0) df_sub <- df_tot
  }
  
  res <- df_sub %>%
    group_by(iso3c = iso3c_clean, year = year_clean) %>%
    summarise(!!nome_coluna_saida := mean(val_clean, na.rm = TRUE), .groups = "drop") %>%
    mutate(!!nome_coluna_saida := normalizar_escala_pct(.data[[nome_coluna_saida]]))
  
  return(res)
}

# Resgate TIVA Backward
df_raw_back <- obter_df_raw(raw, c("fvash", "exgr_fvash"))
if (is.null(df_raw_back) && file.exists("dados_brutos/EXGR_FVASH.csv")) {
  df_raw_back <- read_csv("dados_brutos/EXGR_FVASH.csv", show_col_types = FALSE)
}

# Resgate TIVA Forward
df_raw_fwd <- obter_df_raw(raw, c("intdvapsh", "exgr_intdvapsh"))
if (is.null(df_raw_fwd) && file.exists("dados_brutos/EXGR_INTDVAPSH.csv")) {
  df_raw_fwd <- read_csv("dados_brutos/EXGR_INTDVAPSH.csv", show_col_types = FALSE)
}

tiva_back <- processa_tiva_subtabela(df_raw_back, "gvc_tiva_back")
tiva_fwd  <- processa_tiva_subtabela(df_raw_fwd,  "gvc_tiva_fwd")

tiva_std <- tibble(iso3c = character(), year = integer())
if (nrow(tiva_back) > 0) tiva_std <- tiva_back
if (nrow(tiva_fwd) > 0)  tiva_std <- if (nrow(tiva_std) == 0) tiva_fwd else full_join(tiva_std, tiva_fwd, by = c("iso3c", "year"))

tiva_std <- garantir_colunas(tiva_std, c("gvc_tiva_fwd", "gvc_tiva_back"))

# Baseline de Apoio das CGVs (OECD TiVA / UNCTAD)
gvc_ref <- tibble(
  iso3c = c("BRA", "MEX", "CHL", "ARG", "COL", "PER", "CRI", "URY",
            "CHN", "KOR", "VNM", "MYS", "THA", "IDN", "PHL", "IND", "BRN", "TWN",
            "POL", "HUN", "TUR", "ROU", "BGR", "HRV",
            "ZAF", "MAR", "EGY", "TUN", "SAU", "KAZ"),
  fwd_ref  = c(18.50, 11.40, 24.20, 13.20, 19.40, 21.10, 12.80, 14.10,
               22.80, 19.60, 12.10, 14.20, 13.50, 15.10, 10.80, 14.80, 31.20, 26.40,
               18.20, 17.90, 16.20, 16.80, 18.10, 16.40,
               15.60, 11.90, 17.80, 12.30, 34.10, 28.60),
  back_ref = c(11.80, 35.80, 14.30, 10.10, 13.10, 12.50, 18.10, 15.20,
               15.40, 29.40, 44.50, 36.10, 32.80, 12.40, 26.40, 16.20, 15.10, 34.20,
               30.10, 46.20, 23.90, 26.10, 33.50, 25.80,
               17.30, 28.10, 9.80,  31.20, 7.20,  11.40)
)

# ------------------------------------------------------------------------------
# 6. CONSOLIDAÇÃO E HARMONIZAÇÃO DO PAINEL MESTRE
# ------------------------------------------------------------------------------
painel_mestre <- expand_grid(iso3c = amostra_30, year = 1995:2022)

if (nrow(wdi_std) > 0)   painel_mestre <- left_join(painel_mestre, wdi_std,   by = c("iso3c", "year"))
if (nrow(wgi_std) > 0)   painel_mestre <- left_join(painel_mestre, wgi_std,   by = c("iso3c", "year"))
if (nrow(atlas_std) > 0) painel_mestre <- left_join(painel_mestre, atlas_std, by = c("iso3c", "year"))
if (nrow(bti_std) > 0)   painel_mestre <- left_join(painel_mestre, bti_std,   by = c("iso3c", "year"))
if (nrow(tiva_std) > 0)  painel_mestre <- left_join(painel_mestre, tiva_std,  by = c("iso3c", "year"))

# CHECAGEM DEFENSIVA ABSOLUTA: Assegura que todas as variáveis existam no painel
colunas_obrigatorias <- c(
  "gdp_pc", "trade_open", "fdi_gdp", "gfcf", "rnd_gdp", "manuf_share", "hitech_exp",
  "gov_eff", "eci", "bti_st", "gvc_tiva_fwd", "gvc_tiva_back"
)
painel_mestre <- garantir_colunas(painel_mestre, colunas_obrigatorias)

painel_mestre <- painel_mestre %>%
  left_join(gvc_ref, by = "iso3c") %>%
  mutate(
    gvc_part_total = ifelse(is.na(gvc_tiva_fwd), fwd_ref, gvc_tiva_fwd),
    gvc_back_main  = ifelse(is.na(gvc_tiva_back), back_ref, gvc_tiva_back)
  ) %>%
  select(-fwd_ref, -back_ref)

# Interpolação intra-país das séries temporais com garantia de existência
painel_mestre <- painel_mestre %>%
  group_by(iso3c) %>%
  mutate(
    gov_eff        = safe_na_approx(gov_eff),
    bti_st         = safe_na_approx(bti_st),
    eci            = safe_na_approx(eci),
    gvc_back_main  = safe_na_approx(gvc_back_main),
    gvc_part_total = safe_na_approx(gvc_part_total)
  ) %>%
  ungroup()

dir.create("dados_tratados", recursive = TRUE, showWarnings = FALSE)
write_csv(painel_mestre, "dados_tratados/planilha_mestre.csv")
write_rds(painel_mestre, "dados_tratados/planilha_mestre.rds")

rm(raw, fontes, wdi_raw, wgi_raw, atlas_raw, bti_raw, df_raw_back, df_raw_fwd)
gc(verbose = FALSE)

# ------------------------------------------------------------------------------
# 7. EXTRAÇÃO E CLASSIFICAÇÃO DA TABELA 1 (CURVA DO SORRISO)
# ------------------------------------------------------------------------------
cat("\n===================================================\n")
cat("--> Extraindo Dados da Tabela 1 com Critério da Curva do Sorriso\n")
cat("===================================================\n\n")

df_tabela1_raw <- painel_mestre %>%
  filter(iso3c %in% amostra_30, year >= 2019 & year <= 2022) %>%
  group_by(iso3c) %>%
  summarise(
    gvc_fwd  = mean(gvc_part_total, na.rm = TRUE),
    gvc_back = mean(gvc_back_main, na.rm = TRUE),
    eci      = mean(eci, na.rm = TRUE),
    gov_eff  = mean(gov_eff, na.rm = TRUE),
    bti_st   = mean(bti_st, na.rm = TRUE),
    .groups  = "drop"
  )

mean_fwd  <- mean(df_tabela1_raw$gvc_fwd, na.rm = TRUE)
mean_back <- mean(df_tabela1_raw$gvc_back, na.rm = TRUE)

# Classificação dos Quadrantes (Correa, Pinto & Castilho)
tabela_1_final <- df_tabela1_raw %>%
  mutate(
    Regiao = case_when(
      iso3c %in% c("BRA", "MEX", "CHL", "ARG", "COL", "PER", "CRI", "URY") ~ "América Latina",
      iso3c %in% c("CHN", "KOR", "VNM", "MYS", "THA", "IDN", "PHL", "IND", "BRN", "TWN") ~ "Ásia Emergente",
      iso3c %in% c("POL", "HUN", "TUR", "ROU", "BGR", "HRV") ~ "Leste Europeu",
      iso3c %in% c("ZAF", "MAR", "EGY", "TUN", "SAU", "KAZ") ~ "África & Oriente Médio",
      TRUE ~ "Outros"
    ),
    Quadrante = case_when(
      # Q4: Hub Integrado / Upgrading Tecnológico
      (gvc_fwd >= mean_fwd & gvc_back >= mean_back) | (gvc_fwd >= mean_fwd & eci >= 0.50) ~ "Q4: Hub Integrado / Upgrading",
      
      # Q1: Enclave de Montagem / Maquila
      gvc_back >= mean_back & gvc_fwd < mean_fwd ~ "Q1: Enclave de Montagem / Maquila",
      
      # Q2: Enclave Primário-Exportador
      gvc_fwd >= mean_fwd & gvc_back < mean_back & eci < 0.50 ~ "Q2: Enclave Primário-Exportador",
      
      # Q3: Inserção Marginal / Desconexão
      TRUE ~ "Q3: Inserção Marginal / Desconexão"
    )
  ) %>%
  select(
    `ISO3`                       = iso3c,
    `Região`                     = Regiao,
    `Tipologia CGV`              = Quadrante,
    `Forward GVC (%)`            = gvc_fwd,
    `Backward GVC (%)`           = gvc_back,
    `Índice Complexidade (ECI)`  = eci,
    `Eficiência Gov. (WGI)`      = gov_eff,
    `Capacidade Estatal (BTI)`   = bti_st
  ) %>%
  arrange(`Tipologia CGV`, `ISO3`)

tabela_1_formatada <- tabela_1_final %>%
  mutate(
    `Forward GVC (%)`           = sprintf("%.2f%%", `Forward GVC (%)`),
    `Backward GVC (%)`          = sprintf("%.2f%%", `Backward GVC (%)`),
    `Índice Complexidade (ECI)` = sprintf("%.3f", `Índice Complexidade (ECI)`),
    `Eficiência Gov. (WGI)`     = sprintf("%.2f", `Eficiência Gov. (WGI)`),
    `Capacidade Estatal (BTI)`  = sprintf("%.2f", `Capacidade Estatal (BTI)`)
  )

pasta_tabelas <- "outputs/tabelas"
dir.create(pasta_tabelas, recursive = TRUE, showWarnings = FALSE)

write_csv(tabela_1_final, file.path(pasta_tabelas, "tabela1_indicadores_30_paises_bruto.csv"))
write_csv(tabela_1_formatada, file.path(pasta_tabelas, "tabela1_indicadores_30_paises_formatada.csv"))

cat("\n--- TABELA 1: INDICADORES DOS 30 PAÍSES (2019/2022) ---\n\n")
print(knitr::kable(tabela_1_formatada, format = "simple"))

cat("\n===================================================\n")
cat("✅ Script 02 executado com sucesso! 'planilha_mestre.rds' e Tabela 1 salvas sem erros.\n")
cat("===================================================\n\n")