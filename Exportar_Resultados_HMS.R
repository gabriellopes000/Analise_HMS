################################################################################
# EXTRATOR DE RESULTADOS HEC-HMS - VERSAO 2 - 11/2026
# Leitura de arquivos RUN_*.results e exportacao para Excel
# Execucao: selecione todo o script no RStudio e execute.
################################################################################

# 1. Pacotes ---------------------------------------------------------------
pacotes <- c("dplyr", "tidyr", "purrr", "stringr", "tibble", "xml2", "writexl")
for (p in pacotes) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}

# Usa nomes qualificados para evitar conflitos entre pacotes.

# ============================================================
# 1) Selecionar pasta de origem
# ============================================================

if (!requireNamespace("rstudioapi", quietly = TRUE)) {
  install.packages("rstudioapi")
}

pasta_origem <- rstudioapi::selectDirectory(
  caption = "Selecione a pasta com os resultados do HEC-HMS"
)

if (is.null(pasta_origem) || !nzchar(pasta_origem)) {
  stop("Nenhuma pasta foi selecionada.")
}

arquivos <- list.files(
  path = pasta_origem,
  pattern = "^RUN_.*\\.results$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)
if (!length(arquivos)) stop("Nenhum arquivo RUN_*.results foi encontrado.")
message("Arquivos encontrados: ", length(arquivos))

# 3. Identificar TR, duracao e quartil --------------------------------------
# Aceita, por exemplo:
# RUN_100_24_h.results
# RUN_TT - 100A - - 24 h - Q4.results
# RUN_TT_-_100A_-_-_24_h_-_Q4.results
# RUN_TT_-_PMP_majorada_-_-_5_d_-_Q4.results
interpretar_nome <- function(arquivo) {
  base <- basename(arquivo)
  evento <- stringr::str_remove(base, "(?i)^RUN_")
  evento <- stringr::str_remove(evento, "(?i)\\.results$")
  normal <- stringr::str_replace_all(evento, "_", " ")
  normal <- stringr::str_replace_all(normal, "-", " ")
  normal <- stringr::str_squish(normal)
  
  padrao <- paste0(
    "(?i)(?:^|\\s)",
    "(PMP\\s+majorada|PMP|[0-9]+\\s*A?)",
    "\\s+([0-9]+(?:[.,][0-9]+)?)\\s*(min|m|h|d|dia|dias)\\b"
  )
  grupos <- stringr::str_match(normal, padrao)
  tr <- grupos[1, 2]
  duracao_valor <- grupos[1, 3]
  unidade <- tolower(grupos[1, 4])
  quartil <- stringr::str_match(normal, "(?i)\\b(Q[1-4])\\b")[1, 2]
  
  if (!is.na(tr)) {
    tr <- stringr::str_squish(tr)
    if (grepl("^PMP", tr, ignore.case = TRUE)) {
      tr <- if (grepl("majorada", tr, ignore.case = TRUE)) "PMP majorada" else "PMP"
    } else {
      tr <- gsub("\\s*A$", "", tr, ignore.case = TRUE)
    }
  }
  minutos <- suppressWarnings(as.numeric(gsub(",", ".", duracao_valor)))
  if (!is.na(unidade)) {
    minutos <- minutos * switch(unidade,
                                "min" = 1, "m" = 1, "h" = 60,
                                "d" = 1440, "dia" = 1440, "dias" = 1440)
  }
  duracao <- if (!is.na(duracao_valor)) {
    unid_fmt <- if (unidade %in% c("min", "m")) "min" else if (unidade == "h") "h" else "d"
    paste(duracao_valor, unid_fmt)
  } else NA_character_
  
  tibble::tibble(
    arquivo = base, evento = evento, TR = tr, duracao = duracao,
    duracao_min = minutos, quartil = toupper(quartil)
  )
}

# 4. Ler XML, preservando falhas em vez de interromper todo o lote ----------
extrair_resultados_hms <- function(arquivo) {
  tryCatch({
    doc <- xml2::read_xml(arquivo)
    elementos <- xml2::xml_find_all(doc, "//RunResults/BasinElement")
    if (!length(elementos)) {
      stop("Nao foram encontrados elementos em //RunResults/BasinElement")
    }
    dados <- purrr::map_dfr(elementos, function(el) {
      medidas <- xml2::xml_find_all(el, ".//Statistics/StatisticMeasure")
      if (!length(medidas)) return(NULL)
      tibble::tibble(
        arquivo = basename(arquivo),
        tipo = xml2::xml_attr(el, "type"),
        elemento = xml2::xml_attr(el, "name"),
        medida = xml2::xml_attr(medidas, "type"),
        valor = xml2::xml_attr(medidas, "value"),
        unidade = xml2::xml_attr(medidas, "units")
      )
    })
    if (!nrow(dados)) stop("Nenhuma StatisticMeasure encontrada")
    list(dados = dados, erro = NULL)
  }, error = function(e) {
    list(dados = NULL,
         erro = tibble::tibble(arquivo = arquivo, problema = conditionMessage(e)))
  })
}

leituras <- purrr::map(arquivos, extrair_resultados_hms)
erros <- dplyr::bind_rows(purrr::map(leituras, "erro"))
resultados <- dplyr::bind_rows(purrr::map(leituras, "dados"))
if (!nrow(resultados)) stop("Nenhum resultado extraido. Verifique os arquivos e a estrutura XML.")

# 5. Metadados dos eventos e classificacao dos valores ----------------------
meta <- dplyr::bind_rows(purrr::map(arquivos, interpretar_nome))
nao_identificados <- dplyr::filter(meta, is.na(TR) | is.na(duracao_min))
if (nrow(nao_identificados)) {
  message("Aviso: ", nrow(nao_identificados),
          " arquivo(s) com TR/duracao nao identificados. Consulte 'Avisos'.")
}

resultados <- dplyr::left_join(resultados, meta, by = "arquivo") |>
  dplyr::mutate(
    medida_temporal = stringr::str_detect(
      dplyr::coalesce(medida, ""), stringr::regex("time|date|hora|data", ignore_case = TRUE)
    ),
    valor_num = dplyr::if_else(
      medida_temporal, NA_real_,
      suppressWarnings(as.numeric(gsub(",", ".", valor)))
    ),
    # O criterio exige que o nome da medida indique vazao OU que
    # a unidade de vazao esteja acompanhada de indicacao de maximo/pico.
    medida_pico_vazao = !medida_temporal & (
      (stringr::str_detect(dplyr::coalesce(medida, ""),
                           stringr::regex("peak|max|maximum|pico", ignore_case = TRUE)) &
         stringr::str_detect(dplyr::coalesce(paste(medida, unidade), ""),
                             stringr::regex("flow|discharge|outflow|inflow|vaz[aã]o|m3/s|m\\^3/s|cms|cfs", ignore_case = TRUE)))
    )
  )

ordem_tr <- function(x) {
  dplyr::case_when(
    x == "PMP" ~ Inf - 1,
    x == "PMP majorada" ~ Inf,
    TRUE ~ suppressWarnings(as.numeric(x))
  )
}

# 6. Abas auxiliares --------------------------------------------------------
aba_geral <- resultados |>
  dplyr::mutate(ordem_TR = ordem_tr(TR)) |>
  dplyr::arrange(ordem_TR, duracao_min, tipo, elemento, medida) |>
  dplyr::select(-ordem_TR)

aba_elementos <- resultados |>
  dplyr::distinct(tipo, elemento) |>
  dplyr::arrange(tipo, elemento)

aba_tempos <- resultados |>
  dplyr::filter(medida_temporal) |>
  dplyr::select(TR, duracao, quartil, tipo, elemento, medida, valor, unidade, arquivo) |>
  dplyr::arrange(TR, tipo, elemento, medida, duracao)

# Uma linha por evento/elemento/medida de pico identificada.
aba_picos <- resultados |>
  dplyr::filter(medida_pico_vazao, !is.na(valor_num)) |>
  dplyr::select(TR, duracao, duracao_min, quartil, tipo, elemento,
                medida, valor_num, unidade, arquivo) |>
  dplyr::rename(vazao_pico = valor_num) |>
  dplyr::arrange(tipo, elemento, TR, duracao_min)

# Para cada TR, elemento e tipo de medida, seleciona a maior vazao.
# Em empates, mantem a menor duracao como criterio de desempate.
aba_criticas <- aba_picos |>
  dplyr::filter(!is.na(TR), !is.na(duracao_min)) |>
  dplyr::group_by(TR, tipo, elemento, medida, unidade) |>
  dplyr::arrange(dplyr::desc(vazao_pico), duracao_min, .by_group = TRUE) |>
  dplyr::slice(1L) |>
  dplyr::ungroup() |>
  dplyr::rename(duracao_critica = duracao, vazao_pico_maxima = vazao_pico) |>
  dplyr::select(TR, tipo, elemento, medida, unidade, vazao_pico_maxima,
                duracao_critica, duracao_min, quartil, arquivo) |>
  dplyr::mutate(ordem_TR = ordem_tr(TR)) |>
  dplyr::arrange(ordem_TR, tipo, elemento, medida) |>
  dplyr::select(-ordem_TR)

# 7. Abas por TR: duracoes nas colunas --------------------------------------
# Inclui a medida e unidade na chave para nao misturar grandezas.
# Caso haja multiplos eventos com a mesma duracao/medida, uma
# linha por arquivo evita a perda silenciosa dos valores.
montar_aba_tr <- function(df_tr) {
  x <- df_tr |>
    dplyr::filter(!is.na(valor_num), !is.na(duracao)) |>
    dplyr::select(TR, tipo, elemento, medida, unidade,
                  duracao, duracao_min, valor_num, arquivo)
  if (!nrow(x)) return(tibble::tibble(Observacao = "Nao ha resultados numericos para este TR"))
  
  # Se ha somente uma medicao por duracao/chave, as colunas ficam lado a lado.
  # Em casos repetidos, 'arquivo' e incluido na chave para evitar agregacao.
  chaves <- c("TR", "tipo", "elemento", "medida", "unidade", "duracao")
  repetido <- x |>
    dplyr::count(dplyr::across(dplyr::all_of(chaves))) |>
    dplyr::filter(n > 1L)
  if (nrow(repetido)) {
    x <- x |>
      dplyr::mutate(registro = dplyr::row_number())
    # Numeracao evita listas em celulas, sem substituir resultados diferentes.
    x <- x |>
      dplyr::group_by(dplyr::across(dplyr::all_of(chaves))) |>
      dplyr::mutate(repeticao = dplyr::row_number()) |>
      dplyr::ungroup()
    linhas <- c("TR", "tipo", "elemento", "medida", "unidade", "repeticao")
  } else {
    linhas <- c("TR", "tipo", "elemento", "medida", "unidade")
  }
  duracoes <- x |>
    dplyr::distinct(duracao, duracao_min) |>
    dplyr::arrange(duracao_min) |>
    dplyr::pull(duracao)
  x |>
    dplyr::select(dplyr::all_of(linhas), duracao, valor_num) |>
    tidyr::pivot_wider(names_from = duracao, values_from = valor_num) |>
    dplyr::select(dplyr::all_of(linhas), dplyr::any_of(duracoes)) |>
    dplyr::arrange(tipo, elemento, medida)
}

trs_validos <- resultados |>
  dplyr::filter(!is.na(TR)) |>
  dplyr::distinct(TR) |>
  dplyr::mutate(ordem = ordem_tr(TR)) |>
  dplyr::arrange(ordem) |>
  dplyr::pull(TR)

abas_tr <- purrr::map(trs_validos, function(tr) {
  montar_aba_tr(dplyr::filter(resultados, TR == tr))
})
names(abas_tr) <- paste0("TR_", trs_validos)

# 8. Aba de avisos ----------------------------------------------------------
avisos <- tibble::tibble(arquivo = character(), problema = character())
if (nrow(erros)) avisos <- dplyr::bind_rows(avisos, erros)
if (nrow(nao_identificados)) {
  avisos <- dplyr::bind_rows(
    avisos,
    dplyr::transmute(nao_identificados,
                     arquivo, problema = "Nome nao reconhecido: TR e/ou duracao ausentes")
  )
}
if (!nrow(aba_picos)) {
  avisos <- dplyr::bind_rows(
    avisos,
    tibble::tibble(arquivo = "(geral)",
                   problema = "Nenhuma medida de pico de vazao foi reconhecida. Verifique os nomes de 'medida' na aba Geral.")
  )
}
if (!nrow(avisos)) {
  avisos <- tibble::tibble(arquivo = "(geral)", problema = "Nenhum problema identificado")
}

# 9. Exportacao -------------------------------------------------------------
# Evita nomes invalidos de planilha e o limite de 31 caracteres.
nomes_validos_excel <- function(nomes) {
  nomes <- gsub("[\\[\\]\\*\\?/\\\\:]", "_", nomes)
  nomes <- substr(nomes, 1L, 31L)
  make.unique(nomes, sep = "_")
}

lista_exportacao <- c(
  list(
    Geral = aba_geral,
    Elementos = aba_elementos,
    Tempos = aba_tempos,
    Picos_por_Evento = aba_picos,
    Duracoes_Criticas = aba_criticas,
    Avisos = avisos
  ),
  abas_tr
)
names(lista_exportacao) <- nomes_validos_excel(names(lista_exportacao))

saida <- file.path(
  pasta_origem,
  paste0("Resultados_HEC_HMS_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".xlsx")
)
writexl::write_xlsx(lista_exportacao, path = saida)

message("\nProcessamento concluido.")
message("Arquivos encontrados: ", length(arquivos))
message("Arquivos lidos: ", length(arquivos) - nrow(erros))
message("Arquivos com erro: ", nrow(erros))
message("Registros extraidos: ", nrow(resultados))
message("Picos de vazao identificados: ", nrow(aba_picos))
message("Excel: ", saida)
if (.Platform$OS.type == "windows") shell.exec(normalizePath(saida, winslash = "\\"))
