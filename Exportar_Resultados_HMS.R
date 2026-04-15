library(tidyverse)
library(xml2)
library(stringr)
library(writexl)

# ============================================================
# 1) Selecionar pasta de origem
# ============================================================

pasta_origem <- choose.dir(caption = "Selecione a pasta com os arquivos .results")

if (is.na(pasta_origem)) stop("Nenhuma pasta foi selecionada.")

# ============================================================
# 2) Listar arquivos
# ============================================================

arquivos <- list.files(
  path = pasta_origem,
  pattern = "^RUN_.*\\.results$",
  recursive = TRUE,
  full.names = TRUE
)

if (length(arquivos) == 0) {
  stop("Nenhum arquivo .results foi encontrado na pasta selecionada.")
}

cat("Arquivos encontrados:", length(arquivos), "\n\n")

# ============================================================
# 3) Função para extrair todos os resultados de um arquivo
# ============================================================

extrair_resultados_hms <- function(arquivo) {
  
  doc <- read_xml(arquivo)
  elementos <- xml_find_all(doc, "//RunResults/BasinElement")
  
  resultados <- map_dfr(elementos, function(el) {
    
    nome_elemento <- xml_attr(el, "name")
    tipo_elemento <- xml_attr(el, "type")
    
    medidas <- xml_find_all(el, ".//Statistics/StatisticMeasure")
    
    if (length(medidas) == 0) return(NULL)
    
    tibble(
      arquivo  = basename(arquivo),
      elemento = nome_elemento,
      tipo     = tipo_elemento,
      medida   = xml_attr(medidas, "type"),
      valor    = xml_attr(medidas, "value"),
      unidade  = xml_attr(medidas, "units")
    )
  })
  
  resultados
}

# ============================================================
# 4) Extrair todos os arquivos
# ============================================================

resultados_long <- map_dfr(arquivos, extrair_resultados_hms)

# ============================================================
# 5) Limpar nome do arquivo e separar TR / duração / unidade
# ============================================================

resultados_long <- resultados_long %>%
  mutate(
    arquivo = str_remove(arquivo, "^RUN_"),
    arquivo = str_remove(arquivo, "\\.results$")
  ) %>%
  separate(
    arquivo,
    into = c("TR", "duracao_valor", "duracao_unidade"),
    sep = "_",
    remove = FALSE
  ) %>%
  mutate(
    duracao_num = suppressWarnings(as.numeric(duracao_valor)),
    duracao = case_when(
      duracao_unidade == "min" ~ paste0(duracao_valor, " min"),
      duracao_unidade == "h"   ~ paste0(duracao_valor, " h"),
      duracao_unidade == "dia" ~ paste0(duracao_valor, " dia"),
      TRUE ~ paste(duracao_valor, duracao_unidade)
    )
  )

# ============================================================
# 6) Criar coluna numérica apenas para medidas não temporais
# ============================================================

resultados_long <- resultados_long %>%
  mutate(
    valor_num = if_else(
      str_detect(medida, "Time"),
      NA_real_,
      suppressWarnings(as.numeric(valor))
    )
  )

# ============================================================
# 7) Ordenar durações para análise
# ============================================================

ordem_duracoes <- resultados_long %>%
  distinct(duracao, duracao_unidade, duracao_num) %>%
  mutate(
    ordem = case_when(
      duracao_unidade == "min" ~ duracao_num,
      duracao_unidade == "h"   ~ duracao_num * 60,
      duracao_unidade == "dia" ~ duracao_num * 1440,
      TRUE ~ Inf
    )
  ) %>%
  arrange(ordem) %>%
  pull(duracao)

resultados_long <- resultados_long %>%
  mutate(duracao = factor(duracao, levels = ordem_duracoes))

# ============================================================
# 8) Gerar aba geral com todos os dados
# ============================================================

aba_geral <- resultados_long %>%
  arrange(TR, tipo, elemento, medida, duracao)

# ============================================================
# 9) Gerar uma aba por TR para comparação entre durações
# ============================================================

lista_abas_tr <- resultados_long %>%
  filter(!is.na(valor_num)) %>%
  group_split(TR) %>%
  set_names(unique(resultados_long$TR)) %>%
  map(function(df_tr) {
    
    df_tr %>%
      select(TR, tipo, elemento, medida, unidade, duracao, valor_num) %>%
      distinct() %>%
      pivot_wider(
        names_from = duracao,
        values_from = valor_num
      ) %>%
      arrange(tipo, elemento, medida)
  })

# ============================================================
# 10) Aba adicional para medidas de tempo
# ============================================================

aba_tempos <- resultados_long %>%
  filter(str_detect(medida, "Time")) %>%
  select(TR, duracao, tipo, elemento, medida, valor, unidade) %>%
  arrange(TR, tipo, elemento, medida, duracao)

# ============================================================
# 11) Aba para listar elementos disponíveis
# ============================================================

aba_elementos <- resultados_long %>%
  distinct(tipo, elemento) %>%
  arrange(tipo, elemento)

# ============================================================
# 12) Montar lista final de abas
# ============================================================

lista_exportacao <- c(
  list(
    Geral = aba_geral,
    Elementos = aba_elementos,
    Tempos = aba_tempos
  ),
  lista_abas_tr
)

# ============================================================
# 13) Salvar Excel na pasta de origem
# ============================================================

timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M")

nome_arquivo_saida <- paste0("resultados_HMS_por_TR_", timestamp, ".xlsx")
caminho_saida <- file.path(pasta_origem, nome_arquivo_saida)

write_xlsx(lista_exportacao, caminho_saida)

cat("Arquivo salvo em:\n", caminho_saida, "\n")