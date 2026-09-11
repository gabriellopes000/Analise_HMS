################################################################################
# GERAÇÃO DE HIETOGRAMAS SINTÉTICOS - DISTRIBUIÇÃO DE HUFF
################################################################################
# Descrição: Gera hietogramas sintéticos usando curvas de Huff (Q1-Q4) para
#            diferentes tempos de retorno e cria arquivos para HEC-HMS
# Autor: Gabriel Lopes
# Data: 2024
################################################################################

# 1. CARREGAMENTO DE BIBLIOTECAS -----------------------------------------------
library(dssrip)
library(foreach)
library(stringr)
library(xts)
library(ggplot2)
library(dplyr)
library(matlib)
library(data.table)
library(readxl)
library(splines)
library(beepr)

# 2. CONFIGURAÇÃO DE CAMINHOS --------------------------------------------------

# Defina o diretório base do projeto
dir_base <- "C:\\Users\\Gabriel Lopes\\Desktop\\Projetos\\Minosa\\08_HMS\\01_Codigos\\Extrair_resultados_HMS\\files\\Teste"

# Caminhos dos arquivos
caminho_huff <- file.path(dir_base, "Dist_HUFF.xlsx")
caminho_quantis <- file.path(dir_base, "quantis_TET.xlsx")

# Configuração do projeto HEC-HMS
projectName <- "HMS_Minosa_Teste"
projectFolder <- paste0(file.path(dir_base, projectName), "/")
basin <- "Basin 1"

# Sub-bacias do projeto
subbasins <- c("AD1")

# 3. FUNÇÕES AUXILIARES --------------------------------------------------------

#' Função logística para ajuste de curvas
#' @param x Vetor de tempo (%)
#' @param L Assíntota superior
#' @param k Taxa de crescimento
#' @param x0 Ponto médio da curva
#' @return Valores ajustados pela função logística
logistic <- function(x, L, k, x0) {
  L / (1 + exp(-k * (x - x0)))
}

#' Ajusta modelos de distribuição de Huff para Q1, Q2, Q3 e Q4
#' @param dados Dataframe com colunas Tempo, Q1, Q2, Q3, Q4
#' @return Lista com os 4 modelos ajustados
ajustar_curvas_huff <- function(dados) {
  
  cat("Ajustando curvas de Huff...\n")
  
  modelos <- list()
  
  # Q1: Polinômio de 4º grau
  cat("  - Q1: Polinômio 4º grau\n")
  modelos$Q1 <- tryCatch({
    lm(Q1 ~ poly(Tempo, 4, raw = TRUE), data = dados)
  }, error = function(e) {
    stop("Erro ao ajustar Q1: ", e$message)
  })
  
  # Q2: Função logística
  cat("  - Q2: Função logística\n")
  modelos$Q2 <- tryCatch({
    nls(Q2 ~ logistic(Tempo, L, k, x0), 
        data = dados, 
        start = list(L = max(dados$Q2), k = 0.1, x0 = 50))
  }, error = function(e) {
    stop("Erro ao ajustar Q2: ", e$message)
  })
  
  # Q3: Função logística
  cat("  - Q3: Função logística\n")
  modelos$Q3 <- tryCatch({
    nls(Q3 ~ logistic(Tempo, L, k, x0), 
        data = dados, 
        start = list(L = max(dados$Q3), k = 0.1, x0 = 50))
  }, error = function(e) {
    stop("Erro ao ajustar Q3: ", e$message)
  })
  
  # Q4: Spline cúbico com nós específicos
  cat("  - Q4: Spline cúbico\n")
  modelos$Q4 <- tryCatch({
    lm(Q4 ~ bs(Tempo, degree = 3, knots = c(0, 40, 45, 85, 100)), data = dados)
  }, error = function(e) {
    stop("Erro ao ajustar Q4: ", e$message)
  })
  
  cat("Ajustes concluídos com sucesso!\n\n")
  
  return(modelos)
}

#' Calcula precipitação acumulada para um dado percentual de tempo
#' @param modelo Modelo ajustado (Q1, Q2, Q3 ou Q4)
#' @param percentual_tempo Percentual de tempo (0-100)
#' @param intensidade Precipitação total (mm)
#' @param tipo_quartil Tipo do quartil ("Q1", "Q2", "Q3" ou "Q4")
#' @return Precipitação acumulada (mm)
calcular_precipitacao_acumulada <- function(modelo, percentual_tempo, 
                                           intensidade, tipo_quartil) {
  
  # Criar dataframe para predição
  novos_dados <- data.frame(Tempo = percentual_tempo)
  
  # Calcular precipitação acumulada percentual
  precip_percentual <- predict(modelo, newdata = novos_dados)
  
  # Converter para mm
  precip_mm <- precip_percentual * intensidade / 100
  
  return(as.numeric(precip_mm))
}

#' Calcula fatores de correção para garantir que a precipitação total seja exata
#' @param modelo Modelo ajustado
#' @param NB Número de intervalos
#' @param intensidade Precipitação total (mm)
#' @return Lista com FatorTranslacao e FatorAmplificacao
calcular_fatores_correcao <- function(modelo, NB, intensidade) {
  
  # Fator de Translação: ajusta o início da curva para zero
  # Calcula a precipitação no primeiro intervalo
  df_primeiro <- data.frame(Tempo = (1 / NB) * 100)
  fator_translacao <- predict(modelo, newdata = df_primeiro) * intensidade / 100
  
  # Fator de Amplificação: garante que a precipitação total = intensidade
  # Calcula a precipitação total sem correção
  df_total <- data.frame(Tempo = 100)
  precip_total_sem_correcao <- predict(modelo, newdata = df_total) * intensidade / 100
  
  fator_amplificacao <- intensidade / (precip_total_sem_correcao - fator_translacao)
  
  return(list(
    translacao = as.numeric(fator_translacao),
    amplificacao = as.numeric(fator_amplificacao)
  ))
}

#' Gera hietograma discretizado para um evento
#' @param modelo Modelo ajustado
#' @param NB Número de blocos (intervalos)
#' @param intensidade Precipitação total (mm)
#' @param tipo_quartil Tipo do quartil ("Q1", "Q2", "Q3" ou "Q4")
#' @return Vetor com precipitações incrementais (mm)
gerar_hietograma <- function(modelo, NB, intensidade, tipo_quartil) {
  
  # Calcular fatores de correção
  fatores <- calcular_fatores_correcao(modelo, NB, intensidade)
  
  # Vetor para armazenar precipitação acumulada
  precip_acumulada <- numeric(NB + 1)
  precip_acumulada[1] <- 0
  
  # Calcular precipitação acumulada para cada intervalo
  for (j in 1:NB) {
    percentual_tempo <- (j / NB) * 100
    
    # Caso especial: último intervalo deve ser exatamente a intensidade total
    if (j == NB) {
      precip_acumulada[j + 1] <- intensidade
    } else {
      # Calcular precipitação acumulada bruta
      precip_bruta <- calcular_precipitacao_acumulada(
        modelo, percentual_tempo, intensidade, tipo_quartil
      )
      
      # Aplicar correções
      precip_acumulada[j + 1] <- (precip_bruta - fatores$translacao) * 
                                  fatores$amplificacao
    }
  }
  
  # Converter para precipitação incremental
  precip_incremental <- diff(precip_acumulada)
  
  # Garantir que não há valores negativos
  precip_incremental[precip_incremental < 0] <- 0
  
  return(precip_incremental)
}

#' Determina qual quartil usar com base na duração
#' @param duracao_minutos Duração do evento em minutos
#' @return Nome do quartil ("Q1", "Q2", "Q3" ou "Q4")
determinar_quartil <- function(duracao_minutos) {
  if (duracao_minutos <= 12 * 60) {
    return("Q2")
  } else if (duracao_minutos > 12 * 60 & duracao_minutos < 24 * 60) {
    return("Q3")
  } else {
    return("Q4")
  }
}

# 4. CARREGAMENTO E PREPARAÇÃO DOS DADOS ---------------------------------------

cat("=================================================================\n")
cat("GERAÇÃO DE HIETOGRAMAS SINTÉTICOS - DISTRIBUIÇÃO DE HUFF\n")
cat("=================================================================\n\n")

# Verificar se os arquivos existem
if (!file.exists(caminho_huff)) {
  stop("Arquivo não encontrado: ", caminho_huff)
}
if (!file.exists(caminho_quantis)) {
  stop("Arquivo não encontrado: ", caminho_quantis)
}

# Carregar dados de distribuição de Huff
cat("Carregando dados de distribuição de Huff...\n")
dados_huff <- read_excel(caminho_huff)
cat("  - Dados carregados:", nrow(dados_huff), "linhas\n\n")

# Carregar quantis de precipitação
cat("Carregando quantis de precipitação...\n")
quantis <- read_excel(caminho_quantis)

# Adicionar colunas de duração
tempo_quantis_horas <- c(6/60, 10/60, 15/60, 20/60, 30/60, 1, 2, 3, 4, 6, 8, 
                         10, 12, 18, 24, 24*2, 3*24, 5*24, 7*24, 10*24, 15*24,
                         20*24, 30*24)
quantis$Duracao <- tempo_quantis_horas * 60  # em minutos
quantis$horas <- tempo_quantis_horas
cat("  - Quantis carregados:", nrow(quantis), "durações\n\n")

# 5. AJUSTE DOS MODELOS DE HUFF ------------------------------------------------

modelos_huff <- ajustar_curvas_huff(dados_huff)

# 6. CONFIGURAÇÃO PARA GERAÇÃO DE EVENTOS --------------------------------------

# Tempos de retorno a processar
trs <- c(2, 5, 10, 20, 25, 50, 100, 200, 500, 1000, 10000, 88000, 96800)

# Meses para nomenclatura
meses <- c("January", "February", "March", "April", "May", "June",
           "July", "August", "September", "October", "November", "December")

# Variáveis para armazenar texto dos arquivos
txt_gage <- ""
txt_met <- ""
txt_run <- ""

# Abrir arquivo DSS
projectFileDss <- paste0(projectFolder, projectName, ".dss")
dssFile <- .jcall("hec/heclib/dss/HecDss", "Lhec/heclib/dss/HecDss;", 
                  method = "open", projectFileDss)

# 7. PROCESSAMENTO DOS EVENTOS -------------------------------------------------

cat("Processando eventos de precipitação...\n")
cat("Tempos de retorno:", paste(trs, collapse = ", "), "\n\n")

contador_eventos <- 0

foreach(tr = trs) %do% {
  
  cat("TR =", tr, "anos\n")
  
  # Coluna correspondente ao TR
  col <- as.character(tr)
  intensidades <- quantis[[col]]
  duracoes <- quantis[["Duracao"]]
  
  for (i in 1:length(intensidades)) {
    
    # Extrair informações do evento
    bloco <- quantis$Bloco[i]           # Intervalo de tempo (minutos)
    intensidade <- intensidades[i]       # Precipitação total (mm)
    duracao <- duracoes[i]              # Duração total (minutos)
    NB <- as.integer(duracao / bloco)   # Número de blocos
    dias <- max(1, as.integer(duracao / (24 * 60)))  # Número de dias
    
    # Determinar qual quartil usar
    quartil <- determinar_quartil(duracao)
    nome_evento <- paste0("TT - ", tr, "A ", quantis$Sufix[i], " - ", quartil)
    
    cat("  - Processando:", nome_evento, "\n")
    cat("    Duração:", duracao, "min | Blocos:", NB, "| Quartil:", quartil, "\n")
    
    # Gerar hietograma usando o modelo apropriado
    modelo_escolhido <- modelos_huff[[quartil]]
    precip_acumulada <- numeric(NB + 1)
    precip_acumulada[1] <- 0
    
    # Calcular fatores de correção
    fatores <- calcular_fatores_correcao(modelo_escolhido, NB, intensidade)
    
    # Calcular precipitação acumulada para cada bloco
    for (j in 1:NB) {
      percentual_tempo <- (j / NB) * 100
      
      if (j == NB) {
        # Último intervalo = precipitação total
        precip_acumulada[j + 1] <- intensidade
      } else if (percentual_tempo > 0 && percentual_tempo < 100) {
        # Calcular usando o modelo
        novos_dados <- data.frame(Tempo = percentual_tempo)
        precip_bruta <- predict(modelo_escolhido, newdata = novos_dados) * 
                        intensidade / 100
        precip_acumulada[j + 1] <- (as.numeric(precip_bruta) - fatores$translacao) * 
                                    fatores$amplificacao
      }
    }
    
    # Garantir valores não-negativos
    precip_acumulada[precip_acumulada < 0] <- 0
    
    # 8. GRAVAÇÃO NO ARQUIVO DSS -----------------------------------------------
    
    # Criar TimeSeriesContainer
    tsc <- .jnew("hec/io/TimeSeriesContainer")
    
    # Definir nome e caminho DSS
    dss_path <- paste0("//", nome_evento, "/PRECIP-CUM/31Dec1999 - ", dias, 
                       "Jan2000/", bloco, "MIN/GAGE/")
    tsc$fullName <- dss_path
    
    # Configurar tempo inicial
    start <- .jnew("hec/heclib/util/HecTime", "01Jan2000", "0000")
    tsc$interval <- as.integer(bloco)
    
    # Criar vetor de tempos
    timec <- start$value()
    times <- .jarray(1:(NB + 1))
    times[[1]] <- start$value()
    
    for (j in 1:NB) {
      timec <- timec + bloco
      times[[j + 1]] <- as.integer(timec)
    }
    
    # Calcular tempo final
    time1 <- as.POSIXct("2000-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S")
    time1 <- time1 + bloco * 60 * NB
    timef <- paste0(strftime(time1, format = "%d "),
                    meses[as.integer(strftime(time1, format = "%m"))],
                    strftime(time1, format = " %Y, %H:%M"))
    
    # Configurar valores
    values <- .jarray(precip_acumulada)
    tsc$times <- times
    tsc$values <- values
    tsc$numberValues <- length(values)
    tsc$units <- "MM"
    tsc$type <- "INST-CUM"
    
    # Gravar no DSS
    dssFile$put(tsc)
    
    # 9. GERAÇÃO DE ARQUIVOS HEC-HMS -------------------------------------------
    
    # Arquivo .gage
    txt_gage <- paste0(txt_gage, 
                       "Gage: ", nome_evento, "\n",
                       "Latitude:0\n",
                       "Longitude:0\n",
                       "Gage Type: Precipitation\n",
                       "Precipitation Type: Cumulative\n",
                       "Units: MM\n",
                       "Local to Project: YES\n",
                       "Start Time: 1 January 2000, 00:00\n",
                       "End Time: ", timef, "\n",
                       "Pathname: ", dss_path, "\n",
                       "Units System: SI\n",
                       "Data Type: PRECIP-CUM\n",
                       "dss File: ", projectFileDss, "\n",
                       "End:\n\n")
    
    # Arquivo .met
    metFile <- paste0(projectFolder, nome_evento, ".met")
    metTxt <- paste0("Meteorology: ", nome_evento, "\n",
                     "     Version: 4.12\n",
                     "     Unit System: Metric\n",
                     "     Set Missing Data to Default: Yes\n",
                     "     Precipitation Method: Specified Average\n",
                     "     Short-Wave Radiation Method: None\n",
                     "     Long-Wave Radiation Method: None\n",
                     "     Snowmelt Method: None\n",
                     "     Evapotranspiration Method: No Evapotranspiration\n",
                     "     Use Basin Model: ", basin, "\n",
                     "End:")
    
    # Adicionar sub-bacias ao arquivo .met
    foreach(sb = subbasins) %do% {
      metTxt <- paste0(metTxt, "\nSubbasin: ", sb, "\n",
                       "     Gage:", nome_evento, "\n",
                       "End:\n")
    }
    
    write(metTxt, file = metFile, append = FALSE)
    
    # Arquivo .hms
    txt_met <- paste0(txt_met, 
                      "Precipitation: ", nome_evento, "\n",
                      "     FileName: ", nome_evento, ".met\n",
                      "     Description: \n",
                      "End:\n\n")
    
    # Arquivo .run
    control <- ifelse(dias <= 5, "dp<=5d", "dp>5d")
    txt_run <- paste0(txt_run, "\n\nRun: ", nome_evento, "\n",
                      "    Basin: ", basin, "\n",
                      "    Precip: ", nome_evento, "\n",
                      "    Control: ", control, "\n",
                      "End:")
    
    contador_eventos <- contador_eventos + 1
  }
  
  cat("\n")
}

# 10. FINALIZAÇÃO E SALVAMENTO DOS ARQUIVOS ------------------------------------

cat("\nFinalizando e salvando arquivos...\n")

# Fechar arquivo DSS
dssFile$close()

# Salvar arquivo .gage
gageFile <- paste0(projectFolder, projectName, ".gage")
gageHeader <- paste0("Gage Manager: ", projectName, "\n",
                     "Version: 4.12\n",
                     "Filepath Separator: \\\n",
                     "End:\n")
write(gageHeader, file = gageFile, append = FALSE)
write(txt_gage, file = gageFile, append = TRUE)
cat("  - Arquivo .gage salvo\n")

# Salvar arquivo .hms
txt_hms <- paste0("Project: ", projectName, "\n",
                  "     Description: \n",
                  "     Version: 4.12\n",
                  "     Filepath Separator: \\\n",
                  "     DSS File Name: ", projectName, ".dss\n",
                  "     Time Zone ID: America/Sao_Paulo\n",
                  "End:\n\n",
                  "Basin: ", basin, "\n",
                  "     Filename: ", gsub(" ", "_", basin), ".basin\n",
                  "     Description: \n",
                  "End:\n\n",
                  txt_met, "\n\n",
                  "Control: dp<=5d\n",
                  "     FileName: dp__5d.control\n",
                  "     Description: \n",
                  "End:\n\n",
                  "Control: dp>5d\n",
                  "     FileName: dp_5d.control\n",
                  "     Description: \n",
                  "End:")

hmsFile <- paste0(projectFolder, projectName, ".hms")
write(txt_hms, file = hmsFile, append = FALSE)
cat("  - Arquivo .hms salvo\n")

# Salvar arquivo .run
txt_run_final <- paste0(txt_run, "\n\n",
                        "Control: dp>5d\n",
                        "     FileName: dp_5d.control\n",
                        "     Description: \n",
                        "End:\n\n",
                        "Control: dp<=5d\n",
                        "     FileName: dp__5d.control\n",
                        "     Description: \n",
                        "End:")

runFile <- paste0(projectFolder, projectName, ".run")
write(txt_run_final, file = runFile, append = FALSE)
cat("  - Arquivo .run salvo\n")

# 11. RESUMO FINAL -------------------------------------------------------------

cat("\n=================================================================\n")
cat("PROCESSAMENTO CONCLUÍDO COM SUCESSO!\n")
cat("=================================================================\n")
cat("Total de eventos gerados:", contador_eventos, "\n")
cat("Arquivos criados:\n")
cat("  - ", gageFile, "\n")
cat("  - ", hmsFile, "\n")
cat("  - ", runFile, "\n")
cat("  - ", projectFileDss, "\n")
cat("  - ", contador_eventos, " arquivos .met individuais\n")
cat("=================================================================\n\n")

# Sinal sonoro de conclusão
beep(1)
