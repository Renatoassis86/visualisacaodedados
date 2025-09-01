# 📦 Pacotes
library(PNADcIBGE)
library(dplyr)
library(srvyr)
library(survey)
library(writexl)

# 📁 Caminho dos dados
caminho_base <- "D:/repositorio_geral/pnad_continua/PNAD/Dados/PNADC_012025/"

# 🚀 Leitura dos microdados
pnad_raw <- read_pnadc(
  microdata = paste0(caminho_base, "PNADC_012025.txt"),
  input_txt  = paste0(caminho_base, "input_PNADC_trimestral.txt")
)

# 🔍 Conferir se as variáveis estão corretas
table(pnad_raw$V4012, useNA = "always")
table(pnad_raw$V4029, useNA = "always")
table(pnad_raw$V4019, useNA = "always")

# 🔍 Filtrar ocupados
pnad_tratado <- pnad_raw %>%
  filter(VD4002 == "1") %>% # Ocupados
  mutate(
    local = ifelse(UF == "24", "Rio Grande do Norte", "Brasil")
  )

# ✔️ Criar variável de informalidade
pnad_tratado <- pnad_tratado %>%
  mutate(
    informal = case_when(
      # Empregado setor privado sem carteira
      V4012 == "3" & V4029 == "2" ~ 1,

      # Empregado doméstico sem carteira
      V4012 == "1" & V4029 == "2" ~ 1,

      # Empregador sem CNPJ
      V4012 == "5" & V4019 == "2" ~ 1,

      # Conta própria sem CNPJ
      V4012 == "6" & V4019 == "2" ~ 1,

      # Trabalhador familiar auxiliar
      V4012 == "7" ~ 1,

      # Demais → Formal
      TRUE ~ 0
    )
  )

# 🎯 Criar objeto survey
pnad_design <- pnad_tratado %>%
  as_survey_design(
    ids = UPA,
    strata = Estrato,
    weights = V1028,
    nest = TRUE
  )

# 📊 Calcular informalidade
tabela_informalidade <- pnad_design %>%
  group_by(local) %>%
  summarise(
    ocupados  = survey_total(na.rm = TRUE),
    informais = survey_total(informal, na.rm = TRUE),
    taxa_informalidade = (informais / ocupados) * 100
  ) %>%
  mutate(
    ocupados  = round(ocupados / 1000, 1),  # mil pessoas
    informais = round(informais / 1000, 1),
    formais   = round((ocupados * 1000 - informais * 1000) / 1000, 1),
    taxa_informalidade = round(taxa_informalidade, 1)
  )

# ✔️ Visualizar tabela
print(tabela_informalidade)

# 💾 Salvar no Excel
write_xlsx(tabela_informalidade,
           "D:/repositorio_geral/pnad_continua/Tabela_Informalidade_2025_FINAL.xlsx")


# 📦 Pacotes
library(PNADcIBGE)
library(dplyr)
library(srvyr)
library(survey)
library(writexl)
library(purrr)

# 📁 Caminho base dos dados
caminho_base <- "D:/repositorio_geral/pnad_continua/PNAD/Dados/"

# 🔄 Lista dos trimestres a serem analisados
trimestres <- c("012023", "022023", "032023", "042023", "012024", "022024", "032024", "042024", "012025")

# 🚀 Função para processar cada trimestre
processar_trimestre <- function(trimestre) {
  
  # Caminho do trimestre
  caminho <- paste0(caminho_base, "PNADC_", trimestre, "/")
  
  # Leitura dos microdados
  pnad_raw <- read_pnadc(
    microdata = paste0(caminho, "PNADC_", trimestre, ".txt"),
    input_txt  = paste0(caminho, "input_PNADC_trimestral.txt")
  )
  
  # Tratamento da base
  pnad_tratado <- pnad_raw %>%
    filter(VD4002 == "1") %>% # Ocupados
    mutate(
      local = ifelse(UF == "24", "Rio Grande do Norte", "Brasil")
    )
  
  # Criar variável de informalidade
  pnad_tratado <- pnad_tratado %>%
    mutate(
      informal = case_when(
        # Empregado setor privado sem carteira
        V4012 == "3" & V4029 == "2" ~ 1,
        
        # Empregado doméstico sem carteira
        V4012 == "1" & V4029 == "2" ~ 1,
        
        # Empregador sem CNPJ
        V4012 == "5" & V4019 == "2" ~ 1,
        
        # Conta própria sem CNPJ
        V4012 == "6" & V4019 == "2" ~ 1,
        
        # Trabalhador familiar auxiliar
        V4012 == "7" ~ 1,
        
        # Demais → Formal
        TRUE ~ 0
      )
    )
  
  # Objeto survey
  pnad_design <- pnad_tratado %>%
    as_survey_design(
      ids = UPA,
      strata = Estrato,
      weights = V1028,
      nest = TRUE
    )
  
  # Calcular informalidade
  tabela <- pnad_design %>%
    group_by(local) %>%
    summarise(
      ocupados  = survey_total(na.rm = TRUE),
      informais = survey_total(informal, na.rm = TRUE),
      taxa_informalidade = (informais / ocupados) * 100
    ) %>%
    mutate(
      trimestre = trimestre,
      ocupados  = round(ocupados / 1000, 1),  
      informais = round(informais / 1000, 1),
      formais   = round((ocupados * 1000 - informais * 1000) / 1000, 1),
      taxa_informalidade = round(taxa_informalidade, 1)
    ) %>%
    select(trimestre, local, ocupados, informais, formais, taxa_informalidade)
  
  return(tabela)
}

# 🚀 Aplicar a função para todos os trimestres
resultados_informalidade <- map_dfr(trimestres, processar_trimestre)

# ✔️ Visualizar resultado
print(resultados_informalidade)

# 💾 Salvar em Excel
write_xlsx(resultados_informalidade,
           "D:/repositorio_geral/pnad_continua/Tabela_Informalidade_Todos_Trimestres.xlsx")





####### Rendimentos


# 📦 Pacotes necessários
library(PNADcIBGE)
library(dplyr)
library(srvyr)
library(readxl)

# ⚙️ Permitir estratos com uma única UPA
options(survey.lonely.psu = "certainty")

# 📁 Caminhos
trimestre <- "012025"
caminho_base <- "D:/repositorio_geral/pnad_continua/PNAD/Dados/"
caminho_trimestre <- paste0(caminho_base, "PNADC_", trimestre, "/")

# 🗃️ Caminhos dos arquivos
microdados <- paste0(caminho_trimestre, "PNADC_", trimestre, ".txt")
input_txt  <- paste0(caminho_trimestre, "input_PNADC_trimestral.txt")

# 📥 Ler microdados
pnad <- read_pnadc(
  microdata = microdados,
  input_txt = input_txt,
  vars = c("UF", "UPA", "Estrato", "V1028", "VD4002", "VD4016")
)

# 🎯 Filtrar apenas ocupados com valor de rendimento válido
pnad_filtrado <- pnad %>%
  filter(VD4002 == "1", !is.na(VD4016)) %>%
  mutate(
    local = ifelse(UF == "24", "Rio Grande do Norte", "Brasil"),
    rendimento = as.numeric(VD4016)
  )

# 📊 Objeto survey
pnad_design <- as_survey_design(
  pnad_filtrado,
  ids = UPA,
  strata = Estrato,
  weights = V1028,
  nest = TRUE
)

# 📈 Calcular rendimento médio habitual
tabela_rendimento_012025 <- pnad_design %>%
  group_by(local) %>%
  summarise(rendimento_medio = survey_mean(rendimento, na.rm = TRUE)) %>%
  mutate(trimestre = trimestre)

# 👁️ Visualizar resultado
print(tabela_rendimento_012025)


library(PNADcIBGE)
library(dplyr)

# Caminho
caminho_base <- "D:/repositorio_geral/pnad_continua/PNAD/Dados/PNADC_012025/"
microdados <- paste0(caminho_base, "PNADC_012025.txt")
input_txt  <- paste0(caminho_base, "input_PNADC_trimestral.txt")

# Leitura com variáveis necessárias
pnad <- read_pnadc(
  microdata = microdados,
  input_txt = input_txt,
  vars = c("UF", "V2009", "VD4002", "V1028", "UPA", "Estrato", "posest")
)

# Verifique estrutura da base carregada
glimpse(pnad)


library(dplyr)

pnad_rn <- pnad %>%
  filter(UF == "24", V2009 >= 14)

# Verificar quantidade de registros por posest
pnad_rn %>% count(posest)


pnad %>%
  count(UF, posest) %>%
  arrange(UF, posest)

pnad %>%
  filter(UF == "24") %>%
  count(posest) %>%
  arrange(posest)


library(dplyr)

# Criação da base filtrada e categorização dos estratos do RN
pnad_rn_validada <- pnad %>%
  filter(UF == "24", V2009 >= 14) %>%
  mutate(
    regiao_rn = case_when(
      posest == "241" ~ "Natal",
      posest == "242" ~ "Entorno Metropolitano",
      posest == "243" ~ "Agreste e Litoral Sul",
      posest == "244" ~ "Central",
      posest == "245" ~ "Oeste",
      TRUE ~ NA_character_
    )
  )

# Validação: contar quantos registros há por região
pnad_rn_validada %>% count(regiao_rn)




# 📦 Pacotes
library(PNADcIBGE)
library(dplyr)
library(srvyr)
library(writexl)

# ⚙️ Permitir PSU solitária
options(survey.lonely.psu = "adjust")

# 🗂️ Lista de trimestres
trimestres <- c("012023", "022023", "032023", "042023",
                "012024", "022024", "032024", "042024", "012025")

# 📁 Caminho base
diretorio_base <- "D:/repositorio_geral/pnad_continua/PNAD/Dados/"

# 📦 Lista para armazenar resultados
resultados_regioes <- list()

# 🔁 Loop por trimestre
for (tri in trimestres) {
  cat("🔍 Processando:", tri, "\n")
  
  # Caminhos
  caminho <- paste0(diretorio_base, "PNADC_", tri, "/")
  microdados <- paste0(caminho, "PNADC_", tri, ".txt")
  input_txt  <- paste0(caminho, "input_PNADC_trimestral.txt")

  # Leitura dos dados
  pnad <- read_pnadc(
    microdata = microdados,
    input_txt = input_txt,
    vars = c("UF", "V2009", "VD4002", "V1028", "UPA", "Estrato", "posest")
  )

  # Filtro e classificação de região
  pnad_rn <- pnad %>%
    filter(UF == "24", V2009 >= 14) %>%
    mutate(
      regiao_rn = case_when(
        posest == "241" ~ "Natal",
        posest == "242" ~ "Entorno Metropolitano",
        posest == "243" ~ "Agreste e Litoral Sul",
        posest == "244" ~ "Central",
        posest == "245" ~ "Oeste",
        TRUE ~ NA_character_
      ),
      participa = ifelse(VD4002 %in% c("1", "2"), 1, 0)
    ) %>%
    filter(!is.na(regiao_rn))

  # Objeto survey
  design_rn <- pnad_rn %>%
    as_survey_design(
      ids = UPA,
      strata = Estrato,
      weights = V1028,
      nest = TRUE
    )

  # Cálculo por região
  resultado_tri <- design_rn %>%
    group_by(regiao_rn) %>%
    summarise(
      taxa_participacao = survey_mean(participa, na.rm = TRUE)
    ) %>%
    mutate(
      `Taxa de Participação (%)` = round(taxa_participacao * 100, 1),
      Trimestre = tri
    ) %>%
    select(Trimestre, regiao_rn, `Taxa de Participação (%)`)

  # Armazenar resultado
  resultados_regioes[[tri]] <- resultado_tri
}

# 🔗 Consolidar resultados
tabela_final <- bind_rows(resultados_regioes)

# 🗂️ Formatar trimestres
tabela_final <- tabela_final %>%
  mutate(Trimestre = case_when(
    Trimestre == "012023" ~ "01º tri/2023",
    Trimestre == "022023" ~ "02º tri/2023",
    Trimestre == "032023" ~ "03º tri/2023",
    Trimestre == "042023" ~ "04º tri/2023",
    Trimestre == "012024" ~ "01º tri/2024",
    Trimestre == "022024" ~ "02º tri/2024",
    Trimestre == "032024" ~ "03º tri/2024",
    Trimestre == "042024" ~ "04º tri/2024",
    Trimestre == "012025" ~ "01º tri/2025",
    TRUE ~ Trimestre
  ))

# 💾 Exportar para Excel
write_xlsx(tabela_final, "D:/repositorio_geral/pnad_continua/Tabela_Taxa_Participacao_RN_Regioes.xlsx")

# ✔️ Visualizar
print(tabela_final, n = Inf)
