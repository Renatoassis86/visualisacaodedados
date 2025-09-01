q()

& "C:/Program Files/R/R-4.3.3/bin/R.exe"

# 🧼 Limpa variáveis e define pacotes obrigatórios
rm(list = ls())

# 📦 Lista de pacotes necessários
pacotes <- c("PNADcIBGE", "survey", "dplyr", "ggplot2")
install.packages("writexl")
install.packages(c("tidyverse", "srvyr"))

# 🧩 Instala pacotes ausentes
novos <- pacotes[!(pacotes %in% installed.packages()[, "Package"])]
if (length(novos)) install.packages(novos)

# 🔃 Carrega os pacotes
library(PNADcIBGE); cat("✅ PNADcIBGE carregado\n")
library(survey);    cat("✅ survey carregado\n")
library(dplyr);     cat("✅ dplyr carregado\n")
library(ggplot2);   cat("✅ ggplot2 carregado\n")
library(tidyverse)
library(srvyr)



# 📁 Caminho base dos arquivos da PNAD
caminho <- "D:/repositorio_geral/pnad_continua/PNAD/Dados/PNADC_042024/"


# ✅ Verifica se os arquivos existem

print(file.exists(paste0(caminho, "PNADC_042024.txt")))                   # Microdados
print(file.exists(paste0(caminho, "input_PNADC_trimestral.txt")))        # Input
print(file.exists(paste0(caminho, "deflator_PNADC_2025_trimestral_010203.xls")))  # Deflator


# 📥 Leitura dos microdados da PNAD
pnad_raw <- read_pnadc(
  microdata = paste0(caminho, "PNADC_042024.txt"),
  input_txt = paste0(caminho, "input_PNADC_trimestral.txt")
)

cat("✅ Microdados carregados\n")


# 🧮 Criação do objeto survey usando diretamente pnad_raw
pnad <- svydesign(
  ids = ~UPA,
  strata = ~Estrato,
  weights = ~V1028,
  data = pnad_raw,
  nest = TRUE
)

cat("✅ Objeto survey criado com pnad_raw (sem deflator)\n")


# ⚙️ Garante que o comportamento de PSU solitário está ajustado
options(survey.lonely.psu = "adjust")

# 🇧🇷 Jovens do Brasil (14 a 29 anos)
pnad_jovens_br <- subset(pnad, V2009 >= 14 & V2009 <= 17)

# 📊 Variável de participação
pnad_jovens_br$variables <- pnad_jovens_br$variables %>%
  mutate(participa = ifelse(VD4002 %in% c(1, 2), 1, 0),
         desocupado = ifelse(VD4002 == 2, 1, 0))

# 📈 Taxas para Brasil
taxa_part_br <- svymean(~participa, pnad_jovens_br)
taxa_desoc_br <- svymean(~desocupado, subset(pnad_jovens_br, VD4002 %in% c(1, 2)))

# 🖨️ Resultado Brasil
cat("🇧🇷 Brasil\n")
cat("📊 Taxa de participação:", round(taxa_part_br[1] * 100, 1), "%\n")
cat("📉 Taxa de desocupação:", round(taxa_desoc_br[1] * 100, 1), "%\n")


# 🟩 Jovens do RN (UF 24)
pnad_jovens_rn <- subset(pnad, V2009 >= 14 & V2009 <= 17 & UF == 24)

# 📊 Variável de participação e desocupação
pnad_jovens_rn$variables <- pnad_jovens_rn$variables %>%
  mutate(participa = ifelse(VD4002 %in% c(1, 2), 1, 0),
         desocupado = ifelse(VD4002 == 2, 1, 0))

# 📈 Taxas para RN
taxa_part_rn <- svymean(~participa, pnad_jovens_rn)
taxa_desoc_rn <- svymean(~desocupado, subset(pnad_jovens_rn, VD4002 %in% c(1, 2)))

# 🖨️ Resultado RN
cat("\n🟩 Rio Grande do Norte\n")
cat("📊 Taxa de participação:", round(taxa_part_rn[1] * 100, 1), "%\n")
cat("📉 Taxa de desocupação:", round(taxa_desoc_rn[1] * 100, 1), "%\n")


# Criando as variáveis do estudo

# 📦 Carregar pacotes
library(dplyr)
library(survey)
library(ggplot2)


# ⚙️ Ajuste para PSU solitário
options(survey.lonely.psu = "adjust")

# 🧮 Criar variáveis no objeto pnad
pnad$variables <- pnad$variables %>%
  mutate(
    faixa_etaria = case_when(
      V2009 >= 14 & V2009 <= 17 ~ "14 a 17",
      V2009 >= 18 & V2009 <= 24 ~ "18 a 24",
      V2009 >= 25 & V2009 <= 29 ~ "25 a 29",
      V2009 >= 30              ~ "30 ou mais",
      TRUE ~ NA_character_
    ),
    participa = ifelse(VD4002 %in% c(1, 2), 1, 0)
  )

# 🧾 Função para calcular taxas por faixa
calcular_taxas <- function(data, uf_label = "Brasil", uf_code = NULL) {
  faixas <- c("14 a 17", "18 a 24", "25 a 29", "30 ou mais")
  resultados <- data.frame()

  for (faixa in faixas) {
    sub <- if (is.null(uf_code)) {
      subset(data, faixa_etaria == faixa)
    } else {
      subset(data, faixa_etaria == faixa & UF == uf_code)
    }
    taxa <- svymean(~participa, sub, na.rm = TRUE)
    resultados <- rbind(resultados, data.frame(
      faixa = faixa,
      local = uf_label,
      taxa = as.numeric(taxa)
    ))
  }

  # Total (14+)
  sub_total <- if (is.null(uf_code)) {
    subset(data, V2009 >= 14)
  } else {
    subset(data, V2009 >= 14 & UF == uf_code)
  }
  taxa_total <- svymean(~participa, sub_total, na.rm = TRUE)
  resultados <- rbind(resultados, data.frame(
    faixa = "Total (14+)",
    local = uf_label,
    taxa = as.numeric(taxa_total)
  ))

  return(resultados)
}

# 🇧🇷 Calcular Brasil e RN
tabela_br <- calcular_taxas(pnad)
tabela_rn <- calcular_taxas(pnad, uf_label = "Rio Grande do Norte", uf_code = 24)

# 📊 Combinar
tabela_participacao <- bind_rows(tabela_br, tabela_rn)

# 💾 Salva a tabela como XLS
writexl::write_xlsx(tabela_participacao, "taxa_participacao_faixa_etária.xlsx")

# 🖼️ Gráfico com legendas limpas e marcadores
grafico <- ggplot(tabela_participacao, aes(x = faixa, y = taxa * 100, fill = local)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = paste0(round(taxa * 100, 1), "%")),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    size = 3.5
  ) +
  labs(
    title = "Taxa de Participação dos Jovens por Faixa Etária – Brasil e RN (2024)",
    x = "Faixa Etária",
    y = "Taxa de Participação (%)",
    fill = NULL  # 🔧 Remove o título da legenda
  ) +
  scale_fill_manual(values = c("Brasil" = "indianred", "Rio Grande do Norte" = "royalblue")) +
  theme_minimal()

# 👁️ Exibe o gráfico
print(grafico)

# 💾 Salva o gráfico como imagem
ggsave("grafico_participacao_faixa_etaria.png", plot = grafico, width = 9, height = 6)



#### Jovens fora da força de trabalho

# Pacotes
library(PNADcIBGE)
library(srvyr)
library(dplyr)
library(ggplot2)
library(writexl)
library(scales)



# Criar objeto survey
pnad_design <- pnad %>%
  as_survey_design(
    ids = UPA,
    strata = Estrato,
    weights = V1028,
    nest = TRUE
  )

# Filtrar jovens fora da força de trabalho com motivo respondido (sem renomear variável de idade)
pnad_jovens_fora <- pnad_design %>%
  filter(VD4001 == 2, V2009 >= 14, V2009 <= 29, !is.na(VD4030))

# Tabela geral Brasil
brasil <- pnad_jovens_fora %>%
  group_by(motivo = factor(VD4030,
                           levels = 1:6,
                           labels = c(
                             "Tinha que cuidar dos afazeres domésticos,\nfilho(s) ou parente(s)",
                             "Estava estudando",
                             "Por problema de saúde ou gravidez",
                             "Por ser muito jovem ou muito idoso para trabalhar",
                             "Por não querer trabalhar",
                             "Por outro motivo"
                           ))) %>%
  summarise(proporcao = survey_mean(vartype = NULL, na.rm = TRUE) * 100) %>%
  mutate(local = "Brasil")

# Tabela para o RN (UF == 24)
rn <- pnad_jovens_fora %>%
  filter(UF == 24) %>%
  group_by(motivo = factor(VD4030,
                           levels = 1:6,
                           labels = c(
                             "Tinha que cuidar dos afazeres domésticos,\nfilho(s) ou parente(s)",
                             "Estava estudando",
                             "Por problema de saúde ou gravidez",
                             "Por ser muito jovem ou muito idoso para trabalhar",
                             "Por não querer trabalhar",
                             "Por outro motivo"
                           ))) %>%
  summarise(proporcao = survey_mean(vartype = NULL, na.rm = TRUE) * 100) %>%
  mutate(local = "Rio Grande do Norte")

# Junta os dados
tabela_final <- bind_rows(brasil, rn) %>%
  tidyr::pivot_wider(names_from = local, values_from = proporcao)

# Salvar tabela
write_xlsx(tabela_final, "tabela_motivos_jovens_fora.xlsx")

# Voltar ao formato longo para gráfico
dados_grafico <- bind_rows(brasil, rn)

# Ordena fatores
dados_grafico$motivo <- factor(dados_grafico$motivo, levels = rev(unique(brasil$motivo)))

# Gráfico horizontal
grafico <- ggplot(dados_grafico, aes(x = proporcao, y = motivo, fill = local)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", proporcao)),
            position = position_dodge(width = 0.8),
            hjust = -0.1, size = 3.5) +
  scale_fill_manual(values = c("Brasil" = "#E74C3C", "Rio Grande do Norte" = "#3498DB")) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_blank()
  ) +
  xlim(0, max(dados_grafico$proporcao) + 10)

# Salvar gráfico
ggsave("grafico_2.png", grafico, width = 10, height = 6)

# Exibir no RStudio
print(grafico)


# 📦 Pacotes
library(PNADcIBGE)
library(srvyr)
library(dplyr)
library(ggplot2)
library(writexl)
library(tidyr)

# 📁 Caminho base dos arquivos da PNAD 4º tri 2024
caminho_base <- "D:/repositorio_geral/pnad_continua/PNAD/Dados/PNADC_042024"
microdados <- file.path(caminho_base, "PNADC_042024.txt")
input_txt <- file.path(caminho_base, "input_PNADC_trimestral.txt")

# 📥 Etapa 1: Leitura dos dados
pnad_2024 <- read_pnadc(
  microdata = microdados,
  input_txt = input_txt,
  vars = c("UF", "V2009", "VD4001", "VD4030", "V1028", "UPA", "Estrato")
)

# 🧮 Etapa 2: Criação do objeto survey design
pnad_design <- pnad_2024 %>%
  as_survey_design(ids = UPA, strata = Estrato, weights = V1028, nest = TRUE)

# 🎯 Etapa 3: Filtrar jovens fora da força de trabalho com motivo (VD4030) informado
pnad_jovens <- pnad_design %>%
  filter(VD4001 == 2, V2009 >= 14, V2009 <= 29, !is.na(VD4030)) %>%
  mutate(
    motivo = factor(VD4030,
      levels = 1:6,
      labels = c(
        "Tinha que cuidar dos afazeres domésticos,\nfilho(s) ou parente(s)",
        "Estava estudando",
        "Por problema de saúde ou gravidez",
        "Por ser muito jovem ou muito idoso para trabalhar",
        "Por não querer trabalhar",
        "Por outro motivo"
      ),
      ordered = TRUE
    ),
    local = ifelse(UF == 24, "Rio Grande do Norte", "Brasil")
  )

# ⚙️ Corrigir erro de PSU único por estrato
options(survey.lonely.psu = "adjust")

# 📊 Etapa 4: Calcular proporções com survey
resultados <- pnad_jovens %>%
  group_by(local, motivo) %>%
  summarise(proporcao = survey_mean(na.rm = TRUE), .groups = "drop")

# 🧾 Etapa 5: Criar tabela final
tabela_final <- resultados %>%
  pivot_wider(names_from = local, values_from = proporcao) %>%
  arrange(desc(Brasil))

# 💾 Etapa 6: Salvar tabela em Excel
write_xlsx(tabela_final, "tabela_motivos_2024_corrigida.xlsx")

# 📈 Etapa 7: Criar gráfico final
grafico <- resultados %>%
  ggplot(aes(x = proporcao * 100, y = motivo, fill = local)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +
  geom_text(
    aes(label = paste0(round(proporcao * 100, 1), "%")),
    position = position_dodge(width = 0.9),
    hjust = -0.1,
    size = 3.5
  ) +
  scale_fill_manual(values = c("Brasil" = "#E74C3C", "Rio Grande do Norte" = "#3498DB")) +
  labs(
    title = "Distribuição dos jovens fora do mercado de trabalho (14 a 29 anos)\npor motivo – Brasil e RN – 2024",
    x = NULL, y = NULL, fill = NULL
  ) +
  coord_cartesian(xlim = c(0, max(resultados$proporcao) * 100 + 10)) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# 🖼 Etapa 8: Visualizar gráfico
print(grafico)

# 💾 Etapa 9: Salvar gráfico
ggsave("grafico_3_motivos_2024.png", plot = grafico, width = 10, height = 6, dpi = 300)


## Grafico por genero

# 📦 Pacotes
library(PNADcIBGE)
library(srvyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(writexl)

# 📁 Caminho base
caminho_base <- "D:/repositorio_geral/pnad_continua/PNAD/Dados/PNADC_042024"
microdados <- file.path(caminho_base, "PNADC_042024.txt")
input_txt <- file.path(caminho_base, "input_PNADC_trimestral.txt")

# 📥 Leitura dos dados com variável de sexo (V2007) adicionada
pnad_2024 <- read_pnadc(
  microdata = microdados,
  input_txt = input_txt,
  vars = c("UF", "V2009", "V2007", "VD4001", "VD4030", "V1028", "UPA", "Estrato")
)

# 🎯 Objeto survey
pnad_design <- pnad_2024 %>%
  as_survey_design(ids = UPA, strata = Estrato, weights = V1028, nest = TRUE)

# 🔍 Filtro: jovens fora da força com motivo válido
pnad_jovens <- pnad_design %>%
  filter(VD4001 == 2, V2009 >= 14, V2009 <= 29, !is.na(VD4030), !is.na(V2007)) %>%
  mutate(
    motivo = factor(VD4030,
      levels = 1:6,
      labels = c(
        "Tinha que cuidar dos afazeres domésticos,\nfilho(s) ou parente(s)",
        "Estava estudando",
        "Por problema de saúde ou gravidez",
        "Por ser muito jovem ou muito idoso para trabalhar",
        "Por não querer trabalhar",
        "Por outro motivo"
      ),
      ordered = TRUE
    ),
    sexo = factor(V2007, levels = 1:2, labels = c("Homem", "Mulher")),
    local = ifelse(UF == 24, "Rio Grande do Norte", "Brasil")
  )

# ⚙️ PSU único
options(survey.lonely.psu = "adjust")

# 📊 Cálculo das proporções por sexo e UF
resultados <- pnad_jovens %>%
  group_by(local, sexo, motivo) %>%
  summarise(proporcao = survey_mean(na.rm = TRUE), .groups = "drop")

# 💾 Exportar tabela
tabela_final <- resultados %>%
  pivot_wider(names_from = local, values_from = proporcao)

write_xlsx(tabela_final, "tabela_motivos_2024_por_sexo.xlsx")

# 🖼️ Gráfico final — facetado por local
grafico <- resultados %>%
  ggplot(aes(x = proporcao * 100, y = motivo, fill = sexo)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +
  geom_text(aes(label = paste0(round(proporcao * 100, 1), "%")),
            position = position_dodge(width = 0.9),
            hjust = -0.1,
            size = 3.2) +
  facet_wrap(~local) +
  scale_fill_manual(values = c("Homem" = "#34495E", "Mulher" = "#9B59B6")) +
  labs(
    title = "Distribuição dos jovens fora do mercado de trabalho (14 a 29 anos)\npor motivo e sexo – Brasil e RN – 2024",
    x = NULL, y = NULL, fill = NULL
  ) +
  coord_cartesian(xlim = c(0, max(resultados$proporcao) * 100 + 10)) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
  )

# 💾 Salvar gráfico
ggsave("grafico_3a_motivos_2024_sexo.png", plot = grafico, width = 11, height = 7, dpi = 300)

# Mostrar no RStudio
print(grafico)



# Raça


# 📦 Pacotes
library(PNADcIBGE)
library(srvyr)
library(dplyr)
library(ggplot2)
library(writexl)
library(tidyr)

# 📁 Caminho base dos arquivos da PNAD
caminho <- "D:/repositorio_geral/pnad_continua/PNAD/Dados/PNADC_042024/"
microdados <- file.path(caminho, "PNADC_042024.txt")
input_txt  <- file.path(caminho, "input_PNADC_trimestral.txt")

# 🧩 Leitura dos dados
pnad <- read_pnadc(
  microdata = microdados,
  input_txt = input_txt,
  vars = c("UF", "V2009", "VD4001", "VD4030", "V1028", "UPA", "Estrato", "V2010")
)

# 🧮 Objeto survey design
pnad_design <- pnad %>%
  as_survey_design(ids = UPA, strata = Estrato, weights = V1028, nest = TRUE)

# 🔍 Filtrar jovens fora da força com motivo válido e cor/raça válida
pnad_jovens <- pnad_design %>%
  filter(
    VD4001 == 2,
    V2009 >= 14, V2009 <= 29,
    VD4030 %in% 1:6,
    !is.na(V2010)
  ) %>%
  mutate(
    motivo = factor(VD4030,
      levels = 1:6,
      labels = c(
        "Tinha que cuidar dos afazeres domésticos,\nfilho(s) ou parente(s)",
        "Estava estudando",
        "Por problema de saúde ou gravidez",
        "Por ser muito jovem ou muito idoso para trabalhar",
        "Por não querer trabalhar",
        "Por outro motivo"
      ),
      ordered = TRUE
    ),
    raca = case_when(
      V2010 == 1 ~ "Branca",
      V2010 %in% c(2, 4) ~ "Preta ou Parda",
      TRUE ~ NA_character_
    ),
    local = ifelse(UF == 24, "Rio Grande do Norte", "Brasil")
  ) %>%
  filter(!is.na(raca))  # Remove valores indefinidos de raça

# ⚙️ Corrigir PSU solitária
options(survey.lonely.psu = "adjust")

# 📊 Cálculo das proporções
resultados <- pnad_jovens %>%
  group_by(local, raca, motivo) %>%
  summarise(proporcao = survey_mean(na.rm = TRUE), .groups = "drop")

# 💾 Salvar tabela
tabela_motivos_raca <- resultados %>%
  pivot_wider(names_from = local, values_from = proporcao) %>%
  arrange(raca, desc(Brasil))

write_xlsx(tabela_motivos_raca, "tabela_motivos_por_raca_2024.xlsx")

# 🎨 Gráfico com facet por local (Brasil e RN)
grafico <- resultados %>%
  ggplot(aes(x = proporcao * 100, y = motivo, fill = raca)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +
  geom_text(aes(label = paste0(round(proporcao * 100, 1), "%")),
            position = position_dodge(width = 0.9), hjust = -0.1, size = 3.5) +
  scale_fill_manual(values = c("Branca" = "#1F77B4", "Preta ou Parda" = "#FF7F0E")) +
  facet_wrap(~local) +
  labs(
    title = "Distribuição dos jovens fora do mercado de trabalho (14 a 29 anos)\npor motivo e raça – Brasil e RN – 2024",
    x = NULL, y = NULL, fill = "Raça"
  ) +
  coord_cartesian(xlim = c(0, max(resultados$proporcao) * 100 + 10)) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# Mostrar gráfico
print(grafico)

# Salvar gráfico
ggsave("grafico_3_motivos_por_raca_2024.png", plot = grafico, width = 10, height = 6, dpi = 300)



## Desocupação faixa etária


# 📦 Pacotes
library(PNADcIBGE)
library(srvyr)
library(dplyr)
library(ggplot2)
library(writexl)
library(tidyr)

# 📁 Caminho base dos arquivos da PNAD
caminho <- "D:/repositorio_geral/pnad_continua/PNAD/Dados/PNADC_042024/"
microdados <- file.path(caminho, "PNADC_042024.txt")
input_txt  <- file.path(caminho, "input_PNADC_trimestral.txt")

# 🧩 Leitura dos dados
pnad <- read_pnadc(
  microdata = microdados,
  input_txt = input_txt,
  vars = c("UF", "V2009", "VD4002", "V1028", "UPA", "Estrato")
)

# 🧮 Objeto survey
pnad_design <- pnad %>%
  as_survey_design(ids = UPA, strata = Estrato, weights = V1028, nest = TRUE)

# ⚙️ Ajustar PSU solitária
options(survey.lonely.psu = "adjust")

# 🏷️ Criar variáveis de faixa etária e local
pnad_design <- pnad_design %>%
  mutate(
    faixa_etaria = case_when(
      V2009 >= 14 & V2009 <= 29 ~ "14 a 29 anos",
      V2009 >= 30 ~ "30 ou mais",
      TRUE ~ NA_character_
    ),
    local = ifelse(UF == 24, "Rio Grande do Norte", "Brasil"),
    desocupado = ifelse(VD4002 == 2, 1, 0)  # 2 = desocupado
  ) %>%
  filter(!is.na(faixa_etaria))

# 📊 Calcular taxa de desocupação
taxa_desocupacao <- pnad_design %>%
  filter(VD4002 %in% c(1, 2)) %>%  # Apenas força de trabalho
  group_by(local, faixa_etaria) %>%
  summarise(taxa = survey_mean(desocupado, na.rm = TRUE), .groups = "drop")

# 🎯 Calcular taxa total (todas idades)
taxa_total <- pnad_design %>%
  filter(VD4002 %in% c(1, 2)) %>%
  mutate(faixa_etaria = "Total") %>%
  group_by(local, faixa_etaria) %>%
  summarise(taxa = survey_mean(desocupado, na.rm = TRUE), .groups = "drop")

# 🧮 Juntar resultados
resultados <- bind_rows(taxa_desocupacao, taxa_total)

# 💾 Salvar tabela
write_xlsx(resultados, "tabela_taxa_desocupacao_faixa_etaria_2024.xlsx")

# 📊 Gráfico
grafico <- resultados %>%
  ggplot(aes(x = faixa_etaria, y = taxa * 100, fill = local)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.6) +
  geom_text(aes(label = paste0(round(taxa * 100, 1), "%")),
            position = position_dodge(width = 0.8), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("Brasil" = "#E74C3C", "Rio Grande do Norte" = "#3498DB")) +
  labs(
    title = "Taxa de Desocupação por Faixa Etária – Brasil e RN (4º tri/2024)",
    x = NULL, y = "Taxa de Desocupação (%)", fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# Mostrar
print(grafico)

# Salvar imagem
ggsave("grafico_taxa_desocupacao_faixa_etaria_2024.png", plot = grafico, width = 8, height = 6, dpi = 300)


### Faixa etária ocupado


library(ggplot2)
library(dplyr)
library(writexl)

## 📦 Pacotes (caso ainda não estejam carregados)
library(dplyr)
library(srvyr)
library(ggplot2)
library(writexl)

# 🧮 Criar nova variável de faixa etária agrupada
pnad_faixa_resumida <- pnad_design %>%
  filter(VD4002 == 1) %>%  # Apenas ocupados
  mutate(
    faixa_etaria = case_when(
      V2009 >= 14 & V2009 <= 29 ~ "14 a 29 anos",
      V2009 >= 30 ~ "30 ou mais",
      TRUE ~ NA_character_
    ),
    local = ifelse(UF == 24, "Rio Grande do Norte", "Brasil")
  )

# 📊 Tabela com totais
ocupados_faixa_resumida <- pnad_faixa_resumida %>%
  filter(!is.na(faixa_etaria)) %>%
  group_by(local, faixa_etaria) %>%
  summarise(total = survey_total(vartype = "se"), .groups = "drop")

# 🔢 Proporções
ocupados_faixa_resumida_prop <- ocupados_faixa_resumida %>%
  group_by(local) %>%
  mutate(
    proporcao = total / sum(total) * 100
  ) %>%
  ungroup()

# 💾 Exportar tabela para Excel
write_xlsx(ocupados_faixa_resumida_prop, "ocupados_faixa_etaria_resumida.xlsx")

# 📈 Gráfico - Brasil
grafico_brasil_resumido <- ocupados_faixa_resumida_prop %>%
  filter(local == "Brasil") %>%
  ggplot(aes(x = faixa_etaria, y = total, fill = faixa_etaria)) +
  geom_col() +
  geom_text(aes(label = paste0(round(proporcao, 1), "%")),
            vjust = -0.5, size = 4) +
  labs(
    title = "Distribuição dos ocupados por faixa etária – Brasil (4º tri/2024)",
    x = "Faixa etária", y = "Total de ocupados", fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

ggsave("grafico_ocupados_brasil_faixa_etaria_resumida.png", grafico_brasil_resumido, width = 8, height = 6)
print(grafico_brasil_resumido)

# 📈 Gráfico - RN
grafico_rn_resumido <- ocupados_faixa_resumida_prop %>%
  filter(local == "Rio Grande do Norte") %>%
  ggplot(aes(x = faixa_etaria, y = total, fill = faixa_etaria)) +
  geom_col() +
  geom_text(aes(label = paste0(round(proporcao, 1), "%")),
            vjust = -0.5, size = 4) +
  labs(
    title = "Distribuição dos ocupados por faixa etária – RN (4º tri/2024)",
    x = "Faixa etária", y = "Total de ocupados", fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

ggsave("grafico_ocupados_rn_faixa_etaria_resumida.png", grafico_rn_resumido, width = 8, height = 6)
print(grafico_rn_resumido)



# 📈 Gera gráfico apenas com dados de 2025
df_2025 <- df_final %>%
  filter(grepl("2025", Trimestre)) %>%
  mutate(
    VD4010 = recode(VD4010,
      "01" = "Agropecuária e pesca",
      "02" = "Indústria geral",
      "03" = "Construção",
      "04" = "Comércio e reparação",
      "05" = "Transporte e correio",
      "06" = "Alojamento e alimentação",
      "07" = "Info, finanças e adm.",
      "08" = "Administração pública",
      "09" = "Educação e saúde",
      "10" = "Outros serviços",
      "11" = "Serviços domésticos",
      "12" = "Atividades mal definidas"
    ),
    VD4010 = factor(VD4010, levels = c(
      "Agropecuária e pesca",
      "Indústria geral",
      "Construção",
      "Comércio e reparação",
      "Transporte e correio",
      "Alojamento e alimentação",
      "Info, finanças e adm.",
      "Administração pública",
      "Educação e saúde",
      "Outros serviços",
      "Serviços domésticos",
      "Atividades mal definidas"
    )),
    Trimestre = factor(Trimestre, levels = unique(Trimestre)),
    label_pct = paste0(round(Proporcao * 100, 1), "%")
  )

# 🎨 Gráfico
grafico <- ggplot(df_2025, aes(x = VD4010, y = Proporcao, fill = Local)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = label_pct),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3
  ) +
  facet_wrap(~ Trimestre) +
  labs(
    title = "Distribuição dos Jovens Ocupados (14 a 29 anos)\npor Grupamento de Atividade Principal - 1º Tri 2025",
    x = NULL,
    y = "Proporção"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# 🖥️ Mostra o gráfico
print(grafico)

# 💾 Salva o gráfico
ggsave("grafico_atividade_ocupacao_jovens_2025.png",
       plot = grafico,
       width = 14,
       height = 6,
       dpi = 300)


###################### 15 A 29 ANOS ##########################################################################