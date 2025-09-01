q()

& "C:/Program Files/R/R-4.3.3/bin/R.exe"


  # 🚩 Limpa o ambiente
  rm(list = ls())

  # 📦 Instalação e carregamento dos pacotes
  pacotes <- c("PNADcIBGE", "survey", "srvyr", "dplyr", "tidyr", "ggplot2", "writexl", "openxlsx", "scales")

  novos <- pacotes[!(pacotes %in% installed.packages()[, "Package"])]
  if(length(novos)) install.packages(novos)

  lapply(pacotes, library, character.only = TRUE)

  cat("✅ Pacotes carregados com sucesso!\n")

  # 🗂️ Lista dos trimestres
  trimestres <- c(
    "012023", "022023", "032023", "042023",  # Ano de 2023
    "012024", "022024", "032024", "042024",  # Ano de 2024
    "012025"                                 # Ano de 2025
  )

  # 📁 Diretório base
  diretorio_base <- "D:/repositorio_geral/pnad_continua/PNAD/Dados/"

  # 🗂️ Função de leitura dos dados
  ler_pnad <- function(trimestre) {
    caminho <- paste0(diretorio_base, "PNADC_", trimestre, "/")
    microdados <- paste0(caminho, "PNADC_", trimestre, ".txt")
    input_txt  <- paste0(caminho, "input_PNADC_trimestral.txt")
    
    if (!file.exists(microdados) | !file.exists(input_txt)) {
      stop(paste("❌ Arquivo não encontrado para o trimestre:", trimestre))
    }
    
    read_pnadc(
      microdata = microdados,
      input_txt = input_txt,
      vars = c(
        "UF", "V2009", "V2007", "V2010", "VD4001", "VD4002", "VD4009", "VD4010", "V1028", "UPA", "Estrato"
      )
    ) %>%
      mutate(Trimestre = trimestre)
  }

  # 🔄 Leitura de todos os trimestres
  pnad_lista <- lapply(trimestres, ler_pnad)

  # 🔗 Empilhar os dados (Esperar para carregar o empilhamento)
  pnad_dados <- bind_rows(pnad_lista)

  cat("✅ Dados da PNAD de 2023 a 2025 carregados e empilhados!\n")

  # 🧠 Criação do objeto survey design
  pnad_design <- pnad_dados %>%
    as_survey_design(
      ids = UPA,
      strata = Estrato,
      weights = V1028,
      nest = TRUE
    )

  # 🔧 Ajuste para PSU solitária
  options(survey.lonely.psu = "adjust")

  cat("✅ Objeto survey configurado!\n")



# 🔗 Lista para armazenar os resultados
resultados_part <- list()
resultados_desoc <- list()

# 🔄 Loop para cada trimestre
for (tri in trimestres) {
  
  dados_tri <- subset(pnad_design, Trimestre == tri)
  
  # Criação de variáveis
  dados_tri$variables <- dados_tri$variables %>%
    mutate(
      participa = ifelse(VD4002 %in% c(1, 2), 1, 0),
      desocupado = ifelse(VD4002 == 2, 1, 0),
      sexo = factor(V2007, levels = c(1, 2), labels = c("Homens", "Mulheres"))
    )
  
  ###### ➕ TAXA DE PARTICIPAÇÃO
  
  taxa_part_br <- svymean(~participa, dados_tri, na.rm = TRUE)
  taxa_part_rn <- svymean(~participa, subset(dados_tri, UF == 24), na.rm = TRUE)
  
  taxa_part_sexo_br <- svyby(~participa, ~sexo, dados_tri, svymean, na.rm = TRUE) %>%
    mutate(Local = "Brasil")
  
  taxa_part_sexo_rn <- svyby(~participa, ~sexo, subset(dados_tri, UF == 24), svymean, na.rm = TRUE) %>%
    mutate(Local = "Rio Grande do Norte")
  
  resultado_part <- bind_rows(
    tibble(Sexo = "Total", Local = "Brasil", Taxa = as.numeric(taxa_part_br)),
    tibble(Sexo = "Total", Local = "Rio Grande do Norte", Taxa = as.numeric(taxa_part_rn)),
    taxa_part_sexo_br %>% rename(Taxa = participa) %>% select(Sexo = sexo, Local, Taxa),
    taxa_part_sexo_rn %>% rename(Taxa = participa) %>% select(Sexo = sexo, Local, Taxa)
  ) %>% mutate(Trimestre = tri)
  
  resultados_part[[tri]] <- resultado_part
  
  ###### ➕ TAXA DE DESOCUPAÇÃO
  
  ft_br <- subset(dados_tri, VD4002 %in% c(1, 2))
  ft_rn <- subset(ft_br, UF == 24)
  
  taxa_desoc_br <- svymean(~desocupado, ft_br, na.rm = TRUE)
  taxa_desoc_rn <- svymean(~desocupado, ft_rn, na.rm = TRUE)
  
  taxa_desoc_sexo_br <- svyby(~desocupado, ~sexo, ft_br, svymean, na.rm = TRUE) %>%
    mutate(Local = "Brasil")
  
  taxa_desoc_sexo_rn <- svyby(~desocupado, ~sexo, ft_rn, svymean, na.rm = TRUE) %>%
    mutate(Local = "Rio Grande do Norte")
  
  resultado_desoc <- bind_rows(
    tibble(Sexo = "Total", Local = "Brasil", Taxa = as.numeric(taxa_desoc_br)),
    tibble(Sexo = "Total", Local = "Rio Grande do Norte", Taxa = as.numeric(taxa_desoc_rn)),
    taxa_desoc_sexo_br %>% rename(Taxa = desocupado) %>% select(Sexo = sexo, Local, Taxa),
    taxa_desoc_sexo_rn %>% rename(Taxa = desocupado) %>% select(Sexo = sexo, Local, Taxa)
  ) %>% mutate(Trimestre = tri)
  
  resultados_desoc[[tri]] <- resultado_desoc
}


# 🔗 Consolida as tabelas
tabela_participacao <- bind_rows(resultados_part) %>%
  mutate(`Taxa de Participação (%)` = round(Taxa * 100, 1))

tabela_desocupacao <- bind_rows(resultados_desoc) %>%
  mutate(`Taxa de Desocupação (%)` = round(Taxa * 100, 1))

# 🏷️ Organiza os trimestres com ordem correta
format_tri <- function(x) paste0(as.integer(substr(x, 1, 2)), "º tri/", substr(x, 5, 6))

ordem_trimestres <- c(
  "1º tri/23", "2º tri/23", "3º tri/23", "4º tri/23",
  "1º tri/24", "2º tri/24", "3º tri/24", "4º tri/24",
  "1º tri/25"
)

# 🔧 Função para montar a tabela na ordem correta
montar_tabela_ordenada <- function(tabela, nome_taxa) {
  tabela %>%
    mutate(Trimestre = format_tri(Trimestre)) %>%
    pivot_wider(
      id_cols = c(Trimestre, Local),
      names_from = Sexo,
      values_from = all_of(nome_taxa)
    ) %>%
    select(Trimestre, Local, Total, Homens, Mulheres) %>%
    mutate(Trimestre = factor(Trimestre, levels = ordem_trimestres)) %>%
    arrange(Local, Trimestre)
}

# 🔧 Aplicando na Tabela de Participação e Desocupação
tabela_part_final <- montar_tabela_ordenada(tabela_participacao, "Taxa de Participação (%)")
tabela_desoc_final <- montar_tabela_ordenada(tabela_desocupacao, "Taxa de Desocupação (%)")

# 💾 Exporta Excel
write_xlsx(list(
  "Tabela 1 - Taxa Participação" = tabela_part_final,
  "Tabela 2 - Taxa Desocupação" = tabela_desoc_final
), "Tabelas_Taxas_Participacao_Desocupacao.xlsx")




# 🔄 Dados para gráfico
dados_grafico_part <- tabela_participacao %>%
  mutate(Trimestre = factor(format_tri(Trimestre), levels = ordem_trimestres))

dados_grafico_desoc <- tabela_desocupacao %>%
  mutate(Trimestre = factor(format_tri(Trimestre), levels = ordem_trimestres))


# 🔥 Gráfico 1 – Taxa de Participação
grafico_part <- ggplot(dados_grafico_part, aes(
  x = Trimestre, y = `Taxa de Participação (%)`,
  group = Sexo, color = Sexo
)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 3) +
  geom_text(
    aes(label = `Taxa de Participação (%)`),
    vjust = -0.8, size = 3.8, show.legend = FALSE
  ) +
  facet_wrap(~Local, ncol = 1) +
  scale_color_manual(values = c("Homens" = "#3498DB", "Mulheres" = "#E67E22", "Total" = "#7F8C8D")) +
  labs(
    title = "Gráfico 1 – Taxa de Participação na Força de Trabalho\nBrasil e Rio Grande do Norte – 1º tri/2023 a 1º tri/2025",
    x = NULL, y = "Taxa (%)"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold", lineheight = 1.2),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 11)
  )

ggsave("Grafico_1_Taxa_Participacao.png", plot = grafico_part, width = 10, height = 7, dpi = 300)

print(grafico_part)


# 🔥 Gráfico 2 – Taxa de Desocupação
grafico_desoc <- ggplot(dados_grafico_desoc, aes(
  x = Trimestre, y = `Taxa de Desocupação (%)`,
  group = Sexo, color = Sexo
)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 3) +
  geom_text(
    aes(label = `Taxa de Desocupação (%)`),
    vjust = -0.8, size = 3.8, show.legend = FALSE
  ) +
  facet_wrap(~Local, ncol = 1) +
  scale_color_manual(values = c("Homens" = "#E67E22", "Mulheres" = "#3498DB", "Total" = "#7F8C8D")) +
  labs(
    title = "Gráfico 2 – Taxa de Desocupação\nBrasil e Rio Grande do Norte – 1º tri/2023 a 1º tri/2025",
    x = NULL, y = "Taxa (%)"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold", lineheight = 1.2),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 11)
  )

ggsave("Grafico_2_Taxa_Desocupacao.png", plot = grafico_desoc, width = 10, height = 7, dpi = 300)

print(grafico_desoc)



# 🔗 Lista para armazenar os resultados
resultados_desocup_faixa <- list()

# 🔄 Loop por trimestre
for (tri in trimestres) {
  
  dados_tri <- subset(pnad_design, Trimestre == tri)

  # Criação das variáveis
  dados_tri$variables <- dados_tri$variables %>%
    mutate(
      faixa_etaria = case_when(
        V2009 >= 14 & V2009 <= 29 ~ "14 a 29 anos",
        V2009 >= 30 & V2009 <= 59 ~ "30 a 59 anos",
        V2009 >= 60               ~ "60 anos ou mais",
        TRUE                      ~ NA_character_
      ),
      ativo = ifelse(VD4002 %in% c(1, 2), 1, 0),
      desocupado = ifelse(VD4002 == 2, 1, 0),
      sexo = factor(V2007, levels = c(1, 2), labels = c("Homens", "Mulheres"))
    )
  
  # Filtrar força de trabalho
  ft_brasil <- subset(dados_tri, ativo == 1)
  ft_rn <- subset(ft_brasil, UF == 24)

  ###### 🔹 Brasil
  
  taxa_br_faixa <- svyby(~desocupado, ~faixa_etaria, ft_brasil, svymean, na.rm = TRUE) %>%
    mutate(Sexo = "Total", Local = "Brasil")

  taxa_br_sexo <- svyby(~desocupado, ~interaction(faixa_etaria, sexo), ft_brasil, svymean, na.rm = TRUE) %>%
    separate(`interaction(faixa_etaria, sexo)`, into = c("faixa_etaria", "Sexo"), sep = "\\.") %>%
    mutate(Local = "Brasil")
  
  ###### 🔹 Rio Grande do Norte
  
  taxa_rn_faixa <- svyby(~desocupado, ~faixa_etaria, ft_rn, svymean, na.rm = TRUE) %>%
    mutate(Sexo = "Total", Local = "Rio Grande do Norte")

  taxa_rn_sexo <- svyby(~desocupado, ~interaction(faixa_etaria, sexo), ft_rn, svymean, na.rm = TRUE) %>%
    separate(`interaction(faixa_etaria, sexo)`, into = c("faixa_etaria", "Sexo"), sep = "\\.") %>%
    mutate(Local = "Rio Grande do Norte")
  
  ###### 🔗 Consolida resultados

  resultado_tri <- bind_rows(
    taxa_br_faixa %>% rename(Taxa = desocupado) %>% select(faixa_etaria, Sexo, Local, Taxa),
    taxa_br_sexo %>% rename(Taxa = desocupado) %>% select(faixa_etaria, Sexo, Local, Taxa),
    taxa_rn_faixa %>% rename(Taxa = desocupado) %>% select(faixa_etaria, Sexo, Local, Taxa),
    taxa_rn_sexo %>% rename(Taxa = desocupado) %>% select(faixa_etaria, Sexo, Local, Taxa)
  ) %>% mutate(Trimestre = tri)

  resultados_desocup_faixa[[tri]] <- resultado_tri
}

# 🔗 Consolida a tabela
tabela_desocup_faixa <- bind_rows(resultados_desocup_faixa) %>%
  mutate(`Taxa de Desocupação (%)` = round(Taxa * 100, 1))

# 🏷️ Organiza trimestres
tabela_desocup_faixa_final <- tabela_desocup_faixa %>%
  mutate(Trimestre = format_tri(Trimestre)) %>%
  pivot_wider(
    id_cols = c(Trimestre, Local, faixa_etaria),
    names_from = Sexo,
    values_from = `Taxa de Desocupação (%)`
  ) %>%
  select(Trimestre, Local, faixa_etaria, Total, Homens, Mulheres) %>%
  mutate(
    Trimestre = factor(Trimestre, levels = ordem_trimestres),
    faixa_etaria = factor(faixa_etaria, levels = c(
      "14 a 29 anos", "30 a 59 anos", "60 anos ou mais"
    ))
  ) %>%
  arrange(Local, faixa_etaria, Trimestre)

# 💾 Exporta para Excel
write_xlsx(list(
  "Tabela 3 - Desocupação por Faixa Etária" = tabela_desocup_faixa_final
), "Tabela_Desocupacao_Faixa.xlsx")


# 🔄 Dados para gráfico
dados_grafico_desocup_faixa <- tabela_desocup_faixa %>%
  filter(Sexo == "Total") %>%
  mutate(
    Trimestre = factor(format_tri(Trimestre), levels = ordem_trimestres),
    faixa_etaria = factor(faixa_etaria, levels = c(
      "14 a 29 anos", "30 a 59 anos", "60 anos ou mais"
    ))
  )

# 🔥 Gráfico 4 – Desocupação por Faixa Etária
grafico_desocup_faixa <- ggplot(dados_grafico_desocup_faixa, aes(
  x = Trimestre, y = `Taxa de Desocupação (%)`,
  group = faixa_etaria, color = faixa_etaria
)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 3) +
  geom_text(
    aes(label = `Taxa de Desocupação (%)`),
    vjust = -0.8, size = 3.8, show.legend = FALSE
  ) +
  facet_wrap(~Local, ncol = 1) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Gráfico 4 – Taxa de Desocupação por Faixa Etária\nBrasil e Rio Grande do Norte – 1º tri/2023 a 1º tri/2025",
    x = NULL, y = "Taxa (%)", color = "Faixa Etária"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 11)
  )

# 💾 Salvar
ggsave("Grafico_4_Desocupacao_Faixa.png", plot = grafico_desocup_faixa, width = 10, height = 7, dpi = 300)

print(grafico_desocup_faixa)







# 🔗 Lista para armazenar os resultados
resultados_desocup_raca <- list()

# 🔄 Loop por trimestre
for (tri in trimestres) {
  
  # 🔍 Filtra os dados do trimestre
  dados_tri <- subset(pnad_dados, Trimestre == tri) %>%
    filter(!is.na(V2010), !is.na(VD4002), !is.na(V1028), !is.na(UPA), !is.na(Estrato))

  # 🔨 Cria variáveis necessárias
  dados_tri <- dados_tri %>%
    mutate(
      cor_raca = case_when(
        V2010 == 1 ~ "Branca",
        V2010 %in% c(2, 4) ~ "Preta ou Parda",
        V2010 %in% c(3, 5) ~ NA_character_,  # Ignora Amarela e Indígena
        TRUE ~ NA_character_
      ),
      ativo = ifelse(VD4002 %in% c(1, 2), 1, 0),
      desocupado = ifelse(VD4002 == 2, 1, 0)
    ) %>%
    filter(!is.na(cor_raca))

  # 🔧 Cria o objeto survey design do trimestre
  design_tri <- dados_tri %>%
    as_survey_design(
      ids = UPA,
      strata = Estrato,
      weights = V1028,
      nest = TRUE
    )

  # ⚙️ Ajusta PSU solitária
  options(survey.lonely.psu = "adjust")

  # 🔹 Força de trabalho
  ft <- subset(design_tri, ativo == 1)
  ft_rn <- subset(ft, UF == 24)
  ft_br <- subset(ft, UF != 24 | is.na(UF))  # Brasil inclui todos exceto UF 24

  # 🔥 Calcula para Brasil
  taxa_br_raca <- svyby(~desocupado, ~cor_raca, ft, svymean, na.rm = TRUE) %>%
    mutate(Local = "Brasil")

  # 🔥 Calcula para RN
  taxa_rn_raca <- svyby(~desocupado, ~cor_raca, ft_rn, svymean, na.rm = TRUE) %>%
    mutate(Local = "Rio Grande do Norte")

  # 🔗 Junta
  resultado_tri <- bind_rows(
    taxa_br_raca %>% rename(Taxa = desocupado) %>% select(cor_raca, Local, Taxa),
    taxa_rn_raca %>% rename(Taxa = desocupado) %>% select(cor_raca, Local, Taxa)
  ) %>% mutate(Trimestre = tri)

  resultados_desocup_raca[[tri]] <- resultado_tri
}

# 🔗 Consolida a tabela
tabela_desocup_raca <- bind_rows(resultados_desocup_raca) %>%
  mutate(`Taxa de Desocupação (%)` = round(Taxa * 100, 1))

# 🏗️ Organiza
tabela_desocup_raca_final <- tabela_desocup_raca %>%
  mutate(Trimestre = format_tri(Trimestre)) %>%
  complete(
    Trimestre,
    Local = c("Brasil", "Rio Grande do Norte"),
    cor_raca = c("Branca", "Preta ou Parda")
  ) %>%
  select(Trimestre, Local, cor_raca, `Taxa de Desocupação (%)`) %>%
  mutate(
    Trimestre = factor(Trimestre, levels = ordem_trimestres),
    cor_raca = factor(cor_raca, levels = c("Branca", "Preta ou Parda"))
  ) %>%
  arrange(Local, cor_raca, Trimestre)

# 💾 Exporta
write_xlsx(list(
  "Tabela - Desocupacao por Cor_Raca" = tabela_desocup_raca_final
), "Tabela_Desocupacao_CorRaca_Corrigida.xlsx")



# 🔄 Dados para gráfico
dados_grafico_desocup_raca <- tabela_desocup_raca_final

# 🔥 Gráfico
grafico_desocup_raca <- ggplot(dados_grafico_desocup_raca, aes(
  x = Trimestre, y = `Taxa de Desocupação (%)`,
  group = cor_raca, color = cor_raca
)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 3) +
  geom_text(
    aes(label = `Taxa de Desocupação (%)`),
    vjust = -0.8, size = 3.5, show.legend = FALSE
  ) +
  facet_wrap(~Local, ncol = 1) +
  scale_color_manual(values = c("Branca" = "#1F77B4", "Preta ou Parda" = "#FF7F0E")) +
  labs(
    title = "Gráfico – Taxa de Desocupação por Cor/Raça\nBrasil e Rio Grande do Norte – 1º tri/2023 a 1º tri/2025",
    x = NULL, y = "Taxa (%)", color = "Cor/Raça"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 11)
  )

# 💾 Salvar
ggsave("Grafico_Desocupacao_CorRaca_Corrigido.png", plot = grafico_desocup_raca, width = 10, height = 7, dpi = 300)

print(grafico_desocup_raca)










# 🔗 Lista para armazenar os resultados
resultados_ocup_desocup <- list()

# 🔄 Loop por trimestre
for (tri in trimestres) {
  
  dados_tri <- subset(pnad_design, Trimestre == tri)

  # 🔹 Ocupados (VD4002 == 1)
  ocup_br <- svytotal(~factor(VD4002), dados_tri, na.rm = TRUE)["factor(VD4002)1"]
  ocup_rn <- svytotal(~factor(VD4002), subset(dados_tri, UF == 24), na.rm = TRUE)["factor(VD4002)1"]

  # 🔹 Desocupados (VD4002 == 2)
  desoc_br <- svytotal(~factor(VD4002), dados_tri, na.rm = TRUE)["factor(VD4002)2"]
  desoc_rn <- svytotal(~factor(VD4002), subset(dados_tri, UF == 24), na.rm = TRUE)["factor(VD4002)2"]

  # 🔗 Monta a tabela do trimestre
  resultado_tri <- tibble(
    Trimestre = tri,
    Local = c("Brasil", "Rio Grande do Norte"),
    Ocupados = c(as.numeric(ocup_br), as.numeric(ocup_rn)),
    Desocupados = c(as.numeric(desoc_br), as.numeric(desoc_rn))
  )

  resultados_ocup_desocup[[tri]] <- resultado_tri
}

# 🔗 Junta todos os trimestres
tabela_ocup_desocup <- bind_rows(resultados_ocup_desocup) %>%
  mutate(
    Ocupados = round(Ocupados / 1000, 1),      # em mil pessoas
    Desocupados = round(Desocupados / 1000, 1) # em mil pessoas
  )

# 🏗️ Organiza
tabela_ocup_desocup_final <- tabela_ocup_desocup %>%
  mutate(
    Trimestre = format_tri(Trimestre),
    Trimestre = factor(Trimestre, levels = ordem_trimestres)
  ) %>%
  arrange(Local, Trimestre)

# 💾 Exporta
write_xlsx(list(
  "Tabela - Ocupados e Desocupados" = tabela_ocup_desocup_final
), "Tabela_Ocupados_Desocupados_Corrigido_FINAL.xlsx")

# ✅ Visualiza
print(tabela_ocup_desocup_final)






# ============================================================
# PNADc – Informalidade (tabelas, sem gráficos)
# Período: 1º tri/2023 a 1º tri/2025
# Métrica: taxa de informalidade, ocupados, informais, formais
# Recortes: Brasil | Rio Grande do Norte | (sexo | faixa etária | raça/cor)
# ============================================================

# 🧹 Limpar ambiente
rm(list = ls())
gc()

# 📦 Pacotes
suppressPackageStartupMessages({
  library(PNADcIBGE)
  library(dplyr)
  library(srvyr)
  library(survey)
  library(readr)
  library(openxlsx)
  library(purrr)
  library(stringr)
})

# ⚙️ Opções
options(survey.lonely.psu = "adjust")  # tratar estratos/UPA solitários
options(timeout = 600)

# 📂 Pastas
dir_temp <- "D:/pnad_temp"
if (!dir.exists(dir_temp)) dir.create(dir_temp, showWarnings = FALSE)

dir_out  <- "D:/repositorio_geral/pnad_continua/informalidade_tabelas_2023a2025_1tri"
if (!dir.exists(dir_out)) dir.create(dir_out, showWarnings = FALSE)

# 🗓️ Trimestres alvo (1º tri/2023 → 1º tri/2025)
trimestres <- list(
  list(tri = 1, ano = 2023),
  list(tri = 2, ano = 2023),
  list(tri = 3, ano = 2023),
  list(tri = 4, ano = 2023),
  list(tri = 1, ano = 2024),
  list(tri = 2, ano = 2024),
  list(tri = 3, ano = 2024),
  list(tri = 4, ano = 2024),
  list(tri = 1, ano = 2025)
)

# 🔄 Retry genérico (mesmo espírito do seu código)
executar_com_retry <- function(funcao, tri, ano, max_tentativas = 5, espera = 15) {
  tentativa <- 1
  repeat {
    tryCatch({
      out <- funcao(tri, ano)
      message(sprintf("✅ Sucesso: %dº tri/%d", tri, ano))
      return(out)
    }, error = function(e) {
      message(sprintf("⚠️ Erro no %dº tri/%d - Tentativa %d de %d", tri, ano, tentativa, max_tentativas))
      message("Erro: ", conditionMessage(e))
      if (tentativa >= max_tentativas) stop(sprintf("❌ Máximo de tentativas: %dº tri/%d", tri, ano))
      tentativa <<- tentativa + 1
      Sys.sleep(espera)
    })
  }
}

# 🔧 Importa + desenho amostral (baixando on-line a cada trimestre)
variaveis <- c(
  "Ano","Trimestre","UF","Estrato","UPA","V1028",
  "V2007","V2009","V2010","VD4002","V4012","V4019","V4029"
)

importa_pnad_design <- function(tri, ano) {
  dados <- get_pnadc(
    year    = ano,
    quarter = tri,
    vars    = variaveis,
    labels  = FALSE,
    design  = FALSE,
    savedir = dir_temp
  )

  # Garantir tipos numéricos
  dados <- dados %>% mutate(across(all_of(variaveis), \(x) suppressWarnings(as.numeric(x))))

  # Desenho amostral clássico
  as_survey_design(dados, ids = UPA, strata = Estrato, weights = V1028, nest = TRUE)
}

# 🧮 Variável de informalidade (regra IBGE/PNADc usada no seu código)
# - Empregado do setor privado sem carteira:   V4012==3 & V4029==2
# - Empregado doméstico sem carteira:          V4012==1 & V4029==2
# - Empregador sem CNPJ:                       V4012==5 & V4019==2
# - Conta-própria sem CNPJ:                    V4012==6 & V4019==2
# - Trabalhador familiar auxiliar:             V4012==7
# -> demais = formal
cria_informal <- function(df_srvyr) {
  df_srvyr %>%
    mutate(
      informal = case_when(
        V4012 == 3 & V4029 == 2 ~ 1,
        V4012 == 1 & V4029 == 2 ~ 1,
        V4012 == 5 & V4019 == 2 ~ 1,
        V4012 == 6 & V4019 == 2 ~ 1,
        V4012 == 7             ~ 1,
        TRUE                   ~ 0
      )
    )
}

# 📌 Helpers de rótulos
rotulo_tri <- function(tri, ano) sprintf("%02d%s/%d", tri, "º tri", ano)

# ============================================================
# 1) Tabela TOTAL (Brasil e RN)
# ============================================================
calc_informal_total <- function(tri, ano) {
  dsg <- importa_pnad_design(tri, ano)

  # Ocupados (VD4002==1)
  dsg_occ <- dsg %>% filter(VD4002 == 1)

  # Brasil: usar todos os registros
  br <- dsg_occ %>%
    cria_informal() %>%
    summarise(
      ocupados  = survey_total(na.rm = TRUE),
      informais = survey_total(informal, na.rm = TRUE)
    ) %>%
    mutate(local = "Brasil")

  # RN: filtrar UF==24
  rn <- dsg_occ %>%
    filter(UF == 24) %>%
    cria_informal() %>%
    summarise(
      ocupados  = survey_total(na.rm = TRUE),
      informais = survey_total(informal, na.rm = TRUE)
    ) %>%
    mutate(local = "Rio Grande do Norte")

  bind_rows(br, rn) %>%
    mutate(
      trimestre = rotulo_tri(tri, ano),
      ocupados_k  = round(ocupados / 1000, 1),
      informais_k = round(informais / 1000, 1),
      formais_k   = round((ocupados - informais) / 1000, 1),
      taxa_informalidade = round((informais / ocupados) * 100, 1)
    ) %>%
    select(trimestre, local, ocupados_k, informais_k, formais_k, taxa_informalidade)
}

# ============================================================
# 2) Tabela por SEXO (Brasil e RN)
# ============================================================
calc_informal_sexo <- function(tri, ano) {
  dsg <- importa_pnad_design(tri, ano)
  dsg_occ <- dsg %>% filter(VD4002 == 1)

  # Função auxiliar para um recorte (Brasil ou RN)
  aux <- function(dsg_sub, rotulo_local) {
    dsg_sub %>%
      cria_informal() %>%
      mutate(sexo = if_else(V2007 == 1, "Homem", "Mulher")) %>%
      group_by(sexo) %>%
      summarise(
        ocupados  = survey_total(na.rm = TRUE),
        informais = survey_total(informal, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        local   = rotulo_local,
        trimestre = rotulo_tri(tri, ano),
        ocupados_k  = round(ocupados / 1000, 1),
        informais_k = round(informais / 1000, 1),
        formais_k   = round((ocupados - informais) / 1000, 1),
        taxa_informalidade = round((informais / ocupados) * 100, 1)
      ) %>%
      select(trimestre, local, sexo, ocupados_k, informais_k, formais_k, taxa_informalidade)
  }

  bind_rows(
    aux(dsg_occ, "Brasil"),
    aux(dsg_occ %>% filter(UF == 24), "Rio Grande do Norte")
  )
}

# ============================================================
# 3) Tabela por FAIXA ETÁRIA (Brasil e RN)
# ============================================================
calc_informal_faixa <- function(tri, ano) {
  dsg <- importa_pnad_design(tri, ano)
  dsg_occ <- dsg %>% filter(VD4002 == 1)

  faixas <- function(x) case_when(
    x >= 14 & x <= 24 ~ "14 a 24 anos",
    x >= 25 & x <= 39 ~ "25 a 39 anos",
    x >= 40 & x <= 59 ~ "40 a 59 anos",
    x >= 60          ~ "60 anos ou mais",
    TRUE ~ NA_character_
  )

  aux <- function(dsg_sub, rotulo_local) {
    dsg_sub %>%
      cria_informal() %>%
      mutate(faixa_etaria = faixas(V2009)) %>%
      filter(!is.na(faixa_etaria)) %>%
      group_by(faixa_etaria) %>%
      summarise(
        ocupados  = survey_total(na.rm = TRUE),
        informais = survey_total(informal, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        local   = rotulo_local,
        trimestre = rotulo_tri(tri, ano),
        ocupados_k  = round(ocupados / 1000, 1),
        informais_k = round(informais / 1000, 1),
        formais_k   = round((ocupados - informais) / 1000, 1),
        taxa_informalidade = round((informais / ocupados) * 100, 1)
      ) %>%
      select(trimestre, local, faixa_etaria, ocupados_k, informais_k, formais_k, taxa_informalidade)
  }

  bind_rows(
    aux(dsg_occ, "Brasil"),
    aux(dsg_occ %>% filter(UF == 24), "Rio Grande do Norte")
  )
}

# ============================================================
# 4) Tabela por RAÇA/COR (Brasil e RN)
# ============================================================
calc_informal_raca <- function(tri, ano) {
  dsg <- importa_pnad_design(tri, ano)
  dsg_occ <- dsg %>% filter(VD4002 == 1)

  lbl_raca <- function(v2010) case_when(
    v2010 == 1 ~ "Branco",
    v2010 == 2 ~ "Preto",
    v2010 %in% c(3,4,5) ~ "Demais Raças",
    TRUE ~ NA_character_
  )

  aux <- function(dsg_sub, rotulo_local) {
    dsg_sub %>%
      cria_informal() %>%
      mutate(raca = lbl_raca(V2010)) %>%
      filter(!is.na(raca)) %>%
      group_by(raca) %>%
      summarise(
        ocupados  = survey_total(na.rm = TRUE),
        informais = survey_total(informal, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        local   = rotulo_local,
        trimestre = rotulo_tri(tri, ano),
        ocupados_k  = round(ocupados / 1000, 1),
        informais_k = round(informais / 1000, 1),
        formais_k   = round((ocupados - informais) / 1000, 1),
        taxa_informalidade = round((informais / ocupados) * 100, 1)
      ) %>%
      select(trimestre, local, raca, ocupados_k, informais_k, formais_k, taxa_informalidade)
  }

  bind_rows(
    aux(dsg_occ, "Brasil"),
    aux(dsg_occ %>% filter(UF == 24), "Rio Grande do Norte")
  )
}

# ============================================================
# 5) Rodar para todos os trimestres e salvar
# ============================================================

# Executa e empilha
tb_total <- bind_rows(lapply(trimestres, \(t) executar_com_retry(calc_informal_total, t$tri, t$ano)))
tb_sexo  <- bind_rows(lapply(trimestres, \(t) executar_com_retry(calc_informal_sexo,  t$tri, t$ano)))
tb_faixa <- bind_rows(lapply(trimestres, \(t) executar_com_retry(calc_informal_faixa, t$tri, t$ano)))
tb_raca  <- bind_rows(lapply(trimestres, \(t) executar_com_retry(calc_informal_raca,  t$tri, t$ano)))

# CSVs
write_csv2(tb_total, file.path(dir_out, "informalidade_total_BR_RN.csv"))
write_csv2(tb_sexo,  file.path(dir_out, "informalidade_por_sexo_BR_RN.csv"))
write_csv2(tb_faixa, file.path(dir_out, "informalidade_por_faixa_BR_RN.csv"))
write_csv2(tb_raca,  file.path(dir_out, "informalidade_por_raca_BR_RN.csv"))

# Excel consolidado (abas)
wb <- createWorkbook()
addWorksheet(wb, "Total"); writeData(wb, "Total", tb_total)
addWorksheet(wb, "Sexo");  writeData(wb, "Sexo",  tb_sexo)
addWorksheet(wb, "Faixa"); writeData(wb, "Faixa", tb_faixa)
addWorksheet(wb, "Raca");  writeData(wb, "Raca",  tb_raca)
saveWorkbook(wb, file.path(dir_out, "informalidade_consolidado_BR_RN.xlsx"), overwrite = TRUE)

message("🏁 Finalizado: tabelas salvas em ", dir_out)

























########### Agrupamentos de atividades
# 📦 Pacotes
library(PNADcIBGE)
library(dplyr)
library(srvyr)
library(survey)
library(writexl)

# ✅ Evitar erro de PSU único
options(survey.lonely.psu = "adjust")

# 📁 Caminho dos dados
caminho_base <- "D:/repositorio_geral/pnad_continua/PNAD/Dados/PNADC_012025/"

# 🚀 Leitura dos microdados
pnad_raw <- read_pnadc(
  microdata = paste0(caminho_base, "PNADC_012025.txt"),
  input_txt = paste0(caminho_base, "input_PNADC_trimestral.txt"),
  vars = c("UF", "UPA", "V1028", "Estrato", "VD4002", "VD4010", "V4012", "V4019", "V4029")
)

# 🔍 Filtrar ocupados e classificar informalidade
pnad_tratado <- pnad_raw %>%
  filter(VD4002 == "1") %>%
  mutate(
    local = ifelse(UF == "24", "Rio Grande do Norte", "Brasil"),
    VD4010 = as.numeric(VD4010),  # 🔥 Conversão correta para evitar erro na classificação
    informal = case_when(
      V4012 == "3" & V4029 == "2" ~ 1,
      V4012 == "1" & V4029 == "2" ~ 1,
      V4012 == "5" & V4019 == "2" ~ 1,
      V4012 == "6" & V4019 == "2" ~ 1,
      V4012 == "7" ~ 1,
      TRUE ~ 0
    )
  )

# ✅ Filtrar apenas informais com atividade definida
pnad_filtrado <- pnad_tratado %>%
  filter(informal == 1, !is.na(VD4010))

# 📊 Criar objeto survey
pnad_design <- pnad_filtrado %>%
  as_survey_design(
    ids = UPA,
    strata = Estrato,
    weights = V1028,
    nest = TRUE
  )

# 📈 Gerar tabela com atividades renomeadas corretamente
tabela_atividade <- pnad_design %>%
  group_by(local, VD4010) %>%
  summarise(informais = survey_total(na.rm = TRUE), .groups = "drop") %>%
  group_by(local) %>%
  mutate(percentual = 100 * informais / sum(informais)) %>%
  ungroup() %>%
  mutate(
    atividade = case_when(
      VD4010 == 1  ~ "Agropecuária, produção florestal, pesca e aquicultura",
      VD4010 == 2  ~ "Indústrias extrativas e de transformação, eletricidade, gás, água, esgoto e gestão de resíduos",
      VD4010 == 3  ~ "Construção",
      VD4010 == 4  ~ "Comércio, reparação de veículos automotores e motocicletas",
      VD4010 == 5  ~ "Transporte, armazenagem e correio",
      VD4010 == 6  ~ "Alojamento e alimentação",
      VD4010 == 7  ~ "Informação, comunicação, financeiras, imobiliárias, profissionais e administrativas",
      VD4010 == 8  ~ "Administração pública, defesa e seguridade social",
      VD4010 == 9  ~ "Educação, saúde humana e serviços sociais",
      VD4010 == 10 ~ "Outros serviços",
      VD4010 == 11 ~ "Serviços domésticos",
      VD4010 == 12 ~ "Atividades mal definidas",
      TRUE ~ "Não informado"
    )
  ) %>%
  select(local, VD4010, atividade, informais, percentual) %>%
  arrange(local, desc(percentual))

# ✔️ Ver resultado no console
print(tabela_atividade, n = Inf)

# 💾 Exportar
write_xlsx(tabela_atividade, "D:/repositorio_geral/pnad_continua/Tabela_Distribuicao_Informais_Atividade_012025_FINAL.xlsx")



# 📦 Pacotes
library(ggplot2)
library(dplyr)
library(readxl)
library(patchwork)
library(scales)

# 📁 Caminho da planilha
caminho <- "D:/repositorio_geral/pnad_continua/Tabela_Distribuicao_Informais_Atividade_012025.xlsx"
tabela_atividade <- read_xlsx(caminho)

# 🔧 Arredondar e preparar dados
tabela_atividade <- tabela_atividade %>%
  mutate(
    percentual = round(percentual, 2),
    percentual_txt = format(percentual, nsmall = 2, decimal.mark = ","),
    atividade = factor(atividade, levels = rev(unique(atividade)))
  )

# 🎨 Cores mais escuras
cores_escuras <- rep("#005f73", length(unique(tabela_atividade$atividade)))

# 📊 Função para gráfico horizontal
grafico_barras <- function(dados, titulo = NULL, subtitulo = NULL) {
  ggplot(dados, aes(x = percentual, y = atividade, fill = atividade)) +
    geom_col(width = 0.6) +
    geom_text(
      aes(label = percentual_txt),
      hjust = -0.15, size = 3.5, color = "black", fontface = "bold"
    ) +
    scale_fill_manual(values = cores_escuras, guide = "none") +
    scale_x_continuous(labels = label_number(decimal.mark = ",")) +
    labs(title = titulo, subtitle = subtitulo, x = NULL, y = NULL) +
    coord_cartesian(clip = "off", xlim = c(0, max(dados$percentual) + 5)) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )
}

# 🔎 Filtrar dados
dados_brasil <- tabela_atividade %>% filter(local == "Brasil")
dados_rn     <- tabela_atividade %>% filter(local == "Rio Grande do Norte")

# 📈 Gerar gráficos
graf_brasil <- grafico_barras(
  dados_brasil,
  titulo = "Distribuição percentual dos ocupados informais por atividade",
  subtitulo = "Brasil - 1º trimestre de 2025"
)

graf_rn <- grafico_barras(
  dados_rn,
  titulo = NULL,
  subtitulo = "Rio Grande do Norte - 1º trimestre de 2025"
)

# 🖼️ Combinar com título geral
grafico_final <- graf_brasil / graf_rn

# 📤 Exibir no R
print(grafico_final)

# 💾 Salvar como PNG com fundo transparente
ggsave(
  filename = "D:/repositorio_geral/pnad_continua/Grafico_Informais_Atividade_012025.png",
  plot = grafico_final,
  width = 10, height = 8, dpi = 300, bg = "transparent"
)


###################Rendimentos reais deflacionado pela variável habitual###################################

# 📦 Pacotes
library(PNADcIBGE)
library(dplyr)
library(srvyr)
library(openxlsx)

# 📂 Caminhos
caminho_base <- "D:/repositorio_geral/pnad_continua/PNAD/Dados/"

# 🔄 Lista de trimestres a processar
trimestres <- c("012023", "022023", "032023", "042023",
                "012024", "022024", "032024", "042024",
                "012025")

# 📌 Função para processar cada trimestre
processar_trimestre <- function(tri) {
  microdados <- paste0(caminho_base, "PNADC_", tri, "/PNADC_", tri, ".txt")
  input_txt  <- paste0(caminho_base, "PNADC_", tri, "/input_PNADC_trimestral.txt")
  
  # 📥 Ler microdados com deflator habitual
  pnad <- read_pnadc(
    microdata = microdados,
    input_txt = input_txt,
    vars = c("UF", "VD4016", "habitual", "V1028", "UPA", "Estrato")
  )
  
  # 🧮 Calcular rendimento real diretamente no microdado
  pnad <- pnad %>%
    mutate(rendimento_real = VD4016 * habitual)
  
  # 📊 Definir desenho amostral
  pnad_srvyr <- as_survey_design(pnad, ids = UPA, strata = Estrato, weights = V1028)
  
  # ✅ Brasil (total direto)
  tabela_brasil <- pnad_srvyr %>%
    summarise(
      rendimento_real = survey_mean(rendimento_real, na.rm = TRUE),
      rendimento_nominal = survey_mean(VD4016, na.rm = TRUE)
    ) %>%
    mutate(local = "Brasil", trimestre = tri)
  
  # ✅ Rio Grande do Norte (UF == 24)
  tabela_rn <- pnad_srvyr %>%
    filter(UF == "24") %>%
    summarise(
      rendimento_real = survey_mean(rendimento_real, na.rm = TRUE),
      rendimento_nominal = survey_mean(VD4016, na.rm = TRUE)
    ) %>%
    mutate(local = "Rio Grande do Norte", trimestre = tri)
  
  return(bind_rows(tabela_brasil, tabela_rn))
}

# 🔄 Rodar todos os trimestres
tabela_final <- bind_rows(lapply(trimestres, processar_trimestre))

# 🔀 Reordenar colunas para melhor visualização
tabela_final <- tabela_final %>%
  select(trimestre, local, rendimento_nominal, rendimento_real)

# 💾 Exportar para Excel
wb <- createWorkbook()
addWorksheet(wb, "Rendimento Real")
writeData(wb, "Rendimento Real", tabela_final)
setColWidths(wb, "Rendimento Real", cols = 1:4, widths = "auto")
freezePane(wb, "Rendimento Real", firstActiveRow = 2)
saveWorkbook(wb, file = paste0(caminho_base, "Tabela_Rendimento_Real_Direto.xlsx"), overwrite = TRUE)

print("✅ Tabela com Brasil (total direto) e RN (UF 24) gerada com sucesso!")



#####################RENDIMENTOS MÉDIOS REAIS DOS OCUPADOS - Calculo Manual ###################
##############################################################################

& "C:/Program Files/R/R-4.3.3/bin/R.exe"

# 📦 Pacotes
library(PNADcIBGE)
library(dplyr)
library(srvyr)
library(purrr)
library(openxlsx)

# ⚙️ Permitir estratos com apenas uma UPA
options(survey.lonely.psu = "certainty")

# 📁 Caminhos
caminho_base <- "D:/repositorio_geral/pnad_continua/PNAD/Dados/"
trimestres <- c("012023", "022023", "032023", "042023",
                "012024", "022024", "032024", "042024", "012025")

# 🔁 Função para processar um trimestre
processar_trimestre <- function(tri) {
  cat("🔍 Processando:", tri, "\n")
  caminho <- paste0(caminho_base, "PNADC_", tri, "/")
  
  pnad <- read_pnadc(
    microdata = paste0(caminho, "PNADC_", tri, ".txt"),
    input_txt  = paste0(caminho, "input_PNADC_trimestral.txt"),
    vars = c("UF", "UPA", "Estrato", "V1028", "VD4002", "VD4016")
  )
  
  pnad <- pnad %>%
    filter(VD4002 == 1, !is.na(VD4016)) %>%  # Somente ocupados com valor
    mutate(
      local = ifelse(UF == "24", "Rio Grande do Norte", "Brasil"),
      rendimento = as.numeric(VD4016)
    )
  
  pnad_design <- as_survey_design(
    pnad,
    ids = UPA,
    strata = Estrato,
    weights = V1028,
    nest = TRUE
  )
  
  pnad_design %>%
    group_by(local) %>%
    summarise(rendimento_medio = survey_mean(rendimento, na.rm = TRUE)) %>%
    mutate(trimestre = tri)
}

# 🚀 Aplicar a todos os trimestres
tabela_rendimento_nominal <- map_dfr(trimestres, processar_trimestre)

# 💾 Exportar para Excel
wb <- createWorkbook()
addWorksheet(wb, "Rendimento Nominal")
writeData(wb, sheet = "Rendimento Nominal", tabela_rendimento_nominal)
setColWidths(wb, sheet = "Rendimento Nominal", cols = 1:3, widths = "auto")
freezePane(wb, "Rendimento Nominal", firstActiveRow = 2)

# Salvar
saveWorkbook(wb,
  file = "D:/repositorio_geral/pnad_continua/Tabela_Rendimento_Nominal.xlsx",
  overwrite = TRUE
)







# Pacotes
library(dplyr)
library(readxl)
library(openxlsx)
library(stringr)

# Caminhos
caminho_base <- "D:/repositorio_geral/pnad_continua/"
arquivo_nominal <- paste0(caminho_base, "Tabela_Rendimento_Nominal.xlsx")
arquivo_deflator <- paste0(caminho_base, "PNAD/Dados/PNADC_012025/deflator_PNADC_2025_trimestral_010203.xls")

# Ler rendimentos nominais
tabela_nominal <- read.xlsx(arquivo_nominal, sheet = "Rendimento Nominal") %>%
  mutate(
    ano = substr(trimestre, 3, 6),
    trim = case_when(
      str_sub(trimestre, 1, 2) == "01" ~ "01-02-03",
      str_sub(trimestre, 1, 2) == "02" ~ "04-05-06",
      str_sub(trimestre, 1, 2) == "03" ~ "07-08-09",
      str_sub(trimestre, 1, 2) == "04" ~ "10-11-12",
      TRUE ~ NA_character_
    )
  )

# Ler deflatores por UF
deflatores_uf <- read_excel(arquivo_deflator, sheet = "deflator") %>%
  rename(ano = 1, trim = 2, uf = 3, habitual = 4)

# Deflator médio Brasil (média dos estados por trimestre)
deflator_br <- deflatores_uf %>%
  group_by(ano, trim) %>%
  summarise(deflator_brasil = mean(habitual, na.rm = TRUE), .groups = "drop")

# Deflator RN (UF 24)
deflator_rn <- deflatores_uf %>%
  filter(uf == 24) %>%
  rename(deflator_rn = habitual)

# Juntar ambos deflatores
deflatores <- deflator_br %>%
  left_join(deflator_rn, by = c("ano", "trim"))

# Obter índice base (Brasil 01-02-03/2025)
indice_base_brasil <- deflatores %>%
  filter(ano == 2025, trim == "01-02-03") %>%
  pull(deflator_brasil)

# Calcular deflatores relativos
deflatores <- deflatores %>%
  mutate(
    deflator_brasil = deflator_brasil / indice_base_brasil,
    deflator_rn = deflator_rn / indice_base_brasil
  )

# Juntar com rendimentos e aplicar deflatores distintos
tabela_deflacionada <- tabela_nominal %>%
  left_join(deflatores, by = c("ano", "trim")) %>%
  mutate(
    rendimento_nominal = rendimento_medio,
    rendimento_real = case_when(
      local == "Brasil" ~ round(rendimento_medio / deflator_brasil, 2),
      local == "Rio Grande do Norte" ~ round(rendimento_medio / deflator_rn, 2),
      TRUE ~ NA_real_
    )
  ) %>%
  select(trimestre, ano, trim, local, rendimento_nominal,
         deflator_brasil, deflator_rn, rendimento_real) %>%
  arrange(local, ano, trim)

# Exportar para Excel
wb <- createWorkbook()
addWorksheet(wb, "Rendimento Real")
writeData(wb, "Rendimento Real", tabela_deflacionada)
setColWidths(wb, "Rendimento Real", cols = 1:8, widths = "auto")
freezePane(wb, "Rendimento Real", firstActiveRow = 2)

saveWorkbook(wb,
  file = paste0(caminho_base, "Tabela_Rendimento_Real_Habitual_Completa.xlsx"),
  overwrite = TRUE
)
###############################################################################################################
################################################################################################################
################################################################################################################

#########################################Tabelas por regiões##################################


& "C:/Program Files/R/R-4.3.3/bin/R.exe"


# 📦 Pacotes
library(PNADcIBGE)
library(dplyr)

# ⚙️ Caminhos
caminho_base <- "D:/repositorio_geral/pnad_continua/PNAD/Dados/PNADC_012025/"
microdados <- paste0(caminho_base, "PNADC_012025.txt")
input_txt  <- paste0(caminho_base, "input_PNADC_trimestral.txt")

# 📥 Ler dados com V1023
pnad <- read_pnadc(
  microdata = microdados,
  input_txt = input_txt,
  vars = c("UF", "V1023")
)

# 🔍 Filtrar RN e contar por V1023
pnad %>%
  filter(UF == "24") %>%
  count(V1023) %>%
  mutate(
    Tipo_Area = case_when(
      V1023 == "1" ~ "Capital (Natal)",
      V1023 == "2" ~ "Resto da Região Metropolitana",
      V1023 == "3" ~ "RIDE",
      V1023 == "4" ~ "Interior (fora RM e RIDE)"
    )
  )
