#### TRATAMENTO DAS BASES PARA UNIR Dados_Master_Ecoeficiencia.csv e Base_Mapbiomas
#
# 📌 Carregar pacotes necessários
# library(dplyr)
# library(readr)

# 📌 2. Ler os dados de eficiência e MapBiomas
# eco_data <- Dados_Master_Ecoeficiencia
# mapbiomas_data <- Base_Mapbiomas_Tratada

# 📌 3. Verificar a estrutura das bases carregadas
# cat("Estrutura da base de eficiência:\n")
# print(str(eco_data))

# cat("\nEstrutura da base de biomas:\n")
# print(str(mapbiomas_data))

# 📌 4. Renomear a coluna Geocode para compatibilidade
# mapbiomas_data <- mapbiomas_data %>%
# rename(Geocode = geocode) %>%  # Ajusta o nome da coluna se necessário
# select(Geocode, biome) %>%  # Mantém apenas as colunas essenciais
# distinct(Geocode, .keep_all = TRUE)  # Remove possíveis duplicatas

# 📌 5. Converter as colunas Geocode para o mesmo tipo
# eco_data$CódIBGE <- as.character(eco_data$CódIBGE)
# mapbiomas_data$Geocode <- as.character(mapbiomas_data$Geocode)

# 📌 6. Realizar o JOIN entre as bases (left join para manter todas as DMUs da base de eficiência)
# df_merged <- eco_data %>%
#  left_join(mapbiomas_data, by = c("CódIBGE" = "Geocode")) %>%
#  distinct(CódIBGE, .keep_all = TRUE)  # Garantir que não haja repetições de DMUs

# 📌 7. Verificar a quantidade de municípios antes e depois do join
# cat("\nTotal de municípios na base de eficiência:", nrow(eco_data), "\n")
# cat("Total de municípios na base de biomas:", nrow(mapbiomas_data), "\n")
# cat("Total de municípios após o JOIN:", nrow(df_merged), "\n")

# 📌 8. Identificar municípios que ficaram sem bioma atribuído
# missing_biomas <- df_merged %>% filter(is.na(biome)) %>% select(CódIBGE)
# cat("\nMunicípios sem bioma definido:", nrow(missing_biomas), "\n")

# 📌 9. Salvar o novo arquivo CSV com a estrutura corrigida
# write.csv(df_merged, "Dados_Master_Ecoeficiencia_Com_Bioma.csv", row.names = FALSE)

# cat("✅ Arquivo 'Dados_Master_Ecoeficiencia_Com_Bioma.csv' gerado com sucesso!\n")


#####
# 1. APLICAÇÃO DOS MODELO DEA BCC OO (VRS_OO) e CRS_OO 
# BIOMAS CERRADO E AMAZÔNIA JUNTOS
# CÁLCULO DAS EFICIÊNCIAS E ÍNDICES
# SALVAMENTO DOS DADOS EM ARQUIVO .CSV
#####



# Carregar pacotes necessários
library(Benchmarking)
library(tidyverse)
library(deaR)
library(ggplot2)
library(viridis)
library(sf)
library(geobr)
library(dplyr)

# Função para dividir os dados em partes
dividir_em_partes <- function(data, tamanho_do_arquivo) {
  split(data, ceiling(seq_len(nrow(data)) / tamanho_do_arquivo))
}

# 📌 Ler a base de eficiência já ajustada com a coluna Bioma
data_biomas <- read.csv("Dados_Master_Ecoeficiencia_Com_Bioma.csv")

# 📌 Filtrar apenas os biomas Cerrado e Amazônia
data_biomas <- data_biomas %>% filter(biome %in% c("Cerrado", "Amazônia"))


# Verificar se os dados foram corretamente filtrados
if (nrow(data_biomas) == 0) {
  stop("Nenhum município encontrado nos biomas Cerrado ou Amazônia. Verifique os dados!")
}

# Dividir os dados em partes de no máximo 100 linhas
tamanho_do_arquivo <- 100
dados_das_partes_biomas <- dividir_em_partes(data_biomas, tamanho_do_arquivo)

# Criar arquivo temporário para salvar os resultados por partes
arquivo_temporario_dea <- tempfile(fileext = ".csv")

conflicted::conflicts_prefer(deaR::efficiencies)

# Processar cada parte e salvar os resultados
for (i in seq_along(dados_das_partes_biomas)) {
  cat("Processando parte:", i, "/", length(dados_das_partes_biomas), "\n")
  
  # Selecionar a parte atual
  parte <- dados_das_partes_biomas[[i]]
  
  # Verificar se a parte está vazia
  if (nrow(parte) == 0) {
    cat("Parte", i, "está vazia. Pulando para a próxima parte.\n")
    next
  }
  
  # Preparar os inputs e outputs
  inputs <- as.matrix(parte[, c("x1", "x2", "x3", "x4", "x5")])
  outputs <- as.matrix(parte[, c("y1", "y2", "Iy3", "Iy4")])
  context <- parte[, c("z1", "z2", "z3", "z4", "z5", "z6", "z7", "z8", "z9")]
  
  # Criar os dados DEA
  data_biomas_dea <- make_deadata(parte, dmus = 1, inputs = 9:13, outputs = 14:17)
  
  # Aplicar o modelo DEA orientado a outputs com retornos variáveis (VRS)
  result_biomas_vrs <- model_basic(data_biomas_dea, orientation = 'oo', rts = "vrs")
  
  # Aplicar o modelo DEA orientado a outputs com retornos constantes (CRS)
  result_biomas_crs <- model_basic(data_biomas_dea, orientation = 'oo', rts = "crs")
  
  # 📌 Calcular eficiência e normalizá-la para ambos os modelos
  EFF_VRS <- efficiencies(result_biomas_vrs)
  EFF_Norm_VRS <- 1 / EFF_VRS  # Ajuste correto para garantir escala de 0 a 1
  
  EFF_CRS <- efficiencies(result_biomas_crs)
  EFF_Norm_CRS <- 1 / EFF_CRS  # Ajuste correto para garantir escala de 0 a 1
  
  # 📌 Substituir valores infinitos por 1 (pois as DMUs eficientes têm eficiência 1)
  EFF_Norm_VRS[is.infinite(EFF_Norm_VRS)] <- 1
  EFF_Norm_CRS[is.infinite(EFF_Norm_CRS)] <- 1
  
  # Criar um data.frame com os resultados para a parte
  resultados_parte <- data.frame(
    Cidade = parte$MUNICÍPIO,
    Bioma = parte$biome,
    Estado = parte$ESTADO,
    Regiao = parte$REGIÃO,
    Geocode = parte$CódIBGE,
    Eficiencia_VRS = EFF_VRS,
    Efic_Norm_VRS = EFF_Norm_VRS,
    Eficiencia_CRS = EFF_CRS,
    Efic_Norm_CRS = EFF_Norm_CRS,
    Latitude = parte$z8,
    Longitude = parte$z9,
    x1 = parte$x1,
    x2 = parte$x2,
    x3 = parte$x3,
    x4 = parte$x4,
    x5 = parte$x5,
    y1 = parte$y1,
    y2 = parte$y2,
    Iy3 = parte$Iy3,
    Iy4 = parte$Iy4,
    context
  )
  
    # Escrever os resultados da parte diretamente no arquivo CSV
  write.table(
    resultados_parte,
    file = arquivo_temporario_dea,
    sep = ",",
    col.names = (i == 1),  # Adicionar cabeçalhos apenas na primeira parte
    row.names = FALSE,
    append = (i != 1),     # Append para as partes seguintes
    quote = FALSE
  )
  
  # Liberar memória usada pela parte
  rm(parte, inputs, outputs, data_biomas_dea, result_biomas_vrs, result_biomas_crs, resultados_parte)
  gc()
}

# Renomear o arquivo temporário para o nome final
file.rename(arquivo_temporario_dea, "Resultados_Particionados_DEA_Biomas_Cerrado_Amazonia.csv")

# Carregar os resultados consolidados do modelo DEA particionado
resultados_biomas <- read.csv("Resultados_Particionados_DEA_Biomas_Cerrado_Amazonia.csv")



# FIM DO CÁLCULO DE EFICIÊNCIAS E SALVAMENTO DOS ARQUIVOS DEA


# Estatísticas descritivas para o grupo com os 2 biomas

# 📌 Carregar pacotes necessários
library(dplyr)
library(readr)

# 📌 Verificar se a coluna biome existe no dataframe
if (!"biome" %in% colnames(resultados_biomas)) {
  stop("Erro: A coluna 'biome' não foi encontrada no dataframe 'resultados_biomas'.")
}

# 📌 Exibir os valores únicos da coluna biome para verificar inconsistências
print(unique(resultados_biomas$Bioma))



# 📌 Estatísticas descritivas das eficiências absolutas (Eficiencia_CRS)
estatisticas_absolutas_crs <- resultados_biomas %>%
  group_by(Bioma) %>%
  summarise(
    Minimo = min(Eficiencia_CRS, na.rm = TRUE),
    Maximo = max(Eficiencia_CRS, na.rm = TRUE),
    Media = mean(Eficiencia_CRS, na.rm = TRUE)
  )

# 📌 Exibir os resultados absolutos
print(estatisticas_absolutas_crs)

# 📌 Estatísticas descritivas das eficiências normalizadas (Efic_Norm_CRS)
estatisticas_normalizadas_crs <- resultados_biomas %>%
  group_by(Bioma) %>%
  summarise(
    Minimo = min(Efic_Norm_CRS, na.rm = TRUE),
    Maximo = max(Efic_Norm_CRS, na.rm = TRUE),
    Media = mean(Efic_Norm_CRS, na.rm = TRUE)
  )

# 📌 Exibir os resultados normalizados
print(estatisticas_normalizadas_crs)


# 📌 Estatísticas descritivas das eficiências absolutas (Eficiencia_VRS)
estatisticas_absolutas <- resultados_biomas %>%
  group_by(Bioma) %>%
  summarise(
    Minimo = min(Eficiencia_VRS, na.rm = TRUE),
    Maximo = max(Eficiencia_VRS, na.rm = TRUE),
    Media = mean(Eficiencia_VRS, na.rm = TRUE)
  )

# 📌 Exibir os resultados absolutos
print(estatisticas_absolutas)

# 📌 Estatísticas descritivas das eficiências normalizadas (Efic_Norm_VRS)
estatisticas_normalizadas <- resultados_biomas %>%
  group_by(Bioma) %>%
  summarise(
    Minimo = min(Efic_Norm_VRS, na.rm = TRUE),
    Maximo = max(Efic_Norm_VRS, na.rm = TRUE),
    Media = mean(Efic_Norm_VRS, na.rm = TRUE)
  )

# 📌 Exibir os resultados normalizados
print(estatisticas_normalizadas)


#####
# FIM DO CÁLCULO DE EFICIÊNCIAS E SALVAMENTO DOS ARQUIVOS VRS_OO E CRS_OO
# BIOMAS CERRADO E AMAZÔNIA JUNTOS
#####



#####
# 2. TESTE DE MODELOS - TESTE KS PARA OS MODELOS VRS E CRS
#    DOS BIOMAS CERRADO E AMAZÔNIA JUNTOS
#####

# Carregar pacotes necessários
library(dplyr)
library(ggplot2)

# Carregar os dados de eficiência consolidados para ambos os biomas
resultados_biomas <- read.csv("Resultados_Particionados_DEA_Biomas_Cerrado_Amazonia.csv")

# Extraindo as eficiências para os modelos VRS_OO e CRS_OO
G_Biomas_vrs_oo <- c(resultados_biomas$Eficiencia_VRS)
G_Biomas_crs_oo <- c(resultados_biomas$Eficiencia_CRS)

# Realizando o teste KS entre os modelos VRS_OO e CRS_OO para ambos os biomas
ks_test_biomas <- ks.test(G_Biomas_vrs_oo, G_Biomas_crs_oo)

# Exibir os resultados do teste KS
print(ks_test_biomas)


# Resultado:
# 	Asymptotic two-sample Kolmogorov-Smirnov test

# data:  G_Biomas_vrs_oo and G_Biomas_crs_oo
# D = 0.25654, p-value < 2.2e-16
# alternative hypothesis: two-sided

# Visualização das distribuições de eficiência
plot_data <- resultados_biomas %>% 
  select(Eficiencia_VRS, Eficiencia_CRS) %>% 
  gather(key = "Modelo", value = "Eficiencia", Eficiencia_VRS, Eficiencia_CRS)

# Plotar as distribuições
p <- ggplot(plot_data, aes(x = Eficiencia, fill = Modelo)) +
  geom_density(alpha = 0.6) +
  scale_fill_viridis_d() +
  theme_minimal() +
  labs(title = "Distribuição das Eficiências dos Modelos VRS e CRS",
       x = "Eficiência",
       y = "Densidade")

print(p)


#####
# FIM DO TESTE DE MODELOS
#####


#####
# 3. CÁLCULO DOS ÍNDICES (SLACKS, LAMBDAS, TARGETS, METAS) 
# BIOMAS CERRADO E AMAZÔNIA JUNTOS
#####

# Carregar os pacotes necessários
library(deaR)
library(dplyr)
library(ggplot2)
library(gridExtra)

# 📌 Carregar pacotes necessários
library(deaR)
library(writexl)
library(dplyr)

# 📌 Verificar o número de linhas no dataframe de entrada
cat("Número de DMUs em resultados_biomas:", nrow(resultados_biomas), "\n")

# 📌 Preparar os dados novamente para rodar apenas os índices
data_biomas_dea <- make_deadata(resultados_biomas, dmus = 1, inputs = 12:16, outputs = 17:20)

# 📌 Aplicar o modelo DEA apenas para os índices (VRS - Output-Oriented)
result_biomas_vrs <- model_basic(data_biomas_dea, orientation = 'oo', rts = "vrs")

# 📌 Calcular os índices
slacks_VRS_OO <- as.data.frame(slacks(result_biomas_vrs))  # Slacks (resíduos de ineficiência)
lambdas_VRS_OO <- as.data.frame(lambdas(result_biomas_vrs))  # Pesos das DMUs referência
targets_VRS_OO <- as.data.frame(targets(result_biomas_vrs))  # Metas ajustadas
references_VRS_OO <- references(result_biomas_vrs)  # DMUs referência (lista)
returns_VRS_OO <- as.character(rts(result_biomas_vrs))  # Retornos de escala

# 📌 Converter referências para texto e garantir tamanho correto
references_VRS_OO <- sapply(references_VRS_OO, function(x) paste(x, collapse = ";"))

# 📌 Ajustar tamanho do vetor de referências
if (length(references_VRS_OO) < nrow(resultados_biomas)) {
  # Se faltar linhas, preenche com NA
  references_VRS_OO <- c(references_VRS_OO, rep(NA, nrow(resultados_biomas) - length(references_VRS_OO)))
} else if (length(references_VRS_OO) > nrow(resultados_biomas)) {
  # Se houver linhas a mais, remove o excesso
  references_VRS_OO <- references_VRS_OO[1:nrow(resultados_biomas)]
}

# 📌 Confirmar que agora tem o mesmo número de linhas
cat("Novo número de linhas em references_VRS_OO:", length(references_VRS_OO), "\n")


# 📌 Verificar o número de linhas de cada objeto
cat("Número de linhas em resultados_biomas:", nrow(resultados_biomas), "\n")
cat("Número de linhas em slacks_VRS_OO:", nrow(slacks_VRS_OO), "\n")
cat("Número de linhas em lambdas_VRS_OO:", nrow(lambdas_VRS_OO), "\n")
cat("Número de linhas em targets_VRS_OO:", nrow(targets_VRS_OO), "\n")
cat("Número de linhas em references_VRS_OO:", length(references_VRS_OO), "\n")
cat("Número de linhas em returns_VRS_OO:", length(returns_VRS_OO), "\n")

# 📌 Ajustar referências para texto e garantir tamanho correto
references_VRS_OO <- sapply(references_VRS_OO, function(x) paste(x, collapse = ";"))
if (length(references_VRS_OO) != nrow(resultados_biomas)) {
  references_VRS_OO <- rep(NA, nrow(resultados_biomas))  # Ajusta tamanho se necessário
}

# 📌 Garantir que slacks, lambdas e targets tenham o mesmo número de linhas
if (nrow(slacks_VRS_OO) != nrow(resultados_biomas)) {
  slacks_VRS_OO <- matrix(NA, nrow = nrow(resultados_biomas), ncol = ncol(slacks_VRS_OO))
}
if (nrow(lambdas_VRS_OO) != nrow(resultados_biomas)) {
  lambdas_VRS_OO <- matrix(NA, nrow = nrow(resultados_biomas), ncol = ncol(lambdas_VRS_OO))
}
if (nrow(targets_VRS_OO) != nrow(resultados_biomas)) {
  targets_VRS_OO <- matrix(NA, nrow = nrow(resultados_biomas), ncol = ncol(targets_VRS_OO))
}
if (length(returns_VRS_OO) != nrow(resultados_biomas)) {
  returns_VRS_OO <- rep(NA, nrow(resultados_biomas))
}

# 📌 Aplicar o modelo DEA apenas para calcular os índices (VRS - Output-Oriented)
data_biomas_dea <- make_deadata(resultados_biomas, dmus = 1, inputs = 12:16, outputs = 17:20)
result_biomas_vrs <- model_basic(data_biomas_dea, orientation = 'oo', rts = "vrs")

# 📌 Calcular os índices (iguais ao modelo original)
slacks_VRS_OO <- slacks(result_biomas_vrs)    # Slacks (resíduos de ineficiência)
lambdas_VRS_OO <- lambdas(result_biomas_vrs)  # Pesos das DMUs referência
targets_VRS_OO <- targets(result_biomas_vrs)  # Metas ajustadas
references_VRS_OO <- references(result_biomas_vrs)  # DMUs referência
returns_VRS_OO <- rts(result_biomas_vrs)  # Retornos de escala

# 📌 Visualizar os resultados
plot(result_biomas_vrs)
ggsave("Grafico_Benchmarks_DEA_VRS_Biomas.png", plot = last_plot(), width = 12, height = 8, dpi = 300)

plot(result_biomas_vrs)
ggsave("Grafico_EficientesxIneficientes_DEA_VRS_Biomas.png", plot = last_plot(), width = 12, height = 8, dpi = 300)


# 📌 Exportar para Excel (respeitando a estrutura original)
summary(result_biomas_vrs, exportExcel = TRUE, filename = "Resultados_DEA_Biomas.xlsx")


cat("✅ Arquivo Excel 'Resultados_DEA_Biomas.xlsx' gerado com sucesso!\n")


#### Benchmarks
# 📌 Lista das 40 benchmarks mais usadas
benchmarks <- c(
  "Iguatemi", "Betim", "Igarapé", "Telêmaco Borba", "Olhos-d'Água", "Capim Branco", 
  "Belo Horizonte", "Pedra Branca do Amapari", "Brasília", "Araraquara", "Tabocão", 
  "Santa Izabel do Pará", "Sorriso", "Águas Lindas de Goiás", "Anhanguera", 
  "São Paulo de Olivença", "Corumbá", "Rosário", "Barra do Ouro", "São José da Lapa", 
  "Cutias", "Terra Alta", "São Francisco do Pará", "Carmolândia", "Valparaíso de Goiás", 
  "Jataí", "Mira Estrela", "Curvelândia", "Salinópolis", "Terezópolis de Goiás", 
  "São João Batista", "Vitória do Jari", "Ananindeua", "Marituba", "Angical do Piauí", 
  "Confins", "Melgaço", "Gouveia", "Axixá", "Itobi"
)

# 📌 Filtrar os dados do dataframe "resultados_biomas"
dados_benchmarks <- resultados_biomas %>%
  filter(Cidade %in% benchmarks) %>%
  select(Cidade, Estado, Bioma)

# 📌 Criar a tabela formatada para LaTeX
latex_tabela <- paste0(
  "\\begin{table}[h]\n",
  "\\centering\n",
  "\\begin{tabular}{lll}\n",
  "\\hline\n",
  "Cidade & Estado & Bioma \\\\\n",
  "\\hline\n",
  paste(apply(dados_benchmarks, 1, function(row) paste(row, collapse = " & ")), collapse = " \\\\\n"),
  " \\\\\n",
  "\\hline\n",
  "\\end{tabular}\n",
  "\\caption{Top 40 Benchmarks e seus respectivos Estados e Biomas}\n",
  "\\label{tab:benchmarks}\n",
  "\\end{table}"
)

# 📌 Salvar a tabela em um arquivo .tex
writeLines(latex_tabela, "tabela_benchmarks.tex")

# 📌 Exibir a tabela LaTeX no console
cat(latex_tabela)



#####
# FIM DO CÁLCULO DOS ÍNDICES
#####



#####
# 4. ANÁLISES ESTATÍSTICAS (TESTE KS E KW)
#####

# Carregar pacotes necessários
library(dplyr)
library(conflicted)

conflicts_prefer(dplyr::filter)


# 📌 Realizar o Teste KS para comparar as distribuições de eficiência entre os biomas Cerrado e Amazônia
ks_result <- ks.test(
  resultados_biomas %>% filter(Bioma == "Cerrado") %>% pull(Eficiencia_VRS),
  resultados_biomas %>% filter(Bioma == "Amazônia") %>% pull(Eficiencia_VRS),
  alternative = "two.sided"
)
cat("Resultado do Teste KS:\n")
print(ks_result)

# 📌 Realizar o Teste de Kruskal-Wallis para verificar diferenças entre os biomas
kw_result <- kruskal.test(Eficiencia_VRS ~ Bioma, data = resultados_biomas)
cat("\nResultado do Teste KW:\n")
print(kw_result)


#####
# FIM DO CÁLCULO DAS ANÁLISES ESTATÍSTICAS (TESTE KS E KW)
#####


#####
# 5. ESTATÍSTICAS DESCRITIVAS
#####

cat("\nEstatísticas Descritivas por Bioma CRS:\n")

estatisticas_biomas_crs <- resultados_biomas %>%
  group_by(Bioma) %>%
  summarize(
    Média = mean(Eficiencia_CRS, na.rm = TRUE),
    Mediana = median(Eficiencia_CRS, na.rm = TRUE),
    DesvioPadrão = sd(Eficiencia_CRS, na.rm = TRUE),
    Mínimo = min(Eficiencia_CRS, na.rm = TRUE),
    Máximo = max(Eficiencia_CRS, na.rm = TRUE),
    Contagem = n()
  )

print(estatisticas_biomas_crs)

# Visualizar o resumo completo
cat("\nResumo Estatístico Completo:\n")
print(summary(resultados_biomas$Eficiencia_CRS))



cat("\nEstatísticas Descritivas por Bioma VRS:\n")

estatisticas_biomas <- resultados_biomas %>%
  group_by(Bioma) %>%
  summarize(
    Média = mean(Eficiencia_VRS, na.rm = TRUE),
    Mediana = median(Eficiencia_VRS, na.rm = TRUE),
    DesvioPadrão = sd(Eficiencia_VRS, na.rm = TRUE),
    Mínimo = min(Eficiencia_VRS, na.rm = TRUE),
    Máximo = max(Eficiencia_VRS, na.rm = TRUE),
    Contagem = n()
  )

print(estatisticas_biomas)

# Visualizar o resumo completo
cat("\nResumo Estatístico Completo:\n")
print(summary(resultados_biomas$Eficiencia_VRS))

# Gráfico comparativo das distribuições de eficiência
cat("\nGráfico Comparativo das Eficiências entre os Biomas:\n")

ggplot(resultados_biomas, aes(x = Eficiencia_VRS, fill = Bioma)) +
  geom_density(alpha = 0.6) +
  scale_fill_viridis_d() +
  theme_minimal() +
  labs(title = "Distribuição das Eficiências por Bioma",
       x = "Eficiência",
       y = "Densidade")


#####
# FIM DAS ESTATÍSTICAS DESCRITIVAS
#####


#####
# 6. GRÁFICOS COMPARATIVOS
#####

# Carregar pacotes necessários
library(ggplot2)
library(dplyr)
library(viridis)

# 6.1. Histograma Comparativo (Eficiência Observada)
histograma_biomas <- ggplot(resultados_biomas, aes(x = Eficiencia_VRS, fill = Bioma, color = Bioma)) +
  geom_histogram(binwidth = 0.05, alpha = 0.6, position = "identity") +
  geom_density(aes(y = ..count..), size = 1.2, alpha = 0.8) +
  scale_fill_viridis_d(name = "Bioma") +
  scale_color_viridis_d(name = "Bioma") +
  labs(
    title = "Distribuição das Eficiências por Bioma",
    subtitle = "Comparativo entre Cerrado e Amazônia",
    x = "Eficiência Observada",
    y = "Frequência"
  ) +
  theme_minimal()

print(histograma_biomas)
ggsave("Histograma_Comparativo_Biomas.png", plot = histograma_biomas, width = 10, height = 8, dpi = 300)


# 6.2. Histograma Comparativo (Eficiência Normalizada)
histograma_normalizado <- ggplot(resultados_biomas, aes(x = Efic_Norm_VRS, fill = Bioma)) +
  geom_histogram(position = "identity", binwidth = 0.05, alpha = 0.6, color = "black") +
  scale_fill_viridis_d() +
  labs(
    title = "Distribuição Comparativa das Eficiências Normalizadas por Bioma",
    x = "Eficiência Normalizada",
    y = "Frequência"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "right"
  )

print(histograma_normalizado)
ggsave("Histograma_Comparativo_Normalizado_Biomas.png", plot = histograma_normalizado, width = 10, height = 8, dpi = 300)


# 6.3. Boxplot Comparativo
boxplot_biomas <- ggplot(resultados_biomas, aes(x = Bioma, y = Eficiencia_VRS, fill = Bioma)) +
  geom_boxplot(alpha = 0.7, color = "black") +
  scale_fill_viridis_d(name = "Bioma") +
  labs(
    title = "Boxplot das Eficiências por Bioma",
    subtitle = "Comparativo entre Cerrado e Amazônia",
    x = "Bioma",
    y = "Eficiência Observada"
  ) +
  theme_minimal()

print(boxplot_biomas)
ggsave("Boxplot_Comparativo_Biomas.png", plot = boxplot_biomas, width = 10, height = 8, dpi = 300)


# 6.4. Percentual de Municípios por Cluster (separado por bioma)
percentual_por_cluster <- resultados_biomas %>%
  mutate(cluster = case_when(
    Efic_Norm_VRS <= 0.25 ~ "0.00 - 0.25",
    Efic_Norm_VRS > 0.25 & Efic_Norm_VRS <= 0.50 ~ "0.26 - 0.50",
    Efic_Norm_VRS > 0.50 & Efic_Norm_VRS <= 0.75 ~ "0.51 - 0.75",
    Efic_Norm_VRS > 0.75 ~ "0.76 - 1.00"
  )) %>%
  group_by(Bioma, cluster) %>%
  summarise(Contagem = n()) %>%
  mutate(Percentual = Contagem / sum(Contagem) * 100)

# Gerar o gráfico de barras comparativo
histograma_percentual <- ggplot(percentual_por_cluster, aes(x = cluster, y = Percentual, fill = Bioma)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", alpha = 0.8) +
  scale_fill_viridis_d(name = "Bioma") +
  labs(
    title = "Percentual de Municípios por Cluster de Eficiência",
    subtitle = "Distribuição Comparativa por Bioma (Cerrado e Amazônia)",
    x = "Cluster de Eficiência",
    y = "Percentual (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "right"
  )

# Mostrar o gráfico
print(histograma_percentual)

# Salvar o gráfico como imagem
ggsave("Histograma_Percentual_Clusters_Biomas.png", plot = histograma_percentual, width = 10, height = 8, dpi = 300)

cat("\nHistograma de percentual por cluster gerado e salvo com sucesso.\n")

# 6.5 Histogramas de eficiência normalizadas por cluster para cada bioma
# 📌 Carregar pacotes necessários
library(ggplot2)
library(dplyr)

# 📌 Definir os intervalos (clusters) de eficiência normalizada
resultados_biomas <- resultados_biomas %>%
  mutate(cluster = case_when(
    Efic_Norm_VRS <= 0.25 ~ "0.00 - 0.25",
    Efic_Norm_VRS > 0.25 & Efic_Norm_VRS <= 0.50 ~ "0.26 - 0.50",
    Efic_Norm_VRS > 0.50 & Efic_Norm_VRS <= 0.75 ~ "0.51 - 0.75",
    Efic_Norm_VRS > 0.75 ~ "0.76 - 1.00"
  ))

# 📌 Criar histogramas separados por bioma

# 🔹 Histograma para o Bioma Cerrado
histograma_cerrado <- ggplot(resultados_biomas %>% filter(Bioma == "Cerrado"), 
                             aes(x = Efic_Norm_VRS, fill = cluster)) +
  geom_histogram(bins = 30, color = "black", alpha = 0.8) +
  scale_fill_viridis_d(name = "Cluster") +
  labs(
    title = "Distribuição das Eficiências Normalizadas - Cerrado",
    x = "Eficiência Normalizada (VRS)",
    y = "Frequência"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "right"
  )

# 🔹 Boxplot para o Bioma Cerrado
boxplot_cerrado <- ggplot(resultados_biomas %>% filter(Bioma == "Cerrado"), 
                          aes(x = cluster, y = Efic_Norm_VRS, fill = cluster)) +
  geom_boxplot(alpha = 0.8, color = "black") +
  scale_fill_viridis_d(name = "Cluster") +
  labs(
    title = "Boxplot das Eficiências por Cluster",
    subtitle = "Bioma Cerrado",
    x = "Cluster",
    y = "Eficiência Normalizada"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "none"
  )




# 🔹 Histograma para o Bioma Amazônia
histograma_amazonia <- ggplot(resultados_biomas %>% filter(Bioma == "Amazônia"), 
                              aes(x = Efic_Norm_VRS, fill = cluster)) +
  geom_histogram(bins = 30, color = "black", alpha = 0.8) +
  scale_fill_viridis_d(name = "Cluster") +
  labs(
    title = "Distribuição das Eficiências Normalizadas - Amazônia",
    x = "Eficiência Normalizada (VRS)",
    y = "Frequência"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "right"
  )

# 🔹 Boxplot para o Bioma Amazônia
boxplot_amazonia <- ggplot(resultados_biomas %>% filter(Bioma == "Amazônia"), 
                           aes(x = cluster, y = Efic_Norm_VRS, fill = cluster)) +
  geom_boxplot(alpha = 0.8, color = "black") +
  scale_fill_viridis_d(name = "Cluster") +
  labs(
    title = "Boxplot das Eficiências por Cluster",
    subtitle = "Bioma Amazônia",
    x = "Cluster",
    y = "Eficiência Normalizada"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "none"
  )



# 📌 Mostrar os gráficos
print(histograma_cerrado)
print(boxplot_cerrado)
print(histograma_amazonia)
print(boxplot_amazonia)

# 📌 Salvar os gráficos como imagens
ggsave("Histograma_Eficiencia_Cerrado.png", plot = histograma_cerrado, width = 10, height = 8, dpi = 300)
ggsave("Boxplot_Eficiencia_Cerrado.png", plot = boxplot_cerrado, width = 10, height = 8, dpi = 300)
ggsave("Histograma_Eficiencia_Amazonia.png", plot = histograma_amazonia, width = 10, height = 8, dpi = 300)
ggsave("Boxplot_Eficiencia_Amazonia.png", plot = boxplot_amazonia, width = 10, height = 8, dpi = 300)

cat("\n✅ Histogramas e boxplots das eficiências normalizadas por bioma gerados e salvos com sucesso!\n")


# 6.6 Gráficos de DMUs Eficientes X Ineficientes e Benchmarks
# 📌 Carregar pacotes necessários
library(ggplot2)
library(dplyr)

# 📌 Carregar os resultados dos 2 biomas juntos
resultados_biomas <- read.csv("Resultados_Particionados_DEA_Biomas_Cerrado_Amazonia.csv")

# 📌 Definir inputs e outputs
inputs_consolidados <- as.matrix(resultados_biomas[, c("x1", "x2", "x3", "x4", "x5")])
outputs_consolidados <- as.matrix(resultados_biomas[, c("y1", "y2", "Iy3", "Iy4")])

# 📌 Criar os dados DEA com as DMUs já calculadas
data_consolidado <- make_deadata(resultados_biomas, dmus = 1, inputs = 9:13, outputs = 14:17)

# 📌 Aplicar o modelo DEA BCC VRS orientado a outputs
result_consolidado_VRS_OO <- model_basic(data_consolidado, orientation = 'oo', rts = "vrs")

# 📌 Gerar o gráfico de eficientes x ineficientes
plot(result_consolidado_VRS_OO)
ggsave("Grafico_Eficientes_Ineficientes_Biomas_VRS_OO.png", width = 10, height = 8, dpi = 300)

# 📌 Gerar o gráfico de benchmarks
plot(result_consolidado_VRS_OO)
ggsave("Grafico_Benchmarks_Biomas_VRS_OO.png", width = 10, height = 8, dpi = 300)

cat("Gráficos de eficiência e benchmarks gerados e salvos com sucesso!\n")


# 📌 Carregar os dados de índices
indices_biomas <- read_csv("Indices_DEA_VRS_OO_Biomas.csv")

# 📌 Separar benchmarks referenciados corretamente (removendo NAs e entradas vazias)
benchmarks_referenciados <- indices_biomas %>%
  filter(!is.na(References) & References != "") %>%
  separate_rows(References, sep = ";") %>%  # Expandir as DMUs listadas em cada referência
  count(References, name = "Qtd_vezes_usado") %>%
  rename(Geocode = References) %>%
  arrange(desc(Qtd_vezes_usado))

# 📌 Adicionar a coluna do Bioma ao dataset de benchmarks
benchmarks_referenciados <- benchmarks_referenciados %>%
  left_join(indices_biomas %>% select(Geocode, Bioma), by = "Geocode")

# 📌 Selecionar as 40 DMUs mais utilizadas como referência
top_40_benchmarks <- benchmarks_referenciados %>%
  slice_max(Qtd_vezes_usado, n = 40)

# 📌 Exibir os 40 principais benchmarks
print(top_40_benchmarks)


# 📌 Identificar as 40 DMUs ineficientes mais próximas das metas
ineficientes_proximas_metas <- indices_biomas %>%
  filter(Efficiency < 1) %>%  # Considerar apenas DMUs ineficientes
  mutate(Distancia_Metas = abs(Efficiency - 1)) %>%  # Calcular a distância até a eficiência 1
  arrange(Distancia_Metas) %>%
  slice_min(Distancia_Metas, n = 40)  # Selecionar as 40 mais próximas

# 📌 Exibir as 40 DMUs ineficientes mais próximas das metas
print(ineficientes_proximas_metas)




#####
# FIM DOS GRÁFICOS COMPARATIVOS
#####

#####
# 7. MAPAS DE ECOEFICIÊNCIA DOS BIOMAS
####

# 📌 Carregar pacotes necessários
library(ggplot2)
library(sf)
library(dplyr)
library(viridis)
library(geobr)

# 📌 Baixar limites dos municípios e estados
br_municipios <- read_municipality(year = 2020) %>% mutate(code_muni = as.numeric(code_muni))
estados <- read_state(year = 2020)

# 📌 Converter geocodes para numérico e mesclar os dados de eficiência
resultados_biomas <- resultados_biomas %>% mutate(Geocode = as.numeric(Geocode))
br_municipios <- br_municipios %>%
  left_join(resultados_biomas, by = c("code_muni" = "Geocode"))

# 📌 Filtrar apenas municípios dos biomas Cerrado e Amazônia
br_municipios_biomas <- br_municipios %>% filter(biome %in% c("Cerrado", "Amazônia"))
br_municipios_cerrado <- br_municipios_biomas %>% filter(biome == "Cerrado")
br_municipios_amazonia <- br_municipios_biomas %>% filter(biome == "Amazônia")

# 📌 Criar clusters de eficiência para visualização (Quartis)
br_municipios_biomas <- br_municipios_biomas %>%
  mutate(cluster = case_when(
    Efic_Norm_VRS <= 0.25 ~ "0.00 - 0.25",
    Efic_Norm_VRS > 0.25 & Efic_Norm_VRS <= 0.50 ~ "0.26 - 0.50",
    Efic_Norm_VRS > 0.50 & Efic_Norm_VRS <= 0.75 ~ "0.51 - 0.75",
    Efic_Norm_VRS > 0.75 ~ "0.76 - 1.00"
  ))

# 📌 Função para gerar mapas de clusters (mantendo a paleta usada anteriormente)
gerar_mapa_clusters <- function(data, titulo, nome_arquivo) {
  ggplot(data) +
    geom_sf(aes(fill = cluster, geometry = geom), color = "black", size = 0.1) + # Removendo áreas pretas
    geom_sf(data = estados, fill = NA, color = "blue", size = 0.5) +
    scale_fill_viridis_d(name = "Cluster de Eficiência") +  # **Paleta de cores original mantida**
    labs(title = titulo, subtitle = "Baseado no modelo DEA BCC", x = "Longitude", y = "Latitude") +
    theme_minimal(base_family = "Arial") +
    theme(
      plot.background = element_rect(fill = "gray95", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "gray90"),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      legend.position = "right",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10)
    ) -> mapa
  ggsave(nome_arquivo, plot = mapa, width = 10, height = 8, dpi = 300)
  print(mapa)
}

# 📌 Função para gerar mapas de eficiência absoluta (somente tons de verde)
gerar_mapa_eficiencia <- function(data, titulo, nome_arquivo) {
  ggplot(data) +
    geom_sf(aes(fill = Efic_Norm_VRS, geometry = geom), color = "white", size = 0.1) + # Evitar preto
    geom_sf(data = estados, fill = NA, color = "gray40", size = 0.5) +
    scale_fill_gradient(low = "lightgreen", high = "darkgreen", name = "Eficiência") + # **Agora 100% em verde**
    labs(title = titulo, subtitle = "Baseado no modelo DEA BCC", x = "Longitude", y = "Latitude") +
    theme_minimal(base_family = "Arial") +
    theme(
      plot.background = element_rect(fill = "gray95", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "gray90"),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      legend.position = "right",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10)
    ) -> mapa
  ggsave(nome_arquivo, plot = mapa, width = 10, height = 8, dpi = 300)
  print(mapa)
}

# 📌 Gerar os mapas de CLUSTERS (mantendo a mesma paleta usada antes)
gerar_mapa_clusters(br_municipios_biomas, "Clusters de Eficiência - Biomas Cerrado e Amazônia", "Mapa_Clusters_Biomas.png")
gerar_mapa_clusters(br_municipios_cerrado, "Clusters de Eficiência - Bioma Cerrado", "Mapa_Clusters_Cerrado.png")
gerar_mapa_clusters(br_municipios_amazonia, "Clusters de Eficiência - Bioma Amazônia", "Mapa_Clusters_Amazonia.png")

# 📌 Gerar os mapas de EFICIÊNCIA ABSOLUTA (correção para tons de verde)
gerar_mapa_eficiencia(br_municipios_biomas, "Mapa de Eficiência - Biomas Cerrado e Amazônia", "Mapa_Eficiencia_Biomas.png")
gerar_mapa_eficiencia(br_municipios_cerrado, "Mapa de Eficiência - Bioma Cerrado", "Mapa_Eficiencia_Cerrado.png")
gerar_mapa_eficiencia(br_municipios_amazonia, "Mapa de Eficiência - Bioma Amazônia", "Mapa_Eficiencia_Amazonia.png")




#####
# FIM DOS MAPAS DE ECOEFICIÊNCIA DOS BIOMAS
#####


# 📌 Carregar pacotes necessários
library(ggplot2)
library(sf)
library(dplyr)
library(viridis)
library(geobr)

# 📌 Baixar limites dos municípios e estados
br_municipios <- read_municipality(year = 2020) %>% mutate(code_muni = as.numeric(code_muni))
estados <- read_state(year = 2020)

# 📌 Converter geocodes para numérico e mesclar os dados de eficiência
resultados_biomas <- resultados_biomas %>% mutate(Geocode = as.numeric(Geocode))
br_municipios <- br_municipios %>%
  left_join(resultados_biomas, by = c("code_muni" = "Geocode"))

# 📌 Criar clusters de eficiência para visualização (Quartis)
br_municipios <- br_municipios %>%
  mutate(cluster = case_when(
    Efic_Norm_VRS <= 0.25 ~ "0.00 - 0.25",
    Efic_Norm_VRS > 0.25 & Efic_Norm_VRS <= 0.50 ~ "0.26 - 0.50",
    Efic_Norm_VRS > 0.50 & Efic_Norm_VRS <= 0.75 ~ "0.51 - 0.75",
    Efic_Norm_VRS > 0.75 ~ "0.76 - 1.00"
  ))

# 📌 Função para gerar mapas de clusters
gerar_mapa_clusters <- function(data, titulo, nome_arquivo) {
  ggplot(data) +
    geom_sf(aes(fill = cluster, geometry = geom), color = "black", size = 0.1) + 
    geom_sf(data = estados, fill = NA, color = "blue", size = 0.5) +
    scale_fill_viridis_d(name = "Cluster de Eficiência") +  
    labs(title = titulo, subtitle = "Baseado no modelo DEA BCC", x = "Longitude", y = "Latitude") +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "gray95", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "gray90"),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      legend.position = "right",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10)
    ) -> mapa
  ggsave(nome_arquivo, plot = mapa, width = 10, height = 8, dpi = 300)
  print(mapa)
}

# 📌 Função para gerar mapas de eficiência absoluta
gerar_mapa_eficiencia <- function(data, titulo, nome_arquivo) {
  ggplot(data) +
    geom_sf(aes(fill = Efic_Norm_VRS, geometry = geom), color = "white", size = 0.1) + 
    geom_sf(data = estados, fill = NA, color = "gray40", size = 0.5) +
    scale_fill_gradient(low = "lightgreen", high = "darkgreen", name = "Eficiência") +
    labs(title = titulo, subtitle = "Baseado no modelo DEA BCC", x = "Longitude", y = "Latitude") +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "gray95", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "gray90"),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      legend.position = "right",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10)
    ) -> mapa
  ggsave(nome_arquivo, plot = mapa, width = 10, height = 8, dpi = 300)
  print(mapa)
}

# 📌 Criar função para gerar mapas de eficiência absoluta (usando escala contínua)
gerar_mapa_eficiencia_absoluta <- function(data, titulo, nome_arquivo) {
  mapa <- ggplot(data) +
    geom_sf(aes(fill = Eficiencia_VRS, geometry = geom), color = "white", size = 0.1) + 
    geom_sf(data = estados, fill = NA, color = "gray40", size = 0.5) +
    scale_fill_viridis_c(name = "Eficiência Absoluta") +  
    labs(title = titulo, subtitle = "Baseado no modelo DEA BCC", x = "Longitude", y = "Latitude") +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "gray95", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "gray90"),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      legend.position = "right",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10)
    )
  
  # 📌 Salvar o mapa como arquivo PNG
  ggsave(nome_arquivo, plot = mapa, width = 10, height = 8, dpi = 300)
  print(mapa)
}


# 📌 Gerar os mapas de CLUSTERS
gerar_mapa_clusters(br_municipios, "Clusters de Eficiência - Biomas Cerrado e Amazônia", "Mapa_Clusters_Biomas.png")
gerar_mapa_clusters(filter(br_municipios, Bioma == "Cerrado"), "Clusters de Eficiência - Bioma Cerrado", "Mapa_Clusters_Cerrado.png")
gerar_mapa_clusters(filter(br_municipios, Bioma == "Amazônia"), "Clusters de Eficiência - Bioma Amazônia", "Mapa_Clusters_Amazonia.png")

# 📌 Gerar os mapas de EFICIÊNCIA CLUSTERS EM TONS DE VERDE
gerar_mapa_eficiencia(br_municipios, "Mapa de Eficiência - Biomas Cerrado e Amazônia", "Mapa_Eficiencia_Biomas.png")
gerar_mapa_eficiencia(filter(br_municipios, Bioma == "Cerrado"), "Mapa de Eficiência - Bioma Cerrado", "Mapa_Eficiencia_Cerrado.png")
gerar_mapa_eficiencia(filter(br_municipios, Bioma == "Amazônia"), "Mapa de Eficiência - Bioma Amazônia", "Mapa_Eficiencia_Amazonia.png")

# 📌 Gerar e salvar os mapas de EFICIÊNCIA ABSOLUTA (VRS não normalizado)
gerar_mapa_eficiencia_absoluta(br_municipios, "Mapa de Eficiência Absoluta - Biomas Cerrado e Amazônia", "Mapa_Eficiencia_Absoluta_Biomas.png")
gerar_mapa_eficiencia_absoluta(filter(br_municipios, Bioma == "Cerrado"), "Mapa de Eficiência Absoluta - Bioma Cerrado", "Mapa_Eficiencia_Absoluta_Cerrado.png")
gerar_mapa_eficiencia_absoluta(filter(br_municipios, Bioma == "Amazônia"), "Mapa de Eficiência Absoluta - Bioma Amazônia", "Mapa_Eficiencia_Absoluta_Amazonia.png")

cat("\n✅ Mapas de ecoeficiência gerados e salvos com sucesso!\n")




#####
# 8. ESTATÍSTICA DESCRITIVA DAS VARIÁVEIS (INPUTS, OUTPUTS E CONTEXTO)
#####

# Carregar pacotes necessários
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)

# Definir as variáveis do modelo
variaveis_inputs <- c("x1", "x2", "x3", "x4", "x5")
variaveis_outputs <- c("y1", "y2", "Iy3", "Iy4")
variaveis_contexto <- c("z1", "z2", "z3", "z4", "z5", "z6", "z7", "z8", "z9")

# Carregar a base de dados original
dados_analise <- eco_data

# 1. Estatísticas Descritivas com `summary()`
cat("\nEstatísticas Descritivas - Inputs:\n")
print(summary(dados_analise[, variaveis_inputs]))

cat("\nEstatísticas Descritivas - Outputs:\n")
print(summary(dados_analise[, variaveis_outputs]))

cat("\nEstatísticas Descritivas - Variáveis de Contexto:\n")
print(summary(dados_analise[, variaveis_contexto]))

# 2. Preparar os dados para gráficos
dados_melt <- dados_analise %>%
  select(all_of(variaveis_inputs), all_of(variaveis_outputs), all_of(variaveis_contexto)) %>%
  pivot_longer(cols = everything(), names_to = "Variavel", values_to = "Valor")

# 3. Gerar gráficos de distribuição
eco_data <- data.frame(
  x1 = runif(100, 100, 400), x2 = runif(100, 500, 1100), x3 = runif(100, 5, 35),
  x4 = runif(100, 1000, 2200), x5 = runif(100, 20, 50),
  y1 = runif(100, 0, 1000), y2 = runif(100, 0, 500),
  Iy3 = runif(100, 0, 1), Iy4 = runif(100, 0, 0.5),
  z1 = runif(100, 10, 100), z2 = runif(100, 200, 800), z3 = runif(100, 0, 50),
  z4 = runif(100, 100, 1000), z5 = runif(100, 0, 300), z6 = runif(100, 10, 70),
  z7 = runif(100, 100, 900), z8 = runif(100, 0, 400), z9 = runif(100, 0, 600)
)

# Definir variáveis
variaveis_inputs <- c("x1", "x2", "x3", "x4", "x5")
variaveis_outputs <- c("y1", "y2", "Iy3", "Iy4")
variaveis_contexto <- c("z1", "z2", "z3", "z4", "z5", "z6", "z7", "z8", "z9")

# Função para criar e salvar gráficos
criar_graficos <- function(vars, titulo, nome_arquivo) {
  dados_melt <- eco_data %>%
    select(all_of(vars)) %>%
    pivot_longer(cols = everything(), names_to = "Variavel", values_to = "Valor")
  
  grafico <- ggplot(dados_melt, aes(x = Valor)) +
    geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
    facet_wrap(~Variavel, scales = "free") +
    theme_minimal() +
    labs(title = titulo, x = "Valor", y = "Frequência")
  
  # Salvar o gráfico
  ggsave(nome_arquivo, plot = grafico, width = 12, height = 8, dpi = 300)
  
  return(grafico)
}

# Gerar e salvar os gráficos
grafico_inputs <- criar_graficos(variaveis_inputs, "Distribuição das Variáveis de Input", "Distribuicao_Inputs.png")
grafico_outputs <- criar_graficos(variaveis_outputs, "Distribuição das Variáveis de Output", "Distribuicao_Outputs.png")
grafico_contexto <- criar_graficos(variaveis_contexto, "Distribuição das Variáveis de Contexto", "Distribuicao_Contexto.png")

# Mostrar os gráficos
print(grafico_inputs)
print(grafico_outputs)
print(grafico_contexto)

#####
# FIM DA ESTATÍSTICA DESCRITIVA DAS VARIÁVEIS
#####


#####
# Verificando contagem de DMUs (municípios) por biomas nas transformações
#####

# Municípios do bioma Amazônia não presentes em data_biomas
municipios_faltantes_amazonia <- setdiff(
  mapbiomas_data %>% filter(biome == "Amazônia") %>% pull(geocode),
  data_biomas$CódIBGE
)
print(municipios_faltantes_amazonia)

print(length(unique(data_biomas$CódIBGE)))
print(length(unique(indices_biomas$Geocode)))


municipios_faltantes_final <- setdiff(
  data_biomas$CódIBGE,
  indices_biomas$Geocode
)
print(municipios_faltantes_final)

registros_faltantes <- data_biomas %>% filter(!(CódIBGE %in% indices_biomas$Geocode))
View(registros_faltantes)

#####
# FIM da verificação de contagem de DMUs (municípios) por biomas nas 
# transformações
#####

#####+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##### ESTÁGIO 2
# REGERESSÕES (Q DE TOBIT E BOOTSTRAP)
#####

# 📌 Carregar pacotes necessários
library(Benchmarking)  # Modelos DEA e Bootstrap
library(dplyr)         # Manipulação de dados
library(AER)           # Regressão Tobit
library(ggplot2)       # Visualização de dados

# 📌 1. Definir inputs e outputs para o DEA
inputs <- resultados_biomas %>% select(x1, x2, x3, x4, x5) %>% as.matrix()
outputs <- resultados_biomas %>% select(y1, y2, Iy3, Iy4) %>% as.matrix()

# 📌 2. Parâmetros do Bootstrap
tamanho_do_arquivo <- 100  # Quantidade de municípios por parte
nrep <- 100  # Bootstrap parcial por parte
num_execucoes <- 10  # Número de execuções para atingir 1000 replicações (10x100)

# 📌 3. Função para dividir os dados em partes
dividir_em_partes <- function(data, tamanho_do_arquivo) {
  split(data, ceiling(seq_len(nrow(data)) / tamanho_do_arquivo))
}

# 📌 4. Dividir os dados em partes
dados_partes <- dividir_em_partes(resultados_biomas, tamanho_do_arquivo)

# 📌 5. Inicializar lista para armazenar os resultados de bootstrap
resultados_bootstrap_particoes <- list()

# 📌 6. Loop para realizar o Bootstrap em partes
for (parte_index in seq_along(dados_partes)) {
  cat("Processando parte:", parte_index, "/", length(dados_partes), "\n")
  
  # 📌 Selecionar a parte atual
  parte <- dados_partes[[parte_index]]
  
  # 📌 Preparar inputs e outputs
  inputs_parte <- parte %>% select(x1, x2, x3, x4, x5) %>% as.matrix()
  outputs_parte <- parte %>% select(y1, y2, Iy3, Iy4) %>% as.matrix()
  
  # 📌 Inicializar vetor para armazenar eficiências ajustadas
  bootstrap_eff <- numeric(nrow(parte))
  
  for (execucao in 1:num_execucoes) {
    cat("Execução de bootstrap:", execucao, "/", num_execucoes, "na parte", parte_index, "\n")
    
    # 📌 Executar o DEA Bootstrap
    bootstrap_result <- dea.boot(
      X = inputs_parte,
      Y = outputs_parte,
      RTS = "vrs",
      ORIENTATION = "out",
      NREP = nrep
    )
    
    # 📌 Somar as eficiências ajustadas
    bootstrap_eff <- bootstrap_eff + bootstrap_result$eff.bc
  }
  
  # 📌 Calcular a média das eficiências ajustadas
  bootstrap_eff <- bootstrap_eff / num_execucoes
  
  # 📌 Adicionar os resultados ao dataframe
  parte_resultado <- parte %>% mutate(Eficiencia_Bootstrap = bootstrap_eff)
  
  # 📌 Armazenar o resultado na lista
  resultados_bootstrap_particoes[[parte_index]] <- parte_resultado
  
  # 📌 Liberar memória
  rm(parte, inputs_parte, outputs_parte, bootstrap_result)
  gc()
}

# 📌 7. Combinar os resultados em um único dataframe
resultados_biomas_completos <- bind_rows(resultados_bootstrap_particoes)

# 📌 8. Salvar o dataframe consolidado
write.csv(resultados_biomas_completos, "Resultados_Bootstrap_DEA.csv", row.names = FALSE)

# 📌 9. Verificar se há valores anômalos
cat("Valores NA:", sum(is.na(resultados_biomas_completos)), "\n")
cat("Valores infinitos:", sum(!is.finite(resultados_biomas_completos$Eficiencia_Bootstrap)), "\n")

# 📌 10. Visualizar distribuição das eficiências ajustadas
ggplot(resultados_biomas_completos, aes(x = Eficiencia_Bootstrap)) + 
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black") + 
  theme_minimal() +
  labs(title = "Distribuição das Eficiências Bootstrap", x = "Eficiência Bootstrap", y = "Frequência")

# 📌 11. Normalizar a Eficiência Bootstrap (0 a 1)
resultados_biomas_completos <- resultados_biomas_completos %>%
  mutate(Eficiencia_Bootstrap_Norm = Eficiencia_Bootstrap / max(Eficiencia_Bootstrap))

# 📌 12. Ajustar o modelo Tobit com a eficiência normalizada
formula_tobit_norm <- as.formula("Eficiencia_Bootstrap_Norm ~ z1 + z2 + z3 + z4 + z5 + z6 + z7 + z8 + z9")

modelo_tobit_bootstrap <- tobit(
  formula = formula_tobit_norm,
  data = resultados_biomas_completos,
  left = 0,  # Censura inferior
  right = 1  # Censura superior
)

# 📌 13. Exibir os resultados da regressão Tobit
summary(modelo_tobit_bootstrap)

cat("\n✅ Bootstrap DEA e Regressão Tobit concluídos com sucesso!\n")

#####
# FIM DAS REGERESSÕES (Q DE TOBIT E BOOTSTRAP)
#####


#####
# ANÁLISE GEOESPACIAL E ÍNDICE DE MORAN
#####

# 📌 1. Instalar e carregar pacotes necessários
# install.packages(c("sf", "spdep", "ggplot2", "RColorBrewer", "ggspatial", "tmap", "dplyr"))
library(sf)        # Manipulação de dados espaciais
library(spdep)     # Análise espacial e Índice de Moran
library(ggplot2)   # Visualização de dados
library(RColorBrewer)  # Paleta de cores para mapas
library(ggspatial) # Elementos espaciais
library(tmap)      # Visualização de mapas
library(dplyr)     # Manipulação de dados

# 📌 2. Carregar os dados de ecoeficiência
resultados_biomas <- read.csv("Resultados_Particionados_DEA_Biomas_Cerrado_Amazonia.csv")

# 📌 3. Verificar a estrutura dos dados
str(resultados_biomas)

# 📌 4. Converter para objeto espacial (sf) usando latitude e longitude
dados_sf <- st_as_sf(resultados_biomas, coords = c("Longitude", "Latitude"), crs = 4326)

# 📌 5. Criar matriz de vizinhança espacial baseada na distância geográfica
vizinhanca <- knearneigh(st_coordinates(dados_sf), k=4)  # Considera os 4 vizinhos mais próximos
matriz_vizinhanca <- knn2nb(vizinhanca)  # Criar matriz de vizinhança
pesos <- nb2listw(matriz_vizinhanca, style="W")  # Criar pesos de vizinhança

# 📌 6. Calcular o Índice de Moran Global para eficiência DEA VRS
moran_conjunto <- moran.test(dados_sf$Eficiencia_VRS, pesos)
print(moran_conjunto)

# 📌 7. Teste de Monte Carlo para significância estatística
moran_mc_conjunto <- moran.mc(dados_sf$Eficiencia_VRS, pesos, nsim=999)
print(moran_mc_conjunto)

# 📌 8. Separar análise por bioma (Cerrado e Amazônia)
dados_cerrado <- dados_sf %>% filter(Bioma == "Cerrado")
dados_amazonia <- dados_sf %>% filter(Bioma == "Amazônia")

# 📌 9. Criar matrizes de vizinhança para cada bioma
vizinhanca_cerrado <- knearneigh(st_coordinates(dados_cerrado), k=4)
matriz_cerrado <- knn2nb(vizinhanca_cerrado)
pesos_cerrado <- nb2listw(matriz_cerrado, style="W")

vizinhanca_amazonia <- knearneigh(st_coordinates(dados_amazonia), k=4)
matriz_amazonia <- knn2nb(vizinhanca_amazonia)
pesos_amazonia <- nb2listw(matriz_amazonia, style="W")

# 📌 10. Calcular Índice de Moran para cada bioma separadamente
moran_cerrado <- moran.test(dados_cerrado$Eficiencia_VRS, pesos_cerrado)
moran_amazonia <- moran.test(dados_amazonia$Eficiencia_VRS, pesos_amazonia)

print(moran_cerrado)
print(moran_amazonia)

# 📌 11. Teste de Monte Carlo para cada bioma
moran_mc_cerrado <- moran.mc(dados_cerrado$Eficiencia_VRS, pesos_cerrado, nsim=999)
moran_mc_amazonia <- moran.mc(dados_amazonia$Eficiencia_VRS, pesos_amazonia, nsim=999)

print(moran_mc_cerrado)
print(moran_mc_amazonia)

# 📌 12. Gerar Moran Scatterplot
par(mfrow=c(1,3))  # Configurar área de plotagem para exibir 3 gráficos lado a lado
moran.plot(dados_sf$Eficiencia_VRS, pesos, main="Moran Scatterplot (Cerrado + Amazônia)")
moran.plot(dados_cerrado$Eficiencia_VRS, pesos_cerrado, main="Moran Scatterplot (Cerrado)")
moran.plot(dados_amazonia$Eficiencia_VRS, pesos_amazonia, main="Moran Scatterplot (Amazônia)")

# 📌 13. Criar mapas de clusters espaciais (LISA)
# Índice de Moran Local para análise conjunta
lisa_conjunto <- localmoran(dados_sf$Eficiencia_VRS, pesos)
dados_sf$LISA <- lisa_conjunto[,1]

# Índice de Moran Local para Cerrado
lisa_cerrado <- localmoran(dados_cerrado$Eficiencia_VRS, pesos_cerrado)
dados_cerrado$LISA <- lisa_cerrado[,1]

# Índice de Moran Local para Amazônia
lisa_amazonia <- localmoran(dados_amazonia$Eficiencia_VRS, pesos_amazonia)
dados_amazonia$LISA <- lisa_amazonia[,1]

# 📌 14. Criar e salvar mapas
mapa_moran_conjunto <- ggplot() +
  geom_sf(data=dados_sf, aes(fill=LISA), color="black") +
  scale_fill_distiller(palette = "RdBu", direction = 1, name="Índice LISA") +
  theme_minimal() +
  ggtitle("Mapa de Cluster Espacial (Cerrado + Amazônia)") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("Mapa_Moran_Conjunto.png", plot = mapa_moran_conjunto, width = 10, height = 8, dpi = 300)
print(mapa_moran_conjunto)

mapa_moran_cerrado <- ggplot() +
  geom_sf(data=dados_cerrado, aes(fill=LISA), color="black") +
  scale_fill_distiller(palette = "RdBu", direction = 1, name="Índice LISA") +
  theme_minimal() +
  ggtitle("Mapa de Cluster Espacial (Cerrado)") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("Mapa_Moran_Cerrado.png", plot = mapa_moran_cerrado, width = 10, height = 8, dpi = 300)
print(mapa_moran_cerrado)

mapa_moran_amazonia <- ggplot() +
  geom_sf(data=dados_amazonia, aes(fill=LISA), color="black") +
  scale_fill_distiller(palette = "RdBu", direction = 1, name="Índice LISA") +
  theme_minimal() +
  ggtitle("Mapa de Cluster Espacial (Amazônia)") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("Mapa_Moran_Amazonia.png", plot = mapa_moran_amazonia, width = 10, height = 8, dpi = 300)
print(mapa_moran_amazonia)

cat("✅ Análise Geoestatística e Índice de Moran concluídos com sucesso!\n")


#####
# FIM DA ANÁLISE GEOESPACIAL E ÍNDICE DE MORAN
#####