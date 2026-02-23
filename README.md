README (English Version)

# Ecoefficiency in the Cerrado and Amazon Biomes (Brazil): DEA-based Analysis with IBGE and MapBiomas Data

This repository contains the datasets, R scripts, figures, and manuscript files used in the study:

**“Cerrado ou Amazônia? Uma análise de ecoeficiência agropecuária dos maiores biomas do Brasil”**

## Overview

This study analyzes agricultural ecoefficiency across Brazilian municipalities located primarily in the **Cerrado** and **Amazon** biomes, using a two-stage quantitative approach:

1. **Stage 1**: Data Envelopment Analysis (DEA) to estimate ecoefficiency scores  
   - DEA models with **VRS** and **CRS**
   - Output-oriented specifications (VRS_OO and CRS_OO)
   - Model selection based on **Kolmogorov-Smirnov (KS) test**
   - Benchmark, targets, slacks, returns, and references analysis
   - Comparative tests between biomes (KS and Kruskal-Wallis)
   - Spatial visualization and **Moran’s I** analysis

2. **Stage 2**: Bootstrap-adjusted efficiency + **Tobit regression**
   - Bootstrap procedure to obtain more robust efficiency estimates
   - Tobit model to assess the effect of socioeconomic and environmental context variables

## Data Sources

- **IBGE – Agricultural Census 2017**
- **MapBiomas** (Land Use and Land Cover data, biome and political division statistics)

## Repository Structure

- `paper/` → manuscript files (APA Word version, PDF, optional LaTeX/Overleaf source)
- `data/raw/` → original/filtered source datasets
- `data/processed/` → processed datasets and DEA indices
- `code/R/` → R scripts used for the analyses
- `outputs/figures/` → figures used in the paper
- `outputs/tables/` → (recommended) exported tables used in the manuscript
- `docs/` → reproducibility notes, methods workflow, and metadata

## Main Files (current version)

### Manuscript
- `paper/APA/Artigo_APA_Ecoeficiencia_Cerrado_Amazonia_Paulo_portugues.docx`
- `paper/PDF/Artigo_MQA_PPGA_Ecoeficiencia___Paulo___portugues.pdf`

### Data (examples)
- `data/raw/Base_Mapbiomas_REFILTRADA.csv`
- `data/raw/Base_Mapbiomas_Tratada.csv`
- `data/processed/Dados_Master_Ecoeficiencia.csv`
- `data/processed/Dados_Master_Ecoeficiencia_Com_Bioma.csv`
- `data/processed/Indices_DEA_VRS_OO_Amazonia.csv`
- `data/processed/Indices_DEA_VRS_OO_Cerrado.csv`
- `data/processed/Indices_DEA_VRS_OO_Biomas.csv`

### R Code
- `code/R/Ecoef_Cer_Amz_Juntos.R`
- `code/R/Ecoeff_Biomas.Rproj`

## How to Reproduce (suggested workflow)

1. Open `Ecoeff_Biomas.Rproj` in RStudio.
2. Install required packages (see `docs/reproducibility_guide.md` or script headers).
3. Place input datasets in `data/raw/` and `data/processed/` according to the paths expected by the script.
4. Run `Ecoef_Cer_Amz_Juntos.R`.
5. Check generated outputs in `outputs/figures/` and `outputs/tables/`.

> Note: Depending on your local file paths and package versions, small path adjustments may be required.

## Variables (summary)

The study uses:
- **Inputs** (`x1`–`x5`)
- **Outputs** (`y1`–`y4`)
- **Context / externalities variables** (`z1`–`z9`)

Please refer to `data/dictionaries/data_dictionary_variables.md` for variable definitions.

## Reproducibility and Transparency Notes

- The analysis was designed with computational robustness in mind, including partitioned calculations for DEA and bootstrap steps.
- Figures and tables included in the manuscript are stored in `outputs/`.
- If you reuse this material, please cite both the manuscript and this repository.

## Citation (suggested)

If you use this repository, please cite:
- the manuscript (paper)
- this GitHub repository (Zenodo DOI can be added in future versions)

## License

Choose and include a license (e.g., MIT for code, CC BY 4.0 for text/figures when applicable).



==================================================================================================================
==================================================================================================================
==================================================================================================================

README (Portuguese Version)

# Ecoeficiência nos Biomas Cerrado e Amazônia (Brasil): Análise com DEA usando dados do IBGE e MapBiomas

Este repositório reúne os dados, scripts em R, figuras e arquivos do manuscrito utilizados no estudo:

**“Cerrado ou Amazônia? Uma análise de ecoeficiência agropecuária dos maiores biomas do Brasil”**

## Visão Geral

Este estudo analisa a ecoeficiência agropecuária de municípios brasileiros localizados majoritariamente nos biomas **Cerrado** e **Amazônia**, utilizando uma abordagem quantitativa em dois estágios:

1. **Estágio 1**: Análise Envoltória de Dados (DEA) para estimar os escores de ecoeficiência  
   - Modelos DEA com **VRS** e **CRS**
   - Especificações orientadas a output (VRS_OO e CRS_OO)
   - Escolha do modelo com base no **teste de Kolmogorov-Smirnov (KS)**
   - Análises de benchmarks, targets, slacks, returns e references
   - Testes comparativos entre biomas (KS e Kruskal-Wallis)
   - Visualização espacial e análise do **Índice de Moran**

2. **Estágio 2**: Eficiências ajustadas por bootstrap + **regressão Tobit**
   - Bootstrap para obter estimativas mais robustas das eficiências
   - Modelo Tobit para avaliar o efeito de variáveis de contexto socioeconômicas e ambientais

## Fontes de Dados

- **IBGE – Censo Agropecuário 2017**
- **MapBiomas** (dados de uso e cobertura da terra por bioma e divisão política)

## Estrutura do Repositório

- `paper/` → arquivos do manuscrito (versão Word APA, PDF e opcionalmente LaTeX/Overleaf)
- `data/raw/` → bases originais/filtradas
- `data/processed/` → bases tratadas e índices DEA
- `code/R/` → scripts em R utilizados nas análises
- `outputs/figures/` → figuras utilizadas no artigo
- `outputs/tables/` → (recomendado) tabelas exportadas usadas no manuscrito
- `docs/` → notas de reprodutibilidade, fluxo metodológico e metadados

## Principais Arquivos (versão atual)

### Manuscrito
- `paper/APA/Artigo_APA_Ecoeficiencia_Cerrado_Amazonia_Paulo_portugues.docx`
- `paper/PDF/Artigo_MQA_PPGA_Ecoeficiencia___Paulo___portugues.pdf`

### Dados (exemplos)
- `data/raw/Base_Mapbiomas_REFILTRADA.csv`
- `data/raw/Base_Mapbiomas_Tratada.csv`
- `data/processed/Dados_Master_Ecoeficiencia.csv`
- `data/processed/Dados_Master_Ecoeficiencia_Com_Bioma.csv`
- `data/processed/Indices_DEA_VRS_OO_Amazonia.csv`
- `data/processed/Indices_DEA_VRS_OO_Cerrado.csv`
- `data/processed/Indices_DEA_VRS_OO_Biomas.csv`

### Código em R
- `code/R/Ecoef_Cer_Amz_Juntos.R`
- `code/R/Ecoeff_Biomas.Rproj`

## Como Reproduzir (fluxo sugerido)

1. Abra o `Ecoeff_Biomas.Rproj` no RStudio.
2. Instale os pacotes necessários (ver `docs/reproducibility_guide.md` ou cabeçalho do script).
3. Posicione as bases de entrada em `data/raw/` e `data/processed/` conforme os caminhos esperados pelo script.
4. Execute o script `Ecoef_Cer_Amz_Juntos.R`.
5. Verifique os resultados gerados em `outputs/figures/` e `outputs/tables/`.

> Observação: Dependendo dos caminhos locais e da versão dos pacotes, podem ser necessários pequenos ajustes.

## Variáveis (resumo)

O estudo utiliza:
- **Inputs** (`x1`–`x5`)
- **Outputs** (`y1`–`y4`)
- **Variáveis de contexto / externalidades** (`z1`–`z9`)

Consulte `data/dictionaries/data_dictionary_variables.md` para a descrição detalhada das variáveis.

## Notas de Reprodutibilidade e Transparência

- A análise foi estruturada com foco em robustez computacional, incluindo processamento particionado nas etapas de DEA e bootstrap.
- As figuras e tabelas utilizadas no manuscrito estão armazenadas em `outputs/`.
- Caso reutilize este material, cite o manuscrito e este repositório.

## Como citar (sugestão)

Se você utilizar este repositório, cite:
- o manuscrito (artigo)
- este repositório GitHub (idealmente com DOI via Zenodo em versões futuras)

## Licença

Escolha e inclua uma licença apropriada (ex.: MIT para código; CC BY 4.0 para texto/figuras, quando aplicável).
