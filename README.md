# EHA

This repository hosts all public-facing R code, data, and outputs generated for the Everett Housing Authority (EHA) during contracted work to assist in the development of a community of opportunity identification process.

## Background

Between January - June 2022, EHA contracted a team of Evans Analysts to assist in the development of a communities of opportunity criteria. In 2021, EHA received [HUD's Moving to Work (MTW) designation](https://www.hud.gov/mtw), which allows them more flexibility with federal dollars and the opportunity to design and test innovative strategies. Part of EHA's effort under MTW is to explore potential of identifying community of opportunity zones within Everett, particular aligned with their [2020-2024 Five Year Strategic Plan](http://www.evha.org/sites/default/files/page_attachments/wa006v02.pdf) that focuses on "development activities [that] will address the effects of institutional racism by championing diversity, equity and inclusion." 

## Repository Organization

- The folder `./demographics/` includes an R script, `demographics_mapping_acs.R`, that uses the tidycensus package to generate racial demographic maps for the city of Everett. The code can be easily adjusted to include any variable or census year available in tidycensus. All figure outputs are avaialble in `./demographics/outputs/`.
- The folder `./education/` includes an R script, `education_indicators.R`, that walks through an illustrative geospatial analysis using four potential education indicators. The data used for analysis is hosted in `./education/data/`, and all figure outputs are avaialble in `./education/outputs/`. The `./education/README.md` outlines a list of useful publically available education data sources from Washington State and OSPI. 
