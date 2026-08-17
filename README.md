# SEDA — Sensory Evaluation Data Analysis

SEDA is a jamovi module for sensory and consumer data analysis. It provides guided analyses, interpretable outputs, and reproducible R code intended for use in the jamovi Rj Editor.

## Analyses

### Fixed List of Attributes
- QDA — characterization of the stimulus space
- Confidence ellipses — multivariate representation of the stimulus space
- CATA — Check-All-That-Apply data analysis
- JAR — Just-About-Right analysis, including global and product-specific penalties

### Free Perception
- Napping — consensus analysis based on Multiple Factor Analysis
- Sorting — sorting-task analysis based on Multiple Correspondence Analysis

### Hedonic Data
- Preference Mapping (Carto) — external preference mapping with optional consumer classification

## Statistical foundations

SEDA relies primarily on the R packages **SensoMineR** and **FactoMineR**. The module is designed to keep the statistical workflow transparent: analyses include methodological guidance and, where applicable, R code reproducing the calculations and graphics.

## Example data

The module includes example datasets for QDA, confidence ellipses, CATA, JAR, Napping, Sorting, and preference mapping.

## Development

Source repository: https://github.com/Sebastien-Le/SEDA

Please report issues at: https://github.com/Sebastien-Le/SEDA/issues

## License

SEDA is distributed under **GPL (>= 2)**.
