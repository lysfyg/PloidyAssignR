
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PloidyAssignR

PloidyAssignR is a computational framework that allows automated ploidy
assignment from Strand-seq libraries. The tool observes ploidy-dependent
strand state distribution patterns and optimizes kmeans clustering to
assign the respective ploidy state. This vignette provides a
comprehensive guide to using PloidyAssignR. We will introduce the key
functionalities of PloidyAssignR and demonstrate its application using
sample data from the cell line K562, including first steps of data
visualization and contextualization.

<!-- badges: start -->
<!-- badges: end -->

### Installation

``` r
if (!requireNamespace("devtools")) {
    install.packages("devtools")
}

devtools::install_github("lysfyg/PloidyAssignR")
```

### Loading PloidyAssignR

``` r
library(PloidyAssignR)
```

## How to Use PloidyAssignR

Please refer to the vignette to learn about PloidyAssignR’s
functionality.

``` r
browseVignettes("PloidyAssignR")
```

PloidyAssignR is introduced in a manuscript prepared for publication:
“PloidyAssignR: Exploiting Mitotic Chromatid Segregation Patterns for
Systematic Inference of Ploidy in Single Cells”.

You can find detailed information on the mathematical reasoning,
algorithm and ploidy analysis of four example cell lines: RPE-1, C7,
C29 and K562.

The results where compared to published karyotypes (Riches et
al. (2001), Mardin et al. (2015) and Zhou et al. (2019)) and the tool
was benchmarked in an comparative analysis with Aneufinder (Bakker et.
al., 2016).

### Rshiny Application

The package contains a Rshiny app to help inspect your ploidy analysis
data sets. To get a quick overview consider watching:
<https://drive.google.com/file/d/1y56aibBt9mg97K1fT2-SKESm3Zp0GODe/view?usp=sharing>

To launch the app run:

``` r
run_app()
```

## Summary of PloidyAssignR

PloidyAssignR systematically infers ploidy from single-cell Strand-seq
data by analyzing chromatid segregation patterns.  
The tool includes functions for processing Strand-seq count data,
normalizing read depth and visualizing the results.

## Core Functions for Ploidy Analysis:

- **input_prep**: This function prepares and formats the input data for
  analysis. It ensures the data is structured correctly for
  PloidyAssignR’s pipeline.

- **calWatsonFractions**: A key function for calculating the relative
  W-strand state frequencies (fraction_w).

- **assignConsensusPloidy**: After calculating the relative W-strand
  state frequencies (fraction_w), this function assigns a consensus
  ploidy state to a sliding window. It applies k-means clustering and
  silhouette scores, to determine the most likely ploidy state for each
  analyzed segment.

- **scCoverageNorm**: Normalizes the coverage of single-cell sequencing
  data using user-defined regions of confidence. This function ensures
  accurate single-cell copy number estimations by accounting for
  variations in library size and sequencing depth.

## Visualization Functions:

- **fct_plot_distribution_patterns**: This function is used to create
  scatter plots that visualize the distribution of W-strand state
  frequencies in single cells within a specific genomic region. It helps
  in analyzing patterns of strand segregation.

- **fct_plot_karyogram**: This function generates karyogram-style plots,
  which are useful for visualizing consensus ploidy values across
  chromosomes.

- **fct_plot_sc_heatmap**: This function creates heatmaps to represent
  single-cell normalized count data. It’s especially valuable for
  visualizing variations in normalized counts across cells within
  specific genomic regions, highlighting chromosomal ploidy states at
  the single-cell level.

### Data Sets:

The PloidyAssignR repository includes two data sets:

- Strand-seq count data from K562 cells generated by MosaiCatcher
  (Sanders et al., 2020).

- A ploidy data set generated by PloidyAsssignR from K562 Strand-seq
  count data.

## References

Bakker, B., Taudt, A., Belderbos, M. E., Porubsky, D., Spierings, D. C.
J., de Jong, T. V., Halsema, N., et al. (2016). Single-cell sequencing
reveals karyotype heterogeneity in murine and human malignancies. Genome
Biology, 17(1), 115.

Mardin, B. R., Drainas, A. P., Waszak, S. M., Weischenfeldt, J.,
Isokane, M., Stütz, A. M., Raeder, B., et al. (2015). A cell-based model
system links chromothripsis with hyperploidy. Molecular Systems Biology,
11(9), 828.

Riches, A., Peddie, C., Rendell, S., Bryant, P., Zitzelsberger, H.,
Bruch, J., Smida, J., et al. (2001). Neoplastic Transformation and
Cytogenetic Changes after Gamma Irradiation of Human Epithelial Cells
Expressing Telomerase. Radiation Research, 155(1), 222–229.

Sanders, A.D., Meiers, S., Ghareghani, M. et al. Single-cell analysis of
structural variations and complex rearrangements with tri-channel
processing. Nat Biotechnol 38, 343–354 (2020).
<https://doi.org/10.1038/s41587-019-0366-x>

Zhou, B., Ho, S. S., Greer, S. U., Zhu, X., Bell, J. M., Arthur, J. G.,
Spies, N., et al. (2019). Comprehensive, integrated, and phased
whole-genome analysis of the primary ENCODE cell line K562. Genome
Research, 29(3), 472–484.
