
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PloidyAssignR

PloidyAssignR is a computational framework that allows automated ploidy
assignment from Strand-seq libraries. The tool observes ploidy-dependent
strand state distribution patterns and optimizes kmeans clustering to
assign the respective ploidy state.

<!-- badges: start -->
<!-- badges: end -->

## Installation

You can install PloidyAssignR via terminal from Gitlab EMBL

    cd path/to/your/desired/directory
    git clone https://git.embl.de/christia/ploidyassignr.git

### Loading PloidyAssignR

Open Rstudio and create a Rproject in the directory containing the git
repository you just cloned. PloidyAssignR is still in development and
has not yet been deployed. Therefore library() does not work and you
need to load the package via devtools.

``` r
if (!requireNamespace("devtools")) {
    install.packages("devtools")
}
#> Loading required namespace: devtools

devtools::load_all()
#> ℹ Loading PloidyAssignR
```

Now PloidyAssignR is loaded and you can use its functions etc.

## How to Use PloidyAssignR

Please refer to the vignette to learn about PloidyAssignR’s
functionality.

``` r
browseVignettes("PloidyAssignR")
```

The package contains a Rshiny app to help inspect your ploidy analysis
data sets. To get a quick overview consider watching:
<https://drive.google.com/file/d/1y56aibBt9mg97K1fT2-SKESm3Zp0GODe/view?usp=sharing>

To launch the app run:

``` r
run_app()
```
