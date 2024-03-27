## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>",
    echo = TRUE,
    warning = FALSE,
    message = FALSE
)

## ----setup--------------------------------------------------------------------
library(PloidyAssignR)
library(ggplot2)

## ---- eval = FALSE------------------------------------------------------------
#  PloidyAssignR_run_all(
#    input_data = "path/to/strandseq/count",
#    dt_input_ROC = "path/to/ROC", # optional
#    max_ploidy_force = NULL, #optional
#    input_chrom = NULL, # default: all chromosomes present in data set
#    input_window = 1e+07, # default: window width 10 Mbp
#    input_step = 5e+06, # default: step length 5 Mbp
#    export_path = NULL, # default: does not save data to file.
#    export_file_name = "PloidyAssignR"
#  )

## ----Input Data, message=FALSE------------------------------------------------
data_K562_example_input <- input_prep(input_data = data_K562_strand_seq_count)

data_K562_example_input

## ----Calculate Fractions, message=FALSE, paged.print=FALSE--------------------
data_example <- calWatsonFractions(data_K562_example_input,
                                   input_chrom = NULL, 
                                   # default: all chromosomes present in data set
                                   input_window = 1000000, 
                                   # default: window width 10 Mbp
                                   input_step = 500000 
                                   # default: step length 5 Mbp
                                   )
data_example

## ----Assign Ploidy, message=FALSE---------------------------------------------
data_example <- assignConsensusPloidy(data_example)
print(data_example)

## ----Plot Patterns,fig.align="center", fig.width=10, fig.height=4, fig.cap="Figure: Distribution Patterns of Chr7 K562", message = FALSE----
fct_plot_distribution_patterns(data_example, input_chrom ="chr3", point_size = 0.2)

## ----Plot Karyogram,fig.align="center", fig.width=10, fig.height=10, fig.cap="Figure: Ploidy Assignment for K562", message = FALSE----
fct_plot_karyogram(data_example)

## ----Ploidy Summary, message = FALSE------------------------------------------
data_summary <- ploidy_summary(data_K562_ploidy)
print(data_summary)

## ----ROC example, echo=FALSE--------------------------------------------------
data_K562_ROC

## ----SC Copy Number, message = FALSE------------------------------------------
data_example <- scCoverageNorm(data_example,
                               data_K562_ROC)

print(data_example)

## ----Plot Heatmap,fig.align="center", fig.width=7, fig.height=10, fig.cap="Figure: Example Heatmap of SC Copy Number - Chr2 K562"----
fct_plot_sc_heatmap(data_example, input_chrom = "chr2")

## ----Plot Subclone Heatmap,fig.align="center", fig.width=10, fig.height=10, fig.cap="Figure: K562 Chr20 Subclonal Copy Number Deviation in Eight Cells (Trisomic Signal in p-Arm)"----
fct_plot_sc_heatmap(data_K562_ploidy, input_chrom = "chr20")

## ----Plot Subclone Pattern,fig.align="center", fig.width=10, fig.height=4, fig.cap="Figure: K562 Chr20 p-arm - Example Cell Deviating from Majority Distribution Pattern"----
fct_plot_distribution_patterns(data_K562_ploidy[start<=35000000], 
                               input_chrom ="chr20", point_size = 0.2, 
                               input_cell = "03|380", reg_color = "grey", 
                               cell_color = "black", cell_size = 0.7)

## ----Strand-seq Plot Subclone,fig.align="center", fig.width=7, fig.height=10, fig.cap="Figure: Scaled Strand-seq Count Plot of K562 Cells with Subclonal Trisomic Signal in Chr20p"----
subclone_cells <- c("03|393", "03|324" ,"01|366", "03|337", "03|380", "03|396", "01|370", "03|378")

plot_subclone <- fct_plot_counts_cellwise(data_K562_strand_seq_count[start<=35000000],
                                          data_SC_CN = data_K562_ploidy,
                         selected_cells = subclone_cells,
                         input_chrom = "chr20",
                         lab_title = "K562 Chr20p Subclone Candidates"
                         )
plot_subclone + ylim(-50,50)


## ----Strand-seq Plot Majority,fig.align="center", fig.width=7, fig.height=10, fig.cap="Figure: Scaled Strand-seq Count Plot of eight K562 Cells Chr20p as Example for the Majority Disomic Signal"----
K562_major_clone <- data_K562_strand_seq_count[chrom=="chr20"&start<=30000000]
K562_major_clone <-K562_major_clone[!(cell%in% subclone_cells)]

major_clone <- K562_major_clone[,unique(cell)]
major_clone_subset <- major_clone[1:8]
plot_majority <- fct_plot_counts_cellwise(data_K562_strand_seq_count[start<=35000000],
                                          data_SC_CN = data_K562_ploidy,
                         selected_cells = major_clone_subset,
                         input_chrom = "chr20",
                         lab_title = "K562 Chr20p Eight Cells from Major Clone"
                         )
plot_majority + ylim(-50,50)

## -----------------------------------------------------------------------------
sessionInfo()

