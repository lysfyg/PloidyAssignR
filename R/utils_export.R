## Not documented
PloidyAssignR_colors <- c(
    "0" = "#f2f2f2",
    "1" = "#0f757b",
    "2" = "#acc151",
    "3" = "#e5c009",
    "4" = "#6cb4ca",
    "5" = "orange",
    "6" = "red",
    "bad" = "#990000",
    "ok" = "#ffffff",
    "good" = "#ffffff",
    "TRUE" = "#f2f2f2",
    "FALSE" = "#ff6666",
    ### cytoband colors ###
    "data" = "990000",
    "acen" = "red",
    "gneg" = "white",
    "gpos25" = "#E8E8E8",
    "gpos50" = "#909090",
    "gpos75" = "#404040",
    "gpos100" = "black",
    "gvar" = "darkred",
    "stalk" = "lightblue",
    ### Watson Crick starnds
    "crick" = "paleturquoise4", ## 668b8b
    "watson" = "sandybrown"
) ## f4a460


#' Format Mb
#'
#' readable output for x axis
#'
#' @param x any numeric that will be devided by 1e6
#' @examples format_Mb(1000000)
#'
#'
#' @export
#'
format_Mb <- function(x) {
    x / 1e6
}



#' Export Data Table
#'
#' Wrapper function to export table as .csv, uses fwrite which is faster than standard writing options in r
#'
#' @param data_table_input Any data.table that should be exported.
#' @param omitNA Default is FALSE. If set to TRUE rows containing NA will be removed before export.
#' @param output_folder Path to output folder.
#' @param add_file_name String containing the desired name of the exported file.
#'
#' @return Saves data.table to the desired output folder
#' @examples
#' path_wd <- getwd()
#' export_data(data_K562_ploidy, output_folder = path_wd, add_file_name = "Internal_Example")
#' @export
#'

export_data <- function(data_table_input,
                        omitNA = FALSE,
                        output_folder,
                        add_file_name = "PloidyAssignR") {
    if (omitNA) {
        data_table_input <- na.omit(data_table_input)
    }
    export_path <- file.path(
        tools::file_path_as_absolute(output_folder),
        paste0(Sys.Date(), "_", add_file_name, ".csv")
    )
    data.table::fwrite(data_table_input,
        file = export_path,
        quote = F
    )
}

#' export_plot
#'
#' @import Cairo
#' @importFrom grDevices dev.off
#'
#' @param input_plot ggplot that should be exported
#' @param file_name String containing the desired name of the exported plot
#' @param file_type pdf or png or any other type Cairo accepts
#' @param file_width Default is 297 mm
#' @param file_height Default is 210 mm
#' @param input_dpi Default is 60 dpi.
#'
#' @return Exports plot using Cairo either as pdf or png.
#' @examples
#' # generate plot
#' cars_plot <- ggplot2::ggplot(data= mtcars)+ggplot2::geom_point(ggplot2::aes(x=mpg, y=cyl))
#' # basic export
#' export_plot(cars_plot, file_name="cars_plot.png", file_type = "png")
#'
#' @export
#'

export_plot <- function(input_plot,
                        file_name = "PloidyAssignR",
                        file_type = "pdf",
                        file_width = 297,
                        file_height = 210,
                        input_dpi = 60) {
    Cairo::Cairo(file = file_name, type = file_type, width = file_width, height = file_height,
                 units = "mm", dpi = input_dpi)
    plot(input_plot)
    grDevices::dev.off()
}

#' fct_plot_karyogram
#'
#' @import data.table
#' @import ggplot2
#' @import scales
#'
#' @param data_ploidy data.table containing consensus ploidy values.
#' @param chrom_order Optional: Desired order of chromosomes for plotting.
#'
#' @return Returns a ggplot bar graph using color-coding to show the consensus ploidy for chromosomes.
#' @examples fct_plot_karyogram(data_K562_ploidy)
#' @export
#'

fct_plot_karyogram <- function(data_ploidy,
                               chrom_order = NULL
                               # ,exclusion_list_file = NULL
) {
    # local binding of variable to function
    cons_ploidy <- NULL

    if (is.null(chrom_order)) {
        chrom_order <- extract_chromosomes(data_ploidy)
    }
    plot <- ggplot2::ggplot(data_ploidy, ggplot2::aes(start, y = 1))

    plot <- plot +
        ggplot2::geom_rect(ggplot2::aes(
            xmin = start, xmax = end, ymin = -1, ymax = 1,
            fill = factor(cons_ploidy)
        )) +
        ggplot2::facet_grid(factor(chrom, levels = chrom_order) ~ .,
            switch = "y"
        ) +
        ggplot2::scale_colour_manual(values = PloidyAssignR_colors) +
        ggplot2::scale_fill_manual(name = "Ploidy", values = PloidyAssignR_colors) +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(12), labels = format_Mb) +
        ggplot2::ggtitle(paste0("Estimated Karyogram of Sample: ", data_ploidy[, unique(sample)])) +
        ggplot2::xlab("Start of bin in Chromosome [Mb]") +
        ggplot2::theme_classic() +
        ggplot2::theme(
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            strip.text.y = element_text(angle = 180),
            strip.placement = "outside"
        )
    return(plot)
}



#' fct_plot_distribution_patterns
#'
#' @import data.table
#' @import ggplot2
#' @import scales
#'
#' @param data_ploidy Data.table containing consensus ploidy values.
#' @param input_chrom Character vector containing the desired chromosome to be plotted.
#' @param point_size Default is 0.5. Size should be chosen depending on sample size.
#' @param input_start If supplied will only plot distribution patterns in specified region.
#' @param input_end If supplied will only plot distribution patterns in specified region.
#' @param input_cell If supplied highlights specified cell in distribution plot.
#' @param reg_color color of the scatter plot points
#' @param cell_color color of the highlighted input_cell
#' @param cell_size Point size of cell input. Equivalent to point size parameter of geom_point. T
#'
#' @return A ggplot of the distribution patterns.
#' @examples
#' fct_plot_distribution_patterns(data_K562_ploidy, input_chrom = "chr1")
#' @export
#'

fct_plot_distribution_patterns <- function(data_ploidy,
                                           input_chrom,
                                           point_size = 0.5,
                                           input_start = NULL,
                                           input_end = NULL,
                                           input_cell = NULL,
                                           reg_color = "black",
                                           cell_color = "green",
                                           cell_size = 0.5) {
    # local binding of variable to function
    chrom <- fraction_w <- cons_ploidy <- cell <- NULL

    # Standard watson ratios scatter plot
    data_plot <- data_ploidy[chrom == input_chrom]
    if (!is.null(input_start) & !is.null(input_end)) {
        data_plot <- data_plot[start >= input_start & end <= input_end]
    }
    plot <- ggplot(data = data_plot[chrom == input_chrom]) +
        geom_point(aes(x = start, y = fraction_w), size = point_size, color = reg_color) +
        geom_rect(aes(xmin = start, xmax = end, ymin = -0.1, ymax = -0.15, fill = factor(data_plot[, cons_ploidy]))) +
        scale_fill_manual(name = "Ploidy", values = PloidyAssignR_colors) +
        scale_x_continuous(breaks = scales::pretty_breaks(12), labels = format_Mb) +
        labs(
            x = "Start of bin in Chromosome [Mb]",
            y = "Relative Strand State Frequency [%]"
        ) +
        theme_classic() +
        ggtitle(input_chrom)


    if (!is.null(input_cell)) {
        plot <- plot +
            geom_point(data = data_plot[chrom == input_chrom & cell == input_cell],
                       aes(x = start, y = fraction_w), size = cell_size, color = cell_color)
    }
    return(plot)
}


#' fct_plot_sc_heatmap
#'
#' @import data.table
#' @import pheatmap
#' @import magrittr
#' @importFrom tidyr pivot_wider
#'
#' @param data_ploidy Data.table containing the consensus ploidy values
#' @param input_chrom Desired chromosome that should be plotted.
#' @param input_start If supplied only plots SC ploidy values in the specified segment.
#' @param input_end If supplied only plots SC ploidy values in the specified segment.
#'
#' @return Returns a heatmap of the color-coded SC copy numbers.
#' @examples fct_plot_sc_heatmap(data_K562_ploidy, input_chrom = "chr1")
#' @export
#'
#'

fct_plot_sc_heatmap <- function(data_ploidy,
                                input_chrom,
                                input_start = NULL,
                                input_end = NULL) {
    # local binding of variable to function
    chrom <- norm_count <- NULL

    # table formatting
    data_plot <- data_ploidy[chrom == input_chrom]
    if (!is.null(input_start) & !is.null(input_end)) {
        data_plot <- data_ploidy[start >= input_start & end <= input_end]
    }
    data_plot[,`:=`(start=format_Mb(start), end=format_Mb(end))]
    data_ploidy_wide <- data_plot[, c("start", "cell", "norm_count")] %>% tidyr::pivot_wider(names_from = start, values_from = norm_count)
    heatmap_chrom <- data.matrix(data_ploidy_wide[, -1]) # removes cell column
    rownames(heatmap_chrom) <- data_ploidy_wide$cell # reads cell names of cell column

    # plot heatmap and cluster rows
    plot <- pheatmap::pheatmap(heatmap_chrom,
        color = PloidyAssignR_colors, breaks = c(0, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5),
        cluster_cols = F, labels_row = rownames(heatmap_chrom),
        fontsize = 6, border_color = "NA",
        main = paste0(input_chrom)
    )
    return(plot)
}

#' fct_plot_counts_cellwise
#'
#' @import ggplot2
#' @import scales
#'
#' @param data_counts Data.table containing Strand-seq count data
#' @param data_SC_CN Data.table containing SC normalized count data (SC copy number)
#' @param selected_cells Character-Vector of cells that should be plotted
#' @param input_chrom Specific chromosome that should be plotted
#' @param lab_title Title of plot
#'
#' @return ggplot bar graph using teal and orange to specifiy read orientation.
#' @examples
#' plot_cell <- fct_plot_counts_cellwise(data_K562_strand_seq_count,
#'                             selected_cells = c("01|301", "01|304"),
#'                             input_chrom = "chr1")
#' @export
#'

fct_plot_counts_cellwise <- function(data_counts,
                                     data_SC_CN = NULL,
                                     selected_cells,
                                     input_chrom,
                                     lab_title = NULL) {
    # to address "no visible binding for global variable"
    cell <- w <- chrom <- baseline_coverage <- w_scaled <- c_scaled <- NULL

    data_counts_subset <- data_counts[cell%in%selected_cells&chrom == input_chrom]
    if (is.null(lab_title)) {
        lab_title <- paste0("Selected count data for ", data_counts_subset[,unique(sample)], input_chrom)
    }
    max_start <- data_counts_subset[,max(start)]
    if (!is.null(data_SC_CN)){
        message("You have supplied SC copy number data. Scaling Strand-seq libraries with individual read count baseline.")
        #scaling
        data_ploidies_subset <- data_SC_CN[cell%in%selected_cells&chrom==input_chrom&start<=max_start]

        data_subset_baseline_coverage<-data_ploidies_subset[,unique(baseline_coverage), by=cell]
        colnames(data_subset_baseline_coverage)<- c("cell", "baseline_coverage")
        data_subset_baseline_coverage
        for(cur_cell in selected_cells){

            cur_baseline <- data_subset_baseline_coverage[cell==cur_cell, baseline_coverage]
            data_counts_subset[cell==cur_cell, `:=`(w_scaled = (w*100/cur_baseline), c_scaled = (c*100/cur_baseline))]
        }
        data_counts_subset[, `:=`(w = w_scaled, c= c_scaled)]
    }

    plot <- ggplot(data = data_counts_subset) +
        facet_grid(rows = vars(cell)) +
        geom_linerange(aes(x = start, ymin = 0, ymax = -c), color = PloidyAssignR_colors["crick"], size = 2) +
        geom_linerange(aes(x = start, ymin = 0, ymax = w), color = PloidyAssignR_colors["watson"], size = 2) +
        scale_x_continuous(breaks = scales::pretty_breaks(12), labels = format_Mb) +
        labs(
            title = lab_title,
            x = "Chr. Position  [Mb]", y = "Counts"
        ) +
        theme_classic() +
        theme(
            strip.placement = "outside",
            strip.background = element_blank(),
            legend.position = "none",
            panel.background = element_rect(fill = "transparent"),
            plot.background = element_rect(fill = "transparent", color = NA)
        )
    return(plot)
}
