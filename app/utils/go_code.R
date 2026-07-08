generate_go_code <- function(input, direction = c("Up", "Down"), include_plot = FALSE) {
  direction <- match.arg(direction)

  format_vec <- function(vec) {
    paste0('c(', paste0('"', vec, '"', collapse = ", "), ')')
  }

  is_up       <- identical(direction, "Up")
  filter_expr <- if (is_up) "avg_log2FC > 0" else "avg_log2FC < 0"
  gene_var    <- if (is_up) "up_genes" else "down_genes"
  go_var      <- if (is_up) "go_up" else "go_down"
  suffix      <- tolower(direction)

  # ---- optional plot section - only built (and appended) when the user
  # is currently viewing the Plot tab for this direction ----
  plot_section <- ""
  if (include_plot) {
    field <- function(name) input[[paste0("go_", suffix, "_", name)]]

    terms      <- format_vec(field("terms"))
    plot_title <- field("title") %||% ""
    x_col      <- field("x")
    size_col   <- field("size")
    color_col  <- field("color")

    plot_section <- sprintf(r"(

# -------------------------
# 4. Plot selected GO terms
# -------------------------
library(ggplot2)

plot_df <- %s@result %%>%%
  filter(Description %%in%% %s) %%>%%
  mutate(Description = gsub(" - Mus musculus \\(house mouse\\)$", "", Description))

ggplot(
  plot_df,
  aes(
    x = .data[["%s"]],
    y = reorder(Description, .data[["%s"]]),
    size = .data[["%s"]],
    color = .data[["%s"]]
  )
) +
  geom_point() +
  scale_color_gradient(low = "blue", high = "red", name = "%s") +
  labs(x = "%s", y = NULL, title = "%s") +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )
)",
      go_var, terms,
      x_col, x_col, size_col, color_col,
      color_col, x_col, plot_title
    )
  }

  sprintf(r"(# -------------------------
# GO Term Enrichment Code (%s)
# -------------------------
# Notes: The .RData file is large, ensure your computer has sufficient resources to process it.

library(Seurat)
library(dplyr)
library(clusterProfiler)
library(org.Mm.eg.db)

# -------------------------
# 1. Create valid DEG result (see DEG Code)
# -------------------------
# Note: the following script assumes the DEG result is called filtered_degs

# -------------------------
# 2. Extract %s-regulated genes
# -------------------------
%s <- filtered_degs %%>%%
  filter(%s) %%>%%
  rownames()

# -------------------------
# 3. Run GO term enrichment (Biological Process)
# -------------------------
%s <- enrichGO(
  gene = %s,
  OrgDb = "org.Mm.eg.db",
  keyType = "SYMBOL",
  ont = "BP"
)

%s@result%s
)",
    direction,
    tolower(direction),
    gene_var, filter_expr,
    go_var, gene_var,
    go_var, plot_section
  )
}
