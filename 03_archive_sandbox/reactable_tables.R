# Generates multiple interactive reactable variants from the TIER2 Excel file.
# Run: Rscript reactable_tables.R

required_pkgs <- c("readxl", "reactable", "reactablefmtr", "htmltools")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    "Install missing packages before running: ",
    paste(missing_pkgs, collapse = ", ")
  )
}

data_path <- "TIER2_recommendations.xlsx"
if (!file.exists(data_path)) {
  stop("Input file not found: ", data_path)
}

sheets <- readxl::excel_sheets(data_path)
if (length(sheets) == 0) stop("No sheets found in: ", data_path)

# Pick the first sheet by default; change to a specific sheet if needed.
raw <- readxl::read_xlsx(data_path, sheet = sheets[1])

# Basic cleanup: trim whitespace in names and de-duplicate.
clean_names <- function(x) {
  x <- trimws(x)
  x <- gsub("\\s+", "_", x)
  make.unique(x)
}

names(raw) <- clean_names(names(raw))

# Ensure there is at least one numeric column to drive formatting.
num_cols <- names(raw)[vapply(raw, is.numeric, logical(1))]
if (length(num_cols) == 0) {
  stop("No numeric columns detected. Numeric columns are needed for visual encodings.")
}

# Pick a focal numeric column for scoring/importance visuals.
score_candidates <- intersect(
  tolower(num_cols),
  c("score", "total", "priority", "importance", "rank", "rating")
)
score_col <- if (length(score_candidates) > 0) {
  num_cols[match(score_candidates[1], tolower(num_cols))]
} else {
  num_cols[1]
}

# Common table theme
fmtr_theme <- reactablefmtr::reactablefmtr_theme(
  header_background = "#0e2f44",
  header_font = "#f4f7fb",
  border_color = "#0e2f44",
  striped_color = "#f7f9fb",
  highlight_color = "#fff3c4"
)

# 1) Heatmap-style table (color tiles on numeric columns)
heatmap_cols <- lapply(names(raw), function(col) {
  if (col %in% num_cols) {
    reactable::colDef(
      format = reactable::colFormat(separators = TRUE, digits = 2),
      style = reactablefmtr::color_tiles(
        raw,
        columns = col,
        colors = c("#f7fbff", "#08306b")
      )
    )
  } else {
    reactable::colDef()
  }
})

names(heatmap_cols) <- names(raw)

tbl_heatmap <- reactable::reactable(
  raw,
  columns = heatmap_cols,
  searchable = TRUE,
  highlight = TRUE,
  striped = TRUE,
  compact = TRUE,
  resizable = TRUE,
  defaultPageSize = 15,
  theme = fmtr_theme
)

# 2) Data bars for the top numeric columns
bar_cols <- num_cols[seq_len(min(length(num_cols), 4))]
bar_defs <- lapply(names(raw), function(col) {
  if (col %in% bar_cols) {
    reactable::colDef(
      format = reactable::colFormat(separators = TRUE, digits = 2),
      cell = reactablefmtr::data_bars(
        raw,
        columns = col,
        fill_color = "#1f78b4",
        background = "#e9eef2"
      )
    )
  } else {
    reactable::colDef()
  }
})

names(bar_defs) <- names(raw)

tbl_bars <- reactable::reactable(
  raw,
  columns = bar_defs,
  searchable = TRUE,
  highlight = TRUE,
  striped = TRUE,
  compact = TRUE,
  resizable = TRUE,
  defaultPageSize = 15,
  theme = fmtr_theme
)

# 3) Icon set for the focal score column + subtle row highlighting
icon_defs <- lapply(names(raw), function(col) {
  if (col == score_col) {
    reactable::colDef(
      name = paste0(col, " (priority)")
    )
  } else {
    reactable::colDef()
  }
})

names(icon_defs) <- names(raw)

# Compute thresholds for icon bins.
score_vals <- raw[[score_col]]
score_vals <- score_vals[is.finite(score_vals)]
qs <- stats::quantile(score_vals, probs = c(0.2, 0.5, 0.8), na.rm = TRUE)

icon_defs[[score_col]] <- reactable::colDef(
  format = reactable::colFormat(separators = TRUE, digits = 2),
  cell = reactablefmtr::icon_sets(
    raw,
    columns = score_col,
    icons = c("signal", "signal", "signal", "signal"),
    colors = c("#d73027", "#fc8d59", "#fee08b", "#1a9850"),
    breaks = c(-Inf, qs[[1]], qs[[2]], qs[[3]], Inf)
  )
)

row_highlight <- function(index) {
  val <- raw[[score_col]][index]
  if (!is.finite(val)) return(NULL)
  if (val >= qs[[3]]) "background-color: #e8f5e9;" else NULL
}


tbl_icons <- reactable::reactable(
  raw,
  columns = icon_defs,
  searchable = TRUE,
  highlight = TRUE,
  striped = TRUE,
  compact = TRUE,
  resizable = TRUE,
  defaultPageSize = 15,
  theme = fmtr_theme,
  rowStyle = row_highlight
)

# Combine into a single HTML output
out_dir <- "output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

html_doc <- htmltools::tagList(
  htmltools::tags$h2("TIER2 Recommendations – Reactable Variants"),
  htmltools::tags$p("Heatmap-style table with numeric color tiles."),
  tbl_heatmap,
  htmltools::tags$hr(),
  htmltools::tags$p("Data bars for quick magnitude scanning."),
  tbl_bars,
  htmltools::tags$hr(),
  htmltools::tags$p("Icon-set emphasis on the focal score column."),
  tbl_icons
)

htmltools::save_html(html_doc, file = file.path(out_dir, "reactable_variants.html"))
message("Wrote: ", file.path(out_dir, "reactable_variants.html"))
