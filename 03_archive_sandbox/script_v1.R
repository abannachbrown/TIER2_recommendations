# TIER2 recommendations: interactive tables with reactable + reactablefmtr
# ---------------------------------------------------------------------
# This script reads the Excel workbook, engineers a few helper fields,
# and renders several creative interactive table variants as standalone HTML.
#
# Expected input file:
#   TIER2_recommendations.xlsx
#
# Output files:
#   output/tier2_table_overview.html
#   output/tier2_table_metrics.html
#   output/tier2_table_stakeholder_matrix.html
#   output/tier2_table_by_category.html
#
# Packages -------------------------------------------------------------------

# install.packages("reactable")
# remotes::install_github("kcuilla/reactablefmtr"


required_pkgs <- c(
  "readxl", "dplyr", "stringr", "purrr", "tidyr", "tibble",
  "reactable", "reactablefmtr", "htmltools", "htmlwidgets", "scales", "janitor"
)

missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs)
}

invisible(lapply(required_pkgs, library, character.only = TRUE))



# Paths ----------------------------------------------------------------------
input_file <- "TIER2_recommendations.xlsx"
out_dir <- "output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

if (!file.exists(input_file)) {
  stop(sprintf("Input file not found: %s", normalizePath(input_file, winslash = "/", mustWork = FALSE)))
}

# Read and prepare data ------------------------------------------------------
raw_df <- readxl::read_excel(input_file) |> janitor::clean_names()

stopifnot(all(c("no", "title", "description", "category", "stakeholder") %in% names(raw_df)))

recommendations <- raw_df |>
  mutate(
    no = stringr::str_squish(no),
    title = stringr::str_squish(title),
    description = stringr::str_squish(description),
    category = stringr::str_squish(category),
    stakeholder = stringr::str_squish(stakeholder),
    section = stringr::str_extract(no, "(?<=R)\\d+"),
    section = factor(section, levels = sort(unique(section), na.last = TRUE)),
    recommendation_num = as.numeric(stringr::str_extract(no, "(?<=\\.)\\d+")),
    stakeholder_list = stringr::str_split(stakeholder, pattern = "\\s*;\\s*"),
    stakeholder_count = purrr::map_int(stakeholder_list, length),
    description_words = stringr::str_count(description, "\\S+"),
    title_words = stringr::str_count(title, "\\S+")
  )

all_stakeholders <- recommendations |>
  pull(stakeholder_list) |>
  unlist() |>
  unique() |>
  sort()

# Add binary stakeholder columns for matrix/bubble style views
for (s in all_stakeholders) {
  nm <- make.names(s)
  recommendations[[nm]] <- purrr::map_lgl(recommendations$stakeholder_list, ~ s %in% .x)
}

# Reusable helpers -----------------------------------------------------------
brand_white <- "#FFFFFF"
brand_navy <- "#0D2557"
brand_teal <- "#32dbce"
brand_cloud <- "#ebecf0"

brand_theme <- reactable::reactableTheme(
  style = list(fontFamily = "Arial, sans-serif"),
  headerStyle = list(fontFamily = "Arial, sans-serif", fontWeight = "700")
)

sort_css <- paste0(
  ".tier2-overview .rt-th[aria-sort='ascending']::after,",
  ".tier2-overview .rt-th[aria-sort='descending']::after {",
  " content: ' '; margin-left: 6px; display: inline-block; }",
  ".tier2-overview .rt-th[aria-sort='ascending']::after { content: '▲'; color: ",
  brand_teal, "; font-size: 10px; }",
  ".tier2-overview .rt-th[aria-sort='descending']::after { content: '▼'; color: ",
  brand_teal, "; font-size: 10px; }"
)

add_custom_css <- function(widget, css) {
  htmlwidgets::prependContent(widget, htmltools::tags$style(css))
}

category_palette <- c(
  "Infrastructure, standards and community" = brand_navy,
  "Incentives and policy" = brand_teal,
  "Training and skills" = brand_navy,
  "Evidence-based policy" = brand_teal
)

section_palette <- c("1" = brand_navy, "2" = brand_teal, "3" = brand_navy, "4" = brand_teal)

badge_div <- function(text, bg = brand_cloud, fg = brand_navy, radius = "999px") {
  htmltools::div(
    style = paste(
      "display:inline-block; padding:4px 10px; margin:2px 6px 2px 0;",
      sprintf("background:%s; color:%s; border-radius:%s;", bg, fg, radius),
      "font-size:12px; font-weight:600; line-height:1.2;"
    ),
    text
  )
}

stakeholder_icon <- function(label) {
  key <- tolower(label)
  icon_svg <- function(children) {
    htmltools::tags$svg(
      width = "18", height = "18", viewBox = "0 0 24 24",
      style = paste(
        "display:inline-block; vertical-align:middle; margin-right:6px;",
        "color:", brand_navy, ";"
      ),
      fill = "none",
      stroke = brand_navy,
      `stroke-width` = "1.6",
      `stroke-linecap` = "round",
      `stroke-linejoin` = "round",
      children
    )
  }

  if (grepl("funder", key)) {
    icon_svg(htmltools::tags$path(d = "M17 5H9a5 5 0 0 0 0 10h8M9 9h7M9 13h7"))
  } else if (grepl("publisher", key)) {
    icon_svg(htmltools::tagList(
      htmltools::tags$path(d = "M4 6h6a3 3 0 0 1 3 3v9H7a3 3 0 0 0-3 3z"),
      htmltools::tags$path(d = "M20 6h-6a3 3 0 0 0-3 3v9h6a3 3 0 0 1 3 3z")
    ))
  } else if (grepl("meta", key) || grepl("policy", key)) {
    icon_svg(htmltools::tagList(
      htmltools::tags$path(d = "M7 4h8l4 4v12H7z"),
      htmltools::tags$path(d = "M15 4v4h4"),
      htmltools::tags$path(d = "M9 12h8"),
      htmltools::tags$path(d = "M9 16h6")
    ))
  } else if (grepl("institution", key) || grepl("university", key)) {
    icon_svg(htmltools::tagList(
      htmltools::tags$path(d = "M3 9l9-5 9 5"),
      htmltools::tags$path(d = "M5 10v8"),
      htmltools::tags$path(d = "M9 10v8"),
      htmltools::tags$path(d = "M15 10v8"),
      htmltools::tags$path(d = "M19 10v8"),
      htmltools::tags$path(d = "M3 18h18")
    ))
  } else if (grepl("community", key)) {
    icon_svg(htmltools::tagList(
      htmltools::tags$circle(cx = "7", cy = "10", r = "2.2", fill = brand_teal, stroke = "none"),
      htmltools::tags$circle(cx = "12", cy = "8", r = "2.2", fill = brand_teal, stroke = "none"),
      htmltools::tags$circle(cx = "17", cy = "10", r = "2.2", fill = brand_teal, stroke = "none"),
      htmltools::tags$path(d = "M5 16c1.2-1.6 3.1-2.4 5-2.4s3.8.8 5 2.4")
    ))
  } else {
    icon_svg(htmltools::tags$circle(cx = "12", cy = "12", r = "6", fill = brand_teal, stroke = "none"))
  }
}

stakeholder_badges <- function(value) {
  if (is.na(value) || !nzchar(value)) return("")
  chips <- stringr::str_split(value, pattern = "\\s*;\\s*", simplify = TRUE)
  chips <- chips[chips != ""]
  htmltools::tagList(lapply(chips, function(x) {
    htmltools::tagList(
      stakeholder_icon(x),
      badge_div(x, bg = brand_cloud, fg = brand_navy)
    )
  }))
}

description_block <- function(value, title = NULL) {
  htmltools::div(
    style = "line-height:1.35; white-space:normal;",
    if (!is.null(title)) htmltools::div(style = "font-weight:700; margin-bottom:4px;", title),
    htmltools::div(value)
  )
}

save_widget <- function(widget, file) {
  # reactablefmtr::add_title/subtitle returns HTML tag lists, not pure htmlwidgets.
  if (inherits(widget, "htmlwidget")) {
    htmlwidgets::saveWidget(widget, file = file, selfcontained = TRUE)
  } else {
    htmltools::save_html(htmltools::tagList(widget), file = file)
  }
}

# Table 1: Executive overview ------------------------------------------------
# A card-like table focused on readability and browsing.
overview_tbl <- reactable::reactable(
  recommendations |> select(no, title, description, category, stakeholder, stakeholder_count, description_words),
  searchable = TRUE,
  filterable = TRUE,
  groupBy = "category",
  striped = TRUE,
  bordered = FALSE,
  highlight = TRUE,
  compact = FALSE,
  pagination = FALSE,
  showSortIcon = FALSE,
  showSortable = TRUE,
  defaultPageSize = nrow(recommendations),
  resizable = TRUE,
  defaultSorted = list(category = "asc", no = "asc"),
  class = "tier2-overview",
  theme = utils::modifyList(
    reactablefmtr::fivethirtyeight(
      font_size = 13,
      font_color = brand_navy,
      header_font_color = brand_navy,
      background_color = brand_white,
      cell_padding = 10
    ),
    utils::modifyList(
      brand_theme,
      list(
        groupHeaderStyle = list(
          background = brand_cloud,
          color = brand_navy,
          fontWeight = "700",
          borderTop = paste0("2px solid ", brand_navy)
        )
      )
    )
  ),
  columns = list(
    no = reactable::colDef(
      name = "ID",
      minWidth = 85,
      align = "center",
      style = function(value) {
        section_id <- stringr::str_extract(value, "(?<=R)\\d+")
        list(
          background = section_palette[[section_id]],
          color = "white",
          fontWeight = 700,
          borderRadius = "8px"
        )
      }
    ),
    title = reactable::colDef(
      minWidth = 260,
      html = TRUE,
      cell = function(value, index) {
        description_block(value, title = paste("Section", recommendations$section[index]))
      }
    ),
    description = reactable::colDef(
      minWidth = 420,
      html = TRUE,
      cell = function(value) description_block(value),
      style = list(fontSize = "13px")
    ),
    category = reactable::colDef(show = FALSE),
    stakeholder = reactable::colDef(
      name = "Stakeholders",
      minWidth = 280,
      html = TRUE,
      cell = stakeholder_badges
    ),
    stakeholder_count = reactable::colDef(show = FALSE),
    description_words = reactable::colDef(show = FALSE)
  )
) |>
  reactablefmtr::add_title("TIER2 recommendations — executive browser") |>
  reactablefmtr::add_subtitle("A readable, searchable table with category and stakeholder tagging") |>
  reactablefmtr::add_source("Source: TIER2_recommendations.xlsx")

overview_tbl <- add_custom_css(overview_tbl, sort_css)

save_widget(overview_tbl, file.path(out_dir, "tier2_table_overview.html"))

# Table 2: Metrics-heavy prioritisation view ---------------------------------
# Derive a few metrics to make text recommendations visually scannable.
metrics_tbl <- reactable::reactable(
  recommendations |>
    transmute(
      no,
      section,
      category,
      title,
      stakeholder_count,
      description_words,
      title_words,
      stakeholder = stakeholder
    ),
  searchable = TRUE,
  filterable = TRUE,
  fullWidth = TRUE,
  height = 520,
  defaultPageSize = nrow(recommendations),
  pagination = FALSE,
  compact = TRUE,
  highlight = TRUE,
  bordered = FALSE,
  striped = TRUE,
  theme = utils::modifyList(
    reactablefmtr::slate(
      font_size = 13,
      font_color = brand_navy,
      header_font_color = brand_navy,
      background_color = brand_white,
      cell_padding = 8
    ),
    brand_theme
  ),
  columns = list(
    no = reactable::colDef(name = "ID", minWidth = 85, align = "center", style = list(fontWeight = 700)),
    section = reactable::colDef(
      name = "Section",
      cell = reactablefmtr::color_tiles(recommendations, colors = c(brand_cloud, brand_teal))
    ),
    category = reactable::colDef(minWidth = 180),
    title = reactable::colDef(minWidth = 250),
    stakeholder_count = reactable::colDef(
      name = "Stakeholders",
      minWidth = 170,
      align = "left",
      cell = reactablefmtr::data_bars(
        recommendations,
        fill_color = brand_teal,
        background = brand_cloud,
        text_position = "outside-end",
        round_edges = TRUE,
        text_color = brand_navy
      )
    ),
    description_words = reactable::colDef(
      name = "Description words",
      minWidth = 180,
      align = "left",
      cell = reactablefmtr::data_bars(
        recommendations,
        fill_color = brand_navy,
        background = brand_cloud,
        text_position = "outside-end",
        round_edges = TRUE,
        text_color = brand_navy
      )
    ),
    title_words = reactable::colDef(
      name = "Title words",
      minWidth = 140,
      cell = reactablefmtr::color_scales(recommendations, colors = c(brand_cloud, brand_teal))
    ),
    stakeholder = reactable::colDef(name = "Who is affected", minWidth = 300)
  ),
  defaultSorted = list(stakeholder_count = "desc", description_words = "desc")
) |>
  reactablefmtr::add_title("TIER2 recommendations — metrics view") |>
  reactablefmtr::add_subtitle("Derived metrics create a pseudo-prioritisation table using bars and color scales") |>
  reactablefmtr::add_source("Metrics are derived from the text fields: stakeholder_count, description_words, title_words")

save_widget(metrics_tbl, file.path(out_dir, "tier2_table_metrics.html"))

# Table 3: Stakeholder matrix -------------------------------------------------
# Wide matrix showing which stakeholder groups are touched by each recommendation.
stakeholder_matrix <- recommendations |>
  arrange(category, no) |>
  transmute(
    no,
    title,
    category,
    !!!recommendations |> select(all_of(make.names(all_stakeholders)))
  )

matrix_row_style <- function(index) {
  if (index <= 1) return(NULL)
  if (stakeholder_matrix$category[index] != stakeholder_matrix$category[index - 1]) {
    return(list(borderTop = paste0("2px solid ", brand_navy)))
  }
  NULL
}

matrix_tbl <- reactable::reactable(
  stakeholder_matrix,
  searchable = TRUE,
  filterable = TRUE,
  fullWidth = TRUE,
  height = 520,
  rowStyle = matrix_row_style,
  defaultPageSize = nrow(stakeholder_matrix),
  pagination = FALSE,
  compact = TRUE,
  bordered = FALSE,
  striped = TRUE,
  highlight = TRUE,
  class = "tier2-matrix",
  theme = utils::modifyList(
    reactablefmtr::nytimes(
      font_size = 13,
      font_color = brand_navy,
      header_font_color = brand_navy,
      background_color = brand_white,
      cell_padding = 8
    ),
    brand_theme
  ),
  columns = c(
    list(
      no = reactable::colDef(name = "ID", minWidth = 90, style = list(fontWeight = 700), sticky = "left"),
      title = reactable::colDef(minWidth = 260, sticky = "left"),
      category = reactable::colDef(minWidth = 180, sticky = "left")
    ),
    setNames(
      lapply(make.names(all_stakeholders), function(col_nm) {
        label <- gsub("\\.", " ", col_nm)
        reactable::colDef(
          header = htmltools::tagList(
            stakeholder_icon(label),
            htmltools::tags$span(label)
          ),
          name = label,
          align = "center",
          minWidth = 110,
          cell = function(value) {
            if (isTRUE(value)) {
              htmltools::div(
                  style = sprintf("width:18px; height:18px; margin:auto; border-radius:999px; background:%s;", brand_teal)
              )
            } else {
              htmltools::div(
                  style = sprintf("width:18px; height:18px; margin:auto; border-radius:999px; background:%s;", brand_cloud)
              )
            }
          },
          html = TRUE
        )
      }),
      make.names(all_stakeholders)
    )
  )
) |>
  reactablefmtr::add_title("TIER2 recommendations — stakeholder matrix") |>
  reactablefmtr::add_subtitle("A compact presence/absence matrix for quickly spotting stakeholder coverage") |>
  reactablefmtr::add_source("Blue dots indicate the stakeholder group is explicitly named in the recommendation")

save_widget(matrix_tbl, file.path(out_dir, "tier2_table_stakeholder_matrix.html"))

# Table 4: Grouped-by-category browser ---------------------------------------
by_category_tbl <- reactable::reactable(
  recommendations |>
    arrange(category, no) |>
    select(category, no, title, stakeholder, description, stakeholder_count, description_words),
  groupBy = "category",
  searchable = TRUE,
  filterable = TRUE,
  fullWidth = TRUE,
  height = 520,
  defaultExpanded = TRUE,
  pagination = FALSE,
  bordered = FALSE,
  striped = TRUE,
  highlight = TRUE,
  defaultPageSize = nrow(recommendations),
  theme = utils::modifyList(
    reactablefmtr::fivethirtyeight(
      font_size = 13,
      font_color = brand_navy,
      header_font_color = brand_navy,
      background_color = brand_white,
      cell_padding = 10
    ),
    brand_theme
  ),
  columns = list(
    category = reactable::colDef(show = FALSE),
    no = reactable::colDef(name = "ID", minWidth = 85, style = list(fontWeight = 700)),
    title = reactable::colDef(minWidth = 260),
    stakeholder = reactable::colDef(name = "Stakeholders", minWidth = 280, html = TRUE, cell = stakeholder_badges),
    description = reactable::colDef(minWidth = 420, style = list(whiteSpace = "normal")),
    stakeholder_count = reactable::colDef(
      name = "# stakeholder groups",
      minWidth = 180,
      cell = reactablefmtr::data_bars(
        recommendations,
        fill_color = brand_teal,
        background = brand_cloud,
        text_position = "outside-end",
        round_edges = TRUE,
        text_color = brand_navy
      )
    ),
    description_words = reactable::colDef(
      name = "Description length",
      minWidth = 170,
      cell = reactablefmtr::color_scales(recommendations, colors = c(brand_cloud, brand_teal))
    )
  )
) |>
  reactablefmtr::add_title("TIER2 recommendations — grouped by category") |>
  reactablefmtr::add_subtitle("A category-first layout for workshop discussion or policy review") |>
  reactablefmtr::add_source("Grouped view built with reactable::groupBy")

save_widget(by_category_tbl, file.path(out_dir, "tier2_table_by_category.html"))

# Optional convenience: print a small summary to console ----------------------
message("Created files:")
message(" - ", file.path(out_dir, "tier2_table_overview.html"))
message(" - ", file.path(out_dir, "tier2_table_metrics.html"))
message(" - ", file.path(out_dir, "tier2_table_stakeholder_matrix.html"))
message(" - ", file.path(out_dir, "tier2_table_by_category.html"))
