# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ggforce)      # geom_arc_bar for circle segments
library(patchwork)    # combining plots
library(stringr)
library(readxl)
library(geomtextpath) # curved text along paths
library(ggimage)      # placing image files at plot coordinates


# File paths --------------------------------------------------------------

# Locate the folder from anywhere inside the repository. This also handles an
# RStudio session whose working directory is still 03_archive_sandbox.
find_content_dir <- function(folder, marker) {
  current <- normalizePath(getwd(), mustWork = TRUE)

  repeat {
    candidates <- unique(c(current, file.path(current, folder)))
    match <- candidates[file.exists(file.path(candidates, marker))]

    if (length(match) > 0) {
      return(normalizePath(match[[1]], mustWork = TRUE))
    }

    parent <- dirname(current)
    if (identical(parent, current)) break
    current <- parent
  }

  stop("Could not locate ", folder, ". Open TIER2_recommendations.Rproj and try again.")
}

static_dir <- find_content_dir("01_publication_static", "circle_diagram.R")

static_path <- function(...) file.path(static_dir, ...)


# Load and clean data -----------------------------------------------------

# Read recommendations from Excel, extract theme and rec IDs from the
# numbering column (e.g. "R1.1." → theme_id "R1", rec_id "R1.1"),
# and standardise stakeholder label capitalisation
recs <- read_excel(static_path("TIER2_recommendations.xlsx")) %>%
  transmute(
    theme_id     = str_extract(No, "^R\\d"),
    theme        = Category,
    rec_id       = str_remove(No, "\\.$"),
    rec_title    = Title,
    stakeholders = Stakeholder %>%
      str_replace_all("Research Communities", "Research communities") %>%
      str_replace_all("Meta-Researchers", "Meta-researchers")
  ) %>%
  # R2.1 (Drive culture change) removed based on updates to the recommendations
  filter(rec_id != "R2.1") %>%
  # Renumber remaining R2 recs to close the gap: R2.2 → R2.1, R2.3 → R2.2
  mutate(rec_id = case_when(
    rec_id == "R2.2" ~ "R2.1",
    rec_id == "R2.3" ~ "R2.2",
    TRUE             ~ rec_id
  )) %>%
  # Override rec titles with shortened display versions (uses renumbered IDs)
  mutate(rec_title = case_when(
    rec_id == "R1.1" ~ "Sustainable open infrastructure",
    rec_id == "R1.2" ~ "Standards for data re-use",
    rec_id == "R1.3" ~ "Persistent identifiers & metadata",
    rec_id == "R2.1" ~ "Responsible reproducibility metrics",
    rec_id == "R2.2" ~ "Fund replication studies",
    rec_id == "R3.1" ~ "Training ecosystems & networks",
    rec_id == "R3.2" ~ "Leadership & researcher training",
    rec_id == "R3.3" ~ "Journal infrastructure capacity",
    rec_id == "R4.1" ~ "Efficacy of interventions",
    rec_id == "R4.2" ~ "Costs & benefits of interventions",
    rec_id == "R4.3" ~ "Enable metaresearch workflows"
  ))


# Colour palette ----------------------------------------------------------

# Brand colours — Option C: alternating navy/teal families for max contrast
# between adjacent themes. Navy themes (R1, R3) use white text;
# teal themes (R2, R4) use dark navy text.
theme_cols <- c(
  "R1" = "#0D2557",  # primary navy
  "R2" = "#32dbce",  # primary teal
  "R3" = "#6B8FBF",  # medium navy
  "R4" = "#C2F5F2",  # very light teal
  "inactive" = "#ebecf0"
)

# Lookup for text colour on top of each theme's slice/band background
theme_text_cols <- c(
  "R1" = "#FFFFFF",  # white on navy
  "R2" = "#0D2557",  # dark navy on teal
  "R3" = "#FFFFFF",  # white on mid navy
  "R4" = "#0D2557"   # dark navy on light teal
)


# Compute slice geometry --------------------------------------------------

# Each theme occupies one quarter of the circle (pi/2 radians).
# Slices within a theme are divided equally, so themes with fewer recs
# (e.g. R2 with 2 recs) have wider slices than themes with 3 recs.
# Angles are measured clockwise from the top (12 o'clock):
#   x = r * sin(theta),  y = r * cos(theta)
# label_radius is the midpoint of the arc ring, used to centre text
# vertically within each slice. label_rotation keeps text upright on
# both the left and right halves of the circle.
plot_data <- recs %>%
  group_by(theme_id) %>%
  mutate(
    rec_num    = row_number(),
    n_recs     = n(),
    theme_num  = as.numeric(factor(theme_id, levels = c("R1", "R2", "R3", "R4"))),
    slice_size  = (pi / 2) / n_recs,                                        # arc width per rec
    theta_start = (theme_num - 1) * pi / 2 + (rec_num - 1) * slice_size,   # slice start angle
    theta_end   = (theme_num - 1) * pi / 2 + rec_num * slice_size,          # slice end angle
    label_angle = (theta_start + theta_end) / 2,                            # midpoint angle for label placement
    label_radius = 1.30,                                                     # offset toward outer edge of arc ring (outer edge = 1.95)
    x_label = label_radius * sin(label_angle),
    y_label = label_radius * cos(label_angle),
    label = str_wrap(rec_title, width = 15),                                 # wrap long titles to fit slice
    label_rotation = ((90 - label_angle * 180 / pi + 90) %% 180) - 90,      # keep text readable on all sides
    text_colour = theme_text_cols[theme_id]                                  # white on navy, dark navy on teal
  ) %>%
  ungroup()


# Theme band geometry -----------------------------------------------------

# The outer coloured band groups recs by theme. Each theme spans exactly
# one quadrant (pi/2). mid_angle is used to anchor the curved theme label.
theme_labels <- tibble(
  theme_id = c("R1", "R2", "R3", "R4"),
  theta_start = (0:3) * pi / 2,
  theta_end = (1:4) * pi / 2,
  mid_angle = c(pi / 4, 3 * pi / 4, 5 * pi / 4, 7 * pi / 4),
  label = c(
    "R1 Infrastructure, standards & community",
    "R2 Incentives, policy & culture",
    "R3 Research & metascience",
    "R4 Skills & capacity"
  )
) %>%
  mutate(
    # White text on navy bands (R1, R3); dark navy text on teal bands (R2, R4)
    label_colour = theme_text_cols[theme_id]
  )

# Generate dense point sequences along each theme arc at radius 2.58 so
# geom_textpath can curve the theme label text along the band.
# Small inset (0.12 rad) prevents text running too close to the slice boundaries.
theme_text_data <- theme_labels %>%
  rowwise() %>%
  mutate(theta = list(seq(theta_start + 0.12, theta_end - 0.12, length.out = 120))) %>%
  ungroup() %>%
  unnest(theta) %>%
  mutate(
    x = 2.58 * sin(theta),
    y = 2.58 * cos(theta)
  )


# Main circle diagram -----------------------------------------------------

main_plot <- ggplot(plot_data) +

  # Inner arc ring: one coloured slice per recommendation (r0–r = 0.25–1.95)
  geom_arc_bar(
    aes(
      x0 = 0, y0 = 0,
      r0 = 0.25, r = 1.95,
      start = theta_start,
      end = theta_end,
      fill = theme_id
    ),
    colour = "white",
    linewidth = 1
  ) +

  # Rec title text, rotated to follow the radial direction of each slice;
  # colour mapped per theme (white on navy, dark navy on teal)
  geom_text(
    aes(
      x = x_label,
      y = y_label,
      label = label,
      angle = label_rotation,
      colour = text_colour
    ),
    size = 3,
    fontface = "bold",
    lineheight = 0.85
  ) +

  # Rec ID labels (e.g. R1.1) placed in the white gap between the inner ring
  # and the outer theme band — fixed dark navy as background here is always white
  geom_text(
    aes(
      x = 2.1 * sin(label_angle),
      y = 2.1 * cos(label_angle),
      label = rec_id
    ),
    size = 2.8,
    fontface = "bold",
    colour = "#0D2557"
  ) +

  # Outer theme band: one arc per theme (r0–r = 2.36–2.80)
  geom_arc_bar(
    data = theme_labels,
    aes(
      x0 = 0, y0 = 0,
      r0 = 2.36, r = 2.80,
      start = theta_start,
      end = theta_end,
      fill = theme_id
    ),
    colour = "white",
    linewidth = 1
  ) +

  # Theme label text curved along the midline of the outer band (radius 2.58)
  geom_textpath(
    data = theme_text_data,
    aes(x = x, y = y, label = label, group = theme_id, colour = label_colour),
    size = 3.3,
    fontface = "bold",
    text_only = TRUE,   # draw only text, not the underlying path line
    upright = TRUE,     # flip text on bottom half so it always reads left-to-right
    halign = "center",
    show.legend = FALSE
  ) +

  scale_fill_manual(values = theme_cols) +
  scale_colour_identity() +
  # Tightened limits (theme band outer edge = 2.80) to minimise dead margin
  # and make the diagram fill more of the plot area
  coord_fixed(xlim = c(-2.95, 2.95), ylim = c(-2.95, 2.95)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(1, 1, 1, 1))


# Stakeholder data --------------------------------------------------------

# Pivot stakeholders to long format (one row per rec–stakeholder pair)
# so each rec can be matched against a given stakeholder name
stakeholder_data <- recs %>%
  separate_rows(stakeholders, sep = ";\\s*") %>%
  mutate(stakeholder = str_squish(stakeholders)) %>%
  select(rec_id, stakeholder)

# The five stakeholder groups used as panel titles
stakeholders <- c(
  "Funders",
  "Institutions",
  "Publishers",
  "Meta-researchers",
  "Research communities"
)


# Stakeholder plot function -----------------------------------------------

# Produces a small circle diagram for a single stakeholder showing only
# the slices where that stakeholder has ownership or influence (highlighted
# in theme colour); all other slices are shown in grey ("inactive").
make_stakeholder_plot <- function(stakeholder_name) {

  # Join plot geometry with relevance flag for this stakeholder:
  # is_relevant = TRUE if any row in stakeholder_data matches this rec + stakeholder
  df <- plot_data %>%
    left_join(
      stakeholder_data %>%
        mutate(is_relevant = stakeholder == stakeholder_name) %>%
        group_by(rec_id) %>%
        summarise(is_relevant = any(is_relevant), .groups = "drop"),
      by = "rec_id"
    ) %>%
    mutate(
      is_relevant = replace_na(is_relevant, FALSE),
      # Use theme colour for relevant recs, grey for others
      fill_group = if_else(is_relevant, theme_id, "inactive")
    )

  ggplot(df) +

    # Arc slices coloured by relevance
    geom_arc_bar(
      aes(
        x0 = 0, y0 = 0,
        r0 = 0.38, r = 1.08,
        start = theta_start,
        end = theta_end,
        fill = fill_group
      ),
      colour = "white",
      linewidth = 0.8
    ) +

    # Rec ID labels shown only on relevant (highlighted) slices;
    # colour matched to theme background (white on navy, dark navy on teal)
    geom_text(
      data = ~ filter(.x, is_relevant),
      aes(x = 0.88 * sin(label_angle), y = 0.88 * cos(label_angle),
          label = rec_id, colour = text_colour),
      size = 2.65,
      fontface = "bold"
    ) +

    scale_fill_manual(values = theme_cols) +
    scale_colour_identity() +
    coord_fixed(xlim = c(-1.09, 1.09), ylim = c(-1.09, 1.09)) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, size = 10, face = "bold")
    ) +
    labs(title = stakeholder_name)
}

# Generate one small plot per stakeholder
stakeholder_plots <- map(stakeholders, make_stakeholder_plot)


# Combine into final layout -----------------------------------------------

# Main circle on the left (wider), stakeholder panels in a 2-column grid
# on the right. Width ratio 3.3:2.7 gives the main plot more space.
final_plot <-
  main_plot |
  wrap_plots(stakeholder_plots, ncol = 2) +
  plot_layout(widths = c(3.3, 2.7))

# Optional caption (commented out — kept for reference)
# annotated_plot <- final_plot +
#   plot_annotation(
#     caption = "TIER2 Strategic responsibilities for strengthening reproducibility. Left: all 11 recommendations grouped by strategic theme; each slice is one recommendation and the outer band groups recommendations by theme. Right: stakeholder-specific views show only recommendations where that stakeholder has ownership or influence.",
#     theme = theme(plot.caption = element_text(hjust = 0.5, size = 8, colour = "grey30", margin = margin(t = 8)))
#   )

final_plot


# Export ------------------------------------------------------------------

ggsave(
  filename = static_path("output", "tier2_circle_diagram.pdf"),
  plot = final_plot,
  width = 11.69,
  height = 8.27,
  units = "in"
)

ggsave(
  filename = static_path("output", "tier2_circle_diagram.png"),
  plot = final_plot,
  width = 11.69,
  height = 8.27,
  units = "in",
  dpi = 300
)


# =========================================================================
# Spoke-style circle diagram (UNESCO Open Science style)
# =========================================================================
# Four solid coloured quadrants with the theme name curved inside near the
# outer edge. Each recommendation is a labelled spoke radiating outward:
# a filled dot marks the base on the quadrant edge, a line extends outward,
# and the rec ID + title appear as a label at the end of the spoke.

# Geometry parameters ---------------------------------------------------------
r_quad       <- 2.00   # outer radius of coloured quadrant sectors
r_center     <- 0.62   # radius of white center circle
r_theme_text <- 1.20   # inset theme labels to retain padding from circle edge
r_dot        <- 2.12   # spoke base dot radius (just outside quadrant edge)
r_spoke_end  <- 2.50   # outer end of spoke line (stops just before icon)
r_icon       <- 2.72   # icon centre radius
r_label      <- 3.05   # label text anchor radius (beyond icon)

# Horizontal theme labels centred within each quadrant ------------------------
theme_text_data_spoke <- theme_labels %>%
  mutate(
    x = r_theme_text * sin(mid_angle),
    y = r_theme_text * cos(mid_angle),
    label = str_wrap(label, width = 18)
  )

# Spoke and label geometry ----------------------------------------------------
# Compute dot, line, and text positions for each rec. hjust flips between
# 0 (left-align, right half) and 1 (right-align, left half) so labels
# always read away from the centre. theme_colour stores the hex value
# directly for use with scale_colour_identity() on the spoke lines.
spoke_data <- plot_data %>%
  mutate(
    theme_colour = theme_cols[theme_id],
    dot_x        = r_dot * sin(label_angle),
    dot_y        = r_dot * cos(label_angle),
    spoke_x      = r_spoke_end * sin(label_angle),
    spoke_y      = r_spoke_end * cos(label_angle),
    icon_x       = r_icon * sin(label_angle),
    icon_y       = r_icon * cos(label_angle),
    icon_path    = static_path("icons", paste0(rec_id, ".png")),
    label_x      = r_label * sin(label_angle),
    label_y      = r_label * cos(label_angle),
    label_text   = str_wrap(paste0(rec_id, "  ", rec_title), width = 22),
    hjust        = case_when(
      sin(label_angle) >  0.15 ~ 0,    # right half: left-align
      sin(label_angle) < -0.15 ~ 1,    # left half:  right-align
      TRUE                     ~ 0.5   # top/bottom: centre
    )
  )

# Build spoke plot ------------------------------------------------------------
spoke_plot <- ggplot() +

  # Four solid coloured quadrant sectors filling to near-centre (r0 ≈ 0)
  geom_arc_bar(
    data = theme_labels,
    aes(
      x0 = 0, y0 = 0,
      r0 = 0.02, r = r_quad,
      start = theta_start, end = theta_end,
      fill = theme_id
    ),
    colour = "white",
    linewidth = 1.5
  ) +

  # Horizontal, wrapped theme name centred within each quadrant;
  # label_colour gives white text on navy, dark navy text on teal.
  geom_text(
    data = theme_text_data_spoke,
    aes(x = x, y = y, label = label, colour = label_colour),
    size = 3,
    fontface = "bold",
    lineheight = 0.9,
    hjust = 0.5,
    vjust = 0.5,
    show.legend = FALSE
  ) +

  # White center circle overlaid to create a clean hub
  geom_circle(
    data = tibble(x0 = 0, y0 = 0, r = r_center),
    aes(x0 = x0, y0 = y0, r = r),
    fill = "white", colour = NA
  ) +

  # Center text: title on top, subtitle below
  annotate("text", x = 0, y = 0.14,
           label = "TIER2", size = 5.5, fontface = "bold", colour = "#0D2557") +
  annotate("text", x = 0, y = -0.16,
           label = "Recommendations", size = 2.0, colour = "#0D2557") +

  # Spoke lines from quadrant edge outward, coloured by theme
  geom_segment(
    data = spoke_data,
    aes(x = dot_x, y = dot_y, xend = spoke_x, yend = spoke_y,
        colour = theme_colour),
    linewidth = 0.5
  ) +

  # Filled dot at the spoke base on the quadrant edge
  geom_point(
    data = spoke_data,
    aes(x = dot_x, y = dot_y, fill = theme_id),
    shape = 21, size = 3, colour = "white", stroke = 0.5
  ) +

  # Icon at the outer end of each spoke, sized relative to plot area
  geom_image(
    data = spoke_data,
    aes(x = icon_x, y = icon_y, image = icon_path),
    size = 0.045, 
    use_cache = FALSE
  ) +

  # Rec ID + title label beyond the icon
  geom_text(
    data = spoke_data,
    aes(x = label_x, y = label_y, label = label_text, hjust = hjust),
    size = 2.3,
    colour = "#0D2557",
    lineheight = 0.85
  ) +

  # fill mapped via scale_fill_manual (arc bars + dots use theme_id)
  # colour uses identity scale (spoke lines use hex directly; textpath uses label_colour hex)
  scale_fill_manual(values = theme_cols) +
  scale_colour_identity() +
  coord_fixed(xlim = c(-6.2, 6.2), ylim = c(-6.2, 6.2)) +
  theme_void() +
  theme(legend.position = "none")

spoke_plot

# Export spoke diagram --------------------------------------------------------
ggsave(
  filename = static_path("output", "tier2_spoke_diagram.pdf"),
  plot = spoke_plot,
  width = 10,
  height = 10,
  units = "in"
)

ggsave(
  filename = static_path("output", "tier2_spoke_diagram.png"),
  plot = spoke_plot,
  width = 10,
  height = 10,
  units = "in",
  dpi = 300
)
