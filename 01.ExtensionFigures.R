#Extension plots
load("BRMSextension_ready.RData")

#### Some plots - Marginal Effects

## 1) Slope × Body height interaction
ce1 <- conditional_effects(
  final_main,
  effects = "winSlope_c:Bheight_c",
  re_formula = NA,        # population-level (no random effects)
  method = "fitted"
)
plot(ce1, points = TRUE)

## 2) WtavR_zg × Gulf interaction
ce2 <- conditional_effects(
  final_main,
  effects = "WtavR_zg:gulf",
  re_formula = NA,
  method = "fitted"
)
plot(ce2, points = FALSE)

### Further plots on the original scale - ALL data population level

library(dplyr)
library(tidyr)
library(tidybayes)
library(ggplot2)

# 1) Per-gulf summary: mean, sd, min, max (force dplyr versions)
stats_WtavR <- testex %>%
  dplyr::mutate(gulf = as.character(gulf)) %>%
  dplyr::group_by(gulf) %>%
  dplyr::summarise(
    mean_W = mean(WtavR, na.rm = TRUE),
    sd_W   = sd(WtavR,   na.rm = TRUE),
    min_W  = min(WtavR,  na.rm = TRUE),
    max_W  = max(WtavR,  na.rm = TRUE)
  ) %>%
  dplyr::ungroup()

# 2) Build a common fine grid, then clip per-gulf to observed ranges
gulf_levels <- sort(unique(testex$gulf))
new_WtavR <- seq(min(testex$WtavR, na.rm = TRUE),
                 max(testex$WtavR, na.rm = TRUE),
                 length.out = 200)

nd <- tidyr::crossing(
  WtavR = new_WtavR,
  gulf  = gulf_levels
) %>%
  dplyr::left_join(stats_WtavR, by = "gulf") %>%
  dplyr::filter(WtavR >= min_W, WtavR <= max_W) %>%
  dplyr::mutate(
    WtavR_zg  = (WtavR - mean_W) / sd_W,
    winSlope_c = 0,
    Bheight_c  = 0
  )

# 3) Predict (population-level)
nd_pred <- nd %>%
  tidybayes::add_epred_draws(final_main, re_formula = NA)

## The plot
library(ggplot2)
library(tidybayes)
library(dplyr)

ggplot2::ggplot() +
  # 1️⃣ Raw data (semi-transparent points, coloured by group)
  geom_point(
    data = testex,
    aes(x = WtavR, y = extension, color = gulf),
    alpha = 0.10, size = 1.5
  ) +
  
  # 2️⃣ Model predictions with credible intervals
  tidybayes::stat_lineribbon(
    data = nd_pred,
    aes(x = WtavR, y = .epred, color = gulf, fill = gulf),
    .width    = c(0.95, 0.66),
    .point    = "mean",      # single best-fit line
    alpha     = 0.20,        # transparency of ribbons
    linewidth = 1.1
  ) +
  
  # 3️⃣ Labels and theme
  labs(
    x = "Mean temperature (°C) in coldest 61-day period per year",
    y = expression(paste("Predicted extension (cm ", yr^{-1}, ")")),
    color = "Gulf group",
    fill  = "Gulf group",
    title = "Interaction: WtavR × gulf (original WtavR scale)"
  ) +
  
  theme_classic(base_size = 13) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

##### And for winter slope and colony size:

library(dplyr)
library(tidyr)
library(tidybayes)
library(ggplot2)

## --- Settings / summaries ---
gulf_ref <- "oman"  # change to "gulf" if you prefer that group

mu_winSlope <- mean(testex$winSlope, na.rm = TRUE)
mu_Bheight  <- mean(testex$Bheight,  na.rm = TRUE)

# Quantiles for Bheight (representative line levels)
bh_q <- quantile(testex$Bheight, probs = c(0.10, 0.50, 0.90), na.rm = TRUE)
q10 <- as.numeric(bh_q[1]); q50 <- as.numeric(bh_q[2]); q90 <- as.numeric(bh_q[3])

# Midpoints for non-overlapping bands
b1 <- (q10 + q50) / 2
b2 <- (q50 + q90) / 2

# Assign observations to bands
test_band <- testex %>%
  dplyr::filter(gulf == gulf_ref) %>%
  dplyr::mutate(
    Bheight_band = cut(
      Bheight,
      breaks = c(-Inf, b1, b2, Inf),
      labels = c("Bheight: 10th pct", "Bheight: 50th pct", "Bheight: 90th pct"),
      right  = TRUE
    )
  ) %>%
  dplyr::filter(!is.na(Bheight_band))

# Observed winSlope ranges per band
band_ranges <- test_band %>%
  dplyr::group_by(Bheight_band) %>%
  dplyr::summarise(
    min_win = min(winSlope, na.rm = TRUE),
    max_win = max(winSlope, na.rm = TRUE),
    .groups = "drop"
  )

# Representative Bheight values for the lines
bh_rep <- dplyr::tibble(
  Bheight_band = factor(
    c("Bheight: 10th pct","Bheight: 50th pct","Bheight: 90th pct"),
    levels = c("Bheight: 10th pct","Bheight: 50th pct","Bheight: 90th pct")
  ),
  Bheight = c(q10, q50, q90)
)

# Band-specific prediction grids (only within observed winSlope ranges)
nd2 <- band_ranges %>%
  dplyr::left_join(bh_rep, by = "Bheight_band") %>%
  dplyr::rowwise() %>%
  dplyr::mutate(winSlope = list(seq(min_win, max_win, length.out = 200))) %>%
  tidyr::unnest(winSlope) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    winSlope_c = winSlope - mu_winSlope,
    Bheight_c  = Bheight  - mu_Bheight,
    gulf       = gulf_ref,
    WtavR_zg   = 0
  )

# Posterior expected means (population-level)
nd2_pred <- nd2 %>%
  tidybayes::add_epred_draws(final_main, re_formula = NA)

## --- Plot (same style as WtavR × gulf) ---
ggplot() +
  # Raw data points (transparent)
  ggplot2::geom_point(
    data = test_band,
    ggplot2::aes(x = winSlope, y = extension, color = Bheight_band),
    alpha = 0.10, size = 1.5
  ) +
  # Model predictions (no extrapolation)
  tidybayes::stat_lineribbon(
    data = nd2_pred,
    ggplot2::aes(x = winSlope, y = .epred, color = Bheight_band, fill = Bheight_band),
    .width    = c(0.95, 0.66),
    .point    = "mean",
    alpha     = 0.20,
    linewidth = 1.1
  ) +
  # Labels and theme
  ggplot2::labs(
    x = expression(paste("Winter slope (°C ", Day^{-1}, ")")),
    y = expression(paste("Predicted extension (cm ", yr^{-1}, ")")),
    color = "Branch height",
    fill  = "Branch height",
    title = "Interaction: winter slope × branch height (original scales; no extrapolation)"
  ) +
  ggplot2::theme_classic(base_size = 13) +
  ggplot2::theme(
    legend.position = "top",
    legend.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.box.background = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(face = "bold", hjust = 0.5)
  )

################
### Some contour surface plots
library(dplyr)
library(tidyr)
library(tidybayes)
library(plot3D)
library(viridis)

# --- Setup ---
gulf_ref <- "oman"
mu_winSlope <- mean(testex$winSlope, na.rm = TRUE)
mu_Bheight  <- mean(testex$Bheight,  na.rm = TRUE)

# Coarser grid so colours dominate
x_seq <- seq(min(testex$winSlope, na.rm=TRUE),
             max(testex$winSlope, na.rm=TRUE), length.out = 30)
y_seq <- seq(min(testex$Bheight,  na.rm=TRUE),
             max(testex$Bheight,  na.rm=TRUE), length.out = 30)

grid3D <- tidyr::crossing(
  winSlope = x_seq,
  Bheight  = y_seq
) %>%
  mutate(
    winSlope_c = winSlope - mu_winSlope,
    Bheight_c  = Bheight - mu_Bheight,
    WtavR_zg   = 0,
    gulf       = gulf_ref
  )

# Predict (population-level)
grid_pred <- grid3D %>%
  tidybayes::add_epred_draws(final_main, re_formula = NA) %>%
  group_by(winSlope, Bheight) %>%
  summarise(pred = mean(.epred), .groups = "drop")

# Build Z matrix
zmat <- grid_pred %>%
  arrange(winSlope, Bheight) %>%
  tidyr::pivot_wider(names_from = Bheight, values_from = pred) %>%
  arrange(winSlope) %>%
  select(-winSlope) %>%
  as.matrix()

stopifnot(nrow(zmat) == length(x_seq), ncol(zmat) == length(y_seq))

# --- Plot ---
par(mar = c(2.5, 2.5, 2.5, 4.5))

persp3D(
  x = x_seq, y = y_seq, z = zmat,
  colvar = zmat, col = viridis(120),
  
  # Mesh
  facets = TRUE,
  border = "grey75",
  lwd = 0.5,
  
  # ✅ Only bottom contours; no side-wall contours (so no vertical lines)
  contour = list(col = "grey60", nlevels = 10, side = 1),
  
  # Axes / box / view
  box = TRUE, axes = TRUE, ticktype = "detailed",
  theta = -30, phi = 22, expand = 0.65,
  shade = 0.3, lighting = TRUE, ltheta = 90, lphi = 50,
  
  # Labels
  xlab = "Winter slope (°C/Day)",
  ylab = "Colony height (cm)",
  zlab = "Predicted extension (cm/yr)",
  
  # Colour key
  colkey = list(length = 0.6, width = 0.6)
)

#### Combined plot with heat map - Colony height, winter slope, extension

# assumes you already created: x_seq, y_seq, zmat exactly as in your working 3D code
library(plot3D)
library(viridis)
library(grid)
library(gridExtra)
library(gridGraphics)

pal <- viridis(120)
zlim_shared <- range(zmat, na.rm = TRUE)

# --- Left 3D panel (exact style; tidier text spacing) ---
plot3D_left <- function() {
  op <- par(mar = c(1.4, 1.4, 1.0, 1.0),      # tighter margins
            mgp = c(2.1, 0.6, 0))             # a touch more offset label↔ticks
  on.exit(par(op), add = TRUE)
  
  persp3D(
    x = x_seq, y = y_seq, z = zmat,
    colvar = zmat, col = pal, zlim = zlim_shared,
    facets = TRUE, border = "grey75", lwd = 0.5,
    contour = list(col = "grey60", nlevels = 10, side = 1),   # bottom-only contours
    box = TRUE, axes = TRUE, ticktype = "detailed",
    theta = -30, phi = 22, expand = 0.65,
    shade = 0.3, lighting = TRUE, ltheta = 90, lphi = 50,
    xlab = "Winter slope (°C/Day)",
    ylab = "Colony height (cm)",
    zlab = "Predicted extension (cm/yr)",
    cex.lab = 1.05,           # label size
    cex.axis = 0.80,          # tick size (smaller → less overlap)
    colkey = FALSE
  )
}

# --- Right 2D panel (same palette; base contours without labels) ---
plot2D_right <- function() {
  op <- par(mar = c(3, 3, 1.2, 4.5))
  on.exit(par(op), add = TRUE)
  
  image2D(
    x = x_seq, y = y_seq, z = zmat,
    col = pal, zlim = zlim_shared,
    xlab = "Winter slope (°C/Day)",
    ylab = "Colony height (cm)",
    cex.lab = 1.05, cex.axis = 0.90,
    colkey = list(length = 0.9, width = 0.9)
  )
  
  # Base contour: no labels -> avoids gridGraphics warning
  graphics::contour(
    x = x_seq, y = y_seq, z = zmat,
    add = TRUE, drawlabels = FALSE,
    nlevels = 10, col = "grey30", lwd = 0.7
  )
}

# --- Convert base → grid and combine cleanly ---
g_left <- grid.grabExpr({
  grid.newpage()
  gridGraphics::grid.echo(plot3D_left)
})
g_right <- grid.grabExpr({
  grid.newpage()
  gridGraphics::grid.echo(plot2D_right)
})

gridExtra::grid.arrange(
  grobs = list(g_left, g_right),
  ncol = 2,
  widths = c(1.1, 1)   # nudge left wider so heights match nicely
)

### That will do for now. 

## Now looking at a contour surface plot for Winter average temperature & slope for different size classes:
# =========================
# Six 2D contour maps (2×3) + shared legend (PDF, correct colors)
# =========================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(tidybayes)
  library(viridis); library(plot3D)
})

# --- Config ---
gulf_rows <- c("gulf", "oman")   # top row, bottom row
grid_nx <- 40; grid_ny <- 40
pal <- viridis(120)
pal_fun <- grDevices::colorRampPalette(pal)   # for legend gradient
pdf_file <- "six_2D_surfaces_gulf_oman.pdf"

# --- Height percentiles (overall) ---
bh_q    <- stats::quantile(testex$Bheight, probs = c(0.10, 0.50, 0.90), na.rm = TRUE)
bh_vals <- as.numeric(bh_q)
bh_labs <- c("10th pct","50th pct","90th pct")

# --- Model centering refs ---
mu_winSlope <- mean(testex$winSlope, na.rm = TRUE)
mu_Bheight  <- mean(testex$Bheight,  na.rm = TRUE)

# --- Per-gulf ranges + z-score refs ---
gulf_stats <- testex %>%
  mutate(gulf = as.character(gulf)) %>%
  group_by(gulf) %>%
  summarise(
    min_win = min(winSlope, na.rm = TRUE),
    max_win = max(winSlope, na.rm = TRUE),
    min_W   = min(WtavR,   na.rm = TRUE),
    max_W   = max(WtavR,   na.rm = TRUE),
    mean_W  = mean(WtavR,  na.rm = TRUE),
    sd_W    = sd(WtavR,    na.rm = TRUE),
    .groups = "drop"
  )

get_gulf_row <- function(gs, g) {
  row <- gs %>% filter(.data$gulf == g)
  if (nrow(row) == 0) stop("Gulf not found: ", g)
  row[1, , drop = FALSE]
}

# --- One panel ---
panel_pred <- function(gulf_name, Bheight_fix) {
  gs  <- get_gulf_row(gulf_stats, gulf_name)
  sdW <- ifelse(is.na(gs$sd_W) || gs$sd_W == 0, 1e-9, gs$sd_W)
  
  x_seq <- seq(gs$min_win, gs$max_win, length.out = grid_nx)  # Winter slope
  y_seq <- seq(gs$min_W,   gs$max_W,   length.out = grid_ny)  # Mean winter temp
  
  nd <- tidyr::crossing(winSlope = x_seq, WtavR = y_seq) %>%
    mutate(
      gulf       = gulf_name,
      winSlope_c = winSlope - mu_winSlope,
      Bheight_c  = Bheight_fix - mu_Bheight,
      WtavR_zg   = (WtavR - gs$mean_W) / sdW
    )
  
  pred <- nd %>%
    tidybayes::add_epred_draws(final_main, re_formula = NA) %>%
    group_by(winSlope, WtavR) %>%
    summarise(epred = mean(.epred), .groups = "drop") %>%
    arrange(winSlope, WtavR)
  
  zmat <- pred %>%
    tidyr::pivot_wider(names_from = WtavR, values_from = epred) %>%
    arrange(winSlope) %>%
    select(-winSlope) %>%
    as.matrix()
  
  list(x_seq = x_seq, y_seq = y_seq, zmat = zmat)
}

# --- Build panels + shared z-limits ---
panels <- list()
for (g in gulf_rows) for (j in seq_along(bh_vals))
  panels[[paste(g, j, sep = "_")]] <- panel_pred(g, bh_vals[j])

zlim_shared <- range(unlist(lapply(panels, function(p) p$zmat)), na.rm = TRUE)

# =======================
# Draw to PDF
# =======================
grDevices::pdf(pdf_file, width = 11.5, height = 7.5)  # landscape, A4-ish

graphics::par(oma = c(0,0,0,0))
graphics::layout(
  rbind(c(1, 2, 3, 7),
        c(4, 5, 6, 7)),
  widths  = c(1, 1, 1, 0.35),   # legend column
  heights = c(1, 1)
)

# Tight panel margins
op <- graphics::par(
  mar  = c(3.0, 3.0, 2.0, 0.6),
  mgp  = c(2.0, 0.6, 0),
  xaxs = "i", yaxs = "i"
)

# ---- Six heatmaps ----
for (row_g in gulf_rows) {
  for (col_j in seq_along(bh_vals)) {
    p <- panels[[paste(row_g, col_j, sep = "_")]]
    x_seq <- p$x_seq; y_seq <- p$y_seq; zmat <- p$zmat
    
    plot3D::image2D(
      x = x_seq, y = y_seq, z = zmat,
      col = pal, zlim = zlim_shared,
      xlab = "Winter slope (°C/Day)",
      ylab = "Mean winter temp (°C)",
      cex.lab = 0.95, cex.axis = 0.85,
      colkey = FALSE
    )
    graphics::contour(
      x = x_seq, y = y_seq, z = zmat,
      add = TRUE, drawlabels = FALSE,
      nlevels = 10, col = "grey30", lwd = 0.7
    )
    
    graphics::mtext(paste0(row_g, " – Bheight ", bh_labs[col_j]),
                    side = 3, line = 0.2, cex = 0.9, font = 2)
    if (col_j == 1)
      graphics::mtext(row_g, side = 2, line = 2.6, cex = 0.9, font = 2)
  }
}

# ---- Legend cell (rightmost column) ----
graphics::par(mar = c(3, 0.4, 2, 4.2), xaxs = "i", yaxs = "i")
graphics::plot.new()
graphics::plot.window(xlim = c(0, 1), ylim = zlim_shared)

nbar  <- 500
yvals <- seq(zlim_shared[1], zlim_shared[2], length.out = nbar + 1)
cols  <- pal_fun(nbar)  # <-- MATCH length to bins; no recycling
# draw vertical strip
for (i in seq_len(nbar)) rect(0, yvals[i], 1, yvals[i + 1], col = cols[i], border = NA)

ticks <- pretty(zlim_shared, n = 6)
axis(4, at = ticks, labels = format(ticks, digits = 3), las = 1, cex.axis = 0.9)
mtext("Predicted extension (cm/yr)", side = 4, line = 2.6, cex = 0.9)

graphics::par(op)
grDevices::dev.off()

message("Saved PDF → ", normalizePath(pdf_file))


######This will do. 




