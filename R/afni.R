# Write data.frame to a file
outDF <- function(DF, fl) {
  cat(capture.output(DF), file = paste0(fl, '.txt'), sep = '\n', append = TRUE)
}

# Compute P+ (positive posterior probability)
cnt <- function(x, ns) {
  return(sum(x > 0) / ns)
}

# Extract posterior samples at ROIs for a specific term
# Modified psROI function that accepts ROI variable name
psROI <- function(aa, bb, tm, roi_var = "roi") {
  ps <- apply(bb[[roi_var]][,, tm], 2, '+', aa[, tm])
  return(ps)
}

# Summarize ROI results with mean, SD, P+, and quantiles
sumROI <- function(R0, ns, nd) {
  hubs <- data.frame(cbind(
    apply(R0, 2, mean),
    apply(R0, 2, sd),
    apply(R0, 2, cnt, ns),
    t(apply(R0, 2, quantile, probs = c(0.025, 0.05, 0.5, 0.95, 0.975)))
  ))
  names(hubs) <- c('mean', 'SD', 'P+', '2.5%', '5%', '50%', '95%', '97.5%')
  return(round(hubs, nd))
}

# Add transparency to colors
addTrans <- function(color, trans) {
  if (
    length(color) != length(trans) & !any(c(length(color), length(trans)) == 1)
  ) {
    stop("Vector lengths not correct")
  }
  if (length(color) == 1 & length(trans) > 1) {
    color <- rep(color, length(trans))
  }
  if (length(trans) == 1 & length(color) > 1) {
    trans <- rep(trans, length(color))
  }

  num2hex <- function(x) {
    hex <- unlist(strsplit("0123456789ABCDEF", split = ""))
    return(paste(hex[(x - x %% 16) / 16 + 1], hex[x %% 16 + 1], sep = ""))
  }
  rgb <- rbind(col2rgb(color), trans)
  res <- paste(
    "#",
    apply(apply(rgb, 2, num2hex), 2, paste, collapse = ""),
    sep = ""
  )
  return(res)
}

# Fisher transformation for correlation values
fisher <- function(r) {
  ifelse(
    abs(r) < .995,
    0.5 * (log(1 + r) - log(1 - r)),
    stop('Are you sure that you have correlation values so close to 1 or -1?')
  )
}

# Plot posterior density plots
plotPDP <- function(fn, ps, nR, nr, nc, w = 8) {
  h <- ceiling(8 * nr / (nc * 2))
  png(paste0(fn, "_PDP.png"), width = w, height = h, units = 'in', res = 300)
  par(mfrow = c(nr, nc), mar = c(2.5, 0, 0.0, 0.8), oma = c(0, 0, 0, 0))
  qq <- apply(ps, 2, quantile, c(0.025, 0.05, 0.1, 0.9, 0.95, 0.975))

  for (ii in 1:nR) {
    dens <- density(ps[, ii])
    plot(dens, main = '', axes = F, bty = "n", xlab = '', ylab = '')
    axis(side = 1)
    abline(v = 0, col = 'blue')
    mtext(dimnames(ps)[[2]][ii], side = 3, line = -2, las = 0)

    x1 <- min(which(dens$x >= qq[6, ii]))
    x2 <- max(which(dens$x < 4e10))
    x3 <- min(which(dens$x >= -4e10))
    x4 <- max(which(dens$x < qq[1, ii]))
    x5 <- min(which(dens$x >= qq[5, ii]))
    x6 <- max(which(dens$x < qq[2, ii]))
    x7 <- min(which(dens$x >= qq[4, ii]))
    x8 <- max(which(dens$x < qq[3, ii]))

    with(
      dens,
      polygon(
        x = c(x[c(x1, x1:x2, x2)]),
        y = c(0, y[x1:x2], 0),
        col = addTrans('green', 175)
      )
    )
    with(
      dens,
      polygon(
        x = c(x[c(x3, x3:x4, x4)]),
        y = c(0, y[x3:x4], 0),
        col = addTrans('green', 175)
      )
    )
    with(
      dens,
      polygon(
        x = c(x[c(x5, x5:x1, x1)]),
        y = c(0, y[x5:x1], 0),
        col = addTrans('orange', 150)
      )
    )
    with(
      dens,
      polygon(
        x = c(x[c(x4, x4:x6, x6)]),
        y = c(0, y[x4:x6], 0),
        col = addTrans('orange', 150)
      )
    )
    with(
      dens,
      polygon(
        x = c(x[c(x7, x7:x5, x5)]),
        y = c(0, y[x7:x5], 0),
        col = addTrans('gray', 125)
      )
    )
    with(
      dens,
      polygon(
        x = c(x[c(x6, x6:x8, x8)]),
        y = c(0, y[x6:x8], 0),
        col = addTrans('gray', 125)
      )
    )

    if (qq[1, ii] > 0 | qq[6, ii] < 0) {
      rect(
        range(dens$x)[1],
        range(dens$y)[1],
        range(dens$x)[2],
        range(dens$y)[2],
        lty = 'solid',
        border = addTrans('green', 200),
        lwd = 3
      )
    }
    if ((qq[1, ii] < 0 & qq[2, ii] > 0) | (qq[5, ii] < 0 & qq[6, ii] > 0)) {
      rect(
        range(dens$x)[1],
        range(dens$y)[1],
        range(dens$x)[2],
        range(dens$y)[2],
        lty = 'solid',
        border = addTrans('orange', 150),
        lwd = 3
      )
    }
    if ((qq[2, ii] < 0 & qq[3, ii] > 0) | (qq[4, ii] < 0 & qq[5, ii] > 0)) {
      rect(
        range(dens$x)[1],
        range(dens$y)[1],
        range(dens$x)[2],
        range(dens$y)[2],
        lty = 'solid',
        border = addTrans('gray', 100),
        lwd = 3
      )
    }
  }
  dev.off()
}

# Create ridge plots for posterior distributions
ridge <- function(dat, xlim, labx, wi, hi) {
  library(data.table)
  library(ggplot2)
  library(ggridges)
  library(dplyr)
  library(tidyr)
  library(scales)

  data <- data.frame(dat)
  data$X <- NULL
  nobj <- dim(data)[1]

  rois <- dimnames(dat)[[2]]
  colnames(data) <- rois
  data_stats <- data.frame(1:length(rois))

  data_stats$ROI <- rois
  data_stats$mean <- colMeans(data)
  data_stats$P <- colSums(data > 0) / nobj
  data_stats$Pn <- ifelse(data_stats$P < .5, 1 - data_stats$P, data_stats$P)
  data_stats <- data_stats[order(data_stats$mean), ]

  data_trans <- as.data.frame(t(as.matrix(data)))
  data_trans <- tibble::rownames_to_column(data_trans, "ROI")
  data_trans$X <- 1:nrow(data_trans)

  data_merge <- merge(data_stats, data_trans, by = "ROI")
  data_merge <- data_merge[order(data_merge$X), ]

  data_long <- reshape2::melt(data_trans, id = c("ROI", "X"))
  data_long <- data_long[order(data_long$X), ]

  data_long$mean <- rep(data_merge$mean, each = nobj)
  data_long$P <- rep(data_merge$P, each = nobj)
  data_long$Pn <- rep(data_merge$Pn, each = nobj)

  y.axis.labs <- data_stats$ROI
  sec.y.axis.labs <- round(data_stats$P, 2)

  gradient.colors <- c("blue", "cyan", "gray", "gray", "yellow", "#C9182B")

  ggplot(
    data_long,
    aes(x = value, y = as.numeric(reorder(ROI, mean)), fill = P, group = ROI)
  ) +
    guides(
      fill = guide_colorbar(
        barwidth = 1,
        barheight = 20,
        nbin = 100,
        frame.colour = "black",
        frame.linewidth = 1.5,
        ticks.colour = "black",
        title.position = "top",
        title.hjust = 0.5
      )
    ) +
    stat_density_ridges(
      quantile_lines = TRUE,
      quantiles = 2,
      size = .6,
      alpha = .8,
      scale = 2,
      color = "black"
    ) +
    geom_vline(
      xintercept = 0,
      linetype = "solid",
      alpha = 1,
      size = 1,
      color = "green"
    ) +
    scale_fill_gradientn(
      colors = gradient.colors,
      limits = c(0, 1),
      name = "P+",
      breaks = c(0, 0.05, 0.1, 0.9, 0.95, 1),
      expand = expansion(0),
      labels = c("0", "0.05", "0.1", "0.9", "0.95", "1")
    ) +
    scale_y_continuous(
      breaks = 1:length(rois),
      labels = y.axis.labs,
      sec.axis = sec_axis(~., breaks = 1:length(rois), labels = sec.y.axis.labs)
    ) +
    theme_ridges(font_size = 15, grid = TRUE, center_axis_labels = TRUE) +
    annotate("text", x = 0.06, y = 1.5, label = labx, size = 5.5) +
    theme(
      axis.text.y.left = element_text(size = 15),
      axis.text.y.right = element_text(size = 15),
      axis.text.x = element_text(size = 15),
      legend.title.align = 5,
      legend.title = element_text(size = 15)
    ) +
    labs(x = NULL, y = NULL) +
    scale_x_continuous(limits = xlim) +
    xlab(labx)

  ggsave(file = paste0(labx, "_ridge.png"), width = wi, height = hi, dpi = 120)
}
