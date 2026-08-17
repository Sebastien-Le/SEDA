# Preference Mapping
# This file is a generated template, your changes will not be overwritten
cartoClass = if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "cartoClass",
  inherit = cartoBase,
  private = list(

    # ------------------------------------------------------------------
    # Initialisation and execution
    # ------------------------------------------------------------------

    .init = function() {

      self$results$instructions$setContent(
        "
<div style='
    font-family: inherit;
    margin: 8px 0;
    padding: 14px 18px;
    background-color: #F4F7FB;
    border: 1px solid #CBD8E8;
    border-left: 5px solid #6B9DE8;
    border-radius: 6px;
    color: #333333;
    line-height: 1.45;
'>

  <p style='margin: 0 0 10px 0; color: #355F98; font-size: 1.08em;'>
    <b>What you should know before building a preference map in jamovi</b>
  </p>

  <div style='border-top: 1px solid #CBD8E8; margin-bottom: 12px;'></div>

  <p style='margin: 0 0 9px 0;'>
    <b>Purpose.</b>
    Preference mapping links a two-dimensional product space to individual liking
    scores. Its objective is to identify regions of the product space associated
    with high predicted preference and, when requested, to reveal consumer
    segments with different liking patterns.
  </p>

  <p style='margin: 0 0 9px 0;'>
    <b>Data structure.</b>
    Each row must represent one stimulus. Select two quantitative coordinates
    (<i>X-axis</i> and <i>Y-axis</i>) describing the position of each stimulus and
    several quantitative <i>Liking Variables</i>, one variable per consumer.
    Optional stimulus labels are used only to identify products on the map.
  </p>

  <p style='margin: 0 0 9px 0;'>
    <b>Building the product space.</b>
    For an <i>external preference map</i>, the two coordinates usually come from
    a sensory product space. A common workflow is to run a PCA of the sensory
    attributes in MEDA, save two component coordinates, and use these coordinates
    here. For an <i>internal preference map</i>, the representation can instead be
    constructed from the liking data themselves before running this analysis.
    In every case, coordinates and liking scores must refer to the same stimuli
    in exactly the same row order.
  </p>

  <p style='margin: 0 0 9px 0;'>
    <b>How the preference map is built.</b>
    A response surface is fitted separately for each consumer using the two
    product coordinates. The predicted surface is standardized within consumer,
    converted into a preferred/non-preferred region using the selected threshold,
    and finally superimposed across consumers. The percentages on the map therefore
    describe the proportion of consumers for whom a location belongs to a predicted
    high-preference region.
  </p>

  <p style='margin: 0 0 9px 0;'>
    <b>Regression models.</b>
    The <i>Vector</i> model represents a linear preference direction. The
    <i>Circular</i> model adds a common quadratic curvature, the <i>Elliptical</i>
    model allows separate curvatures on the two axes, and the <i>Quadratic</i>
    model additionally includes the interaction between the axes. More flexible
    models require more products and should be interpreted cautiously with small
    product sets; in practice, around ten or more products is preferable for a
    fully quadratic response surface.
  </p>

  <p style='margin: 0 0 9px 0;'>
    <b>Preference threshold.</b>
    The threshold is expressed in standard deviations of each consumer's predicted
    response surface. With the default value <b>0</b>, a location is counted as
    preferred when its predicted liking is above that consumer's average predicted
    level. Positive values define more selective high-preference regions; negative
    values define broader regions. This is a preference criterion, not a statistical
    significance threshold.
  </p>

  <p style='margin: 0 0 9px 0;'>
    <b>Consumer classification.</b>
    When classification is requested, consumers are compared from their centered
    liking profiles and grouped by Ward hierarchical clustering. With
    <i>Number of clusters = -1</i>, SEDA chooses a cut from the largest upper-level
    jump in the hierarchy. Cluster-specific preference maps then use exactly the
    same product space, regression model and preference threshold as the overall map,
    making the segments directly comparable.
  </p>

  <p style='margin: 0 0 9px 0;'>
    <b>Interpretation.</b>
    The preference map identifies favourable <i>regions</i> of the chosen product
    space; it does not by itself identify the sensory formulation required to reach
    those regions. For an external preference map, interpret the PCA dimensions and
    the sensory attributes associated with them before translating a preferred zone
    into product-development actions. Very small consumer clusters should also be
    interpreted cautiously.
  </p>

  <p style='margin: 0;'>
    <b>Example.</b>
    Open the <b>senso_hedo_cocktail</b> dataset. In MEDA, run a PCA on the sensory
    attributes and save the first two component coordinates. Use these coordinates
    as the X- and Y-axes in SEDA and select the consumer liking variables. Start with
    a threshold of 0, then compare the overall map with the cluster-specific maps if
    consumer classification is requested.
  </p>

</div>"
      )

      self$results$step1Guide$setContent(
        "
<div style='
    margin: 6px 0 10px 0;
    padding: 9px 13px;
    background-color: #F4F7FB;
    border-left: 4px solid #6B9DE8;
    color: #333333;
    line-height: 1.4;
'>
  <b style='color: #355F98;'>Step 1 — Read the overall preference map.</b>
  The map superimposes the standardized predicted preference regions of all selected
  consumers. A high percentage indicates a region of the product space predicted to
  be favourable for many consumers. Product points show where the tested stimuli lie
  relative to these predicted regions.
</div>"
      )

      self$results$step2Guide$setContent(
        "
<div style='
    margin: 6px 0 10px 0;
    padding: 9px 13px;
    background-color: #F4F7FB;
    border-left: 4px solid #6B9DE8;
    color: #333333;
    line-height: 1.4;
'>
  <b style='color: #355F98;'>Step 2 — Identify consumer segments.</b>
  Consumers are clustered from the similarity of their centered liking profiles,
  independently of the two coordinates used to draw the preference map. The size
  and membership tables show the resulting segments; the optional dendrogram shows
  the underlying Ward hierarchy.
</div>"
      )

      self$results$step3Guide$setContent(
        "
<div style='
    margin: 6px 0 10px 0;
    padding: 9px 13px;
    background-color: #F4F7FB;
    border-left: 4px solid #6B9DE8;
    color: #333333;
    line-height: 1.4;
'>
  <b style='color: #355F98;'>Step 3 — Compare segment-specific preference regions.</b>
  Each map is recomputed using only the consumers assigned to that cluster, while
  keeping the same product coordinates, regression model and preference threshold.
  Differences between maps therefore reveal genuine heterogeneity in predicted
  preference patterns rather than changes in the representation space.
</div>"
      )
    },

    .run = function() {
      if (is.null(self$options$coox) || is.null(self$options$cooy) || is.null(self$options$hedo))
        return()

      if (length(self$options$hedo) < 2L) {
        jmvcore::reject("At least two Liking Variables are required to build a preference map")
        return()
      }

      data <- tryCatch(
        private$.buildData(),
        error = function(e) {
          jmvcore::reject(e$message)
          NULL
        }
      )
      if (is.null(data)) return()

      prepared <- tryCatch(
        private$.prepareData(data),
        error = function(e) {
          jmvcore::reject(e$message)
          NULL
        }
      )
      if (is.null(prepared)) return()

      overall_state <- list(Mat = prepared$Mat, MatH = prepared$MatH, title = "Overall Preference Map")
      self$results$plotcarto$setState(overall_state)

      classif <- NULL
      if (isTRUE(self$options$classify)) {
        classif <- tryCatch(
          private$.classifyConsumers(prepared$MatH),
          error = function(e) {
            jmvcore::reject(paste("Consumer classification failed:", e$message))
            NULL
          }
        )
        if (is.null(classif)) return()

        private$.populateClusterTables(classif)

        if (isTRUE(self$options$graphdendro))
          self$results$dendrogram$setState(classif)

        if (isTRUE(self$options$graphgroups)) {
          for (g in seq_len(classif$k)) {
            keep <- classif$cluster == g
            nm <- paste0("Cluster ", g, " (n = ", sum(keep), ")")
            item <- self$results$clusterMaps$addItem(key = nm)
            item$setTitle(nm)
            item$setState(list(
              Mat = prepared$Mat,
              MatH = prepared$MatH[, keep, drop = FALSE],
              title = paste0("Preference Map — Cluster ", g)
            ))
          }
        }
      }

      if (isTRUE(self$options$showCode))
        self$results$code$setContent(private$.code(prepared, classif))
    },

    # ------------------------------------------------------------------
    # Plot renderers
    # ------------------------------------------------------------------

    .plotcartograph = function(image, ...) {
      state <- image$state
      if (is.null(state)) return()
      private$.drawPreferenceMap(state$Mat, state$MatH, state$title)
    },

    .plotClusterMap = function(image, ...) {
      state <- image$state
      if (is.null(state)) return()
      private$.drawPreferenceMap(state$Mat, state$MatH, state$title)
    },

    .plotDendrogram = function(image, ...) {
      state <- image$state
      if (is.null(state) || is.null(state$hc)) return()

      graphics::plot(
        state$hc,
        labels = colnames(state$MatH),
        main = "Consumer Cluster Dendrogram",
        xlab = "Consumers",
        sub = "",
        hang = -1
      )
      TRUE
    },

    .drawPreferenceMap = function(Mat, MatH, title) {
      regtype_gui <- private$.regtype()
      level_gui <- as.numeric(self$options$level)
      colbelow_gui <- private$.getColorFromName(self$options$colbelow, "#4575B4")
      colabove_gui <- private$.getColorFromName(self$options$colabove, "#D73027")

      depasse_df <- tryCatch(
        private$.carto_jamovi(
          Mat,
          MatH,
          level = level_gui,
          regmod = regtype_gui,
          resolution = 150
        ),
        error = function(e) {
          jmvcore::reject(paste("Preference mapping failed:", e$message))
          NULL
        }
      )
      if (is.null(depasse_df) || nrow(depasse_df) == 0L) return()

      palette <- grDevices::colorRampPalette(c(colbelow_gui, "white", colabove_gui))(101)
      contour_levels <- c(20, 30, 40, 50, 60, 70, 80, 90, 95)

      Mat_labels <- as.data.frame(Mat)
      Mat_labels$name <- rownames(Mat_labels)
      colnames(Mat_labels)[1:2] <- c("Var1", "Var2")

      y_range <- range(depasse_df$Var2, na.rm = TRUE)
      offset <- if (diff(y_range) == 0) 0.03 else 0.03 * diff(y_range)

      plot <- ggplot2::ggplot(depasse_df, ggplot2::aes(x = Var1, y = Var2, fill = value)) +
        ggplot2::geom_raster(interpolate = TRUE) +
        ggplot2::geom_contour(
          ggplot2::aes(z = value),
          breaks = contour_levels,
          colour = "grey30",
          size = 0.35,
          show.legend = FALSE
        ) +
        ggplot2::scale_fill_gradientn(
          colours = palette,
          limits = c(0, 100),
          breaks = c(0, 20, 40, 60, 80, 100),
          name = "% of consumers"
        ) +
        ggplot2::coord_equal() +
        ggplot2::labs(
          title = title,
          subtitle = paste0("Preference threshold = ", format(level_gui, trim = TRUE), " SD"),
          x = colnames(Mat)[1],
          y = colnames(Mat)[2]
        ) +
        ggplot2::theme_light() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = ggplot2::element_text(hjust = 0.5),
          legend.position = "right"
        ) +
        ggplot2::geom_point(
          data = Mat_labels,
          ggplot2::aes(x = Var1, y = Var2),
          inherit.aes = FALSE,
          shape = 15,
          size = 2
        ) +
        ggplot2::geom_text(
          data = Mat_labels,
          ggplot2::aes(x = Var1, y = Var2 + offset, label = name),
          inherit.aes = FALSE,
          size = 3
        )

      print(plot)
      TRUE
    },

    # ------------------------------------------------------------------
    # Data preparation and validation
    # ------------------------------------------------------------------

    .buildData = function() {
      coox <- self$options$coox
      cooy <- self$options$cooy
      hedo <- self$options$hedo
      label <- self$options$individus

      if (identical(coox, cooy))
        stop("X-axis and Y-axis must be two different variables")

      if (anyDuplicated(hedo))
        stop("Each Liking Variable must be selected only once")

      if (coox %in% hedo || cooy %in% hedo)
        stop("Coordinate variables cannot also be used as Liking Variables")

      if (!is.null(label) && (label %in% c(coox, cooy, hedo)))
        stop("Stimuli Labels must be different from the coordinate and liking variables")

      selected <- c(coox, cooy, hedo)
      data <- as.data.frame(self$data[, selected, drop = FALSE], check.names = FALSE)

      if (!all(vapply(data, is.numeric, logical(1))))
        stop("X-axis, Y-axis and all Liking Variables must be numeric")

      if (!is.null(label)) {
        ids <- as.character(self$data[[label]])
        if (any(is.na(ids) | trimws(ids) == ""))
          stop("Stimuli Labels contain missing or empty values")
        if (anyDuplicated(ids))
          stop("Stimuli Labels must be unique")
        rownames(data) <- ids
      } else {
        rownames(data) <- as.character(seq_len(nrow(data)))
      }

      data
    },

    .prepareData = function(data) {
      Mat <- data[, 1:2, drop = FALSE]
      MatH <- data[, 3:ncol(data), drop = FALSE]

      if (nrow(Mat) < 4L)
        stop("At least four stimuli are required for preference mapping")

      if (any(!is.finite(as.matrix(Mat))))
        stop("X-axis and Y-axis must contain only finite, non-missing values")

      if (stats::sd(Mat[[1]]) <= sqrt(.Machine$double.eps) ||
          stats::sd(Mat[[2]]) <= sqrt(.Machine$double.eps))
        stop("Both coordinate variables must vary across stimuli")

      minimum_products <- c(`1` = 7L, `2` = 4L, `3` = 5L, `4` = 6L)
      regtype <- as.character(private$.regtype())
      if (nrow(Mat) < minimum_products[[regtype]]) {
        model_name <- c(`1` = "Quadratic", `2` = "Vector", `3` = "Circular", `4` = "Elliptical")[[regtype]]
        stop(paste0(
          model_name, " regression requires at least ", minimum_products[[regtype]],
          " stimuli for a non-saturated fit"
        ))
      }

      rawH <- as.matrix(MatH)
      storage.mode(rawH) <- "double"

      all_missing_consumers <- colnames(MatH)[colSums(!is.na(rawH)) == 0L]
      if (length(all_missing_consumers) > 0L)
        stop(paste("The following Liking Variables contain only missing values:", paste(all_missing_consumers, collapse = ", ")))

      all_missing_products <- rownames(MatH)[rowSums(!is.na(rawH)) == 0L]
      if (length(all_missing_products) > 0L)
        stop(paste("The following stimuli have no liking scores:", paste(all_missing_products, collapse = ", ")))

      MatH <- private$.imputeLiking(MatH)

      if (any(!is.finite(as.matrix(MatH))))
        stop("Liking data could not be converted to finite values after missing-value imputation")

      sds <- vapply(MatH, stats::sd, numeric(1), na.rm = TRUE)
      flat <- names(sds)[!is.finite(sds) | sds <= sqrt(.Machine$double.eps)]
      if (length(flat) > 0L)
        stop(paste(
          "The following consumers have no variation in liking and cannot be used for preference mapping:",
          paste(flat, collapse = ", ")
        ))

      # The geometry of the selected product space must support the chosen model.
      X <- private$.designMatrix(Mat[[1]], Mat[[2]], private$.regtype())
      if (qr(X)$rank < ncol(X))
        stop("The selected coordinates do not contain enough independent information for the chosen Regression Model; use a simpler model or a richer product space")

      if (isTRUE(self$options$classify) && ncol(MatH) < 3L)
        stop("At least three consumers are required for consumer classification")

      list(Mat = Mat, MatH = MatH)
    },

    .imputeLiking = function(MatH) {
      MatH <- as.data.frame(MatH, check.names = FALSE)
      if (!anyNA(MatH)) return(MatH)

      M <- as.matrix(MatH)
      row_mean <- rowMeans(M, na.rm = TRUE)
      col_mean <- colMeans(M, na.rm = TRUE)
      grand_mean <- mean(M, na.rm = TRUE)

      idx <- which(is.na(M), arr.ind = TRUE)
      if (nrow(idx) > 0L) {
        for (i in seq_len(nrow(idx))) {
          r <- idx[i, 1]
          c <- idx[i, 2]
          M[r, c] <- row_mean[r] + col_mean[c] - grand_mean
        }
      }

      out <- as.data.frame(M, check.names = FALSE)
      rownames(out) <- rownames(MatH)
      colnames(out) <- colnames(MatH)
      out
    },

    # ------------------------------------------------------------------
    # Preference response surfaces
    # ------------------------------------------------------------------

    .regtype = function() {
      x <- suppressWarnings(as.integer(as.character(self$options$regtype)))
      if (is.na(x) || !(x %in% 1:4)) x <- 1L
      x
    },

    .designMatrix = function(x1, x2, regmod, grid = FALSE, center = NULL) {
      if (is.null(center))
        center <- c(mean(x1), mean(x2))

      x1c <- x1 - center[1]
      x2c <- x2 - center[2]

      if (regmod == 1L)
        return(cbind(`(Intercept)` = 1, x = x1c, y = x2c, x2 = x1c^2, y2 = x2c^2, xy = x1c * x2c))
      if (regmod == 2L)
        return(cbind(`(Intercept)` = 1, x = x1c, y = x2c))
      if (regmod == 3L)
        return(cbind(`(Intercept)` = 1, x = x1c, y = x2c, r2 = x1c^2 + x2c^2))
      cbind(`(Intercept)` = 1, x = x1c, y = x2c, x2 = x1c^2, y2 = x2c^2)
    },

    .carto_jamovi = function(Mat, MatH, level = 0, regmod = 1, resolution = 150) {
      if (!is.data.frame(Mat)) Mat <- as.data.frame(Mat)
      if (!is.data.frame(MatH)) MatH <- as.data.frame(MatH)
      if (ncol(Mat) != 2L) stop("Mat must contain exactly two coordinate variables")
      if (ncol(MatH) < 1L) stop("MatH must contain at least one liking variable")

      x1 <- Mat[[1]]
      x2 <- Mat[[2]]
      center <- c(mean(x1), mean(x2))
      X <- private$.designMatrix(x1, x2, regmod, center = center)

      span1 <- diff(range(x1))
      span2 <- diff(range(x2))
      step <- max(span1, span2) / resolution
      if (!is.finite(step) || step <= 0) stop("The product space has no usable extent")

      f1 <- seq(min(x1) - 0.05 * span1, max(x1) + 0.05 * span1, by = step)
      f2 <- seq(min(x2) - 0.05 * span2, max(x2) + 0.05 * span2, by = step)
      if (length(f1) < 2L || length(f2) < 2L) stop("The plotting grid could not be constructed")

      grid <- expand.grid(x = f1, y = f2)
      G <- private$.designMatrix(grid$x, grid$y, regmod, center = center)

      liking_centered <- scale(as.matrix(MatH), center = TRUE, scale = FALSE)
      exceed <- numeric(nrow(grid))

      for (i in seq_len(ncol(liking_centered))) {
        fit <- stats::lm.fit(x = X, y = liking_centered[, i])
        beta <- fit$coefficients
        beta[!is.finite(beta)] <- 0
        predicted <- as.vector(G %*% beta)

        sdp <- stats::sd(predicted)
        if (is.finite(sdp) && sdp > sqrt(.Machine$double.eps))
          predicted <- (predicted - mean(predicted)) / sdp
        else
          predicted[] <- 0

        exceed <- exceed + as.numeric(predicted > level)
      }

      data.frame(
        Var1 = grid$x,
        Var2 = grid$y,
        value = 100 * exceed / ncol(liking_centered)
      )
    },

    # ------------------------------------------------------------------
    # Consumer classification
    # ------------------------------------------------------------------

    .classifyConsumers = function(MatH) {
      centered <- scale(as.matrix(MatH), center = TRUE, scale = FALSE)
      hc <- stats::hclust(stats::dist(t(centered)), method = "ward.D2")
      ncons <- ncol(MatH)

      requested <- suppressWarnings(as.integer(self$options$nbclust))
      if (is.na(requested)) requested <- -1L

      if (requested == -1L) {
        jumps <- diff(hc$height)
        if (length(jumps) == 0L)
          k <- 2L
        else
          k <- which.max(rev(jumps)) + 1L
        k <- max(2L, min(k, ncons - 1L))
      } else {
        if (requested < 2L)
          stop("Number of clusters must be -1 (automatic) or an integer of at least 2")
        if (requested >= ncons)
          stop("Number of clusters must be smaller than the number of consumers")
        k <- requested
      }

      cluster <- stats::cutree(hc, k = k)
      names(cluster) <- colnames(MatH)

      list(hc = hc, cluster = cluster, k = k, MatH = MatH)
    },

    .populateClusterTables = function(classif) {
      membership <- self$results$clusterMembership
      sizes <- self$results$clusterSizes

      consumers <- names(classif$cluster)
      for (i in seq_along(consumers)) {
        membership$addRow(
          rowKey = i,
          values = list(
            consumer = consumers[i],
            cluster = as.integer(classif$cluster[i])
          )
        )
      }

      tab <- table(factor(classif$cluster, levels = seq_len(classif$k)))
      for (g in seq_len(classif$k)) {
        sizes$addRow(
          rowKey = g,
          values = list(
            cluster = g,
            n = as.integer(tab[g]),
            percent = 100 * as.integer(tab[g]) / length(classif$cluster)
          )
        )
      }
    },

    # ------------------------------------------------------------------
    # Formatting helpers
    # ------------------------------------------------------------------

    .getColorFromName = function(name, default = "#D73027") {
      palette_map <- c(
        "red" = "#D73027",
        "blue" = "#4575B4",
        "green" = "#1A9850",
        "orange" = "#F46D43",
        "purple" = "#984EA3",
        "brown" = "#A65628",
        "darkgrey" = "#4D4D4D",
        "black" = "#000000"
      )

      name <- tolower(as.character(name))
      if (length(name) == 0L || is.na(name) || !(name %in% names(palette_map)))
        return(default)
      palette_map[[name]]
    },

    .rQuote = function(x) {
      vapply(x, function(z) encodeString(z, quote = '"'), character(1))
    },

    # ------------------------------------------------------------------
    # Reproducible R code
    # ------------------------------------------------------------------

    .code = function(prepared, classif = NULL) {
      q <- private$.rQuote
      vars <- c(self$options$coox, self$options$cooy, self$options$hedo)
      vars_txt <- paste(q(vars), collapse = ", ")
      hedo_txt <- paste(q(self$options$hedo), collapse = ", ")
      label <- self$options$individus
      level <- format(as.numeric(self$options$level), scientific = FALSE, trim = TRUE)
      regmod <- private$.regtype()
      colbelow <- private$.getColorFromName(self$options$colbelow, "#4575B4")
      colabove <- private$.getColorFromName(self$options$colabove, "#D73027")

      label_code <- if (!is.null(label)) {
        paste0(
          "stimulus_labels <- as.character(data[[", q(label), "]])\n",
          "if (any(is.na(stimulus_labels) | trimws(stimulus_labels) == \"\")) stop(\"Stimuli Labels contain missing or empty values\")\n",
          "if (anyDuplicated(stimulus_labels)) stop(\"Stimuli Labels must be unique\")\n",
          "rownames(data_CARTO) <- stimulus_labels\n"
        )
      } else {
        "rownames(data_CARTO) <- as.character(seq_len(nrow(data_CARTO)))\n"
      }

      code <- paste0(
"library(SensoMineR)\n\n",
"# This script can be pasted directly into the jamovi Rj Editor.\n",
"# The response-surface calculation follows the logic of SensoMineR::carto().\n\n",
"data_CARTO <- data[, c(", vars_txt, "), drop = FALSE]\n",
label_code,
"Mat <- data_CARTO[, c(", q(self$options$coox), ", ", q(self$options$cooy), "), drop = FALSE]\n",
"MatH <- data_CARTO[, c(", hedo_txt, "), drop = FALSE]\n\n",
"level_CARTO <- ", level, "\n",
"regmod_CARTO <- ", regmod, "\n",
"resolution_CARTO <- 150\n\n",
".impute_liking <- function(MatH) {\n",
"  MatH <- as.data.frame(MatH, check.names = FALSE)\n",
"  if (!anyNA(MatH)) return(MatH)\n",
"  M <- as.matrix(MatH)\n",
"  if (any(rowSums(!is.na(M)) == 0) || any(colSums(!is.na(M)) == 0))\n",
"    stop(\"Rows or consumers containing only missing liking values cannot be imputed\")\n",
"  rm <- rowMeans(M, na.rm = TRUE); cm <- colMeans(M, na.rm = TRUE); gm <- mean(M, na.rm = TRUE)\n",
"  idx <- which(is.na(M), arr.ind = TRUE)\n",
"  if (nrow(idx) > 0) for (i in seq_len(nrow(idx))) M[idx[i,1], idx[i,2]] <- rm[idx[i,1]] + cm[idx[i,2]] - gm\n",
"  out <- as.data.frame(M, check.names = FALSE); rownames(out) <- rownames(MatH); colnames(out) <- colnames(MatH); out\n",
"}\n\n",
".design_CARTO <- function(x, y, regmod, center) {\n",
"  xc <- x - center[1]; yc <- y - center[2]\n",
"  if (regmod == 1) return(cbind(1, xc, yc, xc^2, yc^2, xc*yc))\n",
"  if (regmod == 2) return(cbind(1, xc, yc))\n",
"  if (regmod == 3) return(cbind(1, xc, yc, xc^2 + yc^2))\n",
"  cbind(1, xc, yc, xc^2, yc^2)\n",
"}\n\n",
".surface_CARTO <- function(Mat, MatH, level = 0, regmod = 1, resolution = 150) {\n",
"  MatH <- .impute_liking(MatH)\n",
"  x <- Mat[[1]]; y <- Mat[[2]]; center <- c(mean(x), mean(y))\n",
"  X <- .design_CARTO(x, y, regmod, center)\n",
"  if (qr(X)$rank < ncol(X)) stop(\"The selected product space does not support this regression model\")\n",
"  sx <- diff(range(x)); sy <- diff(range(y)); step <- max(sx, sy) / resolution\n",
"  gx <- seq(min(x) - .05*sx, max(x) + .05*sx, by = step)\n",
"  gy <- seq(min(y) - .05*sy, max(y) + .05*sy, by = step)\n",
"  grid <- expand.grid(x = gx, y = gy); G <- .design_CARTO(grid$x, grid$y, regmod, center)\n",
"  H <- scale(as.matrix(MatH), center = TRUE, scale = FALSE); exceed <- numeric(nrow(grid))\n",
"  for (i in seq_len(ncol(H))) {\n",
"    b <- lm.fit(X, H[,i])$coefficients; b[!is.finite(b)] <- 0\n",
"    pred <- as.vector(G %*% b); s <- sd(pred)\n",
"    if (is.finite(s) && s > sqrt(.Machine$double.eps)) pred <- (pred - mean(pred))/s else pred[] <- 0\n",
"    exceed <- exceed + as.numeric(pred > level)\n",
"  }\n",
"  data.frame(Var1 = grid$x, Var2 = grid$y, value = 100*exceed/ncol(H))\n",
"}\n\n",
".plot_CARTO <- function(surface, Mat, title, level, low = ", q(colbelow), ", high = ", q(colabove), ") {\n",
"  gx <- sort(unique(surface$Var1)); gy <- sort(unique(surface$Var2))\n",
"  z <- matrix(surface$value, nrow = length(gx), ncol = length(gy))\n",
"  palfun <- grDevices::colorRampPalette(c(low, \"white\", high))\n",
"  contour_levels <- c(20, 30, 40, 50, 60, 70, 80, 90, 95)\n",
"  yr <- range(gy); off <- if (diff(yr) == 0) .03 else .03 * diff(yr)\n",
"  graphics::filled.contour(\n",
"    x = gx, y = gy, z = z,\n",
"    levels = seq(0, 100, by = 10),\n",
"    color.palette = palfun,\n",
"    plot.title = graphics::title(main = paste0(title, \"\\nPreference threshold = \", level, \" SD\"), xlab = names(Mat)[1], ylab = names(Mat)[2]),\n",
"    key.title = graphics::title(main = \"%\", cex.main = .8),\n",
"    plot.axes = {\n",
"      graphics::axis(1); graphics::axis(2)\n",
"      graphics::contour(gx, gy, z, levels = contour_levels, add = TRUE, drawlabels = TRUE, col = \"grey30\", lwd = .7, cex = .7)\n",
"      graphics::points(Mat[[1]], Mat[[2]], pch = 15)\n",
"      graphics::text(Mat[[1]], Mat[[2]] + off, labels = rownames(Mat), cex = .75)\n",
"    }\n",
"  )\n",
"}\n\n",
"MatH <- .impute_liking(MatH)\n",
"surface_overall <- .surface_CARTO(Mat, MatH, level_CARTO, regmod_CARTO, resolution_CARTO)\n",
".plot_CARTO(surface_overall, Mat, \"Overall Preference Map\", level_CARTO)\n"
      )

      if (!isTRUE(self$options$classify) || is.null(classif))
        return(code)

      requested <- as.integer(self$options$nbclust)
      k_line <- if (requested == -1L) {
        "jumps <- diff(hc_CARTO$height)\nk_CARTO <- if (length(jumps) == 0) 2L else which.max(rev(jumps)) + 1L\nk_CARTO <- max(2L, min(k_CARTO, ncol(MatH) - 1L))\n"
      } else {
        paste0("k_CARTO <- ", requested, "L\n")
      }

      code <- paste0(
        code,
        "\n# ------------------------------------------------------------------\n",
        "# Consumer classification from centered liking profiles\n",
        "# ------------------------------------------------------------------\n",
        "H_centered <- scale(as.matrix(MatH), center = TRUE, scale = FALSE)\n",
        "hc_CARTO <- hclust(dist(t(H_centered)), method = \"ward.D2\")\n",
        k_line,
        "consumer_cluster <- cutree(hc_CARTO, k = k_CARTO)\n",
        "cluster_membership <- data.frame(Consumer = names(consumer_cluster), Cluster = unname(consumer_cluster))\n",
        "cluster_sizes <- data.frame(Cluster = seq_len(k_CARTO), N = as.integer(table(factor(consumer_cluster, levels=seq_len(k_CARTO)))))\n",
        "cluster_sizes$Percent <- 100 * cluster_sizes$N / length(consumer_cluster)\n",
        "cluster_sizes\n",
        "cluster_membership\n"
      )

      if (isTRUE(self$options$graphdendro)) {
        code <- paste0(
          code,
          "\nplot(hc_CARTO, labels = colnames(MatH), main = \"Consumer Cluster Dendrogram\", xlab = \"Consumers\", sub = \"\", hang = -1)\n"
        )
      }

      if (isTRUE(self$options$graphgroups)) {
        for (g in seq_len(classif$k)) {
          code <- paste0(
            code,
            "\nMatH_cluster_", g, " <- MatH[, consumer_cluster == ", g, ", drop = FALSE]\n",
            "surface_cluster_", g, " <- .surface_CARTO(Mat, MatH_cluster_", g, ", level_CARTO, regmod_CARTO, resolution_CARTO)\n",
            ".plot_CARTO(surface_cluster_", g, ", Mat, \"Preference Map — Cluster ", g, "\", level_CARTO)\n"
          )
        }
      }

      code
    }
  )
)
