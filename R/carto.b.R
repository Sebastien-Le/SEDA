#' Correlation Analysis
#' @import dplyr
#' @import corrplot
#' @export
# This file is a generated template, your changes will not be overwritten
cartoClass = if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "cartoClass",
  inherit = cartoBase,
  private = list(
    
    #---------------------------------------------
    #### Init + run functions ----
    
    .init = function() {
      if (is.null(self$data) || is.null(self$options$hedo)) {
        if (isTRUE(self$options$tuto))
          self$results$instructions$setVisible(visible = TRUE)
      }
      
      self$results$instructions$setContent(
        "<html>
        <head></head>
        <body>
        <div class='justified-text'>
        <p><b>What you need to know before analysing hedonic data in jamovi</b></p>
        <p>______________________________________________________________________________</p>
        <p> In the SEDA module, to obtain a preference map, you need two elements: a two-dimensional representation space, on the one hand, hedonic scores, on the other hand.</p>
        <p> For the external preference map, open the <b>senso_hedo_cocktail</b> dataset. With the <b>MEDA module</b>, run a PCA on the sensory attributes (from color intensity to thickness) and make sure to save the 2 first components. Once you have
        saved the representation space, you can now use it to represent your hedonic scores.</p>
        <p> Come back to the preference mapping analysis. The X-axis and Y-axis of your representation space are the 2 first components that
        you have previously saved. The liking variables are the columns from O to DJ.</p>
        <p> For the internal preference map, open the <b>senso_hedo_cocktail</b> dataset. With the <b>MEDA module</b>, run a PCA on the liking scores (columns from O to DJ) and make sure to save the 2 first components. Once you have
        saved the representation space, you can now use it to represent your hedonic scores.</p>
        <p> Come back to the preference mapping analysis. The X-axis and Y-axis of your representation space are the 2 first components that
        you have previously saved. The liking variables are the columns from O to DJ.</p>
        <p>______________________________________________________________________________</p>
        </div>
        </body>
        </html>"
      )
    },
    
    .run = function() {
      if (is.null(self$options$coox) || is.null(self$options$cooy) || is.null(self$options$hedo))
        return()
      
      if (length(self$options$hedo) < 2) {
        jmvcore::reject("The number of Liking Variables is too low for the Representation of the Products")
        return()
      }
      
      data <- private$.buildData()
      if (is.null(data))
        return()
      
      self$results$plotcarto$setState(data)
    },
    
    .plotcartograph = function(image, ...) {
      if (is.null(self$options$coox) || is.null(self$options$cooy) || is.null(self$options$hedo))
        return()
      
      if (length(self$options$hedo) < 2) {
        jmvcore::reject("The number of Liking Variables is too low for the Representation of the Products")
        return()
      }
      
      data <- image$state
      if (is.null(data) || ncol(data) < 4) {
        jmvcore::reject("The data are not sufficient to build the preference map")
        return()
      }
      
      regtype_gui <- suppressWarnings(as.numeric(self$options$regtype))
      if (is.na(regtype_gui) || !(regtype_gui %in% c(1, 2, 3, 4)))
        regtype_gui <- 1
      
      colbelow_gui <- private$.getColorFromName(self$options$colbelow, "#4575B4")
      colabove_gui <- private$.getColorFromName(self$options$colabove, "#D73027")
      
      Mat  <- data[, 1:2, drop = FALSE]
      MatH <- data[, 3:ncol(data), drop = FALSE]
      
      depasse_df <- tryCatch(
        private$.carto_jamovi(Mat, MatH, regmod = regtype_gui),
        error = function(e) {
          jmvcore::reject(paste("Preference mapping failed:", e$message))
          NULL
        }
      )
      
      if (is.null(depasse_df) || nrow(depasse_df) == 0)
        return()
      
      breaks_map <- c(0, 10, 20, 25, 30, 35, 40, 50, 60, 65, 70, 75, 80, 90, 95, 100)
      spectral_colors <- grDevices::colorRampPalette(c(colbelow_gui, "white", colabove_gui))
      
      Mat_labels <- as.data.frame(Mat)
      Mat_labels$name <- rownames(Mat_labels)
      colnames(Mat_labels)[1:2] <- c("Var1", "Var2")
      
      y_range <- range(depasse_df$Var2, na.rm = TRUE)
      offset  <- if (diff(y_range) == 0) 0.03 else 0.03 * diff(y_range)
      
      plot <- ggplot(depasse_df, aes(x = Var1, y = Var2, z = value)) +
        geom_contour_filled(breaks = breaks_map) +
        geom_contour(colour = "grey30", show.legend = FALSE) +
        scale_fill_manual(
          values = spectral_colors(length(breaks_map) - 1),
          drop = FALSE
        ) +
        coord_equal() +
        labs(
          title = "Preference Mapping",
          x     = colnames(Mat)[1],
          y     = colnames(Mat)[2],
          fill  = "% of consumers"
        ) +
        theme_light() +
        theme(
          plot.title      = element_text(hjust = 0.5, face = "bold"),
          legend.position = "right"
        ) +
        annotate(
          "point",
          x = Mat_labels$Var1,
          y = Mat_labels$Var2,
          pch = 15,
          size = 2
        ) +
        annotate(
          "text",
          x = Mat_labels$Var1,
          y = Mat_labels$Var2 + offset,
          label = Mat_labels$name,
          size = 3
        )
      
      print(plot)
      TRUE
    },
    
    #---------------------------------------------
    #### Helper functions ----
    
    .getColorFromName = function(name, default = "#D73027") {
      
      palette_map <- c(
        "red"      = "#D73027",
        "blue"     = "#4575B4",
        "green"    = "#1A9850",
        "orange"   = "#F46D43",
        "purple"   = "#984EA3",
        "brown"    = "#A65628",
        "darkgrey" = "#4D4D4D",
        "black"    = "#000000"
      )
      
      name <- tolower(as.character(name))
      
      if (length(name) == 0 || is.na(name) || !(name %in% names(palette_map)))
        return(default)
      
      palette_map[[name]]
    },
    
    .buildData = function() {
      if (is.null(self$options$coox) || is.null(self$options$cooy) || is.null(self$options$hedo))
        return(NULL)
      
      datacoox <- data.frame(self$data[, self$options$coox, drop = FALSE])
      colnames(datacoox) <- self$options$coox
      
      datacooy <- data.frame(self$data[, self$options$cooy, drop = FALSE])
      colnames(datacooy) <- self$options$cooy
      
      datanote <- data.frame(self$data[, self$options$hedo, drop = FALSE])
      colnames(datanote) <- self$options$hedo
      
      data <- data.frame(datacoox, datacooy, datanote, check.names = FALSE)
      
      if (!is.null(self$options$individus)) {
        ids <- self$data[[self$options$individus]]
        ids <- as.character(ids)
        bad <- is.na(ids) | ids == ""
        ids[bad] <- as.character(seq_len(sum(bad)))
        rownames(data) <- make.unique(ids)
      } else {
        rownames(data) <- seq_len(nrow(data))
      }
      
      data
    },
    
    .carto_jamovi = function(Mat, MatH, level = 0, regmod = 1, resolution = 150) {
      
      predire <- function(n1, n2, coeff) {
        coeff[1] + coeff[2] * n1 + coeff[3] * n2 +
          coeff[4] * n1 * n1 + coeff[5] * n2 * n2 + coeff[6] * n1 * n2
      }
      
      if (!is.data.frame(Mat))
        Mat <- as.data.frame(Mat)
      
      if (!is.data.frame(MatH))
        stop("Non convenient selection for MatH")
      
      if (ncol(Mat) != 2)
        stop("Mat must contain exactly two coordinate variables")
      
      if (ncol(MatH) < 1)
        stop("MatH must contain at least one liking variable")
      
      if (!all(rownames(MatH) %in% rownames(Mat)))
        stop("Row names of MatH are not aligned with row names of Mat")
      
      Mat  <- Mat[rownames(MatH), , drop = FALSE]
      MatH <- as.data.frame(MatH)
      
      if (any(is.na(MatH))) {
        missing <- which(is.na(MatH))
        MatH[missing] <- (
          matrix(rep(apply(MatH, 1, mean, na.rm = TRUE), ncol(MatH)), ncol = ncol(MatH)) +
            matrix(rep(apply(MatH, 2, mean, na.rm = TRUE), each = nrow(MatH)), ncol = ncol(MatH)) -
            matrix(rep(mean(as.matrix(MatH), na.rm = TRUE), ncol(MatH) * nrow(MatH)), ncol = ncol(MatH))
        )[missing]
      }
      
      matrice <- cbind(row.names(MatH), Mat, MatH)
      matrice <- as.data.frame(matrice, stringsAsFactors = FALSE)
      
      for (j in 2:ncol(matrice))
        matrice[[j]] <- suppressWarnings(as.numeric(matrice[[j]]))
      
      matrice[, 4:ncol(matrice)] <- scale(
        matrice[, 4:ncol(matrice), drop = FALSE],
        center = TRUE,
        scale = FALSE
      )
      
      nbconso <- ncol(matrice) - 3
      if (nbconso < 1)
        stop("No liking variable available")
      
      x1 <- matrice[, 2]
      x2 <- matrice[, 3]
      
      if (all(is.na(x1)) || all(is.na(x2)))
        stop("Coordinate variables contain only missing values")
      
      x1c <- scale(x1, center = TRUE, scale = FALSE)[, 1]
      x2c <- scale(x2, center = TRUE, scale = FALSE)[, 1]
      x12 <- x1c^2
      x22 <- x2c^2
      x12plusx22 <- x12 + x22
      x3  <- x1c * x2c
      
      XX <- cbind(x1, x2, x12, x22, x3)
      
      etendue.x1 <- diff(range(x1, na.rm = TRUE))
      etendue.x2 <- diff(range(x2, na.rm = TRUE))
      pas <- max(etendue.x1, etendue.x2) / resolution
      
      if (!is.finite(pas) || pas <= 0)
        pas <- 0.01
      
      f1 <- seq(
        min(x1, na.rm = TRUE) - etendue.x1 * 0.05,
        max(x1, na.rm = TRUE) + etendue.x1 * 0.05,
        by = pas
      )
      f2 <- seq(
        min(x2, na.rm = TRUE) - etendue.x2 * 0.05,
        max(x2, na.rm = TRUE) + etendue.x2 * 0.05,
        by = pas
      )
      
      if (length(f1) < 2 || length(f2) < 2)
        stop("Grid resolution is too small to build the map")
      
      depasse <- matrix(0, nrow = length(f1), ncol = length(f2))
      
      for (i in seq_len(nbconso)) {
        y <- matrice[, i + 3]
        coeff <- NULL
        
        if (regmod == 1) {
          coeff <- lm(
            y ~ XX[, 1] + XX[, 2] + XX[, 3] + XX[, 4] + XX[, 5],
            na.action = na.omit
          )$coef
        }
        
        if (regmod == 2) {
          coeff <- lm(y ~ XX[, 1] + XX[, 2], na.action = na.omit)$coef
          coeff <- c(coeff, 0, 0, 0)
        }
        
        if (regmod == 3) {
          coeff <- lm(y ~ x1 + x2 + x12plusx22, na.action = na.omit)$coef
          coeff <- c(coeff, coeff[4], 0)
        }
        
        if (regmod == 4) {
          coeff <- lm(
            y ~ XX[, 1] + XX[, 2] + XX[, 3] + XX[, 4],
            na.action = na.omit
          )$coef
          coeff <- c(coeff, 0)
        }
        
        if (length(coeff) < 6)
          coeff <- c(coeff, rep(0, 6 - length(coeff)))
        
        coeff[is.na(coeff)] <- 0
        
        predites <- outer(f1, f2, predire, coeff)
        
        sdp <- stats::sd(as.vector(predites), na.rm = TRUE)
        if (is.finite(sdp) && sdp != 0)
          predites <- (predites - mean(predites, na.rm = TRUE)) / sdp
        
        depasse <- depasse + matrix(
          as.numeric(predites > level),
          nrow = length(f1),
          ncol = length(f2)
        )
      }
      
      depasse <- round(depasse / nbconso * 100)
      dimnames(depasse) <- list(as.character(f1), as.character(f2))
      
      depasse_df <- reshape2::melt(depasse)
      colnames(depasse_df) <- c("Var1", "Var2", "value")
      depasse_df$Var1 <- as.numeric(as.character(depasse_df$Var1))
      depasse_df$Var2 <- as.numeric(as.character(depasse_df$Var2))
      
      depasse_df
    }
  )
)