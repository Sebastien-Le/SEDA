# This file is a generated template, your changes will not be overwritten
cataClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
  "cataClass",
  inherit = cataBase,
  private = list(

    .init = function() {
      if (is.null(self$data) || is.null(self$options$group)) {
        if (isTRUE(self$options$tuto))
          self$results$instructions$setVisible(visible = TRUE)
      }

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
      <b>What you should know before analysing CATA data in jamovi</b>
    </p>

    <div style='border-top: 1px solid #CBD8E8; margin-bottom: 12px;'></div>

    <p style='margin: 0 0 9px 0;'>
      <b>Purpose.</b>
      CATA (<i>Check-All-That-Apply</i>) questions provide a fixed list of terms.
      For each stimulus, participants select every term they consider applicable.
      The responses are therefore usually coded as binary indicators rather than
      as intensity ratings.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Data structure.</b>
      Select one categorical variable identifying the stimulus and at least three
      numeric CATA variables. The CATA variables are typically coded 0/1. SEDA
      aggregates the individual responses to obtain a stimulus-by-attribute
      frequency table.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Stimulus description.</b>
      The aggregated table is analysed with <i>descfreq</i>. For each stimulus,
      CATA terms selected significantly more or less often than expected from
      their global frequency are identified. The significance threshold controls
      this characterization step.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Correspondence map.</b>
      Correspondence Analysis (CA) represents stimuli and CATA terms in a common
      factorial space. The selected X-axis and Y-axis control the displayed plane.
      Interpret stimulus and term profiles relative to the factorial dimensions;
      row-to-column distances should not be read as ordinary Euclidean distances.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Clustering.</b>
      HCPC groups stimuli using their coordinates on the first factorial
      dimensions. By default, the first five dimensions are used, or all
      available dimensions when fewer than five exist. A value of -1 lets
      FactoMineR determine the number of clusters automatically. The cluster
      map itself is drawn directly by FactoMineR.
    </p>

    <p style='margin: 0;'>
      <b>Recommended reading sequence.</b>
      Start with the statistical description of the stimuli, then read the CA
      map, and finally interpret the clustering and cluster descriptions. The
      wide aggregated frequency table is provided afterwards as a technical
      audit of the data used by the analysis.
    </p>

  </div>"
      )

      self$results$step1Guide$setContent(
        "
  <div style='margin: 6px 0 10px 0; padding: 9px 13px;
      background-color: #F4F7FB; border-left: 4px solid #6B9DE8;
      color: #333333; line-height: 1.4;'>
    <b style='color: #355F98;'>Step 1 — Describe the stimuli statistically.</b>
    The <i>descfreq</i> results identify CATA terms that are over- or
    under-represented for each stimulus relative to their global frequency.
    Positive V-tests correspond to over-representation and negative V-tests to
    under-representation; the p-value quantifies the statistical evidence.
  </div>"
      )

      self$results$step2Guide$setContent(
        "
  <div style='margin: 6px 0 10px 0; padding: 9px 13px;
      background-color: #F4F7FB; border-left: 4px solid #6B9DE8;
      color: #333333; line-height: 1.4;'>
    <b style='color: #355F98;'>Step 2 — Represent the CATA product space.</b>
    The correspondence map summarizes the stimulus-by-CATA frequency structure.
    Use the selected factorial plane to compare stimulus profiles and identify
    the terms that structure the dimensions. The map is an exploratory synthesis
    of frequency profiles, not a representation of sensory intensities.
  </div>"
      )

      self$results$step3Guide$setContent(
        "
  <div style='margin: 6px 0 10px 0; padding: 9px 13px;
      background-color: #F4F7FB; border-left: 4px solid #6B9DE8;
      color: #333333; line-height: 1.4;'>
    <b style='color: #355F98;'>Step 3 — Group and characterize similar stimuli.</b>
    HCPC is performed on the requested number of CA dimensions. The native
    FactoMineR cluster map shows the resulting partition, while the following
    description table identifies CATA terms that characterize each cluster.
  </div>"
      )

      self$results$step4Guide$setContent(
        "
  <div style='margin: 6px 0 10px 0; padding: 9px 13px;
      background-color: #F4F7FB; border-left: 4px solid #6B9DE8;
      color: #333333; line-height: 1.4;'>
    <b style='color: #355F98;'>Aggregated CATA table.</b>
    This wide table contains the stimulus-by-attribute frequencies used by
    <i>descfreq</i>, CA and HCPC. It is useful for checking the aggregated input.
  </div>"
      )
    },

    .run = function() {
      if (is.null(self$options$stimuli) || is.null(self$options$group))
        return()

      if (length(self$options$group) < 3L) {
        jmvcore::reject("Select at least three CATA attributes.")
        return()
      }

      data <- private$.buildData()
      if (is.null(data) || nrow(data) == 0L)
        return()

      private$.validateRawCATA(data)

      res.dataprod <- tryCatch(
        private$.Dataprod(data),
        error = function(e) {
          jmvcore::reject(paste("Aggregation of CATA data failed:", e$message))
          NULL
        }
      )
      if (is.null(res.dataprod) || nrow(res.dataprod) == 0L || ncol(res.dataprod) == 0L)
        return()

      validation <- private$.validateTable(res.dataprod)
      if (!isTRUE(validation$ok)) {
        jmvcore::reject(validation$message)
        return()
      }

      threshold <- suppressWarnings(as.numeric(self$options$thres)) / 100
      if (!is.finite(threshold) || threshold <= 0 || threshold >= 1) {
        jmvcore::reject("The significance threshold must be greater than 0 and lower than 100%.")
        return()
      }

      private$.populateTEXTUALTable(res.dataprod)

      dfres <- tryCatch(
        FactoMineR::descfreq(res.dataprod, proba = threshold),
        error = function(e) NULL
      )

      if (!is.null(dfres)) {
        dfres_nonnull <- dfres[!vapply(dfres, is.null, logical(1))]
        if (length(dfres_nonnull) > 0L) {
          tabs <- lapply(names(dfres_nonnull), function(nm) {
            x <- as.data.frame(dfres_nonnull[[nm]])
            if (nrow(x) == 0L) return(NULL)
            out <- cbind(Modality = rep(nm, nrow(x)), Word = rownames(x), x)
            rownames(out) <- NULL
            out
          })
          tabs <- Filter(Negate(is.null), tabs)
          if (length(tabs) > 0L)
            private$.populateDFTable(do.call(rbind, tabs))
        }
      }

      ncp_cluster <- private$.clusterDimensions(validation$maxdim)
      ncp_ca <- max(ncp_cluster, as.integer(self$options$abs), as.integer(self$options$ord))

      res.ca <- tryCatch(
        FactoMineR::CA(res.dataprod, ncp = ncp_ca, graph = FALSE),
        error = function(e) {
          jmvcore::reject(paste("Correspondence Analysis failed:", e$message))
          NULL
        }
      )
      if (is.null(res.ca))
        return()

      available_dims <- ncol(res.ca$row$coord)
      axes <- as.integer(c(self$options$abs, self$options$ord))
      if (is.null(available_dims) || available_dims < 2L || any(axes > available_dims)) {
        jmvcore::reject("The requested factorial plane is not available for this CATA table.")
        return()
      }
      ncp_cluster <- min(ncp_cluster, available_dims)

      self$results$plotcata$setState(res.ca)

      res.ca.cluster <- private$.CAforClustering(res.ca, ncp_cluster)
      res.classif <- tryCatch(
        FactoMineR::HCPC(
          res.ca.cluster,
          nb.clust = as.integer(self$options$nbclust),
          graph = FALSE
        ),
        error = function(e) {
          jmvcore::reject(paste("HCPC failed:", e$message))
          NULL
        }
      )

      if (!is.null(res.classif)) {
        self$results$plotclassif$setState(list(ca = res.ca, classif = res.classif))

        clust_var <- res.classif$data.clust[, ncol(res.classif$data.clust)]
        names(clust_var) <- rownames(res.classif$data.clust)
        clust_var <- clust_var[rownames(res.dataprod)]
        private$.populateClusterTable(res.dataprod, clust_var, threshold)
      }

      if (isTRUE(self$options$showCode))
        self$results$code$setContent(private$.code(available_dims))
    },

    #### Compute results ----

    .Dataprod = function(data) {
      formula <- reformulate(self$options$stimuli, response = ".")
      data <- aggregate(formula, data = data, sum)
      rownames(data) <- data[, 1]
      data[, -1, drop = FALSE]
    },

    .validateRawCATA = function(data) {
      if (is.null(data) || ncol(data) < 2L)
        jmvcore::reject("Select one stimulus variable and at least three CATA attributes.")

      attrs <- names(data)[-1L]
      stimulus_name <- names(data)[1L]
      if (stimulus_name %in% attrs || anyDuplicated(attrs))
        jmvcore::reject("Stimulus Variable and CATA Attributes must be different variables, with no duplicate attribute selection.")
      stimulus <- data[[1L]]
      if (anyNA(stimulus))
        jmvcore::reject("Stimulus Variable contains missing values. Each CATA response must be assigned to a stimulus.")

      bad_type <- character()
      bad_missing <- character()
      bad_values <- character()

      for (nm in attrs) {
        x <- data[[nm]]
        if (is.logical(x))
          x <- as.integer(x)
        if (!is.numeric(x)) {
          bad_type <- c(bad_type, nm)
          next
        }
        if (anyNA(x))
          bad_missing <- c(bad_missing, nm)
        finite <- x[!is.na(x)]
        if (any(!is.finite(finite)) || any(!(finite %in% c(0, 1))))
          bad_values <- c(bad_values, nm)
      }

      if (length(bad_type) > 0L)
        jmvcore::reject(paste0(
          "CATA attributes must be numeric or logical 0/1 variables. Check: ",
          paste(unique(bad_type), collapse = ", "), "."
        ))
      if (length(bad_missing) > 0L)
        jmvcore::reject(paste0(
          "Missing CATA responses are not treated as unchecked responses. Resolve missing values in: ",
          paste(unique(bad_missing), collapse = ", "), "."
        ))
      if (length(bad_values) > 0L)
        jmvcore::reject(paste0(
          "CATA attributes must contain only 0 and 1. Check: ",
          paste(unique(bad_values), collapse = ", "), "."
        ))

      invisible(TRUE)
    },

    .validateTable = function(tab) {
      maxdim <- min(nrow(tab) - 1L, ncol(tab) - 1L)
      if (!is.finite(maxdim) || maxdim < 2L) {
        return(list(
          ok = FALSE,
          message = "At least three stimuli and three informative CATA attributes are required for a two-dimensional CA map."
        ))
      }

      if (anyNA(tab))
        return(list(ok = FALSE, message = "The aggregated CATA table contains missing values. Check the selected CATA variables before running the analysis."))

      row_totals <- rowSums(tab)
      col_totals <- colSums(tab)

      if (any(!is.finite(row_totals)) || any(!is.finite(col_totals))) {
        return(list(ok = FALSE, message = "The aggregated CATA table contains non-finite values."))
      }

      if (any(row_totals <= 0)) {
        bad <- paste(rownames(tab)[row_totals <= 0], collapse = ", ")
        return(list(
          ok = FALSE,
          message = paste0("The following stimuli have no selected CATA attributes: ", bad, ".")
        ))
      }

      if (any(col_totals <= 0)) {
        bad <- paste(colnames(tab)[col_totals <= 0], collapse = ", ")
        return(list(
          ok = FALSE,
          message = paste0("The following CATA attributes were never selected and cannot enter the CA: ", bad, ".")
        ))
      }

      axes <- suppressWarnings(as.integer(c(self$options$abs, self$options$ord)))
      if (length(axes) != 2L || any(!is.finite(axes)) || any(axes < 1L))
        return(list(ok = FALSE, message = "X-axis and Y-axis must be positive integers."))
      if (axes[1] == axes[2])
        return(list(ok = FALSE, message = "X-axis and Y-axis must be different."))
      if (any(axes > maxdim)) {
        return(list(
          ok = FALSE,
          message = paste0("The selected factorial plane is not available. This CATA table has at most ", maxdim, " CA dimensions.")
        ))
      }

      ncp <- suppressWarnings(as.integer(self$options$ncp))
      if (length(ncp) != 1L || !is.finite(ncp) || ncp < 1L)
        return(list(ok = FALSE, message = "The number of dimensions for clustering must be a positive integer."))

      ncp_cluster <- min(ncp, maxdim)
      if (any(axes > ncp_cluster)) {
        return(list(
          ok = FALSE,
          message = paste0(
            "The selected factorial plane must be included among the dimensions used for clustering. ",
            "Increase 'Number of dimensions for clustering' to at least ", max(axes), "."
          )
        ))
      }

      nbclust <- suppressWarnings(as.integer(self$options$nbclust))
      if (length(nbclust) != 1L || !is.finite(nbclust) || !(nbclust == -1L || nbclust >= 2L))
        return(list(ok = FALSE, message = "The number of clusters must be -1 (automatic) or an integer of at least 2."))
      if (nbclust != -1L && nbclust >= nrow(tab))
        return(list(ok = FALSE, message = "The number of clusters must be smaller than the number of stimuli."))

      list(ok = TRUE, maxdim = as.integer(maxdim))
    },

    .clusterDimensions = function(maxdim) {
      requested <- suppressWarnings(as.integer(self$options$ncp))
      as.integer(min(max(1L, requested), maxdim))
    },

    .CAforClustering = function(res.ca, ncp_cluster) {
      out <- res.ca
      keep <- seq_len(min(ncp_cluster, ncol(res.ca$row$coord)))
      out$row$coord <- res.ca$row$coord[, keep, drop = FALSE]
      if (!is.null(out$eig))
        out$eig <- res.ca$eig[keep, , drop = FALSE]
      if (!is.null(out$call$ncp))
        out$call$ncp <- length(keep)
      out
    },

    .code = function(maxdim) {
      r_literal <- function(value) {
        if (is.null(value))
          return("NULL")
        paste(deparse(value, width.cutoff = 500L), collapse = "\n")
      }

      stimulus <- as.character(self$options$stimuli)[1]
      attributes <- as.character(self$options$group)
      threshold <- suppressWarnings(as.numeric(self$options$thres)) / 100
      axes <- as.integer(c(self$options$abs, self$options$ord))
      ncp_cluster <- private$.clusterDimensions(maxdim)
      ncp_ca <- max(ncp_cluster, axes)
      nbclust <- as.integer(self$options$nbclust)

      code <- c(
        "library(FactoMineR)",
        "",
        "# This script can be pasted directly into the jamovi Rj Editor.",
        "# The dataset open in jamovi is available as data.",
        "",
        "# Keep the stimulus variable first, followed by the selected CATA attributes.",
        paste0("data_CATA <- data[, c(", r_literal(stimulus), ", ", r_literal(attributes), "), drop = FALSE]"),
        "names(data_CATA)[1] <- \".Stimulus\"",
        "data_CATA$.Stimulus <- as.factor(data_CATA$.Stimulus)",
        "",
        "# CATA responses must be complete binary indicators (0 = unchecked, 1 = checked).",
        "data_CATA[, -1] <- lapply(data_CATA[, -1, drop = FALSE], function(x) if (is.logical(x)) as.integer(x) else x)",
        "if (!all(vapply(data_CATA[, -1, drop = FALSE], is.numeric, logical(1)))) stop(\"CATA attributes must be numeric/logical 0/1 variables.\")",
        "if (anyNA(data_CATA[, -1, drop = FALSE])) stop(\"Resolve missing CATA responses before analysis.\")",
        "if (any(!vapply(data_CATA[, -1, drop = FALSE], function(x) all(is.finite(x) & x %in% c(0, 1)), logical(1)))) stop(\"CATA attributes must contain only 0 and 1.\")",
        "",
        "# Aggregate individual checks into a stimulus-by-attribute frequency table.",
        "tab_CATA <- stats::aggregate(",
        paste0("  data_CATA[, ", r_literal(attributes), ", drop = FALSE],"),
        "  by = list(.Stimulus = data_CATA$.Stimulus),",
        "  FUN = sum",
        ")",
        "rownames(tab_CATA) <- as.character(tab_CATA$.Stimulus)",
        "tab_CATA$.Stimulus <- NULL",
        "tab_CATA",
        "",
        "# Describe each stimulus from its CATA frequencies.",
        paste0("desc_stimuli_CATA <- FactoMineR::descfreq(tab_CATA, proba = ", r_literal(threshold), ")"),
        "desc_stimuli_CATA",
        "",
        "# Correspondence Analysis.",
        paste0("axes_CATA <- ", r_literal(axes)),
        paste0("ncp_clustering_CATA <- ", r_literal(ncp_cluster)),
        paste0("ncp_CA_CATA <- ", r_literal(ncp_ca)),
        "res_CA_CATA <- FactoMineR::CA(",
        "  tab_CATA,",
        "  ncp = ncp_CA_CATA,",
        "  graph = FALSE",
        ")",
        "res_CA_CATA$eig",
        "",
        "# Factor map. new.plot = FALSE avoids opening an external graphics device.",
        "FactoMineR::plot.CA(",
        "  res_CA_CATA,",
        "  axes = axes_CATA,",
        "  title = \"Representation of the Stimuli and the CATA\",",
        "  new.plot = FALSE,",
        "  graph.type = \"classic\"",
        ")",
        "",
        "# HCPC uses only the first dimensions requested for clustering, independently",
        "# of the factorial plane displayed above.",
        "res_CA_for_HCPC <- res_CA_CATA",
        "keep_CATA <- seq_len(ncp_clustering_CATA)",
        "res_CA_for_HCPC$row$coord <- res_CA_for_HCPC$row$coord[, keep_CATA, drop = FALSE]",
        "res_CA_for_HCPC$eig <- res_CA_for_HCPC$eig[keep_CATA, , drop = FALSE]",
        "if (!is.null(res_CA_for_HCPC$call$ncp)) res_CA_for_HCPC$call$ncp <- length(keep_CATA)",
        "",
        "res_HCPC_CATA <- FactoMineR::HCPC(",
        "  res_CA_for_HCPC,",
        paste0("  nb.clust = ", r_literal(nbclust), ","),
        "  graph = FALSE",
        ")",
        "res_HCPC_CATA$data.clust",
        "",
        "# Cluster map drawn directly by FactoMineR.",
        "FactoMineR::plot.HCPC(",
        "  res_HCPC_CATA,",
        "  axes = axes_CATA,",
        "  choice = \"map\",",
        "  draw.tree = FALSE,",
        "  new.plot = FALSE,",
        "  title = \"Representation of the Stimuli According to Clusters\"",
        ")",
        "",
        "cluster_CATA <- as.factor(res_HCPC_CATA$data.clust[, ncol(res_HCPC_CATA$data.clust)])",
        "names(cluster_CATA) <- rownames(res_HCPC_CATA$data.clust)",
        "",
        "# Describe the clusters from the original CATA frequency table.",
        "tab_cluster_CATA <- stats::aggregate(",
        "  tab_CATA,",
        "  by = list(Cluster = cluster_CATA[rownames(tab_CATA)]),",
        "  FUN = sum",
        ")",
        "rownames(tab_cluster_CATA) <- paste0(\"Cluster \", tab_cluster_CATA$Cluster)",
        "tab_cluster_CATA$Cluster <- NULL",
        paste0("desc_clusters_CATA <- FactoMineR::descfreq(tab_cluster_CATA, proba = ", r_literal(threshold), ")"),
        "desc_clusters_CATA"
      )

      paste(code, collapse = "\n")
    },

    ### Plot functions ----

    .plotcatatis = function(image, ...) {
      if (is.null(self$options$stimuli) || is.null(self$options$group))
        return()

      res.ca <- image$state
      if (is.null(res.ca) || !inherits(res.ca, "CA"))
        return()

      axes <- as.integer(c(self$options$abs, self$options$ord))
      plot <- FactoMineR::plot.CA(
        res.ca,
        axes = axes,
        title = "Representation of the Stimuli and the CATA",
        graph.type = "classic",
        new.plot = FALSE
      )
      print(plot)
      TRUE
    },

    .plotclassif = function(image, ...) {
      if (is.null(self$options$stimuli) || is.null(self$options$group))
        return()

      state <- image$state
      if (is.null(state) || is.null(state$classif))
        return()

      res.classif <- state$classif
      axes <- as.integer(c(self$options$abs, self$options$ord))

      FactoMineR::plot.HCPC(
        res.classif,
        axes = axes,
        choice = "map",
        draw.tree = FALSE,
        new.plot = FALSE,
        title = "Representation of the Stimuli According to Clusters"
      )
      TRUE
    },

    ### Helper functions ----

    .populateClusterTable = function(res.dataprod, clust_var, threshold) {
      data_by_cluster <- data.frame(
        cluster = clust_var,
        res.dataprod
      )
      formula <- reformulate("cluster", response = ".")
      tab_clust <- aggregate(formula, data = data_by_cluster, sum)
      rownames(tab_clust) <- tab_clust[, 1]
      tab_clust <- tab_clust[, -1, drop = FALSE]

      desc <- tryCatch(
        FactoMineR::descfreq(tab_clust, proba = threshold),
        error = function(e) NULL
      )
      if (is.null(desc)) return()

      dfres_nonnull <- desc[!vapply(desc, is.null, logical(1))]
      if (length(dfres_nonnull) == 0L) return()

      tabs <- lapply(names(dfres_nonnull), function(nm) {
        x <- as.data.frame(dfres_nonnull[[nm]])
        if (nrow(x) == 0L) return(NULL)
        out <- cbind(cluster = rep(nm, nrow(x)), word = rownames(x), x)
        rownames(out) <- NULL
        out
      })
      tabs <- Filter(Negate(is.null), tabs)
      if (length(tabs) == 0L) return()

      tab <- do.call(rbind, tabs)
      table <- self$results$clustergroup$clusterdesc

      for (i in seq_len(nrow(tab))) {
        table$addRow(rowKey = i, values = list(
          cluster     = as.character(tab[i, 1]),
          word        = as.character(tab[i, 2]),
          internper   = tab[i, 3],
          globper     = tab[i, 4],
          internfreq  = tab[i, 5],
          globfreq    = tab[i, 6],
          pvaluedfres = tab[i, 7],
          vtest       = round(tab[i, 8], digits = 2)
        ))
      }
    },

    .populateTEXTUALTable = function(table) {
      textual <- self$results$textualgroup$textual
      coltable <- colnames(table)
      rn <- rownames(table)

      textual$addColumn(name = "rownames", title = "", type = "text")
      for (i in seq_along(coltable))
        textual$addColumn(name = coltable[i], title = coltable[i], type = "integer")

      for (i in seq_len(nrow(table))) {
        row <- list(rownames = rn[i])
        for (j in seq_along(coltable))
          row[[coltable[j]]] <- table[i, j]
        textual$addRow(rowKey = i, values = row)
      }
    },

    .populateDFTable = function(tab) {
      for (i in seq_len(nrow(tab))) {
        self$results$dfresgroup$dfres$addRow(rowKey = i, values = list(
          component   = as.character(tab[i, 1]),
          word        = as.character(tab[i, 2]),
          internper   = tab[i, 3],
          globper     = tab[i, 4],
          internfreq  = tab[i, 5],
          globfreq    = tab[i, 6],
          pvaluedfres = tab[i, 7],
          vtest       = round(tab[i, 8], digits = 2)
        ))
      }
    },

    .buildData = function() {
      datastimuli <- data.frame(self$data[, self$options$stimuli, drop = FALSE])
      colnames(datastimuli) <- self$options$stimuli
      datanote <- data.frame(self$data[, self$options$group, drop = FALSE])
      colnames(datanote) <- self$options$group
      for (nm in names(datanote)) {
        if (is.logical(datanote[[nm]]))
          datanote[[nm]] <- as.integer(datanote[[nm]])
      }
      data.frame(datastimuli, datanote, check.names = FALSE)
    }
  )
)
