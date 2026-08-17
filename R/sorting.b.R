SortingClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "SortingClass",
  inherit = SortingBase,

  active = list(
    dataProcessed = function() {
      if (is.null(private$.dataProcessed))
        private$.dataProcessed <- private$.buildData()
      private$.dataProcessed
    },
    nbclust = function() {
      if (is.null(private$.nbclust))
        private$.nbclust <- private$.computeNbclust()
      private$.nbclust
    },
    SortingResult = function() {
      if (is.null(private$.SortingResult))
        private$.SortingResult <- private$.getSortingResult()
      private$.SortingResult
    }
  ),

  private = list(
    .dataProcessed = NULL,
    .nbclust       = NULL,
    .SortingResult = NULL,

    #---------------------------------------------
    #### Init + run functions ----

    .init = function() {
      if (is.null(self$data) || is.null(self$options$actvars)) {
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
      <b>What you should know before analysing Sorting data in jamovi</b>
    </p>

    <div style='border-top: 1px solid #CBD8E8; margin-bottom: 12px;'></div>

    <p style='margin: 0 0 9px 0;'>
      <b>Purpose.</b>
      In a sorting task, each assessor groups stimuli according to perceived
      similarity. Two stimuli placed in the same group by an assessor are
      considered similar for that assessor. The numerical or textual name of a
      group has no common scale across assessors.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Data structure.</b>
      Each row represents one stimulus and each subject is represented by one
      categorical variable containing the group assigned to every stimulus.
      Use <i>Stimuli Labels</i> to identify the rows, select the assessors that
      define the compromise under <i>Active Subjects</i>, and use
      <i>Supplementary Subjects</i> only for assessors that should be projected
      without contributing to the construction of the factorial space.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Why MCA?</b>
      SEDA analyses the individual partitions with Multiple Correspondence
      Analysis (MCA). Stimuli are the individuals of the MCA, subjects are the
      categorical variables, and the subject-specific groups are their
      categories. Stimuli are close in the factorial space when their sorting
      profiles are similar across assessors.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Sorting labels and verbal descriptions.</b>
      A group label can be a simple arbitrary code such as <i>A</i> or <i>1</i>,
      or it can contain verbal information such as
      <i>floral;soft;fresh</i>. When verbal labels are present, SEDA splits terms
      separated by semicolons and identifies words associated with each
      stimulus. This textual description complements the sorting geometry; it
      does not define the MCA itself.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Rare categories.</b>
      The ventilation option is inherited from MCA. Categories used by only a
      very small proportion of stimuli can be ventilated before the MCA to
      reduce the influence of extremely rare group labels. Because sorting
      categories are assessor-specific, use this option cautiously and inspect
      the category map when changing the threshold.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Interpreting the maps.</b>
      The stimulus map represents the consensus geometry. The subject map shows
      which assessors are associated with the factorial dimensions, whereas the
      category map identifies the subject-specific groups associated with each
      direction. The dimensions are sorting contrasts; they acquire a sensory
      meaning only when the group labels or other external information support
      that interpretation.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Clustering.</b>
      HCPC is performed on the first dimensions retained under
      <i>Number of dimensions to save/use for clustering</i>. Products assigned
      to the same cluster therefore have similar positions in the MCA consensus
      space. A value of <b>-1</b> for the number of clusters lets HCPC choose the
      number automatically.
    </p>

    <p style='margin: 0;'>
      <b>Example.</b>
      Open the <i>perfumes_sorting</i> dataset, use <i>Product</i> as the stimulus
      label and select the subject columns as active variables. Some subjects
      use arbitrary group names whereas others provide verbal labels, allowing
      the geometric and textual readings to be compared.
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
    <b style='color: #355F98;'>Step 1 — Read the verbal information carried by the sorting labels.</b>
    When assessors use meaningful words to name their groups, the table below
    identifies terms associated with each stimulus. Arbitrary labels such as
    A, B or 1 have no common sensory meaning and should not be interpreted as
    descriptors. This textual section complements, but does not determine, the MCA.
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
    <b style='color: #355F98;'>Step 2 — Read the consensus sorting space.</b>
    The MCA summarizes similarities among stimuli across all active assessors.
    Read the eigenvalues first, then the stimulus map. The subject and category
    maps help explain the dimensions: subjects with high associations structure
    the corresponding contrast, while their categories indicate the direction
    of that association.
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
    <b style='color: #355F98;'>Step 3 — Identify groups of stimuli with similar sorting profiles.</b>
    HCPC is applied to the retained MCA coordinates, not only to the two axes
    currently displayed. The cluster map is therefore a projection of a
    multidimensional classification onto the selected factorial plane.
  </div>"
      )
    },

    .run = function() {
      # Rebuild run-dependent objects so edited data cannot reuse a private
      # object from a previous execution.
      private$.dataProcessed <- NULL
      private$.nbclust <- NULL
      private$.SortingResult <- NULL

      if (is.null(self$options$actvars) || length(self$options$actvars) < 2L)
        return()

      private$.validateInputs()

      predata <- self$dataProcessed
      if (is.null(predata))
        return()

      data <- predata$mca
      datatext <- predata$text

      if (isTRUE(self$options$longformat)) {
        self$results$longformatdata$setContent(
          paste(capture.output(print(datatext, row.names = FALSE)), collapse = "\n")
        )
      }

      # Textual description is complementary. A failure here should not prevent
      # the core MCA from being computed.
      description <- private$.stimulusDescription(datatext)
      private$.populateDFTable(description)

      res.mca <- self$SortingResult
      if (is.null(res.mca))
        return()

      self$results$mcaCache$setState(res.mca)

      axes <- private$.getValidAxes(res.mca, reject = TRUE)
      private$.validateDescriptionDimensions(res.mca)

      need_classif <- isTRUE(self$options$graphclassif) ||
        (isTRUE(self$options$newvar2) && self$results$newvar2$isNotFilled())

      res.classif <- NULL
      if (need_classif) {
        private$.validateClustering(res.mca, axes)
        res.classif <- private$.classif(res.mca)
        if (!is.null(res.classif))
          self$results$classifCache$setState(res.classif)
      }

      private$.printeigenTable(res.mca)
      private$.fillDimensionDescription(res.mca)
      private$.printTables(res.mca, "coord")
      private$.printTables(res.mca, "contrib")
      private$.printTables(res.mca, "cos2")

      # Store the complete MCA once; image states only need a lightweight marker.
      marker <- list(ready = TRUE)
      self$results$plotindiv$setState(marker)
      self$results$plotvar$setState(marker)
      self$results$plotitemvar$setState(marker)

      if (isTRUE(self$options$graphclassif) && !is.null(res.classif))
        self$results$plotclassif$setState(list(ready = TRUE))

      if (!is.null(res.classif))
        private$.output2(res.classif)

      private$.output(res.mca)

      if (isTRUE(self$options$showCode))
        self$results$code$setContent(private$.code(res.mca, need_classif))
    },

    #---------------------------------------------
    #### Validation ----

    .isIntegerValue = function(x) {
      x <- suppressWarnings(as.numeric(x))
      length(x) == 1L && is.finite(x) && abs(x - round(x)) < 1e-10
    },

    .validateInputs = function() {
      active <- as.character(self$options$actvars)
      supplementary <- as.character(self$options$qualisup)

      if (length(active) < 2L)
        jmvcore::reject("At least two active subjects are required for Sorting analysis.")

      if (nrow(self$data) < 3L)
        jmvcore::reject("At least three stimuli are required for Sorting analysis.")

      if (anyDuplicated(active))
        jmvcore::reject("The same subject cannot be selected more than once as an active subject.")
      if (anyDuplicated(supplementary))
        jmvcore::reject("The same subject cannot be selected more than once as a supplementary subject.")
      if (length(intersect(active, supplementary)) > 0L)
        jmvcore::reject("A subject cannot be both active and supplementary.")

      labels_var <- as.character(self$options$individus)
      if (length(labels_var) > 0L && !is.na(labels_var[1]) && nzchar(labels_var[1])) {
        labels <- as.character(self$data[[labels_var[1]]])
        if (any(is.na(labels) | !nzchar(trimws(labels))))
          jmvcore::reject("Stimuli Labels contains missing or empty values.")
        if (anyDuplicated(labels))
          jmvcore::reject("Stimuli Labels must contain unique values.")
      }

      private$.validateSubjectVariability(active, "Active")
      if (length(supplementary) > 0L)
        private$.validateSubjectVariability(supplementary, "Supplementary")

      abs_gui <- suppressWarnings(as.numeric(self$options$abs))
      ord_gui <- suppressWarnings(as.numeric(self$options$ord))
      if (!private$.isIntegerValue(abs_gui) || !private$.isIntegerValue(ord_gui) ||
          abs_gui < 1 || ord_gui < 1 || abs_gui == ord_gui)
        jmvcore::reject("X-axis and Y-axis must be two distinct positive integers.")

      if (!private$.isIntegerValue(self$options$nFactors) || self$options$nFactors < 1)
        jmvcore::reject("Number of dimensions to describe must be a positive integer.")

      if (!private$.isIntegerValue(self$options$ncp) || self$options$ncp < 1)
        jmvcore::reject("Number of dimensions to save/use for clustering must be a positive integer.")

      proba <- suppressWarnings(as.numeric(self$options$proba))
      if (!is.finite(proba) || proba <= 0 || proba > 100)
        jmvcore::reject("Significance threshold (%) must be greater than 0 and at most 100.")

      leveltext <- suppressWarnings(as.numeric(self$options$leveltext))
      if (!is.finite(leveltext) || leveltext <= 0 || leveltext > 100)
        jmvcore::reject("Description threshold (%) must be greater than 0 and at most 100.")

      ventil <- suppressWarnings(as.numeric(self$options$ventil))
      if (!is.finite(ventil) || ventil < 0 || ventil >= 100)
        jmvcore::reject("Ventilation level (%) must be at least 0 and strictly below 100.")

      nb <- suppressWarnings(as.numeric(self$options$nbclust))
      if (!private$.isIntegerValue(nb) ||
          !(as.integer(nb) == -1L ||
            (as.integer(nb) >= 2L && as.integer(nb) < nrow(self$data))))
        jmvcore::reject(paste0(
          "Number of clusters must be -1 (automatic) or an integer between 2 and ",
          max(2L, nrow(self$data) - 1L), "."
        ))

      invisible(TRUE)
    },

    .validateSubjectVariability = function(variables, label) {
      if (length(variables) == 0L)
        return(invisible(TRUE))

      for (nm in variables) {
        x <- as.character(self$data[[nm]])
        incomplete <- is.na(x) | !nzchar(trimws(x))
        if (any(incomplete))
          jmvcore::reject(paste0(
            label, " subject '", nm,
            "' contains missing or empty group assignments. A sorting partition must assign every stimulus to a group."
          ))
        observed <- unique(x)
        if (length(observed) < 2L)
          jmvcore::reject(paste0(
            label, " subject '", nm,
            "' uses fewer than two observed sorting categories."
          ))
      }
      invisible(TRUE)
    },

    .requiredNcp = function() {
      candidates <- suppressWarnings(as.numeric(c(
        self$options$ncp,
        self$options$nFactors,
        self$options$abs,
        self$options$ord,
        2
      )))
      candidates <- candidates[is.finite(candidates) & candidates > 0]
      as.integer(ceiling(max(candidates)))
    },

    .maxMcaDimensions = function(data) {
      n_active <- length(self$options$actvars)
      if (is.null(data) || n_active < 1L || nrow(data) < 2L)
        return(0L)

      active_data <- data[, seq_len(n_active), drop = FALSE]
      n_levels <- vapply(active_data, function(x) {
        nlevels(droplevels(as.factor(x)))
      }, integer(1))

      structural <- sum(pmax(n_levels - 1L, 0L))
      as.integer(max(0L, min(nrow(active_data) - 1L, structural)))
    },

    .availableDimensions = function(res) {
      if (is.null(res) || is.null(res$ind) || is.null(res$ind$coord))
        return(0L)
      as.integer(ncol(res$ind$coord))
    },

    .getValidAxes = function(res, reject = FALSE) {
      axes <- suppressWarnings(as.numeric(c(self$options$abs, self$options$ord)))
      n_axes <- private$.availableDimensions(res)

      valid <- length(axes) == 2L && all(is.finite(axes)) &&
        all(abs(axes - round(axes)) < 1e-10) && all(axes >= 1) &&
        axes[1] != axes[2] && all(axes <= n_axes)

      if (!valid) {
        if (isTRUE(reject))
          jmvcore::reject(paste0(
            "The requested factorial plane is not available. Choose two distinct axes between 1 and ",
            max(1L, n_axes), "."
          ))
        return(NULL)
      }

      as.integer(axes)
    },

    .validateDescriptionDimensions = function(res) {
      if (private$.availableDimensions(res) < 1L)
        jmvcore::reject("No MCA dimension is available for automatic description.")
      invisible(TRUE)
    },

    .clusterDimensions = function(res) {
      n_available <- private$.availableDimensions(res)
      n_requested <- suppressWarnings(as.integer(self$options$ncp))
      if (is.na(n_requested) || n_requested < 1L || n_available < 1L)
        return(0L)
      as.integer(min(n_requested, n_available))
    },

    .validateClustering = function(res, axes) {
      n_cluster_dims <- private$.clusterDimensions(res)
      if (n_cluster_dims < 1L)
        jmvcore::reject("At least one MCA dimension is required for clustering.")

      if (nrow(res$ind$coord) < 3L || nrow(unique(as.data.frame(res$ind$coord))) < 2L)
        jmvcore::reject("Clustering requires at least three stimuli and two distinct MCA profiles.")

      if (isTRUE(self$options$graphclassif) && any(axes > n_cluster_dims))
        jmvcore::reject(paste0(
          "The cluster map uses only the first ", n_cluster_dims,
          " dimension(s) retained for clustering. Increase the number of dimensions ",
          "to save/use for clustering or choose axes within this range."
        ))

      invisible(TRUE)
    },

    #---------------------------------------------
    #### Compute results ----

    .computeNbclust = function() {
      as.integer(self$options$nbclust)
    },

    .getSortingResult = function() {
      predata <- self$dataProcessed
      if (is.null(predata) || is.null(predata$mca))
        return(NULL)

      private$.MCA(predata$mca)
    },

    .MCA = function(data) {
      active <- as.character(self$options$actvars)
      supplementary <- as.character(self$options$qualisup)
      ventil <- self$options$ventil / 100

      upper <- private$.maxMcaDimensions(data)
      if (upper < 1L)
        jmvcore::reject("The selected sorting data do not provide an analysable MCA space.")

      ncp_use <- as.integer(min(private$.requiredNcp(), upper))
      n_active <- length(active)
      n_sup <- length(supplementary)

      r <- tryCatch({
        if (n_sup > 0L) {
          FactoMineR::MCA(
            data,
            quali.sup = (n_active + 1L):(n_active + n_sup),
            ncp = ncp_use,
            level.ventil = ventil,
            graph = FALSE
          )
        } else {
          FactoMineR::MCA(
            data,
            ncp = ncp_use,
            level.ventil = ventil,
            graph = FALSE
          )
        }
      }, error = function(e) {
        jmvcore::reject(paste("MCA failed:", e$message))
        NULL
      })

      if (!is.null(r))
        attr(r, "SEDA.ncp.requested") <- ncp_use
      r
    },

    .classif = function(res) {
      if (is.null(res) || is.null(res$ind) || is.null(res$ind$coord))
        return(NULL)

      ncp_cluster <- private$.clusterDimensions(res)
      if (ncp_cluster < 1L)
        return(NULL)

      coord <- as.data.frame(res$ind$coord[, seq_len(ncp_cluster), drop = FALSE])
      if (!all(is.finite(as.matrix(coord)))) {
        jmvcore::reject("Clustering failed: non-finite MCA coordinates were detected.")
        return(NULL)
      }

      result <- tryCatch(
        FactoMineR::HCPC(
          coord,
          nb.clust = self$nbclust,
          graph = FALSE,
          description = FALSE
        ),
        error = function(e) {
          jmvcore::reject(paste("HCPC failed:", e$message))
          NULL
        }
      )

      if (!is.null(result))
        attr(result, "SEDA.ncp.classified") <- ncp_cluster
      result
    },

    #---------------------------------------------
    #### Sorting-label description ----

    .textual = function(data) {
      if (is.null(data) || nrow(data) == 0L || ncol(data) < 2L)
        return(NULL)

      tryCatch(
        FactoMineR::textual(
          data,
          num.text = 2,
          contingence.by = 1,
          sep.word = ";"
        ),
        error = function(e) NULL
      )
    },

    .descfreq = function(res) {
      if (is.null(res) || is.null(res$cont.table))
        return(NULL)

      tryCatch(
        FactoMineR::descfreq(
          res$cont.table,
          proba = self$options$leveltext / 100
        ),
        error = function(e) NULL
      )
    },

    .stimulusDescription = function(datatext) {
      empty <- data.frame(
        component = character(), word = character(), internper = numeric(),
        globper = numeric(), internfreq = numeric(), globfreq = numeric(),
        pvaluedfres = numeric(), vtest = numeric(), stringsAsFactors = FALSE
      )

      res.textual <- private$.textual(datatext)
      dfres <- private$.descfreq(res.textual)
      if (is.null(dfres) || length(dfres) == 0L)
        return(empty)

      pieces <- list()
      out_i <- 0L

      for (j in seq_along(dfres)) {
        x <- dfres[[j]]
        if (is.null(x))
          next

        x <- tryCatch(as.data.frame(x, stringsAsFactors = FALSE), error = function(e) NULL)
        if (is.null(x) || nrow(x) == 0L || ncol(x) < 6L)
          next

        word <- rownames(x)
        if (is.null(word))
          word <- as.character(seq_len(nrow(x)))

        stimulus <- names(dfres)[j]
        if (is.null(stimulus) || is.na(stimulus) || !nzchar(stimulus))
          stimulus <- paste("Stimulus", j)

        out_i <- out_i + 1L
        pieces[[out_i]] <- data.frame(
          component = rep(stimulus, nrow(x)),
          word = word,
          internper = suppressWarnings(as.numeric(x[[1]])),
          globper = suppressWarnings(as.numeric(x[[2]])),
          internfreq = suppressWarnings(as.numeric(x[[3]])),
          globfreq = suppressWarnings(as.numeric(x[[4]])),
          pvaluedfres = suppressWarnings(as.numeric(x[[5]])),
          vtest = suppressWarnings(as.numeric(x[[6]])),
          stringsAsFactors = FALSE
        )
      }

      if (length(pieces) == 0L)
        return(empty)

      out <- do.call(rbind, pieces)
      rownames(out) <- NULL
      out
    },

    .populateDFTable = function(tab) {
      has_rows <- !is.null(tab) && nrow(tab) > 0L
      self$results$dfresgroup$setVisible(visible = has_rows)
      if (!has_rows)
        return(invisible(NULL))

      for (i in seq_len(nrow(tab))) {
        self$results$dfresgroup$dfres$addRow(rowKey = i)
        self$results$dfresgroup$dfres$setRow(
          rowNo = i,
          values = as.list(tab[i, , drop = FALSE])
        )
      }
      invisible(NULL)
    },

    #---------------------------------------------
    #### Dimension description ----

    .dimdesc = function(res) {
      n_available <- private$.availableDimensions(res)
      n_requested <- suppressWarnings(as.integer(self$options$nFactors))
      n_desc <- min(n_requested, n_available)
      if (is.na(n_desc) || n_desc < 1L)
        return(.seda_empty_dimdesc())

      raw <- tryCatch(
        FactoMineR::dimdesc(
          res,
          axes = seq_len(n_desc),
          proba = self$options$proba / 100
        ),
        error = function(e) NULL
      )

      if (is.null(raw))
        return(.seda_empty_dimdesc())

      .seda_tidy_dimdesc(raw)
    },

    .fillDimensionDescription = function(res) {
      tidy <- private$.dimdesc(res)
      .seda_fill_dimdesc_table(self$results$dimdesc$categorical, tidy$categorical)
      .seda_fill_dimdesc_table(self$results$dimdesc$categories, tidy$categories)
      invisible(NULL)
    },

    #---------------------------------------------
    #### Tables ----

    .printeigenTable = function(table) {
      eigen <- table$eig[, 1]
      purcent <- table$eig[, 2]
      purcentcum <- table$eig[, 3]

      for (i in seq_along(eigen)) {
        self$results$eigengroup$eigen$addRow(
          rowKey = i,
          values = list(
            component = paste("Dim.", i),
            eigenvalue = eigen[i],
            purcent = purcent[i],
            purcentcum = purcentcum[i]
          )
        )
      }
    },

    .printTables = function(table, quoi) {
      show_ind <- switch(
        quoi,
        "coord" = isTRUE(self$options$indcoord),
        "contrib" = isTRUE(self$options$indcontrib),
        "cos2" = isTRUE(self$options$indcos),
        FALSE
      )
      show_var <- switch(
        quoi,
        "coord" = isTRUE(self$options$varcoord),
        "contrib" = isTRUE(self$options$varcontrib),
        "cos2" = isTRUE(self$options$varcos),
        FALSE
      )

      if (!show_ind && !show_var)
        return(invisible(NULL))

      nFactors_out <- min(
        suppressWarnings(as.integer(self$options$nFactors)),
        ncol(table$ind$coord)
      )
      if (is.na(nFactors_out) || nFactors_out < 1L)
        return(invisible(NULL))

      if (quoi == "coord") {
        quoivar <- table$var$coord
        quoiind <- table$ind$coord
        tablevar <- self$results$variables$coordonnees
        tableind <- self$results$individus$coordonnees
      } else if (quoi == "contrib") {
        quoivar <- table$var$contrib
        quoiind <- table$ind$contrib
        tablevar <- self$results$variables$contribution
        tableind <- self$results$individus$contribution
      } else if (quoi == "cos2") {
        quoivar <- table$var$cos2
        quoiind <- table$ind$cos2
        tablevar <- self$results$variables$cosinus
        tableind <- self$results$individus$cosinus
      } else {
        return(invisible(NULL))
      }

      if (show_var && !is.null(quoivar)) {
        tablevar$addColumn(name = "variables", title = "Subject", type = "text")
        for (i in seq_len(nrow(quoivar)))
          tablevar$addRow(rowKey = i)
        for (i in seq_len(nFactors_out))
          tablevar$addColumn(
            name = paste0("dim", i),
            title = paste0("Dim.", i),
            type = "number"
          )
        for (var in seq_len(nrow(quoivar))) {
          row <- list(variables = rownames(quoivar)[var])
          for (i in seq_len(nFactors_out))
            row[[paste0("dim", i)]] <- quoivar[var, i]
          tablevar$setRow(rowNo = var, values = row)
        }
      }

      if (show_ind && !is.null(quoiind)) {
        tableind$addColumn(name = "individus", title = "Stimulus", type = "text")
        for (i in seq_len(nrow(quoiind)))
          tableind$addRow(rowKey = i)
        for (i in seq_len(nFactors_out))
          tableind$addColumn(
            name = paste0("dim", i),
            title = paste0("Dim.", i),
            type = "number"
          )
        for (ind in seq_len(nrow(quoiind))) {
          row <- list(individus = rownames(quoiind)[ind])
          for (i in seq_len(nFactors_out))
            row[[paste0("dim", i)]] <- quoiind[ind, i]
          tableind$setRow(rowNo = ind, values = row)
        }
      }

      invisible(NULL)
    },

    #---------------------------------------------
    #### Native FactoMineR plots ----

    .getSharedMCA = function() {
      cached <- self$results$mcaCache$state
      if (!is.null(cached))
        return(cached)
      self$SortingResult
    },

    .plotindiv = function(image, ...) {
      if (is.null(self$options$actvars))
        return(FALSE)

      res.mca <- private$.getSharedMCA()
      axes_ok <- private$.getValidAxes(res.mca, reject = FALSE)
      if (is.null(res.mca) || is.null(axes_ok))
        return(FALSE)

      tryCatch({
        FactoMineR::plot.MCA(
          res.mca,
          axes = axes_ok,
          choix = "ind",
          invisible = c("var", "quali.sup"),
          title = "Representation of the Stimuli",
          graph.type = "classic",
          new.plot = FALSE
        )
        TRUE
      }, error = function(e) {
        jmvcore::reject(paste("Stimulus plot failed:", e$message))
        FALSE
      })
    },

    .plotvar = function(image, ...) {
      if (is.null(self$options$actvars))
        return(FALSE)

      res.mca <- private$.getSharedMCA()
      axes_ok <- private$.getValidAxes(res.mca, reject = FALSE)
      if (is.null(res.mca) || is.null(axes_ok))
        return(FALSE)

      tryCatch({
        FactoMineR::plot.MCA(
          res.mca,
          axes = axes_ok,
          choix = "var",
          title = "Representation of the Subjects",
          graph.type = "classic",
          new.plot = FALSE
        )
        TRUE
      }, error = function(e) {
        jmvcore::reject(paste("Subject plot failed:", e$message))
        FALSE
      })
    },

    .plotitemvar = function(image, ...) {
      if (is.null(self$options$actvars))
        return(FALSE)

      res.mca <- private$.getSharedMCA()
      axes_ok <- private$.getValidAxes(res.mca, reject = FALSE)
      if (is.null(res.mca) || is.null(axes_ok))
        return(FALSE)

      invisible_vec <- "ind"
      if (!isTRUE(self$options$varmodvar))
        invisible_vec <- c(invisible_vec, "var")
      if (!isTRUE(self$options$varmodqualisup))
        invisible_vec <- c(invisible_vec, "quali.sup")

      use_selectMod <- !is.null(self$options$modality) &&
        nzchar(trimws(self$options$modality))

      tryCatch({
        args <- list(
          res.mca,
          axes = axes_ok,
          choix = "ind",
          invisible = invisible_vec,
          title = "Representation of the Sorting Categories",
          graph.type = "classic",
          new.plot = FALSE
        )
        if (use_selectMod)
          args$selectMod <- self$options$modality
        do.call(FactoMineR::plot.MCA, args)
        TRUE
      }, error = function(e) {
        jmvcore::reject(paste("Category plot failed:", e$message))
        FALSE
      })
    },

    .plotclassif = function(image, ...) {
      if (is.null(self$options$actvars))
        return(FALSE)

      res.classif <- self$results$classifCache$state
      if (is.null(res.classif))
        return(FALSE)

      axes <- suppressWarnings(as.integer(c(self$options$abs, self$options$ord)))
      classified_ncp <- suppressWarnings(as.integer(
        attr(res.classif, "SEDA.ncp.classified")
      ))

      valid_axes <- length(axes) == 2L && all(!is.na(axes)) &&
        all(axes >= 1L) && axes[1] != axes[2] &&
        length(classified_ncp) == 1L && !is.na(classified_ncp) &&
        all(axes <= classified_ncp)
      if (!valid_axes)
        return(FALSE)

      tryCatch({
        FactoMineR::plot.HCPC(
          res.classif,
          axes = axes,
          choice = "map",
          draw.tree = FALSE,
          new.plot = FALSE,
          title = "Representation of the Stimuli According to Clusters"
        )
        TRUE
      }, error = function(e) {
        jmvcore::reject(paste("Cluster plot failed:", e$message))
        FALSE
      })
    },

    #---------------------------------------------
    #### Saved outputs ----

    .output = function(res.mca) {
      nFactors_out <- min(
        suppressWarnings(as.integer(self$options$ncp)),
        ncol(res.mca$ind$coord)
      )
      if (is.na(nFactors_out) || nFactors_out < 1L)
        return(invisible(NULL))

      if (self$results$newvar$isNotFilled()) {
        self$results$newvar$set(
          keys = seq_len(nFactors_out),
          titles = paste("Dim.", seq_len(nFactors_out)),
          descriptions = rep("MCA component", nFactors_out),
          measureTypes = rep("continuous", nFactors_out)
        )
      }

      for (i in seq_len(nFactors_out))
        self$results$newvar$setValues(
          index = i,
          as.numeric(res.mca$ind$coord[, i])
        )

      self$results$newvar$setRowNums(seq_len(nrow(self$data)))
      invisible(NULL)
    },

    .output2 = function(res.classif) {
      if (is.null(res.classif) || is.null(res.classif$data.clust))
        return(invisible(NULL))

      output <- self$results$newvar2
      if (output$isNotFilled()) {
        output$set(
          keys = 1,
          titles = "Cluster",
          descriptions = "Cluster variable",
          measureTypes = "nominal"
        )
      }

      cluster <- res.classif$data.clust[, ncol(res.classif$data.clust)]
      output$setValues(index = 1, as.factor(cluster))
      output$setRowNums(seq_len(nrow(self$data)))
      invisible(NULL)
    },

    #---------------------------------------------
    #### R code ----

    .code = function(res.mca, need_classif = FALSE) {
      r_string <- function(x) {
        encodeString(as.character(x), quote = '"')
      }

      active <- as.character(self$options$actvars)
      supplementary <- as.character(self$options$qualisup)
      variables <- c(active, supplementary)
      variable_code <- paste(vapply(variables, r_string, character(1)), collapse = ", ")

      label_var <- as.character(self$options$individus)
      has_labels <- length(label_var) > 0L && !is.na(label_var[1]) && nzchar(label_var[1])

      n_active <- length(active)
      n_sup <- length(supplementary)
      ncp_use <- suppressWarnings(as.integer(attr(res.mca, "SEDA.ncp.requested")))
      if (length(ncp_use) != 1L || is.na(ncp_use))
        ncp_use <- ncol(res.mca$ind$coord)

      n_desc <- min(
        suppressWarnings(as.integer(self$options$nFactors)),
        ncol(res.mca$ind$coord)
      )
      n_cluster <- min(
        suppressWarnings(as.integer(self$options$ncp)),
        ncol(res.mca$ind$coord)
      )

      axes_code <- paste0("c(", self$options$abs, ", ", self$options$ord, ")")
      ventil_code <- format(self$options$ventil / 100, scientific = FALSE, trim = TRUE)
      desc_code <- format(self$options$leveltext / 100, scientific = FALSE, trim = TRUE)
      proba_code <- format(self$options$proba / 100, scientific = FALSE, trim = TRUE)

      lines <- c(
        "library(FactoMineR)",
        "",
        "# This script can be pasted directly into the jamovi Rj Editor.",
        "# The dataset open in jamovi is available as data.",
        paste0("sorting_variables <- c(", variable_code, ")"),
        "data_SORTING <- data[, sorting_variables, drop = FALSE]",
        "data_SORTING[] <- lapply(data_SORTING, as.factor)"
      )

      if (has_labels) {
        lines <- c(
          lines,
          paste0("stimulus_labels <- as.character(data[[", r_string(label_var[1]), "]])"),
          "rownames(data_SORTING) <- stimulus_labels"
        )
      } else {
        lines <- c(lines, "rownames(data_SORTING) <- as.character(seq_len(nrow(data_SORTING)))")
      }

      lines <- c(
        lines,
        "",
        "# ------------------------------------------------------------------",
        "# 1. Optional textual description of the sorting labels",
        "# ------------------------------------------------------------------",
        "sorting_labels_long <- do.call(rbind, lapply(seq_along(data_SORTING), function(j) {",
        "  data.frame(",
        "    Stimulus = rownames(data_SORTING),",
        "    Label = as.character(data_SORTING[[j]]),",
        "    stringsAsFactors = FALSE",
        "  )",
        "}))",
        "res_textual_SORTING <- textual(",
        "  sorting_labels_long,",
        "  num.text = 2, contingence.by = 1, sep.word = \";\"",
        ")",
        paste0("res_words_SORTING <- descfreq(res_textual_SORTING$cont.table, proba = ", desc_code, ")"),
        "res_words_SORTING",
        "",
        "# ------------------------------------------------------------------",
        "# 2. MCA of the sorting partitions",
        "# ------------------------------------------------------------------"
      )

      if (n_sup > 0L) {
        lines <- c(
          lines,
          "res_SORTING <- MCA(",
          "  data_SORTING,",
          paste0("  quali.sup = ", n_active + 1L, ":", n_active + n_sup, ","),
          paste0("  ncp = ", ncp_use, ","),
          paste0("  level.ventil = ", ventil_code, ","),
          "  graph = FALSE",
          ")"
        )
      } else {
        lines <- c(
          lines,
          "res_SORTING <- MCA(",
          "  data_SORTING,",
          paste0("  ncp = ", ncp_use, ","),
          paste0("  level.ventil = ", ventil_code, ","),
          "  graph = FALSE",
          ")"
        )
      }

      lines <- c(
        lines,
        "res_SORTING$eig",
        "",
        "# Stimulus map",
        "plot.MCA(",
        "  res_SORTING,",
        paste0("  axes = ", axes_code, ","),
        "  choix = \"ind\",",
        "  invisible = c(\"var\", \"quali.sup\"),",
        "  title = \"Representation of the Stimuli\",",
        "  graph.type = \"classic\", new.plot = FALSE",
        ")",
        "",
        "# Subject map",
        "plot.MCA(",
        "  res_SORTING,",
        paste0("  axes = ", axes_code, ","),
        "  choix = \"var\",",
        "  title = \"Representation of the Subjects\",",
        "  graph.type = \"classic\", new.plot = FALSE",
        ")"
      )

      invisible_vec <- "\"ind\""
      if (!isTRUE(self$options$varmodvar))
        invisible_vec <- paste(invisible_vec, "\"var\"", sep = ", ")
      if (!isTRUE(self$options$varmodqualisup))
        invisible_vec <- paste(invisible_vec, "\"quali.sup\"", sep = ", ")

      lines <- c(
        lines,
        "",
        "# Sorting-category map",
        "plot.MCA(",
        "  res_SORTING,",
        paste0("  axes = ", axes_code, ","),
        "  choix = \"ind\",",
        paste0("  invisible = c(", invisible_vec, "),")
      )

      modality <- trimws(as.character(self$options$modality))
      if (nzchar(modality))
        lines <- c(lines, paste0("  selectMod = ", r_string(modality), ","))

      lines <- c(
        lines,
        "  title = \"Representation of the Sorting Categories\",",
        "  graph.type = \"classic\", new.plot = FALSE",
        ")",
        "",
        "# Automatic description of the retained dimensions",
        paste0(
          "res_dimdesc_SORTING <- dimdesc(res_SORTING, axes = 1:", n_desc,
          ", proba = ", proba_code, ")"
        ),
        "res_dimdesc_SORTING"
      )

      # Reproduce the optional numerical tables requested in the interface.
      if (isTRUE(self$options$indcoord))
        lines <- c(lines, "", paste0("res_SORTING$ind$coord[, 1:", n_desc, ", drop = FALSE]"))
      if (isTRUE(self$options$indcontrib))
        lines <- c(lines, "", paste0("res_SORTING$ind$contrib[, 1:", n_desc, ", drop = FALSE]"))
      if (isTRUE(self$options$indcos))
        lines <- c(lines, "", paste0("res_SORTING$ind$cos2[, 1:", n_desc, ", drop = FALSE]"))
      if (isTRUE(self$options$varcoord))
        lines <- c(lines, "", paste0("res_SORTING$var$coord[, 1:", n_desc, ", drop = FALSE]"))
      if (isTRUE(self$options$varcontrib))
        lines <- c(lines, "", paste0("res_SORTING$var$contrib[, 1:", n_desc, ", drop = FALSE]"))
      if (isTRUE(self$options$varcos))
        lines <- c(lines, "", paste0("res_SORTING$var$cos2[, 1:", n_desc, ", drop = FALSE]"))

      lines <- c(
        lines,
        "",
        "# Coordinates saved by SEDA",
        paste0("sorting_coordinates <- res_SORTING$ind$coord[, 1:", n_cluster, ", drop = FALSE]"),
        "sorting_coordinates"
      )

      if (isTRUE(need_classif)) {
        lines <- c(
          lines,
          "",
          "# ------------------------------------------------------------------",
          "# 3. HCPC on the retained MCA coordinates",
          "# ------------------------------------------------------------------",
          paste0("coord_HCPC <- as.data.frame(res_SORTING$ind$coord[, 1:", n_cluster, ", drop = FALSE])"),
          "res_HCPC_SORTING <- HCPC(",
          "  coord_HCPC,",
          paste0("  nb.clust = ", self$options$nbclust, ","),
          "  graph = FALSE, description = FALSE",
          ")",
          "res_HCPC_SORTING$data.clust$clust"
        )

        if (isTRUE(self$options$graphclassif)) {
          lines <- c(
            lines,
            "plot.HCPC(",
            "  res_HCPC_SORTING,",
            paste0("  axes = ", axes_code, ","),
            "  choice = \"map\", draw.tree = FALSE,",
            "  new.plot = FALSE,",
            "  title = \"Representation of the Stimuli According to Clusters\"",
            ")"
          )
        }
      }

      paste(lines, collapse = "\n")
    },

    #---------------------------------------------
    #### Data preparation ----

    .buildData = function() {
      active <- as.character(self$options$actvars)
      supplementary <- as.character(self$options$qualisup)
      variables <- c(active, supplementary)
      if (length(variables) == 0L)
        return(NULL)

      data <- as.data.frame(self$data[, variables, drop = FALSE])
      colnames(data) <- variables
      data[] <- lapply(data, as.factor)

      if (!is.null(self$options$individus)) {
        ids <- as.character(self$data[[self$options$individus]])
        rownames(data) <- ids
      } else {
        rownames(data) <- as.character(seq_len(nrow(data)))
      }

      datatext <- do.call(rbind, lapply(seq_along(data), function(j) {
        data.frame(
          Stimulus = rownames(data),
          Label = as.character(data[[j]]),
          stringsAsFactors = FALSE
        )
      }))
      rownames(datatext) <- NULL

      list(mca = data, text = datatext)
    }
  )
)
