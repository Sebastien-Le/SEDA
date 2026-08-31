NappingClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "NappingClass",
  inherit = NappingBase,

  active = list(
    dataProcessed = function() {
      if (is.null(private$.dataProcessed))
        private$.dataProcessed <- private$.buildData()
      return(private$.dataProcessed)
    },
    nbclust = function() {
      if (is.null(private$.nbclust))
        private$.nbclust <- private$.computeNbclust()
      return(private$.nbclust)
    },
    classifResult = function() {
      if (is.null(private$.classifResult))
        private$.classifResult <- private$.getclassifResult()
      return(private$.classifResult)
    },
    NappingResult = function() {
      if (is.null(private$.NappingResult))
        private$.NappingResult <- private$.getNappingResult()
      return(private$.NappingResult)
    }
  ),

  private = list(
    .dataProcessed = NULL,
    .nbclust       = NULL,
    .classifResult = NULL,
    .NappingResult = NULL,

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
      <b>What you should know before analysing Napping data in jamovi</b>
    </p>

    <div style='border-top: 1px solid #CBD8E8; margin-bottom: 12px;'></div>

    <p style='margin: 0 0 9px 0;'>
      <b>Purpose.</b>
      Napping is a holistic sensory method in which assessors position all
      stimuli on a two-dimensional sheet according to their perceived
      similarities and differences. Products placed close together are perceived
      as similar; the individual X and Y axes do not have predefined sensory
      meanings.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Data structure.</b>
      Each row represents one stimulus. Each assessor contributes two numeric
      columns, one X coordinate and one Y coordinate. Select the active variables
      as complete consecutive <i>(X, Y)</i> pairs, for example
      <i>X1, Y1, X2, Y2, ...</i>. At least two active pairs are required.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Why MFA?</b>
      SEDA analyses Napping configurations with Multiple Factor Analysis (MFA).
      Each assessor is treated as one group formed by his or her two coordinates,
      which balances the contribution of the individual configurations when the
      common product space is constructed.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Centered but not standardized.</b>
      The coordinate pairs are analysed as quantitative groups with
      <i>type = \"c\"</i> in FactoMineR. The coordinates are centered but not
      standardized, so the geometry and relative scale of each Napping sheet are
      preserved rather than forcing X and Y to have the same variance.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Supplementary assessors.</b>
      Supplementary coordinate pairs are projected onto the consensus space but
      do not contribute to its construction. They must also be selected as
      complete consecutive <i>(X, Y)</i> pairs.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Interpreting dimensions.</b>
      The consensus map reveals the main perceptual contrasts between stimuli.
      The subject map indicates how strongly each individual configuration is
      related to these common dimensions. The automatic dimension description
      identifies coordinate variables associated with each dimension; it does
      not, by itself, provide a sensory name for the axes.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Clustering.</b>
      If requested, HCPC is performed on the first dimensions retained by the
      <i>Number of dimensions to save/use for clustering</i> option. Therefore,
      the cluster map describes similarity in the retained consensus space, not
      only on the two-dimensional plane currently displayed.
    </p>

    <p style='margin: 0;'>
      <b>Example.</b>
      Open the <b>perfumes_napping</b> dataset. Use <i>Ident</i> as
      <i>Stimuli Labels</i> and select the coordinates as
      <i>X1, Y1, X2, Y2, ...</i>. Start with dimensions 1 and 2, then use the
      eigenvalues and automatic description to decide whether additional
      dimensions should be examined.
    </p>

  </div>"
      )

      self$results$step1Guide$setContent(
        "
  <div style='margin: 6px 0 10px 0; padding: 9px 13px;
      background-color: #F4F7FB; border-left: 4px solid #6B9DE8;
      color: #333333; line-height: 1.4;'>
    <b style='color: #355F98;'>Step 1 — Read the consensus product space.</b>
    Start with the eigenvalues to assess how the common perceptual information
    is distributed across dimensions. On the stimulus map, products that are
    close have been positioned similarly across assessors after MFA has balanced
    the individual Napping configurations.
  </div>"
      )

      self$results$step2Guide$setContent(
        "
  <div style='margin: 6px 0 10px 0; padding: 9px 13px;
      background-color: #F4F7FB; border-left: 4px solid #6B9DE8;
      color: #333333; line-height: 1.4;'>
    <b style='color: #355F98;'>Step 2 — Understand how assessors contribute to the consensus.</b>
    The subject map shows how the individual Napping configurations relate to
    the common dimensions. Assessors pointing in similar directions tend to
    structure the product space in similar ways. The automatic description then
    identifies the X/Y coordinates most strongly associated with each dimension.
  </div>"
      )

      self$results$step3Guide$setContent(
        "
  <div style='margin: 6px 0 10px 0; padding: 9px 13px;
      background-color: #F4F7FB; border-left: 4px solid #6B9DE8;
      color: #333333; line-height: 1.4;'>
    <b style='color: #355F98;'>Step 3 — Identify groups of similar stimuli.</b>
    HCPC is performed on the retained MFA dimensions. Products assigned to the
    same cluster are therefore similar in the retained multivariate consensus
    space and not necessarily only on the two dimensions displayed in the map.
  </div>"
      )
    },

    .run = function() {
      # Recompute from the current data/options on every run. This avoids stale
      # cached MFA/HCPC objects when the variable selection or numerical options
      # are changed in the jamovi interface.
      private$.dataProcessed <- NULL
      private$.nbclust       <- NULL
      private$.classifResult <- NULL
      private$.NappingResult <- NULL

      if (is.null(self$options$actvars) || length(self$options$actvars) == 0L)
        return()

      private$.validateInputs()

      res.napping <- self$NappingResult
      if (is.null(res.napping))
        return()

      axes <- private$.validAxes(res.napping)

      need_classif <- isTRUE(self$options$graphclassif) ||
        (isTRUE(self$options$newvar2) && self$results$newvar2$isNotFilled())

      res.classif <- NULL
      if (need_classif) {
        private$.validateClustering(res.napping, axes)
        res.classif <- self$classifResult
      }

      private$.printeigenTable(res.napping)
      private$.fillDimensionDescription(res.napping)

      self$results$plotind$setState(res.napping)
      self$results$plotgroup$setState(res.napping)

      if (isTRUE(self$options$graphclassif) && !is.null(res.classif))
        self$results$plotclassif$setState(res.classif)

      private$.output(res.napping)

      if (!is.null(res.classif))
        private$.output2(res.classif)

      if (isTRUE(self$options$showCode))
        self$results$code$setContent(private$.code(need_classif = need_classif))
    },

    #### Validation and dimensions ----

    .isIntegerValue = function(x) {
      x <- suppressWarnings(as.numeric(x))
      length(x) == 1L && is.finite(x) && abs(x - round(x)) < 1e-10
    },

    .validateInputs = function() {
      active <- as.character(self$options$actvars)
      supplementary <- as.character(self$options$qualisup)

      if (length(active) < 4L)
        jmvcore::reject('Select at least two complete active (X, Y) coordinate pairs.')

      if (length(active) %% 2L != 0L)
        jmvcore::reject('Active coordinates must be selected as complete (X, Y) pairs.')

      if (length(supplementary) %% 2L != 0L)
        jmvcore::reject('Supplementary coordinates must be selected as complete (X, Y) pairs.')

      if (anyDuplicated(active))
        jmvcore::reject('The same coordinate cannot be selected more than once among active variables.')
      if (anyDuplicated(supplementary))
        jmvcore::reject('The same coordinate cannot be selected more than once among supplementary variables.')
      if (length(intersect(active, supplementary)) > 0L)
        jmvcore::reject('A coordinate cannot be both active and supplementary.')

      if (nrow(self$data) < 3L)
        jmvcore::reject('At least three stimuli are required for Napping analysis.')

      labels_var <- as.character(self$options$individus)
      if (length(labels_var) > 0L && !is.na(labels_var[1]) && nzchar(labels_var[1])) {
        labels <- as.character(self$data[[labels_var[1]]])
        if (any(is.na(labels) | !nzchar(trimws(labels))))
          jmvcore::reject('Stimuli Labels contains missing or empty values.')
        if (anyDuplicated(labels))
          jmvcore::reject('Stimuli Labels must contain unique values.')
      }

      abs_gui <- suppressWarnings(as.numeric(self$options$abs))
      ord_gui <- suppressWarnings(as.numeric(self$options$ord))
      if (!private$.isIntegerValue(abs_gui) || !private$.isIntegerValue(ord_gui) ||
          abs_gui < 1 || ord_gui < 1 || abs_gui == ord_gui)
        jmvcore::reject('X-axis and Y-axis must be two distinct positive integers.')

      if (!private$.isIntegerValue(self$options$nFactors) || self$options$nFactors < 1)
        jmvcore::reject('Number of dimensions must be a positive integer.')

      if (!private$.isIntegerValue(self$options$ncp) || self$options$ncp < 1)
        jmvcore::reject('Number of dimensions to save/use for clustering must be a positive integer.')

      proba <- suppressWarnings(as.numeric(self$options$proba))
      if (!is.finite(proba) || proba <= 0 || proba > 100)
        jmvcore::reject('Significance threshold (%) must be greater than 0 and at most 100.')

      private$.validateCoordinateCompleteness(active, 'Active')
      private$.validatePairVariability(active, 'Active')
      if (length(supplementary) > 0L) {
        private$.validateCoordinateCompleteness(supplementary, 'Supplementary')
        private$.validatePairVariability(supplementary, 'Supplementary')
      }

      invisible(TRUE)
    },

    .validateCoordinateCompleteness = function(variables, label) {
      if (length(variables) == 0L)
        return(invisible(TRUE))

      bad_type <- character()
      bad_value <- character()
      for (nm in variables) {
        x <- self$data[[nm]]
        if (!is.numeric(x)) {
          bad_type <- c(bad_type, nm)
          next
        }
        if (any(!is.finite(x)))
          bad_value <- c(bad_value, nm)
      }

      if (length(bad_type) > 0L)
        jmvcore::reject(paste0(
          label, ' coordinates must be numeric. Check: ',
          paste(unique(bad_type), collapse = ', '), '.'
        ))
      if (length(bad_value) > 0L)
        jmvcore::reject(paste0(
          label, ' Napping coordinates must be complete and finite. Check: ',
          paste(unique(bad_value), collapse = ', '), '.'
        ))

      invisible(TRUE)
    },

    .validatePairVariability = function(variables, label) {
      if (length(variables) == 0L)
        return(invisible(TRUE))

      pair_ids <- seq.int(1L, length(variables), by = 2L)
      for (i in seq_along(pair_ids)) {
        j <- pair_ids[i]
        pair_names <- variables[c(j, j + 1L)]
        variances <- vapply(pair_names, function(nm) {
          x <- suppressWarnings(as.numeric(self$data[[nm]]))
          if (sum(!is.na(x)) < 2L)
            return(0)
          v <- stats::var(x, na.rm = TRUE)
          if (is.finite(v)) v else 0
        }, numeric(1))

        if (sum(variances) <= sqrt(.Machine$double.eps))
          jmvcore::reject(paste0(
            label, ' coordinate pair ', i, ' (', paste(pair_names, collapse = ', '),
            ') has no variability.'
          ))
      }

      invisible(TRUE)
    },

    .availableDimensions = function(res) {
      if (is.null(res) || is.null(res$ind) || is.null(res$ind$coord))
        return(0L)
      as.integer(ncol(res$ind$coord))
    },

    .validAxes = function(res) {
      axes <- suppressWarnings(as.integer(c(self$options$abs, self$options$ord)))
      n_axes <- private$.availableDimensions(res)

      if (length(axes) != 2L || any(is.na(axes)) || any(axes < 1L) ||
          axes[1] == axes[2])
        jmvcore::reject('X-axis and Y-axis must be two distinct positive integers.')

      if (n_axes < 2L || any(axes > n_axes))
        jmvcore::reject(paste0(
          'The requested factorial plane is not available. The current MFA provides ',
          n_axes, ' dimension(s).'
        ))

      axes
    },

    .clusterDimensions = function(res) {
      n_available <- private$.availableDimensions(res)
      n_requested <- suppressWarnings(as.integer(self$options$ncp))
      if (is.na(n_requested) || n_requested < 1L || n_available < 1L)
        return(0L)
      as.integer(min(n_requested, n_available))
    },

    .descriptionDimensions = function(res) {
      n_available <- private$.availableDimensions(res)
      n_requested <- suppressWarnings(as.integer(self$options$nFactors))
      if (is.na(n_requested) || n_requested < 1L || n_available < 1L)
        return(0L)
      as.integer(min(n_requested, n_available))
    },

    .validateClustering = function(res, axes) {
      nb <- suppressWarnings(as.numeric(self$options$nbclust))
      n_stimuli <- nrow(res$ind$coord)
      n_cluster_dims <- private$.clusterDimensions(res)

      if (!private$.isIntegerValue(nb) ||
          !(as.integer(nb) == -1L || (as.integer(nb) >= 2L && as.integer(nb) < n_stimuli)))
        jmvcore::reject(paste0(
          'Number of clusters must be -1 (automatic) or an integer between 2 and ',
          max(2L, n_stimuli - 1L), '.'
        ))

      if (n_cluster_dims < 1L)
        jmvcore::reject('At least one MFA dimension is required for clustering.')

      if (isTRUE(self$options$graphclassif) && any(axes > n_cluster_dims))
        jmvcore::reject(paste0(
          'The cluster map uses only the first ', n_cluster_dims,
          ' dimension(s) retained for clustering. Increase the number of dimensions ',
          'to save/use for clustering or choose axes within this range.'
        ))

      invisible(TRUE)
    },

    #### Compute results ----

    .computeNbclust = function() {
      as.integer(self$options$nbclust)
    },

    .getclassifResult = function() {
      res <- self$NappingResult
      if (is.null(res))
        return(NULL)

      ncp_cluster <- private$.clusterDimensions(res)
      if (ncp_cluster < 1L)
        return(NULL)

      coord <- as.data.frame(
        res$ind$coord[, seq_len(ncp_cluster), drop = FALSE],
        check.names = FALSE
      )
      if (!all(is.finite(as.matrix(coord)))) {
        jmvcore::reject('Clustering failed: non-finite MFA coordinates were detected.')
        return(NULL)
      }

      reshcpc <- tryCatch(
        FactoMineR::HCPC(
          coord,
          nb.clust = self$nbclust,
          graph = FALSE,
          description = FALSE
        ),
        error = function(e) {
          jmvcore::reject(paste('HCPC failed:', e$message))
          NULL
        }
      )

      private$.classifResult <- reshcpc
      private$.classifResult
    },

    .getNappingResult = function() {
      data <- self$dataProcessed
      if (is.null(data))
        return(NULL)

      active <- as.character(self$options$actvars)
      supplementary <- as.character(self$options$qualisup)

      ncp_candidates <- suppressWarnings(as.numeric(c(
        self$options$ncp,
        self$options$nFactors,
        self$options$abs,
        self$options$ord,
        2
      )))
      ncp_candidates <- ncp_candidates[is.finite(ncp_candidates) & ncp_candidates > 0]
      ncp_target <- if (length(ncp_candidates) == 0L) 2L else ceiling(max(ncp_candidates))

      ncp_upper <- min(nrow(data) - 1L, length(active))
      if (!is.finite(ncp_upper) || ncp_upper < 1L)
        jmvcore::reject('The selected Napping data do not provide an analysable MFA space.')
      ncp_use <- as.integer(min(ncp_target, ncp_upper))

      nbgroups <- length(active) / 2L
      nbgroupsup <- length(supplementary) / 2L
      nbg <- nbgroups + nbgroupsup

      r <- tryCatch({
        if (nbgroupsup == 0L) {
          FactoMineR::MFA(
            data,
            group = rep(2, nbgroups),
            type = rep('c', nbgroups),
            ncp = ncp_use,
            name.group = paste0('S', seq_len(nbgroups)),
            graph = FALSE
          )
        } else {
          FactoMineR::MFA(
            data,
            group = rep(2, nbg),
            type = rep('c', nbg),
            num.group.sup = (nbgroups + 1L):nbg,
            ncp = ncp_use,
            name.group = paste0('S', seq_len(nbg)),
            graph = FALSE
          )
        }
      }, error = function(e) {
        jmvcore::reject(paste('MFA failed:', e$message))
        NULL
      })

      private$.NappingResult <- r
      private$.NappingResult
    },

    .dimdesc = function(res) {
      n_desc <- private$.descriptionDimensions(res)
      if (n_desc < 1L)
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

      tidy <- .seda_tidy_dimdesc(raw)

      # For MFA dimdesc objects, the original X matrix is not always available
      # in the returned call. Fill N directly from the selected coordinates.
      if (!is.null(tidy$continuous) && nrow(tidy$continuous) > 0L) {
        d <- self$dataProcessed
        tidy$continuous$n <- vapply(tidy$continuous$variable, function(variable) {
          if (!variable %in% names(d))
            return(NA_integer_)
          as.integer(sum(!is.na(d[[variable]])))
        }, integer(1))
      }

      tidy
    },

    .printeigenTable = function(res) {
      eigen <- res$eig[, 1]
      purcent <- res$eig[, 2]
      purcentcum <- res$eig[, 3]

      for (i in seq_along(eigen)) {
        self$results$eigengroup$eigen$addRow(
          rowKey = i,
          values = list(
            component = paste('Dim.', i),
            eigenvalue = eigen[i],
            purcent = purcent[i],
            purcentcum = purcentcum[i]
          )
        )
      }
    },

    .fillDimensionDescription = function(res) {
      tidy <- private$.dimdesc(res)
      .seda_fill_dimdesc_table(self$results$descdesdim, tidy$continuous)
    },

    #### R code ----

    .code = function(need_classif = FALSE) {
      r_literal <- function(value) {
        if (is.null(value))
          return('NULL')
        paste(deparse(value, width.cutoff = 500L), collapse = '\n')
      }

      active <- as.character(self$options$actvars)
      supplementary <- as.character(self$options$qualisup)
      labels_var <- as.character(self$options$individus)
      labels_var <- if (length(labels_var) > 0L && !is.na(labels_var[1]) && nzchar(labels_var[1]))
        labels_var[1] else NULL

      if (length(active) < 4L || length(active) %% 2L != 0L)
        return('# Select at least two complete active (X, Y) coordinate pairs to generate the Napping code.')
      if (length(supplementary) %% 2L != 0L)
        return('# Select supplementary coordinates as complete (X, Y) pairs to generate the Napping code.')

      axes <- as.integer(c(self$options$abs, self$options$ord))
      nFactors <- as.integer(self$options$nFactors)
      ncp <- as.integer(self$options$ncp)
      proba <- as.numeric(self$options$proba) / 100
      nbclust <- as.integer(self$options$nbclust)
      n_active <- length(active) / 2L
      n_sup <- length(supplementary) / 2L
      n_groups <- n_active + n_sup

      code <- c(
        'library(FactoMineR)',
        '',
        '# This script can be pasted directly into the jamovi Rj Editor.',
        '# The dataset open in jamovi is available as data.',
        '',
        '# Each assessor is one group made of two consecutive coordinates (X, Y).',
        '# type = "c" means quantitative variables are centered but not standardized.',
        paste0('active_coordinates_NAPPING <- ', r_literal(active)),
        paste0('supplementary_coordinates_NAPPING <- ', r_literal(supplementary)),
        'selected_coordinates_NAPPING <- c(active_coordinates_NAPPING, supplementary_coordinates_NAPPING)',
        'data_NAPPING <- data[, selected_coordinates_NAPPING, drop = FALSE]',
        ''
      )

      if (is.null(labels_var)) {
        code <- c(code,
          'rownames(data_NAPPING) <- as.character(seq_len(nrow(data_NAPPING)))',
          ''
        )
      } else {
        code <- c(code,
          paste0('stimulus_labels_NAPPING <- as.character(data[[', r_literal(labels_var), ']])'),
          'rownames(data_NAPPING) <- stimulus_labels_NAPPING',
          ''
        )
      }

      code <- c(code,
        paste0('n_active_subjects_NAPPING <- ', n_active),
        paste0('n_supplementary_subjects_NAPPING <- ', n_sup),
        paste0('axes_NAPPING <- ', r_literal(axes)),
        paste0('n_dimensions_to_describe_NAPPING <- ', nFactors),
        paste0('n_dimensions_for_clustering_NAPPING <- ', ncp),
        'ncp_target_NAPPING <- max(c(n_dimensions_to_describe_NAPPING,',
        '                           n_dimensions_for_clustering_NAPPING,',
        '                           axes_NAPPING, 2L))',
        'ncp_upper_NAPPING <- min(nrow(data_NAPPING) - 1L, length(active_coordinates_NAPPING))',
        'ncp_use_NAPPING <- min(ncp_target_NAPPING, ncp_upper_NAPPING)',
        '',
        '# Multiple Factor Analysis: one centered, non-standardized (X, Y) group per assessor.'
      )

      if (n_sup == 0L) {
        code <- c(code,
          'res_NAPPING <- FactoMineR::MFA(',
          '  data_NAPPING,',
          '  group = rep(2, n_active_subjects_NAPPING),',
          '  type = rep("c", n_active_subjects_NAPPING),',
          '  ncp = ncp_use_NAPPING,',
          '  name.group = paste0("S", seq_len(n_active_subjects_NAPPING)),',
          '  graph = FALSE',
          ')'
        )
      } else {
        code <- c(code,
          'n_groups_NAPPING <- n_active_subjects_NAPPING + n_supplementary_subjects_NAPPING',
          'res_NAPPING <- FactoMineR::MFA(',
          '  data_NAPPING,',
          '  group = rep(2, n_groups_NAPPING),',
          '  type = rep("c", n_groups_NAPPING),',
          '  num.group.sup = (n_active_subjects_NAPPING + 1L):n_groups_NAPPING,',
          '  ncp = ncp_use_NAPPING,',
          '  name.group = paste0("S", seq_len(n_groups_NAPPING)),',
          '  graph = FALSE',
          ')'
        )
      }

      code <- c(code,
        '',
        '# Eigenvalue decomposition.',
        'res_NAPPING$eig',
        '',
        '# Consensus representation of the stimuli.',
        'FactoMineR::plot.MFA(',
        '  res_NAPPING,',
        '  choix = "ind",',
        '  axes = axes_NAPPING,',
        '  habillage = "none",',
        '  title = "Representation of the Stimuli",',
        '  graph.type = "classic",',
        '  new.plot = FALSE',
        ')',
        '',
        '# Representation of the assessors/groups.',
        'FactoMineR::plot.MFA(',
        '  res_NAPPING,',
        '  choix = "group",',
        '  axes = axes_NAPPING,',
        '  title = "Representation of the Subjects",',
        '  graph.type = "classic",',
        '  new.plot = FALSE',
        ')',
        '',
        '# Automatic description of the retained dimensions.',
        'n_desc_NAPPING <- min(n_dimensions_to_describe_NAPPING, ncol(res_NAPPING$ind$coord))',
        paste0('res_dimdesc_NAPPING <- FactoMineR::dimdesc(res_NAPPING, axes = seq_len(n_desc_NAPPING), proba = ', r_literal(proba), ')'),
        'res_dimdesc_NAPPING',
        '',
        '# Coordinates that correspond to the Save option in SEDA.',
        'n_saved_NAPPING <- min(n_dimensions_for_clustering_NAPPING, ncol(res_NAPPING$ind$coord))',
        'coordinates_NAPPING <- res_NAPPING$ind$coord[, seq_len(n_saved_NAPPING), drop = FALSE]',
        'coordinates_NAPPING'
      )

      if (isTRUE(need_classif)) {
        code <- c(code,
          '',
          '# HCPC uses exactly the first dimensions requested for clustering.',
          '# As in MEDA, clustering is performed on the retained factor coordinates.',
          'coord_HCPC_NAPPING <- as.data.frame(',
          '  res_NAPPING$ind$coord[, seq_len(n_saved_NAPPING), drop = FALSE],',
          '  check.names = FALSE',
          ')',
          '',
          'res_HCPC_NAPPING <- FactoMineR::HCPC(',
          '  coord_HCPC_NAPPING,',
          paste0('  nb.clust = ', nbclust, ','),
          '  graph = FALSE,',
          '  description = FALSE',
          ')',
          'cluster_NAPPING <- as.factor(res_HCPC_NAPPING$data.clust[, ncol(res_HCPC_NAPPING$data.clust)])',
          'cluster_NAPPING'
        )

        if (isTRUE(self$options$graphclassif)) {
          code <- c(code,
            '',
            '# Native FactoMineR cluster map.',
            'FactoMineR::plot.HCPC(',
            '  res_HCPC_NAPPING,',
            '  axes = axes_NAPPING,',
            '  choice = "map",',
            '  draw.tree = FALSE,',
            '  new.plot = FALSE,',
            '  title = "Representation of the Stimuli According to Clusters"',
            ')'
          )
        }
      }

      paste(code, collapse = '\n')
    },

    #### Plot functions ----

    .plotindividus = function(image, ...) {
      res.napping <- image$state
      if (is.null(res.napping))
        return()

      axes <- private$.validAxes(res.napping)
      tryCatch({
        FactoMineR::plot.MFA(
          res.napping,
          axes = axes,
          choix = 'ind',
          habillage = 'none',
          title = 'Representation of the Stimuli',
          graph.type = 'classic',
          new.plot = FALSE
        )
        TRUE
      }, error = function(e) FALSE)
    },

    .plotgroups = function(image, ...) {
      res.napping <- image$state
      if (is.null(res.napping))
        return()

      axes <- private$.validAxes(res.napping)
      tryCatch({
        FactoMineR::plot.MFA(
          res.napping,
          axes = axes,
          choix = 'group',
          title = 'Representation of the Subjects',
          graph.type = 'classic',
          new.plot = FALSE
        )
        TRUE
      }, error = function(e) FALSE)
    },

    .plotclassif = function(image, ...) {
      res.classif <- image$state
      if (is.null(res.classif))
        return()

      axes <- suppressWarnings(as.integer(c(self$options$abs, self$options$ord)))
      tryCatch({
        FactoMineR::plot.HCPC(
          res.classif,
          axes = axes,
          choice = 'map',
          draw.tree = FALSE,
          new.plot = FALSE,
          title = 'Representation of the Stimuli According to Clusters'
        )
        TRUE
      }, error = function(e) FALSE)
    },

    #### Saved outputs ----

    .output = function(res.mfa) {
      nFactors_out <- min(
        suppressWarnings(as.integer(self$options$ncp)),
        private$.availableDimensions(res.mfa)
      )
      if (!is.finite(nFactors_out) || nFactors_out < 1L)
        return()

      if (self$results$newvar$isNotFilled()) {
        self$results$newvar$set(
          keys = seq_len(nFactors_out),
          titles = paste('Dim.', seq_len(nFactors_out)),
          descriptions = rep('MFA component', nFactors_out),
          measureTypes = rep('continuous', nFactors_out)
        )
        for (i in seq_len(nFactors_out))
          self$results$newvar$setValues(index = i, as.numeric(res.mfa$ind$coord[, i]))
        self$results$newvar$setRowNums(seq_len(nrow(self$data)))
      }
    },

    .output2 = function(res.classif) {
      if (is.null(res.classif) || is.null(res.classif$data.clust))
        return()

      output <- self$results$newvar2
      if (output$isNotFilled()) {
        output$set(
          keys = 1,
          titles = 'Cluster',
          descriptions = 'HCPC cluster in the retained MFA space',
          measureTypes = 'nominal'
        )
      }

      scores <- as.factor(res.classif$data.clust[, ncol(res.classif$data.clust)])
      output$setValues(index = 1, scores)
      output$setRowNums(seq_len(nrow(self$data)))
    },

    #### Data preparation ----

    .buildData = function() {
      active <- as.character(self$options$actvars)
      supplementary <- as.character(self$options$qualisup)
      selected <- c(active, supplementary)

      if (length(selected) == 0L)
        return(NULL)

      data <- as.data.frame(self$data[, selected, drop = FALSE])
      colnames(data) <- selected

      labels_var <- as.character(self$options$individus)
      if (length(labels_var) > 0L && !is.na(labels_var[1]) && nzchar(labels_var[1]))
        rownames(data) <- as.character(self$data[[labels_var[1]]])
      else
        rownames(data) <- as.character(seq_len(nrow(data)))

      data
    }
  )
)
