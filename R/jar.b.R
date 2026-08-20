# This file is a generated template, your changes will not be overwritten
JARClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "JARClass",
  inherit = JARBase,
  private = list(

    # ---------------------------------------------------------------------
    # Init + run
    # ---------------------------------------------------------------------

    .init = function() {
      if (is.null(self$data) || is.null(self$options$sensoatt)) {
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
      <b>What you should know before analysing JAR data in jamovi</b>
    </p>

    <div style='border-top: 1px solid #CBD8E8; margin-bottom: 12px;'></div>

    <p style='margin: 0 0 9px 0;'>
      <b>Purpose.</b>
      Just-About-Right (JAR) data combine sensory information with liking.
      They are used to identify deviations from an implicit sensory ideal,
      quantify their impact on liking, and determine which products are most
      affected by these deviations.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Data structure.</b>
      Each row should correspond to one subject evaluating one stimulus.
      Select one categorical <i>Stimulus Effect</i>, one categorical
      <i>Subject Effect</i>, one quantitative <i>Liking Variable</i>, and
      the nominal JAR sensory attributes.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>JAR coding.</b>
      The value entered in <i>Coding of the JAR level</i> must exactly match
      the category representing the ideal level in every selected JAR attribute.
      For the classical product-specific analysis, the other categories are
      grouped into <i>Below JAR</i> and <i>Above JAR</i> using the keyword
      patterns specified in the interface. Always inspect the
      <i>JAR Coding Used for the Classical Penalty Analysis</i> table before
      interpreting the penalties.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Two complementary views are provided.</b>
      The <i>global view</i> identifies sensory deviations that reduce liking
      across the entire product space. The <i>product-specific view</i> asks,
      for each product separately, whether consumers who perceive a given
      deviation also report lower liking than consumers who perceive the
      attribute as JAR.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Global view of the product space.</b>
      SensoMineR estimates multidimensional penalty coefficients using all
      products simultaneously. These coefficients describe the importance of
      each sensory deviation in the overall product space; they are therefore
      <i>not product-specific penalty estimates</i>. SEDA combines these global
      penalties with the frequency of each deviation within each product to
      identify which products are particularly exposed to globally important
      defects.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Product-specific classical penalty analysis.</b>
      For each product, attribute and direction (<i>Below JAR</i> or
      <i>Above JAR</i>), liking in the non-JAR group is compared with liking
      in the JAR group. The reported <i>Mean drop</i> is calculated as
      <i>mean liking at JAR - mean liking outside JAR</i>.
      A positive value therefore indicates a loss of liking associated with
      the deviation. SEDA also reports a 95% confidence interval and a p-value.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Thresholds.</b>
      The <i>Consumer threshold</i> and <i>Mean-drop threshold</i> are practical
      decision aids, not statistical significance thresholds. The commonly used
      consumer threshold of 20% can help identify frequent deviations, but a
      deviation affecting fewer consumers may still be statistically meaningful.
      Statistical evidence should therefore be considered separately through
      the confidence interval and p-value.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Recommended reading sequence.</b>
      Check the JAR coding first, then inspect the response distributions.
      Next identify important defects in the global product space and the
      products exposed to them. Then examine the product-specific penalties.
      Finish with the correspondence analysis as a multivariate synthesis of
      the product-by-defect structure.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Reading the product-specific penalty plots.</b>
      The horizontal axis represents the percentage of consumers reporting the
      deviation, whereas the vertical axis represents the mean drop in liking.
      Dashed reference lines correspond to the thresholds selected in the
      interface. Below-JAR and Above-JAR deviations are represented separately;
      an asterisk indicates p &lt; .05, and vertical intervals represent the
      95% confidence interval of the estimated mean drop.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Interpretation.</b>
      A practically important defect is generally one that is both frequent
      and associated with a substantial loss of liking. Frequency, effect size
      and statistical uncertainty provide different information and should be
      considered together rather than reduced to a single cutoff.
    </p>

    <p style='margin: 0;'>
      <b>Example.</b>
      Open the structured milkshake JAR dataset and use <b>jar</b> as the JAR
      level. <i>Balanced</i> is designed to remain relatively close to the ideal,
      whereas <i>SweetBoost</i>, <i>BerryFresh</i> and <i>VanillaRich</i>
      exhibit different structured deviations. Compare the global product-space
      diagnosis with the product-specific penalties to see how the two views
      provide complementary information.
    </p>

  </div>"
      )

      self$results$step1Guide$setContent(
        "
  <div style='margin: 6px 0 10px 0; padding: 9px 13px;
      background-color: #F4F7FB; border-left: 4px solid #6B9DE8;
      color: #333333; line-height: 1.4;'>
    <b style='color: #355F98;'>Step 1 — Check the JAR coding.</b>
    Verify that every response level is classified as <i>Below JAR</i>,
    <i>JAR</i> or <i>Above JAR</i>. If a level is shown as
    <i>Unclassified</i>, the corresponding attribute is intentionally excluded
    from the classical product-specific analysis until its coding is resolved.
  </div>"
      )

      self$results$step2Guide$setContent(
        "
  <div style='margin: 6px 0 10px 0; padding: 9px 13px;
      background-color: #F4F7FB; border-left: 4px solid #6B9DE8;
      color: #333333; line-height: 1.4;'>
    <b style='color: #355F98;'>Step 2 — Describe the JAR response distributions.</b>
    Read the percentages of consumers <i>Below JAR</i>, <i>JAR</i> and
    <i>Above JAR</i> for each product and attribute. These percentages describe
    how frequent a deviation is; they do not by themselves indicate that the
    deviation reduces liking.
  </div>"
      )

      self$results$step3Guide$setContent(
        "
  <div style='margin: 6px 0 10px 0; padding: 9px 13px;
      background-color: #F4F7FB; border-left: 4px solid #6B9DE8;
      color: #333333; line-height: 1.4;'>
    <b style='color: #355F98;'>Step 3 — Global penalty analysis.</b>
    These results describe the product space as a whole. The multidimensional
    penalty coefficients are estimated across all products and identify sensory
    deviations associated with lower liking globally. They are then combined
    with the frequency of each deviation within each product. A
    <i>Priority = Yes</i> result means that the selected consumer-frequency and
    mean-drop thresholds are reached and that the global penalty has p &lt; .05.
  </div>"
      )

      self$results$step4Guide$setContent(
        "
  <div style='margin: 6px 0 10px 0; padding: 9px 13px;
      background-color: #F4F7FB; border-left: 4px solid #6B9DE8;
      color: #333333; line-height: 1.4;'>
    <b style='color: #355F98;'>Step 4 — Product-specific penalty analysis.</b>
    These results are calculated separately for each product. A positive
    <i>Mean drop</i> means that consumers who rated the attribute Below JAR or
    Above JAR gave lower liking scores than consumers who rated it JAR.
    Interpret the percentage of consumers, the size of the mean drop, its
    confidence interval and the p-value together.
  </div>"
      )

      self$results$step5Guide$setContent(
        "
  <div style='margin: 6px 0 10px 0; padding: 9px 13px;
      background-color: #F4F7FB; border-left: 4px solid #6B9DE8;
      color: #333333; line-height: 1.4;'>
    <b style='color: #355F98;'>Step 5 — Multivariate synthesis of defects.</b>
    The correspondence analysis summarizes associations between products and
    non-JAR response categories. First read the statistical description of the
    products, then the factor map. The wide frequency table used to build the
    analysis is shown afterwards as a technical audit table. The statistical
    description in this synthesis intentionally uses a permissive exploratory
    threshold (50%); it should not be read as a conventional significance filter.
    Proximity on the map is exploratory and is not, by itself, evidence of a
    liking penalty.
  </div>"
      )
    },

    .run = function() {
      if (isTRUE(self$options$showCode))
        self$results$rCode$setContent(private$.code())

      if (is.null(self$options$prodeff) || is.null(self$options$paneff) ||
          is.null(self$options$likvar) || is.null(self$options$sensoatt) ||
          length(self$options$sensoatt) == 0L)
        return()

      private$.validateInputs()
      data <- private$.buildData()

      private$.validateRegex(self$options$lowPattern, "Below-JAR keywords")
      private$.validateRegex(self$options$highPattern, "Above-JAR keywords")

      res.jar <- tryCatch(private$.JAR(data), error = function(e) {
        jmvcore::reject(paste("JAR failed:", e$message)); NULL
      })
      if (is.null(res.jar)) return()

      res.freq <- tryCatch(private$.JARCA(data), error = function(e) {
        jmvcore::reject(paste("JAR correspondence analysis failed:", e$message)); NULL
      })
      if (is.null(res.freq)) return()

      res.classic <- tryCatch(private$.classicalPenalty(data), error = function(e) {
        jmvcore::reject(paste("Classical penalty analysis failed:", e$message)); NULL
      })
      if (is.null(res.classic)) return()

      res.ca <- tryCatch(
        FactoMineR::CA(res.freq$Frequency, graph = FALSE),
        error = function(e) NULL
      )
      self$results$step5Map$setState(res.ca)

      # Native SensoMineR global penalty context: one plot per product.
      stimuli <- colnames(res.jar$Frequency)
      for (nm in stimuli) {
        item <- self$results$step3GlobalPlots$addItem(key = nm)
        item$setTitle(nm)
        res.jar.product <- res.jar
        res.jar.product$Frequency <- res.jar$Frequency[, nm, drop = FALSE]
        item$setState(res.jar.product)
      }

      # Classical product-specific diagnostic: one plot per product.
      for (nm in names(res.classic$plots)) {
        item <- self$results$step4Plots$addItem(key = nm)
        item$setTitle(nm)
        item$setState(res.classic$plots[[nm]])
      }

      private$.populateFrequencyTable(res.freq$Frequency)
      private$.populateDescfreqTable(res.freq$res.descfreq)
      private$.populatePenaltyTable(res.jar$penalty2)
      private$.populateGlobalDiagnosticTable(res.jar)
      private$.populateCodingTable(res.classic$coding)
      private$.populateDistributionTable(res.classic$distribution)
      private$.populateClassicalPenaltyTable(res.classic$penalty)
    },

    # ---------------------------------------------------------------------
    # Core computations
    # ---------------------------------------------------------------------

    .JAR = function(data) {
      jarlev <- self$options$jarmod
      res.jar <- SensoMineR::JAR(
        data,
        col.p = 1,
        col.j = 2,
        col.pref = 3,
        jarlevel = jarlev
      )
      colnames(res.jar$penalty2) <- c("Penalty", "Std. Error", "Pr(>|t|)")
      return(res.jar)
    },

    .JARCA = function(data) {
      x <- data
      col.p <- 1
      col.j <- 2
      col.pref <- 3
      jarlevel <- self$options$jarmod

      fct.delete.first <- function(x) x[-1]
      ind.jar <- (1:ncol(x))[-c(col.p, col.j, col.pref)]

      for (i in ind.jar) {
        lev <- levels(as.factor(x[[i]]))
        if (!(jarlevel %in% lev))
          stop(paste0("JAR level '", jarlevel, "' was not found in attribute '", names(x)[i], "'."))
        x[[i]] <- factor(
          as.character(x[[i]]),
          levels = c(jarlevel, setdiff(lev, jarlevel))
        )
      }

      nbmod <- rep(0, ncol(x))
      for (j in ind.jar)
        nbmod[j] <- nlevels(x[, j]) - 1
      nbmodtot <- sum(nbmod)

      nommod <- rep("a", nbmodtot)
      ifin <- 0
      for (j in ind.jar) {
        ideb <- ifin + 1
        ifin <- ideb + nbmod[j] - 1
        npar <- nbmod[j] + 1
        nommod[ideb:ifin] <- paste0(names(x)[j], " = ", levels(x[[j]])[2:npar])
      }

      Frequency <- matrix(NA, nrow = nbmodtot, ncol = nlevels(x[, col.p]))
      for (j in seq_len(ncol(Frequency))) {
        Frequency[, j] <- unlist(lapply(
          lapply(x[x[, col.p] == levels(x[, col.p])[j], ind.jar, drop = FALSE], table),
          fct.delete.first
        ))
      }
      rownames(Frequency) <- nommod
      colnames(Frequency) <- levels(x[, col.p])

      res.descfreq <- FactoMineR::descfreq(t(Frequency), proba = 0.5)

      list(Frequency = Frequency, res.descfreq = res.descfreq)
    },

    .validateInputs = function() {
      prodeff <- as.character(self$options$prodeff)
      paneff <- as.character(self$options$paneff)
      likvar <- as.character(self$options$likvar)
      sensoatt <- as.character(self$options$sensoatt)

      scalar_ok <- function(x) length(x) == 1L && !is.na(x) && nzchar(x)
      if (!scalar_ok(prodeff))
        jmvcore::reject("Select one Stimulus Effect.")
      if (!scalar_ok(paneff))
        jmvcore::reject("Select one Subject Effect.")
      if (!scalar_ok(likvar))
        jmvcore::reject("Select one Liking Variable.")
      if (length(sensoatt) < 1L)
        jmvcore::reject("Select at least one JAR sensory attribute.")

      selected <- c(prodeff, paneff, likvar, sensoatt)
      if (anyDuplicated(selected))
        jmvcore::reject("Stimulus Effect, Subject Effect, Liking Variable and JAR attributes must be different variables.")

      if (anyNA(self$data[[prodeff]]))
        jmvcore::reject("Stimulus Effect contains missing values.")
      if (anyNA(self$data[[paneff]]))
        jmvcore::reject("Subject Effect contains missing values.")

      product <- droplevels(as.factor(self$data[[prodeff]]))
      subject <- droplevels(as.factor(self$data[[paneff]]))
      if (nlevels(product) < 2L)
        jmvcore::reject("Stimulus Effect must contain at least two observed products.")
      if (nlevels(subject) < 2L)
        jmvcore::reject("Subject Effect must contain at least two observed subjects.")

      liking <- self$data[[likvar]]
      if (!is.numeric(liking))
        jmvcore::reject("Liking Variable must be numeric.")
      if (sum(is.finite(liking)) < 4L)
        jmvcore::reject("Liking Variable must contain enough finite observations for penalty analysis.")

      jarlevel <- as.character(self$options$jarmod)
      if (length(jarlevel) != 1L || is.na(jarlevel) || !nzchar(jarlevel))
        jmvcore::reject("Coding of the JAR level cannot be empty.")

      # JAR attributes are intentionally nominal in SEDA. Because jamovi can
      # expose nominal and ordinal columns through compatible factor inputs,
      # enforce the measure contract explicitly in the backend as well.
      non_nominal <- character()
      for (nm in sensoatt) {
        x <- self$data[[nm]]
        measure_type <- attr(x, "measureType", exact = TRUE)
        is_ordinal <- is.ordered(x) ||
          (!is.null(measure_type) && identical(tolower(as.character(measure_type)[1L]), "ordinal"))
        is_explicit_non_nominal <- !is.null(measure_type) &&
          !identical(tolower(as.character(measure_type)[1L]), "nominal")
        if (is_ordinal || is_explicit_non_nominal ||
            (is.null(measure_type) && !(is.factor(x) || is.character(x))))
          non_nominal <- c(non_nominal, nm)
      }
      if (length(non_nominal) > 0L)
        jmvcore::reject(paste0(
          "JAR sensory attributes must be nominal variables. Change the measure type to Nominal for: ",
          paste(unique(non_nominal), collapse = ", "), "."
        ))

      missing_jar <- character()
      low_information <- character()
      for (nm in sensoatt) {
        x <- self$data[[nm]]
        if (!is.factor(x))
          x <- as.factor(x)
        lev <- levels(x)
        if (!(jarlevel %in% lev))
          missing_jar <- c(missing_jar, nm)
        observed <- unique(as.character(x[!is.na(x)]))
        if (length(observed) < 2L)
          low_information <- c(low_information, nm)
      }
      if (length(missing_jar) > 0L)
        jmvcore::reject(paste0(
          "The JAR level '", jarlevel, "' is missing from: ",
          paste(unique(missing_jar), collapse = ", "), "."
        ))
      if (length(low_information) > 0L)
        jmvcore::reject(paste0(
          "The following JAR attributes have fewer than two observed response levels: ",
          paste(unique(low_information), collapse = ", "), "."
        ))

      invisible(TRUE)
    },

    .validateRegex = function(pattern, label) {
      tryCatch(
        grepl(pattern, "test", perl = TRUE, ignore.case = TRUE),
        error = function(e) jmvcore::reject(paste0(label, " contains an invalid regular expression."))
      )
    },

    .normaliseLabel = function(x) {
      trimws(tolower(gsub("[^[:alnum:]]+", " ", x)))
    },

    .mapJarLevels = function(x) {
      jarlevel <- as.character(self$options$jarmod)
      lowPattern <- as.character(self$options$lowPattern)
      highPattern <- as.character(self$options$highPattern)
      lev <- levels(x)

      mapping <- rep(NA_character_, length(lev))
      names(mapping) <- lev

      jar.idx <- match(jarlevel, lev)
      if (is.na(jar.idx))
        stop(paste0("JAR level '", jarlevel, "' was not found."))
      mapping[jar.idx] <- "JAR"

      norm <- private$.normaliseLabel(lev)
      is.low <- grepl(lowPattern, norm, perl = TRUE, ignore.case = TRUE)
      is.high <- grepl(highPattern, norm, perl = TRUE, ignore.case = TRUE)

      mapping[is.low & !is.high] <- "Below JAR"
      mapping[is.high & !is.low] <- "Above JAR"

      # Ordered factors provide a safe fallback when labels themselves are not informative.
      unresolved <- is.na(mapping)
      if (is.ordered(x) && any(unresolved)) {
        idx <- seq_along(lev)
        mapping[unresolved & idx < jar.idx] <- "Below JAR"
        mapping[unresolved & idx > jar.idx] <- "Above JAR"
      }

      mapping
    },

    .classicalPenalty = function(data) {
      products <- levels(as.factor(data[[1]]))
      attributes <- names(data)[4:ncol(data)]
      liking <- suppressWarnings(as.numeric(data[[3]]))

      coding.rows <- list()
      distribution.rows <- list()
      penalty.rows <- list()
      plot.rows <- setNames(vector("list", length(products)), products)

      ic <- 0L
      id <- 0L
      ip <- 0L

      for (attribute in attributes) {
        x <- data[[attribute]]
        if (!is.factor(x))
          x <- as.factor(x)
        mapping <- private$.mapJarLevels(x)

        for (lev in names(mapping)) {
          ic <- ic + 1L
          coding.rows[[ic]] <- data.frame(
            attribute = attribute,
            level = lev,
            group = ifelse(is.na(mapping[[lev]]), "Unclassified", mapping[[lev]]),
            stringsAsFactors = FALSE
          )
        }

        # If one non-JAR level remains unresolved, keep the global SensoMineR
        # analysis but do not manufacture a classical Below/Above result.
        if (any(is.na(mapping[names(mapping) != self$options$jarmod])))
          next

        side.all <- unname(mapping[as.character(x)])

        for (product in products) {
          rows.product <- !is.na(data[[1]]) & as.character(data[[1]]) == product
          side <- side.all[rows.product]
          y <- liking[rows.product]

          n.valid.side <- sum(!is.na(side))
          if (n.valid.side == 0L)
            next

          n.below <- sum(side == "Below JAR", na.rm = TRUE)
          n.jar <- sum(side == "JAR", na.rm = TRUE)
          n.above <- sum(side == "Above JAR", na.rm = TRUE)

          id <- id + 1L
          distribution.rows[[id]] <- data.frame(
            product = product,
            attribute = attribute,
            belowPct = 100 * n.below / n.valid.side,
            jarPct = 100 * n.jar / n.valid.side,
            abovePct = 100 * n.above / n.valid.side,
            n = n.valid.side,
            stringsAsFactors = FALSE
          )

          for (direction in c("Below JAR", "Above JAR")) {
            n.direction <- sum(side == direction, na.rm = TRUE)
            consumerPct <- 100 * n.direction / n.valid.side

            use <- !is.na(y) & side %in% c("JAR", direction)
            grp <- factor(side[use], levels = c("JAR", direction))
            yy <- y[use]

            mean.jar <- if (any(grp == "JAR")) mean(yy[grp == "JAR"], na.rm = TRUE) else NA_real_
            mean.nonjar <- if (any(grp == direction)) mean(yy[grp == direction], na.rm = TRUE) else NA_real_

            mean.drop <- stderr <- pvalue <- ci.low <- ci.high <- NA_real_

            if (sum(grp == "JAR") >= 2L && sum(grp == direction) >= 2L) {
              fit <- stats::lm(yy ~ grp)
              co <- summary(fit)$coefficients
              if (nrow(co) >= 2L) {
                estimate <- unname(co[2, 1])
                stderr <- unname(co[2, 2])
                pvalue <- unname(co[2, 4])
                mean.drop <- -estimate
                tcrit <- stats::qt(0.975, df = stats::df.residual(fit))
                ci.low <- mean.drop - tcrit * stderr
                ci.high <- mean.drop + tcrit * stderr
              }
            }

            ip <- ip + 1L
            row <- data.frame(
              product = product,
              attribute = attribute,
              direction = direction,
              consumerPct = consumerPct,
              meanJar = mean.jar,
              meanNonJar = mean.nonjar,
              meanDrop = mean.drop,
              ciLow = ci.low,
              ciHigh = ci.high,
              stderr = stderr,
              pvalue = pvalue,
              stringsAsFactors = FALSE
            )
            penalty.rows[[ip]] <- row
            plot.rows[[product]][[length(plot.rows[[product]]) + 1L]] <- row
          }
        }
      }

      coding <- if (length(coding.rows)) do.call(rbind, coding.rows) else data.frame()
      distribution <- if (length(distribution.rows)) do.call(rbind, distribution.rows) else data.frame()
      penalty <- if (length(penalty.rows)) do.call(rbind, penalty.rows) else data.frame()

      plots <- lapply(plot.rows, function(x) {
        if (length(x) == 0L) return(data.frame())
        do.call(rbind, x)
      })

      list(coding = coding, distribution = distribution, penalty = penalty, plots = plots)
    },

    # ---------------------------------------------------------------------
    # Tables
    # ---------------------------------------------------------------------

    .populateFrequencyTable = function(Frequency) {
      tab <- t(Frequency)
      stimuli <- rownames(tab)
      defects <- colnames(tab)
      table <- self$results$step5Frequency

      table$addColumn(name = "stimulus", title = "", type = "text")
      for (d in defects)
        table$addColumn(name = d, title = d, type = "integer")

      for (i in seq_along(stimuli)) {
        row <- list(stimulus = stimuli[i])
        for (d in defects)
          row[[d]] <- tab[i, d]
        table$addRow(rowKey = i, values = row)
      }
    },

    .populateDescfreqTable = function(desc) {
      if (is.null(desc)) return()
      dfres_nonnull <- desc[!vapply(desc, is.null, logical(1))]
      if (length(dfres_nonnull) == 0L) return()

      tabs <- lapply(names(dfres_nonnull), function(nm) {
        x <- as.data.frame(dfres_nonnull[[nm]])
        if (nrow(x) == 0L) return(NULL)
        out <- cbind(product = rep(nm, nrow(x)), defect = rownames(x), x)
        rownames(out) <- NULL
        out
      })
      tabs <- Filter(Negate(is.null), tabs)
      if (length(tabs) == 0L) return()

      tab <- do.call(rbind, tabs)
      table <- self$results$step5Description

      for (i in seq_len(nrow(tab))) {
        table$addRow(rowKey = i, values = list(
          product = as.character(tab[i, 1]),
          defect = as.character(tab[i, 2]),
          internper = tab[i, 3],
          globper = tab[i, 4],
          internfreq = tab[i, 5],
          globfreq = tab[i, 6],
          pvaluedfres = tab[i, 7],
          vtest = round(tab[i, 8], digits = 2)
        ))
      }
    },

    .populatePenaltyTable = function(penalty) {
      if (is.null(penalty) || nrow(penalty) == 0L) return()
      table <- self$results$step3GlobalPenalty

      for (i in seq_len(nrow(penalty))) {
        table$addRow(rowKey = i, values = list(
          defect = rownames(penalty)[i],
          meandrop = -penalty[i, 1],
          stderr = penalty[i, 2],
          pvalue = penalty[i, 3]
        ))
      }
    },

    .populateGlobalDiagnosticTable = function(res.jar) {
      penalty <- res.jar$penalty2
      freq <- res.jar$Frequency
      if (is.null(penalty) || is.null(freq)) return()

      table <- self$results$step3ProductExposure
      consumer.threshold <- as.numeric(self$options$consumerThreshold)
      penalty.threshold <- as.numeric(self$options$penaltyThreshold)
      k <- 0L

      for (j in seq_len(ncol(freq))) {
        for (i in seq_len(nrow(freq))) {
          k <- k + 1L
          mean.drop <- -penalty[i, 1]
          pvalue <- penalty[i, 3]
          pct <- freq[i, j]
          priority <- is.finite(pct) && is.finite(mean.drop) && is.finite(pvalue) &&
            pct >= consumer.threshold && mean.drop >= penalty.threshold && pvalue < 0.05

          table$addRow(rowKey = k, values = list(
            product = colnames(freq)[j],
            defect = rownames(freq)[i],
            consumers = pct,
            meandrop = mean.drop,
            pvalue = pvalue,
            priority = ifelse(priority, "Yes", "")
          ))
        }
      }
    },

    .populateCodingTable = function(df) {
      if (is.null(df) || nrow(df) == 0L) return()
      table <- self$results$step1Coding
      for (i in seq_len(nrow(df))) {
        table$addRow(rowKey = i, values = list(
          attribute = df$attribute[i],
          level = df$level[i],
          group = df$group[i]
        ))
      }
    },

    .populateDistributionTable = function(df) {
      if (is.null(df) || nrow(df) == 0L) return()
      table <- self$results$step2Distribution
      for (i in seq_len(nrow(df))) {
        table$addRow(rowKey = i, values = list(
          product = df$product[i],
          attribute = df$attribute[i],
          below = df$belowPct[i],
          jar = df$jarPct[i],
          above = df$abovePct[i],
          n = df$n[i]
        ))
      }
    },

    .populateClassicalPenaltyTable = function(df) {
      if (is.null(df) || nrow(df) == 0L) return()
      table <- self$results$step4Penalty
      for (i in seq_len(nrow(df))) {
        table$addRow(rowKey = i, values = list(
          product = df$product[i],
          attribute = df$attribute[i],
          direction = df$direction[i],
          consumers = df$consumerPct[i],
          meanjar = df$meanJar[i],
          meannonjar = df$meanNonJar[i],
          meandrop = df$meanDrop[i],
          cilow = df$ciLow[i],
          cihigh = df$ciHigh[i],
          pvalue = df$pvalue[i]
        ))
      }
    },

    # ---------------------------------------------------------------------
    # Plots
    # ---------------------------------------------------------------------

    .plotboth = function(image, ...) {
      if (is.null(self$options$sensoatt)) return()
      res.ca <- image$state
      if (is.null(res.ca)) return()

      FactoMineR::plot.CA(
        res.ca,
        title = "Representation of the Products According to Defects",
        graph.type = "classic",
        new.plot = FALSE
      )
      TRUE
    },

    .plotpenalty = function(image, ...) {
      if (is.null(self$options$sensoatt)) return()
      res.jar.product <- image$state
      if (is.null(res.jar.product) || is.null(res.jar.product$Frequency) ||
          ncol(res.jar.product$Frequency) != 1L)
        return()

      name.prod <- colnames(res.jar.product$Frequency)[1]
      if (is.null(name.prod) || is.na(name.prod) || !nzchar(name.prod))
        return()

      tryCatch({
        SensoMineR::plot.JAR(
          res.jar.product,
          name.prod = name.prod,
          model = 2,
          confidence = TRUE,
          level = 0.05
        )
        TRUE
      }, error = function(e) FALSE)
    },

    .plotClassical = function(image, ...) {
      df <- image$state
      if (is.null(df) || nrow(df) == 0L) return()

      ok <- is.finite(df$consumerPct) & is.finite(df$meanDrop)
      df <- df[ok, , drop = FALSE]
      if (nrow(df) == 0L) return()

      consumer.threshold <- as.numeric(self$options$consumerThreshold)
      penalty.threshold <- as.numeric(self$options$penaltyThreshold)

      xmax <- max(c(50, consumer.threshold + 5, df$consumerPct * 1.15), na.rm = TRUE)
      ymin <- min(c(0, df$ciLow, df$meanDrop), na.rm = TRUE)
      ymax <- max(c(penalty.threshold, df$ciHigh, df$meanDrop), na.rm = TRUE)
      yrange <- ymax - ymin
      if (!is.finite(yrange) || yrange <= 0) yrange <- 1

      graphics::plot(
        NA,
        xlim = c(0, min(100, xmax)),
        ylim = c(ymin - 0.08 * yrange, ymax + 0.15 * yrange),
        xlab = "Consumers concerned (%)",
        ylab = "Mean drop in liking",
        main = unique(df$product)[1]
      )
      graphics::abline(v = consumer.threshold, lty = 2)
      graphics::abline(h = penalty.threshold, lty = 2)
      graphics::abline(h = 0, lty = 3)

      pch <- ifelse(df$direction == "Below JAR", 1, 2)
      graphics::points(df$consumerPct, df$meanDrop, pch = pch)

      ci.ok <- is.finite(df$ciLow) & is.finite(df$ciHigh)
      if (any(ci.ok)) {
        graphics::arrows(
          df$consumerPct[ci.ok], df$ciLow[ci.ok],
          df$consumerPct[ci.ok], df$ciHigh[ci.ok],
          angle = 90, code = 3, length = 0.04
        )
      }

      labels <- paste0(
        df$attribute,
        ifelse(df$direction == "Below JAR", " (-)", " (+)"),
        ifelse(is.finite(df$pvalue) & df$pvalue < 0.05, " *", "")
      )
      graphics::text(df$consumerPct, df$meanDrop, labels = labels, pos = 4, cex = 0.72)
      graphics::legend(
        "topright",
        legend = c("Below JAR", "Above JAR", "* p < .05"),
        pch = c(1, 2, NA),
        bty = "n"
      )
      TRUE
    },

    # ---------------------------------------------------------------------
    # Rj code
    # ---------------------------------------------------------------------

    .code = function() {
      r_literal <- function(value) {
        if (is.null(value)) return("NULL")
        paste(deparse(value, width.cutoff = 500L), collapse = "\n")
      }

      prod <- as.character(self$options$prodeff)
      pane <- as.character(self$options$paneff)
      liking <- as.character(self$options$likvar)
      senso <- as.character(self$options$sensoatt)

      if (length(prod) == 0L || is.na(prod[1]) || !nzchar(prod[1]))
        return("# Select a stimulus variable to generate the JAR code.")
      if (length(pane) == 0L || is.na(pane[1]) || !nzchar(pane[1]))
        return("# Select a subject variable to generate the JAR code.")
      if (length(liking) == 0L || is.na(liking[1]) || !nzchar(liking[1]))
        return("# Select a liking variable to generate the JAR code.")
      if (length(senso) == 0L)
        return("# Select at least one JAR sensory attribute to generate the JAR code.")

      variables <- c(prod[1], pane[1], liking[1], senso)
      jarlevel <- as.character(self$options$jarmod)[1]
      lowPattern <- as.character(self$options$lowPattern)[1]
      highPattern <- as.character(self$options$highPattern)[1]
      consumerThreshold <- as.numeric(self$options$consumerThreshold)
      penaltyThreshold <- as.numeric(self$options$penaltyThreshold)

      # Resolve the current products now so that the generated Rj script contains
      # one explicit top-level plotting command per product. Rj captures these
      # commands as separate graphical outputs, whereas loops/mfrow may collapse
      # several plots onto the same device.
      productValues <- tryCatch({
        x <- data.frame(self$data[, prod[1], drop = FALSE])[[1]]
        if (is.factor(x)) {
          levels(droplevels(x))
        } else {
          x <- as.character(x)
          unique(x[!is.na(x) & nzchar(x)])
        }
      }, error = function(e) character())

      globalPlotCode <- if (length(productValues) > 0L) {
        unlist(lapply(productValues, function(product) {
          c(
            paste0(
              "plot.JAR(res_JAR, name.prod = ", r_literal(product),
              ", model = 2, confidence = TRUE, level = 0.05)"
            ),
            ""
          )
        }), use.names = FALSE)
      } else {
        c("# No product level was available when this code was generated.", "")
      }

      classicalPlotCalls <- if (length(productValues) > 0L) {
        unlist(lapply(productValues, function(product) {
          c(paste0("plot_classical_JAR(", r_literal(product), ")"), "")
        }), use.names = FALSE)
      } else {
        c("# No product level was available when this code was generated.", "")
      }

      code <- c(
        "library(SensoMineR)",
        "library(FactoMineR)",
        "",
        "# This script can be pasted directly into the jamovi Rj Editor.",
        "# The dataset open in jamovi is available as data.",
        "",
        paste0("data_JAR <- data[, ", r_literal(variables), ", drop = FALSE]"),
        "data_JAR[[1]] <- as.factor(data_JAR[[1]])",
        "data_JAR[[2]] <- as.factor(data_JAR[[2]])",
        "ordinal_JAR <- names(data_JAR)[4:ncol(data_JAR)][vapply(data_JAR[4:ncol(data_JAR)], function(x) {",
        "  mt <- attr(x, \"measureType\", exact = TRUE)",
        "  is.ordered(x) || (!is.null(mt) && identical(tolower(as.character(mt)[1]), \"ordinal\"))",
        "}, logical(1))]",
        "if (length(ordinal_JAR) > 0) stop(paste(\"JAR sensory attributes must be nominal variables. Change to Nominal:\", paste(ordinal_JAR, collapse = \", \")))",
        "data_JAR[4:ncol(data_JAR)] <- lapply(data_JAR[4:ncol(data_JAR)], as.factor)",
        "names(data_JAR)[1:3] <- c(\".Stimulus\", \".Subject\", \".Liking\")",
        "",
        paste0("jar_level <- ", r_literal(jarlevel)),
        paste0("below_pattern <- ", r_literal(lowPattern)),
        paste0("above_pattern <- ", r_literal(highPattern)),
        paste0("consumer_threshold <- ", r_literal(consumerThreshold)),
        paste0("mean_drop_threshold <- ", r_literal(penaltyThreshold)),
        "",
        "# ------------------------------------------------------------------",
        "# 1. Global multidimensional penalty analysis (all products)",
        "# ------------------------------------------------------------------",
        "res_JAR <- JAR(data_JAR, col.p = 1, col.j = 2, col.pref = 3, jarlevel = jar_level)",
        "global_penalty <- data.frame(",
        "  Defect = rownames(res_JAR$penalty2),",
        "  MeanDrop = -res_JAR$penalty2[, 1],",
        "  StdError = res_JAR$penalty2[, 2],",
        "  p = res_JAR$penalty2[, 3],",
        "  row.names = NULL",
        ")",
        "global_penalty",
        "",
        "global_diagnostic <- do.call(rbind, lapply(seq_len(ncol(res_JAR$Frequency)), function(j) {",
        "  data.frame(",
        "    Product = colnames(res_JAR$Frequency)[j],",
        "    Defect = rownames(res_JAR$Frequency),",
        "    ConsumersPct = res_JAR$Frequency[, j],",
        "    GlobalMeanDrop = -res_JAR$penalty2[, 1],",
        "    p = res_JAR$penalty2[, 3],",
        "    row.names = NULL",
        "  )",
        "}))",
        "global_diagnostic$Priority <- with(global_diagnostic,",
        "  ConsumersPct >= consumer_threshold & GlobalMeanDrop >= mean_drop_threshold & p < 0.05)",
        "global_diagnostic",
        "",
        "# Native SensoMineR plots: one explicit Rj output per product.",
        "# The calls are intentionally not placed in a loop or mfrow layout.",
        "products_JAR <- colnames(res_JAR$Frequency)",
        globalPlotCode,
        "# ------------------------------------------------------------------",
        "# 2. Classical product-specific penalty analysis",
        "# ------------------------------------------------------------------",
        "normalise_label <- function(x) trimws(tolower(gsub(\"[^[:alnum:]]+\", \" \", x)))",
        "map_jar_levels <- function(x) {",
        "  lev <- levels(x)",
        "  map <- setNames(rep(NA_character_, length(lev)), lev)",
        "  jar_i <- match(jar_level, lev)",
        "  if (is.na(jar_i)) stop(paste(\"JAR level not found in\", deparse(substitute(x))))",
        "  map[jar_i] <- \"JAR\"",
        "  z <- normalise_label(lev)",
        "  lo <- grepl(below_pattern, z, perl = TRUE, ignore.case = TRUE)",
        "  hi <- grepl(above_pattern, z, perl = TRUE, ignore.case = TRUE)",
        "  map[lo & !hi] <- \"Below JAR\"",
        "  map[hi & !lo] <- \"Above JAR\"",
        "  map",
        "}",
        "",
        "attributes_JAR <- names(data_JAR)[4:ncol(data_JAR)]",
        "coding_JAR <- do.call(rbind, lapply(attributes_JAR, function(a) {",
        "  x <- data_JAR[[a]]",
        "  if (!is.factor(x)) x <- as.factor(x)",
        "  m <- map_jar_levels(x)",
        "  data.frame(Attribute = a, Level = names(m), Group = ifelse(is.na(m), \"Unclassified\", m))",
        "}))",
        "coding_JAR",
        "",
        "distribution_list <- list()",
        "penalty_list <- list()",
        "k <- 0L",
        "for (a in attributes_JAR) {",
        "  x <- data_JAR[[a]]",
        "  if (!is.factor(x)) x <- as.factor(x)",
        "  m <- map_jar_levels(x)",
        "  if (any(is.na(m[names(m) != jar_level]))) next",
        "  side_all <- unname(m[as.character(x)])",
        "  for (product_JAR in levels(data_JAR[[1]])) {",
        "    keep_product <- data_JAR[[1]] == product_JAR",
        "    side <- side_all[keep_product]",
        "    y <- as.numeric(data_JAR[[3]][keep_product])",
        "    n <- sum(!is.na(side))",
        "    distribution_list[[length(distribution_list) + 1L]] <- data.frame(",
        "      Product = product_JAR, Attribute = a,",
        "      BelowPct = 100 * sum(side == \"Below JAR\", na.rm = TRUE) / n,",
        "      JARPct = 100 * sum(side == \"JAR\", na.rm = TRUE) / n,",
        "      AbovePct = 100 * sum(side == \"Above JAR\", na.rm = TRUE) / n, N = n)",
        "    for (direction in c(\"Below JAR\", \"Above JAR\")) {",
        "      use <- !is.na(y) & side %in% c(\"JAR\", direction)",
        "      grp <- factor(side[use], levels = c(\"JAR\", direction))",
        "      yy <- y[use]",
        "      pct <- 100 * sum(side == direction, na.rm = TRUE) / n",
        "      mj <- if (any(grp == \"JAR\")) mean(yy[grp == \"JAR\"]) else NA_real_",
        "      mn <- if (any(grp == direction)) mean(yy[grp == direction]) else NA_real_",
        "      drop <- se <- p <- lo <- hi <- NA_real_",
        "      if (sum(grp == \"JAR\") >= 2 && sum(grp == direction) >= 2) {",
        "        fit <- lm(yy ~ grp)",
        "        co <- summary(fit)$coefficients[2, ]",
        "        drop <- -co[1]; se <- co[2]; p <- co[4]",
        "        tc <- qt(0.975, df.residual(fit)); lo <- drop - tc * se; hi <- drop + tc * se",
        "      }",
        "      k <- k + 1L",
        "      penalty_list[[k]] <- data.frame(Product = product_JAR, Attribute = a, Direction = direction,",
        "        ConsumersPct = pct, MeanJAR = mj, MeanNonJAR = mn, MeanDrop = drop,",
        "        CILow = lo, CIHigh = hi, p = p)",
        "    }",
        "  }",
        "}",
        "distribution_JAR <- do.call(rbind, distribution_list)",
        "classical_penalty_JAR <- do.call(rbind, penalty_list)",
        "distribution_JAR",
        "classical_penalty_JAR",
        "",
        "# Product-specific diagnostic plot helper.",
        "# Each call below is a separate top-level Rj expression, so each product",
        "# receives its own graphical output instead of sharing/overwriting a device.",
        "plot_classical_JAR <- function(product_JAR) {",
        "  d <- classical_penalty_JAR[classical_penalty_JAR$Product == product_JAR &",
        "    is.finite(classical_penalty_JAR$MeanDrop), , drop = FALSE]",
        "  if (nrow(d) == 0) return(invisible(NULL))",
        "  ylim <- range(c(0, mean_drop_threshold, d$CILow, d$CIHigh), finite = TRUE)",
        "  plot(NA, xlim = c(0, 100), ylim = ylim, xlab = \"Consumers concerned (%)\",",
        "    ylab = \"Mean drop in liking\", main = product_JAR)",
        "  abline(v = consumer_threshold, lty = 2)",
        "  abline(h = mean_drop_threshold, lty = 2)",
        "  abline(h = 0, lty = 3)",
        "  pch <- ifelse(d$Direction == \"Below JAR\", 1, 2)",
        "  points(d$ConsumersPct, d$MeanDrop, pch = pch)",
        "  ok <- is.finite(d$CILow) & is.finite(d$CIHigh)",
        "  if (any(ok)) arrows(d$ConsumersPct[ok], d$CILow[ok], d$ConsumersPct[ok], d$CIHigh[ok],",
        "    angle = 90, code = 3, length = 0.04)",
        "  labs <- paste0(d$Attribute, ifelse(d$Direction == \"Below JAR\", \" (-)\", \" (+)\"),",
        "    ifelse(is.finite(d$p) & d$p < 0.05, \" *\", \"\"))",
        "  text(d$ConsumersPct, d$MeanDrop, labels = labs, pos = 4, cex = 0.7)",
        "  legend(\"topright\", legend = c(\"Below JAR\", \"Above JAR\", \"* p < .05\"),",
        "    pch = c(1, 2, NA), bty = \"n\")",
        "  invisible(NULL)",
        "}",
        "",
        classicalPlotCalls,
        "# ------------------------------------------------------------------",
        "# 3. Multivariate product × defect map",
        "# ------------------------------------------------------------------",
        "data_JAR_CA <- data_JAR",
        "jar_columns <- 4:ncol(data_JAR_CA)",
        "for (j in jar_columns) {",
        "  lev <- levels(as.factor(data_JAR_CA[[j]]))",
        "  if (!(jar_level %in% lev)) stop(paste0(\"JAR level missing from \", names(data_JAR_CA)[j]))",
        "  data_JAR_CA[[j]] <- factor(as.character(data_JAR_CA[[j]]),",
        "    levels = c(jar_level, setdiff(lev, jar_level)))",
        "}",
        "nonjar_levels <- unlist(Map(function(attribute, x) {",
        "  paste0(attribute, \" = \", levels(x)[-1])",
        "}, names(data_JAR_CA)[jar_columns], data_JAR_CA[jar_columns]), use.names = FALSE)",
        "frequency_counts <- matrix(0, nrow = length(nonjar_levels), ncol = nlevels(data_JAR_CA[[1]]),",
        "  dimnames = list(nonjar_levels, levels(data_JAR_CA[[1]])))",
        "for (k in seq_len(nlevels(data_JAR_CA[[1]]))) {",
        "  rows_k <- data_JAR_CA[[1]] == levels(data_JAR_CA[[1]])[k]",
        "  frequency_counts[, k] <- unlist(lapply(data_JAR_CA[rows_k, jar_columns, drop = FALSE],",
        "    function(x) table(x)[-1]))",
        "}",
        "res_descfreq_JAR <- descfreq(t(frequency_counts), proba = 0.5)",
        "res_CA_JAR <- CA(frequency_counts, graph = FALSE)",
        "plot.CA(res_CA_JAR, title = \"Representation of the Products According to Defects\",",
        "  graph.type = \"classic\", new.plot = FALSE)"
      )

      paste(code, collapse = "\n")
    },

    .buildData = function() {
      dataprodeff <- data.frame(self$data[, self$options$prodeff, drop = FALSE])
      colnames(dataprodeff) <- self$options$prodeff
      datapaneff <- data.frame(self$data[, self$options$paneff, drop = FALSE])
      colnames(datapaneff) <- self$options$paneff
      datalikvar <- data.frame(self$data[, self$options$likvar, drop = FALSE])
      colnames(datalikvar) <- self$options$likvar
      datasensoatt <- data.frame(self$data[, self$options$sensoatt, drop = FALSE])
      colnames(datasensoatt) <- self$options$sensoatt
      data.frame(dataprodeff, datapaneff, datalikvar, datasensoatt)
    }
  )
)
