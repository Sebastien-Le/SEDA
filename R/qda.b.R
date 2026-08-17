# This file is a generated template, your changes will not be overwritten
QDAClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "QDAClass",
  inherit = QDABase,
  private = list(
    
    #---------------------------------------------
    #### Init + run functions ----
    
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
      <b>What you should know before analyzing QDA data in jamovi</b>
    </p>

    <div style='border-top: 1px solid #CBD8E8; margin-bottom: 12px;'></div>

    <p style='margin: 0 0 9px 0;'>
      <b>Purpose.</b>
      Quantitative Descriptive Analysis (QDA) describes a set of stimuli using
      a common list of sensory attributes rated quantitatively by subjects.
      The analysis first identifies the attributes that discriminate the
      stimuli and then determines which attributes characterize each stimulus.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Data structure.</b>
      Select one categorical variable as the <i>Stimulus Effect</i>, one
      categorical variable as the <i>Subject Effect</i>, and one or more
      quantitative variables as <i>Sensory Attributes</i>. Each row should
      correspond to an assessment of a stimulus by a subject.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Analysis.</b>
      For each sensory attribute, SEDA tests the stimulus effect while taking
      the subject effect into account. Only attributes whose stimulus effect
      is significant at the selected <i>Significance threshold</i> are retained
      for the detailed characterization of the stimuli.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Interpretation.</b>
      Read the analysis in two steps. First identify the sensory dimensions
      that discriminate the stimuli. Then use the stimulus-description table
      to determine the direction and strength of each characterization. A
      positive V-test indicates a higher-than-overall level of the attribute,
      whereas a negative V-test indicates a lower-than-overall level.
    </p>

    <p style='margin: 0;'>
      <b>Example.</b>
      Open the <b>sensochoc</b> dataset, select the stimulus variable, the
      subject variable, and the sensory attributes. With the default threshold,
      for example, <b>choc1</b> is characterized by high bitterness and low
      sweetness.
    </p>

  </div>"
      )

      self$results$step1Guide$setContent(
        "
  <div style='margin: 6px 0 10px 0; padding: 9px 13px;
      background-color: #F4F7FB; border-left: 4px solid #6B9DE8;
      color: #333333; line-height: 1.4;'>
    <b style='color: #355F98;'>Step 1 — Identify the sensory dimensions.</b>
    Start with the global stimulus test for each sensory attribute. Attributes
    retained here are those for which the stimuli differ significantly at the
    selected threshold. This step determines which dimensions are worth using
    for the detailed sensory characterization.
  </div>"
      )

      self$results$step2Guide$setContent(
        "
  <div style='margin: 6px 0 10px 0; padding: 9px 13px;
      background-color: #F4F7FB; border-left: 4px solid #6B9DE8;
      color: #333333; line-height: 1.4;'>
    <b style='color: #355F98;'>Step 2 — Characterize each stimulus.</b>
    The coefficient and adjusted mean describe the direction and magnitude of
    the stimulus profile on each retained attribute. The V-test standardizes
    this characterization, while the p-value quantifies its statistical
    evidence. Positive and negative V-tests correspond respectively to
    higher- and lower-than-overall attribute levels.
  </div>"
      )
    },

    .run = function() {
      if (is.null(self$options$sensoatt)) return()
      if (is.null(self$options$prodeff))  return()
      if (is.null(self$options$paneff))   return()

      if (isTRUE(self$options$showCode))
        self$results$code$setContent(private$.code())

      private$.validateInputs()
      
      data_eff <- self$data[, c(self$options$prodeff, self$options$paneff), drop = FALSE]
      for (nm in names(data_eff))
        data_eff[[nm]] <- as.factor(data_eff[[nm]])

      # Use safe internal names for the two effects. This mirrors the Rj code
      # and prevents spaces or symbols in jamovi variable names from breaking
      # the ANOVA formula used by the historical QDA engine.
      names(data_eff) <- c(".Stimulus", ".Subject")
      
      data_att <- self$data[, self$options$sensoatt, drop = FALSE]
      
      if (!all(vapply(data_att, is.numeric, logical(1)))) {
        jmvcore::reject("All sensory attributes must be numeric")
        return()
      }
      
      data <- data.frame(data_eff, data_att, check.names = FALSE)
      
      res.decat <- tryCatch(
        private$.decat(data),
        error = function(e) {
          jmvcore::reject(paste("QDA analysis failed:", e$message))
          NULL
        }
      )
      
      if (is.null(res.decat)) return()
      
      ## Le test global F est la porte d'entrée
      if (is.null(res.decat$resF) || !is.data.frame(res.decat$resF) || nrow(res.decat$resF) == 0) {
        jmvcore::reject("No sensory attribute is discriminant at the selected significance threshold.")
        return()
      }
      
      private$.printresFTable(res.decat$resF)
      
      ## Si aucun tableau de tests t n'est disponible, on s'arrête là
      if (is.null(res.decat$resT) || length(res.decat$resT) == 0)
        return()
      
      ## Trouver le premier produit avec au moins un descripteur significatif
      j <- 1
      while (j <= length(res.decat$resT) && nrow(res.decat$resT[[j]]) == 0)
        j <- j + 1
      
      ## Aucun descripteur significatif au niveau des tests t
      if (j > length(res.decat$resT))
        return()
      
      tab <- cbind(names(res.decat$resT)[j], res.decat$resT[[j]])
      tab[, 6] <- as.factor(as.vector(rownames(tab)))
      colnames(tab)[1] <- "Product"
      
      if (j < length(res.decat$resT)) {
        for (i in (j + 1):length(res.decat$resT)) {
          if (nrow(res.decat$resT[[i]]) != 0) {
            pretab <- cbind(names(res.decat$resT)[i], res.decat$resT[[i]])
            colnames(pretab)[1] <- "Product"
            pretab[, 6] <- as.factor(as.vector(rownames(pretab)))
            tab <- rbind(tab, pretab)
          }
        }
      }
      
      rownames(tab) <- NULL
      tab <- tab[, c(1, 6, 2, 3, 4, 5)]
      colnames(tab)[2] <- "Descriptor"
      
      if (length(self$options$sensoatt) == 1)
        tab[, 2] <- self$options$sensoatt
      
      private$.printresTable(tab)
    },
    

    .validateInputs = function() {
      prodeff <- as.character(self$options$prodeff)
      paneff <- as.character(self$options$paneff)
      sensoatt <- as.character(self$options$sensoatt)

      if (length(prodeff) != 1L || is.na(prodeff) || !nzchar(prodeff))
        jmvcore::reject("Select one Stimulus Effect.")
      if (length(paneff) != 1L || is.na(paneff) || !nzchar(paneff))
        jmvcore::reject("Select one Subject Effect.")
      if (identical(prodeff, paneff))
        jmvcore::reject("Stimulus Effect and Subject Effect must be different variables.")
      if (length(sensoatt) < 1L)
        jmvcore::reject("Select at least one sensory attribute.")
      if (prodeff %in% sensoatt || paneff %in% sensoatt)
        jmvcore::reject("Stimulus Effect, Subject Effect and Sensory Attributes must be selected from different variables.")
      if (anyDuplicated(sensoatt))
        jmvcore::reject("The same sensory attribute cannot be selected more than once.")

      threshold <- suppressWarnings(as.numeric(self$options$threshold))
      if (length(threshold) != 1L || !is.finite(threshold) || threshold <= 0 || threshold >= 100)
        jmvcore::reject("Significance threshold (%) must be greater than 0 and lower than 100.")

      stimulus <- droplevels(as.factor(self$data[[prodeff]]))
      subject <- droplevels(as.factor(self$data[[paneff]]))
      if (nlevels(stimulus) < 2L)
        jmvcore::reject("Stimulus Effect must contain at least two observed levels.")
      if (nlevels(subject) < 2L)
        jmvcore::reject("Subject Effect must contain at least two observed levels.")

      bad <- character()
      for (nm in sensoatt) {
        x <- self$data[[nm]]
        if (!is.numeric(x)) {
          bad <- c(bad, nm)
          next
        }
        finite <- x[is.finite(x)]
        if (length(finite) < 2L || stats::var(finite) <= sqrt(.Machine$double.eps))
          bad <- c(bad, nm)
      }
      if (length(bad) > 0L)
        jmvcore::reject(paste0(
          "The following sensory attributes are not informative numeric variables: ",
          paste(unique(bad), collapse = ", "), "."
        ))

      invisible(TRUE)
    },

    #### R code ----

    .code = function() {
      r_literal <- function(value) {
        if (is.null(value))
          return("NULL")
        paste(deparse(value, width.cutoff = 500L), collapse = "\n")
      }

      prodeff <- as.character(self$options$prodeff)
      paneff  <- as.character(self$options$paneff)
      sensoatt <- as.character(self$options$sensoatt)

      if (length(prodeff) == 0L || is.na(prodeff[1]) || !nzchar(prodeff[1]))
        return("# Select a stimulus variable to generate the QDA code.")
      if (length(paneff) == 0L || is.na(paneff[1]) || !nzchar(paneff[1]))
        return("# Select a subject variable to generate the QDA code.")
      if (length(sensoatt) == 0L)
        return("# Select at least one sensory attribute to generate the QDA code.")

      variables <- c(prodeff[1], paneff[1], sensoatt)
      threshold <- suppressWarnings(as.numeric(self$options$threshold)) / 100
      if (length(threshold) == 0L || !is.finite(threshold))
        threshold <- 0.05

      code <- c(
        "library(SensoMineR)",
        "",
        "# This script can be pasted directly into the jamovi Rj Editor.",
        "# The dataset open in jamovi is available as data.",
        "",
        "# Keep the stimulus and subject variables first, followed by the sensory attributes.",
        paste0(
          "data_QDA <- data[, ", r_literal(variables),
          ", drop = FALSE]"
        ),
        "",
        "# The first two variables must be factors.",
        "data_QDA[[1]] <- as.factor(data_QDA[[1]])",
        "data_QDA[[2]] <- as.factor(data_QDA[[2]])",
        "",
        "# Use simple internal names so the ANOVA formula remains valid even",
        "# when the original jamovi variable names contain spaces or symbols.",
        "names(data_QDA)[1:2] <- c(\".Stimulus\", \".Subject\")",
        "",
        "# Characterization of the stimulus space",
        "res_QDA <- decat(",
        "  data_QDA,",
        "  formul = \"~.Stimulus+.Subject\",",
        "  firstvar = 3,",
        "  lastvar = ncol(data_QDA),",
        paste0("  proba = ", r_literal(threshold), ","),
        "  graph = FALSE,",
        "  random = TRUE",
        ")",
        "",
        "# Attributes showing a significant overall stimulus effect",
        "res_QDA$resF",
        "",
        "# Attributes characterizing each stimulus",
        "res_QDA$resT"
      )

      paste(code, collapse = "\n")
    },

    #### Compute results ----
    
    .decat = function(data) {
      
      threshold <- self$options$threshold / 100
      prodeff   <- ".Stimulus"
      paneff    <- ".Subject"
      formul    <- paste0("~", prodeff, "+", paneff)
      firstvar  <- 3
      lastvar   <- ncol(data)
      proba     <- threshold
      random    <- TRUE
      donnee    <- data
      
      # Restaurer les contrastes après la fonction
      old.contr <- options()$contrasts
      on.exit(options(contrasts = old.contr), add = TRUE)
      options(contrasts = c("contr.sum", "contr.sum"))
      
      for (j in 1:(firstvar - 1)) donnee[, j] <- as.factor(donnee[, j])
      level.lower <- -qnorm(proba / 2)
      formul      <- as.formula(paste(formul, collapse = " "))
      lab.sauv    <- colnames(donnee)
      # Use formula-safe internal names for every column while retaining the
      # original sensory names for the output tables.
      lab <- c(".Stimulus", ".Subject", paste0(".Sensory", seq_len(ncol(donnee) - 2L)))
      colnames(donnee) <- lab
      equation <- as.character(formul)
      Terms    <- attr(terms(as.formula(paste(equation, collapse = " "))), "term.labels")
      equation <- paste("~", Terms[1])
      if (length(Terms) > 1)
        for (i in 2:length(Terms)) equation <- paste(equation, "+", Terms[i])
      equation <- as.character(as.formula(paste(equation, collapse = " ")))
      
      dim.donnee <- ncol(donnee)
      
      if (length(strsplit(equation, split = "+", fixed = TRUE)[[2]]) == 1)
        random <- FALSE
      
      col.p <- col.j <- NULL
      for (i in 1:dim.donnee) {
        if (gsub(" ", "", strsplit(equation, split = "+", fixed = TRUE)[[2]][1]) == lab[i])
          col.p <- i
        if (random && length(strsplit(equation, split = "+", fixed = TRUE)[[2]]) >= 2) {
          if (gsub(" ", "", strsplit(equation, split = "+", fixed = TRUE)[[2]][2]) == lab[i])
            col.j <- i
        }
      }
      
      nb.modalite <- nlevels(donnee[, col.p])
      don.aux     <- cbind.data.frame(donnee,
                                      fac = ordered(donnee[, col.p], rev(levels(donnee[, col.p]))))
      don.aux[, col.p] <- as.factor(don.aux[, ncol(don.aux)])
      
      tabF    <- matrix(0, lastvar + 1 - firstvar, 2)
      adjmean <- coeff <- tabT <- matrix(0, lastvar + 1 - firstvar, nb.modalite)
      
      for (varendo in firstvar:lastvar) {
        formule      <- as.formula(paste(lab[varendo], "~", equation[2]))
        res          <- summary(aov(formule, data = donnee, na.action = na.exclude))[[1]]
        nrow.facteur <- nrow(res)
        
        if (random && !is.null(col.j)) {
          panelist <- colnames(donnee)[col.j]
          product  <- colnames(donnee)[col.p]
          for (i in 3:length(Terms)) {
            if (any(grep(product, Terms[i])) &&
                any(grep(":", Terms[i])) &&
                any(grep(panelist, Terms[i])))
              nrow.facteur <- i
          }
        }
        
        tabF[varendo - firstvar + 1, 1] <- -qnorm(pf(
          res[1, 3] / res[nrow.facteur, 3], res[1, 1], res[nrow.facteur, 1],
          lower.tail = FALSE))
        tabF[varendo - firstvar + 1, 2] <- pf(
          res[1, 3] / res[nrow.facteur, 3], res[1, 1], res[nrow.facteur, 1],
          lower.tail = FALSE)
        
        res2 <- summary.lm(aov(formule, data = donnee, na.action = na.exclude))$coef[1:nb.modalite, ]
        moy  <- res2[1, 1]
        res2 <- res2[-1, ]
        
        if (nb.modalite > 2) {
          tabT[varendo - firstvar + 1, 1:(nb.modalite - 1)] <-
            -qnorm((pf(res2[, 3]^2 * (res[nrow(res), 3] / res[nrow.facteur, 3]),
                       1, res[nrow.facteur, 1], lower.tail = FALSE)) / 2) * sign(res2[, 1])
          coeff[varendo - firstvar + 1, 1:(nb.modalite - 1)] <- res2[, 1]
        }
        if (nb.modalite == 2) {
          tabT[varendo - firstvar + 1, 1:(nb.modalite - 1)] <-
            -qnorm((pf(res2[3]^2 * (res[nrow(res), 3] / res[nrow.facteur, 3]),
                       1, res[nrow.facteur, 1], lower.tail = FALSE)) / 2) * sign(res2[1])
          coeff[varendo - firstvar + 1, 1:(nb.modalite - 1)] <- res2[1]
        }
        
        res2 <- summary.lm(aov(formule, data = don.aux, na.action = na.exclude))$coef[2, ]
        tabT[varendo - firstvar + 1, nb.modalite] <-
          -qnorm((pf(res2[3]^2 * (res[nrow(res), 3] / res[nrow.facteur, 3]),
                     1, res[nrow.facteur, 1], lower.tail = FALSE)) / 2) * sign(res2[1])
        coeff[varendo - firstvar + 1, nb.modalite]   <- res2[1]
        adjmean[varendo - firstvar + 1, ]            <- moy + coeff[varendo - firstvar + 1, ]
      }
      
      nomdescripteur <- lab.sauv[firstvar:lastvar]
      dimnames(tabF)   <- list(nomdescripteur, c("Vtest", "P-value"))
      dimnames(adjmean) <- dimnames(coeff) <- dimnames(tabT) <-
        list(nomdescripteur, levels(donnee[, col.p]))
      
      select1 <- which(tabF[order(tabF[, 2]), 2] < proba)
      
      result        <- list()
      result$tabF   <- tabF
      result$tabT   <- t(tabT)
      result$coeff  <- t(coeff)
      result$adjmean <- t(adjmean)
      
      if (length(select1) > 0) {
        resF <- cbind.data.frame(
          qnorm(tabF[order(tabF[, 2]), 2], lower.tail = FALSE)[select1],
          tabF[order(tabF[, 2]), 2][select1]
        )
        dimnames(resF)[[2]] <- c("Vtest", "P-value")
        
        resT <- vector("list", length = nb.modalite)
        for (i in 1:nb.modalite) {
          select <- which(abs(tabT[rev(order(tabT[, i])), i]) >= level.lower)
          resT[[i]] <- cbind.data.frame(
            coeff[rev(order(tabT[, i])), i][select],
            adjmean[rev(order(tabT[, i])), i][select],
            2 * pnorm(-abs(tabT[rev(order(tabT[, i])), i][select])),
            tabT[rev(order(tabT[, i])), i][select]
          )
          dimnames(resT[[i]])[[2]] <- c("Coeff", "Adjust mean", "P-value", "Vtest")
        }
        names(resT)   <- levels(donnee[, col.p])
        result$resF   <- resF
        result$resT   <- resT
      } else {
        result$resF <- NULL
        result$resT <- NULL
        return(result)
      }
      
      return(result)
    },
    
    ### Helper functions ----
    
    .printresTable = function(tab) {
      for (i in seq_len(nrow(tab))) {
        self$results$resT$addRow(rowKey = i, values = list(
          component = as.character(tab[i, 1]),
          var       = as.character(tab[i, 2]),
          coeff     = as.numeric(tab[i, 3]),
          adjmean   = as.numeric(tab[i, 4]),
          pvalue    = as.numeric(tab[i, 5]),
          vtest     = as.numeric(tab[i, 6])
        ))
      }
    },
    
    .printresFTable = function(tab) {
      resF <- self$results$resF
      
      for (i in seq_len(nrow(tab))) {
        resF$addRow(rowKey = i, values = list(
          att    = rownames(tab)[i],
          vtest  = as.numeric(tab[i, 1]),
          pvalue = as.numeric(tab[i, 2])
        ))
      }
    }
  )
)