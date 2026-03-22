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
        "<html>
        <head></head>
        <body>
        <div class='justified-text'>
        <p><b>What you need to know before analysing QDA data in jamovi (1)</b></p>
        <p>______________________________________________________________________________</p>
        <p> Quantitative Descriptive Analysis (QDA) is a sensory evaluation technique used to objectively and systematically measure
        and describe the sensory characteristics of products, such as food, beverages, personal care products, and more (<I>stimuli</I>).</p>
        <p> The goal of QDA is to generate a detailed, quantitative profile of evaluated stimuli based on sensory attributes.</p>
        <p> Therefore, we are interested in pointing out the attributes that are the most characteristic of the set of stimuli as a whole,
        and to each of the stimulus as such.</p>
        <p> The interface returns two types of results. A sorted list of the sensory attributes that characterize the stimulus space and,
        for each stimulus, a list of the attributes that characterize it.</p>
        <p> Open the <b>sensochoc</b> dataset. Use all the sensory attributes to characterize your products. You can see that the chocolate
        <B>choc1</B> was perceived as very bitter and not sweet at all.</p>
        <p>______________________________________________________________________________</p>
        </div>
        </body>
        </html>"
      )
    },
    
    .run = function() {
      if (is.null(self$options$sensoatt)) return()
      if (is.null(self$options$prodeff))  return()
      if (is.null(self$options$paneff))   return()
      
      data_eff <- self$data[, c(self$options$prodeff, self$options$paneff), drop = FALSE]
      for (nm in names(data_eff))
        data_eff[[nm]] <- as.factor(data_eff[[nm]])
      
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
    
    #### Compute results ----
    
    .decat = function(data) {
      
      threshold <- self$options$threshold / 100
      prodeff   <- self$options$prodeff
      paneff    <- self$options$paneff
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
      lab.sauv    <- lab <- colnames(donnee)
      for (i in 1:length(lab)) lab[i] <- gsub(" ", ".", lab[i])
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