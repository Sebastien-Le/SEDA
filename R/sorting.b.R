SortingClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "SortingClass",
  inherit = SortingBase,
  
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
    SortingResult = function() {
      if (is.null(private$.SortingResult))
        private$.SortingResult <- private$.getSortingResult()
      return(private$.SortingResult)
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
        "<html>
        <head></head>
        <body>
        <div class='justified-text'>
        <p><b>What you need to know before analysing sorting data in jamovi</b></p>
        <p>______________________________________________________________________________</p>
        <p> Sorting task refers to a type of cognitive test or experimental procedure where assessors are asked to categorize or group stimuli
        based on the way they perceive the stimuli. The stimuli can be any sensory information, such as visual, auditory, or tactile, and the task aims to understand how people organize and process this information.</p>
        <p> Sorting tasks are commonly used in psychological research to study various aspects of cognition, perception, and information processing.
        These tasks can provide insights into how individuals perceive and make sense of the world around them, how they categorize different stimuli, and how they form mental representations of the information.</p>
        <p> Sometimes groups of stimuli are labelled. These labels are essential for interpreting how the stimuli as a whole are perceived.
        The SEDA module treats labels as textual information and analyses them as such.</p>
        <p> The interface provides different types of results. First, a description of the stimuli based on the labels, second a multivariate
        representation of the stimuli according to the groups provided by the assessors. This multivariate analysis is completed by an unsupervised
        clustering of the stimuli.</p>
        <p> Clustering is based on the number of components saved. By default, clustering is based on the first 5 components,
        <I>i.e.</I> the distance between individuals is calculated on these 5 components.</p>
        <p> By default, the <I>Number of clusters</I> field is set to -1 which means that the number of clusters is automatically chosen by the computer.</p>
        <p>______________________________________________________________________________</p>
        </div>
        </body>
        </html>"
      )
    },
    
    .run = function() {
      if (is.null(self$options$actvars))
        return()
      
      private$.errorCheck()
      
      predata  <- self$dataProcessed
      if (is.null(predata))
        return()
      
      data     <- predata[[1]]
      datatext <- predata[[2]]
      
      self$results$longformatdata$setContent(datatext)
      
      res.textual <- private$.textual(datatext)
      dfres <- private$.descfreq(res.textual)
      
      valid_idx <- which(!vapply(dfres, is.null, logical(1)))
      
      if (length(valid_idx) > 0) {
        j      <- valid_idx[1]
        xinit  <- as.data.frame(dfres[[j]])
        xxinit <- cbind(rep(names(dfres)[j], nrow(xinit)), rownames(xinit), xinit)
        colnames(xxinit)[1:2] <- c("Modality", "Word")
        rownames(xxinit) <- NULL
        
        if (length(valid_idx) > 1) {
          for (j in valid_idx[-1]) {
            xj   <- as.data.frame(dfres[[j]])
            xxj  <- cbind(rep(names(dfres)[j], nrow(xj)), rownames(xj), xj)
            colnames(xxj)[1:2] <- c("Modality", "Word")
            rownames(xxj) <- NULL
            xxinit <- rbind(xxinit, xxj)
          }
        }
        
        private$.populateDFTable(xxinit)
      }
      
      res.mca <- self$SortingResult
      if (is.null(res.mca))
        return()
      
      res.classif <- NULL
      need_classif <- isTRUE(self$options$graphclassif) || !self$results$newvar2$isNotFilled()
      if (need_classif)
        res.classif <- private$.classif(res.mca)
      
      self$results$dimdesc$setContent(private$.dimdesc(res.mca))
      
      private$.printeigenTable(res.mca)
      private$.printTables(res.mca, "coord")
      private$.printTables(res.mca, "contrib")
      private$.printTables(res.mca, "cos2")
      
      self$results$plotindiv$setState(res.mca)
      self$results$plotvar$setState(res.mca)
      self$results$plotitemvar$setState(res.mca)
      
      if (isTRUE(self$options$graphclassif) && !is.null(res.classif))
        self$results$plotclassif$setState(res.classif)
      
      if (!is.null(res.classif))
        private$.output2(res.classif)
      
      private$.output(res.mca)
    },
    
    .getSortingResult = function() {
      predata <- self$dataProcessed
      if (is.null(predata))
        return(NULL)
      
      data <- predata[[1]]
      if (is.null(data))
        return(NULL)
      
      r <- private$.MCA(data)
      private$.SortingResult <- r
      return(private$.SortingResult)
    },
  
    #### Compute results ----
    
    .computeNbclust = function() {
      return(self$options$nbclust)
    },
    
    .MCA = function(data) {
      actvars_gui  <- self$options$actvars
      qualisup_gui <- self$options$qualisup
      ventil       <- self$options$ventil / 100
      
      ncp_candidates <- c(self$options$ncp, self$options$nFactors)
      ncp_candidates <- suppressWarnings(as.numeric(ncp_candidates))
      ncp_candidates <- ncp_candidates[!is.na(ncp_candidates) & ncp_candidates > 0]
      ncp_target     <- if (length(ncp_candidates) == 0) 2 else max(ncp_candidates)
      ncp_target     <- max(ncp_target, 3)
      ncp_use        <- ncp_target
      
      r <- tryCatch({
        if (!is.null(qualisup_gui) && length(qualisup_gui) > 0) {
          FactoMineR::MCA(data,
                          quali.sup    = (length(actvars_gui) + 1):(length(actvars_gui) + length(qualisup_gui)),
                          ncp          = ncp_use,
                          level.ventil = ventil,
                          graph        = FALSE)
        } else {
          FactoMineR::MCA(data,
                          ncp          = ncp_use,
                          level.ventil = ventil,
                          graph        = FALSE)
        }
      }, error = function(e) {
        jmvcore::reject(paste("MCA failed:", e$message))
        return(NULL)
      })
      return(r)
    },
    
    .classif = function(res) {
      data <- self$dataProcessed[[1]]
      if (is.null(data))
        return(NULL)
      
      actvars_gui  <- self$options$actvars
      qualisup_gui <- self$options$qualisup
      ventil       <- self$options$ventil / 100
      
      ncp_clust <- suppressWarnings(as.numeric(self$options$ncp))
      if (is.na(ncp_clust) || ncp_clust < 1)
        ncp_clust <- 1
      
      res_for_hcpc <- tryCatch({
        if (!is.null(qualisup_gui) && length(qualisup_gui) > 0) {
          FactoMineR::MCA(
            data,
            quali.sup    = (length(actvars_gui) + 1):(length(actvars_gui) + length(qualisup_gui)),
            ncp          = ncp_clust,
            level.ventil = ventil,
            graph        = FALSE
          )
        } else {
          FactoMineR::MCA(
            data,
            ncp          = ncp_clust,
            level.ventil = ventil,
            graph        = FALSE
          )
        }
      }, error = function(e) {
        jmvcore::reject(paste("MCA for clustering failed:", e$message))
        return(NULL)
      })
      
      if (is.null(res_for_hcpc))
        return(NULL)
      
      tryCatch(
        FactoMineR::HCPC(res_for_hcpc, nb.clust = self$nbclust, graph = FALSE),
        error = function(e) {
          jmvcore::reject(paste("HCPC failed:", e$message))
          NULL
        }
      )
    },
    
    .dimdesc = function(table) {
      if (is.null(table))
        return("No result available")
      
      nFactors_out <- min(self$options$nFactors, ncol(table$ind$coord))
      if (is.null(nFactors_out) || nFactors_out < 1)
        return("No dimension available")
      
      res <- FactoMineR::dimdesc(table, axes = 1:nFactors_out, proba = self$options$proba / 100)
      paste(capture.output(print(res[-length(res)])), collapse = "\n")
    },
    
    .textual = function(data) {
      FactoMineR::textual(data, num.text = 2, contingence.by = 1, sep.word = ";")
    },
    
    .descfreq = function(res) {
      threshold <- self$options$leveltext / 100
      FactoMineR::descfreq(res$cont.table, proba = threshold)
    },
    
    .printeigenTable = function(table) {
      eigen      <- table$eig[, 1]
      purcent    <- table$eig[, 2]
      purcentcum <- table$eig[, 3]
      for (i in seq_along(eigen)) {
        self$results$eigengroup$eigen$addRow(rowKey = i, values = list(
          component  = paste("Dim.", i),
          eigenvalue = eigen[i],
          purcent    = purcent[i],
          purcentcum = purcentcum[i]
        ))
      }
    },
    
    .printTables = function(table, quoi) {
      
      show_ind <- switch(quoi,
                         "coord"   = isTRUE(self$options$indcoord),
                         "contrib" = isTRUE(self$options$indcontrib),
                         "cos2"    = isTRUE(self$options$indcos),
                         FALSE)
      show_var <- switch(quoi,
                         "coord"   = isTRUE(self$options$varcoord),
                         "contrib" = isTRUE(self$options$varcontrib),
                         "cos2"    = isTRUE(self$options$varcos),
                         FALSE)
      
      if (!show_ind && !show_var) return()
      
      nFactors_out <- min(self$options$nFactors, ncol(table$ind$coord))
      
      individus_gui <- if (!is.null(self$options$individus))
        as.character(self$data[[self$options$individus]])
      else
        as.character(seq_len(nrow(self$data)))
      
      if (quoi == "coord") {
        quoivar  <- table$var$coord
        quoiind  <- table$ind$coord
        tablevar <- self$results$variables$coordonnees
        tableind <- self$results$individus$coordonnees
      } else if (quoi == "contrib") {
        quoivar  <- table$var$contrib
        quoiind  <- table$ind$contrib
        tablevar <- self$results$variables$contribution
        tableind <- self$results$individus$contribution
      } else if (quoi == "cos2") {
        quoivar  <- table$var$cos2
        quoiind  <- table$ind$cos2
        tablevar <- self$results$variables$cosinus
        tableind <- self$results$individus$cosinus
      } else {
        return()
      }
      
      if (show_var) {
        tablevar$addColumn(name = "variables", title = "", type = "text")
        for (i in seq_len(nrow(quoivar)))
          tablevar$addRow(rowKey = i, value = NULL)
        for (i in seq_len(nFactors_out))
          tablevar$addColumn(name = paste0("dim", i), title = paste0("Dim.", i), type = "number")
        for (var in seq_len(nrow(quoivar))) {
          row <- list(variables = rownames(quoivar)[var])
          for (i in seq_len(nFactors_out))
            row[[paste0("dim", i)]] <- quoivar[var, i]
          tablevar$setRow(rowNo = var, values = row)
        }
      }
      
      if (show_ind) {
        tableind$addColumn(name = "individus", title = "", type = "text")
        for (i in seq_len(nrow(quoiind)))
          tableind$addRow(rowKey = i, value = NULL)
        for (i in seq_len(nFactors_out))
          tableind$addColumn(name = paste0("dim", i), title = paste0("Dim.", i), type = "number")
        for (ind in seq_len(nrow(quoiind))) {
          row <- list(individus = individus_gui[ind])
          for (i in seq_len(nFactors_out))
            row[[paste0("dim", i)]] <- quoiind[ind, i]
          tableind$setRow(rowNo = ind, values = row)
        }
      }
    },
    
    .plotitemvar = function(image, ...) {
      if (is.null(self$options$actvars))
        return(FALSE)
      
      res.mca <- image$state
      if (is.null(res.mca))
        return(FALSE)
      
      axes_ok <- private$.getValidAxes(res.mca)
      if (is.null(axes_ok))
        return(FALSE)
      
      invisible_vec <- "ind"
      if (!isTRUE(self$options$varmodvar))
        invisible_vec <- c(invisible_vec, "var")
      if (!isTRUE(self$options$varmodqualisup))
        invisible_vec <- c(invisible_vec, "quali.sup")
      
      use_selectMod <- !is.null(self$options$modality) &&
        nchar(trimws(self$options$modality)) > 0
      
      ok <- tryCatch({
        if (use_selectMod) {
          plot <- FactoMineR::plot.MCA(
            res.mca,
            axes = axes_ok,
            choix = "ind",
            invisible = invisible_vec,
            selectMod = self$options$modality,
            title = "Representation of the Categories"
          )
        } else {
          plot <- FactoMineR::plot.MCA(
            res.mca,
            axes = axes_ok,
            choix = "ind",
            invisible = invisible_vec,
            title = "Representation of the Categories"
          )
        }
        print(plot)
        TRUE
      }, error = function(e) {
        jmvcore::reject(paste("Category plot failed:", e$message))
        FALSE
      })
      
      ok
    },
    
    .plotindiv = function(image, ...) {
      if (is.null(self$options$actvars))
        return(FALSE)
      
      res.mca <- image$state
      if (is.null(res.mca))
        return(FALSE)
      
      axes_ok <- private$.getValidAxes(res.mca)
      if (is.null(axes_ok))
        return(FALSE)
      
      ok <- tryCatch({
        plot <- FactoMineR::plot.MCA(
          res.mca,
          axes = axes_ok,
          choix = "ind",
          invisible = c("var", "quali.sup"),
          title = "Representation of the Individuals"
        )
        print(plot)
        TRUE
      }, error = function(e) {
        jmvcore::reject(paste("Individual plot failed:", e$message))
        FALSE
      })
      
      ok
    },
    
    .plotvar = function(image, ...) {
      if (is.null(self$options$actvars))
        return(FALSE)
      
      res.mca <- image$state
      if (is.null(res.mca))
        return(FALSE)
      
      axes_ok <- private$.getValidAxes(res.mca)
      if (is.null(axes_ok))
        return(FALSE)
      
      ok <- tryCatch({
        plot <- FactoMineR::plot.MCA(
          res.mca,
          axes = axes_ok,
          choix = "var",
          title = "Representation of the Variables"
        )
        print(plot)
        TRUE
      }, error = function(e) {
        jmvcore::reject(paste("Variable plot failed:", e$message))
        FALSE
      })
      
      ok
    },
    
    .populateDFTable = function(tab) {
      for (i in seq_len(nrow(tab)))
        self$results$dfresgroup$dfres$addRow(rowKey = i,
                                             values = list(component = as.character(tab[, 1])[i]))
      for (i in seq_len(nrow(tab))) {
        row <- list(
          word        = as.character(tab[, 2])[i],
          internper   = tab[, 3][i],
          globper     = tab[, 4][i],
          internfreq  = tab[, 5][i],
          globfreq    = tab[, 6][i],
          pvaluedfres = tab[, 7][i],
          vtest       = tab[, 8][i]
        )
        self$results$dfresgroup$dfres$setRow(rowNo = i, values = row)
      }
    },
    
    .plotquantisup = function(image, ...) {
      if (is.null(self$options$actvars) ||
          is.null(self$options$quantisup) ||
          length(self$options$quantisup) == 0)
        return(FALSE)
      
      res.mca <- image$state
      if (is.null(res.mca))
        return(FALSE)
      
      axes_ok <- private$.getValidAxes(res.mca)
      if (is.null(axes_ok))
        return(FALSE)
      
      ok <- tryCatch({
        plot <- FactoMineR::plot.MCA(
          res.mca,
          axes = axes_ok,
          choix = "quanti.sup",
          title = "Representation of the Quantitative Supplementary Variables"
        )
        print(plot)
        TRUE
      }, error = function(e) {
        jmvcore::reject(paste("Quantitative supplementary variable plot failed:", e$message))
        FALSE
      })
      
      ok
    },
    
    .plotclassif = function(image, ...) {
      if (is.null(self$options$actvars))
        return(FALSE)
      
      res.classif <- image$state
      if (is.null(res.classif))
        return(FALSE)
      
      abs_gui <- suppressWarnings(as.numeric(self$options$abs))
      ord_gui <- suppressWarnings(as.numeric(self$options$ord))
      
      if (is.na(abs_gui) || is.na(ord_gui) || abs_gui < 1 || ord_gui < 1)
        return(FALSE)
      
      ok <- tryCatch({
        plot <- FactoMineR::plot.HCPC(
          res.classif,
          axes = c(abs_gui, ord_gui),
          choice = "map",
          draw.tree = FALSE,
          title = "Representation of the Individuals According to Clusters"
        )
        print(plot)
        TRUE
      }, error = function(e) {
        jmvcore::reject(paste("Cluster plot failed:", e$message))
        FALSE
      })
      
      ok
    },
    
    ### Helper functions ----
    
    .getValidAxes = function(res) {
      abs_gui <- suppressWarnings(as.numeric(self$options$abs))
      ord_gui <- suppressWarnings(as.numeric(self$options$ord))
      
      if (is.null(res) || is.null(res$eig))
        return(NULL)
      
      n_axes <- nrow(res$eig)
      
      if (is.na(abs_gui) || is.na(ord_gui) || abs_gui < 1 || ord_gui < 1)
        return(NULL)
      
      if (abs_gui > n_axes || ord_gui > n_axes)
        return(NULL)
      
      c(abs_gui, ord_gui)
    },
    
    .errorCheck = function() {
      nFactors <- suppressWarnings(as.numeric(self$options$nFactors))
      
      if (is.na(nFactors) || nFactors < 1)
        jmvcore::reject("Number of components must be at least 1")
      
      if (is.null(self$options$actvars) || length(self$options$actvars) < 2)
        jmvcore::reject("At least two active variables are required")
      
      if (length(self$options$actvars) < nFactors)
        jmvcore::reject("Number of components cannot exceed the number of active variables")
    },
    
    .output = function(res.mca) {
      data <- self$dataProcessed[[1]]
      nFactors_out <- min(self$options$ncp, ncol(res.mca$ind$coord))
      
      if (self$results$newvar$isNotFilled()) {
        self$results$newvar$set(
          keys         = 1:nFactors_out,
          titles       = paste("Dim.", 1:nFactors_out),
          descriptions = rep("MCA component", nFactors_out),
          measureTypes = rep("continuous", nFactors_out)
        )
      }
      
      for (i in seq_len(nFactors_out))
        self$results$newvar$setValues(index = i, as.numeric(res.mca$ind$coord[, i]))
      
      self$results$newvar$setRowNums(seq_len(nrow(data)))
    },
    
    .output2 = function(res.classif) {
      if (is.null(res.classif) || is.null(res.classif$data.clust))
        return()
      
      data <- self$dataProcessed[[1]]
      output <- self$results$newvar2
      
      if (output$isNotFilled()) {
        output$set(
          keys         = 1,
          titles       = "Cluster",
          descriptions = "Cluster variable",
          measureTypes = "nominal"
        )
      }
      
      output$setValues(index = 1, as.factor(res.classif$data.clust[, ncol(res.classif$data.clust)]))
      output$setRowNums(seq_len(nrow(data)))
    },
    
    .buildData = function() {
      data_list <- list()
      
      if (!is.null(self$options$actvars) && length(self$options$actvars) > 0) {
        dataactvars <- data.frame(self$data[, self$options$actvars, drop = FALSE])
        colnames(dataactvars) <- self$options$actvars
        data_list <- c(data_list, list(dataactvars))
      }
      
      if (!is.null(self$options$qualisup) && length(self$options$qualisup) > 0) {
        dataqualisup <- data.frame(self$data[, self$options$qualisup, drop = FALSE])
        colnames(dataqualisup) <- self$options$qualisup
        data_list <- c(data_list, list(dataqualisup))
      }
      
      if (length(data_list) == 0)
        return(NULL)
      
      data <- as.data.frame(do.call(cbind, data_list))
      
      if (!is.null(self$options$individus)) {
        ids <- as.character(self$data[[self$options$individus]])
        ids[is.na(ids) | ids == ""] <- as.character(seq_len(sum(is.na(ids) | ids == "")))
        rownames(data) <- make.unique(ids)
      } else {
        rownames(data) <- as.character(seq_len(nrow(data)))
      }
      
      c1 <- c()
      c2 <- c()
      for (j in seq_len(ncol(data))) {
        c1 <- c(c1, rownames(data))
        c2 <- c(c2, as.vector(data[, j]))
      }
      datatext <- as.data.frame(cbind(c1, c2), stringsAsFactors = FALSE)
      
      return(list(data, datatext))
    }
  )
)