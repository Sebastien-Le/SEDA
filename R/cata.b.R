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
        "<html>
            <head>
            </head>
            <body>
            <div class='justified-text'>
            <p><b>What you need to know before analysing CATA data in jamovi</b></p>
            <p>______________________________________________________________________________</p>
            <p> CATA stands for <I>Check-All-That-Apply</I>. CATA is a method used in sensory evaluation and consumer research to obtain 
            descriptive and discriminative information about stimuli, products or samples.</p>
            <p> In a CATA test, panelists or consumers are presented with a list of attributes or sensory characteristics related to the stimuli they are evaluating. 
            The panelists are asked to check all the attributes that they perceive or find present in the stimulus. 
            Unlike traditional rating scales where panelists assign a single score to each attribute, CATA allows the panelists 
            to select multiple attributes that they perceive simultaneously in the stimulus.</p>
            <p> CATA captures the multiplicity of attributes: CATA allows panelists to express the complex sensory profiles of 
            stimuli by selecting multiple attributes that co-occur in the same stimulus. CATA is user-friendly: The check-all-that-apply format is 
            straightforward and easy for panelists to understand and complete.</p>
            <p> In the SEDA module, the analysis of CATA data is based on a contingency table, and a two steps procedure. After building the 
            contingency table, the interface provides a description of the stimuli according to the CATA items. Then, a correspondence 
            analysis is performed to vizualize the relationship between the stimuli and the CATA items, followed by an automatic clustering
            of the stimuli and a description of the clusters.</p>
            <p>______________________________________________________________________________</p>
            </div>
            </body>
            </html>"
      )
    },
    
    .run = function() {
      if (is.null(self$options$stimuli) || is.null(self$options$group))
        return()
      
      if (length(self$options$group) < 2) {
        jmvcore::reject("The number of factors is too low")
        return()
      }
      
      data <- private$.buildData()
      if (is.null(data) || nrow(data) == 0)
        return()
      
      res.dataprod <- tryCatch(
        private$.Dataprod(data),
        error = function(e) {
          jmvcore::reject(paste("Dataprod failed:", e$message))
          NULL
        }
      )
      if (is.null(res.dataprod) || nrow(res.dataprod) == 0 || ncol(res.dataprod) == 0)
        return()
      
      self$results$plotcata$setState(res.dataprod)
      
      threshold <- self$options$thres / 100
      
      dfres <- tryCatch(
        private$.descfreq(res.dataprod, threshold),
        error = function(e) NULL
      )
      
      private$.populateTEXTUALTable(res.dataprod)
      
      if (!is.null(dfres)) {
        dfres_nonnull <- dfres[!vapply(dfres, is.null, logical(1))]
        if (length(dfres_nonnull) > 0) {
          tabs <- lapply(names(dfres_nonnull), function(nm) {
            x <- as.data.frame(dfres_nonnull[[nm]])
            if (nrow(x) == 0) return(NULL)
            out <- cbind(Modality = rep(nm, nrow(x)), Word = rownames(x), x)
            rownames(out) <- NULL
            out
          })
          tabs <- Filter(Negate(is.null), tabs)
          if (length(tabs) > 0)
            private$.populateDFTable(do.call(rbind, tabs))
        }
      }
      
      # CA + classification automatique (nécessite au moins 3 attributs)
      if (length(self$options$group) >= 3) {
        res.ca <- tryCatch(
          FactoMineR::CA(res.dataprod, graph = FALSE),
          error = function(e) NULL
        )
        
        if (!is.null(res.ca)) {
          res.classif <- tryCatch(
            FactoMineR::HCPC(res.ca, nb.clust = -1, graph = FALSE),
            error = function(e) NULL
          )
          
          if (!is.null(res.classif)) {
            self$results$plotclassif$setState(res.classif)
            
            # Tableau agrégé par cluster pour descfreq
            clust_var <- res.classif$data.clust[, ncol(res.classif$data.clust)]
            private$.populateClusterTable(res.dataprod, clust_var, threshold)
          }
        }
      }
    },
    
    #### Compute results ----
    
    .descfreq = function(res.dataprod, threshold) {
      descfreq(res.dataprod, proba = threshold)
    },
    
    .Dataprod = function(data) {
      formula <- reformulate(self$options$stimuli, response = ".")
      data <- aggregate(formula, data = data, sum)
      rownames(data) <- data[, 1]
      data[, -1, drop = FALSE]
    },
    
    ### Plot functions ----
    
    .plotcatatis = function(image, ...) {
      if (is.null(self$options$stimuli) || is.null(self$options$group))
        return()
      if (length(self$options$group) < 3) {
        jmvcore::reject("The number of factors is too low for the Representation of the Products and the CATA")
        return()
      }
      res.dataprod <- image$state
      if (is.null(res.dataprod) || nrow(res.dataprod) == 0 || ncol(res.dataprod) == 0)
        return()
      res.ca <- tryCatch(
        FactoMineR::CA(res.dataprod, graph = FALSE),
        error = function(e) NULL
      )
      if (is.null(res.ca)) return()
      plot <- plot.CA(res.ca, title = "Representation of the Stimuli and the CATA")
      print(plot)
      TRUE
    },
    
    .plotclassif = function(image, ...) {
      if (is.null(self$options$stimuli) || is.null(self$options$group)) return()
      res.classif <- image$state
      if (is.null(res.classif)) return()
      plot <- FactoMineR::plot.HCPC(res.classif,
                                    axes      = c(1, 2),
                                    choice    = "map",
                                    draw.tree = FALSE,
                                    title     = "Representation of the Stimuli According to Clusters")
      print(plot)
      TRUE
    },
    
    ### Helper functions ----
    
    .populateClusterTable = function(res.dataprod, clust_var, threshold) {
      
      # Agréger le tableau de contingence par cluster
      data_by_cluster <- data.frame(
        cluster = clust_var,
        res.dataprod
      )
      formula   <- reformulate("cluster", response = ".")
      tab_clust <- aggregate(formula, data = data_by_cluster, sum)
      rownames(tab_clust) <- tab_clust[, 1]
      tab_clust <- tab_clust[, -1, drop = FALSE]
      
      # descfreq sur le tableau agrégé — même fonction, même seuil que pour les stimuli
      desc <- tryCatch(
        descfreq(tab_clust, proba = threshold),
        error = function(e) NULL
      )
      if (is.null(desc)) return()
      
      dfres_nonnull <- desc[!vapply(desc, is.null, logical(1))]
      if (length(dfres_nonnull) == 0) return()
      
      tabs <- lapply(names(dfres_nonnull), function(nm) {
        x <- as.data.frame(dfres_nonnull[[nm]])
        if (nrow(x) == 0) return(NULL)
        out <- cbind(cluster = rep(nm, nrow(x)), word = rownames(x), x)
        rownames(out) <- NULL
        out
      })
      tabs <- Filter(Negate(is.null), tabs)
      if (length(tabs) == 0) return()
      
      tab   <- do.call(rbind, tabs)
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
      textual  <- self$results$textualgroup$textual
      coltable <- colnames(table)
      rn       <- rownames(table)
      
      textual$addColumn(name = "rownames", title = "", type = "text")
      for (i in seq_along(coltable))
        textual$addColumn(name = coltable[i], title = coltable[i], type = "integer")
      
      for (i in seq_len(nrow(table))) {
        row <- list(rownames = rn[i])
        for (j in seq_along(coltable))
          row[[coltable[j]]] <- table[i, j]
        textual$addRow(rowKey = i, values = row)
      }
      
      # total <- list(rownames = "Total")
      # for (j in seq_along(coltable))
      #   total[[coltable[j]]] <- sum(table[, j], na.rm = TRUE)
      # textual$addRow(rowKey = nrow(table) + 1, values = total)
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
      data.frame(datastimuli, datanote)
    }
  )
)