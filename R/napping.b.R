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
        "<html>
        <head></head>
        <body>
        <div class='justified-text'>
        <p><b>What you need to know before analysing Napping data in jamovi</b></p>
        <p>______________________________________________________________________________</p>
        <p> Napping data is a type of data used in sensory analysis to understand the sensory perception
        of stimuli, from a global/holistic point of view, regardless of any <I>a priori</I> list of sensory attributes.
        It was introduced by Jérôme Pagès in 2003.</p>
        <p> The napping data collection method involves presenting a set of stimuli simultaneously to a group of panelists.
        Each panelist is asked to position the stimuli in a two-dimensional space based on their perceived similarities and differences.
        The panelists are typically provided with a sheet or a table where they can physically place the stimuli
        (<I>e.g.</I>, product samples, images, or other sensory samples) on the two-dimensional space.</p>
        <p> The two dimensions used in napping are typically not labeled but correspond to perceptual qualities that the panelists perceive to differentiate the stimuli.
        The distances and positions of the stimuli on the two-dimensional space reflect the perceived similarities and differences between them.
        Stimuli that are closer together are perceived as more similar, while those farther apart are perceived as more different.</p>
        <p> Open the <b>perfumes_napping</b> dataset. Choose the Ident variable as <I>Stimuli Labels</I>. Put as many <B>pairs</B> of (X,Y) coordinates in the first
        block. This is essential to obtain a consensual representation of the stimuli. You can also add <B>pairs</B> of supplementary (X,Y) coordinates. This last feature is optional.</p>
        <p> <B>Warning</B>: an error message will appear if the number of coordinates is odd.</p>
        <p> Clustering is based on the number of components saved.
        By default, clustering is based on the first 5 components,
        <I>i.e.</I> the distance between individuals is calculated on these 5 components.</p>
        <p> By default, the <I>Number of clusters</I> field is set to -1 which means that the number of clusters is automatically chosen by the computer.</p>
        <p>______________________________________________________________________________</p>
        </div>
        </body>
        </html>"
      )
    },
    
    .run = function() {
      if (is.null(self$options$actvars)) return()
      
      if (length(self$options$actvars) < 3 ||
          length(self$options$actvars) %% 2 != 0 ||
          length(self$options$qualisup) %% 2 != 0) {
        private$.errorCheck()
        return()
      }
      
      res.napping <- self$NappingResult
      if (is.null(res.napping)) return()
      
      res.classif <- NULL
      need_classif <- isTRUE(self$options$graphclassif) || !self$results$newvar2$isNotFilled()
      if (need_classif)
        res.classif <- private$.getclassifResult()
      
      private$.printeigenTable()
      self$results$descdesdim$setContent(private$.dimdesc())
      
      self$results$plotgroup$setState(self$NappingResult)
      self$results$plotind$setState(self$NappingResult)
      
      if (isTRUE(self$options$graphclassif) && !is.null(res.classif))
        self$results$plotclassif$setState(res.classif)
      
      if (!is.null(res.classif))
        private$.output2(res.classif)
      
      private$.output()
    },
    
    #### Compute results ----
    
    .computeNbclust = function() {
      return(self$options$nbclust)
    },
    
    .getclassifResult = function() {
      reshcpc <- tryCatch(
        FactoMineR::HCPC(self$NappingResult, nb.clust = self$nbclust, graph = FALSE),
        error = function(e) NULL
      )
      private$.classifResult <- reshcpc
      return(private$.classifResult)
    },
    
    .getNappingResult = function() {
      data <- self$dataProcessed
      if (is.null(data)) return(NULL)
      
      # Logique ncp défensive
      ncp_candidates <- c(self$options$ncp, self$options$nFactors)
      ncp_candidates <- suppressWarnings(as.numeric(ncp_candidates))
      ncp_candidates <- ncp_candidates[!is.na(ncp_candidates) & ncp_candidates > 0]
      ncp_target     <- if (length(ncp_candidates) == 0) 2 else max(ncp_candidates)
      ncp_target     <- max(ncp_target, 3)
      ncp_use        <- ncp_target
      
      nbgroups   <- length(self$options$actvars) / 2
      nbgroupsup <- if (!is.null(self$options$qualisup) && length(self$options$qualisup) > 0)
        length(self$options$qualisup) / 2 else 0
      nbg <- nbgroups + nbgroupsup
      
      r <- tryCatch({
        if (nbgroupsup == 0) {
          FactoMineR::MFA(data,
                          group      = rep(2, nbgroups),
                          type       = rep("c", nbgroups),
                          ncp        = ncp_use,
                          name.group = paste0("S", 1:nbgroups),
                          graph      = FALSE)
        } else {
          FactoMineR::MFA(data,
                          group         = rep(2, nbg),
                          type          = rep("c", nbg),
                          num.group.sup = (nbgroups + 1):nbg,
                          ncp           = ncp_use,
                          name.group    = paste0("S", 1:nbg),
                          graph         = FALSE)
        }
      }, error = function(e) {
        jmvcore::reject(paste("MFA failed:", e$message))
        return(NULL)
      })
      
      private$.NappingResult <- r
      return(private$.NappingResult)
    },
    
    .dimdesc = function() {
      table  <- self$NappingResult
      proba  <- self$options$proba / 100
      nFactors <- self$options$nFactors
      res.dimdesc <- dimdesc(table, axes = 1:nFactors, proba = proba)
      res.dimdesc$call <- NULL
      return(res.dimdesc)
    },
    
    .printeigenTable = function() {
      table      <- self$NappingResult
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
    
    .plotindividus = function(image, ...) {
      if (is.null(self$options$actvars) ||
          length(self$options$actvars) < self$options$nFactors) return()
      res.napping <- image$state
      plot <- FactoMineR::plot.MFA(res.napping,
                                   axes      = c(self$options$abs, self$options$ord),
                                   choix     = "ind",
                                   habillage = "none",
                                   title     = "Representation of the Stimuli")
      print(plot)
      TRUE
    },
    
    .plotvariables = function(image, ...) {
      if (is.null(self$options$actvars) ||
          length(self$options$actvars) < self$options$nFactors) {
        image$setState(NULL)
        return()
      }
      res.napping    <- image$state
      abs_gui        <- self$options$abs
      ord_gui        <- self$options$ord
      limcosvar_gui  <- self$options$limcosvar / 100
      
      limcostest <- sum(
        apply(res.napping$quanti.var$cos2[, c(abs_gui, ord_gui)], 1, sum) > limcosvar_gui,
        ifelse(is.null(res.napping$quanti.var.sup), 0,
               apply(res.napping$quanti.var.sup$cos2[, c(abs_gui, ord_gui)], 1, sum) > limcosvar_gui)
      )
      
      if (limcostest > 0) {
        plot <- FactoMineR::plot.MFA(res.napping,
                                     choix        = "var",
                                     axes         = c(abs_gui, ord_gui),
                                     habillage    = "group",
                                     lim.cos2.var = limcosvar_gui,
                                     graph.type   = "ggplot")
        print(plot)
        TRUE
      } else {
        image$setState(NULL)
      }
    },
    
    .plotgroups = function(image, ...) {
      if (is.null(self$options$actvars) ||
          length(self$options$actvars) < self$options$nFactors) return()
      res.napping <- image$state
      plot <- FactoMineR::plot.MFA(res.napping,
                                   axes      = c(self$options$abs, self$options$ord),
                                   choix     = "group",
                                   habillage = "none",
                                   title     = "Representation of the Subjects")
      print(plot)
      TRUE
    },
    
    .plotclassif = function(image, ...) {
      if (is.null(self$options$actvars)) return()
      res.classif <- image$state
      if (is.null(res.classif)) return()
      plot <- FactoMineR::plot.HCPC(res.classif,
                                    axes      = c(self$options$abs, self$options$ord),
                                    choice    = "map",
                                    draw.tree = FALSE,
                                    title     = "Representation of the Stimuli According to Clusters")
      print(plot)
      TRUE
    },
    
    ### Helper functions ----
    
    .errorCheck = function() {
      if (length(self$options$actvars) > 3 &&
          (length(self$options$actvars) %% 2 != 0 ||
           length(self$options$qualisup) %% 2 != 0))
        jmvcore::reject('The number of coordinates should be even')
    },
    
    .output = function() {
      nFactors_out <- min(self$options$ncp, nrow(self$NappingResult$eig))
      res.mfa      <- self$NappingResult
      if (self$results$newvar$isNotFilled()) {
        self$results$newvar$set(
          keys         = 1:nFactors_out,
          titles       = paste("Dim.", 1:nFactors_out),
          descriptions = rep("MFA component", nFactors_out),
          measureTypes = rep("continuous", nFactors_out)
        )
        for (i in seq_len(nFactors_out))
          self$results$newvar$setValues(index = i, as.numeric(res.mfa$ind$coord[, i]))
        self$results$newvar$setRowNums(rownames(self$data))
      }
    },
    
    .output2 = function(res.classif) {
      if (is.null(res.classif) || is.null(res.classif$data.clust)) return()
      output <- self$results$newvar2
      if (output$isNotFilled()) {
        output$set(
          keys         = 1,
          titles       = "Cluster",
          descriptions = "Cluster variable",
          measureTypes = "nominal"
        )
      }
      scores <- as.factor(res.classif$data.clust[, ncol(res.classif$data.clust)])
      output$setValues(index = 1, scores)
      output$setRowNums(rownames(self$data))
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
      
      if (length(data_list) == 0) return(NULL)
      
      data <- as.data.frame(do.call(cbind, data_list))
      rownames(data) <- if (!is.null(self$options$individus))
        self$data[[self$options$individus]]
      else
        seq_len(nrow(data))
      
      return(data)
    }
  )
)