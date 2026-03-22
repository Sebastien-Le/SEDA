# This file is a generated template, your changes will not be overwritten

QDABOOTClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "QDABOOTClass",
  inherit = QDABOOTBase,
  private = list(
    #---------------------------------------------  
    #### Init + run functions ----
    
    .init = function() {
      if (is.null(self$data) || is.null(self$options$senso)) {
        if (isTRUE(self$options$tuto)) {
          self$results$instructions$setVisible(visible = TRUE)
        }
      }
      
      self$results$instructions$setContent(
        "<html>
        <head>
        </head>
        <body>
        <div class='justified-text'>
        <p><b>What you need to know before analysing QDA data in jamovi (2)</b></p>
        <p>______________________________________________________________________________</p>

        <p> Quantitative Descriptive Analysis (QDA) is a sensory evaluation technique used to objectively and systematically measure 
        and describe the sensory characteristics of products, such as food, beverages, personal care products, and more (<I>stimuli</I>).</p>

        <p> The goal of QDA is to generate a detailed, quantitative profile of evaluated stimuli based on sensory attributes.</p>

        <p> Therefore, we are interested in representing the stimuli on a map such that
        two stimuli are all the closer if they have been perceived in the same way.</p>

        <p> The interface provides a map of the stimuli that takes into account the variability of perception of the assessors, by means of 
        confidence ellipses.</p>

        <p> These ellipses are obtained using resampling techniques. New virtual panels are drawn at random and their perceptions are projected as an additional information 
        in order to represent the ellipses.</p>

        <p> Open the <b>sensochoc</b> dataset. Use all the sensory attributes to represent your products. You can see that chocolates
        <I>choc2</I>, <I>choc5</I>, and <I>choc6</I> have a similar sensory profile, as their respective ellipses are overlapping.</p>

        <p> Variability around sensory attributes can also be represented through the <I>Graphic Options</I> menu. In our example, 
        the sensory attribute crunchy is far less clear in the minds of panelists than the sensory attribute cocoa aroma might be.</p>

        <p>______________________________________________________________________________</p>
        
        </div>
        </body>
        </html>"
      )
    },
    
    .run = function() {
      if (is.null(self$options$prod) || is.null(self$options$pane) || is.null(self$options$senso))
        return()
      
      
      private$.errorCheck()
      
      data1 <- self$data[, self$options$prod, drop = FALSE]
      data2 <- self$data[, self$options$pane, drop = FALSE]
      data3 <- self$data[, self$options$senso, drop = FALSE]
      
      data1[[1]] <- as.factor(data1[[1]])
      data2[[1]] <- as.factor(data2[[1]])
      
      if (!all(vapply(data3, is.numeric, logical(1)))) {
        jmvcore::reject("All sensory attributes must be numeric")
        return()
      }
      
      data <- data.frame(data1, data2, data3, check.names = FALSE)
      
      # Process options
      nbsimul_gui    <- self$options$nbsimul
      nbpane_gui     <- self$options$nbpane
      thresh_gui     <- self$options$thresh / 100
      level_gui      <- self$options$level_search / 100
      scaleunit_gui  <- self$options$scale_unit_box
      centerpane_gui <- self$options$center_pane_box
      scalepane_gui  <- self$options$scale_pane_box
      
      # Graphic options
      abs_gui   <- self$options$abs
      ord_gui   <- self$options$ord
      coord_gui <- c(abs_gui, ord_gui)
      cex_gui   <- 1
      namepan_gui <- FALSE
      color_gui <- c("black","red","green3","blue",
                     "cyan","magenta","darkgray","darkgoldenrod","darkgreen","violet","turquoise","orange","lightpink","lavender","yellow","lightgreen","lightgrey",
                     "lightblue","darkkhaki", "darkmagenta","darkolivegreen","lightcyan", "darkorange",
                     "darkorchid","darkred","darksalmon","darkseagreen","darkslateblue","darkslategray","darkslategrey",
                     "darkturquoise","darkviolet", "lightgray","lightsalmon","lightyellow", "maroon")
      
      don.interesting <- tryCatch(
        SensoMineR::search.desc(
          data,
          col.j = 2, col.p = 1,
          firstvar = 3, lastvar = ncol(data),
          level = level_gui
        ),
        error = function(e) {
          jmvcore::reject(paste("search.desc failed:", e$message))
          NULL
        }
      )
      if (is.null(don.interesting))
        return()
      
      don.interesting <- don.interesting[, c(2, 1, 3:ncol(don.interesting)), drop = FALSE]
      
      graxe <- tryCatch(
        private$.constraxes(
          don.interesting,
          group = NULL, name.group = NULL,
          coord = coord_gui,
          scale.unit = scaleunit_gui,
          centerbypanelist = centerpane_gui,
          scalebypanelist = scalepane_gui,
          graph.type = "ggplot",
          quoi = "gr"
        ),
        error = function(e) {
          jmvcore::reject(paste("constraxes failed:", e$message))
          NULL
        }
      )
      if (is.null(graxe))
        return()
      
      res.pca <- private$.PCA(data[, -(1:2), drop = FALSE])
      if (is.null(res.pca)) {
        jmvcore::reject("PCA failed")
        return()
      }
      
      self$results$dimdesc$setContent(private$.dimdesc(res.pca))
      
      self$results$plotpane$setState(don.interesting)
      self$results$plotind$setState(graxe)
      self$results$plotvar$setState(graxe)
      
      n_axes_available <- nrow(res.pca$eig)
      
      coord_gui <- private$.getValidPanAxes(res.pca)
      if (is.null(coord_gui)) {
        n_axes_available <- if (!is.null(res.pca) && !is.null(res.pca$eig)) nrow(res.pca$eig) else 0
        jmvcore::reject(
          paste0(
            "The selected graphical dimensions are not available. ",
            "Please choose dimensions between 1 and ", n_axes_available, "."
          )
        )
        return()
      }
      
      res_pan <- tryCatch(
        private$.panellipse2(
          don.interesting,
          alpha = thresh_gui,
          coord = coord_gui,
          level.search.desc = level_gui,
          scale.unit = scaleunit_gui,
          nbsimul = nbsimul_gui,
          nbchoix = nbpane_gui,
          group = NULL,
          name.group = NULL,
          centerbypanelist = centerpane_gui,
          scalebypanelist = scalepane_gui,
          name.panelist = namepan_gui,
          cex = cex_gui,
          color = color_gui,
          graph.type = "ggplot"
        ),
        error = function(e) {
          jmvcore::reject(paste("Stimulus ellipses could not be computed:", e$message))
          NULL
        }
      )
      if (is.null(res_pan))
        return()
      
      self$results$plotspa$setState(res_pan)
      self$results$plotvaria$setState(res_pan)
      
      private$.eigTab(res_pan)
      if (!is.null(res_pan$hotelling))
        private$.hotTab(res_pan)
    },
    
    .getValidPanAxes = function(res.pca) {
      abs_gui <- suppressWarnings(as.numeric(self$options$abs))
      ord_gui <- suppressWarnings(as.numeric(self$options$ord))
      
      if (is.null(res.pca) || is.null(res.pca$eig))
        return(NULL)
      
      n_axes <- nrow(res.pca$eig)
      
      if (is.na(abs_gui) || is.na(ord_gui) || abs_gui < 1 || ord_gui < 1)
        return(NULL)
      
      if (abs_gui > n_axes || ord_gui > n_axes)
        return(NULL)
      
      c(abs_gui, ord_gui)
    },
    
    .panellipse2 = function(donnee,col.p=1,col.j=2,firstvar=3,lastvar=ncol(donnee),alpha=0.05,coord=c(1,2),scale.unit=TRUE,nbsimul=300,nbchoix=NULL,group=NULL,name.group=NULL,level.search.desc=0.2,centerbypanelist=TRUE,scalebypanelist=FALSE,name.panelist=FALSE,cex=1,color=NULL,graph.type="ggplot"){
      
      hotelling <- function(d1,d2,n1=nrow(d1),n2=nrow(d2)){
        k <- ncol(d1)
        xbar1 <- apply(d1,2,mean)
        xbar2 <- apply(d2,2,mean)
        dbar <- xbar2-xbar1
        if (n1+n2<3) return(NA)
        v <- ((n1-1)*var(d1)+(n2-1)*var(d2))/(n1+n2-2)
        if (sum(v^2) < 1/10^10) return (NA)
        else t2 <- n1*n2*dbar%*%solve(v)%*%dbar/(n1+n2)
        f <- (n1+n2-k-1)*t2/((n1+n2-2)*k)
        return(pf(f,k,n1+n2-k-1,lower.tail=FALSE))
      }
      don.interesting<-donnee
      axe <- private$.constraxes(don.interesting,group=group,name.group=name.group,coord=coord,scale.unit=scale.unit,centerbypanelist=centerbypanelist,scalebypanelist=scalebypanelist,graph.type=graph.type,quoi = "st")
      labprod = axe$moyen[axe$moyen[,ncol(axe$moyen)]==0,ncol(axe$moyen)-1]
      nbprod = length(labprod)
      nbprod= nlevels(as.factor(don.interesting[,2]))
      nbjuge = nlevels(as.factor(don.interesting[,1]))
      if (is.null(nbchoix)) nbchoix = nbjuge
      if (length(group)<2) {
        mat = matrix(NA,nbprod,nbprod)
        aa = axe$moyen[-(1:nbprod),]
        for (i in 1:(nbprod-1)){
          for (j in i:nbprod) mat[i,j] = mat[j,i] = hotelling(aa[aa[,ncol(aa)-1]==labprod[i],coord],aa[aa[,ncol(aa)-1]==labprod[j],coord],nbchoix,nbchoix)
        }
        diag(mat)=1
        colnames(mat)=rownames(mat)=labprod
      }
      
      simul <- simulation(axe,nbgroup=1,nbchoix=nbchoix,nbsimul=nbsimul)
      auxil <- private$.variabvariable(don.interesting,simul$sample,coord=coord,nbsimul=nbsimul, scale.unit=scale.unit,centerbypanelist=centerbypanelist,scalebypanelist=scalebypanelist,graph.type = "ggplot")
      res <- list()
      res$donnee<-don.interesting
      res$simul<-simul
      res$axe<-axe
      res$eig= axe[[length(names(axe))]]
      res$coordinates<- axe[-length(names(axe))]
      if (nbchoix!=1){
        res$hotelling=mat
      }
      res$correl$moy <- auxil$moy
      res$correl$mini <- auxil$mini
      res$correl$maxi <- auxil$maxi
      
      
      return(res)
    },
    
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
    
    .getValidBootAxes = function(res_pan) {
  abs_gui <- suppressWarnings(as.numeric(self$options$abs))
  ord_gui <- suppressWarnings(as.numeric(self$options$ord))
  
  if (is.null(res_pan) || is.null(res_pan$axe) || is.null(res_pan$axe$eig))
    return(NULL)
  
  eig <- res_pan$axe$eig
  
  if (!is.matrix(eig) && !is.data.frame(eig))
    return(NULL)
  
  n_axes <- nrow(eig)
  
  if (is.na(abs_gui) || is.na(ord_gui) || abs_gui < 1 || ord_gui < 1)
    return(NULL)
  
  if (abs_gui > n_axes || ord_gui > n_axes)
    return(NULL)
  
  c(abs_gui, ord_gui)
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
    
    .PCA = function(table) {
      if (is.null(table) || ncol(table) < 2 || nrow(table) < 2)
        return(NULL)
      
      ncp_target <- suppressWarnings(as.numeric(self$options$nFactors))
      if (is.na(ncp_target) || ncp_target < 1)
        ncp_target <- 1
      
      ncp_use <- min(ncp_target, nrow(table) - 1, ncol(table))
      if (!is.finite(ncp_use) || ncp_use < 1)
        return(NULL)
      
      tryCatch(
        FactoMineR::PCA(table, ncp = ncp_use, graph = FALSE),
        error = function(e) NULL
      )
    },
    
    .variabvariable = function(donnee,echantillon,coord=c(1,2), nbsimul=300, scale.unit=TRUE,nbchoix=NULL,centerbypanelist=TRUE,scalebypanelist=FALSE,graph.type=c("ggplot")){
      
      for (j in 1 :2)  donnee[,j] <- as.factor(donnee[,j])
      nbjuge <- length(levels(donnee[,1]))
      nbprod <- length(levels(donnee[,2]))
      nbdesc=ncol(donnee)-2
      nbcoord=max(coord)
      
      oo <- order(donnee[,2])
      donnee <- donnee[oo,]
      oo <- order(donnee[,1])
      donnee <- donnee[oo,]
      tab=SensoMineR::scalebypanelist(donnee,col.j=1,col.p=2,firstvar=3,center=centerbypanelist,scale=scalebypanelist)
      
      tab.moy <- as.matrix(tab[1:nbprod,-(1:2)])
      tab.byjudge <- array(0,dim=c(nbprod,nbdesc,nbjuge))
      for (j in 1:nbjuge) tab.byjudge[,,j] <- as.matrix(tab[(j*nbprod+1):((j+1)*nbprod),-(1:2)])
      correl = array(NA,dim=c(nbdesc,nbdesc,nbsimul))
      res = array(NA,dim=c(nbsimul,ncol(tab.moy),nbcoord))
      for (k in 1:nbsimul){
        Xb = apply(tab.byjudge[,,echantillon[k,]],c(1,2),mean)
        correl[,,k] = cor(Xb)
        resAF <- FactoMineR::PCA(cbind(tab.moy,Xb),quanti.sup=(ncol(tab.moy)+1):(2*ncol(tab.moy)),ncp=nbcoord,scale.unit=scale.unit,axes=coord,graph=FALSE)
        res[k,,] = as.matrix(resAF$quanti.sup$coord)
        
        x <- y <- NULL  ## just to avoid the note no binding ...
        dta <- cbind.data.frame(x=as.vector(res[,,coord[1]]),y=as.vector(res[,,coord[2]]),var=rep(colnames(tab.moy),each=nbsimul))
      }
      mini=maxi=matrix(0,ncol(tab.moy),ncol(tab.moy))
      for (i in 1:ncol(tab.moy)){
        for (j in 1:ncol(tab.moy)){
          mini[i,j] = correl[i,j,order(correl[i,j,])[round(nbsimul*0.025,0)]]
          maxi[i,j] = correl[i,j,order(correl[i,j,])[round(nbsimul*0.975,0)]]
        }}
      colnames(mini)=rownames(mini)=colnames(maxi)=rownames(maxi)=colnames(tab.moy)
      res <- list()
      res$moy <- cor(tab.moy)
      res$mini <- mini
      res$maxi <- maxi
      return(res)
    },
    
    .constraxes = function(matrice,coord=c(1,2),scale.unit=TRUE,group=NULL,name.group=NULL,centerbypanelist=TRUE,scalebypanelist=FALSE,method="coeff",graph.type=c("ggplot"),quoi="st"){
      
      nbcoord <- max(coord)
      oo <- order(matrice[,2])
      matrice <- matrice[oo,]
      oo <- order(matrice[,1])
      matrice <- matrice[oo,]
      
      nbjuge <- nlevels(matrice[,1])
      if (0%in% summary(matrice[,1])) nbjuge <- nbjuge-1
      nbprod <- length(levels(matrice[,2]))
      nbdesc <- ncol(matrice)-2
      
      moy.aux <- SensoMineR::scalebypanelist(matrice,col.j=1,col.p=2,firstvar=3,center=centerbypanelist,scale=scalebypanelist,method=method)
      rownames(moy.aux) <- paste("i",1:nrow(moy.aux),sep="")
      rownames(moy.aux)[1:nbprod] <- as.character(moy.aux[1:nbprod,2])
      ###AF with active data the averages for all the panelist
      
      if (quoi=="gr"){
        FactoMineR::PCA(moy.aux[,-c(1,2)],ind.sup = (nbprod+1):nrow(moy.aux), scale.unit = scale.unit, ncp = nbcoord,graph=FALSE)
      }
      else if (quoi=="st"){
        axe <- list()
        res_af<-FactoMineR::PCA(moy.aux[,-c(1,2)],ind.sup = (nbprod+1):nrow(moy.aux), scale.unit = scale.unit, ncp = nbcoord,graph=FALSE)
        axe$moyen <- data.frame(rbind(res_af$ind$coord,res_af$ind.sup$coord),as.factor(moy.aux[,2]),as.factor(moy.aux[,1]))
        dimnames(axe$moyen)[2][[1]]<-c (paste("Dim", 1:nbcoord, sep = ""),"Product","Panelist")
        axe$eig <- res_af$eig
        return(axe)
      }
    },

    .eigTab = function(res){
      eige<-res$eig
      for (i in 1:dim(eige)[1]){
        self$results$eigenGr$eigen_table$addRow(rowKey=i, values=list(facto=paste0("Dim. ",i), eig=eige[,1][i], eig_pct=eige[,2][i], eig_pct_cum=eige[,3][i]))
      }
    },
    #Table des correlations des produits
    .hotTab = function(res) {
      hote <- res$hotelling
      if (is.null(hote))
        return()
      
      self$results$hotGr$hot_table$addColumn(name = "Product", title = "Product", type = "text")
      
      for (i in seq_len(nrow(hote))) {
        self$results$hotGr$hot_table$addColumn(
          name = as.character(colnames(hote)[i]),
          title = as.character(colnames(hote)[i]),
          type = "number"
        )
        self$results$hotGr$hot_table$addRow(rowKey = i, values = list())
      }
      
      for (i in seq_len(nrow(hote))) {
        row <- list()
        row[["Product"]] <- as.character(colnames(hote)[i])
        for (j in seq_len(ncol(hote)))
          row[[as.character(colnames(hote)[j])]] <- round(hote[i, j], 5)
        self$results$hotGr$hot_table$setRow(rowNo = i, values = row)
      }
    },
    
.plotSpace = function(image, ...){
  
  if (is.null(self$options$prod) || is.null(self$options$pane) ||
      is.null(self$options$senso) || (length(self$options$senso) < self$options$nFactors))
    return(FALSE)
  
  res_pan <- image$state
  if (is.null(res_pan))
    return(FALSE)
  
  coord_gui <- private$.getValidBootAxes(res_pan)
  if (is.null(coord_gui)) {
    jmvcore::reject("The selected graphical dimensions are not available. Please choose dimensions within the range of the computed axes.")
    return(FALSE)
  }
  
  # Process options
  scaleunit_gui  <- self$options$scale_unit_box
  centerpane_gui <- self$options$center_pane_box
  scalepane_gui  <- self$options$scale_pane_box
  nbsimul_gui    <- self$options$nbsimul
  nbpane_gui     <- self$options$nbpane
  
  # Graphic options
  cex_gui   <- 1
  alpha_gui <- self$options$thresh / 100
  color <- c("black","red","green3","blue",
             "cyan","magenta","darkgray","darkgoldenrod","darkgreen","violet","turquoise","orange","lightpink","lavender","yellow","lightgreen","lightgrey",
             "lightblue","darkkhaki", "darkmagenta","darkolivegreen","lightcyan", "darkorange",
             "darkorchid","darkred","darksalmon","darkseagreen","darkslateblue","darkslategray","darkslategrey",
             "darkturquoise","darkviolet", "lightgray","lightsalmon","lightyellow", "maroon")
  
  ok <- tryCatch({
    mat <- res_pan$simul
    eig <- signif(as.matrix(res_pan$axe$eig), 4)
    
    ################################################################
    ellipse2 <- function(loc, cov,alpha)
    {
      A <- cov
      detA <- A[1, 1] * A[2, 2] - A[1, 2]^2
      dist <- sqrt(qchisq(1-alpha, 2))
      ylimit <- sqrt(A[2, 2]) * dist
      y <- seq( - ylimit, ylimit, 0.01 * ylimit)
      sqrt.discr <- sqrt(detA/A[2, 2]^2 * abs(A[2, 2] * dist^2 - y^2))
      sqrt.discr[c(1, length(sqrt.discr))] <- 0
      b <- loc[1] + A[1, 2]/A[2, 2] * y
      x1 <- b - sqrt.discr
      x2 <- b + sqrt.discr
      y <- loc[2] + y
      return(rbind(cbind(x1, y), cbind(rev(x2), rev(y))))
    }
    #################################################################
    
    matP     <- cbind.data.frame(mat$moy$P[, coord_gui, drop = FALSE], mat$moy$P[, ncol(mat$moy$P), drop = FALSE])
    matPJ    <- cbind.data.frame(mat$moy$PJ[, coord_gui, drop = FALSE], mat$moy$PJ[, ncol(mat$moy$PJ), drop = FALSE])
    matsimul <- cbind.data.frame(mat$moy$simul[, coord_gui, drop = FALSE], mat$moy$simul[, ncol(mat$moy$simul), drop = FALSE])
    
    nbprod <- nrow(matP)
    coord.ellipse.a.tracer <- matrix(0, 402, 2 * nbprod)
    
    nbjuge <- nrow(matPJ) / nrow(matP)
    nbsimul <- nrow(matsimul) / nrow(matP)
    nbgroup <- 1
    
    for (i in 1:nbprod) {
      VX <- var(matsimul[((i - 1) * nbsimul + 1):(i * nbsimul), 1:2])
      coord.ellipse.a.tracer[, (1 + 2 * (i - 1)):(2 * i)] <- ellipse2(t(matP[i, 1:2]), VX, alpha_gui)
    }
    
    minx <- min(coord.ellipse.a.tracer[, 1 + 2 * (0:(nbprod - 1))], na.rm = TRUE)
    maxx <- max(coord.ellipse.a.tracer[, 1 + 2 * (0:(nbprod - 1))], na.rm = TRUE)
    miny <- min(coord.ellipse.a.tracer[, 2 * (1:nbprod)], na.rm = TRUE)
    maxy <- max(coord.ellipse.a.tracer[, 2 * (1:nbprod)], na.rm = TRUE)
    
    lab.x <- paste0("Dim ", coord_gui[1], " (", format(eig[coord_gui[1], 2], nsmall = 2, digits = 2), "%)")
    lab.y <- paste0("Dim ", coord_gui[2], " (", format(eig[coord_gui[2], 2], nsmall = 2, digits = 2), "%)")
    
    gg_graph <- ggplot() +
      coord_fixed(ratio = 1) +
      xlim(c(minx, maxx)) + ylim(c(miny, maxy)) +
      geom_hline(yintercept = 0, lty = 2) +
      geom_vline(xintercept = 0, lty = 2) +
      theme_light() +
      theme(axis.title = element_text(hjust = 1, face = 2), plot.title = element_text(hjust = 0.5, face = 2)) +
      labs(x = lab.x, y = lab.y, title = "Representation of the Stimuli with Ellipses") +
      ggrepel::geom_text_repel(aes(x = matP[, 1], y = matP[, 2], label = matP[, ncol(matP)]), color = color[1:nbprod]) +
      geom_point(aes(x = matP[, 1], y = matP[, 2]), color = rep(color[1:nbprod], 1), pch = 20, size = 0.8 * cex_gui * 3)
    
    for (j in 1:nbgroup) {
      for (i in 1:nbprod) {
        ellipse_df <- data.frame(
          x = coord.ellipse.a.tracer[, 2 * (i + (j - 1) * nbprod) - 1],
          y = coord.ellipse.a.tracer[, 2 * (i + (j - 1) * nbprod)]
        )
        gg_graph <- gg_graph + geom_path(
          data  = ellipse_df,
          aes(x = x, y = y),
          color = color[i], lty = j
        )
      }
    }
    
    print(gg_graph)
    TRUE
  }, error = function(e) {
    jmvcore::reject(paste("Stimulus space plot failed:", e$message))
    FALSE
  })
  
  ok
},
    
.plotvariavar = function(image, ...) {
  
  if (is.null(self$options$prod) || is.null(self$options$pane) ||
      is.null(self$options$senso) || (length(self$options$senso) < self$options$nFactors))
    return(FALSE)
  
  res_pan <- image$state
  if (is.null(res_pan))
    return(FALSE)
  
  abs_gui   <- suppressWarnings(as.numeric(self$options$abs))
  ord_gui   <- suppressWarnings(as.numeric(self$options$ord))
  coord_gui <- c(abs_gui, ord_gui)
  
  if (any(is.na(coord_gui)) || any(coord_gui < 1)) {
    jmvcore::reject("The selected graphical dimensions are not valid.")
    return(FALSE)
  }
  
  if (is.null(res_pan$axe) || is.null(res_pan$axe$eig) ||
      !is.matrix(res_pan$axe$eig) && !is.data.frame(res_pan$axe$eig)) {
    jmvcore::reject("The graphical dimensions are not available.")
    return(FALSE)
  }
  
  n_axes <- nrow(res_pan$axe$eig)
  if (max(coord_gui) > n_axes) {
    jmvcore::reject("The selected graphical dimensions are not available. Please choose dimensions within the range of the computed axes.")
    return(FALSE)
  }
  
  ok <- tryCatch({
    
    # Process options
    scaleunit_gui  <- self$options$scale_unit_box
    centerpane_gui <- self$options$center_pane_box
    scalepane_gui  <- self$options$scale_pane_box
    nbsimul_gui    <- self$options$nbsimul
    
    # Graphic options
    cex_gui <- 1
    colours <- c("black","red","green3","blue",
                 "cyan","magenta","darkgray","darkgoldenrod","darkgreen","violet","turquoise","orange","lightpink","lavender","yellow","lightgreen","lightgrey",
                 "lightblue","darkkhaki", "darkmagenta","darkolivegreen","lightcyan", "darkorange",
                 "darkorchid","darkred","darksalmon","darkseagreen","darkslateblue","darkslategray","darkslategrey",
                 "darkturquoise","darkviolet", "lightgray","lightsalmon","lightyellow", "maroon")
    
    eig <- signif(as.matrix(res_pan$axe$eig), 4)
    
    donnee      <- res_pan$donnee
    echantillon <- res_pan$simul$sample
    
    for (j in 1:2)
      donnee[, j] <- as.factor(donnee[, j])
    
    nbjuge <- length(levels(donnee[, 1]))
    nbprod <- length(levels(donnee[, 2]))
    nbdesc <- ncol(donnee) - 2
    nbcoord <- max(coord_gui)
    
    oo <- order(donnee[, 2])
    donnee <- donnee[oo, ]
    oo <- order(donnee[, 1])
    donnee <- donnee[oo, ]
    
    tab <- SensoMineR::scalebypanelist(
      donnee,
      col.j = 1,
      col.p = 2,
      firstvar = 3,
      center = centerpane_gui,
      scale = scalepane_gui
    )
    
    tab.moy <- as.matrix(tab[1:nbprod, -(1:2)])
    tab.byjudge <- array(0, dim = c(nbprod, nbdesc, nbjuge))
    for (j in 1:nbjuge)
      tab.byjudge[, , j] <- as.matrix(tab[(j * nbprod + 1):((j + 1) * nbprod), -(1:2)])
    
    res <- array(NA, dim = c(nbsimul_gui, ncol(tab.moy), nbcoord))
    
    gg_graph <- NULL
    
    for (k in 1:nbsimul_gui) {
      Xb <- apply(tab.byjudge[, , echantillon[k, ]], c(1, 2), mean)
      
      resAF <- FactoMineR::PCA(
        cbind(tab.moy, Xb),
        quanti.sup = (ncol(tab.moy) + 1):(2 * ncol(tab.moy)),
        ncp = nbcoord,
        scale.unit = scaleunit_gui,
        axes = coord_gui,
        graph = FALSE
      )
      
      res[k, , ] <- as.matrix(resAF$quanti.sup$coord)
      
    }
    
    gg_graph <- plot(
      resAF,
      choix = "var",
      invisible = "quanti.sup",
      axes = coord_gui,
      hab = "none"
    )
    
    dta <- cbind.data.frame(
      x   = as.vector(res[, , coord_gui[1]]),
      y   = as.vector(res[, , coord_gui[2]]),
      var = rep(colnames(tab.moy), each = nbsimul_gui)
    )
    
    gg_graph <- gg_graph +
      geom_point(
        data = dta,
        aes(x = x, y = y, col = var),
        alpha = 0.2,
        pch = 15,
        size = 1.2
      ) +
      guides(color = guide_legend(title = NULL, override.aes = list(size = 2, alpha = 1))) +
      ggtitle("Variability of the Sensory Attributes") +
      scale_color_manual(values = colours)
    
    print(gg_graph)
    TRUE
    
  }, error = function(e) {
    jmvcore::reject(paste("Sensory attribute variability plot failed:", e$message))
    FALSE
  })
  
  ok
},
    
    .plotPane = function(image, ...) {
      if (is.null(self$options$prod) || is.null(self$options$pane) ||
          is.null(self$options$senso) || (length(self$options$senso) < self$options$nFactors))
        return(FALSE)
      
      don.interesting <- image$state
      if (is.null(don.interesting))
        return(FALSE)
      
      scaleunit_gui  <- self$options$scale_unit_box
      centerpane_gui <- self$options$center_pane_box
      scalepane_gui  <- self$options$scale_pane_box
      abs_gui        <- self$options$abs
      ord_gui        <- self$options$ord
      coord_gui      <- c(abs_gui, ord_gui)
      cex_gui        <- 1
      namepan_gui    <- FALSE
      
      color <- c("black","red","green3","blue","cyan","magenta","darkgray","darkgoldenrod",
                 "darkgreen","violet","turquoise","orange","lightpink","lavender","yellow",
                 "lightgreen","lightgrey","lightblue","darkkhaki","darkmagenta","darkolivegreen",
                 "lightcyan","darkorange","darkorchid","darkred","darksalmon","darkseagreen",
                 "darkslateblue","darkslategray","darkslategrey","darkturquoise","darkviolet",
                 "lightgray","lightsalmon","lightyellow","maroon")
      
      ok <- tryCatch({
        axe <- private$.constraxes(
          don.interesting,
          group = NULL, name.group = NULL,
          coord = coord_gui,
          scale.unit = scaleunit_gui,
          centerbypanelist = centerpane_gui,
          scalebypanelist = scalepane_gui,
          graph.type = "ggplot",
          quoi = "st"
        )
        
        if (is.null(axe) || is.null(axe$moyen) || is.null(axe$eig))
          return(FALSE)
        
        mat <- axe$moyen
        eig <- signif(axe$eig, 4)
        
        cex  <- cex_gui
        name <- namepan_gui
        
        mat <- cbind.data.frame(mat[, coord_gui, drop = FALSE], mat[, (ncol(mat)-1):ncol(mat), drop = FALSE])
        nbprod <- length(levels(mat[, ncol(mat)-1]))
        nbpoint <- nbprod * (length(levels(mat[, ncol(mat)])) - 1 + 1)
        
        minx <- min(mat[1:nbpoint, 1], na.rm = TRUE)
        maxx <- max(mat[1:nbpoint, 1], na.rm = TRUE)
        miny <- min(mat[1:nbpoint, 2], na.rm = TRUE)
        maxy <- max(mat[1:nbpoint, 2], na.rm = TRUE)
        
        gg_graph <- ggplot() +
          coord_fixed(ratio = 1) +
          xlim(c(minx, maxx)) + ylim(c(miny, maxy)) +
          geom_hline(yintercept = 0, lty = 2) +
          geom_vline(xintercept = 0, lty = 2) +
          theme_light() +
          theme(axis.title = element_text(hjust = 1, face = 2),
                plot.title = element_text(hjust = 0.5, face = 2)) +
          labs(
            title = "Individual Variability Around Stimuli",
            x = paste0("Dim ", coord_gui[1], " (", eig[coord_gui[1], 2], "%)"),
            y = paste0("Dim ", coord_gui[2], " (", eig[coord_gui[2], 2], "%)")
          ) +
          geom_point(aes(x = mat[1:nbprod, 1], y = mat[1:nbprod, 2]),
                     col = color[1:nbprod], pch = 15) +
          ggrepel::geom_text_repel(
            aes(x = mat[1:nbprod, 1], y = mat[1:nbprod, 2], label = rownames(mat)[1:nbprod]),
            color = color[1:nbprod]
          ) +
          geom_point(
            aes(x = mat[-(1:nbprod), 1], y = mat[-(1:nbprod), 2]),
            col = rep(color[1:nbprod], nrow(mat) / nbprod - 1),
            pch = 20
          )
        
        if (isTRUE(name)) {
          gg_graph <- gg_graph + geom_text(
            aes(x = mat[-(1:nbprod), 1], y = mat[-(1:nbprod), 2], label = mat[-(1:nbprod), 4]),
            col = rep(color[1:nbprod], nrow(mat) / nbprod - 1),
            size = 3
          )
        }
        
        print(gg_graph)
        TRUE
      }, error = function(e) {
        jmvcore::reject(paste("Panelist variability plot failed:", e$message))
        FALSE
      })
      
      ok
    },
    
    .plotIndaxe = function(image, ...) {
      if (is.null(self$options$prod) || is.null(self$options$pane) ||
          is.null(self$options$senso) || (length(self$options$senso) < self$options$nFactors))
        return(FALSE)
      
      graxe <- image$state
      if (is.null(graxe))
        return(FALSE)
      
      coord_gui <- private$.getValidAxes(graxe)
      if (is.null(coord_gui))
        return(FALSE)
      
      ok <- tryCatch({
        ploti <- FactoMineR::plot.PCA(
          graxe,
          choix = "ind",
          invisible = "ind.sup",
          axes = coord_gui,
          title = "Representation of the Stimuli"
        )
        print(ploti)
        TRUE
      }, error = function(e) {
        jmvcore::reject(paste("Stimuli plot failed:", e$message))
        FALSE
      })
      
      ok
    },
    
    .plotVaraxe = function(image, ...) {
      if (is.null(self$options$prod) || is.null(self$options$pane) ||
          is.null(self$options$senso) || (length(self$options$senso) < self$options$nFactors))
        return(FALSE)
      
      graxe <- image$state
      if (is.null(graxe))
        return(FALSE)
      
      coord_gui <- private$.getValidAxes(graxe)
      if (is.null(coord_gui))
        return(FALSE)
      
      ok <- tryCatch({
        plotv <- FactoMineR::plot.PCA(
          graxe,
          choix = "var",
          habillage = "group",
          axes = coord_gui,
          title = "Representation of the Sensory Attributes"
        )
        print(plotv)
        TRUE
      }, error = function(e) {
        jmvcore::reject(paste("Attribute plot failed:", e$message))
        FALSE
      })
      
      ok
    },
    
    .errorCheck = function() {
      nFactors <- suppressWarnings(as.numeric(self$options$nFactors))
      
      if (is.na(nFactors) || nFactors < 1) {
        jmvcore::reject("Number of dimensions must be at least 1")
        return()
      }
      
      if (is.null(self$options$senso) || length(self$options$senso) < 2) {
        jmvcore::reject("At least two sensory attributes are required")
        return()
      }
      
      if (length(self$options$senso) < nFactors) {
        jmvcore::reject("Number of dimensions cannot exceed the number of sensory attributes")
        return()
      }
    }
  )
)