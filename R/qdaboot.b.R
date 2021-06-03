
# This file is a generated template, your changes will not be overwritten

QDABOOTClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "QDABOOTClass",
    inherit = QDABOOTBase,
    private = list(
        .run = function() {
            options(error = recover)
            if(is.null(self$options$prod)||is.null(self$options$pane)||is.null(self$options$senso)) {
                return()
            }
            else if (length(self$options$senso)<self$options$nFactors) {
                private$.errorCheck()
                return()
            }
            else{
                #Preparation du dataframe sur lequel on va effectuer les fonctions
                data1<-(self$data[,self$options$prod])
                data2<-(self$data[,self$options$pane])
                data3<-(self$data[,self$options$senso])
                data<-data.frame(data1,data2,data3)
                
                #Process options
                nbsimul_gui=self$options$nbsimul
                nbpane_gui=self$options$nbpane
                thresh_gui=self$options$thresh/100
                level_gui=self$options$level_search/100
                scaleunit_gui=self$options$scale_unit_box
                centerpane_gui=self$options$center_pane_box
                scalepane_gui=self$options$scale_pane_box
                
                #Graphic options
                abs_gui=self$options$abs
                ord_gui=self$options$ord
                coord_gui=c(abs_gui,ord_gui)
                cex_gui=1
                namepan_gui=FALSE
                color_gui = c("black","red","green3","blue",
                              "cyan","magenta","darkgray","darkgoldenrod","darkgreen","violet","turquoise","orange","lightpink","lavender","yellow","lightgreen","lightgrey",
                              "lightblue","darkkhaki", "darkmagenta","darkolivegreen","lightcyan", "darkorange",
                              "darkorchid","darkred","darksalmon","darkseagreen","darkslateblue","darkslategray","darkslategrey",
                              "darkturquoise","darkviolet", "lightgray","lightsalmon","lightyellow", "maroon")
                
                
                # Lancement de panellipse
                don.interesting <- SensoMineR::search.desc(data,col.j=2,col.p=1,firstvar=3,lastvar=ncol(data),level=level_gui)
                don.interesting <- don.interesting[,c(2,1,3:ncol(don.interesting))]
                
                graxe<-private$.constraxes(don.interesting,group=NULL,name.group=NULL,coord=coord_gui,scale.unit=scaleunit_gui,centerbypanelist=centerpane_gui,scalebypanelist=scalepane_gui,graph.type="ggplot",quoi = "gr")
                
                res.pca <- private$.PCA(data[,-(1:2)])
                res.dimdesc<-private$.dimdesc(res.pca)
                
                self$results$dimdesc$setContent(res.dimdesc)
                
                imagepane=self$results$plotpane
                imagepane$setState(don.interesting)

                imageind=self$results$plotind
                imageind$setState(graxe)

                imagevar=self$results$plotvar
                imagevar$setState(graxe)

                res_pan<-private$.panellipse2(don.interesting, alpha=thresh_gui, coord=coord_gui,level.search.desc = level_gui, scale.unit = scaleunit_gui, nbsimul = nbsimul_gui, nbchoix = nbpane_gui, group = NULL, name.group = NULL, centerbypanelist = centerpane_gui, scalebypanelist = scalepane_gui, name.panelist = namepan_gui, cex = cex_gui, color = color_gui,graph.type = "ggplot")

                imagespace=self$results$plotspa
                imagespace$setState(res_pan)

                imagevaria=self$results$plotvaria
                imagevaria$setState(res_pan)

                # private$.Coord(res_pan)

                # Affichage des différentes tables en options
                private$.eigTab(res_pan)
                private$.hotTab(res_pan)
                # private$.corTab(res_pan,"moy")
                # private$.corTab(res_pan,"min")
                # private$.corTab(res_pan,"max")
            }
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
        
        .dimdesc = function(table) {
            
            proba=self$options$proba/100
            nFactors=self$options$nFactors
            
            res=FactoMineR::dimdesc(table, axes=1:nFactors, proba = proba)
            print(res[-length(res)])
            
        },
        
        .PCA = function(table) {
            
            nFactors=self$options$nFactors
            FactoMineR::PCA(table, ncp=nFactors,graph=FALSE)
            
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
        
        .constraxes = function(matrice,coord=c(1,2),scale.unit=scale.unit,group=NULL,name.group=NULL,centerbypanelist=TRUE,scalebypanelist=FALSE,method="coeff",graph.type=c("ggplot"),quoi="st"){
            
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
        # # Table des coordonnées des produits moyens et des individus
        # .Coord = function(res){
        #     moyen<-res$coordinates$moyen
        #     for (i in 1:dim(moyen)[1]){
        #         if (moyen$Panelist[i]==0){
        #             self$results$coord$avgprod$addRow(rowKey=i, values=list(Product=as.character(moyen[,3])[i],Dim1=moyen[,1][i],Dim2=moyen[,2][i]))
        #         }
        #         if (moyen$Panelist[i]!=0){
        #             self$results$coord$indiv$addRow(rowKey=i, values=list(Panelist=as.character(moyen[,4])[i],Product=as.character(moyen[,3])[i],Dim_1=moyen[,1][i],Dim_2=moyen[,2][i]))
        #         }
        #     }
        # },
        # Table des valeurs propres
        .eigTab = function(res){
            eige<-res$eig
            for (i in 1:dim(eige)[1]){
                self$results$eigenGr$eigen_table$addRow(rowKey=i, values=list(facto=paste0("Dim. ",i), eig=eige[,1][i], eig_pct=eige[,2][i], eig_pct_cum=eige[,3][i]))
            }
        },
        #Table des correlations des produits
        .hotTab = function(res){
            hote<-res$hotelling
            self$results$hotGr$hot_table$addColumn(name = as.character("Product"),title=as.character("Product"),type="text")
            for (i in 1:dim(hote)[1]){
                self$results$hotGr$hot_table$addColumn(name = as.character(colnames(hote)[i]),title = as.character(colnames(hote)[i]),type="number")
                self$results$hotGr$hot_table$addRow(rowKey=i, values=list())
            }
            for (i in 1:dim(hote)[1]){
                row=list()
                row[["Product"]]=as.character(colnames(hote)[i])
                for (j in 1:dim(hote)[1]){
                    row[[as.character(colnames(hote)[j])]]=round(hote[,j][i],5)
                    self$results$hotGr$hot_table$setRow(rowNo=i, values=row)
                }
            }
        },
        #Table des correlations
        # .corTab = function(res, quoi){
        #     if (quoi=="moy"){
        #         corGrp<-self$results$corGr$cor_tab_moy
        #         corr<-as.data.frame(res$correl$moy)
        #     }
        #     else if (quoi=="min"){
        #         corGrp<-self$results$corGr$cor_tab_min
        #         corr<-as.data.frame(res$correl$min)
        #     }
        #     else if (quoi=="max"){
        #         corGrp<-self$results$corGr$cor_tab_max
        #         corr<-as.data.frame(res$correl$max)
        #     }
        #     # Cr?ation de la premi?re colonne
        #     corGrp$addColumn(name="Sensory attribute",type="text")
        # 
        #     for (i in 1:nrow(corr)){
        #         corGrp$addColumn(name = colnames(corr)[i],title = as.character(colnames(corr)[i]),type="number")
        #         corGrp$addRow(rowKey=i, values=list())
        #     }
        #     #remplissage du tableau
        #     for (i in 1:dim(corr)[1]){
        #         row=list()
        #         #Affectation a row de la premiere colonne avec le noms des colonnes du tableau des correlations
        #         row[["Sensory attribute"]]=colnames(corr)[i]
        #         for (j in 1:dim(corr)[1]){
        #             #Affectation a row des autres valeurs
        #             row[[colnames(corr)[j]]]=corr[,j][i]
        #             #Remplissage du tableau
        #             corGrp$setRow(rowNo=i, values=row)
        #         }
        #     }
        # },

        .plotSpace = function(image, ...){

            if(is.null(self$options$prod)||is.null(self$options$pane)||is.null(self$options$senso)||(length(self$options$senso)<self$options$nFactors))return()
            res_pan=image$state

            # Process options
            scaleunit_gui=self$options$scale_unit_box
            centerpane_gui=self$options$center_pane_box
            scalepane_gui=self$options$scale_pane_box
            nbsimul_gui=self$options$nbsimul
            nbpane_gui=self$options$nbpane

            # Graphic options
            abs_gui=self$options$abs
            ord_gui=self$options$ord
            coord_gui=c(abs_gui,ord_gui)
            cex_gui=1
            alpha_gui=self$options$thresh/100
            color = c("black","red","green3","blue",
                      "cyan","magenta","darkgray","darkgoldenrod","darkgreen","violet","turquoise","orange","lightpink","lavender","yellow","lightgreen","lightgrey",
                      "lightblue","darkkhaki", "darkmagenta","darkolivegreen","lightcyan", "darkorange",
                      "darkorchid","darkred","darksalmon","darkseagreen","darkslateblue","darkslategray","darkslategrey",
                      "darkturquoise","darkviolet", "lightgray","lightsalmon","lightyellow", "maroon")

            mat<-res_pan$simul
            eig<-signif(res_pan$axe$eig,4)

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


            matP=cbind.data.frame(mat$moy$P[,coord_gui],mat$moy$P[,ncol(mat$moy$P)])
            matPJ=cbind.data.frame(mat$moy$PJ[,coord_gui],mat$moy$PJ[,ncol(mat$moy$PJ)])
            matsimul=cbind.data.frame(mat$moy$simul[,coord_gui],mat$moy$simul[,ncol(mat$moy$simul)])

            nbprod <- nrow(matP)
            coord.ellipse.a.tracer <- matrix(0,402,2*nbprod)

            p <- 2
            nbjuge <-  nrow(matPJ)/nrow(matP)
            nbsimul <-  nrow(matsimul)/nrow(matP)
            nbgroup<- 1
            for (i in 1:nbprod){
                VX <- var(matsimul[((i-1)*nbsimul+1):(i*nbsimul),1:2])
                coord.ellipse.a.tracer[,(1+2*(i-1)):(2*i)] <- ellipse2(t(matP[i,1:2]),VX,alpha_gui)
            }

            minx <- min(coord.ellipse.a.tracer[,1+2*(0:(nbprod-1))],na.rm=TRUE)
            maxx <- max(coord.ellipse.a.tracer[,1+2*(0:(nbprod-1))],na.rm=TRUE)
            miny <- min(coord.ellipse.a.tracer[,2*(1:nbprod)],na.rm=TRUE)
            maxy <- max(coord.ellipse.a.tracer[,2*(1:nbprod)],na.rm=TRUE)

            lab.x <- paste0("Dim ",coord_gui[1]," (",format(eig[coord_gui[1],2],nsmall=2,digits=2),"%)")
            lab.y <- paste0("Dim ",coord_gui[2]," (",format(eig[coord_gui[2],2],nsmall=2,digits=2),"%)")

            gg_graph <- ggplot()+ coord_fixed(ratio = 1) +  xlim(c(minx,maxx)) + ylim(c(miny,maxy)) +
                geom_hline(yintercept = 0,lty=2) + geom_vline(xintercept = 0,lty=2) +
                theme_light()+ theme(axis.title = element_text(hjust = 1, face = 2), plot.title = element_text(hjust = 0.5, face = 2))
            gg_graph <- gg_graph + labs(x = lab.x, y= lab.y,title = "Representation of the Stimuli with Ellipses")
            gg_graph <- gg_graph + ggrepel::geom_text_repel(aes(x=matP[,1], y=matP[,2], label=matP[,ncol(matP)]), color = color[1:nbprod])
            gg_graph <- gg_graph + geom_point(aes(x=matP[,1], y=matP[,2]),color=rep(color[1:nbprod],1),pch=20,size=0.8*cex_gui*3)

            for (j in 1:nbgroup){
                for (i in 1:nbprod)  gg_graph <- gg_graph + geom_path(aes_string(x=coord.ellipse.a.tracer[,2*(i+(j-1)*nbprod)-1],y=coord.ellipse.a.tracer[,2*(i+(j-1)*nbprod)]), color =color[i],lty=j)
            }
            print(gg_graph)
            TRUE
        },

        .plotvariavar = function(image, ...){

            if(is.null(self$options$prod)||is.null(self$options$pane)||is.null(self$options$senso)||(length(self$options$senso)<self$options$nFactors))return()
            res_pan=image$state

            # Process options
            scaleunit_gui=self$options$scale_unit_box
            centerpane_gui=self$options$center_pane_box
            scalepane_gui=self$options$scale_pane_box
            nbsimul_gui=self$options$nbsimul
            nbpane_gui=self$options$nbpane
            alpha_gui=self$options$thresh/100

            # Graphic options
            abs_gui=self$options$abs
            ord_gui=self$options$ord
            coord_gui=c(abs_gui,ord_gui)
            cex_gui=1
            colours = c("black","red","green3","blue",
                        "cyan","magenta","darkgray","darkgoldenrod","darkgreen","violet","turquoise","orange","lightpink","lavender","yellow","lightgreen","lightgrey",
                        "lightblue","darkkhaki", "darkmagenta","darkolivegreen","lightcyan", "darkorange",
                        "darkorchid","darkred","darksalmon","darkseagreen","darkslateblue","darkslategray","darkslategrey",
                        "darkturquoise","darkviolet", "lightgray","lightsalmon","lightyellow", "maroon")

            mat<-res_pan$simul
            coord<-coord_gui
            eig<-signif(res_pan$axe$eig,4)
            nbgroup=1

            donnee<-res_pan$donnee
            echantillon<-res_pan$simul$sample

            for (j in 1 :2)  donnee[,j] <- as.factor(donnee[,j])
            nbjuge <- length(levels(donnee[,1]))
            nbprod <- length(levels(donnee[,2]))
            nbdesc=ncol(donnee)-2
            nbcoord=max(coord_gui)

            oo <- order(donnee[,2])
            donnee <- donnee[oo,]
            oo <- order(donnee[,1])
            donnee <- donnee[oo,]
            tab=SensoMineR::scalebypanelist(donnee,col.j=1,col.p=2,firstvar=3,center=centerpane_gui,scale=scalepane_gui)

            tab.moy <- as.matrix(tab[1:nbprod,-(1:2)])
            tab.byjudge <- array(0,dim=c(nbprod,nbdesc,nbjuge))
            for (j in 1:nbjuge) tab.byjudge[,,j] <- as.matrix(tab[(j*nbprod+1):((j+1)*nbprod),-(1:2)])
            correl = array(NA,dim=c(nbdesc,nbdesc,nbsimul_gui))
            res = array(NA,dim=c(nbsimul_gui,ncol(tab.moy),nbcoord))
            for (k in 1:nbsimul_gui){
                Xb = apply(tab.byjudge[,,echantillon[k,]],c(1,2),mean)
                correl[,,k] = cor(Xb)
                resAF <- FactoMineR::PCA(cbind(tab.moy,Xb),quanti.sup=(ncol(tab.moy)+1):(2*ncol(tab.moy)),ncp=nbcoord,scale.unit=scaleunit_gui,axes=coord_gui,graph=FALSE)
                res[k,,] = as.matrix(resAF$quanti.sup$coord)

                #Graphique
                x <- y <- NULL  ## just to avoid the note no binding ...
                gg_graph <- plot(resAF,choix="var",invisible="quanti.sup", axes=coord_gui, hab="none")
                dta <- cbind.data.frame(x=as.vector(res[,,coord[1]]),y=as.vector(res[,,coord[2]]),var=rep(colnames(tab.moy),each=nbsimul_gui))
                gg_graph <- gg_graph + geom_point(data=dta,aes(x=x,y=y,col=var),alpha=0.2,pch=15,cex=0.4)+guides(color = guide_legend(title=NULL,override.aes = list(size = 2,alpha=1)))+ggtitle("Variability of the Sensory Attributes")
                #coloration
                gg_graph <- gg_graph + scale_color_manual(values=colours)
            }
            print(gg_graph)
            TRUE
        },
        .plotPane = function(image, ...){
            #Parameters
            if(is.null(self$options$prod)||is.null(self$options$pane)||is.null(self$options$senso)||(length(self$options$senso)<self$options$nFactors))return()
            don.interesting=image$state

            scaleunit_gui=self$options$scale_unit_box
            centerpane_gui=self$options$center_pane_box
            scalepane_gui=self$options$scale_pane_box
            abs_gui=self$options$abs
            ord_gui=self$options$ord
            coord_gui=c(abs_gui,ord_gui)
            cex_gui=1
            namepan_gui=FALSE

            axe <- private$.constraxes(don.interesting,group=NULL,name.group=NULL,coord=coord_gui,scale.unit=scaleunit_gui,centerbypanelist=centerpane_gui,scalebypanelist=scalepane_gui,graph.type="ggplot",quoi = "st")
            mat<-axe$moyen
            eig<-signif(axe$eig,4)

            cex=cex_gui
            name<-namepan_gui
            color = c("black","red","green3","blue","cyan","magenta","darkgray","darkgoldenrod","darkgreen","violet","turquoise","orange","lightpink","lavender","yellow","lightgreen","lightgrey","lightblue","darkkhaki", "darkmagenta","darkolivegreen","lightcyan", "darkorange","darkorchid","darkred","darksalmon","darkseagreen","darkslateblue","darkslategray","darkslategrey","darkturquoise","darkviolet", "lightgray","lightsalmon","lightyellow", "maroon")

            mat <- cbind.data.frame(mat[,coord_gui],mat[,(ncol(mat)-1):ncol(mat)])
            nbprod <- length(levels(mat[,ncol(mat)-1]))
            lab <- mat[1:nbprod,ncol(mat)-1]
            nbjuge <-  length(levels(mat[,ncol(mat)]))-1
            nbpoint=nbprod*(nbjuge+1)

            minx <- min(mat[1:nbpoint,1],na.rm=TRUE)
            maxx <- max(mat[1:nbpoint,1],na.rm=TRUE)
            miny <- min(mat[1:nbpoint,2],na.rm=TRUE)
            maxy <- max(mat[1:nbpoint,2],na.rm=TRUE)

            gg_graph = ggplot() +
                coord_fixed(ratio = 1) +
                xlim(c(minx,maxx)) + ylim(c(miny,maxy)) +
                geom_hline(yintercept = 0,lty=2) + geom_vline(xintercept = 0,lty=2) +
                theme_light()+ theme(axis.title = element_text(hjust = 1, face = 2), plot.title = element_text(hjust = 0.5, face = 2))+
                labs(title = "Individual Variability Around Stimuli", x = paste0("Dim ",coord_gui[1]," (",eig[coord_gui[1],2],"%)"), y= paste0("Dim ",coord_gui[2]," (",eig[coord_gui[2],2],"%)"))+
                geom_point(aes(x=mat[1:nbprod,1],y=mat[1:nbprod,2]),col=color[1:nbprod],pch=15)+
                ggrepel::geom_text_repel(aes(x=mat[1:nbprod,1], y=mat[1:nbprod,2], label=rownames(mat)[1:nbprod]), color = color[1:nbprod])+
                geom_point(aes(x=mat[-(1:nbprod),1],y=mat[-(1:nbprod),2]),col=rep(color[1:nbprod],nrow(mat)/nbprod-1),pch=20)+
                if (name==TRUE) geom_text(aes(x=mat[-(1:nbprod),1],y=mat[-(1:nbprod),2],label=mat[-(1:nbprod),4]),col=rep(color[1:nbprod],nrow(mat)/nbprod-1),size=3)

            print(gg_graph)
            TRUE
        },

        .plotIndaxe = function(image, ...){

            if(is.null(self$options$prod)||is.null(self$options$pane)||is.null(self$options$senso)||(length(self$options$senso)<self$options$nFactors))return()
            graxe=image$state

            #Graphic options
            abs_gui=self$options$abs
            ord_gui=self$options$ord
            coord_gui=c(abs_gui,ord_gui)

            ploti=plot.PCA(graxe,choix="ind",invisible="ind.sup",axes=coord_gui, title ="Representation of the Stimuli")
            print(ploti)
            TRUE
        },
        .plotVaraxe = function(image, ...){

            if(is.null(self$options$prod)||is.null(self$options$pane)||is.null(self$options$senso)||(length(self$options$senso)<self$options$nFactors))return()
            graxe=image$state

            #Graphic options
            abs_gui=self$options$abs
            ord_gui=self$options$ord
            coord_gui=c(abs_gui,ord_gui)

            plotv=plot.PCA(graxe,choix="var",habillage="group",axes=coord_gui, title = "Representation of the Sensory Attributes")
            print(plotv)
            TRUE
        },
        
        .errorCheck = function() {
            if (length(self$options$senso) < self$options$nFactors){
                jmvcore::reject(jmvcore::format('The number of factors is too low'))
            } 
        }
    )
)
