
# This file is automatically generated, you probably don't want to edit this

SortingOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "SortingOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            actvars = NULL,
            individus = NULL,
            quantisup = NULL,
            qualisup = NULL,
            nFactors = 3,
            abs = 1,
            ord = 2,
            varmodqualisup = TRUE,
            varmodvar = TRUE,
            quantimod = FALSE,
            proba = 5,
            indcoord = FALSE,
            indcontrib = FALSE,
            indcos = FALSE,
            varcoord = FALSE,
            varcontrib = FALSE,
            varcos = FALSE,
            ventil = 5,
            modality = "cos2 10",
            leveltext = 5, ...) {

            super$initialize(
                package="SEDA",
                name="Sorting",
                requiresData=TRUE,
                ...)

            private$..actvars <- jmvcore::OptionVariables$new(
                "actvars",
                actvars,
                suggested=list(
                    "nominal",
                    "ordinal"),
                permitted=list(
                    "factor"))
            private$..individus <- jmvcore::OptionVariable$new(
                "individus",
                individus,
                suggested=list(
                    "nominal"),
                permitted=list(
                    "factor"))
            private$..quantisup <- jmvcore::OptionVariables$new(
                "quantisup",
                quantisup,
                suggested=list(
                    "continuous"),
                permitted=list(
                    "numeric"))
            private$..qualisup <- jmvcore::OptionVariables$new(
                "qualisup",
                qualisup,
                suggested=list(
                    "nominal",
                    "ordinal"),
                permitted=list(
                    "factor"))
            private$..nFactors <- jmvcore::OptionInteger$new(
                "nFactors",
                nFactors,
                default=3)
            private$..abs <- jmvcore::OptionInteger$new(
                "abs",
                abs,
                default=1)
            private$..ord <- jmvcore::OptionInteger$new(
                "ord",
                ord,
                default=2)
            private$..varmodqualisup <- jmvcore::OptionBool$new(
                "varmodqualisup",
                varmodqualisup,
                default=TRUE)
            private$..varmodvar <- jmvcore::OptionBool$new(
                "varmodvar",
                varmodvar,
                default=TRUE)
            private$..quantimod <- jmvcore::OptionBool$new(
                "quantimod",
                quantimod,
                default=FALSE)
            private$..proba <- jmvcore::OptionNumber$new(
                "proba",
                proba,
                default=5)
            private$..indcoord <- jmvcore::OptionBool$new(
                "indcoord",
                indcoord,
                default=FALSE)
            private$..indcontrib <- jmvcore::OptionBool$new(
                "indcontrib",
                indcontrib,
                default=FALSE)
            private$..indcos <- jmvcore::OptionBool$new(
                "indcos",
                indcos,
                default=FALSE)
            private$..varcoord <- jmvcore::OptionBool$new(
                "varcoord",
                varcoord,
                default=FALSE)
            private$..varcontrib <- jmvcore::OptionBool$new(
                "varcontrib",
                varcontrib,
                default=FALSE)
            private$..varcos <- jmvcore::OptionBool$new(
                "varcos",
                varcos,
                default=FALSE)
            private$..ventil <- jmvcore::OptionNumber$new(
                "ventil",
                ventil,
                default=5)
            private$..modality <- jmvcore::OptionString$new(
                "modality",
                modality,
                default="cos2 10")
            private$..leveltext <- jmvcore::OptionNumber$new(
                "leveltext",
                leveltext,
                default=5)

            self$.addOption(private$..actvars)
            self$.addOption(private$..individus)
            self$.addOption(private$..quantisup)
            self$.addOption(private$..qualisup)
            self$.addOption(private$..nFactors)
            self$.addOption(private$..abs)
            self$.addOption(private$..ord)
            self$.addOption(private$..varmodqualisup)
            self$.addOption(private$..varmodvar)
            self$.addOption(private$..quantimod)
            self$.addOption(private$..proba)
            self$.addOption(private$..indcoord)
            self$.addOption(private$..indcontrib)
            self$.addOption(private$..indcos)
            self$.addOption(private$..varcoord)
            self$.addOption(private$..varcontrib)
            self$.addOption(private$..varcos)
            self$.addOption(private$..ventil)
            self$.addOption(private$..modality)
            self$.addOption(private$..leveltext)
        }),
    active = list(
        actvars = function() private$..actvars$value,
        individus = function() private$..individus$value,
        quantisup = function() private$..quantisup$value,
        qualisup = function() private$..qualisup$value,
        nFactors = function() private$..nFactors$value,
        abs = function() private$..abs$value,
        ord = function() private$..ord$value,
        varmodqualisup = function() private$..varmodqualisup$value,
        varmodvar = function() private$..varmodvar$value,
        quantimod = function() private$..quantimod$value,
        proba = function() private$..proba$value,
        indcoord = function() private$..indcoord$value,
        indcontrib = function() private$..indcontrib$value,
        indcos = function() private$..indcos$value,
        varcoord = function() private$..varcoord$value,
        varcontrib = function() private$..varcontrib$value,
        varcos = function() private$..varcos$value,
        ventil = function() private$..ventil$value,
        modality = function() private$..modality$value,
        leveltext = function() private$..leveltext$value),
    private = list(
        ..actvars = NA,
        ..individus = NA,
        ..quantisup = NA,
        ..qualisup = NA,
        ..nFactors = NA,
        ..abs = NA,
        ..ord = NA,
        ..varmodqualisup = NA,
        ..varmodvar = NA,
        ..quantimod = NA,
        ..proba = NA,
        ..indcoord = NA,
        ..indcontrib = NA,
        ..indcos = NA,
        ..varcoord = NA,
        ..varcontrib = NA,
        ..varcos = NA,
        ..ventil = NA,
        ..modality = NA,
        ..leveltext = NA)
)

SortingResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "SortingResults",
    inherit = jmvcore::Group,
    active = list(
        plotindiv = function() private$.items[["plotindiv"]],
        plotvar = function() private$.items[["plotvar"]],
        plotitemvar = function() private$.items[["plotitemvar"]],
        plotquantisup = function() private$.items[["plotquantisup"]],
        eigengroup = function() private$.items[["eigengroup"]],
        dimdesc = function() private$.items[["dimdesc"]],
        individus = function() private$.items[["individus"]],
        variables = function() private$.items[["variables"]],
        dfresgroup = function() private$.items[["dfresgroup"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Analysis of Sorting Data")
            self$add(jmvcore::Image$new(
                options=options,
                name="plotindiv",
                title="Representation of the Stimuli",
                width=700,
                height=500,
                renderFun=".plotindiv"))
            self$add(jmvcore::Image$new(
                options=options,
                name="plotvar",
                title="Representation of the Subjects",
                width=700,
                height=500,
                renderFun=".plotvar"))
            self$add(jmvcore::Image$new(
                options=options,
                name="plotitemvar",
                title="Representation of the Categories",
                width=700,
                height=500,
                renderFun=".plotitemvar"))
            self$add(jmvcore::Image$new(
                options=options,
                name="plotquantisup",
                title="Representation of the Quantitative Supplementary Variables",
                visible="(quantimod)",
                width=700,
                height=500,
                renderFun=".plotquantisup"))
            self$add(R6::R6Class(
                inherit = jmvcore::Group,
                active = list(
                    eigen = function() private$.items[["eigen"]]),
                private = list(),
                public=list(
                    initialize=function(options) {
                        super$initialize(
                            options=options,
                            name="eigengroup",
                            title="Eigenvalue Decomposition")
                        self$add(jmvcore::Table$new(
                            options=options,
                            name="eigen",
                            title="Eigenvalue and (Cumulative) Percentage of Variance",
                            columns=list(
                                list(
                                    `name`="component", 
                                    `title`="", 
                                    `type`="text"),
                                list(
                                    `name`="eigenvalue", 
                                    `title`="Eigenvalue", 
                                    `type`="number"),
                                list(
                                    `name`="purcent", 
                                    `title`="% of variance", 
                                    `type`="number"),
                                list(
                                    `name`="purcentcum", 
                                    `title`="Cumulative %", 
                                    `type`="number"))))}))$new(options=options))
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="dimdesc",
                title="Automatic Description of the Axes"))
            self$add(R6::R6Class(
                inherit = jmvcore::Group,
                active = list(
                    coordonnees = function() private$.items[["coordonnees"]],
                    contribution = function() private$.items[["contribution"]],
                    cosinus = function() private$.items[["cosinus"]]),
                private = list(),
                public=list(
                    initialize=function(options) {
                        super$initialize(
                            options=options,
                            name="individus",
                            title="Individual Tables")
                        self$add(jmvcore::Table$new(
                            options=options,
                            name="coordonnees",
                            title="Coordinates Table",
                            visible="(indcoord)",
                            clearWith=list(
                                "nFactors"),
                            columns=list()))
                        self$add(jmvcore::Table$new(
                            options=options,
                            name="contribution",
                            title="Contributions Table",
                            visible="(indcontrib)",
                            clearWith=list(
                                "nFactors"),
                            columns=list()))
                        self$add(jmvcore::Table$new(
                            options=options,
                            name="cosinus",
                            title="Cosine Table",
                            visible="(indcos)",
                            clearWith=list(
                                "nFactors"),
                            columns=list()))}))$new(options=options))
            self$add(R6::R6Class(
                inherit = jmvcore::Group,
                active = list(
                    coordonnees = function() private$.items[["coordonnees"]],
                    contribution = function() private$.items[["contribution"]],
                    cosinus = function() private$.items[["cosinus"]]),
                private = list(),
                public=list(
                    initialize=function(options) {
                        super$initialize(
                            options=options,
                            name="variables",
                            title="Variable Tables")
                        self$add(jmvcore::Table$new(
                            options=options,
                            name="coordonnees",
                            title="Coordinates Table",
                            visible="(varcoord)",
                            clearWith=list(
                                "actvars",
                                "nFactors"),
                            columns=list()))
                        self$add(jmvcore::Table$new(
                            options=options,
                            name="contribution",
                            title="Contributions Table",
                            visible="(varcontrib)",
                            clearWith=list(
                                "actvars",
                                "nFactors"),
                            columns=list()))
                        self$add(jmvcore::Table$new(
                            options=options,
                            name="cosinus",
                            title="Cosine Table",
                            visible="(varcos)",
                            clearWith=list(
                                "actvars",
                                "nFactors"),
                            columns=list()))}))$new(options=options))
            self$add(R6::R6Class(
                inherit = jmvcore::Group,
                active = list(
                    dfres = function() private$.items[["dfres"]]),
                private = list(),
                public=list(
                    initialize=function(options) {
                        super$initialize(
                            options=options,
                            name="dfresgroup",
                            title="Description of the Stimuli")
                        self$add(jmvcore::Table$new(
                            options=options,
                            name="dfres",
                            title="Description of the Stimuli",
                            columns=list(
                                list(
                                    `name`="component", 
                                    `title`="", 
                                    `type`="text", 
                                    `combineBelow`=TRUE),
                                list(
                                    `name`="word", 
                                    `title`="Word", 
                                    `type`="text"),
                                list(
                                    `name`="internper", 
                                    `title`="Intern %", 
                                    `type`="Number"),
                                list(
                                    `name`="globper", 
                                    `title`="Global %", 
                                    `type`="Number"),
                                list(
                                    `name`="internfreq", 
                                    `title`="Intern frequency", 
                                    `type`="Number"),
                                list(
                                    `name`="globfreq", 
                                    `title`="Global frequency", 
                                    `type`="Number"),
                                list(
                                    `name`="pvaluedfres", 
                                    `title`="p", 
                                    `format`="zto,pvalue"),
                                list(
                                    `name`="vtest", 
                                    `title`="Vtest", 
                                    `type`="Number"))))}))$new(options=options))}))

SortingBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "SortingBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "SEDA",
                name = "Sorting",
                version = c(1,0,0),
                options = options,
                results = SortingResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE)
        }))

#' Analysis of Sorting Data
#'
#' 
#' @param data .
#' @param actvars .
#' @param individus .
#' @param quantisup .
#' @param qualisup .
#' @param nFactors .
#' @param abs .
#' @param ord .
#' @param varmodqualisup .
#' @param varmodvar .
#' @param quantimod .
#' @param proba .
#' @param indcoord .
#' @param indcontrib .
#' @param indcos .
#' @param varcoord .
#' @param varcontrib .
#' @param varcos .
#' @param ventil .
#' @param modality .
#' @param leveltext .
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$plotindiv} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$plotvar} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$plotitemvar} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$plotquantisup} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$eigengroup$eigen} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$dimdesc} \tab \tab \tab \tab \tab a preformatted \cr
#'   \code{results$individus$coordonnees} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$individus$contribution} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$individus$cosinus} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$variables$coordonnees} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$variables$contribution} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$variables$cosinus} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$dfresgroup$dfres} \tab \tab \tab \tab \tab a table \cr
#' }
#'
#' @export
Sorting <- function(
    data,
    actvars,
    individus,
    quantisup,
    qualisup,
    nFactors = 3,
    abs = 1,
    ord = 2,
    varmodqualisup = TRUE,
    varmodvar = TRUE,
    quantimod = FALSE,
    proba = 5,
    indcoord = FALSE,
    indcontrib = FALSE,
    indcos = FALSE,
    varcoord = FALSE,
    varcontrib = FALSE,
    varcos = FALSE,
    ventil = 5,
    modality = "cos2 10",
    leveltext = 5) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("Sorting requires jmvcore to be installed (restart may be required)")

    if ( ! missing(actvars)) actvars <- jmvcore::resolveQuo(jmvcore::enquo(actvars))
    if ( ! missing(individus)) individus <- jmvcore::resolveQuo(jmvcore::enquo(individus))
    if ( ! missing(quantisup)) quantisup <- jmvcore::resolveQuo(jmvcore::enquo(quantisup))
    if ( ! missing(qualisup)) qualisup <- jmvcore::resolveQuo(jmvcore::enquo(qualisup))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(actvars), actvars, NULL),
            `if`( ! missing(individus), individus, NULL),
            `if`( ! missing(quantisup), quantisup, NULL),
            `if`( ! missing(qualisup), qualisup, NULL))

    for (v in actvars) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])
    for (v in individus) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])
    for (v in qualisup) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])

    options <- SortingOptions$new(
        actvars = actvars,
        individus = individus,
        quantisup = quantisup,
        qualisup = qualisup,
        nFactors = nFactors,
        abs = abs,
        ord = ord,
        varmodqualisup = varmodqualisup,
        varmodvar = varmodvar,
        quantimod = quantimod,
        proba = proba,
        indcoord = indcoord,
        indcontrib = indcontrib,
        indcos = indcos,
        varcoord = varcoord,
        varcontrib = varcontrib,
        varcos = varcos,
        ventil = ventil,
        modality = modality,
        leveltext = leveltext)

    analysis <- SortingClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

