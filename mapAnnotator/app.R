library(shiny)
library(shinyjs)
library(V8)
library(EBImage)
library(RColorBrewer)
library(markdown)

# d20 image credit: http://turtlegirlman.deviantart.com/art/d20-icon-de-la-Gartoon-60651092

jsCode <- '
shinyjs.addText = function (params) {
    var defaultParams = {
        elementID: "regionDescrip",
        text: ""
    };
    params = shinyjs.getParams(params, defaultParams);
    document.getElementById(params.elementID).value = params.text;
}
'

ui <- tagList(
    useShinyjs(),
    extendShinyjs(text = jsCode),
    navbarPage(
        title = div(img(src = "d20.png", width = "25", height = "25"), "Map annotator"),
        windowTitle = "Map annotator",
        tabPanel("Create/edit",
            fluidRow(
                column(width = 7,
                  textInput("pathToMapImage", label = "Enter path to map image or saved annotations (.rda file):", value = "~/Documents/dnd/campaigns/curse_of_strahd/images/maps/lowres/bluewaterinn_player_gridded.jpg", width = "100%")
                ),
                column(width = 1,
                    HTML("<label for=\"loadImage\">&nbsp</label>"),
                    actionButton("loadImage", label = "Use image")
                )
            ),
            fluidRow(
                column(width = 8,
                    uiOutput("mapUI")
                ),
                column(width = 4,
                    actionButton("startRegion", label = "Record region"),
                    actionButton("stopRegion", label = "Store region"),
                    actionButton("updateRegion", label = "Update region"),
                    textOutput("storeRegionSuccessMessage"),
                    strong("Enter markdown-formatted annotation:"),
                    tags$textarea(id = "regionDescrip", rows = 10, cols = 50, "The tattered walls of this room are lined with rusted armor..."),
                    textInput("pathToSaveFile", label = "Enter filename to save annotations:", value = "~/Documents/dnd/campaigns/curse_of_strahd/mapannots/bluewaterinn.rda", width = "100%"),
                    actionButton("saveOutput", label = "Save output"),
                    textOutput("saveOutputSuccessMessage"),
                    verbatimTextOutput("regionInfo")
                )
            )
        ), # end tab: create
        tabPanel("Load",
            fluidRow(
                column(width = 7,
                  textInput("pathToAnnotsFile", label = "Enter path to saved annotations file:", value = "~/Documents/dnd/campaigns/curse_of_strahd/mapannots/bluewaterinn.rda", width = "100%")
                ),
                column(width = 1,
                    HTML("<label for=\"loadAnnots\">&nbsp</label>"),
                    actionButton("loadAnnots", label = "Load annotations")
                )
            ),
            fluidRow(
                column(width = 8,
                    uiOutput("mapUIannotated")
                ),
                column(width = 4,
                    htmlOutput("descriptions")
                )
            )
        )
    )
)

server <- function(input, output, session) {
    ########## Tab: create/edit annotations ##########
    rv <- reactiveValues(
        pathToMapImage = "",
        regionList = list(),
        subRegionList = list(),
        regionDescripList = list(),
        map = matrix(),
        regionIndexSelected = NA
    )
    

    # imagePath <- eventReactive(input$loadImage, {
    #     input$pathToMapImage
    # })
    observeEvent(input$loadImage, {
        filePath <- input$pathToMapImage
        fileExt <- tail(strsplit(filePath, "\\.")[[1]], 1)
        isRDA <- length(grep("[Rr][Dd][Aa][Tt]{0,1}[Aa]{0,1}", fileExt))==1
        if (isRDA) {
            annotsEdit <- new.env()
            load(filePath, envir = annotsEdit)
            mapPath <- get("mapPath", envir = annotsEdit)
            regionList <- get("regionList", envir = annotsEdit)
            regionDescripList <- get("regionDescripList", envir = annotsEdit)
            rv$pathToMapImage <- mapPath
            rv$regionList <- regionList
            rv$regionDescripList <- regionDescripList
        } else {
            rv$pathToMapImage <- filePath
        }
        map <- readImage(rv$pathToMapImage)
        mapDims <- dim(map)
        if (mapDims[1] > 800) {
            map <- resize(map, w = 800)
        }
        rv$map <- map
    })

    observeEvent(input$mapBrush, {
        coords <- c(input$mapBrush$xmin, input$mapBrush$xmax, input$mapBrush$ymin, input$mapBrush$ymax)
        rv$subRegionList[[length(rv$subRegionList)+1]] <- coords
    })

    observeEvent(input$startRegion, {
        rv$subRegionList <- list()
        output$storeRegionSuccessMessage <- renderText({"Region drawing started!"})
        output$saveOutputSuccessMessage <- renderText({""})
    })

    observeEvent(input$stopRegion, {
        rv$regionList[[length(rv$regionList)+1]] <- rv$subRegionList
        rv$subRegionList <- list()
        rv$regionDescripList[[length(rv$regionDescripList)+1]] <- input$regionDescrip
        output$storeRegionSuccessMessage <- renderText({"Region stored!"})
    })

    observeEvent(input$map_dblclick, {
        info <- getDescripClicked(input$map_dblclick, rv$regionList, rv$regionDescripList, returnIndex = TRUE)
        descripString <- info$descrip
        rv$regionIndexSelected <- info$index
        # input$regionDescrip <- paste0(input$regionDescrip, descripString)
        js$addText(elementID = "regionDescrip", text = descripString)
    })

    observeEvent(input$updateRegion, {
        rv$regionDescripList[[rv$regionIndexSelected]] <- input$regionDescrip
        output$storeRegionSuccessMessage <- renderText({"Region updated!"})
    })

    plotHeight <- reactive({
        # map <- readImage(imagePath())
        # mapDims <- dim(rv$map)
        # if (mapDims[1] > 800) {
        #     ratio <- mapDims[1]/800
        #     newDims <- mapDims/ratio
        # }
        # return(newDims[2])
        dim(rv$map)[2]
    })

    colors <- brewer.pal(12, "Set3")
    drawRects <- reactive({
        coordMatList <- lapply(rv$regionList, function(l) {
            do.call(rbind, l)
        })
        for (i in seq_along(rv$regionList)) {
            col <- colors[((i-1) %% 12) + 1]
            rect(coordMatList[[i]][,1], coordMatList[[i]][,3], coordMatList[[i]][,2], coordMatList[[i]][,4], col = col, border = col)
        }
    })

    output$mapPlot <- renderPlot({
        # map <- readImage(imagePath())
        # mapDims <- dim(map)
        # if (mapDims[1] > 800) {
        #     map <- resize(map, w = 800)
        # }
        display(rv$map, method = "raster")
        drawRects()
    })

    output$mapUI <- renderUI({
        height <- plotHeight()
        plotOutput("mapPlot", width = 800, height = height, 
            brush = brushOpts(
                id = "mapBrush",
                direction = "xy",
                delay = 800
            ),
            dblclick = "map_dblclick"
        )
    })

    output$regionInfo <- renderPrint({
        cat("Region index selected: ", rv$regionIndexSelected, "\n")
        cat(length(rv$regionList), "regions:", "\n")
        for (i in seq_along(rv$regionList)) {
            cat("  ", i, ". ", length(rv$regionList[[i]]), " rect(s). \"", substring(rv$regionDescripList[[i]], 1, 25), "\"\n", sep = "")
        }
    })

    observeEvent(input$saveOutput, {
        mapPath <- rv$pathToMapImage
        regionList <- rv$regionList
        regionDescripList <- rv$regionDescripList
        save(mapPath, regionList, regionDescripList, file = input$pathToSaveFile)
        output$saveOutputSuccessMessage <- renderText({"Annotations saved!"})
    })

    ########## Tab: load annotations ##########
    loadedData <- new.env()
    observeEvent(input$loadAnnots, {
        load(input$pathToAnnotsFile, envir = loadedData)
        mapPath <- get("mapPath", envir = loadedData)
        regionList <- get("regionList", envir = loadedData)
        regionDescripList <- get("regionDescripList", envir = loadedData)

        map <- readImage(mapPath)
        mapDims <- dim(map)
        if (mapDims[1] > 800) {
            ratio <- mapDims[1]/800
            newDims <- mapDims/ratio
        }
        output$mapUIannotated <- renderUI({
            plotOutput("mapPlotAnnotated", width = 800, height = newDims[2],
                click = "map_click"
            )
        })
        output$mapPlotAnnotated <- renderPlot({
            if (mapDims[1] > 800) {
                map <- resize(map, w = 800)
            }
            display(map, method = "raster")
            colors <- brewer.pal(12, "Set3")
            coordMatList <- lapply(regionList, function(l) {
                do.call(rbind, l)
            })
            for (i in seq_along(regionList)) {
                col <- colors[((i-1) %% 12) + 1]
                rect(coordMatList[[i]][,1], coordMatList[[i]][,3], coordMatList[[i]][,2], coordMatList[[i]][,4], col = col, border = col)
            }
        })
    })

    getDescripClicked <- function(click, regionList, regionDescripList, returnIndex = FALSE) {
        if (is.null(click)) {
            return("No annotated region selected!")
        }
        x <- click$x
        y <- click$y
        bool <- sapply(seq_along(regionList), function(i) {
            coordMat <- do.call(rbind, regionList[[i]])
            any(x >= coordMat[,1] & x <= coordMat[,2] & y >= coordMat[,3] & y <= coordMat[,4])
        })
        index <- which(bool)
        if (returnIndex) {
            return(list(index = index, descrip = regionDescripList[[index]]))
        } else {
            return(regionDescripList[[index]])
        }
    }

    observeEvent(input$map_click, {
        output$descriptions <- renderUI({
            descripString <- getDescripClicked(input$map_click, get("regionList", envir = loadedData), get("regionDescripList", envir = loadedData))
            descripString <- markdownToHTML(text = descripString, fragment.only = TRUE)
            HTML(descripString)
        })
    })
}

shinyApp(ui = ui, server = server)