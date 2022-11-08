require(optMTP)

## -----------------------------------------------------------
##
##          FUNCTIONS
##
## -----------------------------------------------------------
glb_label <- c("BCVA q12 Wk48", "BCVA q16 Wk48",
               "DRSS q12 Wk48", "DRSS q16 Wk48",
               "BCVA q12 Wk60", "BCVA q16 Wk60")

get_rst <- function(p_values) {

    p_values <- rbind(p_values)

    ## ---------------------G-SAP--------------------------------
    g_weights <- c(b12 = 0.5, b16 = 0.5, d12 = 0, d16 = 0)
    g_matrix  <- rbind(
        c(0,    0.75, 0.25,    0),
        c(0.75, 0,    0.25,    0),
        c(0,    0.75,    0, 0.25),
        c(0,    1,       0,    0)
    )

    g_rst  <- om_rejection(g_weights,  g_matrix,
                           p_values[, 1:4, drop = FALSE],
                           alpha = 0.05)

    g_text <- c(rep("Reject", 4), "NA", "NA")
    g_text[which(0 == g_rst[3:6])] <- "FAILED"

    ## ---------------------G-SAP--------------------------------
    ep_weights <- c(b12 = 0.5, b16 = 0.5, d12 = 0, d16 = 0,
                    b12_60 = 0, b16_60 = 0)
    ep_matrix  <- rbind(
        c(    0,     0,     0,  0,  1, 0),
        c(    0,     0,     0,  0,  0, 1),
        c(    0,     0,     0,  1,  0, 0),
        c(    0,     0,     0,  0,  0, 0),
        c(    0, 0.999, 0.001,  0,  0, 0),
        c(0.999,     0, 0.001,  0,  0, 0)
    )

    ep_rst <- om_rejection(ep_weights, ep_matrix,
                           p_values, alpha = 0.05)

    ep_text <- rep("Reject", 6)
    ep_text[which(0 == ep_rst[3:8])] <- "FAILED"


    rst           <- cbind(glb_label, g_text, ep_text)
    colnames(rst) <- c("Endpoint", "G-SAP", "EP-SAP")

    rst
}


##-------------------------------------------------------------
##           UI FUNCTIONS
##-------------------------------------------------------------
tab_design <- function() {
    tabPanel("Conduct Test",
             fluidPage(
                 column(12,
                        wellPanel(h4("Multiplicity Control Graph"),
                                  tags$div(img(src = "MCP_Graph.png",
                                               width="100%")))
                        )
             ),
             fluidPage(
                 column(4,
                        wellPanel(h4("Enter Two-Sided  p-Values"),
                                  lapply(1:6, function(i) {
                                      numericInput(paste("inP", i, sep = ""),
                                                   glb_label[i],
                                                   value = c(0.02, 0.04, 0.05, 0.05, 0.03, 0.01)[i],
                                                   step = 0.005,
                                                   min = 0, max = 1)})
                                  ),
                        ),
                 column(8,
                        wellPanel(h4("Rejection Results"),
                                  actionButton("btnTest", "Conduct Test"),
                                  tableOutput("outRst")
                                  ))
             )
             )
}

##define the main tabset for beans
tab_main <- function() {
    tabsetPanel(type = "pills",
                id   = "mainpanel",
                tab_design()
                )
}

get_cur_rst <- reactive({
    userLog$data
})
