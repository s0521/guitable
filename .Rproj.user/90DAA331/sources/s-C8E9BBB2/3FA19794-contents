#Header
tital_ui<-function(id="guitable"){
  ns <- NS(id)
  tagList(
    fluidRow(style='
              background-color:#48D1CC;
             ',
              "guitable:User-friendly R programming language ploting tools"
             ),
    fixedRow(style='
              background-color:#48D1CC;
              border-style:solid;
              border-width:1px;
              border-color:Black;
              position: fixed;
              top: 0px;
              z-index:9;
              width:100%;
             ',
             column(12,"guitable:User-friendly R programming language ploting tools"))
  )
}

toolbar_ui<-function(id="guitable"){
  ns <- NS(id)
  tagList(
    fluidRow('!--toolbar_ui',
             style='background-color:#48D1CC;
             height:35px
             ',
             ),
    fluidRow(style='background-color:#48D1CC;
               border-style:solid;
               border-width:1px;
               border-color:Black;
               position: fixed;
               top: 20px;
               z-index:10;
               width:100%;
             ',
            'toolbar_ui'

            ,actionButton(
              ns("ColseButton"),
              "02.Finish and Close",
              style='
                color:Black;
                background-color:Darkorange;
                font-weight:bold;
                border-color:Black;
                float:right;
              '
            )

            ,actionButton(
              ns("ExecuteButton"),
              "01.Execute!",
              style='
                color:Black;
                background-color:LimeGreen;
                font-weight:bold;
                border-color:Black;
                float:right;
              '
            )

    )
  )
}


setup_tabPanel_panel<-function(id="guitable") {
  ns <- NS(id)
  tabPanel(
    textOutput(ns('tab1')),
    fluidPage(
      style='float:left',
      DTOutput(ns('dt'))
    )
  )
}

results_ui<-function(id="guitable") {
  tabPanel("Results Panel",
    navlistPanel(
      well = TRUE,
      fluid = TRUE,
      widths = c(3, 9),
      "data",
      tabPanel(
        "data"
      ),
      "plot",
      tabPanel(
        "plot"
      ),
      "text",
      tabPanel(
        "text"
      ),
      "other",
      tabPanel(
        "other"
      )
    )
  )
}

table_ui<-function(id="guitable"){
	ns <- NS(id)
	tagList(
		fluidRow(
			column(8,offset = 2,plotOutput(ns('table'),width = "auto", height = "auto"))
		)
	)
}

#Object Options
object_options_ui<-function(id="guitable") {
  ns <- NS(id)
  navlistPanel(
    fluid = TRUE,
    widths = c(3, 9),
    "Plot",
      tabPanel(
        "themes",
        tagList(
          fluidRow(
            column(3,
                   radioButtons(ns("themes"), "themes",
                                c("theme_gray" = "theme_gray",
                                  "theme_bw" = "theme_bw",
                                  "theme_linedraw" = "theme_linedraw",
                                  "theme_light" = "theme_light",
                                  "theme_dark" = "theme_dark",
                                  "theme_minimal" = "theme_minimal",
                                  "theme_classic" = "theme_classic",
                                  "theme_void" = "theme_void",
                                  "theme_minimal" = "theme_minimal")
                                )
            )
          )
        )
      ),
      tabPanel(
        "Layout",
        tagList(
          fluidRow(
            column(3,
                   "Preview Plot Set(pixels)",
                   numericInput(ns('web_plot_height'),'web plot height(pixels)',250),
                   numericInput(ns('web_plot_width'),'web plot width(pixels)',500),
                   numericInput(ns('web_plot_scale'),'web plot scale',2,min = 0.1, max = 100, step = 0.1)
            ),
            column(3,
                   "Output Plot Set(cm)",
                   numericInput(ns('output_plot_height'),label='output plot height(cm)',4,max=4,min=4),
                   numericInput(ns('output_plot_width'),'output plot width(cm)',8),
                   numericInput(ns('output_plot_dpi'),'output plot DPI',300)
            ),
            column(3,
                   numericInput(ns('Outer_Margin'),'Outer Margin',0)
            )
          )
        )
      ),
      tabPanel(
        "Lattice"
      ),
    "Axes",
      tabPanel(
        "X",
        tagList(
          fluidRow(
            column(3,
              radioButtons(ns("X_Scale"), "Scale",
                          c("Linear" = "identity",
                            "log10" = "log10",
                            "log2" = "log2",
                            "logit" = "logit",
                            "probability" = "probability",
                            "sqrt" = "sqrt"))
            ),
            column(3,
                   radioButtons(ns("X_Range"), "Range",
                                c("Auto-scale Uniform" = "none",
                                  "Custom" = "Custom")),
                   numericInput(ns('X_Minimum'),'Minimum',0),
                   numericInput(ns('X_Maximum'),'Maximum',100)
            ),
            column(3,
                   numericInput(ns('X_expand_p'),'expand_plot',0.05),
                   numericInput(ns('X_expand_u'),'expand_unit',0)
            ),
          )
        )
      ),
      tabPanel(
        "Y",
        tagList(
          fluidRow(
            column(3,
                   radioButtons(ns("Y_Scale"), "Scale",
                                c("Linear" = "identity",
                                  "log10" = "log10",
                                  "log2" = "log2",
                                  "logit" = "logit",
                                  "probability" = "probability",
                                  "sqrt" = "sqrt"))
            ),
            column(3,
                   radioButtons(ns("Y_Range"), "Range",
                                c("Auto-scale Uniform" = "none",
                                  "Custom" = "Custom")),
                   numericInput(ns('Y_Minimum'),'Minimum',0),
                   numericInput(ns('Y_Maximum'),'Maximum',100)
            ),
            column(3,
                   numericInput(ns('Y_expand_p'),'expand_plot',0.05),
                   numericInput(ns('Y_expand_u'),'expand_unit',0)
            )
          )
        )
      ),
      tabPanel(
        "Y2"
      ),
    tabPanel(
      "Reference Lines "
    )
  )
}

setup_tabPanel_panel<-function(id="guitable") {
  ns <- NS(id)
  tabPanel(
    textOutput(ns('tab1')),
    fluidPage(
      style='float:left',
      DTOutput(ns('dt'))
    )
  )
}


guitableUI <- fluidPage(
  #Header
  tital_ui("guitable"),
  toolbar_ui("guitable"),
  #geom_type_ui("guitable"),

  ####################################
  #Data and Plot
  tabsetPanel(
      tabPanel("Setup Panel",
               uiOutput("ui"),
               table_ui("guitable")
      ),


	  # setup_ui("guitable"),
	  results_ui("guitable")
  ),
  # plot_ui("guitable"),
  ####################################


  #Object Options
  object_options_ui("guitable"),

  #JS customer
  tags$script(HTML(
    '
    $("ul:gt(1) a").css("padding","1px");
    $("ul:gt(1) .navbar-brand").css("padding","1px");
    $("ul:gt(1) .navbar-brand").css("height","25px");
    '
  ))
)
