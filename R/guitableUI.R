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
setup_tabPanel_panel2<-function(id="guiplot") {
  ns <- NS(id)
  tabPanel(
    'test01',
    fluidPage(
      style='float:left',
      'test02'
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
			column(8,offset = 2,uiOutput(ns('flextable'),width = "auto", height = "auto"))
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
  # object_options_ui("guitable"),

  #JS customer
  tags$script(HTML(
    '
    $("ul:gt(1) a").css("padding","1px");
    $("ul:gt(1) .navbar-brand").css("padding","1px");
    $("ul:gt(1) .navbar-brand").css("height","25px");
    '
  ))
)
