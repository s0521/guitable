#R is a very anti-human language. The package aims to improve this dilemma and create a user-friendly drawing GUI for R.
#plot engine ggplot2
#shiny runGadget callModule browserViewer plotOutput textOutput actionButton NS fluidPage navlistPanel tabPanel tagList fluidRow column
#'
#' @title guitable
#' @param ... Matrix or data frame
#' @param out_dir The storage path of the output picture, recommend 'out_dir=getwb()'
#' @export
#' @return png and pdf of plot
#' @import shiny flextable reshape2
#' @importFrom DT datatable DTOutput renderDT JS editData formatStyle
#' @importFrom rlang parse_expr expr
#' @importFrom stats na.omit
#' @importFrom magrittr %>%
#' @importFrom methods new
#' @examples
#' \donttest{
#' guitable()
#' }
#' \donttest{
#' guitable(PK)
#' }
#' \donttest{
#' guitable(PK,out_dir=getwb())
#' }
guitable <- function(..., out_dir = NULL) {
  ########################################################
  #Static data
# browser()
  c1name <- c('None','RowID','Data','Denpendency','Row','Column')
  c2group <- c(rep("1",6))
  c3display <-c(rep("Table Data",4),rep("Stratification By",2))
  c_name <- matrix (nrow=3,ncol=length(c1name),byrow = T )
  c_name[1,] <- c1name
  c_name[2,] <- c2group
  c_name[3,] <- c3display

  field_groups<-c_name

  #########################################################
  #Obtaining the Data and Parameters
  get_data_arry<-function(...){
    # browser()
    if(...length()<1){
      lsname<-ls(envir =.GlobalEnv)
    }else{
      lsname<-as.character(eval(substitute(alist(...))))
    }

    arry_data<-NULL
    # browser()
    for (i in 1:length(lsname)){
      ind_data <- get_data(lsname[i], name = lsname[i])
      if(is.matrix(ind_data$guitable_data)||is.data.frame(ind_data$guitable_data)){
        arry_data<-rbind(arry_data,ind_data)
      }
    }
    return(arry_data)
  }

  res_data<-get_data_arry(...)

  if(is.null(out_dir)) out_dir<-tempdir()
  # res_data <- get_data(data, name = deparse(substitute(data)))
  #Obtaining and Specifying the Data and Parameters Needed
  ########################################################


  guitableServer = function(input, output, session) {
    # Panl_Height<-reactive({input$Panle_Height})
    # Panl_Width<-reactive({input$Panle_Width})
    # browser()
    callModule(
      module = guitable_tital_Server,
      id = "guitable"
    )

    callModule(
      module = guitable_result_Server,
      id = "guitable",
      out_dir=out_dir
    )

    ##############################
    #setup_tabPanel_panel
    output$ui<-renderUI({
      id = "guitable"
      ns <- NS(id)
      eval(text_panels())
    })

    text_panels<-reactive({
      # browser()
      a<-NULL
      b<-NULL
      for (j in 1:nrow(res_data)){
        Parname<-paste(sep="","data_panae_",j)
        a[j]<-paste(sep="" ,"setup_tabPanel_panel(","'",Parname,"'",")")
      }
      a<-paste(a,collapse =",")
      b<-paste(sep="" ,"navlistPanel(widths = c(3, 9),",a,")")
      parse_expr(b)
    })

    observe(a<-mptable())
    mptable<-reactive({
      # browser()
      mp_table<-list()
      for (j in 1:nrow(res_data)){
        Parname<-paste(sep="","data_panae_",j)
        mp_item<-NULL

        mp_item<- callModule(
          module = guitable_dt_Server,
          id = Parname,
          data_and_name = res_data[j,],
          field_groups = field_groups
        )
          mp_table[j]<-mp_item
      }
      mp_table
    })
    #
    ##############################

    callModule(
      module = guitable_table_Server,
      id = "guitable",
      mapingtable = mptable(),
      dataname=res_data[,2]
    )


    callModule(
      module = guitable_layout_updata_server,
      id = "guitable"
    )

  }

  runGadget(
    #browser(),
    guitableUI, guitableServer, viewer = browserViewer()
  )
}
