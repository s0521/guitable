guitable_tital_Server<- function(input, output, session) {
  observeEvent(input$ColseButton, {
    stopApp()
  })
  onStop(function() {
    stopApp()
    cat("Session stopped\n")
    })
}

guitable_result_Server <- function(input, output, session, out_dir =NULL) {
  # pixelratio<- reactive({session$clientData$pixelratio})
  # web_plot_width <- reactive({input$web_plot_width})
  # web_plot_height <- reactive({input$web_plot_height})
  # web_plot_scale <- reactive({input$web_plot_scale})
  # output_plot_width <- reactive({input$output_plot_width})
  # output_plot_height <- reactive({input$output_plot_height})
  # output_plot_dpi <- reactive({input$output_plot_dpi})
  units <- reactive({"cm"})
  # out_dir<-tempdir()

  observeEvent(input$ExecuteButton, {
    ggsave("ggplot.svg",
           path=out_dir,
           width = input$output_plot_width,
           height =input$output_plot_height,
           units =units(),
           scale = input$web_plot_scale
    )
    ggsave("ggplot.pdf",
           path=out_dir,
           width = input$output_plot_width,
           height =input$output_plot_height,
           units =units(),
           scale = input$web_plot_scale
           )
    ggsave("ggplot.png",
           path=out_dir,
           dpi=input$output_plot_dpi,
           width = input$output_plot_width,
           height =input$output_plot_height,
           units =units(),
           scale = input$web_plot_scale
           )
  })
}

guitable_table_Server <- function(input, output, session, mapingtable =NULL,datanames=NULL) {
  # browser()
  #ls1<-data
  # if(is.null(mapingtable))return()
  data_names<-c(datanames)
  n_data<-length(data_names)
  # facets_codes<-mapingtable
  a<- reactive({
    # browser()
    for (i in 1:n_data){
      dataname<-c(data_names[[i]])
      mptable<-mapingtable[[i]]
      aa1<-mptable()
      aa2<-dataname
      aa3<-colnames(mptable())
      # dataname<-c(data_names[[i]])
      style<-NULL

      env_mp1<- new.env(parent = emptyenv())
      # browser()

      for (j in 1:length(colnames(mptable()))){
        value_1<-colnames(mptable())[j]
        value_2<-GetMappingValue(mptable(),j)
        # browser()
        assign(value_1,value_2,envir=env_mp1)
      }
      # browser()
      # RowID<-GetMappingValue(mptable(),2)
      # Data<-GetMappingValue(mptable(),3)
      # Denpendency<-GetMappingValue(mptable(),4)
      # Row<-GetMappingValue(mptable(),5)
      # Column<-GetMappingValue(mptable(),6)

      a<-get_table_code(dataname,env_mp1,style)
      return(a)
    }
  })
  output$flextable <- renderUI({
    # outfile <- tempfile(fileext='.png')
   return(htmltools_value(a()))
  })
}


guitable_dt_Server <- function(input, output, session, data_and_name =NULL, field_groups=NULL) {
  #server = FALSE
  # browser()

  #################################
  #################################
  #panel的名字dataname
  dataname<-c(data_and_name[[2]])
  output$tab1 <-renderText(dataname)

  #################################
  #################################
  #################################
  #使用的外部数据集
  # data<-as.data.frame(data1[[1]])
  r_name<-c(colnames(data_and_name[[1]]))

  #################################
  #################################
  #################################
  #实例化一个Mapping_Table对象
  obj_mptbl<-Mapping_Table$new(field_groups=field_groups,variable=r_name,default_field=1)
  obj_mptbl$create_mptbl()

  env_guitable<- new.env(parent = emptyenv())
  env_guitable$dat<-obj_mptbl$mapping_table
  sketch <-obj_mptbl$DT_container
  field_right_bound<-obj_mptbl$field_right_bound

  #################################
  #################DataTable#######
  #################################
  output$dt = renderDT({
    datatable(
      env_guitable$dat,
      rownames = TRUE,width=100 ,
      # editable = list(target = "cell"),
      selection = list(mode = 'single', target = 'cell'),
      callback = JS(callback),
      extensions = c('AutoFill'),
      container =sketch,
      class = 'table-hover',
      options = list(
        autoFill = list(horizontal=FALSE,vertical=TRUE,alwaysAsk=FALSE),
        autoWidth = TRUE,
        columnDefs = list(
          list(width = '20px', targets = 1:ncol(env_guitable$dat)),
          list(className = 'dt-center success', targets = 1:ncol(env_guitable$dat))
        ),
        dom = 't',paging = FALSE, ordering = FALSE
      )
    )%>% formatStyle(field_right_bound, 'border-right' = 'solid')
  })

  #################################
  #################reactive########
  #################################
  # observe({
  #   Data_fill()
  #   Data_select()
  # })

  Data_fill <- reactive({
    # browser()
    info <- input[["dt_cells_filled"]]
    if(!is.null(info)){
      info <- unique(info)
      info$value[info$value==""] <- NA

      obj_mptbl$mptbl_event_fill(info)
      env_guitable$dat <- editData(env_guitable$dat, as.data.frame(obj_mptbl$inf_of_mptbl), proxy = "dt")
    }
    env_guitable$dat
  })

  Data_select <- reactive({
    # browser()
    a<-env_guitable$dat
    info <- input[["dt_cells_selected"]]
    if(is.null(info)||ncol(info)<2){
      # return()
    }else{
      info<-cbind(info,env_guitable$dat[info[1],info[2]])

      obj_mptbl$mptbl_event_select(info)
      env_guitable$dat <- editData(env_guitable$dat, as.data.frame(obj_mptbl$inf_of_mptbl), proxy = "dt")
    }
    env_guitable$dat
  })

  #################################
  #################################
  #################################
  #return
  a<-env_guitable$dat
  return(list(mptable=reactive({
    # browser()
    Data_fill()
    Data_select()
    a<-env_guitable$dat
    return(a)
  })))

  # browser()
  #end
  #################################
}

guitable_layout_updata_server<-function(input, output, session){
  x<-reactive({input$web_plot_height/input$web_plot_width})
  observeEvent(input$web_plot_height,
               {
                 new<-isolate(input$output_plot_width)
                 value <- round(x()*new,2)
                 updateNumericInput(session,
                                    "output_plot_height",
                                    'output plot width(cm)',
                                    value = value,
                                    max<- value,
                                    min<- value
                 )

               })

  observeEvent(input$web_plot_width,
               {
                 new<-isolate(input$output_plot_width)
                 value <- round(x()*new,2)
                 updateNumericInput(session,
                                    "output_plot_height",
                                    'output plot width(cm)',
                                    value = value,
                                    max<- value,
                                    min<- value
                 )
               })

  # observeEvent(input$output_plot_height,
  #              {
  #                new<-isolate(input$output_plot_height)
  #                updateNumericInput(session,
  #                                   "output_plot_width",
  #                                   value = round(new/x(),2)
  #                )
  #              })

  observeEvent(input$output_plot_width,
               {
                 new<-isolate(input$output_plot_width)
                 value <- round(x()*new,2)
                 updateNumericInput(session,
                                    "output_plot_height",
                                    'output plot width(cm)',
                                    value = value,
                                    max<- value,
                                    min<- value
                 )
               })
}

#################################
#################################
#################################
#GetMappingValue for guitable_plot_Server
GetMappingValue<-function(data,column){
  # browser()
  nr<-nrow(data)
  if (is.null(nr))
    return()
  var1<-c()
  for (i in seq_len(nr)) {
    if (data[i,column]==1) {
      var1<-c(var1,rownames(data)[i])
    }
  }
  var1
}

#################################
#################################
#################################
#callback for guitable_dt_Server
callback <- c(
  "var tbl = $(table.table().node());",
  "var id = tbl.closest('.datatables').attr('id');",
  "table.on('preAutoFill', function(e, datatable, cells){",
  "  var out = [];",
  "  for(var i = 0; i < cells.length; ++i){",
  "    var cells_i = cells[i];",
  "    for(var j = 0; j < cells_i.length; ++j){",
  "      var c = cells_i[j];",
  "      var value = c.set === null ? '' : c.set;",
  "      out.push({",
  "        row: c.index.row + 1,",
  "        col: c.index.column ,",
  "        value: value",
  "      });",
  "    }",
  "  }",
  "  Shiny.setInputValue(id + '_cells_filled:DT.cellInfo', out);",
  "  table.rows().invalidate();", # this updates the column type
  "});"
)
