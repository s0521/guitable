get_table_code<- function(dataname = NULL, mptable_env = NULL,style=NULL) {
  aa_1<-dataname
  # browser()
  if (is.null(dataname)) {

  }else{

    dcast_data_code<-get_dcast_data_code(dataname = dataname, mptable_env=mptable_env)
    # browser()
    table_value<-eval(parse_expr(as.character(dcast_data_code)))
    flextable(table_value)
  }
}

get_dcast_data_code<- function(dataname = NULL, mptable_env = NULL){
  # dcast(data, formula, fun.aggregate = NULL, ..., margins = NULL,
  #       subset = NULL, fill = NULL, drop = TRUE,
  #       value.var = guess_value(data))
  data_name<-dataname
  # browser()
  row_var<-c(mptable_env$Row,mptable_env$RowID)
  col_var<-c(mptable_env$Column)
  row_txt<-paste(collapse = " + ",row_var)
  col_txt<-paste(collapse = " + ",col_var)
  formula<-paste(sep="~",row_txt,col_txt)

  fun.aggregate<-"list"
  value.var<-mptable_env$Data

  dcast_data_code<-paste(sep="",
        "dcast(",
        data_name,", ",
        formula,",",
        "fun.aggregate =",fun.aggregate,",",
        "value.var =","'",value.var,"'","",
        ")"
        )
  return(dcast_data_code)
}
