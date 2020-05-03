get_table_code<- function(dataname = NULL, mptable_env = NULL,style=NULL) {
  aa_1<-dataname
  # browser()
  if (is.null(dataname)) {

  }else{

    dcast_data_code<-get_dcast_data_code2(dataname = dataname, mptable_env=mptable_env)
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
  formula<-paste(sep=" ~ ",row_txt,col_txt)

  fun.aggregate<-"fun.aggregate = list"
  # value.var<-paste(sep="","value.var = c(",c(mptable_env$Data),")")
  value.var<-paste(sep="","value.var = '",mptable_env$Data,"'")
  dcast_data_code_body<-paste(sep=",",
        data_name,
        formula,
        fun.aggregate,
        value.var
        )
  cat(file=stderr(), "\n dcast_data_code_body is ",dcast_data_code_body)
  dcast_data_code<-paste(sep="",
                              "dcast(",
                              dcast_data_code_body,
                              ")"
  )
  cat(file=stderr(), "\n dcast_data_code is ",dcast_data_code)
  return(dcast_data_code)
}

get_dcast_data_code2<- function(dataname = NULL, mptable_env = NULL){
  data_name<-dataname

  row_var<-c(mptable_env$Row,mptable_env$RowID)
  col_var<-c(mptable_env$Column)
  val_var<-c(mptable_env$Data)

  row_txt<-var2txt(row_var)
  col_txt<-var2txt(col_var)
  val_txt<-var2txt(val_var)

  # fun.aggregate<-"fun.aggregate = list"
  dcast_data_code_body<-paste(sep=",",
                              data_name,
                              row_txt,
                              col_txt,
                              val_txt
  )
  cat(file=stderr(), "\n dcast_data_code_body is ",dcast_data_code_body)
  dcast_data_code<-paste(sep="",
                         "generate_table(",
                         dcast_data_code_body,
                         ")"
  )
  cat(file=stderr(), "\n dcast_data_code is ",dcast_data_code)
  return(dcast_data_code)
}
var2txt<-function(var){
  if(is.null(var))return()
  txt_var<-paste(sep ="","'",var,"'")
  txt_vars<-paste(collapse = ",",txt_var )
  txt_vars<-paste(sep ="","c(",txt_vars,")")
  return(txt_vars)
}
