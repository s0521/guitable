#' @import tidyr dplyr
# generate_table(data,row_col_formula,value)
generate_table<-function (data,row=NULL,col=NULL,value){
  res<-get_data(data)
  res$guitable_data
  # browser()
  if(!is.null(col)&&!is.null(row)){
    row_table<-set_row_table(res$guitable_data,row)
    col_table<-set_col_table(res$guitable_data,col)
    value_table<-get_value_table1(data,row_table,col_table,value)

    col_name<-set_colname_of_table(col_table,value)
    colnames(value_table)<-col_name

    row_value_table<-cbind(row_table,value_table)
  }else if(is.null(row)&&is.null(col)){
    # row_table<-set_row_table(res$guitable_data,row)
    # col_table<-set_col_table(res$guitable_data,col)
    value_table<-get_value_table4(data,NA,NA,value)

    col_name<-set_colname_of_table(,value)
    colnames(value_table)<-col_name

    row_value_table<-value_table
  }else if(is.null(col)){
    row_table<-set_row_table(res$guitable_data,row)
    # col_table<-set_col_table(res$guitable_data,col)
    value_table<-get_value_table2(data,row_table,NA,value)

    col_name<-set_colname_of_table(,value)
    colnames(value_table)<-col_name

    row_value_table<-cbind(row_table,value_table)
  }else if(is.null(row)){
    # row_table<-set_row_table(res$guitable_data,row)
    col_table<-set_col_table(res$guitable_data,col)
    value_table<-get_value_table3(data,NA,col_table,value)

    col_name<-set_colname_of_table(col_table,value)
    colnames(value_table)<-col_name

    row_value_table<-value_table
  }
  # browser()
  row_value_table<-unnest(unnest(row_value_table,cols=colnames(value_table)),cols=colnames(value_table))
  return(row_value_table)
}
set_row_table<-function (data,row){
  row_table<-select(data,row[1:length(row)])
  row_table<-unique(row_table)
  return(row_table)
}
set_col_table<-function (data,col){
  col_table<-select(data,col[1:length(col)])
  col_table<-unique(col_table)
  return(col_table)
}

get_value_table1<-function(data,row_table,col_table,value){
  val_tot<-length(value)
  row_tot<-nrow(row_table)
  col_tot<-nrow(col_table)*val_tot
  # browser()
  value_table <-matrix(rep(NA,row_tot*col_tot),nrow = row_tot, ncol = col_tot, byrow=TRUE)
  # value_table<- as.data.frame(value_table)
  for (c_i in 1:col_tot){
    for (r_i in 1:row_tot){
      col_i<-(c_i-1)%/%val_tot+1
      val_i<-(c_i-1)%%val_tot+1
      rt<-row_table[r_i,,drop = FALSE]
      ct<-col_table[col_i,,drop = FALSE]
      vt<-value[val_i]
      table_s_row<-merge(row_table[r_i,,drop = FALSE],data,all.x=TRUE)
      table_s_row_col<-merge(col_table[col_i,,drop = FALSE],table_s_row,all.x=TRUE)
      # browser()
      table_s_row_col_value<-table_s_row_col[,value[val_i]]
      value_table[r_i,c_i]<-c(table_s_row_col_value)
    }
  }
  value_table
}

get_value_table2<-function(data,row_table,col_table=NA,value){
  val_tot<-length(value)
  row_tot<-nrow(row_table)
  col_tot<-val_tot
  # browser()
  value_table <-matrix(rep(NA,row_tot*col_tot),nrow = row_tot, ncol = col_tot, byrow=TRUE)
  value_table<- as.data.frame(value_table)
  for (c_i in 1:col_tot){
    for (r_i in 1:row_tot){
      col_i<-(c_i-1)%/%val_tot+1
      val_i<-(c_i-1)%%val_tot+1
      rt<-row_table[r_i,,drop = FALSE]
      # ct<-col_table[col_i,,drop = FALSE]
      vt<-value[val_i]
      table_s_row<-merge(row_table[r_i,,drop = FALSE],data,all.x=TRUE)
      # table_s_row_col<-merge(col_table[col_i,,drop = FALSE],table_s_row,all.x=TRUE)
      table_s_row_col_value<-table_s_row[,value[val_i]]
      value_table[[r_i,c_i]]<-list(table_s_row_col_value)
    }
  }
  # browser()
  # value_table<-unnest(unnest(value_table,cols=colnames(value_table)),cols=colnames(value_table))
  value_table
}
get_value_table3<-function(data,row_table=NA,col_table,value){
  val_tot<-length(value)
  # row_tot<-nrow(row_table)
  row_tot<-1
  col_tot<-nrow(col_table)*val_tot
  # browser()
  value_table <-matrix(rep(NA,row_tot*col_tot),nrow = row_tot, ncol = col_tot, byrow=TRUE)
  value_table<- as.data.frame(value_table)
  for (c_i in 1:col_tot){
    for (r_i in 1:row_tot){
      col_i<-(c_i-1)%/%val_tot+1
      val_i<-(c_i-1)%%val_tot+1
      # rt<-row_table[r_i,,drop = FALSE]
      ct<-col_table[col_i,,drop = FALSE]
      vt<-value[val_i]
      # table_s_row<-merge(row_table[r_i,,drop = FALSE],data,all.x=TRUE)
      table_s_row_col<-merge(col_table[col_i,,drop = FALSE],data,all.x=TRUE)
      # browser()
      table_s_row_col_value<-table_s_row_col[,value[val_i]]
      value_table[[r_i,c_i]]<-list(table_s_row_col_value)
    }
  }
  # browser()
  # t1<-unnest(value_table,cols=colnames(value_table))
  # t2<-unnest(t1,cols=colnames(value_table),keep_empty=T)
  # value_table<-t2
  value_table
}
get_value_table4<-function(data,row_table=NA,col_table=NA,value){
  val_tot<-length(value)
  # row_tot<-nrow(row_table)
  row_tot<-1
  col_tot<-val_tot
  # browser()
  value_table <-matrix(rep(NA,row_tot*col_tot),nrow = row_tot, ncol = col_tot, byrow=TRUE)
  value_table<- as.data.frame(value_table)
  for (c_i in 1:col_tot){
    for (r_i in 1:row_tot){
      # col_i<-(c_i-1)%/%val_tot+1
      # val_i<-(c_i-1)%%val_tot+1
      # rt<-row_table[r_i,,drop = FALSE]
      # ct<-col_table[col_i,,drop = FALSE]
      # vt<-value[val_i]
      # table_s_row<-merge(row_table[r_i,,drop = FALSE],data,all.x=TRUE)
      # table_s_row_col<-merge(col_table[col_i,,drop = FALSE],data,all.x=TRUE)
      # browser()
      table_s_row_col_value<-data[,value[c_i]]
      value_table[[r_i,c_i]]<-list(table_s_row_col_value)
    }
  }
  value_table
}
set_colname_of_table<-function(col_table=NULL,value){
  # browser()
  nr<-ifelse(is.null(col_table),1L,nrow(col_table))
  val<-matrix(
    rep(list(value),nr),
    nrow = nr,
    ncol = 1,
    byrow=TRUE
  )
  val<-as.data.frame(val)
  colnames(val)<-"value"
  ifelse(is.null(col_table),col_val<-val,col_val<-cbind(col_table,val))
  unnest_col_val<-unnest(col_val,"value")
  col_name<-apply(unnest_col_val, 1, paste, collapse="_")

  return(col_name)
}
