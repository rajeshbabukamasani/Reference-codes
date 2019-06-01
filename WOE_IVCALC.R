CrossTab_TargetVariable <- function(inp_table,factor_vars,Target_variable,outdir)
  
{
  Table_name_string <- "ddd"
  
  inp_table <- inp_table[, c(Target_variable,factor_vars), with = FALSE]
  
  inp_table <- inp_table[, temp_TarVar := ifelse(is.na(get(Target_variable)),0,get(Target_variable))]
  
  
  
  for(i in 1: length(factor_vars))
    
  {
    
    
    
    tt       <- inp_table[,list(freq = .N), by=c("temp_TarVar", factor_vars[i])]
    
    tt       <- tt[,CountofRecs := sum(freq), by = c(factor_vars[i])]
    
    tt       <- tt[,TotCountofTargetVar := sum(freq), by = c("temp_TarVar")] 
    
    
    
    tt_melt  <- melt(tt, id = c("freq","CountofRecs","TotCountofTargetVar","temp_TarVar"))
    
    tt_melt  <- data.table(tt_melt)
    
    tt_dcast  <- dcast.data.table(tt_melt, CountofRecs + variable + value ~ temp_TarVar  , value.var = c("freq","TotCountofTargetVar"))
    
    
    
    out_table <- data.table(tt_dcast)
    
    out_table <- out_table[,Table_Name := "rajesh"]
    
    out_table <- out_table[,TargetVarName := Target_variable]
    
    
    
    setnames(out_table,c("freq_1","freq_2"),paste0("TargetVar_",c("freq_1","freq_2")))
    
    
    
    out_table <- out_table[, TotCountofRecs := sum(CountofRecs), by = c("TargetVarName")]
    
    out_table <- out_table[, Perc_Records := round(100*CountofRecs/TotCountofRecs,1)]
    
    
    
    out_table <- out_table[, Perc_1 := round(100*TargetVar_freq_1/TotCountofTargetVar_1,4)]
    
    out_table <- out_table[, Perc_2 := round(100*TargetVar_freq_2/TotCountofTargetVar_2,4)] 
    
    
    
    start_cols1 <- c("TargetVarName","Table_Name","variable","value",
                     
                     "CountofRecs", "TotCountofRecs","Perc_Records",
                     
                     "TargetVar_freq_1","TotCountofTargetVar_1","Perc_1",
                     
                     "TargetVar_freq_2","TotCountofTargetVar_2","Perc_2")
    
    
    
    # setcolorder(out_table,c(start_cols1,setdiff(names(out_table),start_cols1)))
    
    
    
    out_table <- out_table[,WOE := round(log(Perc_1/Perc_2),5)]
    
    out_table <- out_table[,IV := round((Perc_1 - Perc_2)*WOE/100,5) ]
    
    # out_table <- out_table[, Net_IV := round(sum(IV),5), by = c("TargetVarName") ]
    
    out_table <- out_table[, Net_IV := round(sum(IV,na.rm = TRUE),5), by = c("TargetVarName") ]
    
    
    
    outfile_name <- paste0("EDA_",factor_vars[i])
    
    assign(outfile_name, data.table(out_table))
    
    
    
    setwd(outdir)
    
    do.call(save,
            
            list(outfile_name, 
                 
                 file = paste0(outdir,outfile_name,".Rda")))
    
    
    
    rm(tt,tt_melt,tt_dcast,out_table)
    
    gc()
    
    
    
  }
  
}