

#input object declaration
UpsettiObj <- R6Class("Upsetti",
                       public = list(
                         JobName = NULL,
                         groupUnique = 2, 
                         groupTotal = 2,
                         groupNames = list(),
                         PathToFile = NULL,
                         InputType = "Default",
                         InputDF = data.frame(),
                         
                         initialize = function(JobName, PathToFile, InputType = NULL) {
                           
                           self$JobName = JobName
                           self$PathToFile = PathToFile 
                           if(!is.null(InputType)) {self$InputType <- InputType}
                         },
                         
                         ReadFile = function(SheetName = NULL) {
                           if (!is.null(SheetName) & str_ends(paste(self$PathToFile), ".xlsm|.xlsx")) {
                             self$InputDF = read_excel(path = paste(self$PathToFile), sheet = paste(SheetName), col_names = T) 
                             #self$DFList[[DFListIndex]] = self$InputDF
                             #return(self$InputDF)
                           } else if (str_ends(paste(self$PathToFile), ".csv")) {
                             self$InputDF = read_csv(paste(self$PathToFile), col_names = T)
                             #self$DFList[[DFListIndex]] = self$InputDF
                             #return(self$InputDF)
                           } else print("invalid input type")
                           
                           BuffDF = self$InputDF %>%
                             column_to_rownames(var = paste(names(self$InputDF[1]))) %>% 
                             t() %>% as.data.frame
                           
                           self$groupUnique = length(as.list(unique(BuffDF[1, ])))
                           self$groupTotal = length(as.list(BuffDF[1, ]))
                           self$groupNames = as.list(unique(BuffDF[1, ]))
                         }
                       )
)


Input_checker = function(enrichment_df) {
  
  Check_list = list(UniqueCols =  T, UniqueRows = T, Only_1_0 = T, all_num = T)
  
  print("garugamesh1")
  
  if (length(unique(names(enrichment_df))) != ncol(enrichment_df)) {
    Check_list["UniqueCols"] = F
  }
  if (length(unique(unlist(enrichment_df[, 1]))) != nrow(enrichment_df)) {
    Check_list["UniqueRows"] = F
  }
  if (any(enrichment_df[2:ncol(enrichment_df)] > 1)) {
    Check_list["Only_1_0"] = F
  }
  if (!any(lapply(enrichment_df[2:ncol(enrichment_df)], function(x) {print(x); is.numeric(x)}))) {
    Check_list["all_num"] = F
  }
  return(Check_list)
}


Obtain_Elements_List = function(enrichment_df) {

  Selected_Elements <- enrichment_df %>% 
    arrange(desc(AcrossAll)) %>%  t() %>% as.data.frame() %>% rownames_to_column(); 
  
  rownames(Selected_Elements) = Selected_Elements$rowname; 
  names(Selected_Elements) = Selected_Elements %>% slice(1)
  Selected_Elements = Selected_Elements %>% slice(-c(1:2)) %>% dplyr::select(-1)
  #view(Selected_Elements)
  
  VectorList = list()
  for (i in 1:ncol(Selected_Elements)) {
    VecBuff = Selected_Elements[[i]]
    VectorList[[i]] = VecBuff
  }
  
  names(VectorList) = names(Selected_Elements)
  UniqCols = unique(VectorList)
  
  ColNameCount = list()
  for (k in 1:length(UniqCols)) {
    ColNameCount[[k]] = "BBUFFER1"
    for (j in 1:length(VectorList)) {
      
      if (identical(UniqCols[[k]], VectorList[[j]])) {
        #print("identical detected")
        ColNameCount[[k]] = str_c(ColNameCount[[k]], names(VectorList)[[j]], sep = " Buffer&Buffer ")
        #print(ColNameCount[[k]]); print(k)
      }
    }
    ColNameCount[[k]] = str_remove(string = ColNameCount[[k]], pattern = "BBUFFER1")
    ColNameCount[[k]] = str_split(string = ColNameCount[[k]], pattern = " Buffer&Buffer ")
  }
  
  ColNameCount = lapply(ColNameCount, FUN = function(x){unlist(x)}) #removing the third list 
  ColNameCount <- lapply(ColNameCount, FUN = function(x) { #removing the first "" element
    x[-1]
  })
  
  return(ColNameCount)
}

GetQueries_Elements = function(Elements, Element_Identity, Show_Number_Bars) {
  
  if (Show_Number_Bars != 0) {
    
    Elements = Elements[order(sapply(Elements, length))[1:Show_Number_Bars]] #gets top n members with the least elements in their list
    
  } else {Elements = Elements[order(sapply(Elements, length))]}
  
  print("length(Elements)"); print(length(Elements))
  
  num_colors <- 130
  ColorsList_130 <- rgb(
    red = runif(num_colors, min = 0, max = 0.8),
    green = runif(num_colors, min = 0, max = 0.8),
    blue = runif(num_colors, min = 0, max = 0.8)
  )
  #subset of these colors to ColorsList_Comps
  ColorsList <- sample(ColorsList_130, length(Elements), replace = FALSE)
  #ColorsList = rgb(runif(length(Elements)), runif(length(Elements)), runif(length(Elements))) #just random colors for now
  QueryList = list()
  
  print('BRRRRRRRRRRRRRR')
  for (i in 1:length(Elements)) {
    #print('queryLoopCheck 1')
    BuffQuery = list(
      query = elements,
      params = list(paste(Element_Identity), c(paste(Elements[[i]]))),
      color = ColorsList[[i]],
      alpha = 0.1,
      active = T,
      query.name = paste("Barchart Section ", i)
    )
    #print(BuffQuery[[6]])
    QueryList[[i]] = BuffQuery
  }
  
  return(list(QueryList,
              ColorsList[1:length(Elements)],
              Elements))
}

PlotUpsetti = function (enrichment_df, FilterCutoff = 1, Show_Number_Bars = 0) {
  
  Element_Identity = names(enrichment_df)[[1]]
  
  enrichment_df[2:ncol(enrichment_df)] = lapply(
    enrichment_df[2:ncol(enrichment_df)],
    function(col_x){
    #if any value is over or equal to the Filter_Cutoff, it gets converted to "1", otherwise "0"
    as.numeric(col_x >= FilterCutoff)
  })
  
  enrichment_df = enrichment_df %>% mutate(
    AcrossAll = rowSums(.[, names(.)[2:ncol(.)]])
  ) %>% dplyr::select(paste(Element_Identity), AcrossAll, everything())
  
  enrichment_df = enrichment_df %>% filter(AcrossAll >0)
  
  Elements_List = Obtain_Elements_List(enrichment_df)
  
  print("length(Elements_List)") ;print(length(Elements_List))
  
  enrichment_df = data.frame(enrichment_df)
  #view(enrichment_df); print(class(enrichment_df));
  
  if (Show_Number_Bars > length(Elements_List) || Show_Number_Bars < 0) { #smaller than 0 too 
    return(list("Warning_ShowNum_2Big", "Warning_ShowNum_2Big", "Warning_ShowNum_2Big"))
  } else {
    Queries_and_Colors = GetQueries_Elements(Elements_List, Element_Identity, Show_Number_Bars)
    Upsetti_bar_colors = Queries_and_Colors[[2]]
    sorted_Elements_List = Queries_and_Colors[[3]]
  }
  
  if (Show_Number_Bars > 0) {
    
    Upsetti_plot = upset(enrichment_df,
                         nintersects = NA,
                         order.by = "freq",
                         sets = names(enrichment_df[3:ncol(enrichment_df)]),
                         decreasing = T,
                         main.bar.color = "#00000040",
                         matrix.color = "#00F00440",
                         # These will still be ignored because of the active query
                         shade.color = "#8827fA40",
                         mainbar.y.label = "UpsettiTestetti",
                         shade.alpha = 0.9, 
                         matrix.dot.alpha = 0.9,
                         text.scale = 1.2,
                         point.size = 3.5,
                         line.size = 1.2,
                         query.legend = "bottom",
                         queries = Queries_and_Colors[[1]]
                         )
  } else {
    Upsetti_plot = upset(enrichment_df,
                         nintersects = NA,
                         order.by = "freq",
                         sets = names(enrichment_df[3:ncol(enrichment_df)]),
                         decreasing = T,
                         main.bar.color = "#00000040",
                         matrix.color = "#00F00440",
                         # These will still be ignored because of the active query
                         shade.color = "#8827fA40",
                         mainbar.y.label = "UpsettiTestetti",
                         shade.alpha = 0.9, 
                         matrix.dot.alpha = 0.9,
                         text.scale = 1.2,
                         point.size = 3.5,
                         line.size = 1.2)
    }
  
  return(list(Upsetti_plot,
              Upsetti_bar_colors,
              sorted_Elements_List))
}


