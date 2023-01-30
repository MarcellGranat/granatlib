environment_diff <- function() {
  
  if (!exists(".global_variables")) {
    .global_variables <<- list()
  } else {
    .global_variables <<- last(.global_variables)
  }
  
  .global_variables <<- ls() |> 
    map(get) |> 
    set_names(ls()) |> 
    list() |> 
    append(x = .global_variables)
  
  obj_names <- names(last(.global_variables))
  
  status <- map_chr(obj_names, \(x) {
    prev = .global_variables[[1]][[x]]
    new = .global_variables[[2]][[x]]
    
    case_when(
      is.null(prev) & !is.null(new) ~ "new",
      !is.null(prev) & is.null(new) ~ "deleted",
      !identical(prev, new) ~ "changed",
      TRUE ~ "unchanged"
    )
  }) |> 
    set_names(obj_names)
  
  deleted_names <- status |> 
    keep(\(x) x == "deleted") |> 
    names()
  
  if (length(deleted_names) > 0) {
    message(crayon::magenta("DELETED\n"))
    
    walk(deleted_names, \(x) {
      message(crayon::magenta(x), "\n")
      
      capture.output(print(.global_variables[[2]][[x]])) |> 
        head(20) |> 
        str_flatten("\n") |> 
        (\(msg) message(crayon::red(msg))) ()
    })
  }
  
  new_names <- status |> 
    keep(\(x) x == "new") |> 
    names()
  
  if (length(new_names) > 0) {
    message(crayon::magenta("NEW\n"))
    
    walk(new_names, \(x) {
      message(crayon::magenta(x), "\n")
      
      capture.output(print(.global_variables[[2]][[x]])) |> 
        head(20) |> 
        str_flatten("\n") |> 
        (\(msg) message(crayon::green(msg))) ()
    })
  }
  
  changed_names <-  status |> 
    keep(\(x) x == "changed") |> 
    names()
  
  if (length(changed_names) > 0) {
    message(crayon::magenta("CHANGED\n"))
    
    walk(changed_names, \(x) {
      message(crayon::magenta(x), "\n")
      
      capture.output(print(.global_variables[[1]][[x]])) |> 
        head(20) |> 
        str_flatten("\n") |> 
        (\(msg) message(crayon::red(msg))) ()
      
      capture.output(print(.global_variables[[2]][[x]])) |> 
        head(20) |> 
        str_flatten("\n") |> 
        (\(msg) message(crayon::cyan(msg))) ()
    })
  }
  
  .global_variables <<- last(.global_variables)
  
}
