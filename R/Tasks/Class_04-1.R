is_leap = function(x){
  return(x %% 400 == 0 || 
           ((x %% 4 == 0) && (x %% 100 != 0)))
}

y = as.integer(readline('Enter'))
cat(ifelse(is_leap(y), 'високостный', 'обычный'))
