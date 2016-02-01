dupli :: [a] -> [a]
dupli list = concat [[x,x] | x <- list]
