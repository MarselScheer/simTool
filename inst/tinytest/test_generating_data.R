#-# Datagen-instance can store data-generating function
f <- function() return(42)
dg <- Data_generator$new(generator_fun = f)
expect_equal(dg$get_generator(), f)
#-# Datagen-instance can execute stored function
expect_equal(dg$generator(), 42)
