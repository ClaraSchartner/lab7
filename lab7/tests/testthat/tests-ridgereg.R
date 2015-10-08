context("ridgereg")

#not finished yet
library(MASS)
data("faithful")
x <- lm.ridge(eruptions~waiting, data=faithful)
y <- ridgereg(eruptions~waiting, data=faithful)
    
testthat("equal",{
    expect_equal(x,y)
})

