context("ridgereg")

y <- ridgereg(eruptions~waiting, data=faithful)
    
test_that("coefficients",{
    expect_less_than(y$coefficients[1] - 0, 0.001)
    expect_less_than(y$coefficients[2] - 0.0756278, 0.001)
})

test_that("class",{
    expect_is(y$coefficients,"numeric")
})

