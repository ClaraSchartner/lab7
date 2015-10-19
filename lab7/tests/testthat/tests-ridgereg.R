context("ridgereg")

ans <- ridgereg1(y = iris$Petal.Length, x = iris$Petal.Width)
    
test_that("class",{
    expect_is(ans$coefficients,"numeric")
    expect_is(ans, class="ridgereg")
})

test_that("error",{
    expect_error(ridgereg1(iris$Petal.Width, "0.1"))
    expect_error(ridgereg1(iris$Petal.Length))
    expect_error(ridgereg1(Petal.Length, Petal.Width))
})

test_that("blackboc",{
    expect_less_than(ans$coefficients - 2.873756, 0.00001)
})