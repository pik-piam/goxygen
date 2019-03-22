library(testthat)

context("gamsequation2tex")

test_that("Equation is properly converted", {
  equation <- "eq_1 .. v_a =e= sum(j,v_b(j)*((1-s_c)+sum(cell(i,j),v_d(i)/f_d(i))));"
  result <- structure("\\begin{multline*}\n v\\_a = \\sum_{j}\\left(v\\_b(j) \\cdot \\left(\\left(1-s\\_c\\right)+\\sum_{cell(i,j)}\\left(\\frac{v\\_d(i)}{f\\_d(i)}\\right)\\right)\\right) \n\\end{multline*}", .Names = "eq_1")
  expect_identical(gamsequation2tex(equation),result)
})
