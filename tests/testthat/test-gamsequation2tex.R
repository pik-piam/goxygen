library(testthat)
library(goxygen)

context("gamsequation2tex")

test_that("Equation is properly converted", {
  equation <- "eq_1 .. v_a =e= sum(j,v_b(j)*((1-s_c)+sum(cell(i,j),v_d(i)/(f_d(i)+f_e(i)))));"
  result <- structure("\\begin{multline*}\n v\\_a = \\sum_{j}\\left(v\\_b(j) \\cdot \\left(\\left(1-s\\_c\\right)+\\sum_{cell(i,j)}\\left(\\frac{v\\_d(i)}{\\left(f\\_d(i)+f\\_e(i)\\right)}\\right)\\right)\\right) \n\\end{multline*}", .Names = "eq_1")
  expect_identical(gamsequation2tex(equation),result)
})

test_that("Unsupported code triggers corresponding warnings", {
  expect_warning(gamsequation2tex("a"), "Cannot handle equations without relational operator!")
  expect_warning(gamsequation2tex("$if true eq1 .. a =e= b;"),"Cannot handle equations with preceeding dollar conditions!")
})

test_that("!! comments are removed and equation properly converted", {
  equation <- 'eq_1(a) .. v_a(a) =e=
      sum(a2b(a,b),     !! a comment
          v_b(a,b))
      + sum(a2c(a,b),      !! another comment
        v_c(a,b) * v_d(a,b));'
  result <- c(`eq_1(a)` = "\\begin{multline*}\n v\\_a(a) = \n \\sum_{a2b(a,b)}\\left(\n v\\_b(a,b)\\right)\n + \\sum_{a2c(a,b)}\\left(\n v\\_c(a,b) \\cdot v\\_d(a,b)\\right) \n\\end{multline*}")
  expect_identical(gamsequation2tex(equation),result)
})