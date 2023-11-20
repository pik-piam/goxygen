test_that("appending works for a new page", {
  l1 <- list(page1 = list(title = "page1", description = "bar"), page2 = list(title = "page2"))
  l2 <- list(page3 = list(title = "page3"))
  out <- appendExtraPageBlocks(l1, l2)
  expect_true(length(out) == 3)
  expect_true("page1" %in% names(out))
  expect_true("page2" %in% names(out))
  expect_true("page3" %in% names(out))
})


test_that("appending blocks for an exisiting page works", {
  l1 <- list(page1 = list(title = "page1", description = "bar"), page2 = list(title = "page2"))
  l2 <- list(page2 = list(description = "baz"))
  out <- appendExtraPageBlocks(l1, l2)
  expect_true(length(out) == 2)
  expect_true("page1" %in% names(out))
  expect_true("page2" %in% names(out))
  expect_true(out[["page1"]][["description"]] == "bar")
  expect_true(out[["page2"]][["description"]] == "baz")
})
