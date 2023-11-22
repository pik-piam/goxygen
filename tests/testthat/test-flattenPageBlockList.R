test_that("flattening evaluates cfg attributes correctly", {

  l <- list(
    title = list(
      content = "foo",
      cfg = NULL
    ),
    description = list(
      content = "bar",
      cfg = list(
        extrapage = "page1",
        size = "1"
      )
    )
  )

  expect_warning(flattenPageBlockList(l),
                 "The following settings are not supported and will be ignored: size")
  f <- suppressWarnings(flattenPageBlockList(l))
  expect_true(all(c("blocks", "extraPageBlocks") %in% names(f)))
  expect_true(f$blocks$title == "foo")
  expect_true(names(f$extraPageBlocks) == "page1")
  expect_true(f$extraPageBlocks$page1$description == "bar")
})

test_that("flattening works for realizations sublist", {

  l <- list(
    title = list(
      content = "foo",
      cfg = NULL
    ),
    description = list(
      content = "bar",
      cfg = NULL
    ),
    realizations = list(
      firstRealization = list(
        description = list(
          content = "baz",
          cfg = NULL
        ),
        description = list(
          content = "bam",
          cfg = NULL
        )
      ),
      secondRealization = list(
        description = list(
          content = "bom",
          cfg = NULL
        ),
        description = list(
          content = "bim",
          cfg = list(
            extrapage = "page1"
          )
        ),
        description = list(
          content = "bem",
          cfg = list(
            extrapage = "page1"
          )
        )
      )
    )
  )
  f <- flattenPageBlockList(l)

  expect_true(all(c("blocks", "extraPageBlocks") %in% names(f)))
  expect_true(f$blocks$title == "foo")
  expect_true(f$blocks$description == "bar")

  expect_true(all(c("firstRealization", "secondRealization") %in% names(f$blocks$realizations)))

  expect_true(all(c("baz", "bam") %in% f$blocks$realizations$firstRealization$description))
  expect_true(all(c("bom") %in% f$blocks$realizations$secondRealization$description))

  expect_true(length(f$extraPageBlocks$page1) == 2)
  expect_true(f$extraPageBlocks$page1[1] == "bim")
  expect_true(f$extraPageBlocks$page1[2] == "bem")
})
