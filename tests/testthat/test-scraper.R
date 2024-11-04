test_that("page range calculation works", {
  ranges <- calculate_page_ranges(100, 4)
  
  expect_equal(length(ranges), 4)
  expect_equal(ranges[[1]]$start_page, 1)
  expect_equal(ranges[[1]]$end_page, 25)
  expect_equal(ranges[[4]]$start_page, 76)
  expect_equal(ranges[[4]]$end_page, 100)
})

test_that("simulated page processing works", {
  results <- simulate_page_processing(1)
  
  expect_true(is.data.frame(results))
  expect_true(nrow(results) >= 1 && nrow(results) <= 5)
  expect_true(all(c("expediente", "descripcion", "fecha", "monto") %in% names(results)))
})

test_that("batch saving works", {
  # Create temp directory
  temp_dir <- tempdir()
  
  # Create test data
  test_data <- tibble(
    expediente = "TEST-1",
    descripcion = "Test licitacion",
    fecha = Sys.time(),
    monto = 1000
  )
  
  # Save batch
  save_batch(test_data, 1, 1, temp_dir)
  
  # Check file exists
  filename <- file.path(temp_dir, "batch_1_1.json")
  expect_true(file.exists(filename))
  
  # Check content
  content <- fromJSON(filename)
  expect_equal(content$expediente, "TEST-1")
})