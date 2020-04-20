context("condition()")

test_that("condition() can be created", {
    cond <- condition("hello", class = "world")
    expect_s3_class(cond, c("world", "condition"))
    expect_equal(conditionMessage(cond), "hello")
    expect_true(attr(cond, "capture_call"))
    expect_output(print(cond), "<world: hello>")
})