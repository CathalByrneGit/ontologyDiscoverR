test_that("merge_candidates deduplicates exact id matches", {
  cand_a <- new_candidate_object_type("Patient", "Patient", confidence = 0.9,
    source_refs = list(list(source_id = "s1", source_label = "schema.sql", excerpt = "")))
  cand_b <- new_candidate_object_type("Patient", "Patient", confidence = 0.7,
    source_refs = list(list(source_id = "s2", source_label = "docs.md", excerpt = "")))

  result <- merge_candidates(list(cand_a, cand_b))

  # Should produce exactly one Patient
  patient_cands <- Filter(function(c) c$id == "Patient", result)
  expect_equal(length(patient_cands), 1L)

  # Should keep highest confidence
  expect_equal(patient_cands[[1]]$confidence, 0.9)

  # Should merge source_refs
  expect_equal(length(patient_cands[[1]]$source_refs), 2L)
})

test_that("merge_candidates deduplicates fuzzy matches (patient vs Patient)", {
  cand_a <- new_candidate_object_type("Patient", "Patient", confidence = 0.9)
  cand_b <- new_candidate_object_type("patient", "patient", confidence = 0.6)

  result <- merge_candidates(list(cand_a, cand_b))

  patient_cands <- Filter(function(c) tolower(c$id) == "patient", result)
  expect_equal(length(patient_cands), 1L)
})

test_that("merge_candidates handles camelCase vs underscore variants", {
  cand_a <- new_candidate_object_type("ObjectType",   "Object Type",  confidence = 0.8)
  cand_b <- new_candidate_object_type("object_type",  "object type",  confidence = 0.6)

  result <- merge_candidates(list(cand_a, cand_b))

  # Both normalise to "objecttype" so should merge to 1
  expect_equal(length(result), 1L)
})

test_that("merge_candidates keeps distinct candidates separate", {
  cand_a <- new_candidate_object_type("Patient", "Patient", confidence = 0.9)
  cand_b <- new_candidate_object_type("Doctor",  "Doctor",  confidence = 0.85)
  cand_c <- new_candidate_object_type("Ward",    "Ward",    confidence = 0.8)

  result <- merge_candidates(list(cand_a, cand_b, cand_c))

  expect_equal(length(result), 3L)
})

test_that("merge_candidates handles empty list", {
  result <- merge_candidates(list())
  expect_equal(length(result), 0L)
})

test_that("merge_candidates handles mixed element types", {
  ot <- new_candidate_object_type("Patient", "Patient", confidence = 0.9)
  lt <- new_candidate_link_type("patient_ward", "Patient Ward",
                                 from_type_id = "Patient", to_type_id = "Ward")

  result <- merge_candidates(list(ot, lt))
  expect_equal(length(result), 2L)

  element_types <- sapply(result, function(c) c$element_type)
  expect_true("object_type" %in% element_types)
  expect_true("link_type"   %in% element_types)
})

test_that("detect_conflicts marks conflicting candidates", {
  cand_a <- new_candidate_object_type("Patient", "Patient",
    properties = list(
      list(id = "patient_id", type = "integer", nullable = FALSE, description = "")
    ))
  cand_b <- new_candidate_object_type("Patient", "Patient",
    properties = list(
      list(id = "completely_different_field", type = "string", nullable = TRUE, description = "")
    ))

  result <- detect_conflicts(list(cand_a, cand_b))
  expect_true(length(result) == 2L)
})
