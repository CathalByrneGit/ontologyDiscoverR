#' Merge and deduplicate a list of candidates
#'
#' @param candidate_list A list of Candidate objects (any mix of types)
#' @return A deduplicated list of Candidate objects
#' @export
merge_candidates <- function(candidate_list) {
  if (length(candidate_list) == 0) return(list())

  # Separate by element type
  by_type <- split(candidate_list, sapply(candidate_list, function(c) c$element_type))

  merged <- lapply(by_type, function(group) {
    .merge_group(group)
  })

  unlist(merged, recursive = FALSE)
}

.merge_group <- function(candidates) {
  if (length(candidates) <= 1L) return(candidates)

  # Step 1: Exact id match → auto-merge
  ids <- sapply(candidates, function(c) tolower(c$id))
  duplicated_ids <- ids[duplicated(ids)]

  if (length(duplicated_ids) == 0) {
    return(.fuzzy_merge(candidates))
  }

  result <- list()
  seen_ids <- character(0)

  for (i in seq_along(candidates)) {
    cand <- candidates[[i]]
    norm_id <- tolower(cand$id)

    if (norm_id %in% seen_ids) next

    # Find all candidates with the same normalised id
    same_id <- candidates[ids == norm_id]

    if (length(same_id) == 1L) {
      result <- c(result, list(cand))
    } else {
      # Keep highest-confidence version, merge source_refs
      best <- same_id[[which.max(sapply(same_id, function(c) c$confidence))]]
      all_refs <- unlist(lapply(same_id, function(c) c$source_refs), recursive = FALSE)
      best$source_refs <- all_refs
      result <- c(result, list(best))
    }
    seen_ids <- c(seen_ids, norm_id)
  }

  .fuzzy_merge(result)
}

.fuzzy_merge <- function(candidates) {
  if (length(candidates) <= 1L) return(candidates)

  # Normalise ids for fuzzy matching: remove underscores, lowercase
  norm <- function(id) tolower(gsub("[_\\-\\s]", "", id))

  normed_ids <- sapply(candidates, function(c) norm(c$id))
  result     <- list()
  merged_set <- logical(length(candidates))

  for (i in seq_along(candidates)) {
    if (merged_set[i]) next

    # Find fuzzy matches (same normalised id)
    same <- which(normed_ids == normed_ids[i])

    if (length(same) == 1L) {
      result <- c(result, list(candidates[[i]]))
    } else {
      # Auto-merge fuzzy duplicates
      group    <- candidates[same]
      best     <- group[[which.max(sapply(group, function(c) c$confidence))]]
      all_refs <- unlist(lapply(group, function(c) c$source_refs), recursive = FALSE)
      best$source_refs <- .dedup_source_refs(all_refs)
      result   <- c(result, list(best))
      merged_set[same] <- TRUE
    }
    merged_set[i] <- TRUE
  }

  result
}

.dedup_source_refs <- function(refs) {
  seen_ids <- character(0)
  result   <- list()
  for (ref in refs) {
    key <- paste(ref$source_id, ref$source_label)
    if (!key %in% seen_ids) {
      result   <- c(result, list(ref))
      seen_ids <- c(seen_ids, key)
    }
  }
  result
}

#' Detect conflicts between candidates
#'
#' @param candidate_list List of Candidate objects
#' @return The same list with $conflicts field populated where relevant
#' @export
detect_conflicts <- function(candidate_list) {
  if (length(candidate_list) == 0) return(candidate_list)

  by_type <- split(candidate_list, sapply(candidate_list, function(c) c$element_type))

  result <- lapply(by_type, function(group) {
    if (length(group) <= 1L) return(group)

    # Look for same id but different properties/types
    for (i in seq_along(group)) {
      for (j in seq_along(group)) {
        if (i >= j) next
        a <- group[[i]]
        b <- group[[j]]

        if (tolower(a$id) == tolower(b$id)) {
          # Check for conflicting properties
          if (!.candidates_compatible(a, b)) {
            group[[i]]$conflicts <- unique(c(group[[i]]$conflicts, list(b$candidate_id)))
            group[[j]]$conflicts <- unique(c(group[[j]]$conflicts, list(a$candidate_id)))
          }
        }
      }
    }
    group
  })

  unlist(result, recursive = FALSE)
}

.candidates_compatible <- function(a, b) {
  if (!identical(a$element_type, b$element_type)) return(FALSE)
  if (a$element_type == "link_type") {
    return(a$from_type_id == b$from_type_id && a$to_type_id == b$to_type_id)
  }
  if (a$element_type == "object_type") {
    # If both have properties, check that core property names overlap reasonably
    a_props <- sapply(a$properties %||% list(), function(p) p$id)
    b_props <- sapply(b$properties %||% list(), function(p) p$id)
    if (length(a_props) > 0 && length(b_props) > 0) {
      overlap <- length(intersect(a_props, b_props))
      return(overlap / max(length(a_props), length(b_props)) >= 0.3)
    }
  }
  TRUE
}
