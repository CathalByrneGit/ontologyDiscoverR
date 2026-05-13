#' Launch the interactive Shiny review interface
#'
#' Opens a Shiny app for reviewing, approving, rejecting, and editing candidates.
#' Returns the updated session when the app is closed.
#'
#' @param session A DiscoverySession object
#' @return Updated DiscoverySession with candidate statuses set by the reviewer
#' @export
dis_review <- function(session) {
  stopifnot(inherits(session, "DiscoverySession"))

  for (pkg in c("shiny", "bslib", "DT")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      rlang::abort(sprintf("Package '%s' is required for dis_review(). Install it with install.packages('%s').", pkg, pkg))
    }
  }

  session_env <- new.env(parent = emptyenv())
  session_env$session <- session

  ui <- bslib::page_navbar(
    title    = paste0("ontologyDiscoverR: Review — ", session$label),
    theme    = bslib::bs_theme(version = 5),

    bslib::nav_panel(
      "Object Types",
      shiny::fluidRow(
        shiny::column(12,
          shiny::div(
            style = "margin-bottom: 10px;",
            shiny::actionButton("approve_all_high_ot", "Approve all >= 0.8 confidence",
                                class = "btn-success btn-sm"),
            shiny::actionButton("reject_all_low_ot",  "Reject all < 0.3 confidence",
                                class = "btn-danger btn-sm ms-2")
          ),
          DT::DTOutput("table_object_types")
        )
      ),
      shiny::hr(),
      shiny::uiOutput("detail_object_type")
    ),

    bslib::nav_panel(
      "Link Types",
      DT::DTOutput("table_link_types"),
      shiny::hr(),
      shiny::uiOutput("detail_link_type")
    ),

    bslib::nav_panel(
      "Concept Hints",
      DT::DTOutput("table_concept_hints"),
      shiny::hr(),
      shiny::uiOutput("detail_concept_hint")
    ),

    bslib::nav_panel(
      "Conflicts",
      shiny::uiOutput("conflicts_panel")
    ),

    bslib::nav_panel(
      title = shiny::actionButton("btn_done", "Done", class = "btn-primary btn-sm"),
      value = "_done"
    )
  )

  server <- function(input, output, shiny_session) {
    rv <- shiny::reactiveValues(
      candidates = session$candidates
    )

    # --- Object Types table ---
    output$table_object_types <- DT::renderDT({
      ot <- rv$candidates$object_types
      if (length(ot) == 0) return(data.frame(Message = "No object types found"))

      df <- data.frame(
        ID           = sapply(ot, function(x) x$id),
        Display      = sapply(ot, function(x) x$display_name),
        Confidence   = sapply(ot, function(x) x$confidence),
        Properties   = sapply(ot, function(x) length(x$properties %||% list())),
        Sources      = sapply(ot, function(x) length(x$source_refs)),
        Status       = sapply(ot, function(x) x$status),
        stringsAsFactors = FALSE
      )
      DT::datatable(df, selection = "single", rownames = FALSE,
                    options = list(pageLength = 25, dom = "lrtip"))
    }, server = TRUE)

    output$detail_object_type <- shiny::renderUI({
      sel <- input$table_object_types_rows_selected
      if (is.null(sel)) return(shiny::p("Select a row to see details."))
      ot <- rv$candidates$object_types[[sel]]

      shiny::tagList(
        shiny::h4(ot$display_name),
        shiny::p(shiny::strong("Description: "), ot$description %||% "(none)"),
        shiny::p(shiny::strong("Confidence: "), sprintf("%.2f", ot$confidence)),
        shiny::h5("Properties:"),
        shiny::tags$ul(lapply(ot$properties %||% list(), function(p) {
          shiny::tags$li(sprintf("%s: %s%s", p$id, p$type,
                                 if (isTRUE(p$nullable)) " (nullable)" else ""))
        })),
        shiny::h5("Sources:"),
        shiny::tags$ul(lapply(ot$source_refs, function(r) {
          shiny::tags$li(r$source_label)
        })),
        shiny::div(
          shiny::actionButton(paste0("approve_ot_", sel), "Approve", class = "btn-success btn-sm"),
          shiny::actionButton(paste0("reject_ot_", sel),  "Reject",  class = "btn-danger btn-sm ms-2")
        )
      )
    })

    # Approve/reject individual object type
    shiny::observe({
      sel <- input$table_object_types_rows_selected
      if (is.null(sel)) return()

      if (!is.null(input[[paste0("approve_ot_", sel)]]) &&
          input[[paste0("approve_ot_", sel)]] > 0) {
        isolate({
          rv$candidates$object_types[[sel]]$status <- "approved"
        })
      }
      if (!is.null(input[[paste0("reject_ot_", sel)]]) &&
          input[[paste0("reject_ot_", sel)]] > 0) {
        isolate({
          rv$candidates$object_types[[sel]]$status <- "rejected"
        })
      }
    })

    # Bulk approve high-confidence object types
    shiny::observeEvent(input$approve_all_high_ot, {
      rv$candidates$object_types <- lapply(rv$candidates$object_types, function(ot) {
        if (ot$confidence >= 0.8 && ot$status == "pending") ot$status <- "approved"
        ot
      })
    })

    # Bulk reject low-confidence object types
    shiny::observeEvent(input$reject_all_low_ot, {
      rv$candidates$object_types <- lapply(rv$candidates$object_types, function(ot) {
        if (ot$confidence < 0.3 && ot$status == "pending") ot$status <- "rejected"
        ot
      })
    })

    # --- Link Types table ---
    output$table_link_types <- DT::renderDT({
      lt <- rv$candidates$link_types
      if (length(lt) == 0) return(data.frame(Message = "No link types found"))

      df <- data.frame(
        ID          = sapply(lt, function(x) x$id),
        From        = sapply(lt, function(x) x$from_type_id),
        To          = sapply(lt, function(x) x$to_type_id),
        Cardinality = sapply(lt, function(x) x$cardinality),
        Confidence  = sapply(lt, function(x) x$confidence),
        Status      = sapply(lt, function(x) x$status),
        stringsAsFactors = FALSE
      )
      DT::datatable(df, selection = "single", rownames = FALSE,
                    options = list(pageLength = 25, dom = "lrtip"))
    }, server = TRUE)

    output$detail_link_type <- shiny::renderUI({
      sel <- input$table_link_types_rows_selected
      if (is.null(sel)) return(shiny::p("Select a row to see details."))
      lt <- rv$candidates$link_types[[sel]]

      shiny::tagList(
        shiny::h4(lt$display_name),
        shiny::p(shiny::strong("From: "), lt$from_type_id, " -> ", shiny::strong("To: "), lt$to_type_id),
        shiny::p(shiny::strong("Cardinality: "), lt$cardinality),
        shiny::p(shiny::strong("Directed: "), if (lt$directed) "Yes" else "No"),
        shiny::p(shiny::strong("Confidence: "), sprintf("%.2f", lt$confidence)),
        shiny::div(
          shiny::actionButton(paste0("approve_lt_", sel), "Approve", class = "btn-success btn-sm"),
          shiny::actionButton(paste0("reject_lt_",  sel), "Reject",  class = "btn-danger btn-sm ms-2")
        )
      )
    })

    shiny::observe({
      sel <- input$table_link_types_rows_selected
      if (is.null(sel)) return()
      if (!is.null(input[[paste0("approve_lt_", sel)]]) &&
          input[[paste0("approve_lt_", sel)]] > 0) {
        isolate({ rv$candidates$link_types[[sel]]$status <- "approved" })
      }
      if (!is.null(input[[paste0("reject_lt_", sel)]]) &&
          input[[paste0("reject_lt_", sel)]] > 0) {
        isolate({ rv$candidates$link_types[[sel]]$status <- "rejected" })
      }
    })

    # --- Concept Hints table ---
    output$table_concept_hints <- DT::renderDT({
      ch <- rv$candidates$concept_hints
      if (length(ch) == 0) return(data.frame(Message = "No concept hints found"))

      df <- data.frame(
        ID         = sapply(ch, function(x) x$id),
        Display    = sapply(ch, function(x) x$display_name),
        ObjectType = sapply(ch, function(x) x$object_type_id %||% ""),
        SQL        = sapply(ch, function(x) substr(x$sql_expr %||% "", 1, 60)),
        Confidence = sapply(ch, function(x) x$confidence),
        Status     = sapply(ch, function(x) x$status),
        stringsAsFactors = FALSE
      )
      DT::datatable(df, selection = "single", rownames = FALSE,
                    options = list(pageLength = 25, dom = "lrtip"))
    }, server = TRUE)

    output$detail_concept_hint <- shiny::renderUI({
      sel <- input$table_concept_hints_rows_selected
      if (is.null(sel)) return(shiny::p("Select a row to see details."))
      ch <- rv$candidates$concept_hints[[sel]]

      shiny::tagList(
        shiny::h4(ch$display_name),
        shiny::p(shiny::strong("Object Type: "), ch$object_type_id %||% "(none)"),
        shiny::p(shiny::strong("Description: "), ch$description %||% "(none)"),
        shiny::h5("SQL Expression:"),
        shiny::tags$pre(ch$sql_expr %||% ""),
        shiny::textAreaInput(paste0("edit_sql_", sel), "Edit SQL:", value = ch$sql_expr %||% "", rows = 3),
        shiny::div(
          shiny::actionButton(paste0("save_sql_", sel),    "Save SQL",  class = "btn-secondary btn-sm"),
          shiny::actionButton(paste0("approve_ch_", sel),  "Approve",   class = "btn-success btn-sm ms-2"),
          shiny::actionButton(paste0("reject_ch_",  sel),  "Reject",    class = "btn-danger btn-sm ms-2")
        )
      )
    })

    shiny::observe({
      sel <- input$table_concept_hints_rows_selected
      if (is.null(sel)) return()
      if (!is.null(input[[paste0("save_sql_", sel)]]) &&
          input[[paste0("save_sql_", sel)]] > 0) {
        isolate({
          rv$candidates$concept_hints[[sel]]$sql_expr <- input[[paste0("edit_sql_", sel)]]
        })
      }
      if (!is.null(input[[paste0("approve_ch_", sel)]]) &&
          input[[paste0("approve_ch_", sel)]] > 0) {
        isolate({ rv$candidates$concept_hints[[sel]]$status <- "approved" })
      }
      if (!is.null(input[[paste0("reject_ch_", sel)]]) &&
          input[[paste0("reject_ch_", sel)]] > 0) {
        isolate({ rv$candidates$concept_hints[[sel]]$status <- "rejected" })
      }
    })

    # --- Conflicts panel ---
    output$conflicts_panel <- shiny::renderUI({
      all_cands <- c(rv$candidates$object_types, rv$candidates$link_types)
      conflicts <- Filter(function(c) length(c$conflicts) > 0, all_cands)

      if (length(conflicts) == 0) {
        return(shiny::div(class = "alert alert-success", "No conflicts detected."))
      }

      shiny::tagList(lapply(conflicts, function(c) {
        shiny::div(class = "card mb-3",
          shiny::div(class = "card-body",
            shiny::h5(paste0("Conflict: ", c$id)),
            shiny::p(sprintf("This %s conflicts with %d other candidate(s).",
                             c$element_type, length(c$conflicts)))
          )
        )
      }))
    })

    # --- Done button ---
    shiny::observeEvent(input$btn_done, {
      session_env$session$candidates <- shiny::isolate(rv$candidates)
      shiny::stopApp()
    })
  }

  shiny::runApp(shiny::shinyApp(ui, server))
  session_env$session
}
