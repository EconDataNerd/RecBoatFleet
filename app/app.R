# Recreational Boating Dashboard 2: Registered Boat Fleet
# Source: Kevin Decker | recreational boating dashboards
#
# Data files in this folder are written by BoatFleetPipeline/run_update.R:
#   fleet_cube.rds            vessels and purchase value by year, moorage county,
#                             boat type, length class, hull, engine drive,
#                             propulsion, fuel
#   fleet_sent_received.csv   year, length class, county, sent, received,
#                             registered equals moorage
#   fleet_turnover.csv        year to year change by moorage county

library(shiny)

# ---- Data ----------------------------------------------------------------
cube <- readRDS("fleet_cube.rds")
sr   <- read.csv("fleet_sent_received.csv", check.names = FALSE, stringsAsFactors = FALSE)
turn <- if (file.exists("fleet_turnover.csv"))
  read.csv("fleet_turnover.csv", check.names = FALSE, stringsAsFactors = FALSE) else NULL

years       <- sort(unique(cube$year))
latest_year <- max(years)

wa_counties <- c(
  "Adams", "Asotin", "Benton", "Chelan", "Clallam", "Clark", "Columbia", "Cowlitz",
  "Douglas", "Ferry", "Franklin", "Garfield", "Grant", "Grays Harbor", "Island",
  "Jefferson", "King", "Kitsap", "Kittitas", "Klickitat", "Lewis", "Lincoln", "Mason",
  "Okanogan", "Pacific", "Pend Oreille", "Pierce", "San Juan", "Skagit", "Skamania",
  "Snohomish", "Spokane", "Stevens", "Thurston", "Wahkiakum", "Walla Walla", "Whatcom",
  "Whitman", "Yakima"
)
# The 15 coastal counties WSG serves
coastal_counties <- c(
  "Clallam", "Grays Harbor", "Island", "Jefferson", "King", "Kitsap", "Mason",
  "Pacific", "Pierce", "San Juan", "Skagit", "Snohomish", "Thurston",
  "Wahkiakum", "Whatcom"
)
western_wa <- c(coastal_counties, "Clark", "Cowlitz", "Lewis", "Skamania")
regions <- list(all = wa_counties, coastal = coastal_counties,
                west = sort(western_wa), east = setdiff(wa_counties, western_wa))
region_labels <- c(all = "Washington", coastal = "the 15 coastal counties",
                   west = "Western Washington", east = "Eastern Washington")

length_labels <- c(
  "10' or less", "11' – 14'", "15' – 18'", "19' – 22'", "23' – 26'",
  "27' – 30'", "31' – 34'", "35' – 38'", "39' – 42'", "43' – 46'",
  "47' – 50'", "51' – 54'", "55' – 58'", "59' – 62'", "63' – 66'",
  "67' – 70'", "More than 70'"
)

# Filterable attributes: label shown to users = column in fleet_cube.rds
explore_dims <- c(
  "Boat Type" = "boat_type", "Length Class" = "length_class", "Hull Material" = "hull",
  "Engine Drive" = "engine", "Propulsion" = "propulsion", "Fuel" = "fuel",
  "Moorage County" = "moor_county"
)
explore_choices <- lapply(explore_dims, function(col) {
  lv <- levels(droplevels(cube[[col]]))
  if (col == "length_class") lv <- c(length_labels[length_labels %in% lv], setdiff(lv, length_labels))
  if (col == "moor_county") lv <- c(wa_counties[wa_counties %in% lv], setdiff(lv, wa_counties))
  if ("Unknown" %in% lv) lv <- c(setdiff(lv, "Unknown"), "Unknown")
  lv
})

# DOL fleet files for these registration years were pulled partway into the
# following registration year, so they also include some boats first registered
# that following year. Counts run a little high and year to year turnover
# involving them is distorted.
early_pull_years <- c(2017, 2019)
early_pull_note <- function(y) if (y %in% early_pull_years) note(
  "Note: the DOL file for ", y, " was pulled partway into the next registration year, so ",
  "counts for ", y, " include some boats first registered in ", y + 1, " and run somewhat high.")

# Bars based on fewer boats than this are grayed out as less reliable
small_n <- 10

# ---- Helpers -------------------------------------------------------------
comma     <- function(x) formatC(round(x), format = "f", digits = 0, big.mark = ",")
fmt_int   <- function(x) if (is.null(x) || is.na(x)) "n/a" else comma(x)
fmt_money <- function(x) if (is.null(x) || is.na(x)) "n/a" else paste0("$", comma(x))
fmt_short_money <- function(x) {
  if (is.null(x) || is.na(x)) return("n/a")
  if (abs(x) >= 1e9) return(paste0("$", formatC(x / 1e9, format = "f", digits = 2), "B"))
  if (abs(x) >= 1e6) return(paste0("$", formatC(x / 1e6, format = "f", digits = 1), "M"))
  paste0("$", comma(x))
}
pct_change <- function(now, last) if (is.na(last) || last == 0) NA_real_ else (now - last) / last * 100
sgn        <- function(x) paste0(ifelse(x > 0, "+", ""), comma(x))
fmt_pct    <- function(x) if (is.na(x)) "" else sprintf("%+.1f%%", x)
chg_lab    <- function(now, prev) ifelse(!is.na(prev) & prev > 0, sprintf("  %+.0f%%", 100 * (now - prev) / prev), "")

# ---- Branding ------------------------------------------------------------
WSG_PURPLE <- "#4b2e83"; WSG_NAVY <- "#03317c"; WSG_TEAL <- "#00674d"
WSG_BLUE <- "#0385ca"; MUTED <- "#45818e"; GRAY <- "#bbbbbb"

logo_file <- local({
  if (!dir.exists("www")) return(NA_character_)
  imgs <- list.files("www", pattern = "(?i)\\.(png|jpe?g|svg|gif|webp)$")
  if (!length(imgs)) return(NA_character_)
  pref <- imgs[grepl("(?i)logo|wsg|sea.?grant", imgs)]
  if (length(pref)) pref[1] else imgs[1]
})
header_brand <- if (!is.na(logo_file)) {
  tags$img(src = logo_file, height = "62px", alt = "Washington Sea Grant", style = "display:block;")
} else {
  tags$span("WSG", style = paste0("font-weight:800; font-size:1.5em; letter-spacing:1px; color:white; ",
                                  "background:", WSG_PURPLE, "; padding:4px 12px; border-radius:4px;"))
}

watermark <- function() {
  mtext("Washington Sea Grant (WSG)", side = 3, line = -1.1, adj = 0.99,
        col = "#c9c9c9", cex = 0.75, font = 2)
}

kpi_card <- function(title, value, sub = NULL) {
  tags$div(
    style = paste0("flex:1; min-width:150px; background:#f7f5fb; border-left:4px solid ", WSG_PURPLE,
                   "; padding:8px 12px; border-radius:4px;"),
    tags$div(title, style = "color:#555; font-size:0.85em;"),
    tags$div(value, style = paste0("font-size:1.45em; font-weight:700; color:", WSG_PURPLE, ";")),
    if (!is.null(sub)) tags$div(sub, style = "color:#777; font-size:0.8em;")
  )
}
kpi_row <- function(...) tags$div(style = "display:flex; flex-wrap:wrap; gap:10px; margin:8px 0 14px;", ...)

note <- function(...) tags$p(..., style = "color:#888; font-size:0.85em; margin:4px 0 10px;")
intro <- function(...) tags$p(..., style = "color:#555; font-size:0.9em; margin:8px 0 10px;")

# Horizontal bar chart with value labels and optional change labels
hbar <- function(labels, values, prev = NULL, col = WSG_PURPLE, fmt = comma, ns = NULL) {
  o <- rev(seq_along(values))
  xmax <- max(values, 1) * 1.3
  cols <- rep(col, length(values))
  if (!is.null(ns)) cols[ns < small_n] <- GRAY
  par(mar = c(3, 11, 1.5, 1))
  bp <- barplot(values[o], horiz = TRUE, names.arg = labels[o], las = 1, border = NA,
                col = cols[o], xlim = c(0, xmax), cex.names = 0.85, xaxt = "n")
  lab <- fmt(values)
  if (!is.null(prev)) lab <- paste0(lab, chg_lab(values, prev))
  text(values[o], bp, lab[o], pos = 4, cex = 0.78, xpd = TRUE)
  ticks <- pretty(c(0, xmax / 1.3)); axis(1, at = ticks, labels = fmt(ticks), cex.axis = 0.8)
  watermark()
}

# ---- UI ------------------------------------------------------------------
ui <- fluidPage(
  title = "WSG Recreational Boat Fleet Dashboard",
  tags$style(HTML(".selectize-input { max-height: 84px; overflow-y: auto; }")),
  tags$div(
    style = paste0("display:flex; align-items:center; gap:16px; padding:10px 4px; ",
                   "margin-bottom:10px; border-bottom:3px solid ", WSG_PURPLE, ";"),
    header_brand,
    tags$div(
      tags$div("Recreational Boat Fleet Dashboard",
               style = paste0("font-weight:700; font-size:1.2em; color:", WSG_PURPLE, ";")),
      tags$div("Washington State registered recreational vessels by registration year",
               style = "color:#555; font-size:0.9em;"))
  ),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("year", "Registration year", choices = rev(years), selected = latest_year),
      tags$p("A registration year runs July 1 to June 30 and is named for the June it ends.",
             style = "color:#666; font-size:0.85em; margin-top:-6px;"),
      conditionalPanel("input.tabs !== 'Explore' && input.tabs !== 'Definitions & Source'",
        radioButtons("region", "Counties shown",
                     choices = c("All of Washington" = "all", "15 coastal counties" = "coastal",
                                 "Western WA" = "west", "Eastern WA" = "east"),
                     selected = "coastal")),
      conditionalPanel("input.tabs === 'Explore'",
        tags$p("This tab has its own filters above the charts.",
               style = "color:#666; font-size:0.9em;"))
    ),
    mainPanel(
      width = 9,
      tabsetPanel(id = "tabs",

        tabPanel("Fleet Overview",
          uiOutput("ov_kpis"),
          uiOutput("ov_early"),
          h4(textOutput("ov_trend_title"), style = "margin:6px 0;"),
          plotOutput("ov_trend_plot", height = "280px"),
          h4(textOutput("ov_type_title"), style = "margin:14px 0 6px;"),
          plotOutput("ov_type_plot", height = "320px"),
          note("Percentages compare with the prior registration year. Boats are counted by ",
               "where they are moored.")
        ),

        tabPanel("Explore",
          intro("Filter the fleet by any combination of attributes. Leave a box empty to include ",
                "everything. Results are for the registration year chosen on the left; the trend ",
                "chart shows every year."),
          fluidRow(lapply(names(explore_dims)[1:4], function(lbl) column(3,
            selectizeInput(paste0("ex_", explore_dims[[lbl]]), lbl, choices = explore_choices[[lbl]],
                           multiple = TRUE,
                           options = list(placeholder = "All", plugins = list("remove_button")))))),
          fluidRow(
            lapply(names(explore_dims)[5:6], function(lbl) column(3,
              selectizeInput(paste0("ex_", explore_dims[[lbl]]), lbl, choices = explore_choices[[lbl]],
                             multiple = TRUE,
                             options = list(placeholder = "All", plugins = list("remove_button"))))),
            column(6,
              selectizeInput("ex_moor_county", "Moorage County", choices = explore_choices[["Moorage County"]],
                             multiple = TRUE,
                             options = list(placeholder = "All", plugins = list("remove_button"))),
              tags$div(style = "margin:-10px 0 8px; font-size:0.85em;",
                actionLink("ex_pick_coastal", "15 coastal counties"), " · ",
                actionLink("ex_pick_clear", "Clear"), " · ",
                actionLink("ex_reset", "Reset all filters")))
          ),
          uiOutput("ex_kpis"),
          fluidRow(
            column(6, selectInput("ex_group", "Compare by", choices = explore_dims, selected = "boat_type")),
            column(6, selectInput("ex_metric", "Show",
                                  choices = c("Registered vessels" = "vessels",
                                              "Average purchase price" = "avg",
                                              "Total purchase value" = "value"),
                                  selected = "vessels"))
          ),
          plotOutput("ex_group_plot", height = "auto"),
          note(textOutput("ex_small_note", inline = TRUE)),
          h4("Registered vessels matching the filters, by year", style = "margin:10px 0 6px;"),
          plotOutput("ex_trend_plot", height = "260px"),
          h4("Summary table", style = "margin:14px 0 6px;"),
          tableOutput("ex_table"),
          downloadButton("ex_download", "Download table (CSV)", class = "btn-sm")
        ),

        tabPanel("Counties",
          intro("Registered vessels by the county where they are moored."),
          h4(textOutput("co_rank_title"), style = "margin:6px 0;"),
          plotOutput("co_rank_plot", height = "auto"),
          note(textOutput("co_rank_note", inline = TRUE)),
          tags$hr(),
          fluidRow(column(4, selectInput("co_county", "County detail", choices = wa_counties,
                                         selected = "Thurston"))),
          h4(textOutput("co_trend_title"), style = "margin:6px 0;"),
          plotOutput("co_trend_plot", height = "260px"),
          h5(textOutput("co_mix_title"), style = "font-weight:700; margin-top:12px;"),
          tableOutput("co_mix")
        ),

        tabPanel("Sent & Received",
          intro("Many owners keep their boat in a different county from the one they live in. ",
                tags$b("Home county"), " counts boats moored in the owner's own county. ",
                tags$b("Sent"), " counts boats owned by county residents but moored elsewhere. ",
                tags$b("Received"), " counts boats moored in the county whose owners live elsewhere, ",
                "including out of state."),
          fluidRow(column(6,
            selectizeInput("sr_len", "Length Class", choices = length_labels, multiple = TRUE,
                           options = list(placeholder = "All lengths", plugins = list("remove_button"))))),
          h4(textOutput("sr_title"), style = "margin:6px 0;"),
          plotOutput("sr_plot", height = "auto"),
          h5("Net boats received (received minus sent)", style = "font-weight:700; margin-top:12px;"),
          tableOutput("sr_table"),
          downloadButton("sr_download", "Download table (CSV)", class = "btn-sm"),
          tags$hr(),
          fluidRow(column(4, selectInput("sr_county", "County detail", choices = wa_counties,
                                         selected = "Thurston"))),
          h4(textOutput("sr_trend_title"), style = "margin:6px 0;"),
          plotOutput("sr_trend_plot", height = "260px")
        ),

        tabPanel("Turnover",
          intro("Year to year change in each county's fleet, following each boat by its hull ID. ",
                tags$b("New to fleet"), " were not registered the year before (new boats, boats ",
                "brought into the state, and boats coming back after a lapse). ",
                tags$b("Left fleet"), " were registered the year before but not this year. ",
                tags$b("Moved in"), " and ", tags$b("Moved out"), " changed moorage county."),
          uiOutput("to_note"),
          h4(textOutput("to_title"), style = "margin:6px 0;"),
          tableOutput("to_table"),
          downloadButton("to_download", "Download table (CSV)", class = "btn-sm"),
          h4(textOutput("to_trend_title"), style = "margin:16px 0 6px;"),
          plotOutput("to_trend_plot", height = "280px")
        ),

        tabPanel("Definitions & Source",
          h4("How the fleet is counted"),
          tags$ul(
            tags$li(tags$b("Registration year"), ": July 1 to June 30, named for the June it ends. ",
                    "2026 means July 2025 to June 2026."),
            tags$li(tags$b("Registered vessels"), ": every recreational vessel with a Washington ",
                    "registration that was valid during that registration year, taken from the ",
                    "annual DOL fleet file. Each boat is counted once per year."),
            tags$li(tags$b("Moorage county"), ": where the owner reports the boat is kept. ",
                    "A small number of records have no usable moorage county and show as Unknown."),
            tags$li(tags$b("Resident county"), ": the county of the owner's ZIP code. Owners with ",
                    "ZIP codes outside Washington are out of state."),
            tags$li(tags$b("Purchase price"), ": the price DOL recorded at the boat's most recent ",
                    "ownership transfer, not its current market value."),
            tags$li(tags$b("Turnover"), ": boats are matched across years by hull ID. Records with ",
                    "missing or placeholder hull IDs cannot be followed and are left out of the ",
                    "new, left, and moved counts."),
            tags$li(tags$b("2017 and 2019"), ": the DOL files for these years were pulled partway ",
                    "into the following registration year, so they include some boats first ",
                    "registered the next year. Their counts run somewhat high, and turnover for ",
                    "2018 through 2020 is distorted.")
          ),
          tags$hr(),
          h4("Data source"),
          tags$p("The results include data obtained from the Washington State Department of ",
                 "Licensing (DOL). DOL did not verify the accuracy of the data or how it was used.")
        )
      )
    )
  ),
  tags$hr(style = "margin-top:18px;"),
  tags$p(
    HTML(paste0(
      "Produced by <b>Washington Sea Grant (WSG)</b> · University of Washington. ",
      "Data source: WA Department of Licensing. &copy; ", format(Sys.Date(), "%Y"),
      " Washington Sea Grant. Not for redistribution or reuse without attribution.")),
    style = "color:#777; font-size:0.8em; text-align:center; padding:4px 0 12px;"
  )
)

# ---- Server --------------------------------------------------------------
server <- function(input, output, session) {

  yr      <- reactive(as.integer(input$year))
  prev_yr <- reactive(if ((yr() - 1L) %in% years) yr() - 1L else NA_integer_)
  reg     <- reactive(if (is.null(input$region)) "coastal" else input$region)
  reg_cty <- reactive(regions[[reg()]])
  reg_lab <- reactive(region_labels[[reg()]])

  # ---- Fleet Overview ----------------------------------------------------
  by_year_region <- reactive({
    d <- cube[as.character(cube$moor_county) %in% reg_cty(), ]
    data.frame(year = years,
               vessels = as.numeric(tapply(d$vessels, factor(d$year, levels = years), sum)),
               value = as.numeric(tapply(d$value, factor(d$year, levels = years), sum)))
  })

  output$ov_kpis <- renderUI({
    y <- yr(); py <- prev_yr()
    all_y <- cube[cube$year == y, ]
    tot <- sum(all_y$vessels)
    tot_p <- if (is.na(py)) NA else sum(cube$vessels[cube$year == py])
    r <- by_year_region()
    rv <- r$vessels[r$year == y]; rv_p <- if (is.na(py)) NA else r$vessels[r$year == py]
    rval <- r$value[r$year == y]
    kpi_row(
      kpi_card("Registered vessels, all of WA", fmt_int(tot),
               if (!is.na(tot_p)) paste(fmt_pct(pct_change(tot, tot_p)), "from", py)),
      kpi_card(paste("Moored in", reg_lab()), fmt_int(rv),
               if (!is.na(rv_p)) paste(fmt_pct(pct_change(rv, rv_p)), "from", py)),
      kpi_card("Total purchase value", fmt_short_money(rval), paste("Boats moored in", reg_lab())),
      kpi_card("Average purchase price", fmt_money(if (rv > 0) rval / rv else NA),
               paste("Boats moored in", reg_lab()))
    )
  })

  output$ov_early <- renderUI(early_pull_note(yr()))

  output$ov_trend_title <- renderText(paste("Registered vessels moored in", reg_lab(), "by registration year"))
  output$ov_trend_plot <- renderPlot({
    r <- by_year_region()
    cols <- ifelse(r$year == yr(), WSG_PURPLE, "#b7a9d6")
    par(mar = c(3, 5, 1.5, 1))
    ymax <- max(r$vessels, 1) * 1.15
    bp <- barplot(r$vessels, names.arg = r$year, col = cols, border = NA, ylim = c(0, ymax),
                  yaxt = "n", cex.names = 0.85)
    text(bp, r$vessels, comma(r$vessels), pos = 3, cex = 0.75, xpd = TRUE)
    ticks <- pretty(c(0, ymax)); axis(2, at = ticks, labels = comma(ticks), las = 1, cex.axis = 0.8)
    watermark()
  })

  output$ov_type_title <- renderText(paste0("Fleet by boat type, ", reg_lab(), ", ", yr()))
  output$ov_type_plot <- renderPlot({
    d <- cube[as.character(cube$moor_county) %in% reg_cty(), ]
    now <- tapply(d$vessels[d$year == yr()], as.character(d$boat_type[d$year == yr()]), sum)
    now <- sort(now[!is.na(now)], decreasing = TRUE)
    prev <- if (is.na(prev_yr())) rep(NA, length(now)) else {
      p <- tapply(d$vessels[d$year == prev_yr()], as.character(d$boat_type[d$year == prev_yr()]), sum)
      as.numeric(p[names(now)])
    }
    hbar(names(now), as.numeric(now), prev, col = WSG_TEAL)
  })

  # ---- Explore -----------------------------------------------------------
  observeEvent(input$ex_pick_coastal, updateSelectizeInput(session, "ex_moor_county", selected = coastal_counties))
  observeEvent(input$ex_pick_clear,   updateSelectizeInput(session, "ex_moor_county", selected = character(0)))
  observeEvent(input$ex_reset, for (col in explore_dims)
    updateSelectizeInput(session, paste0("ex_", col), selected = character(0)))

  ex_all <- reactive({
    keep <- rep(TRUE, nrow(cube))
    for (col in explore_dims) {
      sel <- input[[paste0("ex_", col)]]
      if (length(sel)) keep <- keep & as.character(cube[[col]]) %in% sel
    }
    cube[keep, ]
  })
  ex_year <- reactive({ d <- ex_all(); d[d$year == yr(), ] })

  output$ex_kpis <- renderUI({
    d <- ex_year(); n <- sum(d$vessels); v <- sum(d$value)
    np <- if (is.na(prev_yr())) NA else sum(ex_all()$vessels[ex_all()$year == prev_yr()])
    kpi_row(
      kpi_card(paste("Registered vessels,", yr()), fmt_int(n),
               if (!is.na(np)) paste(fmt_pct(pct_change(n, np)), "from", prev_yr())),
      kpi_card("Total purchase value", fmt_short_money(v)),
      kpi_card("Average purchase price", fmt_money(if (n > 0) v / n else NA))
    )
  })

  ex_grouped <- reactive({
    d <- ex_year(); g <- input$ex_group
    if (!nrow(d)) return(NULL)
    key <- as.character(d[[g]])
    out <- data.frame(group = sort(unique(key)), stringsAsFactors = FALSE)
    out$vessels <- as.numeric(tapply(d$vessels, key, sum)[out$group])
    out$value   <- as.numeric(tapply(d$value, key, sum)[out$group])
    out$avg     <- out$value / out$vessels
    if (g == "length_class") out <- out[order(match(out$group, c(length_labels, "Unknown"))), ]
    else if (g == "moor_county") out <- out[order(-out$vessels), ]
    else out <- out[order(-out$vessels), ]
    out
  })

  output$ex_group_plot <- renderPlot({
    g <- ex_grouped()
    if (is.null(g)) { plot.new(); text(0.5, 0.5, "No vessels match these filters."); return() }
    m <- input$ex_metric
    fmt <- if (m == "vessels") comma else function(x) sapply(x, fmt_short_money)
    hbar(g$group, g[[m]], col = WSG_PURPLE, fmt = fmt, ns = if (m == "avg") g$vessels else NULL)
  }, height = function() {
    g <- ex_grouped(); max(220, (if (is.null(g)) 1 else nrow(g)) * 22 + 60)
  })

  output$ex_small_note <- renderText({
    if (identical(input$ex_metric, "avg"))
      paste0("Gray bars are based on fewer than ", small_n, " vessels.") else ""
  })

  output$ex_trend_plot <- renderPlot({
    d <- ex_all()
    n <- as.numeric(tapply(d$vessels, factor(d$year, levels = years), sum)); n[is.na(n)] <- 0
    par(mar = c(3, 5, 1.5, 1))
    ymax <- max(n, 1) * 1.15
    plot(years, n, type = "o", pch = 19, col = WSG_PURPLE, lwd = 2, ylim = c(0, ymax),
         xlab = "", ylab = "", yaxt = "n", xaxt = "n", bty = "l")
    axis(1, at = years, cex.axis = 0.8)
    ticks <- pretty(c(0, ymax)); axis(2, at = ticks, labels = comma(ticks), las = 1, cex.axis = 0.8)
    points(yr(), n[years == yr()], pch = 19, cex = 1.8, col = WSG_TEAL)
    text(years, n, comma(n), pos = 3, cex = 0.7)
    watermark()
  })

  ex_table_data <- reactive({
    g <- ex_grouped(); if (is.null(g)) return(NULL)
    lbl <- names(explore_dims)[explore_dims == input$ex_group]
    out <- data.frame(g$group, comma(g$vessels), sprintf("%.1f%%", 100 * g$vessels / sum(g$vessels)),
                      sapply(g$value, fmt_short_money), sapply(g$avg, fmt_money),
                      stringsAsFactors = FALSE)
    names(out) <- c(lbl, "Vessels", "Share", "Total purchase value", "Average purchase price")
    out
  })
  output$ex_table <- renderTable(ex_table_data(), align = "lrrrr")
  output$ex_download <- downloadHandler(
    filename = function() paste0("wa_boat_fleet_", input$ex_group, "_", yr(), ".csv"),
    content = function(file) {
      g <- ex_grouped()
      write.csv(data.frame(year = yr(), group = g$group, vessels = g$vessels,
                           total_purchase_value = round(g$value), average_purchase_price = round(g$avg)),
                file, row.names = FALSE)
    })

  # ---- Counties ----------------------------------------------------------
  county_counts <- function(y) {
    d <- cube[cube$year == y, ]
    as.numeric(tapply(d$vessels, factor(as.character(d$moor_county), levels = wa_counties), sum))
  }
  co_data <- reactive({
    now <- county_counts(yr()); now[is.na(now)] <- 0
    prev <- if (is.na(prev_yr())) rep(NA, length(now)) else county_counts(prev_yr())
    df <- data.frame(county = wa_counties, now = now, prev = prev, stringsAsFactors = FALSE)
    df <- df[df$county %in% reg_cty(), ]
    df[order(-df$now), ]
  })
  output$co_rank_title <- renderText(paste0("Registered vessels by moorage county, ", reg_lab(), ", ", yr()))
  output$co_rank_plot <- renderPlot({
    df <- co_data(); hbar(df$county, df$now, df$prev, col = MUTED)
  }, height = function() max(260, length(reg_cty()) * 20 + 60))
  output$co_rank_note <- renderText({
    d <- cube[cube$year == yr(), ]
    oos <- sum(d$vessels[d$moor_county == "Out-of-State"]); unk <- sum(d$vessels[d$moor_county == "Unknown"])
    paste0(if (!is.na(prev_yr())) paste0("Percentages compare with ", prev_yr(), ". ") else "",
           "Not shown: ", comma(oos), " Washington registered boats moored out of state and ",
           comma(unk), " with no usable moorage county.")
  })

  output$co_trend_title <- renderText(paste("Registered vessels moored in", input$co_county, "County"))
  output$co_trend_plot <- renderPlot({
    d <- cube[as.character(cube$moor_county) == input$co_county, ]
    n <- as.numeric(tapply(d$vessels, factor(d$year, levels = years), sum)); n[is.na(n)] <- 0
    par(mar = c(3, 5, 1.5, 1)); ymax <- max(n, 1) * 1.15
    bp <- barplot(n, names.arg = years, col = ifelse(years == yr(), WSG_PURPLE, "#b7a9d6"),
                  border = NA, ylim = c(0, ymax), yaxt = "n", cex.names = 0.85)
    text(bp, n, comma(n), pos = 3, cex = 0.75, xpd = TRUE)
    ticks <- pretty(c(0, ymax)); axis(2, at = ticks, labels = comma(ticks), las = 1, cex.axis = 0.8)
    watermark()
  })
  output$co_mix_title <- renderText(paste0("Boat types moored in ", input$co_county, " County, ", yr()))
  output$co_mix <- renderTable({
    d <- cube[as.character(cube$moor_county) == input$co_county, ]
    now <- tapply(d$vessels[d$year == yr()], as.character(d$boat_type[d$year == yr()]), sum)
    now <- sort(now[!is.na(now)], decreasing = TRUE)
    out <- data.frame(`Boat Type` = names(now), Vessels = comma(now),
                      Share = sprintf("%.1f%%", 100 * now / sum(now)), check.names = FALSE)
    if (!is.na(prev_yr())) {
      p <- tapply(d$vessels[d$year == prev_yr()], as.character(d$boat_type[d$year == prev_yr()]), sum)
      out[[paste("Change from", prev_yr())]] <- sapply(seq_along(now), function(i)
        fmt_pct(pct_change(now[i], as.numeric(p[names(now)[i]]))))
    } else {
      out[["Change from prior year"]] <- ""
    }
    out
  }, align = "lrrr")

  # ---- Sent & Received ---------------------------------------------------
  sr_sel <- reactive({
    d <- sr
    if (length(input$sr_len)) d <- d[d$`Length Class` %in% input$sr_len, ]
    d
  })
  sr_year <- reactive({
    d <- sr_sel(); d <- d[d$Year == yr(), ]
    f <- factor(d$County, levels = wa_counties)
    df <- data.frame(county = wa_counties,
                     home = as.numeric(tapply(d$`Registered equals moorage`, f, sum)),
                     sent = as.numeric(tapply(d$Sent, f, sum)),
                     recv = as.numeric(tapply(d$Received, f, sum)), stringsAsFactors = FALSE)
    df[is.na(df)] <- 0
    df <- df[df$county %in% reg_cty(), ]
    df$net <- df$recv - df$sent
    df[order(-(df$home + df$recv)), ]
  })
  output$sr_title <- renderText(paste0("Boats kept at home, sent, and received, ", reg_lab(), ", ", yr()))
  output$sr_plot <- renderPlot({
    df <- sr_year(); o <- rev(seq_len(nrow(df)))
    m <- rbind(df$recv, df$sent, df$home)[, o, drop = FALSE]
    xmax <- max(m, 1) * 1.2
    par(mar = c(3, 9, 2, 1))
    bp <- barplot(m, beside = TRUE, horiz = TRUE, names.arg = df$county[o], las = 1, border = NA,
                  col = c(WSG_BLUE, "#e69138", WSG_PURPLE), xlim = c(0, xmax), cex.names = 0.85,
                  xaxt = "n", space = c(0, 0.6))
    text(m, bp, comma(m), pos = 4, cex = 0.65, xpd = TRUE)
    ticks <- pretty(c(0, xmax / 1.2)); axis(1, at = ticks, labels = comma(ticks), cex.axis = 0.8)
    legend("bottomright", bty = "n", cex = 0.85,
           fill = c(WSG_PURPLE, "#e69138", WSG_BLUE), border = NA,
           legend = c("Home county", "Sent", "Received"))
    watermark()
  }, height = function() max(300, length(reg_cty()) * 36 + 70))
  sr_table_data <- reactive({
    df <- sr_year(); df <- df[order(-df$net), ]
    data.frame(County = df$county, `Home county` = comma(df$home), Sent = comma(df$sent),
               Received = comma(df$recv), `Net received` = sgn(df$net),
               check.names = FALSE)
  })
  output$sr_table <- renderTable(sr_table_data(), align = "lrrrr")
  output$sr_download <- downloadHandler(
    filename = function() paste0("wa_boat_fleet_sent_received_", yr(), ".csv"),
    content = function(file) {
      df <- sr_year()
      write.csv(data.frame(year = yr(), county = df$county, home_county = df$home, sent = df$sent,
                           received = df$recv, net_received = df$net), file, row.names = FALSE)
    })
  output$sr_trend_title <- renderText(paste(input$sr_county, "County: boats sent and received by year"))
  output$sr_trend_plot <- renderPlot({
    d <- sr_sel(); d <- d[d$County == input$sr_county, ]
    s <- as.numeric(tapply(d$Sent, factor(d$Year, levels = years), sum))
    r <- as.numeric(tapply(d$Received, factor(d$Year, levels = years), sum))
    s[is.na(s)] <- 0; r[is.na(r)] <- 0
    par(mar = c(3, 5, 1.5, 1)); ymax <- max(s, r, 1) * 1.2
    plot(years, s, type = "o", pch = 19, col = "#e69138", lwd = 2, ylim = c(0, ymax),
         xlab = "", ylab = "", yaxt = "n", xaxt = "n", bty = "l")
    lines(years, r, type = "o", pch = 19, col = WSG_BLUE, lwd = 2)
    axis(1, at = years, cex.axis = 0.8)
    ticks <- pretty(c(0, ymax)); axis(2, at = ticks, labels = comma(ticks), las = 1, cex.axis = 0.8)
    legend("bottomleft", bty = "n", horiz = TRUE, lwd = 2, pch = 19, col = c("#e69138", WSG_BLUE),
           legend = c("Sent", "Received"), cex = 0.85)
    watermark()
  })

  # ---- Turnover ----------------------------------------------------------
  to_year <- reactive({
    if (is.null(turn)) return(NULL)
    d <- turn[turn$Year == yr() & turn$`Moorage County` %in% reg_cty(), ]
    if (!nrow(d)) NULL else d[order(-d$Vessels), ]
  })
  output$to_note <- renderUI({
    if (is.null(to_year())) return(note("Turnover needs the year before, so it is not available for ",
                                        yr(), ". Pick a later year on the left."))
    if (any(c(yr(), prev_yr()) %in% early_pull_years))
      note("Note: ", intersect(c(prev_yr(), yr()), early_pull_years)[1], " comes from a DOL file pulled ",
           "partway into the next registration year, so the new and left counts for this pair of ",
           "years are distorted. Comparisons from 2021 on are the most reliable.")
  })
  output$to_title <- renderText(if (!is.null(to_year()))
    paste0("Fleet change by moorage county, ", reg_lab(), ", ", prev_yr(), " to ", yr()) else "")
  to_table_data <- reactive({
    d <- to_year(); if (is.null(d)) return(NULL)
    tot <- colSums(d[, -(1:2)])
    d <- rbind(d, cbind(data.frame(Year = yr(), `Moorage County` = "Total", check.names = FALSE),
                        as.data.frame(t(tot))))
    data.frame(County = d$`Moorage County`,
               `Vessels prior year` = comma(d$`Vessels Prior Year`),
               `New to fleet` = comma(d$`New to Fleet`), `Left fleet` = comma(d$`Left Fleet`),
               `Moved in` = comma(d$`Moved In`), `Moved out` = comma(d$`Moved Out`),
               Vessels = comma(d$Vessels),
               `Net change` = sgn(d$Vessels - d$`Vessels Prior Year`),
               check.names = FALSE)
  })
  output$to_table <- renderTable(to_table_data(), align = "lrrrrrrr")
  output$to_download <- downloadHandler(
    filename = function() paste0("wa_boat_fleet_turnover_", yr(), ".csv"),
    content = function(file) write.csv(to_year(), file, row.names = FALSE))
  output$to_trend_title <- renderText(paste("Boats new to and leaving the fleet,", reg_lab()))
  output$to_trend_plot <- renderPlot({
    if (is.null(turn)) return()
    d <- turn[turn$`Moorage County` %in% reg_cty(), ]
    ys <- sort(unique(d$Year))
    nw <- as.numeric(tapply(d$`New to Fleet`, factor(d$Year, levels = ys), sum))
    lf <- as.numeric(tapply(d$`Left Fleet`, factor(d$Year, levels = ys), sum))
    m <- rbind(nw, lf); ymax <- max(m, 1) * 1.15
    par(mar = c(3, 5, 2, 1))
    bp <- barplot(m, beside = TRUE, names.arg = ys, col = c(WSG_TEAL, "#cc4125"), border = NA,
                  ylim = c(0, ymax), yaxt = "n", cex.names = 0.85)
    text(bp, m, comma(m), pos = 3, cex = 0.6, xpd = TRUE)
    ticks <- pretty(c(0, ymax)); axis(2, at = ticks, labels = comma(ticks), las = 1, cex.axis = 0.8)
    legend("topright", inset = c(0, -0.04), xpd = TRUE, horiz = TRUE, bty = "n", cex = 0.85,
           fill = c(WSG_TEAL, "#cc4125"), border = NA, legend = c("New to fleet", "Left fleet"))
    watermark()
  })
}

shinyApp(ui, server)
