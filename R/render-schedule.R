library(conflicted)
library(dplyr)
conflicts_prefer(dplyr::filter)
library(fontawesome)
library(forcats)
library(fs)
library(glue)
library(gt)
library(gtExtras)
library(here)
library(lubridate)
library(readr)
library(stringr)
library(tidyr)

render_schedule <- function() {
  current_date <- today()

  # Used to sort column names when using `pivot_wider()`
  sorted_types <- c(
    "summaries",
    "pre_activities",
    "slides",
    "activities",
    "recording",
    "link",
    "practice"
  )
  sorted_units <- c("summary", "class", "potw", "lab", "exam")

  single_resource_units <- c("summary", "potw", "lab")

  # Generate ids for each link to join into schedule
  resources_paths <-
    c(
      path("pre-activities"),
      path("activities"),
      path("slides"),
      path("summaries")
    )

  schedule <- read_csv(here("data", "schedule.csv"), show_col_types = FALSE)
  sections <- read_csv(here("data", "sections.csv"), show_col_types = FALSE)
  additional_resources <- read_csv(
    here("data", "additional-resources.csv"),
    show_col_types = FALSE
  )

  resources <-
    tibble(
      resource = dir_ls(resources_paths, glob = "*.qmd"),
      id = path_file(resource) |> str_extract("^[^_]+"),
      type = resource |>
        path_dir() |>
        str_replace("-", "_")
    ) |>
    # Remove unassigned resources indicated by "tbd"
    filter(!str_detect(resource, "tbd")) |>
    relocate(resource, .after = type)

  all_resources <- bind_rows(resources, additional_resources) |>
    mutate(type = fct(type, levels = intersect(sorted_types, type))) |>
    arrange(type)

  detailed_schedule <- schedule |>
    mutate(
      week = date |> isoweek() |> consecutive_id(),
      monday = floor_date(date, unit = "week", week_start = "Mon"),
      current_week = isoweek(date) == isoweek(current_date),
      show_week = isoweek(date) >= isoweek(current_date),
      day = wday(date, label = TRUE, week_start = "Mon") |> str_to_lower(),
      unit = id |> str_extract("^[^-]+"),
      # Only use levels present in data
      unit = unit |> fct(levels = intersect(sorted_units, unit)),
      next_exam = if_else(unit == "exam", date, NA),
      show_exam = between(next_exam, current_date, current_date + days(13)),
      .after = date
    ) |>
    fill(next_exam, show_exam, .direction = "up") |>
    left_join(
      sections,
      by = join_by(closest(date >= start_date)),
      relationship = "many-to-one"
    ) |>
    left_join(
      all_resources,
      by = join_by(id),
      relationship = "one-to-many"
    )

  weeks <- detailed_schedule |>
    distinct(week, monday, current_week, show_week, show_exam)

  classes <- detailed_schedule |>
    filter(unit == "class", !is.na(resource)) |>
    select(week, day, unit, type, resource) |>
    pivot_wider(
      names_from = c(day, unit, type),
      names_sep = "_",
      values_from = resource
    )

  # Units with only a single resource
  other_units <- detailed_schedule |>
    filter(unit %in% single_resource_units, !is.na(resource)) |>
    select(week, unit, resource) |>
    pivot_wider(names_from = unit, values_from = resource)

  exams <- detailed_schedule |>
    filter(unit == "exam", !is.na(resource)) |>
    mutate(exam = id |> str_replace("-", " ") |> str_to_title()) |>
    select(week, exam, exam_due = date, exam_practice = resource)

  weekly_schedule <- weeks |>
    left_join(
      classes,
      by = join_by(week),
      relationship = "one-to-one"
    ) |>
    left_join(
      other_units,
      by = join_by(week),
      relationship = "one-to-one"
    ) |>
    left_join(exams, by = join_by(week), relationship = "one-to-one")

  weekly_schedule |>
    mutate(
      week = if_else(
        !is.na(summary),
        glue("[{week} {fa('circle-info')}]({summary})"),
        as.character(week)
      ),
      exam = if_else(
        show_exam,
        glue("[{exam}](https://us.prairietest.com)", .na = NULL),
        exam
      ),
      exam_practice = if_else(
        show_exam | (show_week),
        exam_practice,
        NA
      ),
      # Remove any resources from future weeks
      across(
        c(starts_with(c("tue", "thu")), lab, potw),
        \(column) if_else(show_week, column, NA)
      )
    ) |>
    gt(
      # groupname_col = "part"
    ) |>
    fmt_url(
      columns = week,
      rows = !is.na(summary),
      show_underline = FALSE
    ) |>
    fmt_date(
      monday,
      date_style = "MMMd"
    ) |>
    sub_missing(
      missing_text = ""
    ) |>
    fmt_url(
      columns = ends_with("slides"),
      label = fa("window-maximize")
    ) |>
    sub_missing(
      columns = ends_with("slides"),
      missing_text = fa("window-maximize", fill_opacity = 0.1)
    ) |>
    fmt_url(
      columns = ends_with("activities"),
      label = fa("file-alt")
    ) |>
    sub_missing(
      columns = ends_with("activities"),
      missing_text = fa("file-alt", fill_opacity = 0.1)
    ) |>
    fmt_url(
      columns = ends_with("pre_activities"),
      label = fa("book")
    ) |>
    sub_missing(
      columns = ends_with("pre_activities"),
      missing_text = fa("book", fill_opacity = 0.1)
    ) |>
    fmt_url(
      columns = ends_with("recording"),
      label = fa("circle-play")
    ) |>
    sub_missing(
      columns = ends_with("recording"),
      missing_text = fa("circle-play", fill_opacity = 0.1)
    ) |>
    fmt_url(
      columns = lab,
      label = fa("laptop-code")
    ) |>
    sub_missing(
      columns = lab,
      missing_text = fa("laptop-code", fill_opacity = 0.1)
    ) |>
    fmt_url(
      columns = potw,
      label = fa("calendar-week")
    ) |>
    sub_missing(
      columns = potw,
      missing_text = fa("calendar-week", fill_opacity = 0.1)
    ) |>
    # fmt_url(
    #   columns = project,
    #   label = fa("list-check")
    # ) |>
    # sub_missing(
    #   columns = project,
    #   missing_text = fa("clipboard-list", fill_opacity = 0.1)
    # ) |>
    # fmt_date(
    #   project_due,
    #   date_style = "MMMd"
    # ) |>
    fmt_url(
      columns = exam,
      rows = show_exam,
      show_underline = FALSE
    ) |>
    fmt_date(
      exam_due,
      date_style = "MMMd"
    ) |>
    fmt_url(
      columns = exam_practice,
      label = fa("pen-to-square")
    ) |>
    cols_label(
      week = "Week",
      monday = "Mon",
      tue_class_pre_activities = "P",
      tue_class_slides = "S",
      tue_class_activities = "A",
      tue_class_recording = "V",
      thu_class_pre_activities = "P",
      thu_class_slides = "S",
      thu_class_activities = "A",
      thu_class_recording = "V",
      lab = "Lab",
      potw = "POTW",
      # project = "Guide",
      # project_due = "Due",
      exam = "Book",
      exam_due = "Due",
      exam_practice = "Practice"
    ) |>
    tab_spanner(
      label = "Tue",
      columns = starts_with("tue")
    ) |>
    tab_spanner(
      label = "Thu",
      columns = starts_with("thu")
    ) |>
    # tab_spanner(
    #   label = "Project",
    #   columns = starts_with("project")
    # ) |>
    tab_spanner(
      label = "Examlet",
      columns = c(exam, exam_due, exam_practice)
    ) |>
    cols_align(
      align = "center",
      columns = c(lab, potw, exam_practice)
    ) |>
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = list(
        cells_row_groups(),
        cells_stubhead(),
        cells_column_labels(),
        cells_column_spanners()
      )
    ) |>
    gt_highlight_rows(
      row = current_week,
      fill = "#ccefff"
    ) |>
    cols_hide(
      c(
        summary,
        current_week,
        show_week,
        show_exam
      )
    ) |>
    tab_options(
      quarto.disable_processing = TRUE,
      table.width = "100%"
    )
}
