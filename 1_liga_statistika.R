# ==========================================
# LFF LVBET līga 2026
# Vārtu guvēji un rezultatīvās piespēles
# - realizēti soda sitieni = vārti
# - nerealizēti soda sitieni != vārti
# - bez komandu kolonnām HTML izvadē
# - tabulām ir filtrs un šķirošana
# - HTML saglabā uz docs/index.html# ==========================================

# ---------- Pakotnes ----------
packages <- c(
  "xml2", "httr", "stringr", "dplyr",
  "purrr", "tibble", "htmltools"
)

new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) {
  install.packages(new_packages, repos = "https://cloud.r-project.org")
}

invisible(lapply(packages, library, character.only = TRUE))

# ---------- Parametri ----------
base_url <- "https://lff.lv"
season_url_games <- "https://lff.lv/sacensibas/viriesi/lvbet-liga/"

# ---------- Parametri ----------
base_url <- "https://lff.lv"
season_url_games <- "https://lff.lv/sacensibas/viriesi/lvbet-liga/"

output_dir <- "docs"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

output_file <- file.path(output_dir, "index.html")

# ---------- Palīgfunkcijas ----------
read_html_safe <- function(url, sleep_sec = 0.4) {
  Sys.sleep(sleep_sec)
  
  tryCatch({
    resp <- httr::GET(
      url,
      httr::user_agent("Mozilla/5.0 (compatible; R scraper)")
    )
    
    if (httr::status_code(resp) != 200) {
      message("Neizdevās atvērt: ", url, " | status: ", httr::status_code(resp))
      return(NULL)
    }
    
    txt <- httr::content(resp, as = "text", encoding = "UTF-8")
    xml2::read_html(txt)
  }, error = function(e) {
    message("Kļūda pie URL: ", url)
    message(e$message)
    NULL
  })
}

clean_text_vec <- function(x) {
  x <- stringr::str_replace_all(x, "[\r\n\t]+", " ")
  x <- stringr::str_replace_all(x, "\\u00A0", " ")
  x <- stringr::str_squish(x)
  x[x != ""]
}

is_minute <- function(x) {
  stringr::str_detect(x, "^\\d+(\\+\\d+)?['’]$")
}

is_regular_goal_line <- function(x) {
  stringr::str_detect(x, stringr::regex("^Vāāāārti!", ignore_case = TRUE))
}

is_penalty_goal_line <- function(x) {
  stringr::str_detect(x, stringr::regex("^Soda sitiens!", ignore_case = TRUE))
}

is_missed_penalty_line <- function(x) {
  stringr::str_detect(x, stringr::regex("^Nerealizēts soda sitiens!", ignore_case = TRUE))
}

is_goal_line <- function(x) {
  is_regular_goal_line(x) || is_penalty_goal_line(x)
}

extract_match_title <- function(doc) {
  h1_node <- xml2::xml_find_first(doc, ".//h1")
  if (!inherits(h1_node, "xml_missing")) {
    x <- xml2::xml_text(h1_node)
    x <- stringr::str_squish(x)
    if (!is.na(x) && x != "") return(x)
  }
  
  title_node <- xml2::xml_find_first(doc, ".//title")
  if (!inherits(title_node, "xml_missing")) {
    x <- xml2::xml_text(title_node)
    x <- stringr::str_squish(x)
    return(x)
  }
  
  NA_character_
}

extract_teams <- function(doc, match_title = NA_character_) {
  team_nodes <- xml2::xml_find_all(doc, ".//a[contains(@href, '/komandas/')]")
  team_names <- xml2::xml_text(team_nodes)
  team_names <- clean_text_vec(team_names)
  
  if (length(team_names) >= 2) {
    return(list(
      home_team = team_names[1],
      away_team = team_names[2]
    ))
  }
  
  if (!is.na(match_title) && stringr::str_detect(match_title, " - ")) {
    parts <- stringr::str_split(match_title, " - ", simplify = TRUE)
    if (ncol(parts) >= 2) {
      return(list(
        home_team = stringr::str_squish(parts[1]),
        away_team = stringr::str_squish(parts[2])
      ))
    }
  }
  
  list(home_team = NA_character_, away_team = NA_character_)
}

extract_match_datetime <- function(lines) {
  x <- stringr::str_subset(
    lines,
    "^\\d{1,2}\\s+[[:alpha:]ĀČĒĢĪĶĻŅŠŪŽāčēģīķļņšūž]+\\s+\\d{4}\\s+\\d{2}:\\d{2}$"
  )
  if (length(x) == 0) return(NA_character_)
  x[1]
}

extract_timeline_lines <- function(all_lines) {
  start_idx <- which(all_lines == "Spēle sākusies!")
  end_idx   <- which(all_lines == "Spēle beigusies!")
  
  if (length(start_idx) == 0 || length(end_idx) == 0) {
    return(character())
  }
  
  start_idx <- start_idx[1]
  end_idx <- end_idx[end_idx > start_idx][1]
  
  if (is.na(end_idx)) return(character())
  
  all_lines[start_idx:end_idx]
}

extract_header_score <- function(all_lines, home_team, away_team) {
  game_gaita_idx <- which(all_lines == "Spēles gaita")[1]
  
  header_lines <- if (!is.na(game_gaita_idx)) {
    all_lines[1:game_gaita_idx]
  } else {
    all_lines
  }
  
  get_next_score <- function(lines, team_name, from_idx = 1L) {
    team_idx <- which(lines == team_name)
    team_idx <- team_idx[team_idx >= from_idx][1]
    
    if (is.na(team_idx)) {
      return(list(score = NA_integer_, idx = NA_integer_))
    }
    
    num_idx <- which(stringr::str_detect(lines, "^\\d+$"))
    num_idx <- num_idx[num_idx > team_idx][1]
    
    if (is.na(num_idx)) {
      return(list(score = NA_integer_, idx = NA_integer_))
    }
    
    list(
      score = as.integer(lines[num_idx]),
      idx = num_idx
    )
  }
  
  home_res <- get_next_score(header_lines, home_team, 1L)
  away_res <- get_next_score(
    header_lines,
    away_team,
    ifelse(is.na(home_res$idx), 1L, home_res$idx + 1L)
  )
  
  tibble::tibble(
    home_score = home_res$score,
    away_score = away_res$score,
    expected_goals = home_res$score + away_res$score
  )
}

get_match_links <- function(season_url, base_url = "https://lff.lv") {
  doc <- read_html_safe(season_url)
  if (is.null(doc)) stop("Neizdevās nolasīt sezonas lapu.")
  
  hrefs <- xml2::xml_find_all(doc, ".//a") |>
    xml2::xml_attr("href")
  
  hrefs <- hrefs[!is.na(hrefs)]
  game_links <- hrefs[stringr::str_detect(hrefs, "^/speles/")]
  unique(paste0(base_url, game_links))
}

parse_goal_event <- function(timeline, start_i, current_minute, match_url) {
  goal_line <- timeline[start_i]
  score <- stringr::str_extract(goal_line, "\\b\\d+\\s*:\\s*\\d+\\b")
  
  detail_lines <- character(0)
  j <- start_i + 1L
  
  while (j <= length(timeline)) {
    x <- timeline[j]
    
    if (is_minute(x)) break
    if (is_goal_line(x)) break
    if (is_missed_penalty_line(x)) break
    if (x %in% c("Spēle beigusies!", "Beidzies pirmais puslaiks")) break
    
    detail_lines <- c(detail_lines, x)
    j <- j + 1L
  }
  
  scorer <- NA_character_
  assister <- NA_character_
  
  if (is_regular_goal_line(goal_line)) {
    scorer_line_idx <- which(stringr::str_detect(detail_lines, "^Vārtus guva\\s+"))[1]
    
    if (!is.na(scorer_line_idx)) {
      scorer_line <- detail_lines[scorer_line_idx]
      
      scorer <- stringr::str_match(
        scorer_line,
        "^Vārtus guva\\s+(.+?)(?=\\s+Rezultatīvi piespēlēja|$)"
      )[, 2]
      
      assister_inline <- stringr::str_match(
        scorer_line,
        "Rezultatīvi piespēlēja\\s+(.+?)$"
      )[, 2]
      
      scorer <- ifelse(is.na(scorer), NA_character_, stringr::str_squish(scorer))
      assister_inline <- ifelse(is.na(assister_inline), NA_character_, stringr::str_squish(assister_inline))
      
      if (!is.na(assister_inline) && assister_inline != "") {
        assister <- assister_inline
      } else {
        following_lines <- if (scorer_line_idx < length(detail_lines)) {
          detail_lines[(scorer_line_idx + 1):length(detail_lines)]
        } else {
          character(0)
        }
        
        if (length(following_lines) > 0) {
          assist_line_idx <- which(stringr::str_detect(following_lines, "^Rezultatīvi piespēlēja\\s+"))[1]
          if (!is.na(assist_line_idx)) {
            assister <- stringr::str_replace(
              following_lines[assist_line_idx],
              "^Rezultatīvi piespēlēja\\s+",
              ""
            )
            assister <- stringr::str_squish(assister)
          }
        }
      }
    }
  }
  
  if (is_penalty_goal_line(goal_line)) {
    scorer <- stringr::str_match(
      goal_line,
      stringr::regex("^Soda sitiens!\\s+\\d+\\s*:\\s*\\d+\\s+(.+?)$", ignore_case = TRUE)
    )[, 2]
    
    scorer <- ifelse(is.na(scorer), NA_character_, stringr::str_squish(scorer))
    assister <- NA_character_
    
    if ((is.na(scorer) || scorer == "") && length(detail_lines) > 0) {
      scorer_line_idx <- which(stringr::str_detect(detail_lines, "^Vārtus guva\\s+"))[1]
      
      if (!is.na(scorer_line_idx)) {
        scorer_line <- detail_lines[scorer_line_idx]
        
        scorer <- stringr::str_match(
          scorer_line,
          "^Vārtus guva\\s+(.+?)(?=\\s+Rezultatīvi piespēlēja|$)"
        )[, 2]
        
        scorer <- ifelse(is.na(scorer), NA_character_, stringr::str_squish(scorer))
      }
    }
  }
  
  tibble::tibble(
    match_url = match_url,
    event_index = start_i,
    minute = current_minute,
    score = score,
    scorer = scorer,
    assister = assister
  )
}

parse_match_page <- function(match_url) {
  doc <- read_html_safe(match_url)
  if (is.null(doc)) return(NULL)
  
  all_lines <- xml2::xml_find_all(doc, ".//*") |>
    xml2::xml_text() |>
    clean_text_vec()
  
  match_title <- extract_match_title(doc)
  teams <- extract_teams(doc, match_title)
  match_datetime <- extract_match_datetime(all_lines)
  
  score_info <- extract_header_score(all_lines, teams$home_team, teams$away_team)
  
  meta <- tibble::tibble(
    match_url = match_url,
    match_title = match_title,
    home_team = teams$home_team,
    away_team = teams$away_team,
    match_datetime = match_datetime,
    home_score = score_info$home_score,
    away_score = score_info$away_score,
    expected_goals = score_info$expected_goals
  )
  
  timeline <- extract_timeline_lines(all_lines)
  
  if (length(timeline) == 0) {
    return(list(meta = meta, events = NULL))
  }
  
  out <- list()
  row_id <- 0L
  current_minute <- NA_character_
  
  i <- 1L
  while (i <= length(timeline)) {
    line <- timeline[i]
    
    if (is_minute(line)) {
      current_minute <- line
      i <- i + 1L
      next
    }
    
    if (is_goal_line(line)) {
      parsed <- parse_goal_event(
        timeline = timeline,
        start_i = i,
        current_minute = current_minute,
        match_url = match_url
      )
      
      if (!is.na(parsed$scorer) && parsed$scorer != "") {
        row_id <- row_id + 1L
        out[[row_id]] <- tibble::tibble(
          match_url = match_url,
          match_title = match_title,
          home_team = teams$home_team,
          away_team = teams$away_team,
          match_datetime = match_datetime,
          event_index = parsed$event_index,
          minute = parsed$minute,
          score = parsed$score,
          scorer = parsed$scorer,
          assister = parsed$assister
        )
      }
    }
    
    i <- i + 1L
  }
  
  events <- if (length(out) == 0) {
    NULL
  } else {
    dplyr::bind_rows(out) |>
      dplyr::mutate(
        scorer = stringr::str_squish(scorer),
        assister = dplyr::if_else(is.na(assister), NA_character_, stringr::str_squish(assister))
      ) |>
      dplyr::distinct(match_url, event_index, .keep_all = TRUE)
  }
  
  list(meta = meta, events = events)
}

make_interactive_table <- function(df, title = NULL, table_id = "tbl1") {
  if (nrow(df) == 0) {
    return(htmltools::tagList(
      if (!is.null(title)) htmltools::tags$h2(title),
      htmltools::tags$p("Nav datu.")
    ))
  }
  
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  df[is.na(df)] <- ""
  
  header_cells <- lapply(seq_along(names(df)), function(i) {
    nm <- names(df)[i]
    htmltools::tags$th(
      nm,
      onclick = sprintf("sortTable('%s', %d)", table_id, i - 1),
      title = "Klikšķini, lai kārtotu",
      style = "border:1px solid #ccc; padding:8px; text-align:left; background:#f3f3f3; cursor:pointer;"
    )
  })
  
  row_tags <- lapply(seq_len(nrow(df)), function(i) {
    row_list <- as.list(df[i, , drop = FALSE])
    
    htmltools::tags$tr(
      lapply(row_list, function(cell) {
        cell_chr <- as.character(cell)
        
        content <- if (length(cell_chr) == 1 && grepl("^https?://", cell_chr)) {
          htmltools::tags$a(href = cell_chr, target = "_blank", cell_chr)
        } else {
          cell_chr
        }
        
        htmltools::tags$td(
          content,
          style = "border:1px solid #ccc; padding:8px; vertical-align:top;"
        )
      })
    )
  })
  
  htmltools::tagList(
    if (!is.null(title)) htmltools::tags$h2(title),
    htmltools::tags$input(
      id = paste0(table_id, "_filter"),
      type = "text",
      placeholder = "Filtrēt tabulu...",
      onkeyup = sprintf("filterTable('%s_filter','%s')", table_id, table_id),
      style = "margin-bottom:10px; padding:8px; width:320px; max-width:100%;"
    ),
    htmltools::tags$table(
      id = table_id,
      class = "report-table",
      style = "border-collapse:collapse; width:100%; margin-bottom:24px;",
      htmltools::tags$thead(htmltools::tags$tr(header_cells)),
      htmltools::tags$tbody(row_tags)
    )
  )
}

# ---------- Savāc datus ----------
game_links <- get_match_links(season_url_games, base_url)

if (length(game_links) == 0) {
  stop("Netika atrastas spēļu saites.")
}

cat("Atrastas spēļu saites:", length(game_links), "\n")

parsed_pages <- lapply(game_links, parse_match_page)

match_meta <- dplyr::bind_rows(lapply(parsed_pages, function(x) x$meta))
events_df  <- dplyr::bind_rows(lapply(parsed_pages, function(x) x$events))

if (nrow(events_df) == 0) {
  warning("Netika atrasti vārtu notikumi.")
  events_df <- tibble::tibble(
    match_url = character(),
    match_title = character(),
    home_team = character(),
    away_team = character(),
    match_datetime = character(),
    event_index = integer(),
    minute = character(),
    score = character(),
    scorer = character(),
    assister = character()
  )
}

events_df <- events_df |>
  dplyr::mutate(
    scorer = stringr::str_squish(scorer),
    assister = dplyr::if_else(is.na(assister), NA_character_, stringr::str_squish(assister))
  ) |>
  dplyr::distinct(match_url, event_index, .keep_all = TRUE)

# ---------- Nosaka komandu katram vārtu notikumam ----------
events_df <- events_df |>
  dplyr::mutate(
    home_goals = suppressWarnings(as.integer(stringr::str_extract(score, "^\\d+"))),
    away_goals = suppressWarnings(as.integer(stringr::str_extract(score, "(?<=:)\\d+$")))
  ) |>
  dplyr::group_by(match_url) |>
  dplyr::arrange(event_index, .by_group = TRUE) |>
  dplyr::mutate(
    prev_home_goals = dplyr::lag(home_goals, default = 0L),
    prev_away_goals = dplyr::lag(away_goals, default = 0L),
    goal_team = dplyr::case_when(
      !is.na(home_goals) & home_goals > prev_home_goals ~ home_team,
      !is.na(away_goals) & away_goals > prev_away_goals ~ away_team,
      TRUE ~ NA_character_
    )
  ) |>
  dplyr::ungroup()

# ---------- Kontrole pret oficiālo rezultātu ----------
parsed_match_counts <- events_df |>
  dplyr::count(match_url, name = "parsed_goals")

match_check <- match_meta |>
  dplyr::left_join(parsed_match_counts, by = "match_url") |>
  dplyr::mutate(
    parsed_goals = dplyr::coalesce(parsed_goals, 0L),
    missing_goals = expected_goals - parsed_goals
  ) |>
  dplyr::arrange(dplyr::desc(missing_goals), match_datetime, match_title)

official_total_goals <- sum(match_check$expected_goals, na.rm = TRUE)
parsed_total_goals   <- sum(match_check$parsed_goals, na.rm = TRUE)
missing_total_goals  <- official_total_goals - parsed_total_goals

# ---------- Kopsavilkumi ----------
goals_summary <- events_df |>
  dplyr::filter(!is.na(scorer), scorer != "") |>
  dplyr::count(scorer, name = "Vārtu_skaits", sort = TRUE) |>
  dplyr::rename(
    Spēlētājs = scorer
  )

assists_summary <- events_df |>
  dplyr::filter(!is.na(assister), assister != "") |>
  dplyr::count(assister, name = "Rezultatīvo_piespēļu_skaits", sort = TRUE) |>
  dplyr::rename(
    Spēlētājs = assister
  )

combined_summary <- dplyr::full_join(
  events_df |>
    dplyr::filter(!is.na(scorer), scorer != "") |>
    dplyr::count(scorer, name = "Vārti") |>
    dplyr::rename(Spēlētājs = scorer),
  events_df |>
    dplyr::filter(!is.na(assister), assister != "") |>
    dplyr::count(assister, name = "Piespēles") |>
    dplyr::rename(Spēlētājs = assister),
  by = "Spēlētājs"
) |>
  dplyr::mutate(
    Vārti = dplyr::coalesce(Vārti, 0L),
    Piespēles = dplyr::coalesce(Piespēles, 0L),
    Kopā = Vārti + Piespēles
  ) |>
  dplyr::arrange(
    dplyr::desc(Kopā),
    dplyr::desc(Vārti),
    dplyr::desc(Piespēles),
    Spēlētājs
  )

match_check_df <- match_check |>
  dplyr::filter(!is.na(expected_goals), missing_goals != 0) |>
  dplyr::select(
    match_title, home_team, away_team, match_datetime,
    home_score, away_score, expected_goals, parsed_goals, missing_goals, match_url
  ) |>
  dplyr::rename(
    Spēle = match_title,
    Mājinieki = home_team,
    Viesi = away_team,
    Datums_laiks = match_datetime,
    Mājinieku_vārti = home_score,
    Viesu_vārti = away_score,
    Oficiālie_vārti = expected_goals,
    Parsētie_vārti = parsed_goals,
    Trūkst = missing_goals,
    Saite = match_url
  )

detail_df <- events_df |>
  dplyr::select(
    match_title, home_team, away_team,
    match_datetime, minute, score,
    scorer, assister, match_url
  ) |>
  dplyr::rename(
    Spēle = match_title,
    Mājinieki = home_team,
    Viesi = away_team,
    Datums_laiks = match_datetime,
    Minūte = minute,
    Rezultāts = score,
    Vārtus_guva = scorer,
    Rezultatīvi_piespēlēja = assister,
    Saite = match_url
  )

# ---------- HTML saturs ----------
html_page <- htmltools::tagList(
  htmltools::tags$head(
    htmltools::tags$meta(charset = "utf-8"),
    htmltools::tags$title("LVBET līga 2026 - vārti un rezultatīvās piespēles"),
    htmltools::tags$style(htmltools::HTML("
      body { font-family: Arial, sans-serif; margin: 24px; line-height: 1.4; }
      table.report-table tbody tr:nth-child(even) { background: #fafafa; }
      table.report-table tbody tr:hover { background: #f2f8ff; }
      th::after { content: ' ↕'; color: #888; font-size: 0.9em; }
    ")),
    htmltools::tags$script(htmltools::HTML("
      function parseSortValue(text) {
        const t = text.trim();

        const minuteMatch = t.match(/^(\\d+)(?:\\+(\\d+))?[\\'’]?$/);
        if (minuteMatch) {
          const main = parseInt(minuteMatch[1], 10);
          const extra = minuteMatch[2] ? parseInt(minuteMatch[2], 10) : 0;
          return main * 1000 + extra;
        }

        const numText = t.replace(/\\s/g, '').replace(',', '.');
        if (/^-?\\d+(\\.\\d+)?$/.test(numText)) {
          return parseFloat(numText);
        }

        return t.toLowerCase();
      }

      function sortTable(tableId, colIndex) {
        const table = document.getElementById(tableId);
        const tbody = table.tBodies[0];
        const rows = Array.from(tbody.rows);
        const th = table.tHead.rows[0].cells[colIndex];
        const currentDir = th.getAttribute('data-sort-dir') || 'none';
        const newDir = currentDir === 'asc' ? 'desc' : 'asc';

        Array.from(table.tHead.rows[0].cells).forEach(cell => cell.setAttribute('data-sort-dir', 'none'));
        th.setAttribute('data-sort-dir', newDir);

        rows.sort((a, b) => {
          const aText = a.cells[colIndex].innerText || a.cells[colIndex].textContent;
          const bText = b.cells[colIndex].innerText || b.cells[colIndex].textContent;

          const av = parseSortValue(aText);
          const bv = parseSortValue(bText);

          if (typeof av === 'number' && typeof bv === 'number') {
            return newDir === 'asc' ? av - bv : bv - av;
          }

          if (av < bv) return newDir === 'asc' ? -1 : 1;
          if (av > bv) return newDir === 'asc' ? 1 : -1;
          return 0;
        });

        rows.forEach(row => tbody.appendChild(row));
      }

      function filterTable(inputId, tableId) {
        const input = document.getElementById(inputId);
        const filter = input.value.toLowerCase();
        const table = document.getElementById(tableId);
        const rows = table.tBodies[0].rows;

        for (let i = 0; i < rows.length; i++) {
          const txt = rows[i].innerText.toLowerCase();
          rows[i].style.display = txt.indexOf(filter) > -1 ? '' : 'none';
        }
      }
    "))
  ),
  htmltools::tags$body(
    htmltools::tags$h1("LVBET līga 2026 - vārtu guvēji un rezultatīvās piespēles"),
    htmltools::tags$p(paste("Ģenerēts:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))),
    htmltools::tags$p(paste("Spēļu saišu skaits:", length(game_links))),
    htmltools::tags$p(paste("Oficiāli vārti sezonā:", official_total_goals)),
    htmltools::tags$p(paste("Parsētie vārtu notikumi:", parsed_total_goals)),
    htmltools::tags$p(paste("Trūkstošie vārti:", missing_total_goals)),
    
    make_interactive_table(goals_summary, "Vārtu guvēju kopsavilkums", "goals_tbl"),
    make_interactive_table(assists_summary, "Rezultatīvo piespēļu kopsavilkums", "assists_tbl"),
    make_interactive_table(combined_summary, "Vārti + rezultatīvās piespēles kopsavilkums", "combined_tbl"),
    make_interactive_table(match_check_df, "Spēles ar neatbilstībām starp oficiālo rezultātu un parseri", "check_tbl"),
    make_interactive_table(detail_df, "Detalizētie vārtu notikumi", "detail_tbl")
  )
)

htmltools::save_html(html_page, file = output_file)

cat("HTML fails saglabāts:\n", normalizePath(output_file, mustWork = FALSE), "\n")
