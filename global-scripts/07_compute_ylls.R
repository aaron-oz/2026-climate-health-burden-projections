# 07_compute_ylls.R — Compute Years of Life Lost (YLLs) from attributable deaths
#
# Multiplies attributable deaths by remaining life expectancy at age of
# death. Burden is now consumed at full (year, subloc_id, age_group_id,
# sex_id, acause) granularity from 05_compute_pafs.R, so this script is
# a straightforward life-table merge + multiply.
#
# Life-table choice:
#   COLOMBIA_VERIFICATION = TRUE: use subloc-specific (department) life
#                                  tables, matching Samuel's methodology.
#   else:                         use national life tables only
#                                  (subloc_id == "00") and broadcast across
#                                  all subloc rows in the burden frame.
#
# Input:  RESULTS_DIR/burden_{LOCATION_ID}.rds
#         LIFETABLE_DIR/{LOCATION_ID}_lifetable.rds
# Output: RESULTS_DIR/ylls_{LOCATION_ID}.rds         — full granularity
#         RESULTS_DIR/ylls_detail_{LOCATION_ID}.rds  — same content, kept
#                                                       for downstream
#                                                       backwards compat

if (!exists("SCRIPTS_DIR")) SCRIPTS_DIR <- dirname(c(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)), ".")[1])
source(file.path(SCRIPTS_DIR, "config.R"))

library(data.table)

log_msg("Computing YLLs for location", LOCATION_ID)

# --- Load attributable burden ---
burden <- readRDS(file.path(RESULTS_DIR, paste0("burden_", LOCATION_ID, ".rds")))
setDT(burden)

# --- Load life tables ---
# Validation (COLOMBIA_VERIFICATION) uses Samuel's department-level life table,
# which lives with the other validation inputs; production uses the canonical
# UN WPP life tables in LIFETABLE_DIR. Separate directories avoid a same-path
# collision at the validation location (125), where both a 2010-2019
# subnational table and a 1990-2100 national table exist.
lt_dir <- if (COLOMBIA_VERIFICATION) {
  file.path(DATA_DIR, "columbia-data-for-verifying-pipeline", "colombia")
} else {
  LIFETABLE_DIR
}
lt_file_rds <- file.path(lt_dir, paste0(LOCATION_ID, "_lifetable.rds"))
lt_file_csv <- file.path(lt_dir, paste0(LOCATION_ID, "_lifetable.csv"))

if (file.exists(lt_file_rds)) {
  lt <- readRDS(lt_file_rds)
  setDT(lt)
} else if (file.exists(lt_file_csv)) {
  lt <- fread(lt_file_csv)
} else {
  stop("No life table found for location ", LOCATION_ID,
       " (looked for ", lt_file_rds, " / .csv). YLLs cannot be computed. ",
       "This is a hard error rather than a silent skip so the runner records ",
       "the combo as failed instead of 'ok' with missing YLLs.")
}

log_msg("Life table loaded:", nrow(lt), "rows")

# Standardize life-table life-expectancy column
if (!"ex" %in% names(lt)) {
  if ("ev" %in% names(lt)) {
    setnames(lt, "ev", "ex")
  } else {
    stop("Life table must have an 'ex' or 'ev' column for life expectancy")
  }
}

# --- Pick the right life-table slice ---
has_subloc <- "subloc_id" %in% names(lt)

if (has_subloc && COLOMBIA_VERIFICATION) {
  # Use department-specific life tables. Samuel does this in
  # 11_carga_atribuible.R:88-113 (filter cod_depto != 00, distinct).
  lt_use <- lt[subloc_id != "00"]
  merge_keys <- c("year_id", "subloc_id", "age_group_id", "sex_id")
  log_msg("COLOMBIA_VERIFICATION: using department-specific life tables (",
          uniqueN(lt_use$subloc_id), " depts)")
} else if (has_subloc) {
  # Production: use national life table only, broadcast across subloc.
  lt_use <- lt[subloc_id == "00", .(year_id, age_group_id, sex_id, ex)]
  merge_keys <- c("year_id", "age_group_id", "sex_id")
  log_msg("Using national life table (subloc_id == '00') broadcast across subloc")
} else {
  # No subloc dimension on the life table — older format. Use as-is.
  lt_use <- lt
  merge_keys <- intersect(c("year_id", "age_group_id", "sex_id"), names(lt_use))
  log_msg("Life table has no subloc dimension; merging on ",
          paste(merge_keys, collapse = ","))
}

# Burden's year column is `year` (renamed from year_id in 05_compute_pafs.R).
# Restore to year_id for the merge so keys align with the life table.
setnames(burden, "year", "year_id")

# --- Apply Samuel's age 0 -> 1 remap for the "0-4" group in verification ---
if (COLOMBIA_VERIFICATION) {
  # Item 33: Samuel filters age == 0 out of the life table before joining,
  # so the "0-4" group gets ex(age=1) rather than ex(age=0). Replicate by
  # remapping mortality age_group_id 0 -> 1 before the merge.
  burden[age_group_id == 0L, age_group_id := 1L]
  log_msg("COLOMBIA_VERIFICATION: remapped age_group_id 0 -> 1 for the 0-4 group")
}

# --- Merge life table to burden ---
ylls <- merge(burden, lt_use, by = merge_keys, all.x = TRUE)

if (sum(!is.na(ylls$ex)) == 0) {
  stop("Life expectancy could not be matched for ANY burden row (loc ", LOCATION_ID,
       "). Most likely the life table does not cover the burden years (have ",
       paste(range(lt_use$year_id), collapse = "-"), "; need ",
       paste(range(burden$year_id), collapse = "-"),
       "). Supply a life table spanning the run years. Hard error so the ",
       "runner records this combo as failed rather than 'ok' with no YLLs.")
}

n_unmatched <- sum(is.na(ylls$ex))
if (n_unmatched > 0) {
  log_msg("Warning: ", n_unmatched, " burden rows have no life-table match (ex set to 0 for those)")
  ylls[is.na(ex), ex := 0]
}

# --- Compute YLLs = attributable deaths × remaining life expectancy ---
if (COLOMBIA_VERIFICATION) {
  # Replicate Samuel (11_carga_atribuible.R:570-577): subtract a 2.5-year
  # mid-bin correction from life expectancy for ages <80, and use a fixed
  # 10 years for the >80 group (since DANE life tables don't extend
  # cleanly past 80). After our remap above, the "0-4" group is encoded
  # as age_group_id=1, the ">80" group as age_group_id=80.
  ylls[, ex_adj := fifelse(age_group_id == 80L, 10, pmax(0, ex - 2.5))]
  ylls[, `:=`(yll_heat   = deaths_heat   * ex_adj,
              yll_cold   = deaths_cold   * ex_adj,
              yll_nonopt = deaths_nonopt * ex_adj)]
  ylls[, ex_adj := NULL]
  log_msg("COLOMBIA_VERIFICATION: applied (ex - 2.5) for age<80 and ex=10 for age>=80")
} else {
  ylls[, `:=`(yll_heat   = deaths_heat * ex,
              yll_cold   = deaths_cold * ex,
              yll_nonopt = deaths_nonopt * ex)]
}

# --- Save ---
ylls[, location_id := LOCATION_ID]
saveRDS(ylls, file.path(RESULTS_DIR, paste0("ylls_", LOCATION_ID, ".rds")))
log_msg("YLLs saved to ", file.path(RESULTS_DIR, paste0("ylls_", LOCATION_ID, ".rds")),
        " (", nrow(ylls), " rows at full granularity)")

# Backwards compat: util_compare_to_samuel.R looks for both ylls_<id>.rds
# and ylls_detail_<id>.rds. Save the same content under both names so
# downstream tooling that expects either path still works.
saveRDS(ylls, file.path(RESULTS_DIR, paste0("ylls_detail_", LOCATION_ID, ".rds")))

# =============================================================================
# Diagnostic plots (if enabled)
# =============================================================================

if (RUN_DIAGNOSTICS) {
  library(ggplot2)
  log_msg("Generating YLL diagnostic plots")

  yll_summary <- ylls[, .(yll_heat = sum(yll_heat, na.rm = TRUE),
                          yll_cold = sum(yll_cold, na.rm = TRUE)),
                      by = .(year_id, acause)]
  yll_long <- melt(yll_summary,
                   id.vars = c("year_id", "acause"),
                   measure.vars = c("yll_heat", "yll_cold"),
                   variable.name = "effect",
                   value.name = "ylls")
  yll_long[, effect := gsub("yll_", "", effect)]

  p <- ggplot(yll_long, aes(x = year_id, y = ylls, fill = effect)) +
    geom_col(position = "stack") +
    facet_wrap(~acause, scales = "free_y") +
    scale_fill_manual(values = c("heat" = "red", "cold" = "blue")) +
    labs(x = "Year", y = "Years of Life Lost", fill = "Effect",
         title = paste("Attributable YLLs — Location", LOCATION_ID)) +
    theme_minimal() +
    theme(legend.position = "bottom", strip.text = element_text(size = 7))

  ggsave(file.path(DIAGNOSTICS_DIR, paste0("ylls_loc", LOCATION_ID, ".png")),
         p, width = 14, height = 10, dpi = 150)

  log_msg("YLL diagnostic plot saved")
}
