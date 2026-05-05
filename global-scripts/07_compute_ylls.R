# 07_compute_ylls.R — Compute Years of Life Lost (YLLs) from attributable deaths
#
# Multiplies attributable deaths by remaining life expectancy at age of death
# to convert from death counts to YLLs (also called AVPP in Spanish).
#
# Input:  RESULTS_DIR/burden_{LOCATION_ID}.rds
#         LIFETABLE_DIR/{LOCATION_ID}_lifetable.csv (or .rds)
#         INTERMEDIATE_DIR/mortality.rds (for age/sex detail)
# Output: RESULTS_DIR/ylls_{LOCATION_ID}.rds

source("config.R")

library(data.table)

log_msg("Computing YLLs for location", LOCATION_ID)

# --- Load attributable burden and mortality ---
burden <- readRDS(file.path(RESULTS_DIR, paste0("burden_", LOCATION_ID, ".rds")))
mort   <- readRDS(file.path(INTERMEDIATE_DIR, "mortality.rds"))
pafs   <- readRDS(file.path(RESULTS_DIR, paste0("pafs_", LOCATION_ID, ".rds")))
setDT(burden); setDT(mort); setDT(pafs)

# --- Load life tables ---
lt_file_rds <- file.path(LIFETABLE_DIR, paste0(LOCATION_ID, "_lifetable.rds"))
lt_file_csv <- file.path(LIFETABLE_DIR, paste0(LOCATION_ID, "_lifetable.csv"))

if (file.exists(lt_file_rds)) {
  lt <- readRDS(lt_file_rds)
  setDT(lt)
} else if (file.exists(lt_file_csv)) {
  lt <- fread(lt_file_csv)
} else {
  warning(paste("No life table found for location", LOCATION_ID,
                "- skipping YLL calculation"))
  quit(save = "no")
}

log_msg("Life table loaded:", nrow(lt), "rows")

# Expected columns: year_id, age_group_id (or age), sex_id, ex (life expectancy)
# Standardize if needed
if ("ex" %in% names(lt)) {
  # Already has life expectancy column
} else if ("ev" %in% names(lt)) {
  setnames(lt, "ev", "ex")
} else {
  stop("Life table must have an 'ex' or 'ev' column for life expectancy")
}

# --- Compute age/sex-specific attributable deaths ---
# Merge PAFs with detailed mortality (age/sex/cause/year)
mort_detail <- merge(mort, pafs,
                     by.x = c("year_id", "acause"),
                     by.y = c("year", "acause"),
                     all.x = TRUE)
mort_detail[is.na(paf_heat), paf_heat := 0]
mort_detail[is.na(paf_cold), paf_cold := 0]
mort_detail[is.na(paf_nonopt), paf_nonopt := 0]

mort_detail[, `:=`(deaths_heat   = deaths * paf_heat,
                   deaths_cold   = deaths * paf_cold,
                   deaths_nonopt = deaths * paf_nonopt)]

if (COLOMBIA_VERIFICATION) {
  # Replicate Samuel (11_carga_atribuible.R:547-554): apply SEV multiplier to
  # attributable deaths in the YLL pathway too. Without this, our YLLs use raw
  # deaths*PAF while Samuel's use deaths*PAF*SEV. Gated on verification mode
  # because Burkart does not use the SEV multiplier (see step2-comparison.md
  # issue #2).
  sev_file <- file.path(RESULTS_DIR, paste0("sevs_", LOCATION_ID, ".rds"))
  if (file.exists(sev_file)) {
    sevs <- setDT(readRDS(sev_file))
    mort_detail <- merge(mort_detail, sevs[, .(year_id = year, acause, sev)],
                         by = c("year_id", "acause"), all.x = TRUE)
    mort_detail[is.na(sev), sev := 0]
    mort_detail[, `:=`(deaths_heat   = deaths_heat   * sev,
                       deaths_cold   = deaths_cold   * sev,
                       deaths_nonopt = deaths_nonopt * sev)]
    log_msg("COLOMBIA_VERIFICATION: applied SEV multiplier to attributable deaths in YLL pathway")
  } else {
    warning("COLOMBIA_VERIFICATION: SEV file not found for YLL multiplier")
  }
}

# --- Merge with life tables ---
if (COLOMBIA_VERIFICATION) {
  # Item 33: Samuel filters age == 0 out of the life table before joining,
  # so the "0-4" group (gru_ed1 == "0-4") gets ex(age=1) rather than ex(age=0).
  # Replicate by remapping mortality age_group_id 0 -> 1 before the merge.
  mort_detail[age_group_id == 0L, age_group_id := 1L]
  log_msg("COLOMBIA_VERIFICATION: remapped age_group_id 0 -> 1 for the 0-4 group")
}

# Try to match on available common columns
merge_cols <- intersect(names(mort_detail), names(lt))
merge_cols <- merge_cols[merge_cols %in% c("year_id", "age_group_id", "sex_id", "age")]

if (length(merge_cols) == 0) {
  warning("Cannot match life table to mortality data — no common age/sex columns. Skipping YLL.")
  quit(save = "no")
}

ylls <- merge(mort_detail, lt, by = merge_cols, all.x = TRUE)

if (sum(!is.na(ylls$ex)) == 0) {
  warning("Life expectancy values could not be matched. Check column formats.")
  quit(save = "no")
}

# --- Compute YLLs = attributable deaths × remaining life expectancy ---
if (COLOMBIA_VERIFICATION) {
  # Replicate Samuel (11_carga_atribuible.R:570-577): subtract a 2.5-year mid-bin
  # correction from life expectancy for ages <80, and use a fixed 10 years for
  # the >80 group (since DANE life tables don't extend cleanly past 80).
  # In our converter, age_group_id encodes the lower bound of the 5-yr bin;
  # age_group_id == 80 corresponds to Samuel's ">80" category.
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

# --- Aggregate ---
yll_summary <- ylls[, .(deaths_heat = sum(deaths_heat, na.rm = TRUE),
                        deaths_cold = sum(deaths_cold, na.rm = TRUE),
                        deaths_nonopt = sum(deaths_nonopt, na.rm = TRUE),
                        yll_heat = sum(yll_heat, na.rm = TRUE),
                        yll_cold = sum(yll_cold, na.rm = TRUE),
                        yll_nonopt = sum(yll_nonopt, na.rm = TRUE)),
                    by = .(year_id, acause)]

yll_summary[, location_id := LOCATION_ID]
saveRDS(yll_summary, file.path(RESULTS_DIR, paste0("ylls_", LOCATION_ID, ".rds")))
log_msg("YLLs saved to", file.path(RESULTS_DIR, paste0("ylls_", LOCATION_ID, ".rds")))

# Also save the detailed (age/sex) version
ylls[, location_id := LOCATION_ID]
saveRDS(ylls, file.path(RESULTS_DIR, paste0("ylls_detail_", LOCATION_ID, ".rds")))

# =============================================================================
# Diagnostic plots (if enabled)
# =============================================================================

if (RUN_DIAGNOSTICS) {
  library(ggplot2)
  log_msg("Generating YLL diagnostic plots")

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
