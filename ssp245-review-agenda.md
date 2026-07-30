# SSP2-4.5 review agenda

For the first review of the global projection run with Samuel. Written
2026-07-30, while the ssp245 pass was finishing on Caspar's machine.

Terms used below. PAF: population attributable fraction, the share of deaths in
a stratum attributable to non-optimal temperature. YLL: years of life lost.
TMREL: theoretical minimum risk exposure level, the death-weighted optimal
temperature for a location. ERF: exposure-response function, the relative-risk
curve relating daily temperature to mortality for one cause. SSP: shared
socioeconomic pathway; ssp245 is the intermediate emissions scenario.

The run covers 204 locations, 27 climate models, and years 2022 to 2050, so
159,732 (location, model, year) combinations. Each one writes the full
(cause x age x sex x draw) grid, which is why nothing below asks anyone to open
the run output directly.

---

## 1. Get the review tables (Caspar runs this, on his machine)

```
cd /data/HEAT/Burkart/FHS/2026-climate-health-burden-projections
git pull
Rscript global-scripts/util_summarize_run.R --scenarios=ssp245 --jobs=16
```

This reads every combo once and writes four small files under `output/summary/`:

| file | what it holds |
|---|---|
| `national_by_year.csv` | (location, model, year) with draw mean and 95% interval for attributable deaths and YLLs |
| `by_cause.csv` | the same, split by the 17 causes, draw means only |
| `coverage.csv` | years found and missing per (location, model), and whether YLLs exist |
| `qa_report.txt` | the arithmetic checks over every combo |

Those four files are what we review. They should total a few MB and can be
mailed. The pass is restartable; it caches per location.

Cost: the read-and-summarize step measured 0.19 s per combo on a warm cache
locally, so roughly 8 core-hours for the full ssp245, meaning under an hour on
16 workers if the storage keeps up. The binding constraint is likely disk
throughput rather than CPU, since the pass has to read every output file once.

## 2. Gate checks, before looking at any science

These either pass or the review is premature. All are in `qa_report.txt`.

1. **The two additive identities.** `paf_nonopt == paf_heat + paf_cold` and
   `deaths_nonopt == deaths_heat + deaths_cold`. A failure is a bug, not a
   modelling choice.
2. **`|paf_nonopt| <= 1`.** Attributable deaths cannot exceed total deaths.
3. **All values finite.**
4. **All-zero burden combos.** The report counts combos whose attributable
   burden is entirely zero and lists them by location. Before 2026-06-15 a
   merge on mismatched subnational keys produced exactly this signature, with
   the run still exiting cleanly. That was fixed and validated, so the expected
   count is zero. Anything above zero needs explaining before we read the
   numbers.
5. **Coverage.** Every location should show 27 models, and every
   (location, model) should show 29 years. Two models publish no ssp245
   (taiesm1, hadgem3-gc31-mm), which is why the target is 27 of our 29 rather
   than 29. Missing years are listed per pair.
6. **YLL presence.** `coverage.csv` flags any (location, model) with no YLL
   output. Missing life tables used to abort silently; that was fixed, but this
   is the check that would catch a regression, and YLLs are half of what the
   project is asked to produce.

## 3. What to look at

In rough order of how much it would change our conclusions if it came out
wrong.

**2022 against a known answer.** 2022 is the first projection year and is close
enough to the present to be checkable. Two anchors:

- The Colombia national benchmark computed 2026-06-16 (location 125, IHME
  mortality, national resolution, 500 draws) gave attributable deaths of about
  1,721 with a 95% interval of roughly [978, 2,417]. That figure is from
  project notes and should be re-confirmed from its source file before being
  used as a reference. It was computed from observed temperature, whereas the
  ssp245 run uses each climate model's own 2022 field, so the right check is
  whether the 27-model spread brackets it, not whether any single model matches.
- Published GBD 2023 non-optimal temperature attributable deaths, for whichever
  locations and years overlap. We should pull these rather than quote them from
  memory.

If 2022 is right, the projection is a question about the climate inputs. If it
is wrong, nothing downstream is worth reading.

**Direction and shape of the trend.** Under warming we expect heat-attributable
burden to rise and cold-attributable burden to fall, with the net depending on
the location's starting climate. Worth checking that the sign of the net change
varies by latitude in the way it should, rather than being uniform.

**Across-model spread.** With 27 models per location, check whether any single
model is an outlier driving the ensemble, and whether the spread is comparable
across locations. A location where the model spread is far tighter or wider
than its neighbours is worth opening up.

**The locations the climate grid barely resolves.** Thirty of the 204 locations
contain at most one 0.25 degree grid cell centre, and 15 contain none. Those get
a nearest-cell fallback: the location is assigned the climate series of the
closest cell carrying data, so its entire climate signal is one grid cell.

The conversion step for these was tested directly against CCKP data pulled from
the public bucket on 2026-07-30, for two models chosen to differ as much as
possible in native resolution (access-cm2 and canesm5). All of them convert, in
about four seconds each, and the annual mean temperature they produce is within
about 1.5 degrees C of published values for every case checked except San Marino,
which comes out 3.4 degrees C warm because its one cell sits lower and further
from the Apennine ridge than the country does. So the fallback works, and these
locations are not empty or crashing.

What has not been checked is the variance. The PAF depends on the distribution of
daily temperature around the TMREL, not on the annual mean, and an island's cell
is a grid-box average heavily weighted by ocean, which physically damps the daily
spread relative to land. The mean being right does not make the spread right, and
the spread is what drives the burden. Worth looking at the 30 sub-grid locations
as a group against comparable larger neighbours before their numbers are used.

**YLLs per attributable death.** This ratio is roughly the remaining life
expectancy at the age at which the attributable deaths occur, so it should sit
in a plausible band and vary sensibly with the age structure of the location.
It is a cheap check that the life-table merge is doing what we think.

## 4. Assumptions that need an explicit decision

These are things the code currently does. None is obviously wrong; all of them
should be a decision we have made on purpose rather than one we inherited,
especially before this scales to 2100 and to four scenarios.

**TMREL is held at its 2020 value for every projection year.** The TMREL inputs
carry 1990, 2010 and 2020 only, and missing years are filled from the nearest
available year (`02_load_tmrel.R`), so 2022 through 2050 all use 2020. This
means no adaptation: the optimal temperature for a population does not move as
its climate warms. That is a defensible choice and a common one, but it is a
strong assumption over a 28-year horizon and a much stronger one to 2100. The
alternative, deriving TMREL annually from projected cause-specific mortality
and the relative-risk curves, already exists in the pipeline
(`util_derive_tmrel.R`). We should decide which one the headline results use,
and say so plainly in the writeup either way.

**Temperature zones are clamped to 6 to 28 degrees C.** A location's zone is
the rounded annual mean temperature, clamped to [6, 28]
(`config.R:254-255`, `03_load_temperature.R:99-100`), and daily temperatures
are then truncated to the modelled range within that zone. The clamp exists
because the ERF curves are only estimated over that range. Under ssp245 to 2050
it will bind for the hottest locations; under ssp585 to 2100 it would bind much
more widely, and every degree beyond the clamp is a degree of warming the
burden estimate cannot see. Worth quantifying how often it binds before we
publish anything at the hot end.

**Mortality comes from IHME national forecasts.** So the projected burden mixes
a climate signal with a mortality forecast, and a change in attributable deaths
over time is partly the forecast's demography and epidemiology rather than
temperature. For the review it is worth looking at PAFs as well as counts,
since the PAF isolates the temperature signal from the mortality trend.

## 5. Operational question to settle before the other three scenarios

The per-combo output carries all 500 draws at full (cause x age x sex)
granularity. Extrapolating from the one real burden file in the repo (17,000
rows in 457 kB, so about 27 bytes per row) to the production shape (17 causes x
17 age groups x 2 sexes x 500 draws, about 289,000 rows per combo) gives
roughly 8 MB per burden file, and 159,732 of them for ssp245 alone. That is on
the order of a terabyte for the deaths files and more again for the YLL files,
which carry four extra columns on the same rows.

This is an extrapolation from a single file and the compression of real data
varies, so it should be replaced with a fact before anyone plans around it:

```
du -sh output/results/cckp
df -h /data
```

If it is anywhere near that scale, the remaining three scenarios are roughly
four times the ssp245 volume (28 + 25 + 29 = 82 model-scenarios against 27), and
we should decide whether to keep the full draw grid on disk or have the burden
step write a compact per-combo summary at the point where the data is already in
memory. The second option would also make this review pass nearly free for the
remaining scenarios, instead of a full re-read of everything.
