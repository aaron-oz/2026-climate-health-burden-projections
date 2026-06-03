# Handoff: Is our IHME-forecast-unscaling plan methodologically valid?

## Mission

Assess whether the team's plan to derive a temperature-free counterfactual
mortality forecast — by "unscaling" IHME's published GBD 2021 cause-specific
forecasts — is methodologically sound, given how IHME's forecasting pipeline
actually applies temperature.

The team met with IHME on 2026-05-02. IHME agreed to share their GBD 2021
reference forecasts (draw × age × sex × location, out to 2050) for the 17
Burkart-temperature-sensitive causes plus all-cause mortality. They did NOT
offer a temperature-free counterfactual.

When the team pitched the plan to unscale IHME's forecasts (divide by the
temperature scalar to recover a temperature-free baseline, then re-apply our
PAFs computed under SSP scenarios), IHME pushed back with two specific
concerns:

1. The temperature scalar adjustment to the 12 (or 17) Burkart causes
   happens **early** in their forecasting pipeline.
2. After the early scalar, several downstream steps cause that temperature
   signal to **propagate to other causes and ages**:
   a. A cohort-component model that "ensures people can't live too long" —
      i.e., forces redistribution of saved deaths into other causes at
      later ages. (Demographic plausibility constraint.)
   b. Cause-of-death alignment that squeezes cause-specific forecasts to
      sum to the all-cause forecast.

The implication IHME raised: simple division by the temperature scalar
won't recover a clean temperature-free baseline, because the temperature
signal has already diffused into other causes and ages by the time the
forecasts are published.

## Your job

Determine:

1. **What exactly IHME's forecasting pipeline does.** Where does the
   temperature scalar enter? What downstream steps modify mortality after
   that point, and how do they propagate temperature signal? Cite the
   methodology paper / appendix sections that describe each step.

2. **Whether the unscaling plan is salvageable as proposed.** If IHME's
   concerns are correct (temperature signal leaks into other causes/ages
   after the initial scalar), can we still recover a usable
   counterfactual? Under what assumptions? What's the magnitude of error
   we'd introduce by unscaling naively?

3. **What alternative approaches exist.** If the unscaling plan is not
   valid, propose alternatives. Examples to consider (not exhaustive):
   - Use IHME's reference forecasts as-is and accept their temperature
     methodology rather than substituting Burkart's
   - Request a custom counterfactual run from IHME (what would we ask
     for specifically?)
   - Apply our PAFs only to the marginal change between scenarios, not to
     levels
   - Re-run the cohort propagation ourselves with a temperature-free
     baseline cause-specific input

4. **A clear recommendation.** Given what the user has from IHME (reference
   forecasts only, no counterfactual), what's the most defensible path
   forward? Be opinionated; flag the assumptions any approach requires.

## Background context

- **Working directory:** `/var/home/aoz/Dropbox/OZ-Labs/WorldBank/2026-climate-health-burden-projections/`
- **The IHME methodology you need to read:**
  `gbd/gbd-2021-forecast-appendix.pdf` — the supplementary appendix of the
  GBD 2021 cause-specific mortality forecasting paper (Vollset et al.,
  Lancet 2024 or similar). Walk through it carefully — the order of
  operations matters for this question.
- **The team's prep doc and the climate-removal concept note:**
  `gbd/ihme-plan-b-prep.pdf` (and corresponding `.tex` source) and
  `gbd/charlie-proposed-climate-removal-concept-note.pdf`. These describe
  the "Plan B" to use IHME forecasts and the proposed climate-removal
  approach. Read these — they're directly on point.
- **Burkart et al. 2021 reference materials** (in case useful):
  `from-samuel/Info Burkart/burkart-ihme-meth-paper.pdf` and
  `from-samuel/Info Burkart/2021_GBDtemp_anexos.pdf`. These describe the
  temperature attribution methodology our pipeline implements. Worth
  comparing against IHME's temperature-scalar approach to know whether
  they're using the same ERFs and TMRELs as we are.
- **Project-wide context:** `CLAUDE.md` describes the broader project —
  quantifying mortality attributable to non-optimal temperature, validating
  with Colombia (2010-2019) before scaling globally and projecting to 2100
  under SSP scenarios.

## Constraints on your work

- **READ-ONLY for code.** Do not modify any pipeline scripts.
- **Web research is encouraged** — the IHME methods may require reading
  external papers, supplementary materials, or methods documentation
  beyond what's local. Use `WebFetch` / `WebSearch` freely.
- **Cite specific sections** of the IHME appendix when describing
  pipeline steps. The user wants to be able to verify claims, not take
  them on faith.
- **Do not assume.** If IHME's appendix is silent on a specific step, say
  so explicitly. If you have to infer, label inferences as such.
- **The user's bar is high.** Real $$ decisions will be made on the basis
  of these forecasts. Your assessment needs to be substantive, not a
  hedge-everything punt. Be willing to recommend "this plan won't work,
  use approach Y instead" if that's what the evidence supports.

## Deliverable

Write a single markdown report. Suggested structure:

1. **Executive summary** (3-5 bullets): is the unscaling plan valid? If
   not, what's the recommended alternative? What assumptions are required?
2. **How IHME's forecasting pipeline applies temperature.** Step by step,
   with citations to the appendix. Be specific about: where in the order
   the scalar is applied, what variables get scaled, which causes are
   affected, what downstream steps modify mortality.
3. **Concrete answer on unscaling validity.** Math-level if possible:
   what does "divide by the temperature scalar" recover, and what does
   it miss? Quantify the residual error if you can.
4. **Alternatives, ranked.** Each with: what it requires (data, IHME
   cooperation, our pipeline changes), what it gives us, what its
   assumptions are.
5. **Recommendation.** One concrete path forward.
6. **Open questions** for follow-up with IHME (what to ask in the next
   meeting).

Length: as much as needed to be substantive, but no longer. Prefer
specificity (file:section, equations, numbers) over hedge-prose.

## Why this matters

The team's project is producing temperature-attributable mortality
projections to 2100 for the World Bank. Wrong methodology means wrong
numbers, and these numbers may inform real spending decisions. The
unscaling plan was developed before the IHME meeting; IHME's pushback
suggests it may not work as imagined. The team needs a clear answer
before committing further engineering effort to the global pipeline.

Take the time you need.
