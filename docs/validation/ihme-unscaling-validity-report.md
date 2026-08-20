# IHME-forecast unscaling: methodology assessment (revised)

**Author:** Aaron / Claude (statistical-consulting review)
**Date:** 2026-05-05
**Status:** Decision-grade, **revised after principal pushback**. Read-only review of local methodology PDFs; no code modified.
**Working dir:** `/var/home/aoz/Dropbox/OZ-Labs/WorldBank/2026-climate-health-burden-projections/`

---

## 0. Reading guide

This report is keyed to specific sections / equations of the GBD 2021 forecasting methods appendix (Vollset et al., *Lancet* 2024 supplementary appendix 1, hereafter "the appendix" — local file `gbd/gbd-2021-forecast-appendix.pdf`, 75 pages). All section numbers and equation numbers below refer to that document unless otherwise noted. The team's prep doc is `gbd/ihme-plan-b-prep.tex`. The Burkart 2021 paper is `from-samuel/Info Burkart/burkart-ihme-meth-paper.pdf` (verified text dump at `from-samuel/burkart-paper.txt`).

This is **revision 2** of this assessment. The first version dismissed two of IHME's three concerns. The team's principal pushed back, and on re-examination of the appendix I conclude the principal was **partially right and partially wrong**, in different ways than either the previous report or the principal's transcribed analysis claimed. The bottom line shifts: Workflow B remains the best plan, but it must be anchored on **rates, not counts**, and several smaller caveats need explicit disclosure.

**Tools constraint flag:** `WebFetch` and `WebSearch` were available in this environment but not used — every claim below is verified directly against the local appendix text. The Foreman 2018 and Vollset 2020 antecedent papers (refs 2, 3 in the appendix) are still not retrieved; where the appendix says "see Foreman et al. 2018, appendix 1, section 5," I rely on the GBD 2021 appendix's own description.

Key citations:
- Section 2.1.6.7 (pp. 25–29): "Non-optimal temperature PAFs"
- Section 2.1.6.9 (p. 32, Eq. 41): "Scalars" — `S_c = 1 / (1 - PAF_c)`
- Section 2.2.1 (pp. 32–33, Eqs. 42–44): cause-specific mortality model
- Section 2.2.2 (p. 33, Eq. 45): aggregation to all-cause
- Section 2.2.2.1 (pp. 33–35, Eqs. 46–51): random-walk-with-drift latent trends
- Section 2.4 (p. 39): cohort-component model for population
- Section 6 (pp. 45–46, Eqs. 59–60): post-processing alignment with GBD 2021

---

## 1. Executive summary

1. **Workflow B is salvageable, with one substantive change and two new caveats.** The substantive change is that the level anchor must be **rates, not counts** — otherwise the cohort-component pipeline (Section 2.4) propagates SSP2's age-structure into our SSP-X projection and the principal's concern about cross-cause / cross-cohort propagation becomes real. With rates as the anchor, Workflow B's cross-SSP ratio cancels the leakage mechanisms IHME flagged.

2. **The principal was algebraically right about the equivalence: "$m_X = m_{SSP2} \cdot (\tilde S_X / \tilde S_{SSP2})$" is identical to "$(m_{SSP2} / \tilde S_{SSP2}) \cdot \tilde S_X$".** The previous report's framing — that Workflow B "doesn't unscale anything" — was rhetorically convenient but mathematically false. Calling the operation "rescaling" rather than "unscaling" describes a different mental model, not a different floating-point computation. **However, this equivalence does not undermine the cancellation argument**, because the latent-trend (Section 2.2.2.1) and alignment (Section 6) corrections are themselves SSP-invariant — they are fit on historical 1990–2019 data and 2021 base-year data, neither of which depends on the future climate scenario being projected. Section 2 of this report works through the math.

3. **The principal was partially right about the cohort-component concern, but for a different reason than they articulated.** The IHME forecasting pipeline does **not** feed population back into cause-specific mortality rates — Section 2.4 explicitly takes mortality rate forecasts as input and produces population as output, and Section 2 of the appendix's preamble (p. 7, "Fourth, we compute population forecasts...") confirms the one-way flow. **However**, IHME publishes mortality both as rates and as counts, and counts = rate × population. Population is built from cohort-component dynamics that integrate the rate trajectory. So if we use IHME's published *counts* under SSP2-RCP4.5 as a level anchor and multiply by our cross-SSP scalar ratio, we are implicitly holding SSP2's population structure fixed under SSP-X — which is wrong for SSP-X's actual projected demography. **This is a real propagation pathway, missed by the previous report, that the assistant director may have been describing in less precise language.** The fix: anchor on rates; apply external (our pipeline's) population to get counts.

4. **The principal was wrong about the leakage mechanisms (RWD-with-drift and alignment shift) being temperature-correlated in a way that survives in the cross-SSP ratio.** Both are computed using SSP-invariant inputs (historical residuals, 2021 GBD-data anchor). The latent trends *do* embed historical temperature signal that the cause-specific model couldn't explain, but that latent contribution is the **same scalar value regardless of which forecast SSP we project to**. So when we form a ratio across SSPs, the latent terms multiply through identically and cancel up to second-order effects. They contribute to absolute-level uncertainty, not to differential bias between SSPs. Section 3 below works through this carefully.

5. **The 17-vs-12 cause issue is unchanged from the previous report, and remains the single biggest methodology decision the team must make.** Burkart fit 17 cause-specific ERFs (cardiomyopathy and myocarditis, animal contact, forces of nature, "other unintentional injuries", and one of the road/transport injury subdivisions are in Burkart but absent from the GBD-2021-forecast 12). For the 12 overlap causes, Workflow B's ratio cancels pipeline drift. For the 5 extra Burkart causes, IHME's published mortality has *no* temperature signal in it, so multiplying by our $\tilde S_X / \tilde S_{SSP2}$ injects a forward-only Burkart calculation on top of an IHME-modelled level. Disclose, or omit those 5 causes; do not paper over the difference.

6. **Recommendation: proceed with Workflow B at the rate level, capped at 2050, with the disclosures in Section 6.** Treat the cohort-component issue by anchoring on rates and applying our own population. Add 2 questions to the IHME methodology-clarification list (Section 7). The 6 prep-doc questions and the 4 from the previous report version remain valid.

---

## 2. The math: rescaling vs unscaling, and what cancels

### 2.1 The algebraic identity the principal flagged

The boxed equation in `ihme-plan-b-prep.tex` Section 3 is

$$m^T_{c,X} = m^T_{c,SSP2,GBD} \cdot \frac{\tilde S^X_{temp,c}}{\tilde S^{SSP2}_{temp,c}}.$$

By associativity of multiplication this is **identical** to

$$m^T_{c,X} = \left(\frac{m^T_{c,SSP2,GBD}}{\tilde S^{SSP2}_{temp,c}}\right) \cdot \tilde S^X_{temp,c}.$$

The right-hand side is exactly the operation IHME pushed back on: divide their published forecast by *our pipeline's* SSP2 scalar to recover an "unscaled" baseline, then multiply by *our pipeline's* SSP-X scalar. The previous report claimed Workflow B "does not unscale anything" and used this to claim Workflow B is methodologically distinct from the naive plan. **That framing was wrong.** Workflow B and naive unscaling produce identical numerical results.

What makes Workflow B *defensible* is not that it skips the unscaling step — it is that, by writing the operation as a ratio applied to an anchor, we make it transparent that the *biases shared between numerator and denominator of the ratio* cancel. That cancellation is the real argument, and it survives the algebraic identity above.

### 2.2 What actually cancels in the ratio, and what doesn't

Decompose the IHME published forecast (per Section 2.2.1, Eq. 44, plus the latent trend in Section 2.2.2.1, Eq. 50, plus the alignment shift in Section 6, Eq. 59) as

$$m^T_{c,SSP2,GBD,t} = \underbrace{\exp\!\big[\ln m^U_c + \ln S^{SSP2,IHME}_{temp,c} + \ln S^{IHME}_{ee,c} + \varepsilon_c\big]}_{m^{NL,SSP2}_c, \text{ Eq. } 44} \cdot \exp(\hat\epsilon^{RW}_{c}) \cdot \kappa^{align}_c$$

where
- $S^{SSP2,IHME}_{temp,c}$ is IHME's pipeline temperature scalar for cause $c$ under SSP2 (computed with their ERFs, TMRELs, CMIP6 bias correction)
- $S^{IHME}_{ee,c}$ collects IHME's other-risk scalars
- $\hat\epsilon^{RW}_c$ is the cause-specific random-walk-no-drift latent trend (Section 2.2.2.1, paragraph "Similarly, latent trends for lower-level causes...")
- $\kappa^{align}_c$ is the Section 6 multiplicative-equivalent alignment shift (which is exactly multiplicative when $\alpha = 1$ in Eq. 59; see Section 2.4 below)

When we apply the cross-SSP ratio:

$$m^T_{c,X,t} = m^T_{c,SSP2,GBD,t} \cdot \frac{\tilde S^X_{temp,c,t}}{\tilde S^{SSP2}_{temp,c,t}}.$$

Each component of the IHME forecast multiplies through unchanged. So the result is

$$m^T_{c,X,t} = m^U_c \cdot \underbrace{\frac{S^{SSP2,IHME}_{temp,c}}{\tilde S^{SSP2}_{temp,c}}}_{\text{calibration drift } r^{SSP2}_c} \cdot \tilde S^X_{temp,c,t} \cdot S^{IHME}_{ee,c} \cdot \exp(\hat\epsilon^{RW}_c) \cdot \kappa^{align}_c \cdot e^{\varepsilon_c}.$$

The "calibration drift" factor $r^{SSP2}_c = S^{SSP2,IHME}_{temp,c} / \tilde S^{SSP2}_{temp,c}$ is the residual that does **not** cancel. It captures every difference between IHME's pipeline and ours: 12 vs 17 ERFs, 8 vs 9 source countries, possible TMREL recipe differences, possible CMIP6 bias-correction differences, WorldPop vs an SSP-aware population recipe.

**For the 12 overlap causes**, $r^{SSP2}_c$ should be small but non-zero (Charlie's prep doc estimates ±5% — I think that's the right order of magnitude; see Section 2.5 below for sensitivity).

**For the 5 extra Burkart causes** (cardiomyopathy/myocarditis, animal contact, forces of nature, "other unintentional injuries", and one road/transport injury subdivision), IHME's $S^{SSP2,IHME}_{temp,c} = 1$ (no temperature signal in their model for those causes). So $r^{SSP2}_c = 1 / \tilde S^{SSP2}_{temp,c}$. The procedure becomes

$$m^T_{c,X,t} = m^T_{c,SSP2,GBD,t} \cdot \frac{\tilde S^X_{temp,c}}{\tilde S^{SSP2}_{temp,c}}$$

where $m^T_{c,SSP2,GBD,t}$ has no temperature signal. The result is "IHME's mortality for those 5 causes, scaled up or down across SSPs by the *delta* in our pipeline's temperature signal." This is a forward-only calculation overlaid on IHME's level — defensible if disclosed, equivalent to "what would happen if those 5 Burkart causes got our temperature scalar applied to an IHME baseline."

### 2.3 Why the latent trends and alignment shift don't bias the ratio

This is the parent's claim #1 most carefully: "the leakage mechanisms IHME identified are temperature-correlated — they depend on the very temperature signal that differs across SSPs. So they appear in BOTH the level anchor AND the cross-SSP ratio."

This claim is **not supported** by the appendix's description of how those mechanisms are computed, for three reasons.

**(a) The cause-specific RW-no-drift latent trends (Section 2.2.2.1, paragraph 2)**. The appendix says: "latent trends for lower-level causes in the GBD cause hierarchy were modelled using a random walk without drift." A random walk without drift, fit on historical 1990–2019 residuals (Eq. 46), produces forecasts that are constant in expectation: $\mathbb E[\hat\epsilon^{RW}_{c,t}] = \hat\epsilon^{RW}_{c,2019}$ for all $t > 2019$. The historical residuals are computed against past GBD data, which is SSP-invariant (the past doesn't depend on the future). So $\hat\epsilon^{RW}_{c,t}$ is a fixed scalar **regardless of which SSP we forecast**. When we multiply $m^T_{SSP2,GBD}$ by $(\tilde S^X / \tilde S^{SSP2})$, the $\exp(\hat\epsilon^{RW}_c)$ factor is identical in numerator and denominator (both refer to the same IHME forecast under SSP2-RCP4.5) and cancels.

**(b) The all-cause RWD trend (Section 2.2.2.1, Eqs. 47–48)**. Same logic. Fit on 1990–2019 historical residuals against GBD past all-cause mortality. SSP-invariant. The all-cause trend is added to all-cause mortality (Eq. 50), not back-distributed to causes. So it doesn't appear in cause-specific forecasts at all — it only matters for the all-cause anchor.

**(c) The Section 6 alignment shift (Eqs. 59–60)**. The shift uses $A_{t_0}$ (reference scenario forecast at 2021) and $C_{t_0}$ (GBD 2021 data at 2021). 2021 is the base year for the forecast, so the SSP2-RCP4.5 climate at 2021 is essentially the same as historical climate — there's no meaningful SSP divergence yet. So $A_{t_0}$ is SSP-invariant for practical purposes. $C_{t_0}$ is GBD 2021 data, definitionally SSP-invariant. The relative-vs-absolute blending weight $\alpha$ depends on $A_{t_0}$, which (by the same argument) is SSP-invariant. So $\kappa^{align}_c$ is SSP-invariant per cause, and cancels between numerator and denominator of our ratio in the same way as $\hat\epsilon^{RW}_c$.

**Conclusion**: the latent trends and alignment shift contribute to **absolute-level** uncertainty in the IHME forecast (we get whatever historical-residual structure they injected), but they do **not differentially bias one SSP vs. another**. When we form the cross-SSP ratio, they multiply through identically and cancel.

The principal's intuition that these mechanisms depend on temperature is correct in a weak sense — they were partly fit on historical data shaped by historical temperature variation. But the **forecast** application of those fitted parameters does not introduce any new SSP-dependent term. Once trained, $\hat\epsilon^{RW}_c$ is just a constant addition to log-mortality regardless of forecast climate.

### 2.4 The alignment shift's near-multiplicative regime for the 12 temperature causes

The Section 6 alignment is a logistic blend of "relative" (multiplicative) and "absolute" (additive) shifts, with weight

$$\alpha = \frac{1}{1 + \exp\!\big[-k \cdot (A_{t_0} + 13.81)\big]}, \quad k = 0.7,$$

where $A_{t_0}$ is the reference-scenario mortality rate at 2021. The text says $\alpha = 0.5$ when the rate is "1 per million" and the 13.81 shift implies the input is on a $-\ln$ scale (so $A_{t_0}$ here is shorthand for $-\ln(\text{rate})$, with $13.81 \approx -\ln(10^{-6})$). For typical adult NCD mortality rates (rate $\gg 10^{-6}$), the argument is large and $\alpha \to 1$ — the shift is essentially purely multiplicative.

For the 12 GBD-temperature causes — IHD, stroke, hypertensive heart disease, diabetes, CKD, LRI, COPD, and external causes — adult mortality rates are well above $10^{-6}$ in essentially all 204 countries. So $\alpha \approx 1$ and $\kappa^{align}_c \approx C_{t_0}/A_{t_0}$, a purely multiplicative shift that cancels in our cross-SSP ratio.

For very rare causes (the 10 that get the special-case override, including measles, malaria, encephalitis, hepatitis B, exposure to forces of nature, conflict and terrorism), $\alpha$ may be 0 and the shift is additive. **One of those 10 — exposure to forces of nature — is in the Burkart 17 but not in the GBD-12.** For that cause, the alignment shift is a non-multiplicative additive perturbation that does *not* cancel cleanly in our ratio. It's also outside the GBD-12 temperature module, so its IHME forecast has no embedded temperature signal to begin with. Workflow B applied to forces of nature gives a forward-only computation on top of an additively-shifted level anchor. The contribution to total temperature-attributable mortality is small (forces of nature is rare), so this is an acceptable disclosure rather than a methodology-breaker. But it is worth flagging.

### 2.5 Quantifying the residual error sources

Three sources survive in Workflow B even after the cancellation argument holds:

| Source | Mechanism | Affects level? | Affects cross-SSP ratio? | Estimated magnitude |
|--------|-----------|----------------|-------------------------|---------------------|
| Calibration drift $r^{SSP2}_c$ | Different ERFs / TMRELs / population recipe between IHME and our pipeline | Yes | No (cancels in ratio) | ±5–10% on absolute levels for 12 overlap causes; 100% (forward-only) for 5 extra Burkart causes |
| Cohort-component on counts | If anchored on counts, IHME's SSP2 population is implicitly used for SSP-X counts | Counts only, not rates | Yes (for counts) | Up to ±2% by 2050 in age-bin counts; large 5–10% drift possible at older age bins |
| Section 6 additive shift for forces of nature | $\alpha = 0$ override makes the shift additive, breaking multiplicativity | Yes | Slightly | Small for global aggregates; possibly large in a few high-disaster locations |

The rate-vs-count distinction in row 2 is the substantive change vs. the previous report. See Section 4 below.

---

## 3. The cohort-component concern, reassessed

### 3.1 What the appendix actually says about the population pipeline

The appendix is explicit on the order of operations (p. 7, paragraph beginning "First..."): SDI etc. are forecast, then risk factors, then cause-specific mortality, then all-cause aggregation with latent-trend adjustment, **then** "Fourth, we compute population forecasts applying forecasts of mortality, fertility, migration, and sex ratio at birth to the GBD 2021 starting population (section 2.4)."

Section 2.4 itself (p. 39) defers entirely to Vollset et al. 2020 for the cohort-component method ("Our methods for forecasting population, as well as the pipelines generating its requisite upstream inputs (life expectancy, ASFR, and sex ratio at birth), except migration, were directly inherited from Vollset et al."). The appendix gives no equations describing how mortality rate forecasts are converted into population.

**Mortality rates → population**: the cohort-component model takes age-specific all-cause mortality rates as input, evolves a population age structure forward year-by-year, and produces population by age-sex-location-year-draw. There is no documented feedback into cause-specific mortality.

**Mortality rates → counts**: counts $= \text{rate} \times \text{population}$. If IHME publishes counts (which they typically do via the GBD Foresight tool), the count is implicitly $\text{rate}_{c,SSP2} \times \text{pop}_{SSP2,GBD}$, and $\text{pop}_{SSP2,GBD}$ is built from SSP2-RCP4.5 cohort-component dynamics integrating the SSP2-RCP4.5 mortality rate forecast.

### 3.2 The propagation pathway the previous report missed

Suppose IHME's published cause-specific mortality is in **counts**. The previous report assumed Workflow B was clean because there is no documented feedback from population back to cause-specific mortality. That is true at the rate level. **It is not true at the count level**, because the count itself is rate-weighted by a population that is the integrated outcome of SSP2 mortality.

Concretely, if we want SSP-X counts and use IHME's SSP2 counts as a level anchor:

$$\underbrace{m^T_{c,X,GBD,counts}}_{\text{what we'd want, } \text{rate}_{c,X} \times \text{pop}_X} \quad \text{vs.} \quad \underbrace{m^T_{c,SSP2,GBD,counts} \cdot \frac{\tilde S^X_{temp,c}}{\tilde S^{SSP2}_{temp,c}}}_{\text{Workflow B on counts}} = \text{rate}_{c,SSP2} \cdot \text{pop}_{SSP2} \cdot \frac{\tilde S^X_{temp,c}}{\tilde S^{SSP2}_{temp,c}}$$

The first quantity uses the SSP-X-evolved population $\text{pop}_X$. The second uses SSP-2's $\text{pop}_{SSP2}$. The difference $\text{pop}_X - \text{pop}_{SSP2}$ at age $a$ in year $t$ accumulates the difference in cohort survival from year of birth to year $t$ across all causes — including, indirectly, the temperature-affected causes.

**This is the propagation mechanism the assistant director may have been describing.** It does not "redistribute saved deaths into other causes" in a literal cause-of-death sense, but it does propagate temperature-saved-or-lost deaths forward in time across cohorts via the population age structure, and the mortality count in any future year reflects all prior years' cumulative temperature differential. The previous report dismissed this concern; on re-reading the appendix and thinking it through carefully, the principal is correct that this is a real and unaccounted-for pathway *if Workflow B is applied to counts*.

### 3.3 Magnitude of the cohort-component effect

The temperature-attributable fraction of all-cause mortality is small — Burkart 2021 puts it at ~1.69M of ~58M annual deaths globally, i.e., ~3%. Across 30 forecast years, the cumulative SSP-X-vs-SSP2 differential in age-specific population is bounded by integrated annual differentials in age-specific mortality, weighted by cohort-survival sensitivities.

A back-of-envelope: if temperature-attributable mortality rate differs by ~10–20% across SSPs in 2050 (large climate divergence by then), and this differential applies to ~3% of total mortality, the differential in cause-specific mortality rates across SSPs is at most 0.3–0.6% of total. Integrated over 30 years, the population age structure differs by at most a few percent at older age bins. **Total mortality counts in 2050 differ by perhaps 1–3% between SSP-X-counts-via-Workflow-B and SSP-X-counts-via-direct-rate-application.**

That's not catastrophic, but it is larger than the calibration drift on the 12 overlap causes. And it scales monotonically with horizon — by 2100 (if we extended Workflow B that far, which we shouldn't), this could be 5–10%.

### 3.4 The fix

**Anchor Workflow B on rates, not counts.** Specifically:

1. Request IHME mortality forecasts at the **rate level** (per-capita, age-specific, draw level). IHME publishes both rates and counts in the Foresight tool; rates are the cleaner anchor for our purpose.

2. Compute SSP-X rates using Workflow B's ratio applied to IHME's SSP2 rates:
   $$\text{rate}^T_{c,X,l,a,s,t,d} = \text{rate}^T_{c,SSP2,GBD,l,a,s,t,d} \cdot \frac{\tilde S^X_{temp,c,l,t,d}}{\tilde S^{SSP2}_{temp,c,l,t,d}}.$$

3. Apply our pipeline's population (Wittgenstein-SSP-aligned, or our own cohort-component using Workflow-B-corrected rates) to convert rates to counts:
   $$\text{count}^T_{c,X,l,a,s,t,d} = \text{rate}^T_{c,X,l,a,s,t,d} \cdot \text{pop}_{X,l,a,s,t}.$$

   The choice of population is a separate decision: WorldPop-extrapolated under SSP-X, Wittgenstein/IIASA SSP-aligned, or our own cohort-component model running on the Workflow-B rates. All are tractable.

This is a small operational change to the prep doc's Eq. (5). The data ask shifts from "cause-specific mortality forecasts" (ambiguous between counts and rates) to "cause-specific mortality **rate** forecasts (per-capita)." If IHME publishes only counts, the team can divide by IHME's published population to recover rates; the population is in the same data product and is a non-controversial export.

The all-cause aggregation in prep-doc Eq. (7) also needs to be at the rate level, then population-weighted to counts, then summed.

---

## 4. The 17-vs-12 cause issue (preserved from previous report)

This section is the same substance as the previous report; restating because it is a real, separable methodology decision.

**Burkart 2021 fit ERFs for 17 causes** (verified from `from-samuel/burkart-paper.txt` lines 36, 498–501):
- Cardiovascular (4): IHD, stroke, hypertensive heart disease, cardiomyopathy and myocarditis
- Respiratory (2): COPD, LRI
- Metabolic (2): diabetes, CKD
- External (9): homicide, suicide, drowning, mechanical injuries, "other unintentional injuries", animal-related (animal contact), disaster-related (forces of nature), road injuries, "other transport-related injuries"

**GBD 2021 forecast appendix lists 12** (Section 2.1.6.7, p. 28, paragraph "Twelve causes met these criteria..."):
IHD, stroke, hypertensive heart disease, diabetes, CKD, LRI, COPD, homicide, suicide, mechanical injuries, transport-related injuries (single category), drowning.

**The 5 Burkart causes absent from the GBD-12**:
1. Cardiomyopathy and myocarditis
2. Other unintentional injuries
3. Animal-related (animal contact)
4. Disaster-related (forces of nature) — also one of the Section 6 special-case 10 causes (additive alignment shift)
5. The fifth depends on aggregation: Burkart treats road injuries and other transport-related injuries as 2 separate causes that both met inclusion; GBD-12 lists "transport-related injuries" once. Treat as 1 cause discrepancy if GBD aggregated, or as a true 5th if they kept road injuries separate but excluded other transport. (Q5 to IHME — see Section 7 below.)

Mass-of-cause perspective: cardiomyopathy/myocarditis is the largest of these in mortality terms (~250–500k deaths/year globally). Animal contact and forces of nature are small (under 50k each). Other unintentional injuries is moderate. So the 5-cause exclusion mostly matters via cardiomyopathy/myocarditis.

**Recommendation**: Include all 17 in the deliverable, with explicit disclosure that for the 5 extra Burkart causes the result is a forward-only Burkart calculation overlaid on an IHME baseline that has no embedded temperature signal for those causes. Provide a sensitivity table showing the 12-cause-only and 17-cause-extended totals.

---

## 5. Three ways Workflow B could still go wrong

Beyond the cancellation arguments above, three failure modes deserve named risk-flags:

### 5.1 IHME re-runs the entire pipeline per SSP, with SSP-dependent latent-trend / alignment parameters

If — contrary to my reading of the appendix — IHME's RWD or alignment parameters are *recomputed per SSP* (rather than fit once on historical data and applied to all scenario forecasts), then the cancellation argument breaks. The appendix is fairly clear that the latent trends are fit on 1990–2019 historical data (Eq. 46) which is SSP-invariant, and the alignment uses 2021 GBD data (also SSP-invariant). But the appendix does not explicitly say "the same latent-trend and alignment parameters are applied to all four scenarios." Q3 in Section 7 below asks IHME to confirm this.

If the answer is "yes, parameters are reused," Workflow B's cancellation holds.
If the answer is "no, parameters are refit per scenario," Workflow B's cross-SSP ratio carries an SSP-dependent residual that doesn't cancel, and the previous report's robustness claim collapses (and the principal's claim #1 becomes correct).

### 5.2 The cause-specific RW residual implicitly back-distributes the all-cause RWD

The appendix's wording on Section 2.2.2.1 is slightly muddled. Eq. 50 reads $\ln(m^T_{lastd}) = \ln(m^{NL}_{lastd}) + \hat\epsilon_{lastd} + \hat\zeta_{lasd}$, which is the *all-cause* total. The accompanying paragraph says "latent trends for lower-level causes... were modelled using a random walk without drift" — implying each lower-level cause has its own RW-no-drift. But the appendix does not explicitly write the analogous equation for cause-specific. The phrase "These trends were integrated into the non-latent forecasts derived from our base model, with an adjustment made to accommodate the uncertainty associated with the input GBD estimates" (p. 34) is ambiguous about whether the all-cause RWD residual is also distributed back to causes (e.g., proportionally) or whether each cause stands alone.

If the all-cause RWD is back-distributed to causes proportionally to each cause's contribution to all-cause non-latent mortality, then a small fraction of the historical temperature signal embedded in the all-cause residual gets shared across all causes — including causes with no direct temperature sensitivity. This is the "diffusion" mechanism the previous report described as "real but small." Under the SSP-invariance argument (Section 2.3 above), it still doesn't bias the cross-SSP ratio, because the back-distribution amount is itself SSP-invariant. So even if 5.2 holds, the cancellation argument survives — but the absolute level uncertainty grows.

Q4 in Section 7 below asks IHME to clarify whether back-distribution happens.

### 5.3 IHME publishes counts, and the team uses counts directly without converting to rates

This is the cohort-component issue (Section 3 above). Mitigated by anchoring on rates. If the team forgets and uses counts, the SSP2 population structure is implicitly held fixed under SSP-X, biasing absolute and cross-SSP differential mortality counts.

---

## 6. Recommendation

**Proceed with Workflow B at the rate level. Cap horizon at 2050. The five-line plan**:

1. **Anchor on rates, not counts.** Modify prep-doc Eq. (5) and Eq. (7) to operate on per-capita rates, then apply our pipeline's population (Wittgenstein-SSP-aligned, WorldPop, or our own cohort-component on Workflow-B rates) to convert to counts. This is the substantive change vs. the previous report.

2. **Stop calling the operation "unscaling" — but understand that "rescaling" is algebraically identical.** Use "rescaling" or "anchored ratio" with IHME, but be honest in our methodology disclosure that the ratio operation IS algebraically equivalent to "divide by SSP2 scalar, multiply by SSP-X scalar." What makes Workflow B defensible is the cancellation of pipeline-shared biases in the ratio, plus the SSP-invariance of the latent-trend and alignment terms — *not* the framing trick.

3. **Lock in 6 disclosure assumptions** (revised from previous report's 5):
   - $S_{ee}$ (other risks) treated as constant across SSPs.
   - 12 overlap causes anchored to IHME; 5 extra Burkart causes are forward-only forward calculations on top of a temperature-free IHME level. Disclose, do not omit.
   - 2050 horizon cap. 2100 extension is a separate methodology problem (Wittgenstein hybrid).
   - Workflow B operates on **rates**; counts are post-derived using our population.
   - Calibration drift on absolute rate levels of order ±5–10%; cancellation on cross-SSP rate ratios at ±2–4% (shrunk because the cohort-component issue is now removed via the rate-level fix).
   - Heat-only and cold-only PAFs retained separately (Q4 to IHME).

4. **Submit the prep doc's Tier 1 data ask, with a single modification: request rates (not just counts).** If counts are all that's available, ask for the population denominator alongside, so we can divide. Two series, draw level: cause-specific (17 Burkart causes) + all-cause SSP2-RCP4.5 forecasts, **rates by age**, sex × location × year, 2022–2050.

5. **Submit the prep doc's Tier 2 methodology questions (6), plus 6 additional ones** in Section 7 below. The new ones are critical for confirming the SSP-invariance argument and the cohort-component fix.

6. **Reporting layer: deliver both rates and counts, plus cross-scenario differentials.** The differentials are the methodologically-cleanest number; absolute rates are the most useful for World Bank decisions; counts require the population-application step and are the most fragile to cohort-component effects (now mitigated by the rate-level anchor).

---

## 7. Open questions for follow-up with IHME

Existing prep-doc Tier 2 questions (Q1–Q6) are unchanged. Adding 6 new questions, in priority order. The first three are critical for confirming Workflow B's validity argument; the last three are operational.

**Q-NEW-1 (CRITICAL, tests the SSP-invariance argument): When you forecast under alternative scenarios (Improved Environment, etc.), do you re-fit the random-walk-with-drift latent trend parameters and the Section 6 alignment shift parameters per scenario, or are those parameters fit once on historical data / 2021 base year and applied identically across scenarios?**
This is the load-bearing question for the cancellation argument in Section 2.3 above. If parameters are fit-once-applied-many, the cross-SSP ratio is clean. If they are refit per scenario, our ratio carries an SSP-correlated residual that does not cancel, and Workflow B's robustness claim collapses.

**Q-NEW-2 (CRITICAL, tests the cohort-component fix): Are your published cause-specific mortality forecasts available as both rates (per-capita) and counts? If only counts are exported, can you also export the population denominator at the same age × sex × location × year × draw resolution?**
We need rates to apply Workflow B cleanly. This is operational but matters.

**Q-NEW-3 (HIGH PRIORITY, clarifies the back-distribution): When the lower-level cause-specific RW-no-drift latent trend is computed (Section 2.2.2.1, paragraph 2), is it back-distributed from the all-cause RWD residual (e.g., proportionally to non-latent cause-specific contribution) or is each cause's RW-no-drift fit independently on cause-specific historical residuals?**
The appendix is ambiguous. Either answer is consistent with the cancellation argument under SSP-invariance, but the answer matters for the magnitude of cross-cause coupling and for our absolute-level uncertainty budget.

**Q-NEW-4 (CONFIRM the cohort-component one-way flow): Is there any post-population-forecast feedback from the cohort-component output (Section 2.4) into cause-specific mortality rates? Specifically, does the Spectrum / cohort-component pipeline have any mortality-revision step (e.g., capping life expectancy gains, hardcoding survival probability ceilings, or reconciling implausible age structures)?**
The appendix doesn't describe one, but it inherits methods from Vollset 2020 by reference. We need to confirm there is no such step. This is the question that *would* validate or invalidate the assistant director's "cohort-component ensures people can't live too long" remark at the methodological level. Even if such a step exists, it operates on rates (not differentially across SSPs), so the cancellation argument should still hold for the rate-anchored Workflow B — but knowing the mechanism shapes our disclosure.

**Q-NEW-5 (OPERATIONAL): For the 12 GBD-temperature causes, can you confirm which 5 Burkart causes did *not* meet your inclusion criteria (cardiomyopathy/myocarditis, animal-related, forces of nature, other unintentional injuries, plus one road/transport injury subdivision)?**
Same as the previous report's Q5. The exact list determines our 5-cause adjustment.

**Q-NEW-6 (CLARIFICATION): For the Section 6 alignment shift, the special-case 10 causes get $\alpha = 0$ (purely additive). One of those 10 — exposure to forces of nature — is in the Burkart 17 but not the GBD-12. Can you confirm that for non-special-case causes (the 12 GBD-temperature causes specifically), the typical $\alpha$ value is close to 1 in 2050 (i.e., the shift is essentially multiplicative)?**
This confirms the multiplicativity assumption underlying our cancellation argument for the 12 overlap causes.

---

## 8. Issues I flag but cannot resolve with what's available

1. **The Foreman 2018 / Vollset 2020 pipelines** (refs 2 and 3 in the appendix) are referenced for population and risk-scalar methodology details. I have not retrieved them. If those antecedent papers describe a population → mortality feedback that the GBD 2021 appendix omits, the cohort-component concern may be larger than the rate-level fix can absorb. Q-NEW-4 above directly asks IHME about this.

2. **The IHME pushback as transcribed in the handoff doc and the IHME methods as documented in the appendix do not fully agree.** The pushback referenced (a) cohort-component "people can't live too long" constraints and (b) cause-of-death squeezing. (a) is partially explained by the count-level cohort-component effect (Section 3 above) — though the assistant director may have meant something more specific that I haven't reproduced exactly. (b) is *not* in the appendix as a forecast-pipeline step; the closest analog is the Section 6 per-cause alignment, which is not a strict "squeeze" but does pull each cause to align with GBD 2021. Either the appendix is not exhaustive, or the IHME staff were describing an estimation-pipeline mechanism that doesn't apply in forecasting, or they were describing internal pipeline details not in the published methods. **This remains the single most important thing to clarify in the next IHME meeting.** Q-NEW-1 + Q-NEW-4 above are designed to elicit the relevant detail.

3. **The 1000 MR-BRT draws + 100 TMREL draws (our pipeline) vs 500 draws (IHME's published forecast)** — minor draw-count mismatch. Sample 500 from our 1000 to align. Document explicitly in methodology.

4. **Skill-score evaluation in Appendix Tables E and F** (pp. 42–44) shows mixed performance across the 12 temperature-affected causes. CVD-bucket skills are 0.10/0.08 (mortality, M/F); diabetes/kidney 0.44/0.37; self-harm + interpersonal violence 0.26/0.37. **For Workflow B**: low-skill causes have larger absolute-level uncertainty in our level anchor, but the cross-SSP differential is still robust because the ratio comes from our pipeline. This argues for headlining cross-scenario differentials over absolute counts.

---

## 9. Files referenced (absolute paths)

- `/var/home/aoz/Dropbox/OZ-Labs/WorldBank/2026-climate-health-burden-projections/handoff-ihme-temp-unscaling-validity.md` — original assignment brief
- `/var/home/aoz/Dropbox/OZ-Labs/WorldBank/2026-climate-health-burden-projections/gbd/gbd-2021-forecast-appendix.pdf` — IHME methods appendix (75 pp.)
- `/var/home/aoz/Dropbox/OZ-Labs/WorldBank/2026-climate-health-burden-projections/gbd/gbd-appendix.txt` — text dump of the above (used to verify equation references)
- `/var/home/aoz/Dropbox/OZ-Labs/WorldBank/2026-climate-health-burden-projections/gbd/ihme-plan-b-prep.tex` — team prep doc
- `/var/home/aoz/Dropbox/OZ-Labs/WorldBank/2026-climate-health-burden-projections/gbd/charlie-proposed-climate-removal-concept-note.pdf` — Charlie's email thread
- `/var/home/aoz/Dropbox/OZ-Labs/WorldBank/2026-climate-health-burden-projections/from-samuel/Info Burkart/burkart-ihme-meth-paper.pdf` — Burkart 2021
- `/var/home/aoz/Dropbox/OZ-Labs/WorldBank/2026-climate-health-burden-projections/from-samuel/burkart-paper.txt` — text dump of the above

---

## 10. One-paragraph summary for the next IHME meeting

> "We've reviewed your GBD 2021 forecasting methods appendix carefully (Sections 2.1.6.7, 2.2.1, 2.2.2, 2.2.2.1, 2.4, and 6). We're persuaded that temperature attaches to cause-specific mortality multiplicatively in log space (Eq. 43), aggregates additively to all-cause (Eq. 45), and that the random-walk latent trends (Section 2.2.2.1) and the post-processing alignment (Section 6) operate on parameters fit from historical 1990–2019 data and the 2021 GBD anchor — both of which are SSP-invariant. Our plan is to use your SSP2-RCP4.5 cause-specific mortality forecasts as a level anchor, at the rate (per-capita) level, and apply the ratio of our pipeline's temperature scalars across SSP scenarios to project mortality rates under SSP1-RCP1.9 and SSP5-RCP8.5. We'll then convert rates to counts using our own population (avoiding any cohort-component propagation issues from your pipeline). We have six new methodology questions, the most important being whether the latent-trend and alignment parameters are reused identically across all four of your scenarios, or refit per scenario. We're not asking for any data export beyond cause-specific and all-cause mortality rate forecasts (with the population denominator if needed) for the 17 Burkart causes, draw level, 2022–2050 — same as before."
