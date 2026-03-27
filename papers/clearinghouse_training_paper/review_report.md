# Review Report: "From Ratings to Returns"

## Consolidated Review for Co-Author Internal Use

**Paper:** "From Ratings to Returns: Peer Information, Farmer Beliefs, and Technology Adoption under Performance Uncertainty"
**Authors:** Miehe, Sparrow, Spielman, Van Campenhout
**Target:** Journal of Development Economics
**Date:** March 2026
**Review method:** Structured academic review + adversarial Reviewer 2 simulation

---

## Overall Score: 6.5/10 (Revise-and-Resubmit territory)

The paper addresses an important question with a well-executed RCT and commendable transparency. The dual-sided design tracking both farmers and agro-dealers is a genuine strength. However, the mechanistic claims outrun the evidence in several places, and specific issues around the bundled intervention, the weakness of the perceptions result, and some internal inconsistencies need to be addressed before submission.

---

## Strengths

1. **Design quality:** Catchment-area-level randomization, placebo SMS for control group, three waves, both buyer and seller-side outcomes. Serious fieldwork.
2. **Transparency:** Pre-registered PAP, mock report on simulated data before outcome collection, public GitHub repo. Exemplary.
3. **Policy relevance:** Can crowd-sourced quality information substitute for weak certification systems? This matters enormously for seed systems across Sub-Saharan Africa.
4. **Alternative mechanisms tested:** Testing for agro-dealer switching and seed quality changes goes beyond what many papers attempt.
5. **Heterogeneity results strengthen the story:** New analysis confirms that adoption and productivity effects are driven by non-purchasers, consistent with the conceptual framework (see Appendix to this report).

---

## Major Issues and Suggested Changes

### 1. Bundled intervention: temper mechanism claims

**Problem:** The treatment combines (a) asking farmers to rate agro-dealers (active recall/reflection), (b) SMS + in-person delivery of ratings, and (c) certificates to agro-dealers. The paper cannot distinguish whether effects come from information content, the act of rating, or agro-dealer behavioral responses. The significant effects on agro-dealer services and signaling (Table on practices) create an internal tension with the claim that the mechanism is purely farmer beliefs.

**Suggested changes:**
- Replace "rule out alternative mechanisms" (abstract, intro, conclusion) with "find no evidence of" or "provide evidence against" throughout. This language appears ~4 times.
- Add 1-2 sentences in the discussion of the agro-dealer practices results acknowledging that improved services could independently contribute to adoption/productivity, making this a complementary rather than ruled-out channel.
- Acknowledge the bundled nature of the intervention explicitly in either Section 3 (Experimental design) or the conclusion. The current design isolates rating information from *salience*, but not from the act of rating or the agro-dealer certificate.

### 2. Perception effect: weak and measured with treatment instrument

**Problem:** The perception index shows +0.09 SD at 10% significance — the weakest link in a causal chain where downstream outcomes (adoption at 5%, yield at 1%) are more significant. Perceptions are elicited using the same rating instrument used in treatment delivery. This conflates treatment delivery with outcome measurement.

**Suggested changes:**
- Discuss the inverted significance pattern transparently. Add a sentence noting that the perception measure may be underpowered (smaller effective sample because it requires non-missing ratings on all 6 dimensions).
- Acknowledge the measurement concern: treated farmers evaluated agro-dealers on the same scale during the intervention. Mention cognitive dissonance and anchoring as alternative explanations for the perception shift (even if only in a footnote).
- Consider adding a power calculation for the perception outcome.

### 3. The adoption decay puzzle

**Problem:** Purchasing from agro-dealers goes from +6pp at midline (significant) to ~+3pp at endline (insignificant). Under Bayesian belief updating, we would expect persistence or increase as farmers gain confirming experience. The decay is more consistent with a salience/attention model.

**Suggested changes:**
- Discuss this pattern explicitly. A possible explanation: at endline, the control group may have caught up through own learning (they now have one more season of experience). Or: the initial effect captures a "try it" response that partially reverts.
- Consider whether the endline effect on "used high-yielding seed" (+4.2pp, significant) captures the more durable behavioral change while the specific "bought at agro-dealer" variable captures a noisier purchasing decision.

### 4. Back-of-envelope calculation for yield effects

**Problem:** A ~4pp increase in agro-dealer purchasing translating into a 13% yield gain implies very large marginal returns per switcher, unless other channels contribute (complementary inputs, effort, agro-dealer service quality).

**Suggested changes:**
- Add a brief calculation: given the adoption shift and the yield gap between farmer-saved and agro-dealer seed in the control group, what yield effect would be implied? If the observed effect exceeds this, discuss what else might explain the gap.
- The Bulte (2025) complementary input channel is invoked in the conceptual framework but never tested. Consider reporting effects on fertilizer/labor use, or explicitly note this as a limitation.

### 5. Ratings were architecturally positive

**Problem:** >91% of agro-dealers rated above 3/5. Quintile labels were deliberately positive ("okay" to "excellent"). The paper claims the system is "agnostic with respect to seed performance" — it is structurally optimistic.

**Suggested changes:**
- Modify the "agnostic" claim (Introduction, around line 588) to acknowledge the positive skew. Something like: "While the system is in principle agnostic, in our context ratings were concentrated at the upper end of the scale, meaning the treatment primarily conveyed positive information about seed performance."
- Discuss what this implies for generalizability: the results may differ in settings with greater quality variation or where ratings would convey more negative signals.

### 6. Factorial design is a black box

**Problem:** The study is embedded in a larger factorial with cross-randomized treatments (the code reveals variables `training`, `clearing`, `farmer`) that are never described in the paper. Readers cannot assess potential interactions.

**Suggested changes:**
- Add a brief description of the other factorial arms (1-2 sentences in Section 3 or a footnote). What are the `training` and `farmer` treatments?
- Report or mention whether interaction effects with the clearing treatment are significant. The Lin (2013) adjustment handles this econometrically, but readers need to know the substantive context.

### 7. Pre-registration deviation needs transparency

**Problem:** A footnote acknowledges reordering outcomes that changes index composition, but this is too brief.

**Suggested changes:**
- Add an appendix table showing pre-registered vs. as-published index compositions side by side.
- Or at minimum, expand the footnote to specify which outcomes were moved and why.

---

## Minor Issues

- **IHS interpretation:** With many zeros in expenditure/revenue data, the "percentage change" interpretation may not hold (see Bellemare & Wichman 2020). Add a caveat.
- **Differential agro-dealer attrition:** Treated dealers are less likely to drop out. If treatment makes businesses more viable, this conditions on a post-treatment outcome. Mention the direction of potential bias.
- **Anderson adoption index at midline is negative and significant:** This appears in the midline data (b=-0.077, p=0.013). Investigate which component drives this and discuss, especially since the Kling index also produces a negative midline harvest result per the robustness appendix.
- **Wild cluster bootstrap:** 130 clusters with heterogeneous sizes (1-18 agro-dealers per catchment area) warrants at least a robustness check for the main results.
- **Missing literature:** Consider citing Fabregas, Kremer & Schilbach (2019) on digital agricultural advice; Cole & Fernando (2021) on agricultural hotlines.
- **Timeline figure:** The sequence of rating collection, dissemination, planting decisions, and data collection across seasons would benefit from a visual timeline.
- **Coefficient plot:** Consider a summary figure showing all main effects across the causal chain. Increasingly standard for JDE.
- **Rating data section (6.5):** The discussion of gender correction in ratings is substantively important but buried in Appendix A.2. Briefly mention in the main text that ratings were adjusted for gender bias, with a forward reference.

---

## New Finding: Heterogeneity Results Support the Framework

New analysis (run on the project data) confirms that adoption and productivity effects are driven by non-purchasers at baseline, consistent with the conceptual framework's prediction that non-purchasers experience a level shift in beliefs while purchasers primarily gain precision.

**Midline adoption (bought at agro-dealer):**
- Non-purchasers: b=0.068, p=0.008 ***
- Purchasers: b=0.041, p=0.165

**Endline yield (kg/acre):**
- Non-purchasers: b=45.3, p=0.005 ***
- Purchasers: b=42.1, p=0.109

**Recommendation:** Report these heterogeneous effects for adoption and harvest outcomes (not just perceptions). While the testable implications only predict heterogeneous *perception* effects, the adoption/yield heterogeneity provides a more complete and compelling test of the mechanism. These results strengthen the paper.

**Note:** The midline harvest index is *negative* and significant for purchasers (b=-0.132, p=0.022) but insignificant for non-purchasers (b=-0.032). This deserves investigation — it may relate to the puzzling negative Kling index result at midline noted in the robustness appendix.

---

## Summary of Priority Changes

| Priority | Issue | Effort |
|----------|-------|--------|
| **High** | Replace "rule out" language throughout | Low (find & replace) |
| **High** | Describe factorial design's other arms | Low (1-2 sentences) |
| **High** | Add heterogeneity results for adoption/yield | Medium (code exists, needs tables) |
| **High** | Discuss adoption decay (midline→endline) | Low (1 paragraph) |
| **Medium** | Back-of-envelope yield calculation | Low (1 paragraph) |
| **Medium** | Acknowledge positive skew of ratings | Low (1-2 sentences) |
| **Medium** | Pre-registration deviation transparency | Medium (appendix table) |
| **Medium** | Perception measurement concern | Low (footnote or paragraph) |
| **Low** | Wild cluster bootstrap | Medium (code exists) |
| **Low** | Timeline figure | Low |
| **Low** | Missing literature | Low |
