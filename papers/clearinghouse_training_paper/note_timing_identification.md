# Note: Using timing of effects to strengthen channel identification

## Study timeline

- **Baseline**: Survey + ratings collected from treated farmers + ratings disseminated → **Season 1** → **Midline**: Survey + second round of ratings collected + disseminated → **Season 2** → **Endline**: Final survey

The intervention runs for two agricultural seasons with three data collection points. At each point, ratings are collected from treated farmers, aggregated, and disseminated before the next planting season.

## Timing predictions by channel

### Channel 1: Belief updating

Ratings are disseminated before Season 1. Farmers update beliefs immediately upon receiving the aggregated peer signal. This means:

- **Perceptions**: Improve at midline (direct effect of receiving ratings) and persist at endline (reinforced by second round of ratings).
- **Adoption**: Increases at midline (updated beliefs push certainty equivalent above threshold) and persists at endline.
- **Yields**: Increase at midline (through adoption in Season 1) and endline.

Key feature: **effects appear immediately** — perceptions and adoption effects visible from midline onward.

### Channel 2: Search cost reduction

Dealer-specific ratings are also disseminated before Season 1, so farmers can switch dealers immediately:

- **Switching**: Increases at midline (farmers use ratings to identify better dealers) and persists at endline.
- **Adoption**: Could increase at midline if lower search costs reduce the effective cost of purchasing.
- **Yields**: Increase at midline (better dealer → better seed) and endline.

Key feature: **also immediate** — similar timing to belief updating. This channel is distinguished from belief updating by the presence of switching, not by timing.

### Channel 3: Supply-side response

This is where timing becomes informative. Dealers learn about the rating system at baseline and may improve seed handling and storage practices. But because seed is a credence good, farmers cannot directly observe quality improvements:

- **Seed quality**: May improve from Season 1 onward (if dealers respond quickly).
- **Yields for existing adopters**: Could improve at midline — farmers who already purchase from agro-dealers receive better seed without knowing it.
- **Perceptions**: Do NOT improve at midline. Even if seed quality has improved, individual farmers cannot attribute yield differences to seed quality (noisy production, many confounding factors). The quality improvement only becomes *observable to other farmers* when midline ratings aggregate Season 1 experiences and reveal the improvement. These updated ratings are disseminated before Season 2.
- **Adoption**: Does NOT increase at midline — non-adopters have no way of knowing that seed quality has improved. Only after updated midline ratings (reflecting improved quality) are disseminated do non-adopters learn about the improvement. Adoption effects therefore appear at **endline only**.

Key feature: **perceptions and adoption effects are delayed to endline**. The credence good property creates a lag — quality improvements need to pass through one round of aggregated ratings before they become observable to farmers.

## Summary table

| Outcome | Belief updating | Search costs | Supply-side |
|---------|:-:|:-:|:-:|
| **Perceptions – Midline** | ✓ | — | ✗ |
| **Perceptions – Endline** | ✓ | Indirect | ✓ |
| **Adoption – Midline** | ✓ | ✓ | ✗ |
| **Adoption – Endline** | ✓ | ✓ | ✓ |
| **Yields – Midline** | ✓ (through adoption) | ✓ (through switching) | ✓ (existing adopters) |
| **Yields – Endline** | ✓ | ✓ | ✓ |
| **Switching – Midline** | ✗ | ✓ | ✗ |
| **Switching – Endline** | ✗ | ✓ | ✗ |
| **Seed quality** | ✗ | ✗ | ✓ |

## Implication for identification

The existing identification strategy relies on *which outcomes* are affected: no switching rules out search costs, no seed quality improvement rules out supply-side. The timing dimension provides **additional, independent evidence**:

- If belief updating is the primary channel, we should see perceptions and adoption effects at **both midline and endline**.
- If supply-side is doing most of the work, perceptions and adoption effects should be concentrated at **endline only** (with possibly some yield effects for existing adopters at midline).

Finding perceptions and adoption effects already at midline is therefore evidence that belief updating — not supply-side response — is the primary driver. This strengthens the identification because it exploits a different source of variation (timing rather than outcome type) and is consistent with the credence good property of seed that motivates the model.

## Question for discussion

Should we incorporate this timing dimension into the predictions table and/or test for it explicitly by comparing midline and endline effects? The data structure supports this — we have outcomes at both midline and endline. This would be an additional (exploratory) piece of evidence, but one that follows naturally from the model and the study design.
