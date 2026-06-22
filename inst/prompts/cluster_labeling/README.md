# Cluster Labeling Prompt Assets

This directory is the canonical source of truth for the cluster-labeling
prompts used by `llm_label_cluster()`.

For ordinary package use, these prompt assets are consumed indirectly by
`label_clusters()`, which is the current high-level entry point.

## Why prompts are stored as Markdown

Prompt text is stored as plain `.md` files instead of hard-coded R
strings because prompts are content, not business logic.

This makes prompts:

- easier to review in diffs
- easier to copy and version deliberately
- easier to compare across variants
- easier to reuse later in evaluation tooling

The Markdown files are still plain text prompts. They are not rendered as
documentation during runtime.

## Structure

- `catalog.json`
  Machine-readable prompt catalog. Defines the available variants,
  generation defaults, and which files belong to each variant.
- `system_scientific_caution_v1.md`
  Shared system prompt.
- `user_*_v*.md`
  User prompt templates for specific strategies and prompt revisions.

`llm_label_cluster()` loads `catalog.json`, resolves the referenced
prompt files, substitutes placeholders, and builds the final structured
input sent to Ollama. `label_clusters()` reuses the same prompt catalog
through that lower-level API.

## How structured input is assembled

The final request is assembled from three layers:

1. A shared system prompt
2. One user prompt template selected by `variant`
3. Runtime substitutions inserted into the user template

Current placeholders:

- `{{CLUSTER_ID}}`
- `{{OUTPUT_SCHEMA_JSON}}`
- `{{CLUSTER_EVIDENCE_TEXT}}`

The values come from:

- the `cluster_evidence` object
- the packaged JSON schema in `inst/schemas/`
- the internal prompt serializer for evidence

## How to add a new prompt variant correctly

Recommended workflow:

1. Copy the closest existing `user_*_v1.md` file
2. Create a new file with a deliberate versioned name such as
   `user_abstain_first_v2.md`
3. Register the new variant in `catalog.json`
4. Keep the same placeholder names unless the R-side assembly code is
   intentionally changed
5. Test the variant via `dry_run = TRUE` before using it in real model
   calls

## Naming recommendations

Prefer names that encode both intent and version:

- good: `user_conservative_interpretation_v2.md`
- good: `user_concise_label_v1.md`
- avoid: `new_prompt.md`
- avoid: `final.md`
- avoid: `better_version.md`

Stable variant IDs are important for reproducible experiments.

If an existing variant already has benchmark history, prefer copying it
to a new versioned file instead of editing the old file in place.

## What to change carefully

Safe changes:

- wording
- emphasis
- caution level
- abstention policy
- label style guidance

Changes that need extra care:

- placeholder names
- output contract wording
- evidence citation rules
- JSON-only requirements
- generation defaults in `catalog.json`

## What not to do

Do not:

- duplicate the same prompt text back into R source files
- keep two competing sources of truth for the same variant
- rename an existing variant ID casually if past experiments refer to it
- remove placeholders without updating the assembly code
- mix runtime prompt assets with ad hoc evaluation notes

## Current workflow modes

- `workflow_steps = 1`
  One-step mode. The selected label-stage prompt must decide everything
  in a single call.
- `workflow_steps = 2`
  Two-step mode. An internal gate prompt,
  `gate_abstain_examples_v1`, decides `label / abstain` first; the
  selected label-stage prompt is called only if the gate allows
  labeling.

On the current cleaned `pilot` benchmark, `w2` changed behavior more
than headline score. It did not improve the overall
`status_allowed_rate`, but it did make some model/prompt combinations
more selective in `gray_zone` cases.

## Current prompt roles

- `concise_label_v1`
  Shortest label style; best when you want compact names and compact
  summaries.
- `conservative_interpretation_v1`
  Slightly richer interpretation style; similar labeling behavior to
  `concise_label_v1`, but with more room for restrained explanation.
- `abstain_first_v1`
  More abstention-oriented than the first two variants, but still a
  label-stage prompt rather than a separate gate.
- `strict_abstention_gate_v1`
  Hardest one-step label-stage prompt; good when false-positive labels
  are more costly than missed labels.
- `speculative_fallback_v1`
  Soft fallback label-stage prompt intended only after the strict
  workflow fails to produce an accepted stable label. It asks for a
  clearly tentative, low-confidence orientation label rather than a
  normal accepted label.
- `speculative_fallback_v2`
  Cleaned speculative fallback prompt. It removes instructions that are
  now handled programmatically in R, but it still allows abstention.
- `speculative_fallback_v3`
  Best-effort orientation prompt. It strongly prefers a broad labeled
  answer over abstention while still preserving meaningful ecological
  direction.
- `speculative_fallback_v4`
  Label-required fallback prompt. It is stricter about always returning
  a label, but tends to collapse toward broad generic labels.
- `speculative_fallback_v5`
  Emergency rescue prompt. It is designed to never leave a cluster
  unlabeled, even if the result is only a very broad heuristic
  orientation label.
- `gate_abstain_examples_v1`
  Internal gate-only prompt used by `workflow_steps = 2`. It is not
  meant to be used as the main public label-stage `variant`.

## Accepted vs speculative outputs

The current project default is still an ordinary strict label-stage call:

- `variant = "strict_abstention_gate_v1"`
- `workflow_steps = 1`
- no speculative fallback unless you enable it explicitly in
  `label_clusters()`

Speculative fallback is a separate workflow layer, not a normal prompt
replacement:

- keep `strict_abstention_gate_v1` as the main public label-stage prompt
- enable `speculative_fallback_mode = "after_rejection"` only when you
  want tentative orientation labels after strict failure
- `speculative_fallback_v1` is then used internally as a softer fallback
  prompt

This is separate from `workflow_steps = 2`:

- `workflow_steps = 2` means gate -> label
- `speculative_fallback_mode = "after_rejection"` means strict failure
  -> optional tentative fallback label

## phi4-mini full-context rescue ladder

The local experiment at
`temp/reports/phi4_speculative_fullctx/phi4_speculative_fullctx_report.md`
used:

- model: `phi4-mini:latest`
- full evidence
- `ollama_options = list(num_ctx = 8192)`
- `num_predict = 2400`
- 6 abstain-prone benchmark cases from the phase A1 slice

Observed behavior:

- `speculative_fallback_v1`
  Still over-abstains completely on this slice and often returns schema
  errors around abstention fields.
- `speculative_fallback_v2`
  Produces cleaner abstentions than `v1`, but still does not solve the
  "give me a usable label" problem.
- `speculative_fallback_v3`
  Current best phi4-specific soft-label candidate on this slice. It
  reached `100%` labeled, `100%` valid JSON/validation, and about
  `83%` label-family match.
- `speculative_fallback_v4`
  Also fully valid, but too generic in practice. It tends to collapse
  many different clusters into `Mixed Herbaceous Assemblage`.
- `speculative_fallback_v5`
  Strongest "never leave unlabeled" rescue prompt, but usually too
  generic and sometimes still drifts into validation problems.

Practical takeaway for phi4 rescue experiments:

- try `speculative_fallback_v3` first
- keep `speculative_fallback_v4` only as a stronger label-forcing backup
- treat `speculative_fallback_v5` as an emergency fallback, not a
  quality-oriented default

Short display example:

- accepted label: `c_12: Mixed Deciduous Woodland`
- speculative label: `c_27: Woodland-transition assemblage*`
- plot footnote: `* tentative / speculative label; strict validation did not accept a stable evidence-backed label`

## Current recommended combinations

These are the current handoff-level recommendations for ordinary use.
They are based on the cleaned local pilot plus the later project choice
to keep the default workflow simple, reproducible, and easy to support.

### Current project default

- `gemma4:12b` + `strict_abstention_gate_v1` + `workflow_steps = 1`
  This is the current default for the project. It is the easiest
  combination to explain, the easiest to run, and the safest simple
  baseline for cautious one-step labeling.
- If you need tentative plot-oriented fallback labels, keep this same
  prompt/model/workflow combination and add
  `speculative_fallback_mode = "after_rejection"` at the
  `label_clusters()` level rather than swapping out the main prompt.

### Keep as secondary alternatives

- `qwen3.5:9b-q4_K_M` + `concise_label_v1` + `workflow_steps = 2`
  Useful as a more selective experimental alternative when you are
  willing to use the gate-then-label workflow.
- `qwen3.5:9b-q4_K_M` + `conservative_interpretation_v1` +
  `workflow_steps = 2`
  Similar to the previous option, but with slightly richer
  interpretation text.

### Deprioritize for now

- `qwen3.5:9b-q4_K_M` + `abstain_first_v1` + `w1` or `w2`
  Over-abstains on the current pilot, including `easy_positive` cases.
- `qwen3.5:9b-q4_K_M` + `strict_abstention_gate_v1` + `w1` or `w2`
  Same problem: currently too strict to serve as the main labeling mode.
- `gemma4:12b` + `abstain_first_v1` + `w1` or `w2`
  Does not currently buy enough extra caution to justify preferring it
  over the stricter Gemma baseline.
- `gemma4:12b` + `concise_label_v1` or
  `conservative_interpretation_v1` + `w1`
  Still useful for exploratory labeling, but not the preferred
  customer-facing default.

## What the current pilot still does not solve

No current combination is yet reliably good on the
`abstain_expected` band.

That means:

- the current benchmark is already useful for comparing behavior
- but the next iteration still needs stronger hard-negative cases and/or
  stronger abstention logic
- prompt revisions should be added as new versioned variants instead of
  overwriting the existing files

## Future compatibility

This layout is intentionally compatible with later evaluation tooling:

- runtime R code can load these files directly
- future eval harnesses can read the same files instead of maintaining a
  second copy

That is the desired long-term pattern:

- one prompt source of truth
- multiple consumers
