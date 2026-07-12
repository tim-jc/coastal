# Publishing

The public dashboard is published from root `index.html`.

## Main Artifact

`index.Rmd` renders to:

```text
index.html
```

Pushing changes to root `index.html` publishes the updated dashboard through GitHub Pages. The render entry point commits and pushes root `index.html` automatically when it changes.

## Before Publishing

Run:

```r
Rscript scripts/check_inputs.R
```

Publish only if the input check passes.

The current known blocker is the empty silver `activity_streams` table. Until that upstream issue is fixed, do not commit or push regenerated dashboard output.

## Suggested Publishing Flow

1. Update metadata or source code.
2. Run `Rscript scripts/check_inputs.R`.
3. Run `Rscript scripts/render_dashboard.R`.
4. Review and commit source changes separately.

## Generated Output

Generated output is intentionally committed when publishing. The render entry point currently publishes:

- `index.html`

Avoid committing generated churn from failed or partial renders. Preview renders to an alternate output path do not publish.
