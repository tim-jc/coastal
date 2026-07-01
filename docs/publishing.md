# Publishing

The public dashboard is published from root `index.html`.

## Main Artifact

`index.Rmd` renders to:

```text
index.html
```

Pushing changes to root `index.html` publishes the updated dashboard through GitHub Pages.

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
3. Render `index.Rmd`.
4. Review source and generated diffs.
5. Commit source changes and generated output together.
6. Push to publish.

## Generated Output

Generated output is intentionally committed when publishing. This includes:

- `index.html`
- rider trace PNGs under `docs/`

Avoid committing generated churn from failed or partial renders.
