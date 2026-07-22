# Publishing

The public dashboard is published from root `index.html`.

## Main Artifact

`index.Rmd` renders to:

```text
index.html
```

Pushing changes to root `index.html` publishes the updated dashboard through GitHub Pages. Rendering and publishing are separate operations.

## Before Publishing

Run:

```r
Rscript scripts/check_inputs.R
```

Publish only if the input check passes.

The current known blocker is the empty silver `activity_streams` table. Until that upstream issue is fixed, do not commit or push regenerated dashboard output.

## Suggested Publishing Flow

1. Update metadata or source code.
2. Run `scripts/render_and_publish.sh`.

The shell entry point validates inputs, renders the dashboard, and publishes it sequentially. It stops without publishing if validation or rendering fails.

To inspect the generated output before publishing, continue to run the three underlying scripts separately.

## Generated Output

Generated output is intentionally committed by the explicit publish entry point:

- `index.html`

Avoid publishing generated churn from failed or partial renders. Rendering never stages, commits, or pushes files.
