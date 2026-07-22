#!/usr/bin/env bash

set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
project_root="$(dirname "$script_dir")"

cd "$project_root"

Rscript scripts/check_inputs.R
Rscript scripts/render_dashboard.R
Rscript scripts/publish_dashboard.R
