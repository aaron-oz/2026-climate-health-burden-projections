#!/usr/bin/env bash
#
# status.sh — How is the run going?
#
#   ./status.sh              one line per location, plus a summary
#   ./status.sh --failures   also list the combos that failed and why
#   ./status.sh --loc=62     just one location
#
# Read-only and safe to run as often as you like while the run is going.
set -euo pipefail
cd "$(dirname "$0")"
export PROJECT_ROOT="$(pwd)"
exec Rscript global-scripts/util_run_status.R "$@"
