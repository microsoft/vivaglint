# vivaglint (development version)

## API Enhancements

* `read_glint_survey_api()` now supports three export modes via a new `mode`
  parameter:
    * `"cycle"` (the existing behavior) — single survey cycle.
    * `"survey"` — every cycle of one survey.
    * `"daterange"` — every survey in the experience active within the
      optional start/end date window.
* `survey_uuid`, `cycle_id`, `mode`, `start_date`, and `end_date` now fall
  back to matching environment variables (`GLINT_SURVEY_UUID`,
  `GLINT_CYCLE_ID`, `GLINT_MODE`, `GLINT_START_DATE`, `GLINT_END_DATE`) when
  the argument is omitted. Explicit arguments always win over env vars.
* When omitted, `mode` is inferred from which identifiers are populated, so
  existing `read_glint_survey_api(survey_uuid, cycle_id)` calls keep working
  unchanged.
* Multi-CSV exports (common in `"survey"` and `"daterange"` modes) are
  returned as a named list of `glint_survey` objects keyed by CSV filename.
  Entries whose CSV does not fit the standard schema fall back to plain
  `data.frame` with a warning.
* Default column mappings in `read_glint_survey_api()` aligned with the
  columns the API export actually emits:
    * `emp_id_col` now defaults to `"Employment ID"` (was `NULL`).
    * `sent_date_col` now defaults to `NULL` (was `"Survey Cycle Sent
      Date"`, a column the API does not include).
    * New `manager_id_col` parameter (default `"Manager ID"`). Previously
      hardcoded inside `build_glint_survey()` and unreachable from the
      caller.
  Net effect: a typical API call like
  `read_glint_survey_api(mode = "daterange")` now works with zero
  column-mapping arguments.
* `validate_glint_structure()` no longer warns about an optional column
  when the caller explicitly passes `NULL` — an explicit `NULL` is now
  treated as "skip this concept" rather than "missing column".
* New `parse` parameter on `read_glint_survey_api()` (default `TRUE`).
  Setting `parse = FALSE` skips the in-memory CSV parsing and returns the
  path to the saved zip. `parse = FALSE` requires `save_zip_to` to be set
  (or `GLINT_SAVE_ZIP_TO`); otherwise the call errors before making the
  API request.
* New `save_zip_to` parameter on `read_glint_survey_api()`. When set to a
  file or directory path (or via the `GLINT_SAVE_ZIP_TO` env var), the raw
  export zip Microsoft Graph returns is written to disk in addition to
  being parsed into R data frames. Useful for audit trails or for feeding
  the zip into other downstream tools. Defaults to `NULL` (no zip written).

# vivaglint 0.1.0

## Initial Release

### Core Functions

* `read_glint_survey()` - Import and validate Viva Glint CSV exports
* `summarize_survey()` - Calculate comprehensive question-level metrics including mean, SD, response rates, and favorability percentages
* `get_response_dist()` - Get detailed response value distributions

### Analysis Features

* **Multi-cycle comparisons**: `compare_cycles()` tracks changes across survey cycles with delta calculations
* **Correlation analysis**: `get_correlations()` supports Spearman, Pearson, and Kendall methods with significance testing
* **Factor analysis**: `extract_survey_factors()` identifies latent constructs with multiple rotation options
* **Attrition prediction**: `analyze_attrition()` links survey responses to employee turnover with risk ratios
* **Demographic analysis**: `analyze_by_attributes()` aggregates results by any combination of employee attributes

### Organizational Features

* `aggregate_by_manager()` - Roll up results by organizational hierarchy (direct reports or full tree)
* `pivot_long()` - Reshape data for advanced analysis

### Data Quality

* Automatic validation of Glint export structure
* Survey metadata extraction
* Built-in favorability classifications for 2-11 point scales based on Glint standards
* Minimum group size filtering for privacy protection

### Documentation

* Comprehensive function documentation with examples
* 144 unit tests covering all major functionality
* Support for glint_survey objects and raw data frames
