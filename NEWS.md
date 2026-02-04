# vivaglint 0.1.0

## Initial CRAN Release

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
