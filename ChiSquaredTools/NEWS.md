# ChiSquaredTools News

## Version 1.1.0

### Bug Fixes
- Fix to the highlighting of Quetelet Index and IJ Factor: values that are beyond the empirical thresholds are now highlighted.
- Fix to column clustering table note displaying wrong method information.
- In SRD, changed column reduction to operate on the pruned matrix (not the row-reduced matrix) so the Column-Grouped Table displays original rows with merged columns.
- In SRD, changed column type from number to integer for count-based columns to remove unnecessary decimal places.

### New Features
- Added Test Validity Assessment table to the Test of Independence facility, providing diagnostic evaluation of chi-squared test validity based on sample size (N ≥ 10), average cell count (N/k ≥ 5), sparsity index (N²/k ≥ 10), and minimum expected frequency (min(E) ≥ 1).
- Decision rule for test selection now follows a structured path: Traditional χ² when N/k ≥ 5; (N-1)/N adjusted χ² when N/k < 5 but N²/k ≥ 10 and min(E) ≥ 1; Permutation or Monte Carlo methods otherwise.
- Added Koehler & Larntz (1980) sparsity criterion to validity diagnostics.
- Integrated Correspondence Analysis facility into ChiSquaredTools, providing multiple visualisation options (Symmetric CA Plot, Geometric Interpretation Plot, Contribution Biplot) with support for both Pearson residuals (χ² distance) and Freeman-Tukey residuals (Hellinger distance). Features include Malinvaud's test for dimensionality, SRD-based clustering of interpreted categories, overdispersion diagnostics, and optional adjusted standardised residuals.

### Updates
- Method Details section now includes explanation of validity assessment criteria and decision rules.
- Gambirasio reference corrected from placeholder to full OSF preprint citation.
- NAMESPACE now includes ggrepel import for CA plot labels.
- 00refs.yaml extended with CA-related references (Beh 2012, Beh et al. 2018, Borg & Groenen 2005, Greenacre 2017, Saporta 2006, Yelland 2010).

### Removed
- Redundant recommendation footnote from Test Results table (guidance now provided by Test Validity Assessment table).

## Version 1.0.0

- Initial release.
