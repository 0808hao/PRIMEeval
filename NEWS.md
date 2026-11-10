# PRIMEeval 0.8.0.9001

* Adds deterministic affine continuation and the matched boundary-greedy
  control from Step 93 through the existing streaming API.
* Adds the Step 94 guarded affine scheduler and dataset diagnostics via
  `prime_gate()`. Pilot labels are charged, and the gate freezes before the
  first non-pilot request.
* Retains the 0.7.0 default and supports existing 0.7 stream states.
* This release candidate freezes the audit implementation for package-level
  validation. The HMC REM prediction workflow is separate.

# PRIMEeval 0.7.0

* Introduces the standalone sequential paired-loss audit API developed in
  PRIME steps 85--88: WIDTH sampling, OFFSET prediction, LEGACY evidence,
  pilot size two, one-half confirmation reserve and SUPPORT task allocation.
* Exposes request/update operation, callback execution, trace inspection,
  resumable RDS states and budget accounting by dataset.
* Supports explicit frozen historical fits and step-specific calibration cuts,
  or fitting from separate historical training and calibration task families.
* Retains historical augmented query-value allocation as an experimental option.
* Separates deterministic support certificates from development e-value flags.
* This focused API does not include the old 0.6.0 perturbation/retraining
  workflows. Use a separate R library for 0.6.0 if those workflows are needed.
