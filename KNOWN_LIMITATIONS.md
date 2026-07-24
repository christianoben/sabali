# Known limitations

- The Shiny app is a guided analysis aid, not a substitute for study-design
  review or a statistician's judgment.
- AI features are optional and disabled without an endpoint. External endpoint
  retention and training policies must be approved separately.
- Optional reference packages (for example `ARTool`, `rcompanion`, `DescTools`,
  and `GGally`) are used only when installed; otherwise the app uses its
  documented fallback or reports that the method is unavailable.
- Very large GLMM, GEE, penalised-regression, and Cox fits may take substantial
  time. The app displays progress but does not cancel a running fit.
- The upload cap is 200 MiB and imported files must contain at least two useful
  columns. Constant, all-missing, and invalid-UTF-8 columns are rejected.
- A production deployment still needs platform-level authentication,
  authorization, HTTPS, monitoring, backups, and incident response.
- Human statistician and security/privacy sign-offs are pending until the
  completed records are entered in `DATA_GOVERNANCE.md`.
