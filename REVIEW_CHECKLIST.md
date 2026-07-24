# Release and human-review checklist

This checklist separates executable gates from decisions that require a human
with the relevant authority.

## Required before production

- [ ] Data-governance sign-off completed in `DATA_GOVERNANCE.md`.
- [ ] Statistician reviewed guided recommendations, assumptions, and known
      limitations against representative study protocols.
- [ ] Security reviewer approved endpoint, authentication, outbound network,
      temporary-file, and secret-store configuration.
- [ ] Deployment smoke test completed with `shinytest2` on the target server.
- [ ] `Rscript tests/run_all.R` passes in the pinned dependency environment.
- [ ] Coverage target is met and the report is archived with the release.
- [ ] A maintainer creates and pushes a signed release tag after the project
      has been placed under the organisation's Git remote.

## Human review questions

1. Are the available methods and recommendation text appropriate for the
   organisation's actual study designs and data-generating processes?
2. Are the assumptions, missing-data handling, reference-level behaviour,
   effect sizes, confidence intervals, and interpretation wording acceptable?
3. Is every enabled AI endpoint approved for the highest data classification
   that users may upload?
4. Are authentication, authorization, HTTPS, backups, and incident response
   supplied by the deployment platform?
