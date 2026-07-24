# Security and dependency policy

Do not commit `.Renviron`, API keys, uploaded files, reports, or patient data.
Use the deployment platform's secret manager for production credentials.

Dependencies are recorded in `renv.lock`. Maintainers should run the pinned
restore, inspect `renv::status()`, and review package advisories before every
release. GitHub Actions and workflow dependencies are monitored by
`.github/dependabot.yml`; a release is blocked when a high-severity advisory
affects a runtime dependency until it is patched, removed, or formally
accepted by the security reviewer.

Report suspected vulnerabilities privately to the application owner rather
than opening an issue containing sensitive details.
