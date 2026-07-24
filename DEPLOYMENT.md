# Deploying Sabali from your own machine

This guide serves the app from your Mac so remote colleagues can test it while
you keep developing. Because Sabali can hold **real patient data**, the setup
below requires a login and an encrypted (HTTPS) connection for anything reaching
beyond your own machine.

> **Read this first (patient-data safety).** A plain `http://<ip>:<port>` link
> sends the login *and every uploaded row* across the network unencrypted.
> That is only acceptable on a trusted local network for non-sensitive data.
> For real patient data reachable over the internet you **must** use the HTTPS
> tunnel in Step 4. Complete the sign-off in `DATA_GOVERNANCE.md` before real
> data is used, and stop the server when you are not testing.

---

## One-time setup

### 1. Install the packages

```bash
Rscript install_dependencies.R
Rscript -e 'install.packages("shinymanager", repos="https://cloud.r-project.org")'
```

### 2. Choose a passphrase and create a login

The passphrase encrypts the credential store. Keep it secret; never commit it.

```bash
export SABALI_AUTH_PASSPHRASE="$(openssl rand -base64 24)"
export SABALI_AUTH_DB="sabali_auth.sqlite"
Rscript setup_auth.R          # prompts for username + password (>= 12 chars)
```

`setup_auth.R` scrypt-hashes each password and encrypts the store. Re-running it
**replaces** the whole user list, so enter every tester in one run (it asks
"Add another user?"). Give each person their own account and remove accounts
when testing ends.

### 3. Record the settings in `.Renviron`

Add these to the project `.Renviron` (already git-ignored) so the launcher picks
them up. Use the same passphrase you set above.

```
SABALI_AUTH_DB=sabali_auth.sqlite
SABALI_AUTH_PASSPHRASE=the-passphrase-you-generated
SABALI_HOST=127.0.0.1
SABALI_PORT=3838
```

---

## Running it

### Local check (only this machine)

```bash
Rscript run_server.R
```

Open `http://127.0.0.1:3838`, sign in, and confirm the app works.

### Step 3b — same office network (LAN)

Bind to all interfaces and find your machine's address:

```bash
SABALI_HOST=0.0.0.0 Rscript run_server.R
ipconfig getifaddr en0     # your LAN IP, e.g. 192.168.1.42 (try en1 on Wi-Fi)
```

Colleagues on the **same network** open `http://<that-ip>:3838`. macOS will ask
to allow incoming connections for R the first time — allow it. The launcher
refuses to bind to `0.0.0.0` unless a login is configured.

This LAN link is still **unencrypted**, so treat it as testing only unless the
network is fully trusted. For real patient data, use HTTPS (next step) even on a
LAN.

### Step 4 — over the internet, encrypted (recommended)

A tunnel gives you an `https://…` URL without opening router ports, and encrypts
everything in transit. Keep `run_server.R` running on `127.0.0.1:3838`, then in
a second terminal:

**Cloudflare Tunnel** (no account needed for a quick tunnel):

```bash
brew install cloudflared
cloudflared tunnel --url http://localhost:3838
```

It prints a `https://<random>.trycloudflare.com` URL — share that. The app's own
login is the access control. For a stable URL and an extra gate in front of the
app, use a named tunnel plus Cloudflare Access (needs a free Cloudflare account
and a domain).

**ngrok** is an alternative:

```bash
brew install ngrok      # then authenticate once: ngrok config add-authtoken <token>
ngrok http 3838
```

Stop the tunnel (Ctrl-C) when you finish a session; the URL then dies.

### Keep the Mac awake while serving

```bash
caffeinate -s Rscript run_server.R
```

---

## Security checklist for real patient data

- [ ] Reached only over **HTTPS** (tunnel in Step 4) — never a plain `http://`
      link across the internet.
- [ ] A **login** is configured (`run_server.R` enforces this off-machine).
- [ ] Each tester has their own account with a strong password (>= 12 chars);
      accounts removed when testing ends.
- [ ] `SABALI_AUTH_PASSPHRASE` is kept secret and only in `.Renviron` (ignored
      by git) — never in a commit or a chat message.
- [ ] FileVault disk encryption on and the screen locks when unattended.
- [ ] The server is **stopped** when not actively testing.
- [ ] AI features stay off unless an endpoint approved for this data class is
      configured (see `DATA_GOVERNANCE.md`).
- [ ] The `DATA_GOVERNANCE.md` sign-off is complete before real patient data is
      used.

What the app already does for you: uploads and per-session data are cleared when
a browser session ends, sessions are isolated per user, no patient rows are
written to disk, and `run_server.R` sanitises errors so raw messages and file
paths are never shown to users.

---

## When you outgrow the laptop

A laptop is fine for short, supervised testing. For ongoing or wider use, move to
a small server or cloud VM you control: run the same `run_server.R` there behind
a reverse proxy (Caddy or nginx) that terminates HTTPS with a real certificate,
enable `renv::restore()` for a reproducible install, and keep the login. A
container image can be added to make that move reproducible — ask if you want a
`Dockerfile` for it.
