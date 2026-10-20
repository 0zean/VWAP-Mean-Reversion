# PLAN.md §0 done-gate: tests, lint, format, types. Stops at the first failure.
$ErrorActionPreference = "Stop"
Set-Location (Join-Path $PSScriptRoot "..")
$steps = @(
    @("uv", "run", "pytest", "-q"),
    @("uvx", "ruff", "check", "."),
    @("uvx", "ruff", "format", "--check", "."),
    @("uv", "run", "ty", "check")
)
foreach ($s in $steps) {
    Write-Host ">> $($s -join ' ')"
    & $s[0] $s[1..($s.Length - 1)]
    if ($LASTEXITCODE -ne 0) { Write-Host "done-gate FAILED at: $($s -join ' ')"; exit $LASTEXITCODE }
}
Write-Host "done-gate passed"
