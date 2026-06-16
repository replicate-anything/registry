# Convert skip_self_run footers to: libraries at top, functions, execute line.
param(
  [string]$Root = (Join-Path (Split-Path $PSScriptRoot -Parent) "papers")
)

$footerPattern = '(?ms)\r?\nif \(!isTRUE\(getOption\(''replicateEverything\.skip_self_run'', FALSE\)\)\) \{(.*)\r?\n\} else \{\r?\n  generate_(table|figure) <- make_[^\r\n]+\r?\n\}\s*$'
$files = Get-ChildItem -Path $Root -Recurse -Filter *.R | Where-Object {
  (Get-Content -LiteralPath $_.FullName -Raw) -match 'skip_self_run'
}

foreach ($file in $files) {
  $content = Get-Content -LiteralPath $file.FullName -Raw
  $match = [regex]::Match($content, $footerPattern)
  if (-not $match.Success) {
    Write-Warning "Could not parse footer: $($file.FullName)"
    continue
  }

  $body = $match.Groups[1].Value
  $libs = [regex]::Matches($body, '(?m)^\s*library\(([^)]+)\)\s*$') |
    ForEach-Object { $_.Groups[1].Value.Trim() } |
    Select-Object -Unique

  $runMatch = [regex]::Match($body, 'generate_(?:table|figure)\s*<-\s*(make_[A-Za-z0-9_]+)\((.+)\)\s*$')
  if (-not $runMatch.Success) {
    Write-Warning "Could not parse run line: $($file.FullName)"
    continue
  }

  $makeCall = $runMatch.Groups[1].Value
  $dataExpr = $runMatch.Groups[2].Value.Trim()
  $dataExpr = [regex]::Replace($dataExpr, 'file\.path\(paper_dir,\s*"([^"]+)"\)', '"../$1"')

  $newContent = $content.Substring(0, $match.Index)
  $newContent = $newContent -replace '# Requires the data/ folder alongside code/ \(see replication\.yml\)\.\r?\n', ''
  $newContent = $newContent -replace 'Rscript <this-file>\.R', ("Rscript " + $file.Name)

  $lines = ($newContent -split "\r?\n")
  $insertAt = 0
  for ($i = 0; $i -lt $lines.Count; $i++) {
    if ($lines[$i] -match '^#') { $insertAt = $i + 1 } elseif ($lines[$i].Trim() -ne '') { break }
  }
  while ($insertAt -lt $lines.Count -and $lines[$insertAt].Trim() -eq '') { $insertAt++ }

  $existingLibs = [regex]::Matches($newContent, '(?m)^library\(([^)]+)\)\s*$') |
    ForEach-Object { $_.Groups[1].Value.Trim() }
  $allLibs = ($existingLibs + $libs) | Select-Object -Unique
  $libBlock = @()
  if ($allLibs.Count -gt 0) {
    $libBlock = ($allLibs | ForEach-Object { "library($_)" }) + ""
  }

  $before = $lines[0..($insertAt - 1)]
  $after = if ($insertAt -lt $lines.Count) { $lines[$insertAt..($lines.Count - 1)] } else { @() }
  $after = $after | Where-Object { $_ -notmatch '^library\(' }
  $newLines = @($before + $libBlock + $after + "" + "$makeCall($dataExpr)")
  Set-Content -LiteralPath $file.FullName -Value ($newLines -join "`n")
  Write-Output $file.FullName
}
