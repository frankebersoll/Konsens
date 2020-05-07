param(
    [switch] $Release
)

if (-not (dotnet tool list | Select-String "^fake-cli")) 
{
    dotnet tool restore
}

$args = @()
if ($Release.IsPresent) {
    $args += "-r"
}

dotnet fake --silent build @args