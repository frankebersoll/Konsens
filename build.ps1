[CmdLetBinding (DefaultParameterSetName="Default")]
param(
    [Parameter(ParameterSetName="Watch")]
    [switch] $Watch,

    [Parameter(ParameterSetName="Default")]
    [switch] $Release
)

dotnet tool restore | Out-Null

$target = $PSCmdlet.ParameterSetName
$args = @()

if ($target -ne "Default") {
    $args += "-t"
    $args += $target
}

if ($Release.IsPresent) {
    $args += "-r"
}

dotnet fake --silent build @args