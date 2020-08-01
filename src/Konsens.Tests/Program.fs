open Konsens.Tests.Infrastructure
open canopy.runner.classic
open canopy.classic
open canopy.types

canopy.configuration.chromeDir <- System.AppContext.BaseDirectory
canopy.configuration.autoPinBrowserRightOnLaunch <- false

//reporter <- new LiveHtmlReporter(Chrome, canopy.configuration.chromeDir) :> IReporter
//let liveHtmlReporter = reporter :?> LiveHtmlReporter
//liveHtmlReporter.reportPath <- Some "reports/AutomationResults"

start chrome

pin FullScreen

module HomeScreen =
    open XPath

    let private codeEntryButton = tag "span" |> withText "Code eingeben"
    let private codeEntry = tag "input" |> withPlaceholder "Code"

    let enterCode code =
        click codeEntryButton
        codeEntry << code

module AbstimmungScreen =
    open XPath

    let private titel = tag "h1" |> withClass "h1"

    let private code =
        tag "div"
        |> withClass "container"
        |> inner "h2"

    let getTitelAndCode () = (titel |> read, code |> read)

module NeuScreen =
    open XPath

    let private titel = inputWithLabel "Titel"
    let private neuerVorschlag = inputWithP "Vorschläge"

    let private dragHandle i =
        tag "span"
        |> withClass "items-panel__drag-handle"
        |> withClass "icon"
        |> index i

    let private anlegenButton = tag "button" |> withText "Anlegen"

    let private deleteHandle i =
        tag "button"
        |> withClass "delete"
        |> index i

    let setTitel text =
        titel << text

    let addVorschlag text =
        neuerVorschlag << text
        press enter

    let moveVorschlag von nach = dragHandle von --> dragHandle nach

    let deleteVorschlag i = deleteHandle i |> click

    let anlegen () =
        click anlegenButton
        AbstimmungScreen.getTitelAndCode ()

module NeueAbstimmungTests =
    open NeuScreen

    context "Neue Abstimmung"

    "Anlegen und per Code aufrufen" &&& fun _ ->

        url "http://localhost/"

        click "Neue Abstimmung"

        setTitel "Halli Hallo"
        addVorschlag "Blah"
        addVorschlag "Blih"
        addVorschlag "Blubb"

        moveVorschlag 2 1

        deleteVorschlag 3

        let (titel, code) = anlegen ()

        describe ("Angelegt: " + titel)
        describe ("Mit Code: " + code)

        titel === "Halli Hallo"

        url "http://localhost/"

        HomeScreen.enterCode code

        let (titel, _) = AbstimmungScreen.getTitelAndCode ()

        titel === "Halli Hallo"

run ()
quit ()