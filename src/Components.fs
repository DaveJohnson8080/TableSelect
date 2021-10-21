namespace App

open Feliz
open Feliz.Router
open Feliz.MaterialUI
open Feliz.MaterialUI.MaterialTable
open Fable.MaterialUI.Icons

type RowData = {
    Id : string
    Description : string
    Selected : bool
}

type Components =
    /// <summary>
    /// The simplest possible React component.
    /// Shows a header with the text Hello World
    /// </summary>
    [<ReactComponent>]
    static member HelloWorld() = Html.h1 "Hello World"

    /// <summary>
    /// A stateful React component that maintains a counter
    /// </summary>
    [<ReactComponent>]
    static member Counter() =
        let (count, setCount) = React.useState(0)
        Html.div [
            Html.h1 count
            Html.button [
                prop.onClick (fun _ -> setCount(count + 1))
                prop.text "Increment"
            ]
        ]

    /// <summary>
    /// A React component that uses Feliz.Router
    /// to determine what to show based on the current URL
    /// </summary>
    [<ReactComponent>]
    static member Router() =
        let (currentUrl, updateUrl) = React.useState(Router.currentUrl())
        React.router [
            router.onUrlChanged updateUrl
            router.children [
                match currentUrl with
                | [ ] -> Html.h1 "Index"
                | [ "hello" ] -> Components.HelloWorld()
                | [ "counter" ] -> Components.Counter()
                | otherwise -> Html.h1 "Not found"
            ]
        ]

    [<ReactComponent>]
    static member ListSelect(props : 
                                {| 
                                    Title : string 
                                    Options : 
                                        {| 
                                            Id : string
                                            Description : string
                                        |} list
                                    SelectedIds : string list
                                    MaxSelections : int option
                                |},
                                (onUpdated : string list -> unit)) =

        let useStyles = Styles.makeStyles(fun styles theme ->
            {|
                dialog = styles.create [
                    style.width (length.px 2000)
                ]

                table = styles.create [
                    style.minWidth (length.px 500)
                ]

                okButton = styles.create [
                    style.alignContent.flexStart
                ]

                revertButton = styles.create [
                    style.float'.right
                ]

                label = styles.create [
                    style.zIndex 1
                    style.custom ("pointer-events", "unset!important")
                    style.cursor.pointer
                ]

                textValue = styles.create [
                    style.cursor.pointer
                ]

                popover = styles.create [
                    style.cursor.pointer
                ]

                adorn = styles.create [
                    style.zIndex 1
                    style.color theme.palette.text.primary
                    style.cursor.pointer
                ]

                adornLink = styles.create [
                    style.zIndex 1
                    style.cursor.pointer
                ]
            |}
        )

        let c = useStyles()

        let originalSelections = props.SelectedIds
        let currentSelections, updateSelections = React.useState(originalSelections)
        let dialogOpen, setDialogOpen = React.useState(true)
        let popoverRef = React.useRef(null)
        let popoverOpen, setPopoverOpen = React.useState(false)
        let changeStack, setChangeStack =
            React.useState(
                {|
                    Current = currentSelections
                    Previous = None
                |}
        )
        let getOptionFromId id =
            props.Options
            |> List.tryFind (fun x ->
                x.Id = id
            )

        let getTopItem selections =
            match selections with
            | [h] ->
                getOptionFromId h
            | _::t ->
                getOptionFromId t.Head
            | _ -> None

        let topItem, setTopItem = React.useState(originalSelections |> getTopItem)

        let pushChange (selections : string list) =
            updateSelections selections
            {|
                changeStack with
                    Current = selections
                    Previous = Some changeStack.Current
            |}
            |> setChangeStack

        let popChange () =
            match changeStack.Previous with
            | None -> ()
            | Some prev ->
                updateSelections prev
                {|
                    changeStack with
                        Current = prev
                        Previous = None
                |}
                |> setChangeStack

        let onCheckChanged id isChecked =
            let found =
                currentSelections
                |> List.tryFind (fun x -> x = id)
            match found, isChecked, props.MaxSelections with
            | Some f, true, _ -> ()
            | Some f, false, _ ->
                let updatedSelections =
                    currentSelections
                    |> List.where (fun x -> x <> id)
                    |> List.sort
                pushChange updatedSelections
            | None, true, Some max ->
                match max with
                | 1 ->
                    let updatedSelections = [ id ]
                    pushChange updatedSelections
                | _ ->
                    if currentSelections.Length < max then
                        let udpatedSelections = id :: currentSelections |> List.sort
                        pushChange udpatedSelections
                    else
                        ()
            | _ -> ()

        let toListData selections =
            props.Options
            |> List.map (fun x ->
                {|
                    Id = x.Id
                    Name = x.Description
                    Selected =
                        selections
                        |> List.contains (x.Id)
                |}
            )

        let totalSelected =
            originalSelections.Length

        let makeListItem id selected (data : string) =
            Mui.listItem [
                Mui.checkbox [
                    checkbox.checked' selected
                    checkbox.onChange (fun b -> onCheckChanged id b)
                ]

                Mui.typography [
                    prop.text (sprintf "%s. %s" id data)
                ]
            ]

        let dialog =
            Mui.dialog [
                prop.className c.dialog
                dialog.fullWidth true
                dialog.open' dialogOpen
                dialog.children [
                    Mui.typography [
                        typography.variant.h4
                        typography.align.center
                        typography.children [
                            Html.text props.Title
                        ]
                    ]
                    match props.MaxSelections with
                    | Some max when max > 1 ->
                        Mui.typography [
                            typography.variant.body1
                            typography.align.center
                            typography.children [
                                Html.text (sprintf "(%i/%i)" currentSelections.Length max)
                            ]
                        ]
                    | _ -> ()
                    Mui.divider []
                    Mui.list [
                        list.disablePadding true
                        list.dense true
                        list.children (
                            currentSelections
                            |> toListData
                            |> List.map (fun x ->
                                makeListItem x.Id x.Selected x.Name
                            )
                        )
                    ]
                    Mui.grid [
                        grid.container true
                        grid.children [
                            Mui.grid [
                                grid.item true
                                grid.xs._6
                                grid.children [
                                    Mui.iconButton [
                                        prop.onClick (fun _ -> 
                                            onUpdated currentSelections
                                            setDialogOpen false
                                        )
                                        iconButton.children [
                                            checkIcon [
                                                icon.fontSize.large
                                                icon.color.primary
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                            Mui.grid [
                                grid.item true
                                grid.xs._6
                                grid.children [
                                    Mui.iconButton [
                                        prop.className c.revertButton
                                        prop.onClick (fun _ -> popChange())
                                        iconButton.children [
                                            replayIcon [
                                                icon.fontSize.large
                                                icon.color.primary
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]

        let popover =
            Mui.popover [
                prop.id "list-select-popover"
                prop.className c.popover
                popover.anchorEl (Some popoverRef.current)
                popover.open' popoverOpen
                popover.anchorOrigin.topLeft
                popover.transformOrigin.topLeft
                popover.disableRestoreFocus true
                prop.children [
                    Html.div [
                        prop.onMouseLeave (fun _ -> setPopoverOpen false)
                        prop.onClick (fun _ -> 
                            setDialogOpen true
                            setPopoverOpen false
                        )
                        prop.children [
                            Mui.list [
                                list.disablePadding true
                                list.dense true
                                list.children (
                                    let instructionItem =
                                        Mui.listItem [
                                            listItem.dense true
                                            listItem.disableGutters true
                                            listItem.children [
                                                Mui.typography [
                                                    prop.className c.textValue
                                                    typography.variant.subtitle1
                                                    typography.children ("Click to Edit")
                                                    typography.color.primary
                                                ]
                                            ]
                                        ]
                                    let selectionItems =
                                        props.Options
                                        |> List.where (fun x ->
                                            currentSelections
                                            |> List.contains (x.Id)
                                        )
                                        |> List.map (fun x ->
                                            Mui.listItem [
                                                listItem.children [
                                                    Mui.typography [
                                                        prop.className c.textValue
                                                        typography.variant.body1
                                                        typography.children
                                                            (sprintf "%s. %s" x.Id x.Description)
                                                    ]
                                                ]
                                            ]
                                        )
                                    instructionItem :: selectionItems
                                )
                            ]
                        ]
                    ]

                ]
            ]

        let textFieldId = System.Guid.NewGuid().ToString()

        let textField =
            Mui.textField [
                prop.id textFieldId
                prop.ariaOwns [| "list-select-popover" |]
                prop.ref (fun r -> popoverRef.current <- r)
                prop.ariaHasPopup true
                prop.className c.textValue
                textField.disabled true
                textField.label [
                    Mui.link [
                        prop.text props.Title
                        prop.onClick (fun _ -> setDialogOpen true)
                    ]
                ]
                textField.variant.outlined
                textField.InputLabelProps [
                    prop.className c.label
                ]
                textField.fullWidth true
                prop.onClick (fun _ -> setDialogOpen true)
                textField.InputProps [
                    input.startAdornment (
                        Mui.inputAdornment [
                            inputAdornment.position.start
                            inputAdornment.children [
                                Mui.typography [
                                    prop.className c.adorn
                                    typography.children (
                                        match topItem with
                                        | Some item ->
                                            let shortenedDesc =
                                                if item.Description.Length > 50 then
                                                    item.Description.Remove(50)
                                                else item.Description
                                            if totalSelected - 1 > 0 then
                                                sprintf "%s. %s ... (+ %i more)" item.Id shortenedDesc (totalSelected - 1)
                                            else
                                                sprintf "%s. %s ..." item.Id shortenedDesc
                                        | None -> "..."
                                    )
                                ]
                            ]
                        ]
                    )
                    input.endAdornment (
                        Mui.inputAdornment [
                            inputAdornment.position.end'
                            inputAdornment.children [
                                Mui.link [
                                    prop.className c.adornLink
                                    link.children (
                                        match props.MaxSelections with
                                        | Some max ->
                                            sprintf "%i/%i" originalSelections.Length max
                                        | None -> "..."
                                    )
                                ]
                            ]
                        ]
                    )
                ]
            ]

        Html.div [
            dialog
            textField
        ]

    [<ReactComponent>]
    static member Root () =
        let initialState =
            {|
                Title = "Protective Devices"
                Options = [
                    {|
                        Id = "1"
                        Description = "None"
                    |}
                    {|
                        Id = "2"
                        Description = "Lap Belt"
                    |}
                    {|
                        Id = "3"
                        Description = "Personal Floatation Device"
                    |}
                    {|
                        Id = "4"
                        Description = "Protective Non-Clothing Gear (e.g., shin guard)"
                    |}
                    {|
                        Id = "5"
                        Description = "Eye Protection"
                    |}
                    {|
                        Id = "6"
                        Description = "Child Restraint(booster seat or child car seat)"
                    |}
                    {|
                        Id = "7"
                        Description = "Helmet (e.g., bicycle, skiing, motorcycle)"
                    |}
                    {|
                        Id = "8"
                        Description = "Airbag Present"
                    |}
                    {|
                        Id = "9"
                        Description = "Protective Clothing (e.g., padded leather pants)"
                    |}
                    {|
                        Id = "10"
                        Description = "Shoulder Belt"
                    |}
                    {|
                        Id = "11"
                        Description = "Other"
                    |}
                    {|
                        Id = "50"
                        Description = "Hard Hat"
                    |}
                    {|
                        Id = "51"
                        Description = "Safety Belt"
                    |}
                    {|
                        Id = "NK/NR"
                        Description = "Not Known/Not Recorded"
                    |}
                ]
                SelectedIds = [ "2"; "10" ]
                MaxSelections = Some 10
            |}
        let state, setState = React.useState(initialState)
        let handleUpdate (ids : string list) =
            {|
                state with
                    SelectedIds = ids
            |}
            |> setState

        Html.div [
            Html.div [
                Components.ListSelect(
                    {|
                        Title = "Initial ED/Hospital GCS-Eye"
                        Options = [
                            {|
                                Id = "1"
                                Description = "No eye movement when assessed"
                            |}
                            {|
                                Id = "2"
                                Description = "Opens eyes in response to painful stimulation"
                            |}
                            {|
                                Id = "3"
                                Description = "Opens eyes in response to verbal stimulation"
                            |}
                            {|
                                Id = "4"
                                Description = "Opens eyes spontaneously"
                            |}
                            {|
                                Id = "NK/NR"
                                Description = "Not Known/Not Recorded"
                            |}
                        ]
                        SelectedIds = [ "2" ]
                        MaxSelections = Some 1
                    |},
                    ignore
                )
            ]
            Html.p []
            Html.div [
                Components.ListSelect(
                    state,
                    handleUpdate
                )
            ]
        ]