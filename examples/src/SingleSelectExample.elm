module SingleSelectExample exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (Html, button, div, form, h1, input, p, text)
import Html.Attributes exposing (id, style)
import Html.Events exposing (onSubmit)
import Html.Styled
import Json.Decode as Decode
import Select
import SingleSelect
import SmartSelect.Settings exposing (defaultSettings)


type alias Product =
    { id : Int
    , name : String
    , price : String
    }


type Country
    = Australia
    | Japan
    | Taiwan


type alias Model =
    { products : List Product
    , select : SingleSelect.SmartSelect Msg Product
    , selectedProduct : Maybe Product
    , wasFormSubmitted : Bool
    , selectState : Select.State
    , items : List (Select.MenuItem Country)
    , selectedCountry : Maybe Country
    }


type Msg
    = HandleSelectUpdate (SingleSelect.Msg Product)
    | HandleSelection ( Product, SingleSelect.Msg Product )
    | HandleFormSubmission
    | OnViewChange
    | SelectMsg (Select.Msg Country)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleSelectUpdate sMsg ->
            let
                ( updatedSelect, selectCmd ) =
                    SingleSelect.update sMsg model.select
            in
            ( { model | select = updatedSelect }, selectCmd )

        HandleSelection ( selection, sMsg ) ->
            let
                ( updatedSelect, selectCmd ) =
                    SingleSelect.update sMsg model.select
            in
            ( { model | selectedProduct = Just selection, select = updatedSelect }, selectCmd )

        HandleFormSubmission ->
            ( { model | wasFormSubmitted = True }, Cmd.none )

        OnViewChange ->
            let
                ( updatedSelect, selectCmd ) =
                    SingleSelect.updatePosition model.select
            in
            ( { model | select = updatedSelect }, selectCmd )

        SelectMsg selectMsg ->
            let
                ( maybeAction, updatedSelectState, selectCmds ) =
                    Select.update selectMsg model.selectState
            in
            ( { model | selectState = updatedSelectState }
            , Cmd.map SelectMsg selectCmds
            )


selectedCountryToMenuItem : Country -> Select.MenuItem Country
selectedCountryToMenuItem country =
    case country of
        Australia ->
            Select.basicMenuItem { item = Australia, label = "Australia" }

        Japan ->
            Select.basicMenuItem { item = Japan, label = "Japan" }

        Taiwan ->
            Select.basicMenuItem { item = Taiwan, label = "Taiwan" }


renderSelect : Model -> Html.Styled.Html (Select.Msg Country)
renderSelect model =
    Select.view
        ((Select.single <| Maybe.map selectedCountryToMenuItem model.selectedCountry)
            |> Select.state model.selectState
            |> Select.menuItems model.items
            |> Select.placeholder "Select your country"
        )


view : Model -> Html Msg
view model =
    div
        [ style "width" "100%"
        , style "height" "100vh"
        , style "padding" "3rem"
        , style "overflow" "auto"
        , Html.Events.on "scroll" (Decode.succeed OnViewChange)
        ]
        [ h1 [] [ text "SingleSelect Example" ]
        , div
            [ style "margin-bottom" "1rem"
            ]
            [ text "This form contains a single select with local search. We use a form here to demonstrate that the select key commands won't inadvertently impact form submission." ]
        , div [ id "form-submission-status", style "margin-bottom" "1rem" ]
            [ text
                (if model.wasFormSubmitted then
                    "Form submitted!"

                 else
                    "Press 'Enter' from input field or push the button below to submit form."
                )
            ]
        , div
            [ style "position" "fixed"
            , style "width" "100%"
            , style "top" "0"
            , style "left" "0"
            , style "height" "100%"
            , style "width" "100%"
            , style "z-index" "1"
            , style "background" "rgba(0,0,0,0.5)"
            , style "display" "flex"
            , style "justify-content" "center"
            , style "align-items" "center"
            ]
            [ div
                [ style "width" "500px"
                , style "height" "500px"
                , style "background" "white"
                , style "padding" "2rem"
                , style "overflow" "auto"
                ]
                [ form [ onSubmit HandleFormSubmission ]
                    [ input [ style "margin-bottom" "2rem" ] []
                    , p [] [ text "The select will automatically open to the top, if there is not enought space." ]
                    , div []
                        [ text "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet." ]
                    , div
                        [ style "margin-bottom" "1rem" ]
                        [ SingleSelect.view
                            { selected = model.selectedProduct
                            , options = model.products
                            , optionLabelFn = .name
                            , settings = defaultSettings
                            }
                            model.select
                        ]
                    , div
                        [ style "margin-bottom" "1rem" ]
                        [ Html.map SelectMsg (renderSelect model |> Html.Styled.toUnstyled) ]
                    , button [] [ text "Submit" ]
                    ]
                ]
            ]
        , div [ style "height" "100vh" ] []
        ]


exampleProducts : List Product
exampleProducts =
    [ { id = 1
      , name = "product 1"
      , price = "$3.00"
      }
    , { id = 2
      , name = "product 2"
      , price = "$5.00"
      }
    , { id = 3
      , name = "product 3"
      , price = "$7.00"
      }
    , { id = 4
      , name = "product 4"
      , price = "$3.00"
      }
    , { id = 5
      , name = "product 5"
      , price = "$5.00"
      }
    , { id = 6
      , name = "product 6"
      , price = "$7.00"
      }
    , { id = 7
      , name = "product 7"
      , price = "$3.00"
      }
    , { id = 8
      , name = "product 8"
      , price = "$5.00"
      }
    , { id = 9
      , name = "product 9"
      , price = "$7.00"
      }
    ]


init : ( Model, Cmd Msg )
init =
    ( { products = exampleProducts
      , select =
            SingleSelect.init
                { selectionMsg = HandleSelection
                , internalMsg = HandleSelectUpdate
                , idPrefix = "single-select"
                }
      , selectedProduct = Nothing
      , wasFormSubmitted = False
      , selectState =
            Select.initState (Select.selectIdentifier "CountrySelector")
      , items =
            [ Select.basicMenuItem
                { item = Australia, label = "Australia" }
            , Select.basicMenuItem
                { item = Japan, label = "Japan" }
            , Select.basicMenuItem
                { item = Taiwan, label = "Taiwan" }
            ]
      , selectedCountry = Nothing
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    SingleSelect.subscriptions model.select
