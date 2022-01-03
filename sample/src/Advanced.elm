module Advanced exposing (Event, Memory, main)

import Advanced.GoatCard as GoatCard
import Html
import Html.Attributes as Attributes exposing (style)
import Html.Events as Events
import Html.Keyed as Keyed
import Thread.Browser as Browser exposing (Document, Program)
import Thread.LocalMemory as LocalMemory exposing (LocalMemory)
import Thread.Procedure as Procedure exposing (Block, Msg, Procedure, ThreadId)
import Thread.Wrapper exposing (Wrapper)


main : Program () Memory Event
main =
    Browser.document
        { init = init
        , procedures = procedures
        , view = view
        , subscriptions = Browser.globalSubscriptions subscriptions
        }


{-| The memory state shared by all threads.
-}
type alias Memory =
    { cards : LocalMemory GoatCard.Memory
    , log : String
    }


init : Memory
init =
    { cards = LocalMemory.init
    , log = ""
    }


{-| Events that only affect a specific thread.
-}
type Event
    = GoatCardEvent GoatCard.Event
    | ClickAddGoatCard



-- View


view : ThreadId -> Memory -> Document (Msg Event)
view tid shared =
    { title = "Advanced sample app"
    , body =
        [ Html.div
            [ style "display" "inline-block"
            , style "padding" "0.4em"
            , style "margin" "0"
            ]
            [ Html.div
                [ style "padding" "0.4em"
                ]
                [ Html.button
                    [ Attributes.type_ "button"
                    , Events.onClick
                        (ClickAddGoatCard
                            |> Procedure.setTarget tid
                        )
                    ]
                    [ Html.text "Add new card"
                    ]
                ]
            , Keyed.node "div"
                [ style "padding" "0.4em"
                ]
                (LocalMemory.toList shared.cards
                    |> List.reverse
                    |> List.map
                        (\( rid, goatCard ) ->
                            ( Procedure.stringifyThreadId rid
                            , GoatCard.view goatCard
                                |> Html.map
                                    (Procedure.setTarget rid << GoatCardEvent)
                            )
                        )
                )
            , Html.pre
                []
                [ Html.text shared.log
                ]
            ]
        ]
    }



-- Subsctiption


subscriptions : Memory -> Sub Event
subscriptions _ =
    Sub.none



-- Procedure


procedures : () -> Block Memory Event
procedures () _ =
    [ Procedure.await <|
        \event _ ->
            case event of
                ClickAddGoatCard ->
                    [ LocalMemory.async
                        { get = .cards >> Just
                        , set = \cards shared -> { shared | cards = cards }
                        }
                        GoatCard.init <|
                        ( \lift _ ->
                            [ receiveSaveNewGoatProcedures
                                |> Procedure.async
                            , GoatCard.procedures
                                |> Procedure.wrapBlock goatCardWrapper
                                |> lift
                                |> Procedure.jump
                            ]
                        )
                    ]

                _ ->
                    []
    , Procedure.jump <| procedures ()
    ]


receiveSaveNewGoatProcedures : Block Memory Event
receiveSaveNewGoatProcedures _ =
    [ Procedure.await <|
        \event_ _ ->
            case goatCardWrapper.unwrap event_ of
                Just (GoatCard.ReceiveSaveNewGoat result) ->
                    [ putLog <|
                        case result of
                            Ok _ ->
                                "Successfully saved"
                            Err _ ->
                                "Failed to save"
                    ]

                _ ->
                    []
    , Procedure.jump <| receiveSaveNewGoatProcedures
    ]


putLog : String -> Procedure Memory Event
putLog log =
    Procedure.modify <| \memory -> { memory | log = memory.log ++ log ++ "\n" }


goatCardWrapper : Wrapper Event GoatCard.Event
goatCardWrapper =
    { unwrap =
        \event ->
            case event of
                GoatCardEvent card ->
                    Just card

                _ ->
                    Nothing
    , wrap = GoatCardEvent
    }
