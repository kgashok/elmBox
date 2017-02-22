port module Drop exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Dom exposing (..)
import Task exposing (..)
import Result exposing (..)
import Time exposing (..)
import Date exposing (..)
import Date.Format exposing (..)
import ElmEscapeHtml exposing (..)
import Markdown exposing (..)
import Version exposing (..)
import Json.Encode exposing (..) 


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { filePath : String
    , dropURL : String
    , contents : String
    , status : String
    , currentTime : Maybe Time
    , flashMessage : String
    }


dropboxAPI : String
dropboxAPI =
    "https://content.dropboxapi.com/2"


initialModel : Model
initialModel =
    Model filePath
        dropboxAPI
        ""
        ""
        Nothing
        "Logger Ready"


init : ( Model, Cmd Msg )
init =
    ( initialModel
    , getFile initialModel
    )



-- UPDATE


type Msg
    = Refresh
    | Download (Result Http.Error ( Time, String ))
    | Append
    | GetTimeAndAppend Time
    | UpdateStatus String
    | Upload
    | UploadStatus (Result Http.Error ( Time, String ))
    | FocusDone (Result Dom.Error ())
    | GetTime
    | NewTime Time


port adjustTextAreaHeight : String -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Refresh ->
            { model | contents = "" }
                ! [ getFile model, Task.perform NewTime Time.now ]

        Download (Ok ( time, contents )) ->
            (model
                |> updateContents contents
                |> setTime time
                |> setFlashMessage "Download successful!"
            )
                ! [ focusUpdate ]

        Download (Err error) ->
            setFlashMessage (toString error) model ! []

        Append ->
            model ! [ Task.perform GetTimeAndAppend Time.now ]

        GetTimeAndAppend time ->
            (model
                |> setTime time
                |> appendStatus
                |> setFlashMessage "Append successful!"
            )
                ! [ focusUpdate ]

        UpdateStatus s ->
            { model | status = s }
                ! [ adjustTextAreaHeight "height-adjusting-textarea" ]

        Upload ->
            model ! [ sendFile model ]

        UploadStatus (Ok ( time, contents )) ->
            (model
                |> setTime time
                |> setFlashMessage "Upload successful!"
            )
                ! [ focusUpdate ]

        UploadStatus (Err error) ->
            (model
                |> setFlashMessage (toString error)
            )
                ! []

        GetTime ->
            model ! [ Task.perform NewTime Time.now ]

        NewTime time ->
            setTime time model ! [ focusUpdate ]

        FocusDone _ ->
            model ! []


focusUpdate : Cmd Msg
focusUpdate =
    Task.attempt FocusDone (Dom.focus "update")


setFlashMessage : String -> Model -> Model
setFlashMessage message model =
    { model | flashMessage = message }


setTime : Time -> Model -> Model
setTime time model =
    { model | currentTime = Just time }


appendStatus : Model -> Model
appendStatus model =
    { model | contents = (timedStatus model) ++ model.contents }


updateContents : String -> Model -> Model
updateContents contents model =
    { model | contents = unescape contents }


formatTime : Maybe Time -> String
formatTime time =
    time |> Maybe.withDefault 0 |> fromTime |> format "%a %b/%d/%y %H:%M:%S "


timedStatus : Model -> String
timedStatus model =
    formatTime model.currentTime ++ "\t" ++ model.status ++ " @@@\n"



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ class "example example-dotted" ]
            [ h1 [] [ text "Daily Log" ]
            , footer
            , hr [ class "style5" ] []
            , button [ id "button1", onClick Refresh ] [ text "Refresh!" ]
            , br [] []
            , div [] [ viewContents model.contents ]
            ]
        , div [ id "titleContainer" ]
            [ hr [ class "style8" ] []
            , h3 [] [ text <| formatTime model.currentTime ++ model.flashMessage ]
            , textarea
                [ class "height-adjusting-textarea"
                , id "update"
                , placeholder "Update?"
                , onInput UpdateStatus
                ]
                []
            , button [ id "button2", onClick Append ] [ text "Append" ]
            , button [ id "button3", onClick Upload ] [ text "Upload!" ]
            , footer
            ]
        ]


viewContents : String -> Html Msg
viewContents contents =
    let
        render material =
            let
                tuple =
                    String.split "\t" material
            in
                case tuple of
                    ts :: [ line ] ->
                        div [ class "answer" ]
                            [ ul [] [ text ts ]
                            , Markdown.toHtml [] line
                            ]

                    _ ->
                        Markdown.toHtml [ class "answer" ] material
    in
        contents
            |> String.split "@@@\n"
            |> List.map render
            |> List.take 46
            |> List.reverse
            >> div []


footer : Html Msg
footer =
    div [ id "footer" ]
        [ a
            [ href (gitRepo ++ "/issues/new")
            , target "_blank"
            , rel "noopener noreferrer"
            ]
            [ text version ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


getFile : Model -> Cmd Msg
getFile model =
    let
        downloadURL =
            model.dropURL ++ "/files/download"

        settings =
            { postSettings | url = downloadURL }

        getTask =
            Http.toTask (Http.request settings)
    in
        Time.now
            |> Task.andThen (\t -> Task.map ((,) t) getTask)
            |> Task.attempt Download



--  Http.send Download (Http.request settings)


sendFile : Model -> Cmd Msg
sendFile model =
    let
        uploadURL =
            dropboxAPI ++ "/files/upload"

        settings =
            { postSettings
                | url = uploadURL
                , headers = uploadHeaders
                , body = stringBody "application/octet-stream" model.contents
            }

        getTask =
            Http.toTask (Http.request settings)
    in
        Time.now
            |> Task.andThen (\t -> Task.map ((,) t) getTask)
            |> Task.attempt UploadStatus



-- Http.toTask (Http.request settings)
--   |> Task.andThen (\req -> Task.map (\t -> (t, req)) Time.now)
--   |> Task.attempt UploadStatus


filePath : String
filePath =
    "/Apps/elmBox/body.txt"

downloadArgs : List (String, Value) 
downloadArgs = [("path", string filePath)] 

uploadArgs : List (String, Value) 
uploadArgs = 
  downloadArgs ++ [("mode", string "overwrite")]

stringify: List ( String, Value ) -> String
stringify = 
  Json.Encode.object >> Json.Encode.encode 0 

downloadHeaders : List Header
downloadHeaders =
    [ Http.header "Authorization" "Bearer 4bhveELh1l8AAAAAAAAg1hjS4PUDWf0EeED2cIsmOsdJE04uqkichInc0sN0QZao"
    , Http.header "Dropbox-API-Arg" (stringify downloadArgs)
    ]


uploadHeaders : List Header
uploadHeaders =
    [ Http.header "Authorization" "Bearer 4bhveELh1l8AAAAAAAAg1hjS4PUDWf0EeED2cIsmOsdJE04uqkichInc0sN0QZao"
    , Http.header "Dropbox-API-Arg" (stringify uploadArgs)
    ]


postSettings :
    { body : Body
    , expect : Expect String
    , headers : List Header
    , method : String
    , timeout : Maybe a
    , url : String
    , withCredentials : Bool
    }
postSettings =
    { method = "POST"
    , headers = downloadHeaders
    , url = ""
    , body = emptyBody
    , expect = expectString
    , timeout = Nothing
    , withCredentials = False
    }



{--
encodeContents : String -> Encode.Value
encodeContents contents =
  Encode.object
    [ ("data", Encode.string contents)]


decodeResponse : Decode.Decoder String
decodeResponse =
  Decode.string

--}
