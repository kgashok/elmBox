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

port setStorage : Model -> Cmd msg

port adjustTextAreaHeight : String -> Cmd msg

port logExternalOut : String -> Cmd msg

logExternal : a -> Cmd msg
logExternal value =
  logExternalOut (toString value)

updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
  let
    ( nextModel, nextCmd ) =
      update msg model
  in
    ( nextModel
    , Cmd.batch
      [ setStorage model
      -- , logExternal msg
      , nextCmd
      ]
    )

main : Program (Maybe Model) Model Msg
main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = updateWithStorage 
    , subscriptions = subscriptions
    }

{--
main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
--}

-- MODEL


type Content
    = NoContent
    | FileContent String
    | FileContentToUpload String (List Post)


type alias Post =
    { timestamp : Time
    , message : String
    }


type alias Model =
    { filePath : String
    , dropURL : String
    , contents : String
    , postsToUpload : Maybe String
    , status : String
    , currentTime : Maybe Time
    , flashMessage : String
    , downloadSuccess : Bool
    , downloadFirst : Bool
    }


dropboxAPI : String
dropboxAPI =
    "https://content.dropboxapi.com/2"


initialModel : Model
initialModel =
    Model filePath
        dropboxAPI
        ""
        -- contents
        Nothing
        -- postsToUpload
        ""
        -- status
        Nothing
        "Logger Ready"
        False
        False


init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
  ( Maybe.withDefault initialModel savedModel, 
    getTimeTask)

{--

init : ( Model, Cmd Msg )
init =
    ( initialModel
      -- , getFile initialModel
    , getTimeTask
    )

--}


-- UPDATE


type Msg
    = Refresh
    | Download (Result Http.Error ( Time, String ))
    | DownloadAndAppend (Result Http.Error ( Time, String ))
    | Append
    | GetTimeAndAppend Time
    | UpdateStatus String
    | Upload
    | UploadStatus (Result Http.Error ( Time, String ))
    | FocusDone (Result Dom.Error ())
    | GetTime
    | NewTime Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Refresh ->
            { model | contents = "", flashMessage = "Downloading...be patient!" }
                ! [ getFileTask model ]

        Download (Ok ( time, contents )) ->
            let
                model_ =
                    model
                        |> setTime time
                        |> updateContents contents
                        |> setFlag True
            in
                case ( model.downloadFirst, model.downloadSuccess ) of
                    ( False, _ ) ->
                        { model_ | flashMessage = "Download successful (case 1)" }
                            ! [ focusUpdate ]

                    ( True, False ) ->
                        let
                            model__ =
                                model_
                                    |> appendPosts
                                    |> setFlashMessage "Download successful! (case 2)"
                        in
                            model__ ! [ sendFileTask model__ ]

                    ( _, True ) ->
                        { model_ | flashMessage = "Download successful (case 3)" }
                            ! [ sendFileTask model ]

        Download (Err error) ->
            let
                model_ =
                    { model | downloadSuccess = False, downloadFirst = False }
            in
                setFlashMessage (toString error) model_ ! []

        DownloadAndAppend (Ok ( time, contents )) ->
            (model
                |> setTime time
                |> updateContents contents
                |> appendStatus
                |> setFlashMessage "Download/Append successful!"
                |> setFlag True
            )
                ! [ focusUpdate ]

        DownloadAndAppend (Err error) ->
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
                ! [ focusUpdate, adjustTextAreaHeight "height-adjusting-textarea" ]

        Upload ->
            let
                model_ =
                    model |> setFlashMessage "Uploading...please be patient!"
            in
                case model_.downloadSuccess of
                    True ->
                        model_ ! [ sendFileTask model ]

                    False ->
                        { model_ | downloadFirst = True }
                            ! [ getFileTask model_ ]

        UploadStatus (Ok ( time, contents )) ->
            (model
                |> setTime time
                |> setFlashMessage "Upload successful!"
                |> setDownloadFirst False
            )
                ! [ focusUpdate ]

        UploadStatus (Err error) ->
            (model
                |> setFlashMessage (toString error)
            )
                ! []

        GetTime ->
            model ! [ getTimeTask ]

        NewTime time ->
            (model |> setTime time) ! [ focusUpdate ]

        FocusDone _ ->
            model ! []


getTimeTask : Cmd Msg
getTimeTask =
    Task.perform NewTime Time.now


focusUpdate : Cmd Msg
focusUpdate =
    Task.attempt FocusDone (Dom.focus "update")


setFlashMessage : String -> Model -> Model
setFlashMessage message model =
    { model | flashMessage = message }


setTime : Time -> Model -> Model
setTime time model =
    { model | currentTime = Just time }


setFlag : Bool -> Model -> Model
setFlag flag model =
    { model | downloadSuccess = flag }


setDownloadFirst : Bool -> Model -> Model
setDownloadFirst flag model =
    { model | downloadFirst = flag }


appendStatus : Model -> Model
appendStatus model =
    case model.downloadSuccess of
        True ->
            { model | contents = (timedPost model) ++ model.contents }

        False ->
            let
                posts =
                    (timedPost model)
                        ++ (Maybe.withDefault "" model.postsToUpload)

                model_ =
                    { model | postsToUpload = Just posts }
            in
                --model_
                { model_ | contents = Maybe.withDefault "" model_.postsToUpload }


appendPosts : Model -> Model
appendPosts model =
    { model
        | contents = (Maybe.withDefault "" model.postsToUpload) ++ model.contents
        , postsToUpload = Nothing
    }


updateContents : String -> Model -> Model
updateContents contents model =
    { model | contents = unescape contents }


timedPost : Model -> String
timedPost model =
    formatTime model.currentTime ++ "\t" ++ model.status ++ " @@@\n"


formatTime : Maybe Time -> String
formatTime time =
    time |> Maybe.withDefault 0 |> fromTime |> format "%a %b/%d/%y %H:%M:%S "



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
                , value model.status
                ]
                []
            , button [ id "button2", onClick Append ] [ text "Append" ]
            , button [ id "button3", onClick Upload ] [ text "Upload!" ]
            , button [ id "button3", onClick (UpdateStatus "") ] [ text "Clear" ]
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


getFile : Model -> Http.Request String
getFile model =
    let
        downloadURL =
            model.dropURL ++ "/files/download"

        settings =
            { postSettings | url = downloadURL }
    in
        Http.request settings


getFileTask : Model -> Cmd Msg
getFileTask model =
    let
        getTask =
            Http.toTask (getFile model)
    in
        getTask
            |> Task.andThen
                (\result ->
                    Time.now
                        |> Task.andThen (\time -> Task.succeed ( time, result ))
                )
            |> Task.attempt Download



{--Time.now
            |> Task.andThen (\t -> Task.map ((,) t) getTask)
            |> Task.attempt Download
        --}


getFileAndAppend : Model -> Cmd Msg
getFileAndAppend model =
    let
        getTask =
            Http.toTask (getFile model)
    in
        Time.now
            |> Task.andThen (\t -> Task.map ((,) t) getTask)
            |> Task.attempt DownloadAndAppend



--  Http.send Download (Http.request settings)


sendFile : Model -> Maybe String -> Http.Request String
sendFile model posts =
    let
        uploadURL =
            dropboxAPI ++ "/files/upload"

        contents =
            model.contents ++ (Maybe.withDefault "" posts)

        settings =
            { postSettings
                | url = uploadURL
                , headers = uploadHeaders
                , body = stringBody "application/octet-stream" contents
            }
    in
        Http.request settings


sendFileTask : Model -> Cmd Msg
sendFileTask model =
    let
        sendTask =
            Http.toTask (sendFile model Nothing)
    in
        Time.now
            |> Task.andThen (\t -> Task.map ((,) t) sendTask)
            |> Task.attempt UploadStatus



-- Http.toTask (Http.request settings)
--   |> Task.andThen (\req -> Task.map (\t -> (t, req)) Time.now)
--   |> Task.attempt UploadStatus


filePath : String
filePath =
    "/Apps/elmBox/body.txt"


downloadArgs : List ( String, Value )
downloadArgs =
    [ ( "path", string filePath ) ]


uploadArgs : List ( String, Value )
uploadArgs =
    downloadArgs ++ [ ( "mode", string "overwrite" ) ]


stringify : List ( String, Value ) -> String
stringify =
    Json.Encode.object >> Json.Encode.encode 0


authorizationHeader : Header
authorizationHeader =
    Http.header "Authorization" "Bearer 4bhveELh1l8AAAAAAAAg1hjS4PUDWf0EeED2cIsmOsdJE04uqkichInc0sN0QZao"


downloadHeaders : List Header
downloadHeaders =
    [ authorizationHeader
    , Http.header "Dropbox-API-Arg" (stringify downloadArgs)
    ]


uploadHeaders : List Header
uploadHeaders =
    [ authorizationHeader
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
