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
import Markdown exposing (..)
import Version exposing (..)
import Json.Encode as Encode
import Json.Decode as Decode exposing (decodeString, field, string, list, dict)
import Json.Decode.Pipeline as Pipeline exposing (..)
import Dict exposing (..)
import Debug exposing (..)
import Keyboard exposing (..)


--import ElmEscapeHtml exposing (..)


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
              --, logExternal msg
            , nextCmd
            ]
        )



{--main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
--}


main : Program Decode.Value Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update =
            updateWithStorage
            --, update = update
        , subscriptions = subscriptions
        }



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
    , contents : String
    , rev : String
    , postsToUpload : Maybe String
    , appendsPending : Bool
    , status : String
    , currentTime : Maybe Time
    , flashMessage : String
    , downloadSuccess : Bool
    , downloadFirst : Bool
    , rawMode : Bool
    }


dropboxAPI : String
dropboxAPI =
    "https://content.dropboxapi.com/2"


initialModel : Model
initialModel =
    Model filePath
        ""
        -- contents
        ""
        -- rev
        Nothing
        -- postsToUpload
        False
        -- appendsPending
        ""
        -- status
        Nothing
        "Logger Ready"
        False
        False
        False



{--init : ( Model, Cmd Msg )
init =
    ( initialModel
      -- , getFile initialModel
    , getTimeTask
    )
--}
{--init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
    ( Maybe.withDefault initialModel savedModel
    , getTimeTask
    )
--}


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    ( flagsToModel flags
    , getTimeTask
    )


flagsToModel : Decode.Value -> Model
flagsToModel flags =
    Decode.decodeValue modelDecoder flags
        |> Result.withDefault
            { initialModel
                | flashMessage =
                    "Model mismatch! Local storage discarded!"
            }


modelDecoder : Decode.Decoder Model
modelDecoder =
    decode Model
        |> Pipeline.required "filePath" Decode.string
        |> Pipeline.required "contents" Decode.string
        |> Pipeline.required "rev" Decode.string
        |> Pipeline.required "postsToUpload" (Decode.nullable string)
        |> Pipeline.required "appendsPending" Decode.bool
        |> Pipeline.required "status" string
        |> Pipeline.required "currentTime" (Decode.nullable Decode.float)
        |> Pipeline.required "flashMessage" Decode.string
        |> Pipeline.required "downloadSuccess" Decode.bool
        |> Pipeline.required "downloadFirst" Decode.bool
        |> Pipeline.required "rawMode" Decode.bool



-- UPDATE


type Msg
    = Refresh
    | Download (Result Http.Error ( Time, FileInfo ))
      --| Download (Result Http.Error ( Time, String ))
    | DownloadAndAppend (Result Http.Error ( Time, FileInfo ))
      --| DownloadAndAppend (Result Http.Error ( Time, String ))
    | Append
    | GetTimeAndAppend Time
    | UpdateStatus String
    | Upload
    | UploadStatus (Result Http.Error ( Time, String ))
      --| UploadStatus (Result Http.Error ( Time, FileInfo ))
    | FocusDone (Result Dom.Error ())
    | GetTime
    | NewTime Time
    | KeyMsg Keyboard.KeyCode



--| UpdateMetadata (Result Http.Error Metadata)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Refresh ->
            { model | contents = "", flashMessage = "Downloading...be patient!" }
                ! [ getFileTask model ]

        -- , getMetaTask model ]
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
                        { model_ | flashMessage = model_.rev ++ ": Download successful (case 1)" }
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

        KeyMsg code ->
            case code of
                17 ->
                    -- Ctrl-q for toggling markdown format
                    let
                        model_ =
                            { model | rawMode = not model.rawMode }
                    in
                        (model_
                            |> setFlashMessage
                                -- ("Received keyboard " ++ toString code)
                                ("<Ctrl-q> to toggle Markdown format!")
                        )
                            ! [ focusUpdate ]

                _ ->
                    model ! [ Cmd.none ]



{- UpdateMetadata (Ok meta) ->
       { model | rev = meta.rev } ! [focusUpdate]


   UpdateMetadata (Err error) ->
       ( model
           |> setFlashMessage (toString error)
       )
           ! []
-}


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
    { model
        | downloadSuccess = flag
    }


setDownloadFirst : Bool -> Model -> Model
setDownloadFirst flag model =
    { model
        | downloadFirst = flag
        , appendsPending = False
    }


appendStatus : Model -> Model
appendStatus model =
    case model.downloadSuccess of
        True ->
            { model
                | contents = (timedPost model) ++ model.contents
                , appendsPending = True
            }

        False ->
            let
                posts =
                    (timedPost model)
                        ++ (Maybe.withDefault "" model.postsToUpload)

                model_ =
                    { model | postsToUpload = Just posts }
            in
                { model_
                    | contents = Maybe.withDefault "" model_.postsToUpload
                    , appendsPending = True
                }


appendPosts : Model -> Model
appendPosts model =
    { model
        | contents = (Maybe.withDefault "" model.postsToUpload) ++ model.contents
        , postsToUpload = Nothing
    }



{--updateContents : String -> Model -> Model
updateContents contents model =
    { model | contents = contents }
--}


updateContents : FileInfo -> Model -> Model
updateContents contents model =
    { model
        | contents = contents.body
        , rev = contents.rev
    }


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
            , br [] []
            , div [] [ viewContents model.contents model.rawMode ]
            ]
        , div [ id "titleContainer" ]
            [ hr [ class "style8" ] []
            , h3 [] [ text <| formatTime model.currentTime ++ model.flashMessage ]
            , textarea
                [ classList
                    [ ( "height-adjusting-textarea", True )
                    , ( "yellowBack", (model.appendsPending /= False) )
                    ]
                  --class "height-adjusting-textarea"
                , id "update"
                , placeholder "Update?"
                , onInput UpdateStatus
                , value model.status
                ]
                []
            , button [ id "button2", onClick Append ] [ text "Append" ]
            , button [ id "button3", onClick Upload ] [ text "Upload!" ]
            , button [ id "button3", onClick (UpdateStatus "") ] [ text "Clear" ]
            , button [ id "button1", onClick Refresh ] [ text "Refresh!" ]
            , footer
            ]
        ]


viewContents : String -> Bool -> Html Msg
viewContents contents rawMode =
    --div [] [ text contents]
    let
        rendersimple material =
            div [ class "answer" ] [ ul [] [ text material ] ]

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
                        case rawMode of
                            False ->
                                Markdown.toHtml [ class "answer" ] material

                            True ->
                                div [ class "answer" ] [ ul [] [ text material ] ]
    in
        contents
            |> String.split "@@@\n"
            |> List.take 100
            |> (case rawMode of
                    True ->
                        List.map rendersimple

                    False ->
                        List.map render
               )
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
    Sub.batch
        [ Keyboard.presses KeyMsg
          -- , Mouse.clicks MouseMsg
        ]



-- HTTP
--getFile : Model -> Http.Request FileInfo
--getFile : Model -> Http.Request String


getFile : Model -> Http.Request FileInfo
getFile model =
    let
        downloadURL =
            dropboxAPI ++ "/files/download"

        settings =
            { postSettings | url = downloadURL }

        --_ =
        --Debug.log "settings: " settings
    in
        Http.request settings


getFileTask : Model -> Cmd Msg
getFileTask model =
    let
        getTask =
            Http.toTask (getFile model)

        --_ =
        --Debug.log "model: " model
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
--sendFile : Model -> Maybe String -> Http.Request String


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
                , headers =
                    uploadHeaders
                , expect = expectString
                , body = stringBody "application/octet-stream" contents
            }
    in
        Http.request settings



{-
   getMetaData model =
       let
           metadataURL =
               -- dropboxAPI ++ "/get_metadata"
               "https://api.dropboxapi.com/2/files/get_metadata"

           settings =
               { postSettings
                   | url = metadataURL
                   , headers = metadataHeaders
                   , expect = expectJson metadataDecoder
                   , body = jsonBody (encodePath model.filePath)
               }
       in
           Http.request settings



   getMetaTask : Model -> Cmd Msg
   getMetaTask model =
       let
           getTask =
               Http.toTask (getMetaData model)
       in
           Task.attempt UpdateMetadata getTask
-}


encodePath : String -> Encode.Value
encodePath path =
    Encode.object
        [ ( "path", Encode.string path ) ]


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


downloadArgs : List ( String, Encode.Value )
downloadArgs =
    [ ( "path", Encode.string filePath ) ]


uploadArgs : List ( String, Encode.Value )
uploadArgs =
    downloadArgs ++ [ ( "mode", Encode.string "overwrite" ) ]


stringify : List ( String, Encode.Value ) -> String
stringify =
    Encode.object >> Encode.encode 0


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



{-
   metadataHeaders : List Header
   metadataHeaders =
       [ authorizationHeader
       ]
-}


postSettings =
    { method = "POST"
    , headers = downloadHeaders
    , url = ""
    , body = emptyBody
    , expect =
        expectStringResponse dropboxResponse
        -- , expect = expectString
        -- , expect = expectJson decodeFileInfo
        -- , expect = expectStringResponse expectRev
        -- ,expect = expectStringResponse fileInfo
        --, timeout = Just (2 * Time.millisecond)
    , timeout = Nothing
    , withCredentials = False
    }


dropboxResponse : Http.Response String -> Result String FileInfo
dropboxResponse response =
    response.headers
        |> Dict.get "dropbox-api-result"
        |> Maybe.map (Decode.decodeString (responseDecoder response.body))
        |> Maybe.withDefault (Err "no dropbox-api-result-header")


responseDecoder : String -> Decode.Decoder FileInfo
responseDecoder body =
    Decode.map2 FileInfo
        (Decode.field "rev" Decode.string)
        (Decode.succeed body)



{--
\{ headers, body } ->
    (Json.Decode.decodeString tableDataDecoder << toJsonObject)
        [ ( "records", body )
        , ( "total", Dict.get "X-Total-Count" headers |> Maybe.withDefault "0" )
        ]


type alias TableData =
    { records : List Record
    , total : Int
    }

tableDataDecoder : Decoder TableData
tableDataDecoder =
    decode TableData
        |> required "records" recordsDecoder
        |> required "total" int
--}


type alias FileInfo =
    { rev : String
    , body : String
    }


expectRev response =
    let
        result =
            Dict.get "dropbox-api-result" response.headers
                |> Maybe.withDefault "NA"

        revision =
            result
                |> Decode.decodeString (Decode.map (,) (Decode.field "rev" Decode.string))

        _ =
            Debug.log "headers: " response.headers

        _ =
            Debug.log "res: " result

        _ =
            Debug.log "raw rev: " (toString revision)
    in
        case revision of
            Ok revString ->
                let
                    _ =
                        Debug.log "success rev: " (toString revString)
                in
                    Decode.decodeString
                        (decodeFileInfo "00")
                        response.body

            Err error ->
                Decode.decodeString
                    (decodeFileInfo "00")
                    response.body


decodeFileInfo res =
    Decode.map (FileInfo res) Decode.string


type alias Metadata =
    { rev : String
    }


metadataUpdate response =
    let
        _ =
            Debug.log "metadata: " response
    in
        Decode.decodeString metadataDecoder response


metadataDecoder =
    decode Metadata
        |> Pipeline.required "rev" Decode.string


fileInfo response =
    let
        _ =
            Debug.log "headers: " response
    in
        Decode.decodeString fileInfoDecoder response.body


fileInfoDecoder : Decode.Decoder FileInfo
fileInfoDecoder =
    decode FileInfo
        |> Pipeline.requiredAt [ "headers", "dropbox-api-result", "rev" ] string
        |> Pipeline.required "body" string



-- Decode.decodeString (Decode.map (,) (Decode.field "rev" Decode.string) ) (Maybe.withDefault "" res)
-- Decode.decodeString
-- (Decode.map (,) (Decode.field "headers" (Decode.dict Decode.string)) ) json
{--
encodeContents : String -> Encode.Value
encodeContents contents =
  Encode.object
    [ ("data", Encode.string contents)]


decodeResponse : Decode.Decoder String
decodeResponse =
  Decode.string

--}
