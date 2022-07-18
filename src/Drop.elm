port module Drop exposing (..)

-- import Date exposing (..)
-- import Date.Format exposing (..)
-- import Keyboard exposing (..) (Browser.Events instead)

import Browser
import Browser.Dom as Dom
import Browser.Events exposing (..)
import DateFormat
import Debug exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as E
import Http exposing (..)
import Json.Decode as D exposing (decodeString, dict, field, list, string)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Markdown
import Result exposing (..)
import Task exposing (..)
import Time exposing (..)
import Version exposing (..)



--import ElmEscapeHtml exposing (..)


-- port setStorage : Model -> Cmd msg, -- TODO -> Fix that


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
        [ -- setStorage model, -- TODO -> Fix that

        --, logExternal msg
        nextCmd
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


main : Program D.Value Model Msg
main =
    Browser.element
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
    { timestamp : Time.Posix
    , message : String
    }


type alias Model =
    { filePath : String
    , contents : String
    , rev : String
    , postsToUpload : Maybe String
    , appendsPending : Bool
    , status : String
    , currentTime : Maybe Time.Posix
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


init : D.Value -> ( Model, Cmd Msg )
init flags =
    ( flagsToModel flags
    , getTimeTask
    )


flagsToModel : D.Value -> Model
flagsToModel flags =
    D.decodeValue modelDecoder flags
        |> Result.withDefault
            { initialModel
                | flashMessage =
                    "Model mismatch! Local storage discarded!"
            }


modelDecoder : D.Decoder Model
modelDecoder =
    D.succeed Model
        |> Pipeline.required "filePath" D.string
        |> Pipeline.required "contents" D.string
        |> Pipeline.required "rev" D.string
        |> Pipeline.required "postsToUpload" (D.nullable string)
        |> Pipeline.required "appendsPending" D.bool
        |> Pipeline.required "status" string
        |> Pipeline.required "currentTime" (D.nullable D.float)
        |> Pipeline.required "flashMessage" D.string
        |> Pipeline.required "downloadSuccess" D.bool
        |> Pipeline.required "downloadFirst" D.bool
        |> Pipeline.required "rawMode" D.bool



-- UPDATE


type Msg
    = Refresh
    | Download (Result Http.Error ( Time.Posix, FileInfo ))
      --| Download (Result Http.Error ( Time, String ))
    | DownloadAndAppend (Result Http.Error ( Time.Posix, FileInfo ))
      --| DownloadAndAppend (Result Http.Error ( Time, String ))
    | Append
    | GetTimeAndAppend Time.Posix
    | UpdateStatus String
    | Upload
    | UploadStatus (Result Http.Error ( Time.Posix, String ))
      --| UploadStatus (Result Http.Error ( Time, FileInfo ))
    | FocusDone (Result Dom.Error ())
    | GetTime
    | NewTime Time.Posix



-- | KeyMsg Keyboard.KeyCode -- (TODO)
--| UpdateMetadata (Result Http.Error Metadata)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Refresh ->
            ( { model | contents = "", flashMessage = "Downloading...be patient!" }
            , getFileTask model
            )

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
                    ( { model_ | flashMessage = model_.rev ++ ": Download successful (case 1)" }
                    , focusUpdate
                    )

                ( True, False ) ->
                    let
                        model__ =
                            model_
                                |> appendPosts
                                |> setFlashMessage "Download successful! (case 2)"
                    in
                    ( model__, sendFileTask model__ )

                ( _, True ) ->
                    ( { model_ | flashMessage = "Download successful (case 3)" }, sendFileTask model )

        Download (Err error) ->
            let
                model_ =
                    { model | downloadSuccess = False, downloadFirst = False }
            in
            ( setFlashMessage (toString error) model_, Cmd.none )

        DownloadAndAppend (Ok ( time, contents )) ->
            ( model
                |> setTime time
                |> updateContents contents
                |> appendStatus
                |> setFlashMessage "Download/Append successful!"
                |> setFlag True
            , focusUpdate
            )

        DownloadAndAppend (Err error) ->
            ( setFlashMessage (toString error) model, Cmd.none )

        Append ->
            ( model, Task.perform GetTimeAndAppend Time.now )

        GetTimeAndAppend time ->
            ( model
                |> setTime time
                |> appendStatus
                |> setFlashMessage "Append successful!"
            , focusUpdate
            )

        UpdateStatus s ->
            ( { model | status = s }, Cmd.batch [ focusUpdate, adjustTextAreaHeight "height-adjusting-textarea" ] )

        Upload ->
            let
                model_ =
                    model |> setFlashMessage "Uploading...please be patient!"
            in
            case model_.downloadSuccess of
                True ->
                    ( model_, sendFileTask model )

                False ->
                    ( { model_ | downloadFirst = True }, getFileTask model_ )

        UploadStatus (Ok ( time, contents )) ->
            ( model
                |> setTime time
                |> setFlashMessage "Upload successful!"
                |> setDownloadFirst False
            , focusUpdate
            )

        UploadStatus (Err error) ->
            ( model
                |> setFlashMessage (toString error)
            , Cmd.none
            )

        GetTime ->
            ( model
            , getTimeTask
            )

        NewTime time ->
            ( model |> setTime time, focusUpdate )

        FocusDone _ ->
            ( model, Cmd.none )



-- KeyMsg code -> -- TODO : handle the keyboard
--     case code of
--         17 ->
--             -- Ctrl-q for toggling markdown format
--             let
--                 model_ =
--                     { model | rawMode = not model.rawMode }
--             in
--             ((model_
--                 |> setFlashMessage
--                     -- ("Received keyboard " ++ toString code)
--                     "<Ctrl-q> to toggle Markdown format!"
--             ), focusUpdate )
--         _ ->
--             (model, Cmd.none
--             )
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


setTime : Time.Posix -> Model -> Model
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
                | contents = timedPost model ++ model.contents
                , appendsPending = True
            }

        False ->
            let
                posts =
                    timedPost model
                        ++ Maybe.withDefault "" model.postsToUpload

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
        | contents = Maybe.withDefault "" model.postsToUpload ++ model.contents
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


formatTime : Maybe Time.Posix -> String
formatTime time =
    time |> Maybe.withDefault (Time.millisToPosix 0) |> DateFormat.format [DateFormat.hourMilitaryFixed] Time.utc -- "%a %b/%d/%y %H:%M:%S "



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ class "example example-dotted" ]
            [ h1 [ style "margin-bottom" "0px" ] [ text "Daily Log" ]
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
                    , ( "yellowBack", model.appendsPending /= False )
                    ]

                --class "height-adjusting-textarea"
                , id "update"
                , placeholder "Update?"
                , E.onInput UpdateStatus
                , value model.status
                ]
                []
            , button [ id "button2", E.onClick Append ] [ text "Append" ]
            , button [ id "button3", E.onClick Upload ] [ text "Upload!" ]
            , button [ id "button3", E.onClick (UpdateStatus "") ] [ text "Clear" ]
            , button [ id "button1", E.onClick Refresh ] [ text "Refresh!" ]
            -- , button [ id "button4", E.onClick (KeyMsg 17) ] [ text "MD" ]
            , footer
            ]
        ]


viewContents : String -> Bool -> Html Msg
viewContents contents rawMode =
    --div [] [ text contents]
    let
        inMultipleLines contents_ =
            contents_
                |> String.split "\n"
                |> List.map (\line -> pre [] [ text line ])
                >> div []

        rendersimple material =
            let
                tuple =
                    String.split "\t" material
            in
            case tuple of
                ts :: [ line ] ->
                    div [ class "answer" ]
                        [ pre [] [ text ts ]
                        , pre [] [ text line ]

                        -- , inMultipleLines line
                        ]

                _ ->
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
                        , div [] (Markdown.toHtml Nothing line)
                        ]

                _ ->
                    case rawMode of
                        False ->
                            div [] (Markdown.toHtml Nothing material) --[ class "answer" ] material

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
        [-- Keyboard.presses KeyMsg
         -- , Mouse.clicks MouseMsg
        ]



-- HTTP
--getFile : Model -> Http.Request FileInfo
--getFile : Model -> Http.Request String


-- getFile : Model -> Http. FileInfo
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
            Http.task (getFile model)

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
            |> Task.andThen (\t -> Task.map (Tuple.pair t) getTask)
            |> Task.attempt Download
        --}


getFileAndAppend : Model -> Cmd Msg
getFileAndAppend model =
    let
        getTask =
            Http.task (getFile model)
    in
    Time.now
        |> Task.andThen (\t -> Task.map (Tuple.pair t) getTask)
        |> Task.attempt DownloadAndAppend



--  Http.send Download (Http.request settings)
--sendFile : Model -> Maybe String -> Http.Request String


sendFile : Model -> Maybe String -> Http.Response String
sendFile model posts =
    let
        uploadURL =
            dropboxAPI ++ "/files/upload"

        contents =
            model.contents ++ Maybe.withDefault "" posts

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
            Http.task (sendFile model Nothing)
    in
    Time.now
        |> Task.andThen (\t -> Task.map (Tuple.pair t) sendTask)
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

type alias Req msg =
    { method : String
    , headers : List Header
    , url : String
    , body : Body
    , expect : Expect msg
    , timeout : Maybe Float
    , tracker : Maybe String
    } 

postSettings : Req Msg
postSettings =
    { method = "POST"
    , headers = downloadHeaders
    , url = ""
    , body = emptyBody
    , expect = Http.expectStringResponse dropboxResponse
        -- Http.expectStringResponse dropboxResponse

    , timeout = Nothing
    , tracker = Nothing
    }
    -- , expect = expectString
    -- , expect = expectJson decodeFileInfo
    -- , expect = expectStringResponse expectRev
    -- ,expect = expectStringResponse fileInfo
    --, timeout = Just (2 * Time.millisecond)
    -- , withCredentials = False


dropboxResponse : Http.Response String -> Result String FileInfo
dropboxResponse response =
    case response of
        GoodStatus_ metaData body ->
            metaData.headers
                |> Dict.get "dropbox-api-result"
                |> Maybe.map (D.decodeString (responseDecoder body))
                |> Maybe.map (Result.mapError (\e -> "toto"))
                |> Maybe.withDefault (Err "toto")
                -- |> Maybe.map (Result.mapError (D.Field "toto" (Result.Err "Error")))
                -- |> Maybe.withDefault Result.Err "Error"
                -- |> Maybe.withDefault (D.Field "no dropbox-api-result-header")
        _ ->    
            -- Nothing
            Err "Toto"
            -- Result.Err "Error"


responseDecoder : String -> D.Decoder FileInfo
responseDecoder body =
    D.map2 FileInfo
        (D.field "rev" D.string)
        (D.succeed body)



{--
\{ headers, body } ->
    (Json.D.decodeString tableDataDecoder << toJsonObject)
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
                |> D.decodeString (D.map Tuple.pair (D.field "rev" D.string))

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
            D.decodeString
                (decodeFileInfo "00")
                response.body

        Err error ->
            D.decodeString
                (decodeFileInfo "00")
                response.body


decodeFileInfo res =
    D.map (FileInfo res) D.string


type alias Metadata =
    { rev : String
    }


metadataUpdate response =
    let
        _ =
            Debug.log "metadata: " response
    in
    D.decodeString metadataDecoder response


metadataDecoder =
    D.succeed Metadata
        |> Pipeline.required "rev" D.string


fileInfo response =
    let
        _ =
            Debug.log "headers: " response
    in
    D.decodeString fileInfoDecoder response.body


fileInfoDecoder : D.Decoder FileInfo
fileInfoDecoder =
    D.succeed FileInfo
        |> Pipeline.requiredAt [ "headers", "dropbox-api-result", "rev" ] string
        |> Pipeline.required "body" string



-- D.decodeString (D.map Tuple.pair (D.field "rev" D.string) ) (Maybe.withDefault "" res)
-- D.decodeString
-- (D.map Tuple.pair (D.field "headers" (D.dict D.string)) ) json
{--
encodeContents : String -> Encode.Value
encodeContents contents =
  Encode.object
    [ ("data", Encode.string contents)]


decodeResponse : D.Decoder String
decodeResponse =
  D.string

--}
