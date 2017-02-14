module Drop exposing (..) 

-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/http.html

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode 
import Dom exposing (..) 
import Task exposing (..) 
import Result exposing (..)
import Time exposing (..) 
import Date exposing (..)
import Date.Format exposing (..) 

import ElmEscapeHtml exposing (..) 

import Version exposing (..)

main : Program Never Model Msg
main =
  Html.program
    { init = init "/Apps/elmBox/body.txt"
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
  , time : Time 
  }


init : String -> (Model, Cmd Msg)
init path =
  ( Model path 
        "https://content.dropboxapi.com/2/files/download"
        ""
        ""
        0
  , getFile path "https://content.dropboxapi.com/2/files/download" 
  )


-- UPDATE


type Msg
  = Refresh
  | Download (Result Http.Error (Time, String))
  | AppendToFile 
  | UpdateStatus String
  | Upload 
  | UploadStatus (Result Http.Error (Time, String))
  | FocusDone (Result Dom.Error() ) 


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Refresh ->
      {model|contents = ""} ! 
        [ getFile model.filePath model.dropURL]

    Download (Ok (time, contents)) ->
      {model|contents = unescape contents, time = time} ! 
        [ Task.attempt FocusDone (Dom.focus "update")] 

    Download (Err error) ->
      {model|contents = toString error} ! [] 

    AppendToFile -> 
      {model|contents = (timedStatus model) ++ model.contents} ! 
        [ Task.attempt FocusDone (Dom.focus "update")]
        -- [ Dom.focus "update"{-- need to upload --}]

    FocusDone _-> 
      model ! []

    UpdateStatus s -> 
      {model| status = s } ! []

    Upload -> 
      model ! [sendFile model]

    UploadStatus (Ok (time, contents)) -> 
      { model | time = time } ! 
        [ Task.attempt FocusDone (Dom.focus "update")]

    UploadStatus (Err error) -> 
      { model|contents = toString error} ! []

timedStatus: Model -> String 
timedStatus model = 
  let 
    fDate = 
      fromTime model.time 
        |> format "%a, %b/%d/%y, %I:%M%P "
  in 
    fDate ++ model.status ++ "\n"

-- VIEW


view : Model -> Html Msg
view model =
  div [] 
    [ div [class "example example-dotted"]
        [ h1 [] [text "Daily Log"]
        , footer
        , hr [class "style5"] []
        , button [ id "button1", onClick Refresh ] [ text "Refresh!" ]
        , br [] []
        , div [] [viewContents model.contents ]
        ]
    , div [id "titleContainer"] 
        [ hr [class "style8"] []
        --, h3 [] [text <| "received: " ++ (toString <| Date.fromTime model.time)]
        , input [ id "update", type_ "text", placeholder "Update?", onInput UpdateStatus ] []
        , button [ id "button2", onClick AppendToFile ] [ text "Append" ]
        , button [ id "button3", onClick Upload] [text "Upload!"]
        , footer
        ]
    ]


viewContents: String -> Html Msg
viewContents contents = 
    contents 
        |> String.split "\n"
        |> List.map (\line -> p [class "answer"] [text line])
        |> List.reverse 
        |> div []


footer : Html Msg
footer = 
  div [id "footer"]
  [ a [href (gitRepo ++ "/issues/new"), 
    target "_blank", 
    rel "noopener noreferrer"] 
    [text version]
  ]

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- HTTP

sendFile : Model -> Cmd Msg 
sendFile model = 
  let 
    headers = 
      [ Http.header "Authorization" "Bearer 4bhveELh1l8AAAAAAAAg1hjS4PUDWf0EeED2cIsmOsdJE04uqkichInc0sN0QZao"
      , Http.header 
        "Dropbox-API-Arg" "{\"path\":\"/Apps/elmBox/body.txt\", \"mode\":\"overwrite\"}"
      -- , Http.header "Content-Type" "application/octet-stream"
      ]

    settings = { postSettings | 
                 headers = headers
               , url  = "https://content.dropboxapi.com/2/files/upload"
               , body = stringBody "application/octet-stream" model.contents 
               }

    getTask = Http.toTask (Http.request settings)

  in 
    Time.now
      |> Task.andThen (\t -> Task.map ((,) t) getTask) 
      |> Task.attempt UploadStatus 

    -- Http.toTask (Http.request settings)
    --   |> Task.andThen (\req -> Task.map (\t -> (t, req)) Time.now)
    --   |> Task.attempt UploadStatus


encodeContents : String -> Encode.Value        
encodeContents contents =
  Encode.object 
    [ ("data", Encode.string contents)]


getFile : String -> String -> Cmd Msg
getFile path url =
  let
    settings = { postSettings | url    = url }
    getTask = Http.toTask (Http.request settings)
  in
    Time.now 
      |> Task.andThen (\t -> Task.map ((,) t) getTask)
      |> Task.attempt Download 
  
  --  Http.send Download (Http.request settings)


decodeResponse : Decode.Decoder String
decodeResponse =
  Decode.string 

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
  { method  = "POST"
  , headers = 
    [ Http.header "Authorization" "Bearer 4bhveELh1l8AAAAAAAAg1hjS4PUDWf0EeED2cIsmOsdJE04uqkichInc0sN0QZao"
    , Http.header "Dropbox-API-Arg" "{\"path\":\"/Apps/elmBox/body.txt\"}"
    -- , Http.header "Content-Type" "application/json"
    ]
  , url     = ""
  , body    = emptyBody
  , expect  = expectString
  , timeout = Nothing
  , withCredentials = False
  }

