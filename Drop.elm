module Drop exposing (..) 

-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/http.html

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as Decode
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
  }


init : String -> (Model, Cmd Msg)
init path =
  ( Model path 
        "https://content.dropboxapi.com/2/files/download"
        ""
        ""
  , getFile path "https://content.dropboxapi.com/2/files/download" 
  )


-- UPDATE


type Msg
  = Refresh
  | Download (Result Http.Error String)
  | AppendToFile 
  | UpdateStatus String
  | Upload 



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Refresh ->
      ({model|contents = ""}, getFile model.filePath model.dropURL)

    Download (Ok contents) ->
      {model|contents = unescape contents} ! [] 

    Download (Err error) ->
      {model|contents = toString error} ! [] 

    AppendToFile -> 
      {model|contents = model.status ++ "\n" ++ model.contents} ! 
        [ {-- need to upload --}]

    UpdateStatus s -> 
      {model| status = s } ! []

    Upload -> 
      model ! []


-- VIEW


view : Model -> Html Msg
view model =
  div [] 
    [ div [class "example example-dotted"]
        [ h3 [] [text model.filePath]
        , footer
        , button [ id "button1", onClick Refresh ] [ text "Refresh!" ]
        , button [ id "button2", onClick AppendToFile ] [ text "Append" ]
        , br [] []
        , input [ type_ "text", placeholder "Update?", onInput UpdateStatus ] []
        , div [] [viewContents model.contents ]
        ]
    , div [] 
        [ button [id "button3", onClick Upload] [text "Upload!"]
        ]
    ]

viewContents: String -> Html Msg
viewContents contents = 
    contents 
        |> String.split "\n"
        |> List.map (\line -> p [class "answer"] [text line])
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


getFile : String -> String -> Cmd Msg
getFile path url =
  let
    settings = { postSettings | url    = url }
  in
    Http.send Download (Http.request settings)

decodeResponse : Decode.Decoder String
decodeResponse =
  Decode.string 

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

