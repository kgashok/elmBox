module Drop exposing (..) 

-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/http.html

import Html exposing (..)
-- import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as Decode


main : Program Never Model Msg
main =
  Html.program
    { init = init "/Apps/Drafts/body.txt"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { filePath : String
  , dropURL : String
  , contents : String 
  }


init : String -> (Model, Cmd Msg)
init path =
  ( Model path 
        "https://content.dropboxapi.com/2/files/download"
        ""
  , getFile path "https://content.dropboxapi.com/2/files/download" 
  )


-- UPDATE


type Msg
  = Refresh
  | Download (Result Http.Error String)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Refresh ->
      (model, getFile model.filePath model.dropURL)

    Download (Ok contents) ->
      (Model model.filePath model.dropURL contents, Cmd.none)

    Download (Err error) ->
      (Model model.filePath model.dropURL (toString error), Cmd.none)



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text model.filePath]
    , button [ onClick Refresh ] [ text "Refresh!" ]
    , br [] []
    , div [] [text model.contents ]
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

