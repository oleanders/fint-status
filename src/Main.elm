module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, a, div, footer, form, h1, h2, h3, h4, i, input, label, li, option, p, pre, section, select, span, strong, text, textarea, ul)
import Html.Attributes exposing (attribute, checked, class, href, target, title, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http exposing (Error(..), Expect, expectStringResponse, header)
import Json.Decode as Decode exposing (Decoder, decodeString, errorToString, field, int, keyValuePairs, maybe, string)
import Lib.Tid exposing (tidMellomToTidspunkt)
import List exposing (head, take)
import RemoteData exposing (RemoteData(..), WebData)
import Task exposing (onError)
import Time exposing (Month(..))
import Url exposing (percentEncode, toString)


allekomponenter : List String
allekomponenter =
    [ "administrasjon/personal"

    --, "administrasjon/fullmakt"
    , "administrasjon/kodeverk"
    , "administrasjon/organisasjon"

    --, "okonomi/faktura"
    --, "okonomi/kodeverk"
    , "utdanning/elev"
    , "utdanning/kodeverk"

    --, "utdanning/timeplan"
    --, "utdanning/utdanningsprogram"
    --, "utdanning/vurdering"
    --, "ressurser/tilganger"
    --, "personvern/tilganger"
    --, "felles/basisklasser"
    --, "felles/kodeverk"
    --, "felles/iso"
    --, "arkiv/kodeverk"
    --, "arkiv/kulturminnevern"
    --, "arkiv/noark"
    --, "arkiv/personal"
    --, "arkiv/samferdsel"
    ]



---- MODEL ----


type alias Model =
    { errorMessage : Maybe String
    , komponenter : List String
    , helsestatus : Dict String (WebData HealthStatus)
    , ressursstatus : Ressursstatus
    , tid : Time.Posix
    , tidLastetInn : Time.Posix
    , timeZone : Time.Zone
    , state : State
    , environments : List String
    }


type alias Ressursstatus =
    Dict ( String, String ) CacheStatus


type alias CacheStatus =
    --      cacheSize   lastUpdated
    ( WebData Int, WebData Int )


type Environment
    = PlayWithFint
    | Beta AccessToken
    | Prod AccessToken


type State
    = LoginForm Environment String (Maybe String) -- tekstfelt feilmelding
    | TryingToAuthenticate Environment
    | RunPlayWithFint String (List String)
    | RunBeta String AccessToken (List String)
    | RunProd String AccessToken (List String)


type alias Credentials =
    { idpUrl : String
    , clientId : String
    , clientSecret : String
    , username : String
    , password : String
    , scope : String
    , assetId : String
    }


type alias AccessToken =
    { accessToken : String
    , expiresIn : Int
    }


type alias HealthStatus =
    { source : String
    , time : Int
    , data : List HealthStatusdata
    , message : Maybe String
    }


type alias HealthStatusdata =
    { component : String
    , status : String
    , timestamp : Int
    }


type alias RessursStatusUrls =
    { cacheSizeUrl : String
    , lastUpdatedUrl : String
    }


init : ( Model, Cmd Msg )
init =
    ( { errorMessage = Nothing
      , komponenter = allekomponenter
      , helsestatus = Dict.fromList (List.map (\komponent -> Tuple.pair komponent Loading) allekomponenter)
      , ressursstatus = Dict.empty
      , tid = Time.millisToPosix 0
      , tidLastetInn = Time.millisToPosix 0
      , timeZone = Time.utc
      , state = RunPlayWithFint "play-with-fint.felleskomponent.no" []
      , environments = [ "play-with-fint.felleskomponent.no", "beta.felleskomponent.no", "api.felleskomponent.no" ]
      }
    , Cmd.batch
        [ Task.perform ClockIsTicking Time.now
        , Task.perform AdjustedTimeZone Time.here
        , apiRequest PlayWithFint allekomponenter
        ]
    )


apiRequest : Environment -> List String -> Cmd Msg
apiRequest environment =
    List.map
        (\komponent ->
            Cmd.batch
                [ httpRequest environment (komponent ++ "/admin/health") (RemoteData.fromResult >> HealthStatusResponse komponent) healtStatusDecoder
                , httpRequest environment (komponent ++ "/") ApiDiscoveryResponse (apiDiscoveryDecoder komponent)
                ]
        )
        >> Cmd.batch



---- UPDATE ----


type Msg
    = UpdatedTextfield Environment String -- tekstfelt
    | RequestedLogIn Environment String -- tekstfelt?
    | ClockIsTicking Time.Posix
    | AdjustedTimeZone Time.Zone
    | ChangeEnvironment String
    | AuthenticateResponse (Result Http.Error AccessToken)
    | HealthStatusResponse String (WebData HealthStatus)
    | ApiDiscoveryResponse (Result Http.Error ( String, List ( String, RessursStatusUrls ) ))
    | LastUpdatedResponse ( String, String ) (WebData Int)
    | CasheSizeResponse ( String, String ) (WebData Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdatedTextfield environment tekst ->
            ( { model | state = LoginForm environment tekst Nothing }, Cmd.none )

        RequestedLogIn environment tekst ->
            case Decode.decodeString credentialsDecoder tekst of
                Ok credentials ->
                    ( { model | state = TryingToAuthenticate environment }, authenticateRequest credentials )

                Err error ->
                    ( { model | state = LoginForm environment tekst (error |> Decode.errorToString |> Just) }
                    , Cmd.none
                    )

        ClockIsTicking newTime ->
            let
                saveTimeOnPageLoad =
                    if model.tidLastetInn == Time.millisToPosix 0 then
                        newTime

                    else
                        model.tidLastetInn
            in
            ( { model | tid = newTime, tidLastetInn = saveTimeOnPageLoad }, Cmd.none )

        AdjustedTimeZone newZone ->
            ( { model | timeZone = newZone }, Cmd.none )

        ChangeEnvironment env ->
            let
                state =
                    case env of
                        "beta.felleskomponent.no" ->
                            LoginForm (Beta (AccessToken "" 0)) "" Nothing

                        "api.felleskomponent.no" ->
                            LoginForm (Prod (AccessToken "" 0)) "" Nothing

                        _ ->
                            RunPlayWithFint "play-with-fint.felleskomponent.no" []

                cmd =
                    case env of
                        "play-with-fint.felleskomponent.no" ->
                            apiRequest PlayWithFint model.komponenter

                        _ ->
                            Cmd.none
            in
            ( { model
                | state = state
                , helsestatus = Dict.fromList (List.map (\komponent -> Tuple.pair komponent Loading) allekomponenter)
                , ressursstatus = Dict.empty
              }
            , cmd
            )

        AuthenticateResponse (Ok accessToken) ->
            case model.state of
                TryingToAuthenticate env ->
                    case env of
                        Beta _ ->
                            ( { model | state = RunBeta "beta.felleskomponent.no" accessToken [] }, apiRequest (Beta accessToken) model.komponenter )

                        Prod _ ->
                            ( { model | state = RunProd "api.felleskomponent.no" accessToken [] }, apiRequest (Beta accessToken) model.komponenter )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        AuthenticateResponse (Err error) ->
            ( { model | errorMessage = httpError error }, Cmd.none )

        HealthStatusResponse komponent webDataHelseStatus ->
            let
                oppdatertDict =
                    Dict.map
                        (\key item ->
                            if key == komponent then
                                webDataHelseStatus

                            else
                                item
                        )
                        model.helsestatus
            in
            ( { model | helsestatus = oppdatertDict }, Cmd.none )

        ApiDiscoveryResponse (Ok ( komponent, urls )) ->
            let
                createStatusHttpRequests environment ( ressurs, urler ) =
                    Cmd.batch
                        [ httpRequest environment urler.cacheSizeUrl (RemoteData.fromResult >> CasheSizeResponse ( komponent, ressurs )) casheSizeDecoder
                        , httpRequest environment urler.lastUpdatedUrl (RemoteData.fromResult >> LastUpdatedResponse ( komponent, ressurs )) lastUpdatedDecoder
                        ]

                dict2 =
                    urls
                        |> List.map (\( navn, _ ) -> ( ( komponent, navn ), ( Loading, Loading ) ))
                        |> Dict.fromList

                ressursstatus_ =
                    Dict.union model.ressursstatus dict2
            in
            case model.state of
                RunPlayWithFint url komponenter ->
                    ( { model | ressursstatus = ressursstatus_ }
                    , urls |> List.map (createStatusHttpRequests PlayWithFint) |> Cmd.batch
                    )

                RunBeta url accessToken _ ->
                    ( { model | ressursstatus = ressursstatus_ }
                    , urls |> List.map (createStatusHttpRequests (Beta accessToken)) |> Cmd.batch
                    )

                RunProd url accessToken _ ->
                    ( { model | ressursstatus = ressursstatus_ }
                    , urls |> List.map (createStatusHttpRequests (Prod accessToken)) |> Cmd.batch
                    )

                _ ->
                    ( model, Cmd.none )

        ApiDiscoveryResponse (Err error) ->
            ( { model | errorMessage = httpError error }, Cmd.none )

        LastUpdatedResponse ( komponent, ressurs ) updated ->
            let
                oppdatertDict =
                    Dict.map
                        (\( komp, hoved ) ( cache, u ) ->
                            if komponent == komp && ressurs == hoved then
                                ( cache, updated )

                            else
                                ( cache, u )
                        )
                        model.ressursstatus
            in
            ( { model | ressursstatus = oppdatertDict }, Cmd.none )

        CasheSizeResponse ( komponent, ressurs ) cache ->
            let
                oppdatertDict =
                    Dict.map
                        (\( komp, hoved ) ( c, updated ) ->
                            if komponent == komp && ressurs == hoved then
                                ( cache, updated )

                            else
                                ( c, updated )
                        )
                        model.ressursstatus
            in
            ( { model | ressursstatus = oppdatertDict }, Cmd.none )


httpError : Error -> Maybe String
httpError x =
    case x of
        BadUrl message ->
            Just message

        Timeout ->
            Just "Timeout"

        NetworkError ->
            Just "NetworkError"

        BadStatus errorCode ->
            Just <| "BasStatus: " ++ String.fromInt errorCode

        BadBody errorMessage ->
            Just <| "BadBody: " ++ errorMessage


authenticateRequest : Credentials -> Cmd Msg
authenticateRequest cred =
    Http.request
        { method = "POST"
        , headers =
            []
        , url = cred.idpUrl
        , body =
            formBody
                [ ( "grant_type", "password" )
                , ( "username", cred.username )
                , ( "password", cred.password )
                , ( "client_id", cred.clientId )
                , ( "client_secret", cred.clientSecret )
                , ( "scope", "fint-client" )
                ]
        , expect = Http.expectJson AuthenticateResponse accessTokenDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


formBody : List ( String, String ) -> Http.Body
formBody =
    List.map (\( key, value ) -> percentEncode key ++ "=" ++ percentEncode value)
        >> String.join "&"
        >> Http.stringBody "application/x-www-form-urlencoded"


httpRequest : Environment -> String -> (Result Error a -> Msg) -> Decoder a -> Cmd Msg
httpRequest environment url msg decoder =
    let
        fixUrl currentUr domain =
            if String.startsWith "http" currentUr then
                currentUr

            else
                domain ++ currentUr

        ( completeUrl, headerAuth ) =
            case environment of
                PlayWithFint ->
                    ( fixUrl url "https://play-with-fint.felleskomponent.no/", [] )

                Beta accessToken ->
                    ( fixUrl url "https://beta.felleskomponent.no/"
                    , [ header "Authorization" ("Bearer " ++ accessToken.accessToken) ]
                    )

                Prod accessToken ->
                    ( fixUrl url "https://prod.felleskomponent.no/"
                    , [ header "Authorization" ("Bearer " ++ accessToken.accessToken) ]
                    )
    in
    Http.request
        { method = "GET"
        , headers =
            [ header "x-client" "fint-client"
            , header "x-org-id" "x"
            ]
                ++ headerAuth
        , url =
            if environment == PlayWithFint then
                completeUrl

            else
                "/proxy/" ++ completeUrl
        , body = Http.emptyBody
        , expect = Http.expectJson msg decoder
        , timeout = Nothing

        --, timeout = Just 1000.0
        , tracker = Nothing
        }


expectJson : (Result Error a -> msg) -> Decoder a -> Expect msg
expectJson toMsg decoder =
    expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    case decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err _ ->
                            Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ metadata body ->
                    case decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (Http.BadBody (Decode.errorToString err))


healtStatusDecoder : Decoder HealthStatus
healtStatusDecoder =
    Decode.map4 HealthStatus
        (field "source" string)
        (field "time" int)
        (Decode.field "data"
            (Decode.list
                (Decode.map3 HealthStatusdata
                    (field "component" string)
                    (field "status" string)
                    (field "timestamp" int)
                )
            )
        )
        (maybe (field "message" string))


apiDiscoveryDecoder : String -> Decoder ( String, List ( String, RessursStatusUrls ) )
apiDiscoveryDecoder komponent =
    Decode.map2 Tuple.pair
        (Decode.succeed komponent)
    <|
        keyValuePairs
            (Decode.map2 RessursStatusUrls
                (field "cacheSizeUrl" string)
                (field "lastUpdatedUrl" string)
            )


lastUpdatedDecoder : Decoder Int
lastUpdatedDecoder =
    field "lastUpdated"
        (Decode.map
            (\str ->
                String.toInt str
                    |> Maybe.withDefault 0
            )
            string
        )


casheSizeDecoder : Decoder Int
casheSizeDecoder =
    field "size" int


credentialsDecoder : Decoder Credentials
credentialsDecoder =
    Decode.map7 Credentials
        (field "idpUri" string)
        (field "clientId" string)
        (field "openIdSecret" string)
        (field "username" string)
        (field "password" string)
        (field "scope" string)
        (field "assetId" string)


accessTokenDecoder : Decoder AccessToken
accessTokenDecoder =
    Decode.map2 AccessToken
        (field "access_token" string)
        (field "expires_in" int)



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        tidSidenOppdatert =
            tidMellomToTidspunkt Time.utc (Time.posixToMillis model.tid) (Time.posixToMillis model.tidLastetInn)
    in
    div []
        [ section [ class "section" ]
            [ div [ class "container" ]
                [ div [ class "columns" ]
                    [ div [ class "column" ] [ h1 [ class "title" ] [ text "FINT helsestatus" ] ]
                    , div [ class "column" ] [ text tidSidenOppdatert ]
                    , div [ class "column" ]
                        [ div [ class "control has-icons-left is-pulled-right" ]
                            [ div [ class "select" ]
                                [ select [ onInput ChangeEnvironment ] <|
                                    List.map
                                        (\env ->
                                            option []
                                                [ text env ]
                                        )
                                        model.environments
                                ]
                            , div [ class "icon is-small is-left" ]
                                [ i [ class "fas fa-globe" ]
                                    []
                                ]
                            ]
                        ]
                    ]
                , case model.state of
                    LoginForm environment tekstfelt error ->
                        viewUtlogget environment tekstfelt error

                    TryingToAuthenticate environment ->
                        div [] [ text "Authenticating..." ]

                    RunPlayWithFint url komponenter ->
                        viewInnlogget url model

                    RunBeta url accessToken komponenter ->
                        viewInnlogget url model

                    RunProd url accessToken komponenter ->
                        viewInnlogget url model
                ]
            ]
        , footer [ class "footer" ]
            [ div [ class "content has-text-centered" ]
                [ p []
                    [ strong [] [ text "FINT helsestatus" ]
                    , text " - for å holde seg frisk og fin. Kildekode på github: "
                    , a [ href "https://github.com/oleanders/fint-status" ]
                        [ text "fint-status" ]
                    , text ". Informasjon om FINT, finnes på "
                    , a [ href "https://www.fintlabs.no" ]
                        [ text "fintlabs.no" ]
                    ]
                ]
            ]
        ]


viewUtlogget : Environment -> String -> Maybe String -> Html Msg
viewUtlogget environment tekstfelt error =
    div []
        [ form
            [ class "box"
            , onSubmit (RequestedLogIn environment tekstfelt)
            ]
          <|
            [ h2 [ class "title is-4" ] [ text "Logg inn" ]
            , h3 [ class "subtitle is-6" ]
                [ text "Legg inn påloggingsinformasjon, som kan "
                , a [ href "https://www.fintlabs.no/#/tutorials?id=klienter" ] [ text "hentes fra Kundeportalen" ]
                , text ", for å se helsestatus på felleskomponentene til FINT."
                ]
            , div [ class "field" ]
                [ textarea
                    [ class "textarea"
                    , onInput (UpdatedTextfield environment)
                    , Html.Attributes.rows 10
                    , Html.Attributes.placeholder "{\n    \"username\": \"x@client.x.no\",\n    \"password\": \"xx...x\",\n    \"clientId\": \"4...bc867dc8e\",\n    \"openIdSecret\": \"dTrl...hPQ\",\n    \"scope\": \"fint-client\",\n    \"idpUri\": \"https://idp.felleskomponent.no/nidp/oauth/nam/token\",\n    \"assetId\": \"....no\"\n}\n"
                    ]
                    [ text tekstfelt ]
                ]
            , p [] [ input [ class "button is-primary", type_ "submit", value "Logg inn" ] [] ]
            ]
                ++ (error
                        |> Maybe.map
                            (\x ->
                                [ pre [ class "error" ] [ text <| "Feilmelding " ++ x ] ]
                            )
                        |> Maybe.withDefault []
                   )
        , div [ class "box" ]
            [ h2 [ class "title is-4" ] [ text "Innstillinger" ]
            , h3 [ class "subtitle is-6" ] [ text "Status sjekkes for komponentene: " ]
            , div [ class "field" ] [ label [ class "checkbox" ] [ input [ type_ "checkbox", checked True ] [], text " administrasjon/personal" ] ]
            , div [ class "field" ] [ label [ class "checkbox" ] [ input [ type_ "checkbox" ] [], text " administrasjon/kodeverk" ] ]
            , div [ class "field" ] [ label [ class "checkbox" ] [ input [ type_ "checkbox" ] [], text " utdanning/elev" ] ]
            ]
        ]


viewInnlogget : String -> Model -> Html Msg
viewInnlogget url model =
    div [ class "columns  is-flex-wrap-wrap" ] <|
        List.map (\( komp, d ) -> viewKomponent model url komp d) (Dict.toList model.helsestatus)


viewKomponent : Model -> String -> String -> WebData HealthStatus -> Html Msg
viewKomponent model url komponent komponentdata =
    let
        viewRessurstatus url_ =
            model.ressursstatus
                |> Dict.filter (\( k, h ) x -> k == komponent)
                |> Dict.toList
                |> List.map (viewRessurs model url_ model.ressursstatus)
    in
    div [ class "column is-two-fifths is-flex-grow-3" ]
        [ div [ class "card m-3" ]
            [ Html.header
                [ class "card-header" ]
                [ p [ class "card-header-title" ] [ text komponent ]
                , a [ attribute "aria-label" "more options", class "card-header-icon has-text-dark" ]
                    [ span [ class "icon" ]
                        [ i [ attribute "aria-hidden" "true", class "fas fa-angle-down" ]
                            []
                        ]
                    ]
                ]
            , div [ class "card-content" ]
                [ h4 [ class "title is-6 mb-3" ] [ text "Helsestatus" ]
                , viewHelsestatus komponentdata
                , h4 [ class "title is-6 mt-5 mb-3" ] [ text "Ressursstatus" ]
                , div [ class "field is-grouped is-grouped-multiline" ] <| viewRessurstatus url
                ]
            ]
        ]


viewHelsestatus : WebData HealthStatus -> Html Msg
viewHelsestatus helsestatus =
    let
        statusIkon hstatus ii =
            case hstatus.data |> take ii |> List.reverse |> head of
                Just x ->
                    let
                        ( icon, color ) =
                            if x.status == "APPLICATION_UNHEALTHY" then
                                ( "fa-times", "is-warning" )

                            else
                                ( "fa-check", "" )
                    in
                    [ span [ class <| "steps-marker " ++ color ] [ span [ class "icon" ] [ i [ class <| "fas " ++ icon ] [] ] ]
                    , span [ class "steps-content" ]
                        [ p [ class "is-size-7" ]
                            [ text (String.replace "_" " " x.status)
                            ]
                        ]
                    ]

                Nothing ->
                    [ span [ class "steps-marker" ] [ span [ class "icon" ] [ i [ class "fas fa-times" ] [] ] ] ]
    in
    case helsestatus of
        Success hstatus ->
            div []
                [ ul [ class "steps is-hollow is-balanced" ]
                    [ li [ class "steps-segment " ] <|
                        statusIkon hstatus 1
                    , li [ class "steps-segment" ] <|
                        statusIkon hstatus 2
                    , li [ class "steps-segment" ] <|
                        statusIkon hstatus 3
                    , li [ class "steps-segment" ] <|
                        statusIkon hstatus 4
                    ]
                ]

        Loading ->
            ul [ class "steps is-hollow" ]
                [ li [ class "steps-segment" ]
                    [ span [ class "steps-marker" ]
                        [ span [ class "icon" ] [ i [ class "fas fa-spinner fa-spin" ] [] ] ]
                    ]
                , li [ class "steps-segment" ]
                    [ span [ class "steps-marker" ]
                        [ span [ class "icon" ] [ i [ class "fas fa-spinner fa-pulse" ] [] ] ]
                    ]
                , li [ class "steps-segment" ]
                    [ span [ class "steps-marker" ]
                        [ span [ class "icon" ] [ i [ class "fas fa-spinner fa-pulse" ] [] ] ]
                    ]
                , li [ class "steps-segment" ]
                    [ span [ class "steps-marker" ]
                        [ span [ class "icon" ] [ i [ class "fas fa-spinner fa-pulse" ] [] ] ]
                    ]
                ]

        NotAsked ->
            text ""

        Failure e ->
            text <| "Feil med å hente helsestatus: " ++ Debug.toString e


viewRessurs : Model -> String -> Ressursstatus -> ( ( String, String ), CacheStatus ) -> Html Msg
viewRessurs model url ressursstatus ( ( komponent, ressurs ), ( cache, updated ) ) =
    div [ class "control" ]
        [ div [ class "tags has-addons" ]
            [ a
                [ class "tag is-dark"
                , href <| "https://" ++ url ++ "/?/" ++ komponent ++ "/" ++ ressurs
                , target "_blank"
                , title "Åpne ressurs i FINT Test Client"
                ]
                [ text ressurs ]
            , viewCasheStatus cache
            , viewUpdatedStatus model updated
            ]
        ]


viewCasheStatus : WebData Int -> Html Msg
viewCasheStatus cache_ =
    case cache_ of
        NotAsked ->
            span [ class "tag" ] [ text "" ]

        Loading ->
            span [ class "tag" ] [ span [ class "icon" ] [ i [ class "fas fa-spinner fa-pulse" ] [] ] ]

        Success cachevalue ->
            if cachevalue == 0 then
                span [ class "tag is-warning" ] [ text <| String.fromInt cachevalue ]

            else
                span [ class "tag is-success" ] [ text <| String.fromInt cachevalue ]

        Failure e ->
            span [ class "tag is-danger" ] [ text <| Debug.toString e ]


viewUpdatedStatus : Model -> WebData Int -> Html Msg
viewUpdatedStatus model updatedd_ =
    case updatedd_ of
        NotAsked ->
            span [ class <| "tag " ] [ text "" ]

        Loading ->
            span [ class <| "tag " ] [ span [ class "icon" ] [ i [ class "fas fa-spinner fa-pulse" ] [] ] ]

        Success updated_ ->
            if updated_ == 0 then
                span [ class <| "tag is-warning" ] [ text "Aldri oppdatert!" ]

            else if Time.posixToMillis model.tidLastetInn - updated_ > (15 * 60 * 1000) then
                span [ class <| "tag is-warning" ]
                    [ text <| Lib.Tid.tidMellomToTidspunkt model.timeZone (Time.posixToMillis model.tidLastetInn) updated_
                    ]

            else
                span [ class <| "tag is-success" ] [ text <| Lib.Tid.tidMellomToTidspunkt model.timeZone (Time.posixToMillis model.tidLastetInn) updated_ ]

        Failure e ->
            span [ class <| "tag is-danger" ] [ text <| Debug.toString e ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = \_ -> Time.every 1000 ClockIsTicking
        }
