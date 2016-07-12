import Time exposing (..)
import List exposing (..)
import AnimationFrame
import Keyboard.Extra as Keyboard
import Window
import Collage exposing (..)
import Element exposing (..)
import Color exposing (..)
import Html.App as App
import Html
import Random

-- MODEL

type alias Point = {
    x: Float,
    y: Float
}

type alias Player =
  { angle: Float,
    points: List Point,
    length: Int
  }
type Direction = Left | Right | Still

type alias Game =
  {
    player : Player,
    food: Point,
    direction : Direction,
    keyboardModel : Keyboard.Model
  }


type Msg
  = Step Time
  | KeyboardExtraMsg Keyboard.Msg
  | NewFood (Float, Float)


(gameWidth, gameHeight) = (1024, 576) -- 16:9
(halfWidth, halfHeight) = (gameWidth/2, gameHeight/2)
(iHalfWidth, iHalfHeight) = (gameWidth//2, gameHeight//2)

playerRadius : Float
playerRadius = gameWidth / 10.0
playerSize: Float
playerSize = 10.0
playerSpeed : Float
playerSpeed = 0.06
bgBlack : Color
bgBlack =
  rgb 20 20 20


{-| Game loop: Transition from one state to the next. -}
update : Msg -> Game -> (Game, Cmd Msg)
update msg game =
  case msg of
    KeyboardExtraMsg keyMsg -> onUserInput keyMsg game
    Step time -> onFrame time game
    NewFood (x, y) -> ({ game | food = Point x y }, Cmd.none)

{-| Updates the game state on a keyboard command -}
onUserInput : Keyboard.Msg -> Game -> (Game, Cmd Msg)
onUserInput keyMsg game =
  let
    ( keyboardModel, keyboardCmd ) =
      Keyboard.update keyMsg game.keyboardModel
    direction =
      if (Keyboard.arrows keyboardModel).x < 0 then Left
      else if (Keyboard.arrows keyboardModel).x > 0 then Right
      else Still
  in
    ( { game | keyboardModel = keyboardModel, direction = direction }, Cmd.map KeyboardExtraMsg keyboardCmd )

{-| Updates the game state on every frame -}
onFrame : Time -> Game -> (Game, Cmd Msg)
onFrame time game =
  let
    updatedPlayer = updatePlayer game.direction game
    nextCmd = if willEatFood updatedPlayer game.food then generateFoodPosition else Cmd.none
  in
    ( { game |
        player = updatedPlayer
    }, nextCmd )

willEatFood: Player -> Point -> Bool
willEatFood player food =
    case head player.points of
        Nothing -> False
        Just point -> isPointInCircle point food (playerSize*2)

isPointInCircle: Point -> Point -> Float -> Bool
isPointInCircle point circleCenter radius =
    sqrt ((point.x-circleCenter.x)*(point.x-circleCenter.x) + (point.y-circleCenter.y)*(point.y-circleCenter.y)) < radius

updatePlayerAngle: Float -> Direction -> Float
updatePlayerAngle angle dir =
  let
    sign =
      if dir == Left then 1
      else if dir == Right then -1
      else 0
    newAngle = angle + toFloat sign * playerSpeed
  in
    if newAngle < 0 then
      newAngle + 2 * pi
    else if newAngle > 2 * pi then
      newAngle - 2 * pi
    else
      newAngle

updatePlayerPosition: Player -> Float -> List Point
updatePlayerPosition player angle =
    let
       newHead =
                case head player.points of
                    Nothing ->
                        Point (2*cos angle) (2*sin angle)
                    Just point ->
                        Point (point.x+(4*cos angle)) (point.y+(4*sin angle))
    in
        newHead :: player.points |> (\l -> if length l > player.length then take player.length l else l)
    

updatePlayer: Direction -> Game -> Player
updatePlayer dir {player,food} =
  let
    newAngle = updatePlayerAngle player.angle dir
    points = updatePlayerPosition player newAngle
    newLength = if willEatFood { player | points = points } food then player.length+10 else player.length

  in
    { player | angle = newAngle, points = points, length = newLength }


moveRadial : Float -> Float -> Form -> Form
moveRadial angle radius =
  move (radius * cos angle, radius * sin angle)

makePlayer : Player -> List Form
makePlayer player =
  let
    angle = player.angle
  in
    indexedMap  (\i p -> 
        circle playerSize
            |> filled (hsl (toFloat i) 1 0.5)
            |> move (p.x, p.y)
            |> rotate angle)
        player.points
view : Game -> Html.Html Msg
view game =
  let
    bg = rect gameWidth gameHeight |> filled bgBlack
    field = makePlayer game.player
    food = circle playerSize
            |> filled (hsl 2 1 0.5)
            |> move (game.food.x, game.food.y)
  in
    toHtml <|
    container gameWidth gameHeight middle <|
    collage gameWidth gameHeight
      (bg::food::field)

main =
  App.program
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions }


init : (Game, Cmd Msg)
init =
  let
    ( keyboardModel, keyboardCmd ) = Keyboard.init
  in
    ( { player = Player (degrees 0) [] 10,
        food = Point 0 0,
        keyboardModel = keyboardModel,
        direction = Still
      }
    , Cmd.batch [Cmd.map KeyboardExtraMsg keyboardCmd, generateFoodPosition]
    )

generateFoodPosition: Cmd Msg
generateFoodPosition =
    Random.generate NewFood (Random.pair (Random.float -halfWidth halfWidth) (Random.float -halfHeight halfHeight))

subscriptions : Game -> Sub Msg
subscriptions game =
  Sub.batch [
    AnimationFrame.times (\time -> Step time),
    Sub.map KeyboardExtraMsg Keyboard.subscriptions
  ]