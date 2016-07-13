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

playerSize: Float
playerSize = 10.0
playerSpeed : Float
playerSpeed = 0.12
playerFwdSpeed : Float
playerFwdSpeed = 4
playerGrowth : Int
playerGrowth = 10
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
    newPlayer = if isDead updatedPlayer then initialPlayer else updatedPlayer
    nextCmd = if willEatFood updatedPlayer game.food then generateFoodPosition else Cmd.none
  in
    ( { game |
        player = newPlayer
    }, nextCmd )

willEatFood: Player -> Point -> Bool
willEatFood player food =
  case head player.points of
      Nothing -> False
      Just point -> intersects point food playerSize

isDead: Player -> Bool
isDead player =
  let
    maybeHead = head player.points
    headHitBody = case maybeHead of
                    Nothing -> False
                    Just headPoint ->
                      any (\p -> (intersects headPoint p playerSize)) (drop 20 player.points)
    headHitWall = 
                  case maybeHead of
                    Nothing -> False
                    Just headPoint ->
                      isPointInCircle (Point halfWidth headPoint.y) headPoint playerSize ||
                      isPointInCircle (Point -halfWidth headPoint.y) headPoint playerSize ||
                      isPointInCircle (Point headPoint.x halfHeight) headPoint playerSize ||
                      isPointInCircle (Point headPoint.x -halfHeight) headPoint playerSize

  in
    headHitBody || headHitWall

isPointInCircle: Point -> Point -> Float -> Bool
isPointInCircle point circleCenter radius =
  sqrt ((point.x-circleCenter.x)*(point.x-circleCenter.x) + (point.y-circleCenter.y)*(point.y-circleCenter.y)) < radius

intersects: Point -> Point -> Float -> Bool
intersects c1 c2 radius =
  let
    distanceX = c1.x - c2.x
    distanceY = c1.y - c2.y
    radiusSum = radius*2
  in
    distanceX * distanceX + distanceY * distanceY <= radiusSum * radiusSum

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
                        Point (playerFwdSpeed*cos angle) (playerFwdSpeed*sin angle)
                    Just point ->
                        Point (point.x+(playerFwdSpeed*cos angle)) (point.y+(playerFwdSpeed*sin angle))
    in
        newHead :: player.points |> (\l -> if length l > player.length then take player.length l else l)
    

updatePlayer: Direction -> Game -> Player
updatePlayer dir {player,food} =
  let
    newAngle = updatePlayerAngle player.angle dir
    points = updatePlayerPosition player newAngle
    newLength = if willEatFood { player | points = points } food then player.length+playerGrowth else player.length

  in
    { player | angle = newAngle, points = points, length = newLength }

makePlayer : Player -> List Form
makePlayer player =
  let
    angle = player.angle
  in
    indexedMap  (\i p -> 
        circle playerSize
            |> filled (hsl ((toFloat i)*0.03) 1 0.5)
            |> move (p.x, p.y))
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


initialPlayer: Player
initialPlayer =
  Player (degrees 0) [] 10

init : (Game, Cmd Msg)
init =
  let
    ( keyboardModel, keyboardCmd ) = Keyboard.init
  in
    ( { player = initialPlayer,
        food = Point 0 0,
        keyboardModel = keyboardModel,
        direction = Still
      }
    , Cmd.batch [Cmd.map KeyboardExtraMsg keyboardCmd, generateFoodPosition]
    )

randomPosition: Float -> Random.Generator Float
randomPosition size =
  Random.float (-size+50) (size-50)

generateFoodPosition: Cmd Msg
generateFoodPosition =
    Random.generate NewFood
      <| Random.pair (randomPosition halfWidth) (randomPosition halfHeight)

subscriptions : Game -> Sub Msg
subscriptions game =
  Sub.batch [
    AnimationFrame.times (\time -> Step time),
    Sub.map KeyboardExtraMsg Keyboard.subscriptions
  ]