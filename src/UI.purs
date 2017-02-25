{-
  (C) 2017 David Lettier
  lettier.com
-}

module UI where

import Prelude

import Data.Generic (gShow)
import Data.Int (fromString)
import Data.Tuple (fst, snd)
import Data.Array (null, range, updateAt, (!!))
import Data.Either (Either(..))
import Data.Maybe (fromMaybe, isJust)

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Free (class Affable)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)

import Halogen as H
import Halogen.HTML.Core (className)
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Indexed as HH
import Halogen.Util (awaitBody, runHalogenAff, selectElement)

import Utils (
      maybeToString
    , stringToMaybeNumber
  )

import MatrixInverse (
      Matrix
    , invertMatrix
    , matrixSizeValid
    , maybeMatrixValue
    , defaultMatrix
    , identityMatrix
    , defaultMatrixValue
    , minMatrixSize
    , maxMatrixSize
  )

type Effects eff = H.HalogenEffects (console :: CONSOLE | eff)

data Query a = UpdateMatrixSize String a | UpdateMatrixValue Int Int String a | Run a

type State = {
      matrixSize :: Int
    , matrixA :: Matrix
    , matrixB :: Matrix
    , status :: String
    , running :: Boolean
    , finished :: Boolean
  }

initialState :: State
initialState = { matrixSize: 0, matrixA: [], matrixB: [], status: "", running: false, finished: false }

runApp :: Eff (Effects ()) Unit
runApp = runHalogenAff do
  body <- awaitBody
  uiContainer <- selectElement "#uiContainer"
  H.runUI matrixUIComponent initialState (fromMaybe body uiContainer)

matrixUIComponent :: forall eff. H.Component State Query (Aff (Effects eff))
matrixUIComponent = H.component { render, eval }
  where
    render :: State -> H.ComponentHTML Query
    render state =
      HH.div_
        [
            HH.div_ [
                  HH.div [
                        HP.class_ (className "status")
                    ] [
                        HH.b_ [ HH.text "Status: " ]
                      , HH.text state.status
                    ]
                , HH.input [
                        HP.value ""
                      , HP.placeholder "Input Matrix Size and Press Enter"
                      , HE.onValueChange (HE.input UpdateMatrixSize)
                      , HP.class_ (className "matrixSizeInput")
                    ]
                , HH.button [
                        HE.onClick (HE.input_ Run)
                      , HP.class_ (className "runButton")
                      , HP.title "RUN"
                    ] [
                        HH.text "RUN"
                    ]
              ]
          , HH.div [
              HP.class_ (className "matricesContainer")
            ]
            if matrixSizeValid state.matrixSize
              then [
                    HH.div [
                          HP.class_ (className "matrixAContainer")
                      ] (
                        map (\ row ->
                              HH.div [
                                    HP.class_ (className "matrixRow")
                                ] (
                                  map (\ col ->
                                      HH.div [
                                            HP.classes $ map className ["matrixValue"]
                                        ] [
                                            HH.input [
                                                  HP.value $ maybeToString $ maybeMatrixValue state.matrixA row col
                                                , HP.placeholder (gShow defaultMatrixValue)
                                                , HE.onValueChange (HE.input (UpdateMatrixValue row col))
                                                , HP.classes $ map className [
                                                      "matrixValueInput"
                                                    , if row == col
                                                        then
                                                          if state.finished
                                                            then "lightRed"
                                                            else "lightBlue"
                                                        else
                                                          if state.finished
                                                            then "red"
                                                            else "blue"
                                                  ]
                                                , HP.disabled state.running
                                                , HP.title $ maybeToString $ maybeMatrixValue state.matrixA row col
                                              ]
                                        ]
                                    ) (range 0 (state.matrixSize -1))
                                )
                          ) (range 0 (state.matrixSize - 1))
                      )
                  , HH.div [
                          HP.class_ (className "matrixBContainer")
                      ] (
                        map (\ row ->
                              HH.div [
                                    HP.class_ (className "matrixRow")
                                ] (
                                  map (\ col ->
                                      HH.div [
                                            HP.classes $ map className ["matrixValue"]
                                        ] [
                                            HH.input [
                                                  HP.value $ maybeToString $ maybeMatrixValue state.matrixB row col
                                                , HP.placeholder (gShow defaultMatrixValue)
                                                , HP.classes $ map className [
                                                      "matrixValueInput"
                                                    , if row == col
                                                        then
                                                          if state.finished
                                                            then "lightBlue"
                                                            else "lightRed"
                                                        else
                                                          if state.finished
                                                            then "blue"
                                                            else "red"
                                                  ]
                                                , HP.disabled true
                                                , HP.title $ maybeToString $ maybeMatrixValue state.matrixB row col
                                              ]
                                        ]
                                    ) (range 0 (state.matrixSize -1))
                                )
                          ) (range 0 (state.matrixSize - 1))
                      )
                ]
              else []
        ]
    eval :: Query ~> H.ComponentDSL State Query (Aff (Effects eff))
    eval (UpdateMatrixSize value next) = do
      let matrixSize = fromMaybe 0 (fromString value)
      if not $ matrixSizeValid matrixSize
        then H.modify (\ state ->
            state {
                  matrixSize = 0
                , matrixA    = []
                , matrixB    = []
                , status     = "Enter a number between " <> (gShow minMatrixSize) <> " and " <> (gShow maxMatrixSize)
                , running    = false
                , finished   = false
              }
          )
        else do
          H.modify (\ state ->
              state {
                    matrixSize = matrixSize
                  , matrixA    = defaultMatrix matrixSize
                  , matrixB    = identityMatrix matrixSize
                  , status     = ""
                  , running    = false
                  , finished   = false
                }
            )
      pure next
    eval (UpdateMatrixValue row col value next) = do
      log' value
      currentState <- H.get
      let maybeNumber = stringToMaybeNumber value
      let matrixRow = fromMaybe [] (currentState.matrixA !! row)
      let number = fromMaybe defaultMatrixValue maybeNumber
      when (not $ null matrixRow) do
        let maybeUpdatedRow = updateAt col number matrixRow
        when (isJust maybeUpdatedRow) do
          let updatedRow = fromMaybe [] maybeUpdatedRow
          let maybeUpdatedMatrix = updateAt row updatedRow currentState.matrixA
          when (isJust maybeUpdatedMatrix) do
            H.modify (\ state ->
                state {
                      matrixA  = (fromMaybe [] maybeUpdatedMatrix)
                    , running  = false
                    , finished = false
                  }
              )
      pure next
    eval (Run next) = do
      H.modify (\ state -> state { status = "Running", running = true })
      currentState <- H.get
      let matrix = currentState.matrixA
      log' (gShow matrix)
      let result = invertMatrix matrix
      let fstResult = fst result
      let sndResult = snd result
      case fstResult of
        Left s ->
          H.modify (\ state ->
              state {
                    status  = s
                  , running = false
                  , finished = false
                }
            )
        Right m -> do
          log' (gShow m)
          log' (gShow sndResult)
          H.modify (\ state ->
              state {
                    status   = "Finished"
                  , matrixA  = m
                  , matrixB  = sndResult
                  , running  = false
                  , finished = true
                }
            )
      pure next

log' :: forall a b. Affable (console :: CONSOLE | b) a => String -> a Unit
log' string = H.fromAff $ when debug $ log string

debug :: Boolean
debug = true
