module DotRender(
                 writeDotRun
                ) where

import Data.List
import Dot
import Resolver.Log
import Resolver.PrettyPrint
import Resolver.Types
import System.IO
import Text.Printf
import Types
import qualified Data.Set as Set


inBounds :: Params -> Integer -> Bool
inBounds params n = let first = maybe 0 id (firstStep params) in
                    n >= first && maybe True (\max -> n < first + max) (maxSteps params)

choiceText :: LinkChoice -> String
choiceText (LinkChoice (InstallVersion ver _ _)) = "Install " ++ pp ver
choiceText (LinkChoice (BreakSoftDep d)) = "Break " ++ pp d
choiceText Unknown = "(...)"

dotChoiceLabel :: LinkChoice -> String
dotChoiceLabel lc@(LinkChoice c) = choiceText lc
dotChoiceLabel Unknown           = ""

inferTargetFormat :: Params -> TargetFormat
inferTargetFormat (Params { targetFormat = fmt,
                            dotOutput    = output }) =
    case fmt of
      Nothing -> PS
      Just fmt' -> fmt'

cloudImage :: Params -> String
cloudImage params =
    case inferTargetFormat params of
      PS -> "cloud.eps"
      PNG -> "cloud.png"

dotStepNode :: Params -> ProcessingStep -> Node
dotStepNode params step = node (name $ printf "step%d" (stepOrder step))
                          <<< set "label" (printf "Step: %d\nScore: %d\nTier: %s"
                                           (stepOrder step)
                                           (solScore $ stepSol step)
                                           (show $ solTier $ stepSol step))
                          <<< Set.null (solBrokenDeps (stepSol step)) `thenDo`
                              set "style" "filled" `andAlso`
                              set "fillColor" "lightgrey"

-- Generate nodes for any successors that were not processed in the
-- render.
dotUnprocessedSuccs :: Params -> ProcessingStep -> [Node]
dotUnprocessedSuccs params step = unprocessed ++ excluded
    where unprocessed = [ node (name $ printf "step%dunproc%d" (stepOrder step) stepNum)
                          <<< set "label" (printf "Unprocessed\nScore: %d\nTier: %s"
                                           (solScore succSol)
                                           (show $ solTier succSol))
                          <<< set "style" "dashed"
                          <<< Set.null (solBrokenDeps (stepSol step)) `thenDo`
                              set "style" "dashed,filled" `andAlso`
                              set "fillcolor" "lightgrey"
                          | ((Unprocessed { successorChoice    = succChoice,
                                            successorSolution  = succSol }),
                             stepNum)
                          <- zip (stepSuccessors step) ([0..] :: [Integer]) ]
          excluded    = [ node (name $ printf "step%d" (stepOrder step))
                          <<< set "label" (printf "Step %d+\n%d nodes..." (stepOrder step) (stepBranchSize step))
                          <<< set "shape" "plaintext"
                          <<< set "image" (cloudImage params)
                          | (Successor { successorStep = step }) <- stepSuccessors step,
                            not $ inBounds params (stepOrder step) ]

dotEdges params step = processed ++ unprocessed
    where processed   = [ edge (node (name $ printf "step%d" (stepOrder step)))
                               (node (name $ printf "step%d" (stepOrder step')))
                          <<< set "label" (dotChoiceLabel succChoice)
                          <<< forced `thenDo`
                              set "style" "bold" `andAlso`
                              set "color" "darkgreen" `andAlso`
                              set "labelcolor" "darkgreen"
                          | Successor { successorStep   = step',
                                        successorChoice = succChoice,
                                        successorForced = forced } <- stepSuccessors step ]
          unprocessed = [ edge (node (name $ printf "step%d" (stepOrder step)))
                               (node (name $ printf "step%dunproc%d" (stepOrder step) stepNum))
                          <<< set "label" (dotChoiceLabel succChoice)
                          <<< forced `thenDo` set "style" "bold"
                          | ((Unprocessed { successorChoice = succChoice,
                                            successorForced = forced  }), stepNum)
                              <- zip (stepSuccessors step) ([0..] :: [Integer]) ]

dotOrderEdges steps =
    [ edge (node (name $ printf "step%d" (stepOrder step1)))
           (node (name $ printf "step%d" (stepOrder step2)))
      <<< set "constraint" "false"
      <<< set "style" "dotted"
      <<< set "color" "blue"
      | (step1, step2) <- zip steps (drop 1 steps) ]

renderDot :: Params -> [ProcessingStep] -> Digraph
renderDot params steps =
    let droppedSteps   = maybe steps (\n -> genericDrop n steps) (firstStep params)
        truncatedSteps = maybe droppedSteps (\n -> genericTake n droppedSteps) (maxSteps params) in
    if null truncatedSteps
    then error "No steps to render."
    else let stepNodes          = map (dotStepNode params) truncatedSteps
             unprocessed        = concat $ map (dotUnprocessedSuccs params) truncatedSteps
             stepEdges          = concat $ map (dotEdges params) truncatedSteps
             orderEdges         = dotOrderEdges truncatedSteps in
         digraph (stepNodes ++ unprocessed) (stepEdges ++ orderEdges)

writeDotRun params steps outputFile =
    do let dot = renderDot params steps
       withFile outputFile WriteMode $ \h ->
           hPutStrLn h (show dot)
