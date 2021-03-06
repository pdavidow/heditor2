{-# LANGUAGE NumericUnderscores  #-}

module Scanner
    ( scanFile
    , opCountUpperLimit       
    , appendageLengthSumUpperLimit
    , charDeleteCountSumUpperLimit
    )
    where
        
import           Control.Monad.IO.Class (MonadIO, liftIO)        
import           Control.Monad.Trans.Except (ExceptT(..), except, runExceptT)
import           Text.Read (readMaybe)
import           Data.Char (isAlpha, isLower)
import           Data.Either.Combinators (maybeToRight)
import           Data.String.Utils (rstrip)
import           Data.Text (dropEnd, pack, takeEnd, unpack)
import           Safe (atMay, headMay, tailMay) 

 
data OpAppend = OpAppend String LineNum deriving (Eq, Show)
data OpDelete = OpDelete Int LineNum deriving (Eq, Show)
data OpPrint = OpPrint Int LineNum deriving (Eq, Show)
newtype OpUndo = OpUndo LineNum deriving (Eq, Show)
data OpUndoAppend = OpUndoAppend Int LineNum deriving (Eq, Show)
data OpUndoDelete = OpUndoDelete String LineNum deriving (Eq, Show)

data TaggedOp 
    = TaggedOpAppend OpAppend 
    | TaggedOpDelete OpDelete 
    | TaggedOpPrint OpPrint 
    | TaggedOpUndo OpUndo
    | TaggedOpUndoAppend OpUndoAppend
    | TaggedOpUndoDelete OpUndoDelete

data TaggedOpUndo
    = TaggedOpUndoOpUndoAppend OpUndoAppend
    | TaggedOpUndoOpUndoDelete OpUndoDelete

type ErrorMsg = String 
type Line = String      
type LineNum = Int

data Model = Model
    { _string :: String
    , _undos :: [TaggedOpUndo]
    , _appendageLengthSum :: Int
    , _charDeleteCountSum :: Int
    , _printOutput :: FilePath
    }

    
initialModel :: FilePath -> Model
initialModel = Model "" [] 0 0 


opCountUpperLimit, appendageLengthSumUpperLimit, charDeleteCountSumUpperLimit :: Int
opCountUpperLimit            = 1_000_000
appendageLengthSumUpperLimit = 1_000_000
charDeleteCountSumUpperLimit = 2_000_000


scanFile :: MonadIO m => FilePath -> FilePath -> m (Either ErrorMsg ())
scanFile input output = runExceptT $ do
    lines <- getLines input
    h <- except $ maybeToRight (errorWithLineNum 1 "Operation count expected") $ headMay lines
    opCount <- except $ maybeToRight (errorWithLineNum 1 "Operation count expected") (readMaybe h :: Maybe Int)
    opLines <- except $ maybeToRight (errorWithLineNum 2 "Operation expected") $ tailMay lines
    _ <- ExceptT $ parseOps output opCount opLines
    except $ Right ()


getLines :: MonadIO m => FilePath -> m [Line]
getLines x = do
    contents <- liftIO $ readFile x -- lazy
    pure $ lines $ rstrip contents       


parseOps :: MonadIO m => FilePath -> Int -> [Line] -> m (Either ErrorMsg Model)
parseOps output opCount xs = runExceptT $ 
    if opCount > opCountUpperLimit then except $ Left $ errorWithLineNum 1 $ "Operation count must be a positive integer <= " ++ show opCountUpperLimit
    else if opCount /= length xs then except $ Left $ errorWithLineNum 1 "Operation count does not match actual"
    else do
        let numberedLines = zip [(2 :: Int)..] xs 
        ops <- except $ parseCleanOps [] numberedLines
        ExceptT $ performOps (initialModel output) ops              


parseCleanOps :: [TaggedOp] -> [(Int, Line)] -> Either ErrorMsg [TaggedOp]
parseCleanOps acc [] = Right acc
parseCleanOps acc (numberedLine : xs) = do
    op <- parseOp numberedLine 
    parseCleanOps (acc ++ [op]) xs


parseOp :: (Int, Line) -> Either ErrorMsg TaggedOp
parseOp (lineNum, line) = do
    let tokens = words line 
    let tokenLength = length tokens
    _ <- if tokenLength > 0 then Right () else Left $ errorWithLineNum lineNum "Operation type expected"
    opCode <- maybeToRight (errorWithLineNum lineNum "Invalid operation type") (readMaybe $ head tokens :: Maybe Int) -- safe head

    case opCode of                    
        1 -> do
            args <- maybeToRight (errorWithLineNum lineNum "Append has one arg") $ tailMay tokens                                  
            if length args == 1 then do
                let appendage = head args -- safe
                if length appendage == length (filter (\x -> isAlpha x && isLower x) appendage) then
                    Right $ TaggedOpAppend $ OpAppend appendage lineNum
                else
                    Left $ errorWithLineNum lineNum "All input characters are lowercase English letters"  
            else
                Left $ errorWithLineNum lineNum "Append has one arg"  

        2 -> do
            args <- maybeToRight (errorWithLineNum lineNum "Delete has one arg") $ tailMay tokens    
            if length args == 1 then do
                let mbArg = readMaybe $ head args :: Maybe Int -- safe head
                case mbArg of
                    Just arg -> Right $ TaggedOpDelete $ OpDelete arg lineNum
                    Nothing -> Left $ errorWithLineNum lineNum "Delete has one int arg"
            else
                Left $ errorWithLineNum lineNum "Delete has one arg"  

        3 -> do
            args <- maybeToRight (errorWithLineNum lineNum "Print has one arg") $ tailMay tokens    
            if length args == 1 then do
                let mbArg = readMaybe $ head args :: Maybe Int -- safe head
                case mbArg of
                    Just arg -> 
                        Right $ TaggedOpPrint $ OpPrint arg lineNum
                    Nothing -> 
                        Left $ errorWithLineNum lineNum "Print has one int arg"
            else
                Left $ errorWithLineNum lineNum "Print has one arg"    

        4 ->
            if tokenLength == 1 then Right $ TaggedOpUndo $ OpUndo lineNum
            else Left $ errorWithLineNum lineNum "Undo has no args"    

        _ -> 
            Left $ errorWithLineNum lineNum "Invalid operation type"        
    

performOps :: MonadIO m => Model -> [TaggedOp] -> m (Either ErrorMsg Model)
performOps model [] = pure $ Right model
performOps model (op : xs) =
    case op of
        TaggedOpAppend (OpAppend appendage lineNum) ->
            let
                len = length appendage
                sum = _appendageLengthSum model + len
            in
                if sum <= appendageLengthSumUpperLimit then
                    let
                        model' =
                            Model
                                (_string model ++ appendage)
                                (_undos model ++ [TaggedOpUndoOpUndoAppend $ OpUndoAppend len 0]) -- dummy lineNum
                                sum
                                (_charDeleteCountSum model)
                                (_printOutput model)
                    in
                        performOps model' xs

                else
                    pure $ Left $ errorWithLineNum lineNum $ "Sum of lengths for all appendage args (optype 1) must be <= " ++ show appendageLengthSumUpperLimit ++ " (actual " ++ show sum ++ ")"
    
        TaggedOpDelete (OpDelete charsToDelete_Count lineNum) ->
            let
                sum = _charDeleteCountSum model + charsToDelete_Count
                appendage = unpack $ takeEnd charsToDelete_Count $ pack $ _string model
            in
                if sum > charDeleteCountSumUpperLimit then
                    pure $ Left $ errorWithLineNum lineNum $ "The total char delete count (for Delete) must be <= " ++ show charDeleteCountSumUpperLimit ++ ", but instead is " ++ show sum
                else 
                    case basicDelete charsToDelete_Count lineNum model of
                        Right model' -> 
                            let                                
                                model'' =
                                    Model
                                        (_string model')
                                        (_undos model' ++ [TaggedOpUndoOpUndoDelete $ OpUndoDelete appendage 0]) -- dummy lineNum
                                        (_appendageLengthSum model')
                                        sum
                                        (_printOutput model')
                            in
                                performOps model'' xs

                        Left err -> 
                            pure $ Left err

        TaggedOpPrint (OpPrint pos lineNum) ->
                if (pos - 1) < length (_string model) then do
                    liftIO $ appendFile (_printOutput model) $ (_string model !! (pos - 1)) : "\n" -- safe

                    let model' = Model            
                            (_string model)
                            (_undos model)
                            (_appendageLengthSum model)
                            (_charDeleteCountSum model)
                            (_printOutput model) 

                    performOps model' xs

                else
                    pure $ Left $ errorWithLineNum lineNum "Char position for Print exceeds string length"

        TaggedOpUndo (OpUndo lineNum) ->
            if null $ _undos model then performOps model xs
            else do
                let taggedOp = case last $ _undos model of
                        TaggedOpUndoOpUndoAppend (OpUndoAppend x _) -> TaggedOpUndoAppend (OpUndoAppend x lineNum)
                        TaggedOpUndoOpUndoDelete (OpUndoDelete x _) -> TaggedOpUndoDelete (OpUndoDelete x lineNum)
                
                result <- performOps model [taggedOp]

                case result of
                    Right model' ->
                        let 
                            model'' = 
                                Model
                                    (_string model')
                                    (init $ _undos model')
                                    (_appendageLengthSum model')
                                    (_charDeleteCountSum model')
                                    (_printOutput model')        
                        in
                            performOps model'' xs

                    Left err -> 
                        pure $ Left err

        TaggedOpUndoAppend (OpUndoAppend charsToDelete_Count lineNum) ->
            case basicDelete charsToDelete_Count lineNum model of
                Right model' -> 
                    performOps model' xs

                Left err -> 
                    pure $ Left err

        TaggedOpUndoDelete (OpUndoDelete appendage lineNum) -> 
            let
                len = length appendage
            in
                let
                    model' =
                        Model
                            (_string model ++ appendage)
                            (_undos model)
                            (_appendageLengthSum model)
                            (_charDeleteCountSum model)
                            (_printOutput model)
                in
                    performOps model' xs                    


basicDelete :: Int -> LineNum -> Model -> Either ErrorMsg Model
basicDelete charsToDelete_Count lineNum model =
    let
        len = length $ _string model
    in
        if len == 0 then
            Left $ errorWithLineNum lineNum "String may not be empty"
        else if charsToDelete_Count == 0 || charsToDelete_Count > len then
            Left $ errorWithLineNum lineNum "1 <= count <= string length"
        else Right $ Model
            (unpack $ dropEnd charsToDelete_Count $ pack $ _string model)
            (_undos model)
            (_appendageLengthSum model)
            (_charDeleteCountSum model)
            (_printOutput model)  


errorWithLineNum :: Int -> String -> String
errorWithLineNum lineNum error =
    error ++ ", line " ++ show lineNum
