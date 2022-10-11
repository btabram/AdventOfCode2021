import Control.Exception (assert)
import Data.List (foldl')
import Data.Map ((!))
import qualified Data.Map as Map
import Text.Regex.TDFA ((=~)) -- required `cabal install --lib regex-tdfa`


type Memory = Map.Map Char Integer
type State = (Memory, [Integer])
type Command = State -> State


main = do
    input <- fmap lines $ readFile "input.txt"
    let commands = map toCommand input

    {-
    The problem is too big to brute force. Inspecting the input we see that the same group of instructions are repeated
    14 times. Below is one example from input.txt, every reptition is the same except for three values which are marked
    with (*). With the instructions are notes on what they're doing:

    inp w       w = input
    mul x 0     x = 0
    add x z     x = z
    mod x 26    x = z % 26
(*) div z 26    z = z // 26
(*) add x -11   x = (z % 26) - 11
    eql x w     x = ((z % 26) - 11) == input ? 1 : 0
    eql x 0     x = ((z % 26) - 11) == input ? 0 : 1
    mul y 0     y = 0
    add y 25    y = 25
    mul y x     y = 0       | 25                            <depending on x>
    add y 1     y = 1       | 26                            <depending on x>
    mul z y     z = z // 26 | (z // 26) * 26                <depending on x>
    mul y 0     y = 0
    add y w     y = input
(*) add y 5     y = input + 5
    mul y x     y = 0       | input + 5                     <depending on x>
    add z y     z = z // 26 | ((z // 26) * 26) + input + 5  <depending on x>

    We can see that this repeated group of instructions is equivalent to:
        if ((z % 26) + b == input) {
            z = z // a
        } else {
            z = ((z // a) * 26) + input + c
        }
    where:
        a (div z value)
        b (add x value)
        c (add y value)

    We can now rewrite the program in a concise way, pulling out the values of a, b and c by hand:
        (where i1 is the first input value etc., i.e. the first digit of the model number)
    -}

    let prog = [step 1 10 2,     -- always False,                  then z1 = i1 + 2
                step 1 10 4,     -- always False,                  then z2 = (26 * z1) + i2 + 4
                step 1 14 8,     -- always False,                  then z3 = (26 * z2) + i3 + 8
                step 1 11 7,     -- always False,                  then z4 = (26 * z3) + i4 + 7
                step 1 14 12,    -- always False,                  then z5 = (26 * z4) + i5 + 12
                step 26 (-14) 7, -- True when (i5 + 12 - 14 = i6), then z6 = z4
                step 26 0 10,    -- True when (i4 + 7 + 0 = i7),   then z7 = z3
                step 1 10 14,    -- always False,                  then z8 = (26 * z3) + i8 + 14
                step 26 (-10) 2, -- True when (i8 + 14 - 10 = i9), then z9 = z3
                step 1 13 6,     -- always False,                  then zA = (26 * z3) + iA + 6
                step 26 (-12) 8, -- True when (iA + 6 - 12 = iB),  then zB = z3
                step 26 (-3) 11, -- True when (i3 + 8 - 3 = iC),   then zC = z2
                step 26 (-11) 5, -- True when (i2 + 4 - 11 = iD),  then zD = z1
                step 26 (-2) 11] -- True when (i1 + 2 - 2 = iE),   then zE = 0
    {-
    We can see that there's seven steps with a = 1 and seven with a = 26. Due to the b values, the modulo z condition
    in every a = 1 step must always be False and these steps are all therfore multiplying by 26 and adding a constant.
    Looking at the a = 26 steps we can see that they either div z by 26, if the modulo condition is True, or they change
    the constant part of z. To end up with z = 0 we need all seven a = 1 step to div by 26, cancelling out the seven
    a = 1 steps which are multiplying by 26. By working through the z values at every step we can get a series of
    constaints for what makes a valid model number:

        i5 = i6 + 2, i7 = i4 + 7, i9 = i8 + 4, iA = iB + 6, iC = i3 + 5, i2 = iD + 7, i1 = iE
    -}

    -- Which allows us to constuct our answers:
    --           1  2  3  4  5  6  7  8  9  A  B  C  D  E
    let part1 = [9, 9, 4, 2, 9, 7, 9, 5, 9, 9, 3, 9, 2, 9] -- the biggest valid model number
    let part2 = [1, 8, 1, 1, 3, 1, 8, 1, 5, 7, 1, 6, 1, 1] -- the smallest valid model number

    let isValidFull inputs = (fst $ foldl' (\acc command -> command acc) (emptyMemory, inputs) commands) ! 'z' == 0
    let isValidConcise inputs = foldl' (\z (step, input) -> step input z) 0 (zip prog inputs) == 0
    let validate inputs = assert (isValidFull inputs) assert (isValidConcise inputs) inputs

    putStrLn $ "The answer to Part 1 is " ++ map (head . show) (validate part1)
    putStrLn $ "The answer to Part 2 is " ++ map (head . show) (validate part2)


commandRegex = "^(inp|add|mul|div|mod|eql) (w|x|y|z) ?(-?[0-9]+|w|x|y|z)?$"

toCommand :: String -> Command
toCommand str = let (_, _, _, groups) = str =~ commandRegex :: (String, String, String, [String]) in parse groups
    where   parse ["inp", [a], _] = input a
            parse ["add", [a], b] = add a b
            parse ["mul", [a], b] = multiply a b
            parse ["div", [a], b] = divide a b
            parse ["mod", [a], b] = modulo a b
            parse ["eql", [a], b] = equal a b


emptyMemory :: Memory
emptyMemory = foldr (`Map.insert` 0) Map.empty "wxyz"


input :: Char -> State -> State
input a (vars, inputs) = let i : rest = inputs in (Map.insert a i vars, rest)

instructionBase :: (Integer -> Integer -> Integer) -> Char -> String -> State -> State
instructionBase op a b (vars, inputs) = let result = (vars ! a) `op` (getValue b) in (Map.insert a result vars, inputs)
    where getValue x
            | x `elem` ["w", "x", "y", "z"] = vars ! (head x)
            | otherwise                     = read x

add = instructionBase (+)
multiply = instructionBase (*)
divide = instructionBase div
modulo = instructionBase mod
equal = instructionBase (\a b -> if a == b then 1 else 0)


step :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
step a b c input z = if (z `mod` 26) + b == input then z `div` a else ((z `div` a) * 26) + input + c