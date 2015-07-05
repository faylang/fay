module D3TreeSample where

import FFI
import Data.Char

data Event
data D3SvgCanvas
data D3SvgDiagonal
data D3SvgLink
data D3SvgNode
data D3LayoutTree
data D3LayoutTreeNodes
data D3LayoutTreeLinks

--------- D3: A JavaScript visualization library for HTML and SVG. http://d3js.org
--------- Usage example with Fay

--------- JSON FEED

data JsonTree = JsonNode { name :: String, children :: JsonForest }
type JsonForest = [JsonTree]

unfoldTree :: (Int -> (String, [Int])) -> Int -> JsonTree
unfoldTree f b = let (a, bs) = f b in JsonNode a (unfoldForest f bs)

unfoldForest :: (Int -> (String, [Int])) -> [Int] -> JsonForest
unfoldForest f = map (unfoldTree f)

createJson :: Int -> JsonTree
createJson = unfoldTree (\n -> ([chr (n + ord 'a')], replicate n (n-1)))

--------- FFI

addWindowEvent :: String -> (Event -> Fay ()) -> Fay ()
addWindowEvent = ffi "window.addEventListener(%1, %2)"

createSvgCanvas :: Int -> Int -> Fay D3SvgCanvas
createSvgCanvas = ffi "d3.select('#viz')\
                                        \.append('svg:svg').attr('width', %1).attr('height', %2)\
                                        \.append('svg:g').attr('transform', 'translate(40, 40)')"

createTree :: Int -> Int -> Fay D3LayoutTree
createTree = ffi "d3.layout.tree().size([%1,%2])"

createDiagonal :: Fay D3SvgDiagonal
createDiagonal = ffi "d3.svg.diagonal().projection(function(d) { return [d.x, d.y]; })"

createNodes :: D3LayoutTree -> JsonTree -> Fay D3LayoutTreeNodes
createNodes = ffi "%1.nodes(%2)"

createLinks :: D3LayoutTree -> D3LayoutTreeNodes -> Fay D3LayoutTreeLinks
createLinks = ffi "%1.links(%2)"

displayLinks :: D3SvgCanvas -> D3LayoutTreeLinks -> D3SvgDiagonal -> Fay D3SvgLink
displayLinks = ffi "%1.selectAll('pathlink').data(%2)\
                                                     \.enter().append('svg:path')\
                                                     \.attr('class', 'link')\
                                                     \.attr('d', %3)"

displayNodes :: D3SvgCanvas -> D3LayoutTreeNodes -> Fay D3SvgNode
displayNodes = ffi "%1.selectAll('g.node').data(%2)\
                                                   \.enter().append('svg:g')\
                                                   \.attr('transform', \
                                                   \function(d) { return 'translate(' + d.x + ',' + d.y + ')'; })"

addDotToNodes :: D3SvgNode -> Double -> Fay ()
addDotToNodes = ffi "%1.append('svg:circle').attr('r', %2)"

addTextToNodes :: D3SvgNode -> Fay()
addTextToNodes = ffi "%1.append('svg:text')\
                                           \.attr('dx', function(d) { return d.children ? -8 : 8; })\
                                           \.attr('dy', 3)\
                                           \.attr('text-anchor', function(d) { return d.children ? 'end' : 'start'; })\
                                           \.text(function(d) { return d.name; })"

--------- START

d3TreeSample :: Event -> Fay()
d3TreeSample event = do
    svgCanvas <- createSvgCanvas 800 600
    d3Tree <- createTree 600 300
    diagonal <- createDiagonal

    nodes <- createNodes d3Tree (createJson 4)
    links <- createLinks d3Tree nodes

    svgLink <- displayLinks svgCanvas links diagonal
    svgNode <- displayNodes svgCanvas nodes

    addDotToNodes svgNode 2.5
    addTextToNodes svgNode


main :: Fay ()
main = do
    addWindowEvent "load" d3TreeSample
