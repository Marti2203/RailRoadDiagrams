namespace RailroadDiagrams
open System.Text.RegularExpressions
open System.Collections.Generic
open System.IO
[<AutoOpen>]
module Utilities =
    let mutable DefaultConfiguration = new DiagramConfiguration()
    let EscapeEntities text =
        Regex.Replace(text,"[*_\`\[\]<&]",MatchEvaluator(fun m -> sprintf "&#%i" <| int m.Value.[0]))
    let DetermineGaps(outer:float, inner:float) =
            let diff = outer - inner
            match DefaultConfiguration.InternalAlignment with
            | TextAlignment.Left -> (0.,diff)
            | TextAlignment.Right -> (diff,0.)
            | TextAlignment.Center -> (diff/2.,diff/2.)
            | _ -> invalidArg "configuration.InternalAlignment" "Invalid alignment"

    let internal diDict vs = Dictionary(dict vs)
    let internal dictFromPair (k,v) = diDict [k,v]
    let DefaultStyle = """
        svg.railroad-diagram {
            background-color:hsl(30,20%,95%);
        }
        svg.railroad-diagram path {
            stroke-width:3;
            stroke:black;
            fill:rgba(0,0,0,0);
        }
        svg.railroad-diagram text {
            font:bold 14px monospace;
            text-anchor:middle;
        }
        svg.railroad-diagram text.label{
            text-anchor:start;
        }
        svg.railroad-diagram text.comment{
            font:italic 12px monospace;
        }
        svg.railroad-diagram rect{
            stroke-width:3;
            stroke:black;
            fill:hsl(120,100%,90%);
        }
    """

/// Base Class for an item
type DiagramItem(name:string,attributes:Dictionary<string,string>,text:DiString option) =
        let childContainer = match text with
                             | Some(text) ->  ResizeArray([text])
                             | None ->  ResizeArray()
        new (name:string) = DiagramItem(name,Dictionary(),None)
        new (name:string, attributes:Dictionary<string,string>) = DiagramItem(name,attributes, None)
        /// Distance it projects above the entry line
        member val Up = 0. with get,set
        /// Distance it projects below the exit line
        member val Down = 0. with get,set
        /// Size of element
        member val Width = 0. with get,set
        /// Distance between the entry/exit lines
        member val Height = 0. with get,set
        member val Attributes = attributes with get
        member val Children = childContainer with get
        /// Needs space between other elements
        member val NeedsSpace = false with get,set

        member internal self.AddDebug(el:DiagramItem)=
            if DefaultConfiguration.Debug then
                el.Attributes.["data-x"] <- (sprintf "%s w:%f h:%f/%f/%f" (el.GetType().Name) el.Width el.Up el.Height el.Down)


        /// Format Item for specific size. Chainable
        abstract Format : float * float * float -> DiagramItem
        default self.Format (x,y,width) = self
        member self.AddTo(parent:DiagramItem) =
            parent.Children.Add(Choice1Of2 self)
            self

        override self.ToString() = sprintf "DiagramItem %f %f %f %f" self.Up self.Down self.Width self.Height
        ///Output SVG to writer
        abstract WriteSvg : TextWriter -> unit
        default self.WriteSvg (writer:TextWriter) =
                writer.Write(sprintf "<%s" name)
                self.Attributes
                |> Seq.sortBy (fun x -> x.Key)
                |> Seq.iter (fun x -> writer.Write(sprintf" %s=\"%s\"" x.Key <| EscapeEntities x.Value))
                writer.Write(">")
                if Seq.contains name <| ["g"; "svg"] then
                    writer.Write("\n")
                self.Children
                |> Seq.iter(fun child -> match child with
                                         | Choice1Of2 c -> c.WriteSvg(writer)
                                         | Choice2Of2 x -> x |> EscapeEntities |> writer.Write)
                writer.Write(sprintf "</%s>" name)
and DiString = Choice<DiagramItem,string>

///A line
type Path(x, y)=
    inherit DiagramItem("path", dictFromPair("d",sprintf "M%f %f" x y))
    ///Move start of path to
    member self.MoveTo(x:float, y:float) =
        self.Attributes.["d"] <- self.Attributes.["d"] + sprintf "m%f %f" x y
        self

    //Draw a line to x,y
    member self.LineTo(x:float, y:float) =
        self.Attributes.["d"] <- self.Attributes.["d"] + sprintf "l%f %f" x y
        self

    member self.HorizontalLine(value:float) =
        self.Attributes.["d"] <- self.Attributes.["d"] + sprintf "h%f" value
        self

    member self.RightLine(value:float) = self.HorizontalLine(max 0. value)

    member self.LeftLine(value:float) = self.HorizontalLine(- max 0.  value)

    member self.VerticalLine(value:float) =
        self.Attributes.["d"] <- self.Attributes.["d"] +  sprintf "v%f" value
        self

    member self.DownLine(value:float) = self.VerticalLine( max 0. value)

    member self.UpLine(value:float) = self.VerticalLine(- max 0. value)

    ///Draw an eigth of an circle
    member self.Arc(start, dir)=
        let AR = DefaultConfiguration.ArcRadius
        let s2 = (AR / sqrt(2.))
        let s2inv = AR - s2
        let path = sprintf "a %f %f 0 0 %i %f %f" AR AR 
                    <|  if dir = "cw" then 1 else 0
                    <|| match start + dir with
                        | "swccw"  
                        | "ncw"   -> (s2, s2inv) 
                        | "wccw"   
                        | "necw"  -> (s2inv, s2) 
                        | "nwccw"  
                        | "ecw"   -> (-s2inv, s2) 
                        | "nccw"  
                        | "secw"  -> (-s2, s2inv) 
                        | "neccw"  
                        | "scw"   -> (-s2, -s2inv) 
                        | "eccw"   
                        | "swcw"  -> (-s2inv, -s2) 
                        | "seccw"  
                        | "wcw"   -> (s2inv, -s2) 
                        | "nwcw"  
                        | "sccw"  -> (s2, -s2inv) 
                        | _ -> failwith "ERROR??"

        self.Attributes.["d"] <- self.Attributes.["d"] + path
        self

    member self.Arc(sweep:string)=
        let AR = DefaultConfiguration.ArcRadius
        let x = AR * if sweep.[0] = 'e' || sweep.[1] = 'w' then -1. else 1.
        let y = AR * if sweep.[0] = 's' || sweep.[1] = 'n' then -1. else 1.
        let cw = if sweep = "ne" || sweep = "es" || sweep = "sw" || sweep = "wn" then 1 else 0
        self.Attributes.["d"] <- self.Attributes.["d"] + sprintf "a%f %f 0 0 %i %f %f" AR AR cw x y
        self

    override self.Format (x,y,width)=
        self.Attributes.["d"] <- self.Attributes.["d"] + "h.5"
        self :> DiagramItem

    override self.ToString() = sprintf "Path(%f %f)" x y
/// CSS Styling
type Style(css:string) =
    inherit DiagramItem("style")  
    override self.ToString() = sprintf "Style(%s)" css
    override self.WriteSvg(writer:TextWriter) =
        //Write included stylesheet as CDATA. See https:#developer.mozilla.org/en-US/docs/Web/SVG/Element/style
        let cdata = sprintf "/* <![CDATA[ */\n{%s}\n/* ]]> */\n" css
        writer.Write(sprintf "<style>%s</style>" css)
/// Terminal item with an ability for a reference
type Terminal(text:string,?href:string,?title:string) as self=
    inherit DiagramItem( "g", dictFromPair("class","terminal"))
    do
        self.Width <- float text.Length * DefaultConfiguration.CharWidth + 20.
        self.Up <- 11.
        self.Down <- 11.
        self.NeedsSpace <- true
        self.AddDebug(self)

    override self.ToString()= sprintf "Terminal(%s, href=%A, title=%A)" text href title

    override self.Format (x,y,width)=
        let leftGap, rightGap = DetermineGaps(width, self.Width)

        //# Hook up the two sides if self is narrower than its stated width.
        Path(x, y).HorizontalLine(leftGap).AddTo(self) |> ignore
        Path(x + leftGap + self.Width, y).HorizontalLine(rightGap).AddTo(self) |> ignore

        DiagramItem("rect", diDict ["x", (x + leftGap).ToString()
                                    "y", (y - 11.).ToString()
                                    "width", self.Width.ToString()
                                    "height", (self.Up + self.Down).ToString()
                                    "rx","10"
                                    "ry","10"]).AddTo(self) |> ignore
        let textItem = DiagramItem("text", diDict ["x",(x + width / 2.).ToString()
                                                   "y",(y + 4.).ToString()], Some <| Choice2Of2 text)

        textItem.AddTo(match href with
                       | Some(href) -> DiagramItem("a", dictFromPair("xlink:href", href), Some <| Choice2Of2 text).AddTo(self) 
                       | None -> self :> DiagramItem) |> ignore
        match title with
        | Some(title) -> DiagramItem("title", Dictionary(), (Some <| Choice2Of2 title)).AddTo(self) |> ignore
        | None -> ()
        self :> DiagramItem


/// Complexity of en element
type ComplexityType = Simple | Complex
/// Starting element of a diagram
type Start(startType:ComplexityType,?label:string) as self=
    inherit DiagramItem( "g")
    do
        self.Width <- match label with
                      | Some(label) -> max 20. (float label.Length * DefaultConfiguration.CharWidth + 10.)
                      | None  -> 20.

        self.Up <- 10.
        self.Down <- 10.
        self.AddDebug(self)

    new () = Start(Simple)
    override self.Format (x,y,width)=
        let path = Path(x, y - 10.)
        (match startType with
         | Complex -> path.DownLine(20.).MoveTo(0., -10.).RightLine(self.Width)
         | Simple -> path.DownLine(20.).MoveTo(10., -20.).DownLine(20.).MoveTo(-10., -10.).RightLine(self.Width)
        ).AddTo(self) |> ignore
        match label with
        | Some(label) -> DiagramItem("text", diDict ["x",x.ToString()
                                                     "y",(y - 15.).ToString()
                                                     "style","text-anchor:start"], Some <| Choice2Of2 label).AddTo(self) 
                                                     |> ignore
        | None -> ()

        self :> DiagramItem

    override self.ToString() = sprintf "Start(type=%A, label=%A)" startType label
/// Ending diagram element
type End(endType:ComplexityType) as self=
    inherit DiagramItem("path")
    do    
        self.Width <- 20.
        self.Up <- 10.
        self.Down <- 10.
        self.AddDebug(self)
    new () = End(Simple)
    override self.Format (x,y,width)=
        match endType with 
        | Simple -> self.Attributes.["d"] <- sprintf "M %f %f h 20 m -10 -10 v 20 m 10 -20 v 20" x y
        | Complex -> self.Attributes.["d"] <- sprintf "M %f %f h 20 m 0 -10 v 20" x y
        self :> DiagramItem

    override self.ToString()= sprintf "End(type=%A)" endType

type Skip() as self=
    inherit DiagramItem("g")
    do
        self.AddDebug(self)

    override self.Format (x,y,width) =
        Path(x, y).RightLine(width).AddTo(self) |> ignore
        self :> DiagramItem

    override self.ToString() = "Skip()"

[<AutoOpen>]
module Wrapper =
    let Wrap item = match item with
                    | Choice1Of2(diagram) -> diagram
                    | Choice2Of2(str) -> Terminal(str) :> DiagramItem
type Diagram (items: DiString seq, diagramType:ComplexityType, css:string) as self =
    inherit DiagramItem("svg", dictFromPair ("class",DefaultConfiguration.DiagramClass))
    let formattedContainer = ref false
    let itemContainer = ResizeArray(items |> Seq.map Wrap)
    do 
        if not <| (self.Items.[0] :? Start) then
            self.Items.Insert(0, Start(self.DiagramType))
        if not <| (Seq.last self.Items :? End) then
            self.Items.Add(End(self.DiagramType))
        self.Items.Insert(0, Style(self.CSS))
        self.Items
        |> Seq.iter(fun item ->
            if item :? Style then ()
            else
                self.Width <- self.Width + item.Width + (if item.NeedsSpace then 20. else 0.)
                self.Up <- max self.Up (item.Up - self.Height)
                self.Height <- item.Height
                self.Down <- max (self.Down - item.Height) item.Down
        )
        if self.Items.[0].NeedsSpace then
            self.Width <- self.Width - 10.
        if (self.Items |> Seq.last).NeedsSpace then
            self.Width <- self.Width - 10.
    new (items:DiString seq) = Diagram(items,Simple,DefaultStyle)
    member self.Formatted = formattedContainer
    member self.DiagramType : ComplexityType =  diagramType
    member private self.Items :ResizeArray<DiagramItem> = itemContainer
    member self.CSS = css

    override self.ToString() =
        let items =  String.concat ", " <| (self.Items |> Seq.skip(2) |> Seq.map(fun x-> x.ToString()))
        let pieces = ResizeArray([items])
        if self.CSS <> DefaultStyle then
            pieces.Add(sprintf "css=%s" self.CSS)
        if self.DiagramType <> Simple then
            pieces.Add(sprintf "type=%A" self.DiagramType)
        sprintf "Diagram(%s)" <| String.concat ", " pieces

    override self.Format (x,y,width) = self.FormatBig <| Some(x) <| Some(y) <| Some(width) <| None
    member self.FormatBig (paddingTop:float option) (paddingRight:float option) (paddingBottom:float option) (paddingLeft:float option) =
        let paddingTop = defaultArg paddingTop 20.
        let paddingRight = defaultArg paddingRight paddingTop
        let paddingBottom = defaultArg paddingBottom paddingTop
        let paddingLeft = defaultArg paddingLeft paddingRight
        let mutable x = paddingLeft
        let mutable y = paddingTop + self.Up
        let g = DiagramItem("g")
        if DefaultConfiguration.IsStrokeOddPixelLength then
            g.Attributes.["transform"] <- "translate(.5 .5)"
        self.Items
        |> Seq.iter(fun item ->
            if item.NeedsSpace then
                Path(x, y).HorizontalLine(10.).AddTo(g) |> ignore
                x <- x + 10.
            item.Format(x,y,item.Width).AddTo(g) |> ignore
            x <- x + item.Width
            y <- y + item.Height
            if item.NeedsSpace then
                Path(x, y).HorizontalLine(10.).AddTo(g) |> ignore
                x <- x + 10.
        )
        self.Attributes.["width"] <- (self.Width + paddingLeft + paddingRight).ToString()
        self.Attributes.["height"] <- (self.Up + self.Height + self.Down + paddingTop + paddingBottom).ToString()
        self.Attributes.["viewBox"] <- sprintf "0 0 %s %s" self.Attributes.["width"] self.Attributes.["height"]
        g.AddTo(self) |> ignore
        self.Formatted := true
        self :> DiagramItem

    override self.WriteSvg(writer)=
        if not !self.Formatted then
            self.FormatBig None None None None |> ignore
        base.WriteSvg(writer)

    //let parseCSSGrammar(text) =
        //let token_patterns = {
        //    "keyword"= r"[\w-]+\(?",
        //    "type"= r"<[\w-]+(\(\))?>",
        //    "char"= r"[/,()]",
        //    "literal"= r""(.)"",
        //    "openbracket"= r"\[",
        //    "closebracket"= r"\]",
        //    "closebracketbang"= r"\]!",
        //    "bar"= r"\|",
        //    "doublebar"= r"\|\|",
        //    "doubleand"= r"&&",
        //    "multstar"= r"\*",
        //    "multplus"= r"\+",
        //    "multhash"= r"#",
        //    "multnum1"= r"{\s*(\d+)\s*}",
        //    "multnum2"= r"{\s*(\d+)\s*,\s*(\d*)\s*}",
        //    "multhashnum1"= r"#{\s*(\d+)\s*}",
        //    "multhashnum2"= r"{\s*(\d+)\s*,\s*(\d*)\s*}"
        //}


type Sequence(items:DiString seq) as self =
    inherit DiagramItem("g")
    let itemContainer = ResizeArray(items |> Seq.map Wrap)
    do  
        self.NeedsSpace <- true
        self.Items 
        |> Seq.iter(fun item -> 
            self.Width <- self.Width + item.Width + (if item.NeedsSpace then 20. else 0.)
            self.Up <- max self.Up (item.Up - self.Height)
            self.Height <- self.Height + item.Height
            self.Down <- max (self.Down - item.Height) item.Down
        )
        if self.Items.[0].NeedsSpace then
            self.Width <- self.Width - 10.
        if (self.Items |> Seq.last).NeedsSpace then
            self.Width <- self.Width - 10.
        self.AddDebug(self)

    member private self.Items :ResizeArray<DiagramItem> = itemContainer
    override self.ToString() =
        let items = String.concat ", " <| (self.Items |> Seq.map(fun x -> x.ToString()))
        sprintf "Sequence(%s)" items

    override self.Format (x,y,width)=
        let leftGap, rightGap = DetermineGaps(width,self.Width)
        Path(x, y).HorizontalLine(leftGap).AddTo(self) |> ignore
        Path(x+ leftGap + self.Width, y+ self.Height).HorizontalLine(rightGap).AddTo(self) |> ignore
        let mutable x = x + leftGap
        let mutable y = y
        let last = self.Items.Count - 1
        self.Items
        |> Seq.iteri(fun i item -> 
            if item.NeedsSpace && i > 0 then
                Path(x, y).HorizontalLine(10.).AddTo(self) |> ignore
                x <- x + 10.
            item.Format(x,y,item.Width).AddTo(self) |> ignore
            x <- x + item.Width
            y <- y + item.Height
            if item.NeedsSpace && i < last then
                Path(x, y).HorizontalLine(10.).AddTo(self) |> ignore
                x <- x + 10.
        )
        self :> DiagramItem


type Stack(items: DiString seq) as self=
    inherit DiagramItem("g")
    let itemContainer = ResizeArray(items |> Seq.map Wrap)
    let AR = DefaultConfiguration.ArcRadius
    let VS = DefaultConfiguration.VerticalSeperation
    do
        self.NeedsSpace <- true
        self.Width <- self.Items
                      |> Seq.map (fun item  -> if item.NeedsSpace then 20. else 0. + item.Width)
                      |> Seq.max
        if self.Items.Count > 1 then
            self.Width <- self.Width + AR * 2.
        self.Up <- self.Items.[0].Up
        self.Down <- (self.Items |> Seq.last).Down
        let last = self.Items.Count - 1
        self.Items
        |> Seq.iteri(fun i item->
        self.Height <- self.Height + item.Height
        if i > 0 then
            self.Height <- self.Height + max (AR * 2.) (item.Up + VS)
        if i < last then
            self.Height <- self.Height + max (AR * 2.) (item.Down + VS)
        )
        self.AddDebug(self)
    member private self.Items :ResizeArray<DiagramItem> = itemContainer

    override self.ToString() = 
        let items = self.Items 
                    |> Seq.map(fun x -> x.ToString())
                    |> String.concat ", "
        sprintf "Stack(%s)" items

    override self.Format(x,y,width) =
        let leftGap, rightGap = DetermineGaps(width, self.Width)
        Path(x, y).HorizontalLine(leftGap).AddTo(self) |> ignore
        let mutable x = x + leftGap
        let mutable y = y
        let xInitial = x
        let innerWidth = 
            if  self.Items.Count > 1 then
                Path(x, y).HorizontalLine(AR).AddTo(self) |> ignore
                x <- x + AR
                self.Width - AR * 2.
            else self.Width
        self.Items
        |> Seq.iteri(fun i item -> 
            item.Format(x,y,innerWidth).AddTo(self) |> ignore
            x <- x + innerWidth
            y <- y + item.Height
            if i <> self.Items.Count - 1 then
                Path(x,y).Arc("ne").DownLine(max 0. (item.Down + VS - AR*2.))
                          .Arc("es").LeftLine(innerWidth)
                          .Arc("nw").DownLine(max 0. (self.Items.[i+1].Up + VS - AR*2.))
                          .Arc("ws").AddTo(self) |> ignore
                y <- y + (max (item.Down + VS) AR*2.) + (max (self.Items.[i+1].Up + VS) AR*2.)
                x <- xInitial + AR
            if self.Items.Count > 1 then
               Path(x, y).HorizontalLine(AR).AddTo(self) |> ignore
               x <- x + AR
        )
        Path(x, y).HorizontalLine(rightGap).AddTo(self) |> ignore
        self :> DiagramItem


type OptionalSequenceImpl(items:DiString seq) as self=
    inherit DiagramItem("g")
    let itemContainer = ResizeArray(items |> Seq.map Wrap)
    let VS = DefaultConfiguration.VerticalSeperation
    let AR = DefaultConfiguration.ArcRadius
    do 
        self.Height <- self.Items |> Seq.sumBy(fun (item : DiagramItem) -> item.Height) 
        self.Down <- self.Items.[0].Down
        let mutable heightSoFar = 0.
        self.Items
        |> Seq.iteri( fun i item -> 
            self.Up <- max self.Up ((max (AR * 2.) (item.Up + VS)) - heightSoFar)
            heightSoFar <- heightSoFar + item.Height
            if i > 0 then
                self.Down <- (max (self.Height + self.Down) (heightSoFar + (max (AR * 2.) (item.Down + VS)))) - self.Height
            let itemWidth = item.Width + (if item.NeedsSpace then 20. else 0.)
            if i = 0 then
                self.Width <- self.Width +  (AR + max itemWidth AR)
            else
                self.Width <- self.Width +  (AR * 3. + max itemWidth AR)
                )
        self.AddDebug(self)

    member private self.Items :ResizeArray<DiagramItem> = itemContainer

    override self.ToString() =
        let items = self.Items |> Seq.map(fun x -> x.ToString()) |> String.concat ", "
        sprintf "OptionalSequence(%s)" items

    override self.Format(x,y,width) =
        let leftGap, rightGap = DetermineGaps(width, self.Width)
        Path(x, y).RightLine(leftGap).AddTo(self) |> ignore
        Path(x + leftGap + self.Width, y + self.Height).RightLine(rightGap).AddTo(self) |> ignore
        let mutable x = x + leftGap
        let mutable y = y
        let upperLineY = y - self.Up
        let last = self.Items.Count - 1
        self.Items
        |> Seq.iteri(fun i item -> 
            let itemSpace = if item.NeedsSpace then 10. else 0.
            let itemWidth = item.Width + itemSpace
            if i = 0 then
                (Path(x,y)
                    .Arc("se")
                    .UpLine(y - upperLineY - AR*2.)
                    .Arc("wn")
                    .RightLine(itemWidth - AR)
                    .Arc("ne")
                    .DownLine(y + item.Height - upperLineY - AR*2.)
                    .Arc("ws")
                    .AddTo(self)) |> ignore
                (Path(x, y)
                    .RightLine(itemSpace + AR)
                    .AddTo(self)) |> ignore
                item.Format(x + itemSpace + AR,y,item.Width)
                    .AddTo(self) |> ignore
                x <- x + itemWidth + AR
                y <- y + item.Height
            elif i < last then
                (Path(x, upperLineY)
                    .RightLine(AR*3. + (max itemWidth AR))
                    .Arc("ne")
                    .DownLine(y - upperLineY + item.Height - AR*2.)
                    .Arc("ws")
                    .AddTo(self)) |> ignore
                (Path(x,y)
                    .RightLine(AR * 2.)
                    .AddTo(self)) |> ignore
                item.Format(x + AR * 2.,y,item.Width)
                    .AddTo(self) |> ignore
                (Path(x + item.Width + AR*2., y + item.Height)
                    .RightLine(itemSpace + AR)
                    .AddTo(self)) |> ignore
                (Path(x,y)
                    .Arc("ne")
                    .DownLine(item.Height + (max (item.Down + VS) (AR * 2.)) - AR*2.)
                    .Arc("ws")
                    .RightLine(itemWidth - AR)
                    .Arc("se")
                    .UpLine(item.Down + VS - AR*2.)
                    .Arc("wn")
                    .AddTo(self)) |> ignore
                x <- x + AR*2. + (max itemWidth AR) + AR
                y <- y + item.Height
            else
                (Path(x, y)
                    .RightLine(AR*2.)
                    .AddTo(self)) |> ignore
                item.Format(x + AR * 2.,y,item.Width)
                    .AddTo(self) |> ignore
                (Path(x + AR*2. + item.Width, y + item.Height)
                    .RightLine(itemSpace + AR)
                    .AddTo(self)) |> ignore
                (Path(x,y)
                    .Arc("ne")
                    .DownLine(item.Height + (max (item.Down + VS) (AR*2.)) - AR*2.)
                    .Arc("ws")
                    .RightLine(itemWidth - AR)
                    .Arc("se")
                    .UpLine(item.Down + VS - AR*2.)
                    .Arc("wn")
                    .AddTo(self))|> ignore) 
        self :> DiagramItem


type AlternatingSequence(first:DiString,second:DiString) as self=
    inherit DiagramItem("g")
    let first = Wrap first
    let second = Wrap second
    let AR = DefaultConfiguration.ArcRadius
    let VS = DefaultConfiguration.VerticalSeperation
    do 
       let arc = AR
       let vert = VS
       let arcX = arc * sqrt(2.)
       let arcY = (2. - sqrt(2.)) * arc 
       let crossY = max arc vert
       let crossX = (crossY - arcY) + arcX
       let firstOut = [2. * arc;crossY / 2. + arc * 2.;crossY / 2. + vert + first.Down] |> Seq.max
       self.Up <- firstOut + first.Height + first.Up
       let secondIn = [2. * arc; crossY / 2. + 2. * arc;crossY / 2. + vert + second.Up] |> Seq.max
       self.Down <- secondIn + second.Height + second.Down
       let firstWidth = (if first.NeedsSpace then 20. else 0.) + first.Width
       let secondWidth = (if second.NeedsSpace then 20. else 0.) + second.Width
       self.Width <-  4. * arc  + Seq.max [firstWidth;crossX;secondWidth]
       self.AddDebug(self)

    override self.ToString() = sprintf "AlternatingSequence(%s %s)" <| first.ToString() <| second.ToString()

    override self.Format(x,y,width) =
        let arc = AR
        let gaps = DetermineGaps(width, self.Width)
        Path(x,y).RightLine(fst gaps).AddTo(self) |> ignore
        let mutable x = x + fst gaps
        Path(x + self.Width, y).RightLine(snd gaps).AddTo(self) |> ignore
        //# bounding box
        //# Path(x+gaps[0], y).Up(self.Up).right(self.Width).down(self.Up+self.Down).left(self.Width).Up(self.Down).AddTo(self)

        //# top
        let firstIn = self.Up - first.Up
        let firstOut = self.Up - first.Up - first.Height
        Path(x,y).Arc("se").UpLine(firstIn - 2.*arc).Arc("wn").AddTo(self) |> ignore
        first.Format(x + 2. * arc,y - firstIn,self.Width - 4.*arc)
            .AddTo(self) |> ignore
        Path(x + self.Width - 2.*arc, y - firstOut).Arc("ne").DownLine(firstOut - 2. * arc).Arc("ws").AddTo(self) |> ignore

        //# bottom
        let secondIn = self.Down - second.Down - second.Height
        let secondOut = self.Down - second.Down
        Path(x,y).Arc("ne").DownLine(secondIn - 2. *arc).Arc("ws").AddTo(self) |> ignore
        second.Format(x + 2. * arc,y + secondIn,self.Width - 4. * arc).AddTo(self) |> ignore
        Path(x + self.Width - 2. *arc, y + secondOut).Arc("se").UpLine(secondOut - 2. *arc).Arc("wn").AddTo(self) |> ignore

        //# crossover
        let arcX = sqrt(2.) * arc
        let arcY = (2. - sqrt(2.) ) * arc 
        let crossY = max arc VS
        let crossX = (crossY - arcY) + arcX
        let crossBar = (self.Width - 4. * arc - crossX) / 2.
        (Path(x+arc, y - crossY/ 2. - arc).Arc("ws").RightLine(crossBar)
            .Arc("n", "cw").LineTo(crossX - arcX, crossY - arcY).Arc("sw", "ccw")
            .RightLine(crossBar).Arc("ne").AddTo(self)) |> ignore
        (Path(x+arc, y + crossY/2. + arc).Arc("wn").RightLine(crossBar)
            .Arc("s", "ccw").LineTo(crossX - arcX, arcY - crossY ).Arc("nw", "cw")
            .RightLine(crossBar).Arc("se").AddTo(self)) |> ignore
        self :> DiagramItem


type Choice(defaultChoice:int,items:DiString seq) as self=
    inherit DiagramItem("g")
    let itemContainer = ResizeArray(items |> Seq.map Wrap)
    let AR = DefaultConfiguration.ArcRadius
    let VS = DefaultConfiguration.VerticalSeperation
    do    
        self.Width <- AR * 4. + (self.Items |> Seq.map (fun item -> item.Width) |> Seq.max)
        self.Up <- self.Items.[0].Up
        self.Down <- (self.Items |> Seq.last).Down
        self.Height <- self.Items.[defaultChoice].Height
        self.Items |> Seq.iteri(fun i item ->
            let arcs = (if ([defaultChoice-1;defaultChoice+1] |> Seq.contains i) then
                            AR * 2.
                       else 
                            AR)

            if i < defaultChoice then
                self.Up <- self.Up + max arcs (item.Height + item.Down + VS + self.Items.[i+1].Up)
            elif i = defaultChoice then ()
            else
                self.Down <- self.Down + max arcs (item.Up + VS + self.Items.[i-1].Down + self.Items.[i-1].Height)
            
        )
        self.Down <- self.Down - self.Items.[defaultChoice].Height //# already counted in self.Height
        self.AddDebug(self)
    member private self.Items : ResizeArray<DiagramItem> = itemContainer

    override self.ToString() = 
        let items = self.Items |> Seq.map( fun x -> x.ToString()) |> String.concat ", "
        sprintf "Choice(%i, %s)" defaultChoice items
    override self.Format(x,y,width) =
        let leftGap, rightGap = DetermineGaps(width, self.Width)

        //# Hook up the two sides if self is narrower than its stated width.
        Path(x, y).HorizontalLine(leftGap).AddTo(self) |> ignore
        Path(x + leftGap + self.Width, y + self.Height).HorizontalLine(rightGap).AddTo(self) |> ignore
        let mutable x = x + leftGap

        let innerWidth = self.Width - AR * 4.
        let defaultItem = self.Items.[defaultChoice]

        //# Do the elements that curve above
        let above = self.Items |> Seq.take defaultChoice |> Seq.rev
        if above |> Seq.isEmpty |> not then
            let firstAbove = above |> Seq.item 0
            let mutable distanceFromY = max (AR * 2.) (defaultItem.Up + VS + firstAbove.Down + firstAbove.Height)
            let length = Seq.length above
            above |> Seq.iteri(fun i item -> 
                let ni = i - length
                Path(x, y).Arc("se").UpLine(distanceFromY - AR * 2.).Arc("wn").AddTo(self) |> ignore
                item.Format(x + AR * 2.,y - distanceFromY,innerWidth).AddTo(self) |> ignore
                Path(x + AR * 2. + innerWidth, y - distanceFromY + item.Height).Arc("ne") 
                    .DownLine(distanceFromY - item.Height + defaultItem.Height - AR*2.).Arc("ws").AddTo(self) |> ignore
                if ni < -1 then
                    let nextAbove = above |> Seq.item (i+1)
                    distanceFromY <- distanceFromY + max AR (item.Up + VS + nextAbove.Down + nextAbove.Height)
            )
        //# Do the straight-line path.
        Path(x, y).RightLine(AR * 2.).AddTo(self) |> ignore
        self.Items.[defaultChoice].Format(x + AR * 2.,y,innerWidth).AddTo(self) |> ignore
        Path(x + AR * 2. + innerWidth, y+ self.Height).RightLine(AR * 2.).AddTo(self) |> ignore

        //# Do the elements that curve below
        let below = self.Items |> Seq.skip (defaultChoice + 1)
        if below |> Seq.isEmpty |> not then
            let mutable distanceFromY = max (AR * 2.) (defaultItem.Height + defaultItem.Down + VS + (below |> Seq.item 0).Up)
            below |> Seq.iteri(fun i item ->
            Path(x, y).Arc("ne").DownLine(distanceFromY - AR * 2.).Arc("ws").AddTo(self) |> ignore
            item.Format(x + AR * 2.,y + distanceFromY,innerWidth).AddTo(self) |> ignore
            Path(x + AR * 2. + innerWidth, y + distanceFromY + item.Height).Arc("se")
                .UpLine(distanceFromY - AR * 2. + item.Height - defaultItem.Height).Arc("wn").AddTo(self) |> ignore
            distanceFromY <- distanceFromY + max AR 
                    (item.Height + item.Down + VS + ( if (i+1 < Seq.length below) then (below |> Seq.item (i + 1)).Up else 0.))
            )
        self :> DiagramItem

type ChoiceType = 
    | Any = 0 
    | All = 1

type MultipleChoice(defaultChoice:int,choiceType:ChoiceType,items: DiString seq) as self=
    inherit DiagramItem( "g")
    let itemContainer = ResizeArray(items |> Seq.map Wrap)
    let innerWidthValue = itemContainer |> Seq.map (fun item -> item.Width) |> Seq.max
    let defaultChoice = max defaultChoice 0
    let AR = DefaultConfiguration.ArcRadius
    let VS = DefaultConfiguration.VerticalSeperation
    do
        self.NeedsSpace <- true
        self.Width <- 30. + AR + self.InnerWidth + AR + 20.
        self.Up <- self.Items.[0].Up
        self.Down <- (self.Items |> Seq.last).Down
        self.Height <- self.Items.[defaultChoice].Height
        self.Items
        |> Seq.iteri(fun i item -> 
        let minimum = 
            if [defaultChoice - 1; defaultChoice + 1] |> (Seq.contains i) then
                10. + AR
            else
                AR
        if i < defaultChoice then
            self.Up <- self.Up + max minimum (item.Height + item.Down + VS + self.Items.[i+1].Up)
        elif i = defaultChoice then
            ()
        else
            self.Down <- self.Down + max minimum (item.Up + VS + self.Items.[i-1].Down + self.Items.[i-1].Height)
        )
        self.Down <- self.Down - self.Items.[defaultChoice].Height //# already counted in self.Height
        self.AddDebug(self)
    member private self.InnerWidth = innerWidthValue
    member private self.Items : ResizeArray<DiagramItem> = itemContainer
    override self.ToString()=
        let items = self.Items |> Seq.map(fun x -> x.ToString()) |> String.concat ", "
        sprintf "MultipleChoice(%i, %A, %s)" defaultChoice choiceType items

    override self.Format(x,y,width) =
        let leftGap, rightGap = DetermineGaps(width, self.Width)

        //# Hook up the two sides if self is narrower than its stated width.
        Path(x, y).HorizontalLine(leftGap).AddTo(self) |> ignore
        Path(x + leftGap + self.Width, y + self.Height).HorizontalLine(rightGap).AddTo(self) |> ignore
        let mutable x = x + leftGap

        let defaultItem = self.Items.[defaultChoice]

        //# Do the elements that curve above
        let above = self.Items |> Seq.skip defaultChoice |> Seq.rev
        if above |> Seq.isEmpty |> not then
            let length = above |> Seq.length
            let aboveFirst = above |> Seq.item 0
            let mutable distanceFromY = max (10. + AR) (defaultItem.Up + VS + aboveFirst.Down + aboveFirst.Height)
            above |> Seq.iteri(fun i item -> 
            let ni = length - i
            (Path(x + 30., y)
                .UpLine(distanceFromY - AR)
                .Arc("wn")
                .AddTo(self)) |> ignore
            item.Format(x + 30. + AR,y - distanceFromY,self.InnerWidth).AddTo(self) |> ignore
            (Path(x + 30. + AR + self.InnerWidth, y - distanceFromY + item.Height)
                .Arc("ne")
                .DownLine(distanceFromY - item.Height + defaultItem.Height - AR - 10.)
                .AddTo(self)) |> ignore
            if ni < -1 then
                distanceFromY <- distanceFromY + max AR (item.Up + VS + (above |> Seq.item (i+1)).Down + (above |> Seq.item (i+1)).Height)
             )
        //# Do the straight-line path.
        Path(x + 30., y).RightLine(AR).AddTo(self) |> ignore
        self.Items.[defaultChoice].Format(x + 30. + AR,y,self.InnerWidth).AddTo(self) |> ignore
        Path(x + 30. + AR + self.InnerWidth, y + self.Height).RightLine(AR).AddTo(self) |> ignore

        //# Do the elements that curve below
        let below = self.Items |> Seq.skip defaultChoice  /// TASK DEBUG
        if below |> Seq.isEmpty |> not then
            let length = below |> Seq.length
            let mutable distanceFromY = max (10. + AR) (defaultItem.Height + defaultItem.Down + VS + (below|> Seq.item 0).Up)
            below |> Seq.iteri(fun i item -> 
            (Path(x+30., y)
                .DownLine(distanceFromY - AR)
                .Arc("ws")
                .AddTo(self)) |> ignore
            item.Format(x + 30. + AR,y + distanceFromY,self.InnerWidth).AddTo(self) |> ignore
            (Path(x + 30. + AR + self.InnerWidth, y + distanceFromY + item.Height)
                .Arc("se")
                .UpLine(distanceFromY - AR + item.Height - defaultItem.Height - 10.)
                .AddTo(self)) |> ignore
            distanceFromY <- distanceFromY + max AR (item.Height + item.Down + VS + (if i+1 < length 
                                                                                            then (below |> Seq.item(i + 1)).Up 
                                                                                            else 0.))
        )
        let text = DiagramItem("g", dictFromPair("class","diagram-text")).AddTo(self) 
        DiagramItem("title"
                    , Dictionary()
                    , Some <| Choice2Of2(match choiceType with
                                         | ChoiceType.Any -> "take one or more branches, once each, in any order" 
                                         | ChoiceType.All -> "take all branches, once each, in any order"
                                         | _ -> invalidArg "choiceType" "How did you put an invalid choice?"))
            .AddTo(text) |> ignore
        DiagramItem("path", diDict ["d",sprintf "M %f %f h -26 a 4 4 0 0 0 -4 4 v 12 a 4 4 0 0 0 4 4 h 26 z" (x+30.) (y-10.)
                                    "class","diagram-text"
                                   ]).AddTo(text) |> ignore
        DiagramItem("text", diDict ["x",(x + 15.).ToString()
                                    "y",(y + 4.).ToString()
                                    "class","diagram-text"]
                          , Some <| Choice2Of2( match choiceType with
                                                      | ChoiceType.Any -> "1+"
                                                      | ChoiceType.All -> "all"
                                                      | _ -> invalidArg "choiceType" "Invalid Choice. How did you do it?"))
                                          .AddTo(text) |> ignore
        DiagramItem("path", diDict [
            "d", sprintf "M %f %f h 16 a 4 4 0 0 1 4 4 v 12 a 4 4 0 0 1 -4 4 h -16 z" (x + self.Width - 20.) (y - 10.)
            "class","diagram-text"
            ]).AddTo(text) |> ignore
        DiagramItem("text",  Dictionary(dict [
            "x", (x + self.Width - 10.).ToString()
            "y", (y + 4.).ToString()
            "class","diagram-arrow"
            ]),Some <| Choice2Of2 "↺" ).AddTo(text) |> ignore
        self :> DiagramItem

type HorizontalChoiceImpl(items: DiString seq) as self =
    inherit DiagramItem( "g")
    let AR = DefaultConfiguration.ArcRadius
    let VS = DefaultConfiguration.VerticalSeperation
    let allButLast = self.Items |> Seq.take (self.Items.Count - 1) 
    let middles = self.Items |> Seq.skip 1
    let first = self.Items.[0]
    let last = self.Items |> Seq.last
    let upperTrack = [AR*2.;VS;(allButLast |> Seq.map(fun x -> x.Up) |> Seq.max) + VS] |> Seq.max

    let mutable LowerTrackContainer = 0.
    do
        self.Width <- (AR * 4. * float (self.Items.Count - 1) //# inbetween tracks
                      + (Seq.sumBy( fun (x:DiagramItem) -> x.Width + if x.NeedsSpace then 20. else 0. ) self.Items) //#items
                      + (if last.Height > 0. then AR else 0.)) //# needs space to curve up 
                      //#ending track
        ////# Always exits at entrance height
        ////# All but the last have a track running above them
        self.Up <- max upperTrack last.Up
        let t = if middles |> Seq.isEmpty then 0. else middles |> Seq.map (fun x -> x.Height + max (x.Down + VS) (AR*2.) ) |> Seq.max 
        self.LowerTrack <- [VS;t;last.Height + last.Down + VS] |> Seq.max

        ////# All but the first have a track running below them
        ////# Last either straight-lines or curves up, so has different calculation
        if first.Height < self.LowerTrack then
            //# Make sure there"s at least 2*AR room between first exit and lower track
            self.LowerTrack <- max self.LowerTrack (first.Height + AR*2.)
        self.Down <- max self.LowerTrack (first.Height + first.Down)
        self.AddDebug(self)
    /// All items
    member private self.Items: ResizeArray<DiagramItem> = ResizeArray(items |> Seq.map Wrap)
    /// Depth of the lower track
    member val LowerTrack = 0. with get,set     

    override self.Format(x,y,width) =
        //# Hook up the two sides if self is narrower than its stated width.
        let leftGap, rightGap = DetermineGaps(width, self.Width)
        Path(x, y).HorizontalLine(leftGap).AddTo(self) |> ignore
        Path(x + leftGap + self.Width, y + self.Height).HorizontalLine(rightGap).AddTo(self) |> ignore
        let mutable x = x + leftGap

        //# upper track 
        let upperSpan = allButLast 
                        |> Seq.sumBy(fun x -> x.Width +  if x.NeedsSpace then 20. else 0.) 
                        |> (+) (float (self.Items.Count - 2) * AR) 
        (Path(x,y)
            .Arc("se")
            .UpLine(upperTrack - AR*2.)
            .Arc("wn")
            .HorizontalLine(upperSpan)
            .AddTo(self)) |> ignore

        //# lower track
        let lowerSpan = (self.Items
                        |> Seq.skip 1
                        |> Seq.sumBy (fun x -> x.Width + (if x.NeedsSpace then 20. else 0.) ))
                        + float (self.Items.Count - 2) * AR 
                        + (if last.Height > 0. then AR else 0.)
        let lowerStart = x + AR * 3. + first.Width + (if first.NeedsSpace then 20. else 0.)
        (Path(lowerStart, y+ self.LowerTrack)
            .HorizontalLine(lowerSpan)
            .Arc("se")
            .UpLine(self.LowerTrack - AR*2.)
            .Arc("wn")
            .AddTo(self)) |> ignore
        self.Items
        |> Seq.iteri(fun i item ->
        //# Items
            //# input track
            if i = 0 then
                (Path(x,y).HorizontalLine(AR).AddTo(self)) |> ignore
                x <- x + AR
            else
                (Path(x, y - upperTrack)
                    .Arc("ne")
                    .VerticalLine(upperTrack - AR * 2.)
                    .Arc("ws")
                    .AddTo(self)) |> ignore
                x <- x + AR * 2.

            //# item
            let itemWidth = item.Width + (if item.NeedsSpace then 20. else 0.)
            item.Format(x,y,itemWidth).AddTo(self) |> ignore
            x <- x + itemWidth

            //# output track
            if i = self.Items.Count-1 then
                if item.Height = 0. then
                    (Path(x,y).HorizontalLine(AR).AddTo(self)) |> ignore
                else
                    (Path(x,y + item.Height).Arc("se").AddTo(self)) |> ignore
            elif i = 0 && item.Height > self.LowerTrack then
                //# Needs to arc up to meet the lower track, not down.
                if item.Height - self.LowerTrack >= AR*2. then
                    (Path(x, y+ item.Height)
                        .Arc("se")
                        .VerticalLine(self.LowerTrack - item.Height + AR*2.)
                        .Arc("wn")
                        .AddTo(self)) |> ignore
                else
                   // # Not enough space to fit two arcs
                   // # so just bail and draw a straight line for now.
                    (Path(x, y + item.Height)
                        .LineTo(AR*2., self.LowerTrack - item.Height)
                        .AddTo(self)) |> ignore
            else
                (Path(x, y + item.Height)
                    .Arc("ne")
                    .VerticalLine(self.LowerTrack - item.Height - AR*2.)
                    .Arc("ws")
                    .AddTo(self)) |> ignore
                    )
        self :> DiagramItem

type OneOrMore(item:DiString, repeat: DiString) as self=
    inherit DiagramItem( "g")
    let repeat = repeat |> Wrap
    let item = Wrap(item)
    let AR = DefaultConfiguration.ArcRadius
    let VS = DefaultConfiguration.VerticalSeperation
    do 
        self.Width <- (max item.Width repeat.Width) + AR * 2.
        self.Height <- item.Height
        self.Up <- item.Up
        self.Down <- max (AR * 2.) (item.Down + VS + repeat.Up + repeat.Height + repeat.Down)
        self.NeedsSpace <- true
        self.AddDebug(self)
    new (item:DiString) = OneOrMore(item,Skip() :> DiagramItem |> Choice1Of2)
    override self.Format(x,y,width)=
        let leftGap, rightGap = DetermineGaps(width, self.Width)

        //# Hook up the two sides if self is narrower than its stated width.
        Path(x, y).HorizontalLine(leftGap).AddTo(self) |> ignore
        Path(x + leftGap + self.Width, y + self.Height).HorizontalLine(rightGap).AddTo(self) |> ignore
        let mutable x = x + leftGap

        ////# Draw item
        Path(x, y).RightLine(AR).AddTo(self) |> ignore
        item.Format(x + AR,y,self.Width - AR * 2.).AddTo(self) |> ignore
        Path(x + self.Width - AR, y + self.Height).RightLine(AR).AddTo(self) |> ignore

        //# Draw repeat arc
        let distanceFromY = max (AR*2.) (item.Height + item.Down + VS   + repeat.Up)
        Path(x + AR, y).Arc("nw").DownLine(distanceFromY - AR * 2.).Arc("ws").AddTo(self) |> ignore
        repeat.Format(x + AR,y + distanceFromY,self.Width - AR * 2.).AddTo(self) |> ignore
        Path(x + self.Width - AR, y + distanceFromY + repeat.Height).Arc("se") 
            .UpLine(distanceFromY - AR * 2. + repeat.Height - item.Height).Arc("en").AddTo(self) |> ignore

        self :> DiagramItem

    override self.ToString()=
        sprintf "OneOrMore(%s, repeat=%s)" <| item.ToString() <| repeat.ToString()


module AuxilaryStructures =
    let OptionalSequence(items:DiString seq) =
        if items |> Seq.length <= 1
            then Sequence(items) :> DiagramItem
        else OptionalSequenceImpl(items) :> DiagramItem
    let HorizontalChoice(items:DiString seq) = 
        if (items |> Seq.length) <= 1
            then Sequence(items) :> DiagramItem
        else HorizontalChoiceImpl(items) :> DiagramItem

    let Optional(item, skip) = Choice((if skip then 0 else 1) , [Skip() :> DiagramItem |> Choice1Of2;Choice1Of2 item])
    
    let ZeroOrMore(item, repeat: DiString option, skip) = match repeat with
                                                          | Some(repeat) -> Optional(OneOrMore(item, repeat), skip)
                                                          | None -> Optional(OneOrMore(item),skip)

type NonTerminal(text:string,?href:string,?title:string) as self=
    inherit DiagramItem( "g", dictFromPair("class","non-terminal"))
    do
        self.Width <- float text.Length * DefaultConfiguration.CharWidth + 20.
        self.Up <- 11.
        self.Down <- 11.
        self.NeedsSpace <- true
        self.AddDebug(self)

    override self.ToString()= sprintf "NonTerminal(%s, href=%A, title=%A)" text href title

    override self.Format(x,y,width)=
        let leftGap, rightGap = DetermineGaps(width, self.Width)

        //# Hook up the two sides if self is narrower than its stated width.
        Path(x, y).HorizontalLine(leftGap).AddTo(self) |> ignore
        Path(x + leftGap + self.Width, y).HorizontalLine(rightGap).AddTo(self) |> ignore

        DiagramItem("rect",diDict ["x",(x + leftGap).ToString()
                                   "y",(y - 11.).ToString()
                                   "width", self.Width.ToString()
                                   "height",(self.Up + self.Down).ToString()])
                                   .AddTo(self) |> ignore
        let textItem = DiagramItem("text", diDict ["x",(x + width / 2.).ToString()
                                                   "y",(y + 4.).ToString()], Some <| Choice2Of2 text)
        textItem.AddTo(match href with
                       | Some(href) -> DiagramItem("a", dictFromPair("xlink:href",href), Some <| Choice2Of2 text).AddTo(self)
                       | None -> self :> DiagramItem) |> ignore
        match title with
        | Some(title) -> DiagramItem("title", Dictionary(), Some <| Choice2Of2 title).AddTo(self) |> ignore
        | None -> ()
        self :> DiagramItem


type Comment(text:string,?href:string ,?title:string) as self=
    inherit DiagramItem("g")
    do
        self.Width <- float text.Length * DefaultConfiguration.CommentCharWidth + 10.
        self.Up <- 11.
        self.Down <- 11.
        self.NeedsSpace <- true
        self.AddDebug(self)

    override self.ToString()=
        sprintf "Comment(%s, href=%A, title=%A)" text (href) (title)

    override self.Format(x,y,width)=
        let leftGap, rightGap = DetermineGaps(width, self.Width)

        //# Hook up the two sides if self is narrower than its stated width.
        Path(x, y).HorizontalLine(leftGap).AddTo(self) |> ignore
        Path(x + leftGap + self.Width, y).HorizontalLine(rightGap).AddTo(self) |> ignore

        let textDi = DiagramItem("text", diDict [ "x",(x + width / 2.).ToString()
                                                  "y",(y + 5.).ToString()
                                                  "class", "comment"],Some <| Choice2Of2 text)
        textDi.AddTo(match href with
                     | Some (href) -> DiagramItem("a", dictFromPair("xlink:href",href), Some <| Choice2Of2 text).AddTo(self) 
                     | None -> self :> DiagramItem) |> ignore
        match title with
        | Some(title) -> DiagramItem("title", Dictionary(),Some <| Choice2Of2 title).AddTo(self) |> ignore
        | None -> ()
        self :> DiagramItem


