namespace RailroadDiagrams
    type TextAlignment = 
    | Left = 1
    | Center = 2 
    | Right = 3
    [<Sealed>]
    type DiagramConfiguration (debug:bool, verticalSeperation:float
                               ,arcRadius:float,diagramClass:string
                               ,isStrokeOddPixelLength:bool ,internalAlignment:TextAlignment
                               ,charWidth:float ,commentCharWidth: float) =
            new () = DiagramConfiguration(debug= false,verticalSeperation= 8.,
                                          arcRadius= 10.,diagramClass= "railroad-diagram",
                                          isStrokeOddPixelLength= true, internalAlignment= TextAlignment.Center,
                                          charWidth= 8.5,commentCharWidth= 7.)
            /// Should Output Debug Information
            member this.Debug = debug
            /// Minimum vertical separation between things. For a 3px stroke, must be at least 4
            member this.VerticalSeperation = verticalSeperation
            /// Radius of arcs
            member this.ArcRadius = arcRadius
            /// class to put on the root <svg>
            member this.DiagramClass = diagramClass
            /// Is the stroke width an odd pixel length?
            member this.IsStrokeOddPixelLength = isStrokeOddPixelLength
            /// Aligning items when they have extra space
            member this.InternalAlignment = internalAlignment
            /// Width of each monospace character. play until you find the right value for your font
            member this.CharWidth = charWidth
            /// Comments char width. Configurable, because they are smaller by default
            member this.CommentCharWidth = commentCharWidth  
