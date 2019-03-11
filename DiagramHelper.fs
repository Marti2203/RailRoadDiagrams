module RailroadDiagrams.Auxilary
open RailroadDiagrams.AuxilaryStructures
let text v = Choice2Of2 v
let items vs = Diagram vs
let itemsTwo (a,b) = items [a;b]
let itemsThree (a,b,c) = items[a;b;c] 
let inline item (v: DiagramItem)= Choice1Of2 v
let nonterm v = NonTerminal(v)
let nontermItem v = (nonterm >> item) v
let term v = Terminal(v)
let zeroMore v = ZeroOrMore(v,None,false)
let (?) v = ZeroOrMore(v,None,false)
let zeroMoreItem v = (zeroMore >> item) v
let oneMore v = OneOrMore(v)
let (+) v = OneOrMore(v)
let oneMoreRepeat v r = OneOrMore(v,r)
let oneMoreRepeatItem v r = oneMoreRepeat v r |> item 
let oneMoreItem v = (oneMore >> item) v
let choice i vs = Choice(i,vs)
let choiceItem i vs = ((choice i) >> item) vs
let choiceItemZ vs = choiceItem 0 vs
let sequence vs = Sequence(vs)
let sequenceItem vs = (sequence >> item) vs 
let comment v = Comment(v)
let commentItem v = (comment >> item) v 
let optional v skip = Optional(v,false)
let optionalItem v skip = (optional v skip) |> item