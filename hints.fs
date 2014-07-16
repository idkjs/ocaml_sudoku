module hints

open sudoku
open puzzlemap

exception CellStateInvalid


let first (candidates:Set<Candidate>) = Set.toList candidates |> List.head

let convertToH (h:AnnotatedSymbol<HintAnnotatedCandidate> -> HintAnnotatedSymbol) (hac:'a->HintAnnotatedCandidate) (entry:AnnotatedSymbol<'a>):HintAnnotatedSymbol =
    match entry with
    | Given s -> h (Given s)
    | Set s -> h (Set s)
    | Candidates candidateMap -> h (Candidates (candidateMap >> hac))

let setHintId =
    fun (etoc:Cell->AnnotatedSymbol<AnnotatedCandidate>) ->
        fun (cell:Cell) ->
            let hac = convertToH HASHId HACId
            (etoc >> hac) cell

let setHint (hintCells:Set<Cell>) : (Cell->AnnotatedSymbol<AnnotatedCandidate>)->(Cell->HintAnnotatedSymbol) =
    fun (etoc:Cell->AnnotatedSymbol<AnnotatedCandidate>) ->
        fun (cell:Cell) ->
            let hac = 
                if Set.contains cell hintCells then
                    convertToH HASHint HACId
                else
                    HASId
            (etoc >> hac) cell

let houseSymbolTo (houseCells:Set<Cell>) : (Cell->AnnotatedSymbol<AnnotatedCandidate>)->(Cell->HintAnnotatedSymbol) =
    fun (etoc:Cell->AnnotatedSymbol<AnnotatedCandidate>) ->
        fun (cell:Cell) ->
            let hac =
                if Set.contains cell houseCells then
                    convertToH HASHint HACHouse
                else
                    HASId
            (etoc >> hac) cell

let setCellHint (setCell:Cell) (setCandidate:Candidate) (candidateLookup:Cell->Set<Candidate>) =
    fun (etoc:Cell->HintAnnotatedSymbol) ->
        fun (cell:Cell) ->
            let hac l =
                if cell = setCell then
                    //let ccs = candidateLookup cell
                    match l with
                    | HASHint (Candidates candidates) ->
                        let newHC c =
                            if c = setCandidate then
                                HACSet
                            //else if Set.contains c ccs then
                            //    Reduction
                            else
                                candidates c

                        HASHint (Candidates newHC)
                    | _ -> l
                else
                    l
            (etoc >> hac) cell

let setReductions (candidate:Candidate) (candidateReductions:Set<Cell>) =
    fun (toLabel:Cell->HintAnnotatedSymbol) ->
        fun (cell:Cell) ->
            let l = toLabel cell
            if Set.contains cell candidateReductions then
                match l with
                | HASHId (Candidates candidates) ->
                    let newHC c = 
                        if c = candidate then
                            Reduction
                        else
                            candidates c

                    HASHint (Candidates newHC)
                | _ -> l
            else
                l

let setReductions2 (candidateReductions:CandidateReduction list) =
    fun (etoc:Cell->HintAnnotatedSymbol) ->
        fun (cell:Cell) ->
            let label = etoc cell

            let o = List.tryFind (fun cr -> cell = cr.cell) candidateReductions
            match o with
            | Some cr ->
                match label with
                | HASHint (Candidates candidates) ->
                    let newHC candidate =
                        if Set.contains candidate cr.symbols then
                            Reduction
                        else
                            candidates candidate

                    HASHint (Candidates newHC)
                | _ -> label
            | _ ->
                label

let setPointer (cells:Set<Cell>) (symbols:Set<Candidate>) =
    fun (etoc:Cell->HintAnnotatedSymbol) ->
        fun (cell:Cell) ->
            let label = etoc cell

            if Set.contains cell cells then
                match label with
                | HASHint (Candidates candidates) ->
                    let newHC candidate =
                        if Set.contains candidate symbols then
                            Pointer
                        else
                            candidates candidate

                    HASHint (Candidates newHC)
                | _ -> raise CellStateInvalid
            else
                label
