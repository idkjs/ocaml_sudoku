module hints.hints

open core.setCell
open core.sudoku

exception CellStateInvalid

let first (set : Set<'a>) = Set.toList set |> List.head

let firstOpt (set : Set<'a>) = 
    let l = Set.toList set
    match l with
    | s :: ss -> Some s
    | _ -> None

let convertToH : AnnotatedSymbol<AnnotatedCandidate> -> AnnotatedSymbol<HintAnnotatedCandidate> = 
    fun entry -> 
        match entry with
        | Given s -> Given s
        | Set s -> Set s
        | Candidates candidateMap -> Candidates(candidateMap >> HACId)

let setHint (houseCells : Set<Cell>) : (Cell -> AnnotatedSymbol<AnnotatedCandidate>) -> Cell -> HintAnnotatedSymbol = 
    fun (etoc : Cell -> AnnotatedSymbol<AnnotatedCandidate>) (cell : Cell) -> 
        let hac label = 
            { HintAnnotatedSymbol.symbol = convertToH label
              hintHouse = Set.contains cell houseCells }
        (etoc >> hac) cell

let rewriteHASHIdCandidates (newHC : (Candidate -> HintAnnotatedCandidate) -> Candidate -> HintAnnotatedCandidate) 
    (hintAnnotatedSymbol : HintAnnotatedSymbol) = 
    match hintAnnotatedSymbol.symbol with
    | Candidates candidates -> { hintAnnotatedSymbol with symbol = Candidates(newHC candidates) }
    | _ -> hintAnnotatedSymbol

let setCellHint (setCell : Cell) (setCandidate : Candidate) = 
    let newHC candidates c = 
        if c = setCandidate then HACSet
        else candidates c
    
    let hintAnnotationTransformer cell = 
        if cell = setCell then rewriteHASHIdCandidates newHC
        else id
    
    fun (etoc : Cell -> HintAnnotatedSymbol) (cell : Cell) -> (hintAnnotationTransformer cell) (etoc cell)

let setCellHintOption (setCellValueOption : SetCellValue option) = 
    match setCellValueOption with
    | Some { cell = cell; candidate = value } -> setCellHint cell value
    | None -> id

let setReductions2 (candidateReductions : Set<CandidateReduction>) hac = 
    let newHC (crSymbols : Set<Candidate>) candidates candidate = 
        if Set.contains candidate crSymbols then hac
        else candidates candidate
    
    let hintAnnotationTransformer cell = 
        let o = Set.filter (fun cr -> cell = cr.cell) candidateReductions
        match firstOpt o with
        | Some cr -> rewriteHASHIdCandidates (newHC cr.symbols)
        | _ -> id
    

    fun (etoc : Cell -> HintAnnotatedSymbol) (cell : Cell) -> (hintAnnotationTransformer cell) (etoc cell)

type HintDescription = 
    { house : House option
      candidateReductions : Set<CandidateReduction>
      setCellValue : SetCellValue option
      pointers : Set<CandidateReduction> }

let mhas (hd : HintDescription) (houseCells : House -> Set<Cell>) (cellHouseCells : Cell -> Set<Cell>) 
    (candidateLookup : Cell -> Set<Candidate>) (solutionGrid : Cell -> AnnotatedSymbol<AnnotatedCandidate>) = 
    let houseCells = 
        match hd.house with
        | Some house -> houseCells house
        | None -> set []
    
    let crs = 
        match hd.setCellValue with
        | Some scv -> setCellCandidateReductions scv cellHouseCells candidateLookup
        | None -> set []
    
    (setHint houseCells
     >> setReductions2 crs Reduction
     >> setReductions2 hd.candidateReductions Reduction
     >> setReductions2 hd.pointers Pointer
     >> setCellHintOption hd.setCellValue) solutionGrid
