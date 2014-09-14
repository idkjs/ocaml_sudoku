module hints.hints

open System
open System.Text

open core.setCell
open core.sudoku

exception CellStateInvalid

let first (set : Set<'a>) = Set.toList set |> List.head

let rec doSetSubsets (list : List<'a>) (size : int) (prefix : List<'a>) : List<List<'a>> =
    match list with
    | x :: xs when size > 0 ->
        if size = 1 then
            (x :: prefix) :: doSetSubsets xs 1 prefix
        else
            let inc = doSetSubsets xs (size - 1) (x :: prefix)
            let dec = doSetSubsets xs size prefix

            List.append inc dec
    | _ -> []

let rec setSubsets (list : List<'a>) (size : int) : List<List<'a>> =
    doSetSubsets list size []

(*
    let s0 = []
    let p00 = setSubsets s0 0
    let p01 = setSubsets s0 1
    let p02 = setSubsets s0 2

    let s1 = [ 1 ]
    let p10 = setSubsets s1 0
    let p11 = setSubsets s1 1
    let p12 = setSubsets s1 2
    let p13 = setSubsets s1 3

    let s2 = [ 1; 2 ]
    let p20 = setSubsets s2 0
    let p21 = setSubsets s2 1
    let p22 = setSubsets s2 2
    let p23 = setSubsets s2 3
    let p24 = setSubsets s2 4

    let s3 = [ 1; 2; 3 ]
    let p30 = setSubsets s3 0
    let p31 = setSubsets s3 1
    let p32 = setSubsets s3 2
    let p33 = setSubsets s3 3
    let p34 = setSubsets s3 4
    let p35 = setSubsets s3 5
*)


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

let setHint (primaryHouseCells : Set<Cell>) (secondaryHouseCells : Set<Cell>) : (Cell -> AnnotatedSymbol<AnnotatedCandidate>) -> Cell -> HintAnnotatedSymbol = 
    fun (etoc : Cell -> AnnotatedSymbol<AnnotatedCandidate>) (cell : Cell) -> 
        let hac label = 
            { HintAnnotatedSymbol.symbol = convertToH label
              primaryHintHouse = Set.contains cell primaryHouseCells
              secondaryHintHouse = Set.contains cell secondaryHouseCells }
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
    { primaryHouses : Set<House>
      secondaryHouses : Set<House>
      candidateReductions : Set<CandidateReduction>
      setCellValue : SetCellValue option
      pointers : Set<CandidateReduction> }
    override this.ToString() = 
        let sb = StringBuilder()

        sb.AppendLine(String.Format("Primary Houses {0}", String.Join(",", Set.toArray this.primaryHouses))) 
        |> ignore
        sb.AppendLine(String.Format("Secondary Houses {0}", String.Join(",", Set.toArray this.secondaryHouses))) 
        |> ignore
        sb.AppendLine(String.Format("Pointers {0}", String.Join(",", Set.toArray this.pointers))) 
        |> ignore

        Set.iter (fun (cr : CandidateReduction) -> sb.AppendLine(String.Format("  {0}", cr)) |> ignore) 
            this.candidateReductions

        sb.ToString()

let mhas (hd : HintDescription) (houseCells : House -> Set<Cell>) (cellHouseCells : Cell -> Set<Cell>) 
    (candidateLookup : Cell -> Set<Candidate>) (solutionGrid : Cell -> AnnotatedSymbol<AnnotatedCandidate>) = 
    let primaryHouseCells = Set.map houseCells hd.primaryHouses |> Set.unionMany
    let secondaryHouseCells = Set.map houseCells hd.secondaryHouses |> Set.unionMany

    let crs = 
        match hd.setCellValue with
        | Some scv -> setCellCandidateReductions scv cellHouseCells candidateLookup
        | None -> set []
    
    (setHint primaryHouseCells secondaryHouseCells
     >> setReductions2 crs Reduction
     >> setReductions2 hd.candidateReductions Reduction
     >> setReductions2 hd.pointers Pointer
     >> setCellHintOption hd.setCellValue) solutionGrid
